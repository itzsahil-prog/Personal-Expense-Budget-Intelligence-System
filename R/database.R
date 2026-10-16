# =============================================================================
# R/database.R
# SQLite persistence layer for the Personal Expense & Budget Intelligence System.
#
# ALL database I/O is centralised here.  No other module writes SQL directly.
#
# Public API (functions intended for use by server.R and other modules)
# ----------------------------------------------------------------------
# Connection
#   connect_database()      — open / create the SQLite file, return connection
#   disconnect_database()   — safely close a connection
#   initialize_database()   — create schema + seed reference data (idempotent)
#
# Transactions
#   insert_transaction()    — validate + insert one transaction, return its ID
#   update_transaction()    — validate + update five editable fields
#   delete_transaction()    — delete by ID
#   get_transactions()      — flexible filtered read, returns data.frame
#
# Budgets
#   insert_budget()         — insert or replace a budget row, return budget_id
#   get_budgets()           — filtered read, returns data.frame
#   update_budget()         — update budget_amount for an existing row
#
# Internal helpers (prefixed with a dot, not intended for direct use)
#   .seed_categories()
#   .seed_settings()
#
# Utility (used by server and reporting modules)
#   get_transaction_by_id()
#   get_budget_utilization()
#   get_settings()
#   update_setting()
#   get_categories()
#   get_summary_counts()
#   db_health_check()
#   db_bulk_insert_transactions()
#   db_delete_budgets_for_month()
#
# Schema (spec-mandated column names)
# ------------------------------------
# transactions : transaction_id TEXT PK, date TEXT, category TEXT,
#                description TEXT, amount REAL, payment_method TEXT,
#                created_at TEXT
# budgets      : budget_id INTEGER PK AUTOINCREMENT, month TEXT,
#                category TEXT, budget_amount REAL
# categories   : category_id INTEGER PK AUTOINCREMENT,
#                category_name TEXT UNIQUE, display_label TEXT,
#                icon TEXT, color_hex TEXT, is_active INTEGER,
#                sort_order INTEGER
# settings     : key TEXT PK, value TEXT
#
# Note on budgets schema
# ----------------------
# The spec defines budget_id as INTEGER PRIMARY KEY AUTOINCREMENT and month as
# TEXT.  We store month as "YYYY-MM" (e.g. "2026-09") so queries can use
# standard string comparison.  The upsert logic checks for existing rows by
# (month, category) before inserting to avoid duplicates.
#
# Dependencies: DBI, RSQLite, validation.R
# =============================================================================

library(DBI)
library(RSQLite)


# =============================================================================
# PATH CONSTANT
# =============================================================================

# Default path — override by passing db_path explicitly (useful in tests).
DEFAULT_DB_PATH <- file.path("data", "expenses.sqlite")


# =============================================================================
# CONNECTION
# =============================================================================

#' Open (or create) the SQLite database and return a DBI connection.
#'
#' The parent directory is created automatically when it does not exist.
#' Safe to call repeatedly; SQLite creates the file on first connect.
#'
#' @param db_path  Character path to the .sqlite file.
#'                 Defaults to DEFAULT_DB_PATH.
#' @return         A DBI connection object.
connect_database <- function(db_path = DEFAULT_DB_PATH) {
  dir.create(dirname(db_path), showWarnings = FALSE, recursive = TRUE)
  DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
}


#' Safely close a DBI connection.
#'
#' Silently does nothing if con is NULL or already closed.
#'
#' @param con  DBI connection returned by connect_database().
#' @return     NULL invisibly.
disconnect_database <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
  }
  invisible(NULL)
}


# =============================================================================
# SCHEMA INITIALISATION
# =============================================================================

#' Create all tables and seed reference data.
#'
#' Idempotent: uses CREATE TABLE IF NOT EXISTS and INSERT OR IGNORE throughout.
#' Safe to call on every application start.
#'
#' @param con  DBI connection from connect_database().
#' @return     TRUE invisibly.
initialize_database <- function(con) {

  # SQLite foreign-key enforcement is off by default
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # ------------------------------------------------------------------
  # transactions
  # Spec schema: transaction_id TEXT PK, date, category, description,
  #              amount REAL, payment_method, created_at TEXT
  # We also keep updated_at internally for audit purposes.
  # ------------------------------------------------------------------
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS transactions (
      transaction_id  TEXT  PRIMARY KEY,
      date            TEXT  NOT NULL,
      category        TEXT  NOT NULL,
      description     TEXT,
      amount          REAL  NOT NULL CHECK(amount > 0),
      payment_method  TEXT  NOT NULL,
      created_at      TEXT,
      updated_at      TEXT
    );
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_txn_date
      ON transactions(date);
  ")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_txn_category
      ON transactions(category);
  ")

  # ------------------------------------------------------------------
  # budgets
  # Spec schema: budget_id INTEGER PK AUTOINCREMENT, month TEXT,
  #              category TEXT, budget_amount REAL
  # We also keep created_at / updated_at for audit purposes.
  # ------------------------------------------------------------------
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS budgets (
      budget_id      INTEGER PRIMARY KEY AUTOINCREMENT,
      month          TEXT    NOT NULL,
      category       TEXT    NOT NULL,
      budget_amount  REAL    NOT NULL CHECK(budget_amount >= 0),
      created_at     TEXT,
      updated_at     TEXT,
      UNIQUE(month, category)
    );
  ")

  # ------------------------------------------------------------------
  # categories
  # Spec schema: category_id INTEGER PK AUTOINCREMENT,
  #              category_name TEXT UNIQUE NOT NULL
  # We add display metadata columns that the UI needs.
  # ------------------------------------------------------------------
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS categories (
      category_id    INTEGER PRIMARY KEY AUTOINCREMENT,
      category_name  TEXT    UNIQUE NOT NULL,
      display_label  TEXT    NOT NULL,
      icon           TEXT,
      color_hex      TEXT,
      is_active      INTEGER DEFAULT 1,
      sort_order     INTEGER DEFAULT 0
    );
  ")

  # ------------------------------------------------------------------
  # settings
  # Spec schema: key TEXT PK, value TEXT
  # We add updated_at to track when preferences were last changed.
  # ------------------------------------------------------------------
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS settings (
      key        TEXT PRIMARY KEY,
      value      TEXT,
      updated_at TEXT
    );
  ")

  .seed_categories(con)
  .seed_settings(con)

  invisible(TRUE)
}


# ------------------------------------------------------------------
# Internal: seed categories
# ------------------------------------------------------------------
.seed_categories <- function(con) {
  existing <- DBI::dbGetQuery(
    con, "SELECT category_name FROM categories;"
  )$category_name

  icons <- c(
    "Food"          = "utensils",
    "Grocery"       = "shopping-basket",
    "Transport"     = "bus",
    "Rent/PG"       = "home",
    "Education"     = "graduation-cap",
    "Shopping"      = "shopping-bag",
    "Healthcare"    = "heartbeat",
    "Entertainment" = "film",
    "Bills"         = "file-invoice",
    "Other"         = "ellipsis-h"
  )
  colours <- c(
    "Food"          = "#FF6B6B",
    "Grocery"       = "#4ECDC4",
    "Transport"     = "#45B7D1",
    "Rent/PG"       = "#96CEB4",
    "Education"     = "#FFEAA7",
    "Shopping"      = "#DDA0DD",
    "Healthcare"    = "#98D8C8",
    "Entertainment" = "#F7DC6F",
    "Bills"         = "#85C1E9",
    "Other"         = "#CCD1D1"
  )

  for (i in seq_along(VALID_CATEGORIES)) {
    cat_name <- VALID_CATEGORIES[i]
    if (!cat_name %in% existing) {
      DBI::dbExecute(con,
        "INSERT OR IGNORE INTO categories
           (category_name, display_label, icon, color_hex, is_active, sort_order)
         VALUES (?, ?, ?, ?, 1, ?);",
        params = list(
          cat_name,
          cat_name,
          icons[[cat_name]],
          colours[[cat_name]],
          i
        )
      )
    }
  }
}


# ------------------------------------------------------------------
# Internal: seed default settings
# INSERT OR IGNORE means existing user preferences are never overwritten.
# ------------------------------------------------------------------
.seed_settings <- function(con) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  defaults <- list(
    list(key = "currency",              value = "INR"),
    list(key = "currency_symbol",       value = "\u20b9"),
    list(key = "fiscal_month_start",    value = "1"),
    list(key = "anomaly_method",        value = "IQR"),
    list(key = "anomaly_iqr_threshold", value = "1.5"),
    list(key = "anomaly_z_threshold",   value = "2.5"),
    list(key = "forecast_horizon_days", value = "30"),
    list(key = "forecast_method",       value = "ets")
  )

  for (s in defaults) {
    DBI::dbExecute(con,
      "INSERT OR IGNORE INTO settings (key, value, updated_at)
       VALUES (?, ?, ?);",
      params = list(s$key, s$value, ts)
    )
  }
}


# =============================================================================
# INPUT VALIDATION HELPERS
# These are internal guards called by the write functions.
# They do NOT duplicate the full UI-level validation in validation.R; they
# enforce the database-level invariants.
# =============================================================================

# Stops with a descriptive error if any field fails its database-level check.
.validate_transaction_fields <- function(date, category, description,
                                          amount, payment_method) {
  # date
  if (is.null(date) || is.na(date) || trimws(as.character(date)) == "") {
    stop("insert_transaction: 'date' is required.")
  }
  parsed_date <- tryCatch(
    as.Date(as.character(date)),
    error = function(e) NA, warning = function(w) NA
  )
  if (is.na(parsed_date)) {
    stop(sprintf(
      "insert_transaction: '%s' cannot be parsed as a date (YYYY-MM-DD expected).",
      date
    ))
  }
  if (parsed_date > Sys.Date()) {
    stop("insert_transaction: date cannot be in the future.")
  }

  # category
  if (is.null(category) || trimws(category) == "") {
    stop("insert_transaction: 'category' is required.")
  }
  if (!category %in% VALID_CATEGORIES) {
    stop(sprintf(
      "insert_transaction: category '%s' is not valid. Allowed: %s.",
      category, paste(VALID_CATEGORIES, collapse = ", ")
    ))
  }

  # description — allowed to be empty per spec (TEXT, no NOT NULL)
  if (!is.null(description) && nchar(trimws(as.character(description))) > 200) {
    stop("insert_transaction: description must be 200 characters or fewer.")
  }

  # amount
  amt <- suppressWarnings(as.numeric(amount))
  if (is.null(amount) || is.na(amt)) {
    stop("insert_transaction: 'amount' is required and must be numeric.")
  }
  if (amt <= 0) {
    stop("insert_transaction: amount must be greater than zero.")
  }
  if (amt > MAX_TRANSACTION_AMOUNT) {
    stop(sprintf(
      "insert_transaction: amount exceeds the maximum of %s.",
      format(MAX_TRANSACTION_AMOUNT, big.mark = ",", scientific = FALSE)
    ))
  }

  # payment_method
  if (is.null(payment_method) || trimws(payment_method) == "") {
    stop("insert_transaction: 'payment_method' is required.")
  }
  if (!payment_method %in% VALID_PAYMENT_METHODS) {
    stop(sprintf(
      "insert_transaction: payment method '%s' is not valid. Allowed: %s.",
      payment_method, paste(VALID_PAYMENT_METHODS, collapse = ", ")
    ))
  }

  invisible(TRUE)
}


.validate_budget_fields <- function(month, category, budget_amount) {
  # month — must be "YYYY-MM"
  if (is.null(month) || is.na(month) || trimws(as.character(month)) == "") {
    stop("insert_budget: 'month' is required.")
  }
  if (!grepl("^\\d{4}-\\d{2}$", as.character(month))) {
    stop(sprintf(
      "insert_budget: month '%s' must be in YYYY-MM format.", month
    ))
  }
  month_parts <- as.integer(strsplit(as.character(month), "-")[[1]])
  if (month_parts[2] < 1L || month_parts[2] > 12L) {
    stop(sprintf("insert_budget: month value %d is outside 1-12.", month_parts[2]))
  }

  # category
  if (is.null(category) || trimws(category) == "") {
    stop("insert_budget: 'category' is required.")
  }
  if (!category %in% VALID_CATEGORIES) {
    stop(sprintf(
      "insert_budget: category '%s' is not valid. Allowed: %s.",
      category, paste(VALID_CATEGORIES, collapse = ", ")
    ))
  }

  # budget_amount
  ba <- suppressWarnings(as.numeric(budget_amount))
  if (is.null(budget_amount) || is.na(ba)) {
    stop("insert_budget: 'budget_amount' is required and must be numeric.")
  }
  if (ba < 0) {
    stop("insert_budget: budget_amount cannot be negative.")
  }
  if (ba > MAX_TRANSACTION_AMOUNT) {
    stop("insert_budget: budget_amount exceeds the maximum allowed value.")
  }

  invisible(TRUE)
}


# =============================================================================
# TRANSACTIONS — WRITE
# =============================================================================

#' Insert a single transaction after validating all fields.
#'
#' User-supplied values are bound via parameterised queries — never
#' concatenated into SQL strings.
#'
#' @param con             DBI connection.
#' @param date            Character or Date "YYYY-MM-DD".
#' @param category        One of VALID_CATEGORIES.
#' @param description     Character (may be empty). Max 200 chars.
#' @param amount          Positive numeric.
#' @param payment_method  One of VALID_PAYMENT_METHODS.
#'
#' @return Character transaction_id of the newly inserted row.
#'         Stops with an error if validation fails.
insert_transaction <- function(con, date, category, description,
                                amount, payment_method) {
  .validate_transaction_fields(date, category, description,
                                amount, payment_method)

  ts     <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  txn_id <- generate_transaction_id(as.character(date))

  DBI::dbExecute(con,
    "INSERT INTO transactions
       (transaction_id, date, category, description,
        amount, payment_method, created_at, updated_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?);",
    params = list(
      txn_id,
      format(as.Date(as.character(date)), "%Y-%m-%d"),
      as.character(category),
      trimws(as.character(description %||% "")),
      as.numeric(amount),
      as.character(payment_method),
      ts,
      ts
    )
  )

  txn_id
}


#' Update the five editable fields of an existing transaction.
#'
#' created_at is preserved.  updated_at is set to the current timestamp.
#'
#' @param con             DBI connection.
#' @param transaction_id  Character ID of the row to update.
#' @param date            New date (Character or Date).
#' @param category        New category.
#' @param description     New description.
#' @param amount          New amount.
#' @param payment_method  New payment method.
#'
#' @return TRUE invisibly. Stops with an error if the ID is not found
#'         or validation fails.
update_transaction <- function(con, transaction_id, date, category,
                                description, amount, payment_method) {
  # Verify the row exists before validating the new values
  existing <- get_transaction_by_id(con, transaction_id)
  if (nrow(existing) == 0L) {
    stop(sprintf("update_transaction: transaction '%s' not found.", transaction_id))
  }

  .validate_transaction_fields(date, category, description,
                                amount, payment_method)

  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  DBI::dbExecute(con,
    "UPDATE transactions
     SET date           = ?,
         category       = ?,
         description    = ?,
         amount         = ?,
         payment_method = ?,
         updated_at     = ?
     WHERE transaction_id = ?;",
    params = list(
      format(as.Date(as.character(date)), "%Y-%m-%d"),
      as.character(category),
      trimws(as.character(description %||% "")),
      as.numeric(amount),
      as.character(payment_method),
      ts,
      as.character(transaction_id)
    )
  )

  invisible(TRUE)
}


#' Permanently delete a transaction by ID.
#'
#' @param con             DBI connection.
#' @param transaction_id  Character transaction ID.
#'
#' @return TRUE invisibly. Stops with an error if the ID is not found.
delete_transaction <- function(con, transaction_id) {
  existing <- get_transaction_by_id(con, transaction_id)
  if (nrow(existing) == 0L) {
    stop(sprintf("delete_transaction: transaction '%s' not found.", transaction_id))
  }

  DBI::dbExecute(con,
    "DELETE FROM transactions WHERE transaction_id = ?;",
    params = list(as.character(transaction_id))
  )

  invisible(TRUE)
}


# =============================================================================
# TRANSACTIONS — READ
# =============================================================================

#' Retrieve transactions with optional filtering.
#'
#' All filter parameters are optional.  Omitting a parameter means no filter
#' is applied on that dimension.  All user-supplied filter values are bound as
#' parameters — never interpolated into the SQL string.
#'
#' @param con             DBI connection.
#' @param date_from       Character "YYYY-MM-DD" or NULL.
#' @param date_to         Character "YYYY-MM-DD" or NULL.
#' @param categories      Character vector of category names or NULL.
#' @param payment_methods Character vector of payment method names or NULL.
#' @param search_text     Substring matched against description
#'                        (case-insensitive) or NULL.
#' @param limit           Maximum rows to return (NULL = all rows).
#' @param order_by        SQL ORDER BY fragment. Default "date DESC".
#'                        Only a fixed whitelist of values is accepted to
#'                        prevent SQL injection via this parameter.
#'
#' @return data.frame with columns: transaction_id, date, category,
#'         description, amount, payment_method, created_at, updated_at.
#'         Returns a zero-row data.frame with correct column types when no
#'         rows match.
get_transactions <- function(con,
                              date_from       = NULL,
                              date_to         = NULL,
                              categories      = NULL,
                              payment_methods = NULL,
                              search_text     = NULL,
                              limit           = NULL,
                              order_by        = "date DESC, created_at DESC") {

  # Whitelist order_by to prevent injection via that string argument
  allowed_order <- c(
    "date DESC, created_at DESC",
    "date ASC, created_at ASC",
    "date DESC",
    "date ASC",
    "amount DESC",
    "amount ASC",
    "category ASC",
    "category DESC",
    "created_at DESC",
    "created_at ASC"
  )
  if (!order_by %in% allowed_order) {
    order_by <- "date DESC, created_at DESC"
  }

  clauses <- character(0)
  params  <- list()

  if (!is.null(date_from) && nchar(trimws(date_from)) > 0) {
    clauses <- c(clauses, "date >= ?")
    params  <- c(params, list(as.character(date_from)))
  }
  if (!is.null(date_to) && nchar(trimws(date_to)) > 0) {
    clauses <- c(clauses, "date <= ?")
    params  <- c(params, list(as.character(date_to)))
  }
  if (!is.null(categories) && length(categories) > 0) {
    ph      <- paste(rep("?", length(categories)), collapse = ", ")
    clauses <- c(clauses, sprintf("category IN (%s)", ph))
    params  <- c(params, as.list(as.character(categories)))
  }
  if (!is.null(payment_methods) && length(payment_methods) > 0) {
    ph      <- paste(rep("?", length(payment_methods)), collapse = ", ")
    clauses <- c(clauses, sprintf("payment_method IN (%s)", ph))
    params  <- c(params, as.list(as.character(payment_methods)))
  }
  if (!is.null(search_text) && nchar(trimws(search_text)) > 0) {
    clauses <- c(clauses, "LOWER(description) LIKE ?")
    params  <- c(params, list(paste0("%", tolower(trimws(search_text)), "%")))
  }

  where_sql <- if (length(clauses) > 0) {
    paste("WHERE", paste(clauses, collapse = " AND "))
  } else ""

  limit_sql <- if (!is.null(limit) && !is.na(as.integer(limit))) {
    sprintf("LIMIT %d", as.integer(limit))
  } else ""

  sql <- sprintf(
    "SELECT transaction_id, date, category, description,
            amount, payment_method, created_at, updated_at
     FROM   transactions
     %s
     ORDER BY %s
     %s;",
    where_sql, order_by, limit_sql
  )

  result <- DBI::dbGetQuery(con, sql, params = params)

  # Enforce correct R types regardless of SQLite storage class
  if (nrow(result) > 0) {
    result$amount <- as.numeric(result$amount)
    result$date   <- as.character(result$date)
  } else {
    result <- data.frame(
      transaction_id = character(0),
      date           = character(0),
      category       = character(0),
      description    = character(0),
      amount         = numeric(0),
      payment_method = character(0),
      created_at     = character(0),
      updated_at     = character(0),
      stringsAsFactors = FALSE
    )
  }

  result
}


#' Retrieve a single transaction by its ID.
#'
#' @param con             DBI connection.
#' @param transaction_id  Character transaction ID.
#' @return Single-row data.frame, or a zero-row data.frame if not found.
get_transaction_by_id <- function(con, transaction_id) {
  result <- DBI::dbGetQuery(con,
    "SELECT transaction_id, date, category, description,
            amount, payment_method, created_at, updated_at
     FROM   transactions
     WHERE  transaction_id = ?;",
    params = list(as.character(transaction_id))
  )
  if (nrow(result) > 0) result$amount <- as.numeric(result$amount)
  result
}


#' Bulk-insert a pre-validated, normalised transactions data.frame.
#'
#' Intended for use after normalise_import_df() has already been applied.
#' Runs inside a single SQLite transaction for atomicity and performance.
#' Rows with duplicate transaction_id values are silently ignored
#' (INSERT OR IGNORE), preventing duplicate data on re-import.
#'
#' @param con  DBI connection.
#' @param df   data.frame with columns: transaction_id, date, category,
#'             description, amount, payment_method, created_at, updated_at.
#' @return     Integer count of rows successfully inserted.
db_bulk_insert_transactions <- function(con, df) {
  if (is.null(df) || nrow(df) == 0L) return(0L)

  DBI::dbWithTransaction(con, {
    for (i in seq_len(nrow(df))) {
      row <- df[i, , drop = FALSE]
      DBI::dbExecute(con,
        "INSERT OR IGNORE INTO transactions
           (transaction_id, date, category, description,
            amount, payment_method, created_at, updated_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?);",
        params = list(
          as.character(row$transaction_id),
          as.character(row$date),
          as.character(row$category),
          as.character(row$description),
          as.numeric(row$amount),
          as.character(row$payment_method),
          as.character(row$created_at),
          as.character(row$updated_at)
        )
      )
    }
  })

  nrow(df)
}


# =============================================================================
# BUDGETS — WRITE
# =============================================================================

#' Insert a budget for a month/category combination.
#'
#' If a budget already exists for the same (month, category) pair it is
#' replaced (UPSERT semantics via UPDATE + INSERT guard).  The budget_id
#' of the affected row is returned in both cases.
#'
#' @param con            DBI connection.
#' @param month          Character "YYYY-MM", e.g. "2026-09".
#' @param category       One of VALID_CATEGORIES.
#' @param budget_amount  Non-negative numeric.
#'
#' @return Integer budget_id of the inserted / updated row.
#'         Stops with an error if validation fails.
insert_budget <- function(con, month, category, budget_amount) {
  .validate_budget_fields(month, category, budget_amount)

  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  month     <- as.character(month)
  category  <- as.character(category)
  ba        <- as.numeric(budget_amount)

  # Check for an existing row to decide INSERT vs UPDATE
  existing <- DBI::dbGetQuery(con,
    "SELECT budget_id FROM budgets WHERE month = ? AND category = ?;",
    params = list(month, category)
  )

  if (nrow(existing) > 0L) {
    DBI::dbExecute(con,
      "UPDATE budgets
       SET budget_amount = ?, updated_at = ?
       WHERE month = ? AND category = ?;",
      params = list(ba, ts, month, category)
    )
    return(as.integer(existing$budget_id[1]))
  }

  DBI::dbExecute(con,
    "INSERT INTO budgets (month, category, budget_amount, created_at, updated_at)
     VALUES (?, ?, ?, ?, ?);",
    params = list(month, category, ba, ts, ts)
  )

  # Retrieve the autoincrement ID assigned by SQLite
  row <- DBI::dbGetQuery(con,
    "SELECT budget_id FROM budgets WHERE month = ? AND category = ?;",
    params = list(month, category)
  )
  as.integer(row$budget_id[1])
}


#' Update the budget_amount of an existing budget row.
#'
#' @param con            DBI connection.
#' @param budget_id      Integer budget_id returned by insert_budget().
#' @param budget_amount  New non-negative numeric budget amount.
#'
#' @return TRUE invisibly. Stops if budget_id is not found or amount is invalid.
update_budget <- function(con, budget_id, budget_amount) {
  ba <- suppressWarnings(as.numeric(budget_amount))
  if (is.na(ba) || ba < 0) {
    stop("update_budget: budget_amount must be a non-negative number.")
  }
  if (ba > MAX_TRANSACTION_AMOUNT) {
    stop("update_budget: budget_amount exceeds the maximum allowed value.")
  }

  existing <- DBI::dbGetQuery(con,
    "SELECT budget_id FROM budgets WHERE budget_id = ?;",
    params = list(as.integer(budget_id))
  )
  if (nrow(existing) == 0L) {
    stop(sprintf("update_budget: budget_id %d not found.", as.integer(budget_id)))
  }

  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con,
    "UPDATE budgets SET budget_amount = ?, updated_at = ? WHERE budget_id = ?;",
    params = list(ba, ts, as.integer(budget_id))
  )

  invisible(TRUE)
}


# =============================================================================
# BUDGETS — READ
# =============================================================================

#' Retrieve budget rows with optional month and/or category filtering.
#'
#' @param con       DBI connection.
#' @param month     Character "YYYY-MM" or NULL (all months).
#' @param category  Character category name or NULL (all categories).
#'
#' @return data.frame with columns: budget_id (integer), month, category,
#'         budget_amount, created_at, updated_at.
#'         Sorted by month DESC, category ASC.
get_budgets <- function(con, month = NULL, category = NULL) {
  clauses <- character(0)
  params  <- list()

  if (!is.null(month) && nchar(trimws(as.character(month))) > 0) {
    clauses <- c(clauses, "month = ?")
    params  <- c(params, list(as.character(month)))
  }
  if (!is.null(category) && nchar(trimws(as.character(category))) > 0) {
    clauses <- c(clauses, "category = ?")
    params  <- c(params, list(as.character(category)))
  }

  where_sql <- if (length(clauses) > 0) {
    paste("WHERE", paste(clauses, collapse = " AND "))
  } else ""

  sql <- sprintf(
    "SELECT budget_id, month, category, budget_amount, created_at, updated_at
     FROM   budgets
     %s
     ORDER BY month DESC, category ASC;",
    where_sql
  )

  result <- DBI::dbGetQuery(con, sql, params = params)
  if (nrow(result) > 0) {
    result$budget_id      <- as.integer(result$budget_id)
    result$budget_amount  <- as.numeric(result$budget_amount)
  }
  result
}


#' Retrieve budget utilisation for a given month.
#'
#' Joins the budgets table with actual transaction totals for the same month
#' to return budget_used, budget_remaining, and utilization_pct per category.
#'
#' @param con    DBI connection.
#' @param month  Character "YYYY-MM".
#'
#' @return data.frame with columns: category, budget_amount, budget_used,
#'         budget_remaining, utilization_pct.
#'         Sorted by utilization_pct DESC.
get_budget_utilization <- function(con, month) {
  month <- as.character(month)

  DBI::dbGetQuery(con,
    "SELECT
       b.category,
       b.budget_amount,
       COALESCE(SUM(t.amount), 0.0)
         AS budget_used,
       b.budget_amount - COALESCE(SUM(t.amount), 0.0)
         AS budget_remaining,
       CASE
         WHEN b.budget_amount > 0
         THEN ROUND(
                COALESCE(SUM(t.amount), 0.0) / b.budget_amount * 100.0,
                1
              )
         ELSE 0.0
       END AS utilization_pct
     FROM budgets b
     LEFT JOIN transactions t
       ON  t.category = b.category
       AND strftime('%Y-%m', t.date) = ?
     WHERE b.month = ?
     GROUP BY b.category, b.budget_amount
     ORDER BY utilization_pct DESC;",
    params = list(month, month)
  )
}


#' Delete all budget rows for a given month.
#'
#' @param con    DBI connection.
#' @param month  Character "YYYY-MM".
#' @return Integer count of rows deleted.
db_delete_budgets_for_month <- function(con, month) {
  DBI::dbExecute(con,
    "DELETE FROM budgets WHERE month = ?;",
    params = list(as.character(month))
  )
}


# =============================================================================
# SETTINGS
# =============================================================================

#' Retrieve all settings as a named character vector.
#'
#' @param con  DBI connection.
#' @return Named character vector (names = keys, values = setting values).
get_settings <- function(con) {
  rows <- DBI::dbGetQuery(con, "SELECT key, value FROM settings;")
  stats::setNames(as.character(rows$value), rows$key)
}


#' Update a single setting value.
#'
#' Uses INSERT OR REPLACE so new keys can be created at runtime.
#'
#' @param con    DBI connection.
#' @param key    Character setting key.
#' @param value  Character (or coercible) new value.
#' @return TRUE invisibly.
update_setting <- function(con, key, value) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con,
    "INSERT OR REPLACE INTO settings (key, value, updated_at)
     VALUES (?, ?, ?);",
    params = list(as.character(key), as.character(value), ts)
  )
  invisible(TRUE)
}


# =============================================================================
# CATEGORIES — READ
# =============================================================================

#' Retrieve all active categories ordered by sort_order.
#'
#' @param con  DBI connection.
#' @return data.frame with columns: category_id, category_name, display_label,
#'         icon, color_hex, is_active, sort_order.
get_categories <- function(con) {
  DBI::dbGetQuery(con,
    "SELECT category_id, category_name, display_label,
            icon, color_hex, is_active, sort_order
     FROM   categories
     WHERE  is_active = 1
     ORDER BY sort_order ASC;"
  )
}


# =============================================================================
# UTILITY
# =============================================================================

#' Return headline counts for the dashboard status bar.
#'
#' @param con  DBI connection.
#' @return Named list: total_transactions (integer), earliest_date (character),
#'         latest_date (character).
get_summary_counts <- function(con) {
  row <- DBI::dbGetQuery(con,
    "SELECT COUNT(*)  AS total_transactions,
            MIN(date) AS earliest_date,
            MAX(date) AS latest_date
     FROM transactions;"
  )
  list(
    total_transactions = as.integer(row$total_transactions),
    earliest_date      = as.character(row$earliest_date),
    latest_date        = as.character(row$latest_date)
  )
}


#' Check whether the connection is alive and all required tables exist.
#'
#' Does not stop() on failure — returns FALSE so callers can handle gracefully.
#'
#' @param con  DBI connection.
#' @return TRUE if healthy, FALSE otherwise.
db_health_check <- function(con) {
  tryCatch({
    if (is.null(con) || !DBI::dbIsValid(con)) return(FALSE)
    tables   <- DBI::dbListTables(con)
    required <- c("transactions", "budgets", "categories", "settings")
    all(required %in% tables)
  }, error = function(e) FALSE)
}


# =============================================================================
# BACKWARD-COMPATIBILITY ALIASES
# The original codebase used db_* prefixed names.  These thin wrappers let
# existing callers (server.R, reporting.R, etc.) continue to work unchanged
# while the new spec-mandated names are the canonical API.
# =============================================================================

db_connect               <- connect_database
db_disconnect            <- disconnect_database
db_initialize            <- initialize_database
db_insert_transaction    <- function(con, inputs) {
  insert_transaction(
    con,
    date           = inputs$date,
    category       = inputs$category,
    description    = inputs$description %||% "",
    amount         = inputs$amount,
    payment_method = inputs$payment_method
  )
}
db_update_transaction    <- function(con, transaction_id, inputs) {
  update_transaction(
    con,
    transaction_id = transaction_id,
    date           = inputs$date,
    category       = inputs$category,
    description    = inputs$description %||% "",
    amount         = inputs$amount,
    payment_method = inputs$payment_method
  )
}
db_delete_transaction    <- delete_transaction
db_get_transactions      <- get_transactions
db_get_transaction_by_id <- get_transaction_by_id
db_upsert_budget         <- function(con, year, month, category, budget_amount) {
  insert_budget(
    con,
    month         = sprintf("%d-%02d", as.integer(year), as.integer(month)),
    category      = category,
    budget_amount = budget_amount
  )
}
db_get_budgets           <- function(con, year = NULL, month = NULL) {
  m <- if (!is.null(year) && !is.null(month)) {
    sprintf("%d-%02d", as.integer(year), as.integer(month))
  } else NULL
  get_budgets(con, month = m)
}
db_get_budget_utilization <- function(con, year, month) {
  get_budget_utilization(
    con,
    month = sprintf("%d-%02d", as.integer(year), as.integer(month))
  )
}
db_get_settings          <- get_settings
db_update_setting        <- update_setting
db_get_categories        <- get_categories
db_get_summary_counts    <- get_summary_counts


# =============================================================================
# NULL COALESCING OPERATOR
# Defined here so database.R has no dependency on ui_helpers.R which also
# defines it.  R 4.4+ has |> but not %||%; we define it defensively.
# =============================================================================
if (!exists("%||%", envir = baseenv())) {
  `%||%` <- function(x, y) if (!is.null(x) && !is.na(x)[1]) x else y
}
