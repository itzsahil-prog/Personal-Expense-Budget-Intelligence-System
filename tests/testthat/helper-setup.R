# =============================================================================
# tests/testthat/helper-setup.R
# Loaded automatically by testthat before every test file.
# Sources all project modules in dependency order and provides test helpers.
# =============================================================================

# ── Resolve project root regardless of working directory ──────────────────────
.proj_root <- function() {
  # Strategy 1: walk up from the current working directory
  path <- normalizePath(getwd(), mustWork = FALSE)
  for (i in seq_len(8L)) {
    if (file.exists(file.path(path, "app.R"))) return(path)
    parent <- dirname(path)
    if (parent == path) break   # reached filesystem root
    path <- parent
  }

  # Strategy 2: walk up from this helper file's own location
  here <- tryCatch(
    normalizePath(sys.frame(0)$ofile, mustWork = FALSE),
    error = function(e) NULL
  )
  if (!is.null(here)) {
    path <- dirname(here)
    for (i in seq_len(8L)) {
      if (file.exists(file.path(path, "app.R"))) return(path)
      parent <- dirname(path)
      if (parent == path) break
      path <- parent
    }
  }

  # Strategy 3: testthat's own path resolution (only valid inside a test run)
  tp <- tryCatch(
    testthat::test_path("."),
    error = function(e) NULL
  )
  if (!is.null(tp)) {
    path <- normalizePath(tp, mustWork = FALSE)
    for (i in seq_len(8L)) {
      if (file.exists(file.path(path, "app.R"))) return(path)
      parent <- dirname(path)
      if (parent == path) break
      path <- parent
    }
  }

  stop(paste0(
    "Cannot locate project root (app.R not found). ",
    "Run tests from the project root directory, e.g.:\n",
    "  setwd('f:/R-Projects/expense-intelligence')\n",
    "  testthat::test_dir('tests/testthat')"
  ))
}

PROJECT_ROOT <- .proj_root()

src <- function(f) source(file.path(PROJECT_ROOT, "R", f), local = FALSE)

src("validation.R")
src("database.R")
src("data_processing.R")
src("analytics.R")
src("budget.R")
src("anomaly_detection.R")
src("prediction.R")
src("reporting.R")


# ── In-memory database factory ─────────────────────────────────────────────────
# Returns a fresh in-memory DBI connection with the full schema initialised.
# Each test that needs a DB should call fresh_db() and disconnect_database()
# in a withr::defer() block (or on.exit).
fresh_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  initialize_database(con)
  con
}


# ── Minimal valid transaction builder ──────────────────────────────────────────
# Returns a named list that passes validate_transaction_input().
valid_txn <- function(date           = "2025-06-15",
                       category       = "Food",
                       description    = "Test lunch",
                       amount         = 250,
                       payment_method = "UPI") {
  list(date = date, category = category, description = description,
       amount = amount, payment_method = payment_method)
}


# ── Minimal valid transaction data.frame row ───────────────────────────────────
valid_txn_df <- function(n = 1, category = "Food", amount = 250,
                          date = "2025-06-15") {
  data.frame(
    date           = rep(date,    n),
    category       = rep(category, n),
    description    = paste0("Txn ", seq_len(n)),
    amount         = rep(as.numeric(amount), n),
    payment_method = rep("UPI", n),
    stringsAsFactors = FALSE
  )
}


# ── Seed a DB with n transactions ───────────────────────────────────────────────
seed_transactions <- function(con, n = 10, category = "Food", amount = 250,
                               date = "2025-06-15") {
  ids <- character(n)
  for (i in seq_len(n)) {
    ids[i] <- insert_transaction(
      con,
      date           = date,
      category       = category,
      description    = paste("Transaction", i),
      amount         = amount + i,
      payment_method = "UPI"
    )
  }
  invisible(ids)
}


# ── Multi-category transaction data.frame ──────────────────────────────────────
multi_category_df <- function() {
  data.frame(
    transaction_id = paste0("txn_", seq_len(30)),
    date           = rep(c("2025-06-01","2025-06-15","2025-06-28"), 10),
    category       = rep(c("Food","Transport","Bills"), 10),
    description    = paste0("Item ", seq_len(30)),
    amount         = c(rep(300, 10), rep(100, 10), rep(500, 10)),
    payment_method = rep(c("UPI","Cash","Card"), 10),
    stringsAsFactors = FALSE
  )
}
