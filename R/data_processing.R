# =============================================================================
# R/data_processing.R
# Aggregation, reshaping, and KPI computation functions.
# All functions accept a plain data.frame (already fetched from SQLite) and
# return a plain data.frame or named list.  No database calls are made here.
#
# Dependencies: dplyr, lubridate, scales, validation.R (CATEGORY_COLOURS)
# =============================================================================

library(dplyr)
library(lubridate)
library(scales)


# =============================================================================
# GUARD HELPER
# Returns TRUE when a transactions data.frame is non-NULL and has rows.
# Used at the top of every function to handle the empty-dataset application
# state cleanly.
# =============================================================================
.has_data <- function(df) {
  !is.null(df) && is.data.frame(df) && nrow(df) > 0
}


# =============================================================================
# DATE COERCION
# Ensure the date column is always a Date object regardless of how SQLite
# returned it (character ISO-8601 is the usual case).
# =============================================================================
.coerce_dates <- function(df) {
  if ("date" %in% names(df)) {
    df$date <- as.Date(df$date)
  }
  df
}


# =============================================================================
# DAILY SUMMARY
# =============================================================================

#' Aggregate transactions to a daily total.
#'
#' @param df  data.frame from db_get_transactions().
#' @return    data.frame with columns: date (Date), total_amount (numeric),
#'            transaction_count (integer).
#'            Returns an empty data.frame with those columns if df has no rows.
compute_daily_summary <- function(df) {
  empty <- data.frame(
    date              = as.Date(character(0)),
    total_amount      = numeric(0),
    transaction_count = integer(0),
    stringsAsFactors  = FALSE
  )

  if (!.has_data(df)) return(empty)

  df <- .coerce_dates(df)

  df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      total_amount      = sum(amount, na.rm = TRUE),
      transaction_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(date)
}


# =============================================================================
# WEEKLY SUMMARY
# =============================================================================

#' Aggregate transactions to a weekly total (ISO weeks: Monday start).
#'
#' @param df  data.frame from db_get_transactions().
#' @return    data.frame with columns: year_week (character "YYYY-Www"),
#'            week_start (Date of Monday), total_amount, transaction_count.
compute_weekly_summary <- function(df) {
  empty <- data.frame(
    year_week         = character(0),
    week_start        = as.Date(character(0)),
    total_amount      = numeric(0),
    transaction_count = integer(0),
    stringsAsFactors  = FALSE
  )

  if (!.has_data(df)) return(empty)

  df <- .coerce_dates(df)

  df %>%
    dplyr::mutate(
      year_week  = paste0(lubridate::isoyear(date), "-W",
                          sprintf("%02d", lubridate::isoweek(date))),
      week_start = lubridate::floor_date(date, unit = "week",
                                         week_start = 1)  # Monday = 1
    ) %>%
    dplyr::group_by(year_week, week_start) %>%
    dplyr::summarise(
      total_amount      = sum(amount, na.rm = TRUE),
      transaction_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(week_start)
}


# =============================================================================
# MONTHLY SUMMARY
# =============================================================================

#' Aggregate transactions to a monthly total.
#'
#' @param df  data.frame from db_get_transactions().
#' @return    data.frame with columns: year_month (character "YYYY-MM"),
#'            month_start (Date), total_amount, transaction_count,
#'            avg_daily_amount.
compute_monthly_summary <- function(df) {
  empty <- data.frame(
    year_month        = character(0),
    month_start       = as.Date(character(0)),
    total_amount      = numeric(0),
    transaction_count = integer(0),
    avg_daily_amount  = numeric(0),
    stringsAsFactors  = FALSE
  )

  if (!.has_data(df)) return(empty)

  df <- .coerce_dates(df)

  df %>%
    dplyr::mutate(
      year_month  = format(date, "%Y-%m"),
      month_start = lubridate::floor_date(date, unit = "month")
    ) %>%
    dplyr::group_by(year_month, month_start) %>%
    dplyr::summarise(
      total_amount      = sum(amount, na.rm = TRUE),
      transaction_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      days_in_month    = lubridate::days_in_month(month_start),
      avg_daily_amount = round(total_amount / days_in_month, 2)
    ) %>%
    dplyr::select(-days_in_month) %>%
    dplyr::arrange(month_start)
}


# =============================================================================
# CATEGORY SUMMARY
# =============================================================================

#' Aggregate transactions by category.
#'
#' @param df  data.frame from db_get_transactions().
#' @return    data.frame with columns: category, total_amount,
#'            transaction_count, pct_of_total, color_hex.
#'            Sorted descending by total_amount.
compute_category_summary <- function(df) {
  empty <- data.frame(
    category          = character(0),
    total_amount      = numeric(0),
    transaction_count = integer(0),
    pct_of_total      = numeric(0),
    color_hex         = character(0),
    stringsAsFactors  = FALSE
  )

  if (!.has_data(df)) return(empty)

  grand_total <- sum(df$amount, na.rm = TRUE)
  if (grand_total == 0) return(empty)

  result <- df %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(
      total_amount      = sum(amount, na.rm = TRUE),
      transaction_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pct_of_total = round(total_amount / grand_total * 100, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(total_amount))

  # Attach colour from the validation.R constant
  result$color_hex <- CATEGORY_COLOURS[result$category]
  result$color_hex[is.na(result$color_hex)] <- "#CCCCCC"

  result
}


# =============================================================================
# PAYMENT METHOD SUMMARY
# =============================================================================

#' Aggregate transactions by payment method.
#'
#' @param df  data.frame from db_get_transactions().
#' @return    data.frame with columns: payment_method, total_amount,
#'            transaction_count, pct_of_total.
compute_payment_summary <- function(df) {
  empty <- data.frame(
    payment_method    = character(0),
    total_amount      = numeric(0),
    transaction_count = integer(0),
    pct_of_total      = numeric(0),
    stringsAsFactors  = FALSE
  )

  if (!.has_data(df)) return(empty)

  grand_total <- sum(df$amount, na.rm = TRUE)
  if (grand_total == 0) return(empty)

  df %>%
    dplyr::group_by(payment_method) %>%
    dplyr::summarise(
      total_amount      = sum(amount, na.rm = TRUE),
      transaction_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pct_of_total = round(total_amount / grand_total * 100, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(total_amount))
}


# =============================================================================
# KPI STATISTICS
# Used to populate the dashboard KPI cards.
# =============================================================================

#' Compute all dashboard KPI values from a transactions data.frame.
#'
#' @param df              data.frame from db_get_transactions() — typically the
#'                        full unfiltered history so totals are correct.
#' @param currency_symbol Character symbol, e.g. "\u20b9".
#'
#' @return Named list:
#'   total_spending           — numeric: sum of all transactions
#'   current_month_spending   — numeric: sum for the current calendar month
#'   today_spending           — numeric: sum for today
#'   avg_daily_spending       — numeric: mean daily total (days with at least
#'                               one transaction)
#'   highest_category         — character: category with highest total spend
#'   highest_category_amount  — numeric: that category's total
#'   transaction_count        — integer: total number of transactions
#'   current_month_label      — character: e.g. "September 2026"
#'   data_available           — logical: FALSE when df has no rows
compute_kpi_stats <- function(df, currency_symbol = "\u20b9") {

  today      <- Sys.Date()
  cur_year   <- lubridate::year(today)
  cur_month  <- lubridate::month(today)

  # Empty-dataset safe defaults
  if (!.has_data(df)) {
    return(list(
      total_spending          = 0,
      current_month_spending  = 0,
      today_spending          = 0,
      avg_daily_spending      = 0,
      highest_category        = "—",
      highest_category_amount = 0,
      transaction_count       = 0L,
      current_month_label     = format(today, "%B %Y"),
      data_available          = FALSE
    ))
  }

  df <- .coerce_dates(df)

  total_spending <- sum(df$amount, na.rm = TRUE)

  current_month_df <- df %>%
    dplyr::filter(
      lubridate::year(date)  == cur_year,
      lubridate::month(date) == cur_month
    )
  current_month_spending <- sum(current_month_df$amount, na.rm = TRUE)

  today_spending <- sum(
    df$amount[df$date == today],
    na.rm = TRUE
  )

  # Average daily spending: total / number of distinct days with transactions
  distinct_days <- dplyr::n_distinct(df$date)
  avg_daily_spending <- if (distinct_days > 0) {
    round(total_spending / distinct_days, 2)
  } else 0

  # Highest spending category
  cat_totals <- df %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total))

  highest_category <- if (nrow(cat_totals) > 0) cat_totals$category[1] else "—"
  highest_category_amount <- if (nrow(cat_totals) > 0) cat_totals$total[1] else 0

  list(
    total_spending          = total_spending,
    current_month_spending  = current_month_spending,
    today_spending          = today_spending,
    avg_daily_spending      = avg_daily_spending,
    highest_category        = highest_category,
    highest_category_amount = highest_category_amount,
    transaction_count       = nrow(df),
    current_month_label     = format(today, "%B %Y"),
    data_available          = TRUE
  )
}


# =============================================================================
# BUDGET STATUS
# Merges budget rows with actual spending to derive status flags.
# =============================================================================

#' Compute budget status for a given year/month, combining budget targets with
#' actual spending from the transactions data.frame.
#'
#' This function works entirely in-memory; it is given the two data.frames
#' already fetched by the reactive layer.
#'
#' @param budget_df       data.frame from db_get_budgets(year, month) with
#'                        columns: category, budget_amount.
#' @param transactions_df data.frame from db_get_transactions() — should cover
#'                        at minimum the same year/month.
#' @param year            Integer year (used to filter transactions_df).
#' @param month           Integer month (used to filter transactions_df).
#'
#' @return data.frame with columns:
#'   category, budget_amount, budget_used, budget_remaining,
#'   utilization_pct, status ("ok" | "warning" | "over_budget")
#'   Sorted descending by utilization_pct.
compute_budget_status <- function(budget_df, transactions_df, year, month) {
  empty <- data.frame(
    category        = character(0),
    budget_amount   = numeric(0),
    budget_used     = numeric(0),
    budget_remaining = numeric(0),
    utilization_pct = numeric(0),
    status          = character(0),
    stringsAsFactors = FALSE
  )

  if (is.null(budget_df) || nrow(budget_df) == 0) return(empty)

  # Filter transactions to the requested month
  if (.has_data(transactions_df)) {
    transactions_df <- .coerce_dates(transactions_df)
    month_txns <- transactions_df %>%
      dplyr::filter(
        lubridate::year(date)  == as.integer(year),
        lubridate::month(date) == as.integer(month)
      )
  } else {
    month_txns <- data.frame(category = character(0), amount = numeric(0))
  }

  # Actual spending per category for this month
  actual <- month_txns %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(budget_used = sum(amount, na.rm = TRUE), .groups = "drop")

  # Left-join: every budgeted category appears even with zero spending
  result <- budget_df %>%
    dplyr::select(category, budget_amount) %>%
    dplyr::left_join(actual, by = "category") %>%
    dplyr::mutate(
      budget_used      = dplyr::coalesce(budget_used, 0),
      budget_remaining = budget_amount - budget_used,
      utilization_pct  = dplyr::case_when(
        budget_amount > 0 ~ round(budget_used / budget_amount * 100, 1),
        TRUE              ~ 0
      ),
      status = dplyr::case_when(
        utilization_pct >= 100 ~ "over_budget",
        utilization_pct >= 80  ~ "warning",
        TRUE                   ~ "ok"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(utilization_pct))

  result
}


# =============================================================================
# SPENDING STATISTICS (for analytics tab)
# =============================================================================

#' Compute descriptive spending statistics over a filtered transactions set.
#'
#' @param df  data.frame from db_get_transactions().
#' @return    Named list:
#'   highest_day          — Date of highest single-day total
#'   highest_day_amount   — numeric total on that day
#'   highest_category     — category name
#'   highest_cat_amount   — numeric total for that category
#'   avg_daily            — mean daily total
#'   median_transaction   — median single-transaction amount
#'   total                — sum of all amounts
#'   transaction_count    — integer
compute_spending_stats <- function(df) {
  empty <- list(
    highest_day        = NA,
    highest_day_amount = 0,
    highest_category   = "—",
    highest_cat_amount = 0,
    avg_daily          = 0,
    median_transaction = 0,
    total              = 0,
    transaction_count  = 0L
  )

  if (!.has_data(df)) return(empty)

  df <- .coerce_dates(df)

  daily <- compute_daily_summary(df)
  cat   <- compute_category_summary(df)

  highest_day_row <- daily[which.max(daily$total_amount), , drop = FALSE]
  highest_cat_row <- cat[1, , drop = FALSE]

  list(
    highest_day        = if (nrow(highest_day_row) > 0) highest_day_row$date[1]         else NA,
    highest_day_amount = if (nrow(highest_day_row) > 0) highest_day_row$total_amount[1] else 0,
    highest_category   = if (nrow(highest_cat_row) > 0) highest_cat_row$category[1]     else "—",
    highest_cat_amount = if (nrow(highest_cat_row) > 0) highest_cat_row$total_amount[1] else 0,
    avg_daily          = round(mean(daily$total_amount, na.rm = TRUE), 2),
    median_transaction = round(stats::median(df$amount, na.rm = TRUE), 2),
    total              = sum(df$amount, na.rm = TRUE),
    transaction_count  = nrow(df)
  )
}


# =============================================================================
# CURRENCY FORMATTING HELPER
# Shared by all modules that need to display monetary values as strings.
# =============================================================================

#' Format a numeric amount as a currency string.
#'
#' @param amount          Numeric value.
#' @param currency_symbol Character symbol prepended to the value.
#' @param digits          Decimal places. Default 2.
#' @return                Character string, e.g. "\u20b9 1,234.50".
format_currency <- function(amount, currency_symbol = "\u20b9", digits = 2) {
  if (is.na(amount) || !is.numeric(amount)) return(paste0(currency_symbol, " 0.00"))
  paste0(
    currency_symbol, " ",
    formatC(amount, format = "f", digits = digits, big.mark = ",")
  )
}

# =============================================================================
# IMPORT PIPELINE  (spec-mandated names)
#
# These two functions are the sole entry points for all file-based import
# workflows.  They own the complete pipeline:
#
#   read file  →  parse  →  validate  →  clean  →  normalise  →  return result
#
# Both return the same structured list so server.R can handle CSV and Excel
# imports with a single code path.
#
# Return schema (both functions)
# --------------------------------
#   $status          chr   "ok" | "partial" | "error"
#   $source          chr   "csv" | "excel"
#   $file_path       chr   the path that was read
#   $clean_df        data.frame  normalised rows ready for
#                               db_bulk_insert_transactions()
#   $discarded_df    data.frame  invalid rows with a `validation_errors`
#                               column — never NULL, may be zero-row
#   $clean_count     int
#   $discarded_count int
#   $total_rows      int
#   $has_clean       lgl
#   $has_discarded   lgl
#   $is_partial      lgl   TRUE when some rows clean AND some discarded
#   $summary_message chr   human-readable one-liner
#   $detail_messages chr   one message per field-level failure (may be
#                          character(0))
#   $parse_error     chr   parse-stage error message, or NA when parse
#                          succeeded
#
# Dependency notes
# ----------------
# Calls validate_transaction_data() and clean_transaction_data() from
# validation.R, so validation.R MUST be sourced before data_processing.R.
# readr is already loaded at the top of this file.
# readxl is loaded inside import_excel() with requireNamespace() so the
# package is only required when Excel import is actually used.
# =============================================================================

library(readr)


# Internal: build the standard "error" result used when reading or parsing
# the file itself fails (before validation even runs).
.import_error_result <- function(source, file_path, parse_error_msg) {
  empty_df <- data.frame(stringsAsFactors = FALSE)
  list(
    status          = "error",
    source          = source,
    file_path       = as.character(file_path),
    clean_df        = data.frame(
      transaction_id = character(0), date = character(0),
      category = character(0), description = character(0),
      amount = numeric(0), payment_method = character(0),
      created_at = character(0), updated_at = character(0),
      stringsAsFactors = FALSE
    ),
    discarded_df    = empty_df,
    clean_count     = 0L,
    discarded_count = 0L,
    total_rows      = 0L,
    has_clean       = FALSE,
    has_discarded   = FALSE,
    is_partial      = FALSE,
    summary_message = sprintf("Could not read file: %s", parse_error_msg),
    detail_messages = parse_error_msg,
    parse_error     = parse_error_msg
  )
}

# Internal: promote a clean_transaction_data() result into the full return
# schema, adding the fields that are common to both import functions.
.wrap_clean_result <- function(clean_result, source, file_path) {
  status <- if (!clean_result$has_clean && clean_result$has_discarded) {
    "error"
  } else if (clean_result$is_partial) {
    "partial"
  } else {
    "ok"
  }

  c(
    list(
      status      = status,
      source      = source,
      file_path   = as.character(file_path),
      parse_error = NA_character_
    ),
    clean_result
  )
}


# -----------------------------------------------------------------------------
# import_csv()
# -----------------------------------------------------------------------------

#' Read and import a CSV file of transactions.
#'
#' Reads the file with readr::read_csv(), skipping any lines that begin with
#' '#' (the metadata comment written by export_transactions_csv()), then
#' passes the parsed frame through the full validation and cleaning pipeline.
#'
#' Column names in the CSV are case-insensitive and may contain spaces;
#' normalisation is handled by clean_transaction_data().
#'
#' Required columns (in any order, any case):
#'   date, category, description, amount, payment_method
#'
#' @param file_path  Character path to a .csv file.  Typically
#'                   `input$import_file$datapath` from Shiny fileInput().
#' @param locale     readr::locale() object controlling encoding and decimal
#'                   mark.  Default: UTF-8, period decimal.
#'
#' @return Named list — see "Return schema" in the section header above.
#'         Never throws; parse failures are captured into $parse_error and
#'         $status == "error".
import_csv <- function(file_path,
                       locale = readr::locale(encoding = "UTF-8",
                                              decimal_mark = ".")) {

  # ---- Guard -----------------------------------------------------------------
  if (is.null(file_path) || !nzchar(trimws(file_path))) {
    return(.import_error_result("csv", file_path %||% "", "No file path supplied."))
  }
  if (!file.exists(file_path)) {
    return(.import_error_result(
      "csv", file_path,
      sprintf("File not found: %s", file_path)
    ))
  }

  # ---- Parse -----------------------------------------------------------------
  raw_df <- tryCatch({
    # comment = "#" silently drops the metadata header line written by
    # export_transactions_csv(), but keeps all data rows intact.
    readr::read_csv(
      file          = file_path,
      locale        = locale,
      comment       = "#",
      col_types     = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      trim_ws       = TRUE,
      progress      = FALSE
    )
  }, error = function(e) {
    conditionMessage(e)       # return the error string, not a data.frame
  }, warning = function(w) {
    # readr promotes some parse failures to warnings; treat them as errors
    conditionMessage(w)
  })

  if (is.character(raw_df)) {
    # parse produced an error string, not a data.frame
    return(.import_error_result("csv", file_path, raw_df))
  }

  if (!is.data.frame(raw_df) || nrow(raw_df) == 0L) {
    return(.import_error_result(
      "csv", file_path,
      "File parsed but contained no data rows."
    ))
  }

  # ---- Validate + clean ------------------------------------------------------
  clean_result <- clean_transaction_data(as.data.frame(raw_df), source = "csv")

  .wrap_clean_result(clean_result, source = "csv", file_path = file_path)
}


# -----------------------------------------------------------------------------
# import_excel()
# -----------------------------------------------------------------------------

#' Read and import an Excel file (.xlsx or .xls) of transactions.
#'
#' Reads the first sheet of the workbook with readxl::read_excel(), coerces
#' all columns to character (so type inference does not corrupt dates or
#' amounts), then passes the frame through the full validation and cleaning
#' pipeline.
#'
#' Column names are case-insensitive and may contain spaces.
#'
#' Required columns (in any order, any case):
#'   date, category, description, amount, payment_method
#'
#' @param file_path  Character path to a .xlsx or .xls file.  Typically
#'                   `input$import_file$datapath` from Shiny fileInput().
#' @param sheet      Sheet name or index to read.  Default: 1 (first sheet).
#'
#' @return Named list — see "Return schema" in the section header above.
#'         Never throws; parse failures are captured into $parse_error and
#'         $status == "error".
import_excel <- function(file_path, sheet = 1L) {

  # ---- Guard -----------------------------------------------------------------
  if (is.null(file_path) || !nzchar(trimws(file_path))) {
    return(.import_error_result("excel", file_path %||% "", "No file path supplied."))
  }
  if (!file.exists(file_path)) {
    return(.import_error_result(
      "excel", file_path,
      sprintf("File not found: %s", file_path)
    ))
  }

  # Check extension — give a clear message rather than a cryptic readxl error
  ext <- tolower(tools::file_ext(file_path))
  if (!ext %in% c("xlsx", "xls")) {
    return(.import_error_result(
      "excel", file_path,
      sprintf(
        "Expected an Excel file (.xlsx or .xls) but received '.%s'. Use import_csv() for CSV files.",
        ext
      )
    ))
  }

  # ---- Ensure readxl is available --------------------------------------------
  if (!requireNamespace("readxl", quietly = TRUE)) {
    return(.import_error_result(
      "excel", file_path,
      "Package 'readxl' is required for Excel import but is not installed. Run install.packages('readxl')."
    ))
  }

  # ---- Parse -----------------------------------------------------------------
  raw_df <- tryCatch({
    readxl::read_excel(
      path      = file_path,
      sheet     = sheet,
      col_types = "text",    # keep everything as character; clean_transaction_data coerces
      trim_ws   = TRUE,
      .name_repair = "minimal"
    )
  }, error = function(e) {
    conditionMessage(e)
  }, warning = function(w) {
    conditionMessage(w)
  })

  if (is.character(raw_df)) {
    return(.import_error_result("excel", file_path, raw_df))
  }

  if (!is.data.frame(raw_df) || nrow(raw_df) == 0L) {
    return(.import_error_result(
      "excel", file_path,
      sprintf(
        "Sheet %s parsed but contained no data rows.",
        if (is.numeric(sheet)) as.character(sheet) else sprintf("'%s'", sheet)
      )
    ))
  }

  # ---- Remove completely empty rows (common in Excel exports) ----------------
  # A row is considered empty when every cell is NA or blank after trimming.
  non_empty_mask <- apply(raw_df, 1, function(row) {
    any(!is.na(row) & nzchar(trimws(as.character(row))))
  })
  raw_df <- raw_df[non_empty_mask, , drop = FALSE]

  if (nrow(raw_df) == 0L) {
    return(.import_error_result(
      "excel", file_path,
      "Sheet contained only empty rows after stripping blank lines."
    ))
  }

  # ---- Validate + clean ------------------------------------------------------
  clean_result <- clean_transaction_data(as.data.frame(raw_df), source = "excel")

  .wrap_clean_result(clean_result, source = "excel", file_path = file_path)
}


# Null-coalescing operator — defined here defensively in case data_processing.R
# is sourced before validation.R or database.R in tests.
if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(x, y) if (!is.null(x)) x else y
}
