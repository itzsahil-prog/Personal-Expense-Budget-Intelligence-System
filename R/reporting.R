# =============================================================================
# R/reporting.R
# Export and report-generation functions.
#
# All functions write to a temp file and return the path.  The Shiny
# downloadHandler passes that path to the browser.  Temp files are cleaned up
# by R's session garbage collector; this module never deletes them manually to
# avoid race conditions with in-flight downloads.
#
# Public API
# ----------
# Point exports (write one focused file from already-fetched data)
#   export_transactions_csv()     — transactions → CSV
#   export_transactions_excel()   — transactions → Excel (.xlsx)
#   export_analytics_csv()        — category + payment summaries → CSV
#   export_anomaly_csv()          — flagged transactions → CSV
#
# Monthly report
#   generate_report_data()        — assembles all sections from current DB state
#   generate_monthly_report()     — turns assembled data into a multi-sheet Excel
#
# Filename helpers (used in Shiny downloadHandler(filename = ...))
#   download_filename_transactions()
#   download_filename_monthly_report()
#   download_filename_analytics()
#   download_filename_anomalies()
#
# Cross-module dependencies (must be sourced before this file)
#   validation.R          — VALID_CATEGORIES, generate_transaction_id
#   database.R            — get_transactions, get_budgets
#   analytics.R           — calculate_category_spending,
#                           calculate_payment_method_spending,
#                           calculate_monthly_spending
#   budget.R              — calculate_budget_status,
#                           BUDGET_STATUS_UNDER/NEAR/OVER
#   anomaly_detection.R   — get_anomaly_summary, detect_transaction_anomalies
#   prediction.R          — run_forecast, calculate_forecast_summary
#
# Column name conventions in exported files
# ------------------------------------------
# All human-readable ("Title Case") — never snake_case.
# Amounts are exported as raw numerics so users can re-import without parsing
# formatted strings.
#
# Dependencies: dplyr, lubridate, readr, writexl
# =============================================================================

library(dplyr)
library(lubridate)
library(readr)
library(writexl)


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

# Format a numeric amount as a plain character suitable for a summary cell.
# Uses commas as thousands separators, 2 decimal places.  No currency symbol
# in the value itself — callers add the symbol at the cell level where needed.
.fmt_amt <- function(x, symbol = "") {
  if (is.na(x) || !is.finite(x)) return("—")
  formatted <- formatC(x, format = "f", digits = 2, big.mark = ",")
  if (nzchar(symbol)) paste0(symbol, "\u00a0", formatted) else formatted
}

# Standardise a transactions data.frame into export-ready display columns.
# Preserves column order; amounts stay numeric.
.prepare_transactions_for_export <- function(df) {
  empty <- data.frame(
    `Transaction ID` = character(0),
    Date             = character(0),
    Category         = character(0),
    Description      = character(0),
    Amount           = numeric(0),
    `Payment Method` = character(0),
    check.names      = FALSE,
    stringsAsFactors = FALSE
  )

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(empty)

  df %>%
    dplyr::mutate(date = as.Date(as.character(date))) %>%
    dplyr::arrange(dplyr::desc(date)) %>%
    dplyr::transmute(
      `Transaction ID` = as.character(transaction_id),
      Date             = format(date, "%Y-%m-%d"),
      Category         = as.character(category),
      Description      = as.character(description),
      Amount           = round(as.numeric(amount), 2),
      `Payment Method` = as.character(payment_method)
    )
}

# Generate a safe, timestamped filename for downloads.
.export_filename <- function(prefix, ext) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  sprintf("%s_%s.%s", prefix, ts, ext)
}

# Write lines to a temp file and return the path.
.write_lines_tmp <- function(lines, pattern, ext) {
  path <- tempfile(pattern = pattern, fileext = paste0(".", ext))
  writeLines(lines, con = path)
  path
}

# Metadata header written as the first line of every CSV export.
.csv_header <- function(label, extra = NULL) {
  extras <- if (!is.null(extra)) paste0(" | ", extra) else ""
  sprintf(
    "# Expense Intelligence — %s | Generated: %s%s",
    label,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    extras
  )
}

# Convert a data.frame to CSV lines using readr (returns character vector).
.df_to_csv_lines <- function(df) {
  utils::capture.output(readr::write_csv(df, file = stdout(), na = ""))
}


# =============================================================================
# 1. EXPORT TRANSACTIONS — CSV
# =============================================================================

#' Export a filtered transactions data.frame to CSV.
#'
#' @param df              data.frame from get_transactions() — may be
#'                        pre-filtered by the caller.
#' @param currency_symbol Character. Written into the header comment only;
#'                        amounts in the data are plain numerics.
#' @param date_range      Character label for the filter period, e.g.
#'                        "2026-01 to 2026-09".  NULL → "all time".
#'
#' @return Character path to a temp CSV file, or NULL when df is empty.
export_transactions_csv <- function(df,
                                     currency_symbol = "\u20b9",
                                     date_range      = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(NULL)

  export_df <- .prepare_transactions_for_export(df)

  period <- if (!is.null(date_range) && nzchar(date_range)) date_range
            else "all time"

  lines <- c(
    .csv_header(
      "Transaction Export",
      sprintf(
        "Currency: %s | Period: %s | Rows: %d",
        currency_symbol, period, nrow(export_df)
      )
    ),
    .df_to_csv_lines(export_df)
  )

  .write_lines_tmp(lines, "expense_transactions_", "csv")
}


# =============================================================================
# 2. EXPORT TRANSACTIONS — EXCEL
# =============================================================================

#' Export a filtered transactions data.frame to an Excel (.xlsx) file.
#'
#' @param df  data.frame from get_transactions() — may be pre-filtered.
#' @return Character path to a temp .xlsx file, or NULL when df is empty.
export_transactions_excel <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(NULL)

  export_df <- .prepare_transactions_for_export(df)

  path <- tempfile(pattern = "expense_transactions_", fileext = ".xlsx")
  writexl::write_xlsx(list(Transactions = export_df), path = path)
  path
}


# =============================================================================
# 3. EXPORT ANALYTICS SUMMARY — CSV
# =============================================================================

#' Export category and payment-method spending summaries to a single CSV.
#'
#' The file has two labelled sections separated by a blank line.  Each section
#' is a self-contained CSV block so it can be sliced out by downstream tools.
#'
#' @param transactions_df  data.frame from get_transactions(). The function
#'                         computes the summaries itself so the caller does not
#'                         need to pre-aggregate.
#' @param date_range       Character period label (used in header comment).
#'
#' @return Character path to a temp CSV file, or NULL when no data.
export_analytics_csv <- function(transactions_df, date_range = "all time") {
  if (is.null(transactions_df) || !is.data.frame(transactions_df) ||
      nrow(transactions_df) == 0L) {
    return(NULL)
  }

  # Compute summaries using analytics.R functions
  cat_df <- tryCatch(
    calculate_category_spending(transactions_df),
    error = function(e) NULL
  )
  pay_df <- tryCatch(
    calculate_payment_method_spending(transactions_df),
    error = function(e) NULL
  )

  if ((is.null(cat_df) || nrow(cat_df) == 0L) &&
      (is.null(pay_df) || nrow(pay_df) == 0L)) {
    return(NULL)
  }

  lines <- .csv_header("Analytics Export", sprintf("Period: %s", date_range))

  if (!is.null(cat_df) && nrow(cat_df) > 0L) {
    cat_export <- cat_df %>%
      dplyr::transmute(
        Category              = category,
        `Total Amount`        = round(total_amount, 2),
        `Transaction Count`   = as.integer(transaction_count),
        `% of Total`          = round(pct_of_total, 1)
      )
    lines <- c(lines, "", "## Spending by Category", .df_to_csv_lines(cat_export))
  }

  if (!is.null(pay_df) && nrow(pay_df) > 0L) {
    pay_export <- pay_df %>%
      dplyr::transmute(
        `Payment Method`      = payment_method,
        `Total Amount`        = round(total_amount, 2),
        `Transaction Count`   = as.integer(transaction_count),
        `% of Total`          = round(pct_of_total, 1)
      )
    lines <- c(lines, "", "## Spending by Payment Method", .df_to_csv_lines(pay_export))
  }

  .write_lines_tmp(lines, "expense_analytics_", "csv")
}


# =============================================================================
# 4. EXPORT ANOMALY RESULTS — CSV
# =============================================================================

#' Export anomaly detection results to CSV.
#'
#' Column names align with the output schema of detect_transaction_anomalies():
#' excess_amount and scope (not the old anomaly_score / detection_scope columns).
#'
#' @param flagged_df      data.frame from detect_transaction_anomalies() or
#'                        get_anomaly_summary()$flagged_df.
#' @param iqr_multiplier  Numeric: the multiplier that was applied (for header).
#' @param currency_symbol Character currency symbol.
#'
#' @return Character path to a temp CSV file, or NULL when flagged_df is empty.
export_anomaly_csv <- function(flagged_df,
                                iqr_multiplier  = IQR_DEFAULT_MULTIPLIER,
                                currency_symbol = "\u20b9") {
  if (is.null(flagged_df) || !is.data.frame(flagged_df) ||
      nrow(flagged_df) == 0L) {
    return(NULL)
  }

  export_df <- flagged_df %>%
    dplyr::mutate(date = as.Date(as.character(date))) %>%
    dplyr::arrange(dplyr::desc(excess_amount)) %>%
    dplyr::transmute(
      `Transaction ID`   = as.character(transaction_id),
      Date               = format(date, "%Y-%m-%d"),
      Category           = as.character(category),
      Description        = as.character(description),
      Amount             = round(as.numeric(amount), 2),
      `Upper Bound (Q3 + k*IQR)` = round(as.numeric(upper_bound), 2),
      `Excess Amount`    = round(as.numeric(excess_amount), 2),
      `IQR Multiplier`   = as.numeric(threshold),
      `Q1`               = round(as.numeric(Q1), 2),
      `Q3`               = round(as.numeric(Q3), 2),
      `IQR`              = round(as.numeric(IQR), 2),
      `Distribution Used` = dplyr::if_else(
        as.character(scope) == "category",
        "Per-category", "Global fallback"
      ),
      Reason             = as.character(reason)
    )

  lines <- c(
    .csv_header(
      "Anomaly Detection Export",
      sprintf(
        "IQR Multiplier: %.1f | Flagged Rows: %d",
        iqr_multiplier, nrow(export_df)
      )
    ),
    .df_to_csv_lines(export_df)
  )

  .write_lines_tmp(lines, "expense_anomalies_", "csv")
}


# =============================================================================
# 5. ASSEMBLE REPORT DATA — generate_report_data()
# =============================================================================

#' Assemble all data needed for a monthly report from the current DB state.
#'
#' This function is the single point of truth for what goes into the report.
#' It queries or receives all required data.frames and runs the cross-module
#' computations so that generate_monthly_report() receives a clean named list
#' and does nothing but format.
#'
#' The report is reproducible: given the same DB state it produces identical
#' output.  No random numbers, no system state beyond Sys.time() in the header.
#'
#' @param transactions_df  data.frame from get_transactions() for the full DB
#'                         (not pre-filtered — filtering is done here).
#' @param budget_df        data.frame from get_budgets(con, month = "YYYY-MM")
#'                         for the target month, or NULL.
#' @param month            Character "YYYY-MM", e.g. "2026-09".
#' @param iqr_multiplier   Numeric IQR multiplier for anomaly detection.
#'                         Default: IQR_DEFAULT_MULTIPLIER from anomaly_detection.R.
#' @param forecast_result  Named list from run_forecast() or NULL.  When NULL,
#'                         the forecast section of the report is marked
#'                         "unavailable" rather than raising an error.
#' @param currency_symbol  Character. Default "\u20b9".
#'
#' @return Named list with all sections populated:
#'   $month              chr   "YYYY-MM"
#'   $month_label        chr   "Month YYYY"
#'   $generated_at       chr   timestamp
#'   $currency_symbol    chr
#'   $month_txns         data.frame  — transactions in the target month
#'   $total_spending     num
#'   $transaction_count  int
#'   $avg_daily_spending num
#'   $category_summary   data.frame — from calculate_category_spending()
#'   $payment_summary    data.frame — from calculate_payment_method_spending()
#'   $monthly_trend      data.frame — from calculate_monthly_spending()
#'   $budget_status      data.frame — from calculate_budget_status() or empty
#'   $budget_available   lgl   — TRUE when budget_df had rows
#'   $anomaly_summary    named list — from get_anomaly_summary()
#'   $forecast_summary   named list — from calculate_forecast_summary() or empty
#'   $forecast_available lgl   — TRUE when forecast ran successfully
generate_report_data <- function(transactions_df,
                                  budget_df       = NULL,
                                  month,
                                  iqr_multiplier  = IQR_DEFAULT_MULTIPLIER,
                                  forecast_result = NULL,
                                  currency_symbol = "\u20b9") {

  # ---- Validate month --------------------------------------------------------
  month <- tryCatch({
    m <- trimws(as.character(month))
    if (!grepl("^\\d{4}-\\d{2}$", m)) stop("bad format")
    m
  }, error = function(e) {
    stop(sprintf(
      "generate_report_data: 'month' must be 'YYYY-MM'. Got: '%s'.", month
    ))
  })

  month_label <- format(
    as.Date(paste0(month, "-01")),
    "%B %Y"
  )
  generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # ---- Filter to the target month -------------------------------------------
  month_txns <- if (!is.null(transactions_df) &&
                     is.data.frame(transactions_df) &&
                     nrow(transactions_df) > 0L &&
                     all(c("date", "amount") %in% names(transactions_df))) {

    transactions_df %>%
      dplyr::mutate(date = as.Date(as.character(date))) %>%
      dplyr::filter(format(date, "%Y-%m") == month) %>%
      dplyr::filter(!is.na(amount), is.finite(as.numeric(amount)), amount > 0)

  } else {
    # Typed empty frame matching get_transactions() output schema
    data.frame(
      transaction_id = character(0), date = as.Date(character(0)),
      category       = character(0), description = character(0),
      amount         = numeric(0),   payment_method = character(0),
      stringsAsFactors = FALSE
    )
  }

  # ---- Top-level KPIs -------------------------------------------------------
  total_spending    <- sum(month_txns$amount, na.rm = TRUE)
  transaction_count <- nrow(month_txns)

  avg_daily_spending <- if (transaction_count > 0L) {
    n_days <- dplyr::n_distinct(month_txns$date)
    if (n_days > 0L) round(total_spending / n_days, 2) else 0
  } else 0

  # ---- Analytics summaries ---------------------------------------------------
  category_summary <- tryCatch(
    calculate_category_spending(month_txns),
    error = function(e) {
      data.frame(
        category = character(0), total_amount = numeric(0),
        transaction_count = integer(0), pct_of_total = numeric(0),
        color_hex = character(0), stringsAsFactors = FALSE
      )
    }
  )

  payment_summary <- tryCatch(
    calculate_payment_method_spending(month_txns),
    error = function(e) {
      data.frame(
        payment_method = character(0), total_amount = numeric(0),
        transaction_count = integer(0), pct_of_total = numeric(0),
        stringsAsFactors = FALSE
      )
    }
  )

  # Monthly trend uses the full transaction history (not just this month)
  monthly_trend <- tryCatch(
    calculate_monthly_spending(transactions_df),
    error = function(e) {
      data.frame(
        year_month = character(0), month_start = as.Date(character(0)),
        total_amount = numeric(0), transaction_count = integer(0),
        avg_daily_amount = numeric(0), stringsAsFactors = FALSE
      )
    }
  )

  # ---- Budget status ---------------------------------------------------------
  budget_available <- !is.null(budget_df) &&
                      is.data.frame(budget_df) &&
                      nrow(budget_df) > 0L

  budget_status <- if (budget_available) {
    tryCatch(
      calculate_budget_status(
        budget_df       = budget_df,
        transactions_df = month_txns,
        month           = month
      ),
      error = function(e) {
        warning(sprintf("generate_report_data: budget status failed: %s",
                        conditionMessage(e)))
        data.frame(
          category = character(0), budget = numeric(0),
          actual_spending = numeric(0), remaining = numeric(0),
          utilization_pct = numeric(0), utilization_ratio = numeric(0),
          status = character(0), stringsAsFactors = FALSE
        )
      }
    )
  } else {
    data.frame(
      category = character(0), budget = numeric(0),
      actual_spending = numeric(0), remaining = numeric(0),
      utilization_pct = numeric(0), utilization_ratio = numeric(0),
      status = character(0), stringsAsFactors = FALSE
    )
  }

  # ---- Anomaly detection (run on month transactions) ------------------------
  anomaly_summary <- tryCatch(
    get_anomaly_summary(month_txns, iqr_multiplier = iqr_multiplier),
    error = function(e) {
      warning(sprintf("generate_report_data: anomaly detection failed: %s",
                      conditionMessage(e)))
      list(
        flagged_count   = 0L,
        total_checked   = 0L,
        flagged_df      = data.frame(stringsAsFactors = FALSE),
        status          = "error",
        status_message  = conditionMessage(e)
      )
    }
  )

  # ---- Forecast summary ------------------------------------------------------
  forecast_available <- !is.null(forecast_result) &&
                        is.list(forecast_result) &&
                        identical(forecast_result$status, "ok")

  forecast_summary <- if (forecast_available) {
    tryCatch(
      calculate_forecast_summary(forecast_result$forecast_df),
      error = function(e) {
        list(has_data = FALSE,
             caveat   = "Forecast summary unavailable.")
      }
    )
  } else {
    fc_message <- if (!is.null(forecast_result) &&
                       !is.null(forecast_result$message)) {
      forecast_result$message
    } else {
      paste0(
        "Forecasting requires at least ", FORECAST_MIN_DAYS,
        " days of transaction history."
      )
    }
    list(
      has_data = FALSE,
      caveat   = fc_message
    )
  }

  list(
    month              = month,
    month_label        = month_label,
    generated_at       = generated_at,
    currency_symbol    = currency_symbol,
    month_txns         = month_txns,
    total_spending     = total_spending,
    transaction_count  = as.integer(transaction_count),
    avg_daily_spending = avg_daily_spending,
    category_summary   = category_summary,
    payment_summary    = payment_summary,
    monthly_trend      = monthly_trend,
    budget_status      = budget_status,
    budget_available   = budget_available,
    anomaly_summary    = anomaly_summary,
    forecast_summary   = forecast_summary,
    forecast_available = forecast_available
  )
}


# =============================================================================
# 6. MONTHLY REPORT — generate_monthly_report()
# =============================================================================

#' Generate a multi-sheet monthly Excel report from assembled report data.
#'
#' Accepts either a named list from generate_report_data() (recommended) or
#' raw parameters for backward compatibility.
#'
#' Sheets:
#'   1. Summary          — KPI table for the month
#'   2. Transactions     — all transactions in the month
#'   3. Category         — category spending breakdown
#'   4. Payment Methods  — payment method breakdown
#'   5. Budget Status    — budget vs actual (omitted when no budgets set)
#'   6. Anomalies        — flagged transactions (omitted when none flagged)
#'   7. Forecast         — forecast summary statistics (omitted when unavailable)
#'   8. Monthly Trend    — month-over-month totals for the full history
#'
#' @param report_data   Named list from generate_report_data().  When supplied,
#'                      all other parameters are ignored.
#' @param year          Integer year (used only when report_data is NULL).
#' @param month_num     Integer month 1-12 (used only when report_data is NULL).
#' @param transactions_df  Passed to generate_report_data() when report_data NULL.
#' @param budget_df        Passed to generate_report_data() when report_data NULL.
#' @param currency_symbol  Character. Default "\u20b9".
#'
#' @return Character path to a temp .xlsx file.
generate_monthly_report <- function(report_data     = NULL,
                                     year            = NULL,
                                     month_num       = NULL,
                                     transactions_df = NULL,
                                     budget_df       = NULL,
                                     currency_symbol = "\u20b9") {

  # ---- Accept either pre-assembled or raw parameters -------------------------
  if (is.null(report_data)) {
    if (is.null(year) || is.null(month_num)) {
      stop("generate_monthly_report: supply either report_data or year + month_num.")
    }
    month_str <- sprintf("%d-%02d", as.integer(year), as.integer(month_num))
    report_data <- generate_report_data(
      transactions_df = transactions_df,
      budget_df       = budget_df,
      month           = month_str,
      currency_symbol = currency_symbol
    )
  }

  sym <- report_data$currency_symbol

  # ---- Sheet 1: Summary ------------------------------------------------------
  # Determine highest-spending category for the summary row
  top_cat <- if (nrow(report_data$category_summary) > 0L) {
    report_data$category_summary$category[1]
  } else "—"

  total_budget <- if (report_data$budget_available &&
                      nrow(report_data$budget_status) > 0L) {
    sum(report_data$budget_status$budget, na.rm = TRUE)
  } else NA_real_

  budget_util_pct <- if (!is.na(total_budget) && total_budget > 0) {
    sprintf("%.1f%%", report_data$total_spending / total_budget * 100)
  } else "N/A — no budget set"

  over_budget_cats <- if (report_data$budget_available &&
                           nrow(report_data$budget_status) > 0L) {
    over <- report_data$budget_status[
      report_data$budget_status$status == BUDGET_STATUS_OVER, , drop = FALSE
    ]
    if (nrow(over) > 0L) paste(over$category, collapse = ", ") else "None"
  } else "N/A — no budget set"

  summary_sheet <- data.frame(
    Metric = c(
      "Report Period",
      "Generated At",
      "Total Spending",
      "Total Transactions",
      "Average Daily Spending",
      "Highest Spending Category",
      "Total Budget (if set)",
      "Budget Utilisation",
      "Over-Budget Categories",
      "Anomalies Detected",
      "Forecast Available"
    ),
    Value = c(
      report_data$month_label,
      report_data$generated_at,
      .fmt_amt(report_data$total_spending, sym),
      as.character(report_data$transaction_count),
      .fmt_amt(report_data$avg_daily_spending, sym),
      top_cat,
      if (!is.na(total_budget)) .fmt_amt(total_budget, sym) else "Not set",
      budget_util_pct,
      over_budget_cats,
      as.character(report_data$anomaly_summary$flagged_count),
      if (report_data$forecast_available) "Yes" else "No"
    ),
    stringsAsFactors = FALSE
  )

  # ---- Sheet 2: Transactions -------------------------------------------------
  txn_sheet <- if (nrow(report_data$month_txns) > 0L) {
    .prepare_transactions_for_export(report_data$month_txns)
  } else {
    data.frame(
      Message = "No transactions recorded for this period.",
      stringsAsFactors = FALSE
    )
  }

  # ---- Sheet 3: Category Breakdown -------------------------------------------
  cat_sheet <- if (nrow(report_data$category_summary) > 0L) {
    report_data$category_summary %>%
      dplyr::transmute(
        Category            = category,
        `Total Amount`      = round(total_amount, 2),
        `Transactions`      = as.integer(transaction_count),
        `% of Total`        = round(pct_of_total, 1)
      )
  } else {
    data.frame(Message = "No transactions to summarise.", stringsAsFactors = FALSE)
  }

  # ---- Sheet 4: Payment Methods ----------------------------------------------
  pay_sheet <- if (nrow(report_data$payment_summary) > 0L) {
    report_data$payment_summary %>%
      dplyr::transmute(
        `Payment Method`    = payment_method,
        `Total Amount`      = round(total_amount, 2),
        `Transactions`      = as.integer(transaction_count),
        `% of Total`        = round(pct_of_total, 1)
      )
  } else {
    data.frame(Message = "No transactions to summarise.", stringsAsFactors = FALSE)
  }

  # ---- Build the workbook sheet list ----------------------------------------
  sheets <- list(
    Summary          = summary_sheet,
    Transactions     = txn_sheet,
    `By Category`    = cat_sheet,
    `By Payment`     = pay_sheet
  )

  # ---- Sheet 5: Budget Status (conditional) ----------------------------------
  if (report_data$budget_available && nrow(report_data$budget_status) > 0L) {
    budget_sheet <- report_data$budget_status %>%
      dplyr::transmute(
        Category           = category,
        `Budget`           = round(budget, 2),
        `Actual Spending`  = round(actual_spending, 2),
        `Remaining`        = round(remaining, 2),
        `Utilisation (%)`  = dplyr::if_else(
          !is.na(utilization_pct),
          round(utilization_pct, 1),
          NA_real_
        ),
        Status             = dplyr::case_when(
          status == BUDGET_STATUS_OVER  ~ "Over Budget",
          status == BUDGET_STATUS_NEAR  ~ "Near Limit (>80%)",
          TRUE                          ~ "Under Budget"
        )
      )
    sheets[["Budget Status"]] <- budget_sheet
  }

  # ---- Sheet 6: Anomalies (conditional) -------------------------------------
  flagged_df <- report_data$anomaly_summary$flagged_df
  if (!is.null(flagged_df) && is.data.frame(flagged_df) &&
      nrow(flagged_df) > 0L) {

    anomaly_sheet <- flagged_df %>%
      dplyr::mutate(date = as.Date(as.character(date))) %>%
      dplyr::arrange(dplyr::desc(excess_amount)) %>%
      dplyr::transmute(
        Date                        = format(date, "%Y-%m-%d"),
        Category                    = as.character(category),
        Description                 = as.character(description),
        Amount                      = round(as.numeric(amount), 2),
        `Upper Bound (Q3 + k*IQR)`  = round(as.numeric(upper_bound), 2),
        `Excess Amount`             = round(as.numeric(excess_amount), 2),
        `IQR Multiplier`            = as.numeric(threshold),
        `Distribution Used`         = dplyr::if_else(
          as.character(scope) == "category",
          "Per-category", "Global fallback"
        ),
        Reason                      = as.character(reason)
      )
    sheets[["Anomalies"]] <- anomaly_sheet
  }

  # ---- Sheet 7: Forecast Summary (conditional) ------------------------------
  if (report_data$forecast_available) {
    fs <- report_data$forecast_summary

    forecast_rows <- if (isTRUE(fs$has_data)) {
      data.frame(
        Metric = c(
          "Forecast Horizon (days)",
          "Average Daily Projection",
          "Total Projected Spending",
          "Peak Projected Day",
          "Peak Projected Amount",
          "Average Lower Bound (95% PI)",
          "Average Upper Bound (95% PI)",
          "Average Prediction Interval Width",
          "Disclaimer"
        ),
        Value = c(
          as.character(fs$horizon_days),
          .fmt_amt(fs$avg_daily_forecast, sym),
          .fmt_amt(fs$total_forecast, sym),
          if (!is.na(fs$peak_forecast_date)) {
            format(as.Date(fs$peak_forecast_date), "%d %b %Y")
          } else "—",
          .fmt_amt(fs$peak_forecast_amount, sym),
          .fmt_amt(fs$avg_lower_bound, sym),
          .fmt_amt(fs$avg_upper_bound, sym),
          .fmt_amt(fs$uncertainty_width, sym),
          fs$caveat
        ),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        Metric = "Status",
        Value  = if (!is.null(fs$caveat)) fs$caveat else "Forecast unavailable.",
        stringsAsFactors = FALSE
      )
    }

    sheets[["Forecast"]] <- forecast_rows
  }

  # ---- Sheet 8: Monthly Trend -----------------------------------------------
  if (!is.null(report_data$monthly_trend) &&
      is.data.frame(report_data$monthly_trend) &&
      nrow(report_data$monthly_trend) > 0L) {

    trend_sheet <- report_data$monthly_trend %>%
      dplyr::transmute(
        Month                = year_month,
        `Total Spending`     = round(total_amount, 2),
        `Transactions`       = as.integer(transaction_count),
        `Avg Daily Spending` = round(avg_daily_amount, 2)
      ) %>%
      dplyr::arrange(dplyr::desc(Month))

    sheets[["Monthly Trend"]] <- trend_sheet
  }

  # ---- Write workbook --------------------------------------------------------
  path <- tempfile(
    pattern = sprintf(
      "monthly_report_%s_",
      gsub("-", "", report_data$month)
    ),
    fileext = ".xlsx"
  )

  tryCatch(
    writexl::write_xlsx(sheets, path = path),
    error = function(e) {
      stop(sprintf(
        "Report could not be written to file: %s. Check available disk space.",
        conditionMessage(e)
      ))
    }
  )
  path
}


# =============================================================================
# DOWNLOAD FILENAME HELPERS
# =============================================================================

#' Return a timestamped filename for a transaction export.
#' @param format Character: "csv" or "xlsx".
download_filename_transactions <- function(format = "csv") {
  .export_filename("expense_transactions", format)
}

#' Return a timestamped filename for a monthly report.
#' @param month Character "YYYY-MM".
download_filename_monthly_report <- function(month) {
  sprintf(
    "monthly_report_%s_%s.xlsx",
    gsub("-", "", as.character(month)),
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )
}

#' Return a timestamped filename for an analytics export.
download_filename_analytics <- function() {
  .export_filename("expense_analytics", "csv")
}

#' Return a timestamped filename for an anomaly export.
download_filename_anomalies <- function() {
  .export_filename("expense_anomalies", "csv")
}
