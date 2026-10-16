# =============================================================================
# R/budget.R
# Budget management module — pure computation layer.
#
# This module describes spending relative to user-defined budgets.
# It makes no financial recommendations and contains no advice about how
# users should adjust their spending.
#
# All functions operate on plain data.frames already fetched from SQLite by
# the caller.  No database calls are made here.
#
# Public API
# ----------
#   get_monthly_budget()           — build the canonical budget frame for one month
#   calculate_budget_status()      — per-category status + all derived columns
#   calculate_budget_remaining()   — remaining amount per category
#   calculate_budget_utilization() — utilization percentage per category
#   get_over_budget_categories()   — subset of rows where status == OVER_BUDGET
#
# Status values (character constants, not magic strings in downstream code)
# -------------------------------------------------------------------------
#   BUDGET_STATUS_UNDER  "UNDER_BUDGET"
#   BUDGET_STATUS_NEAR   "NEAR_LIMIT"
#   BUDGET_STATUS_OVER   "OVER_BUDGET"
#
# Threshold defaults
# ------------------
#   BUDGET_THRESHOLD_NEAR  0.80  (>= 80% → NEAR_LIMIT)
#   BUDGET_THRESHOLD_OVER  1.00  (>= 100% → OVER_BUDGET)
#
# Both thresholds are arguments on every function, so callers can override them
# without touching this file.
#
# Input contracts
# ---------------
# budget_df       — from get_budgets(con, month = "YYYY-MM")
#                   Required columns: category (chr), budget_amount (num)
#
# transactions_df — from get_transactions(con, ...)
#                   Required columns: date (chr/Date), category (chr),
#                   amount (num)
#
# month           — character "YYYY-MM" used to slice transactions_df to the
#                   relevant period.  Must be supplied whenever transactions_df
#                   covers more than one month.
#
# Output contract (calculate_budget_status)
# -----------------------------------------
# Every public function that touches status returns a data.frame with at
# minimum these columns, always in this order:
#   category            chr
#   budget              num   (the user-defined budget amount)
#   actual_spending     num   (actual spend in the period)
#   remaining           num   (budget - actual_spending; negative = over)
#   utilization_pct     num   (actual / budget * 100, rounded 1 dp; NA when budget == 0)
#   utilization_ratio   num   (actual / budget; NA when budget == 0)
#   status              chr   (one of the three BUDGET_STATUS_* constants)
#
# Dependencies: dplyr, lubridate, validation.R (VALID_CATEGORIES,
#               CATEGORY_COLOURS, CATEGORY_ICONS)
# =============================================================================

library(dplyr)
library(lubridate)


# =============================================================================
# STATUS CONSTANTS
# Reference these names in all downstream code — never compare to the raw
# string "OVER_BUDGET" etc.
# =============================================================================

BUDGET_STATUS_UNDER <- "UNDER_BUDGET"
BUDGET_STATUS_NEAR  <- "NEAR_LIMIT"
BUDGET_STATUS_OVER  <- "OVER_BUDGET"


# =============================================================================
# DEFAULT THRESHOLDS
# =============================================================================

BUDGET_THRESHOLD_NEAR <- 0.80   # utilization_ratio >= this → NEAR_LIMIT
BUDGET_THRESHOLD_OVER <- 1.00   # utilization_ratio >= this → OVER_BUDGET


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

# Typed zero-row result that every public function returns on bad input.
# Having correct column types here prevents downstream type errors in Shiny.
.empty_budget_df <- function() {
  data.frame(
    category          = character(0),
    budget            = numeric(0),
    actual_spending   = numeric(0),
    remaining         = numeric(0),
    utilization_pct   = numeric(0),
    utilization_ratio = numeric(0),
    status            = character(0),
    stringsAsFactors  = FALSE
  )
}


# Validate that a data.frame exists, is a data.frame, has rows, and contains
# all columns listed in `required`.  Returns TRUE or FALSE — never stops().
.budget_df_ok <- function(df, required = character(0)) {
  is.data.frame(df) &&
    nrow(df) > 0L &&
    all(required %in% names(df))
}


# Validate the month argument.  Accepts "YYYY-MM".
# Returns the trimmed string on success; stops with a clear message on failure.
.validate_month <- function(month) {
  if (is.null(month) || is.na(month)) {
    stop("budget.R: 'month' is required and must be a character string 'YYYY-MM'.")
  }
  m <- trimws(as.character(month))
  if (!grepl("^\\d{4}-\\d{2}$", m)) {
    stop(sprintf(
      "budget.R: month '%s' is not in YYYY-MM format.", m
    ))
  }
  parts <- as.integer(strsplit(m, "-")[[1]])
  if (parts[2] < 1L || parts[2] > 12L) {
    stop(sprintf(
      "budget.R: month value %d is outside the valid range 1-12.", parts[2]
    ))
  }
  m
}


# Validate the two threshold arguments and return them in a named list.
.validate_thresholds <- function(near_threshold, over_threshold) {
  n <- suppressWarnings(as.numeric(near_threshold))
  o <- suppressWarnings(as.numeric(over_threshold))

  if (is.na(n) || n < 0 || n > 1) {
    stop(sprintf(
      "budget.R: near_threshold must be a number in [0, 1]. Got: %s",
      near_threshold
    ))
  }
  if (is.na(o) || o < 0) {
    stop(sprintf(
      "budget.R: over_threshold must be a non-negative number. Got: %s",
      over_threshold
    ))
  }
  if (n >= o) {
    stop(sprintf(
      "budget.R: near_threshold (%.2f) must be less than over_threshold (%.2f).",
      n, o
    ))
  }
  list(near = n, over = o)
}


# Filter a transactions data.frame to a single "YYYY-MM" period.
# Returns a zero-row data.frame (with amount + category columns) when nothing
# matches.
.filter_to_month <- function(transactions_df, month) {
  empty <- data.frame(
    category = character(0),
    amount   = numeric(0),
    stringsAsFactors = FALSE
  )

  if (!.budget_df_ok(transactions_df, c("date", "category", "amount"))) {
    return(empty)
  }

  df <- transactions_df
  df$date <- suppressWarnings(as.Date(as.character(df$date)))

  # Drop rows where date or amount is NA / non-finite
  df <- df[!is.na(df$date) & !is.na(df$amount) & is.finite(df$amount), , drop = FALSE]
  if (nrow(df) == 0L) return(empty)

  # Keep only the target month
  df <- df[format(df$date, "%Y-%m") == month, , drop = FALSE]
  df
}


# Core join: left-join budget rows against actual spend, compute all derived
# columns, and classify each row.  Used by calculate_budget_status() and the
# two scalar helpers.
.compute_budget_join <- function(budget_df,
                                  transactions_df,
                                  month,
                                  near_threshold,
                                  over_threshold) {
  thresholds <- .validate_thresholds(near_threshold, over_threshold)

  # Actual spend per category for the period
  month_txns <- .filter_to_month(transactions_df, month)

  actual <- if (nrow(month_txns) > 0L) {
    month_txns %>%
      dplyr::group_by(category) %>%
      dplyr::summarise(
        actual_spending = sum(amount, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    data.frame(
      category        = character(0),
      actual_spending = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  # Left-join so every budgeted category appears even with zero spend
  result <- budget_df %>%
    dplyr::select(category, budget_amount) %>%
    dplyr::rename(budget = budget_amount) %>%
    dplyr::left_join(actual, by = "category") %>%
    dplyr::mutate(
      actual_spending   = dplyr::coalesce(actual_spending, 0),
      remaining         = budget - actual_spending,
      # utilization_ratio and utilization_pct are NA when budget == 0 so that
      # callers can distinguish "no budget set" from "zero spending"
      utilization_ratio = dplyr::if_else(
        budget > 0,
        actual_spending / budget,
        NA_real_
      ),
      utilization_pct   = dplyr::if_else(
        !is.na(utilization_ratio),
        round(utilization_ratio * 100, 1),
        NA_real_
      ),
      status = dplyr::case_when(
        is.na(utilization_ratio)                        ~ BUDGET_STATUS_UNDER,
        utilization_ratio >= thresholds$over            ~ BUDGET_STATUS_OVER,
        utilization_ratio >= thresholds$near            ~ BUDGET_STATUS_NEAR,
        TRUE                                            ~ BUDGET_STATUS_UNDER
      )
    ) %>%
    dplyr::select(
      category, budget, actual_spending, remaining,
      utilization_pct, utilization_ratio, status
    ) %>%
    dplyr::arrange(dplyr::desc(
      dplyr::coalesce(utilization_ratio, -1)
    ))

  result
}


# =============================================================================
# PUBLIC FUNCTIONS
# =============================================================================

# -----------------------------------------------------------------------------
# get_monthly_budget()
# -----------------------------------------------------------------------------

#' Build the canonical budget frame for a given month.
#'
#' Retrieves the budget rows for the requested month and enriches them with
#' display metadata (colour, icon) from validation.R constants.  This is the
#' starting point before any spend comparison is needed.
#'
#' The function is intentionally DB-agnostic: it takes the budget data.frame
#' that the caller fetched (via get_budgets()), not a DB connection.
#'
#' @param budget_df   data.frame from get_budgets(con, month = "YYYY-MM").
#'                    Required columns: category, budget_amount.
#' @param month       Character "YYYY-MM" stored as metadata in the result.
#'                    Not used for filtering (the caller has already filtered).
#'
#' @return data.frame with columns:
#'           month           (chr  "YYYY-MM")
#'           category        (chr)
#'           budget_amount   (num  >= 0)
#'           color_hex       (chr  from CATEGORY_COLOURS)
#'           icon            (chr  from CATEGORY_ICONS)
#'         One row per budgeted category, sorted by category name.
#'         Returns a zero-row data.frame with those column types when
#'         budget_df is empty or missing required columns.
get_monthly_budget <- function(budget_df, month) {
  empty <- data.frame(
    month         = character(0),
    category      = character(0),
    budget_amount = numeric(0),
    color_hex     = character(0),
    icon          = character(0),
    stringsAsFactors = FALSE
  )

  month <- tryCatch(.validate_month(month), error = function(e) {
    warning(conditionMessage(e))
    return(NULL)
  })
  if (is.null(month)) return(empty)

  if (!.budget_df_ok(budget_df, c("category", "budget_amount"))) {
    return(empty)
  }

  df <- budget_df %>%
    dplyr::select(category, budget_amount) %>%
    dplyr::filter(!is.na(category), !is.na(budget_amount)) %>%
    dplyr::mutate(
      month         = month,
      budget_amount = as.numeric(budget_amount),
      color_hex     = dplyr::coalesce(CATEGORY_COLOURS[category], "#CCCCCC"),
      icon          = dplyr::coalesce(CATEGORY_ICONS[category],   "tag")
    ) %>%
    dplyr::select(month, category, budget_amount, color_hex, icon) %>%
    dplyr::arrange(category)

  df
}


# -----------------------------------------------------------------------------
# calculate_budget_status()
# -----------------------------------------------------------------------------

#' Compute full per-category budget status for one month.
#'
#' This is the primary function of this module.  Every other public function
#' either delegates to this one or uses .compute_budget_join() directly.
#'
#' @param budget_df        data.frame from get_budgets(con, month).
#'                         Required columns: category, budget_amount.
#' @param transactions_df  data.frame from get_transactions(con).
#'                         Required columns: date, category, amount.
#' @param month            Character "YYYY-MM" — the period to compare against.
#' @param near_threshold   Numeric [0, 1): utilization_ratio at or above this
#'                         value → NEAR_LIMIT.  Default: BUDGET_THRESHOLD_NEAR.
#' @param over_threshold   Numeric (near_threshold, ∞): utilization_ratio at
#'                         or above this value → OVER_BUDGET.
#'                         Default: BUDGET_THRESHOLD_OVER.
#'
#' @return data.frame — one row per budgeted category — with columns:
#'           category          (chr)
#'           budget            (num   user-defined budget amount)
#'           actual_spending   (num   sum of transaction amounts in the period)
#'           remaining         (num   budget - actual_spending)
#'           utilization_pct   (num   0-N, rounded 1 dp; NA when budget == 0)
#'           utilization_ratio (num   0-N; NA when budget == 0)
#'           status            (chr   UNDER_BUDGET | NEAR_LIMIT | OVER_BUDGET)
#'         Rows are sorted descending by utilization_ratio so the most
#'         utilised categories appear first.
#'         Returns .empty_budget_df() when budget_df is empty or inputs are
#'         invalid.
calculate_budget_status <- function(budget_df,
                                     transactions_df,
                                     month,
                                     near_threshold = BUDGET_THRESHOLD_NEAR,
                                     over_threshold = BUDGET_THRESHOLD_OVER) {

  month <- tryCatch(.validate_month(month), error = function(e) {
    warning(conditionMessage(e))
    return(NULL)
  })
  if (is.null(month)) return(.empty_budget_df())

  if (!.budget_df_ok(budget_df, c("category", "budget_amount"))) {
    return(.empty_budget_df())
  }

  tryCatch(
    .compute_budget_join(
      budget_df       = budget_df,
      transactions_df = transactions_df,
      month           = month,
      near_threshold  = near_threshold,
      over_threshold  = over_threshold
    ),
    error = function(e) {
      warning(sprintf("calculate_budget_status: %s", conditionMessage(e)))
      .empty_budget_df()
    }
  )
}


# -----------------------------------------------------------------------------
# calculate_budget_remaining()
# -----------------------------------------------------------------------------

#' Calculate the remaining budget per category for one month.
#'
#' A convenience function that returns only the category, budget, actual spend,
#' and remaining amount — without the utilization or status columns.
#'
#' @param budget_df        data.frame from get_budgets(con, month).
#' @param transactions_df  data.frame from get_transactions(con).
#' @param month            Character "YYYY-MM".
#' @param near_threshold   Passed through to calculate_budget_status().
#' @param over_threshold   Passed through to calculate_budget_status().
#'
#' @return data.frame with columns:
#'           category        (chr)
#'           budget          (num)
#'           actual_spending (num)
#'           remaining       (num — negative when over budget)
#'         Sorted by remaining ASC (most over-budget / least remaining first).
#'         Returns a zero-row data.frame with those column types on bad input.
calculate_budget_remaining <- function(budget_df,
                                        transactions_df,
                                        month,
                                        near_threshold = BUDGET_THRESHOLD_NEAR,
                                        over_threshold = BUDGET_THRESHOLD_OVER) {
  empty <- data.frame(
    category        = character(0),
    budget          = numeric(0),
    actual_spending = numeric(0),
    remaining       = numeric(0),
    stringsAsFactors = FALSE
  )

  status_df <- calculate_budget_status(
    budget_df       = budget_df,
    transactions_df = transactions_df,
    month           = month,
    near_threshold  = near_threshold,
    over_threshold  = over_threshold
  )

  if (nrow(status_df) == 0L) return(empty)

  status_df %>%
    dplyr::select(category, budget, actual_spending, remaining) %>%
    dplyr::arrange(remaining)   # most negative (most over) first
}


# -----------------------------------------------------------------------------
# calculate_budget_utilization()
# -----------------------------------------------------------------------------

#' Calculate utilization percentage per category for one month.
#'
#' A convenience function that returns only the category, budget, actual spend,
#' and utilization columns — without the remaining or status columns.
#'
#' @param budget_df        data.frame from get_budgets(con, month).
#' @param transactions_df  data.frame from get_transactions(con).
#' @param month            Character "YYYY-MM".
#' @param near_threshold   Passed through to calculate_budget_status().
#' @param over_threshold   Passed through to calculate_budget_status().
#'
#' @return data.frame with columns:
#'           category          (chr)
#'           budget            (num)
#'           actual_spending   (num)
#'           utilization_pct   (num  0-N, 1 dp; NA when budget == 0)
#'           utilization_ratio (num  0-N; NA when budget == 0)
#'         Sorted descending by utilization_ratio.
#'         Returns a zero-row data.frame with those column types on bad input.
calculate_budget_utilization <- function(budget_df,
                                          transactions_df,
                                          month,
                                          near_threshold = BUDGET_THRESHOLD_NEAR,
                                          over_threshold = BUDGET_THRESHOLD_OVER) {
  empty <- data.frame(
    category          = character(0),
    budget            = numeric(0),
    actual_spending   = numeric(0),
    utilization_pct   = numeric(0),
    utilization_ratio = numeric(0),
    stringsAsFactors  = FALSE
  )

  status_df <- calculate_budget_status(
    budget_df       = budget_df,
    transactions_df = transactions_df,
    month           = month,
    near_threshold  = near_threshold,
    over_threshold  = over_threshold
  )

  if (nrow(status_df) == 0L) return(empty)

  status_df %>%
    dplyr::select(
      category, budget, actual_spending,
      utilization_pct, utilization_ratio
    ) %>%
    dplyr::arrange(dplyr::desc(dplyr::coalesce(utilization_ratio, -1)))
}


# -----------------------------------------------------------------------------
# get_over_budget_categories()
# -----------------------------------------------------------------------------

#' Return only the categories whose status is OVER_BUDGET.
#'
#' @param budget_df        data.frame from get_budgets(con, month).
#' @param transactions_df  data.frame from get_transactions(con).
#' @param month            Character "YYYY-MM".
#' @param near_threshold   Passed through to calculate_budget_status().
#' @param over_threshold   Passed through to calculate_budget_status().
#'
#' @return data.frame with the full calculate_budget_status() schema
#'         filtered to rows where status == BUDGET_STATUS_OVER.
#'         Returns .empty_budget_df() (zero rows) when nothing is over budget
#'         or when inputs are invalid.
get_over_budget_categories <- function(budget_df,
                                        transactions_df,
                                        month,
                                        near_threshold = BUDGET_THRESHOLD_NEAR,
                                        over_threshold = BUDGET_THRESHOLD_OVER) {
  status_df <- calculate_budget_status(
    budget_df       = budget_df,
    transactions_df = transactions_df,
    month           = month,
    near_threshold  = near_threshold,
    over_threshold  = over_threshold
  )

  if (nrow(status_df) == 0L) return(.empty_budget_df())

  over <- status_df[status_df$status == BUDGET_STATUS_OVER, , drop = FALSE]

  if (nrow(over) == 0L) return(.empty_budget_df())

  # Sort by how far over budget each category is (largest overspend first)
  over %>%
    dplyr::arrange(remaining)   # remaining is most negative for worst offenders
}
