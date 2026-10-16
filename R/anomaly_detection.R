# =============================================================================
# R/anomaly_detection.R
# IQR-based transaction anomaly detector.
#
# Philosophy
# ----------
# This module identifies transactions that are statistically unusual relative
# to the user's own spending history within each category.  "Unusual" means
# the amount sits above the IQR upper fence for that category.  Nothing in
# this module classifies transactions as errors, fraud, or bad decisions.
# Every flagged row includes a plain-language explanation of the arithmetic
# that caused the flag.
#
# Public API
# ----------
#   detect_transaction_anomalies(data, iqr_multiplier, min_obs)
#     Primary entry point.  Returns a data.frame of flagged transactions
#     with the spec-required columns including Q1, Q3, IQR, upper_bound,
#     threshold, and reason.
#
#   get_anomaly_summary(data, iqr_multiplier, min_obs)
#     Summary statistics over the full detection run.  Returns a named list
#     suitable for rendering KPI cards in the Shiny UI.
#
# Internal functions (dot-prefixed, not exported)
# -----------------------------------------------
#   .anomaly_input_ok()        — input guard
#   .clean_transactions()      — type coercion + NA removal
#   .compute_category_stats()  — Q1/Q3/IQR/upper_bound per category
#   .compute_global_stats()    — fallback fence over all transactions
#   .build_reason()            — vectorised human-readable explanation
#   .empty_flagged_df()        — typed zero-row result
#
# Output schema of detect_transaction_anomalies()
# ------------------------------------------------
#   transaction_id  chr
#   date            Date
#   category        chr
#   description     chr
#   amount          num
#   Q1              num   — lower quartile of the reference distribution
#   Q3              num   — upper quartile of the reference distribution
#   IQR             num   — interquartile range (Q3 - Q1)
#   upper_bound     num   — Q3 + iqr_multiplier * IQR  (the fence)
#   threshold       num   — the iqr_multiplier that was applied
#   scope           chr   — "category" or "global_fallback"
#   excess_amount   num   — amount - upper_bound  (always > 0 for flagged rows)
#   reason          chr   — plain-language description of why flagged
#
# Minimum observations
# --------------------
# IQR is not meaningful with fewer than IQR_MIN_OBS transactions.
# When a category has fewer than IQR_MIN_OBS observations the global
# distribution (all categories combined) is used instead, and scope is
# set to "global_fallback" so users can see which distribution was applied.
#
# Dependencies: dplyr
# =============================================================================

library(dplyr)


# =============================================================================
# CONSTANTS
# =============================================================================

# Minimum transactions in a single category before per-category IQR stats are
# computed.  Below this the global distribution is used as a fallback.
IQR_MIN_OBS <- 5L

# Default IQR multiplier.  Upper fence = Q3 + IQR_DEFAULT_MULTIPLIER * IQR.
# 1.5 is the standard Tukey fence; increase for fewer but more extreme flags.
IQR_DEFAULT_MULTIPLIER <- 1.5


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

# Typed zero-row data.frame with the exact column schema the public API
# guarantees.  Used as the canonical empty result throughout the module.
.empty_flagged_df <- function() {
  data.frame(
    transaction_id = character(0),
    date           = as.Date(character(0)),
    category       = character(0),
    description    = character(0),
    amount         = numeric(0),
    Q1             = numeric(0),
    Q3             = numeric(0),
    IQR            = numeric(0),
    upper_bound    = numeric(0),
    threshold      = numeric(0),
    scope          = character(0),
    excess_amount  = numeric(0),
    reason         = character(0),
    stringsAsFactors = FALSE
  )
}


# Returns TRUE if df is a non-NULL data.frame with the required columns and
# at least `min_rows` rows.  Never stops() — callers branch on the result.
.anomaly_input_ok <- function(df, min_rows = 1L) {
  required <- c("transaction_id", "date", "category",
                "description", "amount")
  is.data.frame(df) &&
    nrow(df) >= min_rows &&
    all(required %in% names(df))
}


# Coerce types and remove rows that cannot be used in the analysis:
#   - amount must be finite and positive
#   - date must be parseable
#   - category and description must be non-empty strings
# Returns a clean data.frame or a zero-row data.frame.
.clean_transactions <- function(df) {
  df$date        <- suppressWarnings(as.Date(as.character(df$date)))
  df$amount      <- suppressWarnings(as.numeric(df$amount))
  df$category    <- trimws(as.character(df$category))
  df$description <- trimws(as.character(df$description))

  df <- df[
    !is.na(df$date)     &
    !is.na(df$amount)   &
    is.finite(df$amount) &
    df$amount > 0       &
    nchar(df$category) > 0,
    , drop = FALSE
  ]
  df
}


# Compute per-category IQR statistics.
# Returns a data.frame with columns:
#   category, n_obs, Q1, Q3, IQR, upper_bound, use_global (logical)
.compute_category_stats <- function(df, iqr_multiplier) {
  df %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(
      n_obs = dplyr::n(),
      Q1    = stats::quantile(amount, 0.25, na.rm = TRUE),
      Q3    = stats::quantile(amount, 0.75, na.rm = TRUE),
      IQR   = stats::IQR(amount,            na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      upper_bound = Q3 + iqr_multiplier * IQR,
      # If IQR is exactly 0 (all amounts identical), the fence equals Q3, so
      # only strictly greater amounts will be flagged — which is correct.
      use_global  = n_obs < IQR_MIN_OBS
    )
}


# Compute the global IQR statistics across all transactions.
# Returns a named list: Q1, Q3, IQR, upper_bound.
.compute_global_stats <- function(df, iqr_multiplier) {
  amounts <- df$amount[is.finite(df$amount) & df$amount > 0]
  q1    <- stats::quantile(amounts, 0.25, na.rm = TRUE)
  q3    <- stats::quantile(amounts, 0.75, na.rm = TRUE)
  iqr   <- stats::IQR(amounts,            na.rm = TRUE)
  list(
    Q1          = q1,
    Q3          = q3,
    IQR         = iqr,
    upper_bound = q3 + iqr_multiplier * iqr
  )
}


# Vectorised reason builder.
# Arguments are all equal-length vectors — safe to call inside dplyr::mutate.
# Returns a character vector with one plain-language explanation per row.
.build_reason <- function(category, amount, Q1, Q3, IQR,
                           upper_bound, threshold, scope) {
  mapply(
    function(cat, amt, q1v, q3v, iqrv, ub, thr, sc) {
      scope_clause <- if (sc == "category") {
        sprintf("the '%s' category", cat)
      } else {
        sprintf(
          "all categories combined (global fallback: '%s' has fewer than %d transactions)",
          cat, IQR_MIN_OBS
        )
      }

      sprintf(
        paste0(
          "Amount %.2f exceeds the IQR upper bound of %.2f for %s. ",
          "Reference distribution: Q1 = %.2f, Q3 = %.2f, ",
          "IQR = %.2f, multiplier = %.1f ",
          "(upper bound = Q3 + %.1f \u00d7 IQR = %.2f + %.2f = %.2f). ",
          "This transaction is %.2f above the upper bound."
        ),
        amt, ub, scope_clause,
        q1v, q3v,
        iqrv, thr,
        thr, q3v, thr * iqrv, ub,
        amt - ub
      )
    },
    category, amount, Q1, Q3, IQR, upper_bound, threshold, scope,
    SIMPLIFY   = TRUE,
    USE.NAMES  = FALSE
  )
}


# =============================================================================
# detect_transaction_anomalies()
# =============================================================================

#' Detect unusually large transactions using the IQR method.
#'
#' For each category with at least `min_obs` transactions, computes:
#'   Q1, Q3, IQR, upper_bound = Q3 + iqr_multiplier * IQR
#' and flags any transaction whose amount exceeds upper_bound.
#'
#' Categories with fewer than `min_obs` transactions fall back to the
#' global distribution (all categories combined).  The `scope` column in
#' the output records which distribution was used for each flagged row.
#'
#' No financial advice is provided.  Labels describe statistical position
#' only: "unusually large relative to the category distribution."
#'
#' @param data            data.frame from get_transactions().  Required columns:
#'                        transaction_id, date, category, description, amount.
#'                        Additional columns are preserved in the output.
#' @param iqr_multiplier  Numeric multiplier applied to IQR.
#'                        Default: IQR_DEFAULT_MULTIPLIER (1.5).
#'                        Higher values flag fewer, more extreme transactions.
#' @param min_obs         Integer: minimum category observations before
#'                        per-category stats are used.
#'                        Default: IQR_MIN_OBS (5).
#'
#' @return data.frame with one row per flagged transaction and columns:
#'           transaction_id, date (Date), category, description, amount,
#'           Q1, Q3, IQR, upper_bound, threshold, scope, excess_amount, reason.
#'         Rows are sorted by excess_amount descending (largest anomaly first).
#'         Returns .empty_flagged_df() (zero rows, correct types) when:
#'           - data is NULL or not a data.frame
#'           - data has fewer than 3 usable rows
#'           - no transactions exceed their category upper bound
detect_transaction_anomalies <- function(data,
                                          iqr_multiplier = IQR_DEFAULT_MULTIPLIER,
                                          min_obs        = IQR_MIN_OBS) {

  # ---- Input guards ----------------------------------------------------------
  if (!.anomaly_input_ok(data, min_rows = 3L)) {
    return(.empty_flagged_df())
  }

  iqr_multiplier <- suppressWarnings(as.numeric(iqr_multiplier))
  min_obs        <- suppressWarnings(as.integer(min_obs))

  if (is.na(iqr_multiplier) || iqr_multiplier <= 0) {
    warning("detect_transaction_anomalies: iqr_multiplier must be > 0. Using default 1.5.")
    iqr_multiplier <- IQR_DEFAULT_MULTIPLIER
  }
  if (is.na(min_obs) || min_obs < 1L) {
    warning("detect_transaction_anomalies: min_obs must be >= 1. Using default 5.")
    min_obs <- IQR_MIN_OBS
  }

  # ---- Clean -----------------------------------------------------------------
  df <- .clean_transactions(data)
  if (nrow(df) < 3L) return(.empty_flagged_df())

  # ---- Per-category statistics -----------------------------------------------
  cat_stats <- .compute_category_stats(df, iqr_multiplier)

  # ---- Global fallback statistics --------------------------------------------
  global <- .compute_global_stats(df, iqr_multiplier)

  # ---- Join statistics back to each transaction ------------------------------
  df_joined <- df %>%
    dplyr::left_join(
      cat_stats %>% dplyr::select(category, n_obs, Q1, Q3, IQR,
                                   upper_bound, use_global),
      by = "category"
    ) %>%
    dplyr::mutate(
      # Apply per-category stats when the category is well-observed,
      # global stats otherwise.
      eff_Q1    = dplyr::if_else(use_global, global$Q1,          Q1),
      eff_Q3    = dplyr::if_else(use_global, global$Q3,          Q3),
      eff_IQR   = dplyr::if_else(use_global, global$IQR,         IQR),
      eff_bound = dplyr::if_else(use_global, global$upper_bound, upper_bound),
      scope     = dplyr::if_else(use_global, "global_fallback",  "category"),
      is_flagged = amount > eff_bound
    )

  # ---- Extract flagged rows --------------------------------------------------
  flagged <- df_joined %>%
    dplyr::filter(is_flagged) %>%
    dplyr::mutate(
      Q1           = round(eff_Q1,    2),
      Q3           = round(eff_Q3,    2),
      IQR          = round(eff_IQR,   2),
      upper_bound  = round(eff_bound, 2),
      threshold    = iqr_multiplier,
      excess_amount = round(amount - eff_bound, 2),
      reason       = .build_reason(
        category    = category,
        amount      = amount,
        Q1          = eff_Q1,
        Q3          = eff_Q3,
        IQR         = eff_IQR,
        upper_bound = eff_bound,
        threshold   = iqr_multiplier,
        scope       = scope
      )
    ) %>%
    dplyr::select(
      transaction_id, date, category, description, amount,
      Q1, Q3, IQR, upper_bound, threshold, scope, excess_amount, reason
    ) %>%
    dplyr::arrange(dplyr::desc(excess_amount))

  if (nrow(flagged) == 0L) return(.empty_flagged_df())

  flagged
}


# =============================================================================
# get_anomaly_summary()
# =============================================================================

#' Compute a summary of the IQR anomaly detection run.
#'
#' Runs detect_transaction_anomalies() internally and wraps the results into
#' a named list suitable for rendering as KPI cards or a summary panel.
#'
#' The per-category breakdown shows the IQR statistics for every category
#' that had enough observations, regardless of whether any transactions were
#' flagged.  This makes the detection transparent: users can see what
#' thresholds were applied to each category.
#'
#' @param data            data.frame from get_transactions().
#' @param iqr_multiplier  Numeric IQR multiplier. Default: IQR_DEFAULT_MULTIPLIER.
#' @param min_obs         Integer minimum observations. Default: IQR_MIN_OBS.
#'
#' @return Named list:
#'   $flagged_count       int   — number of flagged transactions
#'   $total_checked       int   — total usable transactions evaluated
#'   $flagged_df          data.frame — full output of detect_transaction_anomalies()
#'   $flagged_amount_total num  — sum of amounts of flagged transactions
#'   $highest_amount      num  — largest flagged amount (NA if none)
#'   $categories_flagged  chr  — unique categories that had flagged transactions
#'   $categories_skipped  chr  — categories below min_obs (used global fallback)
#'   $category_stats      data.frame — per-category Q1/Q3/IQR/upper_bound table
#'   $iqr_multiplier      num  — multiplier that was applied
#'   $min_obs             int  — minimum observation threshold used
#'   $status              chr  — "ok" | "no_anomalies" | "insufficient_data"
#'   $status_message      chr  — human-readable description of the run result
get_anomaly_summary <- function(data,
                                 iqr_multiplier = IQR_DEFAULT_MULTIPLIER,
                                 min_obs        = IQR_MIN_OBS) {

  # Default return for degenerate inputs
  empty_summary <- list(
    flagged_count        = 0L,
    total_checked        = 0L,
    flagged_df           = .empty_flagged_df(),
    flagged_amount_total = 0,
    highest_amount       = NA_real_,
    categories_flagged   = character(0),
    categories_skipped   = character(0),
    category_stats       = data.frame(
      category    = character(0),
      n_obs       = integer(0),
      Q1          = numeric(0),
      Q3          = numeric(0),
      IQR         = numeric(0),
      upper_bound = numeric(0),
      scope       = character(0),
      stringsAsFactors = FALSE
    ),
    iqr_multiplier  = IQR_DEFAULT_MULTIPLIER,
    min_obs         = IQR_MIN_OBS,
    status          = "insufficient_data",
    status_message  = "No transaction data available for anomaly detection."
  )

  # ---- Input guard -----------------------------------------------------------
  if (!.anomaly_input_ok(data, min_rows = 3L)) {
    n_provided <- if (is.data.frame(data)) nrow(data) else 0L
    empty_summary$status_message <- sprintf(
      paste0(
        "Anomaly detection requires at least 3 valid transactions. ",
        "%d row%s provided."
      ),
      n_provided,
      if (n_provided == 1L) "" else "s"
    )
    return(empty_summary)
  }

  # ---- Coerce multiplier / min_obs -------------------------------------------
  iqr_multiplier <- suppressWarnings(as.numeric(iqr_multiplier))
  min_obs        <- suppressWarnings(as.integer(min_obs))
  if (is.na(iqr_multiplier) || iqr_multiplier <= 0) iqr_multiplier <- IQR_DEFAULT_MULTIPLIER
  if (is.na(min_obs)        || min_obs < 1L)         min_obs        <- IQR_MIN_OBS

  # ---- Clean data ------------------------------------------------------------
  df <- .clean_transactions(data)
  if (nrow(df) < 3L) {
    empty_summary$status_message <-
      "After removing invalid rows, fewer than 3 transactions remain."
    return(empty_summary)
  }

  total_checked <- nrow(df)

  # ---- Per-category statistics (for the stats table) -------------------------
  cat_stats <- .compute_category_stats(df, iqr_multiplier)
  global    <- .compute_global_stats(df, iqr_multiplier)

  # Build the category stats display table
  category_stats <- cat_stats %>%
    dplyr::mutate(
      scope = dplyr::if_else(
        use_global,
        "global_fallback",
        "category"
      ),
      # For display, replace stats with global values when falling back
      Q1_display          = dplyr::if_else(use_global, global$Q1,          Q1),
      Q3_display          = dplyr::if_else(use_global, global$Q3,          Q3),
      IQR_display         = dplyr::if_else(use_global, global$IQR,         IQR),
      upper_bound_display = dplyr::if_else(use_global, global$upper_bound, upper_bound)
    ) %>%
    dplyr::transmute(
      category    = category,
      n_obs       = as.integer(n_obs),
      Q1          = round(Q1_display,          2),
      Q3          = round(Q3_display,          2),
      IQR         = round(IQR_display,         2),
      upper_bound = round(upper_bound_display, 2),
      scope       = scope
    ) %>%
    dplyr::arrange(category)

  categories_skipped <- cat_stats$category[cat_stats$use_global]

  # ---- Run detection ---------------------------------------------------------
  flagged_df <- detect_transaction_anomalies(
    data           = data,
    iqr_multiplier = iqr_multiplier,
    min_obs        = min_obs
  )

  flagged_count <- nrow(flagged_df)

  # ---- Build status ----------------------------------------------------------
  status <- if (flagged_count == 0L) "no_anomalies" else "ok"

  status_message <- if (flagged_count == 0L) {
    sprintf(
      paste0(
        "No unusually large transactions detected across %d transaction%s ",
        "(IQR multiplier: %.1f, minimum observations per category: %d)."
      ),
      total_checked,
      if (total_checked == 1L) "" else "s",
      iqr_multiplier,
      min_obs
    )
  } else {
    sprintf(
      paste0(
        "%d unusually large transaction%s identified out of %d total ",
        "(IQR multiplier: %.1f). ",
        "Affected %s: %s."
      ),
      flagged_count,
      if (flagged_count == 1L) "" else "s",
      total_checked,
      iqr_multiplier,
      if (length(unique(flagged_df$category)) == 1L) "category" else "categories",
      paste(sort(unique(flagged_df$category)), collapse = ", ")
    )
  }

  list(
    flagged_count        = flagged_count,
    total_checked        = total_checked,
    flagged_df           = flagged_df,
    flagged_amount_total = if (flagged_count > 0L) {
      sum(flagged_df$amount, na.rm = TRUE)
    } else 0,
    highest_amount       = if (flagged_count > 0L) {
      max(flagged_df$amount, na.rm = TRUE)
    } else NA_real_,
    categories_flagged   = if (flagged_count > 0L) {
      sort(unique(flagged_df$category))
    } else character(0),
    categories_skipped   = categories_skipped,
    category_stats       = category_stats,
    iqr_multiplier       = iqr_multiplier,
    min_obs              = min_obs,
    status               = status,
    status_message       = status_message
  )
}


# =============================================================================
# BACKWARD-COMPATIBILITY WRAPPERS
# The Shiny server and DT renderer use the older function names from the
# previous version of this module.  These thin wrappers keep those callers
# working without modification.
# =============================================================================

#' @rdname detect_transaction_anomalies
detect_anomalies_iqr <- function(df, threshold = IQR_DEFAULT_MULTIPLIER) {
  detect_transaction_anomalies(df, iqr_multiplier = threshold)
}

#' @rdname get_anomaly_summary
run_anomaly_detection <- function(df,
                                   method    = "IQR",
                                   threshold = NULL) {
  if (!is.null(threshold) && !is.na(suppressWarnings(as.numeric(threshold)))) {
    mult <- as.numeric(threshold)
  } else {
    mult <- IQR_DEFAULT_MULTIPLIER
  }

  result <- get_anomaly_summary(df, iqr_multiplier = mult)

  # Re-map the new status values to the old ones expected by server.R
  old_status <- switch(result$status,
    ok                = "ok",
    no_anomalies      = "no_anomalies",
    insufficient_data = "insufficient_data",
    "insufficient_data"
  )

  list(
    status        = old_status,
    flagged_df    = result$flagged_df,
    method        = "IQR",
    threshold     = mult,
    total_checked = result$total_checked,
    message       = result$status_message
  )
}

#' @rdname get_anomaly_summary
anomaly_summary_stats <- function(flagged_df) {
  if (is.null(flagged_df) || nrow(flagged_df) == 0L) {
    return(list(
      count               = 0L,
      total_amount        = 0,
      highest_amount      = 0,
      categories_affected = character(0)
    ))
  }
  list(
    count               = nrow(flagged_df),
    total_amount        = sum(flagged_df$amount,  na.rm = TRUE),
    highest_amount      = max(flagged_df$amount,  na.rm = TRUE),
    categories_affected = unique(flagged_df$category)
  )
}


# =============================================================================
# DT TABLE RENDERER
# Kept here so the UI layer has a single import point for anomaly-related
# rendering.  No logic — purely display formatting.
# =============================================================================

#' Render the flagged transactions as a styled DT datatable.
#'
#' @param flagged_df      data.frame from detect_transaction_anomalies().
#' @param currency_symbol Character currency symbol. Default "\u20b9".
#' @return DT::datatable object.
render_anomaly_table <- function(flagged_df, currency_symbol = "\u20b9") {

  empty_display <- data.frame(
    Date          = character(0),
    Category      = character(0),
    Description   = character(0),
    Amount        = character(0),
    `Upper Bound` = character(0),
    Scope         = character(0),
    `Excess`      = character(0),
    Reason        = character(0),
    check.names   = FALSE,
    stringsAsFactors = FALSE
  )

  if (is.null(flagged_df) || nrow(flagged_df) == 0L) {
    return(
      DT::datatable(
        empty_display,
        rownames = FALSE,
        class    = "display",
        options  = list(
          dom      = "t",
          language = list(emptyTable = "No unusually large transactions detected.")
        )
      )
    )
  }

  fmt <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

  display_df <- flagged_df %>%
    dplyr::mutate(
      Date          = format(as.Date(date), "%d %b %Y"),
      Category      = category,
      Description   = description,
      Amount        = paste0(currency_symbol, "\u00a0", fmt(amount)),
      `Upper Bound` = paste0(currency_symbol, "\u00a0", fmt(upper_bound)),
      Scope         = dplyr::if_else(
        scope == "category", "Per-category", "Global fallback"
      ),
      Excess        = paste0("+", currency_symbol, "\u00a0",
                             fmt(excess_amount)),
      Reason        = reason
    ) %>%
    dplyr::select(Date, Category, Description, Amount,
                  `Upper Bound`, Scope, Excess, Reason)

  n_rows      <- nrow(display_df)
  row_colours <- grDevices::colorRampPalette(c("#FFF3CD", "#F8D7DA"))(n_rows)

  DT::datatable(
    display_df,
    rownames  = FALSE,
    escape    = FALSE,
    class     = "display stripe hover compact",
    options   = list(
      pageLength = 10,
      scrollX    = TRUE,
      autoWidth  = TRUE,
      dom        = "lfrtip",
      columnDefs = list(
        list(width = "35%",       targets = 7),
        list(className = "dt-right",  targets = c(3, 4, 6)),
        list(className = "dt-center", targets = c(0, 5))
      ),
      language = list(
        search     = "Search anomalies:",
        lengthMenu = "Show _MENU_ anomalies"
      )
    )
  ) %>%
    DT::formatStyle(
      columns    = "Amount",
      color      = "#C0392B",
      fontWeight = "bold"
    ) %>%
    DT::formatStyle(
      columns    = "Excess",
      color      = "#922B21",
      fontWeight = "600"
    ) %>%
    DT::formatStyle(
      columns         = 0,
      target          = "row",
      backgroundColor = DT::styleRow(seq_len(n_rows), row_colours)
    )
}
