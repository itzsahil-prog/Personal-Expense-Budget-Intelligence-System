# =============================================================================
# R/prediction.R
# Daily expense forecasting module.
#
# Philosophy
# ----------
# This module produces statistical projections of future daily spending based
# on the user's own historical data.  Projections are estimates derived from
# observed patterns — they are not guaranteed future values.  Every result
# object carries an explicit uncertainty flag and a human-readable caveat
# string so the UI can surface this clearly.
#
# Public API (spec-mandated names)
# ---------------------------------
#   prepare_forecast_data(transactions_df)
#     Aggregates raw transactions to daily totals, fills date gaps,
#     validates that enough history exists, and returns a structured object
#     ready for model training.
#
#   train_expense_forecast(prepared_data, method, horizon_days)
#     Fits the best available time-series model (ETS → ARIMA → mean naive)
#     to the prepared daily series.
#
#   generate_expense_forecast(trained_model)
#     Applies the fitted model to produce a combined historical + forecast
#     data.frame with columns: date, actual, forecast, lower_bound, upper_bound.
#
#   calculate_forecast_summary(forecast_df, currency_symbol)
#     Computes KPI statistics over the forecast portion of the output from
#     generate_expense_forecast().
#
# Output schema of generate_expense_forecast()
# ---------------------------------------------
#   date          Date    — calendar date
#   actual        num     — observed daily total (NA for future rows)
#   forecast      num     — model point estimate (NA for historical rows)
#   lower_bound   num     — 95% prediction interval lower end (NA for historical)
#   upper_bound   num     — 95% prediction interval upper end (NA for historical)
#   is_forecast   lgl     — TRUE for future rows, FALSE for historical rows
#
# Forecasting availability thresholds
# ------------------------------------
#   FORECAST_MIN_DAYS   14   Minimum distinct calendar days with observations
#                            required before any model is attempted.
#   FORECAST_MAX_HORIZON 90  Hard cap on the number of future days returned.
#
# Model selection
# ---------------
#   1. ETS  (Error/Trend/Seasonality) via forecast::ets()   — primary
#   2. ARIMA via forecast::auto.arima()                      — first fallback
#   3. Mean naive via forecast::meanf()                      — last resort
# All model fitting is wrapped in tryCatch; a failure in one method
# automatically falls through to the next.  The model type actually used is
# stored in the trained-model object and surfaced in calculate_forecast_summary().
#
# Dependencies: forecast, dplyr, lubridate, plotly
# =============================================================================

library(forecast)
library(dplyr)
library(lubridate)
library(plotly)


# =============================================================================
# CONSTANTS
# =============================================================================

FORECAST_MIN_DAYS    <- 14L   # minimum history days before forecasting
FORECAST_MAX_HORIZON <- 90L   # maximum future days allowed
FORECAST_CONFIDENCE  <- 95L   # confidence level for prediction intervals (%)


# =============================================================================
# INTERNAL RESULT CONSTRUCTORS
# Every public function returns one of these typed lists so server.R can
# branch on $status without fragile string comparisons.
# =============================================================================

.result_ok <- function(historical_df, forecast_df, model_label,
                        model_details, summary_stats) {
  list(
    status        = "ok",
    historical_df = historical_df,   # daily totals used for training
    forecast_df   = forecast_df,     # full generate_expense_forecast() output
    model_label   = model_label,
    model_details = model_details,
    summary_stats = summary_stats,
    is_estimate   = TRUE,            # always TRUE — projections are not guaranteed
    caveat        = paste0(
      "These projections are statistical estimates based on historical patterns. ",
      "Actual future spending may differ."
    )
  )
}

.result_insufficient <- function(days_available, days_required) {
  list(
    status         = "insufficient_data",
    days_available = as.integer(days_available),
    days_required  = as.integer(days_required),
    message        = sprintf(
      paste0(
        "Forecasting requires at least %d days of transaction history. ",
        "You currently have %d day%s of data. ",
        "Add more transactions and try again."
      ),
      days_required,
      days_available,
      if (days_available == 1L) "" else "s"
    )
  )
}

.result_error <- function(msg) {
  list(
    status  = "error",
    message = as.character(msg)
  )
}


# =============================================================================
# INTERNAL: empty typed data.frame for the forecast output schema
# =============================================================================

.empty_forecast_df <- function() {
  data.frame(
    date        = as.Date(character(0)),
    actual      = numeric(0),
    forecast    = numeric(0),
    lower_bound = numeric(0),
    upper_bound = numeric(0),
    is_forecast = logical(0),
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# 1. prepare_forecast_data()
# =============================================================================

#' Prepare raw transaction data for time-series forecasting.
#'
#' Steps:
#'   1. Coerce and validate input types.
#'   2. Remove rows with missing or non-positive amounts or unparseable dates.
#'   3. Aggregate amounts to one row per calendar day.
#'   4. Fill internal date gaps with zero (days with no spending).
#'   5. Check that the resulting series has enough observations.
#'   6. Construct a ts object with weekly seasonality when possible.
#'
#' @param transactions_df  data.frame from get_transactions().  Required
#'                         columns: date (chr/Date), amount (num).
#'
#' @return Named list with status "ready" or "insufficient_data":
#'
#'   On success ($status == "ready"):
#'     $daily_df    data.frame — one row per day, columns: date (Date),
#'                  total_amount (num)
#'     $ts_object   ts object  — gap-filled daily totals
#'     $n_obs       int        — number of rows in daily_df (after gap fill)
#'     $n_txn_days  int        — number of days that actually had transactions
#'     $date_range  chr        — "DD Mon YYYY to DD Mon YYYY"
#'     $frequency   int        — ts frequency (7 = weekly, 1 = none)
#'
#'   On failure ($status == "insufficient_data"):
#'     $days_available  int
#'     $days_required   int
#'     $message         chr
prepare_forecast_data <- function(transactions_df) {

  # ---- Input guard -----------------------------------------------------------
  if (!is.data.frame(transactions_df) ||
      !all(c("date", "amount") %in% names(transactions_df))) {
    return(.result_insufficient(0L, FORECAST_MIN_DAYS))
  }

  df <- transactions_df
  df$date   <- suppressWarnings(as.Date(as.character(df$date)))
  df$amount <- suppressWarnings(as.numeric(df$amount))

  # Drop rows that cannot contribute to the analysis
  df <- df[!is.na(df$date) & !is.na(df$amount) &
             is.finite(df$amount) & df$amount > 0, , drop = FALSE]

  if (nrow(df) == 0L) {
    return(.result_insufficient(0L, FORECAST_MIN_DAYS))
  }

  # ---- Aggregate to daily totals ---------------------------------------------
  daily_raw <- df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      total_amount = sum(amount, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(date)

  n_txn_days <- nrow(daily_raw)

  # ---- Check minimum history before gap-fill --------------------------------
  if (n_txn_days < FORECAST_MIN_DAYS) {
    return(.result_insufficient(n_txn_days, FORECAST_MIN_DAYS))
  }

  # ---- Fill date gaps with zero spend ----------------------------------------
  full_seq <- data.frame(
    date = seq(min(daily_raw$date), max(daily_raw$date), by = "day"),
    stringsAsFactors = FALSE
  )

  daily_filled <- full_seq %>%
    dplyr::left_join(daily_raw, by = "date") %>%
    dplyr::mutate(total_amount = dplyr::coalesce(total_amount, 0))

  n_obs <- nrow(daily_filled)

  # ---- Build ts object -------------------------------------------------------
  # Use weekly seasonality (frequency = 7) when we have at least 2 full weeks
  # of observations; otherwise no seasonal component (frequency = 1).
  freq <- if (n_obs >= 14L) 7L else 1L

  ts_obj <- stats::ts(daily_filled$total_amount, frequency = freq)

  list(
    status      = "ready",
    daily_df    = daily_filled,
    ts_object   = ts_obj,
    n_obs       = n_obs,
    n_txn_days  = n_txn_days,
    date_range  = sprintf(
      "%s to %s",
      format(min(daily_filled$date), "%d %b %Y"),
      format(max(daily_filled$date), "%d %b %Y")
    ),
    frequency   = freq
  )
}


# =============================================================================
# 2. train_expense_forecast()
# =============================================================================

#' Fit the best available time-series model to prepared daily spending data.
#'
#' Model selection order:
#'   1. ETS (Error, Trend, Seasonality) — handles trend and weekly patterns
#'   2. ARIMA — used when ETS fails
#'   3. Mean naive — always succeeds; used as last resort
#'
#' All fitting is wrapped in tryCatch.  A failure in one tier automatically
#' falls through to the next.  No predictions are created here — this function
#' only trains the model.
#'
#' @param prepared_data  Named list from prepare_forecast_data() with
#'                       $status == "ready".
#' @param method         Character: "auto" (try in order), "ets", "arima",
#'                       or "naive".  Default: "auto".
#' @param horizon_days   Integer: how many future days will be forecast.
#'                       Must be between 1 and FORECAST_MAX_HORIZON.
#'                       Default: 30.
#'
#' @return Named list:
#'   $status        chr  — "trained" | "insufficient_data" | "error"
#'   $model         obj  — fitted model object (NULL on failure)
#'   $model_type    chr  — "ets" | "arima" | "naive" | "none"
#'   $model_label   chr  — human-readable model description
#'   $horizon_days  int  — clamped forecast horizon
#'   $prepared_data lst  — the prepared_data input (passed through for chaining)
#'   $aic           num  — AIC of the fitted model (NA for naive)
#'   $rmse          num  — in-sample RMSE (NA for naive)
train_expense_forecast <- function(prepared_data,
                                    method       = "auto",
                                    horizon_days = 30L) {

  # ---- Guard -----------------------------------------------------------------
  if (is.null(prepared_data) || prepared_data$status != "ready") {
    if (!is.null(prepared_data) && prepared_data$status == "insufficient_data") {
      return(c(
        list(status = "insufficient_data", model = NULL,
             model_type = "none", model_label = "No model",
             horizon_days = 30L),
        prepared_data[c("days_available", "days_required", "message")]
      ))
    }
    return(.result_error("train_expense_forecast: invalid prepared_data input.")
           |> c(list(model = NULL, model_type = "none",
                     model_label = "No model", horizon_days = 30L,
                     prepared_data = prepared_data)))
  }

  horizon_days <- min(
    max(suppressWarnings(as.integer(horizon_days)), 1L),
    FORECAST_MAX_HORIZON
  )

  ts_obj <- prepared_data$ts_object

  # ---- Attempt ETS -----------------------------------------------------------
  if (method %in% c("auto", "ets")) {
    ets_result <- tryCatch({
      m     <- forecast::ets(ts_obj)
      comps <- m$components
      label <- sprintf("ETS(%s,%s,%s)", comps[1], comps[2], comps[3])
      list(
        status        = "trained",
        model         = m,
        model_type    = "ets",
        model_label   = label,
        horizon_days  = horizon_days,
        prepared_data = prepared_data,
        aic           = tryCatch(round(m$aic, 2), error = function(e) NA_real_),
        rmse          = .in_sample_rmse(m)
      )
    }, error = function(e) NULL, warning = function(w) NULL)

    if (!is.null(ets_result)) return(ets_result)
  }

  # ---- Attempt ARIMA ---------------------------------------------------------
  if (method %in% c("auto", "arima")) {
    arima_result <- tryCatch({
      m <- forecast::auto.arima(
        ts_obj,
        stepwise      = TRUE,
        approximation = TRUE,
        seasonal      = stats::frequency(ts_obj) > 1L
      )
      p <- m$arma[1]; d <- m$arma[6]; q <- m$arma[2]
      label <- sprintf("ARIMA(%d,%d,%d)", p, d, q)
      list(
        status        = "trained",
        model         = m,
        model_type    = "arima",
        model_label   = label,
        horizon_days  = horizon_days,
        prepared_data = prepared_data,
        aic           = tryCatch(round(m$aic, 2), error = function(e) NA_real_),
        rmse          = .in_sample_rmse(m)
      )
    }, error = function(e) NULL, warning = function(w) NULL)

    if (!is.null(arima_result)) return(arima_result)
  }

  # ---- Mean naive (always succeeds) -----------------------------------------
  mean_val <- mean(as.numeric(ts_obj), na.rm = TRUE)
  sd_val   <- stats::sd(as.numeric(ts_obj), na.rm = TRUE)
  if (is.na(sd_val) || sd_val == 0) sd_val <- abs(mean_val) * 0.10

  list(
    status        = "trained",
    model         = list(mean_val = mean_val, sd_val = sd_val,
                          ts_obj = ts_obj),
    model_type    = "naive",
    model_label   = "Mean (naive fallback — insufficient pattern in data)",
    horizon_days  = horizon_days,
    prepared_data = prepared_data,
    aic           = NA_real_,
    rmse          = NA_real_
  )
}


# Internal: compute in-sample RMSE from a fitted model object.
.in_sample_rmse <- function(model) {
  tryCatch({
    resid <- as.numeric(stats::residuals(model))
    round(sqrt(mean(resid^2, na.rm = TRUE)), 2)
  }, error = function(e) NA_real_)
}


# =============================================================================
# 3. generate_expense_forecast()
# =============================================================================

#' Generate the combined historical + forecast data.frame.
#'
#' Applies the fitted model from train_expense_forecast() to produce future
#' daily spending estimates, then stacks them with the historical daily totals
#' into a single data.frame with the spec-mandated output schema.
#'
#' The output uses `NA` as the sentinel for "not applicable":
#'   - Historical rows: forecast / lower_bound / upper_bound are NA.
#'   - Future rows:     actual is NA.
#' This makes it straightforward to plot both series on the same axis and to
#' filter each half independently.
#'
#' @param trained_model  Named list from train_expense_forecast() with
#'                       $status == "trained".
#'
#' @return On success: a .result_ok() list whose $forecast_df has columns:
#'           date (Date), actual (num|NA), forecast (num|NA),
#'           lower_bound (num|NA), upper_bound (num|NA), is_forecast (lgl)
#'         On failure: .result_insufficient() or .result_error().
generate_expense_forecast <- function(trained_model) {

  # ---- Guard -----------------------------------------------------------------
  if (is.null(trained_model)) {
    return(.result_error("generate_expense_forecast: trained_model is NULL."))
  }
  if (trained_model$status == "insufficient_data") {
    return(.result_insufficient(
      trained_model$days_available,
      trained_model$days_required
    ))
  }
  if (trained_model$status != "trained" ||
      is.null(trained_model$model)) {
    return(.result_error(
      "generate_expense_forecast: model was not trained successfully."
    ))
  }

  prepared  <- trained_model$prepared_data
  horizon   <- trained_model$horizon_days
  last_date <- max(prepared$daily_df$date)
  future_dates <- seq(last_date + 1L, by = "day", length.out = horizon)

  # ---- Generate future predictions -------------------------------------------
  future_df <- tryCatch({
    if (trained_model$model_type == "naive") {
      mu  <- trained_model$model$mean_val
      sig <- trained_model$model$sd_val
      # 95% interval: mu ± 1.96 * sd
      data.frame(
        date        = future_dates,
        actual      = NA_real_,
        forecast    = pmax(rep(mu, horizon), 0),
        lower_bound = pmax(rep(mu - 1.96 * sig, horizon), 0),
        upper_bound = pmax(rep(mu + 1.96 * sig, horizon), 0),
        is_forecast = TRUE,
        stringsAsFactors = FALSE
      )
    } else {
      fc_obj <- forecast::forecast(
        trained_model$model,
        h     = horizon,
        level = FORECAST_CONFIDENCE
      )
      data.frame(
        date        = future_dates,
        actual      = NA_real_,
        forecast    = pmax(as.numeric(fc_obj$mean),    0),
        lower_bound = pmax(as.numeric(fc_obj$lower),   0),
        upper_bound = pmax(as.numeric(fc_obj$upper),   0),
        is_forecast = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }, error = function(e) {
    NULL
  })

  if (is.null(future_df)) {
    return(.result_error(paste0(
      "Model was trained successfully but could not generate future projections. ",
      "Try reducing the forecast horizon or adding more historical data."
    )))
  }

  # ---- Build historical half -------------------------------------------------
  hist_df <- prepared$daily_df %>%
    dplyr::transmute(
      date        = as.Date(date),
      actual      = total_amount,
      forecast    = NA_real_,
      lower_bound = NA_real_,
      upper_bound = NA_real_,
      is_forecast = FALSE
    )

  # ---- Stack and return ------------------------------------------------------
  forecast_df <- dplyr::bind_rows(hist_df, future_df) %>%
    dplyr::arrange(date)

  model_details <- list(
    type       = trained_model$model_type,
    label      = trained_model$model_label,
    n_obs      = prepared$n_obs,
    n_txn_days = prepared$n_txn_days,
    date_range = prepared$date_range,
    frequency  = prepared$frequency,
    aic        = trained_model$aic,
    rmse       = trained_model$rmse,
    horizon    = horizon
  )

  summary_stats <- calculate_forecast_summary(forecast_df)

  .result_ok(
    historical_df = prepared$daily_df,
    forecast_df   = forecast_df,
    model_label   = trained_model$model_label,
    model_details = model_details,
    summary_stats = summary_stats
  )
}


# =============================================================================
# 4. calculate_forecast_summary()
# =============================================================================

#' Compute KPI statistics over the forecast portion of the output.
#'
#' Operates only on rows where is_forecast == TRUE.  The historical portion
#' is ignored here because its statistics are computed in analytics.R.
#'
#' @param forecast_df     data.frame from generate_expense_forecast()$forecast_df.
#'                        Must contain columns: date, forecast, lower_bound,
#'                        upper_bound, is_forecast.
#' @param currency_symbol Character currency symbol. Default "\u20b9".
#'
#' @return Named list:
#'   $horizon_days         int  — number of forecast days
#'   $avg_daily_forecast   num  — mean of point estimates
#'   $total_forecast       num  — sum of point estimates over the horizon
#'   $peak_forecast_date   Date — date of the highest point estimate
#'   $peak_forecast_amount num  — highest point estimate
#'   $avg_lower_bound      num  — mean lower bound
#'   $avg_upper_bound      num  — mean upper bound
#'   $uncertainty_width    num  — mean(upper_bound - lower_bound); measure of
#'                                how wide the prediction interval is on average
#'   $has_data             lgl  — FALSE when forecast_df is empty or NULL
#'   $caveat               chr  — standard estimate disclaimer
calculate_forecast_summary <- function(forecast_df,
                                        currency_symbol = "\u20b9") {
  empty <- list(
    horizon_days         = 0L,
    avg_daily_forecast   = 0,
    total_forecast       = 0,
    peak_forecast_date   = NA,
    peak_forecast_amount = 0,
    avg_lower_bound      = 0,
    avg_upper_bound      = 0,
    uncertainty_width    = 0,
    has_data             = FALSE,
    caveat               = paste0(
      "These projections are statistical estimates based on historical patterns. ",
      "Actual future spending may differ."
    )
  )

  if (is.null(forecast_df) || !is.data.frame(forecast_df) ||
      nrow(forecast_df) == 0L) {
    return(empty)
  }

  if (!"is_forecast" %in% names(forecast_df)) return(empty)

  fc_rows <- forecast_df[
    forecast_df$is_forecast == TRUE &
    !is.na(forecast_df$forecast),
    , drop = FALSE
  ]

  if (nrow(fc_rows) == 0L) return(empty)

  peak_idx <- which.max(fc_rows$forecast)

  list(
    horizon_days         = nrow(fc_rows),
    avg_daily_forecast   = round(mean(fc_rows$forecast,     na.rm = TRUE), 2),
    total_forecast       = round(sum(fc_rows$forecast,      na.rm = TRUE), 2),
    peak_forecast_date   = fc_rows$date[peak_idx],
    peak_forecast_amount = round(fc_rows$forecast[peak_idx], 2),
    avg_lower_bound      = round(mean(fc_rows$lower_bound,  na.rm = TRUE), 2),
    avg_upper_bound      = round(mean(fc_rows$upper_bound,  na.rm = TRUE), 2),
    uncertainty_width    = round(
      mean(fc_rows$upper_bound - fc_rows$lower_bound, na.rm = TRUE), 2
    ),
    has_data             = TRUE,
    caveat               = paste0(
      "These projections are statistical estimates based on historical patterns. ",
      "Actual future spending may differ."
    )
  )
}


# =============================================================================
# CONVENIENCE WRAPPER
# run_forecast() was the old primary entry point called by server.R.
# It is kept as a single-call orchestration that chains all four steps.
# =============================================================================

#' Orchestrate prepare → train → generate in one call.
#'
#' @param transactions_df  data.frame from get_transactions().
#' @param horizon_days     Integer forecast horizon (1 – FORECAST_MAX_HORIZON).
#' @param method           Character: "auto" | "ets" | "arima" | "naive".
#'
#' @return One of .result_ok(), .result_insufficient(), or .result_error().
run_forecast <- function(transactions_df,
                          horizon_days = 30L,
                          method       = "auto") {

  # Step 1 — prepare
  prepared <- prepare_forecast_data(transactions_df)
  if (is.null(prepared) || prepared$status != "ready") {
    days_avail <- if (!is.null(prepared$n_txn_days)) prepared$n_txn_days else 0L
    return(.result_insufficient(days_avail, FORECAST_MIN_DAYS))
  }

  # Step 2 — train
  trained <- train_expense_forecast(
    prepared_data = prepared,
    method        = method,
    horizon_days  = horizon_days
  )
  if (is.null(trained) || trained$status != "trained") {
    if (!is.null(trained) && trained$status == "insufficient_data") {
      return(.result_insufficient(
        trained$days_available, trained$days_required
      ))
    }
    return(.result_error(
      "Could not fit a forecasting model. Try adding more historical transactions."
    ))
  }

  # Step 3 — generate
  result <- generate_expense_forecast(trained)
  result
}


# =============================================================================
# BACKWARD-COMPATIBILITY SHIMS
# The old API used prepare_ts_data() / fit_forecast_model() / generate_forecast().
# These thin wrappers map old call sites to the new internals.
# =============================================================================

#' @rdname prepare_forecast_data
prepare_ts_data <- function(daily_df) {
  # Old callers pass an already-aggregated daily_df; wrap it so
  # prepare_forecast_data() can accept it.
  if (!is.null(daily_df) && "total_amount" %in% names(daily_df) &&
      !"amount" %in% names(daily_df)) {
    daily_df$amount <- daily_df$total_amount
  }
  result <- prepare_forecast_data(daily_df)
  if (result$status != "ready") return(NULL)
  # Return the old format expected by fit_forecast_model()
  list(
    ts_object  = result$ts_object,
    filled_df  = result$daily_df,
    n_obs      = result$n_obs,
    date_range = result$date_range
  )
}

#' @rdname train_expense_forecast
fit_forecast_model <- function(ts_data, method = "auto") {
  # Reconstruct a minimal prepared_data from the old ts_data format
  prepared <- list(
    status     = "ready",
    daily_df   = ts_data$filled_df,
    ts_object  = ts_data$ts_object,
    n_obs      = ts_data$n_obs,
    n_txn_days = ts_data$n_obs,
    date_range = ts_data$date_range,
    frequency  = stats::frequency(ts_data$ts_object)
  )
  trained <- train_expense_forecast(prepared, method = method, horizon_days = 30L)
  if (is.null(trained) || trained$status != "trained") return(NULL)
  list(
    model = trained$model,
    label = trained$model_label,
    type  = trained$model_type
  )
}

#' @rdname generate_expense_forecast
generate_forecast <- function(model_result, ts_data, horizon_days = 30L) {
  # Re-wrap into the new format and call through
  prepared <- list(
    status     = "ready",
    daily_df   = ts_data$filled_df,
    ts_object  = ts_data$ts_object,
    n_obs      = ts_data$n_obs,
    n_txn_days = ts_data$n_obs,
    date_range = ts_data$date_range,
    frequency  = stats::frequency(ts_data$ts_object)
  )
  trained <- list(
    status        = "trained",
    model         = model_result$model,
    model_type    = model_result$type,
    model_label   = model_result$label,
    horizon_days  = as.integer(horizon_days),
    prepared_data = prepared,
    aic           = NA_real_,
    rmse          = NA_real_
  )
  result <- generate_expense_forecast(trained)
  if (result$status != "ok") return(NULL)
  # Return old format: future rows only with mean/lo80/hi80/lo95/hi95
  fc_rows <- result$forecast_df[result$forecast_df$is_forecast == TRUE, ]
  data.frame(
    date        = fc_rows$date,
    mean        = fc_rows$forecast,
    lo80        = fc_rows$lower_bound,
    hi80        = fc_rows$upper_bound,
    lo95        = fc_rows$lower_bound,
    hi95        = fc_rows$upper_bound,
    is_forecast = TRUE,
    stringsAsFactors = FALSE
  )
}

#' @rdname calculate_forecast_summary
get_forecast_summary <- function(forecast_df, currency_symbol = "\u20b9") {
  s <- calculate_forecast_summary(forecast_df, currency_symbol)
  # Return old field names for existing server.R callers
  list(
    avg_predicted   = s$avg_daily_forecast,
    total_predicted = s$total_forecast,
    peak_date       = s$peak_forecast_date,
    peak_amount     = s$peak_forecast_amount
  )
}


# =============================================================================
# PLOTLY CHART FUNCTIONS
# Kept here so the UI layer has one import point for all forecast rendering.
# =============================================================================

#' Render the forecast as a Plotly chart.
#'
#' Displays the historical series and the forecast projection with a shaded
#' prediction-interval band.  Clearly distinguishes historical (solid blue)
#' from forecast (dashed red) to avoid presenting projections as certainty.
#'
#' @param forecast_result  Named list from run_forecast() or
#'                         generate_expense_forecast().
#' @param currency_symbol  Character. Default "\u20b9".
#' @return plotly object.
plot_forecast <- function(forecast_result, currency_symbol = "\u20b9") {

  if (is.null(forecast_result)) {
    return(.forecast_empty_chart("No forecast result available."))
  }
  if (forecast_result$status == "insufficient_data") {
    return(.forecast_empty_chart(forecast_result$message))
  }
  if (forecast_result$status == "error") {
    return(.forecast_empty_chart(
      paste("Unable to generate forecast:", forecast_result$message)
    ))
  }
  if (forecast_result$status != "ok") {
    return(.forecast_empty_chart("Forecast unavailable."))
  }

  fc_df   <- forecast_result$forecast_df
  hist_df <- fc_df[!fc_df$is_forecast, ]
  fut_df  <- fc_df[ fc_df$is_forecast, ]

  fmt <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

  hist_hover <- paste0(
    "<b>", format(hist_df$date, "%d %b %Y"), "</b><br>",
    "Actual: ", currency_symbol, "\u00a0", fmt(hist_df$actual)
  )

  fut_hover <- paste0(
    "<b>", format(fut_df$date, "%d %b %Y"), "</b><br>",
    "Projection: ", currency_symbol, "\u00a0", fmt(fut_df$forecast), "<br>",
    sprintf("%d%% interval: [", FORECAST_CONFIDENCE),
    currency_symbol, "\u00a0", fmt(fut_df$lower_bound), " \u2013 ",
    currency_symbol, "\u00a0", fmt(fut_df$upper_bound), "]"
  )

  y_max <- max(c(hist_df$actual, fut_df$upper_bound), na.rm = TRUE) * 1.05

  p <- plotly::plot_ly() %>%

    # Prediction interval ribbon
    plotly::add_ribbons(
      data       = fut_df,
      x          = ~date,
      ymin       = ~lower_bound,
      ymax       = ~upper_bound,
      fillcolor  = "rgba(231, 76, 60, 0.12)",
      line       = list(color = "transparent"),
      name       = sprintf("%d%% Prediction interval", FORECAST_CONFIDENCE),
      hoverinfo  = "none",
      showlegend = TRUE
    ) %>%

    # Historical line
    plotly::add_trace(
      data      = hist_df,
      x         = ~date,
      y         = ~actual,
      type      = "scatter",
      mode      = "lines",
      line      = list(color = "#3498DB", width = 2),
      text      = hist_hover,
      hoverinfo = "text",
      name      = "Actual spending"
    ) %>%

    # Forecast line
    plotly::add_trace(
      data      = fut_df,
      x         = ~date,
      y         = ~forecast,
      type      = "scatter",
      mode      = "lines",
      line      = list(color = "#E74C3C", width = 2, dash = "dot"),
      text      = fut_hover,
      hoverinfo = "text",
      name      = "Projection (estimate)"
    ) %>%

    # Vertical boundary marker
    plotly::add_segments(
      x         = max(hist_df$date),
      xend      = max(hist_df$date),
      y         = 0,
      yend      = y_max,
      line      = list(color = "#95A5A6", width = 1, dash = "dash"),
      hoverinfo = "none",
      showlegend = FALSE
    ) %>%

    plotly::layout(
      title = list(
        text = paste0(
          "Expense Projection \u2014 ",
          forecast_result$model_label,
          "<br><sub style='font-size:11px;color:#95A5A6;'>",
          "Statistical estimate only. Not a guarantee of future spending.",
          "</sub>"
        ),
        font = list(size = 14, color = "#2C3E50")
      ),
      xaxis = list(
        title    = "Date",
        showgrid = TRUE, gridcolor = "#ECF0F1",
        tickfont = list(size = 11)
      ),
      yaxis = list(
        title     = paste0("Daily spending (", currency_symbol, ")"),
        showgrid  = TRUE, gridcolor = "#ECF0F1",
        rangemode = "tozero",
        tickfont  = list(size = 11)
      ),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FAFAFA",
      legend        = list(orientation = "h", x = 0, y = -0.2),
      margin        = list(t = 70, r = 20, b = 70, l = 70)
    ) %>%
    plotly::config(
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d"),
      displaylogo            = FALSE
    )

  p
}

.forecast_empty_chart <- function(message) {
  plotly::plot_ly(type = "scatter", mode = "text") %>%
    plotly::add_text(
      x        = 0.5,
      y        = 0.5,
      text     = message,
      textfont = list(size = 14, color = "#95A5A6")
    ) %>%
    plotly::layout(
      xaxis         = list(visible = FALSE),
      yaxis         = list(visible = FALSE),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FAFAFA",
      margin        = list(t = 40, r = 20, b = 40, l = 20)
    ) %>%
    plotly::config(displayModeBar = FALSE)
}
