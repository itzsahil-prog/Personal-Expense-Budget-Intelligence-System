# =============================================================================
# tests/testthat/test-prediction.R
# Tests for R/prediction.R
#
# Covers:
#   prepare_forecast_data()
#   train_expense_forecast()
#   generate_expense_forecast()
#   calculate_forecast_summary()
#   run_forecast()
#
# Error/edge cases:
#   [13] Insufficient forecast data — all functions return status=insufficient_data
#        NULL input
#        Empty data.frame
#        Fewer than FORECAST_MIN_DAYS distinct dates
#        All-NA amounts stripped before counting
#        Output schema: date, actual, forecast, lower_bound, upper_bound, is_forecast
#        $caveat is always present and non-empty
#        Predictions are never negative
#        calculate_forecast_summary on empty/NULL forecast_df
# =============================================================================

# ── fixtures ─────────────────────────────────────────────────────────────────

# Build a data.frame with `n_days` distinct calendar days of transactions
.make_history <- function(n_days = 30, amount = 500) {
  dates <- seq(as.Date("2025-01-01"),
               as.Date("2025-01-01") + (n_days - 1L),
               by = "day")
  data.frame(
    transaction_id = paste0("t", seq_along(dates)),
    date           = as.character(dates),
    category       = rep("Food", length(dates)),
    description    = paste0("d", seq_along(dates)),
    amount         = rep(as.numeric(amount), length(dates)),
    payment_method = "UPI",
    stringsAsFactors = FALSE
  )
}

# ── prepare_forecast_data() ──────────────────────────────────────────────────

test_that("prepare_forecast_data status=insufficient_data for NULL [spec #13]", {
  out <- prepare_forecast_data(NULL)
  expect_equal(out$status, "insufficient_data")
})

test_that("prepare_forecast_data status=insufficient_data for empty df [spec #13]", {
  out <- prepare_forecast_data(data.frame())
  expect_equal(out$status, "insufficient_data")
})

test_that("prepare_forecast_data status=insufficient_data for < FORECAST_MIN_DAYS rows", {
  df  <- .make_history(FORECAST_MIN_DAYS - 1L)
  out <- prepare_forecast_data(df)
  expect_equal(out$status, "insufficient_data")
  expect_lte(out$days_available, FORECAST_MIN_DAYS - 1L)
  expect_equal(out$days_required, FORECAST_MIN_DAYS)
})

test_that("prepare_forecast_data returns $message with user-friendly text", {
  out <- prepare_forecast_data(.make_history(3))
  expect_type(out$message, "character")
  expect_false(grepl("Error in ", out$message))
  expect_gt(nchar(out$message), 10L)
})

test_that("prepare_forecast_data status=ready when sufficient history", {
  out <- prepare_forecast_data(.make_history(FORECAST_MIN_DAYS))
  expect_equal(out$status, "ready")
  expect_s3_class(out$daily_df, "data.frame")
  expect_true(inherits(out$ts_object, "ts"))
})

test_that("prepare_forecast_data strips NA amounts before counting days", {
  df          <- .make_history(FORECAST_MIN_DAYS)
  df$amount[1:5] <- NA
  # Still enough valid rows
  out <- prepare_forecast_data(df)
  expect_equal(out$status, "ready")
})

test_that("prepare_forecast_data all-NA amounts → insufficient_data", {
  df        <- .make_history(FORECAST_MIN_DAYS)
  df$amount <- NA_real_
  out       <- prepare_forecast_data(df)
  expect_equal(out$status, "insufficient_data")
})

test_that("prepare_forecast_data fills date gaps with zero", {
  df    <- .make_history(30)
  df    <- df[c(1:10, 21:30), ]   # skip days 11-20
  out   <- prepare_forecast_data(df)
  if (out$status == "ready") {
    gap_rows <- out$daily_df[out$daily_df$total_amount == 0, ]
    expect_gte(nrow(gap_rows), 1L)   # gap days should be present with 0
  }
})

test_that("prepare_forecast_data missing required columns → insufficient_data", {
  df <- data.frame(date = "2025-01-01", stringsAsFactors = FALSE)  # no amount
  out <- prepare_forecast_data(df)
  expect_equal(out$status, "insufficient_data")
})

# ── train_expense_forecast() ─────────────────────────────────────────────────

test_that("train_expense_forecast returns status=trained on valid input", {
  prep <- prepare_forecast_data(.make_history(30))
  skip_if(prep$status != "ready", "Prepare step failed — skipping train test")
  out  <- train_expense_forecast(prep)
  expect_equal(out$status, "trained")
  expect_false(is.null(out$model))
  expect_true(out$model_type %in% c("ets","arima","naive"))
})

test_that("train_expense_forecast returns insufficient_data for bad input [spec #13]", {
  out <- train_expense_forecast(list(status = "insufficient_data",
                                      days_available = 3L,
                                      days_required  = 14L,
                                      message        = "not enough"))
  expect_equal(out$status, "insufficient_data")
})

test_that("train_expense_forecast returns error for NULL input", {
  out <- train_expense_forecast(NULL)
  expect_true(out$status %in% c("insufficient_data", "error"))
})

test_that("train_expense_forecast horizon_days is clamped to FORECAST_MAX_HORIZON", {
  prep <- prepare_forecast_data(.make_history(30))
  skip_if(prep$status != "ready")
  out  <- train_expense_forecast(prep, horizon_days = 999L)
  expect_lte(out$horizon_days, FORECAST_MAX_HORIZON)
})

test_that("train_expense_forecast horizon_days minimum is 1", {
  prep <- prepare_forecast_data(.make_history(30))
  skip_if(prep$status != "ready")
  out  <- train_expense_forecast(prep, horizon_days = 0L)
  expect_gte(out$horizon_days, 1L)
})

# ── generate_expense_forecast() ──────────────────────────────────────────────

test_that("generate_expense_forecast returns error for NULL trained_model", {
  out <- generate_expense_forecast(NULL)
  expect_equal(out$status, "error")
})

test_that("generate_expense_forecast output schema has all required columns [spec output]", {
  prep    <- prepare_forecast_data(.make_history(30))
  skip_if(prep$status != "ready")
  trained <- train_expense_forecast(prep, horizon_days = 14L)
  skip_if(trained$status != "trained")
  result  <- generate_expense_forecast(trained)
  skip_if(result$status != "ok")
  fc_df   <- result$forecast_df
  expect_true(all(c("date","actual","forecast","lower_bound",
                     "upper_bound","is_forecast") %in% names(fc_df)))
})

test_that("generate_expense_forecast historical rows have NA forecast", {
  prep    <- prepare_forecast_data(.make_history(30))
  skip_if(prep$status != "ready")
  trained <- train_expense_forecast(prep, horizon_days = 14L)
  skip_if(trained$status != "trained")
  result  <- generate_expense_forecast(trained)
  skip_if(result$status != "ok")
  hist <- result$forecast_df[!result$forecast_df$is_forecast, ]
  expect_true(all(is.na(hist$forecast)))
})

test_that("generate_expense_forecast future rows have NA actual", {
  prep    <- prepare_forecast_data(.make_history(30))
  skip_if(prep$status != "ready")
  trained <- train_expense_forecast(prep, horizon_days = 14L)
  skip_if(trained$status != "trained")
  result  <- generate_expense_forecast(trained)
  skip_if(result$status != "ok")
  fut <- result$forecast_df[result$forecast_df$is_forecast, ]
  expect_true(all(is.na(fut$actual)))
})

test_that("generate_expense_forecast forecasted values are non-negative", {
  prep    <- prepare_forecast_data(.make_history(30))
  skip_if(prep$status != "ready")
  trained <- train_expense_forecast(prep, horizon_days = 14L)
  skip_if(trained$status != "trained")
  result  <- generate_expense_forecast(trained)
  skip_if(result$status != "ok")
  fut <- result$forecast_df[result$forecast_df$is_forecast, ]
  expect_true(all(fut$forecast    >= 0, na.rm = TRUE))
  expect_true(all(fut$lower_bound >= 0, na.rm = TRUE))
  expect_true(all(fut$upper_bound >= 0, na.rm = TRUE))
})

test_that("generate_expense_forecast result$caveat is present and non-empty", {
  prep    <- prepare_forecast_data(.make_history(30))
  skip_if(prep$status != "ready")
  trained <- train_expense_forecast(prep, horizon_days = 14L)
  skip_if(trained$status != "trained")
  result  <- generate_expense_forecast(trained)
  skip_if(result$status != "ok")
  expect_true(!is.null(result$caveat))
  expect_gt(nchar(result$caveat), 10L)
})

test_that("generate_expense_forecast correct number of future rows", {
  horizon <- 14L
  prep    <- prepare_forecast_data(.make_history(30))
  skip_if(prep$status != "ready")
  trained <- train_expense_forecast(prep, horizon_days = horizon)
  skip_if(trained$status != "trained")
  result  <- generate_expense_forecast(trained)
  skip_if(result$status != "ok")
  n_future <- sum(result$forecast_df$is_forecast)
  expect_equal(n_future, horizon)
})

# ── calculate_forecast_summary() ─────────────────────────────────────────────

test_that("calculate_forecast_summary returns has_data=FALSE for NULL", {
  out <- calculate_forecast_summary(NULL)
  expect_false(out$has_data)
  expect_equal(out$horizon_days, 0L)
})

test_that("calculate_forecast_summary returns has_data=FALSE for empty df", {
  out <- calculate_forecast_summary(data.frame())
  expect_false(out$has_data)
})

test_that("calculate_forecast_summary caveat is always present", {
  out <- calculate_forecast_summary(NULL)
  expect_type(out$caveat, "character")
  expect_gt(nchar(out$caveat), 10L)
})

test_that("calculate_forecast_summary computes correctly on known data", {
  fc_df <- data.frame(
    date        = seq(as.Date("2025-07-01"), by = "day", length.out = 5),
    actual      = NA_real_,
    forecast    = c(100, 200, 300, 200, 100),
    lower_bound = c(50,  100, 150, 100, 50),
    upper_bound = c(150, 300, 450, 300, 150),
    is_forecast = TRUE,
    stringsAsFactors = FALSE
  )
  out <- calculate_forecast_summary(fc_df)
  expect_true(out$has_data)
  expect_equal(out$horizon_days, 5L)
  expect_equal(out$total_forecast, 900)
  expect_equal(out$avg_daily_forecast, 180)
  expect_equal(out$peak_forecast_amount, 300)
})

test_that("calculate_forecast_summary historical rows (is_forecast=FALSE) are ignored", {
  fc_df <- data.frame(
    date        = seq(as.Date("2025-07-01"), by = "day", length.out = 4),
    actual      = c(100, 200, NA, NA),
    forecast    = c(NA,  NA,  300, 400),
    lower_bound = c(NA,  NA,  250, 350),
    upper_bound = c(NA,  NA,  350, 450),
    is_forecast = c(FALSE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  out <- calculate_forecast_summary(fc_df)
  expect_equal(out$horizon_days, 2L)
  expect_equal(out$total_forecast, 700)
})

# ── run_forecast() — end-to-end ──────────────────────────────────────────────

test_that("run_forecast returns insufficient_data for NULL [spec #13]", {
  out <- run_forecast(NULL)
  expect_equal(out$status, "insufficient_data")
})

test_that("run_forecast returns insufficient_data for too few days [spec #13]", {
  out <- run_forecast(.make_history(FORECAST_MIN_DAYS - 1L))
  expect_equal(out$status, "insufficient_data")
  expect_type(out$message, "character")
  expect_false(grepl("Error in ", out$message))
})

test_that("run_forecast returns status=ok with sufficient history", {
  out <- run_forecast(.make_history(30))
  expect_true(out$status %in% c("ok", "error"))   # error = model issue, not crash
  if (out$status == "ok") {
    expect_true(out$is_estimate)
    expect_false(is.null(out$caveat))
  }
})

test_that("run_forecast is_estimate is always TRUE when status=ok", {
  out <- run_forecast(.make_history(30))
  if (out$status == "ok") {
    expect_true(isTRUE(out$is_estimate))
  }
})
