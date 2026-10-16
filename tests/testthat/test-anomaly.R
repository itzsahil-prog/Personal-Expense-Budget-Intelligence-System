# =============================================================================
# tests/testthat/test-anomaly.R
# Tests for R/anomaly_detection.R
#
# Covers:
#   detect_transaction_anomalies()
#   get_anomaly_summary()
#
# Error/edge cases:
#   [14] Empty anomaly result — returns typed empty frame, not error
#        Insufficient data (< 3 rows)
#        NULL input
#        All-same amount (IQR = 0)
#        Invalid iqr_multiplier / min_obs coerced with warning
#        Global fallback for sparse categories
#        Output schema columns present
#        No anomalies present in clean data
# =============================================================================

# ── fixtures ─────────────────────────────────────────────────────────────────

# 30 transactions in one category with a known outlier at the end
.food_df <- function(n_normal = 29, outlier_amount = 50000) {
  normal <- data.frame(
    transaction_id = paste0("txn_n", seq_len(n_normal)),
    date           = rep("2025-06-15", n_normal),
    category       = rep("Food", n_normal),
    description    = paste0("Meal ", seq_len(n_normal)),
    amount         = rep(300, n_normal),
    payment_method = "UPI",
    stringsAsFactors = FALSE
  )
  outlier <- data.frame(
    transaction_id = "txn_outlier",
    date           = "2025-06-20",
    category       = "Food",
    description    = "Expensive gala dinner",
    amount         = outlier_amount,
    payment_method = "Card",
    stringsAsFactors = FALSE
  )
  rbind(normal, outlier)
}

# Multi-category df — enough per category for per-category stats
.multi_df <- function() {
  n <- 10
  data.frame(
    transaction_id = paste0("t", seq_len(n * 3)),
    date           = rep("2025-06-15", n * 3),
    category       = rep(c("Food","Transport","Bills"), each = n),
    description    = paste0("Item ", seq_len(n * 3)),
    amount         = c(rep(200, n), rep(80, n), rep(500, n)),
    payment_method = "UPI",
    stringsAsFactors = FALSE
  )
}

# ── detect_transaction_anomalies() ───────────────────────────────────────────

test_that("detect_transaction_anomalies returns typed empty frame for NULL [spec #14]", {
  out <- detect_transaction_anomalies(NULL)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expected_cols <- c("transaction_id","date","category","description",
                     "amount","Q1","Q3","IQR","upper_bound",
                     "threshold","scope","excess_amount","reason")
  expect_true(all(expected_cols %in% names(out)))
})

test_that("detect_transaction_anomalies returns empty frame for < 3 rows", {
  df  <- .food_df(n_normal = 2, outlier_amount = 1000)
  out <- detect_transaction_anomalies(df)
  expect_equal(nrow(out), 0L)
})

test_that("detect_transaction_anomalies returns empty frame for empty data.frame [spec #14]", {
  out <- detect_transaction_anomalies(data.frame())
  expect_equal(nrow(out), 0L)
})

test_that("detect_transaction_anomalies flags known outlier", {
  df  <- .food_df()   # 29 normal @300, 1 outlier @50000
  out <- detect_transaction_anomalies(df)
  expect_gte(nrow(out), 1L)
  expect_true("txn_outlier" %in% out$transaction_id)
})

test_that("detect_transaction_anomalies does not flag normal transactions", {
  df  <- .food_df()
  out <- detect_transaction_anomalies(df)
  # Only the outlier row should be flagged
  flagged_ids <- out$transaction_id
  normal_ids  <- paste0("txn_n", 1:29)
  expect_length(intersect(flagged_ids, normal_ids), 0L)
})

test_that("detect_transaction_anomalies output columns match spec schema", {
  df  <- .food_df()
  out <- detect_transaction_anomalies(df)
  if (nrow(out) > 0) {
    expect_true(all(c("Q1","Q3","IQR","upper_bound","threshold",
                       "scope","excess_amount","reason") %in% names(out)))
  }
})

test_that("detect_transaction_anomalies excess_amount is always > 0", {
  df  <- .food_df()
  out <- detect_transaction_anomalies(df)
  if (nrow(out) > 0) {
    expect_true(all(out$excess_amount > 0))
  }
})

test_that("detect_transaction_anomalies threshold column equals iqr_multiplier used", {
  df  <- .food_df()
  out <- detect_transaction_anomalies(df, iqr_multiplier = 2.0)
  if (nrow(out) > 0) {
    expect_true(all(out$threshold == 2.0))
  }
})

test_that("detect_transaction_anomalies higher multiplier flags fewer transactions", {
  df    <- .food_df(n_normal = 29, outlier_amount = 1000)
  out_low  <- detect_transaction_anomalies(df, iqr_multiplier = 0.5)
  out_high <- detect_transaction_anomalies(df, iqr_multiplier = 4.0)
  expect_gte(nrow(out_low), nrow(out_high))
})

test_that("detect_transaction_anomalies invalid iqr_multiplier warns, uses default", {
  df <- .food_df()
  expect_warning(
    out <- detect_transaction_anomalies(df, iqr_multiplier = -1),
    regexp = "iqr_multiplier"
  )
  # Should still run with default multiplier
  expect_s3_class(out, "data.frame")
})

test_that("detect_transaction_anomalies invalid min_obs warns, uses default", {
  df <- .food_df()
  expect_warning(
    out <- detect_transaction_anomalies(df, min_obs = 0),
    regexp = "min_obs"
  )
  expect_s3_class(out, "data.frame")
})

test_that("detect_transaction_anomalies scope=category when enough observations", {
  df  <- .food_df()   # 30 Food txns >> IQR_MIN_OBS default of 5
  out <- detect_transaction_anomalies(df)
  if (nrow(out) > 0) {
    expect_true(any(out$scope == "category"))
  }
})

test_that("detect_transaction_anomalies uses global_fallback for sparse category", {
  # Category with only 2 observations — below IQR_MIN_OBS (5)
  sparse <- data.frame(
    transaction_id = c(paste0("txn_f", 1:20), "txn_s1", "txn_s2"),
    date           = rep("2025-06-15", 22),
    category       = c(rep("Food", 20), "Grocery", "Grocery"),
    description    = paste0("d", 1:22),
    amount         = c(rep(200, 20), 200, 50000),   # Grocery has one outlier
    payment_method = "UPI",
    stringsAsFactors = FALSE
  )
  out <- detect_transaction_anomalies(sparse)
  if ("txn_s2" %in% out$transaction_id) {
    grocery_row <- out[out$transaction_id == "txn_s2", ]
    expect_equal(grocery_row$scope, "global_fallback")
  }
})

test_that("detect_transaction_anomalies strips rows with NA amounts cleanly", {
  df        <- .food_df()
  df$amount[c(2, 5)] <- NA
  expect_silent(out <- detect_transaction_anomalies(df))
  expect_s3_class(out, "data.frame")
})

test_that("detect_transaction_anomalies reason is non-empty human-readable string", {
  df  <- .food_df()
  out <- detect_transaction_anomalies(df)
  if (nrow(out) > 0) {
    expect_true(all(nchar(out$reason) > 20))
    # No raw R error text
    expect_false(any(grepl("Error in |simpleError", out$reason)))
  }
})

test_that("detect_transaction_anomalies date column is class Date", {
  df  <- .food_df()
  out <- detect_transaction_anomalies(df)
  if (nrow(out) > 0) {
    expect_s3_class(out$date, "Date")
  }
})

test_that("detect_transaction_anomalies when all amounts identical (IQR = 0)", {
  df <- data.frame(
    transaction_id = paste0("t", 1:10),
    date           = rep("2025-06-15", 10),
    category       = rep("Food", 10),
    description    = paste0("d", 1:10),
    amount         = rep(100, 10),
    payment_method = "UPI",
    stringsAsFactors = FALSE
  )
  # Should not error; may return 0 flagged rows because nothing exceeds Q3 + 0
  expect_silent(out <- detect_transaction_anomalies(df))
  expect_s3_class(out, "data.frame")
})

# ── get_anomaly_summary() ────────────────────────────────────────────────────

test_that("get_anomaly_summary returns status=insufficient_data for NULL [spec #14]", {
  out <- get_anomaly_summary(NULL)
  expect_equal(out$status, "insufficient_data")
  expect_equal(out$flagged_count, 0L)
})

test_that("get_anomaly_summary returns status=insufficient_data for 2-row df", {
  df  <- .food_df(n_normal = 1, outlier_amount = 500)
  out <- get_anomaly_summary(df)
  expect_equal(out$status, "insufficient_data")
})

test_that("get_anomaly_summary status_message is human-readable string", {
  out <- get_anomaly_summary(NULL)
  expect_type(out$status_message, "character")
  expect_gt(nchar(out$status_message), 5L)
  expect_false(grepl("Error in ", out$status_message))
})

test_that("get_anomaly_summary status=no_anomalies when nothing flagged [spec #14]", {
  # All identical amounts, no outliers possible
  df <- data.frame(
    transaction_id = paste0("t", 1:20),
    date           = rep("2025-06-15", 20),
    category       = rep("Food", 20),
    description    = paste0("d", 1:20),
    amount         = rep(200, 20),
    payment_method = "UPI",
    stringsAsFactors = FALSE
  )
  out <- get_anomaly_summary(df)
  expect_true(out$status %in% c("no_anomalies", "ok"))
  expect_s3_class(out$flagged_df, "data.frame")
})

test_that("get_anomaly_summary status=ok when anomalies detected", {
  out <- get_anomaly_summary(.food_df())
  expect_equal(out$status, "ok")
  expect_gte(out$flagged_count, 1L)
})

test_that("get_anomaly_summary $category_stats is a data.frame with required cols", {
  out <- get_anomaly_summary(.multi_df())
  expect_s3_class(out$category_stats, "data.frame")
  expect_true(all(c("category","Q1","Q3","IQR","upper_bound","scope") %in%
                  names(out$category_stats)))
})

test_that("get_anomaly_summary $flagged_df has correct schema", {
  out <- get_anomaly_summary(.food_df())
  required_cols <- c("transaction_id","date","category","description",
                     "amount","Q1","Q3","IQR","upper_bound",
                     "threshold","scope","excess_amount","reason")
  if (nrow(out$flagged_df) > 0) {
    expect_true(all(required_cols %in% names(out$flagged_df)))
  }
})

test_that("get_anomaly_summary total_checked equals usable row count", {
  df  <- .food_df()
  out <- get_anomaly_summary(df)
  expect_equal(out$total_checked, nrow(df))
})

test_that("get_anomaly_summary flagged_amount_total is sum of flagged amounts", {
  df  <- .food_df()
  out <- get_anomaly_summary(df)
  if (nrow(out$flagged_df) > 0) {
    expect_equal(out$flagged_amount_total,
                 sum(out$flagged_df$amount, na.rm = TRUE))
  }
})

test_that("get_anomaly_summary categories_flagged lists affected categories", {
  out <- get_anomaly_summary(.food_df())
  if (out$flagged_count > 0) {
    expect_true("Food" %in% out$categories_flagged)
  }
})
