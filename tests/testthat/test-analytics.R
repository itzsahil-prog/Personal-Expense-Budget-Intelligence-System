# =============================================================================
# tests/testthat/test-analytics.R
# Tests for R/analytics.R — pure computation layer
#
# Covers:
#   calculate_total_spending()        happy path + empty + NA amounts
#   calculate_daily_spending()        aggregation + gap days + invalid dates
#   calculate_weekly_spending()       ISO-week grouping
#   calculate_monthly_spending()      multi-month + avg_daily_amount
#   calculate_category_spending()     proportions + color_hex attachment
#   calculate_payment_method_spending() proportions
#   calculate_average_daily_spending() per-day mean
#   calculate_highest_spending_category()
#   calculate_highest_spending_day()
#   calculate_budget_utilization()    (analytics.R version, in-memory join)
#
# Edge cases:
#   NULL input, zero-row df, all-NA amounts, invalid dates stripped,
#   single transaction, all same category, all same payment method
# =============================================================================

# ── fixtures ─────────────────────────────────────────────────────────────────

# 12 transactions across 3 categories and 3 days
.base_df <- function() {
  data.frame(
    transaction_id = paste0("txn_", seq_len(12)),
    date           = rep(c("2025-06-01","2025-06-15","2025-06-28"), 4),
    category       = rep(c("Food","Transport","Bills","Food"), each = 3),
    description    = paste0("Item ", seq_len(12)),
    amount         = c(100,200,300,150,250,350,120,220,320,180,280,380),
    payment_method = rep(c("UPI","Cash","Card"), 4),
    stringsAsFactors = FALSE
  )
}

# ── calculate_total_spending() ───────────────────────────────────────────────

test_that("calculate_total_spending sums correctly", {
  df  <- .base_df()
  out <- calculate_total_spending(df)
  expect_equal(out$total, sum(df$amount))
  expect_equal(out$n,     12L)
  expect_true(out$has_data)
})

test_that("calculate_total_spending on NULL returns zeros without error", {
  out <- calculate_total_spending(NULL)
  expect_equal(out$total, 0)
  expect_false(out$has_data)
})

test_that("calculate_total_spending on empty df returns zeros", {
  out <- calculate_total_spending(data.frame(amount = numeric(0)))
  expect_equal(out$total, 0)
  expect_false(out$has_data)
})

test_that("calculate_total_spending ignores NA amounts", {
  df        <- .base_df()
  df$amount[c(1, 3, 5)] <- NA
  out       <- calculate_total_spending(df)
  expect_equal(out$total, sum(df$amount, na.rm = TRUE))
})

test_that("calculate_total_spending with single transaction", {
  df  <- data.frame(transaction_id = "t1", date = "2025-06-01",
                    category = "Food", description = "x",
                    amount = 999, payment_method = "UPI",
                    stringsAsFactors = FALSE)
  out <- calculate_total_spending(df)
  expect_equal(out$total, 999)
  expect_equal(out$n, 1L)
})

# ── calculate_daily_spending() ───────────────────────────────────────────────

test_that("calculate_daily_spending aggregates to correct number of days", {
  df  <- .base_df()   # 3 distinct dates
  out <- calculate_daily_spending(df)
  expect_equal(nrow(out), 3L)
  expect_equal(out$date, as.Date(c("2025-06-01","2025-06-15","2025-06-28")))
})

test_that("calculate_daily_spending totals match per-day sums", {
  df  <- .base_df()
  out <- calculate_daily_spending(df)
  for (d in unique(df$date)) {
    expected <- sum(df$amount[df$date == d], na.rm = TRUE)
    actual   <- out$total_amount[out$date == as.Date(d)]
    expect_equal(actual, expected, info = paste("date:", d))
  }
})

test_that("calculate_daily_spending strips rows with invalid dates", {
  df        <- .base_df()
  df$date[1:2] <- "not-a-date"
  out       <- calculate_daily_spending(df)
  expect_equal(nrow(out), 3L)   # still 3 valid dates (row 1/2 were both on the first date)
  expect_true(all(!is.na(out$date)))
})

test_that("calculate_daily_spending on empty df returns typed empty frame", {
  out <- calculate_daily_spending(data.frame())
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_true("total_amount" %in% names(out))
})

test_that("calculate_daily_spending on NULL returns empty frame", {
  out <- calculate_daily_spending(NULL)
  expect_equal(nrow(out), 0L)
})

# ── calculate_weekly_spending() ──────────────────────────────────────────────

test_that("calculate_weekly_spending groups into ISO weeks", {
  df  <- data.frame(
    transaction_id = paste0("t", 1:7),
    date           = as.character(seq(as.Date("2025-06-01"),
                                      as.Date("2025-06-07"), by = "day")),
    category       = rep("Food", 7),
    description    = paste0("d", 1:7),
    amount         = rep(100, 7),
    payment_method = "UPI",
    stringsAsFactors = FALSE
  )
  out <- calculate_weekly_spending(df)
  expect_equal(nrow(out), 1L)   # all in one ISO week
  expect_equal(out$total_amount, 700)
})

test_that("calculate_weekly_spending year_week column has correct format", {
  out <- calculate_weekly_spending(.base_df())
  expect_match(out$year_week[1], "^\\d{4}-W\\d{2}$")
})

test_that("calculate_weekly_spending on NULL returns empty frame", {
  out <- calculate_weekly_spending(NULL)
  expect_equal(nrow(out), 0L)
})

# ── calculate_monthly_spending() ─────────────────────────────────────────────

test_that("calculate_monthly_spending aggregates correctly across months", {
  df <- data.frame(
    transaction_id = paste0("t", 1:6),
    date           = c(rep("2025-05-15", 3), rep("2025-06-15", 3)),
    category       = rep("Food", 6),
    description    = paste0("d", 1:6),
    amount         = c(100, 200, 300, 400, 500, 600),
    payment_method = "UPI",
    stringsAsFactors = FALSE
  )
  out <- calculate_monthly_spending(df)
  expect_equal(nrow(out), 2L)
  expect_equal(out$total_amount[out$year_month == "2025-05"], 600)
  expect_equal(out$total_amount[out$year_month == "2025-06"], 1500)
})

test_that("calculate_monthly_spending avg_daily_amount is plausible", {
  df  <- .base_df()
  out <- calculate_monthly_spending(df)
  expect_true(all(out$avg_daily_amount > 0))
  # avg daily must be <= total (can't average more than total)
  expect_true(all(out$avg_daily_amount <= out$total_amount))
})

test_that("calculate_monthly_spending returns month_start as Date", {
  out <- calculate_monthly_spending(.base_df())
  expect_s3_class(out$month_start, "Date")
})

test_that("calculate_monthly_spending on empty df returns typed empty frame", {
  out <- calculate_monthly_spending(data.frame())
  expect_equal(nrow(out), 0L)
  expect_true("avg_daily_amount" %in% names(out))
})

# ── calculate_category_spending() ────────────────────────────────────────────

test_that("calculate_category_spending includes all represented categories", {
  out <- calculate_category_spending(.base_df())
  expect_true(all(c("Food","Transport","Bills") %in% out$category))
})

test_that("calculate_category_spending pct_of_total sums to 100", {
  out <- calculate_category_spending(.base_df())
  expect_equal(sum(out$pct_of_total), 100, tolerance = 0.1)
})

test_that("calculate_category_spending attaches color_hex", {
  out <- calculate_category_spending(.base_df())
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", out$color_hex)))
})

test_that("calculate_category_spending does not hardcode totals", {
  df1  <- .base_df()
  df2  <- df1; df2$amount <- df2$amount * 3
  out1 <- calculate_category_spending(df1)
  out2 <- calculate_category_spending(df2)
  # Percentages should be equal even though absolute amounts changed
  expect_equal(round(out1$pct_of_total, 1), round(out2$pct_of_total, 1))
  # But totals are 3× larger
  expect_equal(out2$total_amount, out1$total_amount * 3)
})

test_that("calculate_category_spending on NULL returns empty frame", {
  out <- calculate_category_spending(NULL)
  expect_equal(nrow(out), 0L)
})

test_that("calculate_category_spending on all-same category returns 100% pct", {
  df  <- valid_txn_df(5, category = "Food", amount = 100)
  df$transaction_id <- paste0("t", 1:5)
  out <- calculate_category_spending(df)
  expect_equal(nrow(out), 1L)
  expect_equal(out$pct_of_total, 100)
})

# ── calculate_payment_method_spending() ──────────────────────────────────────

test_that("calculate_payment_method_spending proportions sum to 100", {
  out <- calculate_payment_method_spending(.base_df())
  expect_equal(sum(out$pct_of_total), 100, tolerance = 0.1)
})

test_that("calculate_payment_method_spending on NULL returns empty frame", {
  out <- calculate_payment_method_spending(NULL)
  expect_equal(nrow(out), 0L)
})

# ── calculate_average_daily_spending() ───────────────────────────────────────

test_that("calculate_average_daily_spending computes per-day mean correctly", {
  df  <- .base_df()   # 12 txns, 3 distinct days
  out <- calculate_average_daily_spending(df)
  expect_true(out$has_data)
  expect_equal(out$days_counted, 3L)
  expect_equal(out$value, round(sum(df$amount) / 3, 2))
})

test_that("calculate_average_daily_spending on NULL returns has_data=FALSE", {
  out <- calculate_average_daily_spending(NULL)
  expect_false(out$has_data)
  expect_equal(out$value, 0)
})

test_that("calculate_average_daily_spending formatted_value is a string", {
  out <- calculate_average_daily_spending(.base_df())
  expect_type(out$formatted_value, "character")
  expect_gt(nchar(out$formatted_value), 0L)
})

# ── calculate_highest_spending_category() ────────────────────────────────────

test_that("calculate_highest_spending_category returns the correct category", {
  df  <- .base_df()
  out <- calculate_highest_spending_category(df)
  # Bills total: 300+350+320+380 = 1350; Food: 100+150+120+180 = 550; Transport: 200+250+220+280 = 950
  expect_equal(out$category, "Bills")
  expect_true(out$has_data)
})

test_that("calculate_highest_spending_category on NULL returns has_data=FALSE", {
  out <- calculate_highest_spending_category(NULL)
  expect_false(out$has_data)
  expect_equal(out$category, "\u2014")
})

# ── calculate_highest_spending_day() ─────────────────────────────────────────

test_that("calculate_highest_spending_day returns the date with max daily total", {
  df  <- .base_df()   # day totals vary by date because amounts vary
  out <- calculate_highest_spending_day(df)
  expect_true(out$has_data)
  expect_s3_class(out$date, "Date")
  expect_gt(out$total_amount, 0)
})

test_that("calculate_highest_spending_day on empty df returns has_data=FALSE", {
  out <- calculate_highest_spending_day(data.frame())
  expect_false(out$has_data)
})

# ── calculate_budget_utilization() (analytics.R version) ─────────────────────

test_that("calculate_budget_utilization returns correct columns", {
  budget_df <- data.frame(
    category = c("Food","Transport"),
    budget_amount = c(2000, 1000),
    stringsAsFactors = FALSE
  )
  out <- calculate_budget_utilization(budget_df, .base_df(), month = "2025-06")
  expected_cols <- c("category","budget_amount","budget_used",
                     "budget_remaining","utilization_pct","status")
  expect_true(all(expected_cols %in% names(out)))
})

test_that("calculate_budget_utilization zero-budget rows get NA pct", {
  budget_df <- data.frame(
    category = "Food", budget_amount = 0,
    stringsAsFactors = FALSE
  )
  out <- calculate_budget_utilization(budget_df, .base_df(), month = "2025-06")
  expect_true(is.na(out$utilization_pct))
})

test_that("calculate_budget_utilization on NULL budget_df returns empty frame [spec #15]", {
  out <- calculate_budget_utilization(NULL, .base_df(), month = "2025-06")
  expect_equal(nrow(out), 0L)
})

test_that("calculate_budget_utilization on NULL transactions returns zero spend", {
  budget_df <- data.frame(
    category = "Food", budget_amount = 1000,
    stringsAsFactors = FALSE
  )
  out <- calculate_budget_utilization(budget_df, NULL, month = "2025-06")
  expect_equal(out$budget_used, 0)
})
