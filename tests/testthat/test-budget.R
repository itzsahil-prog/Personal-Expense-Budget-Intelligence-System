# =============================================================================
# tests/testthat/test-budget.R
# Tests for R/budget.R
#
# Covers:
#   get_monthly_budget()
#   calculate_budget_status()
#   calculate_budget_remaining()
#   calculate_budget_utilization()
#   get_over_budget_categories()
#
# Error/edge cases:
#   [15] No budget configured — all functions return typed empty frames
#   Bad month format
#   Zero-budget categories (NA pct)
#   Configurable thresholds
#   UNDER_BUDGET / NEAR_LIMIT / OVER_BUDGET classification
#   Spending exactly at boundary
# =============================================================================

# ── fixtures ─────────────────────────────────────────────────────────────────

.budget_df <- function() {
  data.frame(
    category     = c("Food","Grocery","Transport"),
    budget_amount = c(5000, 3000, 2000),
    stringsAsFactors = FALSE
  )
}

# Transactions in "2025-06" that produce known utilization rates:
#   Food     : 4000 / 5000 = 80 %  → NEAR_LIMIT (boundary)
#   Grocery  : 1500 / 3000 = 50 %  → UNDER_BUDGET
#   Transport: 2200 / 2000 = 110%  → OVER_BUDGET
.txn_df <- function() {
  data.frame(
    transaction_id = paste0("t", 1:7),
    date           = c(rep("2025-06-10", 3), rep("2025-06-20", 4)),
    category       = c("Food","Grocery","Transport","Food","Food","Food","Transport"),
    description    = paste0("Item ", 1:7),
    amount         = c(1000, 1500, 1000, 1000, 1000, 1000, 1200),
    payment_method = "UPI",
    stringsAsFactors = FALSE
  )
}

# ── get_monthly_budget() ─────────────────────────────────────────────────────

test_that("get_monthly_budget returns correct columns", {
  out <- get_monthly_budget(.budget_df(), "2025-06")
  expect_true(all(c("month","category","budget_amount","color_hex","icon") %in%
                  names(out)))
})

test_that("get_monthly_budget attaches colour and icon from validation.R", {
  out <- get_monthly_budget(.budget_df(), "2025-06")
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", out$color_hex)))
  expect_true(all(nchar(out$icon) > 0))
})

test_that("get_monthly_budget returns empty frame for NULL budget_df [spec #15]", {
  out <- get_monthly_budget(NULL, "2025-06")
  expect_equal(nrow(out), 0L)
  expect_true("budget_amount" %in% names(out))
})

test_that("get_monthly_budget warns and returns empty for invalid month", {
  expect_warning(out <- get_monthly_budget(.budget_df(), "June-2025"))
  expect_equal(nrow(out), 0L)
})

test_that("get_monthly_budget month is stored in the result rows", {
  out <- get_monthly_budget(.budget_df(), "2025-06")
  expect_true(all(out$month == "2025-06"))
})

# ── calculate_budget_status() ────────────────────────────────────────────────

test_that("calculate_budget_status returns all required columns", {
  out <- calculate_budget_status(.budget_df(), .txn_df(), "2025-06")
  expected <- c("category","budget","actual_spending","remaining",
                "utilization_pct","utilization_ratio","status")
  expect_true(all(expected %in% names(out)))
})

test_that("calculate_budget_status OVER_BUDGET classification is correct", {
  out  <- calculate_budget_status(.budget_df(), .txn_df(), "2025-06")
  over <- out[out$category == "Transport", ]
  expect_equal(over$status, BUDGET_STATUS_OVER)
  expect_lt(over$remaining, 0)
})

test_that("calculate_budget_status NEAR_LIMIT classification at 80% boundary", {
  out  <- calculate_budget_status(.budget_df(), .txn_df(), "2025-06")
  near <- out[out$category == "Food", ]
  # 4000/5000 = 80 % exactly — default threshold is >= 80 → NEAR_LIMIT
  expect_equal(near$status, BUDGET_STATUS_NEAR)
  expect_equal(near$utilization_pct, 80)
})

test_that("calculate_budget_status UNDER_BUDGET for 50% utilization", {
  out   <- calculate_budget_status(.budget_df(), .txn_df(), "2025-06")
  under <- out[out$category == "Grocery", ]
  expect_equal(under$status, BUDGET_STATUS_UNDER)
})

test_that("calculate_budget_status uses constant values, not magic strings", {
  out <- calculate_budget_status(.budget_df(), .txn_df(), "2025-06")
  expect_true(all(out$status %in% c(BUDGET_STATUS_UNDER,
                                     BUDGET_STATUS_NEAR,
                                     BUDGET_STATUS_OVER)))
})

test_that("calculate_budget_status remaining is budget minus actual_spending", {
  out <- calculate_budget_status(.budget_df(), .txn_df(), "2025-06")
  for (i in seq_len(nrow(out))) {
    expect_equal(out$remaining[i],
                 out$budget[i] - out$actual_spending[i],
                 tolerance = 0.01)
  }
})

test_that("calculate_budget_status configurable near_threshold respected", {
  # Raise threshold to 0.90 — Food at 80% should now be UNDER
  out  <- calculate_budget_status(.budget_df(), .txn_df(), "2025-06",
                                   near_threshold = 0.90)
  food <- out[out$category == "Food", ]
  expect_equal(food$status, BUDGET_STATUS_UNDER)
})

test_that("calculate_budget_status configurable over_threshold respected", {
  # Lower threshold to 0.50 — Grocery at 50% should now be OVER
  out <- calculate_budget_status(.budget_df(), .txn_df(), "2025-06",
                                  near_threshold = 0.40,
                                  over_threshold = 0.50)
  groc <- out[out$category == "Grocery", ]
  expect_equal(groc$status, BUDGET_STATUS_OVER)
})

test_that("calculate_budget_status NULL budget_df returns empty frame [spec #15]", {
  out <- calculate_budget_status(NULL, .txn_df(), "2025-06")
  expect_equal(nrow(out), 0L)
})

test_that("calculate_budget_status NULL transactions_df gives zero spending", {
  out <- calculate_budget_status(.budget_df(), NULL, "2025-06")
  expect_equal(nrow(out), 3L)
  expect_true(all(out$actual_spending == 0))
  expect_true(all(out$status == BUDGET_STATUS_UNDER))
})

test_that("calculate_budget_status invalid month returns empty frame with warning", {
  expect_warning(out <- calculate_budget_status(.budget_df(), .txn_df(), "bad"))
  expect_equal(nrow(out), 0L)
})

test_that("calculate_budget_status only counts transactions for the specified month", {
  txn_other_month <- .txn_df()
  txn_other_month$date <- "2025-07-10"   # different month
  out <- calculate_budget_status(.budget_df(), txn_other_month, "2025-06")
  # No June transactions, so all spending should be 0
  expect_true(all(out$actual_spending == 0))
})

test_that("calculate_budget_status with zero budget_amount gives NA utilization_pct", {
  bd <- data.frame(category = "Food", budget_amount = 0,
                   stringsAsFactors = FALSE)
  out <- calculate_budget_status(bd, .txn_df(), "2025-06")
  expect_true(is.na(out$utilization_pct))
})

# ── calculate_budget_remaining() ─────────────────────────────────────────────

test_that("calculate_budget_remaining returns correct columns only", {
  out <- calculate_budget_remaining(.budget_df(), .txn_df(), "2025-06")
  expect_named(out, c("category","budget","actual_spending","remaining"))
})

test_that("calculate_budget_remaining sorts ascending by remaining (most over first)", {
  out <- calculate_budget_remaining(.budget_df(), .txn_df(), "2025-06")
  # Transport is over budget, so its remaining is most negative
  expect_equal(out$category[1], "Transport")
  expect_lt(out$remaining[1], 0)
})

test_that("calculate_budget_remaining empty budget_df returns empty frame [spec #15]", {
  out <- calculate_budget_remaining(NULL, .txn_df(), "2025-06")
  expect_equal(nrow(out), 0L)
})

# ── calculate_budget_utilization() (budget.R) ────────────────────────────────

test_that("calculate_budget_utilization returns utilization columns", {
  out <- calculate_budget_utilization(.budget_df(), .txn_df(), "2025-06")
  expect_true(all(c("utilization_pct","utilization_ratio") %in% names(out)))
})

test_that("calculate_budget_utilization ratios are between 0 and >1 for over-budget", {
  out <- calculate_budget_utilization(.budget_df(), .txn_df(), "2025-06")
  transport_ratio <- out$utilization_ratio[out$category == "Transport"]
  expect_gt(transport_ratio, 1.0)
})

test_that("calculate_budget_utilization empty budget_df [spec #15]", {
  out <- calculate_budget_utilization(NULL, .txn_df(), "2025-06")
  expect_equal(nrow(out), 0L)
})

# ── get_over_budget_categories() ─────────────────────────────────────────────

test_that("get_over_budget_categories returns only OVER_BUDGET rows", {
  out <- get_over_budget_categories(.budget_df(), .txn_df(), "2025-06")
  expect_true(all(out$status == BUDGET_STATUS_OVER))
  expect_equal(nrow(out), 1L)
  expect_equal(out$category, "Transport")
})

test_that("get_over_budget_categories returns empty frame when nothing over [spec #15]", {
  # Use very generous budgets
  big_budget <- data.frame(
    category     = c("Food","Grocery","Transport"),
    budget_amount = c(100000, 100000, 100000),
    stringsAsFactors = FALSE
  )
  out <- get_over_budget_categories(big_budget, .txn_df(), "2025-06")
  expect_equal(nrow(out), 0L)
})

test_that("get_over_budget_categories returns empty frame for NULL budget [spec #15]", {
  out <- get_over_budget_categories(NULL, .txn_df(), "2025-06")
  expect_equal(nrow(out), 0L)
})

test_that("get_over_budget_categories respects custom over_threshold", {
  # Set threshold to 0.40 so Grocery (50%) also becomes over-budget
  out <- get_over_budget_categories(.budget_df(), .txn_df(), "2025-06",
                                     near_threshold = 0.30,
                                     over_threshold = 0.40)
  expect_true("Grocery" %in% out$category)
})

test_that("get_over_budget_categories over-budget rows sorted by remaining ASC", {
  # Create two over-budget categories with different excess amounts
  bd <- data.frame(
    category      = c("Food","Transport"),
    budget_amount  = c(100, 100),
    stringsAsFactors = FALSE
  )
  txn <- data.frame(
    transaction_id = c("t1","t2"),
    date           = c("2025-06-01","2025-06-01"),
    category       = c("Food","Transport"),
    description    = c("a","b"),
    amount         = c(500, 300),   # Food 400% over, Transport 200% over
    payment_method = "UPI",
    stringsAsFactors = FALSE
  )
  out <- get_over_budget_categories(bd, txn, "2025-06")
  # Most negative remaining first (Food at -400 < Transport at -200)
  expect_equal(out$category[1], "Food")
})
