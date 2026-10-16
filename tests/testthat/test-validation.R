# =============================================================================
# tests/testthat/test-validation.R
# Tests for R/validation.R
#
# Covers:
#   validate_transaction_input()   — all field-level rules
#   validate_import_file()         — structural + row-level checks
#   validate_transaction_data()    — wrapper return schema
#   clean_transaction_data()       — quarantine / normalise behaviour
#   Error cases from spec:
#     [2]  empty CSV / empty data.frame
#     [3]  invalid CSV content (wrong columns, bad values)
#     [5]  missing columns
#     [6]  invalid date
#     [7]  negative amount
#     [8]  zero amount
#     [9]  invalid category
#     [10] invalid payment method
#     [11] duplicate transaction (ID collision handled by normalise_import_df)
# =============================================================================

test_that("valid transaction input passes all checks", {
  result <- validate_transaction_input(valid_txn())
  expect_true(result$valid)
  expect_length(result$errors, 0L)
})

# ── date field ───────────────────────────────────────────────────────────────

test_that("missing date is rejected with a message", {
  r <- validate_transaction_input(valid_txn(date = ""))
  expect_false(r$valid)
  expect_true(any(grepl("Date", r$errors)))
})

test_that("NA date is rejected", {
  r <- validate_transaction_input(valid_txn(date = NA))
  expect_false(r$valid)
})

test_that("invalid date string is rejected [spec #6]", {
  r <- validate_transaction_input(valid_txn(date = "not-a-date"))
  expect_false(r$valid)
  expect_true(any(grepl("valid date", r$errors, ignore.case = TRUE)))
})

test_that("future date is rejected", {
  r <- validate_transaction_input(valid_txn(date = format(Sys.Date() + 10)))
  expect_false(r$valid)
  expect_true(any(grepl("future", r$errors, ignore.case = TRUE)))
})

test_that("date before 2000 is rejected", {
  r <- validate_transaction_input(valid_txn(date = "1999-12-31"))
  expect_false(r$valid)
})

# ── amount field ─────────────────────────────────────────────────────────────

test_that("negative amount is rejected [spec #7]", {
  r <- validate_transaction_input(valid_txn(amount = -100))
  expect_false(r$valid)
  expect_true(any(grepl("greater than zero", r$errors, ignore.case = TRUE)))
})

test_that("zero amount is rejected [spec #8]", {
  r <- validate_transaction_input(valid_txn(amount = 0))
  expect_false(r$valid)
  expect_true(any(grepl("greater than zero", r$errors, ignore.case = TRUE)))
})

test_that("NA amount is rejected", {
  r <- validate_transaction_input(valid_txn(amount = NA))
  expect_false(r$valid)
})

test_that("non-numeric amount is rejected", {
  r <- validate_transaction_input(valid_txn(amount = "abc"))
  expect_false(r$valid)
})

test_that("amount above MAX_TRANSACTION_AMOUNT is rejected", {
  r <- validate_transaction_input(valid_txn(amount = MAX_TRANSACTION_AMOUNT + 1))
  expect_false(r$valid)
  expect_true(any(grepl("maximum", r$errors, ignore.case = TRUE)))
})

test_that("fractional amount is accepted", {
  r <- validate_transaction_input(valid_txn(amount = 0.5))
  expect_true(r$valid)
})

# ── category field ───────────────────────────────────────────────────────────

test_that("invalid category is rejected [spec #9]", {
  r <- validate_transaction_input(valid_txn(category = "InvalidCat"))
  expect_false(r$valid)
  expect_true(any(grepl("not recognised", r$errors, ignore.case = TRUE)))
})

test_that("empty category is rejected", {
  r <- validate_transaction_input(valid_txn(category = ""))
  expect_false(r$valid)
})

test_that("all 10 valid categories are accepted", {
  for (cat in VALID_CATEGORIES) {
    r <- validate_transaction_input(valid_txn(category = cat))
    expect_true(r$valid, info = paste("Category failed:", cat))
  }
})

# ── payment method ───────────────────────────────────────────────────────────

test_that("invalid payment method is rejected [spec #10]", {
  r <- validate_transaction_input(valid_txn(payment_method = "Bitcoin"))
  expect_false(r$valid)
  expect_true(any(grepl("not recognised", r$errors, ignore.case = TRUE)))
})

test_that("empty payment method is rejected", {
  r <- validate_transaction_input(valid_txn(payment_method = ""))
  expect_false(r$valid)
})

test_that("all 4 valid payment methods are accepted", {
  for (pm in VALID_PAYMENT_METHODS) {
    r <- validate_transaction_input(valid_txn(payment_method = pm))
    expect_true(r$valid, info = paste("Payment method failed:", pm))
  }
})

# ── description field ────────────────────────────────────────────────────────

test_that("empty description is rejected", {
  r <- validate_transaction_input(valid_txn(description = ""))
  expect_false(r$valid)
})

test_that("description over 200 chars is rejected", {
  r <- validate_transaction_input(valid_txn(description = strrep("x", 201)))
  expect_false(r$valid)
  expect_true(any(grepl("200 char", r$errors, ignore.case = TRUE)))
})

test_that("description of exactly 200 chars is accepted", {
  r <- validate_transaction_input(valid_txn(description = strrep("x", 200)))
  expect_true(r$valid)
})

test_that("multiple validation errors are all reported", {
  r <- validate_transaction_input(
    valid_txn(date = "bad", amount = -1, category = "WRONG", payment_method = "X")
  )
  expect_false(r$valid)
  expect_gte(length(r$errors), 3L)
})

# ── validate_import_file() ───────────────────────────────────────────────────

test_that("NULL input is rejected structurally", {
  r <- validate_import_file(NULL)
  expect_false(r$valid)
  expect_gt(length(r$errors), 0L)
})

test_that("empty data.frame is rejected [spec #2]", {
  r <- validate_import_file(data.frame())
  expect_false(r$valid)
})

test_that("zero-row data.frame with correct columns is rejected [spec #2]", {
  df <- data.frame(date = character(), category = character(),
                   description = character(), amount = numeric(),
                   payment_method = character(), stringsAsFactors = FALSE)
  r <- validate_import_file(df)
  expect_false(r$valid)
  expect_match(r$errors[1], "no data rows", ignore.case = TRUE)
})

test_that("missing required columns reported individually [spec #5]", {
  df <- data.frame(date = "2025-01-01", category = "Food",
                   amount = 100, stringsAsFactors = FALSE)
  r <- validate_import_file(df)
  expect_false(r$valid)
  expect_true(any(grepl("description|payment_method", r$errors, ignore.case = TRUE)))
})

test_that("invalid date in import is captured as row error [spec #6]", {
  df <- data.frame(date = "not-a-date", category = "Food",
                   description = "Lunch", amount = "100",
                   payment_method = "UPI", stringsAsFactors = FALSE)
  r <- validate_import_file(df)
  expect_gt(nrow(r$row_errors), 0L)
  expect_true(any(r$row_errors$field == "date"))
})

test_that("negative amount in import is captured [spec #7]", {
  df <- data.frame(date = "2025-06-01", category = "Food",
                   description = "Test", amount = "-50",
                   payment_method = "UPI", stringsAsFactors = FALSE)
  r <- validate_import_file(df)
  expect_gt(nrow(r$row_errors), 0L)
  expect_true(any(r$row_errors$field == "amount"))
})

test_that("zero amount in import is captured [spec #8]", {
  df <- data.frame(date = "2025-06-01", category = "Food",
                   description = "Test", amount = "0",
                   payment_method = "UPI", stringsAsFactors = FALSE)
  r <- validate_import_file(df)
  expect_gt(nrow(r$row_errors), 0L)
  expect_true(any(r$row_errors$field == "amount"))
})

test_that("invalid category in import is captured [spec #9]", {
  df <- data.frame(date = "2025-06-01", category = "FakeCategory",
                   description = "Test", amount = "100",
                   payment_method = "UPI", stringsAsFactors = FALSE)
  r <- validate_import_file(df)
  expect_gt(nrow(r$row_errors), 0L)
  expect_true(any(r$row_errors$field == "category"))
})

test_that("invalid payment method in import is captured [spec #10]", {
  df <- data.frame(date = "2025-06-01", category = "Food",
                   description = "Test", amount = "100",
                   payment_method = "Crypto", stringsAsFactors = FALSE)
  r <- validate_import_file(df)
  expect_gt(nrow(r$row_errors), 0L)
  expect_true(any(r$row_errors$field == "payment_method"))
})

test_that("column names are case-insensitive", {
  df <- data.frame(DATE = "2025-06-01", CATEGORY = "Food",
                   DESCRIPTION = "Lunch", AMOUNT = "200",
                   PAYMENT_METHOD = "UPI", stringsAsFactors = FALSE)
  r <- validate_import_file(df)
  expect_true(r$valid)
})

test_that("mixed valid and invalid rows are both detected", {
  df <- data.frame(
    date           = c("2025-06-01", "not-a-date", "2025-06-03"),
    category       = c("Food",       "Food",       "INVALID"),
    description    = c("A",          "B",          "C"),
    amount         = c("100",        "200",        "300"),
    payment_method = c("UPI",        "UPI",        "UPI"),
    stringsAsFactors = FALSE
  )
  r <- validate_import_file(df)
  expect_false(r$valid)
  invalid_rows <- unique(r$row_errors$row)
  expect_true(2L %in% invalid_rows)   # bad date
  expect_true(3L %in% invalid_rows)   # bad category
  expect_false(1L %in% invalid_rows)  # row 1 is clean
})

# ── validate_transaction_data() ──────────────────────────────────────────────

test_that("validate_transaction_data returns correct schema fields", {
  df <- valid_txn_df()
  r  <- validate_transaction_data(df)
  expect_named(r, c("is_valid","total_rows","valid_row_count",
                     "invalid_row_count","structural_errors",
                     "row_errors","summary_message","detail_messages"))
})

test_that("validate_transaction_data is_valid TRUE for clean data", {
  r <- validate_transaction_data(valid_txn_df(5))
  expect_true(r$is_valid)
  expect_equal(r$total_rows, 5L)
  expect_equal(r$valid_row_count, 5L)
  expect_equal(r$invalid_row_count, 0L)
})

test_that("validate_transaction_data summary_message is human-readable", {
  r <- validate_transaction_data(valid_txn_df(3))
  expect_type(r$summary_message, "character")
  expect_gt(nchar(r$summary_message), 0L)
  # Should NOT contain raw R error strings
  expect_false(grepl("Error in |traceback|call stack", r$summary_message))
})

test_that("validate_transaction_data handles NULL without stopping", {
  r <- validate_transaction_data(NULL)
  expect_false(r$is_valid)
  expect_gt(nchar(r$summary_message), 0L)
})

# ── clean_transaction_data() ─────────────────────────────────────────────────

test_that("clean_transaction_data returns all-clean for valid input", {
  r <- clean_transaction_data(valid_txn_df(5))
  expect_equal(r$clean_count, 5L)
  expect_equal(r$discarded_count, 0L)
  expect_false(r$is_partial)
  expect_true(r$has_clean)
})

test_that("clean_transaction_data quarantines invalid rows, never drops silently", {
  df <- rbind(
    valid_txn_df(3),
    data.frame(date = "bad", category = "Food", description = "X",
               amount = "100", payment_method = "UPI",
               stringsAsFactors = FALSE)
  )
  r <- clean_transaction_data(df)
  expect_equal(r$total_rows, 4L)
  expect_equal(r$clean_count + r$discarded_count, 4L)  # every row accounted for
  expect_true(r$is_partial)
  expect_true("validation_errors" %in% names(r$discarded_df))
})

test_that("clean_transaction_data empty input returns structured result", {
  r <- clean_transaction_data(data.frame())
  expect_equal(r$total_rows, 0L)
  expect_false(r$has_clean)
  expect_match(r$summary_message, "No data", ignore.case = TRUE)
})

test_that("clean data has correct output columns after normalisation", {
  r <- clean_transaction_data(valid_txn_df(2))
  expected_cols <- c("transaction_id","date","category","description",
                     "amount","payment_method","created_at","updated_at")
  expect_true(all(expected_cols %in% names(r$clean_df)))
})

test_that("all-invalid input results in zero clean rows", {
  df <- data.frame(
    date = c("bad1","bad2"), category = c("WRONG","WRONG"),
    description = c("x","y"), amount = c("-1","-2"),
    payment_method = c("X","Y"), stringsAsFactors = FALSE
  )
  r <- clean_transaction_data(df)
  expect_equal(r$clean_count, 0L)
  expect_false(r$has_clean)
  expect_equal(r$discarded_count, 2L)
})

# ── generate_transaction_id() ────────────────────────────────────────────────

test_that("generate_transaction_id produces correct format", {
  id <- generate_transaction_id("2025-06-15")
  expect_match(id, "^txn_20250615_[0-9a-f]{6}$")
})

test_that("generate_transaction_id produces unique IDs", {
  ids <- replicate(100, generate_transaction_id("2025-06-15"))
  expect_equal(length(unique(ids)), 100L)
})
