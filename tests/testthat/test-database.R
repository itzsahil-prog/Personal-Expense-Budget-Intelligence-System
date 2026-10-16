# =============================================================================
# tests/testthat/test-database.R
# Tests for R/database.R
#
# Covers (spec error cases):
#   [1]  Empty database — reads return typed empty frames, not errors
#   [11] Duplicate transaction — second insert with same ID rejected by SQLite
#   [12] Database unavailable — db_health_check() returns FALSE, never stops
#   [16] Database connection failure — connect to read-only / bad path handled
#
# Functional coverage:
#   initialize_database()       idempotency
#   insert_transaction()        success + every field-level rejection
#   update_transaction()        success + unknown ID rejection
#   delete_transaction()        success + unknown ID rejection
#   get_transactions()          empty result, filters, order_by whitelist
#   get_transaction_by_id()     hit + miss
#   insert_budget() / get_budgets() / update_budget()
#   db_health_check()
#   db_bulk_insert_transactions()
# =============================================================================

# ── helpers ──────────────────────────────────────────────────────────────────

new_db <- function() {
  con <- fresh_db()
  withr::defer(disconnect_database(con), envir = parent.frame())
  con
}

# ── initialize_database() ────────────────────────────────────────────────────

test_that("initialize_database creates all four required tables", {
  con    <- fresh_db()
  on.exit(disconnect_database(con))
  tables <- DBI::dbListTables(con)
  expect_true(all(c("transactions","budgets","categories","settings") %in% tables))
})

test_that("initialize_database is idempotent (safe to call twice)", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_silent(initialize_database(con))
  expect_silent(initialize_database(con))
  expect_true(db_health_check(con))
})

test_that("initialize_database seeds default settings", {
  con      <- fresh_db()
  on.exit(disconnect_database(con))
  settings <- get_settings(con)
  expect_true("currency" %in% names(settings))
  expect_true("forecast_method" %in% names(settings))
})

test_that("initialize_database seeds all 10 categories", {
  con  <- fresh_db()
  on.exit(disconnect_database(con))
  cats <- get_categories(con)
  expect_equal(nrow(cats), 10L)
  expect_true(all(VALID_CATEGORIES %in% cats$category_name))
})

# ── empty database [spec #1] ─────────────────────────────────────────────────

test_that("get_transactions on empty DB returns typed empty data.frame [spec #1]", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  df  <- get_transactions(con)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
  expect_true(all(c("transaction_id","date","category","description",
                     "amount","payment_method") %in% names(df)))
})

test_that("get_budgets on empty DB returns typed empty data.frame [spec #1]", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  df  <- get_budgets(con)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
  expect_true("budget_amount" %in% names(df))
})

test_that("get_summary_counts on empty DB returns zero, not an error [spec #1]", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  s   <- get_summary_counts(con)
  expect_equal(s$total_transactions, 0L)
})

# ── insert_transaction() ─────────────────────────────────────────────────────

test_that("insert_transaction returns a txn_* ID on success", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  id  <- insert_transaction(con, "2025-06-15", "Food", "Lunch", 250, "UPI")
  expect_match(id, "^txn_")
})

test_that("inserted transaction is retrievable", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  id  <- insert_transaction(con, "2025-06-15", "Food", "Lunch", 250, "UPI")
  row <- get_transaction_by_id(con, id)
  expect_equal(nrow(row), 1L)
  expect_equal(row$category, "Food")
  expect_equal(row$amount,   250)
})

test_that("insert_transaction rejects invalid date [spec #6]", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_error(
    insert_transaction(con, "not-a-date", "Food", "Test", 100, "UPI"),
    regexp = "date"
  )
})

test_that("insert_transaction rejects future date", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_error(
    insert_transaction(con, format(Sys.Date() + 5), "Food", "Test", 100, "UPI"),
    regexp = "future"
  )
})

test_that("insert_transaction rejects negative amount [spec #7]", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_error(
    insert_transaction(con, "2025-06-15", "Food", "Test", -50, "UPI"),
    regexp = "greater than zero"
  )
})

test_that("insert_transaction rejects zero amount [spec #8]", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_error(
    insert_transaction(con, "2025-06-15", "Food", "Test", 0, "UPI"),
    regexp = "greater than zero"
  )
})

test_that("insert_transaction rejects invalid category [spec #9]", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_error(
    insert_transaction(con, "2025-06-15", "FakeCategory", "Test", 100, "UPI"),
    regexp = "not valid"
  )
})

test_that("insert_transaction rejects invalid payment method [spec #10]", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_error(
    insert_transaction(con, "2025-06-15", "Food", "Test", 100, "Crypto"),
    regexp = "not valid"
  )
})

test_that("insert_transaction rejects description over 200 chars", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_error(
    insert_transaction(con, "2025-06-15", "Food", strrep("x", 201), 100, "UPI"),
    regexp = "200"
  )
})

test_that("error messages from insert_transaction are strings, not raw R conditions", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  err_msg <- tryCatch(
    insert_transaction(con, "bad", "Food", "Test", 100, "UPI"),
    error = function(e) conditionMessage(e)
  )
  expect_type(err_msg, "character")
  expect_false(grepl("Error in DBI::", err_msg))   # no raw DBI message exposed
  expect_gt(nchar(err_msg), 10L)
})

# ── duplicate transaction [spec #11] ─────────────────────────────────────────

test_that("duplicate transaction_id is rejected by SQLite [spec #11]", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  id  <- insert_transaction(con, "2025-06-15", "Food", "Lunch 1", 250, "UPI")
  # Force-insert same ID via DBI to test the constraint (not the R layer)
  expect_error(
    DBI::dbExecute(con,
      "INSERT INTO transactions
         (transaction_id, date, category, description, amount, payment_method)
       VALUES (?, ?, ?, ?, ?, ?);",
      params = list(id, "2025-06-15", "Food", "Lunch dup", 300, "Cash")
    )
  )
})

# ── update_transaction() ─────────────────────────────────────────────────────

test_that("update_transaction changes fields correctly", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  id  <- insert_transaction(con, "2025-06-15", "Food", "Lunch", 250, "UPI")
  expect_true(
    update_transaction(con, id, "2025-06-16", "Grocery", "Bread", 80, "Cash")
  )
  row <- get_transaction_by_id(con, id)
  expect_equal(row$category,       "Grocery")
  expect_equal(row$amount,         80)
  expect_equal(row$payment_method, "Cash")
  expect_equal(row$date,           "2025-06-16")
})

test_that("update_transaction on nonexistent ID throws descriptive error", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_error(
    update_transaction(con, "txn_99999999_zzzzzz", "2025-06-15",
                       "Food", "Test", 100, "UPI"),
    regexp = "not found"
  )
})

test_that("update_transaction rejects invalid amount [spec #7 / #8]", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  id  <- insert_transaction(con, "2025-06-15", "Food", "Lunch", 250, "UPI")
  expect_error(
    update_transaction(con, id, "2025-06-15", "Food", "Lunch", 0, "UPI"),
    regexp = "greater than zero"
  )
})

# ── delete_transaction() ─────────────────────────────────────────────────────

test_that("delete_transaction removes the row", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  id  <- insert_transaction(con, "2025-06-15", "Food", "Lunch", 250, "UPI")
  expect_true(delete_transaction(con, id))
  expect_equal(nrow(get_transaction_by_id(con, id)), 0L)
})

test_that("delete_transaction on nonexistent ID throws descriptive error", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_error(
    delete_transaction(con, "txn_00000000_xxxxxx"),
    regexp = "not found"
  )
})

# ── get_transactions() filters ───────────────────────────────────────────────

test_that("get_transactions date_from filter works", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  insert_transaction(con, "2025-01-10", "Food", "A", 100, "UPI")
  insert_transaction(con, "2025-06-15", "Food", "B", 200, "UPI")
  df <- get_transactions(con, date_from = "2025-06-01")
  expect_equal(nrow(df), 1L)
  expect_equal(df$description, "B")
})

test_that("get_transactions date_to filter works", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  insert_transaction(con, "2025-01-10", "Food", "A", 100, "UPI")
  insert_transaction(con, "2025-06-15", "Food", "B", 200, "UPI")
  df <- get_transactions(con, date_to = "2025-03-01")
  expect_equal(nrow(df), 1L)
  expect_equal(df$description, "A")
})

test_that("get_transactions category filter works", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  insert_transaction(con, "2025-06-01", "Food",      "A", 100, "UPI")
  insert_transaction(con, "2025-06-02", "Transport", "B", 200, "UPI")
  df <- get_transactions(con, categories = "Transport")
  expect_equal(nrow(df), 1L)
  expect_equal(df$category, "Transport")
})

test_that("get_transactions search_text filter is case-insensitive", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  insert_transaction(con, "2025-06-01", "Food", "Lunch at Haldiram", 150, "UPI")
  insert_transaction(con, "2025-06-02", "Food", "Dinner out",        200, "UPI")
  df <- get_transactions(con, search_text = "haldiram")
  expect_equal(nrow(df), 1L)
})

test_that("get_transactions invalid order_by silently falls back to default", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  insert_transaction(con, "2025-06-01", "Food", "A", 100, "UPI")
  # Should not error; should still return data
  expect_silent(df <- get_transactions(con, order_by = "DROP TABLE transactions;"))
  expect_equal(nrow(df), 1L)
})

# ── budget functions ──────────────────────────────────────────────────────────

test_that("insert_budget and get_budgets round-trip correctly", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  bid <- insert_budget(con, "2025-06", "Food", 5000)
  expect_type(bid, "integer")
  df  <- get_budgets(con, month = "2025-06")
  expect_equal(nrow(df), 1L)
  expect_equal(df$budget_amount, 5000)
})

test_that("insert_budget upserts on (month, category) collision", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  insert_budget(con, "2025-06", "Food", 5000)
  insert_budget(con, "2025-06", "Food", 6000)   # should update, not duplicate
  df <- get_budgets(con, month = "2025-06")
  expect_equal(nrow(df), 1L)
  expect_equal(df$budget_amount, 6000)
})

test_that("update_budget changes amount", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  bid <- insert_budget(con, "2025-06", "Food", 5000)
  update_budget(con, bid, 7500)
  df  <- get_budgets(con, month = "2025-06")
  expect_equal(df$budget_amount, 7500)
})

test_that("insert_budget rejects invalid month format", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_error(insert_budget(con, "June-2025", "Food", 5000), regexp = "YYYY-MM")
})

test_that("insert_budget rejects invalid category [spec #9]", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_error(insert_budget(con, "2025-06", "InvalidCat", 5000), regexp = "not valid")
})

test_that("insert_budget rejects negative budget_amount", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_error(insert_budget(con, "2025-06", "Food", -100), regexp = "negative")
})

# ── db_health_check() [spec #12 / #16] ───────────────────────────────────────

test_that("db_health_check returns TRUE on healthy connection [spec #12]", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_true(db_health_check(con))
})

test_that("db_health_check returns FALSE on NULL connection [spec #16]", {
  expect_false(db_health_check(NULL))
})

test_that("db_health_check returns FALSE on closed connection [spec #12]", {
  con <- fresh_db()
  disconnect_database(con)
  expect_false(db_health_check(con))
})

test_that("db_health_check never throws even with invalid input [spec #16]", {
  expect_false(db_health_check("not_a_connection"))
  expect_false(db_health_check(list()))
  expect_false(db_health_check(42L))
})

# ── db_bulk_insert_transactions() ────────────────────────────────────────────

test_that("db_bulk_insert_transactions inserts all clean rows", {
  con   <- fresh_db()
  on.exit(disconnect_database(con))
  clean <- clean_transaction_data(valid_txn_df(5))$clean_df
  n     <- db_bulk_insert_transactions(con, clean)
  expect_equal(n, 5L)
  expect_equal(nrow(get_transactions(con)), 5L)
})

test_that("db_bulk_insert_transactions with NULL returns 0 without error", {
  con <- fresh_db()
  on.exit(disconnect_database(con))
  expect_equal(db_bulk_insert_transactions(con, NULL), 0L)
})

test_that("db_bulk_insert_transactions silently skips duplicate IDs [spec #11]", {
  con   <- fresh_db()
  on.exit(disconnect_database(con))
  clean <- clean_transaction_data(valid_txn_df(3))$clean_df
  db_bulk_insert_transactions(con, clean)
  # Insert same rows again — duplicates should be ignored (INSERT OR IGNORE)
  expect_silent(db_bulk_insert_transactions(con, clean))
  # Still only 3 rows
  expect_equal(nrow(get_transactions(con)), 3L)
})
