# =============================================================================
# data/validate_sample_data.R
# Standalone quality-check script for data/sample_data.csv.
#
# Checks every requirement from the dataset specification:
#   1.  File exists and is readable
#   2.  Required columns present, in the correct order
#   3.  Row count == 1500
#   4.  No duplicate transaction_id values
#   5.  transaction_id format: txn_YYYYMMDD_NNNNN
#   6.  Date column: valid ISO 8601 dates, no future dates, no impossible dates
#   7.  Date range covers at least 12 distinct calendar months
#   8.  Category: only values from the 10-item allowed set
#   9.  Amount: numeric, > 0, no NAs, no infinities
#  10.  Payment method: only values from the 4-item allowed set
#  11.  Description: non-empty strings, max 200 characters
#  12.  All 10 categories represented
#  13.  All 4 payment methods represented
#  14.  Realistic amount distributions per category
#  15.  Anomaly signal: at least 5 transactions with amount ≥ 2500
#  16.  Monthly patterns: Rent/PG appears at most once per month
#  17.  Valid CSV formatting (no stray quotes, correct column count per row)
#
# Run:
#   Rscript data/validate_sample_data.R
#   # or from RStudio:
#   source("data/validate_sample_data.R")
#
# Output:
#   Prints a structured PASS/FAIL report.
#   Exits with code 1 if any FAIL is found (useful in CI).
# =============================================================================

# ---------------------------------------------------------------------------
# CONFIGURATION
# ---------------------------------------------------------------------------
CSV_PATH <- "data/sample_data.csv"

REQUIRED_COLUMNS    <- c("transaction_id","date","category",
                          "description","amount","payment_method")

VALID_CATEGORIES     <- c("Food","Grocery","Transport","Rent/PG","Education",
                           "Shopping","Healthcare","Entertainment","Bills","Other")
VALID_PAYMENT_METHODS <- c("UPI","Cash","Card","Bank Transfer")

EXPECTED_ROWS        <- 1500L
MIN_MONTHS_COVERED   <- 12L
MIN_ANOMALY_ROWS     <- 5L   # rows with amount >= 2500
MAX_DESCRIPTION_CHARS <- 200L

# Expected amount ranges (soft check — warn if median is far outside)
CATEGORY_EXPECTED_MEDIANS <- list(
  Food          = c(lo =  50, hi =  500),
  Grocery       = c(lo = 100, hi = 2000),
  Transport     = c(lo =  30, hi =  500),
  `Rent/PG`     = c(lo = 5000, hi = 15000),
  Education     = c(lo =  80, hi = 5000),
  Shopping      = c(lo = 100, hi = 5000),
  Healthcare    = c(lo =  50, hi = 3000),
  Entertainment = c(lo = 100, hi = 2000),
  Bills         = c(lo = 100, hi = 3000),
  Other         = c(lo =  50, hi = 1000)
)


# ---------------------------------------------------------------------------
# HELPERS
# ---------------------------------------------------------------------------
pass_count <- 0L
fail_count <- 0L
warn_count <- 0L

report <- function(check_name, passed, detail = NULL, is_warning = FALSE) {
  if (is_warning) {
    warn_count <<- warn_count + 1L
    symbol <- "  WARN"
    cat(sprintf("%s  %-52s %s\n",
                symbol, check_name,
                if (!is.null(detail)) paste0("[", detail, "]") else ""))
  } else if (passed) {
    pass_count <<- pass_count + 1L
    symbol <- "  PASS"
    cat(sprintf("%s  %-52s %s\n",
                symbol, check_name,
                if (!is.null(detail)) paste0("[", detail, "]") else ""))
  } else {
    fail_count <<- fail_count + 1L
    symbol <- "! FAIL"
    cat(sprintf("%s  %-52s %s\n",
                symbol, check_name,
                if (!is.null(detail)) paste0("[", detail, "]") else ""))
  }
}

section <- function(title) {
  cat(sprintf("\n── %s %s\n", title,
              strrep("─", max(0, 60 - nchar(title) - 4))))
}


# ---------------------------------------------------------------------------
# SECTION 0: HEADER
# ---------------------------------------------------------------------------
cat(strrep("=", 68), "\n")
cat("  SAMPLE DATA QUALITY REPORT\n")
cat(sprintf("  File : %s\n", CSV_PATH))
cat(sprintf("  Run  : %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(strrep("=", 68), "\n")


# ---------------------------------------------------------------------------
# SECTION 1: FILE
# ---------------------------------------------------------------------------
section("1  FILE")

file_ok <- file.exists(CSV_PATH)
report("File exists", file_ok,
       detail = if (!file_ok) paste("not found:", CSV_PATH) else CSV_PATH)

if (!file_ok) {
  cat("\nCannot continue — file not found.\n")
  quit(save = "no", status = 1L)
}

file_size_kb <- round(file.info(CSV_PATH)$size / 1024, 1)
report("File is non-empty", file_size_kb > 0,
       detail = sprintf("%.1f KB", file_size_kb))


# ---------------------------------------------------------------------------
# SECTION 2: PARSE
# ---------------------------------------------------------------------------
section("2  PARSE")

df <- tryCatch({
  read.csv(CSV_PATH, stringsAsFactors = FALSE, na.strings = c("","NA"))
}, error = function(e) {
  report("CSV parses without error", FALSE, detail = conditionMessage(e))
  quit(save = "no", status = 1L)
})

report("CSV parses without error", TRUE,
       detail = sprintf("%d rows × %d columns", nrow(df), ncol(df)))


# ---------------------------------------------------------------------------
# SECTION 3: STRUCTURE
# ---------------------------------------------------------------------------
section("3  STRUCTURE")

# Column names
actual_cols   <- names(df)
missing_cols  <- setdiff(REQUIRED_COLUMNS, actual_cols)
extra_cols    <- setdiff(actual_cols, REQUIRED_COLUMNS)
report("All required columns present",
       length(missing_cols) == 0,
       detail = if (length(missing_cols) > 0)
                  paste("missing:", paste(missing_cols, collapse=", "))
                else
                  paste(REQUIRED_COLUMNS, collapse=", "))

if (length(extra_cols) > 0)
  report("No unexpected extra columns", FALSE,
         detail = paste("extra:", paste(extra_cols, collapse=", ")))

# Column order
col_order_ok <- identical(actual_cols[seq_along(REQUIRED_COLUMNS)],
                           REQUIRED_COLUMNS)
report("Column order matches specification", col_order_ok,
       detail = paste(actual_cols, collapse=", "))

# Row count
report("Row count == 1500",
       nrow(df) == EXPECTED_ROWS,
       detail = sprintf("%d rows", nrow(df)))


# ---------------------------------------------------------------------------
# SECTION 4: transaction_id
# ---------------------------------------------------------------------------
section("4  TRANSACTION_ID")

n_na_id   <- sum(is.na(df$transaction_id))
n_dup_id  <- sum(duplicated(df$transaction_id))
n_bad_fmt <- sum(!grepl("^txn_\\d{8}_\\d{5}$", df$transaction_id, perl=TRUE))

report("No NA transaction_ids",         n_na_id  == 0, detail = sprintf("%d NA",   n_na_id))
report("No duplicate transaction_ids",  n_dup_id == 0, detail = sprintf("%d dups", n_dup_id))
report("ID format: txn_YYYYMMDD_NNNNN", n_bad_fmt == 0,
       detail = if (n_bad_fmt > 0)
                  sprintf("%d malformed (e.g. '%s')",
                          n_bad_fmt, head(df$transaction_id[
                            !grepl("^txn_\\d{8}_\\d{5}$",
                                   df$transaction_id, perl=TRUE)], 1))
                else "all valid")


# ---------------------------------------------------------------------------
# SECTION 5: DATE
# ---------------------------------------------------------------------------
section("5  DATE")

parsed_dates <- suppressWarnings(as.Date(df$date, format="%Y-%m-%d"))
n_na_date    <- sum(is.na(parsed_dates))
n_future     <- sum(!is.na(parsed_dates) & parsed_dates > Sys.Date())
n_before_2020 <- sum(!is.na(parsed_dates) & parsed_dates < as.Date("2020-01-01"))

report("No NA / unparseable dates", n_na_date   == 0, detail = sprintf("%d bad", n_na_date))
report("No future dates",           n_future    == 0, detail = sprintf("%d future", n_future))
report("No dates before 2020",      n_before_2020 == 0, detail = sprintf("%d pre-2020", n_before_2020))

date_range_ok <- !is.na(min(parsed_dates, na.rm=TRUE))
if (date_range_ok) {
  d_min  <- min(parsed_dates, na.rm=TRUE)
  d_max  <- max(parsed_dates, na.rm=TRUE)
  report("Date range reported",  TRUE,
         detail = sprintf("%s to %s (%d days span)",
                          d_min, d_max, as.integer(d_max - d_min)))

  # Coverage: at least 12 distinct YYYY-MM values
  n_months <- length(unique(format(parsed_dates[!is.na(parsed_dates)], "%Y-%m")))
  report(sprintf("At least %d calendar months covered", MIN_MONTHS_COVERED),
         n_months >= MIN_MONTHS_COVERED,
         detail = sprintf("%d months", n_months))
}


# ---------------------------------------------------------------------------
# SECTION 6: CATEGORY
# ---------------------------------------------------------------------------
section("6  CATEGORY")

n_na_cat    <- sum(is.na(df$category))
invalid_cats <- df$category[!is.na(df$category) &
                             !df$category %in% VALID_CATEGORIES]
n_invalid   <- length(invalid_cats)

report("No NA categories",       n_na_cat  == 0, detail = sprintf("%d NA", n_na_cat))
report("All categories valid",   n_invalid == 0,
       detail = if (n_invalid > 0)
                  sprintf("%d invalid (e.g. '%s')", n_invalid, unique(invalid_cats)[1])
                else paste(sort(unique(df$category)), collapse=", "))

# All 10 categories represented
missing_cats <- setdiff(VALID_CATEGORIES, unique(df$category))
report("All 10 categories present",
       length(missing_cats) == 0,
       detail = if (length(missing_cats) > 0)
                  paste("absent:", paste(missing_cats, collapse=", "))
                else "all 10 present")

# Category distribution
cat("\n  Category counts:\n")
cat_counts <- sort(table(df$category), decreasing=TRUE)
for (nm in names(cat_counts)) {
  pct <- round(cat_counts[[nm]] / nrow(df) * 100, 1)
  bar <- strrep("|", min(40, round(pct * 1.5)))
  cat(sprintf("    %-15s %4d  (%4.1f%%)  %s\n", nm, cat_counts[[nm]], pct, bar))
}


# ---------------------------------------------------------------------------
# SECTION 7: AMOUNT
# ---------------------------------------------------------------------------
section("7  AMOUNT")

df$amount <- suppressWarnings(as.numeric(df$amount))

n_na_amt    <- sum(is.na(df$amount))
n_inf_amt   <- sum(is.infinite(df$amount))
n_neg_amt   <- sum(!is.na(df$amount) & df$amount <= 0)

report("No NA amounts",        n_na_amt  == 0, detail = sprintf("%d NA", n_na_amt))
report("No infinite amounts",  n_inf_amt == 0, detail = sprintf("%d Inf", n_inf_amt))
report("All amounts > 0",      n_neg_amt == 0, detail = sprintf("%d zero/negative", n_neg_amt))

if (n_na_amt == 0 && n_neg_amt == 0) {
  overall_stats <- c(
    min    = min(df$amount),
    median = median(df$amount),
    mean   = mean(df$amount),
    max    = max(df$amount)
  )
  report("Amount range reported", TRUE,
         detail = sprintf("min=%.0f  median=%.0f  mean=%.0f  max=%.0f",
                          overall_stats["min"], overall_stats["median"],
                          overall_stats["mean"], overall_stats["max"]))

  # Anomaly signal check
  n_large <- sum(df$amount >= 2500, na.rm=TRUE)
  report(sprintf("At least %d rows with amount >= 2500 (anomaly signal)",
                 MIN_ANOMALY_ROWS),
         n_large >= MIN_ANOMALY_ROWS,
         detail = sprintf("%d rows >= 2500", n_large))

  # Per-category median plausibility
  cat("\n  Per-category amount summary:\n")
  cat(sprintf("    %-15s  %7s  %7s  %7s  %7s  %s\n",
              "Category","Min","Median","Mean","Max","Plausible?"))
  for (cat_name in VALID_CATEGORIES) {
    vals <- df$amount[df$category == cat_name & !is.na(df$amount)]
    if (length(vals) == 0) {
      cat(sprintf("    %-15s  (no rows)\n", cat_name)); next
    }
    lo  <- CATEGORY_EXPECTED_MEDIANS[[cat_name]]["lo"]
    hi  <- CATEGORY_EXPECTED_MEDIANS[[cat_name]]["hi"]
    med <- median(vals)
    ok  <- med >= lo && med <= hi
    flag <- if (ok) "" else " <-- WARN"
    if (!ok) warn_count <<- warn_count + 1L
    cat(sprintf("    %-15s  %7.0f  %7.0f  %7.0f  %7.0f  %s%s\n",
                cat_name,
                min(vals), med, mean(vals), max(vals),
                if (ok) "yes" else "no (median outside expected range)", flag))
  }
}


# ---------------------------------------------------------------------------
# SECTION 8: PAYMENT METHOD
# ---------------------------------------------------------------------------
section("8  PAYMENT_METHOD")

n_na_pm    <- sum(is.na(df$payment_method))
invalid_pm <- df$payment_method[!is.na(df$payment_method) &
                                 !df$payment_method %in% VALID_PAYMENT_METHODS]
n_invalid_pm <- length(invalid_pm)

report("No NA payment methods",        n_na_pm     == 0, detail = sprintf("%d NA", n_na_pm))
report("All payment methods valid",    n_invalid_pm == 0,
       detail = if (n_invalid_pm > 0)
                  sprintf("%d invalid (e.g. '%s')", n_invalid_pm, unique(invalid_pm)[1])
                else paste(sort(unique(df$payment_method)), collapse=", "))

missing_pm <- setdiff(VALID_PAYMENT_METHODS, unique(df$payment_method))
report("All 4 payment methods present",
       length(missing_pm) == 0,
       detail = if (length(missing_pm) > 0)
                  paste("absent:", paste(missing_pm, collapse=", "))
                else "all 4 present")

cat("\n  Payment method counts:\n")
pm_counts <- sort(table(df$payment_method), decreasing=TRUE)
for (nm in names(pm_counts)) {
  pct <- round(pm_counts[[nm]] / nrow(df) * 100, 1)
  bar <- strrep("|", min(40, round(pct * 0.6)))
  cat(sprintf("    %-16s %4d  (%4.1f%%)  %s\n", nm, pm_counts[[nm]], pct, bar))
}


# ---------------------------------------------------------------------------
# SECTION 9: DESCRIPTION
# ---------------------------------------------------------------------------
section("9  DESCRIPTION")

n_na_desc   <- sum(is.na(df$description))
n_empty_desc <- sum(!is.na(df$description) & trimws(df$description) == "")
n_long_desc  <- sum(!is.na(df$description) &
                    nchar(df$description) > MAX_DESCRIPTION_CHARS)

report("No NA descriptions",          n_na_desc  == 0, detail = sprintf("%d NA", n_na_desc))
report("No empty descriptions",       n_empty_desc == 0, detail = sprintf("%d empty", n_empty_desc))
report(sprintf("All descriptions <= %d chars", MAX_DESCRIPTION_CHARS),
       n_long_desc == 0, detail = sprintf("%d too long", n_long_desc))

n_unique_desc <- length(unique(df$description[!is.na(df$description)]))
report("Description vocabulary reported", TRUE,
       detail = sprintf("%d unique descriptions", n_unique_desc))


# ---------------------------------------------------------------------------
# SECTION 10: MONTHLY PATTERN — Rent/PG
# ---------------------------------------------------------------------------
section("10 MONTHLY PATTERN")

rent_df <- df[!is.na(df$category) & df$category == "Rent/PG", ]
if (nrow(rent_df) > 0 && !anyNA(parsed_dates)) {
  rent_months <- format(parsed_dates[df$category == "Rent/PG" &
                                      !is.na(df$category)], "%Y-%m")
  n_dup_rent  <- sum(duplicated(rent_months))
  report("Rent/PG: at most 1 payment per month",
         n_dup_rent == 0,
         detail = sprintf("%d months with >1 payment", n_dup_rent))
  report("Rent/PG count reported",
         TRUE,
         detail = sprintf("%d payments across %d months",
                          nrow(rent_df), length(unique(rent_months))))
} else {
  report("Rent/PG rows present", nrow(rent_df) > 0,
         detail = sprintf("%d Rent/PG rows found", nrow(rent_df)))
}


# ---------------------------------------------------------------------------
# SECTION 11: CSV FORMAT
# ---------------------------------------------------------------------------
section("11 CSV FORMAT")

raw_lines <- readLines(CSV_PATH, warn=FALSE)
expected_fields <- length(REQUIRED_COLUMNS)

# Count quoted fields per line correctly with a simple comma count heuristic
# (works because none of our description values contain commas)
# For rows that have commas inside quoted fields we use a more careful parse
field_counts <- vapply(raw_lines[-1], function(line) {
  # Replace commas inside double-quoted segments temporarily
  cleaned <- gsub('"[^"]*"', '""', line)
  sum(chartr(",","_",cleaned) != cleaned) + 1L   # n_commas + 1 = n_fields
}, integer(1), USE.NAMES=FALSE)

n_wrong_fields <- sum(field_counts != expected_fields)
report(sprintf("All data rows have exactly %d fields", expected_fields),
       n_wrong_fields == 0,
       detail = sprintf("%d rows with wrong field count", n_wrong_fields))

n_raw_lines <- length(raw_lines)
report("Line count matches row count + 1 header",
       n_raw_lines == EXPECTED_ROWS + 1L,
       detail = sprintf("%d lines (expected %d)", n_raw_lines, EXPECTED_ROWS + 1L))


# ---------------------------------------------------------------------------
# SUMMARY
# ---------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 68), "\n")
cat(sprintf("  PASS  %3d    FAIL  %3d    WARN  %3d\n",
            pass_count, fail_count, warn_count))
cat(strrep("=", 68), "\n")

if (fail_count == 0 && warn_count == 0) {
  cat("  All checks passed. Dataset is ready to use.\n\n")
} else if (fail_count == 0) {
  cat(sprintf("  No failures. %d warning(s) noted (see above).\n\n", warn_count))
} else {
  cat(sprintf("  %d check(s) FAILED. Review output above.\n\n", fail_count))
}

# Exit with non-zero status when there are failures (useful in CI)
if (fail_count > 0) quit(save="no", status=1L)
