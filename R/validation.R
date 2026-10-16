# =============================================================================
# R/validation.R
# Domain constants and all input/import validation functions.
# Single source of truth for valid categories, payment methods, and colours.
# All other modules import constants from here — never define them elsewhere.
# =============================================================================

# ── DOMAIN CONSTANTS ─────────────────────────────────────────────────────────

VALID_CATEGORIES <- c(
  "Food", "Grocery", "Transport", "Rent/PG", "Education",
  "Shopping", "Healthcare", "Entertainment", "Bills", "Other"
)

VALID_PAYMENT_METHODS <- c("UPI", "Cash", "Card", "Bank Transfer")

# Colour per category (used by charts and budget bars)
CATEGORY_COLOURS <- c(
  "Food"          = "#FF6B6B",
  "Grocery"       = "#4ECDC4",
  "Transport"     = "#45B7D1",
  "Rent/PG"       = "#96CEB4",
  "Education"     = "#FFEAA7",
  "Shopping"      = "#DDA0DD",
  "Healthcare"    = "#98D8C8",
  "Entertainment" = "#F7DC6F",
  "Bills"         = "#85C1E9",
  "Other"         = "#CCD1D1"
)

# Font Awesome 5 icon name per category (without "fa-" prefix)
CATEGORY_ICONS <- c(
  "Food"          = "utensils",
  "Grocery"       = "shopping-basket",
  "Transport"     = "bus",
  "Rent/PG"       = "home",
  "Education"     = "graduation-cap",
  "Shopping"      = "shopping-bag",
  "Healthcare"    = "heartbeat",
  "Entertainment" = "film",
  "Bills"         = "file-invoice",
  "Other"         = "ellipsis-h"
)

# Required column names for CSV/Excel imports (lowercase, post-normalisation)
IMPORT_REQUIRED_COLUMNS <- c(
  "date", "category", "description", "amount", "payment_method"
)

# Hard upper bound on a single transaction amount (sanity guard, not anomaly threshold)
MAX_TRANSACTION_AMOUNT <- 1e7   # ₹ 1 crore


# ── TRANSACTION ID GENERATOR ─────────────────────────────────────────────────

#' Generate a unique transaction ID.
#' Format: txn_YYYYMMDD_<6-hex-chars>
generate_transaction_id <- function(date_str = NULL) {
  date_part <- if (!is.null(date_str) && nchar(as.character(date_str)) == 10)
    gsub("-", "", as.character(date_str))
  else
    format(Sys.Date(), "%Y%m%d")

  rand_part <- paste0(
    sample(c(0:9, letters[1:6]), 6, replace = TRUE),
    collapse = ""
  )
  paste0("txn_", date_part, "_", rand_part)
}


# ── FORM VALIDATION ───────────────────────────────────────────────────────────

#' Validate the named-list of fields from the Add/Edit transaction form.
#'
#' @param inputs  Named list: date, category, description, amount, payment_method
#' @return list(valid = lgl, errors = chr vector)
validate_transaction_input <- function(inputs) {
  errors <- character(0)

  # date
  if (is.null(inputs$date) || is.na(inputs$date) || inputs$date == "") {
    errors <- c(errors, "Date is required.")
  } else {
    pd <- tryCatch(as.Date(inputs$date), error = function(e) NA, warning = function(w) NA)
    if (is.na(pd))
      errors <- c(errors, "Date is not valid (expected YYYY-MM-DD).")
    else if (pd > Sys.Date())
      errors <- c(errors, "Date cannot be in the future.")
    else if (pd < as.Date("2000-01-01"))
      errors <- c(errors, "Date cannot be before 01 Jan 2000.")
  }

  # category
  if (is.null(inputs$category) || inputs$category == "")
    errors <- c(errors, "Category is required.")
  else if (!inputs$category %in% VALID_CATEGORIES)
    errors <- c(errors, sprintf("Category '%s' is not recognised.", inputs$category))

  # description
  if (is.null(inputs$description) || trimws(inputs$description) == "")
    errors <- c(errors, "Description is required.")
  else if (nchar(trimws(inputs$description)) > 200)
    errors <- c(errors, "Description must be 200 characters or fewer.")

  # amount
  if (is.null(inputs$amount) || is.na(inputs$amount)) {
    errors <- c(errors, "Amount is required.")
  } else {
    amt <- suppressWarnings(as.numeric(inputs$amount))
    if (is.na(amt))
      errors <- c(errors, "Amount must be a number.")
    else if (amt <= 0)
      errors <- c(errors, "Amount must be greater than zero.")
    else if (amt > MAX_TRANSACTION_AMOUNT)
      errors <- c(errors, sprintf(
        "Amount exceeds the maximum of %s.",
        format(MAX_TRANSACTION_AMOUNT, big.mark = ",", scientific = FALSE)
      ))
  }

  # payment_method
  if (is.null(inputs$payment_method) || inputs$payment_method == "")
    errors <- c(errors, "Payment method is required.")
  else if (!inputs$payment_method %in% VALID_PAYMENT_METHODS)
    errors <- c(errors, sprintf(
      "Payment method '%s' is not recognised.", inputs$payment_method
    ))

  list(valid = length(errors) == 0L, errors = errors)
}


#' Validate the named-list of fields from the Add/Edit budget form.
#'
#' @param inputs  Named list: year, month, category, budget_amount
#' @return list(valid = lgl, errors = chr vector)
validate_budget_input <- function(inputs) {
  errors <- character(0)

  year_val  <- suppressWarnings(as.integer(inputs$year))
  if (is.na(year_val) || year_val < 2000 || year_val > 2100)
    errors <- c(errors, "Year must be between 2000 and 2100.")

  month_val <- suppressWarnings(as.integer(inputs$month))
  if (is.na(month_val) || month_val < 1 || month_val > 12)
    errors <- c(errors, "Month must be between 1 and 12.")

  if (is.null(inputs$category) || inputs$category == "")
    errors <- c(errors, "Category is required.")
  else if (!inputs$category %in% VALID_CATEGORIES)
    errors <- c(errors, sprintf("Category '%s' is not recognised.", inputs$category))

  ba <- suppressWarnings(as.numeric(inputs$budget_amount))
  if (is.null(inputs$budget_amount) || is.na(ba))
    errors <- c(errors, "Budget amount is required and must be a number.")
  else if (ba < 0)
    errors <- c(errors, "Budget amount cannot be negative.")
  else if (ba > MAX_TRANSACTION_AMOUNT)
    errors <- c(errors, "Budget amount exceeds the maximum allowed value.")

  list(valid = length(errors) == 0L, errors = errors)
}


# ── IMPORT FILE VALIDATION ────────────────────────────────────────────────────

#' Validate a raw imported data.frame against the transaction schema.
#'
#' @param df      data.frame from read_csv / read_excel (pre-normalisation)
#' @param source  "csv" or "excel" — used in error messages only
#' @return list(valid, errors = chr, row_errors = data.frame(row,field,value,reason))
validate_import_file <- function(df, source = "csv") {
  row_errors <- data.frame(
    row    = integer(0), field  = character(0),
    value  = character(0), reason = character(0),
    stringsAsFactors = FALSE
  )

  if (!is.data.frame(df))
    return(list(valid = FALSE,
                errors     = "Uploaded file could not be parsed into a table.",
                row_errors = row_errors))

  if (nrow(df) == 0L)
    return(list(valid = FALSE,
                errors     = "The uploaded file contains no data rows.",
                row_errors = row_errors))

  # Normalise column names
  names(df) <- trimws(tolower(names(df)))
  names(df) <- gsub("\\s+", "_", names(df))

  missing_cols <- setdiff(IMPORT_REQUIRED_COLUMNS, names(df))
  if (length(missing_cols) > 0L)
    return(list(
      valid      = FALSE,
      errors     = sprintf(
        "Missing required column(s): %s. Found: %s.",
        paste(missing_cols, collapse = ", "),
        paste(names(df), collapse = ", ")
      ),
      row_errors = row_errors
    ))

  errs <- list()
  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]

    # date
    dr  <- as.character(row$date)
    pd  <- tryCatch(as.Date(dr), error = function(e) NA, warning = function(w) NA)
    if (is.na(pd))
      errs[[length(errs) + 1L]] <- data.frame(
        row = i, field = "date", value = dr,
        reason = "Cannot be parsed as a date (YYYY-MM-DD expected).",
        stringsAsFactors = FALSE)
    else if (pd > Sys.Date())
      errs[[length(errs) + 1L]] <- data.frame(
        row = i, field = "date", value = dr,
        reason = "Date is in the future.",
        stringsAsFactors = FALSE)

    # amount
    ar  <- as.character(row$amount)
    amt <- suppressWarnings(as.numeric(ar))
    if (is.na(amt))
      errs[[length(errs) + 1L]] <- data.frame(
        row = i, field = "amount", value = ar,
        reason = "Not a valid number.", stringsAsFactors = FALSE)
    else if (amt <= 0)
      errs[[length(errs) + 1L]] <- data.frame(
        row = i, field = "amount", value = ar,
        reason = "Must be greater than zero.", stringsAsFactors = FALSE)
    else if (amt > MAX_TRANSACTION_AMOUNT)
      errs[[length(errs) + 1L]] <- data.frame(
        row = i, field = "amount", value = ar,
        reason = sprintf("Exceeds maximum of %s.",
                         format(MAX_TRANSACTION_AMOUNT, big.mark = ",")),
        stringsAsFactors = FALSE)

    # category
    cr <- trimws(as.character(row$category))
    if (!cr %in% VALID_CATEGORIES)
      errs[[length(errs) + 1L]] <- data.frame(
        row = i, field = "category", value = cr,
        reason = sprintf("Not valid. Allowed: %s.",
                         paste(VALID_CATEGORIES, collapse = ", ")),
        stringsAsFactors = FALSE)

    # payment_method
    pr <- trimws(as.character(row$payment_method))
    if (!pr %in% VALID_PAYMENT_METHODS)
      errs[[length(errs) + 1L]] <- data.frame(
        row = i, field = "payment_method", value = pr,
        reason = sprintf("Not valid. Allowed: %s.",
                         paste(VALID_PAYMENT_METHODS, collapse = ", ")),
        stringsAsFactors = FALSE)

    # description
    desc_r <- trimws(as.character(row$description))
    if (is.na(desc_r) || desc_r == "" || desc_r == "NA")
      errs[[length(errs) + 1L]] <- data.frame(
        row = i, field = "description", value = desc_r,
        reason = "Description cannot be empty.", stringsAsFactors = FALSE)
  }

  if (length(errs) > 0L)
    row_errors <- do.call(rbind, errs)

  list(
    valid      = nrow(row_errors) == 0L,
    errors     = character(0),
    row_errors = row_errors
  )
}


#' Normalise a validated import data.frame into the canonical column schema.
#' Call ONLY after validate_import_file() has returned valid = TRUE.
#'
#' @return data.frame: transaction_id, date, category, description,
#'                     amount, payment_method, created_at, updated_at
normalise_import_df <- function(df) {
  names(df) <- trimws(tolower(names(df)))
  names(df) <- gsub("\\s+", "_", names(df))

  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  result <- data.frame(
    transaction_id = vapply(as.character(df$date),
                            generate_transaction_id, character(1)),
    date           = format(as.Date(as.character(df$date)), "%Y-%m-%d"),
    category       = trimws(as.character(df$category)),
    description    = trimws(as.character(df$description)),
    amount         = as.numeric(df$amount),
    payment_method = trimws(as.character(df$payment_method)),
    created_at     = ts,
    updated_at     = ts,
    stringsAsFactors = FALSE
  )

  # Resolve any duplicate IDs from same-date rows
  if (any(duplicated(result$transaction_id))) {
    dups <- which(duplicated(result$transaction_id))
    for (idx in dups)
      result$transaction_id[idx] <- generate_transaction_id(result$date[idx])
  }

  result
}


# ── PIPELINE ENTRY POINTS ─────────────────────────────────────────────────────

#' Validate a raw data.frame and return a Shiny-friendly result.
#' @return list: is_valid, total_rows, valid_row_count, invalid_row_count,
#'               structural_errors, row_errors, summary_message, detail_messages
validate_transaction_data <- function(df, source = "csv") {
  result     <- validate_import_file(df, source)
  total_rows <- if (is.data.frame(df)) nrow(df) else 0L

  if (length(result$errors) > 0L)
    return(list(
      is_valid          = FALSE,
      total_rows        = total_rows,
      valid_row_count   = 0L,
      invalid_row_count = total_rows,
      structural_errors = result$errors,
      row_errors        = result$row_errors,
      summary_message   = paste("File cannot be imported:", result$errors),
      detail_messages   = result$errors
    ))

  re          <- result$row_errors
  invalid_n   <- if (nrow(re) > 0L) length(unique(re$row)) else 0L
  valid_n     <- total_rows - invalid_n

  detail_msgs <- if (nrow(re) > 0L) {
    apply(re, 1, function(r)
      sprintf("Row %s — %s ('%s'): %s", r["row"], r["field"], r["value"], r["reason"]))
  } else character(0)

  summary_msg <- if (result$valid)
    sprintf("All %d row%s passed validation.", total_rows, if (total_rows == 1) "" else "s")
  else
    sprintf("%d of %d row%s have errors. %d row%s can be imported.",
            invalid_n, total_rows, if (total_rows == 1) "" else "s",
            valid_n,   if (valid_n  == 1) "" else "s")

  list(
    is_valid          = result$valid,
    total_rows        = total_rows,
    valid_row_count   = valid_n,
    invalid_row_count = invalid_n,
    structural_errors = character(0),
    row_errors        = re,
    summary_message   = summary_msg,
    detail_messages   = detail_msgs
  )
}


#' Clean a raw data.frame: quarantine invalid rows, normalise clean rows.
#' @return list: clean_df, discarded_df, clean_count, discarded_count,
#'               total_rows, has_clean, has_discarded, summary_message, is_partial
clean_transaction_data <- function(df, source = "csv") {
  empty_clean <- data.frame(
    transaction_id = character(0), date = character(0),
    category = character(0), description = character(0),
    amount = numeric(0), payment_method = character(0),
    created_at = character(0), updated_at = character(0),
    stringsAsFactors = FALSE
  )

  if (!is.data.frame(df) || nrow(df) == 0L)
    return(list(
      clean_df = empty_clean, discarded_df = data.frame(),
      clean_count = 0L, discarded_count = 0L, total_rows = 0L,
      has_clean = FALSE, has_discarded = FALSE,
      summary_message = "No data to process.", is_partial = FALSE
    ))

  total_rows <- nrow(df)
  vr         <- validate_import_file(df, source)

  # Structural failure — nothing can be imported
  if (length(vr$errors) > 0L) {
    df$validation_errors <- paste(vr$errors, collapse = "; ")
    return(list(
      clean_df = empty_clean, discarded_df = df,
      clean_count = 0L, discarded_count = total_rows,
      total_rows = total_rows, has_clean = FALSE, has_discarded = TRUE,
      summary_message = paste("Import blocked:", vr$errors), is_partial = FALSE
    ))
  }

  re       <- vr$row_errors
  bad_rows <- if (nrow(re) > 0L) as.integer(names(split(re, re$row))) else integer(0)

  # Normalise column names for splitting
  names(df) <- trimws(tolower(names(df)))
  names(df) <- gsub("\\s+", "_", names(df))

  clean_idx   <- setdiff(seq_len(total_rows), bad_rows)
  clean_raw   <- df[clean_idx,  , drop = FALSE]

  discarded_df <- if (length(bad_rows) > 0L) {
    d <- df[bad_rows, , drop = FALSE]
    by_row <- split(re, re$row)
    d$validation_errors <- vapply(bad_rows, function(r) {
      paste(by_row[[as.character(r)]]$reason, collapse = "; ")
    }, character(1))
    d
  } else data.frame()

  clean_df <- if (nrow(clean_raw) > 0L) {
    tryCatch(normalise_import_df(clean_raw), error = function(e) empty_clean)
  } else empty_clean

  cc <- nrow(clean_df)
  dc <- nrow(discarded_df)

  msg <- if (dc == 0L)
    sprintf("All %d row%s imported successfully.", cc, if (cc == 1) "" else "s")
  else if (cc == 0L)
    sprintf("Import failed: all %d row%s had errors.", dc, if (dc == 1) "" else "s")
  else
    sprintf("%d row%s imported; %d skipped due to errors.",
            cc, if (cc == 1) "" else "s", dc)

  list(
    clean_df = clean_df, discarded_df = discarded_df,
    clean_count = cc, discarded_count = dc, total_rows = total_rows,
    has_clean = cc > 0L, has_discarded = dc > 0L,
    summary_message = msg, is_partial = cc > 0L && dc > 0L
  )
}
