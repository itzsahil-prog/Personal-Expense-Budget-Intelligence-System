# =============================================================================
# R/analytics.R
# Two distinct layers:
#
#   LAYER 1 — Pure computation functions (this section)
#   ----------------------------------------------------
#   Accept a plain transactions data.frame (already fetched from SQLite) and
#   return a plain data.frame or scalar.  No plotting, no database calls.
#   Every function is safe to call with an empty or NULL data.frame — it
#   returns a typed empty result rather than an error.
#
#   calculate_total_spending()
#   calculate_daily_spending()
#   calculate_weekly_spending()
#   calculate_monthly_spending()
#   calculate_category_spending()
#   calculate_payment_method_spending()
#   calculate_average_daily_spending()
#   calculate_highest_spending_category()
#   calculate_highest_spending_day()
#   calculate_budget_utilization_inline()
#
#   LAYER 2 — Plotly chart functions (below)
#   -----------------------------------------
#   plot_daily_trend()
#   plot_weekly_trend()
#   plot_monthly_trend()
#   plot_category_breakdown()
#   plot_payment_breakdown()
#   plot_budget_vs_actual()
#   plot_category_monthly_trend()
#
# Dependencies: plotly, ggplot2, dplyr, lubridate, scales,
#               data_processing.R, validation.R
# =============================================================================

library(plotly)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)


# =============================================================================
# INTERNAL GUARD
# Shared by every computation function to handle empty / invalid input.
# =============================================================================

# Returns TRUE when df is a non-NULL, non-empty data.frame that contains at
# least the columns listed in `required_cols`.
.analytics_has_data <- function(df, required_cols = c("amount")) {
  is.data.frame(df) &&
    nrow(df) > 0L &&
    all(required_cols %in% names(df))
}

# Coerce the date column to Date class.  Safe to call even when there is no
# date column (the check is in .analytics_has_data).
.analytics_coerce_dates <- function(df) {
  if ("date" %in% names(df)) df$date <- as.Date(df$date)
  df
}


# =============================================================================
# LAYER 1 — PURE COMPUTATION FUNCTIONS
# =============================================================================

# -----------------------------------------------------------------------------
# 1. calculate_total_spending()
# -----------------------------------------------------------------------------

#' Calculate the total sum of all transaction amounts.
#'
#' @param df  data.frame from get_transactions(). Must contain an `amount`
#'            column.
#' @return    Named list:
#'              $total      — numeric: sum of all amounts (0 when df is empty)
#'              $n          — integer: number of transactions
#'              $has_data   — logical
calculate_total_spending <- function(df) {
  if (!.analytics_has_data(df)) {
    return(list(total = 0, n = 0L, has_data = FALSE))
  }
  list(
    total    = sum(df$amount, na.rm = TRUE),
    n        = nrow(df),
    has_data = TRUE
  )
}


# -----------------------------------------------------------------------------
# 2. calculate_daily_spending()
# -----------------------------------------------------------------------------

#' Aggregate transaction amounts to a daily total.
#'
#' @param df  data.frame from get_transactions(). Must contain `date` and
#'            `amount` columns.
#' @return    data.frame with columns:
#'              date              (Date)
#'              total_amount      (numeric)
#'              transaction_count (integer)
#'            Rows are sorted ascending by date.
#'            Returns a zero-row data.frame with correct column types when df
#'            is empty.
calculate_daily_spending <- function(df) {
  empty <- data.frame(
    date              = as.Date(character(0)),
    total_amount      = numeric(0),
    transaction_count = integer(0),
    stringsAsFactors  = FALSE
  )

  if (!.analytics_has_data(df, c("date", "amount"))) return(empty)

  df <- .analytics_coerce_dates(df)
  df <- df[!is.na(df$date) & !is.na(df$amount), , drop = FALSE]
  if (nrow(df) == 0L) return(empty)

  df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      total_amount      = sum(amount, na.rm = TRUE),
      transaction_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(date)
}


# -----------------------------------------------------------------------------
# 3. calculate_weekly_spending()
# -----------------------------------------------------------------------------

#' Aggregate transaction amounts to a weekly total (ISO weeks, Monday start).
#'
#' @param df  data.frame from get_transactions().
#' @return    data.frame with columns:
#'              year_week         (character "YYYY-Www")
#'              week_start        (Date — the Monday of that week)
#'              total_amount      (numeric)
#'              transaction_count (integer)
#'            Sorted ascending by week_start.
calculate_weekly_spending <- function(df) {
  empty <- data.frame(
    year_week         = character(0),
    week_start        = as.Date(character(0)),
    total_amount      = numeric(0),
    transaction_count = integer(0),
    stringsAsFactors  = FALSE
  )

  if (!.analytics_has_data(df, c("date", "amount"))) return(empty)

  df <- .analytics_coerce_dates(df)
  df <- df[!is.na(df$date) & !is.na(df$amount), , drop = FALSE]
  if (nrow(df) == 0L) return(empty)

  df %>%
    dplyr::mutate(
      year_week  = paste0(
        lubridate::isoyear(date), "-W",
        sprintf("%02d", lubridate::isoweek(date))
      ),
      week_start = lubridate::floor_date(date, unit = "week", week_start = 1L)
    ) %>%
    dplyr::group_by(year_week, week_start) %>%
    dplyr::summarise(
      total_amount      = sum(amount, na.rm = TRUE),
      transaction_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(week_start)
}


# -----------------------------------------------------------------------------
# 4. calculate_monthly_spending()
# -----------------------------------------------------------------------------

#' Aggregate transaction amounts to a monthly total.
#'
#' @param df  data.frame from get_transactions().
#' @return    data.frame with columns:
#'              year_month        (character "YYYY-MM")
#'              month_start       (Date — first day of the month)
#'              total_amount      (numeric)
#'              transaction_count (integer)
#'              avg_daily_amount  (numeric — total / calendar days in month)
#'            Sorted ascending by month_start.
calculate_monthly_spending <- function(df) {
  empty <- data.frame(
    year_month        = character(0),
    month_start       = as.Date(character(0)),
    total_amount      = numeric(0),
    transaction_count = integer(0),
    avg_daily_amount  = numeric(0),
    stringsAsFactors  = FALSE
  )

  if (!.analytics_has_data(df, c("date", "amount"))) return(empty)

  df <- .analytics_coerce_dates(df)
  df <- df[!is.na(df$date) & !is.na(df$amount), , drop = FALSE]
  if (nrow(df) == 0L) return(empty)

  df %>%
    dplyr::mutate(
      year_month  = format(date, "%Y-%m"),
      month_start = lubridate::floor_date(date, unit = "month")
    ) %>%
    dplyr::group_by(year_month, month_start) %>%
    dplyr::summarise(
      total_amount      = sum(amount, na.rm = TRUE),
      transaction_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      avg_daily_amount = round(
        total_amount / lubridate::days_in_month(month_start), 2
      )
    ) %>%
    dplyr::arrange(month_start)
}


# -----------------------------------------------------------------------------
# 5. calculate_category_spending()
# -----------------------------------------------------------------------------

#' Aggregate transaction amounts by category.
#'
#' @param df  data.frame from get_transactions().
#' @return    data.frame with columns:
#'              category          (character)
#'              total_amount      (numeric)
#'              transaction_count (integer)
#'              pct_of_total      (numeric — rounded to 1 decimal place)
#'              color_hex         (character — from CATEGORY_COLOURS)
#'            Sorted descending by total_amount.
calculate_category_spending <- function(df) {
  empty <- data.frame(
    category          = character(0),
    total_amount      = numeric(0),
    transaction_count = integer(0),
    pct_of_total      = numeric(0),
    color_hex         = character(0),
    stringsAsFactors  = FALSE
  )

  if (!.analytics_has_data(df, c("category", "amount"))) return(empty)
  df <- df[!is.na(df$amount) & !is.na(df$category), , drop = FALSE]
  if (nrow(df) == 0L) return(empty)

  grand_total <- sum(df$amount, na.rm = TRUE)
  if (grand_total == 0) return(empty)

  result <- df %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(
      total_amount      = sum(amount, na.rm = TRUE),
      transaction_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pct_of_total = round(total_amount / grand_total * 100, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(total_amount))

  result$color_hex <- CATEGORY_COLOURS[result$category]
  result$color_hex[is.na(result$color_hex)] <- "#CCCCCC"

  result
}


# -----------------------------------------------------------------------------
# 6. calculate_payment_method_spending()
# -----------------------------------------------------------------------------

#' Aggregate transaction amounts by payment method.
#'
#' @param df  data.frame from get_transactions().
#' @return    data.frame with columns:
#'              payment_method    (character)
#'              total_amount      (numeric)
#'              transaction_count (integer)
#'              pct_of_total      (numeric)
#'            Sorted descending by total_amount.
calculate_payment_method_spending <- function(df) {
  empty <- data.frame(
    payment_method    = character(0),
    total_amount      = numeric(0),
    transaction_count = integer(0),
    pct_of_total      = numeric(0),
    stringsAsFactors  = FALSE
  )

  if (!.analytics_has_data(df, c("payment_method", "amount"))) return(empty)
  df <- df[!is.na(df$amount) & !is.na(df$payment_method), , drop = FALSE]
  if (nrow(df) == 0L) return(empty)

  grand_total <- sum(df$amount, na.rm = TRUE)
  if (grand_total == 0) return(empty)

  df %>%
    dplyr::group_by(payment_method) %>%
    dplyr::summarise(
      total_amount      = sum(amount, na.rm = TRUE),
      transaction_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pct_of_total = round(total_amount / grand_total * 100, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(total_amount))
}


# -----------------------------------------------------------------------------
# 7. calculate_average_daily_spending()
# -----------------------------------------------------------------------------

#' Calculate the mean spending per day across all days that have at least one
#' transaction.
#'
#' @param df              data.frame from get_transactions().
#' @param currency_symbol Character symbol used in the formatted_value field.
#' @return                Named list:
#'                          $value           — numeric average (0 if no data)
#'                          $formatted_value — character, e.g. "₹ 1,234.50"
#'                          $days_counted    — integer: distinct days with txns
#'                          $has_data        — logical
calculate_average_daily_spending <- function(df, currency_symbol = "\u20b9") {
  if (!.analytics_has_data(df, c("date", "amount"))) {
    return(list(
      value           = 0,
      formatted_value = paste0(currency_symbol, " 0.00"),
      days_counted    = 0L,
      has_data        = FALSE
    ))
  }

  df      <- .analytics_coerce_dates(df)
  df      <- df[!is.na(df$date) & !is.na(df$amount), , drop = FALSE]
  n_days  <- dplyr::n_distinct(df$date)

  if (n_days == 0L) {
    return(list(
      value           = 0,
      formatted_value = paste0(currency_symbol, " 0.00"),
      days_counted    = 0L,
      has_data        = FALSE
    ))
  }

  avg <- sum(df$amount, na.rm = TRUE) / n_days

  list(
    value           = round(avg, 2),
    formatted_value = paste0(
      currency_symbol, " ",
      formatC(avg, format = "f", digits = 2, big.mark = ",")
    ),
    days_counted    = n_days,
    has_data        = TRUE
  )
}


# -----------------------------------------------------------------------------
# 8. calculate_highest_spending_category()
# -----------------------------------------------------------------------------

#' Find the category with the highest total spending.
#'
#' @param df  data.frame from get_transactions().
#' @return    Named list:
#'              $category      — character category name ("—" if no data)
#'              $total_amount  — numeric
#'              $pct_of_total  — numeric (0–100)
#'              $color_hex     — character
#'              $has_data      — logical
calculate_highest_spending_category <- function(df) {
  none <- list(
    category     = "\u2014",
    total_amount = 0,
    pct_of_total = 0,
    color_hex    = "#CCCCCC",
    has_data     = FALSE
  )

  if (!.analytics_has_data(df, c("category", "amount"))) return(none)

  cat_summary <- calculate_category_spending(df)
  if (nrow(cat_summary) == 0L) return(none)

  top <- cat_summary[1, , drop = FALSE]
  list(
    category     = top$category,
    total_amount = top$total_amount,
    pct_of_total = top$pct_of_total,
    color_hex    = top$color_hex,
    has_data     = TRUE
  )
}


# -----------------------------------------------------------------------------
# 9. calculate_highest_spending_day()
# -----------------------------------------------------------------------------

#' Find the calendar day with the highest total spending.
#'
#' @param df  data.frame from get_transactions().
#' @return    Named list:
#'              $date              — Date (NA if no data)
#'              $date_label        — character formatted as "dd Mon YYYY"
#'              $total_amount      — numeric
#'              $transaction_count — integer
#'              $has_data          — logical
calculate_highest_spending_day <- function(df) {
  none <- list(
    date              = NA,
    date_label        = "\u2014",
    total_amount      = 0,
    transaction_count = 0L,
    has_data          = FALSE
  )

  if (!.analytics_has_data(df, c("date", "amount"))) return(none)

  daily <- calculate_daily_spending(df)
  if (nrow(daily) == 0L) return(none)

  top_idx <- which.max(daily$total_amount)
  top     <- daily[top_idx, , drop = FALSE]

  list(
    date              = top$date,
    date_label        = format(top$date, "%d %b %Y"),
    total_amount      = top$total_amount,
    transaction_count = as.integer(top$transaction_count),
    has_data          = TRUE
  )
}


# -----------------------------------------------------------------------------
# 10. calculate_budget_utilization()
# -----------------------------------------------------------------------------

#' Compute budget utilisation by joining budget targets with actual spending.
#'
#' Works entirely in-memory — no database calls are made.  The caller fetches
#' both data.frames from the database and passes them in.
#'
#' @param budget_df        data.frame from get_budgets(month = "YYYY-MM").
#'                         Must contain columns: category, budget_amount.
#' @param transactions_df  data.frame from get_transactions().
#'                         Must contain columns: date, category, amount.
#' @param month            Character "YYYY-MM" — used to filter transactions_df
#'                         to the correct period.
#'
#' @return data.frame with columns:
#'           category          (character)
#'           budget_amount     (numeric)
#'           budget_used       (numeric — actual spend; 0 when no transactions)
#'           budget_remaining  (numeric — may be negative when over budget)
#'           utilization_pct   (numeric 0-N, rounded to 1 decimal place)
#'           status            (character: "ok" | "warning" | "over_budget")
#'         Sorted descending by utilization_pct.
#'         Returns a zero-row data.frame with correct column types when
#'         budget_df is NULL or empty.
calculate_budget_utilization_inline <- function(budget_df, transactions_df,
                                          month = NULL) {
  empty <- data.frame(
    category         = character(0),
    budget_amount    = numeric(0),
    budget_used      = numeric(0),
    budget_remaining = numeric(0),
    utilization_pct  = numeric(0),
    status           = character(0),
    stringsAsFactors = FALSE
  )

  if (is.null(budget_df) || !is.data.frame(budget_df) ||
      nrow(budget_df) == 0L) {
    return(empty)
  }
  if (!all(c("category", "budget_amount") %in% names(budget_df))) {
    return(empty)
  }

  # Filter transactions to the requested month when month is supplied
  if (.analytics_has_data(transactions_df, c("date", "category", "amount")) &&
      !is.null(month) && nchar(trimws(month)) == 7L) {

    transactions_df <- .analytics_coerce_dates(transactions_df)
    transactions_df <- transactions_df[
      !is.na(transactions_df$date) & !is.na(transactions_df$amount),
      , drop = FALSE
    ]
    transactions_df <- transactions_df[
      format(transactions_df$date, "%Y-%m") == as.character(month),
      , drop = FALSE
    ]
  } else {
    # No valid transactions to join against — treat as zero spend
    transactions_df <- data.frame(
      category = character(0),
      amount   = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  # Actual spend per category for the period
  actual <- if (nrow(transactions_df) > 0L) {
    transactions_df %>%
      dplyr::group_by(category) %>%
      dplyr::summarise(budget_used = sum(amount, na.rm = TRUE), .groups = "drop")
  } else {
    data.frame(
      category    = character(0),
      budget_used = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  # Left-join so every budgeted category appears even with zero actual spend
  result <- budget_df %>%
    dplyr::select(category, budget_amount) %>%
    dplyr::left_join(actual, by = "category") %>%
    dplyr::mutate(
      budget_used      = dplyr::coalesce(budget_used, 0),
      budget_remaining = budget_amount - budget_used,
      utilization_pct  = dplyr::case_when(
        budget_amount > 0 ~ round(budget_used / budget_amount * 100, 1),
        TRUE              ~ 0
      ),
      status = dplyr::case_when(
        utilization_pct >= 100 ~ "over_budget",
        utilization_pct >= 80  ~ "warning",
        TRUE                   ~ "ok"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(utilization_pct))

  result
}


# =============================================================================
# LAYER 2 — PLOTLY CHART FUNCTIONS
# (unchanged — see below)
# =============================================================================


# =============================================================================
# SHARED THEME
# A consistent visual language applied to every chart.
# =============================================================================

.plotly_theme <- function(p,
                           title       = NULL,
                           x_title     = NULL,
                           y_title     = NULL,
                           legend      = TRUE) {
  p <- p %>%
    plotly::layout(
      title = list(
        text = title,
        font = list(size = 16, color = "#2C3E50", family = "Segoe UI, sans-serif")
      ),
      xaxis = list(
        title      = x_title,
        showgrid   = TRUE,
        gridcolor  = "#ECF0F1",
        gridwidth  = 1,
        zeroline   = FALSE,
        tickfont   = list(size = 11, color = "#7F8C8D")
      ),
      yaxis = list(
        title      = y_title,
        showgrid   = TRUE,
        gridcolor  = "#ECF0F1",
        gridwidth  = 1,
        zeroline   = FALSE,
        tickfont   = list(size = 11, color = "#7F8C8D")
      ),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FAFAFA",
      font          = list(family = "Segoe UI, sans-serif"),
      legend        = list(
        orientation = "h",
        x           = 0,
        y           = -0.2,
        font        = list(size = 11)
      ),
      showlegend = legend,
      margin     = list(t = 60, r = 20, b = 60, l = 60)
    ) %>%
    plotly::config(displayModeBar = TRUE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d",
                                               "autoScale2d", "toggleSpikelines"),
                   displaylogo = FALSE)
  p
}


# Returns a blank plotly chart with a centred message — used for all
# empty-dataset and insufficient-data states.
.empty_chart <- function(message = "No data available") {
  plotly::plot_ly(type = "scatter", mode = "text") %>%
    plotly::add_text(x = 0.5, y = 0.5, text = message,
                     textfont = list(size = 16, color = "#95A5A6")) %>%
    plotly::layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FAFAFA",
      margin        = list(t = 40, r = 20, b = 40, l = 20)
    ) %>%
    plotly::config(displayModeBar = FALSE)
}


# =============================================================================
# 1. DAILY SPENDING TREND
# =============================================================================

#' Plot daily total spending as a filled area chart with a smoothed trend line.
#'
#' @param daily_df        data.frame from compute_daily_summary().
#' @param currency_symbol Character currency symbol.
#' @return plotly object.
plot_daily_trend <- function(daily_df, currency_symbol = "\u20b9") {

  if (is.null(daily_df) || nrow(daily_df) == 0) {
    return(.empty_chart("No transactions to display.\nAdd transactions to see daily spending trend."))
  }

  # Tooltip text
  daily_df <- daily_df %>%
    dplyr::mutate(
      hover_text = paste0(
        "<b>", format(date, "%d %b %Y"), "</b><br>",
        "Total: ", currency_symbol, " ",
        formatC(total_amount, format = "f", digits = 2, big.mark = ","), "<br>",
        "Transactions: ", transaction_count
      )
    )

  p <- plotly::plot_ly(data = daily_df, x = ~date) %>%
    plotly::add_trace(
      y          = ~total_amount,
      type       = "scatter",
      mode       = "lines",
      fill       = "tozeroy",
      fillcolor  = "rgba(52, 152, 219, 0.15)",
      line       = list(color = "#3498DB", width = 2),
      text       = ~hover_text,
      hoverinfo  = "text",
      name       = "Daily Spending"
    )

  # Add smoothed trend when there are enough points
  if (nrow(daily_df) >= 7) {
    loess_fit  <- stats::loess(total_amount ~ as.numeric(date),
                               data = daily_df, span = 0.4)
    daily_df$trend <- stats::predict(loess_fit)
    daily_df$trend <- pmax(daily_df$trend, 0)  # no negative predictions

    p <- p %>%
      plotly::add_trace(
        data       = daily_df,
        x          = ~date,
        y          = ~trend,
        type       = "scatter",
        mode       = "lines",
        line       = list(color = "#E74C3C", width = 2, dash = "dash"),
        name       = "Trend",
        hoverinfo  = "none"
      )
  }

  .plotly_theme(p,
    title   = "Daily Spending",
    x_title = "Date",
    y_title = paste0("Amount (", currency_symbol, ")")
  )
}


# =============================================================================
# 2. WEEKLY SPENDING TREND
# =============================================================================

#' Plot weekly total spending as a bar chart.
#'
#' @param weekly_df       data.frame from compute_weekly_summary().
#' @param currency_symbol Character currency symbol.
#' @return plotly object.
plot_weekly_trend <- function(weekly_df, currency_symbol = "\u20b9") {

  if (is.null(weekly_df) || nrow(weekly_df) == 0) {
    return(.empty_chart("No transactions to display.\nAdd transactions to see weekly spending trend."))
  }

  weekly_df <- weekly_df %>%
    dplyr::mutate(
      hover_text = paste0(
        "<b>Week of ", format(week_start, "%d %b %Y"), "</b><br>",
        "Total: ", currency_symbol, " ",
        formatC(total_amount, format = "f", digits = 2, big.mark = ","), "<br>",
        "Transactions: ", transaction_count
      )
    )

  p <- plotly::plot_ly(
    data      = weekly_df,
    x         = ~week_start,
    y         = ~total_amount,
    type      = "bar",
    marker    = list(
      color       = "#2ECC71",
      line        = list(color = "#27AE60", width = 1)
    ),
    text      = ~hover_text,
    hoverinfo = "text",
    name      = "Weekly Spending"
  )

  .plotly_theme(p,
    title   = "Weekly Spending",
    x_title = "Week Starting",
    y_title = paste0("Amount (", currency_symbol, ")")
  )
}


# =============================================================================
# 3. MONTHLY SPENDING TREND
# =============================================================================

#' Plot monthly total spending as a combined bar + line chart.
#'
#' @param monthly_df      data.frame from compute_monthly_summary().
#' @param currency_symbol Character currency symbol.
#' @return plotly object.
plot_monthly_trend <- function(monthly_df, currency_symbol = "\u20b9") {

  if (is.null(monthly_df) || nrow(monthly_df) == 0) {
    return(.empty_chart("No transactions to display.\nAdd transactions to see monthly spending trend."))
  }

  monthly_df <- monthly_df %>%
    dplyr::mutate(
      month_label = format(month_start, "%b %Y"),
      hover_bar   = paste0(
        "<b>", format(month_start, "%B %Y"), "</b><br>",
        "Total: ", currency_symbol, " ",
        formatC(total_amount, format = "f", digits = 2, big.mark = ","), "<br>",
        "Transactions: ", transaction_count
      ),
      hover_line  = paste0(
        "<b>", format(month_start, "%B %Y"), "</b><br>",
        "Avg/day: ", currency_symbol, " ",
        formatC(avg_daily_amount, format = "f", digits = 2, big.mark = ",")
      )
    )

  p <- plotly::plot_ly(data = monthly_df, x = ~month_label) %>%
    plotly::add_bars(
      y         = ~total_amount,
      marker    = list(color = "#9B59B6",
                       line  = list(color = "#8E44AD", width = 1)),
      text      = ~hover_bar,
      hoverinfo = "text",
      name      = "Monthly Total"
    ) %>%
    plotly::add_trace(
      y         = ~avg_daily_amount,
      type      = "scatter",
      mode      = "lines+markers",
      yaxis     = "y2",
      line      = list(color = "#E67E22", width = 2),
      marker    = list(color = "#E67E22", size = 7),
      text      = ~hover_line,
      hoverinfo = "text",
      name      = "Avg Daily"
    ) %>%
    plotly::layout(
      yaxis2 = list(
        overlaying = "y",
        side       = "right",
        title      = paste0("Avg Daily (", currency_symbol, ")"),
        showgrid   = FALSE,
        tickfont   = list(size = 11, color = "#E67E22")
      ),
      barmode = "group"
    )

  .plotly_theme(p,
    title   = "Monthly Spending Overview",
    x_title = "Month",
    y_title = paste0("Total Amount (", currency_symbol, ")")
  )
}


# =============================================================================
# 4. CATEGORY BREAKDOWN
# =============================================================================

#' Plot spending by category as a horizontal bar chart with category colours.
#'
#' @param category_df     data.frame from compute_category_summary().
#' @param currency_symbol Character currency symbol.
#' @return plotly object.
plot_category_breakdown <- function(category_df, currency_symbol = "\u20b9") {

  if (is.null(category_df) || nrow(category_df) == 0) {
    return(.empty_chart("No transactions to display.\nAdd transactions to see category breakdown."))
  }

  # Order bars ascending so largest appears at top
  category_df <- category_df %>%
    dplyr::arrange(total_amount) %>%
    dplyr::mutate(
      category = factor(category, levels = category),
      hover_text = paste0(
        "<b>", category, "</b><br>",
        "Total: ", currency_symbol, " ",
        formatC(total_amount, format = "f", digits = 2, big.mark = ","), "<br>",
        "Share: ", pct_of_total, "%<br>",
        "Transactions: ", transaction_count
      )
    )

  p <- plotly::plot_ly(
    data        = category_df,
    x           = ~total_amount,
    y           = ~category,
    type        = "bar",
    orientation = "h",
    marker      = list(color = ~color_hex,
                       line  = list(color = "#FFFFFF", width = 0.5)),
    text        = ~hover_text,
    hoverinfo   = "text",
    name        = "Category Spending"
  )

  .plotly_theme(p,
    title   = "Spending by Category",
    x_title = paste0("Total Amount (", currency_symbol, ")"),
    y_title = NULL,
    legend  = FALSE
  )
}


# =============================================================================
# 5. PAYMENT METHOD BREAKDOWN
# =============================================================================

#' Plot spending by payment method as a donut chart.
#'
#' @param payment_df      data.frame from compute_payment_summary().
#' @param currency_symbol Character currency symbol.
#' @return plotly object.
plot_payment_breakdown <- function(payment_df, currency_symbol = "\u20b9") {

  if (is.null(payment_df) || nrow(payment_df) == 0) {
    return(.empty_chart("No transactions to display.\nAdd transactions to see payment method breakdown."))
  }

  payment_colours <- c(
    "UPI"           = "#3498DB",
    "Cash"          = "#2ECC71",
    "Card"          = "#E74C3C",
    "Bank Transfer" = "#9B59B6"
  )

  payment_df <- payment_df %>%
    dplyr::mutate(
      color = dplyr::coalesce(payment_colours[payment_method], "#BDC3C7"),
      hover_text = paste0(
        "<b>", payment_method, "</b><br>",
        "Total: ", currency_symbol, " ",
        formatC(total_amount, format = "f", digits = 2, big.mark = ","), "<br>",
        "Share: ", pct_of_total, "%<br>",
        "Transactions: ", transaction_count
      )
    )

  p <- plotly::plot_ly(
    data         = payment_df,
    labels       = ~payment_method,
    values       = ~total_amount,
    type         = "pie",
    hole         = 0.5,
    marker       = list(colors = ~color,
                        line   = list(color = "#FFFFFF", width = 2)),
    text         = ~hover_text,
    hoverinfo    = "text",
    textinfo     = "label+percent",
    textfont     = list(size = 12)
  ) %>%
    plotly::layout(
      title = list(
        text = "Spending by Payment Method",
        font = list(size = 16, color = "#2C3E50")
      ),
      showlegend    = TRUE,
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FAFAFA",
      legend        = list(orientation = "v", x = 1.0, y = 0.5),
      margin        = list(t = 60, r = 100, b = 40, l = 40)
    ) %>%
    plotly::config(displayModeBar = FALSE)

  p
}


# =============================================================================
# 6. BUDGET vs ACTUAL (grouped bar)
# =============================================================================

#' Plot budgeted vs actual spending per category for a given month.
#'
#' @param budget_status_df  data.frame from compute_budget_status().
#' @param currency_symbol   Character currency symbol.
#' @return plotly object.
plot_budget_vs_actual <- function(budget_status_df, currency_symbol = "\u20b9") {

  if (is.null(budget_status_df) || nrow(budget_status_df) == 0) {
    return(.empty_chart("No budget data available.\nSet monthly budgets in the Budget tab."))
  }

  # Colour-code actuals: red if over budget, amber if warning, green if ok
  budget_status_df <- budget_status_df %>%
    dplyr::mutate(
      bar_color = dplyr::case_when(
        status == "over_budget" ~ "#E74C3C",
        status == "warning"     ~ "#F39C12",
        TRUE                    ~ "#2ECC71"
      ),
      hover_budget = paste0(
        "<b>", category, " — Budget</b><br>",
        currency_symbol, " ",
        formatC(budget_amount, format = "f", digits = 2, big.mark = ",")
      ),
      hover_actual = paste0(
        "<b>", category, " — Actual</b><br>",
        currency_symbol, " ",
        formatC(budget_used, format = "f", digits = 2, big.mark = ","), "<br>",
        "Utilisation: ", utilization_pct, "%"
      )
    )

  p <- plotly::plot_ly(data = budget_status_df, x = ~category) %>%
    plotly::add_bars(
      y         = ~budget_amount,
      name      = "Budget",
      marker    = list(color = "rgba(52,152,219,0.4)",
                       line  = list(color = "#3498DB", width = 1.5)),
      text      = ~hover_budget,
      hoverinfo = "text"
    ) %>%
    plotly::add_bars(
      y         = ~budget_used,
      name      = "Actual",
      marker    = list(color = ~bar_color,
                       line  = list(color = "#FFFFFF", width = 0.5)),
      text      = ~hover_actual,
      hoverinfo = "text"
    ) %>%
    plotly::layout(barmode = "group")

  .plotly_theme(p,
    title   = "Budget vs Actual Spending",
    x_title = "Category",
    y_title = paste0("Amount (", currency_symbol, ")")
  )
}


# =============================================================================
# 7. CATEGORY TREND OVER TIME (heatmap / stacked area)
# Shows how each category's monthly spending changes over the selected period.
# =============================================================================

#' Plot a stacked area chart of monthly spending by category.
#'
#' @param df              data.frame from db_get_transactions().
#' @param currency_symbol Character currency symbol.
#' @return plotly object.
plot_category_monthly_trend <- function(df, currency_symbol = "\u20b9") {

  if (is.null(df) || nrow(df) == 0) {
    return(.empty_chart("No transactions to display."))
  }

  df <- df %>%
    dplyr::mutate(
      date        = as.Date(date),
      year_month  = format(date, "%Y-%m"),
      month_start = lubridate::floor_date(date, unit = "month")
    )

  monthly_cat <- df %>%
    dplyr::group_by(month_start, category) %>%
    dplyr::summarise(total_amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(month_start)

  if (nrow(monthly_cat) == 0) return(.empty_chart("No data available."))

  categories_present <- unique(monthly_cat$category)

  p <- plotly::plot_ly()

  for (cat in categories_present) {
    cat_data  <- monthly_cat %>% dplyr::filter(category == cat)
    cat_color <- CATEGORY_COLOURS[cat]
    if (is.na(cat_color)) cat_color <- "#CCCCCC"

    p <- p %>%
      plotly::add_trace(
        data      = cat_data,
        x         = ~month_start,
        y         = ~total_amount,
        type      = "scatter",
        mode      = "lines",
        stackgroup = "one",
        name      = cat,
        line      = list(color = cat_color, width = 0.5),
        fillcolor = paste0(cat_color, "CC"),   # 80% opacity hex
        text      = ~paste0("<b>", cat, "</b><br>",
                            format(month_start, "%B %Y"), "<br>",
                            currency_symbol, " ",
                            formatC(total_amount, format = "f",
                                    digits = 2, big.mark = ",")),
        hoverinfo = "text"
      )
  }

  .plotly_theme(p,
    title   = "Monthly Spending by Category",
    x_title = "Month",
    y_title = paste0("Amount (", currency_symbol, ")")
  )
}
