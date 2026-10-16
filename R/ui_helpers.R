# =============================================================================
# R/ui_helpers.R
# Reusable Shiny UI component functions.
# No server logic here — pure tag constructors only.
#
# Functions
# ─────────
#   %||%                      null-coalescing operator
#   format_currency()         numeric → "₹ 1,234.50"
#   kpi_card()                KPI summary card
#   kpi_row()                 flex row of kpi_card()s
#   budget_progress_bar()     labelled progress bar for one budget category
#   status_badge()            coloured inline badge
#   alert_box()               alert / notification box
#   empty_state_ui()          centred empty-state placeholder
#   section_header()          section title + optional right content
#   loading_spinner()         inline spinner
#   transaction_form_fields() shared form fields for Add/Edit modals
#   add_transaction_modal()   modal for adding a new transaction
#   edit_transaction_modal()  modal pre-filled for editing
#   delete_confirm_modal()    confirmation modal for deletion
#   import_error_table()      DT table of row-level import errors
#   forecast_summary_card()   stats card below forecast chart
#   anomaly_summary_card()    summary card on Anomalies tab
#
# Dependencies: shiny, DT, validation.R (VALID_CATEGORIES, VALID_PAYMENT_METHODS)
# =============================================================================

library(shiny)


# ── NULL-COALESCING OPERATOR ──────────────────────────────────────────────────

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && !is.na(x)[1]) x else y


# ── CURRENCY FORMATTER ────────────────────────────────────────────────────────

#' Format a numeric amount as a currency string.
#' @param amount          Numeric value.
#' @param currency_symbol Character prepended symbol.
#' @param digits          Decimal places (default 2).
#' @return Character, e.g. "₹ 1,234.50"
format_currency <- function(amount, currency_symbol = "\u20b9", digits = 2) {
  if (is.null(amount) || is.na(amount) || !is.numeric(amount))
    return(paste0(currency_symbol, "\u00a0", "0.00"))
  paste0(
    currency_symbol, "\u00a0",
    formatC(amount, format = "f", digits = digits, big.mark = ",")
  )
}


# ── KPI CARDS ─────────────────────────────────────────────────────────────────

#' Render a KPI summary card with coloured top stripe and icon box.
#'
#' @param id       Unique HTML id.
#' @param label    Short uppercase label.
#' @param value    Formatted value string.
#' @param icon     Font Awesome icon name (without "fa-").
#' @param color    "primary" | "violet" | "emerald" | "amber" | "rose" |
#'                 "success" | "warning" | "danger" | "info" | raw hex.
#' @param subtitle Optional secondary line.
#' @return shiny::tagList
kpi_card <- function(id, label, value, icon = "chart-bar",
                     color = "primary", subtitle = NULL) {

  palette <- list(
    primary  = list(accent = "var(--kpi-blue)",    icon_bg = "var(--kpi-blue-bg)"),
    violet   = list(accent = "var(--kpi-violet)",  icon_bg = "var(--kpi-violet-bg)"),
    emerald  = list(accent = "var(--kpi-emerald)", icon_bg = "var(--kpi-emerald-bg)"),
    amber    = list(accent = "var(--kpi-amber)",   icon_bg = "var(--kpi-amber-bg)"),
    rose     = list(accent = "var(--kpi-rose)",    icon_bg = "var(--kpi-rose-bg)"),
    success  = list(accent = "var(--kpi-emerald)", icon_bg = "var(--kpi-emerald-bg)"),
    warning  = list(accent = "var(--kpi-amber)",   icon_bg = "var(--kpi-amber-bg)"),
    danger   = list(accent = "var(--kpi-rose)",    icon_bg = "var(--kpi-rose-bg)"),
    info     = list(accent = "var(--kpi-violet)",  icon_bg = "var(--kpi-violet-bg)")
  )
  pal <- if (color %in% names(palette)) palette[[color]] else
    list(accent = color, icon_bg = "rgba(0,0,0,.05)")

  tags$div(
    id    = id,
    class = "kpi-card",
    style = sprintf("--kpi-accent:%s; --kpi-icon-bg:%s;", pal$accent, pal$icon_bg),
    tags$div(
      class = "kpi-card-inner",
      tags$div(
        class = "kpi-card-text",
        tags$div(class = "kpi-label", label),
        tags$div(class = "kpi-value", id = paste0(id, "_value"), value),
        if (!is.null(subtitle)) tags$div(class = "kpi-subtitle", subtitle)
      ),
      tags$div(
        class = "kpi-card-icon",
        tags$i(class = sprintf("fa fa-%s", icon), `aria-hidden` = "true")
      )
    )
  )
}

#' Flex row of kpi_card() elements.
kpi_row <- function(...) {
  tags$div(class = "kpi-row", tagList(list(...)))
}


# ── BUDGET PROGRESS BAR ───────────────────────────────────────────────────────

#' Render a labelled progress bar for one budget category.
#' Communicates status via colour AND text (never colour alone).
#'
#' @param category        Category name.
#' @param used            Amount spent.
#' @param budget          Budget amount (0 = no limit).
#' @param currency_symbol Currency symbol.
#' @return shiny::tagList
budget_progress_bar <- function(category, used, budget,
                                 currency_symbol = "\u20b9") {
  has_budget  <- !is.na(budget) && budget > 0
  pct_raw     <- if (has_budget) round(used / budget * 100, 1) else 0
  pct_display <- min(pct_raw, 100)

  bar_color <- if (!has_budget)    "#94A3B8"
  else if (pct_raw >= 100) "var(--c-danger)"
  else if (pct_raw >= 80)  "var(--c-warning)"
  else                     "var(--c-success)"

  over_budget <- has_budget && used > budget
  near_limit  <- has_budget && pct_raw >= 80 && pct_raw < 100

  label_right <- if (has_budget)
    sprintf("%s\u00a0%.0f / %s\u00a0%.0f", currency_symbol, used, currency_symbol, budget)
  else
    sprintf("%s\u00a0%.0f (no budget set)", currency_symbol, used)

  pct_label <- if (has_budget) sprintf("%.0f%%", pct_raw) else "\u2014"

  tags$div(
    class = "budget-bar-container",
    # Header
    tags$div(
      class = "budget-bar-header",
      tags$span(class = "budget-bar-category", category),
      tags$span(
        class = if (over_budget) "budget-bar-amounts over-budget" else "budget-bar-amounts",
        label_right
      )
    ),
    # Track
    tags$div(
      class = "budget-bar-track",
      tags$div(
        class = "budget-bar-fill",
        style = sprintf("width:%s%%; background:%s;", pct_display, bar_color),
        role = "progressbar",
        `aria-valuenow` = pct_display, `aria-valuemin` = "0", `aria-valuemax` = "100",
        `aria-label` = sprintf("%s: %s%%", category, pct_display)
      )
    ),
    # Footer
    tags$div(
      class = "budget-bar-footer",
      tags$span(class = "budget-bar-pct", pct_label),
      if (over_budget)
        tags$span(
          class = "budget-bar-warning",
          tags$i(class = "fa fa-exclamation-circle"),
          sprintf(" Over budget by %s\u00a0%.0f", currency_symbol, used - budget)
        )
      else if (near_limit)
        tags$span(
          class = "budget-bar-warning",
          style = "color:var(--c-warning);",
          tags$i(class = "fa fa-exclamation-triangle"),
          " Approaching limit"
        )
      else if (!has_budget)
        tags$span(
          style = "font-size:var(--fs-xs); color:var(--c-text-light);",
          "No limit set"
        )
    )
  )
}


# ── STATUS BADGE ──────────────────────────────────────────────────────────────

#' Inline coloured status badge.
#' @param status "ok" | "warning" | "over_budget" | "anomaly" | "success" |
#'               "error" | "info"
#' @param label  Override default label.
status_badge <- function(status, label = NULL) {
  cfg <- switch(status,
    ok          = list(cls = "badge-success",   default = "On Track"),
    warning     = list(cls = "badge-warning",   default = "Warning"),
    over_budget = list(cls = "badge-danger",    default = "Over Budget"),
    anomaly     = list(cls = "badge-danger",    default = "Anomaly"),
    success     = list(cls = "badge-success",   default = "Success"),
    error       = list(cls = "badge-danger",    default = "Error"),
    info        = list(cls = "badge-info",      default = "Info"),
    list(cls = "badge-secondary", default = status)
  )
  tags$span(
    class = paste("status-badge", cfg$cls),
    if (!is.null(label)) label else cfg$default
  )
}


# ── ALERT BOX ─────────────────────────────────────────────────────────────────

#' Dismissible alert / notification box.
#' @param message    Text (may contain HTML).
#' @param type       "success" | "warning" | "danger" | "info"
#' @param dismissible Add a close button?
alert_box <- function(message, type = "info", dismissible = TRUE) {
  icon_map <- c(success = "check-circle", warning = "exclamation-triangle",
                danger  = "times-circle",  info    = "info-circle")
  icon_nm  <- icon_map[[type]] %||% "info-circle"

  tags$div(
    class = sprintf("alert-box alert-%s", type),
    role  = "alert",
    tags$i(class = sprintf("fa fa-%s alert-icon", icon_nm)),
    tags$span(class = "alert-message", HTML(message)),
    if (dismissible)
      tags$button(
        type = "button", class = "alert-close",
        `aria-label` = "Close",
        tags$span(`aria-hidden` = "true", HTML("&times;"))
      )
  )
}


# ── EMPTY STATE ───────────────────────────────────────────────────────────────

#' Centred empty-state block.
#' @param message     Primary message.
#' @param sub_message Secondary hint (optional).
#' @param icon        Font Awesome icon name.
empty_state_ui <- function(message, sub_message = NULL, icon = "inbox") {
  tags$div(
    class = "empty-state",
    tags$div(
      class = "empty-state-inner",
      tags$div(
        class = "empty-state-icon",
        tags$i(class = sprintf("fa fa-%s", icon), `aria-hidden` = "true")
      ),
      tags$p(class = "empty-state-message", message),
      if (!is.null(sub_message))
        tags$p(class = "empty-state-sub", sub_message)
    )
  )
}


# ── SECTION HEADER ────────────────────────────────────────────────────────────

#' Section title bar with optional subtitle and right-side content.
#' @param title    Section title.
#' @param subtitle Smaller secondary text (optional).
#' @param right_ui Shiny tag placed on the right (optional).
section_header <- function(title, subtitle = NULL, right_ui = NULL) {
  tags$div(
    class = "section-header",
    tags$div(
      class = "section-header-left",
      tags$h4(class = "section-title", title),
      if (!is.null(subtitle)) tags$p(class = "section-subtitle", subtitle)
    ),
    if (!is.null(right_ui))
      tags$div(class = "section-header-right", right_ui)
  )
}


# ── LOADING SPINNER ───────────────────────────────────────────────────────────

#' Inline animated loading spinner.
loading_spinner <- function(label = "Loading\u2026") {
  tags$div(
    class = "loading-spinner-container",
    tags$div(class = "loading-spinner"),
    if (!is.null(label)) tags$span(class = "loading-label", label)
  )
}


# ── TRANSACTION FORM FIELDS ───────────────────────────────────────────────────

#' Shared form field set for Add and Edit modals.
#' @param ns           Namespace function (use `identity` outside a module).
#' @param date_val     Default date.
#' @param category_val Default category.
#' @param desc_val     Default description.
#' @param amount_val   Default amount.
#' @param payment_val  Default payment method.
transaction_form_fields <- function(ns           = identity,
                                     date_val     = Sys.Date(),
                                     category_val = "Food",
                                     desc_val     = "",
                                     amount_val   = "",
                                     payment_val  = "UPI") {
  tagList(
    tags$div(
      class = "form-row",
      tags$div(
        class = "form-group col-md-6",
        tags$label(`for` = ns("txn_date"), "Date"),
        shiny::dateInput(ns("txn_date"), NULL,
                         value = date_val, max = Sys.Date(),
                         format = "yyyy-mm-dd", width = "100%")
      ),
      tags$div(
        class = "form-group col-md-6",
        tags$label(`for` = ns("txn_category"), "Category"),
        shiny::selectInput(ns("txn_category"), NULL,
                           choices = VALID_CATEGORIES,
                           selected = category_val, width = "100%")
      )
    ),
    tags$div(
      class = "form-group",
      tags$label(`for` = ns("txn_description"), "Description"),
      shiny::textInput(ns("txn_description"), NULL,
                       value = desc_val,
                       placeholder = "e.g. Lunch at Haldiram\u2019s",
                       width = "100%")
    ),
    tags$div(
      class = "form-row",
      tags$div(
        class = "form-group col-md-6",
        tags$label(`for` = ns("txn_amount"), "Amount (\u20b9)"),
        shiny::numericInput(
          ns("txn_amount"), NULL,
          value = if (nzchar(as.character(amount_val))) suppressWarnings(as.numeric(amount_val)) else NA,
          min = 0.01, step = 1, width = "100%"
        )
      ),
      tags$div(
        class = "form-group col-md-6",
        tags$label(`for` = ns("txn_payment"), "Payment Method"),
        shiny::selectInput(ns("txn_payment"), NULL,
                           choices = VALID_PAYMENT_METHODS,
                           selected = payment_val, width = "100%")
      )
    )
  )
}


# ── MODAL BUILDERS ────────────────────────────────────────────────────────────

#' Modal for adding a new transaction.
add_transaction_modal <- function(ns = identity) {
  shiny::modalDialog(
    title = tags$div(
      tags$i(class = "fa fa-plus-circle",
             style = "color:var(--c-primary); margin-right:8px;"),
      "Add Transaction"
    ),
    size = "m", easyClose = FALSE,
    footer = tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("btn_save_add"),
                          tagList(tags$i(class = "fa fa-save"), " Save"),
                          class = "btn btn-primary")
    ),
    shiny::uiOutput(ns("add_validation_msg")),
    transaction_form_fields(ns = ns)
  )
}

#' Modal for editing an existing transaction (pre-filled).
edit_transaction_modal <- function(ns = identity, txn_row) {
  shiny::modalDialog(
    title = tags$div(
      tags$i(class = "fa fa-edit",
             style = "color:var(--c-warning); margin-right:8px;"),
      "Edit Transaction"
    ),
    size = "m", easyClose = FALSE,
    footer = tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("btn_save_edit"),
                          tagList(tags$i(class = "fa fa-save"), " Save Changes"),
                          class = "btn btn-warning")
    ),
    shiny::uiOutput(ns("edit_validation_msg")),
    transaction_form_fields(
      ns           = ns,
      date_val     = as.Date(txn_row$date),
      category_val = txn_row$category,
      desc_val     = txn_row$description,
      amount_val   = txn_row$amount,
      payment_val  = txn_row$payment_method
    )
  )
}

#' Confirmation modal for permanently deleting a transaction.
delete_confirm_modal <- function(ns = identity, txn_row,
                                  currency_symbol = "\u20b9") {
  shiny::modalDialog(
    title = tags$div(
      tags$i(class = "fa fa-trash",
             style = "color:var(--c-danger); margin-right:8px;"),
      "Delete Transaction"
    ),
    size = "s", easyClose = TRUE,
    footer = tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("btn_confirm_delete"),
                          tagList(tags$i(class = "fa fa-trash"), " Delete"),
                          class = "btn btn-danger")
    ),
    tags$div(
      class = "delete-confirm-body",
      tags$p("Are you sure you want to permanently delete this transaction?"),
      tags$table(
        class = "delete-summary-table",
        tags$tr(tags$th("Date"),        tags$td(as.character(txn_row$date))),
        tags$tr(tags$th("Category"),    tags$td(txn_row$category)),
        tags$tr(tags$th("Description"), tags$td(txn_row$description)),
        tags$tr(tags$th("Amount"),
                tags$td(sprintf("%s\u00a0%.2f", currency_symbol,
                                as.numeric(txn_row$amount))))
      ),
      tags$p(class = "delete-warning",
             tags$i(class = "fa fa-exclamation-triangle"),
             tags$strong(" This action cannot be undone."))
    )
  )
}


# ── IMPORT ERROR TABLE ────────────────────────────────────────────────────────

#' DT table of row-level import validation errors.
import_error_table <- function(row_errors) {
  if (is.null(row_errors) || nrow(row_errors) == 0L) return(NULL)
  DT::datatable(
    row_errors,
    colnames  = c("Row #", "Field", "Value Found", "Reason"),
    rownames  = FALSE,
    escape    = TRUE,
    class     = "display compact",
    options   = list(
      pageLength = 10, dom = "tip", scrollX = TRUE,
      columnDefs = list(list(width = "40%", targets = 3))
    )
  ) %>%
    DT::formatStyle("reason", color = "var(--c-danger)") %>%
    DT::formatStyle("row",    fontWeight = "bold", textAlign = "center")
}


# ── FORECAST SUMMARY CARD ─────────────────────────────────────────────────────

#' Stats card shown beneath the forecast chart.
#' Accepts both old ($avg_predicted) and new ($avg_daily_forecast) field names.
forecast_summary_card <- function(summary_stats, model_label,
                                   model_details  = NULL,
                                   horizon_days   = 30L,
                                   currency_symbol = "\u20b9") {
  if (is.null(summary_stats)) return(NULL)

  avg_val   <- summary_stats$avg_daily_forecast %||%
               summary_stats$avg_predicted %||% 0
  total_val <- summary_stats$total_forecast %||%
               summary_stats$total_predicted %||% 0

  tags$div(
    class = "forecast-summary-card",
    tags$div(
      class = "forecast-summary-header",
      tags$span(class = "forecast-model-label",
                tags$i(class = "fa fa-robot"), " ", model_label),
      tags$span(class = "forecast-horizon",
                sprintf("Horizon: %d days", as.integer(horizon_days)))
    ),
    tags$div(
      class = "forecast-summary-stats",
      tags$div(
        class = "forecast-stat",
        tags$div(class = "forecast-stat-label", "Avg Projected / Day"),
        tags$div(class = "forecast-stat-value",
                 format_currency(avg_val, currency_symbol))
      ),
      tags$div(
        class = "forecast-stat",
        tags$div(class = "forecast-stat-label",
                 sprintf("Total Projected (%d days)", as.integer(horizon_days))),
        tags$div(class = "forecast-stat-value",
                 format_currency(total_val, currency_symbol))
      ),
      if (!is.null(model_details) && !is.na(model_details$rmse))
        tags$div(
          class = "forecast-stat",
          tags$div(class = "forecast-stat-label", "In-sample RMSE"),
          tags$div(class = "forecast-stat-value",
                   format_currency(model_details$rmse, currency_symbol))
        ),
      if (!is.null(model_details) && !is.na(model_details$n_obs))
        tags$div(
          class = "forecast-stat",
          tags$div(class = "forecast-stat-label", "Training Days"),
          tags$div(class = "forecast-stat-value",
                   as.character(model_details$n_obs))
        )
    )
  )
}


# ── ANOMALY SUMMARY CARD ──────────────────────────────────────────────────────

#' Summary header card for the Anomaly Detection tab.
#' Handles status: "ok", "no_anomalies", "insufficient_data", "error".
anomaly_summary_card <- function(result, currency_symbol = "\u20b9") {
  if (is.null(result)) return(NULL)

  # Insufficient data or error → simple alert
  if (result$status %in% c("insufficient_data", "error")) {
    msg  <- result$message %||% result$status_message %||%
            "Anomaly detection could not run."
    type <- if (result$status == "error") "danger" else "warning"
    return(alert_box(msg, type = type, dismissible = FALSE))
  }

  # No anomalies found
  if (result$status == "no_anomalies") {
    msg <- result$status_message %||% result$message %||%
           "No unusually large transactions detected."
    return(alert_box(msg, type = "success", dismissible = FALSE))
  }

  # Anomalies found — build the summary card
  cnt <- result$flagged_count %||% 0L
  tot <- result$total_checked %||% 0L
  amt <- result$flagged_amount_total %||%
         sum(result$flagged_df$amount, na.rm = TRUE) %||% 0

  cats <- if (!is.null(result$categories_flagged) &&
              length(result$categories_flagged) > 0L)
    paste(result$categories_flagged, collapse = ", ")
  else "—"

  tags$div(
    class = "anomaly-summary-card",
    tags$div(
      class = "anomaly-summary-header",
      tags$span(
        class = "anomaly-badge-large",
        tags$i(class = "fa fa-exclamation-triangle"),
        sprintf(" %d Anomal%s Detected",
                cnt, if (cnt == 1L) "y" else "ies")
      ),
      tags$span(
        class = "anomaly-method-label",
        sprintf("IQR method | Checked %d transaction%s",
                tot, if (tot == 1L) "" else "s")
      )
    ),
    tags$div(
      class = "anomaly-summary-stats",
      tags$div(
        class = "anomaly-stat",
        tags$span(class = "anomaly-stat-label", "Total Flagged Amount"),
        tags$span(class = "anomaly-stat-value",
                  format_currency(amt, currency_symbol))
      ),
      tags$div(
        class = "anomaly-stat",
        tags$span(class = "anomaly-stat-label", "Categories Affected"),
        tags$span(class = "anomaly-stat-value", cats)
      )
    ),
    tags$p(
      class = "anomaly-disclaimer",
      tags$i(class = "fa fa-info-circle"),
      " Flagged transactions are statistically unusual relative to category ",
      "spending patterns. They are not automatically classified as errors."
    )
  )
}
