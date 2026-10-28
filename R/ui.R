# =============================================================================
# R/ui.R
# Application UI — professional SaaS sidebar layout.
#
# Structure
#   tagList
#     useShinyjs()
#     tags$head(...)
#     div.app-shell
#       div.app-sidebar          ← persistent dark navigation rail
#       div.page-body
#         div.top-bar            ← sticky contextual header
#         div.tab-content        ← pane switcher
#           div.tab-pane#dashboard
#           div.tab-pane#transactions
#           ...
#
# Navigation fires input$main_nav via Shiny.setInputValue.
# All input IDs are unchanged from the previous version.
#
# Dependencies: shiny, shinyjs, DT, plotly, ui_helpers.R, validation.R
# =============================================================================

# Domain constants (VALID_CATEGORIES, etc.) — app.R loads validation.R first;
# this guard supports sourcing ui.R in isolation during development.
if (!exists("VALID_CATEGORIES", inherits = FALSE)) {
  val_path <- file.path("R", "validation.R")
  if (!file.exists(val_path)) {
    stop("validation.R must be loaded before ui.R.", call. = FALSE)
  }
  sys.source(val_path, envir = environment(), encoding = "UTF-8")
}

library(shiny)
library(shinyjs)
library(DT)
library(plotly)

CURRENCY_SYMBOL <- "\u20b9"
APP_TITLE       <- "  FinX"

# =============================================================================
# HELPERS
# =============================================================================

# Shared date + category + payment filter block.
# id_prefix differentiates txn_* vs an_* IDs.
.filter_panel <- function(id_prefix, title = "Filters") {
  tags$div(
    class = "filter-panel",
    section_header(title),
    tags$div(
      class = "form-group",
      tags$label("Date range"),
      dateInput(paste0(id_prefix, "_date_from"), NULL,
                value = Sys.Date() - 365, width = "100%"),
      tags$div(style = "margin-top:var(--sp-2);",
        dateInput(paste0(id_prefix, "_date_to"), NULL,
                  value = Sys.Date(), width = "100%")
      )
    ),
    tags$div(
      class = "form-group",
      selectInput(paste0(id_prefix, "_categories"), "Category",
                  choices = c("All", VALID_CATEGORIES),
                  selected = "All", width = "100%")
    ),
    tags$div(
      class = "form-group",
      selectInput(paste0(id_prefix, "_payment"), "Payment method",
                  choices = c("All", VALID_PAYMENT_METHODS),
                  selected = "All", width = "100%")
    ),
    tags$div(
      style = "display:flex; gap:6px;",
      actionButton(paste0(id_prefix, "_apply"), "Apply",
                   class = "btn btn-primary btn-sm", style = "flex:1;"),
      actionButton(paste0(id_prefix, "_reset"), "Reset",
                   class = "btn btn-default btn-sm", style = "flex:1;")
    )
  )
}

# One sidebar navigation button. Fires input$main_nav on click.
.nav_link <- function(tab_id, icon_class, label, active = FALSE) {
  tags$li(
    class = "sidebar-nav-item",
    tags$button(
      class = paste0("sidebar-nav-link", if (isTRUE(active)) " active" else ""),
      id      = paste0("nav_", tab_id),
      type    = "button",
      onclick = sprintf(
        "Shiny.setInputValue('main_nav','%s',{priority:'event'});closeSidebar();",
        tab_id
      ),
      tags$span(class = paste("nav-icon fa", icon_class), `aria-hidden` = "true"),
      tags$span(class = "nav-label", label)
    )
  )
}

# Wrap content in a standard tab pane div.
.pane <- function(tab_id, active = FALSE, ...) {
  tags$div(
    class = paste0("tab-pane", if (isTRUE(active)) " active" else ""),
    id    = tab_id,
    role  = "tabpanel",
    `aria-labelledby` = paste0("nav_", tab_id),
    ...
  )
}

# Top bar for a given pane. Shows page name + optional right content.
.top_bar <- function(icon_class, title, subtitle = NULL, right_ui = NULL) {
  tags$div(
    class = "top-bar",
    tags$div(
      class = "top-bar-left",
      # Hamburger (shown only on mobile via CSS)
      tags$button(
        class   = "hamburger-btn",
        id      = "hamburger",
        type    = "button",
        onclick = "toggleSidebar();",
        `aria-label` = "Open navigation",
        tags$i(class = "fa fa-bars")
      ),
      tags$i(class = paste("fa", icon_class),
             style = "color:var(--c-primary); margin-right:8px; font-size:15px;"),
      tags$div(
        tags$div(class = "top-bar-page-title", title),
        if (!is.null(subtitle))
          tags$div(class = "top-bar-subtitle", subtitle)
      )
    ),
    tags$div(
      class = "top-bar-right",
      if (!is.null(right_ui)) right_ui else
        tags$span(class = "top-bar-date",
                  tags$i(class = "fa fa-calendar-alt",
                         style = "margin-right:5px; opacity:.6;"),
                  format(Sys.Date(), "%d %b %Y"))
    )
  )
}

# =============================================================================
# SIDEBAR
# =============================================================================
.sidebar <- tags$nav(
  class        = "app-sidebar",
  id           = "app-sidebar",
  role         = "navigation",
  `aria-label` = "Main navigation",

  # Brand
  tags$div(
    class = "sidebar-brand",
    tags$div(
      class = "sidebar-brand-icon",
      tags$i(class = "fa fa-wallet", `aria-hidden` = "true")
    ),
    tags$div(
      tags$div(class = "sidebar-brand-text", APP_TITLE),
      tags$div(class = "sidebar-brand-sub",  "Personal Finance")
    )
  ),

  tags$div(class = "sidebar-section-label", "Overview"),
  tags$ul(
    class = "sidebar-nav",
    .nav_link("dashboard",    "fa-tachometer-alt", "Dashboard",       active = TRUE),
    .nav_link("transactions", "fa-list-ul",        "Transactions"),
    .nav_link("add_txn",      "fa-plus-circle",    "Add Expense")
  ),

  tags$div(class = "sidebar-section-label", "Planning"),
  tags$ul(
    class = "sidebar-nav",
    .nav_link("budgets",   "fa-bullseye",    "Budgets"),
    .nav_link("analytics", "fa-chart-bar",   "Analytics"),
    .nav_link("forecast",  "fa-chart-line",  "Forecast")
  ),

  tags$div(class = "sidebar-section-label", "Tools"),
  tags$ul(
    class = "sidebar-nav",
    .nav_link("anomalies",     "fa-exclamation-triangle", "Anomalies"),
    .nav_link("import_export", "fa-exchange-alt",         "Import / Export")
  ),

  tags$div(class = "sidebar-footer",
           "\u00a9 ", format(Sys.Date(), "%Y"), " Expense Intelligence")
)

# =============================================================================
# PANE 1 — DASHBOARD
# =============================================================================
.pane_dashboard <- .pane(
  "dashboard", active = TRUE,

  .top_bar("fa-tachometer-alt", "Dashboard",
           right_ui = tags$div(
             style = "display:flex; gap:8px; align-items:center;",
             uiOutput("dash_budget_month_label"),
             tags$span(class = "top-bar-date",
                       tags$i(class = "fa fa-calendar-alt",
                              style = "margin-right:5px; opacity:.6;"),
                       format(Sys.Date(), "%d %b %Y"))
           )),

  tags$div(
    class = "main-content",

    # ── Hero ──────────────────────────────────────────────────────────────
    tags$div(
      class = "dash-hero",
      tags$div(
        class = "dash-hero-content",
        tags$div(
          class = "dash-hero-eyebrow",
          tags$span(class = "dash-hero-eyebrow-dot"),
          "Personal Expense Intelligence"
        ),
        tags$h1(class = "dash-hero-title",
                "Understand where your", tags$br(), "money goes."),
        tags$p(class = "dash-hero-subtitle",
               "Track spending, set budgets, detect unusual transactions,",
               " and forecast future expenses — all in one place."),
        tags$div(
          class = "dash-hero-stats",
          tags$div(
            class = "dash-hero-stat",
            uiOutput("dash_hero_total"),
            tags$div(class = "dash-hero-stat-label", "Total spending")
          ),
          tags$div(class = "dash-hero-stat-divider"),
          tags$div(
            class = "dash-hero-stat",
            uiOutput("dash_hero_month"),
            tags$div(class = "dash-hero-stat-label", "This month")
          ),
          tags$div(class = "dash-hero-stat-divider"),
          tags$div(
            class = "dash-hero-stat",
            uiOutput("dash_hero_txn_count"),
            tags$div(class = "dash-hero-stat-label", "Transactions")
          )
        )
      )
    ),

    # ── KPI row ────────────────────────────────────────────────────────────
    uiOutput("dash_kpi_row"),

    # ── Charts row 1 ──────────────────────────────────────────────────────
    fluidRow(
      column(8,
        tags$div(class = "content-card",
          section_header("Monthly Spending Trend",
                         subtitle = "Last 12 months"),
          plotlyOutput("dash_plot_monthly", height = "280px")
        )
      ),
      column(4,
        tags$div(class = "content-card",
          section_header("By Category", subtitle = "All time"),
          plotlyOutput("dash_plot_category", height = "280px")
        )
      )
    ),

    # ── Charts row 2 ──────────────────────────────────────────────────────
    fluidRow(
      column(4,
        tags$div(class = "content-card",
          section_header("Payment Methods", subtitle = "All time"),
          plotlyOutput("dash_plot_payment", height = "260px")
        )
      ),
      column(8,
        tags$div(class = "content-card",
          section_header("Daily Spending", subtitle = "Last 30 days"),
          plotlyOutput("dash_plot_daily", height = "260px")
        )
      )
    ),

    # ── Bottom row: budget bars + recent transactions ─────────────────────
    fluidRow(
      column(6,
        tags$div(class = "content-card",
          section_header("Budget Utilisation",
                         subtitle = "Current month vs. set budgets"),
          uiOutput("dash_budget_bars")
        )
      ),
      column(6,
        tags$div(class = "content-card",
          section_header(
            "Recent Transactions",
            subtitle   = "Latest 8 records",
            right_ui   = tags$button(
              type    = "button",
              class   = "btn btn-default btn-sm",
              onclick = "Shiny.setInputValue('main_nav','transactions',{priority:'event'});",
              "View all"
            )
          ),
          uiOutput("dash_recent_transactions")
        )
      )
    )
  )
)

# =============================================================================
# PANE 2 — TRANSACTIONS
# =============================================================================
.pane_transactions <- .pane(
  "transactions",

  .top_bar("fa-list-ul", "Transactions",
           subtitle  = "Browse, search, edit and delete records",
           right_ui  = tags$div(
             style = "display:flex; gap:8px;",
             downloadButton("txn_download_csv",   tagList(tags$i(class="fa fa-file-csv"),   " CSV"),
                            class = "btn btn-default btn-sm"),
             downloadButton("txn_download_excel", tagList(tags$i(class="fa fa-file-excel"), " Excel"),
                            class = "btn btn-default btn-sm")
           )),

  tags$div(
    class = "main-content",
    fluidRow(
      column(3, .filter_panel("txn")),
      column(9,
        tags$div(
          class = "content-card",
          tags$div(
            style = "display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:8px; margin-bottom:14px;",
            tags$div(
              style = "display:flex; gap:6px; align-items:center;",
              textInput("txn_search", NULL,
                        placeholder = "Search description\u2026",
                        width = "260px"),
              actionButton("txn_search_btn",
                           tags$i(class = "fa fa-search", `aria-label` = "Search"),
                           class = "btn btn-default btn-sm")
            ),
            uiOutput("txn_count_badge")
          ),
          DT::dataTableOutput("txn_table")
        )
      )
    )
  )
)

# =============================================================================
# PANE 3 — ADD EXPENSE
# =============================================================================
.pane_add <- .pane(
  "add_txn",

  .top_bar("fa-plus-circle", "Add Expense",
           subtitle = "Record a new transaction"),

  tags$div(
    class = "main-content",
    fluidRow(
      column(
        width = 6, offset = 3,
        tags$div(
          class = "content-card content-card-raised",

          # Form header
          tags$div(
            style = "margin-bottom:var(--sp-6);",
            tags$p(class = "section-title", "New Expense"),
            tags$p(style = "font-size:var(--fs-sm); color:var(--c-text-muted); margin-top:var(--sp-1);",
                   "All fields are required except Description.")
          ),

          uiOutput("add_validation_msg"),

          # Date + Category row
          tags$div(
            class = "form-row",
            tags$div(
              class = "form-group col-md-6",
              tags$label("Date"),
              dateInput("add_date", NULL,
                        value = Sys.Date(), max = Sys.Date(), width = "100%")
            ),
            tags$div(
              class = "form-group col-md-6",
              tags$label("Category"),
              selectInput("add_category", NULL,
                          choices = VALID_CATEGORIES, width = "100%")
            )
          ),

          # Description
          tags$div(
            class = "form-group",
            tags$label("Description"),
            textInput("add_description", NULL,
                      placeholder = "e.g. Lunch at Haldiram\u2019s",
                      width = "100%")
          ),

          # Amount + Payment row
          tags$div(
            class = "form-row",
            tags$div(
              class = "form-group col-md-6",
              tags$label(paste0("Amount (", CURRENCY_SYMBOL, ")")),
              numericInput("add_amount", NULL,
                           value = NA, min = 0.01, step = 1, width = "100%")
            ),
            tags$div(
              class = "form-group col-md-6",
              tags$label("Payment method"),
              selectInput("add_payment", NULL,
                          choices = VALID_PAYMENT_METHODS, width = "100%")
            )
          ),

          tags$hr(style = "margin: 20px 0;"),

          actionButton(
            "add_save_btn",
            tagList(tags$i(class = "fa fa-check"), " Save Expense"),
            class = "btn btn-primary btn-lg btn-block",
            width = "100%"
          ),

          tags$div(style = "margin-top:14px;",
                   uiOutput("add_success_msg"))
        )
      )
    )
  )
)

# =============================================================================
# PANE 4 — BUDGETS
# =============================================================================
.pane_budgets <- .pane(
  "budgets",

  .top_bar("fa-bullseye", "Budgets",
           subtitle = "Set and track monthly spending limits"),

  tags$div(
    class = "main-content",
    fluidRow(
      column(3,
        tags$div(
          class = "filter-panel",
          section_header("Select Month"),
          tags$div(
            class = "form-group",
            tags$label("Month (YYYY-MM)"),
            textInput("budget_month", NULL,
                      value       = format(Sys.Date(), "%Y-%m"),
                      placeholder = "YYYY-MM",
                      width       = "100%"),
            tags$div(class = "form-hint", "e.g. 2026-09")
          ),
          actionButton("budget_load_btn",
                       tagList(tags$i(class = "fa fa-sync-alt"), " Load"),
                       class = "btn btn-primary btn-sm btn-block",
                       width = "100%"),
          tags$hr(),
          tags$div(
            style = "font-size:12px; color:var(--c-text-muted); line-height:1.6;",
            tags$i(class = "fa fa-info-circle", style = "margin-right:4px;"),
            "Set a budget of 0 to indicate no spending limit for that category."
          )
        )
      ),
      column(9,
        tags$div(
          class = "content-card",
          uiOutput("budget_form_ui"),
          uiOutput("budget_save_status")
        )
      )
    )
  )
)

# =============================================================================
# PANE 5 — ANALYTICS
# =============================================================================
.pane_analytics <- .pane(
  "analytics",

  .top_bar("fa-chart-bar", "Analytics",
           subtitle = "Spending trends, breakdowns and patterns"),

  tags$div(
    class = "main-content",
    fluidRow(
      column(3, .filter_panel("an")),
      column(9,
        tags$div(
          class = "content-card",
          tabsetPanel(
            type = "tabs",

            tabPanel("Spending Trends",
              tags$div(style = "padding-top:16px;",
                plotlyOutput("an_plot_monthly", height = "300px"),
                tags$br(),
                DT::dataTableOutput("an_table_monthly")
              )
            ),

            tabPanel("Categories",
              tags$div(style = "padding-top:16px;",
                fluidRow(
                  column(6, plotlyOutput("an_plot_category_bar", height = "300px")),
                  column(6, plotlyOutput("an_plot_category_pie", height = "300px"))
                ),
                tags$br(),
                DT::dataTableOutput("an_table_category")
              )
            ),

            tabPanel("Payment Methods",
              tags$div(style = "padding-top:16px;",
                fluidRow(
                  column(6, plotlyOutput("an_plot_payment_bar", height = "300px")),
                  column(6, plotlyOutput("an_plot_payment_pie", height = "300px"))
                ),
                tags$br(),
                DT::dataTableOutput("an_table_payment")
              )
            ),

            tabPanel("Daily Patterns",
              tags$div(style = "padding-top:16px;",
                plotlyOutput("an_plot_daily", height = "300px")
              )
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# PANE 6 — FORECAST
# =============================================================================
.pane_forecast <- .pane(
  "forecast",

  .top_bar("fa-chart-line", "Forecast",
           subtitle = "Statistical projection of future spending"),

  tags$div(
    class = "main-content",
    fluidRow(
      column(3,
        tags$div(
          class = "filter-panel",
          section_header("Settings"),

          sliderInput("fc_horizon", "Forecast horizon (days)",
                      min = 7, max = 90, value = 30, step = 1, width = "100%"),

          tags$div(class = "form-group",
            tags$label("Model"),
            selectInput("fc_method", NULL,
                        choices  = c("Auto (recommended)" = "auto",
                                     "ETS"                = "ets",
                                     "ARIMA"              = "arima",
                                     "Mean (naive)"       = "naive"),
                        selected = "auto",
                        width    = "100%")
          ),

          actionButton("fc_run_btn",
                       tagList(tags$i(class = "fa fa-play"), " Run Forecast"),
                       class = "btn btn-primary btn-sm btn-block",
                       width = "100%"),

          tags$div(
            class = "forecast-disclaimer",
            style = "margin-top:16px;",
            tags$i(class = "fa fa-shield-alt", `aria-hidden` = "true"),
            tags$span("Statistical estimates only. Not guaranteed future values.")
          )
        )
      ),
      column(9,
        uiOutput("fc_status_msg"),
        tags$div(
          class = "content-card",
          section_header("Spending Projection",
                         subtitle = "Historical data and forecast with confidence interval"),
          plotlyOutput("fc_chart", height = "340px")
        ),
        uiOutput("fc_summary_card")
      )
    )
  )
)

# =============================================================================
# PANE 7 — ANOMALIES
# =============================================================================
.pane_anomalies <- .pane(
  "anomalies",

  .top_bar("fa-exclamation-triangle", "Anomalies",
           subtitle = "Statistically unusual transactions detected by IQR analysis"),

  tags$div(
    class = "main-content",
    fluidRow(
      column(3,
        tags$div(
          class = "filter-panel",
          section_header("Detection Settings"),

          sliderInput("anom_multiplier", "IQR multiplier (k)",
                      min = 0.5, max = 4.0, value = 1.5,
                      step = 0.1, width = "100%"),
          tags$div(
            class = "form-hint",
            style = "margin-top:-8px; margin-bottom:12px;",
            "Upper bound = Q3 + k \u00d7 IQR"
          ),

          sliderInput("anom_min_obs",
                      "Min. observations per category",
                      min = 2, max = 20, value = 5,
                      step = 1, width = "100%"),

          actionButton("anom_run_btn",
                       tagList(tags$i(class = "fa fa-search"), " Detect"),
                       class = "btn btn-primary btn-sm btn-block",
                       width = "100%"),
          tags$hr(),
          downloadButton("anom_download",
                         tagList(tags$i(class="fa fa-download"), " Export CSV"),
                         class = "btn btn-default btn-sm btn-block",
                         width = "100%")
        )
      ),
      column(9,
        uiOutput("anom_summary_ui"),
        tags$div(
          class = "content-card",
          uiOutput("anom_stats_heading"),
          DT::dataTableOutput("anom_table")
        ),
        tags$div(
          class = "content-card",
          uiOutput("anom_category_stats_heading"),
          DT::dataTableOutput("anom_cat_stats_table")
        )
      )
    )
  )
)

# =============================================================================
# PANE 8 — IMPORT / EXPORT
# =============================================================================
.pane_import_export <- .pane(
  "import_export",

  .top_bar("fa-exchange-alt", "Import / Export",
           subtitle = "Upload transactions or download reports"),

  tags$div(
    class = "main-content",
    fluidRow(
      # ── IMPORT ────────────────────────────────────────────────────────
      column(6,
        tags$div(
          class = "content-card",
          section_header("Import Data",
                         subtitle = "Upload a CSV or Excel file"),

          fileInput(
            "import_file", NULL,
            accept      = c(".csv", ".xlsx", ".xls", "text/csv",
                            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
            buttonLabel = tagList(tags$i(class = "fa fa-upload"), " Choose file"),
            placeholder = "No file selected",
            width       = "100%"
          ),

          tags$details(
            style = "margin-bottom:var(--sp-4);",
            tags$summary(
              style = "cursor:pointer; color:var(--c-primary); font-size:13px; font-weight:600; list-style:none; display:flex; align-items:center; gap:6px;",
              tags$i(class = "fa fa-chevron-right",
                     style = "font-size:10px; transition:transform .2s;"),
              " Required columns"
            ),
            tags$ul(
              style = "font-size:13px; margin-top:10px; padding-left:18px; line-height:2; color:var(--c-text-muted);",
              tags$li(tags$code("date"),           " \u2014 YYYY-MM-DD"),
              tags$li(tags$code("category"),       " \u2014 ",
                      paste(VALID_CATEGORIES, collapse = ", ")),
              tags$li(tags$code("description"),    " \u2014 free text"),
              tags$li(tags$code("amount"),         " \u2014 positive number"),
              tags$li(tags$code("payment_method"), " \u2014 ",
                      paste(VALID_PAYMENT_METHODS, collapse = ", "))
            )
          ),

          uiOutput("import_validation_ui"),
          uiOutput("import_action_ui")
        )
      ),

      # ── EXPORT ────────────────────────────────────────────────────────
      column(6,
        tags$div(
          class = "content-card",
          section_header("Export Data",
                         subtitle = "Uses current Transactions tab filters"),
          fluidRow(
            column(6,
              downloadButton("export_csv",
                             tagList(tags$i(class = "fa fa-file-csv"), " Download CSV"),
                             class = "btn btn-success btn-block",
                             width = "100%")
            ),
            column(6,
              downloadButton("export_excel",
                             tagList(tags$i(class = "fa fa-file-excel"), " Download Excel"),
                             class = "btn btn-success btn-block",
                             width = "100%")
            )
          )
        ),

        tags$div(
          class = "content-card",
          section_header("Monthly Report",
                         subtitle = "Full workbook: transactions, budgets, anomalies, forecast"),
          tags$div(
            style = "display:flex; gap:10px; align-items:flex-end;",
            tags$div(
              style = "flex:1;",
              tags$label(class = "control-label", "Report month"),
              textInput("report_month", NULL,
                        value       = format(Sys.Date(), "%Y-%m"),
                        placeholder = "YYYY-MM",
                        width       = "100%")
            ),
            downloadButton(
              "export_monthly_report",
              tagList(tags$i(class = "fa fa-file-download"), " Generate"),
              class = "btn btn-primary",
              style = "margin-bottom:0; white-space:nowrap;"
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# JAVASCRIPT
# Tab switching, sidebar active state, mobile menu toggle
# =============================================================================
.app_js <- tags$script(HTML('
/* ── Tab switching ─────────────────────────────────────────────────────── */
document.addEventListener("DOMContentLoaded", function() {
  activateTab("dashboard");
});

$(document).on("shiny:inputchanged", function(e) {
  if (e.name === "main_nav") activateTab(e.value);
});

function activateTab(tabId) {
  document.querySelectorAll(".tab-pane").forEach(function(p) {
    p.classList.remove("active");
  });
  var target = document.getElementById(tabId);
  if (target) target.classList.add("active");

  document.querySelectorAll(".sidebar-nav-link").forEach(function(l) {
    l.classList.remove("active");
  });
  var link = document.getElementById("nav_" + tabId);
  if (link) link.classList.add("active");

  var body = document.querySelector(".page-body");
  if (body) body.scrollTop = 0;
}

/* ── Mobile sidebar toggle ─────────────────────────────────────────────── */
function toggleSidebar() {
  var sb  = document.getElementById("app-sidebar");
  var ov  = document.getElementById("sidebar-overlay");
  var btn = document.getElementById("hamburger");
  if (!sb) return;
  var open = sb.classList.toggle("open");
  if (ov)  { open ? ov.classList.add("visible") : ov.classList.remove("visible"); }
  if (btn) { btn.setAttribute("aria-label", open ? "Close navigation" : "Open navigation"); }
}

function closeSidebar() {
  var sb = document.getElementById("app-sidebar");
  var ov = document.getElementById("sidebar-overlay");
  if (sb) sb.classList.remove("open");
  if (ov) ov.classList.remove("visible");
}
'))

# =============================================================================
# ASSEMBLE UI
# =============================================================================
ui <- tagList(
  useShinyjs(),

  tags$head(
    tags$meta(name = "viewport",
              content = "width=device-width, initial-scale=1.0"),

    # Preconnect for faster Google Fonts load
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com",
              crossorigin = ""),

    # Google Fonts — Inter
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"
    ),

    # Font Awesome 5
    tags$link(
      rel         = "stylesheet",
      href        = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css",
      integrity   = "sha512-1ycn6IcaQQ40/MKBW37qJ5VcSFubFEP6P4/CJKoUMFCe2L5oSJh5PuMd8NuQ+W8hV4tRJGpjSJDyM4uy+ZaA==",
      crossorigin = "anonymous"
    ),

    # Application stylesheet
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),

    tags$title(APP_TITLE),

    # Suppress Bootstrap default padding
    tags$style(HTML("
      .navbar { display: none !important; }
      body    { padding-top: 0 !important; }
      html, body { height: 100%; }
    "))
  ),

  # Mobile overlay
  tags$div(class = "sidebar-overlay", id = "sidebar-overlay",
           onclick = "closeSidebar();"),

  tags$div(
    class = "app-shell",

    .sidebar,

    # Page body wraps top-bar + tab panes
    tags$div(
      class = "page-body",
      tags$div(
        class = "tab-content",
        .pane_dashboard,
        .pane_transactions,
        .pane_add,
        .pane_budgets,
        .pane_analytics,
        .pane_forecast,
        .pane_anomalies,
        .pane_import_export
      )
    )
  ),

  .app_js
)
