# =============================================================================
# app.R
# Entry point for the Personal Expense & Budget Intelligence System.
#
# This file does exactly three things:
#   1. Runs the startup package check (never installs — see requirements.R).
#   2. Sources all R modules in strict dependency order.
#   3. Calls shinyApp(ui, server).
#
# Rules
# -----
#   - No logic, reactive expressions, or UI layout belong here.
#   - Business logic   → R/database.R, R/analytics.R, R/budget.R, etc.
#   - UI layout        → R/ui.R
#   - Server logic     → R/server.R
#   - Shared constants → R/validation.R
#
# First-time setup
# ----------------
#   source("requirements.R")
#   install_packages()        # installs missing packages once; safe to re-run
# =============================================================================


# ---------------------------------------------------------------------------
# 1. PACKAGE CHECK
#    Aborts with a clear message if any required package is missing.
#    Does NOT reinstall — run install_packages() once from requirements.R.
# ---------------------------------------------------------------------------
source("requirements.R")
check_packages()


# ---------------------------------------------------------------------------
# 2. SOURCE MODULES
#
# Dependency order — each file may only use symbols defined in files that
# appear ABOVE it in this list.
#
#  Layer 0 — no intra-project dependencies
#    validation.R       domain constants (VALID_CATEGORIES, etc.), ID
#                       generators, all input/import validators
#
#  Layer 1 — depends on validation.R only
#    database.R         SQLite CRUD: connect_database(), insert_transaction(),
#                       get_transactions(), insert_budget(), get_budgets(), …
#
#  Layer 2 — depends on validation.R + database.R
#    data_processing.R  aggregation (daily/weekly/monthly summaries),
#                       KPI stats, import_csv(), import_excel()
#
#  Layer 3 — depends on layers 0-2
#    analytics.R        calculate_*() pure-computation functions +
#                       Plotly chart functions (plot_daily_trend(), etc.)
#    budget.R           calculate_budget_status(), get_monthly_budget(), …
#    anomaly_detection.R detect_transaction_anomalies(), get_anomaly_summary()
#    prediction.R       prepare_forecast_data(), train_expense_forecast(),
#                       generate_expense_forecast(), run_forecast(), …
#
#  Layer 4 — depends on all layers above
#    reporting.R        export_transactions_csv/excel(), export_anomaly_csv(),
#                       generate_report_data(), generate_monthly_report()
#
#  Layer 5 — UI helpers, depends on validation.R + shiny
#    ui_helpers.R       kpi_card(), budget_progress_bar(), alert_box(), …
#
#  Layer 6 — assembled UI and server, depend on everything above
#    ui.R               ui object (navbarPage, all 8 tabs)
#    server.R           server function (all reactives, observers, outputs)
# ---------------------------------------------------------------------------

# Load every module into one environment. Shiny evaluates app.R inside a
# private env; source(..., local = FALSE) would put constants in .GlobalEnv
# while UI helpers close over the app env — causing "VALID_CATEGORIES not found".
.app_env <- new.env(parent = globalenv())

.app_modules <- c(
  "validation.R",
  "database.R",
  "data_processing.R",
  "analytics.R",
  "budget.R",
  "anomaly_detection.R",
  "prediction.R",
  "reporting.R",
  "ui_helpers.R",
  "ui.R",
  "server.R"
)

for (mod in .app_modules) {
  path <- file.path("R", mod)
  if (!file.exists(path)) {
    stop(sprintf("Missing module file: %s", path), call. = FALSE)
  }
  sys.source(path, envir = .app_env, encoding = "UTF-8")
}


# ---------------------------------------------------------------------------
# 3. LAUNCH
# ---------------------------------------------------------------------------
shiny::shinyApp(ui = .app_env$ui, server = .app_env$server)
