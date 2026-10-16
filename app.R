# Diagnostic app.R — Expense Intelligence
app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

cat("APP DIRECTORY:", app_dir, "\n")
cat("APP FILE:", normalizePath("app.R", winslash = "/", mustWork = TRUE), "\n")

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
  path <- file.path(app_dir, "R", mod)
  
  cat("Loading:", path, "\n")
  
  if (!file.exists(path)) {
    stop("Missing module: ", path)
  }
  
  sys.source(path, envir = .app_env)
  
  if (mod == "ui_helpers.R") {
    cat(
      "HELPER LOADED:",
      exists("section_header", envir = .app_env, inherits = FALSE),
      "\n"
    )
  }
}

cat(
  "HELPER BEFORE APP CREATION:",
  exists("section_header", envir = .app_env, inherits = FALSE),
  "\n"
)

cat("UI CLASS:", paste(class(.app_env$ui), collapse = ", "), "\n")

shiny::shinyApp(
  ui = .app_env$ui,
  server = .app_env$server
)
