# =============================================================================
# requirements.R
# Run this file once, manually, before launching the application for the
# first time.  It installs any missing packages but does NOT reinstall ones
# that are already present.
#
# app.R should source only check_packages() from this file, which verifies
# that every required package is available without triggering any installs.
# =============================================================================

required_packages <- c(
  "shiny",
  "tidyverse",
  "ggplot2",
  "plotly",
  "DT",
  "lubridate",
  "readxl",
  "DBI",
  "RSQLite",
  "forecast",
  "scales",
  "shinyjs",
  "writexl"
)

# -----------------------------------------------------------------------------
# install_packages()
# Installs every package in required_packages that is not yet present.
# Call this function once from an interactive R session, not from app.R.
# -----------------------------------------------------------------------------
install_packages <- function() {
  missing_pkgs <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_pkgs) == 0) {
    message("All required packages are already installed.")
    return(invisible(NULL))
  }

  message(sprintf(
    "Installing %d missing package(s): %s",
    length(missing_pkgs),
    paste(missing_pkgs, collapse = ", ")
  ))

  install.packages(missing_pkgs, dependencies = TRUE)
  message("Installation complete.")
}

# -----------------------------------------------------------------------------
# check_packages()
# Verifies that every required package can be loaded.  Returns TRUE if all
# packages pass, FALSE otherwise.  Safe to call from app.R at startup because
# it never installs anything.
# -----------------------------------------------------------------------------
check_packages <- function() {
  results <- vapply(required_packages, function(pkg) {
    tryCatch(
      { requireNamespace(pkg, quietly = TRUE) },
      error = function(e) FALSE
    )
  }, logical(1))

  if (all(results)) {
    message("Startup check passed: all required packages are available.")
    return(invisible(TRUE))
  }

  missing_pkgs <- names(results[!results])
  stop(sprintf(
    paste(
      "Startup check failed. The following package(s) are not installed:\n  %s\n",
      "Run `source('requirements.R')` then `install_packages()` to fix this."
    ),
    paste(missing_pkgs, collapse = "\n  ")
  ))
}

# -----------------------------------------------------------------------------
# When this file is sourced directly (e.g. Rscript requirements.R), run both
# the installer and the verification check automatically.
# -----------------------------------------------------------------------------
if (!interactive()) {
  install_packages()
  check_packages()
}
