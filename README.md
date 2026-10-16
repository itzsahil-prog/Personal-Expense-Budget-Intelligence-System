# Personal Expense & Budget Intelligence System

A Shiny application for tracking personal expenses, monitoring budgets, detecting anomalies, and forecasting future spending.

---

## Project Structure

```
expense-intelligence/
├── app.R                   # Entry point — sources modules, launches the app
├── requirements.R          # Package installer + startup check
│
├── R/
│   ├── validation.R        # Domain constants (categories, payment methods) and all input validators
│   ├── database.R          # SQLite CRUD layer — all DB access lives here
│   ├── data_processing.R   # Aggregation, KPI computation, budget status
│   ├── analytics.R         # Plotly chart functions (daily / weekly / monthly / category)
│   ├── anomaly_detection.R # IQR and Z-score outlier detection
│   ├── prediction.R        # ETS / ARIMA / naive expense forecasting
│   ├── reporting.R         # CSV and Excel export functions
│   ├── ui_helpers.R        # Reusable Shiny UI component constructors
│   ├── ui.R                # Application UI definition (to be created)
│   └── server.R            # Application server logic (to be created)
│
├── data/
│   ├── expenses.db         # SQLite database (auto-created on first run)
│   ├── sample_data.csv     # Seed transactions (January 2025 – February 2026, 1 500 rows)
│   └── budgets.csv         # Seed monthly budgets (July–September 2026)
│
├── models/                 # Reserved for saved forecast model objects
├── reports/                # Reserved for generated report files
│
├── www/
│   └── style.css           # Application stylesheet
│
└── tests/
    └── testthat/           # Unit tests
```

---

## Features

| Tab | Description |
|-----|-------------|
| Dashboard | KPI cards (total spend, current month, today, average daily), recent transactions |
| Transactions | Add, edit, delete, search, filter, and export transactions |
| Budget | Set monthly category budgets, track utilisation with progress bars |
| Analytics | Daily, weekly, and monthly trend charts; category and payment breakdowns |
| Anomaly Detection | IQR and Z-score based outlier flagging with per-category analysis |
| Forecasting | ETS/ARIMA expense forecasting with confidence intervals |
| Import | Bulk-import transactions from CSV or Excel |
| Reports | Generate and download monthly Excel reports and CSV exports |

---

## First-Time Setup

### 1. Install R

Download from [https://cran.r-project.org](https://cran.r-project.org). R 4.2 or later is recommended.

### 2. Install required packages

Open an R console or RStudio, navigate to the project folder, and run:

```r
source("requirements.R")
install_packages()
```

This installs all required packages. It only installs packages that are not already present — it will not reinstall or downgrade existing packages.

### 3. Load sample data (optional)

To seed the database with three months of realistic transactions and budgets, run the following once from an R session:

```r
source("requirements.R")
check_packages()
source("R/validation.R")
source("R/database.R")

con <- db_connect()
db_initialize(con)

# Import sample transactions
txns <- read.csv("data/sample_data.csv", stringsAsFactors = FALSE)
source("R/data_processing.R")
normalised <- normalise_import_df(txns)
db_bulk_insert_transactions(con, normalised)

# Import sample budgets
budgets <- read.csv("data/budgets.csv", stringsAsFactors = FALSE)
for (i in seq_len(nrow(budgets))) {
  db_upsert_budget(con, budgets$year[i], budgets$month[i],
                   budgets$category[i], budgets$budget_amount[i])
}

db_disconnect(con)
message("Sample data loaded.")
```

### 4. Run the application

```r
shiny::runApp(".")
```

Or open `app.R` in RStudio and click **Run App**.

---

## Deploy to shinyapps.io

Hosted instances start the app **non-interactively**. Two common causes of **“The application failed to start (exit status 1)”** are:

1. **Startup package check** — `check_packages()` requires every package in `requirements.R`. The bundle must include them (use a manifest; do not rely on the `tidyverse` meta-package if the code only loads `dplyr` / `readr`).
2. **SQLite path** — the deployed app directory is not writable; the database is created under `tempdir()` when running on shinyapps.io (see `R/database.R`).

From the project root in RStudio:

```r
install.packages(c("rsconnect", "shiny"))
rsconnect::setAccountInfo(...)   # once, from your shinyapps.io token

rsconnect::writeManifest(".")    # records dependencies from your code
rsconnect::deployApp(".")
```

After changing `requirements.R` or adding packages, run `writeManifest(".")` again before redeploying.

View startup errors on the shinyapps.io dashboard under **Logs** for the application instance.

---

## Required Packages

| Package | Purpose |
|---------|---------|
| shiny | Web application framework |
| dplyr | Data manipulation |
| readr | CSV parsing for import/export |
| ggplot2 | Chart rendering (used internally by some helpers) |
| plotly | Interactive charts |
| DT | Interactive data tables |
| lubridate | Date and time operations |
| readxl | Read Excel files (.xlsx, .xls) |
| DBI | Database interface abstraction |
| RSQLite | SQLite driver |
| forecast | ETS and ARIMA forecasting models |
| scales | Number and currency formatting |
| shinyjs | JavaScript helpers for Shiny |
| writexl | Write Excel files (no Java dependency) |

---

## Data Model

### transactions

| Column | Type | Notes |
|--------|------|-------|
| transaction_id | TEXT PK | Format: `txn_YYYYMMDD_xxxxxx` |
| date | TEXT | ISO 8601: YYYY-MM-DD |
| category | TEXT | One of 10 valid categories |
| description | TEXT | Max 200 characters |
| amount | REAL | Must be > 0 |
| payment_method | TEXT | UPI, Cash, Card, or Bank Transfer |
| created_at | TEXT | ISO 8601 timestamp |
| updated_at | TEXT | ISO 8601 timestamp |

### budgets

| Column | Type | Notes |
|--------|------|-------|
| budget_id | TEXT PK | Format: `bgt_YYYYMM_Category` |
| year | INTEGER | |
| month | INTEGER | 1–12 |
| category | TEXT | |
| budget_amount | REAL | >= 0 |

### Valid categories

Food, Grocery, Transport, Rent/PG, Education, Shopping, Healthcare, Entertainment, Bills, Other

### Valid payment methods

UPI, Cash, Card, Bank Transfer

---

## Import File Format

CSV or Excel files must contain these columns (column names are case-insensitive):

| Column | Format | Example |
|--------|--------|---------|
| date | YYYY-MM-DD | 2026-09-15 |
| category | See valid categories above | Food |
| description | Text, max 200 chars | Lunch at cafe |
| amount | Positive number | 350 |
| payment_method | See valid methods above | UPI |

---

## Architecture Notes

- `app.R` is a thin orchestrator only. It sources modules and calls `shinyApp()`. No logic lives in it.
- All database access is centralised in `R/database.R`. No other module writes SQL.
- All domain constants (valid categories, payment methods, colour maps) are defined once in `R/validation.R` and imported by every other module.
- UI components are constructed by helper functions in `R/ui_helpers.R` — the UI definition file imports these and composes the layout.
- The `data/expenses.db` SQLite file is created automatically on first run. It is gitignored by default.

---

## Development Notes

- The `models/` directory is reserved for persisting fitted forecast model objects between sessions.
- The `reports/` directory is reserved for cached report files.

---

## Running the Tests

```r
# From the project root in RStudio or an R console:
setwd("f:/R-Projects/expense-intelligence")   # adjust to your path
install.packages(c("testthat", "withr"))       # first time only
testthat::test_dir("tests/testthat")
```

The test suite requires all packages in `requirements.R` to be installed (run `install_packages()` once) because `tests/testthat/helper-setup.R` sources every R module before any test runs. 207 tests across 6 files cover validation, database CRUD, analytics aggregation, budget status, anomaly detection, and forecast input validation.

---

## Screenshots

> Screenshots will be added once the application is running. Launch with
> `shiny::runApp(".")` and capture each tab.

---
