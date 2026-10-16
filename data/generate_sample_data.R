# =============================================================================
# data/generate_sample_data.R
#
# Generates a realistic 1500-row synthetic expense dataset.
# Output: data/sample_data.csv
#
# Design
# ------
# Date range   : 2025-01-01 to 2026-02-28  (14 months)
# Target rows  : exactly 1500
# Trim / pad   : if raw generation overshoots, excess non-anchor rows are
#                removed; if it undershoots, extra Food/Transport rows are
#                added until the target is met.
# Anomalies    : 18 hand-crafted rows with amounts 4-20x the category mean
#                so both IQR and Z-score detectors have clear signal.
# IDs          : txn_YYYYMMDD_NNNNN  (zero-padded sequential per final sort)
#
# Run: Rscript data/generate_sample_data.R
# =============================================================================

set.seed(20250101)

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

# ---------------------------------------------------------------------------
# CONSTANTS
# ---------------------------------------------------------------------------
DATE_START <- as.Date("2025-01-01")
DATE_END   <- as.Date("2026-02-28")
ALL_MONTHS <- seq(DATE_START, DATE_END, by = "month")   # 14 months
ALL_DAYS   <- seq(DATE_START, DATE_END, by = "day")     # 424 days
TARGET     <- 1500L

VALID_CATEGORIES     <- c("Food","Grocery","Transport","Rent/PG","Education",
                           "Shopping","Healthcare","Entertainment","Bills","Other")
VALID_PAYMENT_METHODS <- c("UPI","Cash","Card","Bank Transfer")

# ---------------------------------------------------------------------------
# ACCUMULATOR
# ---------------------------------------------------------------------------
rows <- vector("list", 2000L)
n    <- 0L

push <- function(date, category, description, amount, payment_method) {
  n <<- n + 1L
  rows[[n]] <<- data.frame(
    date           = format(as.Date(date), "%Y-%m-%d"),
    category       = category,
    description    = description,
    amount         = max(round(amount, 0), 1),
    payment_method = payment_method,
    stringsAsFactors = FALSE
  )
}

pm <- function(weights) {
  sample(VALID_PAYMENT_METHODS, 1,
         prob = weights / sum(weights))
}

# ---------------------------------------------------------------------------
# 1. RENT/PG  — 1 payment on the 1st of every month
# ---------------------------------------------------------------------------
for (mo in ALL_MONTHS) {
  mo   <- as.Date(mo)
  rent <- if (mo >= as.Date("2025-07-01")) 9500 else 8500
  push(mo, "Rent/PG", "Monthly PG accommodation rent", rent, "Bank Transfer")
}
# 14 rows

# ---------------------------------------------------------------------------
# 2. BILLS  — fixed per month
# ---------------------------------------------------------------------------
bill_templates <- list(
  list(desc = "Electricity bill",        base = 1650, sd = 280,  day = 5,  pm_w = c(1,0,0,0)),  # UPI
  list(desc = "Internet broadband bill", base = 999,  sd = 0,    day = 7,  pm_w = c(1,0,0,0)),
  list(desc = "Mobile recharge",         base = 299,  sd = 0,    day = 3,  pm_w = c(1,0,0,0)),
  list(desc = "Piped gas bill",          base = 430,  sd = 90,   day = 12, pm_w = c(1,0,0,0))
)
ott_sched <- list(
  list(desc = "Netflix subscription",    amt = 199, months = c(1,4,7,10), day = 20, pm_w = c(0,0,1,0)),
  list(desc = "Spotify premium",         amt = 179, months = c(1,3,5,7,9,11), day = 21, pm_w = c(0,0,1,0)),
  list(desc = "Amazon Prime renewal",    amt = 299, months = c(2,5,8,11), day = 22, pm_w = c(0,0,1,0)),
  list(desc = "YouTube Premium",         amt = 129, months = c(1,7),       day = 23, pm_w = c(0,0,1,0))
)

for (mo in ALL_MONTHS) {
  mo   <- as.Date(mo)
  mnum <- month(mo)
  # Always: electricity, internet, mobile, gas (gas ~70%)
  for (bt in bill_templates) {
    if (identical(bt$desc, "Piped gas bill") && runif(1) < 0.30) next
    noise <- if (bt$sd > 0) rnorm(1, 0, bt$sd) else 0
    d <- mo + (bt$day - 1L)
    if (d > DATE_END) next
    push(d, "Bills", bt$desc, bt$base + noise, "UPI")
  }
  # OTT — conditional on month
  for (ott in ott_sched) {
    if (mnum %in% ott$months) {
      d <- mo + (ott$day - 1L)
      if (d > DATE_END) next
      push(d, "Bills", ott$desc, ott$amt, "Card")
    }
  }
}
# ~70-80 rows

# ---------------------------------------------------------------------------
# 3. FOOD  — 3-4 transactions per day
# ---------------------------------------------------------------------------
food_meals <- list(
  breakfast = list(
    descs = c("Idli vada at canteen","Poha at stall","Bread omelette",
              "Paratha from dhaba","Swiggy breakfast order",
              "Hotel breakfast buffet","Upma at mess","Banana and chai"),
    mean_log = log(110), sd_log = 0.35,
    pm_w     = c(0.35, 0.50, 0.10, 0.05)
  ),
  lunch = list(
    descs = c("Office cafeteria lunch","Swiggy lunch order",
              "Zomato lunch order","Thali at restaurant",
              "Canteen lunch","Mess food","Lunch with colleagues",
              "Quick bite at food court","Domino's lunch deal"),
    mean_log = log(210), sd_log = 0.42,
    pm_w     = c(0.48, 0.22, 0.18, 0.12)
  ),
  snacks = list(
    descs = c("Tea and biscuits","Evening snacks","Chai at stall",
              "Vada pav","Cold coffee","Juice corner","Nimbu pani"),
    mean_log = log(60), sd_log = 0.30,
    pm_w     = c(0.15, 0.65, 0.10, 0.10)
  ),
  dinner = list(
    descs = c("Swiggy dinner order","Zomato dinner","Restaurant dinner",
              "Dhaba dinner","Mess food — dinner","Pizza delivery",
              "Biryani from nearby shop","Home-cooked supplement"),
    mean_log = log(300), sd_log = 0.52,
    pm_w     = c(0.42, 0.18, 0.22, 0.18)
  )
)

for (d in ALL_DAYS) {
  d   <- as.Date(d)
  dow <- wday(d)     # 1=Sun, 7=Sat
  n_extra_snack   <- sample(0:1, 1, prob = c(0.55, 0.45))
  meals_today     <- c("breakfast", "lunch", "dinner")
  if (n_extra_snack > 0) meals_today <- c(meals_today, "snacks")

  for (meal in meals_today) {
    tmpl <- food_meals[[meal]]
    amt  <- rlnorm(1, tmpl$mean_log, tmpl$sd_log)
    desc <- sample(tmpl$descs, 1)
    p    <- pm(tmpl$pm_w)
    push(d, "Food", desc, amt, p)
  }
}
# ~1300 rows so far — will be trimmed

# ---------------------------------------------------------------------------
# 4. GROCERY  — 4 weekly shops + 3-5 top-ups per month
# ---------------------------------------------------------------------------
grocery_main  <- c("Big Bazaar weekly groceries","DMart monthly restock",
                   "Reliance Fresh groceries","More Supermarket shop",
                   "Blinkit grocery order","Zepto grocery delivery",
                   "Supermarket monthly restock","Weekend grocery run",
                   "JioMart grocery order")
grocery_topup <- c("Milk and curd from dairy","Bread eggs butter",
                   "Fruits and vegetables","Onion potato tomato",
                   "Pulses and rice","Packaged snacks","Cooking oil")

for (mo in ALL_MONTHS) {
  mo       <- as.Date(mo)
  dim_mo   <- days_in_month(mo)
  # 4 main shops
  sdays <- sort(sample(seq_len(dim_mo), 4, replace = FALSE))
  for (sd in sdays) {
    d <- mo + (sd - 1L); if (d > DATE_END) next
    push(d, "Grocery", sample(grocery_main, 1),
         rlnorm(1, log(1350), 0.34),
         pm(c(0.35, 0.15, 0.50, 0)))
  }
  # 3-5 top-ups
  n_topup <- sample(3:5, 1)
  tdays   <- sort(sample(seq_len(dim_mo), n_topup, replace = FALSE))
  for (td in tdays) {
    d <- mo + (td - 1L); if (d > DATE_END) next
    push(d, "Grocery", sample(grocery_topup, 1),
         rlnorm(1, log(270), 0.42),
         pm(c(0.40, 0.55, 0.05, 0)))
  }
}

# ---------------------------------------------------------------------------
# 5. TRANSPORT  — weekday commute + occasional extras
# ---------------------------------------------------------------------------
commute_descs <- c("Auto to office","Metro commute","Bus to office",
                   "Rapido bike taxi","Auto from office",
                   "Metro card top-up","Bus pass top-up")
cab_descs     <- c("Ola cab","Uber ride","Rapido cab","InDrive ride")
petrol_descs  <- c("Petrol refill","Fuel at HP pump","Fuel at Indian Oil",
                   "CNG refill")
misc_descs    <- c("Parking charges","Toll charges","Railway platform ticket",
                   "Bus ticket — outstation","Airport metro")

for (d in ALL_DAYS) {
  d   <- as.Date(d)
  dow <- wday(d)
  is_weekend <- dow %in% c(1L, 7L)

  if (!is_weekend) {
    # Morning commute
    push(d, "Transport", sample(commute_descs, 1),
         rlnorm(1, log(72), 0.36),
         pm(c(0.55, 0.35, 0.10, 0)))
    # Evening commute ~82%
    if (runif(1) < 0.82)
      push(d, "Transport", sample(commute_descs, 1),
           rlnorm(1, log(68), 0.36),
           pm(c(0.55, 0.35, 0.10, 0)))
    # Mid-day cab ~18%
    if (runif(1) < 0.18)
      push(d, "Transport", sample(cab_descs, 1),
           rlnorm(1, log(175), 0.48),
           pm(c(0.78, 0.12, 0.10, 0)))
  } else {
    # Weekend outing ~62%
    if (runif(1) < 0.62)
      push(d, "Transport",
           sample(c(cab_descs, misc_descs), 1),
           rlnorm(1, log(240), 0.52),
           pm(c(0.60, 0.22, 0.18, 0)))
  }
  # Petrol: Wednesday ~55%
  if (wday(d) == 4L && runif(1) < 0.55)
    push(d, "Transport", sample(petrol_descs, 1),
         rlnorm(1, log(1020), 0.22),
         pm(c(0.50, 0.22, 0.28, 0)))
}

# ---------------------------------------------------------------------------
# 6. EDUCATION  — 1-3 per month, tiered amounts
# ---------------------------------------------------------------------------
edu_small  <- list(
  descs = c("Stationery — pens and notebooks","Photocopies and printouts",
            "Library card renewal","Exam stationery"),
  mu = 160, sd = 55
)
edu_medium <- list(
  descs = c("Udemy course purchase","Book purchase — technical reference",
            "Online workshop registration","Coursera monthly subscription",
            "Book — data science","Exam registration fee"),
  mu = 580, sd = 220
)
edu_large  <- list(
  descs = c("Semester exam fee","Professional certification exam fee",
            "Annual course subscription — Coursera Plus"),
  mu = 3200, sd = 700
)

for (mo in ALL_MONTHS) {
  mo     <- as.Date(mo)
  dim_mo <- days_in_month(mo)
  n_edu  <- sample(1:3, 1)
  edays  <- sort(sample(seq_len(dim_mo), n_edu, replace = FALSE))
  for (ed in edays) {
    d    <- mo + (ed - 1L); if (d > DATE_END) next
    tier <- sample(c("small","medium","large"), 1, prob = c(0.40, 0.50, 0.10))
    spec <- switch(tier, small = edu_small, medium = edu_medium, large = edu_large)
    push(d, "Education", sample(spec$descs, 1),
         max(rnorm(1, spec$mu, spec$sd), 10),
         pm(c(0.10, 0.10, 0.80, 0)))
  }
}

# ---------------------------------------------------------------------------
# 7. SHOPPING  — 2-4 per month
# ---------------------------------------------------------------------------
shop_small  <- c("Miscellaneous household items","Stationery purchase",
                 "Phone accessories","Umbrella","Wallet replacement",
                 "Blinkit non-grocery order","Small home decor item")
shop_medium <- c("Formal shirt","Casual t-shirts 2-pack","Jeans",
                 "Sneakers","Kurta","Jacket","Laptop bag","Backpack",
                 "Bedsheet set","Kitchen utensils set","Formal shoes")
shop_large  <- c("Wireless headphones","Running shoes — premium",
                 "Smartwatch strap","External hard drive",
                 "Keyboard and mouse combo","Office chair — compact")

for (mo in ALL_MONTHS) {
  mo     <- as.Date(mo)
  dim_mo <- days_in_month(mo)
  n_sh   <- sample(2:4, 1)
  shdays <- sort(sample(seq_len(dim_mo), n_sh, replace = FALSE))
  for (sd in shdays) {
    d    <- mo + (sd - 1L); if (d > DATE_END) next
    tier <- sample(c("small","medium","large"), 1, prob = c(0.35, 0.50, 0.15))
    if (tier == "small") {
      push(d, "Shopping", sample(shop_small, 1),
           rlnorm(1, log(340), 0.42), pm(c(0.35, 0.12, 0.53, 0)))
    } else if (tier == "medium") {
      push(d, "Shopping", sample(shop_medium, 1),
           rlnorm(1, log(1350), 0.46), pm(c(0.28, 0.10, 0.62, 0)))
    } else {
      push(d, "Shopping", sample(shop_large, 1),
           rlnorm(1, log(3400), 0.40), pm(c(0.20, 0.05, 0.75, 0)))
    }
  }
}

# ---------------------------------------------------------------------------
# 8. HEALTHCARE  — 0-2 per month
# ---------------------------------------------------------------------------
hc_small  <- c("Pharmacy — cold and flu medicine","Pharmacy — vitamins",
               "Eye drops","Band-aids and antiseptic","Cough syrup",
               "Paracetamol strip","Antacid tablets","ORS sachets")
hc_medium <- c("GP doctor consultation","Dermatologist visit",
               "Dental checkup","Physiotherapy session",
               "Eye test and prescription","Blood test — CBC routine",
               "Diagnostic lab — urine test")
hc_large  <- c("Specialist doctor consultation","Dental treatment — filling",
               "Blood test + ultrasound package","Annual health checkup",
               "ENT specialist visit","Ophthalmologist appointment")

for (mo in ALL_MONTHS) {
  mo     <- as.Date(mo)
  dim_mo <- days_in_month(mo)
  n_hc   <- sample(0:2, 1, prob = c(0.15, 0.55, 0.30))
  if (n_hc == 0) next
  hdays  <- sort(sample(seq_len(dim_mo), n_hc, replace = FALSE))
  for (hd in hdays) {
    d    <- mo + (hd - 1L); if (d > DATE_END) next
    tier <- sample(c("small","medium","large"), 1, prob = c(0.45, 0.40, 0.15))
    descs_list <- switch(tier, small = hc_small, medium = hc_medium, large = hc_large)
    mu   <- switch(tier, small = 270, medium = 580, large = 1750)
    sig  <- switch(tier, small = 100, medium = 220, large = 600)
    push(d, "Healthcare", sample(descs_list, 1),
         max(rnorm(1, mu, sig), 20),
         pm(c(0.40, 0.30, 0.30, 0)))
  }
}

# ---------------------------------------------------------------------------
# 9. ENTERTAINMENT  — 2-4 per month
# ---------------------------------------------------------------------------
ent_descs <- c("BookMyShow — movie tickets","Movie tickets — PVR",
               "Concert / live event ticket","Museum / gallery entry",
               "Escape room with friends","Board game cafe outing",
               "Stand-up comedy show","BookMyShow — event pass",
               "Go-karting session","Bowling alley","Sporting event ticket",
               "Amusement park entry")

for (mo in ALL_MONTHS) {
  mo     <- as.Date(mo)
  dim_mo <- days_in_month(mo)
  n_ent  <- sample(2:4, 1)
  edays  <- sort(sample(seq_len(dim_mo), n_ent, replace = FALSE))
  for (ed in edays) {
    d <- mo + (ed - 1L); if (d > DATE_END) next
    tier_idx <- sample(1:3, 1, prob = c(0.40, 0.40, 0.20))
    mu  <- c(340, 680, 1150)[tier_idx]
    sig <- c(0.36, 0.42, 0.46)[tier_idx]
    push(d, "Entertainment", sample(ent_descs, 1),
         rlnorm(1, log(mu), sig),
         pm(c(0.52, 0.14, 0.34, 0)))
  }
}

# ---------------------------------------------------------------------------
# 10. OTHER  — 1-3 per month
# ---------------------------------------------------------------------------
other_descs <- c("Haircut and grooming","Barber shop",
                 "Laundry service","Ironing charges",
                 "Gym membership monthly fee","Swimming pool entry",
                 "Donation — temple","Newspaper subscription",
                 "Courier / delivery charge","Key duplicate",
                 "Society maintenance fee","Drinking water can",
                 "Car wash","ATM withdrawal charge")

for (mo in ALL_MONTHS) {
  mo     <- as.Date(mo)
  dim_mo <- days_in_month(mo)
  n_oth  <- sample(1:3, 1)
  odays  <- sort(sample(seq_len(dim_mo), n_oth, replace = FALSE))
  for (od in odays) {
    d <- mo + (od - 1L); if (d > DATE_END) next
    push(d, "Other", sample(other_descs, 1),
         rlnorm(1, log(290), 0.62),
         pm(c(0.35, 0.50, 0.15, 0)))
  }
}

# ---------------------------------------------------------------------------
# 11. INJECTED ANOMALIES (18 rows)
# Each is 4–20× the category mean — clear signal for both IQR and Z-score.
# ---------------------------------------------------------------------------
anomalies <- list(
  # Food
  list(d="2025-02-14", cat="Food",         desc="Valentine's Day fine-dining dinner",         amt=5200, pm="Card"),
  list(d="2025-08-15", cat="Food",         desc="Independence Day celebration dinner",         amt=4900, pm="Card"),
  list(d="2025-12-31", cat="Food",         desc="New Year's Eve gala dinner",                  amt=6800, pm="Card"),
  # Transport
  list(d="2025-03-22", cat="Transport",    desc="Ola outstation cab — family trip",            amt=3900, pm="UPI"),
  list(d="2025-10-05", cat="Transport",    desc="Airport cab — Dussehra travel",               amt=2950, pm="UPI"),
  list(d="2025-12-24", cat="Transport",    desc="Airport cab — Christmas travel",              amt=3300, pm="Card"),
  # Shopping
  list(d="2025-04-10", cat="Shopping",     desc="Sony noise-cancelling headphones",            amt=13500,pm="Card"),
  list(d="2025-09-15", cat="Shopping",     desc="Mixer grinder — Bajaj",                      amt=4400, pm="Card"),
  list(d="2025-11-11", cat="Shopping",     desc="Big Billion Day — laptop accessories bundle", amt=9200, pm="Card"),
  # Healthcare
  list(d="2025-06-18", cat="Healthcare",   desc="Minor surgical procedure — day care",         amt=19500,pm="Card"),
  list(d="2025-10-22", cat="Healthcare",   desc="MRI scan + specialist consultation",          amt=5200, pm="UPI"),
  # Education
  list(d="2025-05-12", cat="Education",    desc="AWS Solutions Architect exam fee",            amt=15000,pm="Card"),
  list(d="2025-11-20", cat="Education",    desc="PMP certification training programme",        amt=22000,pm="Card"),
  # Entertainment
  list(d="2025-03-08", cat="Entertainment",desc="Holi festival music concert — VIP pass",     amt=3800, pm="UPI"),
  list(d="2025-12-20", cat="Entertainment",desc="Christmas music event — 2 premium passes",   amt=4500, pm="Card"),
  # Grocery (bulk stock-up)
  list(d="2025-01-05", cat="Grocery",      desc="New Year bulk grocery stock-up",              amt=7900, pm="Card"),
  list(d="2025-08-01", cat="Grocery",      desc="Onam special grocery haul",                  amt=6400, pm="Card"),
  # Bills (unexpected large bill)
  list(d="2025-07-05", cat="Bills",        desc="Electricity bill — summer AC surge",         amt=5800, pm="UPI")
)

for (a in anomalies) {
  push(as.Date(a$d), a$cat, a$desc, a$amt, a$pm)
}

# ---------------------------------------------------------------------------
# 12. ASSEMBLE DATA FRAME
# ---------------------------------------------------------------------------
df <- do.call(rbind, rows[seq_len(n)])
df$date   <- as.character(df$date)
df$amount <- as.numeric(df$amount)

# Remove rows outside the date window (guard against off-by-one in loops)
df <- df[df$date >= format(DATE_START) & df$date <= format(DATE_END), ]

cat(sprintf("Raw rows generated: %d\n", nrow(df)))

# ---------------------------------------------------------------------------
# 13. TRIM OR PAD TO EXACTLY 1500 ROWS
# ---------------------------------------------------------------------------
# Protect: Rent/PG rows, anomaly rows (identified by high amount threshold),
# and all non-Food/Transport rows (keep variety).
protected_mask <- df$category == "Rent/PG" | df$amount >= 2500

if (nrow(df) > TARGET) {
  trimmable_idx <- which(!protected_mask & df$category %in% c("Food","Transport"))
  n_trim        <- nrow(df) - TARGET
  if (n_trim > length(trimmable_idx)) n_trim <- length(trimmable_idx)
  remove_idx    <- sample(trimmable_idx, n_trim, replace = FALSE)
  df            <- df[-remove_idx, ]
  cat(sprintf("Trimmed %d rows → %d rows\n", n_trim, nrow(df)))
}

if (nrow(df) < TARGET) {
  n_pad    <- TARGET - nrow(df)
  pad_days <- sample(ALL_DAYS, n_pad, replace = TRUE)
  food_pad <- c("Swiggy top-up snack order","Chai at stall",
                "Quick bite","Street food","Evening snacks",
                "Canteen quick lunch","Auto top-up fare")
  trans_pad <- c("Auto ride","Bus ticket","Metro tap",
                 "Rapido ride","Bus pass — single trip")
  pad_rows <- lapply(seq_len(n_pad), function(i) {
    d <- as.Date(pad_days[i])
    if (runif(1) < 0.60) {
      data.frame(date = format(d), category = "Food",
                 description = sample(food_pad, 1),
                 amount      = round(rlnorm(1, log(140), 0.35), 0),
                 payment_method = pm(c(0.50, 0.40, 0.10, 0)),
                 stringsAsFactors = FALSE)
    } else {
      data.frame(date = format(d), category = "Transport",
                 description = sample(trans_pad, 1),
                 amount      = round(rlnorm(1, log(75), 0.30), 0),
                 payment_method = pm(c(0.55, 0.38, 0.07, 0)),
                 stringsAsFactors = FALSE)
    }
  })
  df <- rbind(df, do.call(rbind, pad_rows))
  cat(sprintf("Padded %d rows → %d rows\n", n_pad, nrow(df)))
}

# ---------------------------------------------------------------------------
# 14. FINAL SORT AND ID GENERATION
# ---------------------------------------------------------------------------
df <- df[order(df$date), ]
df$transaction_id <- sprintf("txn_%s_%05d",
                              gsub("-", "", df$date),
                              seq_len(nrow(df)))

# Reorder columns to match spec
df <- df[, c("transaction_id","date","category","description",
             "amount","payment_method")]

# ---------------------------------------------------------------------------
# 15. VALIDATION CHECKS BEFORE WRITING
# ---------------------------------------------------------------------------
stopifnot(nrow(df) == TARGET)
stopifnot(!any(duplicated(df$transaction_id)))
stopifnot(!any(is.na(df$amount)))
stopifnot(all(df$amount > 0))
stopifnot(all(df$category       %in% c("Food","Grocery","Transport","Rent/PG",
                                        "Education","Shopping","Healthcare",
                                        "Entertainment","Bills","Other")))
stopifnot(all(df$payment_method %in% c("UPI","Cash","Card","Bank Transfer")))
stopifnot(all(!is.na(as.Date(df$date))))
stopifnot(all(df$date >= format(DATE_START) & df$date <= format(DATE_END)))

# ---------------------------------------------------------------------------
# 16. WRITE OUTPUT
# ---------------------------------------------------------------------------
# Resolve output path robustly under both Rscript and source()
out_path <- tryCatch({
  # When called via Rscript the script path is in commandArgs()
  args      <- commandArgs(trailingOnly = FALSE)
  file_flag <- grep("^--file=", args, value = TRUE)
  if (length(file_flag) == 1L) {
    script_dir <- dirname(normalizePath(sub("^--file=", "", file_flag)))
    file.path(script_dir, "sample_data.csv")
  } else {
    # Sourced interactively — fall back to project-relative path
    "data/sample_data.csv"
  }
}, error = function(e) "data/sample_data.csv")

write.csv(df, out_path, row.names = FALSE, quote = TRUE)

cat(sprintf("\n=== Dataset written to: %s ===\n", out_path))
cat(sprintf("Rows             : %d\n",  nrow(df)))
cat(sprintf("Date range       : %s to %s\n", min(df$date), max(df$date)))
cat(sprintf("Unique IDs       : %d\n",  length(unique(df$transaction_id))))
cat(sprintf("Amount range     : %.0f to %.0f\n", min(df$amount), max(df$amount)))
cat("\nCategory distribution:\n")
print(sort(table(df$category), decreasing = TRUE))
cat("\nPayment method distribution:\n")
print(sort(table(df$payment_method), decreasing = TRUE))
