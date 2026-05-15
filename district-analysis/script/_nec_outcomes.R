################################################################################
# NEC 2018 firm census - DISTRICT cross-section reduced-form SSIV.
# Ported from script/archive/municipality/baseline_nec_cs.R; adapted for the
# 77-district aggregation produced by 05_district_aggregate.R.
#
# Spec (cross-section, 75 districts, no district FE since 1 obs/district):
#   y_d = alpha + beta * (z_{d, 2018-LAG}_std * mig_per_1000_z)
#         + lambda' * controls_d
#         + eps_d                             cov = HC1 robust
#
# Ladder M1 - M4 same as Khanna spec:
#   M1: bare interaction
#   M2: + mig_per_1000_z
#   M3: + bare z_lagged_std (C_fx)
#   M4: + 6 dest-region shares (C_X)
#
# Expected input (pushed by user from script/archive/municipality/vars/nec2018/
# 05_district_aggregate.R output -> data/clean/nec2018/district_analysis.csv):
#   district-analysis/data/clean/nec/nec_2018_district.csv
#   must contain either column 'dname' or a 'DIST' (numeric) code we can map.
#
# Output: district-analysis/output/tab/nec_outcomes.csv
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

NEC_FILE <- "district-analysis/data/clean/nec/nec_2018_district.csv"
NEC_YEAR <- 2018
LAG      <- 2L   # 2018 - LAG = 2016 (matches our headline lag-2 spec)

# ---- name normalization (matches first-stage scripts) ----
dofe_to_census <- c(
  "CHITWAN" = "Chitawan", "DHANUSHA" = "Dhanusa", "KAPILVASTU" = "Kapilbastu",
  "MAKAWANPUR" = "Makwanpur", "TANAHUN" = "Tanahu", "TEHRATHUM" = "Terhathum",
  "KABHREPALANCHOK" = "Kavrepalanchok"
)
to_dname <- function(s) {
  u <- toupper(trimws(as.character(s)))
  ifelse(u %in% names(dofe_to_census),
         dofe_to_census[u],
         tools::toTitleCase(tolower(as.character(s))))
}

# DIST numeric code -> dname (from NEC codebook).  Used if input has DIST not dname.
DIST_LOOKUP <- c(
  "101"="Taplejung","102"="Sankhuwasabha","103"="Solukhumbu","104"="Okhaldhunga",
  "105"="Khotang","106"="Bhojpur","107"="Dhankuta","108"="Terhathum",
  "109"="Panchthar","110"="Ilam","111"="Jhapa","112"="Morang","113"="Sunsari",
  "114"="Udayapur",
  "201"="Saptari","202"="Siraha","203"="Dhanusa","204"="Mahottari","205"="Sarlahi",
  "206"="Rautahat","207"="Bara","208"="Parsa",
  "301"="Dolakha","302"="Sindhupalchok","303"="Rasuwa","304"="Dhading",
  "305"="Nuwakot","306"="Kathmandu","307"="Bhaktapur","308"="Lalitpur",
  "309"="Kavrepalanchok","310"="Ramechhap","311"="Sindhuli","312"="Makwanpur",
  "313"="Chitawan",
  "401"="Gorkha","402"="Manang","403"="Mustang","404"="Myagdi","405"="Kaski",
  "406"="Lamjung","407"="Tanahu","408"="Nawalparasi_E","409"="Syangja",
  "410"="Parbat","411"="Baglung",
  "501"="Rukum_E","502"="Rolpa","503"="Pyuthan","504"="Gulmi","505"="Arghakhanchi",
  "506"="Palpa","507"="Nawalparasi_W","508"="Rupandehi","509"="Kapilbastu",
  "510"="Dang","511"="Banke","512"="Bardiya",
  "601"="Dolpa","602"="Mugu","603"="Humla","604"="Jumla","605"="Kalikot",
  "606"="Dailekh","607"="Jajarkot","608"="Rukum_W","609"="Salyan","610"="Surkhet",
  "701"="Bajura","702"="Bajhang","703"="Darchula","704"="Baitadi","705"="Dadeldhura",
  "706"="Doti","707"="Achham","708"="Kailali","709"="Kanchanpur"
)

# ---- check file ----
if (!file.exists(NEC_FILE)) {
  stop(sprintf(
    "NEC district file not found: %s\nPush the output of script/archive/municipality/vars/nec2018/05_district_aggregate.R to that path.",
    NEC_FILE
  ))
}

# ---- shared loads (same as first-stage) ----
forex    <- read_csv("district-analysis/data/clean/forex_2000_2023.csv", show_col_types = FALSE)
dofe_raw <- read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv", show_col_types = FALSE)
pop_file <- read_csv("district-analysis/data/clean/foreign_migration_district_population.csv", show_col_types = FALSE)
regions  <- read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv", show_col_types = FALSE)
nec      <- read_csv(NEC_FILE, show_col_types = FALSE)

# Normalize dname in NEC file
if ("dname" %in% names(nec)) {
  # already there
} else if ("DIST" %in% names(nec)) {
  nec$dname <- DIST_LOOKUP[as.character(nec$DIST)]
  if (any(is.na(nec$dname))) {
    cat("WARNING: unmapped DIST codes:\n")
    print(unique(nec$DIST[is.na(nec$dname)]))
  }
} else if ("district_name" %in% names(nec)) {
  nec$dname <- to_dname(nec$district_name)
} else {
  stop("Could not find dname / DIST / district_name in NEC file.")
}

# ---- FX panel ----
nepal_fx <- forex %>% filter(country == "Nepal") %>%
  transmute(year, npr_per_usd = forex)
fx <- forex %>%
  filter(!country %in% c("Nepal", "India"), !is.na(forex)) %>%
  transmute(country, year, lcu_per_usd = forex) %>%
  inner_join(nepal_fx, by = "year") %>%
  mutate(log_npr_per_lcu = log(npr_per_usd / lcu_per_usd)) %>%
  filter(!is.na(log_npr_per_lcu)) %>%
  group_by(country) %>%
  mutate(base_2010 = log_npr_per_lcu[year == 2010][1]) %>%
  ungroup() %>%
  filter(!is.na(base_2010)) %>%
  mutate(rer = log_npr_per_lcu - base_2010) %>%
  select(country, year, rer)

# ---- v2 SSIV ----
dofe <- dofe_raw %>%
  filter(!is.na(country)) %>%
  mutate(dname = to_dname(district_rename)) %>%
  group_by(dname, country, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  filter(!country %in% c("Nepal", "India"))

v2_tot <- dofe %>% filter(year %in% c(2009, 2010)) %>%
  group_by(country) %>% summarise(tot = sum(permits), .groups = "drop")
set_v2 <- sort(intersect(v2_tot$country[v2_tot$tot > 0], unique(fx$country)))

sh_v2 <- dofe %>%
  filter(year %in% c(2009, 2010), country %in% set_v2) %>%
  group_by(dname, country) %>%
  summarise(permits = sum(permits), .groups = "drop") %>%
  group_by(dname) %>%
  mutate(share = permits / sum(permits)) %>%
  ungroup() %>%
  select(dname, country, share)

z_lagged <- sh_v2 %>%
  inner_join(fx %>% filter(year == NEC_YEAR - LAG),
             by = "country", relationship = "many-to-many") %>%
  mutate(x = share * rer) %>%
  group_by(dname) %>%
  summarise(z_lagged = sum(x, na.rm = TRUE), .groups = "drop")

# ---- mig intensity ----
mi <- dofe %>%
  filter(year %in% c(2009, 2010)) %>%
  group_by(dname) %>%
  summarise(num = mean(permits), .groups = "drop") %>%
  left_join(
    pop_file %>%
      mutate(dname = to_dname(district)) %>%
      select(dname, pop_2011 = district_population_2011) %>%
      distinct(dname, .keep_all = TRUE),
    by = "dname"
  ) %>%
  mutate(mig_int      = num / pop_2011,
         mig_per_1000 = mig_int * 1000,
         mig_per_1000_z = (mig_per_1000 - mean(mig_per_1000)) / sd(mig_per_1000)) %>%
  select(dname, mig_per_1000_z)

# ---- cross-section assembly ----
cs <- nec %>%
  inner_join(z_lagged, by = "dname") %>%
  inner_join(mi,       by = "dname") %>%
  left_join(regions,   by = "dname") %>%
  mutate(z_lagged_std = (z_lagged - mean(z_lagged, na.rm = TRUE)) /
                       sd(z_lagged, na.rm = TRUE))

cat(sprintf("NEC %d cross-section: %d districts, z at year %d (lag %d)\n",
            NEC_YEAR, nrow(cs), NEC_YEAR - LAG, LAG))

# ---- regressions ----
REGION_COLS <- c("share_e_asia", "share_gulf", "share_oecd_north",
                 "share_s_asia", "share_se_asia", "share_oecd_europe")

stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))
}

run_ladder_cs <- function(panel, ycol, is_log) {
  y <- panel[[ycol]]
  if (is_log) y <- log1p(pmax(y, 0))   # asinh-like for safety
  panel$.y <- y

  panel$z_inter <- panel$z_lagged_std * panel$mig_per_1000_z
  panel$z_bare  <- panel$z_lagged_std

  region_terms <- paste(REGION_COLS, collapse = " + ")
  f_M1 <- as.formula(".y ~ z_inter")
  f_M2 <- as.formula(".y ~ z_inter + mig_per_1000_z")
  f_M3 <- as.formula(".y ~ z_inter + mig_per_1000_z + z_bare")
  f_M4 <- as.formula(sprintf(".y ~ z_inter + mig_per_1000_z + z_bare + %s", region_terms))

  rows <- list()
  for (mlabel in c("M1","M2","M3","M4")) {
    f <- switch(mlabel, M1=f_M1, M2=f_M2, M3=f_M3, M4=f_M4)
    fit <- tryCatch(feols(f, data = panel, vcov = "hetero"),
                    error = function(e) NULL)
    if (is.null(fit)) next
    s <- summary(fit)$coeftable
    if (!"z_inter" %in% rownames(s)) next
    b  <- s["z_inter","Estimate"]; se <- s["z_inter","Std. Error"]
    pv <- s["z_inter","Pr(>|t|)"]
    mean_y <- mean(panel$.y, na.rm = TRUE)
    rows[[length(rows)+1]] <- tibble(
      outcome = ycol, log = is_log, model = mlabel,
      beta = round(b, 4), se = round(se, 4),
      t = round(s["z_inter","t value"], 2), p = round(pv, 4),
      sig = stars(pv),
      mean_y = round(mean_y, 4),
      pct_of_mean = round(100 * b / mean_y, 2),
      ci_lo = round(b - 1.96 * se, 4),
      ci_hi = round(b + 1.96 * se, 4),
      n = nobs(fit)
    )
  }
  bind_rows(rows)
}

# ---- outcome groups (mirrors baseline_nec_cs.R; "log" col = TRUE -> log1p) ----
GROUPS <- list(
  "FIRM PRESENCE & SCALE" = list(
    c("n_firms",           "TRUE"),
    c("emp_total",         "TRUE"),
    c("mean_emp_per_firm", "FALSE"),
    c("p90_emp_per_firm",  "FALSE")
  ),
  "FIRM SIZE COMPOSITION" = list(
    c("share_firms_size_micro_1",     "FALSE"),
    c("share_firms_size_small_2_9",   "FALSE"),
    c("share_firms_size_medium_10_50","FALSE"),
    c("share_firms_size_large_51p",   "FALSE")
  ),
  "FORMALITY" = list(
    c("share_registered",      "FALSE"),
    c("share_tax_registered",  "FALSE"),
    c("share_keeps_accounts",  "FALSE"),
    c("share_incorporated",    "FALSE"),
    c("formality_index",       "FALSE")
  ),
  "SECTOR COMPOSITION" = list(
    c("share_firms_sec_sec_manuf",       "FALSE"),
    c("share_firms_sec_sec_construct",   "FALSE"),
    c("share_firms_sec_sec_wholesale",   "FALSE"),
    c("share_firms_sec_sec_hospitality", "FALSE"),
    c("share_firms_sec_sec_transport",   "FALSE"),
    c("share_firms_sec_sec_services",    "FALSE"),
    c("share_firms_sec_sec_health",      "FALSE"),
    c("share_firms_sec_sec_education",   "FALSE")
  ),
  "TRADABILITY" = list(
    c("share_trd_tradable_goods",        "FALSE"),
    c("share_trd_tradable_services",     "FALSE"),
    c("share_trd_non_tradable_services", "FALSE")
  ),
  "MODERNITY" = list(
    c("share_modern_modern_services",      "FALSE"),
    c("share_modern_modern_manuf",         "FALSE"),
    c("share_modern_traditional_commerce", "FALSE"),
    c("share_modern_traditional_services", "FALSE")
  ),
  "PRODUCTIVITY & CAPITAL" = list(
    c("rev_mean",                 "TRUE"),
    c("labor_prod_median",        "TRUE"),
    c("value_added_pw_median",    "TRUE"),
    c("capital_intensity_median", "TRUE"),
    c("profit_margin_median",     "FALSE")
  ),
  "CREDIT & FINANCE" = list(
    c("share_borrowed_any",  "FALSE"),
    c("share_formal_credit", "FALSE"),
    c("interest_p50",        "FALSE")
  ),
  "GENDER" = list(
    c("share_female_manager", "FALSE"),
    c("share_female_owner",   "FALSE"),
    c("share_female_led",     "FALSE"),
    c("share_female_workers", "FALSE"),
    c("share_emp_female",     "FALSE")
  ),
  "FIRM AGE" = list(
    c("share_firms_young_5y",   "FALSE"),
    c("share_firms_mature_10y", "FALSE"),
    c("median_firm_age",        "FALSE")
  )
)

all_rows <- list()
for (grp_name in names(GROUPS)) {
  cat(sprintf("\n==== %s ====\n", grp_name))
  cat(sprintf("%-32s | %-5s | %-11s | %-7s | %-10s | %-7s | %-22s | %s\n",
              "outcome","model","beta","p","mean(Y)","b/Y%","95% CI","n"))
  cat(strrep("-", 120), "\n", sep = "")
  for (item in GROUPS[[grp_name]]) {
    yc <- item[1]; is_log <- as.logical(item[2])
    if (!yc %in% names(cs)) {
      cat(sprintf("  skip %s (not in file)\n", yc)); next
    }
    res <- run_ladder_cs(cs, yc, is_log)
    if (nrow(res) == 0) next
    res$group <- grp_name
    all_rows[[length(all_rows)+1]] <- res
    for (i in seq_len(nrow(res))) {
      r <- res[i, ]
      tag <- if (i == 1) sprintf("%s%s", yc, if (is_log) " [log]" else "") else ""
      cat(sprintf("%-32s | %-5s | %.4f%-3s | %.4f | %10.4f | %6.2f%% | [%9.4f, %9.4f] | %d\n",
                  tag, r$model, r$beta, r$sig, r$p, r$mean_y, r$pct_of_mean,
                  r$ci_lo, r$ci_hi, r$n))
    }
  }
}

out <- bind_rows(all_rows)
out <- out %>% select(group, everything())
dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(out, "district-analysis/output/tab/nec_outcomes.csv")
cat(sprintf("\nSaved: district-analysis/output/tab/nec_outcomes.csv (%d rows)\n",
            nrow(out)))
