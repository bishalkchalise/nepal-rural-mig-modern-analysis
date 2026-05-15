################################################################################
# NEC 2018 outcomes - district cross-section second-stage on v2 SSIV.
#
# Spec (cross-section, no district FE since 1 obs/district):
#   y_d = alpha + beta * (z_{d, 2016} * mig_per_1000_z)
#         + gamma * mig_per_1000_z
#         + delta * z_{d, 2016}
#         + lambda' * X_{d, 0}        (6 dest-region shares)
#         + eps_d
#
# Lag 2: 2018 outcome -> z built from rer_{c, 2016}.
# SE: HC1 robust.  Ladder M1 (bare interaction) -> M4 (+ region shares).
#
# Expected file (push to district-analysis/data/clean/nec/):
#   nec_2018_district.csv  with column 'dname' and outcomes listed below.
#
# Edit OUTCOMES vector to match what's actually in your district-level file.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

NEC_FILE <- "district-analysis/data/clean/nec/nec_2018_district.csv"
NEC_YEAR <- 2018
LAG      <- 2   # shifter uses rer at NEC_YEAR - LAG = 2016

OUTCOMES <- c(
  # Edit these to match the columns in your file.
  # Counts (use log + 1 if heavy-tailed):
  "n_establishments", "log_n_establishments",
  "n_employees", "log_n_employees",
  "employees_per_estab",
  # Sector shares (already proportions):
  "share_manufacturing", "share_trade", "share_services",
  "share_hotels", "share_transport",
  # Firm size shares:
  "share_micro", "share_small", "share_medium", "share_large",
  # Formal economy:
  "share_registered", "share_irs_registered"
)

# ---- check file ----
if (!file.exists(NEC_FILE)) {
  stop(sprintf(
    "NEC file not found: %s\nPush a district-level NEC 2018 file to that path.",
    NEC_FILE
  ))
}

# ---- name normalization ----
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

# ---- shared loads ----
forex    <- read_csv("district-analysis/data/clean/forex_2000_2023.csv", show_col_types = FALSE)
dofe_raw <- read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv", show_col_types = FALSE)
pop_file <- read_csv("district-analysis/data/clean/foreign_migration_district_population.csv", show_col_types = FALSE)
regions  <- read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv", show_col_types = FALSE)
nec      <- read_csv(NEC_FILE, show_col_types = FALSE)

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
fx_countries <- unique(fx$country)

# ---- v2 SSIV ----
dofe <- dofe_raw %>%
  filter(!is.na(country)) %>%
  mutate(dname = to_dname(district_rename)) %>%
  group_by(dname, country, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  filter(!country %in% c("Nepal", "India"))

v2_tot <- dofe %>% filter(year %in% c(2009, 2010)) %>%
  group_by(country) %>% summarise(tot = sum(permits), .groups = "drop")
set_v2 <- sort(intersect(v2_tot$country[v2_tot$tot > 0], fx_countries))

sh_v2 <- dofe %>%
  filter(year %in% c(2009, 2010), country %in% set_v2) %>%
  group_by(dname, country) %>%
  summarise(permits = sum(permits), .groups = "drop") %>%
  group_by(dname) %>%
  mutate(share = permits / sum(permits)) %>%
  ungroup() %>%
  select(dname, country, share)

# z at NEC_YEAR - LAG (= 2016 for default)
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

# ---- cross-section ----
# Normalize dname in NEC file
if (!"dname" %in% names(nec)) {
  if      ("district_name" %in% names(nec)) nec$dname <- to_dname(nec$district_name)
  else if ("district77"    %in% names(nec)) nec$dname <- to_dname(nec$district77)
  else if ("DIST"          %in% names(nec)) nec$dname <- as.character(nec$DIST)  # adapt if needed
  else stop("No district column found in NEC file. Need 'dname' or 'district_name'.")
}

cs <- nec %>%
  inner_join(z_lagged, on = "dname", by = "dname") %>%
  inner_join(mi,       by = "dname") %>%
  left_join(regions,   by = "dname") %>%
  mutate(z_lagged_std = (z_lagged - mean(z_lagged, na.rm = TRUE)) /
                       sd(z_lagged, na.rm = TRUE))

# ---- regressions ----
REGION_COLS <- c("share_e_asia", "share_gulf", "share_oecd_north",
                 "share_s_asia", "share_se_asia", "share_oecd_europe")

stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))
}

run_ladder_cs <- function(panel, ycol) {
  panel$z_inter <- panel$z_lagged_std * panel$mig_per_1000_z
  panel$z_bare  <- panel$z_lagged_std

  region_terms <- paste(REGION_COLS, collapse = " + ")
  f_M1 <- as.formula(sprintf("%s ~ z_inter", ycol))
  f_M2 <- as.formula(sprintf("%s ~ z_inter + mig_per_1000_z", ycol))
  f_M3 <- as.formula(sprintf("%s ~ z_inter + mig_per_1000_z + z_bare", ycol))
  f_M4 <- as.formula(sprintf("%s ~ z_inter + mig_per_1000_z + z_bare + %s",
                             ycol, region_terms))

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
    mean_y <- mean(panel[[ycol]], na.rm = TRUE)
    rows[[length(rows)+1]] <- tibble(
      outcome = ycol, model = mlabel,
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

cat(sprintf("\n==== NEC %d cross-section (n=%d districts, lag %d => z at %d) ====\n\n",
            NEC_YEAR, nrow(cs), LAG, NEC_YEAR - LAG))
cat(sprintf("%-30s | %-5s | %-11s | %-7s | %-10s | %-7s | %-22s | %s\n",
            "outcome","model","beta","p","mean(Y)","b/Y%","95% CI","n"))
cat(strrep("-", 120), "\n", sep = "")

all_rows <- list()
for (yc in OUTCOMES) {
  if (!yc %in% names(cs)) {
    cat(sprintf("  skip %s (not in file)\n", yc)); next
  }
  res <- run_ladder_cs(cs, yc)
  if (nrow(res) == 0) next
  all_rows[[length(all_rows)+1]] <- res
  for (i in seq_len(nrow(res))) {
    r <- res[i, ]
    cat(sprintf("%-30s | %-5s | %.4f%-3s | %.4f | %10.4f | %6.2f%% | [%9.4f, %9.4f] | %d\n",
                if (i == 1) r$outcome else "",
                r$model, r$beta, r$sig, r$p, r$mean_y, r$pct_of_mean,
                r$ci_lo, r$ci_hi, r$n))
  }
}

out <- bind_rows(all_rows)
dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(out, "district-analysis/output/tab/nec_outcomes.csv")
cat(sprintf("\nSaved: district-analysis/output/tab/nec_outcomes.csv (%d rows)\n",
            nrow(out)))
