################################################################################
# RVS non-migration outcomes: agriculture, enterprise, health, shocks, social
# protection.  Same v2 SSIV spec as _dist_panel_ladder.R, HH x year panel,
# HH FE + year FE, cluster ~dname, lag 2, ladder M1-M4.
#
# Expected data files (push these to district-analysis/data/clean/rvs/):
#   - agriculture_hh_year.csv
#   - enterprise_hh_year.csv
#   - health_hh_year.csv
#   - shocks_coping_shocked_hh_year.csv
#   - social_protection_hh_year.csv
#
# Each file must have columns: hhid, year, district_name (or district77).
# Output: district-analysis/output/tab/rvs_other_outcomes.csv
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

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

# ---- shared loads (same as first-stage) ----
forex    <- read_csv("district-analysis/data/clean/forex_2000_2023.csv", show_col_types = FALSE)
dofe_raw <- read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv", show_col_types = FALSE)
pop_file <- read_csv("district-analysis/data/clean/foreign_migration_district_population.csv", show_col_types = FALSE)
regions  <- read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv", show_col_types = FALSE)

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

z_v2 <- sh_v2 %>%
  inner_join(fx, by = "country", relationship = "many-to-many") %>%
  mutate(x = share * rer) %>%
  group_by(dname, year) %>%
  summarise(z = sum(x, na.rm = TRUE), .groups = "drop") %>%
  rename(z_v2 = z)

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
         log_mi       = log(pmax(mig_per_1000, 1e-6)),
         log_mi_z     = (log_mi - mean(log_mi)) / sd(log_mi)) %>%
  select(dname, log_mi_z)

# ---- helper to load + standardize z (lag 2) into HH panel ----
prep_panel <- function(df) {
  # district key
  dist_col <- if ("district_name" %in% names(df)) "district_name"
              else if ("district77" %in% names(df)) "district77"
              else stop("No district column in input file")
  df$dname <- to_dname(df[[dist_col]])
  out <- df %>%
    inner_join(mi, by = "dname") %>%
    left_join(regions, by = "dname")
  # attach z at lag 2 (z_{d, year-2})
  z_lag <- z_v2 %>% mutate(year = year + 2) %>%
    select(dname, year, z_v2_L2 = z_v2)
  out <- out %>% left_join(z_lag, by = c("dname", "year"))
  m_z <- mean(out$z_v2_L2, na.rm = TRUE)
  s_z <- sd(out$z_v2_L2,   na.rm = TRUE)
  out$z_v2_L2_std <- (out$z_v2_L2 - m_z) / s_z
  out
}

# ---- regression engine: presents beta, p, mean(Y), b/Y%, 95% CI, n ----
REGION_COLS <- c("share_e_asia", "share_gulf", "share_oecd_north",
                 "share_s_asia", "share_se_asia", "share_oecd_europe")

stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))
}

run_ladder <- function(panel, ycol, group, label) {
  refyr <- min(panel$year, na.rm = TRUE)
  region_terms <- paste(sprintf("i(year, %s, ref = %d)", REGION_COLS, refyr),
                        collapse = " + ")

  panel$z_inter <- panel$z_v2_L2_std * panel$log_mi_z
  panel$z_bare  <- panel$z_v2_L2_std

  f_M1 <- as.formula(sprintf("%s ~ z_inter | hhid + year", ycol))
  f_M2 <- as.formula(sprintf("%s ~ z_inter + i(year, log_mi_z, ref = %d) | hhid + year",
                             ycol, refyr))
  f_M3 <- as.formula(sprintf("%s ~ z_inter + i(year, log_mi_z, ref = %d) + z_bare | hhid + year",
                             ycol, refyr))
  f_M4 <- as.formula(sprintf("%s ~ z_inter + i(year, log_mi_z, ref = %d) + z_bare + %s | hhid + year",
                             ycol, refyr, region_terms))

  rows <- list()
  for (mlabel in c("M1","M2","M3","M4")) {
    f <- switch(mlabel, M1=f_M1, M2=f_M2, M3=f_M3, M4=f_M4)
    fit <- tryCatch(feols(f, data = panel, cluster = ~dname),
                    error = function(e) NULL)
    if (is.null(fit)) next
    s <- summary(fit)$coeftable
    if (!"z_inter" %in% rownames(s)) next
    b  <- s["z_inter","Estimate"]; se <- s["z_inter","Std. Error"]
    pv <- s["z_inter","Pr(>|t|)"]
    mean_y <- mean(predict(fit) + residuals(fit), na.rm = TRUE)
    rows[[length(rows)+1]] <- tibble(
      group = group, outcome = label, model = mlabel,
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

# ---- outcome groups ----
GROUPS <- list(
  agriculture = list(
    file = "district-analysis/data/clean/rvs/agriculture_hh_year.csv",
    vars = c("agro_hh", "cultivated_area_sqm", "crop_sale_rs_12m",
             "total_input_cost_rs", "livestock_has",
             "owns_storage_struct", "owns_irrigation_kit",
             "n_crops_total", "crop_simpson_diversity",
             "any_crop_sold")
  ),
  enterprise = list(
    file = "district-analysis/data/clean/rvs/enterprise_hh_year.csv",
    vars = c("has_enterprise", "n_enterprises", "n_workers_total",
             "revenue_12m", "profit_12m", "capex_12m",
             "sector_manufacturing", "sector_trade", "sector_services")
  ),
  health = list(
    file = "district-analysis/data/clean/rvs/health_hh_year.csv",
    vars = c("any_insured", "n_chronic", "n_acute_illness",
             "any_health_spending", "hlt_spend_total",
             "hlt_spend_hospital", "hlt_spend_medicines")
  ),
  shocks = list(
    file = "district-analysis/data/clean/rvs/shocks_coping_shocked_hh_year.csv",
    vars = c("any_shock", "n_shocks", "total_loss_rs",
             "health_shock_any", "natural_disaster_shock_any",
             "agricultural_shock_any", "economic_shock_any",
             "cope_savings_any", "cope_borrow_any", "cope_sell_assets_any",
             "cope_migration_remittance_any", "severe_loss_any")
  ),
  social_protection = list(
    file = "district-analysis/data/clean/rvs/social_protection_hh_year.csv",
    vars = c("public_support_any", "public_support_amt",
             "private_support_any", "private_support_amt",
             "remittance_any", "remittance_amt", "total_support_amt",
             "public_cash_any", "demographic_cash_any",
             "education_cash_any", "ngo_support_any")
  )
)

all_rows <- list()
for (grp in names(GROUPS)) {
  meta <- GROUPS[[grp]]
  if (!file.exists(meta$file)) {
    cat(sprintf("** SKIP %s: %s not found.  Push it to run this group.\n",
                grp, meta$file))
    next
  }
  cat(sprintf("\n==== %s outcomes (HH x year, hhid+year FE, cluster ~dname, lag 2) ====\n",
              toupper(grp)))
  df <- read_csv(meta$file, show_col_types = FALSE)
  panel <- prep_panel(df)
  for (yc in meta$vars) {
    if (!yc %in% names(panel)) {
      cat(sprintf("  skip %s (not in file)\n", yc)); next
    }
    res <- run_ladder(panel, yc, grp, yc)
    if (nrow(res) == 0) next
    all_rows[[length(all_rows) + 1]] <- res
    # Print compact view per outcome
    cat(sprintf("\n%-32s\n", yc))
    cat(sprintf("  %-5s | %-11s | %-7s | %-10s | %-7s | %-20s | %s\n",
                "model","beta","p","mean(Y)","b/Y%","95% CI","n"))
    for (i in seq_len(nrow(res))) {
      r <- res[i, ]
      cat(sprintf("  %-5s | %.4f%-3s | %.4f | %10.4f | %6.2f%% | [%8.4f, %8.4f]| %d\n",
                  r$model, r$beta, r$sig, r$p, r$mean_y, r$pct_of_mean,
                  r$ci_lo, r$ci_hi, r$n))
    }
  }
}

out <- bind_rows(all_rows)
dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(out, "district-analysis/output/tab/rvs_other_outcomes.csv")
cat(sprintf("\nSaved: district-analysis/output/tab/rvs_other_outcomes.csv (%d rows)\n",
            nrow(out)))
