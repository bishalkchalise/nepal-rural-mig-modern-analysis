################################################################################
#
# KHANNA-DIRECTION (LCU/NPR) STAGGERED FIRST-STAGE
# ------------------------------------------------------------------------------
# Same six-rung ladder as first_stage_staggered.R, but fxshock is built in
# Khanna's published convention:
#
#   fx_to_npr_LCU(c, t) = LCU_per_USD(c, t) / NPR_per_USD(t)        # Khanna
#   fx_index(c, t)      = fx_to_npr_LCU(c, t) / fx_to_npr_LCU(c, baseline_year)
#   fxshock_d,t         = sum_c  share_dc(baseline_year)  *  fx_index(c, t)
#
# fxshock now FALLS as NPR depreciates. The expected sign of beta on
# fx_z * log_mi_z under controls is NEGATIVE (Khanna's published direction:
# "more devaluation -> more migration" appears as a negative coefficient).
#
# This script builds:
#   - fxshock_LCU_DOFE_dofebase  : DOFE 2009-10 shares + 2009-10 baseline
#   - fxshock_LCU_DOFE_2001base  : DOFE 2009-10 shares + 2001 baseline
#
# 2001-share-baseline-vintage requires re-running instrument.R with the LCU/NPR
# direction flag (a one-line edit; see comment at the bottom).
#
# Run from repo root:
#     source("district-analysis/script/estimate/first_stage_staggered_khanna.R")
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

# ----------------------------------------------------------------------------
# 1. Build LCU/NPR fxshock from raw forex + DOFE shares
# ----------------------------------------------------------------------------

forex     <- read.csv("district-analysis/data/clean/forex_2000_2023.csv",
                      stringsAsFactors = FALSE)
dofe      <- read.csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv",
                      stringsAsFactors = FALSE)
pop_file  <- read.csv("district-analysis/data/clean/foreign_migration_district_population.csv",
                      stringsAsFactors = FALSE)
region_sh <- read.csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv",
                      stringsAsFactors = FALSE)

dofe_to_census <- c("CHITWAN"="Chitawan","DHANUSHA"="Dhanusa","KAPILVASTU"="Kapilbastu",
                    "MAKAWANPUR"="Makwanpur","TANAHUN"="Tanahu","TEHRATHUM"="Terhathum",
                    "KABHREPALANCHOK"="Kavrepalanchok")

to_dname <- function(x) {
  u <- toupper(str_squish(x))
  ifelse(!is.na(dofe_to_census[u]),
         dofe_to_census[u],
         str_to_title(tolower(u)))
}

# LCU per NPR per (country, year)
nepal_fx <- forex %>%
  filter(country == "Nepal") %>%
  transmute(year, npr_per_usd = as.numeric(forex))

fx_lcu <- forex %>%
  rename(lcu_per_usd = forex) %>%
  inner_join(nepal_fx, by = "year") %>%
  filter(country != "Nepal", country != "India") %>%
  mutate(fx_lcu_per_npr = as.numeric(lcu_per_usd) / npr_per_usd) %>%
  select(country, year, fx_lcu_per_npr)

fx_index_baseline <- function(fx, baseline_years) {
  base <- fx %>%
    filter(year %in% baseline_years) %>%
    group_by(country) %>%
    summarise(base = mean(fx_lcu_per_npr, na.rm = TRUE), .groups = "drop")
  fx %>%
    inner_join(base, by = "country") %>%
    mutate(fx_index = fx_lcu_per_npr / base) %>%
    select(country, year, fx_index)
}

fx_idx_2001 <- fx_index_baseline(fx_lcu, 2001)
fx_idx_dofe <- fx_index_baseline(fx_lcu, c(2009, 2010))

# DOFE district-country panel
dofe_dc <- dofe %>%
  mutate(dname = to_dname(district_rename)) %>%
  filter(country != "India", country != "Nepal")

# DOFE 2009-10 share per (district, country)
shares_dofe <- dofe_dc %>%
  filter(year %in% c(2009, 2010)) %>%
  group_by(dname, country) %>%
  summarise(mig = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  group_by(dname) %>%
  mutate(share = mig / sum(mig, na.rm = TRUE)) %>%
  ungroup() %>%
  select(dname, country, share)

build_fxshock <- function(shares, fx_idx) {
  shares %>%
    inner_join(fx_idx, by = "country") %>%
    group_by(dname, year) %>%
    summarise(fxshock_LCU = sum(share * fx_index, na.rm = TRUE),
              .groups = "drop")
}

fxshock_dofe_dofebase <- build_fxshock(shares_dofe, fx_idx_dofe)
fxshock_dofe_2001base <- build_fxshock(shares_dofe, fx_idx_2001)

cat(sprintf("Khanna-direction fxshock built: dofe-base %d rows, 2001-base %d rows\n",
            nrow(fxshock_dofe_dofebase), nrow(fxshock_dofe_2001base)))

# ----------------------------------------------------------------------------
# 2. mig_int (DOFE 2009-10 / pop_2011)
# ----------------------------------------------------------------------------

dofe_dy <- dofe %>%
  group_by(district_rename, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  mutate(dname = to_dname(district_rename)) %>%
  select(dname, year, permits)

mi_dofe <- dofe_dy %>%
  filter(year %in% 2009:2010) %>%
  group_by(dname) %>%
  summarise(num = mean(permits), .groups = "drop")

pop_2011 <- pop_file %>%
  mutate(dname = to_dname(district)) %>%
  distinct(dname, district_population_2011) %>%
  rename(pop_2011 = district_population_2011)

mi_dofe <- mi_dofe %>%
  left_join(pop_2011, by = "dname") %>%
  mutate(mig_int_dofe = num / pop_2011) %>%
  select(dname, mig_int_dofe)

# ----------------------------------------------------------------------------
# 3. Load RVS panels (if present locally)
# ----------------------------------------------------------------------------

try_load <- function(p) if (file.exists(p)) read.csv(p, stringsAsFactors = FALSE) else NULL

rvs_dist <- try_load("district-analysis/data/clean/rvs/migration_district_year.csv")
rvs_hh   <- try_load("district-analysis/data/clean/rvs/migration_hh_year.csv")
rvs_mig  <- try_load("district-analysis/data/clean/rvs/migration_migrant_year.csv")

if (!is.null(rvs_dist)) {
  rvs_dist <- rvs_dist %>%
    mutate(dname = to_dname(dname_raw),
           log_n_intl = log(n_intl_migrants + 1),
           log_remit  = log(remit_amount_intl_12m_rs + 1),
           share_hh   = n_hh_with_intl_migrant / pmax(n_hh, 1))
}
add_dname_rvs <- function(df) {
  if (is.null(df)) return(NULL)
  k <- intersect(c("district_name","district77","district"), names(df))[1]
  if (is.na(k)) return(df)
  df %>% mutate(dname = to_dname(.data[[k]]))
}
rvs_hh  <- add_dname_rvs(rvs_hh)
rvs_mig <- add_dname_rvs(rvs_mig)

# ----------------------------------------------------------------------------
# 4. Helpers
# ----------------------------------------------------------------------------

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

REGION_COLS <- c("share_e_asia","share_gulf","share_oecd_north",
                 "share_oecd_europe","share_s_asia","share_se_asia")

fit_stage <- function(panel, outcome, level, ref_year, entity_col = "dname") {
  d <- panel
  d <- d[!is.na(d[[outcome]]) & !is.na(d$fxshock_LCU) & !is.na(d$mig_int_dofe), ]
  if (nrow(d) < 30) return(NULL)

  d$fx_z      <- zscore(d$fxshock_LCU)
  d$log_mi_z  <- zscore(log(pmax(d$mig_int_dofe, 1e-12)))
  d$treatment <- d$fx_z * d$log_mi_z

  rhs <- "treatment"
  if (level >= 2) rhs <- c(rhs, "fx_z")
  if (level >= 4) rhs <- c(rhs, "i(year, log_mi_z)")
  if (level >= 5) rhs <- c(rhs, "i(year, fx_z)")
  if (level >= 6) for (c in REGION_COLS) if (c %in% names(d))
                    rhs <- c(rhs, sprintf("i(year, %s)", c))

  fe <- if (level >= 3) sprintf("%s + year", entity_col) else entity_col
  fml <- as.formula(sprintf("%s ~ %s | %s", outcome, paste(rhs, collapse = " + "), fe))
  m <- tryCatch(feols(fml, data = d, cluster = as.formula(paste0("~", entity_col)),
                      warn = FALSE, notes = FALSE),
                error = function(e) e)
  if (inherits(m, "error")) return(list(err = substr(conditionMessage(m), 1, 100)))
  ct <- as.data.frame(summary(m)$coeftable)
  if (!"treatment" %in% rownames(ct)) return(list(err = "treatment absorbed"))
  list(beta = ct["treatment","Estimate"],
       se   = ct["treatment","Std. Error"],
       t    = ct["treatment","t value"],
       p    = ct["treatment","Pr(>|t|)"],
       n    = nobs(m),
       r2_w = fitstat(m, "wr2", simplify = TRUE))
}

stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
}

SPEC_LABELS <- c("1" = "M1 treat | dname FE",
                 "2" = "M2 +fx_z main",
                 "3" = "M3 +year FE",
                 "4" = "M4 +C_mig",
                 "5" = "M5 +C_fx",
                 "6" = "M6 +C_X")

# ----------------------------------------------------------------------------
# 5. Build outcome-specific panels and loop
# ----------------------------------------------------------------------------

mk_panel <- function(left, fxshock_panel) {
  left %>%
    inner_join(fxshock_panel, by = c("dname","year")) %>%
    left_join(mi_dofe,    by = "dname") %>%
    left_join(region_sh,  by = "dname")
}

run_for_baseline <- function(fxshock_panel, baseline_label) {
  outcomes_list <- list(
    list(name = "DOFE_permits",  outcome = "log_permits",
         panel = mk_panel(dofe_dy %>% mutate(log_permits = log(permits + 1)),
                          fxshock_panel),
         entity = "dname")
  )
  if (!is.null(rvs_dist))
    outcomes_list <- c(outcomes_list, list(
      list(name = "RVS_d_n_intl", outcome = "log_n_intl",
           panel = mk_panel(rvs_dist, fxshock_panel), entity = "dname"),
      list(name = "RVS_d_share",  outcome = "share_hh",
           panel = mk_panel(rvs_dist, fxshock_panel), entity = "dname"),
      list(name = "RVS_d_remit",  outcome = "log_remit",
           panel = mk_panel(rvs_dist, fxshock_panel), entity = "dname")))
  if (!is.null(rvs_hh) && "has_migrant_intl" %in% names(rvs_hh)) {
    p_hh <- mk_panel(rvs_hh %>% filter(has_migrant_intl == 1), fxshock_panel) %>%
            mutate(log_n_intl = log(n_intl_migrants + 1),
                   log_remit  = log(remit_amount_intl_12m_rs + 1))
    outcomes_list <- c(outcomes_list, list(
      list(name = "RVS_hh_n_intl", outcome = "log_n_intl",
           panel = p_hh, entity = "hhid"),
      list(name = "RVS_hh_remit",  outcome = "log_remit",
           panel = p_hh, entity = "hhid")))
  }
  if (!is.null(rvs_mig) && "is_international" %in% names(rvs_mig)) {
    p_mig <- mk_panel(rvs_mig %>% filter(is_international == 1), fxshock_panel) %>%
             mutate(log_remit        = log(coalesce(remit_amount_rs, 0) + 1),
                    log_earn_primary = log(coalesce(earning_primary_rs, 0) + 1))
    outcomes_list <- c(outcomes_list, list(
      list(name = "RVS_mig_remit", outcome = "log_remit",
           panel = p_mig, entity = "hhid"),
      list(name = "RVS_mig_sent",  outcome = "remit_sent_flag",
           panel = p_mig, entity = "hhid"),
      list(name = "RVS_mig_earn",  outcome = "log_earn_primary",
           panel = p_mig, entity = "hhid")))
  }

  rows <- list(); i <- 0L
  for (o in outcomes_list) {
    for (lvl in 1:6) {
      ref <- if (o$name == "DOFE_permits") 2016L else 2017L
      r <- fit_stage(o$panel, o$outcome, lvl, ref, entity_col = o$entity)
      i <- i + 1L
      rows[[i]] <- tibble(
        baseline = baseline_label,
        outcome  = o$name, y = o$outcome,
        spec     = SPEC_LABELS[as.character(lvl)],
        beta     = if (is.list(r) && is.null(r$err)) r$beta  else NA_real_,
        se       = if (is.list(r) && is.null(r$err)) r$se    else NA_real_,
        t_stat   = if (is.list(r) && is.null(r$err)) r$t     else NA_real_,
        p_val    = if (is.list(r) && is.null(r$err)) r$p     else NA_real_,
        n_obs    = if (is.list(r) && is.null(r$err)) r$n     else NA_integer_,
        r2_w     = if (is.list(r) && is.null(r$err)) r$r2_w  else NA_real_,
        note     = if (is.list(r) && !is.null(r$err)) r$err  else ""
      )
    }
  }
  bind_rows(rows) %>% mutate(sig = sapply(p_val, stars))
}

results <- bind_rows(
  run_for_baseline(fxshock_dofe_2001base, "FX_base_2001 (DOFE shares)"),
  run_for_baseline(fxshock_dofe_dofebase, "FX_base_DOFE (DOFE shares)")
)

dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write.csv(results,
          "district-analysis/output/tab/first_stage_staggered_khanna.csv",
          row.names = FALSE)
cat(sprintf("\nSaved: output/tab/first_stage_staggered_khanna.csv (%d rows)\n",
            nrow(results)))

# Console preview
for (oc in unique(results$outcome)) {
  cat(sprintf("\n=== %s ===\n", oc))
  pv <- results %>%
    filter(outcome == oc) %>%
    select(spec, baseline, beta, t_stat, sig, n_obs) %>%
    mutate(across(c(beta, t_stat), ~ round(., 4))) %>%
    pivot_wider(names_from = baseline,
                values_from = c(beta, t_stat, sig, n_obs))
  print(as.data.frame(pv))
}

# ----------------------------------------------------------------------------
# Note: this script only covers DOFE-vintage shares (2009-10) for both
# FX-baseline-year columns. To also run with 2001 census shares in Khanna
# direction, edit instrument.R line 194 from
#     mutate(fx_to_npr = npr_per_usd / lcu_per_usd)        # NPR/LCU (current)
# to
#     mutate(fx_to_npr = lcu_per_usd / npr_per_usd)        # LCU/NPR (Khanna)
# and re-run instrument.R, then re-run first_stage_staggered.R; the
# fxshock column will now be in Khanna direction throughout.
# ----------------------------------------------------------------------------
