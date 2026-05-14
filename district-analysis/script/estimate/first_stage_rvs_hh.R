################################################################################
#
# FIRST-STAGE: NRVS HH-LEVEL ON DISTRICT FXSHOCK
# ------------------------------------------------------------------------------
# Purpose : Same identification as first_stage_rvs.R, but keep the analysis at
#           the household level instead of pre-aggregating to district x year.
#           Treatment (fxshock) is assigned at (dname, year); outcomes are
#           per-HH. With dname + year FE and SE clustered by dname, inference
#           is correct for cluster-level treatment (Moulton / BRL).
#
#           This recovers the within-district variation we threw away in the
#           district aggregate -- usually a big power gain when district
#           sample sizes are small (NRVS = ~50-150 HH per district).
#
# Inputs :
#   - district-analysis/data/clean/rvs/migration_hh_year.csv
#   - district-analysis/data/clean/instrument/instrument_forex_dist.csv
#
# Outcomes :
#   (i)  has_migrant_intl                       (linear probability)
#   (ii) log(remit_amount_intl_12m_rs + 1)      (intensive margin)
#
# Source : run from repo root,
#            source("district-analysis/script/estimate/first_stage_rvs_hh.R")
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

hh <- read_csv(
  "district-analysis/data/clean/rvs/migration_hh_year.csv",
  show_col_types = FALSE, progress = FALSE
)

instr <- read.csv(
  "district-analysis/data/clean/instrument/instrument_forex_dist.csv",
  stringsAsFactors = FALSE
) %>%
  select(dname, year, fxshock, geog_intensity_2001)

instr_dofe <- read.csv(
  "district-analysis/data/clean/instrument/instrument_dofe_dist.csv",
  stringsAsFactors = FALSE
) %>%
  select(dname, year, fxshock_dofe)

# ------------------------------------------------------------------------------
# 1. Normalize HH-side district names to instrument convention
# ------------------------------------------------------------------------------

rvs_to_census <- c(
  "CHITWAN"    = "Chitawan",
  "DHANUSHA"   = "Dhanusa",
  "KAPILVASTU" = "Kapilbastu",
  "MAKAWANPUR" = "Makwanpur",
  "TANAHUN"    = "Tanahu",
  "TEHRATHUM"      = "Terhathum",
  "KABHREPALANCHOK" = "Kavrepalanchok"
)

dist_key <- if ("district_name" %in% names(hh)) "district_name" else
            if ("district77"    %in% names(hh)) "district77"    else
            if ("district"      %in% names(hh)) "district"      else
            stop("No district column in migration_hh_year.csv")

hh <- hh %>%
  rename(dname_raw = all_of(dist_key)) %>%
  mutate(
    upr   = toupper(str_squish(dname_raw)),
    dname = rvs_to_census[upr],
    dname = ifelse(is.na(dname), str_to_title(tolower(upr)), dname)
  ) %>%
  select(-upr)

# ------------------------------------------------------------------------------
# 2. Join (HH x year) onto instrument (dname x year)
# ------------------------------------------------------------------------------

fs_df <- hh %>%
  inner_join(instr,      by = c("dname", "year")) %>%
  inner_join(instr_dofe, by = c("dname", "year")) %>%
  # CONDITIONAL SAMPLE: HHs with at least one intl migrant. Note that this
  # is selection-on-outcome -- the coefficient is the intensive-margin
  # response among migrant HHs, not an unbiased average treatment effect.
  filter(has_migrant_intl == 1) %>%
  mutate(
    log_n_intl_migrants = log(n_intl_migrants + 1),
    log_remit_intl      = log(remit_amount_intl_12m_rs + 1),
    log_mig_int         = log(pmax(geog_intensity_2001, 1e-12)),
    fx_x_logmi          = fxshock      * log_mig_int,
    fxdofe_x_logmi      = fxshock_dofe * log_mig_int
  )

unmatched <- setdiff(unique(hh$dname), unique(instr$dname))
if (length(unmatched) > 0) {
  cat("WARNING - NRVS districts with no instrument match:\n")
  print(unmatched)
}

cat(sprintf(
  "HH-level first-stage panel: %d HH x year obs, %d districts, %d year(s) (%s)\n",
  nrow(fs_df),
  length(unique(fs_df$dname)),
  length(unique(fs_df$year)),
  paste(sort(unique(fs_df$year)), collapse = ", ")
))

# ------------------------------------------------------------------------------
# 3. First-stage regressions
# ------------------------------------------------------------------------------

n_years <- length(unique(fs_df$year))

run_quad <- function(outcome) {
  fit <- function(regressor) {
    f <- as.formula(paste0(outcome, " ~ ", regressor, " | dname + year"))
    feols(f, data = fs_df, cluster = ~dname)
  }
  list(
    bare_2001  = fit("fxshock"),
    inter_2001 = fit("fx_x_logmi"),
    bare_dofe  = fit("fxshock_dofe"),
    inter_dofe = fit("fxdofe_x_logmi")
  )
}

if (n_years > 1) {
  q_n     <- run_quad("log_n_intl_migrants")
  q_remit <- run_quad("log_remit_intl")

  cat("\n=== HH-level first-stage: log(n_intl_migrants+1) ===\n")
  print(etable(q_n$bare_2001, q_n$inter_2001, q_n$bare_dofe, q_n$inter_dofe,
               cluster = ~dname,
               headers = c("fx_2001", "fx_2001 x logmi",
                           "fx_dofe", "fx_dofe x logmi"),
               digits = 4, fitstat = c("n", "r2", "wr2")))

  cat("\n=== HH-level first-stage: log(remit_intl+1) ===\n")
  print(etable(q_remit$bare_2001, q_remit$inter_2001,
               q_remit$bare_dofe, q_remit$inter_dofe,
               cluster = ~dname,
               headers = c("fx_2001", "fx_2001 x logmi",
                           "fx_dofe", "fx_dofe x logmi"),
               digits = 4, fitstat = c("n", "r2", "wr2")))
} else {
  # Single wave: dname FE alone collapses (perfect collinearity), so use
  # cross-section with log_pop control, HC1 SE.
  m1 <- feols(has_migrant_intl ~ fxshock,                       data = fs_df, se = "hetero")
  m1c<- feols(has_migrant_intl ~ fxshock + log(geog_pop_2001),  data = fs_df, se = "hetero")
  m2 <- feols(log_remit_intl   ~ fxshock,                       data = fs_df, se = "hetero")
  m2c<- feols(log_remit_intl   ~ fxshock + log(geog_pop_2001),  data = fs_df, se = "hetero")

  cat("\n=== HH-level first-stage on fxshock (single wave, HC1) ===\n")
  print(etable(m1, m1c, m2, m2c,
               headers = c("has_mig", "has_mig+pop",
                           "log_remit", "log_remit+pop"),
               digits  = 4,
               fitstat = c("n", "r2")))
}

# ------------------------------------------------------------------------------
# 4. Save summary
# ------------------------------------------------------------------------------

dir.create("district-analysis/output/tab",
           recursive = TRUE, showWarnings = FALSE)

summarise_quad <- function(quad, outcome_label) {
  mods    <- list(quad$bare_2001, quad$inter_2001, quad$bare_dofe, quad$inter_dofe)
  regnms  <- c("fxshock", "fx_x_logmi", "fxshock_dofe", "fxdofe_x_logmi")
  tibble(
    outcome = outcome_label,
    spec    = c("fx_2001", "fx_2001 x logmi", "fx_dofe", "fx_dofe x logmi"),
    coef    = mapply(function(m, nm) coef(m)[nm], mods, regnms),
    se      = mapply(function(m, nm)
                       sqrt(diag(vcov(m, cluster = ~dname)))[nm], mods, regnms),
    n_obs   = sapply(mods, nobs),
    r2_w    = sapply(mods, function(m) fitstat(m, "wr2", simplify = TRUE))
  ) %>%
    mutate(t_stat = coef / se,
           p_val  = 2 * pnorm(-abs(t_stat)))
}

if (n_years > 1) {
  fs_summary <- bind_rows(
    summarise_quad(q_n,     "log(n_intl_migrants+1)"),
    summarise_quad(q_remit, "log(remit_intl+1)")
  )
} else {
  fs_summary <- tibble(
    outcome = c("has_mig", "has_mig+pop",
                "log_remit", "log_remit+pop"),
    coef    = c(coef(m1)["fxshock"],  coef(m1c)["fxshock"],
                coef(m2)["fxshock"],  coef(m2c)["fxshock"]),
    se      = c(sqrt(diag(vcov(m1,  se = "hetero")))["fxshock"],
                sqrt(diag(vcov(m1c, se = "hetero")))["fxshock"],
                sqrt(diag(vcov(m2,  se = "hetero")))["fxshock"],
                sqrt(diag(vcov(m2c, se = "hetero")))["fxshock"]),
    n_obs   = c(nobs(m1), nobs(m1c), nobs(m2), nobs(m2c)),
    r2      = c(fitstat(m1,  "r2", simplify = TRUE),
                fitstat(m1c, "r2", simplify = TRUE),
                fitstat(m2,  "r2", simplify = TRUE),
                fitstat(m2c, "r2", simplify = TRUE))
  ) %>%
    mutate(t_stat = coef / se,
           p_val  = 2 * pnorm(-abs(t_stat)))
}

write.csv(fs_summary,
          "district-analysis/output/tab/first_stage_rvs_hh.csv",
          row.names = FALSE)

cat("\nSaved: district-analysis/output/tab/first_stage_rvs_hh.csv\n")
