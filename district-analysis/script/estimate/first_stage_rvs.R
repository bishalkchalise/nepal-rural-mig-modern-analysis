################################################################################
#
# FIRST-STAGE: NRVS MIGRANTS + REMITTANCE ON FXSHOCK
# ------------------------------------------------------------------------------
# Purpose : Validate the forex shift-share instrument against two NRVS-based
#           migration outcomes:
#             (i)  district count of international migrants
#             (ii) district sum of international remittance (NPR, 12 months)
#
# Inputs :
#   - district-analysis/data/clean/rvs/migration_district_year.csv
#       (build via script/vars/rvs/04_migration.R)
#   - district-analysis/data/clean/instrument/instrument_forex_dist.csv
#
# Identification :
#   - If NRVS spans multiple years: panel with dname + year FE, cluster ~dname.
#   - If single wave: cross-section, optional log_pop_2001 control,
#                     heteroskedasticity-robust SE.
#
# Source : run from repo root,
#            source("district-analysis/script/estimate/first_stage_rvs.R")
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

rvs <- read.csv(
  "district-analysis/data/clean/rvs/migration_district_year.csv",
  stringsAsFactors = FALSE
)

instr <- read.csv(
  "district-analysis/data/clean/instrument/instrument_forex_dist.csv",
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 1. Normalize NRVS district names to match instrument convention
# ------------------------------------------------------------------------------
# Instrument uses title case ("Achham", "Chitawan", ...). NRVS spelling
# variants we know to differ from the census/instrument convention:
rvs_to_census <- c(
  "CHITWAN"    = "Chitawan",
  "DHANUSHA"   = "Dhanusa",
  "KAPILVASTU" = "Kapilbastu",
  "MAKAWANPUR" = "Makwanpur",
  "TANAHUN"    = "Tanahu",
  "TEHRATHUM"      = "Terhathum",
  "KABHREPALANCHOK" = "Kavrepalanchok"
)

rvs <- rvs %>%
  mutate(
    upr   = toupper(str_squish(dname_raw)),
    dname = rvs_to_census[upr],
    dname = ifelse(is.na(dname), str_to_title(tolower(upr)), dname)
  ) %>%
  select(-upr)

# ------------------------------------------------------------------------------
# 2. Join + diagnostics
# ------------------------------------------------------------------------------

fs_df <- rvs %>%
  inner_join(instr, by = c("dname", "year")) %>%
  mutate(
    # Outcome 1: total number of intl migrants from the district (log+1)
    log_n_intl_migrants = log(n_intl_migrants + 1),

    # Outcome 2: share of HHs with >=1 intl migrant (extensive margin)
    share_hh_with_migrant = n_hh_with_intl_migrant / pmax(n_hh, 1),

    # Outcome 3: total intl remittance received in district, 12-mo, NPR (log+1)
    log_remit_intl = log(remit_amount_intl_12m_rs + 1),

    log_pop_2001 = log(geog_pop_2001)
  )

unmatched <- setdiff(unique(rvs$dname), unique(instr$dname))
if (length(unmatched) > 0) {
  cat("WARNING - NRVS districts with no instrument match:\n")
  print(unmatched)
}

years <- sort(unique(fs_df$year))
n_years <- length(years)

cat(sprintf(
  "NRVS first-stage panel: %d obs, %d districts, %d year(s) (%s)\n",
  nrow(fs_df),
  length(unique(fs_df$dname)),
  n_years,
  paste(years, collapse = ", ")
))

# ------------------------------------------------------------------------------
# 3. Regressions
# ------------------------------------------------------------------------------

if (n_years > 1) {
  # Panel: dname + year FE, cluster by dname
  m1 <- feols(log_n_intl_migrants   ~ fxshock | dname + year,
              data = fs_df, cluster = ~dname)
  m2 <- feols(share_hh_with_migrant ~ fxshock | dname + year,
              data = fs_df, cluster = ~dname)
  m3 <- feols(log_remit_intl        ~ fxshock | dname + year,
              data = fs_df, cluster = ~dname)

  cat("\n=== District first-stage on fxshock (dname + year FE) ===\n")
  print(etable(m1, m2, m3,
               cluster = ~dname,
               headers = c("log(n_intl_migrants+1)",
                           "share_hh_with_migrant",
                           "log(intl_remit+1)"),
               digits  = 4,
               fitstat = c("n", "r2", "wr2")))
} else {
  # Cross-section: optional log_pop_2001 control, HC1 SE
  m1  <- feols(log_n_intl_migrants   ~ fxshock,                data = fs_df, se = "hetero")
  m1c <- feols(log_n_intl_migrants   ~ fxshock + log_pop_2001, data = fs_df, se = "hetero")
  m2  <- feols(share_hh_with_migrant ~ fxshock,                data = fs_df, se = "hetero")
  m2c <- feols(share_hh_with_migrant ~ fxshock + log_pop_2001, data = fs_df, se = "hetero")
  m3  <- feols(log_remit_intl        ~ fxshock,                data = fs_df, se = "hetero")
  m3c <- feols(log_remit_intl        ~ fxshock + log_pop_2001, data = fs_df, se = "hetero")

  cat(sprintf("\n=== Cross-section first-stage on fxshock (year = %d) ===\n",
              years[1]))
  print(etable(m1, m1c, m2, m2c, m3, m3c,
               headers = c("n_mig", "n_mig+pop",
                           "share", "share+pop",
                           "remit", "remit+pop"),
               digits = 4, fitstat = c("n", "r2")))
}

# ------------------------------------------------------------------------------
# 4. Save coefficient summary
# ------------------------------------------------------------------------------

dir.create("district-analysis/output/tab",
           recursive = TRUE, showWarnings = FALSE)

if (n_years > 1) {
  mods <- list(m1, m2, m3)
  fs_summary <- tibble(
    outcome = c("log(n_intl_migrants+1)",
                "share_hh_with_migrant",
                "log(intl_remit+1)"),
    coef    = sapply(mods, function(m) coef(m)["fxshock"]),
    se      = sapply(mods,
                     function(m) sqrt(diag(vcov(m, cluster = ~dname)))["fxshock"]),
    n_obs   = sapply(mods, nobs),
    r2_w    = sapply(mods, function(m) fitstat(m, "wr2", simplify = TRUE))
  ) %>%
    mutate(t_stat = coef / se,
           p_val  = 2 * pnorm(-abs(t_stat)))
} else {
  mods <- list(m1, m1c, m2, m2c, m3, m3c)
  fs_summary <- tibble(
    outcome = c("n_intl_migrants", "n_intl_migrants+pop",
                "share_hh_with_migrant", "share_hh_with_migrant+pop",
                "intl_remit", "intl_remit+pop"),
    coef    = sapply(mods, function(m) coef(m)["fxshock"]),
    se      = sapply(mods,
                     function(m) sqrt(diag(vcov(m, se = "hetero")))["fxshock"]),
    n_obs   = sapply(mods, nobs),
    r2      = sapply(mods, function(m) fitstat(m, "r2", simplify = TRUE))
  ) %>%
    mutate(t_stat = coef / se,
           p_val  = 2 * pnorm(-abs(t_stat)))
}

write.csv(fs_summary,
          "district-analysis/output/tab/first_stage_rvs.csv",
          row.names = FALSE)

cat("\nSaved: district-analysis/output/tab/first_stage_rvs.csv\n")
