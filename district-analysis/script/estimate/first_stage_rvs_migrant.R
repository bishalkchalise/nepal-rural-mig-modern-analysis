################################################################################
#
# FIRST-STAGE: NRVS MIGRANT-LEVEL ON DISTRICT FXSHOCK
# ------------------------------------------------------------------------------
# Purpose : Same identification strategy as the HH-level first-stage, but the
#           unit of analysis is each migrant. Sample is filtered to
#           international migrants only (is_international == 1). Treatment
#           (fxshock) is assigned at (dname, year); SE clustered by dname.
#
# Outcomes :
#   (i)   log(remit_amount_rs + 1)      per-migrant remittance sent home, 12 mo
#   (ii)  remit_sent_flag               binary: migrant sent any remittance
#   (iii) log(earning_primary_rs + 1)   per-migrant primary earnings
#
# Inputs :
#   - district-analysis/data/clean/rvs/migration_migrant_year.csv
#   - district-analysis/data/clean/instrument/instrument_forex_dist.csv
#
# Source : run from repo root,
#            source("district-analysis/script/estimate/first_stage_rvs_migrant.R")
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

mig <- read_csv(
  "district-analysis/data/clean/rvs/migration_migrant_year.csv",
  show_col_types = FALSE, progress = FALSE
)

instr <- read.csv(
  "district-analysis/data/clean/instrument/instrument_forex_dist.csv",
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 1. Normalize district names
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

dist_key <- if ("district_name" %in% names(mig)) "district_name" else
            if ("district77"    %in% names(mig)) "district77"    else
            if ("district"      %in% names(mig)) "district"      else
            stop("No district column in migration_migrant_year.csv")

mig <- mig %>%
  rename(dname_raw = all_of(dist_key)) %>%
  mutate(
    upr   = toupper(str_squish(dname_raw)),
    dname = rvs_to_census[upr],
    dname = ifelse(is.na(dname), str_to_title(tolower(upr)), dname)
  ) %>%
  select(-upr)

# ------------------------------------------------------------------------------
# 2. Filter to international migrants and join instrument
# ------------------------------------------------------------------------------

fs_df <- mig %>%
  filter(is_international == 1) %>%
  inner_join(instr, by = c("dname", "year")) %>%
  mutate(
    log_remit         = log(coalesce(remit_amount_rs, 0)      + 1),
    log_earn_primary  = log(coalesce(earning_primary_rs, 0)   + 1)
  )

cat(sprintf(
  "Migrant-level first-stage: %d intl migrant x year obs, %d districts, %d year(s) (%s)\n",
  nrow(fs_df),
  length(unique(fs_df$dname)),
  length(unique(fs_df$year)),
  paste(sort(unique(fs_df$year)), collapse = ", ")
))

# ------------------------------------------------------------------------------
# 3. First-stage regressions
# ------------------------------------------------------------------------------

n_years <- length(unique(fs_df$year))

if (n_years > 1) {
  m1 <- feols(log_remit        ~ fxshock | dname + year,
              data = fs_df, cluster = ~dname)
  m2 <- feols(remit_sent_flag  ~ fxshock | dname + year,
              data = fs_df, cluster = ~dname)
  m3 <- feols(log_earn_primary ~ fxshock | dname + year,
              data = fs_df, cluster = ~dname)

  cat("\n=== Migrant-level first-stage on fxshock (intl migrants) ===\n")
  print(etable(m1, m2, m3,
               cluster = ~dname,
               headers = c("log(remit+1)", "remit_sent_flag",
                           "log(earn_primary+1)"),
               digits  = 4,
               fitstat = c("n", "r2", "wr2")))
} else {
  m1  <- feols(log_remit        ~ fxshock,                       data = fs_df, se = "hetero")
  m1c <- feols(log_remit        ~ fxshock + log(geog_pop_2001),  data = fs_df, se = "hetero")
  m2  <- feols(remit_sent_flag  ~ fxshock,                       data = fs_df, se = "hetero")
  m3  <- feols(log_earn_primary ~ fxshock,                       data = fs_df, se = "hetero")

  cat("\n=== Migrant-level first-stage on fxshock (single wave, intl migrants) ===\n")
  print(etable(m1, m1c, m2, m3,
               headers = c("log(remit+1)", "log(remit+1)+pop",
                           "remit_sent", "log(earn+1)"),
               digits  = 4, fitstat = c("n", "r2")))
}

# ------------------------------------------------------------------------------
# 4. Save summary
# ------------------------------------------------------------------------------

dir.create("district-analysis/output/tab",
           recursive = TRUE, showWarnings = FALSE)

if (n_years > 1) {
  mods <- list(m1, m2, m3)
  fs_summary <- tibble(
    outcome = c("log(remit+1)", "remit_sent_flag", "log(earn_primary+1)"),
    coef    = sapply(mods, function(m) coef(m)["fxshock"]),
    se      = sapply(mods,
                     function(m) sqrt(diag(vcov(m, cluster = ~dname)))["fxshock"]),
    n_obs   = sapply(mods, nobs),
    r2_w    = sapply(mods, function(m) fitstat(m, "wr2", simplify = TRUE))
  ) %>%
    mutate(t_stat = coef / se,
           p_val  = 2 * pnorm(-abs(t_stat)))
} else {
  mods <- list(m1, m1c, m2, m3)
  fs_summary <- tibble(
    outcome = c("log(remit+1)", "log(remit+1)+pop",
                "remit_sent_flag", "log(earn_primary+1)"),
    coef    = sapply(mods, function(m) coef(m)["fxshock"]),
    se      = sapply(mods, function(m) sqrt(diag(vcov(m, se = "hetero")))["fxshock"]),
    n_obs   = sapply(mods, nobs),
    r2      = sapply(mods, function(m) fitstat(m, "r2", simplify = TRUE))
  ) %>%
    mutate(t_stat = coef / se,
           p_val  = 2 * pnorm(-abs(t_stat)))
}

write.csv(fs_summary,
          "district-analysis/output/tab/first_stage_rvs_migrant.csv",
          row.names = FALSE)

cat("\nSaved: district-analysis/output/tab/first_stage_rvs_migrant.csv\n")
