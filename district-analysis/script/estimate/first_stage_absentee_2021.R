################################################################################
#
# FIRST-STAGE: 2021 PCMS ABSENTEES ON FOREX SSIV (CROSS-SECTION)
# ------------------------------------------------------------------------------
# Purpose : Validate the SSIV against the 2021 census stock of non-India
#           economic absentees. Cross-section regression of log(absentees+1)
#           on the SSIV level shifters, with log baseline population as a
#           control. Heteroskedasticity-robust SE (HC1).
#
# Inputs :
#   - district-analysis/data/clean/census/absentee_2021_non_india_dist.csv
#       (build via script/vars/absentee_2021.R)
#   - district-analysis/data/clean/instrument/instrument_forex_dist.csv
#       (filtered to year == 2021)
#
# Source : run from repo root,
#            source("district-analysis/script/estimate/first_stage_absentee_2021.R")
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

abst <- read.csv(
  "district-analysis/data/clean/census/absentee_2021_non_india_dist.csv",
  stringsAsFactors = FALSE
)

instr_2021 <- read.csv(
  "district-analysis/data/clean/instrument/instrument_forex_dist.csv",
  stringsAsFactors = FALSE
) %>%
  filter(year == 2021)

cs_df <- abst %>%
  inner_join(instr_2021, by = c("dname", "year")) %>%
  mutate(log_absentees = log(n_absentees + 1),
         log_pop_2001  = log(geog_pop_2001))

cat(sprintf("Cross-section: %d districts (2021)\n", nrow(cs_df)))

# Bare (no controls)
m1 <- feols(log_absentees ~ ssiv_index_2001,       data = cs_df, se = "hetero")
m2 <- feols(log_absentees ~ shareshock_index_2001, data = cs_df, se = "hetero")
m3 <- feols(log_absentees ~ absexp_index_2001,     data = cs_df, se = "hetero")

# Controlling for 2001 population (scale of district)
m4 <- feols(log_absentees ~ ssiv_index_2001       + log_pop_2001,
            data = cs_df, se = "hetero")
m5 <- feols(log_absentees ~ shareshock_index_2001 + log_pop_2001,
            data = cs_df, se = "hetero")
m6 <- feols(log_absentees ~ absexp_index_2001     + log_pop_2001,
            data = cs_df, se = "hetero")

cat("\n=== First-stage: log(2021 non-India absentees + 1) on SSIV level shifters ===\n")
print(etable(m1, m2, m3, m4, m5, m6,
             headers = c("ssiv", "share", "abs",
                         "ssiv+pop", "share+pop", "abs+pop"),
             digits = 4, fitstat = c("n", "r2")))

# Save coefficient table
dir.create("district-analysis/output/tab",
           recursive = TRUE, showWarnings = FALSE)

mods <- list(m1, m2, m3, m4, m5, m6)
fs_summary <- tibble(
  spec       = c("ssiv_index_2001", "shareshock_index_2001", "absexp_index_2001",
                 "ssiv_index_2001+log_pop", "shareshock_index_2001+log_pop",
                 "absexp_index_2001+log_pop"),
  coef       = sapply(mods, function(m) coef(m)[2]),
  se         = sapply(mods, function(m) sqrt(diag(vcov(m, se = "hetero")))[2]),
  n_obs      = sapply(mods, nobs),
  r2         = sapply(mods, function(m) fitstat(m, "r2", simplify = TRUE))
) %>%
  mutate(t_stat = coef / se,
         p_val  = 2 * pnorm(-abs(t_stat)))

write.csv(fs_summary,
          "district-analysis/output/tab/first_stage_absentee_2021.csv",
          row.names = FALSE)

cat("\nSaved: district-analysis/output/tab/first_stage_absentee_2021.csv\n")
