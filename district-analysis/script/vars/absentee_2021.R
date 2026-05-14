################################################################################
#
# PCMS 2021 ABSENTEE PANEL (DISTRICT, NON-INDIA, ECONOMIC REASONS)
# ------------------------------------------------------------------------------
# Purpose : Aggregate the PCMS2021 individual absentee record to district-level
#           counts of international migrants to non-India destinations whose
#           reason for absence is economic / educational.
#
# Filter rules :
#   - h17rsn in 1..4   (salary/wage, trade/business, study/training, seeking job)
#                       drops 5+ : dependent / others / not reported / don't know
#   - h17cntry != India (destination = international, non-India)
#
# Input  : data/raw/Full Census Data/Census 2021/Data/PCMS2021_Absentees.dta
# Output : district-analysis/data/clean/census/absentee_2021_non_india_dist.csv
#          columns: dname, year, n_absentees, n_absentees_weighted,
#                   n_male, n_female
#
# Source : run from repo root,
#            source("district-analysis/script/vars/absentee_2021.R")
#
################################################################################

rm(list = ls()); cat("\14")

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
})

abst <- read_dta(
  "data/raw/Full Census Data/Census 2021/Data/PCMS2021_Absentees.dta",
  col_select = any_of(c("dist", "h17sex", "h17rsn", "h17cntry", "absentees_wt"))
)

cat(sprintf("Loaded %d absentee records\n", nrow(abst)))

# Decode labelled doubles into strings/integers
abst_clean <- abst %>%
  mutate(
    dname     = as.character(as_factor(dist)),
    country   = as.character(as_factor(h17cntry)),
    reason_cd = as.integer(h17rsn),
    sex_cd    = as.integer(h17sex)
  )

# Apply user filters
abst_kept <- abst_clean %>%
  filter(reason_cd %in% 1:4,
         !is.na(country),
         country != "India")

cat(sprintf("After filters (reasons 1-4, non-India): %d records (%d%%)\n",
            nrow(abst_kept),
            round(100 * nrow(abst_kept) / nrow(abst_clean))))

# Aggregate to district
absentee_district <- abst_kept %>%
  group_by(dname) %>%
  summarise(
    n_absentees          = n(),
    n_absentees_weighted = sum(absentees_wt, na.rm = TRUE),
    n_male               = sum(sex_cd == 1, na.rm = TRUE),
    n_female             = sum(sex_cd == 2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = 2021) %>%
  select(dname, year, n_absentees, n_absentees_weighted, n_male, n_female)

dir.create("district-analysis/data/clean/census",
           recursive = TRUE, showWarnings = FALSE)

write.csv(absentee_district,
          "district-analysis/data/clean/census/absentee_2021_non_india_dist.csv",
          row.names = FALSE)

cat(sprintf("Saved: %d districts, %d total absentees (non-India, econ/study reasons)\n",
            nrow(absentee_district),
            sum(absentee_district$n_absentees)))
