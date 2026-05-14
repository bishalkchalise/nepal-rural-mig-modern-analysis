################################################################################
#
# SECOND STAGE: CENSUS OUTCOMES ON FXSHOCK (REDUCED FORM)
# ------------------------------------------------------------------------------
# Purpose : Run the reduced-form regression of each census-derived outcome on
#           fxshock, with dname + year FE and SE clustered by dname. With 2-3
#           census years and the SSIV measured at each year, this gives the
#           reduced-form effect of the FX-driven migration shock.
#
#           This is intentionally a minimal panel spec - no `i(year, log_mi)`
#           interactions, no lagged shocks. Once the basics work end-to-end
#           we can layer back complexity.
#
# Inputs :
#   - district-analysis/data/clean/census/outcomes_district.csv
#   - district-analysis/data/clean/instrument/instrument_forex_dist.csv
#   - district-analysis/script/estimation_display.R  (outcome groupings)
#
# Outputs :
#   - district-analysis/output/tab/second_stage_census.csv
#
# Source : run from repo root,
#            source("district-analysis/script/estimate/second_stage_census.R")
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

# ------------------------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------------------------

outcomes_panel <- read.csv(
  "district-analysis/data/clean/census/outcomes_district.csv",
  stringsAsFactors = FALSE
)

instrument <- read.csv(
  "district-analysis/data/clean/instrument/instrument_forex_dist.csv",
  stringsAsFactors = FALSE
)

# Outcome group definitions (amenities, assets, housing, ...)
source("district-analysis/script/estimation_display.R")

# ------------------------------------------------------------------------------
# 2. Build panel: outcomes x instrument at census years
# ------------------------------------------------------------------------------

panel_df <- outcomes_panel %>%
  inner_join(instrument, by = c("dname", "year")) %>%
  arrange(dname, year) %>%
  # Optionally drop 2001 - reduced-form is identified off the change between
  # waves with FE. Comment out the next line to include 2001.
  filter(year != 2001)

cat(sprintf(
  "Panel: %d obs, %d districts, %d census years (%s)\n",
  nrow(panel_df),
  length(unique(panel_df$dname)),
  length(unique(panel_df$year)),
  paste(sort(unique(panel_df$year)), collapse = ", ")
))

# ------------------------------------------------------------------------------
# 3. Reduced-form runner
# ------------------------------------------------------------------------------

run_rf <- function(outcome_vec, group_name, data = panel_df) {
  cat(sprintf("\n--- %s ---\n", group_name))

  results <- map_dfr(outcome_vec, function(y) {
    if (!y %in% names(data)) {
      return(tibble(outcome = y, n_obs = NA_integer_, coef = NA_real_,
                    se = NA_real_, t_stat = NA_real_, p_val = NA_real_,
                    mean_y = NA_real_, group = group_name,
                    note = "outcome not in panel"))
    }

    f <- as.formula(paste0(y, " ~ fxshock | dname + year"))
    m <- tryCatch(
      feols(f, data = data, cluster = ~dname),
      error = function(e) NULL
    )

    if (is.null(m)) {
      return(tibble(outcome = y, n_obs = NA_integer_, coef = NA_real_,
                    se = NA_real_, t_stat = NA_real_, p_val = NA_real_,
                    mean_y = mean(data[[y]], na.rm = TRUE), group = group_name,
                    note = "feols error"))
    }

    ct <- as.data.frame(summary(m)$coeftable)
    if (!"fxshock" %in% rownames(ct)) {
      return(tibble(outcome = y, n_obs = nobs(m), coef = NA_real_,
                    se = NA_real_, t_stat = NA_real_, p_val = NA_real_,
                    mean_y = mean(data[[y]], na.rm = TRUE), group = group_name,
                    note = "fxshock dropped (singleton/collinear)"))
    }

    tibble(
      outcome = y,
      n_obs   = nobs(m),
      coef    = ct["fxshock", "Estimate"],
      se      = ct["fxshock", "Std. Error"],
      t_stat  = ct["fxshock", "t value"],
      p_val   = ct["fxshock", "Pr(>|t|)"],
      mean_y  = mean(data[[y]], na.rm = TRUE),
      group   = group_name,
      note    = ""
    )
  })

  # Quick console table with stars
  print(results %>%
          mutate(across(c(coef, se, t_stat, mean_y), ~ round(., 4)),
                 p_val = round(p_val, 4),
                 sig   = case_when(
                   is.na(p_val) ~ "",
                   p_val < 0.01  ~ "***",
                   p_val < 0.05  ~ "**",
                   p_val < 0.1   ~ "*",
                   TRUE          ~ ""
                 )) %>%
          select(outcome, n_obs, coef, se, t_stat, p_val, sig, mean_y))

  results
}

# ------------------------------------------------------------------------------
# 4. Run across outcome groups
# ------------------------------------------------------------------------------

groups <- list(
  amenities        = amenities,
  assets           = assets,
  housing          = housing,
  female_ownership = female_ownership,
  enterprise       = enterprise,
  education        = education,
  demography       = demography,
  mortality        = mortality,
  work             = work,
  occupation       = occupation,
  migration        = migration,
  gender           = gender,
  household        = household,
  industry         = industry
)

all_results <- map_dfr(
  names(groups),
  ~ run_rf(unname(groups[[.x]]), group_name = .x)
) %>%
  mutate(sig = case_when(
    is.na(p_val)  ~ "",
    p_val < 0.01  ~ "***",
    p_val < 0.05  ~ "**",
    p_val < 0.1   ~ "*",
    TRUE          ~ ""
  ))
# Convention used: *** p<0.01, ** p<0.05, * p<0.1 (standard econ convention).

# ------------------------------------------------------------------------------
# 5. Save
# ------------------------------------------------------------------------------

dir.create("district-analysis/output/tab",
           recursive = TRUE, showWarnings = FALSE)

write.csv(all_results,
          "district-analysis/output/tab/second_stage_census.csv",
          row.names = FALSE)

cat("\nSaved: district-analysis/output/tab/second_stage_census.csv\n")
cat(sprintf("Total: %d outcomes across %d groups\n",
            nrow(all_results), length(groups)))
cat(sprintf("Significant at p<0.05: %d\n",
            sum(all_results$p_val < 0.05, na.rm = TRUE)))
