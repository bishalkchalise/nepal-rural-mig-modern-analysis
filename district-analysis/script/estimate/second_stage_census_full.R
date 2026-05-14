################################################################################
#
# SECOND STAGE - FULL SPECIFICATION (CENSUS PANEL)
# ------------------------------------------------------------------------------
# Implements the specification from the project memo:
#
#   y_{m,t} = beta * [ fxshock_{m,t}   * log(mig_int_{m,0}) ]      # main IV term
#           + lambda1' * [ log(mig_int_{m,0}) * tau_t ]            # C_mig
#           + lambda2' * [ fxshock_{m,t}      * tau_t ]            # C_fx
#           + delta'   * [ X_{m,0}           * tau_t ]             # C_X
#           + alpha_m  + gamma_t + eps_{m,t}
#
# Where (m = district here):
#   - fxshock      = share-weighted forex shock (= shareshock_index_2001,
#                    aliased as 'fxshock' in instrument_forex_dist.csv)
#   - log(mig_int) = log of 2001 migration intensity = log(geog_intensity_2001)
#   - X            = 2001 destination-weighted characteristics
#                    Here: six destination-region migrant shares (E Asia,
#                    Gulf, OECD-N, OECD-Europe, S Asia, SE Asia).
#                    `share_other' is the omitted residual category.
#                    Destination-weighted GDP-per-capita NOT included
#                    (fetch_wdi_dest_gdp.py is still a stub; add when built).
#   - tau_t        = year dummies relative to 2011 reference
#   - alpha_m      = district FE
#   - gamma_t      = year FE
#   - SE clustered at district level
#
# Identification : restrict to (2011, 2021) so the SSIV has both cross-
# district and within-district year variation (fxshock = 1 by construction
# at 2001).
#
# Inputs :
#   - district-analysis/data/clean/census/outcomes_district.csv
#   - district-analysis/data/clean/instrument/instrument_forex_dist.csv
#   - district-analysis/data/clean/instrument/dest_region_shares_2001.csv
#   - district-analysis/script/estimation_display.R  (outcome groupings)
#
# Output :
#   - district-analysis/output/tab/second_stage_census_full.csv
#
# Source : run from repo root,
#            source("district-analysis/script/estimate/second_stage_census_full.R")
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

region_shares <- read.csv(
  "district-analysis/data/clean/instrument/dest_region_shares_2001.csv",
  stringsAsFactors = FALSE
)

source("district-analysis/script/estimation_display.R")

# ------------------------------------------------------------------------------
# 2. Build panel
# ------------------------------------------------------------------------------

panel_df <- outcomes_panel %>%
  inner_join(instrument,    by = c("dname", "year")) %>%
  inner_join(region_shares, by = "dname") %>%
  arrange(dname, year) %>%
  filter(year != 2001) %>%                                # drop 2001 baseline
  mutate(
    log_mig_int   = log(pmax(geog_intensity_2001, 1e-12)),
    fx_x_logmi    = fxshock * log_mig_int
  )

cat(sprintf(
  "Panel: %d obs, %d districts, years %s\n",
  nrow(panel_df),
  length(unique(panel_df$dname)),
  paste(sort(unique(panel_df$year)), collapse = ", ")
))

# ------------------------------------------------------------------------------
# 3. Full-spec runner
# ------------------------------------------------------------------------------
# Returns the coefficient on the main fx_x_logmi term plus standard errors
# clustered by dname.

run_full <- function(outcome_vec, group_name, data = panel_df) {
  cat(sprintf("\n--- %s ---\n", group_name))

  results <- map_dfr(outcome_vec, function(y) {
    if (!y %in% names(data)) {
      return(tibble(outcome = y, group = group_name,
                    n_obs = NA_integer_,
                    coef = NA_real_, se = NA_real_,
                    t_stat = NA_real_, p_val = NA_real_,
                    mean_y = NA_real_,
                    note = "outcome not in panel"))
    }

    f <- as.formula(paste0(
      y, " ~ fx_x_logmi ",
      "+ i(year, log_mig_int, ref = 2011) ",
      "+ i(year, fxshock,     ref = 2011) ",
      "+ i(year, share_e_asia,     ref = 2011) ",
      "+ i(year, share_gulf,       ref = 2011) ",
      "+ i(year, share_oecd_north, ref = 2011) ",
      "+ i(year, share_oecd_europe,ref = 2011) ",
      "+ i(year, share_s_asia,     ref = 2011) ",
      "+ i(year, share_se_asia,    ref = 2011) ",
      "| dname + year"
    ))

    m <- tryCatch(
      feols(f, data = data, cluster = ~dname),
      error = function(e) NULL
    )

    if (is.null(m)) {
      return(tibble(outcome = y, group = group_name,
                    n_obs = NA_integer_,
                    coef = NA_real_, se = NA_real_,
                    t_stat = NA_real_, p_val = NA_real_,
                    mean_y = mean(data[[y]], na.rm = TRUE),
                    note = "feols error"))
    }

    ct <- as.data.frame(summary(m)$coeftable)
    if (!"fx_x_logmi" %in% rownames(ct)) {
      return(tibble(outcome = y, group = group_name,
                    n_obs = nobs(m),
                    coef = NA_real_, se = NA_real_,
                    t_stat = NA_real_, p_val = NA_real_,
                    mean_y = mean(data[[y]], na.rm = TRUE),
                    note = "fx_x_logmi dropped (singleton/collinear)"))
    }

    tibble(
      outcome = y, group = group_name,
      n_obs   = nobs(m),
      coef    = ct["fx_x_logmi", "Estimate"],
      se      = ct["fx_x_logmi", "Std. Error"],
      t_stat  = ct["fx_x_logmi", "t value"],
      p_val   = ct["fx_x_logmi", "Pr(>|t|)"],
      mean_y  = mean(data[[y]], na.rm = TRUE),
      note    = ""
    )
  })

  # Console table with significance stars (*** p<.01, ** p<.05, * p<.1)
  print(results %>%
          mutate(across(c(coef, se, t_stat, mean_y), ~ round(., 4)),
                 p_val = round(p_val, 4),
                 sig   = case_when(
                   is.na(p_val)  ~ "",
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
  ~ run_full(unname(groups[[.x]]), group_name = .x)
) %>%
  mutate(sig = case_when(
    is.na(p_val)  ~ "",
    p_val < 0.01  ~ "***",
    p_val < 0.05  ~ "**",
    p_val < 0.1   ~ "*",
    TRUE          ~ ""
  ))

# Convention: *** p<0.01, ** p<0.05, * p<0.1.

# ------------------------------------------------------------------------------
# 5. Save
# ------------------------------------------------------------------------------

dir.create("district-analysis/output/tab",
           recursive = TRUE, showWarnings = FALSE)

write.csv(all_results,
          "district-analysis/output/tab/second_stage_census_full.csv",
          row.names = FALSE)

cat("\nSaved: district-analysis/output/tab/second_stage_census_full.csv\n")
cat(sprintf("Total: %d outcomes across %d groups\n",
            nrow(all_results), length(groups)))
cat(sprintf("Significant at p<0.05 (on fx_x_logmi): %d\n",
            sum(all_results$p_val < 0.05, na.rm = TRUE)))
