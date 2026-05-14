################################################################################
#
# ROBUSTNESS: intensity scaling for the MUNICIPALITY second-stage
# ------------------------------------------------------------------------------
# Mirrors the district robustness script but runs on the archived
# municipality-level data. For a chosen set of headline outcomes, runs the
# slide's spec with five different intensity transforms, both WITHOUT and
# WITH the C_mig and C_fx year-trend controls.
#
#   Intensities tested
#     1. log(mig_int)            (slide's original, always negative)
#     2. log(1000   * mig_int)   (per-thousand log scale; mixed sign at muni)
#     3. log(100000 * mig_int)   (per-100k log scale; uniformly positive)
#     4. mig_int (linear)        (always positive, no log; sign-uniform)
#
# Inputs (archived municipality paths -- must exist locally) :
#   - data/clean/instrument/instrument_mun.csv
#   - data/clean/census/census_outcomes_municipality.csv
#
# Output :
#   - data/clean/robustness_intensity_scaling_muni.csv
#     (one row per outcome x intensity x controls)
#
# Run from the original (municipality-era) project root, NOT the
# district-analysis sub-project.  The script writes the CSV to
# data/clean/ relative to the working directory.
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

# ------------------------------------------------------------------------------
# 1. Load muni data
# ------------------------------------------------------------------------------

instrument <- read.csv(
  "data/clean/instrument/instrument_mun.csv",
  stringsAsFactors = FALSE
)

outcomes <- read.csv(
  "data/clean/census/census_outcomes_municipality.csv",
  stringsAsFactors = FALSE
)

# Join + build intensity transforms
panel_df <- outcomes %>%
  inner_join(instrument, by = c("lgcode", "year")) %>%
  filter(year != 2001) %>%                            # SSIV = 1 by construction
  mutate(
    log_mig_int       = log(pmax(geog_intensity_2001, 1e-12)),
    log_mig_int_x1k   = log(pmax(1000   * geog_intensity_2001, 1e-12)),
    log_mig_int_x100k = log(pmax(100000 * geog_intensity_2001, 1e-12)),
    mig_int_lin       = geog_intensity_2001
  )

cat(sprintf("Muni panel: %d obs, %d municipalities, years %s\n",
            nrow(panel_df),
            length(unique(panel_df$lgcode)),
            paste(sort(unique(panel_df$year)), collapse = ", ")))

# ------------------------------------------------------------------------------
# 2. Outcomes to test (headline picks across groups)
# ------------------------------------------------------------------------------

# Keep this list small to avoid running thousands of regressions. Adjust if
# you want to test more outcomes -- the script handles any column present in
# the outcomes file.
outcomes_to_test <- c(
  "amen_water_piped",
  "amen_cooking_modern",
  "amen_lighting_electricity",
  "amen_toilet_modern",
  "housing_own",
  "housing_foundation_modern",
  "mig_in_share",
  "mig_in_international",
  "ent_has_nonagro",
  "head_female_share",
  "flfp_all",
  "fert_total",                       # adjust if not in your file
  "edu_years_15p",                    # adjust if not in your file
  "amen_asset_count_mean"             # adjust if not in your file
)

# Drop any outcomes not present in the panel
outcomes_to_test <- intersect(outcomes_to_test, names(panel_df))
cat(sprintf("Testing %d outcomes:\n  %s\n",
            length(outcomes_to_test),
            paste(outcomes_to_test, collapse = ", ")))

# ------------------------------------------------------------------------------
# 3. Cell runner
# ------------------------------------------------------------------------------

intensity_specs <- list(
  "log(mig_int)"        = "log_mig_int",
  "log(1000*mi)"        = "log_mig_int_x1k",
  "log(100000*mi)"      = "log_mig_int_x100k",
  "mi_linear"           = "mig_int_lin"
)

run_cell <- function(outcome, intensity_col, with_controls) {

  df <- panel_df
  df$main <- df$fxshock * df[[intensity_col]]

  rhs <- "main"
  if (with_controls) {
    rhs <- paste(rhs,
                 sprintf("i(year, %s)", intensity_col),
                 "i(year, fxshock)",
                 sep = " + ")
  }
  f <- as.formula(paste0(outcome, " ~ ", rhs, " | lgcode + year"))

  m <- tryCatch(feols(f, data = df, cluster = ~lgcode),
                error = function(e) NULL)
  if (is.null(m)) {
    return(tibble(coef = NA_real_, se = NA_real_,
                  t_stat = NA_real_, p_val = NA_real_,
                  n_obs = NA_integer_, r2_w = NA_real_))
  }
  ct <- as.data.frame(summary(m)$coeftable)
  if (!"main" %in% rownames(ct)) {
    return(tibble(coef = NA_real_, se = NA_real_,
                  t_stat = NA_real_, p_val = NA_real_,
                  n_obs = nobs(m), r2_w = NA_real_))
  }
  tibble(
    coef   = ct["main", "Estimate"],
    se     = ct["main", "Std. Error"],
    t_stat = ct["main", "t value"],
    p_val  = ct["main", "Pr(>|t|)"],
    n_obs  = nobs(m),
    r2_w   = fitstat(m, "wr2", simplify = TRUE)
  )
}

# ------------------------------------------------------------------------------
# 4. Loop
# ------------------------------------------------------------------------------

cat("Running ", length(outcomes_to_test), " outcomes x ",
    length(intensity_specs), " intensities x 2 control sets = ",
    length(outcomes_to_test) * length(intensity_specs) * 2,
    " cells\n", sep = "")

results <- map_dfr(outcomes_to_test, function(y) {
  map_dfr(names(intensity_specs), function(mlabel) {
    mcol <- intensity_specs[[mlabel]]
    map_dfr(c(FALSE, TRUE), function(ctrl) {
      out <- run_cell(y, mcol, ctrl)
      out %>% mutate(outcome   = y,
                     intensity = mlabel,
                     controls  = if (ctrl) "FULL" else "no")
    })
  })
})

results <- results %>%
  mutate(sig = case_when(
    is.na(p_val)  ~ "",
    p_val < 0.01  ~ "***",
    p_val < 0.05  ~ "**",
    p_val < 0.1   ~ "*",
    TRUE          ~ ""
  )) %>%
  select(outcome, intensity, controls,
         coef, se, t_stat, p_val, sig, n_obs, r2_w)

# Mean of the outcome for reference (helps interpret coef magnitude)
out_means <- panel_df %>%
  select(any_of(outcomes_to_test)) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "mean_y")

results <- results %>% left_join(out_means, by = "outcome")

# ------------------------------------------------------------------------------
# 5. Save
# ------------------------------------------------------------------------------

write.csv(results,
          "data/clean/robustness_intensity_scaling_muni.csv",
          row.names = FALSE)

cat("\nSaved: data/clean/robustness_intensity_scaling_muni.csv\n")
cat(sprintf("Total: %d rows\n", nrow(results)))

# Quick preview: stability of sign / sig across the 4 transforms,
# one outcome at a time (no-controls panel).
cat("\n=== Preview: no-controls coefficients across intensities ===\n")
print(results %>%
        filter(controls == "no") %>%
        select(outcome, intensity, coef, t_stat, sig) %>%
        mutate(coef = round(coef, 4), t_stat = round(t_stat, 2)) %>%
        pivot_wider(names_from = intensity,
                    values_from = c(coef, t_stat, sig)) %>%
        as.data.frame())
