# =============================================================================
# CENSUS-LEVEL FX SHOCK ANALYSIS  (Nepal Population Census: 2001, 2011, 2021)
# =============================================================================
# Specs:
#   S1: y_muni ~ fx_z + i(year, log_migint_z, ref=2001) | lgcode + year
#   S2: y_muni ~ fx_z + i(year, mig_int_z, ref=2001)    | lgcode + year
#   S3: y_muni ~ ssiv_z + fx_z + i(year, log_migint_z, ref=2001)
#                       + i(year, mig_int_z, ref=2001)  | lgcode + year
#
# Default FE: lgcode + year
# Standard errors clustered at lgcode (level of treatment variation).
# Reference year: 2001 (first census wave).
# =============================================================================

rm(list = ls())
cat("\14")
options(scipen = 999)

library(fixest)
library(tidyverse)
library(broom)
library(purrr)
library(tidyr)


# =========================================================
# 0. LOAD DATA + BUILD PANEL
# =========================================================

instrument     <- read.csv("data/clean/instrument/instrument_mun.csv")
census_outcome <- read.csv("data/clean/census/census_outcomes_municipality.csv")

instrument <- instrument %>%
  mutate(
    fx_z         = as.numeric(scale(avg_fx_shock_2001)),
    mig_int_z    = as.numeric(scale(migrants_per_capita_2001)),
    log_migint_z = as.numeric(scale(log(migrants_per_capita_2001 + 1e-8))),
    ssiv_z       = as.numeric(scale(ssiv_per_capita_2001)),
    ssiv_log     = as.numeric(avg_fx_shock_2001 * log_migint_z)
  )

# Census panel: one row per (lgcode, year) where year in {2001, 2011, 2021}
# Adjust the join key if your census file uses a different muni column name.
census_panel <- census_outcome %>%
  left_join(instrument, by = c("lgcode", "year"))


# -------------------------------------------------------------------------
# 1. Census outcome groups
# -------------------------------------------------------------------------
# Edit this list to match the variable names actually in your census file.
# Outcomes that don't exist in the data will be flagged and skipped.

census_outcome_groups <- list(

  Labor_Market = c(
    "work_lfp", "flfp_all", "ag_share", "nonag_share",
    "self_employed_share", "wage_employed_share"
  ),

  FLFP = c(
    "flfp_all", "flfp_ag", "flfp_nonag",
    "female_self_employed", "female_wage_employed"
  ),

  Education = c(
    "literacy_share", "literacy_male", "literacy_female",
    "school_attend_share", "secondary_complete_share",
    "tertiary_complete_share"
  ),

  Migration_LeftBehind = c(
    "absentee_share", "absentee_male_share", "absentee_female_share",
    "female_headed_share", "elderly_headed_share",
    "left_behind_children_share"
  ),

  Marriage_Fertility = c(
    "married_share", "ever_married_female",
    "child_ever_born_mean", "child_surviving_mean",
    "age_at_first_marriage_mean"
  ),

  Housing_Wealth = c(
    "owns_house_share", "permanent_house_share",
    "electricity_share", "piped_water_share",
    "toilet_share", "internet_share"
  ),

  Demographics = c(
    "hh_size_mean", "dependency_ratio",
    "elderly_share", "child_share"
  ),

  Female_Ownership = c(
    "female_owns_house", "female_owns_land", "female_owns_any"
  )
)


# -------------------------------------------------------------------------
# 2. Display function for census-level analysis
# -------------------------------------------------------------------------

display_group_census <- function(group_name,
                                 outcome_groups,
                                 panel,
                                 threshold = 0,
                                 fe_structure = "lgcode + year",
                                 cluster_var = "lgcode",
                                 ref_year = 2001,
                                 show_translated = TRUE) {

  if (!(group_name %in% names(outcome_groups))) {
    stop(sprintf("Group '%s' not found.", group_name))
  }

  if (is.null(threshold) || threshold <= 0) {
    panel_use <- panel
    sample_label <- "FULL SAMPLE (no migration threshold)"
  } else {
    panel_use <- panel %>% filter(total_migrants_2001 >= threshold)
    sample_label <- sprintf("MAIN SAMPLE (>=%d migrants in 2001)", threshold)
  }

  # Re-standardize treatments at MUNI level on working sample
  muni_z <- panel_use %>%
    distinct(lgcode, year, avg_fx_shock_2001,
             migrants_per_capita_2001, ssiv_per_capita_2001) %>%
    mutate(
      fx_z         = as.numeric(scale(avg_fx_shock_2001)),
      mig_int_z    = as.numeric(scale(migrants_per_capita_2001)),
      log_migint_z = as.numeric(scale(log(migrants_per_capita_2001 + 1e-8))),
      ssiv_z       = as.numeric(scale(ssiv_per_capita_2001))
    ) %>%
    select(lgcode, year, fx_z, mig_int_z, log_migint_z, ssiv_z)

  panel_use <- panel_use %>%
    select(-any_of(c("fx_z", "mig_int_z", "log_migint_z", "ssiv_z"))) %>%
    left_join(muni_z, by = c("lgcode", "year"))

  cat(strrep("=", 110), "\n", sep = "")
  cat(sprintf("OUTCOME GROUP: %s   |   SAMPLE: %s\n",
              toupper(group_name), sample_label))
  cat(sprintf("FE STRUCTURE: %s   |   CLUSTER: %s   |   REF YEAR: %d\n",
              fe_structure, cluster_var, ref_year))
  cat(strrep("=", 110), "\n\n", sep = "")

  cat(sprintf("Sample size: %d muni-year obs, %d unique munis, %d census waves\n\n",
              nrow(panel_use),
              n_distinct(panel_use$lgcode),
              n_distinct(panel_use$year)))

  cat("STANDARDIZATION CHECK (treatments at muni level):\n")
  cat(sprintf("  fx_z:         mean=%+.4f  sd=%.4f\n",
              mean(panel_use$fx_z, na.rm=TRUE), sd(panel_use$fx_z, na.rm=TRUE)))
  cat(sprintf("  mig_int_z:    mean=%+.4f  sd=%.4f\n",
              mean(panel_use$mig_int_z, na.rm=TRUE), sd(panel_use$mig_int_z, na.rm=TRUE)))
  cat(sprintf("  log_migint_z: mean=%+.4f  sd=%.4f\n",
              mean(panel_use$log_migint_z, na.rm=TRUE), sd(panel_use$log_migint_z, na.rm=TRUE)))
  cat(sprintf("  ssiv_z:       mean=%+.4f  sd=%.4f\n\n",
              mean(panel_use$ssiv_z, na.rm=TRUE), sd(panel_use$ssiv_z, na.rm=TRUE)))

  cat("Specifications (FE: ", fe_structure, "):\n", sep = "")
  cat(sprintf("S1: fx_z + i(year, log_migint_z, ref=%d)\n", ref_year))
  cat(sprintf("S2: fx_z + i(year, mig_int_z, ref=%d)     [linear migint - main spec]\n", ref_year))
  cat(sprintf("S3: ssiv_z + fx_z + i(year, log_migint_z, ref=%d) + i(year, mig_int_z, ref=%d)\n\n",
              ref_year, ref_year))

  outcomes  <- outcome_groups[[group_name]]
  available <- intersect(outcomes, names(panel_use))
  missing   <- setdiff(outcomes, names(panel_use))
  if (length(missing) > 0) cat("Missing from data: ", paste(missing, collapse = ", "), "\n\n")

  star <- function(b, s) {
    if (is.na(b) || is.na(s) || s == 0) return("")
    t <- abs(b/s)
    if (t > 2.58) "***" else if (t > 1.96) "**" else if (t > 1.65) "*" else ""
  }

  extract_coef <- function(model, varname) {
    if (is.null(model) || !(varname %in% names(coef(model)))) {
      return(c(NA_real_, NA_real_))
    }
    c(coef(model)[varname], se(model)[varname])
  }

  is_proportion_like <- function(x) {
    x_clean <- x[!is.na(x)]
    if (length(x_clean) == 0) return(FALSE)
    max(abs(x_clean)) <= 1.5 && sd(x_clean) <= 0.5
  }

  translate <- function(b, is_prop, out_mean) {
    if (is.na(b)) return("---")
    if (is_prop) sprintf("%.2f pp", b * 100)
    else         sprintf("%.1f%% of mean", 100 * b / out_mean)
  }

  cluster_fml <- as.formula(paste("~", cluster_var))
  rows <- list()

  for (out in available) {
    out_vec <- panel_use[[out]]
    if (sum(!is.na(out_vec)) < 50) next
    if (sd(out_vec, na.rm = TRUE) == 0) next

    out_mean <- mean(out_vec, na.rm = TRUE)
    out_sd   <- sd(out_vec, na.rm = TRUE)
    out_n    <- sum(!is.na(out_vec))
    is_prop  <- is_proportion_like(out_vec)

    fml_s1 <- as.formula(sprintf("%s ~ fx_z + i(year, log_migint_z, ref = %d) | %s",
                                  out, ref_year, fe_structure))
    fml_s2 <- as.formula(sprintf("%s ~ fx_z + i(year, mig_int_z, ref = %d) | %s",
                                  out, ref_year, fe_structure))
    fml_s3 <- as.formula(sprintf(paste0("%s ~ ssiv_z + fx_z + i(year, log_migint_z, ref = %d) ",
                                        "+ i(year, mig_int_z, ref = %d) | %s"),
                                  out, ref_year, ref_year, fe_structure))

    s1 <- tryCatch(feols(fml_s1, data = panel_use, cluster = cluster_fml), error = function(e) NULL)
    s2 <- tryCatch(feols(fml_s2, data = panel_use, cluster = cluster_fml), error = function(e) NULL)
    s3 <- tryCatch(feols(fml_s3, data = panel_use, cluster = cluster_fml), error = function(e) NULL)

    if (is.null(s1) || is.null(s2)) next

    s1_fx   <- extract_coef(s1, "fx_z")
    s2_fx   <- extract_coef(s2, "fx_z")
    s3_ssiv <- extract_coef(s3, "ssiv_z")

    # ---- Skip outcomes where all three specs returned NA coefficients ----
    if (all(is.na(c(s1_fx[1], s2_fx[1], s3_ssiv[1])))) {
      message("  Skipping ", out, " — all three coefficients are NA")
      next
    }

    fmt_raw <- function(b, se, sig) {
      if (is.na(b)) return("---")
      sprintf("%.4f%s\n(%.4f)", b, sig, se)
    }

    row <- tibble(
      outcome  = out,
      type     = if (is_prop) "prop" else "count/cont",
      mean     = sprintf("%.3f", out_mean),
      sd       = sprintf("%.3f", out_sd),
      n        = out_n,
      `S1 (fx, log)` = fmt_raw(s1_fx[1],   s1_fx[2],   star(s1_fx[1], s1_fx[2])),
      `S2 (fx, lin)` = fmt_raw(s2_fx[1],   s2_fx[2],   star(s2_fx[1], s2_fx[2])),
      `S3 (ssiv)`    = fmt_raw(s3_ssiv[1], s3_ssiv[2], star(s3_ssiv[1], s3_ssiv[2])),
      `% of SD`      = sprintf("%.1f%%", 100 * s2_fx[1] / out_sd)
    )

    if (show_translated) {
      row <- row %>%
        mutate(
          `S1 transl.` = translate(s1_fx[1],   is_prop, out_mean),
          `S2 transl.` = translate(s2_fx[1],   is_prop, out_mean),
          `S3 transl.` = translate(s3_ssiv[1], is_prop, out_mean)
        )
    }

    rows[[out]] <- row
  }

  if (length(rows) == 0) {
    cat("No valid estimates produced\n")
    return(invisible(NULL))
  }

  result_df <- bind_rows(rows)

  cat("Coefficients shown in RAW regression units (decimals).\n")
  cat("All represent the effect of a 1 SD increase in the standardized treatment.\n")
  cat("Standard errors in parentheses below each coefficient.\n")
  if (show_translated) {
    cat("'S1/S2/S3 transl.' columns: pp for binary/proportion, % of mean for continuous.\n")
  }
  cat("\n")

  print(result_df, n = Inf, width = Inf)

  cat("\n", strrep("-", 110), "\n", sep = "")
  cat("Significance: *** p<0.01, ** p<0.05, * p<0.10\n")
  cat("'% of SD' = S2 coefficient as fraction of outcome's SD\n")
  cat(strrep("-", 110), "\n", sep = "")

  invisible(result_df)
}


# =========================================================================
# USAGE
# =========================================================================

# Default: lgcode + year FE, clustered at lgcode, ref_year = 2001.

# Run for each group:
display_group_census("Labor_Market", census_outcome_groups, census_panel)
# display_group_census("FLFP",                 census_outcome_groups, census_panel)
# display_group_census("Education",            census_outcome_groups, census_panel)
# display_group_census("Migration_LeftBehind", census_outcome_groups, census_panel)
# display_group_census("Marriage_Fertility",   census_outcome_groups, census_panel)
# display_group_census("Housing_Wealth",       census_outcome_groups, census_panel)
# display_group_census("Demographics",         census_outcome_groups, census_panel)
# display_group_census("Female_Ownership",     census_outcome_groups, census_panel)

# Restrict to munis with sufficient migration:
# display_group_census("Labor_Market", census_outcome_groups, census_panel,
#                      threshold = 50)
