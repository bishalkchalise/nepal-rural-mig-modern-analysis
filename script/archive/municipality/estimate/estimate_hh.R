# =============================================================================
# HOUSEHOLD-LEVEL FX SHOCK ANALYSIS  (NRVS HH-year panel: 2016, 2017, 2018)
# =============================================================================
# Specs:
#   S1: y_hh ~ fx_z + i(year, log_migint_z, ref=2016) | hhid + year
#   S2: y_hh ~ fx_z + i(year, mig_int_z, ref=2016)    | hhid + year
#   S3: y_hh ~ ssiv_z + fx_z + i(year, log_migint_z, ref=2016)
#                     + i(year, mig_int_z, ref=2016)  | hhid + year
#
# Default FE: hhid + year  (HH-FE absorb all time-invariant HH characteristics)
# Standard errors clustered at lgcode (level of treatment variation).
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

instrument         <- read.csv("data/clean/instrument/instrument_mun.csv")
rvs_outcome_agrohh <- read.csv("data/clean/rvs_outcomes/agriculture_hh_year.csv")

instrument <- instrument %>%
  mutate(
    fx_z         = as.numeric(scale(avg_fx_shock_2001)),
    mig_int_z    = as.numeric(scale(migrants_per_capita_2001)),
    log_migint_z = as.numeric(scale(log(migrants_per_capita_2001 + 1e-8))),
    ssiv_z       = as.numeric(scale(ssiv_per_capita_2001)),
    ssiv_log     = as.numeric(avg_fx_shock_2001 * log_migint_z)
  )

agro_hh_panel <- rvs_outcome_agrohh %>%
  rename(lgcode = vmun_code) %>%
  left_join(instrument, by = c("lgcode", "year"))


# -------------------------------------------------------------------------
# 1. HH-level outcome groups
# -------------------------------------------------------------------------

hh_outcome_groups <- list(

  Land_Portfolio = c(
    "agro_hh", "n_plots_owned", "total_owned_area_sqm",
    "cultivated_area_sqm", "cultivated_area_total_sqm", "rented_in_area_sqm"
  ),

  Land_Use_HH = c(
    "share_self_wet", "share_rented_out_wet", "share_fallow_wet",
    "share_self_dry", "share_fallow_dry", "share_both_seasons"
  ),

  Irrigation_HH = c(
    "share_irr_surface_wet", "share_irr_groundwater_wet", "share_irr_rainfed_wet",
    "share_irr_surface_dry", "share_irr_groundwater_dry", "share_irr_rainfed_dry"
  ),

  Land_Market = c(
    "land_sold_any", "land_bought_any",
    "land_sold_12m_rs", "land_bought_12m_rs"
  ),

  Input_Use_Wet = c(
    "wet_used_seed", "wet_used_fert", "wet_used_insect",
    "wet_used_equip", "wet_used_labour"
  ),

  Input_Use_Dry = c(
    "dry_used_seed", "dry_used_fert", "dry_used_insect",
    "dry_used_equip", "dry_used_labour"
  ),

  Input_Costs = c(
    "wet_cost_fert", "wet_cost_insect", "wet_cost_equip", "wet_cost_labour",
    "dry_cost_fert", "dry_cost_labour",
    "total_input_cost_rs", "input_intensity_per_sqm"
  ),

  Crop_Choice = c(
    "n_crops_total", "n_crops_wet", "n_crops_dry", "multi_season",
    "grows_staple", "grows_cashcrop", "grows_horticulture"
  ),

  Crop_Specialisation = c(
    "crop_hhi", "crop_simpson_diversity", "effective_n_crops",
    "staple_value_share", "cashcrop_value_share", "horti_value_share"
  ),

  Crop_Market = c(
    "any_crop_sold", "crop_sale_rs_12m", "crop_sale_share"
  ),

  Equipment = c(
    "owns_plough", "owns_powered_machinery", "owns_irrigation_kit",
    "owns_transport", "owns_storage_struct",
    "n_equip_categories", "equip_stock_value_rs"
  ),

  Livestock = c("livestock_has")
)


# -------------------------------------------------------------------------
# 2. Display function for HH-level analysis
# -------------------------------------------------------------------------

display_group_hh <- function(group_name,
                             outcome_groups,
                             panel,
                             threshold = 0,
                             fe_structure = "hhid + year",   # locked default
                             cluster_var = "lgcode",
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
  cat(sprintf("FE STRUCTURE: %s   |   CLUSTER: %s\n", fe_structure, cluster_var))
  cat(strrep("=", 110), "\n\n", sep = "")

  cat(sprintf("Sample size: %d HH-year obs, %d unique HH, %d munis\n\n",
              nrow(panel_use),
              n_distinct(panel_use$hhid),
              n_distinct(panel_use$lgcode)))

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
  cat("S1: fx_z + i(year, log_migint_z, ref=2016)\n")
  cat("S2: fx_z + i(year, mig_int_z, ref=2016)     [linear migint - main spec]\n")
  cat("S3: ssiv_z + fx_z + i(year, log_migint_z, ref=2016) + i(year, mig_int_z, ref=2016)\n\n")

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

    fml_s1 <- as.formula(sprintf("%s ~ fx_z + i(year, log_migint_z, ref = 2016) | %s",
                                  out, fe_structure))
    fml_s2 <- as.formula(sprintf("%s ~ fx_z + i(year, mig_int_z, ref = 2016) | %s",
                                  out, fe_structure))
    fml_s3 <- as.formula(sprintf(paste0("%s ~ ssiv_z + fx_z + i(year, log_migint_z, ref = 2016) ",
                                        "+ i(year, mig_int_z, ref = 2016) | %s"),
                                  out, fe_structure))

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

# Default FE is hhid + year (do not need to override).
# Standard errors clustered at lgcode (level of treatment variation).

# Run for each group:
display_group_hh("Land_Portfolio",       hh_outcome_groups, agro_hh_panel)
# display_group_hh("Land_Use_HH",          hh_outcome_groups, agro_hh_panel)
# display_group_hh("Irrigation_HH",        hh_outcome_groups, agro_hh_panel)
# display_group_hh("Land_Market",          hh_outcome_groups, agro_hh_panel)
# display_group_hh("Input_Use_Wet",        hh_outcome_groups, agro_hh_panel)
# display_group_hh("Input_Use_Dry",        hh_outcome_groups, agro_hh_panel)
# display_group_hh("Input_Costs",          hh_outcome_groups, agro_hh_panel)
# display_group_hh("Crop_Choice",          hh_outcome_groups, agro_hh_panel)
# display_group_hh("Crop_Specialisation",  hh_outcome_groups, agro_hh_panel)
# display_group_hh("Crop_Market",          hh_outcome_groups, agro_hh_panel)
# display_group_hh("Equipment",            hh_outcome_groups, agro_hh_panel)
# display_group_hh("Livestock",            hh_outcome_groups, agro_hh_panel)

# Override FE if needed (NOT recommended for HH analysis):
# display_group_hh("Land_Portfolio", hh_outcome_groups, agro_hh_panel,
#                  fe_structure = "lgcode + year")
