# =============================================================================
# PLOT-LEVEL FX SHOCK ANALYSIS  (NRVS plot-year panel: 2016, 2017, 2018)
# =============================================================================
# Specs:
#   S1: y_plot ~ fx_z + i(year, log_migint_z, ref=2016) | <FE>
#   S2: y_plot ~ fx_z + i(year, mig_int_z, ref=2016)    | <FE>
#   S3: y_plot ~ ssiv_z + fx_z + i(year, log_migint_z, ref=2016)
#                       + i(year, mig_int_z, ref=2016) | <FE>
#
# Default FE: lgcode + year  (override with fe_structure = "hhid + year" or
# "plotid + year" for tighter absorption).
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

instrument           <- read.csv("data/clean/instrument/instrument_mun.csv")
rvs_outcome_agroplot <- read.csv("data/clean/rvs_outcomes/agriculture_plot_year.csv")

instrument <- instrument %>%
  mutate(
    fx_z         = as.numeric(scale(avg_fx_shock_2001)),
    mig_int_z    = as.numeric(scale(migrants_per_capita_2001)),
    log_migint_z = as.numeric(scale(log(migrants_per_capita_2001 + 1e-8))),
    ssiv_z       = as.numeric(scale(ssiv_per_capita_2001)),
    ssiv_log     = as.numeric(avg_fx_shock_2001 * log_migint_z)
  )

agro_plot_panel <- rvs_outcome_agroplot %>%
  rename(lgcode = vmun_code) %>%
  left_join(instrument, by = c("lgcode", "year"))


# -------------------------------------------------------------------------
# 1. Build derived plot outcomes
# -------------------------------------------------------------------------

prepare_plot_outcomes <- function(panel) {
  panel %>%
    mutate(
      log_area_total = log(area_sqm + 1),

      any_modern_irrigation = pmax(
        wet_irr_surface, wet_irr_groundwater,
        dry_irr_surface, dry_irr_groundwater,
        na.rm = TRUE
      ),
      any_modern_irrigation = ifelse(is.infinite(any_modern_irrigation),
                                     NA_real_, any_modern_irrigation),

      only_rainfed = as.numeric(
        (wet_irr_rainfed == 1 | dry_irr_rainfed == 1) &
          wet_irr_surface == 0 & wet_irr_groundwater == 0 &
          dry_irr_surface == 0 & dry_irr_groundwater == 0
      ),

      any_fallow = pmax(wet_fallow, dry_fallow, na.rm = TRUE),
      any_fallow = ifelse(is.infinite(any_fallow), NA_real_, any_fallow),

      any_rented_out = pmax(wet_rented_out, dry_rented_out, na.rm = TRUE),
      any_rented_out = ifelse(is.infinite(any_rented_out), NA_real_, any_rented_out),

      any_self_cultivated = pmax(wet_self_cultivated, dry_self_cultivated, na.rm = TRUE),
      any_self_cultivated = ifelse(is.infinite(any_self_cultivated),
                                   NA_real_, any_self_cultivated)
    )
}


# -------------------------------------------------------------------------
# 2. Outcome groups
# -------------------------------------------------------------------------

plot_outcome_groups <- list(

  Land_Use_All = c(
    "wet_self_cultivated", "wet_rented_out", "wet_fallow",
    "dry_self_cultivated", "dry_rented_out", "dry_fallow"
  ),

  Land_Use_Wet = c(
    "wet_self_cultivated", "wet_rented_out", "wet_fallow", "wet_other_use"
  ),

  Land_Use_Dry = c(
    "dry_self_cultivated", "dry_rented_out", "dry_fallow", "dry_other_use"
  ),

  Land_Use_Combined = c(
    "any_self_cultivated", "any_rented_out", "any_fallow"
  ),

  Irrigation_Wet = c(
    "wet_irr_surface", "wet_irr_groundwater", "wet_irr_rainfed"
  ),

  Irrigation_Dry = c(
    "dry_irr_surface", "dry_irr_groundwater", "dry_irr_rainfed"
  ),

  Irrigation_Combined = c(
    "any_modern_irrigation", "only_rainfed"
  ),

  Plot_Size = c(
    "log_area_total", "area_sqm"
  )
)


# -------------------------------------------------------------------------
# 3. Display function for plot-level analysis
# -------------------------------------------------------------------------

display_group_plot <- function(group_name,
                               outcome_groups,
                               panel,
                               threshold = 0,
                               fe_structure = "lgcode + year",
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

  cat(sprintf("Sample size: %d plot-year obs, %d unique plots, %d unique HH, %d munis\n\n",
              nrow(panel_use),
              n_distinct(panel_use$plotid),
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
  cat("Binary outcomes interpreted as Linear Probability Models (LPM):\n")
  cat("   coefficient = change in probability that plot has [feature].\n")
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

# 1. Build outcomes (run once)
agro_plot_panel <- prepare_plot_outcomes(agro_plot_panel)

# 2. Run for each group
# display_group_plot("Land_Use_All",        plot_outcome_groups, agro_plot_panel)
# display_group_plot("Land_Use_Combined",   plot_outcome_groups, agro_plot_panel)
# display_group_plot("Irrigation_Wet",      plot_outcome_groups, agro_plot_panel)
# display_group_plot("Irrigation_Dry",      plot_outcome_groups, agro_plot_panel)
# display_group_plot("Irrigation_Combined", plot_outcome_groups, agro_plot_panel)
# display_group_plot("Plot_Size",           plot_outcome_groups, agro_plot_panel)

# 3. Different FE structures to compare:
#  - lgcode + year  (DEFAULT — simplest, pools all plots in muni)
#  - hhid + year    (drops between-plot-within-HH variation)
#  - plotid + year  (most controlled — within-plot over time)

# Default run
display_group_plot("Land_Use_All", plot_outcome_groups, agro_plot_panel)

# Tighter FE
# display_group_plot("Land_Use_All", plot_outcome_groups, agro_plot_panel,
#                    fe_structure = "hhid + year")
# display_group_plot("Land_Use_All", plot_outcome_groups, agro_plot_panel,
#                    fe_structure = "plotid + year")
