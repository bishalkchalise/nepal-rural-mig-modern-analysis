# =============================================================================
# Replica of script/build_results.py in R, to diagnose website vs local
# coefficient differences.
#
# UPDATED specs (interaction-based — coefficient of interest in [brackets]):
#   S1: y ~ fx_z + [fx_z:mig_int_z]                                          | FE
#       (interaction with linear migint, NO year × migint control)
#   S2: y ~ fx_z + [fx_z:log_migint_z] + i(year, mig_int_z, ref)             | FE   ← MAIN
#   S3: y ~ fx_z + [fx_z:mig_int_z]    + i(year, log_migint_z, ref)          | FE
#
# Run from project root:
#   source("script/estimate/_match_python.R")
# =============================================================================

rm(list = ls()); options(scipen = 999)
library(fixest); library(tidyverse)

# ---- 1. Load instrument; resolve column names flexibly ---------------------
inst_raw <- read.csv("data/clean/instrument/instrument_mun.csv")

resolve_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) stop("Missing all of: ", paste(candidates, collapse = ", "))
  hit[1]
}
src <- list(
  fxshock        = resolve_col(inst_raw, c("fxshock","avg_fx_shock_2001","shareshock_index_2001")),
  mig_intensity  = resolve_col(inst_raw, c("mig_intensity","migrants_per_capita_2001","geog_intensity_2001")),
  total_migrants = resolve_col(inst_raw, c("total_migrants","total_migrants_2001","geog_total_mig_2001"))
)
inst <- inst_raw %>% transmute(
  lgcode = lgcode, year = year,
  fxshock        = .data[[src$fxshock]],
  mig_intensity  = .data[[src$mig_intensity]],
  total_migrants = .data[[src$total_migrants]],
  log_mig_intensity = log(mig_intensity + 1e-8)
)
cat("Instrument cols mapped from:\n")
for (k in names(src)) cat("  ", k, "<-", src[[k]], "\n")

# ---- 2. Z-score on FULL instrument panel ------------------------------------
inst <- inst %>%
  mutate(
    fx_z         = as.numeric(scale(fxshock)),
    mig_int_z    = as.numeric(scale(mig_intensity)),
    log_migint_z = as.numeric(scale(log_mig_intensity))
  )

# ---- 3. Re-standardise on the WORKING sample (muni-year level) -------------
restandardise <- function(panel) {
  muni_yr <- panel %>%
    distinct(lgcode, year, fxshock, mig_intensity, log_mig_intensity) %>%
    mutate(
      fx_z         = as.numeric(scale(fxshock)),
      mig_int_z    = as.numeric(scale(mig_intensity)),
      log_migint_z = as.numeric(scale(log_mig_intensity))
    ) %>%
    select(lgcode, year, fx_z, mig_int_z, log_migint_z)
  panel %>%
    select(-any_of(c("fx_z","mig_int_z","log_migint_z"))) %>%
    left_join(muni_yr, by = c("lgcode","year"))
}

# ---- 4. Fit one cell (matches Python fit_one) ------------------------------
fit_cell <- function(panel, y, spec, entity, year_col, ref_year, cluster_col) {
  d <- panel %>%
    select(all_of(c(entity, year_col, y, cluster_col,
                    "fx_z","mig_int_z","log_migint_z"))) %>%
    drop_na(all_of(c(y, "fx_z")))
  if (nrow(d) < 50 || sd(d[[y]], na.rm = TRUE) == 0) return(NULL)

  if (spec == "S1") {
    fml <- as.formula(sprintf("`%s` ~ fx_z + fx_z:mig_int_z | %s + %s",
                              y, entity, year_col))
    report <- "fx_z:mig_int_z"
  } else if (spec == "S2") {
    fml <- as.formula(sprintf(
      "`%s` ~ fx_z + fx_z:log_migint_z + i(year, mig_int_z, ref = %d) | %s + %s",
      y, ref_year, entity, year_col))
    report <- "fx_z:log_migint_z"
  } else if (spec == "S3") {
    fml <- as.formula(sprintf(
      "`%s` ~ fx_z + fx_z:mig_int_z + i(year, log_migint_z, ref = %d) | %s + %s",
      y, ref_year, entity, year_col))
    report <- "fx_z:mig_int_z"
  } else stop("unknown spec ", spec)

  cluster_fml <- as.formula(paste0("~", cluster_col))
  m <- tryCatch(feols(fml, data = d, cluster = cluster_fml),
                error = function(e) NULL)
  if (is.null(m) || !(report %in% names(coef(m)))) return(NULL)

  list(beta   = unname(coef(m)[report]),
       se     = unname(se(m)[report]),
       pval   = unname(pvalue(m)[report]),
       n      = nobs(m),
       n_unit = n_distinct(d[[entity]]),
       n_muni = n_distinct(d[[cluster_col]]),
       mean_y = mean(d[[y]], na.rm = TRUE),
       sd_y   = sd(d[[y]],   na.rm = TRUE))
}

# ---- 5. Wrapper -------------------------------------------------------------
run_set <- function(panel, ys, spec, threshold, entity, year_col, ref_year, cluster_col) {
  sub <- if (threshold > 0) panel %>% filter(total_migrants >= threshold) else panel
  cat("\n### spec =", spec, "  thr =", threshold,
      "  N =", nrow(sub),
      "  n_unit =", n_distinct(sub[[entity]]),
      "  n_muni =", n_distinct(sub[[cluster_col]]), "\n")
  sub <- restandardise(sub)
  for (y in ys) {
    if (!(y %in% names(sub))) { cat("  ", y, ": missing\n"); next }
    r <- fit_cell(sub, y, spec, entity, year_col, ref_year, cluster_col)
    if (is.null(r)) { cat("  ", y, ": NULL\n"); next }
    pct_sd <- if (r$sd_y > 0) 100 * r$beta / r$sd_y else NA
    sig <- ifelse(r$pval < 0.01, "***",
           ifelse(r$pval < 0.05, "**",
           ifelse(r$pval < 0.10, "*", "")))
    cat(sprintf("  %-32s  beta=%+.4f%-3s  se=%.4f  mean=%.3f  N=%d  n_unit=%d  pct_sd=%+.1f%%\n",
                y, r$beta, sig, r$se, r$mean_y, r$n, r$n_unit, pct_sd))
  }
}

# ============================================================================
# 6. CENSUS — Assets / Industry / Occupation (validation subset)
# ============================================================================
cen <- read.csv("data/clean/census/census_outcomes_municipality.csv") %>%
  inner_join(inst, by = c("lgcode","year"))

CENSUS_YS <- c(
  "amen_assets_radio","amen_assets_tv","amen_assets_cycle",
  "amen_assets_motorcycle","amen_assets_car","amen_assets_fridge",
  "amen_assets_landline","amen_assets_mobile","amen_assets_computer",
  "amen_assets_internet","amen_assets_none","amen_asset_count_mean",
  "ind_agri_forestry_fish","ind_manufacturing","ind_construction",
  "ind_wholesale_retail","ind_transport_accommodation",
  "ind_finance_real_estate_prof","ind_public_admin_defence",
  "ind_education","ind_health","ind_arts_recreation","ind_others",
  "occ_share_armed_forces","occ_share_managers","occ_share_professionals",
  "occ_share_technicians","occ_share_office_assistants",
  "occ_share_service_sales","occ_share_agriculture",
  "occ_share_craft_trades","occ_share_machine_operators",
  "occ_share_elementary"
)
for (s in c("S1","S2","S3")) for (thr in c(0, 25, 50, 100))
  run_set(cen, CENSUS_YS, s, thr, "lgcode", "year", 2001, "lgcode")


# ============================================================================
# 7. HOUSEHOLD — agriculture
# ============================================================================
hh <- read.csv("data/clean/rvs_outcomes/agriculture_hh_year.csv") %>%
  rename(lgcode = vmun_code) %>%
  inner_join(inst, by = c("lgcode","year"))

HH_YS <- c(
  "agro_hh","n_plots_owned","total_owned_area_sqm",
  "cultivated_area_sqm","cultivated_area_total_sqm","rented_in_area_sqm",
  "share_self_wet","share_rented_out_wet","share_fallow_wet",
  "share_self_dry","share_fallow_dry","share_both_seasons",
  "n_crops_total","n_crops_wet","n_crops_dry","multi_season",
  "grows_staple","grows_cashcrop","grows_horticulture"
)
for (s in c("S1","S2","S3")) for (thr in c(0, 25, 50, 100))
  run_set(hh, HH_YS, s, thr, "hhid", "year", 2016, "lgcode")
