# =============================================================================
# Replica of script/build_results.py in R, to diagnose why locally-run
# estimate_census.R differs from the website's coefficients.
#
# Run from project root:
#   source("script/estimate/_match_python.R")
# =============================================================================

rm(list = ls()); options(scipen = 999)
library(fixest); library(tidyverse)

# ---- 1. Load instrument with INTUITIVE aliases ------------------------------
# Note: the website's build patches instrument_mun.csv to have these columns
# alongside the technical ones (shareshock_index_2001, etc.). Confirm yours
# has them — if not, pull from origin/claude/read-project-files-gVgsF.
inst <- read.csv("data/clean/instrument/instrument_mun.csv")

required_cols <- c("fxshock", "mig_intensity",
                   "fxshock_x_mig_intensity", "total_migrants")
miss <- setdiff(required_cols, names(inst))
if (length(miss)) {
  stop("Instrument is missing the new aliases: ",
       paste(miss, collapse = ", "),
       ". Re-pull, or run script/vars/instrument.R.")
}

# ---- 2. Z-score on the FULL instrument panel (all years) --------------------
inst <- inst %>%
  mutate(
    log_mig_intensity = log(mig_intensity + 1e-8),
    fxshock_z                 = as.numeric(scale(fxshock)),
    mig_intensity_z           = as.numeric(scale(mig_intensity)),
    log_mig_intensity_z       = as.numeric(scale(log_mig_intensity)),
    fxshock_x_mig_intensity_z = as.numeric(scale(fxshock_x_mig_intensity))
  )

cat("Instrument:", nrow(inst), "rows,",
    n_distinct(inst$lgcode), "munis, years",
    min(inst$year), "-", max(inst$year), "\n")
cat("  fxshock_z              : mean =", round(mean(inst$fxshock_z), 4),
    " sd =", round(sd(inst$fxshock_z), 4), "\n")
cat("  mig_intensity_z        : mean =", round(mean(inst$mig_intensity_z), 4),
    " sd =", round(sd(inst$mig_intensity_z), 4), "\n")

INST_KEEP <- c("lgcode","year","total_migrants",
               "fxshock","mig_intensity","log_mig_intensity",
               "fxshock_x_mig_intensity",
               "fxshock_z","mig_intensity_z","log_mig_intensity_z",
               "fxshock_x_mig_intensity_z")

# ---- 3. Helper: re-z-score on the working sample at MUNI-YEAR level ---------
restandardise <- function(panel) {
  muni_yr <- panel %>%
    distinct(lgcode, year, fxshock, mig_intensity,
             log_mig_intensity, fxshock_x_mig_intensity) %>%
    mutate(
      fxshock_z                 = as.numeric(scale(fxshock)),
      mig_intensity_z           = as.numeric(scale(mig_intensity)),
      log_mig_intensity_z       = as.numeric(scale(log_mig_intensity)),
      fxshock_x_mig_intensity_z = as.numeric(scale(fxshock_x_mig_intensity))
    ) %>%
    select(lgcode, year,
           fxshock_z, mig_intensity_z,
           log_mig_intensity_z, fxshock_x_mig_intensity_z)

  panel %>%
    select(-any_of(c("fxshock_z","mig_intensity_z",
                     "log_mig_intensity_z","fxshock_x_mig_intensity_z"))) %>%
    left_join(muni_yr, by = c("lgcode","year"))
}


# ---- 4. Generic fitter for one (outcome, spec) cell -------------------------
fit_cell <- function(panel, y, spec, entity, year_col, ref_year, cluster_col) {
  d <- panel %>%
    select(all_of(c(entity, year_col, y, cluster_col,
                    "fxshock_z","fxshock_x_mig_intensity_z",
                    "mig_intensity_z","log_mig_intensity_z"))) %>%
    drop_na(all_of(c(y, "fxshock_z")))

  if (nrow(d) < 50 || sd(d[[y]], na.rm = TRUE) == 0) return(NULL)

  # Build i(year, x, ref=ref_year) — only over years actually present
  yrs <- sort(unique(d[[year_col]]))
  build_i <- function(treat_col) {
    cols <- c()
    for (yy in yrs) {
      if (yy == ref_year) next
      colname <- paste0(treat_col, "_x_", yy)
      d[[colname]] <<- d[[treat_col]] * (d[[year_col]] == yy)
      cols <- c(cols, colname)
    }
    cols
  }
  log_int_cols <- if (spec %in% c("S1","S3")) build_i("log_mig_intensity_z") else c()
  lin_int_cols <- if (spec %in% c("S2","S3")) build_i("mig_intensity_z")     else c()

  if (spec == "S3") {
    rhs        <- c("fxshock_x_mig_intensity_z","fxshock_z",log_int_cols,lin_int_cols)
    report_var <- "fxshock_x_mig_intensity_z"
  } else {
    rhs        <- c("fxshock_z", log_int_cols, lin_int_cols)
    report_var <- "fxshock_z"
  }

  fml <- as.formula(paste0(
    "`", y, "` ~ ", paste(rhs, collapse = " + "),
    " | ", entity, " + ", year_col
  ))
  cluster_fml <- as.formula(paste0("~", cluster_col))

  m <- tryCatch(feols(fml, data = d, cluster = cluster_fml),
                error = function(e) NULL)
  if (is.null(m) || !(report_var %in% names(coef(m)))) return(NULL)

  list(beta   = unname(coef(m)[report_var]),
       se     = unname(se(m)[report_var]),
       pval   = unname(pvalue(m)[report_var]),
       n      = nobs(m),
       n_unit = n_distinct(d[[entity]]),
       n_muni = n_distinct(d[[cluster_col]]),
       mean_y = mean(d[[y]], na.rm = TRUE),
       sd_y   = sd(d[[y]],   na.rm = TRUE))
}


# ---- 5. Wrapper: run one (dataset, spec, threshold) over a vector of ys -----
run_one_set <- function(panel, ys, spec, threshold, entity, year_col,
                        ref_year, cluster_col) {
  sub <- if (threshold > 0) panel %>% filter(total_migrants >= threshold) else panel
  cat("\n### spec =", spec, "  threshold =", threshold,
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
    cat(sprintf("  %-30s  beta=%+.4f%-3s  se=%.4f  mean=%.3f  N=%d  n_unit=%d  pct_sd=%+.1f%%\n",
                y, r$beta, sig, r$se, r$mean_y, r$n, r$n_unit, pct_sd))
  }
}


# =============================================================================
# 6. CENSUS validation — Assets / Industry / Occupation
# =============================================================================
cen <- read.csv("data/clean/census/census_outcomes_municipality.csv") %>%
  inner_join(inst[, INST_KEEP], by = c("lgcode","year"))
cat("\n=== CENSUS panel:", nrow(cen), "rows,",
    n_distinct(cen$lgcode), "munis, years",
    paste(sort(unique(cen$year)), collapse = ","), "===\n")

CENSUS_YS <- c(
  # Assets
  "amen_assets_radio","amen_assets_tv","amen_assets_cycle",
  "amen_assets_motorcycle","amen_assets_car","amen_assets_fridge",
  "amen_assets_landline","amen_assets_mobile","amen_assets_computer",
  "amen_assets_internet","amen_assets_none","amen_asset_count_mean",
  # Industry
  "ind_agri_forestry_fish","ind_manufacturing","ind_construction",
  "ind_wholesale_retail","ind_transport_accommodation",
  "ind_finance_real_estate_prof","ind_public_admin_defence",
  "ind_education","ind_health","ind_arts_recreation","ind_others",
  # Occupation
  "occ_share_armed_forces","occ_share_managers","occ_share_professionals",
  "occ_share_technicians","occ_share_office_assistants",
  "occ_share_service_sales","occ_share_agriculture",
  "occ_share_craft_trades","occ_share_machine_operators",
  "occ_share_elementary"
)

for (s in c("S1","S2","S3")) {
  for (thr in c(0, 25, 50, 100)) {
    run_one_set(cen, CENSUS_YS, spec = s, threshold = thr,
                entity = "lgcode", year_col = "year",
                ref_year = 2001, cluster_col = "lgcode")
  }
}


# =============================================================================
# 7. HOUSEHOLD validation — Land Portfolio / Land Use / Crop Choice
# =============================================================================
hh <- read.csv("data/clean/rvs_outcomes/agriculture_hh_year.csv") %>%
  rename(lgcode = vmun_code) %>%
  inner_join(inst[, INST_KEEP], by = c("lgcode","year"))
cat("\n=== HH panel:", nrow(hh), "rows,",
    n_distinct(hh$hhid), "HHs,",
    n_distinct(hh$lgcode), "munis, years",
    paste(sort(unique(hh$year)), collapse = ","), "===\n")

HH_YS <- c(
  "agro_hh","n_plots_owned","total_owned_area_sqm",
  "cultivated_area_sqm","cultivated_area_total_sqm","rented_in_area_sqm",
  "share_self_wet","share_rented_out_wet","share_fallow_wet",
  "share_self_dry","share_fallow_dry","share_both_seasons",
  "n_crops_total","n_crops_wet","n_crops_dry","multi_season",
  "grows_staple","grows_cashcrop","grows_horticulture"
)

for (s in c("S1","S2","S3")) {
  for (thr in c(0, 25, 50, 100)) {
    run_one_set(hh, HH_YS, spec = s, threshold = thr,
                entity = "hhid", year_col = "year",
                ref_year = 2016, cluster_col = "lgcode")
  }
}
