# =============================================================================
# script/curated_heterogeneity_urb.R
#
# RURAL / URBAN heterogeneity for the curated HH + census outcomes that
# appear on the main slides.  Mirrors curated_heterogeneity_net.R but
# classifies munis by `lgtype` from LandCoverMatched.csv:
#   RURAL = Gaunpalika
#   URBAN = Nagarpalika + Upamahanagarpalika + Mahanagarpalika
#
# Same anchor spec, same 4 thresholds, single CSV.
#
# Run from repo root:
#   source("script/curated_heterogeneity_urb.R")
# =============================================================================

source("script/_specs_lib.R")

# ---- 1.  Build rural/urban classifier --------------------------------------
inst        <- load_instrument()
full_census <- load_census()
full_hh     <- load_hh()

land <- fread("data/clean/LandCoverMatched.csv")
urb <- unique(land[, .(lgcode, lgtype)])
urb[, sample := fcase(
  lgtype %in% c("Nagarpalika","Upamahanagarpalika","Mahanagarpalika"), "urban",
  lgtype == "Gaunpalika", "rural",
  default = NA_character_
)]
urb <- urb[!is.na(sample)]
cat(sprintf("Urban/rural lookup: %d urban, %d rural, %d total\n",
            urb[sample == "urban", .N], urb[sample == "rural", .N], nrow(urb)))

urban_munis <- urb[sample == "urban", lgcode]
rural_munis <- urb[sample == "rural", lgcode]

# ---- 2.  Same on-the-fly log columns as net script --------------------------
for (v in c("total_input_cost_rs","dry_cost_seed","wet_cost_seed",
            "wet_cost_fert","dry_cost_fert","wet_cost_labour","dry_cost_labour")) {
  if (v %in% names(full_hh) && !(paste0("log_", v) %in% names(full_hh)))
    full_hh[, (paste0("log_", v)) := log1p(pmax(get(v), 0))]
}

# ---- 3.  Outcome lists (identical to curated_heterogeneity_net.R) ----------
CENSUS_OUTCOMES <- c(
  "absent_hh_share",
  "amen_asset_count_mean", "housing_foundation_modern", "amen_toilet_any",
  "edu_literate", "edu_literate_female", "edu_school_attend_6_16",
  "ind_agri_forestry_fish", "ind_manufacturing", "ind_construction",
  "ind_wholesale_retail", "ind_finance_real_estate_prof", "ind_public_admin_defence",
  "occ_share_managers", "occ_share_professionals", "occ_share_technicians",
  "occ_share_service_sales", "occ_share_craft_trades", "occ_share_machine_operators"
)
HH_OUTCOMES <- c(
  "log_remit_amount_intl_12m_rs", "log_n_migrants_international",
  "hlt_spend_total",
  "share_self_wet", "share_self_dry", "share_both_seasons",
  "share_fallow_wet", "share_fallow_dry",
  "crop_simpson_diversity", "grows_horticulture",
  "owns_plough", "owns_powered_machinery", "owns_irrigation_kit",
  "log_total_input_cost_rs", "log_dry_cost_seed",
  "food_exp_total_7day", "food_exp_protein_7day",
  "nonfood_exp_12m", "nonfood_clothing_footwear_12m", "nonfood_fuel_lighting_12m",
  "food_insec_score"
)

# ---- 4.  Sample-injection helper -------------------------------------------
set_sample <- function(lg) {
  if (is.null(lg)) {
    .census_cache <<- full_census
    .hh_cache     <<- full_hh
  } else {
    .census_cache <<- full_census[lgcode %in% lg]
    .hh_cache     <<- full_hh[lgcode %in% lg]
  }
}

run_quiet <- function(...) {
  invisible(capture.output(res <- run_spec(...), file = nullfile()))
  res
}

# ---- 5.  Loop ---------------------------------------------------------------
SAMPLES <- list(full = NULL, rural = rural_munis, urban = urban_munis)
THR     <- c(0L, 25L, 50L, 100L)

rows <- list(); t_start <- Sys.time()
for (smp_name in names(SAMPLES)) {
  set_sample(SAMPLES[[smp_name]])
  for (thr in THR) {
    cat(sprintf("\n--- sample=%s thr=%d ---\n", smp_name, thr))
    r <- run_quiet(
      spec_label = paste0("urb_", smp_name),
      dataset    = "census", threshold = thr,
      treatment  = "log_int", c_mig = TRUE, c_fx = TRUE,
      c_block_a  = TRUE, c_block_b = FALSE, c_block_c = FALSE,
      outcomes   = list(curated = CENSUS_OUTCOMES),
      save       = FALSE
    )
    if (!is.null(r)) { r[, sample := smp_name]; rows[[length(rows)+1]] <- r }
    r <- run_quiet(
      spec_label = paste0("urb_", smp_name),
      dataset    = "hh", threshold = thr,
      treatment  = "log_int", c_mig = TRUE, c_fx = TRUE,
      c_block_a  = TRUE, c_block_b = FALSE, c_block_c = FALSE,
      outcomes   = list(curated = HH_OUTCOMES),
      save       = FALSE
    )
    if (!is.null(r)) { r[, sample := smp_name]; rows[[length(rows)+1]] <- r }
  }
}
set_sample(NULL)

cat(sprintf("\nWall-clock: %.1f min\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))

# ---- 6.  Save ---------------------------------------------------------------
out <- rbindlist(rows, fill = TRUE)
out[, sample := factor(sample, levels = c("full","rural","urban"))]
setcolorder(out, c("dataset","outcome","sample","threshold",
                   "beta","stars","mean_y","beta_pp","pct_of_mean",
                   "se","pval","n","n_unit","n_muni","n_years","sd_y","r2_within","err"))
out <- out[order(dataset, outcome, sample, threshold)]

dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(ROOT, "output/tab/curated_heterogeneity_urb.csv")
fwrite(out, out_path)
cat("\nSaved: ", normalizePath(out_path, winslash="/"), "  (", nrow(out), " rows)\n", sep = "")

cat("\n========== Rural vs Urban gap at k=25 ==========\n")
for (ds in c("census","hh")) {
  s <- out[dataset == ds & threshold == 25 & !is.na(beta)]
  if (!nrow(s)) next
  pw <- dcast(s, outcome ~ sample, value.var = c("beta","stars"))
  print(pw, nrows = 60)
}
