# =============================================================================
# script/curated_heterogeneity_net.R
#
# Net-migration heterogeneity: split municipalities into
#   RECEIVER  munis where 2001 in-migration > 2001 out-migration  (net inflow)
#   SENDER    munis where 2001 in-migration < 2001 out-migration  (net outflow)
#
# For each curated outcome (census + HH), run the preferred anchor spec
# at four thresholds across three samples (full / receiver / sender).
# Output one CSV.
#
# Run from repo root:
#   source("script/curated_heterogeneity_net.R")
# Wall-clock estimate: ~5–8 min.
# =============================================================================

source("script/_specs_lib.R")

# ---- 1.  Build net-migrant classifier (2001 baseline) ----------------------
inst        <- load_instrument()
full_census <- load_census()
full_hh     <- load_hh()

in_2001  <- full_census[year == 2001 & !is.na(mig_in_share),
                        .(lgcode, mig_in_share)]
out_2001 <- inst[year == 2001 & !is.na(mig_intensity),
                 .(lgcode, mig_intensity)]
cls <- merge(in_2001, out_2001, by = "lgcode")
cls[, net_migrant := mig_in_share - mig_intensity]
cls[, sample := fifelse(net_migrant >= 0, "receiver", "sender")]
cat(sprintf("Net-migrant classifier (2001 baseline):\n  receivers (in ≥ out): %d munis\n  senders   (in <  out): %d munis\n",
            cls[sample == "receiver", .N], cls[sample == "sender", .N]))

receivers <- cls[sample == "receiver", lgcode]
senders   <- cls[sample == "sender",   lgcode]

# ---- 2.  Add log columns the engine doesn't pre-build ---------------------
for (v in c("total_input_cost_rs","dry_cost_seed","wet_cost_seed",
            "wet_cost_fert","dry_cost_fert","wet_cost_labour","dry_cost_labour")) {
  if (v %in% names(full_hh) && !(paste0("log_", v) %in% names(full_hh)))
    full_hh[, (paste0("log_", v)) := log1p(pmax(get(v), 0))]
}

# ---- 3.  Curated outcome list (same domain as curated_ladder.R) ------------
CENSUS_OUTCOMES <- c(
  # first-stage proxy
  "absent_hh_share",
  # HH investment
  "amen_asset_count_mean", "housing_foundation_modern", "amen_toilet_any",
  # human capital
  "edu_literate", "edu_literate_female", "edu_school_attend_6_16",
  # sectors
  "ind_agri_forestry_fish", "ind_manufacturing", "ind_construction",
  "ind_wholesale_retail", "ind_finance_real_estate_prof", "ind_public_admin_defence",
  # occupations
  "occ_share_managers", "occ_share_technicians",
  "occ_share_service_sales", "occ_share_craft_trades", "occ_share_machine_operators",
  # labour
  "mlfp_all", "flfp_chores_only", "fem_share_of_ag_workers", "work_share_student"
)
HH_OUTCOMES <- c(
  # first-stage on HH side
  "remit_amount_intl_12m_rs", "log_remit_amount_intl_12m_rs",
  "n_migrants_international", "log_n_migrants_international",
  "remittance_any", "remittance_amt",
  # health spending
  "hlt_spend_medicines", "hlt_spend_total",
  # consumption
  "food_exp_total_7day", "food_exp_protein_7day",
  "nonfood_exp_12m", "nonfood_clothing_footwear_12m", "nonfood_fuel_lighting_12m",
  "food_insec_score",
  # land use & input use
  "share_self_wet", "share_self_dry", "share_both_seasons",
  "crop_simpson_diversity", "grows_horticulture",
  "log_total_input_cost_rs", "log_dry_cost_seed",
  "owns_irrigation_kit"
)

# ---- 4.  Helper: temporarily restrict the cached datasets to a sample ------
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

# ---- 5.  Loop over (sample × threshold × dataset × outcome) ----------------
SAMPLES <- list(full = NULL, receiver = receivers, sender = senders)
THR     <- c(0L, 25L, 50L, 100L)

rows <- list(); t_start <- Sys.time()
for (smp_name in names(SAMPLES)) {
  set_sample(SAMPLES[[smp_name]])
  for (thr in THR) {
    cat(sprintf("\n--- sample=%s thr=%d ---\n", smp_name, thr))
    # census
    r <- run_quiet(
      spec_label = paste0("net_", smp_name),
      dataset    = "census", threshold = thr,
      treatment  = "log_int", c_mig = TRUE, c_fx = TRUE,
      c_block_a  = TRUE, c_block_b = FALSE, c_block_c = FALSE,
      outcomes   = list(curated = CENSUS_OUTCOMES),
      save       = FALSE
    )
    if (!is.null(r)) { r[, sample := smp_name]; rows[[length(rows)+1]] <- r }
    # hh
    r <- run_quiet(
      spec_label = paste0("net_", smp_name),
      dataset    = "hh", threshold = thr,
      treatment  = "log_int", c_mig = TRUE, c_fx = TRUE,
      c_block_a  = TRUE, c_block_b = FALSE, c_block_c = FALSE,
      outcomes   = list(curated = HH_OUTCOMES),
      save       = FALSE
    )
    if (!is.null(r)) { r[, sample := smp_name]; rows[[length(rows)+1]] <- r }
  }
}
# restore full caches
set_sample(NULL)

cat(sprintf("\nWall-clock: %.1f min\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))

# ---- 6.  Save ---------------------------------------------------------------
out <- rbindlist(rows, fill = TRUE)
out[, sample := factor(sample, levels = c("full","receiver","sender"))]
setcolorder(out, c("dataset","outcome","sample","threshold",
                   "beta","stars","mean_y","beta_pp","pct_of_mean",
                   "se","pval","n","n_unit","n_muni","n_years","sd_y","r2_within","err"))
out <- out[order(dataset, outcome, sample, threshold)]

dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(ROOT, "output/tab/curated_heterogeneity_net.csv")
fwrite(out, out_path)
cat("\nSaved: ", normalizePath(out_path, winslash="/"), "  (", nrow(out), " rows)\n", sep = "")

# ---- 7.  Quick summary: receiver vs sender at k=25 (anchor) ----------------
cat("\n========== Receiver vs Sender gap (β_receiver − β_sender) at k=25 ==========\n")
for (ds in c("census","hh")) {
  s <- out[dataset == ds & threshold == 25 & !is.na(beta)]
  if (!nrow(s)) next
  pw <- dcast(s, outcome ~ sample, value.var = c("beta","stars"))
  print(pw, nrows = 60)
}
