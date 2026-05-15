# =============================================================================
# script/curated_ladder.R
#
# Cumulative ladder of specs for the curated outcomes we present in the deck.
# For each outcome × threshold × spec-level, runs `run_spec` and stacks
# the results into a single CSV (one row per estimate cell).
#
# Spec levels (cumulative; each adds one control block):
#   L1  "raw"        treatment + entity + year FE                  (no extra ctrls)
#   L2  "+ Cmig"     + year × mig_intensity trend
#   L3  "+ Cfx"      + year × fx trend
#   L4  "+ BlockA"   + year × destination-weighted baseline X  (saturated, MAIN)
#
# Thresholds: 0, 25, 50, 100 (2001 baseline migrants per muni).
#
# Categories included:
#   first_stage          (census + HH-intensive)
#   hh_investment        (census)
#   human_capital        (census + HH)
#   sector               (census)
#   occupation           (census)
#   land_use             (HH)
#   input_use            (HH; log columns derived on the fly)
#   consumption          (HH)
#
# NEC firm-side outcomes are NOT in this script -- they would require
# extending the engine to load NEC panel and NEC cross-section.  They are
# already estimated at all four spec levels for every threshold in
# docs/results.json; ask if you want a JSON-extractor too.
#
# Output:  output/tab/curated_ladder.csv  (single CSV, grouped by category)
#
# Run from repo root:  source("script/curated_ladder.R")
# =============================================================================

source("script/_specs_lib.R")

# ---- 1.  Curated outcome lists, grouped ------------------------------------
GROUPS <- list(
  first_stage = list(
    census = c("absent_hh_share"),
    hh     = c("remit_amount_intl_12m_rs","log_remit_amount_intl_12m_rs",
               "n_migrants_international","log_n_migrants_international")
  ),
  hh_investment = list(
    census = c("amen_asset_count_mean","housing_foundation_modern","amen_toilet_any")
  ),
  human_capital = list(
    census = c("edu_literate","edu_literate_female","edu_literate_male","edu_school_attend_6_16"),
    hh     = c("hlt_spend_medicines","hlt_spend_total")
  ),
  sector = list(
    census = c("ind_agri_forestry_fish","ind_manufacturing","ind_construction",
               "ind_wholesale_retail","ind_finance_real_estate_prof","ind_public_admin_defence")
  ),
  occupation = list(
    census = c("occ_share_managers","occ_share_professionals","occ_share_technicians",
               "occ_share_service_sales","occ_share_craft_trades","occ_share_machine_operators")
  ),
  land_use = list(
    hh = c("share_self_wet","share_self_dry","share_both_seasons",
           "share_fallow_wet","share_fallow_dry",
           "crop_simpson_diversity","grows_horticulture")
  ),
  input_use = list(
    hh = c("owns_plough","owns_powered_machinery","owns_irrigation_kit",
           "log_total_input_cost_rs","log_dry_cost_seed")
  ),
  consumption = list(
    hh = c("food_exp_total_7day","food_exp_protein_7day",
           "nonfood_exp_12m","nonfood_clothing_footwear_12m",
           "nonfood_fuel_lighting_12m","food_insec_score")
  )
)

# ---- 2.  Add log columns to the HH master that load_hh() doesn't already make
#         (so input_use outcomes can be regressed directly)
hh <- load_hh()
for (v in c("total_input_cost_rs","dry_cost_seed","wet_cost_seed",
            "wet_cost_fert","dry_cost_fert","wet_cost_labour","dry_cost_labour")) {
  if (v %in% names(hh) && !(paste0("log_", v) %in% names(hh)))
    hh[, (paste0("log_", v)) := log1p(pmax(get(v), 0))]
}

# ---- 3.  Spec ladder -------------------------------------------------------
LEVELS <- list(
  L1_raw     = list(label = "L1 raw treatment",
                    c_mig = FALSE, c_fx = FALSE, c_block_a = FALSE),
  L2_Cmig    = list(label = "L2 + year x mig_int",
                    c_mig = TRUE,  c_fx = FALSE, c_block_a = FALSE),
  L3_Cfx     = list(label = "L3 + year x fx",
                    c_mig = TRUE,  c_fx = TRUE,  c_block_a = FALSE),
  L4_BlockA  = list(label = "L4 + Block A (saturated, MAIN)",
                    c_mig = TRUE,  c_fx = TRUE,  c_block_a = TRUE)
)

THR <- c(0L, 25L, 50L, 100L)

run_quiet <- function(...) {
  invisible(capture.output(res <- run_spec(...), file = nullfile()))
  res
}

# ---- 4.  Loop over (category, dataset, outcome, level, threshold) -----------
rows <- list()
t_start <- Sys.time()
for (cat_name in names(GROUPS)) {
  cat_outcomes <- GROUPS[[cat_name]]
  for (ds in names(cat_outcomes)) {
    outs <- cat_outcomes[[ds]]
    for (out in outs) {
      for (lvl_key in names(LEVELS)) {
        lvl <- LEVELS[[lvl_key]]
        for (thr in THR) {
          cat(sprintf("[%s] %-12s %-32s %-25s thr=%-3d ... ",
                      cat_name, ds, out, lvl_key, thr))
          r <- run_quiet(
            spec_label = lvl_key,
            dataset    = ds,
            threshold  = thr,
            treatment  = "log_int",
            c_mig      = lvl$c_mig,
            c_fx       = lvl$c_fx,
            c_block_a  = lvl$c_block_a,
            c_block_b  = FALSE,
            c_block_c  = FALSE,
            outcomes   = list(category = out),
            save       = FALSE
          )
          if (!is.null(r)) {
            r[, `:=`(category = cat_name, spec_level = lvl_key, spec_label = lvl$label)]
            rows[[length(rows)+1]] <- r
          }
          cat("done\n")
        }
      }
    }
  }
}
cat(sprintf("\nTotal wall-clock: %.1f min\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))

# ---- 5.  Combine & save ----------------------------------------------------
combined <- rbindlist(rows, fill = TRUE)
# Order columns sensibly
setcolorder(combined, c("category","dataset","outcome","spec_level","spec_label","threshold",
                        "beta","stars","mean_y","beta_pp","pct_of_mean",
                        "se","pval","n","n_unit","n_muni","n_years",
                        "sd_y","r2_within","err"))

# Sort: category → outcome → spec_level → threshold
combined[, spec_level := factor(spec_level,
            levels = c("L1_raw","L2_Cmig","L3_Cfx","L4_BlockA"))]
combined <- combined[order(category, dataset, outcome, spec_level, threshold)]

dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(ROOT, "output/tab/curated_ladder.csv")
fwrite(combined, out_path)

cat("\n", strrep("=", 78), "\n",
    " CURATED LADDER -- ", nrow(combined), " rows\n",
    strrep("=", 78), "\n", sep = "")
cat("  saved to: ", normalizePath(out_path, winslash="/"), "\n\n", sep="")

# Quick on-screen summary: # significant at L4_BlockA per category
sumtab <- combined[spec_level == "L4_BlockA",
                   .(n_outcomes = uniqueN(outcome),
                     sig05 = sum(pval < 0.05, na.rm = TRUE),
                     sig10 = sum(pval < 0.10, na.rm = TRUE)),
                   by = .(category, threshold)]
print(sumtab[order(category, threshold)])
