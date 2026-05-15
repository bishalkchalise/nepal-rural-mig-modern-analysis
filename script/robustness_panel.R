# =============================================================================
# script/robustness_panel.R
#
# Companion to script/robustness_nec.R — extends the census + HH
# robustness grid to the same 33-spec layout (3 scales × 11 lags 0–10).
#
# The existing robustness_final.csv + robustness_final_fill.csv already
# cover 21 of the 33 (lag 0/1/2/3/4/5/10 × 3 scales) cells.  This script
# fills the missing 12 cells: lag 6, 7, 8, 9 for log/lin, log/log, lin/lin.
#
# Datasets:
#   census (3 census waves: 2001, 2011, 2021)
#   hh     (HRVS HH-year, 2016–2018)
# Thresholds:  k = 0, 25, 50, 100
#
# Output: output/tab/robustness_panel.csv
# Wall-clock: ~2–3 hours (HH is the slow side; census is fast).
# Crash-tolerant: incremental save after each spec × threshold.
#
# After the run, the JSON builder merges this CSV with the existing
# robustness_final.csv + robustness_final_fill.csv automatically.
# =============================================================================

suppressPackageStartupMessages({
  library(fixest); library(data.table)
})
source("script/_specs_lib.R")
options(scipen = 999); setDTthreads(0)
ROOT <- normalizePath(".")

# -----------------------------------------------------------------------------
# 1. Spec list — the 12 cells that complete the 33-spec annual-lag grid.
# -----------------------------------------------------------------------------
SCALES <- list(
  log_lin = list(treatment = "log_int", c_mig_log = FALSE, label = "log/lin"),
  log_log = list(treatment = "log_int", c_mig_log = TRUE,  label = "log/log"),
  lin_lin = list(treatment = "lin_int", c_mig_log = FALSE, label = "lin/lin")
)
LAGS_FILL <- c(6L, 7L, 8L, 9L)
THRESHOLDS <- c(0L, 25L, 50L, 100L)

SPECS <- list()
for (sk in names(SCALES)) {
  scfg <- SCALES[[sk]]
  for (L in LAGS_FILL) {
    nm <- sprintf("S_%s_lag%d", sk, L)
    SPECS[[nm]] <- list(
      treatment = scfg$treatment,
      c_mig_log = scfg$c_mig_log,
      lag       = as.integer(L),
      scale     = scfg$label
    )
  }
}

# -----------------------------------------------------------------------------
# 2. Outcome catalogues
# -----------------------------------------------------------------------------
build_outcomes <- function() {
  cen <- load_census(); hh <- load_hh()
  cen_id <- c("lgcode","year","district","district77","district_name")
  cen_num <- setdiff(names(cen)[sapply(cen, is.numeric)], cen_id)
  hh_id <- c("hhid","year","lgcode","district","district77","district_name",
             "wt_hh","psu","vdc","vmun_code","s00q03a","s00q03b","s00q03c",
             "member_id","fxshock","mig_intensity","log_mig_intensity",
             "total_migrants","fx_z","mig_int_z","log_migint_z")
  hh_num <- setdiff(names(hh)[sapply(hh, is.numeric)], hh_id)
  list(census = cen_num, hh = hh_num)
}

stars_fn <- function(p) fifelse(is.na(p), "",
                                 fifelse(p < .01, "***",
                                 fifelse(p < .05, "**",
                                 fifelse(p < .10, "*", ""))))
run_quiet <- function(...) {
  invisible(capture.output(res <- run_spec(...), file = nullfile()))
  res
}

# -----------------------------------------------------------------------------
# 3. Group + interpretation helpers (same logic as robustness_final.R)
# -----------------------------------------------------------------------------
classify_outcome <- function(o) {
  fcase(
    o %in% c("absent_hh_share","mig_in_share"), "First stage (migrant share)",
    grepl("^log_(remit|n_migrants)", o), "First stage (remit / migrants, log)",
    o %in% c("remittance_any","remittance_amt","remit_amount_12m_rs","remit_amount_intl_12m_rs",
             "n_migrants_total","n_migrants_international","n_migrants_internal",
             "n_migrants_male","n_migrants_female","share_male_migrants"), "Migration / remittance",
    grepl("^mig_in_", o), "Internal migration",
    grepl("^remit_use_", o), "Remittance use",
    grepl("^amen_", o), "Assets / amenities",
    grepl("^housing_", o), "Housing",
    grepl("^edu_", o), "Education",
    grepl("^hlt_", o), "Health spending",
    grepl("^ind_", o), "Industry employment (census)",
    grepl("^occ_", o), "Occupation",
    grepl("^mar_", o), "Marriage",
    grepl("^fert_", o), "Fertility",
    grepl("^(mlfp|flfp|^lfp|work_|gap_lfp)", o), "Labour force",
    grepl("(share_self|share_fallow|share_both|crop_|grows_horti|n_crops|effective_n_crops|horti_value_share|cashcrop_value_share|share_irr|share_rented|n_plots_rented|land_sold|any_crop_sold|crop_sale)", o), "Land use / cropping",
    grepl("^owns_|cost_seed|cost_fert|cost_labour|cost_insect|cost_equip|input_cost|total_input", o), "Input use / capital",
    grepl("(livestock|effective_n_animals)", o), "Livestock",
    grepl("^(food_exp|food_insec|nonfood_)", o), "Consumption",
    grepl("^(any_health|hh_health)", o), "Health utilisation",
    grepl("^(private_|public_|support_|disaster|insurance|safety_net|social_protection|cash_transfer|relief)", o), "Social protection",
    grepl("^(shock|coping|severe|multiple_shocks|n_shocks)", o), "Shocks & coping",
    grepl("^(enterprise|biz_|business_)", o), "Household enterprise",
    grepl("^(left_|n_left|child_|caregiver)", o), "Family structure",
    default = "Other"
  )
}
detect_scale <- function(o, m) {
  if (grepl("^log_|_log$", o)) return("log")
  if (grepl("(_rs$|_rs_|_amt$|spend_|food_exp_|nonfood_|cost_per_|^cap_|crop_sale_rs|earning_total_rs|cost_12m_rs|land_sold_12m_rs)", o)) return("rs")
  if (!is.na(m) && abs(m) <= 1 && m >= 0) return("share")
  if (grepl("(diversity|hhi|index|score|n_industries)", o)) return("index")
  if (grepl("(^n_|count|firms_|emp_total|^new_firms)", o)) return("count")
  "other"
}
interpret_beta <- function(b, o, m) {
  if (is.na(b)) return("")
  scale <- detect_scale(o, m)
  pct <- function() {
    if (is.na(m) || abs(m) < 1e-9) return("")
    sprintf(" (%+.0f%% of mean)", 100*b/m)
  }
  if (scale == "log") sprintf("%+.1f%% change (log outcome)", b*100)
  else if (scale == "rs") sprintf("%+s Rs%s", formatC(round(b), format="d", big.mark=","), pct())
  else if (scale == "share") sprintf("%+.2fpp%s", b*100, pct())
  else if (scale == "index") sprintf("%+.3f index pts (baseline %.3f)", b, m)
  else if (scale == "count") sprintf("%+.3f units%s", b, pct())
  else                       sprintf("%+.3f (baseline %.3f)", b, m)
}

# -----------------------------------------------------------------------------
# 4. Loop with incremental save
# -----------------------------------------------------------------------------
outcs <- build_outcomes()
out_path <- file.path(ROOT, "output/tab/robustness_panel.csv")
dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
all_rows <- list(); t_start <- Sys.time()

cat("========== robustness_panel (fill lags 6/7/8/9 × 3 scales) ==========\n")
cat(sprintf("Specs:      %d (3 scales × lags 6,7,8,9)\n", length(SPECS)))
cat(sprintf("Thresholds: %s\n", paste(THRESHOLDS, collapse=", ")))
cat(sprintf("Outcomes:   census=%d  hh=%d\n",
            length(outcs$census), length(outcs$hh)))
total <- length(SPECS) * length(THRESHOLDS) * (length(outcs$census) + length(outcs$hh))
cat(sprintf("Total cells: %d\n\n", total))

save_partial <- function() {
  d <- rbindlist(all_rows, fill = TRUE)
  for (col in c("beta","se","pval","mean_y","sd_y","n","n_muni","n_unit"))
    if (!col %in% names(d)) d[, (col) := NA_real_]
  if (!"stars" %in% names(d)) d[, stars := ""]
  if (!"err"   %in% names(d)) d[, err   := ""]
  d[, outcome_group := classify_outcome(outcome)]
  d[, interpret     := mapply(interpret_beta, beta, outcome, mean_y)]
  KEEP <- c("outcome_group","dataset","outcome","spec","threshold","lag",
            "treatment_kind","c_mig_log_flag","scale_form",
            "beta","stars","se","pval","mean_y","sd_y","n","n_muni","n_unit","interpret","err")
  d <- d[, intersect(KEEP, names(d)), with = FALSE]
  d[, spec := factor(spec, levels = names(SPECS))]
  fwrite(d[order(outcome_group, dataset, outcome, spec, threshold)], out_path)
}

for (spec_name in names(SPECS)) {
  cfg <- SPECS[[spec_name]]
  for (thr in THRESHOLDS) {
    cat(sprintf("--- %s @ k=%d (treatment=%s, lag=%d, c_mig_log=%s, scale=%s) ---\n",
                spec_name, thr, cfg$treatment, cfg$lag, cfg$c_mig_log, cfg$scale))

    # census via run_spec
    r <- run_quiet(spec_label = spec_name, dataset = "census", threshold = thr,
                   treatment = cfg$treatment, c_mig = TRUE,
                   c_mig_log = cfg$c_mig_log, c_fx = TRUE,
                   c_block_a = TRUE, c_block_b = FALSE, c_block_c = FALSE,
                   outcomes = list(robust = outcs$census), save = FALSE, lag = cfg$lag)
    if (!is.null(r) && nrow(r) > 0) {
      r[, treatment_kind := cfg$treatment]
      r[, c_mig_log_flag := cfg$c_mig_log]
      r[, scale_form := cfg$scale]
      all_rows[[length(all_rows)+1]] <- r
    }
    # hh via run_spec
    r <- run_quiet(spec_label = spec_name, dataset = "hh", threshold = thr,
                   treatment = cfg$treatment, c_mig = TRUE,
                   c_mig_log = cfg$c_mig_log, c_fx = TRUE,
                   c_block_a = TRUE, c_block_b = FALSE, c_block_c = FALSE,
                   outcomes = list(robust = outcs$hh), save = FALSE, lag = cfg$lag)
    if (!is.null(r) && nrow(r) > 0) {
      r[, treatment_kind := cfg$treatment]
      r[, c_mig_log_flag := cfg$c_mig_log]
      r[, scale_form := cfg$scale]
      all_rows[[length(all_rows)+1]] <- r
    }

    cat(sprintf("  done %s @ k=%d — %.1f min elapsed.  Writing partial CSV…\n",
                spec_name, thr,
                as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
    save_partial(); gc(verbose = FALSE)
  }
}

save_partial()
cat(sprintf("\nFinal wall-clock: %.1f min\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
cat("Saved: ", normalizePath(out_path, winslash = "/"), "\n", sep = "")

out <- fread(out_path)
cat("\n========== Rows per (dataset × scale × lag) ==========\n")
print(out[, .N, by = .(dataset, scale_form, lag)][order(dataset, scale_form, lag)],
      nrows = 100)
