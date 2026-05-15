# =============================================================================
# script/robustness_final_fill.R
#
# Fills in the missing (scale × lag) cells of the robustness grid:
#   log/log scale at lags 1, 2, 3, 4, 5, 10
#   lin/lin scale at lags 1, 2, 3, 4, 5, 10
# = 12 additional specs × 4 thresholds × ~290 outcomes × 4 datasets.
#
# These plug into the same downstream pipeline as robustness_final.R:
# build_robustness_json.py will combine this CSV with the main one
# automatically.
#
# Output: output/tab/robustness_final_fill.csv
# Wall-clock: ~3 hours.  Crash-tolerant: writes CSV after each spec.
# Re-run is safe; the script does not touch robustness_final.csv.
# =============================================================================

suppressPackageStartupMessages({
  library(fixest); library(data.table)
})
source("script/_specs_lib.R")
options(scipen = 999); setDTthreads(0)
ROOT <- normalizePath(".")

SPECS <- list(
  # log/log scale × lags
  S_both_log_lag1     = list(treatment = "log_int", lag = 1L,  c_mig_log = TRUE),
  S_both_log_lag2     = list(treatment = "log_int", lag = 2L,  c_mig_log = TRUE),
  S_both_log_lag3     = list(treatment = "log_int", lag = 3L,  c_mig_log = TRUE),
  S_both_log_lag4     = list(treatment = "log_int", lag = 4L,  c_mig_log = TRUE),
  S_both_log_lag5     = list(treatment = "log_int", lag = 5L,  c_mig_log = TRUE),
  S_both_log_lag10    = list(treatment = "log_int", lag = 10L, c_mig_log = TRUE),
  # lin/lin scale × lags
  S_both_linear_lag1  = list(treatment = "lin_int", lag = 1L,  c_mig_log = FALSE),
  S_both_linear_lag2  = list(treatment = "lin_int", lag = 2L,  c_mig_log = FALSE),
  S_both_linear_lag3  = list(treatment = "lin_int", lag = 3L,  c_mig_log = FALSE),
  S_both_linear_lag4  = list(treatment = "lin_int", lag = 4L,  c_mig_log = FALSE),
  S_both_linear_lag5  = list(treatment = "lin_int", lag = 5L,  c_mig_log = FALSE),
  S_both_linear_lag10 = list(treatment = "lin_int", lag = 10L, c_mig_log = FALSE)
)
THRESHOLDS <- c(0L, 25L, 50L, 100L)

# ----- Outcome catalogues (same as robustness_final.R) -----
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

nec_p <- fread("data/clean/nec2018/mun_entry_panel_new.csv")
nec_p <- nec_p[year >= 2001 & year <= 2018]
for (v in c("new_firms","new_firms_size_1_worker","new_firms_size_2_9_workers",
            "new_firms_size_10_50_workers","new_firms_size_51plus_workers",
            "new_firms_agriculture","new_firms_manufacturing","new_firms_construction",
            "new_firms_trade_retail","new_firms_hospitality_food",
            "new_firms_transport_storage","new_firms_other_services",
            "new_firms_finance_prof_realestate","new_firms_education_health_social")) {
  if (v %in% names(nec_p) && !(paste0("log_", v) %in% names(nec_p)))
    nec_p[, (paste0("log_", v)) := log1p(pmax(get(v), 0))]
}
parts <- lapply(c("mun_industry_structure","mun_productivity_profitability","mun_size_formality"),
                function(f) {
                  p <- file.path("data/clean/nec2018", paste0(f, ".csv"))
                  if (file.exists(p)) fread(p) else NULL
                })
parts <- parts[!sapply(parts, is.null)]
if (length(parts) == 0) {
  fallback_p <- "data/clean/nec2018/municipality_analysis.csv"
  if (!file.exists(fallback_p))
    stop("None of mun_industry_structure / mun_productivity_profitability / ",
         "mun_size_formality found, and fallback ", fallback_p,
         " is also missing.\nRun 03_municipality_wide.R first.")
  cat("  using fallback NEC cs file: ", fallback_p, "\n", sep = "")
  parts <- list(fread(fallback_p))
}
nec_cs <- Reduce(function(a, b) {
  new_cols <- setdiff(names(b), names(a))
  if (length(new_cols)) merge(a, b[, c("lgcode", new_cols), with = FALSE],
                              by = "lgcode", all = TRUE) else a
}, parts)
if (!data.table::is.data.table(nec_cs)) nec_cs <- as.data.table(nec_cs)
nec_cs[, DIST := lgcode %/% 100]
for (v in c("n_firms","emp_total","rev_total","value_added_total","cap_total",
            "exp_total","profit_proxy_total")) {
  if (v %in% names(nec_cs) && !(paste0("log_", v) %in% names(nec_cs)))
    nec_cs[, (paste0("log_", v)) := log1p(pmax(get(v), 0))]
}
NEC_PANEL_OUTCOMES <- intersect(c(
  "log_new_firms","log_new_firms_size_1_worker","log_new_firms_size_2_9_workers",
  "log_new_firms_size_10_50_workers","log_new_firms_size_51plus_workers",
  "log_new_firms_agriculture","log_new_firms_manufacturing","log_new_firms_construction",
  "log_new_firms_trade_retail","log_new_firms_hospitality_food",
  "log_new_firms_transport_storage","log_new_firms_other_services",
  "log_new_firms_finance_prof_realestate","log_new_firms_education_health_social"
), names(nec_p))
NEC_CS_OUTCOMES <- intersect(c(
  "log_n_firms","log_emp_total","log_rev_total","log_value_added_total",
  "log_cap_total","log_exp_total","log_profit_proxy_total",
  "mean_value_added_per_worker","median_value_added_per_worker",
  "mean_rev_per_worker","mean_capital_per_worker","mean_profit_per_worker",
  "mean_profit_margin","wage_share_of_revenue","value_added_share_of_revenue",
  "capital_intensity_aggregate",
  "formality_index","share_registered","share_tax_registered",
  "share_keeps_accounts","share_operates_year_round",
  "share_borrowed","share_uses_formal_credit","share_has_foreign_capital",
  "share_female_led","mean_emp_per_firm",
  "share_size_1_worker","share_size_2_9_workers","share_size_10_50_workers","share_size_51plus_workers",
  "industry_diversity","industry_hhi","n_industries_present",
  "share_modern_proxy","share_services_total",
  "share_agriculture","share_manufacturing","share_construction","share_trade_retail",
  "share_hospitality","share_finance_prof_info","share_social_services",
  "share_transport","share_other_services"
), names(nec_cs))

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}
build_year_dummies_nec <- function(d, X_col, prefix, ref_year) {
  yrs <- sort(unique(d$year))
  if (length(yrs) < 2) return(character(0))
  ref_use <- if (ref_year %in% yrs) ref_year else min(yrs)
  cols <- character(0)
  for (yr in yrs) {
    if (yr == ref_use) next
    cnm <- sprintf("%s_x_%s", prefix, yr)
    set(d, j = cnm, value = d[[X_col]] * as.numeric(d$year == yr))
    cols <- c(cols, cnm)
  }
  cols
}
inst <- load_instrument()
bxA  <- build_block_A()
BLOCK_A      <- bxA$bx
BLOCK_A_COLS <- bxA$cols

make_treatment <- function(d, treatment_kind) {
  if (treatment_kind == "lin_int") d[, treatment := fx_z * mig_int_z]
  else                              d[, treatment := fx_z * log_migint_z]
  d
}

fit_nec_panel <- function(outcome, treatment_kind, lag_L, c_mig_log, threshold) {
  inst_use <- inst[, .(lgcode, year, fxshock, mig_intensity, total_migrants)]
  inst_use[, log_mig_intensity := log(mig_intensity + 1e-8)]
  if (lag_L != 0L) inst_use[, year := year + as.integer(lag_L)]
  panel <- merge(nec_p, inst_use, by = c("lgcode","year"), suffixes = c("",".inst"))
  panel <- panel[total_migrants >= threshold]
  if (nrow(panel) < 50) return(NULL)
  muni_yr <- unique(panel[, .(lgcode, year, fxshock, mig_intensity, log_mig_intensity)])
  muni_yr[, fx_z         := zscore(fxshock)]
  muni_yr[, mig_int_z    := zscore(mig_intensity)]
  muni_yr[, log_migint_z := zscore(log_mig_intensity)]
  panel <- merge(panel, muni_yr[, .(lgcode, year, fx_z, mig_int_z, log_migint_z)],
                 by = c("lgcode","year"))
  if (!is.null(BLOCK_A)) panel <- merge(panel, BLOCK_A, by = "lgcode", all.x = TRUE)
  d <- panel[!is.na(get(outcome)) & !is.na(fx_z)]
  if (nrow(d) < 50 || uniqueN(d[[outcome]]) < 2 || sd(d[[outcome]], na.rm = TRUE) == 0)
    return(NULL)
  d <- copy(d); d <- make_treatment(d, treatment_kind)
  year_cols <- character(0)
  x_mig <- if (c_mig_log) "log_migint_z" else "mig_int_z"
  year_cols <- c(year_cols, build_year_dummies_nec(d, x_mig, "cmig", 2001L))
  year_cols <- c(year_cols, build_year_dummies_nec(d, "fx_z", "cfx",  2001L))
  for (k in BLOCK_A_COLS)
    year_cols <- c(year_cols, build_year_dummies_nec(d, k, paste0("cA_", k), 2001L))
  rhs <- c("treatment", year_cols)
  fml <- as.formula(sprintf("%s ~ %s | lgcode + year", outcome, paste(rhs, collapse = " + ")))
  fit <- tryCatch(feols(fml, data = d, cluster = ~lgcode, notes = FALSE, warn = FALSE),
                  error = function(e) e)
  if (inherits(fit, "error")) return(list(err = substr(conditionMessage(fit), 1, 120)))
  cf <- coef(fit); se_ <- se(fit); pv <- pvalue(fit)
  if (!("treatment" %in% names(cf))) return(list(err = "treatment absorbed"))
  list(beta = unname(cf["treatment"]), se = unname(se_["treatment"]),
       pval = unname(pv["treatment"]),
       n = as.integer(fit$nobs), n_muni = uniqueN(d$lgcode),
       mean_y = mean(d[[outcome]], na.rm = TRUE),
       sd_y   = sd(d[[outcome]], na.rm = TRUE))
}

fit_nec_cs <- function(outcome, treatment_kind, lag_L, c_mig_log, threshold, cluster = "DIST") {
  yr <- 2018L - as.integer(lag_L)
  inst_use <- inst[year == yr, .(lgcode, fxshock, mig_intensity, total_migrants)]
  if (nrow(inst_use) == 0) return(list(err = sprintf("no FX for year %d", yr)))
  inst_use[, log_mig_intensity := log(mig_intensity + 1e-8)]
  cs <- merge(nec_cs, inst_use, by = "lgcode", suffixes = c("",".inst"))
  cs <- cs[total_migrants >= threshold]
  if (nrow(cs) < 30) return(NULL)
  cs[, fx_z         := zscore(fxshock)]
  cs[, mig_int_z    := zscore(mig_intensity)]
  cs[, log_migint_z := zscore(log_mig_intensity)]
  if (!is.null(BLOCK_A)) cs <- merge(cs, BLOCK_A, by = "lgcode", all.x = TRUE)
  d <- cs[!is.na(get(outcome)) & !is.na(fx_z)]
  if (nrow(d) < 30 || uniqueN(d[[outcome]]) < 2 || sd(d[[outcome]], na.rm = TRUE) == 0)
    return(NULL)
  d <- copy(d); d <- make_treatment(d, treatment_kind)
  rhs <- c("treatment", "fx_z",
           if (c_mig_log) "log_migint_z" else "mig_int_z",
           BLOCK_A_COLS)
  fml <- as.formula(sprintf("%s ~ %s | %s", outcome, paste(rhs, collapse = " + "), cluster))
  fit <- tryCatch(feols(fml, data = d, cluster = as.formula(paste0("~", cluster)),
                        notes = FALSE, warn = FALSE),
                  error = function(e) e)
  if (inherits(fit, "error")) return(list(err = substr(conditionMessage(fit), 1, 120)))
  cf <- coef(fit); se_ <- se(fit); pv <- pvalue(fit)
  if (!("treatment" %in% names(cf))) return(list(err = "treatment absorbed"))
  list(beta = unname(cf["treatment"]), se = unname(se_["treatment"]),
       pval = unname(pv["treatment"]),
       n = as.integer(fit$nobs), n_muni = nrow(d),
       mean_y = mean(d[[outcome]], na.rm = TRUE),
       sd_y   = sd(d[[outcome]], na.rm = TRUE))
}

stars_fn <- function(p) fifelse(is.na(p), "",
                                 fifelse(p < .01, "***",
                                 fifelse(p < .05, "**",
                                 fifelse(p < .10, "*", ""))))
run_quiet <- function(...) {
  invisible(capture.output(res <- run_spec(...), file = nullfile()))
  res
}

# ---- group + interpret helpers (same as robustness_final.R) ----
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
    o == "log_new_firms",                "Firm entry — all (NEC panel)",
    grepl("^log_new_firms_size_", o),    "Firm entry by size (NEC panel)",
    grepl("^log_new_firms_", o),         "Firm entry by industry (NEC panel)",
    o %in% c("log_n_firms","log_emp_total","log_rev_total","log_cap_total",
             "log_value_added_total","log_exp_total","log_profit_proxy_total"),
                                          "Firm scale (NEC 2018)",
    grepl("(per_worker|profit_margin|wage_share|value_added_share|capital_intensity)", o),
                                          "Firm productivity (NEC 2018)",
    grepl("^share_size_", o), "Firm size distribution (NEC 2018)",
    o %in% c("formality_index","share_registered","share_tax_registered",
             "share_keeps_accounts","share_operates_year_round",
             "share_borrowed","share_uses_formal_credit","share_has_foreign_capital",
             "share_female_led","mean_emp_per_firm"), "Firm structure / formality (NEC 2018)",
    grepl("^share_(agriculture|manufacturing|construction|trade_retail|hospitality|finance_prof_info|social_services|transport|other_services)", o),
                                          "Firm industry mix (NEC 2018)",
    o %in% c("industry_diversity","industry_hhi","n_industries_present",
             "share_modern_proxy","share_services_total"), "Firm composition (NEC 2018)",
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

# ----- Main loop with incremental save -----
outcs <- build_outcomes()
out_path <- file.path(ROOT, "output/tab/robustness_final_fill.csv")
dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
all_rows <- list(); t_start <- Sys.time()

cat("========== robustness_final_fill ==========\n")
cat(sprintf("Specs:      %d (%s)\n", length(SPECS), paste(names(SPECS), collapse=", ")))
cat(sprintf("Thresholds: %s\n", paste(THRESHOLDS, collapse=", ")))
cat(sprintf("Outcomes:   census=%d hh=%d nec_panel=%d nec_cs=%d\n\n",
            length(outcs$census), length(outcs$hh),
            length(NEC_PANEL_OUTCOMES), length(NEC_CS_OUTCOMES)))

save_partial <- function() {
  d <- rbindlist(all_rows, fill = TRUE)
  for (col in c("beta","se","pval","mean_y","sd_y","n","n_muni"))
    if (!col %in% names(d)) d[, (col) := NA_real_]
  if (!"stars" %in% names(d)) d[, stars := ""]
  if (!"err"   %in% names(d)) d[, err   := ""]
  d[, outcome_group := classify_outcome(outcome)]
  d[, interpret     := mapply(interpret_beta, beta, outcome, mean_y)]
  KEEP <- c("outcome_group","dataset","outcome","spec","threshold","lag",
            "treatment_kind","c_mig_log_flag",
            "beta","stars","se","pval","mean_y","sd_y","n","n_muni","interpret","err")
  d <- d[, intersect(KEEP, names(d)), with = FALSE]
  d[, spec := factor(spec, levels = names(SPECS))]
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  fwrite(d[order(outcome_group, dataset, outcome, spec, threshold)], out_path)
}

for (spec_name in names(SPECS)) {
  cfg <- SPECS[[spec_name]]
  for (thr in THRESHOLDS) {
    cat(sprintf("--- %s @ k=%d (treatment=%s, lag=%d, c_mig_log=%s) ---\n",
                spec_name, thr, cfg$treatment, cfg$lag, cfg$c_mig_log))
    # census
    r <- run_quiet(spec_label = spec_name, dataset = "census", threshold = thr,
                   treatment = cfg$treatment, c_mig = TRUE,
                   c_mig_log = cfg$c_mig_log, c_fx = TRUE,
                   c_block_a = TRUE, c_block_b = FALSE, c_block_c = FALSE,
                   outcomes = list(robust = outcs$census), save = FALSE, lag = cfg$lag)
    if (!is.null(r) && nrow(r) > 0) {
      r[, treatment_kind := cfg$treatment]; r[, c_mig_log_flag := cfg$c_mig_log]
      all_rows[[length(all_rows)+1]] <- r
    }
    # hh
    r <- run_quiet(spec_label = spec_name, dataset = "hh", threshold = thr,
                   treatment = cfg$treatment, c_mig = TRUE,
                   c_mig_log = cfg$c_mig_log, c_fx = TRUE,
                   c_block_a = TRUE, c_block_b = FALSE, c_block_c = FALSE,
                   outcomes = list(robust = outcs$hh), save = FALSE, lag = cfg$lag)
    if (!is.null(r) && nrow(r) > 0) {
      r[, treatment_kind := cfg$treatment]; r[, c_mig_log_flag := cfg$c_mig_log]
      all_rows[[length(all_rows)+1]] <- r
    }
    # NEC panel
    for (y in NEC_PANEL_OUTCOMES) {
      fit <- fit_nec_panel(y, cfg$treatment, cfg$lag, cfg$c_mig_log, thr)
      base <- data.table(dataset = "nec_panel", outcome = y, spec = spec_name,
                         threshold = thr, lag = cfg$lag,
                         treatment_kind = cfg$treatment, c_mig_log_flag = cfg$c_mig_log)
      if (is.null(fit)) { base[, err := "NULL/degenerate"] }
      else if (!is.null(fit$err)) { base[, err := fit$err] }
      else base[, `:=`(beta=fit$beta, se=fit$se, pval=fit$pval, stars=stars_fn(fit$pval),
                       n=fit$n, n_muni=fit$n_muni, mean_y=fit$mean_y, sd_y=fit$sd_y, err="")]
      all_rows[[length(all_rows)+1]] <- base
    }
    # NEC cs
    for (y in NEC_CS_OUTCOMES) {
      fit <- fit_nec_cs(y, cfg$treatment, cfg$lag, cfg$c_mig_log, thr)
      base <- data.table(dataset = "nec_cs", outcome = y, spec = spec_name,
                         threshold = thr, lag = cfg$lag,
                         treatment_kind = cfg$treatment, c_mig_log_flag = cfg$c_mig_log)
      if (is.null(fit)) { base[, err := "NULL/degenerate"] }
      else if (!is.null(fit$err)) { base[, err := fit$err] }
      else base[, `:=`(beta=fit$beta, se=fit$se, pval=fit$pval, stars=stars_fn(fit$pval),
                       n=fit$n, n_muni=fit$n_muni, mean_y=fit$mean_y, sd_y=fit$sd_y, err="")]
      all_rows[[length(all_rows)+1]] <- base
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
