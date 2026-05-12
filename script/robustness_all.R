# =============================================================================
# script/robustness_all.R
#
# Single comprehensive robustness sweep covering all four datasets, every
# outcome we have, all four thresholds, and 12 spec variants in one CSV.
#
# Specs (per outcome × threshold):
#   S0_baseline      — anchor: treatment + year × mig_int_z + year × fx_z + Block A × year + FE
#   S1_cmig_log      — same as S0, but year × log(mig_int_z) instead of linear
#   S2_lag1 .. S7_lag10
#                    — treatment uses FX shifter lagged 1/2/3/4/5/10 years; trend
#                      controls remain contemporaneous; year-2001 obs drop for lag>0
#   S8_no_cmig       — drop year × mig_int_z (keep year × fx_z + Block A × year)
#   S9_no_cfx        — drop year × fx_z      (keep year × mig_int_z + Block A × year)
#   S10_only_blockA  — keep only Block A × year (drop both year × mig_int_z and year × fx_z)
#   S11_FE_only      — bare minimum: just muni + year FE on top of treatment
#
# Thresholds: 0 / 25 / 50 / 100  (k=25 is the anchor; the other three test
# sample composition).
#
# Output: output/tab/robustness_all.csv
#   Columns:
#     outcome_group   — first column, e.g. "Assets", "Industry employment", "Firm entry"
#     dataset         — census / hh / nec_panel / nec_cs
#     outcome         — variable name in the data
#     spec            — S0_baseline … S11_FE_only
#     threshold       — 0 / 25 / 50 / 100
#     lag             — integer lag applied to FX shifter (0 for non-lag specs)
#     c_mig_log_flag  — TRUE iff the year × mig_int_z control uses log scale
#     beta, stars, se, pval, mean_y, sd_y, n, n_muni
#     interpret       — plain-English reading of the beta (pp / % / Rs / unit)
#     err             — error / skip reason (empty when fit succeeded)
#
# Run from repo root:  source("script/robustness_all.R")
# Wall-clock estimate: ~3–4 hours on a typical laptop.
# =============================================================================

suppressPackageStartupMessages({
  library(fixest); library(data.table)
})
source("script/_specs_lib.R")
options(scipen = 999); setDTthreads(0)
ROOT <- normalizePath(".")

# -----------------------------------------------------------------------------
# 1. Spec list — 12 variants
#     Each entry sets: lag, c_mig_log, c_mig, c_fx, c_block_a
#     (run_spec defaults treatment to "log_int" and uses muni + year FE.)
# -----------------------------------------------------------------------------
SPECS <- list(
  S0_baseline     = list(lag = 0L,  c_mig_log = FALSE, c_mig = TRUE,  c_fx = TRUE,  c_block_a = TRUE),
  S1_cmig_log     = list(lag = 0L,  c_mig_log = TRUE,  c_mig = TRUE,  c_fx = TRUE,  c_block_a = TRUE),
  S2_lag1         = list(lag = 1L,  c_mig_log = FALSE, c_mig = TRUE,  c_fx = TRUE,  c_block_a = TRUE),
  S3_lag2         = list(lag = 2L,  c_mig_log = FALSE, c_mig = TRUE,  c_fx = TRUE,  c_block_a = TRUE),
  S4_lag3         = list(lag = 3L,  c_mig_log = FALSE, c_mig = TRUE,  c_fx = TRUE,  c_block_a = TRUE),
  S5_lag4         = list(lag = 4L,  c_mig_log = FALSE, c_mig = TRUE,  c_fx = TRUE,  c_block_a = TRUE),
  S6_lag5         = list(lag = 5L,  c_mig_log = FALSE, c_mig = TRUE,  c_fx = TRUE,  c_block_a = TRUE),
  S7_lag10        = list(lag = 10L, c_mig_log = FALSE, c_mig = TRUE,  c_fx = TRUE,  c_block_a = TRUE),
  S8_no_cmig      = list(lag = 0L,  c_mig_log = FALSE, c_mig = FALSE, c_fx = TRUE,  c_block_a = TRUE),
  S9_no_cfx       = list(lag = 0L,  c_mig_log = FALSE, c_mig = TRUE,  c_fx = FALSE, c_block_a = TRUE),
  S10_only_blockA = list(lag = 0L,  c_mig_log = FALSE, c_mig = FALSE, c_fx = FALSE, c_block_a = TRUE),
  S11_FE_only     = list(lag = 0L,  c_mig_log = FALSE, c_mig = FALSE, c_fx = FALSE, c_block_a = FALSE)
)
THRESHOLDS <- c(0L, 25L, 50L, 100L)

# -----------------------------------------------------------------------------
# 2. Outcome catalogues (every numeric outcome in each dataset, minus IDs)
# -----------------------------------------------------------------------------
build_outcomes <- function() {
  cen <- load_census()
  hh  <- load_hh()
  cen_id <- c("lgcode","year","district","district77","district_name")
  cen_num <- setdiff(names(cen)[sapply(cen, is.numeric)], cen_id)
  hh_id <- c("hhid","year","lgcode","district","district77","district_name",
             "wt_hh","psu","vdc","vmun_code","s00q03a","s00q03b","s00q03c",
             "member_id","fxshock","mig_intensity","log_mig_intensity",
             "total_migrants","fx_z","mig_int_z","log_migint_z")
  hh_num <- setdiff(names(hh)[sapply(hh, is.numeric)], hh_id)
  list(census = cen_num, hh = hh_num)
}

# -----------------------------------------------------------------------------
# 3. NEC datasets (loaded once)
# -----------------------------------------------------------------------------
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
nec_cs <- Reduce(function(a, b) {
  new_cols <- setdiff(names(b), names(a))
  if (length(new_cols)) merge(a, b[, c("lgcode", new_cols), with = FALSE],
                              by = "lgcode", all = TRUE) else a
}, parts)
nec_cs[, DIST := lgcode %/% 100]
for (v in c("n_firms","emp_total","rev_total","value_added_total","cap_total",
            "exp_total","profit_proxy_total")) {
  if (v %in% names(nec_cs) && !(paste0("log_", v) %in% names(nec_cs)))
    nec_cs[, (paste0("log_", v)) := log1p(pmax(get(v), 0))]
}

NEC_PANEL_OUTCOMES <- intersect(c(
  "log_new_firms",
  "log_new_firms_size_1_worker","log_new_firms_size_2_9_workers",
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

# -----------------------------------------------------------------------------
# 4. Helpers shared by all fits
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# 5. NEC fitters (handle lag + every control toggle)
# -----------------------------------------------------------------------------
fit_nec_panel <- function(outcome, lag_L, c_mig_log, c_mig, c_fx, c_block_a, threshold) {
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
  if (c_block_a && !is.null(BLOCK_A))
    panel <- merge(panel, BLOCK_A, by = "lgcode", all.x = TRUE)
  d <- panel[!is.na(get(outcome)) & !is.na(fx_z)]
  if (nrow(d) < 50 || uniqueN(d[[outcome]]) < 2 || sd(d[[outcome]], na.rm = TRUE) == 0)
    return(NULL)
  d <- copy(d)
  d[, treatment := fx_z * log_migint_z]
  year_cols <- character(0)
  if (c_mig) {
    x_mig <- if (c_mig_log) "log_migint_z" else "mig_int_z"
    year_cols <- c(year_cols, build_year_dummies_nec(d, x_mig, "cmig", 2001L))
  }
  if (c_fx)
    year_cols <- c(year_cols, build_year_dummies_nec(d, "fx_z", "cfx", 2001L))
  if (c_block_a)
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

fit_nec_cs <- function(outcome, lag_L, c_mig_log, c_mig, c_fx, c_block_a, threshold, cluster = "DIST") {
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
  if (c_block_a && !is.null(BLOCK_A))
    cs <- merge(cs, BLOCK_A, by = "lgcode", all.x = TRUE)
  d <- cs[!is.na(get(outcome)) & !is.na(fx_z)]
  if (nrow(d) < 30 || uniqueN(d[[outcome]]) < 2 || sd(d[[outcome]], na.rm = TRUE) == 0)
    return(NULL)
  d <- copy(d)
  d[, treatment := fx_z * log_migint_z]
  rhs <- "treatment"
  if (c_fx)  rhs <- c(rhs, "fx_z")
  if (c_mig) rhs <- c(rhs, if (c_mig_log) "log_migint_z" else "mig_int_z")
  if (c_block_a) rhs <- c(rhs, BLOCK_A_COLS)
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

# -----------------------------------------------------------------------------
# 6. Helpers — outcome_group classifier + interpret_beta
# -----------------------------------------------------------------------------
classify_outcome <- function(o) {
  fcase(
    o %in% c("absent_hh_share","mig_in_share"), "First stage (migrant share)",
    grepl("^log_(remit|n_migrants)", o), "First stage (remit / migrants, log)",
    o %in% c("remittance_any","remittance_amt","remit_amount_12m_rs","remit_amount_intl_12m_rs",
             "n_migrants_total","n_migrants_international","n_migrants_internal",
             "n_migrants_male","n_migrants_female","share_male_migrants",
             "has_migrant","has_migrant_international","has_migrant_internal",
             "has_only_internal","has_only_international",
             "has_both_internal_and_international",
             "intl_migration_share","migrant_mean_age","migrant_mean_months_away",
             "share_long_term_migrants","remit_received","share_migrants_sending",
             "remit_frequency_avg","remit_via_formal_any","remit_via_informal_any",
             "remit_via_hundi_any","remit_per_migrant","remit_per_international_migrant",
             "cost_per_migrant","cost_to_monthly_earning_ratio","mig_cost_12m_rs",
             "migrant_monthly_earning_total_rs"), "Migration / remittance",
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
    o %in% c("share_self_wet","share_self_dry","share_both_seasons",
             "share_fallow_wet","share_fallow_dry","crop_simpson_diversity",
             "crop_hhi","crop_sale_share","any_crop_sold","grows_horticulture",
             "horti_value_share","cashcrop_value_share","n_crops_total",
             "n_crops_dry","n_crops_wet","effective_n_crops","crop_sale_rs_12m",
             "share_irr_rainfed_dry","share_irr_rainfed_wet","share_irr_surface_dry",
             "share_irr_surface_wet","share_irr_other_dry","share_irr_other_wet",
             "share_rented_in_wet","share_rented_in_dry",
             "n_plots_rented_out_wet","n_plots_rented_out_dry",
             "land_sold_12m_rs"),  "Land use / cropping",
    grepl("^owns_", o) | grepl("(cost_seed|cost_fert|cost_labour|cost_insect|cost_equip|input_cost|total_input)", o),
                                  "Input use / capital",
    grepl("(livestock|effective_n_animals)", o),
                                  "Livestock",
    grepl("^(food_exp|food_insec|nonfood_)", o), "Consumption",
    grepl("^(any_health|hh_health)", o), "Health utilisation",
    grepl("^(private_|public_|support_|disaster|insurance|safety_net|social_protection|cash_transfer|relief)", o),
                                  "Social protection",
    grepl("^(shock|coping|severe|multiple_shocks|n_shocks)", o),
                                  "Shocks & coping",
    grepl("^(enterprise|biz_|business_)", o), "Household enterprise",
    grepl("^(left_|n_left|child_|caregiver)", o), "Family structure",
    o == "log_new_firms",         "Firm entry — all (NEC panel)",
    grepl("^log_new_firms_size_", o), "Firm entry by size (NEC panel)",
    grepl("^log_new_firms_", o),  "Firm entry by industry (NEC panel)",
    o %in% c("log_n_firms","log_emp_total","log_rev_total","log_cap_total",
             "log_value_added_total","log_exp_total","log_profit_proxy_total"),
                                  "Firm scale (NEC 2018)",
    grepl("(per_worker|profit_margin|wage_share|value_added_share|capital_intensity)", o),
                                  "Firm productivity (NEC 2018)",
    grepl("^share_size_", o),     "Firm size distribution (NEC 2018)",
    o %in% c("formality_index","share_registered","share_tax_registered",
             "share_keeps_accounts","share_operates_year_round",
             "share_borrowed","share_uses_formal_credit","share_has_foreign_capital",
             "share_female_led","mean_emp_per_firm"),
                                  "Firm structure / formality (NEC 2018)",
    grepl("^share_(agriculture|manufacturing|construction|trade_retail|hospitality|finance_prof_info|social_services|transport|other_services)", o),
                                  "Firm industry mix (NEC 2018)",
    o %in% c("industry_diversity","industry_hhi","n_industries_present",
             "share_modern_proxy","share_services_total"),
                                  "Firm composition (NEC 2018)",
    default = "Other"
  )
}

# Detect outcome's natural scale to format the interpretation cell
detect_scale <- function(outcome, mean_y) {
  if (grepl("^log_|_log$", outcome)) return("log")
  if (grepl("(_rs$|_rs_|_amt$|spend_|food_exp_|nonfood_|cost_per_|^cap_|crop_sale_rs|earning_total_rs|cost_12m_rs|land_sold_12m_rs)", outcome))
    return("rs")
  # If mean is in [0,1] interpret as a share (pp)
  if (!is.na(mean_y) && abs(mean_y) <= 1 && mean_y >= 0)
    return("share")
  # Specific index-like outcomes
  if (grepl("(diversity|hhi|index|score|n_industries)", outcome))
    return("index")
  if (grepl("(^n_|count|firms_|emp_total|^new_firms)", outcome))
    return("count")
  "other"
}

interpret_beta <- function(beta, outcome, mean_y) {
  if (is.na(beta)) return("")
  scale <- detect_scale(outcome, mean_y)
  pct_of_mean <- function() {
    if (is.na(mean_y) || abs(mean_y) < 1e-9) return("")
    sprintf(" (%+.0f%% of mean)", 100*beta/mean_y)
  }
  if (scale == "log") {
    sprintf("%+.1f%% change (log outcome)", beta*100)
  } else if (scale == "rs") {
    sprintf("%+s Rs%s",
            formatC(round(beta), format = "d", big.mark = ","),
            pct_of_mean())
  } else if (scale == "share") {
    sprintf("%+.2fpp%s", beta*100, pct_of_mean())
  } else if (scale == "index") {
    sprintf("%+.3f index pts (baseline %.3f)", beta, mean_y)
  } else if (scale == "count") {
    sprintf("%+.3f units%s", beta, pct_of_mean())
  } else {
    sprintf("%+.3f (baseline %.3f)", beta, mean_y)
  }
}

# -----------------------------------------------------------------------------
# 7. Loop  (spec × threshold × dataset × outcome)
# -----------------------------------------------------------------------------
outcs <- build_outcomes()
all_rows <- list()
t_start <- Sys.time()
cat(sprintf("\n========== robustness_all ==========\n"))
cat(sprintf("Specs:      %d  (%s)\n", length(SPECS), paste(names(SPECS), collapse=", ")))
cat(sprintf("Thresholds: %s\n", paste(THRESHOLDS, collapse=", ")))
cat(sprintf("Outcomes:   census=%d  hh=%d  nec_panel=%d  nec_cs=%d  (total=%d)\n",
            length(outcs$census), length(outcs$hh),
            length(NEC_PANEL_OUTCOMES), length(NEC_CS_OUTCOMES),
            length(outcs$census)+length(outcs$hh)+
              length(NEC_PANEL_OUTCOMES)+length(NEC_CS_OUTCOMES)))
total_cells <- length(SPECS) * length(THRESHOLDS) *
  (length(outcs$census)+length(outcs$hh)+length(NEC_PANEL_OUTCOMES)+length(NEC_CS_OUTCOMES))
cat(sprintf("Total (spec × thr × outcome × dataset) cells: %d\n\n", total_cells))

for (spec_name in names(SPECS)) {
  cfg <- SPECS[[spec_name]]
  for (thr in THRESHOLDS) {
    cat(sprintf("--- %s @ k=%d (lag=%d, c_mig_log=%s, c_mig=%s, c_fx=%s, blockA=%s) ---\n",
                spec_name, thr, cfg$lag, cfg$c_mig_log, cfg$c_mig, cfg$c_fx, cfg$c_block_a))

    # census via run_spec
    r <- run_quiet(
      spec_label = spec_name, dataset = "census", threshold = thr,
      treatment = "log_int",
      c_mig = cfg$c_mig, c_mig_log = cfg$c_mig_log, c_fx = cfg$c_fx,
      c_block_a = cfg$c_block_a, c_block_b = FALSE, c_block_c = FALSE,
      outcomes = list(robust = outcs$census),
      save = FALSE, lag = cfg$lag
    )
    if (!is.null(r) && nrow(r) > 0) {
      r[, c_mig_log_flag := cfg$c_mig_log]
      all_rows[[length(all_rows)+1]] <- r
    }

    # hh via run_spec
    r <- run_quiet(
      spec_label = spec_name, dataset = "hh", threshold = thr,
      treatment = "log_int",
      c_mig = cfg$c_mig, c_mig_log = cfg$c_mig_log, c_fx = cfg$c_fx,
      c_block_a = cfg$c_block_a, c_block_b = FALSE, c_block_c = FALSE,
      outcomes = list(robust = outcs$hh),
      save = FALSE, lag = cfg$lag
    )
    if (!is.null(r) && nrow(r) > 0) {
      r[, c_mig_log_flag := cfg$c_mig_log]
      all_rows[[length(all_rows)+1]] <- r
    }

    # NEC panel
    for (y in NEC_PANEL_OUTCOMES) {
      fit <- fit_nec_panel(y, cfg$lag, cfg$c_mig_log, cfg$c_mig, cfg$c_fx, cfg$c_block_a, thr)
      if (is.null(fit)) {
        all_rows[[length(all_rows)+1]] <- data.table(
          dataset = "nec_panel", outcome = y, spec = spec_name,
          threshold = thr, lag = cfg$lag, c_mig_log_flag = cfg$c_mig_log,
          err = "NULL/degenerate"); next
      }
      if (!is.null(fit$err)) {
        all_rows[[length(all_rows)+1]] <- data.table(
          dataset = "nec_panel", outcome = y, spec = spec_name,
          threshold = thr, lag = cfg$lag, c_mig_log_flag = cfg$c_mig_log,
          err = fit$err); next
      }
      all_rows[[length(all_rows)+1]] <- data.table(
        dataset = "nec_panel", outcome = y, spec = spec_name,
        threshold = thr, lag = cfg$lag, c_mig_log_flag = cfg$c_mig_log,
        beta = fit$beta, se = fit$se, pval = fit$pval, stars = stars_fn(fit$pval),
        n = fit$n, n_muni = fit$n_muni, mean_y = fit$mean_y, sd_y = fit$sd_y, err = "")
    }

    # NEC cs
    for (y in NEC_CS_OUTCOMES) {
      fit <- fit_nec_cs(y, cfg$lag, cfg$c_mig_log, cfg$c_mig, cfg$c_fx, cfg$c_block_a, thr)
      if (is.null(fit)) {
        all_rows[[length(all_rows)+1]] <- data.table(
          dataset = "nec_cs", outcome = y, spec = spec_name,
          threshold = thr, lag = cfg$lag, c_mig_log_flag = cfg$c_mig_log,
          err = "NULL/degenerate"); next
      }
      if (!is.null(fit$err)) {
        all_rows[[length(all_rows)+1]] <- data.table(
          dataset = "nec_cs", outcome = y, spec = spec_name,
          threshold = thr, lag = cfg$lag, c_mig_log_flag = cfg$c_mig_log,
          err = fit$err); next
      }
      all_rows[[length(all_rows)+1]] <- data.table(
        dataset = "nec_cs", outcome = y, spec = spec_name,
        threshold = thr, lag = cfg$lag, c_mig_log_flag = cfg$c_mig_log,
        beta = fit$beta, se = fit$se, pval = fit$pval, stars = stars_fn(fit$pval),
        n = fit$n, n_muni = fit$n_muni, mean_y = fit$mean_y, sd_y = fit$sd_y, err = "")
    }

    cat(sprintf("  done %s @ k=%d  —  %.1f min elapsed\n", spec_name, thr,
                as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
  }
}

# -----------------------------------------------------------------------------
# 8. Combine, decorate, save
# -----------------------------------------------------------------------------
out <- rbindlist(all_rows, fill = TRUE)
# Default values for cells that didn't produce a fit (so columns exist)
for (col in c("beta","se","pval","mean_y","sd_y","n","n_muni"))
  if (!col %in% names(out)) out[, (col) := NA_real_]
if (!"stars" %in% names(out)) out[, stars := ""]
if (!"err"   %in% names(out)) out[, err   := ""]

# Compute outcome_group and interpret
out[, outcome_group := classify_outcome(outcome)]
out[, interpret     := mapply(interpret_beta, beta, outcome, mean_y)]

# Final column order — outcome_group FIRST per user request
KEEP <- c("outcome_group","dataset","outcome","spec","threshold","lag","c_mig_log_flag",
          "beta","stars","se","pval","mean_y","sd_y","n","n_muni","interpret","err")
out <- out[, intersect(KEEP, names(out)), with = FALSE]

# Stable spec ordering: respect names(SPECS); within spec, by threshold asc
out[, spec := factor(spec, levels = names(SPECS))]
out <- out[order(outcome_group, dataset, outcome, spec, threshold)]

dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(ROOT, "output/tab/robustness_all.csv")
fwrite(out, out_path)

cat(sprintf("\nWall-clock: %.1f min\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
cat("Saved: ", normalizePath(out_path, winslash = "/"),
    "  (", nrow(out), " rows)\n", sep = "")

# -----------------------------------------------------------------------------
# 9. Quick on-screen summary
# -----------------------------------------------------------------------------
cat("\n========== Rows per (dataset × spec × threshold) ==========\n")
print(out[, .N, by = .(dataset, spec, threshold)][order(dataset, spec, threshold)],
      nrows = 200)
cat("\n========== Distinct outcome groups ==========\n")
print(out[, .(n_outcomes = uniqueN(outcome)), by = outcome_group][order(-n_outcomes)])
cat("\n========== Errors summary (top 20) ==========\n")
err <- out[!is.na(err) & err != "" & err != "NULL/degenerate"][, .N, by = err]
if (nrow(err)) print(err[order(-N)][1:20]) else cat("  (no non-trivial errors)\n")
