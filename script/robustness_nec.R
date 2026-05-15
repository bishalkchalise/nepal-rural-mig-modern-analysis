# =============================================================================
# script/robustness_nec.R
#
# Standalone robustness sweep for the NEC datasets only.
#
# Spec grid (33 specs per outcome × threshold):
#   3 scale forms × 11 lags  =  33
#     scales: log/lin (baseline mix), log/log (both log), lin/lin (both linear)
#     lags:   0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10  years
#
# Datasets:
#   nec_panel  (14 outcomes — log_new_firms + size + industry)
#   nec_cs     (44 outcomes — firm structure 2018)
# Thresholds:  k = 0, 25, 50, 100
#
# Total cells: 33 × (14 + 44) × 4 = 7,656 regressions.
# Wall-clock estimate: ~45–75 minutes.
# Crash-tolerant: writes output/tab/robustness_nec.csv after each spec.
#
# Output schema matches robustness_final.csv (same columns + outcome_group +
# interpret) so it slots into the existing portal pipeline.  To use:
#
#   source("script/robustness_nec.R")
#   git add output/tab/robustness_nec.csv
#   git commit -m "Run NEC-only annual-lag robustness"
#   git push -u origin <branch>
#
# Then on the server side:
#   python3 script/build_robustness_json.py    # auto-merges if you wire it
# =============================================================================

suppressPackageStartupMessages({
  library(fixest); library(data.table)
})
source("script/_specs_lib.R")
options(scipen = 999); setDTthreads(0)
ROOT <- normalizePath(".")

# -----------------------------------------------------------------------------
# 1. Spec grid: 3 scales × 11 lags = 33 specs
# -----------------------------------------------------------------------------
SCALES <- list(
  log_lin = list(treatment = "log_int", c_mig_log = FALSE, label = "log/lin"),
  log_log = list(treatment = "log_int", c_mig_log = TRUE,  label = "log/log"),
  lin_lin = list(treatment = "lin_int", c_mig_log = FALSE, label = "lin/lin")
)
LAGS <- 0:10
THRESHOLDS <- c(0L, 25L, 50L, 100L)

# Build list of (spec_name, treatment, lag, c_mig_log) tuples
SPECS <- list()
for (sk in names(SCALES)) {
  scfg <- SCALES[[sk]]
  for (L in LAGS) {
    nm <- if (sk == "log_lin" && L == 0L) "S0_baseline"
          else sprintf("S_%s_lag%d", sk, L)
    SPECS[[nm]] <- list(
      treatment = scfg$treatment,
      c_mig_log = scfg$c_mig_log,
      lag       = as.integer(L),
      scale     = scfg$label
    )
  }
}

# -----------------------------------------------------------------------------
# 2. Load NEC panel + cs (same as robustness_final.R)
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
# 3. Estimators
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

make_treatment <- function(d, treatment_kind) {
  if (treatment_kind == "lin_int")  d[, treatment := fx_z * mig_int_z]
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
  year_cols <- c(year_cols, build_year_dummies_nec(d, x_mig,  "cmig", 2001L))
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

# -----------------------------------------------------------------------------
# 4. Group + interpretation helpers
# -----------------------------------------------------------------------------
classify_outcome <- function(o) {
  fcase(
    o == "log_new_firms",                "Firm entry — total + by size",
    grepl("^log_new_firms_size_", o),    "Firm entry — total + by size",
    grepl("^log_new_firms_", o),         "Firm entry — by industry (NEC panel)",
    o %in% c("log_n_firms","log_emp_total","log_rev_total","log_cap_total",
             "log_value_added_total","log_exp_total","log_profit_proxy_total","mean_emp_per_firm"),
                                          "Firm count & scale",
    grepl("(per_worker|profit_margin|wage_share|value_added_share|capital_intensity)", o),
                                          "Per-worker productivity / factor shares",
    grepl("^share_size_", o),             "Firm size distribution",
    o %in% c("formality_index","share_registered","share_tax_registered",
             "share_keeps_accounts","share_operates_year_round"),  "Formality",
    o %in% c("share_borrowed","share_uses_formal_credit","share_has_foreign_capital"),
                                          "Credit",
    o == "share_female_led",              "Firm demographics",
    grepl("^share_(agriculture|manufacturing|construction|trade_retail|hospitality|finance_prof_info|social_services|transport|other_services)", o),
                                          "Industry shares",
    o %in% c("industry_diversity","industry_hhi","n_industries_present",
             "share_modern_proxy","share_services_total"),         "Industry composition",
    default = "Other"
  )
}
detect_scale <- function(o, m) {
  if (grepl("^log_|_log$", o)) return("log")
  if (grepl("(_rs$|_rs_|_amt$|spend_|cost_per_|^cap_)", o)) return("rs")
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
# 5. Loop with incremental save
# -----------------------------------------------------------------------------
out_path <- file.path(ROOT, "output/tab/robustness_nec.csv")
dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
all_rows <- list(); t_start <- Sys.time()

cat("========== robustness_nec ==========\n")
cat(sprintf("Specs:      %d (3 scales × 11 lags)\n", length(SPECS)))
cat(sprintf("Thresholds: %s\n", paste(THRESHOLDS, collapse=", ")))
cat(sprintf("Outcomes:   nec_panel=%d  nec_cs=%d\n",
            length(NEC_PANEL_OUTCOMES), length(NEC_CS_OUTCOMES)))
total_cells <- length(SPECS) * length(THRESHOLDS) *
  (length(NEC_PANEL_OUTCOMES) + length(NEC_CS_OUTCOMES))
cat(sprintf("Total cells: %d\n\n", total_cells))

save_partial <- function() {
  d <- rbindlist(all_rows, fill = TRUE)
  for (col in c("beta","se","pval","mean_y","sd_y","n","n_muni"))
    if (!col %in% names(d)) d[, (col) := NA_real_]
  if (!"stars" %in% names(d)) d[, stars := ""]
  if (!"err"   %in% names(d)) d[, err   := ""]
  d[, outcome_group := classify_outcome(outcome)]
  d[, interpret     := mapply(interpret_beta, beta, outcome, mean_y)]
  KEEP <- c("outcome_group","dataset","outcome","spec","threshold","lag",
            "treatment_kind","c_mig_log_flag","scale_form",
            "beta","stars","se","pval","mean_y","sd_y","n","n_muni","interpret","err")
  d <- d[, intersect(KEEP, names(d)), with = FALSE]
  d[, spec := factor(spec, levels = names(SPECS))]
  fwrite(d[order(outcome_group, dataset, outcome, spec, threshold)], out_path)
}

for (spec_name in names(SPECS)) {
  cfg <- SPECS[[spec_name]]
  for (thr in THRESHOLDS) {
    cat(sprintf("--- %s @ k=%d (treatment=%s, lag=%d, c_mig_log=%s, scale=%s) ---\n",
                spec_name, thr, cfg$treatment, cfg$lag, cfg$c_mig_log, cfg$scale))
    # NEC panel
    for (y in NEC_PANEL_OUTCOMES) {
      fit <- fit_nec_panel(y, cfg$treatment, cfg$lag, cfg$c_mig_log, thr)
      base <- data.table(dataset = "nec_panel", outcome = y, spec = spec_name,
                         threshold = thr, lag = cfg$lag,
                         treatment_kind = cfg$treatment, c_mig_log_flag = cfg$c_mig_log,
                         scale_form = cfg$scale)
      if (is.null(fit))             { base[, err := "NULL/degenerate"] }
      else if (!is.null(fit$err))   { base[, err := fit$err] }
      else base[, `:=`(beta=fit$beta, se=fit$se, pval=fit$pval, stars=stars_fn(fit$pval),
                       n=fit$n, n_muni=fit$n_muni, mean_y=fit$mean_y, sd_y=fit$sd_y, err="")]
      all_rows[[length(all_rows)+1]] <- base
    }
    # NEC cs
    for (y in NEC_CS_OUTCOMES) {
      fit <- fit_nec_cs(y, cfg$treatment, cfg$lag, cfg$c_mig_log, thr)
      base <- data.table(dataset = "nec_cs", outcome = y, spec = spec_name,
                         threshold = thr, lag = cfg$lag,
                         treatment_kind = cfg$treatment, c_mig_log_flag = cfg$c_mig_log,
                         scale_form = cfg$scale)
      if (is.null(fit))             { base[, err := "NULL/degenerate"] }
      else if (!is.null(fit$err))   { base[, err := fit$err] }
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

# Quick summary
out <- fread(out_path)
cat("\n========== Rows per (dataset × scale × lag) ==========\n")
print(out[, .N, by = .(dataset, scale_form, lag)][order(dataset, scale_form, lag)],
      nrows = 200)
