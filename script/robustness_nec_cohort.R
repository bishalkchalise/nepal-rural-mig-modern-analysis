# =============================================================================
# script/robustness_nec_cohort.R
#
# Runs the same 33-spec robustness grid (3 scales × 11 lags 0-10) on the
# two NEW cohort-restricted 2018 firm-stock files:
#   nec_cs_post2001  →  firms founded 2001–2018 (whole treatment cohort)
#   nec_cs_post2011  →  firms founded 2011–2018 (Gulf-boom cohort)
#
# These complement the existing nec_cs analysis (which uses ALL 2018
# firms regardless of founding year).  Comparing the three lets us see
# whether the lag-strengthening pattern we saw is driven by pre-shock
# incumbents clearing out, or by the cumulative shock effect on the
# treatment-period firms themselves.
#
# Output: output/tab/robustness_nec_cohort.csv  (same column schema as
# robustness_nec.csv with a `dataset` field of nec_cs_post2001 / _post2011)
#
# Wall-clock: ~20–30 min (cross-section, fast).
# Crash-tolerant: writes partial CSV after each (spec × threshold).
# =============================================================================

suppressPackageStartupMessages({
  library(fixest); library(data.table)
})
source("script/_specs_lib.R")
options(scipen = 999); setDTthreads(0)
ROOT <- normalizePath(".")

# -----------------------------------------------------------------------------
# 1. Spec grid (same as robustness_nec.R)
# -----------------------------------------------------------------------------
SCALES <- list(
  log_lin = list(treatment = "log_int", c_mig_log = FALSE, label = "log/lin"),
  log_log = list(treatment = "log_int", c_mig_log = TRUE,  label = "log/log"),
  lin_lin = list(treatment = "lin_int", c_mig_log = FALSE, label = "lin/lin")
)
LAGS <- 0:10
THRESHOLDS <- c(0L, 25L, 50L, 100L)

SPECS <- list()
for (sk in names(SCALES)) {
  scfg <- SCALES[[sk]]
  for (L in LAGS) {
    nm <- if (sk == "log_lin" && L == 0L) "S0_baseline"
          else sprintf("S_%s_lag%d", sk, L)
    SPECS[[nm]] <- list(treatment = scfg$treatment,
                        c_mig_log = scfg$c_mig_log,
                        lag       = as.integer(L),
                        scale     = scfg$label)
  }
}

# -----------------------------------------------------------------------------
# 2. Load the two cohort cross-section files
# -----------------------------------------------------------------------------
load_cohort <- function(label, path) {
  d <- fread(path)
  d[, dataset := label]
  d
}
nec_cohorts <- list(
  nec_cs_post2001 = load_cohort("nec_cs_post2001",
                                "output/tab/mun_cohort_stock_post2001.csv"),
  nec_cs_post2011 = load_cohort("nec_cs_post2011",
                                "output/tab/mun_cohort_stock_post2011.csv")
)

# Outcomes — use the columns we built in build_nec_cohort_stocks.R
COHORT_OUTCOMES <- c(
  # Scale (counts + log)
  "n_firms","log_n_firms","emp_total","log_emp_total",
  "rev_total","log_rev_total","cap_total","log_cap_total",
  "mean_emp_per_firm",
  # Size distribution
  "n_firms_size_1_worker","n_firms_size_2_9_workers",
  "n_firms_size_10_50_workers","n_firms_size_51plus_workers",
  "share_size_1_worker","share_size_2_9_workers",
  "share_size_10_50_workers","share_size_51plus_workers",
  # Industry shares (grouped)
  "share_agriculture","share_manufacturing","share_construction",
  "share_trade_retail","share_transport","share_hospitality",
  "share_finance_prof_info","share_social_services",
  "share_other_services","share_utilities_mining",
  # Composition
  "industry_diversity","industry_hhi","n_industries_present",
  # Firm-level age
  "median_firm_age_years_wmean"
)
# Filter to columns that actually exist in the data
COHORT_OUTCOMES <- intersect(COHORT_OUTCOMES, names(nec_cohorts[[1]]))

# -----------------------------------------------------------------------------
# 3. Estimator
# -----------------------------------------------------------------------------
zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}
inst <- load_instrument()
bxA  <- build_block_A()
BLOCK_A      <- bxA$bx
BLOCK_A_COLS <- bxA$cols

make_treatment <- function(d, treatment_kind) {
  if (treatment_kind == "lin_int") d[, treatment := fx_z * mig_int_z]
  else                             d[, treatment := fx_z * log_migint_z]
  d
}

fit_cohort_cs <- function(cs, outcome, treatment_kind, lag_L, c_mig_log,
                          threshold, cluster = "DIST") {
  yr <- 2018L - as.integer(lag_L)
  inst_use <- inst[year == yr, .(lgcode, fxshock, mig_intensity, total_migrants)]
  if (nrow(inst_use) == 0) return(list(err = sprintf("no FX for year %d", yr)))
  inst_use[, log_mig_intensity := log(mig_intensity + 1e-8)]
  d0 <- merge(cs, inst_use, by = "lgcode", suffixes = c("",".inst"))
  d0 <- d0[total_migrants >= threshold]
  if (nrow(d0) < 30) return(NULL)
  d0[, fx_z         := zscore(fxshock)]
  d0[, mig_int_z    := zscore(mig_intensity)]
  d0[, log_migint_z := zscore(log_mig_intensity)]
  if (!is.null(BLOCK_A)) d0 <- merge(d0, BLOCK_A, by = "lgcode", all.x = TRUE)
  d <- d0[!is.na(get(outcome)) & !is.na(fx_z)]
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
    o %in% c("n_firms","log_n_firms","emp_total","log_emp_total",
             "rev_total","log_rev_total","cap_total","log_cap_total",
             "mean_emp_per_firm"),                       "Firm count & scale (cohort)",
    grepl("^n_firms_size_|^share_size_", o),             "Firm size distribution (cohort)",
    grepl("^share_(agriculture|manufacturing|construction|trade_retail|hospitality|transport|finance|social_services|other_services|utilities_mining)", o),
                                                          "Industry shares (cohort)",
    o %in% c("industry_diversity","industry_hhi","n_industries_present"),
                                                          "Industry composition (cohort)",
    grepl("firm_age", o),                                 "Firm age (cohort)",
    default = "Other (cohort)"
  )
}
detect_scale <- function(o, m) {
  if (grepl("^log_|_log$", o)) return("log")
  if (grepl("(_total$|_per_)", o) && !grepl("share_|n_firms", o)) return("rs")
  if (!is.na(m) && abs(m) <= 1 && m >= 0) return("share")
  if (grepl("(diversity|hhi|index|n_industries|firm_age)", o)) return("index")
  if (grepl("^(n_firms|^new_firms|count)", o)) return("count")
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
  else if (scale == "rs") sprintf("%+s units%s",
                                  formatC(round(b), format="d", big.mark=","), pct())
  else if (scale == "share") sprintf("%+.2fpp%s", b*100, pct())
  else if (scale == "index") sprintf("%+.3f index pts (baseline %.3f)", b, m)
  else if (scale == "count") sprintf("%+.3f units%s", b, pct())
  else                       sprintf("%+.3f (baseline %.3f)", b, m)
}

# -----------------------------------------------------------------------------
# 5. Loop with incremental save
# -----------------------------------------------------------------------------
out_path <- file.path(ROOT, "output/tab/robustness_nec_cohort.csv")
dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
all_rows <- list(); t_start <- Sys.time()

cat("========== robustness_nec_cohort ==========\n")
cat(sprintf("Specs:      %d (3 scales × 11 lags)\n", length(SPECS)))
cat(sprintf("Thresholds: %s\n", paste(THRESHOLDS, collapse=", ")))
cat(sprintf("Datasets:   %s\n", paste(names(nec_cohorts), collapse=", ")))
cat(sprintf("Outcomes:   %d  (per dataset)\n", length(COHORT_OUTCOMES)))
total_cells <- length(SPECS) * length(THRESHOLDS) * length(nec_cohorts) * length(COHORT_OUTCOMES)
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
  fwrite(d[order(dataset, outcome_group, outcome, spec, threshold)], out_path)
}

for (spec_name in names(SPECS)) {
  cfg <- SPECS[[spec_name]]
  for (thr in THRESHOLDS) {
    for (ds_name in names(nec_cohorts)) {
      cs <- nec_cohorts[[ds_name]]
      for (y in COHORT_OUTCOMES) {
        fit <- fit_cohort_cs(cs, y, cfg$treatment, cfg$lag, cfg$c_mig_log, thr)
        base <- data.table(dataset = ds_name, outcome = y, spec = spec_name,
                           threshold = thr, lag = cfg$lag,
                           treatment_kind = cfg$treatment,
                           c_mig_log_flag = cfg$c_mig_log,
                           scale_form = cfg$scale)
        if (is.null(fit))           { base[, err := "NULL/degenerate"] }
        else if (!is.null(fit$err)) { base[, err := fit$err] }
        else base[, `:=`(beta=fit$beta, se=fit$se, pval=fit$pval,
                         stars=stars_fn(fit$pval),
                         n=fit$n, n_muni=fit$n_muni,
                         mean_y=fit$mean_y, sd_y=fit$sd_y, err="")]
        all_rows[[length(all_rows)+1]] <- base
      }
    }
    cat(sprintf("  %s @ k=%d (both cohorts) — %.1f min elapsed.  Writing partial CSV…\n",
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
