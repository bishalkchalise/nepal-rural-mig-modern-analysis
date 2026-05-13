# =============================================================================
# script/robustness_firm_log_lag.R
#
# Three specs on the NEC PANEL only: combine the aggressive
# `year × log(mig_int_z)` control with FX lags of 2, 3, 5 years.
# Same outcome universe + same column schema as robustness_test.csv so the
# results plug into the same analysis.
#
# Why this matters:
#   `log_mig_int_z` is time-invariant (2001 baseline) so "lagging the share"
#   is a no-op.  The substantive question is whether lagging the SHIFTER
#   (fx) breaks the collinearity between treatment and `year × log_mig_int_z`
#   enough to identify a firm-entry effect under that aggressive control.
#
# Output: output/tab/robustness_firm_log_lag.csv
# Wall-clock: ~5 minutes (firm panel only, 14 outcomes × 3 specs).
# =============================================================================

suppressPackageStartupMessages({
  library(fixest); library(data.table)
})
source("script/_specs_lib.R")
options(scipen = 999); setDTthreads(0)
ROOT <- normalizePath(".")

SPECS <- list(
  S12_cmig_log_lag2 = list(lag = 2L, c_mig_log = TRUE, c_mig = TRUE, c_fx = TRUE, c_block_a = TRUE),
  S13_cmig_log_lag3 = list(lag = 3L, c_mig_log = TRUE, c_mig = TRUE, c_fx = TRUE, c_block_a = TRUE),
  S14_cmig_log_lag5 = list(lag = 5L, c_mig_log = TRUE, c_mig = TRUE, c_fx = TRUE, c_block_a = TRUE)
)
THRESHOLD <- 25L

# Load NEC panel + log columns
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
NEC_PANEL_OUTCOMES <- intersect(c(
  "log_new_firms",
  "log_new_firms_size_1_worker","log_new_firms_size_2_9_workers",
  "log_new_firms_size_10_50_workers","log_new_firms_size_51plus_workers",
  "log_new_firms_agriculture","log_new_firms_manufacturing","log_new_firms_construction",
  "log_new_firms_trade_retail","log_new_firms_hospitality_food",
  "log_new_firms_transport_storage","log_new_firms_other_services",
  "log_new_firms_finance_prof_realestate","log_new_firms_education_health_social"
), names(nec_p))

# Helpers
zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}
build_year_dummies <- function(d, X_col, prefix, ref_year) {
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

fit <- function(outcome, lag_L, c_mig_log, threshold = 25L) {
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
  d <- copy(d)
  d[, treatment := fx_z * log_migint_z]
  x_mig <- if (c_mig_log) "log_migint_z" else "mig_int_z"
  year_cols <- character(0)
  year_cols <- c(year_cols, build_year_dummies(d, x_mig,  "cmig", 2001L))
  year_cols <- c(year_cols, build_year_dummies(d, "fx_z", "cfx",  2001L))
  for (k in BLOCK_A_COLS)
    year_cols <- c(year_cols, build_year_dummies(d, k, paste0("cA_", k), 2001L))
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

stars_fn <- function(p) fifelse(is.na(p), "",
                                 fifelse(p < .01, "***",
                                 fifelse(p < .05, "**",
                                 fifelse(p < .10, "*", ""))))

# Loop
rows <- list(); t_start <- Sys.time()
cat("========== robustness_firm_log_lag ==========\n")
cat(sprintf("Specs:    %d  (%s)\n", length(SPECS), paste(names(SPECS), collapse=", ")))
cat(sprintf("Outcomes: %d  (NEC panel only)\n\n", length(NEC_PANEL_OUTCOMES)))

for (spec_name in names(SPECS)) {
  cfg <- SPECS[[spec_name]]
  cat(sprintf("--- %s (lag=%d, c_mig_log=%s) ---\n", spec_name, cfg$lag, cfg$c_mig_log))
  for (y in NEC_PANEL_OUTCOMES) {
    r <- fit(y, cfg$lag, cfg$c_mig_log, THRESHOLD)
    base <- data.table(dataset = "nec_panel", outcome = y, spec = spec_name,
                       threshold = THRESHOLD, lag = cfg$lag, c_mig_log_flag = cfg$c_mig_log)
    if (is.null(r))             { base[, err := "NULL/degenerate"] }
    else if (!is.null(r$err))   { base[, err := r$err] }
    else base[, `:=`(beta=r$beta, se=r$se, pval=r$pval, stars=stars_fn(r$pval),
                     n=r$n, n_muni=r$n_muni, mean_y=r$mean_y, sd_y=r$sd_y, err="")]
    rows[[length(rows)+1]] <- base
  }
  cat(sprintf("  done %s — %.1f min elapsed\n", spec_name,
              as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
}

# Interpretation columns (same logic as robustness_test.R)
interpret_beta <- function(b, o, m) {
  if (is.na(b)) return("")
  if (grepl("^log_", o)) sprintf("%+.1f%% change (log outcome)", b*100)
  else if (!is.na(m) && abs(m) <= 1 && m >= 0)
       sprintf("%+.2fpp (%+.0f%% of mean)", b*100, 100*b/m)
  else sprintf("%+.3f (baseline %.3f)", b, m)
}
out <- rbindlist(rows, fill = TRUE)
out[, outcome_group := fcase(
  outcome == "log_new_firms",              "Firm entry — all (NEC panel)",
  grepl("^log_new_firms_size_", outcome),  "Firm entry by size (NEC panel)",
  default                                = "Firm entry by industry (NEC panel)"
)]
out[, interpret := mapply(interpret_beta, beta, outcome, mean_y)]
KEEP <- c("outcome_group","dataset","outcome","spec","threshold","lag","c_mig_log_flag",
          "beta","stars","se","pval","mean_y","sd_y","n","n_muni","interpret","err")
out <- out[, intersect(KEEP, names(out)), with = FALSE]
out[, spec := factor(spec, levels = names(SPECS))]
out <- out[order(outcome_group, outcome, spec)]

dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(ROOT, "output/tab/robustness_firm_log_lag.csv")
fwrite(out, out_path)
cat(sprintf("\nWall-clock: %.1f min\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
cat("Saved: ", normalizePath(out_path, winslash = "/"), "\n", sep = "")

# Quick screen
cat("\n========== Firm entry coefficients (k>=25, year × log(mig_int) control) ==========\n")
print(out[, .(outcome, spec, beta = signif(beta,4), stars,
              se = signif(se,3), pval = signif(pval,3),
              n = n, interpret)],
      nrows = 60)
