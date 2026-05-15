# =============================================================================
# script/education_lag.R — re-estimate education outcomes with lagged FX shock.
#
# For each outcome, we run the preferred spec (log_int, c_mig=T, c_fx=T,
# Block A only, threshold = 25) at three lag values:
#
#   L = 0   contemporaneous shock at the outcome year (current default)
#   L = 1   one-year-lagged shock  (outcome at year t merges with shock at t-1)
#   L = 2   two-year-lagged shock
#
# For the census panel (waves 2001/2011/2021) L=1 uses shocks at 2000/2010/2020;
# L=2 uses 1999/2009/2019 (drops the 2001 wave because 1999 shock is unavailable
# in the IMF FX panel).
#
# For the HRVS HH panel (2016/17/18) all three lags are fully available.
#
# Output: output/tab/education_lag.csv  (one CSV, one row per outcome x lag).
#
# Run from repo root:  source("script/education_lag.R")
# =============================================================================

source("script/_specs_lib.R")

LAGS <- c(0L, 1L, 2L)
CFG <- list(
  treatment  = "log_int",
  threshold  = 25L,
  c_mig      = TRUE,
  c_fx       = TRUE,
  c_block_a  = TRUE,
  c_block_b  = FALSE,
  c_block_c  = FALSE
)

# Outcome lists (only outcomes that exist in the data — see CENSUS_GROUPS /
# HH_GROUPS in _specs_lib.R for the full catalogues).
CENSUS_EDU <- c(
  "edu_literate", "edu_literate_female", "edu_literate_male",
  "edu_school_attend_6_16",
  "edu_attain_primary_plus", "edu_attain_secondary_plus",
  "edu_attain_higher_secondary_plus", "edu_attain_tertiary",
  "edu_years_mean"
)

HH_EDU <- c(
  "any_enrolled", "n_enrolled", "n_private_school", "n_scholarship",
  "edu_spend_total_12m", "edu_spend_per_enrolled",
  "edu_spend_tuition_12m", "edu_spend_books_12m"
)

run_quiet <- function(...) {
  invisible(capture.output(res <- run_spec(...), file = nullfile()))
  res
}

cat("Running education lag specs (silent) ...\n")

rows <- list()

for (L in LAGS) {
  cat(sprintf("  census  L=%d ...\n", L))
  r <- run_quiet(
    spec_label = sprintf("edu_L%d", L),
    dataset    = "census",
    threshold  = CFG$threshold,
    treatment  = CFG$treatment,
    c_mig      = CFG$c_mig,  c_fx = CFG$c_fx,
    c_block_a  = CFG$c_block_a, c_block_b = CFG$c_block_b, c_block_c = CFG$c_block_c,
    outcomes   = list("Education" = CENSUS_EDU),
    save       = FALSE,
    lag        = L
  )
  if (!is.null(r)) rows[[length(rows)+1]] <- r

  cat(sprintf("  hh      L=%d ...\n", L))
  r <- run_quiet(
    spec_label = sprintf("edu_L%d", L),
    dataset    = "hh",
    threshold  = CFG$threshold,
    treatment  = CFG$treatment,
    c_mig      = CFG$c_mig,  c_fx = CFG$c_fx,
    c_block_a  = CFG$c_block_a, c_block_b = CFG$c_block_b, c_block_c = CFG$c_block_c,
    outcomes   = list("Education spending" = HH_EDU),
    save       = FALSE,
    lag        = L
  )
  if (!is.null(r)) rows[[length(rows)+1]] <- r
}

combined <- rbindlist(rows, fill = TRUE)

# Order rows: dataset, outcome, lag
combined[, dataset := factor(dataset, levels = c("census","hh"))]
combined[, outcome := factor(outcome,
            levels = c(CENSUS_EDU, HH_EDU))]
combined <- combined[order(dataset, outcome, lag)]

dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(ROOT, "output/tab/education_lag.csv")
fwrite(combined, out_path)

cat("\n", strrep("=", 78), "\n",
    " EDUCATION outcomes -- lag robustness (preferred spec, thr = 25)\n",
    strrep("=", 78), "\n", sep = "")

print(
  combined[, .(
    dataset, outcome, lag,
    beta     = signif(beta,    4),
    stars,
    mean_y   = signif(mean_y,  4),
    beta_pp  = signif(beta_pp, 4),
    pct_mean = signif(pct_of_mean, 4),
    se       = signif(se,      3),
    pval     = signif(pval,    3),
    n        = n,
    n_unit   = n_unit,
    n_muni   = n_muni
  )],
  nrows = nrow(combined)
)

cat("\nSaved to:  ", normalizePath(out_path, winslash = "/"), "\n", sep = "")
