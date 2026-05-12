# =============================================================================
# script/first_stage.R — first-stage evidence + two tightening options:
#
#   (A) log(remit_amount_*) — semi-elasticity outcomes, tighter SE than Rs.
#   (B) muni FE instead of HH FE for the HH panel — cross-HH-within-muni
#       identification (looser within-HH but pools more variation).
#
# Preferred spec: log_int, c_mig=T, c_fx=T, Block A=T, Block B=F, Block C=F.
# Four migrant thresholds: 0, 25, 50, 100.
#
# Run:  source("script/first_stage.R")
# =============================================================================

source("script/_specs_lib.R")

THR <- c(0L, 25L, 50L, 100L)

CFG <- list(
  spec_label = "first_stage_pref",
  treatment  = "log_int",
  c_mig      = TRUE,
  c_fx       = TRUE,
  c_block_a  = TRUE,
  c_block_b  = FALSE,
  c_block_c  = FALSE
)

# Variables ordered for the final table.
OUT_CENSUS <- "absent_hh_share"
OUT_HH_FULL <- c("remittance_any","remittance_amt","log_remittance_amt")
OUT_HH_MIG  <- c(
  "remit_received",
  "remit_amount_12m_rs","log_remit_amount_12m_rs",
  "remit_amount_intl_12m_rs","log_remit_amount_intl_12m_rs",
  "n_migrants_total","log_n_migrants_total",
  "n_migrants_international","log_n_migrants_international"
)

run_quiet <- function(...) {
  invisible(capture.output(res <- run_spec(...), file = nullfile()))
  res
}

cat("Running first-stage regressions (silent, ~1-2 min) ...\n")

# ---------- Census ----------
census_rows <- list()
for (thr in THR) {
  r <- run_quiet(
    spec_label = CFG$spec_label, dataset = "census", threshold = thr,
    treatment  = CFG$treatment, c_mig = CFG$c_mig, c_fx = CFG$c_fx,
    c_block_a = CFG$c_block_a, c_block_b = CFG$c_block_b, c_block_c = CFG$c_block_c,
    outcomes   = list("first_stage" = OUT_CENSUS),
    output_path = file.path(ROOT, "output", "tab",
                            sprintf("first_stage_census_absentee_thr%d.csv", thr))
  )
  if (!is.null(r)) { r[, fe_struct := "lgcode + year"]; census_rows[[length(census_rows)+1]] <- r }
}
census_first <- rbindlist(census_rows, fill = TRUE)
fwrite(census_first, file.path(ROOT, "output", "tab",
                               "first_stage_census_absentee.csv"))

# ---------- HH (default: hhid + year FE) ----------
hh_rows_def <- list()
for (thr in THR) {
  r <- run_quiet(
    spec_label = CFG$spec_label, dataset = "hh", threshold = thr,
    treatment  = CFG$treatment, c_mig = CFG$c_mig, c_fx = CFG$c_fx,
    c_block_a = CFG$c_block_a, c_block_b = CFG$c_block_b, c_block_c = CFG$c_block_c,
    outcomes   = list(
      "full sample"      = OUT_HH_FULL,
      "migrant HHs only" = OUT_HH_MIG
    ),
    fe         = NULL,    # default: hhid + year
    output_path = file.path(ROOT, "output", "tab",
                            sprintf("first_stage_hh_remit_thr%d_hhFE.csv", thr))
  )
  if (!is.null(r)) { r[, fe_struct := "hhid + year"]; hh_rows_def[[length(hh_rows_def)+1]] <- r }
}
hh_first_def <- rbindlist(hh_rows_def, fill = TRUE)

# ---------- HH (Option B: lgcode + year FE) ----------
hh_rows_muni <- list()
for (thr in THR) {
  r <- run_quiet(
    spec_label = CFG$spec_label, dataset = "hh", threshold = thr,
    treatment  = CFG$treatment, c_mig = CFG$c_mig, c_fx = CFG$c_fx,
    c_block_a = CFG$c_block_a, c_block_b = CFG$c_block_b, c_block_c = CFG$c_block_c,
    outcomes   = list(
      "full sample"      = OUT_HH_FULL,
      "migrant HHs only" = OUT_HH_MIG
    ),
    fe         = "muni_year",   # lgcode + year  (drop HH FE)
    output_path = file.path(ROOT, "output", "tab",
                            sprintf("first_stage_hh_remit_thr%d_muniFE.csv", thr))
  )
  if (!is.null(r)) { r[, fe_struct := "lgcode + year"]; hh_rows_muni[[length(hh_rows_muni)+1]] <- r }
}
hh_first_muni <- rbindlist(hh_rows_muni, fill = TRUE)

hh_combined <- rbind(hh_first_def, hh_first_muni, fill = TRUE)
fwrite(hh_combined, file.path(ROOT, "output", "tab",
                              "first_stage_hh_remittance.csv"))

# ============================================================================
# One consolidated table
# ============================================================================
combined <- rbind(census_first, hh_combined, fill = TRUE)

ord_outcomes <- c(OUT_CENSUS, OUT_HH_FULL, OUT_HH_MIG)
combined[, outcome := factor(outcome, levels = ord_outcomes)]
combined[, fe_struct := factor(fe_struct,
                               levels = c("lgcode + year","hhid + year"))]
combined <- combined[order(outcome, fe_struct, threshold)]

# Sample-group label per outcome
sample_lbl <- c(
  "absent_hh_share"               = "census (muni)",
  "remittance_any"                = "HH full",
  "remittance_amt"                = "HH full",
  "log_remittance_amt"            = "HH full",
  "remit_received"                = "HH migrant-only",
  "remit_amount_12m_rs"           = "HH migrant-only",
  "log_remit_amount_12m_rs"       = "HH migrant-only",
  "remit_amount_intl_12m_rs"      = "HH migrant-only",
  "log_remit_amount_intl_12m_rs"  = "HH migrant-only",
  "n_migrants_total"              = "HH migrant-only",
  "log_n_migrants_total"          = "HH migrant-only",
  "n_migrants_international"      = "HH migrant-only",
  "log_n_migrants_international"  = "HH migrant-only"
)
combined[, sample := sample_lbl[as.character(outcome)]]

cat("\n\n", strrep("=", 90), "\n",
    " FIRST STAGE — preferred spec, two FE structures, 4 thresholds\n",
    strrep("=", 90), "\n", sep = "")

print(
  combined[, .(
    outcome,
    sample,
    fe_struct,
    threshold,
    beta      = signif(beta,    4),
    stars,
    beta_pp   = signif(beta_pp, 4),
    pct_mean  = signif(pct_of_mean, 4),
    se        = signif(se,      3),
    pval      = signif(pval,    3),
    n         = n,
    n_unit    = n_unit,
    n_muni    = n_muni
  )],
  nrows = nrow(combined)
)

cat("\nFiles saved:\n")
cat("  output/tab/first_stage_census_absentee.csv\n")
cat("  output/tab/first_stage_hh_remittance.csv\n")
