# =============================================================================
# script/first_stage.R — first-stage evidence that the FX-driven shock
# moves outmigration (census) and remittance (HRVS HH) before any
# downstream interpretation.
#
# Preferred spec:  log_int treatment, c_mig=T, c_fx=T, Block A=T,
#                   Block B=F, Block C=T.  All four migrant-count
#                   thresholds reported in one consolidated table.
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
  c_block_c  = TRUE
)

run_quiet <- function(...) {
  # silence run_spec's per-call print blocks; keep its return value
  invisible(capture.output(
    res <- run_spec(...),
    file = nullfile()
  ))
  res
}

cat("Running first-stage regressions (silent, may take ~1 min) ...\n")

# ------------------ Census ------------------
census_rows <- list()
for (thr in THR) {
  r <- run_quiet(
    spec_label = CFG$spec_label, dataset = "census", threshold = thr,
    treatment  = CFG$treatment, c_mig = CFG$c_mig, c_fx = CFG$c_fx,
    c_block_a = CFG$c_block_a, c_block_b = CFG$c_block_b, c_block_c = CFG$c_block_c,
    outcomes   = list("first_stage" = "absent_hh_share"),
    output_path = file.path(ROOT, "output", "tab",
                            sprintf("first_stage_census_absentee_thr%d.csv", thr))
  )
  if (!is.null(r)) census_rows[[length(census_rows)+1]] <- r
}
census_first <- rbindlist(census_rows, fill = TRUE)
fwrite(census_first, file.path(ROOT, "output", "tab",
                               "first_stage_census_absentee.csv"))

# ------------------ HH ------------------
hh_rows <- list()
for (thr in THR) {
  r <- run_quiet(
    spec_label = CFG$spec_label, dataset = "hh", threshold = thr,
    treatment  = CFG$treatment, c_mig = CFG$c_mig, c_fx = CFG$c_fx,
    c_block_a = CFG$c_block_a, c_block_b = CFG$c_block_b, c_block_c = CFG$c_block_c,
    outcomes   = list(
      "full sample" = c("remittance_any","remittance_amt"),
      "migrant HHs only" = c("remit_received","remit_amount_12m_rs","remit_amount_intl_12m_rs")
    ),
    output_path = file.path(ROOT, "output", "tab",
                            sprintf("first_stage_hh_remit_thr%d.csv", thr))
  )
  if (!is.null(r)) hh_rows[[length(hh_rows)+1]] <- r
}
hh_first <- rbindlist(hh_rows, fill = TRUE)
fwrite(hh_first, file.path(ROOT, "output", "tab",
                           "first_stage_hh_remittance.csv"))


# ============================================================================
# One consolidated table.
# ============================================================================
combined <- rbind(census_first, hh_first, fill = TRUE)

# Order: census first, then HH unconditional, then HH intensive margin.
# Within each, outcomes in a fixed reporting order; within each outcome,
# thresholds 0/25/50/100 in order.
ord_outcomes <- c(
  "absent_hh_share",                # census
  "remittance_any","remittance_amt",# HH unconditional
  "remit_received","remit_amount_12m_rs","remit_amount_intl_12m_rs"  # HH intensive
)
combined[, outcome := factor(outcome, levels = ord_outcomes)]
combined <- combined[order(outcome, threshold)]

# Sample-group label per outcome for clarity in the printed table
sample_lbl <- c(
  "absent_hh_share"          = "census (muni)",
  "remittance_any"           = "HH full",
  "remittance_amt"           = "HH full",
  "remit_received"           = "HH migrant-only",
  "remit_amount_12m_rs"      = "HH migrant-only",
  "remit_amount_intl_12m_rs" = "HH migrant-only"
)
combined[, sample := sample_lbl[as.character(outcome)]]

cat("\n\n", strrep("=", 78), "\n",
    " FIRST STAGE — preferred spec across 4 thresholds\n",
    strrep("=", 78), "\n", sep = "")

print(
  combined[, .(
    outcome,
    sample,
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
