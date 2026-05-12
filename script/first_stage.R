# =============================================================================
# script/first_stage.R — first-stage evidence that the FX-driven shock
# moves the things it must move BEFORE we believe downstream outcomes.
#
# Two pieces of evidence:
#
#   (1) Census — does the shock raise the share of HHs with at least one
#       absentee member?  `absent_hh_share` is the closest census-level
#       proxy for outmigration available across 2001 / 2011 / 2021.  It
#       lumps internal + international absentees but in rural Nepal the
#       overwhelming majority of absent HH members are working abroad,
#       so it is a defensible first-stage proxy.
#
#   (2) HRVS HH panel — does the shock raise the probability that a HH
#       reports receiving any remittance, and the rupee amount?
#       Variables: remittance_any, remittance_amt  (full sample, 18,056
#       HH-years; not conditional on having a migrant — that's the right
#       sample for an ITT-style first stage).
#
# Both panels run at the preferred specification:
#   treatment = log_int   ( fx_z * log(mig_int_z) )
#   c_mig = TRUE          (year x mig_intensity trend partialled out)
#   c_fx  = TRUE          (year x fx trend partialled out)
#   Block A = TRUE        (dest-weighted baseline X x year)
#   Block B = FALSE       (origin baseline X off, matches user preference)
#   Block C = TRUE        (trade SSIV level controls)
#
# All four migrant-count thresholds reported (0 / 25 / 50 / 100).
#
# Outputs:
#   output/tab/first_stage_census_absentee.csv
#   output/tab/first_stage_hh_remittance.csv
# Run:
#   source("script/first_stage.R")
# =============================================================================

source("script/_specs_lib.R")

THR <- c(0L, 25L, 50L, 100L)

cat("\n#### FIRST STAGE — census absentee share ####\n")
census_rows <- list()
for (thr in THR) {
  r <- run_spec(
    spec_label = "first_stage_pref",
    dataset    = "census",
    threshold  = thr,
    treatment  = "log_int",
    c_mig      = TRUE,
    c_fx       = TRUE,
    c_block_a  = TRUE,
    c_block_b  = FALSE,
    c_block_c  = TRUE,
    outcomes   = list("Absent HH" = c("absent_hh_share","hh_death_12m")),
    output_path = file.path(ROOT, "output", "tab",
                            sprintf("first_stage_census_absentee_thr%d.csv", thr))
  )
  if (!is.null(r)) census_rows[[length(census_rows)+1]] <- r
}
census_first <- rbindlist(census_rows, fill = TRUE)
fwrite(census_first, file.path(ROOT, "output", "tab",
                               "first_stage_census_absentee.csv"))


cat("\n\n#### FIRST STAGE — HH remittance receipt + amount ####\n")
hh_rows <- list()
for (thr in THR) {
  r <- run_spec(
    spec_label = "first_stage_pref",
    dataset    = "hh",
    threshold  = thr,
    treatment  = "log_int",
    c_mig      = TRUE,
    c_fx       = TRUE,
    c_block_a  = TRUE,
    c_block_b  = FALSE,
    c_block_c  = TRUE,
    outcomes   = list("HH remittance" = c("remittance_any","remittance_amt")),
    output_path = file.path(ROOT, "output", "tab",
                            sprintf("first_stage_hh_remit_thr%d.csv", thr))
  )
  if (!is.null(r)) hh_rows[[length(hh_rows)+1]] <- r
}
hh_first <- rbindlist(hh_rows, fill = TRUE)
fwrite(hh_first, file.path(ROOT, "output", "tab",
                           "first_stage_hh_remittance.csv"))


# --- Combined summary printout ---
cat("\n\n", strrep("=", 70), "\n", sep = "")
cat(" FIRST-STAGE SUMMARY (preferred spec, all thresholds)\n")
cat(strrep("=", 70), "\n", sep = "")

cat("\n[1] CENSUS — absent_hh_share (mean", round(mean(census_first$mean_y, na.rm=TRUE), 3), ")\n")
print(census_first[outcome == "absent_hh_share",
                   .(threshold, beta, stars, beta_pp, pct_of_mean, se, pval, n, n_muni)],
      digits = 4)

cat("\n[1b] CENSUS — hh_death_12m (placebo-ish — should NOT be moved by shock)\n")
print(census_first[outcome == "hh_death_12m",
                   .(threshold, beta, stars, beta_pp, pct_of_mean, se, pval, n, n_muni)],
      digits = 4)

cat("\n[2] HRVS HH — remittance_any (any remit, mean", round(mean(hh_first[outcome=='remittance_any']$mean_y, na.rm=TRUE), 3), ")\n")
print(hh_first[outcome == "remittance_any",
               .(threshold, beta, stars, beta_pp, pct_of_mean, se, pval, n, n_unit, n_muni)],
      digits = 4)

cat("\n[3] HRVS HH — remittance_amt (Rs, mean", round(mean(hh_first[outcome=='remittance_amt']$mean_y, na.rm=TRUE), 1), ")\n")
print(hh_first[outcome == "remittance_amt",
               .(threshold, beta, stars, pct_of_mean, se, pval, n, n_unit, n_muni)],
      digits = 4)

cat("\nFiles saved:\n")
cat("  output/tab/first_stage_census_absentee.csv      (4 rows: 2 outcomes x 4 thresholds, wait — 8 rows)\n")
cat("  output/tab/first_stage_hh_remittance.csv         (8 rows: 2 outcomes x 4 thresholds)\n")
cat("  + per-threshold individual CSVs in output/tab/\n")
