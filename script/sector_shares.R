# =============================================================================
# script/sector_shares.R — primary / secondary / tertiary employment shares.
#
# Builds 3 aggregate sector-employment shares from the 11 individual
# census industry shares, then runs the preferred spec (log_int, c_mig=T,
# c_fx=T, Block A only, no Block B/C) at all four migrant thresholds.
#
# Aggregation:
#   primary    = ind_agri_forestry_fish
#   secondary  = ind_manufacturing + ind_construction
#   tertiary   = ind_wholesale_retail + ind_transport_accommodation
#              + ind_finance_real_estate_prof + ind_public_admin_defence
#              + ind_education + ind_health + ind_arts_recreation + ind_others
#
# 11 shares sum to 1.0 by construction → pri + sec + ter = 1.0.
#
# Output: output/tab/sector_shares.csv
# Run:    source("script/sector_shares.R")
# =============================================================================

source("script/_specs_lib.R")

# Force-load the census panel into the cache, then add three derived columns.
census <- load_census()
census[, pri_share := ind_agri_forestry_fish]
census[, sec_share := ind_manufacturing + ind_construction]
census[, ter_share := ind_wholesale_retail + ind_transport_accommodation +
                       ind_finance_real_estate_prof + ind_public_admin_defence +
                       ind_education + ind_health + ind_arts_recreation + ind_others]

cat(sprintf("Sector-share means (across muni-years):\n  primary  %.3f  |  secondary %.3f  |  tertiary  %.3f  |  sum %.3f\n",
            mean(census$pri_share, na.rm=TRUE),
            mean(census$sec_share, na.rm=TRUE),
            mean(census$ter_share, na.rm=TRUE),
            mean(census$pri_share + census$sec_share + census$ter_share, na.rm=TRUE)))

run_quiet <- function(...) {
  invisible(capture.output(res <- run_spec(...), file = nullfile()))
  res
}

rows <- list()
for (thr in c(0L, 25L, 50L, 100L)) {
  r <- run_quiet(
    spec_label = "sector_shares",
    dataset    = "census",
    threshold  = thr,
    treatment  = "log_int",
    c_mig      = TRUE, c_fx = TRUE,
    c_block_a  = TRUE, c_block_b = FALSE, c_block_c = FALSE,
    outcomes   = list("Sector shares" = c("pri_share","sec_share","ter_share")),
    save       = FALSE
  )
  if (!is.null(r)) rows[[length(rows)+1]] <- r
}
out <- rbindlist(rows, fill = TRUE)

# Sort for display: primary -> secondary -> tertiary, threshold ascending
out[, outcome := factor(outcome, levels = c("pri_share","sec_share","ter_share"))]
out <- out[order(outcome, threshold)]

dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
fwrite(out, file.path(ROOT, "output/tab/sector_shares.csv"))

cat("\n", strrep("=", 78), "\n",
    " Employment shares by sector — anchor spec, 4 thresholds\n",
    strrep("=", 78), "\n", sep = "")
print(out[, .(outcome, threshold,
              beta     = signif(beta,    4),
              stars,
              mean_y   = signif(mean_y,  4),
              beta_pp  = signif(beta_pp, 4),
              pct_mean = signif(pct_of_mean, 4),
              se       = signif(se,      3),
              pval     = signif(pval,    3),
              n        = n,
              n_muni   = n_muni)],
      nrows = nrow(out))
cat("\nSaved:  ", normalizePath(file.path(ROOT, "output/tab/sector_shares.csv"), winslash="/"), "\n", sep="")
