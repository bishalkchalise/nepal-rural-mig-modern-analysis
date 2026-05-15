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

# Force-load the census panel into the cache, then add derived columns.
# Individual sectors (presentation-ready):
#   agri    = ind_agri_forestry_fish                  (primary)
#   manuf   = ind_manufacturing                       (secondary)
#   constr  = ind_construction                        (secondary)
#   trade   = ind_wholesale_retail                    (tertiary)
#   serv    = sum of the remaining 7 tertiary cats    (tertiary residual)
# Aggregates (ISIC-style):
#   pri_share = ind_agri_forestry_fish
#   sec_share = ind_manufacturing + ind_construction
#   ter_share = sum of 8 tertiary categories
census <- load_census()
census[, `:=`(
  # Disaggregated sectors
  agri_share      = ind_agri_forestry_fish,
  manuf_share     = ind_manufacturing,
  constr_share    = ind_construction,
  trade_share     = ind_wholesale_retail,
  transport_share = ind_transport_accommodation,
  finance_share   = ind_finance_real_estate_prof,
  public_share    = ind_public_admin_defence,
  education_share = ind_education,
  health_share    = ind_health,
  arts_share      = ind_arts_recreation,
  others_share    = ind_others,
  # ISIC aggregates
  pri_share = ind_agri_forestry_fish,
  sec_share = ind_manufacturing + ind_construction,
  ter_share = ind_wholesale_retail + ind_transport_accommodation +
              ind_finance_real_estate_prof + ind_public_admin_defence +
              ind_education + ind_health + ind_arts_recreation + ind_others
)]

cat(sprintf(
"Sector means (across muni-years):
  agriculture        %.3f
  manufacturing      %.3f
  construction       %.3f
  trade (retail)     %.3f
  transport          %.3f
  finance            %.3f
  public admin       %.3f
  education          %.3f
  health             %.3f
  arts/recreation    %.3f
  others             %.3f
  ---
  primary            %.3f   (= agriculture)
  secondary          %.3f   (= manuf + constr)
  tertiary           %.3f   (= trade + transport + finance + public + edu + health + arts + others)
  sum                %.3f
",
  mean(census$agri_share, na.rm=TRUE),
  mean(census$manuf_share, na.rm=TRUE),
  mean(census$constr_share, na.rm=TRUE),
  mean(census$trade_share, na.rm=TRUE),
  mean(census$transport_share, na.rm=TRUE),
  mean(census$finance_share, na.rm=TRUE),
  mean(census$public_share, na.rm=TRUE),
  mean(census$education_share, na.rm=TRUE),
  mean(census$health_share, na.rm=TRUE),
  mean(census$arts_share, na.rm=TRUE),
  mean(census$others_share, na.rm=TRUE),
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
    outcomes   = list(
      "Individual sectors" = c(
        "agri_share","manuf_share","constr_share",
        "trade_share","transport_share","finance_share",
        "public_share","education_share","health_share",
        "arts_share","others_share"
      ),
      "Aggregates" = c("pri_share","sec_share","ter_share")
    ),
    save       = FALSE
  )
  if (!is.null(r)) rows[[length(rows)+1]] <- r
}
out <- rbindlist(rows, fill = TRUE)

# Sort for display: disaggregated sectors first, then aggregates
out[, outcome := factor(outcome,
       levels = c("agri_share","manuf_share","constr_share",
                  "trade_share","transport_share","finance_share",
                  "public_share","education_share","health_share",
                  "arts_share","others_share",
                  "pri_share","sec_share","ter_share"))]
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
