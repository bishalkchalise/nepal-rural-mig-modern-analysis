# =============================================================================
# script/build_nec_cohort_stocks.R
#
# Build cohort-restricted 2018 firm-stock files from the per-cohort
# surviving-firms panel.  Two cutoffs:
#
#   post-2001  →  firms founded in 2001 or later, still operating in 2018
#                 (the entire treatment-period cohort)
#   post-2011  →  firms founded in 2011 or later, still operating in 2018
#                 (the Gulf-boom intensification cohort)
#
# Why we want this:
#   The existing files mun_industry_structure.csv, mun_productivity_*.csv,
#   mun_size_formality.csv aggregate over EVERY firm operating in 2018,
#   including incumbents founded before the FX shock.  Pre-treatment firms
#   dilute the estimated effect.  Restricting the stock to firms founded
#   during the treatment period isolates the cohort that could have been
#   affected.
#
# Caveat:
#   `entry_cohort_panel.csv` only tracks firms SURVIVING to 2018, so this
#   measures the joint effect of entry × early survival.  Firms that
#   entered after 2001 and exited before 2018 are invisible.  This is the
#   same limitation as the annual entry panel.
#
# Output:
#   output/tab/mun_cohort_stock_post2001.csv
#   output/tab/mun_cohort_stock_post2011.csv
#
# Same column schema as the existing 2018 stock files (n_firms, emp_total,
# rev_total, cap_total, share_size_*, n_firms_<industry>, share_<industry>,
# industry_diversity, industry_hhi, n_industries_present, plus the
# tradability / modernity / tech-tier marginals).
#
# Run from repo root:
#   source("script/build_nec_cohort_stocks.R")
# Wall-clock: < 1 minute.
# =============================================================================

suppressPackageStartupMessages({ library(data.table) })
options(scipen = 999)
ROOT <- normalizePath(".")

# -----------------------------------------------------------------------------
# 1.  Load per-cohort surviving-firms panel
# -----------------------------------------------------------------------------
panel <- fread("data/clean/nec2018/entry_cohort_panel.csv")
cat(sprintf("Loaded entry_cohort_panel: %d rows, %d munis, founding years %d–%d\n",
            nrow(panel), uniqueN(panel$lgcode),
            min(panel$founding_year_ad), max(panel$founding_year_ad)))

# -----------------------------------------------------------------------------
# 2.  Build cohort-restricted muni-level stock
# -----------------------------------------------------------------------------
build_cohort_stock <- function(min_year) {
  d <- panel[founding_year_ad >= min_year]
  # Identify the count / amount columns to sum vs ratio columns
  num_cols <- setdiff(names(d), c("lgcode","founding_year_ad","median_firm_age_years"))
  # All num_cols are counts/sums → just sum within muni
  agg <- d[, lapply(.SD, sum, na.rm = TRUE), by = lgcode, .SDcols = num_cols]
  # Add a single weighted-mean firm age (years) per muni
  age <- d[, .(median_firm_age_years_wmean =
                 sum(median_firm_age_years * n_firms_surviving, na.rm = TRUE) /
                 pmax(sum(n_firms_surviving, na.rm = TRUE), 1)),
           by = lgcode]
  agg <- merge(agg, age, by = "lgcode", all.x = TRUE)

  # Derived columns — match the existing mun_* file naming so downstream
  # robustness scripts can use these as a drop-in replacement.
  setnames(agg, "n_firms_surviving",       "n_firms")
  setnames(agg, "emp_surviving",           "emp_total")
  setnames(agg, "rev_surviving",           "rev_total")
  setnames(agg, "cap_surviving",           "cap_total")
  # Size columns: rename to match mun_size_formality.csv naming
  setnames(agg, "n_firms_surviving_size_micro_1",       "n_firms_size_1_worker")
  setnames(agg, "n_firms_surviving_size_small_2_9",     "n_firms_size_2_9_workers")
  setnames(agg, "n_firms_surviving_size_medium_10_50",  "n_firms_size_10_50_workers")
  setnames(agg, "n_firms_surviving_size_large_51p",     "n_firms_size_51plus_workers")
  # Industry columns: rename to short sector names (matching mun_industry_structure.csv)
  ind_map <- c(
    n_firms_surviving_sec_manuf       = "n_firms_manufacturing",
    n_firms_surviving_sec_education   = "n_firms_education",
    n_firms_surviving_sec_services    = "n_firms_other_services",
    n_firms_surviving_sec_transport   = "n_firms_transport",
    n_firms_surviving_sec_wholesale   = "n_firms_trade_retail",
    n_firms_surviving_sec_health      = "n_firms_health",
    n_firms_surviving_sec_hospitality = "n_firms_hospitality",
    n_firms_surviving_sec_construct   = "n_firms_construction",
    n_firms_surviving_sec_water_waste = "n_firms_water_waste",
    n_firms_surviving_sec_agro        = "n_firms_agriculture",
    n_firms_surviving_sec_arts        = "n_firms_arts",
    n_firms_surviving_sec_energy      = "n_firms_energy",
    n_firms_surviving_sec_finance     = "n_firms_finance",
    n_firms_surviving_sec_prof_tech   = "n_firms_prof_tech",
    n_firms_surviving_sec_admin_sup   = "n_firms_admin_sup",
    n_firms_surviving_sec_info_comm   = "n_firms_info_comm",
    n_firms_surviving_sec_mining      = "n_firms_mining",
    n_firms_surviving_sec_real_estate = "n_firms_real_estate"
  )
  setnames(agg, names(ind_map)[names(ind_map) %in% names(agg)],
                ind_map[names(ind_map) %in% names(agg)])
  # Compose grouped industries to match mun_industry_structure.csv
  agg[, n_firms_finance_prof_info :=
        (n_firms_finance %||% 0) + (n_firms_prof_tech %||% 0) + (n_firms_info_comm %||% 0) +
        (n_firms_real_estate %||% 0)]
  agg[, n_firms_social_services :=
        (n_firms_education %||% 0) + (n_firms_health %||% 0)]
  agg[, n_firms_utilities_mining :=
        (n_firms_water_waste %||% 0) + (n_firms_energy %||% 0) + (n_firms_mining %||% 0)]
  # Shares (of total firms)
  total <- pmax(agg$n_firms, 1)
  for (g in c("agriculture","manufacturing","construction","trade_retail",
              "transport","hospitality","finance_prof_info","social_services",
              "other_services","utilities_mining"))
    agg[, (paste0("share_",g)) := get(paste0("n_firms_",g)) / total]
  # Size shares
  for (s in c("1_worker","2_9_workers","10_50_workers","51plus_workers"))
    agg[, (paste0("share_size_",s)) := get(paste0("n_firms_size_",s)) / total]
  # Industry diversity / HHI (over the 10 grouped sectors above)
  ind_share_cols <- c("share_agriculture","share_manufacturing","share_construction",
                      "share_trade_retail","share_transport","share_hospitality",
                      "share_finance_prof_info","share_social_services",
                      "share_other_services","share_utilities_mining")
  agg[, industry_hhi := rowSums(.SD^2, na.rm = TRUE), .SDcols = ind_share_cols]
  agg[, industry_diversity := 1 - industry_hhi]
  agg[, n_industries_present := rowSums(.SD > 0, na.rm = TRUE), .SDcols = ind_share_cols]
  # Mean emp per firm
  agg[, mean_emp_per_firm := emp_total / pmax(n_firms, 1)]
  # log transforms (log1p) — force numeric to avoid silent failures when
  # data.table reads big counts as integer64 (log1p on integer64 returns
  # the original value as a double rather than computing the log).
  for (v in c("n_firms","emp_total","rev_total","cap_total"))
    agg[, (paste0("log_", v)) := log1p(pmax(as.numeric(get(v)), 0))]
  # Derive DIST
  agg[, DIST := lgcode %/% 100]
  agg
}

`%||%` <- function(a, b) if (is.null(a)) b else a

post2001 <- build_cohort_stock(2001L)
post2011 <- build_cohort_stock(2011L)

# -----------------------------------------------------------------------------
# 3.  Save
# -----------------------------------------------------------------------------
out_dir <- file.path(ROOT, "output/tab")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fwrite(post2001, file.path(out_dir, "mun_cohort_stock_post2001.csv"))
fwrite(post2011, file.path(out_dir, "mun_cohort_stock_post2011.csv"))

cat(sprintf("\nWrote:\n  %s\n  %s\n",
            file.path(out_dir, "mun_cohort_stock_post2001.csv"),
            file.path(out_dir, "mun_cohort_stock_post2011.csv")))

# -----------------------------------------------------------------------------
# 4.  Quick summary
# -----------------------------------------------------------------------------
cat("\n========== mun_cohort_stock_post2001 summary ==========\n")
cat(sprintf("  munis: %d\n", nrow(post2001)))
cat(sprintf("  total firms (post-2001 cohort):  %s\n",
            format(sum(post2001$n_firms), big.mark=",")))
cat(sprintf("  mean n_firms per muni:           %.1f\n", mean(post2001$n_firms)))
cat("\n========== mun_cohort_stock_post2011 summary ==========\n")
cat(sprintf("  munis: %d\n", nrow(post2011)))
cat(sprintf("  total firms (post-2011 cohort):  %s\n",
            format(sum(post2011$n_firms), big.mark=",")))
cat(sprintf("  mean n_firms per muni:           %.1f\n", mean(post2011$n_firms)))

# Comparison with existing stock file
existing <- fread("data/clean/nec2018/mun_industry_structure.csv")
cat("\n========== Comparison with existing 2018 full stock ==========\n")
cat(sprintf("  all 2018 firms:      %s\n", format(sum(existing$n_firms), big.mark=",")))
cat(sprintf("  post-2001 cohort:    %s  (%.0f%% of all)\n",
            format(sum(post2001$n_firms), big.mark=","),
            100*sum(post2001$n_firms)/sum(existing$n_firms)))
cat(sprintf("  post-2011 cohort:    %s  (%.0f%% of all)\n",
            format(sum(post2011$n_firms), big.mark=","),
            100*sum(post2011$n_firms)/sum(existing$n_firms)))
