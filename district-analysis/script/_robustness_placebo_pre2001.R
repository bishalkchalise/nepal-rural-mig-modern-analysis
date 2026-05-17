################################################################################
# Robustness: Pre-period placebo
# ---------------------------------------------------------------------------
# Take outcomes from 2001 (pre-migration-boom; before DOFE permits ramped up
# in 2003+ and before the 2008-onwards FX-shock window). Regress on the
# same SSIV z but at a POST-2001 year (e.g., z_2018), so the "shock" is in
# the FUTURE relative to the outcome.
#
# If β is insignificant: the SSIV doesn't pick up pre-trends / selection;
#   the headline 2011-2021 negative effect is identifying actual response.
# If β is significant: pre-period correlation casts doubt on the headline.
#
# Output: district-analysis/output/tab/robustness_placebo_pre2001.csv
#
# Run: source("district-analysis/script/_robustness_placebo_pre2001.R")
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

t0 <- Sys.time()
SKIP_RUN <- TRUE
suppressMessages(suppressWarnings({
  source("district-analysis/script/_robustness_all_panels.R")
}))

stars <- function(p) ifelse(is.na(p), "",
  ifelse(p<0.01,"***", ifelse(p<0.05,"**", ifelse(p<0.10,"*",""))))

# ---- 2001 outcomes -------------------------------------------------------
y01 <- census %>% filter(year == 2001)
cat(sprintf("2001 outcomes: %d districts\n", nrow(y01)))

# Outcomes to test: take whatever from CENSUS_OUTCOMES has 2001 coverage.
# (Some outcomes like mig_*_internal_share are 2011+2021 only -> auto-skipped.)
test_outs <- intersect(CENSUS_OUTCOMES, names(y01))
test_outs <- test_outs[sapply(test_outs, function(c) sum(!is.na(y01[[c]])) >= 50)]
cat(sprintf("Outcomes with >=50 districts of 2001 coverage: %d\n", length(test_outs)))

# ---- Placebo z's: pick "future" shock years ------------------------------
# Headline is z_lag2 -> for outcome year 2001, lag-2 z is from 1999 (not in data).
# Placebo: outcome at 2001 vs z from FUTURE years.
placebo_years <- c(2009, 2015, 2018, 2020)

out_rows <- list()
for (yr in placebo_years) {
  cat(sprintf("\n=== Placebo: outcome 2001 vs z at %d ===\n", yr))
  zfit <- z_v2 %>% filter(year == yr) %>% select(dname, z_v2)
  if (nrow(zfit) == 0) { cat("  no z values at year ", yr, "\n"); next }
  pn <- y01 %>%
    inner_join(zfit,  by = "dname") %>%
    inner_join(mi,    by = "dname") %>%
    left_join(regions, by = "dname") %>%
    mutate(across(any_of(REGION_COLS), ~ if_else(is.na(.x), 0, .x))) %>%
    mutate(z_std    = (z_v2 - mean(z_v2, na.rm = TRUE)) / sd(z_v2, na.rm = TRUE),
           mig_var  = log_mi_z,
           z_inter  = z_std * log_mi_z,
           z_bare   = z_std)
  for (yc in test_outs) {
    if (!yc %in% names(pn)) next
    y <- pn[[yc]]
    if (sum(!is.na(y)) < 50) next
    f <- as.formula(sprintf("%s ~ z_inter + mig_var + z_bare + %s",
                            yc, paste(REGION_COLS, collapse = " + ")))
    fit <- tryCatch(feols(f, data = pn, vcov = "hetero"), error = function(e) NULL)
    if (is.null(fit)) next
    s <- summary(fit)$coeftable
    if (!"z_inter" %in% rownames(s)) next
    out_rows[[length(out_rows)+1]] <- tibble(
      placebo_year = yr,
      outcome      = yc,
      beta = s["z_inter","Estimate"],
      se   = s["z_inter","Std. Error"],
      p    = s["z_inter","Pr(>|t|)"],
      sig  = stars(s["z_inter","Pr(>|t|)"]),
      n    = nobs(fit)
    )
  }
}

out <- bind_rows(out_rows)
write_csv(out, "district-analysis/output/tab/robustness_placebo_pre2001.csv")

cat(sprintf("\nWrote %d rows to robustness_placebo_pre2001.csv\n", nrow(out)))

# Per-year summary
cat("\n=== Sig-rate by placebo year (lower = better placebo pass) ===\n")
print(out %>% group_by(placebo_year) %>%
        summarise(n_outcomes = n(),
                  pct_sig_p10 = round(mean(p < 0.10, na.rm = TRUE) * 100, 1),
                  pct_sig_p05 = round(mean(p < 0.05, na.rm = TRUE) * 100, 1),
                  pct_sig_p01 = round(mean(p < 0.01, na.rm = TRUE) * 100, 1),
                  .groups = "drop"))
cat(sprintf("Elapsed: %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))
