# =============================================================================
# script/robustness_specs_extra.R
#
# Additive robustness — drop progressively more controls to test the
# concern that the current spec is over-controlling on a 3-year panel.
#
# Variants (all at k>=25, full sample, contemporaneous treatment):
#   S8_no_cmig    : drop year × mig_int_z                     (keep year × fx_z, Block A × year)
#   S9_no_cfx     : drop year × fx_z                          (keep year × mig_int_z, Block A × year)
#   S10_only_blockA : drop year × mig_int_z AND year × fx_z   (keep Block A × year)
#   S11_FE_only   : drop all year-interacted controls         (just muni + year FE)
#
# All four are run identically to S0_baseline EXCEPT for the toggled controls.
# Run from repo root:
#   source("script/robustness_specs_extra.R")
# Wall-clock: 15–25 min depending on machine.
#
# Output: output/tab/robustness_specs_extra.csv
# Combine with robustness_specs.csv in downstream analysis (same column schema).
# =============================================================================

suppressPackageStartupMessages({
  library(fixest); library(data.table)
})
source("script/_specs_lib.R")
options(scipen = 999); setDTthreads(0)
ROOT <- normalizePath(".")

# -----------------------------------------------------------------------------
# 1. Spec list — only the new variants
# -----------------------------------------------------------------------------
SPECS <- list(
  S8_no_cmig      = list(c_mig = FALSE, c_fx = TRUE,  c_block_a = TRUE),
  S9_no_cfx       = list(c_mig = TRUE,  c_fx = FALSE, c_block_a = TRUE),
  S10_only_blockA = list(c_mig = FALSE, c_fx = FALSE, c_block_a = TRUE),
  S11_FE_only     = list(c_mig = FALSE, c_fx = FALSE, c_block_a = FALSE)
)
THRESHOLD <- 25L

# -----------------------------------------------------------------------------
# 2. Outcome catalogues (identical to robustness_specs.R)
# -----------------------------------------------------------------------------
build_outcomes <- function() {
  cen <- load_census()
  hh  <- load_hh()
  cen_id <- c("lgcode","year","district","district77","district_name")
  cen_num <- setdiff(names(cen)[sapply(cen, is.numeric)], cen_id)
  hh_id <- c("hhid","year","lgcode","district","district77","district_name",
             "wt_hh","psu","vdc","vmun_code","s00q03a","s00q03b","s00q03c",
             "member_id","fxshock","mig_intensity","log_mig_intensity",
             "total_migrants","fx_z","mig_int_z","log_migint_z")
  hh_num <- setdiff(names(hh)[sapply(hh, is.numeric)], hh_id)
  list(census = cen_num, hh = hh_num)
}

# -----------------------------------------------------------------------------
# 3. Load NEC panel and NEC cs (identical to robustness_specs.R)
# -----------------------------------------------------------------------------
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
parts <- lapply(c("mun_industry_structure","mun_productivity_profitability","mun_size_formality"),
                function(f) {
                  p <- file.path("data/clean/nec2018", paste0(f, ".csv"))
                  if (file.exists(p)) fread(p) else NULL
                })
parts <- parts[!sapply(parts, is.null)]
nec_cs <- Reduce(function(a, b) {
  new_cols <- setdiff(names(b), names(a))
  if (length(new_cols)) merge(a, b[, c("lgcode", new_cols), with = FALSE],
                              by = "lgcode", all = TRUE) else a
}, parts)
nec_cs[, DIST := lgcode %/% 100]
for (v in c("n_firms","emp_total","rev_total","value_added_total","cap_total",
            "exp_total","profit_proxy_total")) {
  if (v %in% names(nec_cs) && !(paste0("log_", v) %in% names(nec_cs)))
    nec_cs[, (paste0("log_", v)) := log1p(pmax(get(v), 0))]
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

NEC_CS_OUTCOMES <- intersect(c(
  "log_n_firms","log_emp_total","log_rev_total","log_value_added_total",
  "log_cap_total","log_exp_total","log_profit_proxy_total",
  "mean_value_added_per_worker","median_value_added_per_worker",
  "mean_rev_per_worker","mean_capital_per_worker","mean_profit_per_worker",
  "mean_profit_margin","wage_share_of_revenue","value_added_share_of_revenue",
  "capital_intensity_aggregate",
  "formality_index","share_registered","share_tax_registered",
  "share_keeps_accounts","share_operates_year_round",
  "share_borrowed","share_uses_formal_credit","share_has_foreign_capital",
  "share_female_led","mean_emp_per_firm",
  "share_size_1_worker","share_size_2_9_workers","share_size_10_50_workers","share_size_51plus_workers",
  "industry_diversity","industry_hhi","n_industries_present",
  "share_modern_proxy","share_services_total",
  "share_agriculture","share_manufacturing","share_construction","share_trade_retail",
  "share_hospitality","share_finance_prof_info","share_social_services",
  "share_transport","share_other_services"
), names(nec_cs))

# -----------------------------------------------------------------------------
# 4. NEC fitters (parameterised on which controls to include)
# -----------------------------------------------------------------------------
zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}
build_year_dummies_nec <- function(d, X_col, prefix, ref_year) {
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

fit_nec_panel <- function(outcome, c_mig, c_fx, c_block_a, threshold = 25L) {
  inst_use <- inst[, .(lgcode, year, fxshock, mig_intensity, total_migrants)]
  inst_use[, log_mig_intensity := log(mig_intensity + 1e-8)]
  panel <- merge(nec_p, inst_use, by = c("lgcode","year"), suffixes = c("",".inst"))
  panel <- panel[total_migrants >= threshold]
  if (nrow(panel) < 50) return(NULL)
  muni_yr <- unique(panel[, .(lgcode, year, fxshock, mig_intensity, log_mig_intensity)])
  muni_yr[, fx_z         := zscore(fxshock)]
  muni_yr[, mig_int_z    := zscore(mig_intensity)]
  muni_yr[, log_migint_z := zscore(log_mig_intensity)]
  panel <- merge(panel, muni_yr[, .(lgcode, year, fx_z, mig_int_z, log_migint_z)],
                 by = c("lgcode","year"))
  if (c_block_a && !is.null(BLOCK_A)) panel <- merge(panel, BLOCK_A, by = "lgcode", all.x = TRUE)
  d <- panel[!is.na(get(outcome)) & !is.na(fx_z)]
  if (nrow(d) < 50 || uniqueN(d[[outcome]]) < 2 || sd(d[[outcome]], na.rm = TRUE) == 0)
    return(NULL)
  d <- copy(d)
  d[, treatment := fx_z * log_migint_z]
  year_cols <- character(0)
  if (c_mig)
    year_cols <- c(year_cols, build_year_dummies_nec(d, "mig_int_z", "cmig", 2001L))
  if (c_fx)
    year_cols <- c(year_cols, build_year_dummies_nec(d, "fx_z",      "cfx",  2001L))
  if (c_block_a)
    for (k in BLOCK_A_COLS)
      year_cols <- c(year_cols, build_year_dummies_nec(d, k, paste0("cA_", k), 2001L))
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

fit_nec_cs <- function(outcome, c_mig, c_fx, c_block_a, threshold = 25L, cluster = "DIST") {
  inst_use <- inst[year == 2018, .(lgcode, fxshock, mig_intensity, total_migrants)]
  inst_use[, log_mig_intensity := log(mig_intensity + 1e-8)]
  cs <- merge(nec_cs, inst_use, by = "lgcode", suffixes = c("",".inst"))
  cs <- cs[total_migrants >= threshold]
  if (nrow(cs) < 30) return(NULL)
  cs[, fx_z         := zscore(fxshock)]
  cs[, mig_int_z    := zscore(mig_intensity)]
  cs[, log_migint_z := zscore(log_mig_intensity)]
  if (c_block_a && !is.null(BLOCK_A)) cs <- merge(cs, BLOCK_A, by = "lgcode", all.x = TRUE)
  d <- cs[!is.na(get(outcome)) & !is.na(fx_z)]
  if (nrow(d) < 30 || uniqueN(d[[outcome]]) < 2 || sd(d[[outcome]], na.rm = TRUE) == 0)
    return(NULL)
  d <- copy(d)
  d[, treatment := fx_z * log_migint_z]
  rhs <- c("treatment")
  if (c_fx)       rhs <- c(rhs, "fx_z")
  if (c_mig)      rhs <- c(rhs, "mig_int_z")
  if (c_block_a)  rhs <- c(rhs, BLOCK_A_COLS)
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
run_quiet <- function(...) {
  invisible(capture.output(res <- run_spec(...), file = nullfile()))
  res
}

# -----------------------------------------------------------------------------
# 5. Loop
# -----------------------------------------------------------------------------
outcs <- build_outcomes()
all_rows <- list()
t_start <- Sys.time()
cat(sprintf("\n========== robustness_specs_extra (drop-controls variants) ==========\n"))
cat(sprintf("Specs:    %d  (%s)\n", length(SPECS), paste(names(SPECS), collapse=", ")))
cat(sprintf("Outcomes: census=%d  hh=%d  nec_panel=%d  nec_cs=%d\n\n",
            length(outcs$census), length(outcs$hh),
            length(NEC_PANEL_OUTCOMES), length(NEC_CS_OUTCOMES)))

for (spec_name in names(SPECS)) {
  cfg <- SPECS[[spec_name]]
  cat(sprintf("--- %s (c_mig=%s, c_fx=%s, block_a=%s) ---\n",
              spec_name, cfg$c_mig, cfg$c_fx, cfg$c_block_a))

  # census via run_spec
  r <- run_quiet(
    spec_label = spec_name, dataset = "census", threshold = THRESHOLD,
    treatment = "log_int",
    c_mig = cfg$c_mig, c_fx = cfg$c_fx, c_block_a = cfg$c_block_a,
    c_block_b = FALSE, c_block_c = FALSE,
    outcomes = list(robust = outcs$census), save = FALSE, lag = 0L
  )
  if (!is.null(r) && nrow(r) > 0) all_rows[[length(all_rows)+1]] <- r

  # hh via run_spec
  r <- run_quiet(
    spec_label = spec_name, dataset = "hh", threshold = THRESHOLD,
    treatment = "log_int",
    c_mig = cfg$c_mig, c_fx = cfg$c_fx, c_block_a = cfg$c_block_a,
    c_block_b = FALSE, c_block_c = FALSE,
    outcomes = list(robust = outcs$hh), save = FALSE, lag = 0L
  )
  if (!is.null(r) && nrow(r) > 0) all_rows[[length(all_rows)+1]] <- r

  # NEC panel
  for (y in NEC_PANEL_OUTCOMES) {
    fit <- fit_nec_panel(y, cfg$c_mig, cfg$c_fx, cfg$c_block_a, THRESHOLD)
    if (is.null(fit)) {
      all_rows[[length(all_rows)+1]] <- data.table(
        dataset="nec_panel", outcome=y, spec=spec_name, threshold=THRESHOLD,
        err="NULL/degenerate"); next
    }
    if (!is.null(fit$err)) {
      all_rows[[length(all_rows)+1]] <- data.table(
        dataset="nec_panel", outcome=y, spec=spec_name, threshold=THRESHOLD,
        err=fit$err); next
    }
    all_rows[[length(all_rows)+1]] <- data.table(
      dataset="nec_panel", outcome=y, spec=spec_name, threshold=THRESHOLD,
      beta=fit$beta, se=fit$se, pval=fit$pval, stars=stars_fn(fit$pval),
      n=fit$n, n_muni=fit$n_muni, mean_y=fit$mean_y, sd_y=fit$sd_y, err="")
  }

  # NEC cs
  for (y in NEC_CS_OUTCOMES) {
    fit <- fit_nec_cs(y, cfg$c_mig, cfg$c_fx, cfg$c_block_a, THRESHOLD)
    if (is.null(fit)) {
      all_rows[[length(all_rows)+1]] <- data.table(
        dataset="nec_cs", outcome=y, spec=spec_name, threshold=THRESHOLD,
        err="NULL/degenerate"); next
    }
    if (!is.null(fit$err)) {
      all_rows[[length(all_rows)+1]] <- data.table(
        dataset="nec_cs", outcome=y, spec=spec_name, threshold=THRESHOLD,
        err=fit$err); next
    }
    all_rows[[length(all_rows)+1]] <- data.table(
      dataset="nec_cs", outcome=y, spec=spec_name, threshold=THRESHOLD,
      beta=fit$beta, se=fit$se, pval=fit$pval, stars=stars_fn(fit$pval),
      n=fit$n, n_muni=fit$n_muni, mean_y=fit$mean_y, sd_y=fit$sd_y, err="")
  }
  cat(sprintf("  done %s — %.1f min elapsed\n", spec_name,
              as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
}

# -----------------------------------------------------------------------------
# 6. Save
# -----------------------------------------------------------------------------
out <- rbindlist(all_rows, fill = TRUE)
KEEP <- c("dataset","outcome","spec","threshold","beta","stars","se","pval",
          "mean_y","sd_y","n","n_muni","err")
out <- out[, intersect(KEEP, names(out)), with = FALSE]
out <- out[order(dataset, outcome, spec)]
dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(ROOT, "output/tab/robustness_specs_extra.csv")
fwrite(out, out_path)
cat(sprintf("\nWall-clock: %.1f min\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
cat("Saved: ", normalizePath(out_path, winslash = "/"),
    "  (", nrow(out), " rows)\n", sep = "")

# Quick on-screen summary
cat("\n========== Rows per (dataset, spec) ==========\n")
print(out[, .N, by = .(dataset, spec)][order(dataset, spec)])
