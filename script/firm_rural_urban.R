# =============================================================================
# script/firm_rural_urban.R
#
# Firm-level heterogeneity by rural vs urban municipality.
# Splits the sample by `lgtype` from LandCoverMatched.csv:
#   RURAL  = Gaunpalika              (460 munis)
#   URBAN  = Nagarpalika + Upamahanagarpalika + Mahanagarpalika  (293 munis)
#
# For each (outcome × subsample × threshold) cell, runs the preferred anchor
# spec (log_int treatment, c_mig=T, c_fx=T, Block A).
#
# Datasets:
#   nec_panel  -- muni × founding-year, 2001–2018; FE = lgcode + year
#   nec_cs     -- muni cross-section 2018;        FE = district only
#
# Output: output/tab/firm_rural_urban.csv  (one row per cell)
#
# Run from repo root:  source("script/firm_rural_urban.R")
# =============================================================================

suppressPackageStartupMessages({
  library(fixest); library(data.table)
})
options(scipen = 999); setDTthreads(0)
ROOT <- normalizePath(".")

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# -----------------------------------------------------------------------------
# 1.  Instrument
# -----------------------------------------------------------------------------
inst_raw <- fread("data/clean/instrument/instrument_mun.csv")
inst <- inst_raw[, .(
  lgcode, year,
  fxshock        = fxshock,
  mig_intensity  = mig_intensity,
  total_migrants = total_migrants
)]
inst[, log_mig_intensity := log(mig_intensity + 1e-8)]

# -----------------------------------------------------------------------------
# 2.  Block A (Khanna baseline X)
# -----------------------------------------------------------------------------
build_block_A <- function() {
  region_p <- "data/clean/instrument/dest_region_shares_2001.csv"
  wdi_p    <- "data/clean/instrument/wdi_dest_gdp_2001.csv"
  share_p  <- "data/clean/instrument/dest_mun_mig_share_2001.csv"
  if (!all(file.exists(c(region_p, wdi_p, share_p))))
    return(list(bx = NULL, cols = character()))
  wdi <- fread(wdi_p)[!is.na(gdp_pc_2001), .(country, gdp_pc_2001)]
  share <- fread(share_p)
  dest_gdp <- merge(share, wdi, by = "country")[
      , .(num = sum(mun_mig_share_2001 * gdp_pc_2001),
          cov = sum(mun_mig_share_2001)), by = lgcode
    ][, dest_gdp_pc_2001 := num / fifelse(cov > 0, cov, 1)
    ][, .(lgcode, dest_gdp_pc_2001)]
  region <- fread(region_p)
  region_cols <- grep("^share_", names(region), value = TRUE)
  if (length(region_cols)) {
    means <- sapply(region_cols, function(c) mean(region[[c]], na.rm = TRUE))
    region_cols <- setdiff(region_cols, names(means)[which.max(means)])
  }
  bx <- merge(region[, c("lgcode", region_cols), with = FALSE], dest_gdp,
              by = "lgcode", all = TRUE)
  bx_cols <- c(region_cols, "dest_gdp_pc_2001")
  for (c in bx_cols) bx[is.na(get(c)), (c) := mean(bx[[c]], na.rm = TRUE)]
  list(bx = bx, cols = bx_cols)
}
bxA <- build_block_A()
BLOCK_A      <- bxA$bx
BLOCK_A_COLS <- bxA$cols

# -----------------------------------------------------------------------------
# 3.  Rural/urban classification from LandCoverMatched
# -----------------------------------------------------------------------------
land <- fread("data/clean/LandCoverMatched.csv")
urb <- unique(land[, .(lgcode, lgtype)])
urb[, sample := fcase(
  lgtype %in% c("Nagarpalika","Upamahanagarpalika","Mahanagarpalika"), "urban",
  lgtype == "Gaunpalika", "rural",
  default = NA_character_
)]
urb <- urb[!is.na(sample), .(lgcode, sample)]
cat(sprintf("Urban/rural lookup: %d urban, %d rural, %d total\n",
            urb[sample == "urban", .N], urb[sample == "rural", .N], nrow(urb)))

# -----------------------------------------------------------------------------
# 4.  NEC panel
# -----------------------------------------------------------------------------
nec_p_raw <- fread("data/clean/nec2018/mun_entry_panel_new.csv")
nec_p <- nec_p_raw[year >= 2001 & year <= 2018]
nec_p <- merge(nec_p, urb, by = "lgcode", all.x = FALSE)   # drops protected-area munis
cat(sprintf("NEC panel: %d rows, %d munis (%d urban, %d rural)\n",
            nrow(nec_p), uniqueN(nec_p$lgcode),
            uniqueN(nec_p[sample=='urban']$lgcode), uniqueN(nec_p[sample=='rural']$lgcode)))

# Add log columns for any cost / count variables we want
for (v in c("new_firms",
            "new_firms_size_1_worker","new_firms_size_2_9_workers",
            "new_firms_size_10_50_workers","new_firms_size_51plus_workers",
            "new_firms_manufacturing","new_firms_construction",
            "new_firms_trade_retail","new_firms_hospitality_food",
            "new_firms_other_services","new_firms_finance_prof_realestate",
            "new_firms_education_health_social")) {
  if (v %in% names(nec_p) && !(paste0("log_", v) %in% names(nec_p)))
    nec_p[, (paste0("log_", v)) := log1p(pmax(get(v), 0))]
}

# -----------------------------------------------------------------------------
# 5.  NEC cross-section (2018)
# -----------------------------------------------------------------------------
nec_cs_files <- c("mun_industry_structure","mun_productivity_profitability","mun_size_formality")
parts <- lapply(nec_cs_files, function(f) {
  p <- file.path("data/clean/nec2018", paste0(f, ".csv"))
  if (file.exists(p)) fread(p) else NULL
})
parts <- parts[!sapply(parts, is.null)]
nec_cs <- Reduce(function(a, b) {
  new_cols <- setdiff(names(b), names(a))
  if (length(new_cols)) merge(a, b[, c("lgcode", new_cols), with = FALSE],
                              by = "lgcode", all = TRUE) else a
}, parts)
# Derive DIST from lgcode prefix (first 3 digits = district code)
nec_cs[, DIST := lgcode %/% 100]
nec_cs <- merge(nec_cs, urb, by = "lgcode", all.x = FALSE)
cat(sprintf("NEC cs:    %d munis (%d urban, %d rural), %d districts\n",
            nrow(nec_cs),
            nec_cs[sample=='urban', .N], nec_cs[sample=='rural', .N],
            uniqueN(nec_cs$DIST)))

# Add log columns for any totals we want
for (v in c("n_firms","emp_total","rev_total","value_added_total","cap_total",
            "n_firms_size_micro_1","n_firms_size_small_2_9","n_firms_size_medium_10_50",
            "n_firms_size_large_51p")) {
  if (v %in% names(nec_cs) && !(paste0("log_", v) %in% names(nec_cs)))
    nec_cs[, (paste0("log_", v)) := log1p(pmax(get(v), 0))]
}

# -----------------------------------------------------------------------------
# 6.  Outcome catalogues
# -----------------------------------------------------------------------------
NEC_PANEL_OUTCOMES <- c(
  "log_new_firms",
  "log_new_firms_size_1_worker",
  "log_new_firms_size_2_9_workers",
  "log_new_firms_size_10_50_workers",
  "log_new_firms_size_51plus_workers",
  "log_new_firms_manufacturing",
  "log_new_firms_construction",
  "log_new_firms_trade_retail",
  "log_new_firms_hospitality_food",
  "log_new_firms_other_services"
)
NEC_CS_OUTCOMES <- c(
  "log_n_firms",
  "log_emp_total",
  "log_rev_total",
  "log_value_added_total",
  "log_cap_total",
  "log_n_firms_size_micro_1",
  "log_n_firms_size_small_2_9",
  "log_n_firms_size_medium_10_50",
  "log_n_firms_size_large_51p",
  "formality_index",
  "share_registered",
  "mean_emp_per_firm",
  "share_emp_female"
)
NEC_PANEL_OUTCOMES <- intersect(NEC_PANEL_OUTCOMES, names(nec_p))
NEC_CS_OUTCOMES    <- intersect(NEC_CS_OUTCOMES,    names(nec_cs))

# -----------------------------------------------------------------------------
# 7.  Estimator (PANEL, with year × X interactions for Block A)
# -----------------------------------------------------------------------------
build_year_dummies <- function(d, X_col, prefix, ref_year) {
  yrs <- sort(unique(d$year))
  if (length(yrs) < 2) return(character(0))
  ref_use <- if (ref_year %in% yrs) ref_year else min(yrs)
  cols <- character(0)
  for (yr in yrs) {
    if (yr == ref_use) next
    c <- sprintf("%s_x_%s", prefix, yr)
    set(d, j = c, value = d[[X_col]] * as.numeric(d$year == yr))
    cols <- c(cols, c)
  }
  cols
}

fit_panel <- function(panel, outcome, thr, ref_year, fe = "lgcode + year") {
  sub <- panel[total_migrants >= thr]
  if (nrow(sub) < 50) return(NULL)
  muni_yr <- unique(sub[, .(lgcode, year, fxshock, mig_intensity, log_mig_intensity)])
  muni_yr[, fx_z         := zscore(fxshock)]
  muni_yr[, mig_int_z    := zscore(mig_intensity)]
  muni_yr[, log_migint_z := zscore(log_mig_intensity)]
  sub <- merge(sub, muni_yr[, .(lgcode, year, fx_z, mig_int_z, log_migint_z)],
               by = c("lgcode","year"))
  if (!is.null(BLOCK_A)) sub <- merge(sub, BLOCK_A, by = "lgcode", all.x = TRUE)

  d <- sub[!is.na(get(outcome)) & !is.na(fx_z)]
  if (nrow(d) < 50 || uniqueN(d[[outcome]]) < 2 || sd(d[[outcome]], na.rm = TRUE) == 0)
    return(NULL)
  d <- copy(d)
  d[, treatment := fx_z * log_migint_z]

  year_cols <- character(0)
  year_cols <- c(year_cols, build_year_dummies(d, "mig_int_z", "cmig", ref_year))
  year_cols <- c(year_cols, build_year_dummies(d, "fx_z",      "cfx",  ref_year))
  for (k in BLOCK_A_COLS)
    year_cols <- c(year_cols, build_year_dummies(d, k, paste0("cA_", k), ref_year))

  rhs <- c("treatment", year_cols)
  fml <- as.formula(sprintf("%s ~ %s | %s", outcome,
                            paste(rhs, collapse = " + "), fe))
  fit <- tryCatch(feols(fml, data = d, cluster = ~lgcode, notes = FALSE, warn = FALSE),
                  error = function(e) e)
  if (inherits(fit, "error")) return(list(err = substr(conditionMessage(fit), 1, 100)))
  cf <- coef(fit); se_ <- se(fit); pv <- pvalue(fit)
  if (!("treatment" %in% names(cf))) return(list(err = "treatment absorbed"))
  list(beta   = unname(cf["treatment"]),
       se     = unname(se_["treatment"]),
       pval   = unname(pv["treatment"]),
       n      = as.integer(fit$nobs),
       n_muni = uniqueN(d$lgcode),
       mean_y = mean(d[[outcome]], na.rm = TRUE),
       sd_y   = sd(d[[outcome]],  na.rm = TRUE))
}

fit_cs <- function(cs, outcome, thr, cluster = "DIST") {
  sub <- cs[total_migrants >= thr]
  if (nrow(sub) < 50) return(NULL)
  # z-score on cross-section sample
  sub[, fx_z         := zscore(fxshock)]
  sub[, mig_int_z    := zscore(mig_intensity)]
  sub[, log_migint_z := zscore(log_mig_intensity)]
  if (!is.null(BLOCK_A)) sub <- merge(sub, BLOCK_A, by = "lgcode", all.x = TRUE)
  d <- sub[!is.na(get(outcome)) & !is.na(fx_z)]
  if (nrow(d) < 30 || uniqueN(d[[outcome]]) < 2 || sd(d[[outcome]], na.rm = TRUE) == 0)
    return(NULL)
  d <- copy(d)
  d[, treatment := fx_z * log_migint_z]
  rhs <- c("treatment", "fx_z", "mig_int_z", BLOCK_A_COLS)
  fml <- as.formula(sprintf("%s ~ %s | %s", outcome,
                            paste(rhs, collapse = " + "), cluster))
  fit <- tryCatch(feols(fml, data = d, cluster = as.formula(paste0("~", cluster)),
                        notes = FALSE, warn = FALSE),
                  error = function(e) e)
  if (inherits(fit, "error")) return(list(err = substr(conditionMessage(fit), 1, 100)))
  cf <- coef(fit); se_ <- se(fit); pv <- pvalue(fit)
  if (!("treatment" %in% names(cf))) return(list(err = "treatment absorbed"))
  list(beta   = unname(cf["treatment"]),
       se     = unname(se_["treatment"]),
       pval   = unname(pv["treatment"]),
       n      = as.integer(fit$nobs),
       n_muni = nrow(d),
       mean_y = mean(d[[outcome]], na.rm = TRUE),
       sd_y   = sd(d[[outcome]],  na.rm = TRUE))
}

# -----------------------------------------------------------------------------
# 8.  Loop  outcome × subsample × threshold
# -----------------------------------------------------------------------------
THR <- c(0L, 25L, 50L, 100L)
SAMPLES <- list(full = NULL, rural = "rural", urban = "urban")

# Pre-merge instrument into panel + cs (for total_migrants threshold use)
nec_p <- merge(nec_p, inst[, .(lgcode, year, fxshock, mig_intensity, log_mig_intensity, total_migrants)],
               by = c("lgcode","year"), suffixes = c("", ".inst"))
nec_cs_inst <- inst[year == 2018, .(lgcode, fxshock, mig_intensity, log_mig_intensity, total_migrants)]
nec_cs <- merge(nec_cs, nec_cs_inst, by = "lgcode", suffixes = c("", ".inst"))

rows <- list()
for (samp_name in names(SAMPLES)) {
  filt <- SAMPLES[[samp_name]]
  for (thr in THR) {
    # -- NEC panel --
    panel_d <- if (is.null(filt)) nec_p else nec_p[sample == filt]
    for (y in NEC_PANEL_OUTCOMES) {
      r <- fit_panel(panel_d, y, thr = thr, ref_year = 2001L)
      if (is.null(r) || !is.null(r$err)) next
      rows[[length(rows)+1]] <- data.table(
        dataset = "nec_panel", outcome = y, sample = samp_name, threshold = thr,
        beta = r$beta, se = r$se, pval = r$pval,
        n = r$n, n_muni = r$n_muni, mean_y = r$mean_y, sd_y = r$sd_y
      )
    }
    # -- NEC cs (2018) --
    cs_d <- if (is.null(filt)) nec_cs else nec_cs[sample == filt]
    for (y in NEC_CS_OUTCOMES) {
      r <- fit_cs(cs_d, y, thr = thr)
      if (is.null(r) || !is.null(r$err)) next
      rows[[length(rows)+1]] <- data.table(
        dataset = "nec_cs", outcome = y, sample = samp_name, threshold = thr,
        beta = r$beta, se = r$se, pval = r$pval,
        n = r$n, n_muni = r$n_muni, mean_y = r$mean_y, sd_y = r$sd_y
      )
    }
    cat(sprintf("  done %s, thr=%d\n", samp_name, thr))
  }
}
out <- rbindlist(rows, fill = TRUE)
out[, stars := fifelse(is.na(pval), "",
                fifelse(pval < .01, "***",
                fifelse(pval < .05, "**",
                fifelse(pval < .10, "*", ""))))]
out[, beta_pp     := beta * 100]
out[, pct_of_mean := 100 * beta / fifelse(mean_y == 0, NA_real_, mean_y)]

setcolorder(out, c("dataset","outcome","sample","threshold",
                   "beta","stars","mean_y","beta_pp","pct_of_mean",
                   "se","pval","n","n_muni","sd_y"))
out[, sample := factor(sample, levels = c("full","rural","urban"))]
out <- out[order(dataset, outcome, sample, threshold)]

dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(ROOT, "output/tab/firm_rural_urban.csv")
fwrite(out, out_path)
cat("\nSaved: ", normalizePath(out_path, winslash = "/"), "  (", nrow(out), " rows)\n", sep = "")

# -----------------------------------------------------------------------------
# 9.  Print compact summary at thr=0 (anchor)
# -----------------------------------------------------------------------------
cat("\n========== NEC PANEL: log(# new firms) by sub-sample (thr = 0) ==========\n")
print(out[dataset == "nec_panel" & threshold == 0,
          .(outcome, sample,
            beta = signif(beta, 4), stars,
            beta_pp = signif(beta_pp, 4), pct = signif(pct_of_mean, 4),
            se = signif(se, 3), pval = signif(pval, 3),
            n, n_muni)],
      nrows = 60)
cat("\n========== NEC CS 2018: muni-level outcomes by sub-sample (thr = 0) ==========\n")
print(out[dataset == "nec_cs" & threshold == 0,
          .(outcome, sample,
            beta = signif(beta, 4), stars,
            beta_pp = signif(beta_pp, 4), pct = signif(pct_of_mean, 4),
            se = signif(se, 3), pval = signif(pval, 3),
            n_muni)],
      nrows = 60)
