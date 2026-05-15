# =============================================================================
# SPEC 04 — HRVS HH panel.  spec_02 + Block B (origin baseline X * year FE)
# + Block C (trade SSIV level controls).  C_mig still dropped, threshold = 25.
#
# Same RHS structure as spec_03 but on HH x year data:
#   FE = hhid + year, ref year = 2016, threshold = 25.
#
# Output: output/tab/spec_04_hh_results.csv
# Run from repo root in RStudio:  source("script/spec_04_hh.R")
# =============================================================================

suppressPackageStartupMessages({
  library(fixest)
  library(data.table)
})
setDTthreads(0)
ROOT <- normalizePath(".")

# --- 1. Instrument -----------------------------------------------------------
inst_raw <- fread("data/clean/instrument/instrument_mun.csv")
resolve  <- function(candidates) intersect(candidates, names(inst_raw))[1]
fx_col   <- resolve(c("fxshock",       "avg_fx_shock_2001",        "shareshock_index_2001"))
mig_col  <- resolve(c("mig_intensity", "migrants_per_capita_2001", "geog_intensity_2001"))
tmig_col <- resolve(c("total_migrants","total_migrants_2001",      "geog_total_mig_2001"))
stopifnot(!is.na(fx_col), !is.na(mig_col), !is.na(tmig_col))

inst <- inst_raw[, .(
  lgcode, year,
  fxshock         = get(fx_col),
  mig_intensity   = get(mig_col),
  total_migrants  = get(tmig_col)
)]
inst[, log_mig_intensity := log(mig_intensity + 1e-8)]

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# --- 2. Block A — destination-weighted baseline X ----------------------------
build_baseline_X_dest <- function() {
  region_p <- "data/clean/instrument/dest_region_shares_2001.csv"
  wdi_p    <- "data/clean/instrument/wdi_dest_gdp_2001.csv"
  share_p  <- "data/clean/instrument/dest_mun_mig_share_2001.csv"
  if (!all(file.exists(c(region_p, wdi_p, share_p))))
    return(list(bx = NULL, cols = character()))
  wdi   <- fread(wdi_p)[!is.na(gdp_pc_2001), .(country, gdp_pc_2001)]
  share <- fread(share_p)
  dest_gdp <- merge(share, wdi, by = "country")[
      , .(num = sum(mun_mig_share_2001 * gdp_pc_2001),
          cov = sum(mun_mig_share_2001)), by = lgcode
    ][, dest_gdp_pc_2001 := num / fifelse(cov > 0, cov, 1)
    ][, .(lgcode, dest_gdp_pc_2001)]
  region      <- fread(region_p)
  region_cols <- grep("^share_", names(region), value = TRUE)
  if (length(region_cols)) {
    means <- sapply(region_cols, function(c) mean(region[[c]], na.rm = TRUE))
    ref   <- names(means)[which.max(means)]
    region_cols <- setdiff(region_cols, ref)
  }
  bx <- merge(region[, c("lgcode", region_cols), with = FALSE], dest_gdp,
              by = "lgcode", all = TRUE)
  bx_cols <- c(region_cols, "dest_gdp_pc_2001")
  for (c in bx_cols) bx[is.na(get(c)), (c) := mean(bx[[c]], na.rm = TRUE)]
  list(bx = bx, cols = bx_cols)
}
bxA <- build_baseline_X_dest()
BLOCK_A      <- bxA$bx
BLOCK_A_COLS <- bxA$cols

# --- 3. Block B — origin baseline X (2001 wave + log pop) --------------------
build_baseline_X_origin <- function() {
  co <- fread("data/clean/census/census_outcomes_municipality.csv")
  b01 <- co[year == 2001, .(lgcode,
                            edu_literate, edu_attain_secondary_plus,
                            work_share_agriculture, flfp_all,
                            amen_lighting_electricity,
                            head_female_share, share_women)]
  setnames(b01,
    old = c("edu_literate","edu_attain_secondary_plus","work_share_agriculture",
            "flfp_all","amen_lighting_electricity","head_female_share","share_women"),
    new = c("orig_literacy_2001","orig_secondary_plus_2001","orig_agri_share_2001",
            "orig_flfp_2001","orig_electric_light_2001",
            "orig_female_head_2001","orig_share_women_2001"))
  pop01 <- inst[year == 2001, .(lgcode,
                                orig_log_pop_2001 = log(total_migrants /
                                                        pmax(mig_intensity, 1e-8) + 1))]
  bx <- merge(pop01, b01, by = "lgcode", all = TRUE)
  bx_cols <- setdiff(names(bx), "lgcode")
  for (c in bx_cols) bx[is.na(get(c)), (c) := mean(bx[[c]], na.rm = TRUE)]
  list(bx = bx, cols = bx_cols)
}
bxB <- build_baseline_X_origin()
BLOCK_B      <- bxB$bx
BLOCK_B_COLS <- bxB$cols
cat(sprintf("Block A (dest)   covariates: %d\n", length(BLOCK_A_COLS)))
cat(sprintf("Block B (origin) covariates: %d (%s)\n",
            length(BLOCK_B_COLS), paste(BLOCK_B_COLS, collapse = ", ")))

# --- 4. Block C — trade SSIV (time-varying) ----------------------------------
trade_ssiv <- tryCatch(fread("data/clean/instrument/trade_ssiv.csv"),
                       error = function(e) NULL)
if (!is.null(trade_ssiv))
  cat(sprintf("Block C: trade_ssiv panel loaded (%d rows, years %d-%d)\n",
              nrow(trade_ssiv), min(trade_ssiv$year), max(trade_ssiv$year)))

# --- 5. Load HH master -------------------------------------------------------
base_path <- "data/clean/rvs_outcomes"
agri <- fread(file.path(base_path, "agriculture_hh_year.csv"))
setnames(agri, old = intersect("vmun_code", names(agri)), new = "lgcode")

drop_idents <- c("wt_hh","psu","vdc","lgname","district77","district_name",
                 "s00q03a","s00q03b","s00q03c","district","member_id","vmun_code")
keep <- c("hhid","year","lgcode",
          setdiff(names(agri), c("hhid","year","lgcode", drop_idents)))
master <- agri[, ..keep]

extra <- c("consumption_hh_year","education_hh_year","enterprise_hh_year",
           "health_hh_year","social_protection_hh_year",
           "shocks_coping_shocked_hh_year","migration_hh_year_migrant_only")
for (f in extra) {
  p <- file.path(base_path, paste0(f, ".csv"))
  if (!file.exists(p)) { message("  skip ", f); next }
  df <- fread(p)
  keep_cols <- c("hhid","year",
                 setdiff(names(df), c("hhid","year","lgcode", drop_idents)))
  df <- unique(df[, ..keep_cols], by = c("hhid","year"))
  master <- merge(master, df, by = c("hhid","year"), all.x = TRUE)
  cat(sprintf("  + %s: %d cols, %d rows\n", f, ncol(master), nrow(master)))
}

# --- 6. HH outcome catalogue -------------------------------------------------
HH_GROUPS <- list(
  "Land — agriculture" = c("agro_hh","n_plots_owned","total_owned_area_sqm",
                           "cultivated_area_sqm","cultivated_area_total_sqm","rented_in_area_sqm"),
  "Land use — wet/dry" = c("share_self_wet","share_rented_out_wet","share_fallow_wet",
                           "share_self_dry","share_fallow_dry","share_both_seasons"),
  "Cropping" = c("n_crops_total","multi_season","grows_staple","grows_cashcrop",
                 "grows_horticulture","crop_simpson_diversity","staple_value_share"),
  "Consumption — food" = c("food_exp_total_7day","food_exp_protein_7day","food_exp_staples_7day",
                           "food_insec_any","food_insec_score","food_insec_worried"),
  "Consumption — non-food" = c("nonfood_exp_30day","nonfood_exp_12m","nonfood_communication_12m",
                               "nonfood_transport_12m","nonfood_entertainment_leisure_12m",
                               "nonfood_ceremonies_12m","nonfood_fuel_lighting_12m",
                               "nonfood_clothing_footwear_12m"),
  "Education spending" = c("any_enrolled","n_enrolled","n_private_school","n_scholarship",
                           "edu_spend_total_12m","edu_spend_per_enrolled",
                           "edu_spend_tuition_12m","edu_spend_books_12m"),
  "Health" = c("any_insured","n_insured","n_chronic","n_acute_illness",
               "any_health_spending","hlt_spend_total","hlt_spend_medicines","hlt_spend_hospital"),
  "Enterprise" = c("has_enterprise","n_enterprises","n_workers_total","revenue_12m","expenses_12m",
                   "profit_12m","capex_12m","sector_manufacturing","sector_services",
                   "sector_trade","sector_hotels","sector_transport"),
  "Migration — HH" = c("has_migrant","has_migrant_internal","has_migrant_international",
                       "has_only_internal","has_only_international","has_both_internal_and_international",
                       "n_migrants_total","n_migrants_male","n_migrants_female",
                       "share_male_migrants","share_long_term_migrants",
                       "mig_reason_work","mig_reason_education","mig_reason_marriage_family"),
  "Shocks & coping" = c("any_shock","n_shocks","total_loss_rs",
                        "health_shock_any","death_shock_any","natural_disaster_shock_any",
                        "agricultural_shock_any","economic_shock_any","any_coping_reported",
                        "cope_savings_any","cope_borrow_any","cope_sell_assets_any",
                        "cope_migration_remittance_any","cope_public_private_aid_any"),
  "Social protection" = c("public_support_any","public_support_amt","public_cash_any","public_cash_amt",
                          "demographic_cash_any","disaster_cash_any","public_inkind_any","public_work_any",
                          "private_support_any","ngo_support_any","remittance_any","remittance_amt")
)
ALL_OUTCOMES <- unlist(HH_GROUPS, use.names = FALSE)
OUT2GRP <- setNames(rep(names(HH_GROUPS), lengths(HH_GROUPS)), ALL_OUTCOMES)

# --- 7. Build working panel (thr = 25) ---------------------------------------
REF_YEAR  <- 2016L
THRESHOLD <- 25L

panel <- merge(
  master,
  inst[, .(lgcode, year, fxshock, mig_intensity, log_mig_intensity, total_migrants)],
  by = c("lgcode","year")
)
sub <- panel[total_migrants >= THRESHOLD]

muni_yr <- unique(sub[, .(lgcode, year, fxshock, mig_intensity, log_mig_intensity)])
muni_yr[, fx_z         := zscore(fxshock)]
muni_yr[, mig_int_z    := zscore(mig_intensity)]
muni_yr[, log_migint_z := zscore(log_mig_intensity)]
sub <- merge(sub, muni_yr[, .(lgcode, year, fx_z, mig_int_z, log_migint_z)],
             by = c("lgcode","year"))

if (!is.null(BLOCK_A)) sub <- merge(sub, BLOCK_A, by = "lgcode", all.x = TRUE)
if (!is.null(BLOCK_B)) sub <- merge(sub, BLOCK_B, by = "lgcode", all.x = TRUE)
if (!is.null(trade_ssiv)) {
  sub <- merge(sub, trade_ssiv[, .(lgcode, year, trade_ssiv_imp, trade_ssiv_exp)],
               by = c("lgcode","year"), all.x = TRUE)
  for (c in c("trade_ssiv_imp","trade_ssiv_exp"))
    sub[is.na(get(c)), (c) := mean(sub[[c]], na.rm = TRUE)]
  sub[, trade_ssiv_imp_z := zscore(trade_ssiv_imp)]
  sub[, trade_ssiv_exp_z := zscore(trade_ssiv_exp)]
}

cat(sprintf("\nthr=%d: %d HH-year rows · %d HH · %d munis · years %s\n",
            THRESHOLD, nrow(sub),
            uniqueN(sub$hhid), uniqueN(sub$lgcode),
            paste(sort(unique(sub$year)), collapse = ",")))

# --- 8. Estimation -----------------------------------------------------------
build_formula <- function(y) {
  rhs <- c(
    "fx_z:log_migint_z",
    sprintf("i(year, fx_z, ref = %d)", REF_YEAR)
  )
  if (length(BLOCK_A_COLS))
    rhs <- c(rhs, sapply(BLOCK_A_COLS, function(k)
      sprintf("i(year, %s, ref = %d)", k, REF_YEAR)))
  if (length(BLOCK_B_COLS))
    rhs <- c(rhs, sapply(BLOCK_B_COLS, function(k)
      sprintf("i(year, %s, ref = %d)", k, REF_YEAR)))
  if (!is.null(trade_ssiv))
    rhs <- c(rhs, "trade_ssiv_imp_z", "trade_ssiv_exp_z")
  as.formula(sprintf("%s ~ %s | hhid + year",
                     y, paste(rhs, collapse = " + ")))
}

results <- vector("list", length(ALL_OUTCOMES))
for (i in seq_along(ALL_OUTCOMES)) {
  y    <- ALL_OUTCOMES[i]
  base <- data.table(
    dataset = "hh", outcome = y, group = OUT2GRP[y],
    spec = "spec_04", threshold = THRESHOLD,
    beta = NA_real_, se = NA_real_, pval = NA_real_,
    n = NA_integer_, n_unit = NA_integer_, n_muni = NA_integer_,
    mean_y = NA_real_, sd_y = NA_real_, r2_within = NA_real_,
    err = NA_character_
  )
  if (!(y %in% names(sub))) { base$err <- "outcome not in data"; results[[i]] <- base; next }
  d <- sub[!is.na(get(y)) & !is.na(fx_z)]
  if (nrow(d) < 50 || uniqueN(d[[y]]) < 2 || sd(d[[y]], na.rm = TRUE) == 0) {
    base$n <- nrow(d); base$n_unit <- uniqueN(d$hhid); base$n_muni <- uniqueN(d$lgcode)
    base$mean_y <- mean(d[[y]], na.rm = TRUE); base$sd_y <- sd(d[[y]], na.rm = TRUE)
    base$err <- "degenerate"
    results[[i]] <- base; next
  }
  fit <- tryCatch(feols(build_formula(y), data = d,
                        cluster = ~lgcode, notes = FALSE, warn = FALSE),
                  error = function(e) e)
  if (inherits(fit, "error")) {
    base$n <- nrow(d); base$n_unit <- uniqueN(d$hhid); base$n_muni <- uniqueN(d$lgcode)
    base$mean_y <- mean(d[[y]], na.rm = TRUE); base$sd_y <- sd(d[[y]], na.rm = TRUE)
    base$err <- substr(conditionMessage(fit), 1, 80)
    results[[i]] <- base; next
  }
  cf <- coef(fit); se_ <- se(fit); pv <- pvalue(fit)
  nm <- "fx_z:log_migint_z"
  if (!(nm %in% names(cf))) { base$err <- "treatment absorbed"; results[[i]] <- base; next }
  base$beta      <- unname(cf[nm])
  base$se        <- unname(se_[nm])
  base$pval      <- unname(pv[nm])
  base$n         <- as.integer(fit$nobs)
  base$n_unit    <- uniqueN(d$hhid)
  base$n_muni    <- uniqueN(d$lgcode)
  base$mean_y    <- mean(d[[y]], na.rm = TRUE)
  base$sd_y      <- sd(d[[y]], na.rm = TRUE)
  base$r2_within <- tryCatch(unname(r2(fit, "wr2")), error = function(e) NA_real_)
  results[[i]]   <- base
}

out <- rbindlist(results, fill = TRUE)
out[, stars := fifelse(is.na(pval), "",
                fifelse(pval < 0.01, "***",
                fifelse(pval < 0.05, "**",
                fifelse(pval < 0.10, "*", ""))))]

dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(ROOT, "output/tab/spec_04_hh_results.csv")
fwrite(out, out_path)

cat("\n", strrep("=", 70), "\n", sep = "")
cat(" SAVED TO:\n   ", normalizePath(out_path, winslash = "/", mustWork = TRUE), "\n", sep = "")
cat(strrep("=", 70), "\n", sep = "")
cat(sprintf(" Rows: %d   |   With estimates: %d   |   Errors: %d\n",
            nrow(out), sum(!is.na(out$beta)), sum(!is.na(out$err))))

cat("\n--- Significant cells (thr=25) ---\n")
print(out[!is.na(beta), .(
  n_outcomes = .N,
  pos_sig_05 = sum(pval < 0.05 & beta > 0, na.rm = TRUE),
  neg_sig_05 = sum(pval < 0.05 & beta < 0, na.rm = TRUE),
  sig_01     = sum(pval < 0.01, na.rm = TRUE)
)])

cat("\n--- Headline HH outcomes ---\n")
headline <- c("remittance_any","remittance_amt",
              "food_insec_any","food_insec_score",
              "nonfood_exp_12m","nonfood_communication_12m",
              "any_enrolled","n_private_school","edu_spend_total_12m",
              "any_insured","hlt_spend_total",
              "has_enterprise","revenue_12m","profit_12m",
              "has_migrant","has_migrant_international",
              "any_shock","n_shocks")
print(out[outcome %in% headline,
          .(outcome, group, beta, se, pval, stars, mean_y, n, n_unit)],
      digits = 4)

cat("\n--- All outcomes ---\n")
print(out[, .(outcome, group, beta, se, pval, stars, mean_y, n)],
      digits = 4, nrows = 200)

cat("\nCSV path again:  ", normalizePath(out_path, winslash = "/"), "\n", sep = "")
