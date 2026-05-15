# =============================================================================
# SPEC 03 — census panel.  spec_01 + Block B (origin baseline X * year FE) +
# Block C (trade SSIV level controls).  C_mig is still dropped.
#
#   y_it = beta * (fx_z * log(mig_int_z))                     # treatment
#        + sum_t lambda_{2,t} * fx_z   * 1{year==t}           # C_fx
#        + sum_{k,t} delta_{k,t} * X_dest_k * 1{year==t}      # C_X  Block A
#        + sum_{j,t} eta_{j,t}   * X_orig_j * 1{year==t}      # C_X  Block B  (NEW)
#        + rho_1 * trade_ssiv_imp_z + rho_2 * trade_ssiv_exp_z #     Block C  (NEW)
#        + alpha_m + gamma_t + e_it
#
# Block B (origin baseline X, taken from the 2001 census wave + instrument):
#   log_pop_2001                  -- from instrument (total_migrants/mig_intensity)
#   edu_literate_2001
#   edu_attain_secondary_plus_2001
#   work_share_agriculture_2001
#   flfp_all_2001
#   amen_lighting_electricity_2001
#   head_female_share_2001
#   share_women_2001
#
# Block C (time-varying trade Bartik shock, source: Comtrade pipeline):
#   trade_ssiv_imp, trade_ssiv_exp  -- z-scored on the working sample.
#
# Reference year: 2001.  SE clustered at lgcode.
# Output: output/tab/spec_03_census_results.csv (one row per outcome x threshold).
#
# Run from repo root in RStudio:  source("script/spec_03_census.R")
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
  if (!all(file.exists(c(region_p, wdi_p, share_p)))) {
    message("Block A inputs not all present - Block A will be empty.")
    return(list(bx = NULL, cols = character()))
  }
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

# --- 3. Block B — origin baseline X (Nepal 2001 wave + log pop) --------------
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

  # log population — derive from instrument at 2001
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

# --- 4. Block C — trade SSIV (Comtrade-driven, contemporaneous) --------------
trade_ssiv <- tryCatch(fread("data/clean/instrument/trade_ssiv.csv"),
                       error = function(e) NULL)
if (!is.null(trade_ssiv)) {
  cat(sprintf("Block C: trade_ssiv panel loaded (%d rows, years %d-%d)\n",
              nrow(trade_ssiv), min(trade_ssiv$year), max(trade_ssiv$year)))
} else {
  message("Block C: trade_ssiv.csv not found - skipping trade controls.")
}

# --- 5. Census outcomes + group catalogue ------------------------------------
co <- fread("data/clean/census/census_outcomes_municipality.csv")

CENSUS_GROUPS <- list(
  "Amenities" = c("amen_water_piped","amen_water_traditional","amen_cooking_wood",
                  "amen_cooking_kerosene","amen_cooking_lpg","amen_cooking_biogas",
                  "amen_cooking_electric","amen_cooking_modern","amen_cooking_traditional",
                  "amen_lighting_electricity","amen_lighting_kerosene","amen_lighting_biogas",
                  "amen_lighting_others","amen_toilet_modern","amen_toilet_ordinary",
                  "amen_toilet_none","amen_toilet_any"),
  "Assets" = c("amen_assets_radio","amen_assets_tv","amen_assets_landline","amen_assets_mobile",
               "amen_assets_computer","amen_assets_internet","amen_assets_cycle",
               "amen_assets_motorcycle","amen_assets_car","amen_assets_fridge",
               "amen_assets_none","amen_asset_count_mean"),
  "Education" = c("edu_literate","edu_literate_female","edu_literate_male",
                  "edu_school_attend_6_16","edu_school_attend_6_16_female",
                  "edu_school_attend_6_16_male","edu_attain_primary_plus",
                  "edu_attain_secondary_plus","edu_attain_higher_secondary_plus",
                  "edu_attain_tertiary","edu_years_mean"),
  "Housing" = c("housing_own","housing_rented","housing_foundation_modern",
                "housing_foundation_traditional","housing_roof_modern","housing_roof_traditional"),
  "Female Ownership" = c("fem_ownership_house","fem_ownership_land","fem_ownership_both","fem_ownership_livestock"),
  "Enterprise" = c("ent_has_nonagro","ent_cottage","ent_trade","ent_transport","ent_services","ent_other","ent_female_owner_share"),
  "Absent HH" = c("absent_hh_share","hh_death_12m"),
  "Marriage" = c("mar_ever_married_15_60","mar_never_married_15_60","mar_female_age_first_mean",
                 "mar_female_married_by_18","mar_female_married_by_20"),
  "Fertility" = c("fert_birth_mean","fert_birth_son_mean","fert_birth_dau_mean",
                  "fert_births_last12m_share","fert_births_last12m_rate"),
  "Child Mortality" = c("mort_children_dead_mean","mort_child_dead_any","mort_child_death_ratio"),
  "Work Activity" = c("work_share_agriculture","work_share_nonagriculture","work_share_wage_nonagri",
                      "work_share_own_nonagri","work_share_extended_econ","work_share_job_seeking",
                      "work_share_household_chores","work_share_student","work_share_no_work","work_lfp"),
  "Occupation" = c("occ_share_armed_forces","occ_share_managers","occ_share_professionals",
                   "occ_share_technicians","occ_share_office_assistants","occ_share_service_sales",
                   "occ_share_agriculture","occ_share_craft_trades","occ_share_machine_operators",
                   "occ_share_elementary"),
  "Industry" = c("ind_agri_forestry_fish","ind_manufacturing","ind_construction",
                 "ind_wholesale_retail","ind_transport_accommodation","ind_finance_real_estate_prof",
                 "ind_public_admin_defence","ind_education","ind_health","ind_arts_recreation","ind_others"),
  "Employment Status" = c("emp_share_employer","emp_share_employee","emp_share_self_employed",
                          "emp_share_unpaid_family_worker"),
  "In-migration" = c("mig_in_share","mig_in_domestic","mig_in_international",
                     "mig_in_from_rural","mig_in_from_urban","mig_in_reason_economic",
                     "mig_in_reason_noneconomic","mig_in_reason_study","mig_in_reason_marriage",
                     "mig_in_return"),
  "Female Labor" = c("flfp_all","fem_employment_rate","flfp_agri","flfp_nonagri",
                     "flfp_wage","flfp_chores_only"),
  "Male Labor" = c("mlfp_all","mlfp_agri","mlfp_nonagri"),
  "Gender Gaps" = c("share_women","share_men","fem_share_of_ag_workers",
                    "fem_ag_specialization_ratio","fem_wage_share_of_employment",
                    "gap_lfp_m_minus_f","gap_nonagri_m_minus_f"),
  "HH Head" = c("head_female_share","head_age_mean","head_elderly_share",
                "head_young_share","head_female_elderly"),
  "Left-behind children" = c("left_not_with_both","left_mother_only","left_father_only",
                             "left_with_relatives","left_without_parents")
)
ALL_OUTCOMES <- unlist(CENSUS_GROUPS, use.names = FALSE)
OUT2GRP <- setNames(rep(names(CENSUS_GROUPS), lengths(CENSUS_GROUPS)), ALL_OUTCOMES)

# --- 6. Estimation -----------------------------------------------------------
REF_YEAR   <- 2001
THRESHOLDS <- c(0L, 25L, 50L, 100L)

panel_full <- merge(
  co,
  inst[, .(lgcode, year, fxshock, mig_intensity, log_mig_intensity, total_migrants)],
  by = c("lgcode","year")
)

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
  as.formula(sprintf("%s ~ %s | lgcode + year",
                     y, paste(rhs, collapse = " + ")))
}

results <- vector("list", length(ALL_OUTCOMES) * length(THRESHOLDS))
idx <- 0L

for (thr in THRESHOLDS) {
  sub <- if (thr == 0L) copy(panel_full) else panel_full[total_migrants >= thr]

  # Re-z-score treatments on the muni-year working sample
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
    # Z-score trade SSIV on the working sample (mean-impute first to avoid NA loss)
    for (c in c("trade_ssiv_imp","trade_ssiv_exp")) {
      sub[is.na(get(c)), (c) := mean(sub[[c]], na.rm = TRUE)]
    }
    sub[, trade_ssiv_imp_z := zscore(trade_ssiv_imp)]
    sub[, trade_ssiv_exp_z := zscore(trade_ssiv_exp)]
  }

  cat(sprintf("\nthr=%d: %d rows, %d munis\n",
              thr, nrow(sub), uniqueN(sub$lgcode)))

  for (y in ALL_OUTCOMES) {
    idx <- idx + 1L
    base <- data.table(
      dataset = "census", outcome = y, group = OUT2GRP[y],
      spec = "spec_03", threshold = thr,
      beta = NA_real_, se = NA_real_, pval = NA_real_,
      n = NA_integer_, n_unit = NA_integer_, n_muni = NA_integer_,
      mean_y = NA_real_, sd_y = NA_real_, r2_within = NA_real_,
      err = NA_character_
    )
    if (!(y %in% names(sub))) { base$err <- "outcome not in data"; results[[idx]] <- base; next }
    d <- sub[!is.na(get(y)) & !is.na(fx_z)]
    if (nrow(d) < 50 || uniqueN(d[[y]]) < 2 || sd(d[[y]], na.rm = TRUE) == 0) {
      base$n <- nrow(d); base$n_unit <- uniqueN(d$lgcode); base$n_muni <- uniqueN(d$lgcode)
      base$mean_y <- mean(d[[y]], na.rm = TRUE); base$sd_y <- sd(d[[y]], na.rm = TRUE)
      base$err <- "degenerate"
      results[[idx]] <- base; next
    }
    fit <- tryCatch(feols(build_formula(y), data = d,
                          cluster = ~lgcode, notes = FALSE, warn = FALSE),
                    error = function(e) e)
    if (inherits(fit, "error")) {
      base$n <- nrow(d); base$n_unit <- uniqueN(d$lgcode); base$n_muni <- uniqueN(d$lgcode)
      base$mean_y <- mean(d[[y]], na.rm = TRUE); base$sd_y <- sd(d[[y]], na.rm = TRUE)
      base$err <- substr(conditionMessage(fit), 1, 80)
      results[[idx]] <- base; next
    }
    cf <- coef(fit); se_ <- se(fit); pv <- pvalue(fit)
    nm <- "fx_z:log_migint_z"
    if (!(nm %in% names(cf))) { base$err <- "treatment absorbed"; results[[idx]] <- base; next }
    base$beta      <- unname(cf[nm])
    base$se        <- unname(se_[nm])
    base$pval      <- unname(pv[nm])
    base$n         <- as.integer(fit$nobs)
    base$n_unit    <- uniqueN(d$lgcode)
    base$n_muni    <- uniqueN(d$lgcode)
    base$mean_y    <- mean(d[[y]], na.rm = TRUE)
    base$sd_y      <- sd(d[[y]], na.rm = TRUE)
    base$r2_within <- tryCatch(unname(r2(fit, "wr2")), error = function(e) NA_real_)
    results[[idx]] <- base
  }
}

out <- rbindlist(results, fill = TRUE)
out[, stars := fifelse(is.na(pval), "",
                fifelse(pval < 0.01, "***",
                fifelse(pval < 0.05, "**",
                fifelse(pval < 0.10, "*", ""))))]

dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(ROOT, "output/tab/spec_03_census_results.csv")
fwrite(out, out_path)

cat("\n", strrep("=", 70), "\n", sep = "")
cat(" SAVED TO:\n   ", normalizePath(out_path, winslash = "/", mustWork = TRUE), "\n", sep = "")
cat(strrep("=", 70), "\n", sep = "")
cat(sprintf(" Rows: %d   |   With estimates: %d   |   Errors: %d\n",
            nrow(out), sum(!is.na(out$beta)), sum(!is.na(out$err))))

cat("\n--- Significant cells by threshold ---\n")
print(out[!is.na(beta), .(
  n_outcomes = .N,
  pos_sig_05 = sum(pval < 0.05 & beta > 0, na.rm = TRUE),
  neg_sig_05 = sum(pval < 0.05 & beta < 0, na.rm = TRUE),
  sig_01     = sum(pval < 0.01, na.rm = TRUE)
), by = threshold])

cat("\n--- Headline outcomes at thr=0 ---\n")
headline <- c("amen_assets_car","amen_assets_internet","amen_assets_mobile",
              "amen_lighting_electricity","amen_water_piped","amen_cooking_lpg",
              "edu_attain_higher_secondary_plus","edu_attain_tertiary",
              "work_share_agriculture","work_lfp",
              "ind_manufacturing","ind_construction",
              "flfp_all","mlfp_all","gap_lfp_m_minus_f")
print(out[threshold == 0 & outcome %in% headline,
          .(outcome, group, beta, se, pval, stars, mean_y, n_muni)],
      digits = 4)

cat("\n--- All outcomes at thr=0 ---\n")
print(out[threshold == 0,
          .(outcome, group, beta, se, pval, stars, mean_y, n_muni)],
      digits = 4, nrows = 250)

cat("\nCSV path again:  ", normalizePath(out_path, winslash = "/"), "\n", sep = "")
