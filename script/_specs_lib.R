# =============================================================================
# script/_specs_lib.R — engine.  Sourced by run_spec.R (or interactively).
#
# Public API:
#   run_spec(
#     spec_label  = "my_spec",     # label written to the `spec` column
#     dataset     = "census",      # "census" or "hh"
#     threshold   = 0L,            # 0, 25, 50, 100, ...  (single value per call)
#     treatment   = "log_int",     # "log_int" | "lin_int" | "fx_alone"
#     c_mig       = FALSE,         # include year x mig_intensity (linear)
#     c_fx        = TRUE,          # include year x fx
#     c_block_a   = TRUE,          # dest-weighted baseline X x year
#     c_block_b   = TRUE,          # origin baseline X (2001 census) x year
#     c_block_c   = TRUE,          # trade SSIV level controls
#     outcomes    = NULL,          # NULL=full catalogue, or character vector,
#                                  # or named list  list(GroupA = c(...), ...)
#     ref_year    = NULL,          # NULL => 2001 (census) / 2016 (hh)
#     output_path = NULL           # NULL => output/tab/<label>_<ds>_thr<thr>.csv
#   )
#
# Notes:
# - Treatments z-scored on the muni-year working sample after threshold filter.
# - Year x X interactions are built manually as dummy columns: for each
#   outcome subset, the reference year is whichever spec ref_year is present;
#   if it isn't (e.g. outcome only measured in two later waves) we silently
#   fall back to the earliest year in the subset, so the regression still
#   runs on available years.
# - Errors per cell are caught and recorded in the `err` column.
# =============================================================================

suppressPackageStartupMessages({
  library(fixest)
  library(data.table)
})
setDTthreads(0)
ROOT <- normalizePath(".")

# Suppress R's default switch to scientific (1.23e+05) notation in printed
# tables.  Higher penalty = wider numbers before switching to "e".
options(scipen = 999)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# -----------------------------------------------------------------------------
# Instrument (lazy-loaded singleton)
# -----------------------------------------------------------------------------
.inst_cache <- NULL
load_instrument <- function() {
  if (!is.null(.inst_cache)) return(.inst_cache)
  raw <- fread("data/clean/instrument/instrument_mun.csv")
  resolve <- function(cands) intersect(cands, names(raw))[1]
  fx_col   <- resolve(c("fxshock", "avg_fx_shock_2001", "shareshock_index_2001"))
  mig_col  <- resolve(c("mig_intensity", "migrants_per_capita_2001", "geog_intensity_2001"))
  tmig_col <- resolve(c("total_migrants","total_migrants_2001","geog_total_mig_2001"))
  stopifnot(!is.na(fx_col), !is.na(mig_col), !is.na(tmig_col))
  inst <- raw[, .(
    lgcode, year,
    fxshock        = get(fx_col),
    mig_intensity  = get(mig_col),
    total_migrants = get(tmig_col)
  )]
  inst[, log_mig_intensity := log(mig_intensity + 1e-8)]
  .inst_cache <<- inst
  inst
}

# -----------------------------------------------------------------------------
# Block A — destination-weighted baseline X
# -----------------------------------------------------------------------------
.blockA_cache <- NULL
build_block_A <- function() {
  if (!is.null(.blockA_cache)) return(.blockA_cache)
  region_p <- "data/clean/instrument/dest_region_shares_2001.csv"
  wdi_p    <- "data/clean/instrument/wdi_dest_gdp_2001.csv"
  share_p  <- "data/clean/instrument/dest_mun_mig_share_2001.csv"
  if (!all(file.exists(c(region_p, wdi_p, share_p)))) {
    message("Block A inputs missing; Block A will be empty.")
    .blockA_cache <<- list(bx = NULL, cols = character()); return(.blockA_cache)
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
    region_cols <- setdiff(region_cols, names(means)[which.max(means)])
  }
  bx <- merge(region[, c("lgcode", region_cols), with = FALSE], dest_gdp,
              by = "lgcode", all = TRUE)
  bx_cols <- c(region_cols, "dest_gdp_pc_2001")
  for (c in bx_cols) bx[is.na(get(c)), (c) := mean(bx[[c]], na.rm = TRUE)]
  .blockA_cache <<- list(bx = bx, cols = bx_cols)
  .blockA_cache
}

# -----------------------------------------------------------------------------
# Block B — origin (Nepal 2001) baseline X
# -----------------------------------------------------------------------------
.blockB_cache <- NULL
build_block_B <- function() {
  if (!is.null(.blockB_cache)) return(.blockB_cache)
  co <- fread("data/clean/census/census_outcomes_municipality.csv")
  candidates <- c("edu_literate","edu_attain_secondary_plus",
                  "work_share_agriculture","flfp_all",
                  "amen_lighting_electricity","head_female_share","share_women")
  present <- intersect(candidates, names(co))
  b01 <- co[year == 2001, c("lgcode", present), with = FALSE]
  setnames(b01, present, paste0("orig_", present, "_2001"))
  inst <- load_instrument()
  pop01 <- inst[year == 2001, .(lgcode,
                                orig_log_pop_2001 = log(total_migrants /
                                                        pmax(mig_intensity, 1e-8) + 1))]
  bx <- merge(pop01, b01, by = "lgcode", all = TRUE)
  bx_cols <- setdiff(names(bx), "lgcode")
  for (c in bx_cols) bx[is.na(get(c)), (c) := mean(bx[[c]], na.rm = TRUE)]
  .blockB_cache <<- list(bx = bx, cols = bx_cols)
  .blockB_cache
}

# -----------------------------------------------------------------------------
# Block C — trade SSIV (time-varying, level controls)
# -----------------------------------------------------------------------------
.trade_cache <- NULL
load_trade_ssiv <- function() {
  if (!is.null(.trade_cache)) return(.trade_cache)
  p <- "data/clean/instrument/trade_ssiv.csv"
  if (!file.exists(p)) {
    message("Block C: trade_ssiv.csv missing; Block C will be empty.")
    .trade_cache <<- NULL; return(NULL)
  }
  ts <- fread(p)[, .(lgcode, year, trade_ssiv_imp, trade_ssiv_exp)]
  .trade_cache <<- ts
  ts
}

# -----------------------------------------------------------------------------
# Default outcome catalogues
# -----------------------------------------------------------------------------
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
                  "edu_school_attend_6_16","edu_school_attend_6_16_female","edu_school_attend_6_16_male",
                  "edu_attain_primary_plus","edu_attain_secondary_plus",
                  "edu_attain_higher_secondary_plus","edu_attain_tertiary","edu_years_mean"),
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

# -----------------------------------------------------------------------------
# Dataset loaders
# -----------------------------------------------------------------------------
.census_cache <- NULL
load_census <- function() {
  if (!is.null(.census_cache)) return(.census_cache)
  .census_cache <<- fread("data/clean/census/census_outcomes_municipality.csv")
  .census_cache
}

.hh_cache <- NULL
load_hh <- function() {
  if (!is.null(.hh_cache)) return(.hh_cache)
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
    if (!file.exists(p)) next
    df <- fread(p)
    keep_cols <- c("hhid","year",
                   setdiff(names(df), c("hhid","year","lgcode", drop_idents)))
    df <- unique(df[, ..keep_cols], by = c("hhid","year"))
    master <- merge(master, df, by = c("hhid","year"), all.x = TRUE)
  }
  # Derived log-count columns for migrant outcomes.  Defined as log(1 + x) so
  # zero-migrant HHs survive (rather than dropping to -Inf).  Note: the raw
  # n_migrants_* live in migration_hh_year_migrant_only.csv, which restricts
  # to HHs with any migrant.  Non-migrant HHs therefore have NA for these
  # raw columns; the log versions stay NA there.  Use them only on the
  # migrant-conditional sample (intensive margin).
  for (v in c("n_migrants_total","n_migrants_international",
              "n_migrants_male","n_migrants_female")) {
    if (v %in% names(master))
      master[, (paste0("log_", v)) := log1p(get(v))]
  }
  .hh_cache <<- master
  master
}

# -----------------------------------------------------------------------------
# Year-interaction column builder.  Adds dummy columns to d for X * 1{year=t},
# t != actual_ref.  Returns the column names.  If only one year present,
# returns character(0) (i.e., omit the block).
# -----------------------------------------------------------------------------
build_year_dummies <- function(d, X_col, prefix, actual_ref) {
  years_present <- sort(unique(d$year))
  if (length(years_present) < 2) return(character(0))
  ref_use <- if (actual_ref %in% years_present) actual_ref else min(years_present)
  out_cols <- character(0)
  for (yr in years_present) {
    if (yr == ref_use) next
    col <- sprintf("%s_x_%s", prefix, yr)
    set(d, j = col, value = d[[X_col]] * as.numeric(d$year == yr))
    out_cols <- c(out_cols, col)
  }
  out_cols
}

# -----------------------------------------------------------------------------
# Outcome -> group lookup from either named-list or character-vector input
# -----------------------------------------------------------------------------
normalize_outcomes <- function(outcomes, default_groups) {
  if (is.null(outcomes)) {
    groups <- default_groups
  } else if (is.list(outcomes) && !is.null(names(outcomes))) {
    groups <- outcomes
  } else if (is.character(outcomes)) {
    groups <- list("custom" = outcomes)
  } else {
    stop("`outcomes` must be NULL, a character vector, or a named list of vectors.")
  }
  all_y <- unlist(groups, use.names = FALSE)
  lookup <- setNames(rep(names(groups), lengths(groups)), all_y)
  list(all = all_y, lookup = lookup)
}

# -----------------------------------------------------------------------------
# Main entry point
# -----------------------------------------------------------------------------
run_spec <- function(spec_label,
                     dataset,
                     threshold     = 0L,
                     treatment     = c("log_int","lin_int","fx_alone"),
                     c_mig         = FALSE,
                     c_fx          = TRUE,
                     c_block_a     = TRUE,
                     c_block_b     = TRUE,
                     c_block_c     = TRUE,
                     outcomes      = NULL,
                     ref_year      = NULL,
                     output_path   = NULL) {

  treatment <- match.arg(treatment)
  stopifnot(dataset %in% c("census","hh"))
  if (is.null(ref_year)) ref_year <- if (dataset == "census") 2001L else 2016L
  entity_col <- if (dataset == "census") "lgcode" else "hhid"
  default_groups <- if (dataset == "census") CENSUS_GROUPS else HH_GROUPS
  ouc <- normalize_outcomes(outcomes, default_groups)

  inst <- load_instrument()
  bxA  <- if (c_block_a) build_block_A() else list(bx = NULL, cols = character())
  bxB  <- if (c_block_b) build_block_B() else list(bx = NULL, cols = character())
  trd  <- if (c_block_c) load_trade_ssiv() else NULL

  base_data <- if (dataset == "census") load_census() else load_hh()
  panel <- merge(
    base_data,
    inst[, .(lgcode, year, fxshock, mig_intensity, log_mig_intensity, total_migrants)],
    by = c("lgcode","year")
  )

  sub <- if (threshold == 0L) copy(panel) else panel[total_migrants >= threshold]

  # Z-score treatments on muni-year working sample
  muni_yr <- unique(sub[, .(lgcode, year, fxshock, mig_intensity, log_mig_intensity)])
  muni_yr[, `:=`(fx_z         = zscore(fxshock),
                 mig_int_z    = zscore(mig_intensity),
                 log_migint_z = zscore(log_mig_intensity))]
  sub <- merge(sub, muni_yr[, .(lgcode, year, fx_z, mig_int_z, log_migint_z)],
               by = c("lgcode","year"))

  if (!is.null(bxA$bx)) sub <- merge(sub, bxA$bx, by = "lgcode", all.x = TRUE)
  if (!is.null(bxB$bx)) sub <- merge(sub, bxB$bx, by = "lgcode", all.x = TRUE)
  if (!is.null(trd)) {
    sub <- merge(sub, trd, by = c("lgcode","year"), all.x = TRUE)
    for (c in c("trade_ssiv_imp","trade_ssiv_exp"))
      sub[is.na(get(c)), (c) := mean(sub[[c]], na.rm = TRUE)]
    sub[, trade_ssiv_imp_z := zscore(trade_ssiv_imp)]
    sub[, trade_ssiv_exp_z := zscore(trade_ssiv_exp)]
  }

  # Build the treatment column
  if (treatment == "log_int") {
    sub[, treatment_col := fx_z * log_migint_z]
  } else if (treatment == "lin_int") {
    sub[, treatment_col := fx_z * mig_int_z]
  } else {
    sub[, treatment_col := fx_z]
  }

  cat(sprintf("\n[run_spec] %s · dataset=%s · thr=%d · treatment=%s\n",
              spec_label, dataset, threshold, treatment))
  cat(sprintf("  controls: c_mig=%s  c_fx=%s  block_a=%s(%d)  block_b=%s(%d)  block_c=%s\n",
              c_mig, c_fx,
              c_block_a, length(bxA$cols),
              c_block_b, length(bxB$cols),
              c_block_c && !is.null(trd)))
  cat(sprintf("  panel: %d rows · %d %ss · %d munis · years %s\n",
              nrow(sub), uniqueN(sub[[entity_col]]), entity_col, uniqueN(sub$lgcode),
              paste(sort(unique(sub$year)), collapse = ",")))

  fit_one <- function(y) {
    if (!(y %in% names(sub))) return(list(err = "outcome not in data"))
    d <- sub[!is.na(get(y)) & !is.na(fx_z)]
    if (nrow(d) < 50 || uniqueN(d[[y]]) < 2 || sd(d[[y]], na.rm = TRUE) == 0)
      return(list(d = d, err = "degenerate"))

    d <- copy(d)   # mutate locally for year-dummy columns
    years_present <- sort(unique(d$year))

    year_cols <- character(0)
    if (length(years_present) >= 2) {
      if (c_mig)
        year_cols <- c(year_cols, build_year_dummies(d, "mig_int_z", "cmig", ref_year))
      if (c_fx)
        year_cols <- c(year_cols, build_year_dummies(d, "fx_z",      "cfx",  ref_year))
      if (c_block_a && length(bxA$cols))
        for (k in bxA$cols)
          year_cols <- c(year_cols, build_year_dummies(d, k, paste0("cA_", k), ref_year))
      if (c_block_b && length(bxB$cols))
        for (k in bxB$cols)
          year_cols <- c(year_cols, build_year_dummies(d, k, paste0("cB_", k), ref_year))
    }

    level_cols <- character(0)
    if (c_block_c && !is.null(trd))
      level_cols <- c("trade_ssiv_imp_z","trade_ssiv_exp_z")

    rhs <- c("treatment_col", year_cols, level_cols)

    # 1-year outcomes: drop entity FE (it would absorb everything) and just
    # run a cross-section with the level controls + Block A/B as levels.
    if (length(years_present) < 2) {
      bx_levels <- c(
        if (c_block_a && length(bxA$cols)) bxA$cols else character(0),
        if (c_block_b && length(bxB$cols)) bxB$cols else character(0)
      )
      rhs <- c("treatment_col", bx_levels, level_cols)
      fml <- as.formula(sprintf("%s ~ %s", y, paste(rhs, collapse = " + ")))
      cross_section <- TRUE
    } else {
      fe  <- paste(entity_col, "year", sep = " + ")
      fml <- as.formula(sprintf("%s ~ %s | %s", y, paste(rhs, collapse = " + "), fe))
      cross_section <- FALSE
    }

    fit <- tryCatch(
      feols(fml, data = d, cluster = ~lgcode,
            notes = FALSE, warn = FALSE,
            combine.quick = FALSE),
      error = function(e) e
    )
    if (inherits(fit, "error"))
      return(list(d = d, err = substr(conditionMessage(fit), 1, 100)))
    if (!("treatment_col" %in% names(coef(fit))))
      return(list(d = d, err = "treatment absorbed"))
    list(d = d, fit = fit, cross_section = cross_section,
         years = length(years_present))
  }

  rows <- vector("list", length(ouc$all))
  for (i in seq_along(ouc$all)) {
    y <- ouc$all[i]
    base <- data.table(
      dataset = dataset, outcome = y, group = ouc$lookup[y],
      spec = spec_label, threshold = threshold,
      beta = NA_real_, stars = "",
      mean_y = NA_real_, pct_of_mean = NA_real_,
      se = NA_real_, pval = NA_real_,
      n = NA_integer_, n_unit = NA_integer_, n_muni = NA_integer_,
      n_years = NA_integer_, sd_y = NA_real_, r2_within = NA_real_,
      err = NA_character_
    )
    r <- fit_one(y)
    if (!is.null(r$err) && is.null(r$fit)) {
      if (!is.null(r$d)) {
        base$n       <- nrow(r$d)
        base$n_unit  <- uniqueN(r$d[[entity_col]])
        base$n_muni  <- uniqueN(r$d$lgcode)
        base$n_years <- uniqueN(r$d$year)
        base$mean_y  <- mean(r$d[[y]], na.rm = TRUE)
        base$sd_y    <- sd(r$d[[y]], na.rm = TRUE)
      }
      base$err <- r$err
      rows[[i]] <- base; next
    }
    fit <- r$fit; d <- r$d
    cf <- coef(fit); se_ <- se(fit); pv <- pvalue(fit)
    base$beta      <- unname(cf["treatment_col"])
    base$se        <- unname(se_["treatment_col"])
    base$pval      <- unname(pv["treatment_col"])
    base$n         <- as.integer(fit$nobs)
    base$n_unit    <- uniqueN(d[[entity_col]])
    base$n_muni    <- uniqueN(d$lgcode)
    base$n_years   <- r$years
    base$mean_y    <- mean(d[[y]], na.rm = TRUE)
    base$sd_y      <- sd(d[[y]], na.rm = TRUE)
    base$r2_within <- tryCatch(unname(r2(fit, "wr2")), error = function(e) NA_real_)
    if (isTRUE(r$cross_section) && is.na(base$err))
      base$err <- sprintf("1-year outcome: cross-section (no entity FE)")
    rows[[i]] <- base
  }

  out <- rbindlist(rows, fill = TRUE)

  # Stars
  out[, stars := fifelse(is.na(pval), "",
                  fifelse(pval < 0.01, "***",
                  fifelse(pval < 0.05, "**",
                  fifelse(pval < 0.10, "*", ""))))]
  # beta_pp     : beta * 100 — interpretable as pp for share/proportion outcomes
  # pct_of_mean : 100 * beta / mean_y — response as % of baseline level (any outcome)
  out[, beta_pp     := fifelse(is.na(beta), NA_real_, beta * 100)]
  out[, pct_of_mean := fifelse(is.na(beta) | is.na(mean_y) | mean_y == 0,
                               NA_real_, 100 * beta / mean_y)]

  # Final column order
  setcolorder(out, c("dataset","outcome","group","spec","threshold",
                     "beta","stars","mean_y","beta_pp","pct_of_mean",
                     "se","pval","n","n_unit","n_muni","n_years",
                     "sd_y","r2_within","err"))

  if (is.null(output_path))
    output_path <- file.path(ROOT, "output", "tab",
                             sprintf("%s_%s_thr%d_results.csv",
                                     spec_label, dataset, threshold))
  output_path <- path.expand(output_path)

  out_dir <- dirname(output_path)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(" Working dir : ", getwd(), "\n", sep = "")
  cat(" Will save to: ", output_path, "\n", sep = "")
  cat(" Directory exists: ", dir.exists(out_dir),
      "  |  writable: ", file.access(out_dir, mode = 2) == 0, "\n", sep = "")

  write_ok <- tryCatch({ fwrite(out, output_path); TRUE },
                      error = function(e) { message("fwrite failed: ",
                                                    conditionMessage(e)); FALSE })
  if (write_ok && file.exists(output_path)) {
    fi <- file.info(output_path)
    cat(" SAVED OK   : ", output_path, "\n", sep = "")
    cat(sprintf("    size = %s bytes   |   modified = %s\n",
                format(fi$size, big.mark = ","), format(fi$mtime)))
  } else {
    cat(" *** SAVE FAILED ***  (see message above)\n")
  }
  cat(strrep("=", 70), "\n", sep = "")
  cat(sprintf(" Rows: %d   |   With estimates: %d   |   Errors/notes: %d\n",
              nrow(out), sum(!is.na(out$beta)), sum(!is.na(out$err))))

  # --------- Summary ----------------------------------------------------------
  cat("\n--- Summary ---\n")
  print(out[!is.na(beta), .(
    n_outcomes = .N,
    pos_sig_05 = sum(pval < 0.05 & beta > 0, na.rm = TRUE),
    neg_sig_05 = sum(pval < 0.05 & beta < 0, na.rm = TRUE),
    sig_01     = sum(pval < 0.01, na.rm = TRUE)
  )])

  # --------- Helper to print a data.table broken up by `group` ---------------
  # Preserves natural outcome order within each group (the order in which the
  # user defined them in CENSUS_GROUPS / HH_GROUPS / a custom list).
  print_by_group <- function(dt) {
    if (nrow(dt) == 0) { cat("(none)\n"); return(invisible()) }
    cols <- c("outcome","beta","stars","mean_y","beta_pp","pct_of_mean",
              "se","pval","n_unit","n_muni","n_years")
    groups_in_order <- unique(dt$group)
    for (g in groups_in_order) {
      sub <- dt[group == g, ..cols]
      cat(sprintf("\n  -- %s  (n=%d) --\n", g, nrow(sub)))
      print(sub, digits = 4, nrows = nrow(sub))
    }
  }

  # --------- Block 1: significant outcomes (any star) by category ------------
  sig <- out[!is.na(pval) & pval < 0.10]
  cat(sprintf("\n========== SIGNIFICANT (p < 0.10, any star) — %d outcomes, grouped ==========\n",
              nrow(sig)))
  print_by_group(sig)

  # --------- Block 2: all outcomes by category -------------------------------
  cat(sprintf("\n========== ALL OUTCOMES — %d, grouped by category ==========\n",
              nrow(out)))
  print_by_group(out)

  if (any(!is.na(out$err))) {
    cat(sprintf("\n--- Notes / skipped cells (%d) ---\n", sum(!is.na(out$err))))
    print(out[!is.na(err), .(outcome, group, n_years, err)], nrows = 60)
  }
  cat("\nCSV path again:  ", output_path, "\n", sep = "")
  invisible(out)
}
