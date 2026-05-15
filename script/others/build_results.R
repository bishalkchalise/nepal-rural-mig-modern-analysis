###############################################################################
# build_results.R
# ----------------------------------------------------------------------------
# R / RStudio companion to script/build_results.py.  Produces the same
# docs/results.json that the interactive HTML explorer reads, so you can
# regenerate the explorer entirely inside RStudio without leaving R.
#
# Required packages:
#   install.packages(c("tidyverse","fixest","jsonlite"))
#
# Run from the project root:
#   source("script/build_results.R")
#
# Output: docs/results.json
###############################################################################

.req <- c("tidyverse","fixest","jsonlite","here")
.miss <- setdiff(.req, rownames(installed.packages()))
if (length(.miss)) install.packages(.miss, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(jsonlite)
  library(here)
})

ROOT <- tryCatch(here::here(), error = function(e) getwd())

inst <- read_csv(file.path(ROOT, "data/clean/instrument/instrument_mun.csv"),
                 show_col_types = FALSE)
out  <- read_csv(file.path(ROOT, "data/clean/census/census_outcomes_municipality.csv"),
                 show_col_types = FALSE)

ssiv_cols <- grep("^(ssiv|shareshock|absexp)_", names(inst), value = TRUE)

# ============================================================================
# Configuration: panels, lags, shocks, control sets, outcome groups
# ============================================================================

PANELS <- list(
  P2_ref2011 = list(label = "2011, 2021 (ref 2011)",      years = c(2011, 2021),     ref = 2011),
  P3_ref2001 = list(label = "2001, 2011, 2021 (ref 2001)", years = c(2001, 2011, 2021), ref = 2001),
  P3_ref2011 = list(label = "2001, 2011, 2021 (ref 2011)", years = c(2001, 2011, 2021), ref = 2011)
)

LAGS <- list(
  lag0 = list(label = "Contemporaneous (Z_t)",   lag = 0),
  lag1 = list(label = "Lagged 1 year (Z_{t-1})", lag = 1),
  lag2 = list(label = "Lagged 2 years (Z_{t-2})",lag = 2)
)

SHOCKS <- list(
  ssiv_index = list(raw = "ssiv_index_2001", label = "SSIV (level index)",
                    desc = "Per-capita SSIV; 2001=baseline. Main spec."),
  ssiv_w99   = list(raw = "ssiv_w99",        label = "SSIV winsorized at 99%",
                    desc = "Outlier-robust version of the main SSIV (clipped at 99th percentile).")
)

# 2001 baseline X (must have actual variation; redundant cols dropped)
BASE_X_DEV <- c("amen_lighting_electricity", "amen_water_piped",
                "ent_has_nonagro", "head_age_mean", "edu_literate")
BASE_X_IND <- c("work_share_agriculture", "ind_manufacturing", "flfp_all")
BASE_X_BIG <- c(BASE_X_DEV, BASE_X_IND)

CONTROLS <- list(
  none        = "No controls (just FEs)",
  mi          = "+ MI × 1{t≠ref}  (basic Khanna baseline)",
  khanna_full = "+ MI + ShareShock + baseline X × 1{t≠ref}  (full Khanna; needs optional inputs)"
)

# Outcome groups (mirrors Python; keep in sync if you edit either)
GROUPS <- list(
  "Amenities — water & toilet" = list(
    c("amen_water_piped","Piped water"),
    c("amen_water_traditional","Traditional water source"),
    c("amen_toilet_modern","Modern toilet"),
    c("amen_toilet_ordinary","Ordinary toilet"),
    c("amen_toilet_any","Any toilet"),
    c("amen_toilet_none","No toilet")
  ),
  "Amenities — cooking & lighting" = list(
    c("amen_cooking_lpg","LPG (cooking)"),
    c("amen_cooking_wood","Wood (cooking)"),
    c("amen_cooking_kerosene","Kerosene (cooking)"),
    c("amen_cooking_biogas","Biogas (cooking)"),
    c("amen_cooking_electric","Electric (cooking)"),
    c("amen_cooking_modern","Modern fuel"),
    c("amen_cooking_traditional","Traditional fuel"),
    c("amen_lighting_electricity","Electric lighting"),
    c("amen_lighting_kerosene","Kerosene lighting"),
    c("amen_lighting_biogas","Biogas lighting"),
    c("amen_lighting_others","Other lighting")
  ),
  "Assets" = list(
    c("amen_assets_radio","Radio"), c("amen_assets_tv","TV"),
    c("amen_assets_cycle","Bicycle"), c("amen_assets_motorcycle","Motorcycle"),
    c("amen_assets_car","Car"), c("amen_assets_fridge","Fridge"),
    c("amen_assets_landline","Landline"), c("amen_assets_mobile","Mobile"),
    c("amen_assets_computer","Computer"), c("amen_assets_internet","Internet"),
    c("amen_assets_none","No durable assets"),
    c("amen_asset_count_mean","Mean asset count")
  ),
  "Housing" = list(
    c("housing_own","Own house"), c("housing_rented","Rented house"),
    c("housing_foundation_modern","Modern foundation"),
    c("housing_foundation_traditional","Traditional foundation"),
    c("housing_roof_modern","Modern roof"),
    c("housing_roof_traditional","Traditional roof")
  ),
  "Labour (15-60)" = list(
    c("work_share_agriculture","Agri work"),
    c("work_share_nonagriculture","Non-agri work"),
    c("work_share_wage_nonagri","Wage non-agri"),
    c("work_share_own_nonagri","Own-account non-agri"),
    c("work_lfp","LFP"), c("work_share_student","Student"),
    c("work_share_household_chores","Household chores"),
    c("work_share_job_seeking","Job seeking"),
    c("work_share_no_work","No work")
  ),
  "Employment type" = list(
    c("emp_share_employer","Employer"), c("emp_share_employee","Employee"),
    c("emp_share_self_employed","Self-employed"),
    c("emp_share_unpaid_family_worker","Unpaid family worker")
  ),
  "Industry shares" = list(
    c("ind_agri_forestry_fish","Agriculture, forestry & fishing"),
    c("ind_manufacturing","Manufacturing"),
    c("ind_construction","Construction"),
    c("ind_wholesale_retail","Wholesale & retail trade"),
    c("ind_transport_accommodation","Transport & accommodation"),
    c("ind_finance_real_estate_prof","Finance, RE & professional"),
    c("ind_public_admin_defence","Public admin & defence"),
    c("ind_education","Education"), c("ind_health","Health"),
    c("ind_arts_recreation","Arts & recreation"),
    c("ind_others","Other industries")
  ),
  "Occupation" = list(
    c("occ_share_armed_forces","Armed forces"),
    c("occ_share_managers","Managers"),
    c("occ_share_professionals","Professionals"),
    c("occ_share_technicians","Technicians"),
    c("occ_share_office_assistants","Office assistants"),
    c("occ_share_service_sales","Service & sales"),
    c("occ_share_agriculture","Agriculture workers"),
    c("occ_share_craft_trades","Craft & trades"),
    c("occ_share_machine_operators","Machine operators"),
    c("occ_share_elementary","Elementary")
  ),
  "Education" = list(
    c("edu_literate","Literate"),
    c("edu_literate_female","Literate (female)"),
    c("edu_literate_male","Literate (male)"),
    c("edu_school_attend_6_16","School attendance (6-16)"),
    c("edu_school_attend_6_16_female","School attendance (6-16, F)"),
    c("edu_school_attend_6_16_male","School attendance (6-16, M)"),
    c("edu_attain_primary_plus","Primary+"),
    c("edu_attain_secondary_plus","Secondary+"),
    c("edu_attain_higher_secondary_plus","Higher secondary+"),
    c("edu_attain_tertiary","Tertiary"),
    c("edu_years_mean","Mean years schooling")
  ),
  "In-migration" = list(
    c("mig_in_share","In-migrant share"),
    c("mig_in_domestic","Domestic in-migrants"),
    c("mig_in_international","International in-migrants"),
    c("mig_in_from_rural","From rural"),
    c("mig_in_from_urban","From urban"),
    c("mig_in_reason_economic","Reason: economic"),
    c("mig_in_reason_noneconomic","Reason: non-economic"),
    c("mig_in_reason_study","Reason: study"),
    c("mig_in_reason_marriage","Reason: marriage")
  ),
  "Female labour & ownership" = list(
    c("flfp_all","Female LFP"), c("flfp_agri","Female LFP (agri)"),
    c("flfp_nonagri","Female LFP (non-agri)"), c("flfp_wage","Female LFP (wage)"),
    c("flfp_chores_only","Female chores only"),
    c("fem_employment_rate","Female employment rate"),
    c("fem_share_of_ag_workers","Female share of agri workers"),
    c("fem_ag_specialization_ratio","Female agri specialization"),
    c("fem_wage_share_of_employment","Female wage employment share"),
    c("fem_ownership_house","Women own house"),
    c("fem_ownership_land","Women own land"),
    c("fem_ownership_both","Women own house+land")
  ),
  "Male labour" = list(
    c("mlfp_all","Male LFP"), c("mlfp_agri","Male LFP (agri)"),
    c("mlfp_nonagri","Male LFP (non-agri)")
  ),
  "Gender gaps & shares" = list(
    c("share_women","Share women"), c("share_men","Share men"),
    c("gap_lfp_m_minus_f","LFP gap (M-F)"),
    c("gap_nonagri_m_minus_f","Non-agri LFP gap (M-F)")
  ),
  "Household structure" = list(
    c("head_age_mean","Head age (mean)"),
    c("head_elderly_share","Elderly head"),
    c("head_young_share","Young head"),
    c("head_female_share","Female-headed HH"),
    c("head_female_elderly","Female elderly head"),
    c("absent_hh_share","HH with absentee")
  ),
  "Children left behind" = list(
    c("left_not_with_both","Left without both parents"),
    c("left_mother_only","Left with mother only"),
    c("left_father_only","Left with father only"),
    c("left_with_relatives","Left with relatives"),
    c("left_without_parents","Left without parents")
  ),
  "Marriage" = list(
    c("mar_ever_married_15_60","Ever married (15-60)"),
    c("mar_never_married_15_60","Never married (15-60)"),
    c("mar_female_age_first_mean","Female age at first marriage"),
    c("mar_female_married_by_18","Female married by 18"),
    c("mar_female_married_by_20","Female married by 20")
  ),
  "Fertility & mortality" = list(
    c("fert_birth_mean","Births (mean)"),
    c("fert_birth_son_mean","Sons (mean)"),
    c("fert_birth_dau_mean","Daughters (mean)"),
    c("fert_births_last12m_share","Births last 12 months (share)"),
    c("fert_births_last12m_rate","Births last 12 months (rate)"),
    c("mort_children_dead_mean","Children dead (mean)"),
    c("mort_child_dead_any","Any child death"),
    c("mort_child_death_ratio","Child death ratio")
  )
)

# ============================================================================
# Helpers
# ============================================================================

build_panel <- function(years, lag, ref_year) {
  # Lag the instrument by shifting its `year` variable forward by `lag`,
  # so the row tagged with year=t actually carries Z_{t-lag}.
  inst_lag <- inst |>
    mutate(year = year + lag) |>
    select(lgcode, year, geog_intensity_2001, geog_total_mig_2001, all_of(ssiv_cols))

  # Keep only baseline X with non-trivial 2001 variation
  y2001_cols <- intersect(BASE_X_BIG, names(out))
  y2001 <- out |> filter(year == 2001) |> select(lgcode, all_of(y2001_cols))
  drop  <- y2001 |> summarise(across(-lgcode, ~ sum(!is.na(.)) <= 100 | sd(., na.rm=TRUE) < 1e-6)) |>
    pivot_longer(everything()) |> filter(value) |> pull(name)
  y2001_cols <- setdiff(y2001_cols, drop)
  base2001 <- out |> filter(year == 2001) |>
    select(lgcode, all_of(y2001_cols)) |>
    rename_with(~ paste0(., "_2001"), -lgcode)

  p <- out |>
    left_join(inst_lag,  by = c("lgcode","year")) |>
    left_join(base2001,  by = "lgcode") |>
    filter(year %in% years)

  p[ssiv_cols] <- replace(p[ssiv_cols], is.na(p[ssiv_cols]), 0)
  p$geog_intensity_2001 <- replace_na(p$geog_intensity_2001, 0)
  p$log_mi <- asinh(p$geog_intensity_2001)

  # Alt shock variant: 99%-winsorised SSIV
  hi <- quantile(p$ssiv_index_2001, 0.99, na.rm = TRUE)
  p$ssiv_w99 <- pmin(p$ssiv_index_2001, hi)

  # Standardize each shock
  for (k in names(SHOCKS)) {
    col <- SHOCKS[[k]]$raw
    sd  <- sd(p[[col]], na.rm = TRUE)
    p[[paste0(k, "_z")]] <- if (!is.na(sd) && sd > 0) (p[[col]] - mean(p[[col]], na.rm=TRUE)) / sd else 0
  }

  # Build × 1{t = k} interactions for every non-reference year
  other_years <- setdiff(years, ref_year)
  base_present <- intersect(paste0(BASE_X_BIG, "_2001"), names(p))
  for (y in other_years) {
    ind <- as.integer(p$year == y)
    p[[paste0("mi_x_", y)]]         <- p$log_mi * ind
    p[[paste0("shareshock_x_", y)]] <- p$shareshock_index_2001 * ind
    for (c in base_present) {
      v <- replace_na(p[[c]], mean(p[[c]], na.rm = TRUE))
      p[[paste0(c, "_x_", y)]] <- v * ind
    }
  }

  list(panel = p, other_years = other_years, base_present = base_present)
}

control_cols <- function(tag, other_years, base_present, panel_cols) {
  yrs <- other_years
  cols <- character(0)
  if (tag == "none") return(cols)
  if (tag %in% c("mi","khanna_full"))
    cols <- c(cols, paste0("mi_x_", yrs))
  if (tag == "khanna_full") {
    cols <- c(cols, paste0("shareshock_x_", yrs))
    full <- as.vector(outer(base_present, yrs, function(c,y) paste0(c,"_x_",y)))
    cols <- c(cols, intersect(full, panel_cols))
  }
  cols
}

fit_one <- function(df, y, shock_z, controls) {
  d <- df[!is.na(df[[y]]), ]
  if (length(unique(d[[y]])) < 2 || nrow(d) < 50) return(NULL)
  rhs_terms <- c(shock_z, controls[controls %in% names(d)])
  fml <- as.formula(paste0("`", y, "` ~ ",
                           paste0("`", rhs_terms, "`", collapse = " + "),
                           " | lgcode + year"))
  m <- tryCatch(
    feols(fml, data = d, cluster = "lgcode", warn = FALSE, notes = FALSE),
    error = function(e) e
  )
  if (inherits(m, "error")) return(list(err = substr(conditionMessage(m), 1, 80)))
  ct <- m$coeftable
  if (!shock_z %in% rownames(ct)) return(list(err = "shock absorbed"))
  list(
    beta      = unname(ct[shock_z, "Estimate"]),
    se        = unname(ct[shock_z, "Std. Error"]),
    pval      = unname(ct[shock_z, "Pr(>|t|)"]),
    n         = nrow(d),
    n_mun     = length(unique(d$lgcode)),
    mean_y    = mean(d[[y]], na.rm = TRUE),
    r2_within = unname(fitstat(m, "wr2", verbose = FALSE)$wr2)
  )
}

# ============================================================================
# Run the grid
# ============================================================================

results <- list(
  meta = list(
    panels   = lapply(PANELS, function(p) list(label=p$label, years=p$years, ref=p$ref)),
    lags     = lapply(LAGS, `[[`, "label"),
    shocks   = lapply(SHOCKS, function(s) list(label=s$label, desc=s$desc)),
    controls = CONTROLS,
    groups   = names(GROUPS)
  ),
  outcomes    = list(),
  panel_info  = list(),
  estimates   = list()
)

for (gname in names(GROUPS)) {
  for (item in GROUPS[[gname]]) {
    results$outcomes[[item[1]]] <- list(label = item[2], group = gname)
  }
}

for (panel_key in names(PANELS)) {
  P <- PANELS[[panel_key]]
  results$estimates[[panel_key]]  <- list()
  results$panel_info[[panel_key]] <- list()
  for (lag_key in names(LAGS)) {
    L <- LAGS[[lag_key]]
    bp <- build_panel(P$years, L$lag, P$ref)
    df <- bp$panel
    results$estimates[[panel_key]][[lag_key]] <- list()
    results$panel_info[[panel_key]][[lag_key]] <- list(
      n_obs  = nrow(df),
      n_muni = length(unique(df$lgcode)),
      years  = sort(unique(df$year))
    )

    for (shock_key in names(SHOCKS)) {
      shock_z <- paste0(shock_key, "_z")
      if (sd(df[[shock_z]], na.rm=TRUE) < 1e-8) next
      results$estimates[[panel_key]][[lag_key]][[shock_key]] <- list()
      for (ctrl_key in names(CONTROLS)) {
        cc <- control_cols(ctrl_key, bp$other_years, bp$base_present, names(df))
        results$estimates[[panel_key]][[lag_key]][[shock_key]][[ctrl_key]] <- list()
        for (item in unlist(GROUPS, recursive = FALSE)) {
          y <- item[1]
          if (!y %in% names(df)) next
          r <- fit_one(df, y, shock_z, cc)
          if (is.null(r)) next
          results$estimates[[panel_key]][[lag_key]][[shock_key]][[ctrl_key]][[y]] <- r
        }
      }
    }
    cat(sprintf("  done panel=%s lag=%s\n", panel_key, lag_key))
  }
}

write_json(results, file.path(ROOT, "docs/results.json"),
           auto_unbox = TRUE, na = "null", null = "null", digits = NA)

n_cells <- sum(sapply(results$estimates, function(p)
  sum(sapply(p, function(l) sum(sapply(l, function(s) sum(sapply(s, length))))))))
cat(sprintf("\nTotal estimates saved: %d\nFile: %s\n", n_cells,
            file.path(ROOT, "docs/results.json")))
