# =========================================================
# EXTENDED OUTCOME GROUPS (COPY-PASTE READY)
# =========================================================
# =========================================================
# 1. OUTCOME GROUPS
# =========================================================

#--------------------------------------------------
# Amenities
#--------------------------------------------------
amenities <- c(
  "Piped water"               = "amen_water_piped",
  "Traditional water"        = "amen_water_traditional",
  "Modern cooking fuel"      = "amen_cooking_modern",
  "Traditional cooking fuel" = "amen_cooking_traditional",
  "Electric lighting"        = "amen_lighting_electricity",
  "Modern toilet"            = "amen_toilet_modern",
  "Any toilet"              = "amen_toilet_any"
)

#--------------------------------------------------
# Assets
#--------------------------------------------------
assets <- c(
  "Radio"                    = "amen_assets_radio",
  "TV"                       = "amen_assets_tv",
  "Bicycle"                  = "amen_assets_cycle",
  "Motorcycle"               = "amen_assets_motorcycle",
  "Car"                      = "amen_assets_car",
  "Fridge"                   = "amen_assets_fridge",
  "Landline"                 = "amen_assets_landline",
  "Mobile phone"             = "amen_assets_mobile",
  "Computer"                 = "amen_assets_computer",
  "Internet"                 = "amen_assets_internet",
  "Mean asset count"         = "amen_asset_count_mean"
)

#--------------------------------------------------
# Housing
#--------------------------------------------------
housing <- c(
  "Own house"                = "housing_own",
  "Rented house"             = "housing_rented",
  "Modern foundation"        = "housing_foundation_modern",
  "Traditional foundation"   = "housing_foundation_traditional",
  "Modern roof"              = "housing_roof_modern",
  "Traditional roof"         = "housing_roof_traditional"
)

#--------------------------------------------------
# Female ownership
#--------------------------------------------------
female_ownership <- c(
  "Women own house"          = "fem_ownership_house",
  "Women own land"           = "fem_ownership_land",
  "Women own both"           = "fem_ownership_both")

#--------------------------------------------------
# Enterprise
#--------------------------------------------------
enterprise <- c(
  "Any non-farm enterprise"  = "ent_has_nonagro",
  "Cottage industry"         = "ent_cottage",
  "Trade business"           = "ent_trade",
  "Transport business"       = "ent_transport",
  "Service business"         = "ent_services",
  "Other business"           = "ent_other")

#--------------------------------------------------
# Education
#--------------------------------------------------
education <- c(
  "Literate"                         = "edu_literate",
  "Female literate"                 = "edu_literate_female",
  "Male literate"                   = "edu_literate_male",
  "School attendance (6-16)"        = "edu_school_attend_6_16",
  "Female attendance (6-16)"        = "edu_school_attend_6_16_female",
  "Male attendance (6-16)"          = "edu_school_attend_6_16_male",
  "Primary+"                        = "edu_attain_primary_plus",
  "Secondary+"                      = "edu_attain_secondary_plus",
  "Higher secondary+"               = "edu_attain_higher_secondary_plus",
  "Tertiary"                        = "edu_attain_tertiary",
  "Mean years schooling"            = "edu_years_mean"
)

#--------------------------------------------------
# Marriage / Fertility
#--------------------------------------------------
demography <- c(
  "Ever married (15-60)"            = "mar_ever_married_15_60",
  "Never married (15-60)"           = "mar_never_married_15_60",
  "Female age at first marriage"    = "mar_female_age_first_mean",
  "Female married by 18"            = "mar_female_married_by_18",
  "Female married by 20"            = "mar_female_married_by_20",
  "Births mean"                     = "fert_birth_mean",
  "Sons mean"                       = "fert_birth_son_mean",
  "Daughters mean"                  = "fert_birth_dau_mean"
)

#--------------------------------------------------
# Mortality
#--------------------------------------------------
mortality <- c(
  "Children dead mean"             = "mort_children_dead_mean",
  "Any child death"                = "mort_child_dead_any",
  "Child death ratio"              = "mort_child_death_ratio"
)

#--------------------------------------------------
# Work
#--------------------------------------------------
work <- c(
  "Agriculture work"               = "work_share_agriculture",
  "Non-agriculture work"           = "work_share_nonagriculture",
  "Wage non-agriculture"           = "work_share_wage_nonagri",
  "Own-account non-agriculture"    = "work_share_own_nonagri",
  "Labour force participation"     = "work_lfp",
  "Student"                        = "work_share_student",
  "No work"                        = "work_share_no_work"
)

#--------------------------------------------------
# Occupation
#--------------------------------------------------
occupation <- c(
  "Armed forces"                = "occ_share_armed_forces",
  "Managers"                   = "occ_share_managers",
  "Professionals"              = "occ_share_professionals",
  "Technicians"                = "occ_share_technicians",
  "Office assistants"          = "occ_share_office_assistants",
  "Service & sales"            = "occ_share_service_sales",
  "Agriculture workers"        = "occ_share_agriculture",
  "Craft & trades"             = "occ_share_craft_trades",
  "Machine operators"          = "occ_share_machine_operators",
  "Elementary occupations"     = "occ_share_elementary"
)

#--------------------------------------------------
# Migration
#--------------------------------------------------
migration <- c(
  "In-migrant share"               = "mig_in_share",
  "Domestic migrants"              = "mig_in_domestic",
  "International migrants"         = "mig_in_international",
  "From rural"                     = "mig_in_from_rural",
  "From urban"                     = "mig_in_from_urban",
  "Economic reason"                = "mig_in_reason_economic",
  "Noneconomic reason"             = "mig_in_reason_noneconomic",
  "Study reason"                   = "mig_in_reason_study",
  "Marriage reason"                = "mig_in_reason_marriage"
)

#--------------------------------------------------
# Gender / FLFP
#--------------------------------------------------
gender <- c(
  "Female LFP"                     = "flfp_all",
  "Female employment rate"         = "fem_employment_rate",
  "Female agriculture"             = "flfp_agri",
  "Female non-agriculture"         = "flfp_nonagri",
  "Male LFP"                       = "mlfp_all",
  "Share women"                    = "share_women",
  "Share men"                      = "share_men",
  "Female-headed households"       = "head_female_share"
)

#--------------------------------------------------
# Household structure
#--------------------------------------------------
household <- c(
  "Head age mean"                 = "head_age_mean",
  "Head elderly share"            = "head_elderly_share",
  "Head young share"              = "head_young_share",
  "Left without both parents"     = "left_not_with_both",
  "Left mother only"              = "left_mother_only",
  "Left father only"              = "left_father_only",
  "Left with relatives"           = "left_with_relatives",
  "Left without parents"          = "left_without_parents"
)


#--------------------------------------------------
# Industry
#--------------------------------------------------
industry <- c(
  "Agriculture, forestry & fishing"   = "ind_agri_forestry_fish",
  "Manufacturing"                    = "ind_manufacturing",
  "Construction"                     = "ind_construction",
  "Wholesale & retail trade"         = "ind_wholesale_retail",
  "Transport & accommodation"        = "ind_transport_accommodation",
  "Finance, real estate & professional" = "ind_finance_real_estate_prof",
  "Public admin & defence"           = "ind_public_admin_defence",
  "Education"                        = "ind_education",
  "Health"                           = "ind_health",
  "Arts & recreation"                = "ind_arts_recreation",
  "Other industries"                 = "ind_others"
)

library(fixest)
library(dplyr)
library(tibble)
library(purrr)

run_ssiv_regressions <- function(outcomes, 
                                 data = panel_df,
                                 shock_var = "fxshock",
                                 interaction_var = "log_mi",
                                 ref_year = 2011,
                                 fe = c("lgcode", "year"),
                                 cluster_var = "lgcode",
                                 digits = 4) {
  
  # Build FE formula piece
  fe_formula <- paste(fe, collapse = " + ")
  
  # Helper: format coefficient with stars
  star_it <- function(p) {
    case_when(
      is.na(p)   ~ "",
      p < 0.001  ~ "***",
      p < 0.01   ~ "**",
      p < 0.05   ~ "*",
      p < 0.1    ~ ".",
      TRUE       ~ ""
    )
  }
  
  # Helper: format coef (est) with stars and SE below
  fmt_coef <- function(est, se, p) {
    sprintf("%.*f%s (%.*f)", digits, est, star_it(p), digits, se)
  }
  
  # Run one regression and extract what we need
  run_one <- function(outcome) {
    f <- as.formula(
      paste0(outcome, " ~ ", shock_var, 
             " + i(year, ", interaction_var, ", ref = ", ref_year, ") | ",
             fe_formula)
    )
    
    m <- feols(f, data = data, cluster = as.formula(paste0("~", cluster_var)))
    
    # Coefficient table (clustered SEs already applied)
    ct <- as.data.frame(summary(m)$coeftable)
    ct$term <- rownames(ct)
    
    # Find the two rows we care about
    shock_row <- ct[ct$term == shock_var, ]
    # The interaction term name fixest builds; pull the non-reference year term
    int_row <- ct[grepl(paste0("^year::.*:", interaction_var, "$"), ct$term), ]
    
    # 95% CI (t-based using model df)
    ci_shock <- confint(m, parm = shock_var, level = 0.95)
    ci_int   <- confint(m)[grep(paste0("^year::.*:", interaction_var, "$"), rownames(confint(m))), , drop = FALSE]
    
    tibble(
      outcome      = outcome,
      n_obs        = nobs(m),
      outcome_mean = mean(data[[outcome]], na.rm = TRUE),
      
      # fxshock
      shock_coef   = fmt_coef(shock_row$Estimate, shock_row$`Std. Error`, shock_row$`Pr(>|t|)`),
      shock_p      = shock_row$`Pr(>|t|)`,
      shock_ci     = sprintf("[%.*f, %.*f]", digits, ci_shock[1], digits, ci_shock[2]),
      
      # interaction (log_mi × non-ref year)
      int_term     = int_row$term,
      int_coef     = fmt_coef(int_row$Estimate, int_row$`Std. Error`, int_row$`Pr(>|t|)`),
      int_p        = int_row$`Pr(>|t|)`,
      int_ci       = sprintf("[%.*f, %.*f]", digits, ci_int[1, 1], digits, ci_int[1, 2])
    )
  }
  
  results <- map_dfr(outcomes, run_one)
  return(results)
}