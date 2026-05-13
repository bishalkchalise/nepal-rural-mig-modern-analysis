"""
Curated outcome groupings for the Robustness portal.

Each (dataset, variable) is assigned to one named group with a polished
display label.  Variables not listed are dropped from the portal — that
trims the 461 raw outcomes down to a curated working set, kills the
dumping-ground 'Other' group, and merges artificial splits like
'Health' vs 'Health spending'.

The data structure: CURATED[dataset] is a dict
    var_name -> {"group": group_name, "label": display_label}
Order is preserved so groups render in the order they're declared.

Renames applied here (NOT changing the underlying variable name):
  - "Absent" / "Absentee" framing -> "Migrants"
  - "Owns irrigation kit" -> "Owns irrigation equipment"
  - Plain code names get sentence-case readable labels everywhere.
"""

CURATED = {}

# ============================================================================
# CENSUS — ~80 outcomes across 13 groups
# ============================================================================
CURATED["census"] = {
  # ---- Migrants (was "Absent HH" + In-migration) ----
  "absent_hh_share":            {"group": "Migrants & migration", "label": "Share of HH with migrant member"},
  "hh_death_12m":               {"group": "Migrants & migration", "label": "HH with death in last 12m"},
  "mig_in_share":               {"group": "Migrants & migration", "label": "In-migrant share (population)"},
  "mig_in_domestic":            {"group": "Migrants & migration", "label": "In-migrants from within Nepal"},
  "mig_in_international":       {"group": "Migrants & migration", "label": "In-migrants from abroad"},
  "mig_in_from_rural":          {"group": "Migrants & migration", "label": "In-migrants from rural Nepal"},
  "mig_in_from_urban":          {"group": "Migrants & migration", "label": "In-migrants from urban Nepal"},
  "mig_in_reason_economic":     {"group": "Migrants & migration", "label": "In-mig: economic reason"},
  "mig_in_reason_marriage":     {"group": "Migrants & migration", "label": "In-mig: marriage reason"},
  "mig_in_reason_study":        {"group": "Migrants & migration", "label": "In-mig: study reason"},
  "mig_in_reason_noneconomic":  {"group": "Migrants & migration", "label": "In-mig: non-economic reason"},

  # ---- Labour force participation ----
  "mlfp_all":                   {"group": "Labour force", "label": "Male LFP (all)"},
  "mlfp_agri":                  {"group": "Labour force", "label": "Male LFP in agriculture"},
  "mlfp_nonagri":               {"group": "Labour force", "label": "Male LFP in non-agriculture"},
  "flfp_all":                   {"group": "Labour force", "label": "Female LFP (all)"},
  "flfp_agri":                  {"group": "Labour force", "label": "Female LFP in agriculture"},
  "flfp_nonagri":               {"group": "Labour force", "label": "Female LFP in non-agriculture"},
  "flfp_wage":                  {"group": "Labour force", "label": "Female wage-LFP"},
  "flfp_chores_only":           {"group": "Labour force", "label": "Female: chores only"},
  "fem_employment_rate":        {"group": "Labour force", "label": "Female employment rate"},
  "gap_lfp_m_minus_f":          {"group": "Labour force", "label": "Gender gap in LFP (M − F)"},
  "gap_nonagri_m_minus_f":      {"group": "Labour force", "label": "Gender gap in non-ag (M − F)"},
  "work_lfp":                   {"group": "Labour force", "label": "Working population LFP"},

  # ---- Industry (sector of employment) ----
  "ind_agri_forestry_fish":     {"group": "Industry (sector)", "label": "Agriculture, forestry, fishing"},
  "ind_manufacturing":          {"group": "Industry (sector)", "label": "Manufacturing"},
  "ind_construction":           {"group": "Industry (sector)", "label": "Construction"},
  "ind_wholesale_retail":       {"group": "Industry (sector)", "label": "Wholesale & retail trade"},
  "ind_transport_accommodation":{"group": "Industry (sector)", "label": "Transport & accommodation"},
  "ind_finance_real_estate_prof":{"group":"Industry (sector)", "label": "Finance / real estate / prof"},
  "ind_public_admin_defence":   {"group": "Industry (sector)", "label": "Public admin & defence"},
  "ind_education":              {"group": "Industry (sector)", "label": "Education sector"},
  "ind_health":                 {"group": "Industry (sector)", "label": "Health sector"},
  "ind_arts_recreation":        {"group": "Industry (sector)", "label": "Arts & recreation"},
  "ind_others":                 {"group": "Industry (sector)", "label": "Other industries"},

  # ---- Occupation (ISCO major) ----
  "occ_share_managers":         {"group": "Occupation", "label": "Managers"},
  "occ_share_professionals":    {"group": "Occupation", "label": "Professionals"},
  "occ_share_technicians":      {"group": "Occupation", "label": "Technicians"},
  "occ_share_office_assistants":{"group": "Occupation", "label": "Office assistants"},
  "occ_share_service_sales":    {"group": "Occupation", "label": "Service & sales"},
  "occ_share_agriculture":      {"group": "Occupation", "label": "Agricultural occupations"},
  "occ_share_craft_trades":     {"group": "Occupation", "label": "Craft & trades"},
  "occ_share_machine_operators":{"group": "Occupation", "label": "Machine operators"},
  "occ_share_elementary":       {"group": "Occupation", "label": "Elementary"},
  "occ_share_armed_forces":     {"group": "Occupation", "label": "Armed forces"},

  # ---- Work activity / employment status ----
  "work_share_agriculture":     {"group": "Work activity", "label": "Working in agriculture"},
  "work_share_nonagriculture":  {"group": "Work activity", "label": "Working in non-agriculture"},
  "work_share_wage_nonagri":    {"group": "Work activity", "label": "Wage employment, non-ag"},
  "work_share_own_nonagri":     {"group": "Work activity", "label": "Self-employed, non-ag"},
  "work_share_student":         {"group": "Work activity", "label": "Studying"},
  "work_share_household_chores":{"group": "Work activity", "label": "Household chores only"},
  "work_share_job_seeking":     {"group": "Work activity", "label": "Job-seeking"},
  "work_share_no_work":         {"group": "Work activity", "label": "Not working"},
  "emp_share_employee":         {"group": "Work activity", "label": "Employed for wages"},
  "emp_share_employer":         {"group": "Work activity", "label": "Self: employer"},
  "emp_share_self_employed":    {"group": "Work activity", "label": "Self: own-account"},
  "emp_share_unpaid_family_worker":{"group":"Work activity", "label":"Unpaid family worker"},

  # ---- Marriage ----
  "mar_ever_married_15_60":     {"group": "Marriage", "label": "Ever married (15–60)"},
  "mar_never_married_15_60":    {"group": "Marriage", "label": "Never married (15–60)"},
  "mar_female_age_first_mean":  {"group": "Marriage", "label": "Female age at first marriage"},
  "mar_female_married_by_18":   {"group": "Marriage", "label": "Female married by 18"},
  "mar_female_married_by_20":   {"group": "Marriage", "label": "Female married by 20"},

  # ---- Fertility ----
  "fert_birth_mean":            {"group": "Fertility", "label": "Total births per woman"},
  "fert_birth_son_mean":        {"group": "Fertility", "label": "Sons born per woman"},
  "fert_birth_dau_mean":        {"group": "Fertility", "label": "Daughters born per woman"},
  "fert_births_last12m_rate":   {"group": "Fertility", "label": "Births in last 12m (rate)"},
  "fert_births_last12m_share":  {"group": "Fertility", "label": "Births in last 12m (share)"},

  # ---- Child mortality ----
  "mort_child_dead_any":        {"group": "Child mortality", "label": "Any child death"},
  "mort_child_death_ratio":     {"group": "Child mortality", "label": "Child deaths per woman (ratio)"},
  "mort_children_dead_mean":    {"group": "Child mortality", "label": "Mean children dead"},

  # ---- Education ----
  "edu_literate":               {"group": "Education", "label": "Literacy rate"},
  "edu_literate_female":        {"group": "Education", "label": "Female literacy"},
  "edu_literate_male":          {"group": "Education", "label": "Male literacy"},
  "edu_school_attend_6_16":     {"group": "Education", "label": "School attendance 6–16"},
  "edu_school_attend_6_16_female":{"group":"Education", "label":"Female attendance 6–16"},
  "edu_school_attend_6_16_male":{"group":"Education", "label":"Male attendance 6–16"},
  "edu_years_mean":             {"group": "Education", "label": "Mean years of schooling"},
  "edu_attain_primary_plus":    {"group": "Education", "label": "Attained primary+"},
  "edu_attain_secondary_plus":  {"group": "Education", "label": "Attained secondary+"},
  "edu_attain_higher_secondary_plus":{"group":"Education", "label":"Attained higher-secondary+"},
  "edu_attain_tertiary":        {"group": "Education", "label": "Attained tertiary"},

  # ---- Housing ----
  "housing_own":                {"group": "Housing", "label": "Own dwelling"},
  "housing_rented":             {"group": "Housing", "label": "Rented dwelling"},
  "housing_foundation_modern":  {"group": "Housing", "label": "Modern foundation"},
  "housing_foundation_traditional":{"group":"Housing", "label":"Traditional foundation"},
  "housing_roof_modern":        {"group": "Housing", "label": "Modern roof"},
  "housing_roof_traditional":   {"group": "Housing", "label": "Traditional roof"},

  # ---- Amenities (cooking / lighting / water / toilet) ----
  "amen_cooking_modern":        {"group": "Amenities", "label": "Modern cooking fuel"},
  "amen_cooking_traditional":   {"group": "Amenities", "label": "Traditional cooking fuel"},
  "amen_cooking_lpg":           {"group": "Amenities", "label": "Cooks with LPG"},
  "amen_cooking_electric":      {"group": "Amenities", "label": "Cooks with electricity"},
  "amen_cooking_biogas":        {"group": "Amenities", "label": "Cooks with biogas"},
  "amen_cooking_wood":          {"group": "Amenities", "label": "Cooks with wood"},
  "amen_cooking_kerosene":      {"group": "Amenities", "label": "Cooks with kerosene"},
  "amen_lighting_electricity":  {"group": "Amenities", "label": "Electricity for lighting"},
  "amen_lighting_kerosene":     {"group": "Amenities", "label": "Kerosene lighting"},
  "amen_lighting_biogas":       {"group": "Amenities", "label": "Biogas lighting"},
  "amen_lighting_others":       {"group": "Amenities", "label": "Other lighting"},
  "amen_water_piped":           {"group": "Amenities", "label": "Piped water"},
  "amen_water_traditional":     {"group": "Amenities", "label": "Traditional water source"},
  "amen_toilet_any":            {"group": "Amenities", "label": "Any toilet"},
  "amen_toilet_modern":         {"group": "Amenities", "label": "Modern toilet"},
  "amen_toilet_ordinary":       {"group": "Amenities", "label": "Ordinary toilet"},
  "amen_toilet_none":           {"group": "Amenities", "label": "No toilet"},

  # ---- Assets (durables) ----
  "amen_asset_count_mean":      {"group": "Assets (durables)", "label": "Durables per HH (count)"},
  "amen_assets_mobile":         {"group": "Assets (durables)", "label": "Owns mobile"},
  "amen_assets_radio":          {"group": "Assets (durables)", "label": "Owns radio"},
  "amen_assets_tv":             {"group": "Assets (durables)", "label": "Owns TV"},
  "amen_assets_fridge":         {"group": "Assets (durables)", "label": "Owns fridge"},
  "amen_assets_computer":       {"group": "Assets (durables)", "label": "Owns computer"},
  "amen_assets_internet":       {"group": "Assets (durables)", "label": "Has internet"},
  "amen_assets_landline":       {"group": "Assets (durables)", "label": "Has landline"},
  "amen_assets_cycle":          {"group": "Assets (durables)", "label": "Owns bicycle"},
  "amen_assets_motorcycle":     {"group": "Assets (durables)", "label": "Owns motorcycle"},
  "amen_assets_car":            {"group": "Assets (durables)", "label": "Owns car"},
  "amen_assets_none":           {"group": "Assets (durables)", "label": "No durables"},

  # ---- Female autonomy ----
  "fem_ownership_house":        {"group": "Female autonomy", "label": "Female owns house"},
  "fem_ownership_land":         {"group": "Female autonomy", "label": "Female owns land"},
  "fem_ownership_both":         {"group": "Female autonomy", "label": "Female owns house + land"},
  "fem_share_of_ag_workers":    {"group": "Female autonomy", "label": "Female share of ag workers"},
  "fem_wage_share_of_employment":{"group":"Female autonomy", "label":"Female share of wage emp"},
  "fem_ag_specialization_ratio":{"group":"Female autonomy", "label":"Female ag-specialisation ratio"},

  # ---- HH demographics ----
  "head_female_share":          {"group": "HH demographics", "label": "Female-headed HH"},
  "head_age_mean":              {"group": "HH demographics", "label": "Mean head age"},
  "head_elderly_share":         {"group": "HH demographics", "label": "Elderly head"},
  "head_young_share":           {"group": "HH demographics", "label": "Young head"},
  "head_female_elderly":        {"group": "HH demographics", "label": "Female elderly head"},
  "share_men":                  {"group": "HH demographics", "label": "Male population share"},
  "share_women":                {"group": "HH demographics", "label": "Female population share"},

  # ---- Left-behind children ----
  "left_father_only":           {"group": "Left-behind children", "label": "Left with father only"},
  "left_mother_only":           {"group": "Left-behind children", "label": "Left with mother only"},
  "left_with_relatives":        {"group": "Left-behind children", "label": "Left with relatives"},
  "left_not_with_both":         {"group": "Left-behind children", "label": "Not with both parents"},
  "left_without_parents":       {"group": "Left-behind children", "label": "Without parents"},

  # ---- HH enterprise ----
  "ent_has_nonagro":            {"group": "HH enterprise", "label": "Has non-ag enterprise"},
  "ent_cottage":                {"group": "HH enterprise", "label": "Cottage industry"},
  "ent_trade":                  {"group": "HH enterprise", "label": "Trade enterprise"},
  "ent_services":               {"group": "HH enterprise", "label": "Services enterprise"},
  "ent_transport":              {"group": "HH enterprise", "label": "Transport enterprise"},
  "ent_other":                  {"group": "HH enterprise", "label": "Other enterprise"},
}

# ============================================================================
# HH (HRVS) — ~120 outcomes across 15 groups
# ============================================================================
CURATED["hh"] = {
  # ---- Migrants (1st stage / extensive margin) ----
  "has_migrant_international":  {"group": "Migrants", "label": "HH with int'l migrant"},
  "has_migrant_internal":       {"group": "Migrants", "label": "HH with internal migrant"},
  "has_only_international":     {"group": "Migrants", "label": "Only international migrants"},
  "has_only_internal":          {"group": "Migrants", "label": "Only internal migrants"},
  "has_both_internal_and_international":{"group":"Migrants","label":"Both internal + international"},
  "n_migrants_total":           {"group": "Migrants", "label": "Total migrants per HH"},
  "n_migrants_international":   {"group": "Migrants", "label": "Int'l migrants per HH"},
  "n_migrants_internal":        {"group": "Migrants", "label": "Internal migrants per HH"},
  "n_migrants_male":            {"group": "Migrants", "label": "Male migrants per HH"},
  "n_migrants_female":          {"group": "Migrants", "label": "Female migrants per HH"},
  "log_n_migrants_total":       {"group": "Migrants", "label": "log(# migrants total)"},
  "log_n_migrants_international":{"group":"Migrants", "label":"log(# int'l migrants)"},
  "log_n_migrants_male":        {"group": "Migrants", "label": "log(# male migrants)"},
  "log_n_migrants_female":      {"group": "Migrants", "label": "log(# female migrants)"},
  "share_long_term_migrants":   {"group": "Migrants", "label": "Share long-term migrants"},
  "share_male_migrants":        {"group": "Migrants", "label": "Share male migrants"},
  "intl_migration_share":       {"group": "Migrants", "label": "Share of intl in migrants"},

  # ---- Remittances ----
  "remit_amount_intl_12m_rs":   {"group": "Remittances", "label": "Intl remit, Rs (12m)"},
  "remit_amount_12m_rs":        {"group": "Remittances", "label": "Total remit, Rs (12m)"},
  "log_remit_amount_intl_12m_rs":{"group":"Remittances","label":"log(intl remit, Rs)"},
  "log_remit_amount_12m_rs":    {"group": "Remittances", "label": "log(total remit, Rs)"},
  "remittance_amt":             {"group": "Remittances", "label": "Remittance amount (alt)"},
  "remittance_any":             {"group": "Remittances", "label": "Receives any remittance"},
  "log_remittance_amt":         {"group": "Remittances", "label": "log(remittance amt)"},
  "remit_received":             {"group": "Remittances", "label": "Any remittance received"},
  "remit_per_international_migrant":{"group":"Remittances","label":"Remit per intl migrant"},
  "remit_per_migrant":          {"group": "Remittances", "label": "Remit per migrant"},
  "share_migrants_sending":     {"group": "Remittances", "label": "Share of migrants sending"},

  # ---- Migration costs ----
  "mig_cost_12m_rs":            {"group": "Migration costs", "label": "Mig cost (12m, Rs)"},
  "mig_cost_financed_by_loan_any":{"group":"Migration costs","label":"Loan-financed cost"},
  "cost_per_migrant":           {"group": "Migration costs", "label": "Cost per migrant (Rs)"},
  "cost_to_monthly_earning_ratio":{"group":"Migration costs","label":"Cost/earnings ratio"},
  "migrant_monthly_earning_total_rs":{"group":"Migration costs","label":"Migrant monthly earnings"},

  # ---- Remittance use ----
  "remit_use_consumption_any":  {"group": "Remittance use", "label": "Used for consumption"},
  "remit_use_human_capital_any":{"group": "Remittance use", "label": "Used for human capital"},
  "remit_use_business_investment_any":{"group":"Remittance use","label":"Used for business inv."},
  "remit_use_land_housing_any": {"group": "Remittance use", "label": "Used for land / housing"},
  "remit_use_investment_any_hh":{"group": "Remittance use", "label": "Used for any investment"},
  "remit_use_debt_any":         {"group": "Remittance use", "label": "Used to repay debt"},

  # ---- Land area ----
  "agro_hh":                    {"group": "Land area", "label": "Agricultural HH"},
  "cultivated_area_sqm":        {"group": "Land area", "label": "Cultivated area (sqm)"},
  "cultivated_area_total_sqm":  {"group": "Land area", "label": "Total cultivated area"},
  "total_owned_area_sqm":       {"group": "Land area", "label": "Total owned area"},
  "rented_in_area_sqm":         {"group": "Land area", "label": "Rented-in area"},
  "n_plots_owned":              {"group": "Land area", "label": "# plots owned"},

  # ---- Land use (season × use) ----
  "share_self_wet":             {"group": "Land use", "label": "Own-cultivated, wet"},
  "share_self_dry":             {"group": "Land use", "label": "Own-cultivated, dry"},
  "share_both_seasons":         {"group": "Land use", "label": "Cultivated both seasons"},
  "share_fallow_wet":           {"group": "Land use", "label": "Fallow, wet"},
  "share_fallow_dry":           {"group": "Land use", "label": "Fallow, dry"},
  "share_rented_out_wet":       {"group": "Land use", "label": "Rented out, wet"},

  # ---- Cropping ----
  "crop_simpson_diversity":     {"group": "Cropping", "label": "Crop diversity (Simpson)"},
  "crop_hhi":                   {"group": "Cropping", "label": "Crop HHI"},
  "effective_n_crops":          {"group": "Cropping", "label": "Effective # crops"},
  "n_crops_total":              {"group": "Cropping", "label": "# crops (total)"},
  "n_crops_wet":                {"group": "Cropping", "label": "# crops (wet)"},
  "n_crops_dry":                {"group": "Cropping", "label": "# crops (dry)"},
  "grows_horticulture":         {"group": "Cropping", "label": "Grows horticulture"},
  "grows_staple":               {"group": "Cropping", "label": "Grows staple"},
  "grows_cashcrop":             {"group": "Cropping", "label": "Grows cash crop"},
  "multi_season":               {"group": "Cropping", "label": "Cultivates multi-season"},
  "any_crop_sold":              {"group": "Cropping", "label": "Any crop sold"},
  "crop_sale_rs_12m":           {"group": "Cropping", "label": "Crop sales (Rs/12m)"},
  "crop_sale_share":            {"group": "Cropping", "label": "Crop-sale share"},
  "horti_value_share":          {"group": "Cropping", "label": "Horticulture value share"},
  "staple_value_share":         {"group": "Cropping", "label": "Staple value share"},
  "cashcrop_value_share":       {"group": "Cropping", "label": "Cash-crop value share"},

  # ---- Irrigation ----
  "share_irr_groundwater_wet":  {"group": "Irrigation", "label": "Groundwater irr., wet"},
  "share_irr_groundwater_dry":  {"group": "Irrigation", "label": "Groundwater irr., dry"},
  "share_irr_surface_wet":      {"group": "Irrigation", "label": "Surface irr., wet"},
  "share_irr_surface_dry":      {"group": "Irrigation", "label": "Surface irr., dry"},
  "share_irr_rainfed_wet":      {"group": "Irrigation", "label": "Rainfed, wet"},
  "share_irr_rainfed_dry":      {"group": "Irrigation", "label": "Rainfed, dry"},
  "n_irrigation_types":         {"group": "Irrigation", "label": "# irrigation types"},

  # ---- Capital equipment ----
  "owns_plough":                {"group": "Capital equipment", "label": "Owns plough"},
  "owns_powered_machinery":     {"group": "Capital equipment", "label": "Owns powered machinery"},
  "owns_irrigation_kit":        {"group": "Capital equipment", "label": "Owns irrigation equipment"},
  "owns_storage_struct":        {"group": "Capital equipment", "label": "Owns storage structure"},
  "owns_transport":             {"group": "Capital equipment", "label": "Owns transport"},
  "n_equip_categories":         {"group": "Capital equipment", "label": "# equipment categories"},
  "n_powered_types":            {"group": "Capital equipment", "label": "# powered types"},
  "equip_stock_value_rs":       {"group": "Capital equipment", "label": "Equipment stock (Rs)"},

  # ---- Input costs ----
  "total_input_cost_rs":        {"group": "Input costs", "label": "Total input cost (Rs)"},
  "wet_cost_seed":              {"group": "Input costs", "label": "Seed cost (wet)"},
  "dry_cost_seed":              {"group": "Input costs", "label": "Seed cost (dry)"},
  "wet_cost_fert":              {"group": "Input costs", "label": "Fertiliser cost (wet)"},
  "dry_cost_fert":              {"group": "Input costs", "label": "Fertiliser cost (dry)"},
  "wet_cost_labour":            {"group": "Input costs", "label": "Hired labour (wet)"},
  "dry_cost_labour":            {"group": "Input costs", "label": "Hired labour (dry)"},
  "wet_cost_insect":            {"group": "Input costs", "label": "Pesticide cost (wet)"},
  "dry_cost_insect":            {"group": "Input costs", "label": "Pesticide cost (dry)"},
  "input_intensity_per_sqm":    {"group": "Input costs", "label": "Input intensity per sqm"},

  # ---- Livestock ----
  "livestock_has":              {"group": "Livestock", "label": "Owns livestock"},

  # ---- Food consumption ----
  "food_exp_total_7day":        {"group": "Food consumption", "label": "Food spending / week (Rs)"},
  "food_exp_protein_7day":      {"group": "Food consumption", "label": "Protein spending / week"},
  "food_exp_staples_7day":      {"group": "Food consumption", "label": "Staples spending / week"},
  "food_exp_purchased_7day":    {"group": "Food consumption", "label": "Purchased food / week"},
  "food_exp_homeprod_7day":     {"group": "Food consumption", "label": "Home-produced food / week"},
  "food_exp_vice_7day":         {"group": "Food consumption", "label": "Alcohol & tobacco / week"},
  "food_insec_score":           {"group": "Food consumption", "label": "Food insecurity score (HFIAS)"},
  "food_insec_any":             {"group": "Food consumption", "label": "Any food insecurity"},
  "food_insec_worried":         {"group": "Food consumption", "label": "Worried about food"},

  # ---- Non-food consumption ----
  "nonfood_exp_12m":            {"group": "Non-food consumption", "label": "Non-food spending (12m)"},
  "nonfood_exp_30day":          {"group": "Non-food consumption", "label": "Non-food spending (30d)"},
  "nonfood_clothing_footwear_12m":{"group":"Non-food consumption","label":"Clothing & footwear"},
  "nonfood_fuel_lighting_12m":  {"group": "Non-food consumption", "label": "Fuel & lighting"},
  "nonfood_transport_12m":      {"group": "Non-food consumption", "label": "Transport"},
  "nonfood_communication_12m":  {"group": "Non-food consumption", "label": "Communication"},
  "nonfood_personal_care_12m":  {"group": "Non-food consumption", "label": "Personal care"},
  "nonfood_household_goods_12m":{"group":"Non-food consumption","label":"Household goods"},
  "nonfood_housing_improvement_12m":{"group":"Non-food consumption","label":"Housing improvement"},
  "nonfood_electronics_tech_12m":{"group":"Non-food consumption","label":"Electronics / tech"},
  "nonfood_jewellery_luxury_12m":{"group":"Non-food consumption","label":"Jewellery / luxury"},
  "nonfood_entertainment_leisure_12m":{"group":"Non-food consumption","label":"Entertainment"},
  "nonfood_ceremonies_12m":     {"group": "Non-food consumption", "label": "Ceremonies"},
  "nonfood_taxes_12m":          {"group": "Non-food consumption", "label": "Taxes"},
  "nonfood_other_nonfood_12m":  {"group": "Non-food consumption", "label": "Other non-food"},
  "durables_stock_value":       {"group": "Non-food consumption", "label": "Durables stock value"},
  "durables_use_value_12m":     {"group": "Non-food consumption", "label": "Durables use-value (12m)"},

  # ---- Health ----
  "any_health_spending":        {"group": "Health", "label": "Any health spending"},
  "any_insured":                {"group": "Health", "label": "Any insurance"},
  "n_insured":                  {"group": "Health", "label": "# insured members"},
  "n_chronic":                  {"group": "Health", "label": "# chronic conditions"},
  "n_acute_illness":            {"group": "Health", "label": "# acute illnesses"},
  "hlt_spend_total":            {"group": "Health", "label": "Health spending total (Rs)"},
  "hlt_spend_medicines":        {"group": "Health", "label": "Medicines spending"},
  "hlt_spend_hospital":         {"group": "Health", "label": "Hospital spending"},
  "hlt_spend_fees":             {"group": "Health", "label": "Doctor fees"},
  "hlt_spend_tests":            {"group": "Health", "label": "Diagnostic tests"},
  "hlt_spend_other":            {"group": "Health", "label": "Other health spending"},

  # ---- Education ----
  "any_enrolled":               {"group": "Education", "label": "Any HH member enrolled"},
  "n_enrolled":                 {"group": "Education", "label": "# enrolled"},
  "n_private_school":           {"group": "Education", "label": "# in private school"},
  "n_scholarship":              {"group": "Education", "label": "# with scholarship"},
  "scholarship_amt_12m":        {"group": "Education", "label": "Scholarship amt (12m)"},
  "n_school_age":               {"group": "Education", "label": "# school-age children"},
  "edu_spend_total_12m":        {"group": "Education", "label": "Total education spend (12m)"},
  "edu_spend_tuition_12m":      {"group": "Education", "label": "Tuition spend"},
  "edu_spend_books_12m":        {"group": "Education", "label": "Books / stationery"},
  "edu_spend_uniforms_12m":     {"group": "Education", "label": "Uniforms"},
  "edu_spend_transport_12m":    {"group": "Education", "label": "Education transport"},
  "edu_spend_food_lodging_12m": {"group": "Education", "label": "Food & lodging (edu)"},
  "edu_spend_other_12m":        {"group": "Education", "label": "Other education spend"},
  "edu_spend_per_enrolled":     {"group": "Education", "label": "Spend per enrolled"},

  # ---- HH enterprise ----
  "has_enterprise":             {"group": "HH enterprise", "label": "Has any enterprise"},
  "n_enterprises":              {"group": "HH enterprise", "label": "# enterprises"},
  "n_workers_total":            {"group": "HH enterprise", "label": "# enterprise workers"},
  "revenue_12m":                {"group": "HH enterprise", "label": "Enterprise revenue"},
  "profit_12m":                 {"group": "HH enterprise", "label": "Enterprise profit"},
  "expenses_12m":               {"group": "HH enterprise", "label": "Enterprise expenses"},
  "capex_12m":                  {"group": "HH enterprise", "label": "Enterprise capex"},
  "sector_trade":               {"group": "HH enterprise", "label": "Trade enterprise"},
  "sector_manufacturing":       {"group": "HH enterprise", "label": "Manufacturing enterprise"},
  "sector_services":            {"group": "HH enterprise", "label": "Services enterprise"},
  "sector_hotels":              {"group": "HH enterprise", "label": "Hotels / hospitality"},
  "sector_transport":           {"group": "HH enterprise", "label": "Transport enterprise"},

  # ---- Shocks ----
  "any_shock":                  {"group": "Shocks", "label": "Any shock"},
  "agricultural_shock_any":     {"group": "Shocks", "label": "Agricultural shock"},
  "economic_shock_any":         {"group": "Shocks", "label": "Economic shock"},
  "death_shock_any":            {"group": "Shocks", "label": "Death shock"},
  "health_shock_any":           {"group": "Shocks", "label": "Health shock"},
  "natural_disaster_shock_any": {"group": "Shocks", "label": "Natural disaster shock"},
  "multiple_shocks":            {"group": "Shocks", "label": "Multiple shocks (>1)"},
  "n_shocks":                   {"group": "Shocks", "label": "# shocks"},
  "severe_loss_any":            {"group": "Shocks", "label": "Severe loss"},
  "total_loss_rs":              {"group": "Shocks", "label": "Total loss (Rs)"},
  "mean_loss_per_shock_rs":     {"group": "Shocks", "label": "Mean loss per shock"},

  # ---- Coping ----
  "any_coping_reported":        {"group": "Coping", "label": "Any coping reported"},
  "cope_borrow_any":            {"group": "Coping", "label": "Borrowed to cope"},
  "cope_savings_any":           {"group": "Coping", "label": "Used savings"},
  "cope_sell_assets_any":       {"group": "Coping", "label": "Sold assets"},
  "cope_migration_remittance_any":{"group":"Coping","label":"Used migration / remit"},
  "cope_public_private_aid_any":{"group": "Coping", "label": "Took public / private aid"},
  "cope_credit_or_asset_any":   {"group": "Coping", "label": "Credit or asset sale"},
  "cope_external_support_any":  {"group": "Coping", "label": "External support"},
  "cope_self_insurance_any":    {"group": "Coping", "label": "Self-insurance"},
  "main_coping_borrow":         {"group": "Coping", "label": "Main coping: borrow"},
  "main_coping_savings_consumption":{"group":"Coping","label":"Main: savings / cut cons."},
  "main_coping_sell_assets":    {"group": "Coping", "label": "Main: sell assets"},
  "main_coping_external_support":{"group":"Coping","label":"Main: external support"},

  # ---- Social protection ----
  "public_cash_any":            {"group": "Social protection", "label": "Public cash transfer"},
  "public_cash_amt":            {"group": "Social protection", "label": "Public cash amount"},
  "public_inkind_any":          {"group": "Social protection", "label": "Public in-kind"},
  "public_inkind_amt":          {"group": "Social protection", "label": "Public in-kind amount"},
  "public_support_any":         {"group": "Social protection", "label": "Any public support"},
  "public_support_amt":         {"group": "Social protection", "label": "Public support amount"},
  "public_work_any":            {"group": "Social protection", "label": "Public works employed"},
  "public_work_days":           {"group": "Social protection", "label": "Public works days"},
  "public_work_earnings":       {"group": "Social protection", "label": "Public works earnings"},
  "demographic_cash_any":       {"group": "Social protection", "label": "Demographic cash"},
  "demographic_cash_amt":       {"group": "Social protection", "label": "Demographic cash amount"},
  "disaster_cash_any":          {"group": "Social protection", "label": "Disaster cash"},
  "disaster_cash_amt":          {"group": "Social protection", "label": "Disaster cash amount"},
  "private_gift_any":           {"group": "Social protection", "label": "Private gift received"},
  "private_gift_amt":           {"group": "Social protection", "label": "Private gift amount"},
  "private_support_any":        {"group": "Social protection", "label": "Any private support"},
  "private_support_amt":        {"group": "Social protection", "label": "Private support amount"},
  "ngo_support_any":            {"group": "Social protection", "label": "NGO support"},
  "informal_support_any":       {"group": "Social protection", "label": "Informal support"},
  "external_support_any":       {"group": "Social protection", "label": "External support (any)"},
  "total_support_amt":          {"group": "Social protection", "label": "Total support amount"},
  "support_diversified":        {"group": "Social protection", "label": "Support diversified"},
  "n_support_sources":          {"group": "Social protection", "label": "# support sources"},
}

# ============================================================================
# NEC PANEL — 14 outcomes, 3 groups
# ============================================================================
CURATED["nec_panel"] = {
  "log_new_firms":                            {"group": "Firm entry — total",       "label": "All new firms (log)"},
  "log_new_firms_size_1_worker":              {"group": "Firm entry — by size",     "label": "Size: 1 worker"},
  "log_new_firms_size_2_9_workers":           {"group": "Firm entry — by size",     "label": "Size: 2–9 workers"},
  "log_new_firms_size_10_50_workers":         {"group": "Firm entry — by size",     "label": "Size: 10–50 workers"},
  "log_new_firms_size_51plus_workers":        {"group": "Firm entry — by size",     "label": "Size: 51+ workers"},
  "log_new_firms_agriculture":                {"group": "Firm entry — by industry", "label": "Agriculture"},
  "log_new_firms_manufacturing":              {"group": "Firm entry — by industry", "label": "Manufacturing"},
  "log_new_firms_construction":               {"group": "Firm entry — by industry", "label": "Construction"},
  "log_new_firms_trade_retail":               {"group": "Firm entry — by industry", "label": "Trade & retail"},
  "log_new_firms_hospitality_food":           {"group": "Firm entry — by industry", "label": "Hospitality & food"},
  "log_new_firms_transport_storage":          {"group": "Firm entry — by industry", "label": "Transport & storage"},
  "log_new_firms_finance_prof_realestate":    {"group": "Firm entry — by industry", "label": "Finance / RE / prof"},
  "log_new_firms_education_health_social":    {"group": "Firm entry — by industry", "label": "Education / health / social"},
  "log_new_firms_other_services":             {"group": "Firm entry — by industry", "label": "Other services"},
}

# ============================================================================
# NEC CS — ~35 outcomes, 8 groups
# ============================================================================
CURATED["nec_cs"] = {
  # Firm count & scale
  "log_n_firms":                {"group": "Firm count & scale", "label": "log(# firms)"},
  "log_emp_total":              {"group": "Firm count & scale", "label": "log(total employment)"},
  "log_rev_total":              {"group": "Firm count & scale", "label": "log(revenue)"},
  "log_cap_total":              {"group": "Firm count & scale", "label": "log(capital stock)"},
  "log_exp_total":              {"group": "Firm count & scale", "label": "log(expenses)"},
  "log_value_added_total":      {"group": "Firm count & scale", "label": "log(value added)"},
  "log_profit_proxy_total":     {"group": "Firm count & scale", "label": "log(profit proxy)"},
  "mean_emp_per_firm":          {"group": "Firm count & scale", "label": "Mean emp per firm"},

  # Productivity per worker
  "mean_value_added_per_worker":  {"group": "Per-worker productivity", "label": "Value added / worker (mean)"},
  "median_value_added_per_worker":{"group": "Per-worker productivity", "label": "Value added / worker (median)"},
  "mean_rev_per_worker":          {"group": "Per-worker productivity", "label": "Revenue / worker"},
  "mean_capital_per_worker":      {"group": "Per-worker productivity", "label": "Capital / worker"},
  "mean_profit_per_worker":       {"group": "Per-worker productivity", "label": "Profit / worker"},

  # Profitability & factor shares
  "mean_profit_margin":         {"group": "Profitability & factor shares", "label": "Profit margin"},
  "wage_share_of_revenue":      {"group": "Profitability & factor shares", "label": "Wage share of revenue"},
  "value_added_share_of_revenue":{"group":"Profitability & factor shares","label":"Value-added share"},
  "capital_intensity_aggregate":{"group": "Profitability & factor shares", "label": "Capital intensity (agg)"},

  # Formality
  "formality_index":            {"group": "Formality", "label": "Formality index"},
  "share_registered":           {"group": "Formality", "label": "Share registered"},
  "share_tax_registered":       {"group": "Formality", "label": "Share tax-registered"},
  "share_keeps_accounts":       {"group": "Formality", "label": "Keeps accounts"},
  "share_operates_year_round":  {"group": "Formality", "label": "Year-round operation"},

  # Credit
  "share_borrowed":             {"group": "Credit", "label": "Share borrowed"},
  "share_uses_formal_credit":   {"group": "Credit", "label": "Uses formal credit"},
  "share_has_foreign_capital":  {"group": "Credit", "label": "Foreign capital"},

  # Demographics
  "share_female_led":           {"group": "Firm demographics", "label": "Female-led firms"},

  # Size distribution
  "share_size_1_worker":        {"group": "Firm size distribution", "label": "1 worker (share)"},
  "share_size_2_9_workers":     {"group": "Firm size distribution", "label": "2–9 workers (share)"},
  "share_size_10_50_workers":   {"group": "Firm size distribution", "label": "10–50 workers (share)"},
  "share_size_51plus_workers":  {"group": "Firm size distribution", "label": "51+ workers (share)"},

  # Industry composition
  "industry_diversity":         {"group": "Industry composition", "label": "Industry diversity (1−HHI)"},
  "industry_hhi":               {"group": "Industry composition", "label": "Industry HHI"},
  "n_industries_present":       {"group": "Industry composition", "label": "# industries present"},
  "share_modern_proxy":         {"group": "Industry composition", "label": "Share 'modern' firms"},
  "share_services_total":       {"group": "Industry composition", "label": "Share services (total)"},

  # Industry shares
  "share_agriculture":          {"group": "Industry shares", "label": "Agriculture"},
  "share_manufacturing":        {"group": "Industry shares", "label": "Manufacturing"},
  "share_construction":         {"group": "Industry shares", "label": "Construction"},
  "share_trade_retail":         {"group": "Industry shares", "label": "Trade & retail"},
  "share_hospitality":          {"group": "Industry shares", "label": "Hospitality"},
  "share_transport":            {"group": "Industry shares", "label": "Transport"},
  "share_finance_prof_info":    {"group": "Industry shares", "label": "Finance / prof / info"},
  "share_social_services":      {"group": "Industry shares", "label": "Social services"},
  "share_other_services":       {"group": "Industry shares", "label": "Other services"},
}
