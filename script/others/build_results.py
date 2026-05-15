"""
Build docs/results.json — Family A & B over four datasets:

  census   : muni × year (2001/11/21), FE = lgcode + year, ref = 2001
  hh       : HH × year (2016/17/18), FE = hhid + year, ref = 2016
  nec_panel: muni × founding-year (2001-2018, reshaped from mun_entry_new.csv)
             FE = lgcode + year, ref = 2001
  nec_cs   : muni × {2018}, FE = district only (true cross-section)
             baseline X enter as level controls (no year interaction)

Family A — cumulative builds (treatment held at log interaction):
  A1: treatment only
  A2: + i(year, mig_int_z, ref)
  A3: + i(year, fx_z, ref)
  A4: + i(year, X_baseline, ref)        (saturated)

Family B — saturated-model variants:
  B1: A4 reference
  B2: linear interaction
  B3: drop year × mig_int
  B4: drop year × fx
  B5: drop baseline X (= A3)
  B6: Fx alone (no interaction)
"""
import sys, json, os
import pandas as pd
import numpy as np
from pathlib import Path
from linearmodels import PanelOLS
import statsmodels.api as sm
import warnings
warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

# =============================================================================
# 1.  Load instrument
# =============================================================================
inst_raw = pd.read_csv(ROOT / "data/clean/instrument/instrument_mun.csv")
COL_ALIAS = {
    "fxshock":        ["fxshock",        "avg_fx_shock_2001",       "shareshock_index_2001"],
    "mig_intensity":  ["mig_intensity",  "migrants_per_capita_2001","geog_intensity_2001"],
    "total_migrants": ["total_migrants", "total_migrants_2001",     "geog_total_mig_2001"],
}
inst = pd.DataFrame({"lgcode": inst_raw["lgcode"], "year": inst_raw["year"]})
for canon, candidates in COL_ALIAS.items():
    found = next((c for c in candidates if c in inst_raw.columns), None)
    if found is None:
        raise SystemExit(f"missing one of {candidates}")
    inst[canon] = inst_raw[found]
inst["log_mig_intensity"] = np.log(inst["mig_intensity"] + 1e-8)

def _std(s):
    sd = s.std(ddof=1)
    return (s - s.mean()) / sd if sd > 0 else s * 0.0

inst["fx_z"]         = _std(inst["fxshock"])
inst["mig_int_z"]    = _std(inst["mig_intensity"])
inst["log_migint_z"] = _std(inst["log_mig_intensity"])

INST_KEEP = ["lgcode","year","total_migrants",
             "fxshock","mig_intensity","log_mig_intensity",
             "fx_z","mig_int_z","log_migint_z"]

print(f"Instrument: {len(inst):,} rows · {inst['lgcode'].nunique()} munis · "
      f"years {inst['year'].min()}–{inst['year'].max()}")


# =============================================================================
# 2.  Baseline X (Khanna Block A — destination-weighted)
# =============================================================================
def build_baseline_X():
    region_path = ROOT / "data/clean/instrument/dest_region_shares_2001.csv"
    wdi_path    = ROOT / "data/clean/instrument/wdi_dest_gdp_2001.csv"
    share_path  = ROOT / "data/clean/instrument/dest_mun_mig_share_2001.csv"
    if not all(p.exists() for p in (region_path, wdi_path, share_path)):
        return None, []
    wdi   = pd.read_csv(wdi_path)[["country","gdp_pc_2001"]].dropna()
    share = pd.read_csv(share_path)
    dest_gdp = (share.merge(wdi, on="country", how="inner")
                     .assign(prod=lambda d: d["mun_mig_share_2001"]*d["gdp_pc_2001"])
                     .groupby("lgcode", as_index=False)
                     .agg(dest_gdp_pc_2001=("prod","sum"),
                          coverage=("mun_mig_share_2001","sum")))
    dest_gdp["dest_gdp_pc_2001"] = (dest_gdp["dest_gdp_pc_2001"] /
                                    dest_gdp["coverage"].where(dest_gdp["coverage"]>0,1))
    dest_gdp = dest_gdp[["lgcode","dest_gdp_pc_2001"]]
    region = pd.read_csv(region_path)
    region_cols = [c for c in region.columns if c.startswith("share_")]
    if region_cols:
        ref = region[region_cols].mean().idxmax()
        region_cols = [c for c in region_cols if c != ref]
    bx = region[["lgcode"] + region_cols].merge(dest_gdp, on="lgcode", how="outer")
    bx_cols = region_cols + ["dest_gdp_pc_2001"]
    for c in bx_cols:
        bx[c] = bx[c].fillna(bx[c].mean())
    return bx, bx_cols

BASELINE_X, BASELINE_X_COLS = build_baseline_X()
print(f"Block A baseline-X covariates: {len(BASELINE_X_COLS)} ({', '.join(BASELINE_X_COLS)})")


# =============================================================================
# 3.  SPEC catalogues (Family A & B)
# =============================================================================
SPECS_A = {
    "A1": {"label": "Treatment only", "treatment": "log_int", "c_mig": False, "c_fx": False, "c_X": False},
    "A2": {"label": "+ year × mig_int", "treatment": "log_int", "c_mig": True, "c_fx": False, "c_X": False},
    "A3": {"label": "+ year × fx",       "treatment": "log_int", "c_mig": True, "c_fx": True,  "c_X": False},
    "A4": {"label": "+ year × baseline X (saturated)", "treatment": "log_int", "c_mig": True, "c_fx": True, "c_X": True},
}
SPECS_B = {
    "B1": {"label": "A4 saturated — main", "treatment": "log_int", "c_mig": True, "c_fx": True, "c_X": True},
    "B2": {"label": "Linear interaction", "treatment": "lin_int", "c_mig": True, "c_fx": True, "c_X": True},
    "B3": {"label": "Drop year × mig_int", "treatment": "log_int", "c_mig": False, "c_fx": True, "c_X": True},
    "B4": {"label": "Drop year × fx",      "treatment": "log_int", "c_mig": True, "c_fx": False, "c_X": True},
    "B5": {"label": "Drop baseline X (= A3)", "treatment": "log_int", "c_mig": True, "c_fx": True, "c_X": False},
    "B6": {"label": "Fx alone — no interaction", "treatment": "fx_alone", "c_mig": True, "c_fx": True, "c_X": True},
}
FAMILIES = {
    "A": {"label": "Cumulative builds", "specs": SPECS_A},
    "B": {"label": "Variants (saturated)", "specs": SPECS_B},
}


# =============================================================================
# 4.  Outcome groups — list of (var, human_label) tuples per group
# =============================================================================
CENSUS_GROUPS = {
    "Amenities": [
        ("amen_water_piped",          "Piped water"),
        ("amen_water_traditional",    "Traditional water source"),
        ("amen_cooking_wood",         "Wood/firewood cooking"),
        ("amen_cooking_kerosene",     "Kerosene cooking"),
        ("amen_cooking_lpg",          "LPG cooking"),
        ("amen_cooking_biogas",       "Biogas cooking"),
        ("amen_cooking_electric",     "Electric cooking"),
        ("amen_cooking_modern",       "Modern cooking fuel"),
        ("amen_cooking_traditional",  "Traditional cooking fuel"),
        ("amen_lighting_electricity", "Electric lighting"),
        ("amen_lighting_kerosene",    "Kerosene lighting"),
        ("amen_lighting_biogas",      "Biogas lighting"),
        ("amen_lighting_others",      "Other lighting"),
        ("amen_toilet_modern",        "Modern toilet"),
        ("amen_toilet_ordinary",      "Ordinary toilet"),
        ("amen_toilet_none",          "No toilet"),
        ("amen_toilet_any",           "Any toilet"),
    ],
    "Assets": [
        ("amen_assets_radio",      "Radio"),
        ("amen_assets_tv",         "Television"),
        ("amen_assets_landline",   "Landline phone"),
        ("amen_assets_mobile",     "Mobile phone"),
        ("amen_assets_computer",   "Computer"),
        ("amen_assets_internet",   "Internet"),
        ("amen_assets_cycle",      "Bicycle"),
        ("amen_assets_motorcycle", "Motorcycle"),
        ("amen_assets_car",        "Car / jeep"),
        ("amen_assets_fridge",     "Refrigerator"),
        ("amen_assets_none",       "No assets"),
        ("amen_asset_count_mean",  "Mean asset count"),
    ],
    "Education": [
        ("edu_literate",                       "Literacy"),
        ("edu_literate_female",                "Female literacy"),
        ("edu_literate_male",                  "Male literacy"),
        ("edu_school_attend_6_16",             "School attendance (6-16)"),
        ("edu_school_attend_6_16_female",      "School attendance (6-16) — female"),
        ("edu_school_attend_6_16_male",        "School attendance (6-16) — male"),
        ("edu_attain_primary_plus",            "Primary+ attainment"),
        ("edu_attain_secondary_plus",          "Secondary+ attainment"),
        ("edu_attain_higher_secondary_plus",   "Higher-secondary+ attainment"),
        ("edu_attain_tertiary",                "Tertiary attainment"),
        ("edu_years_mean",                     "Mean years of schooling"),
    ],
    "Housing": [
        ("housing_own",                  "Own dwelling"),
        ("housing_rented",               "Rented dwelling"),
        ("housing_foundation_modern",    "Modern foundation"),
        ("housing_foundation_traditional","Traditional foundation"),
        ("housing_roof_modern",          "Modern roof"),
        ("housing_roof_traditional",     "Traditional roof"),
    ],
    "Female Ownership": [
        ("fem_ownership_house",     "Female owns house"),
        ("fem_ownership_land",      "Female owns land"),
        ("fem_ownership_both",      "Female owns both"),
        ("fem_ownership_livestock", "Female owns livestock"),
    ],
    "Enterprise": [
        ("ent_has_nonagro",          "HH operates non-agri enterprise"),
        ("ent_cottage",              "Cottage industry"),
        ("ent_trade",                "Trade"),
        ("ent_transport",            "Transport"),
        ("ent_services",             "Services"),
        ("ent_other",                "Other enterprise"),
        ("ent_female_owner_share",   "Female-owner share"),
    ],
    "Absent HH": [
        ("absent_hh_share",        "HH share with absentee"),
        ("hh_death_12m",           "Death in HH last 12m"),
    ],
    "Marriage": [
        ("mar_ever_married_15_60",     "Ever married 15-60"),
        ("mar_never_married_15_60",    "Never married 15-60"),
        ("mar_female_age_first_mean",  "Female age at first marriage"),
        ("mar_female_married_by_18",   "Female married by 18"),
        ("mar_female_married_by_20",   "Female married by 20"),
    ],
    "Fertility": [
        ("fert_birth_mean",            "Mean # of births"),
        ("fert_birth_son_mean",        "Mean # of sons born"),
        ("fert_birth_dau_mean",        "Mean # of daughters born"),
        ("fert_births_last12m_share",  "Births last 12m (share of HHs)"),
        ("fert_births_last12m_rate",   "Births last 12m (rate)"),
    ],
    "Child Mortality": [
        ("mort_children_dead_mean",   "Mean # children deceased"),
        ("mort_child_dead_any",       "Any child deceased"),
        ("mort_child_death_ratio",    "Child death ratio"),
    ],
    "Work Activity": [
        ("work_share_agriculture",      "Agri share of work"),
        ("work_share_nonagriculture",   "Non-agri share of work"),
        ("work_share_wage_nonagri",     "Wage non-agri"),
        ("work_share_own_nonagri",      "Own-account non-agri"),
        ("work_share_extended_econ",    "Extended economic activity"),
        ("work_share_job_seeking",      "Job seeking"),
        ("work_share_household_chores", "Chores only"),
        ("work_share_student",          "Student"),
        ("work_share_no_work",          "No work"),
        ("work_lfp",                    "Labour-force participation"),
    ],
    "Occupation": [
        ("occ_share_armed_forces",      "Armed forces"),
        ("occ_share_managers",          "Managers"),
        ("occ_share_professionals",     "Professionals"),
        ("occ_share_technicians",       "Technicians"),
        ("occ_share_office_assistants", "Office assistants"),
        ("occ_share_service_sales",     "Service & sales"),
        ("occ_share_agriculture",       "Agricultural occupations"),
        ("occ_share_craft_trades",      "Craft & trades"),
        ("occ_share_machine_operators", "Machine operators"),
        ("occ_share_elementary",        "Elementary"),
    ],
    "Industry": [
        ("ind_agri_forestry_fish",      "Agriculture, forestry, fishing"),
        ("ind_manufacturing",           "Manufacturing"),
        ("ind_construction",            "Construction"),
        ("ind_wholesale_retail",        "Wholesale & retail"),
        ("ind_transport_accommodation", "Transport & accommodation"),
        ("ind_finance_real_estate_prof","Finance / RE / professional"),
        ("ind_public_admin_defence",    "Public admin & defence"),
        ("ind_education",               "Education"),
        ("ind_health",                  "Health"),
        ("ind_arts_recreation",         "Arts & recreation"),
        ("ind_others",                  "Other industries"),
    ],
    "Employment Status": [
        ("emp_share_employer",             "Employer"),
        ("emp_share_employee",             "Employee"),
        ("emp_share_self_employed",        "Self-employed"),
        ("emp_share_unpaid_family_worker", "Unpaid family worker"),
    ],
    "In-migration": [
        ("mig_in_share",                "In-migrant share"),
        ("mig_in_domestic",             "Domestic in-migrants"),
        ("mig_in_international",        "International in-migrants"),
        ("mig_in_from_rural",           "From rural"),
        ("mig_in_from_urban",           "From urban"),
        ("mig_in_reason_economic",      "Economic reason"),
        ("mig_in_reason_noneconomic",   "Non-economic reason"),
        ("mig_in_reason_study",         "Study reason"),
        ("mig_in_reason_marriage",      "Marriage reason"),
        ("mig_in_return",               "Return migrants"),
    ],
    "Female Labor": [
        ("flfp_all",            "Female LFP"),
        ("fem_employment_rate", "Female employment rate"),
        ("flfp_agri",           "Female LFP — agri"),
        ("flfp_nonagri",        "Female LFP — non-agri"),
        ("flfp_wage",           "Female LFP — wage"),
        ("flfp_chores_only",    "Female chores only"),
    ],
    "Male Labor": [
        ("mlfp_all",     "Male LFP"),
        ("mlfp_agri",    "Male LFP — agri"),
        ("mlfp_nonagri", "Male LFP — non-agri"),
    ],
    "Gender Gaps": [
        ("share_women",                    "Share women"),
        ("share_men",                      "Share men"),
        ("fem_share_of_ag_workers",        "Female share of ag workers"),
        ("fem_ag_specialization_ratio",    "Female ag specialization ratio"),
        ("fem_wage_share_of_employment",   "Female wage share of employment"),
        ("gap_lfp_m_minus_f",              "LFP gap (M − F)"),
        ("gap_nonagri_m_minus_f",          "Non-agri gap (M − F)"),
    ],
    "HH Head": [
        ("head_female_share",      "Female head"),
        ("head_age_mean",          "Mean head age"),
        ("head_elderly_share",     "Elderly head"),
        ("head_young_share",       "Young head"),
        ("head_female_elderly",    "Female elderly head"),
    ],
    "Left-behind children": [
        ("left_not_with_both",     "Not with both parents"),
        ("left_mother_only",       "With mother only"),
        ("left_father_only",       "With father only"),
        ("left_with_relatives",    "With relatives"),
        ("left_without_parents",   "Without either parent"),
    ],
}

HH_GROUPS = {
    "Land — agriculture": [
        ("agro_hh",                    "HH operates farm"),
        ("n_plots_owned",              "# plots owned"),
        ("total_owned_area_sqm",       "Owned area (sqm)"),
        ("cultivated_area_sqm",        "Cultivated area (sqm)"),
        ("cultivated_area_total_sqm",  "Total cultivated area"),
        ("rented_in_area_sqm",         "Area rented in"),
    ],
    "Land use — wet/dry": [
        ("share_self_wet",       "Self-cultivated (wet)"),
        ("share_rented_out_wet", "Rented out (wet)"),
        ("share_fallow_wet",     "Fallow (wet)"),
        ("share_self_dry",       "Self-cultivated (dry)"),
        ("share_fallow_dry",     "Fallow (dry)"),
        ("share_both_seasons",   "Cultivated both seasons"),
    ],
    "Cropping": [
        ("n_crops_total",       "# crops grown"),
        ("multi_season",        "Multi-season HH"),
        ("grows_staple",        "Grows staples"),
        ("grows_cashcrop",      "Grows cash crops"),
        ("grows_horticulture",  "Grows horticulture"),
        ("crop_simpson_diversity","Crop diversity (Simpson)"),
        ("staple_value_share",  "Staple share of value"),
    ],
    "Consumption — food": [
        ("food_exp_total_7day",   "Food expenditure 7d (Rs)"),
        ("food_exp_protein_7day", "Protein expenditure 7d"),
        ("food_exp_staples_7day", "Staples expenditure 7d"),
        ("food_insec_any",        "Any food insecurity"),
        ("food_insec_score",      "Food insecurity score"),
        ("food_insec_worried",    "Food-worried"),
    ],
    "Consumption — non-food": [
        ("nonfood_exp_30day",                "Non-food 30d (Rs)"),
        ("nonfood_exp_12m",                  "Non-food 12m (Rs)"),
        ("nonfood_communication_12m",        "Communication 12m"),
        ("nonfood_transport_12m",            "Transport 12m"),
        ("nonfood_entertainment_leisure_12m","Entertainment 12m"),
        ("nonfood_ceremonies_12m",           "Ceremonies 12m"),
        ("nonfood_fuel_lighting_12m",        "Fuel & lighting 12m"),
        ("nonfood_clothing_footwear_12m",    "Clothing 12m"),
    ],
    "Education spending": [
        ("any_enrolled",              "Any enrolled HH"),
        ("n_enrolled",                "# enrolled in HH"),
        ("n_private_school",          "# in private school"),
        ("n_scholarship",             "# scholarships"),
        ("edu_spend_total_12m",       "Total edu spend 12m"),
        ("edu_spend_per_enrolled",    "Edu spend per enrolled"),
        ("edu_spend_tuition_12m",     "Tuition spend"),
        ("edu_spend_books_12m",       "Books spend"),
    ],
    "Health": [
        ("any_insured",          "Any HH member insured"),
        ("n_insured",            "# insured"),
        ("n_chronic",            "# chronic conditions"),
        ("n_acute_illness",      "# acute illnesses"),
        ("any_health_spending",  "Any health spending"),
        ("hlt_spend_total",      "Total health spend"),
        ("hlt_spend_medicines",  "Medicines spend"),
        ("hlt_spend_hospital",   "Hospital spend"),
    ],
    "Enterprise": [
        ("has_enterprise",        "Operates enterprise"),
        ("n_enterprises",         "# enterprises"),
        ("n_workers_total",       "# workers"),
        ("revenue_12m",           "Revenue 12m"),
        ("expenses_12m",          "Expenses 12m"),
        ("profit_12m",            "Profit 12m"),
        ("capex_12m",             "Capex 12m"),
        ("sector_manufacturing",  "Sector — manuf"),
        ("sector_services",       "Sector — services"),
        ("sector_trade",          "Sector — trade"),
        ("sector_hotels",         "Sector — hotels"),
        ("sector_transport",      "Sector — transport"),
    ],
    "Migration — HH": [
        ("has_migrant",                     "Has any migrant"),
        ("has_migrant_internal",            "Has internal migrant"),
        ("has_migrant_international",       "Has international migrant"),
        ("has_only_internal",               "Only internal"),
        ("has_only_international",          "Only international"),
        ("has_both_internal_and_international", "Both internal & intl"),
        ("n_migrants_total",                "# migrants"),
        ("n_migrants_male",                 "# male migrants"),
        ("n_migrants_female",               "# female migrants"),
        ("share_male_migrants",             "Male share of migrants"),
        ("share_long_term_migrants",        "Long-term share"),
        ("mig_reason_work",                 "Reason: work"),
        ("mig_reason_education",            "Reason: education"),
        ("mig_reason_marriage_family",      "Reason: marriage/family"),
    ],
    "Shocks & coping": [
        ("any_shock",                 "Any shock"),
        ("n_shocks",                  "# shocks"),
        ("total_loss_rs",             "Total loss (Rs)"),
        ("health_shock_any",          "Health shock"),
        ("death_shock_any",           "Death shock"),
        ("natural_disaster_shock_any","Natural disaster"),
        ("agricultural_shock_any",    "Agricultural shock"),
        ("economic_shock_any",        "Economic shock"),
        ("any_coping_reported",       "Any coping reported"),
        ("cope_savings_any",          "Cope: savings"),
        ("cope_borrow_any",           "Cope: borrow"),
        ("cope_sell_assets_any",      "Cope: sell assets"),
        ("cope_migration_remittance_any","Cope: migration / remit"),
        ("cope_public_private_aid_any", "Cope: aid"),
    ],
    "Social protection": [
        ("public_support_any",          "Any public support"),
        ("public_support_amt",          "Public support amount"),
        ("public_cash_any",             "Public cash"),
        ("public_cash_amt",             "Public cash amount"),
        ("demographic_cash_any",        "Demographic cash"),
        ("disaster_cash_any",           "Disaster cash"),
        ("public_inkind_any",           "Public in-kind"),
        ("public_work_any",             "Public works"),
        ("private_support_any",         "Private support"),
        ("ngo_support_any",             "NGO support"),
        ("remittance_any",              "Any remittance"),
        ("remittance_amt",              "Remittance amount"),
    ],
}

# NEC panel: muni × year, mun_entry_panel_new.csv
# Yearly entry counts/shares by sector and size, plus total + log.
NEC_PANEL_GROUPS = {
    "Firm entry — total (level + log)": [
        ("new_firms",                       "# new firms (per year)"),
        ("log_new_firms",                   "log(1 + new firms)"),
    ],
    "Entry by sector — counts": [
        ("new_firms_agriculture",            "Agriculture"),
        ("new_firms_manufacturing",          "Manufacturing"),
        ("new_firms_hospitality_food",       "Hospitality & food"),
        ("new_firms_education_health_social","Education, health, social"),
        ("new_firms_trade_retail",           "Trade & retail"),
        ("new_firms_other_services",         "Other services"),
        ("new_firms_utilities_mining",       "Utilities & mining"),
        ("new_firms_finance_prof_realestate","Finance, professional, RE"),
        ("new_firms_construction",           "Construction"),
        ("new_firms_transport_storage",      "Transport & storage"),
        ("new_firms_admin_support",          "Admin & support"),
        ("new_firms_information_comm",       "Information & comms"),
    ],
    # NOTE: 'Entry by sector — shares' dropped — share_new_<sector>
    # columns in mun_entry_panel_new.csv don't sum to 1 (mean 0.74,
    # max 4.29). Sectoral counts above remain.
    "Entry by size — counts": [
        ("new_firms_size_1_worker",         "Micro (1 worker)"),
        ("new_firms_size_2_9_workers",      "Small (2–9)"),
        ("new_firms_size_10_50_workers",    "Medium (10–50)"),
        ("new_firms_size_51plus_workers",   "Large (51+)"),
    ],
    # NOTE: 'Entry by size — shares' dropped — share_new_size_*
    # columns also don't sum to 1 (mean 1.01, max 3.00). Counts above
    # remain.
}

# NEC cross-section: split into smaller groups; pair level+log where both exist.
NEC_CS_GROUPS = {
    "Industry — counts": [
        ("n_firms",                   "# firms (total)"),
        ("n_firms_agriculture",       "Agriculture"),
        ("n_firms_manufacturing",     "Manufacturing"),
        ("n_firms_construction",      "Construction"),
        ("n_firms_trade_retail",      "Trade & retail"),
        ("n_firms_transport",         "Transport"),
        ("n_firms_hospitality",       "Hospitality"),
        ("n_firms_finance_prof_info", "Finance/prof/info"),
        ("n_firms_social_services",   "Social services"),
        ("n_firms_other_services",    "Other services"),
        ("n_firms_utilities_mining",  "Utilities & mining"),
    ],
    "Industry — shares": [
        ("share_agriculture",        "Agriculture"),
        ("share_manufacturing",      "Manufacturing"),
        ("share_construction",       "Construction"),
        ("share_trade_retail",       "Trade & retail"),
        ("share_transport",          "Transport"),
        ("share_hospitality",        "Hospitality"),
        ("share_finance_prof_info",  "Finance/prof/info"),
        ("share_social_services",    "Social services"),
        ("share_other_services",     "Other services"),
        ("share_utilities_mining",   "Utilities & mining"),
    ],
    "Industry — concentration": [
        ("industry_hhi",         "Industry HHI"),
        ("industry_diversity",   "Industry diversity"),
        ("n_industries_present", "# industries present"),
    ],
    "Productivity — totals (level + log)": [
        ("rev_total",                   "Total revenue"),
        ("log_rev_total",               "log(revenue)"),
        ("exp_total",                   "Total expenditure"),
        ("log_exp_total",               "log(expenditure)"),
        ("wage_bill_total",             "Total wage bill"),
        ("log_wage_bill_total",         "log(wage bill)"),
        ("value_added_total",           "Total value added"),
        ("log_value_added_total",       "log(value added)"),
        ("value_added_clean_total",     "Total value added (cleaned)"),
        ("log_value_added_clean_total", "log(value added cleaned)"),
        ("cap_total",                   "Total capital"),
        ("log_cap_total",               "log(capital)"),
        ("cap_fixed_total",             "Fixed capital"),
        ("log_cap_fixed_total",         "log(fixed capital)"),
        ("emp_total",                   "Total employment"),
        ("log_emp_total",               "log(employment)"),
    ],
    "Productivity — per worker": [
        ("mean_rev_per_worker",                 "Mean rev / worker"),
        ("median_rev_per_worker",               "Median rev / worker"),
        ("p90_rev_per_worker",                  "p90 rev / worker"),
        ("mean_value_added_per_worker",         "Mean VA / worker"),
        ("median_value_added_per_worker",       "Median VA / worker"),
        ("mean_value_added_clean_per_worker",   "Mean VA (clean) / worker"),
        ("median_value_added_clean_per_worker", "Median VA (clean) / worker"),
        ("mean_capital_per_worker",             "Mean capital / worker"),
        ("median_capital_per_worker",           "Median capital / worker"),
    ],
    "Productivity — margins & ratios": [
        ("mean_revenue_per_capital",      "Mean rev / capital"),
        ("median_revenue_per_capital",    "Median rev / capital"),
        ("mean_value_added_margin",       "Mean VA margin"),
        ("median_value_added_margin",     "Median VA margin"),
        ("wage_share_of_revenue",         "Wage share of revenue"),
        ("value_added_share_of_revenue",  "VA share of revenue"),
        ("capital_intensity_aggregate",   "Capital intensity (aggregate)"),
        ("exp_rev_ratio_aggregate",       "Expenditure / revenue (aggregate)"),
    ],
    "Data quality flags": [
        ("share_positive_revenue",        "Share with positive revenue"),
        ("share_positive_capital",        "Share with positive capital"),
        ("n_extreme_finance_records",     "# extreme-finance records"),
        ("share_extreme_finance_records", "Share extreme-finance records"),
        ("value_added_negative_flag",     "Has negative VA"),
        ("extreme_exp_rev_ratio_flag",    "Extreme exp/rev ratio"),
    ],
    "Size — counts (level + log)": [
        ("n_firms",                  "# firms"),
        ("log_n_firms",              "log(# firms)"),
        ("emp_total",                "Total employment"),
        ("log_emp_total",            "log(employment)"),
        ("mean_emp_per_firm",        "Mean emp / firm"),
        ("median_emp_per_firm",      "Median emp / firm"),
        ("p90_emp_per_firm",         "p90 emp / firm"),
    ],
    "Size — distribution (counts)": [
        ("n_firms_size_1_worker",       "Micro (1)"),
        ("n_firms_size_2_9_workers",    "Small (2–9)"),
        ("n_firms_size_10_50_workers",  "Medium (10–50)"),
        ("n_firms_size_51plus_workers", "Large (51+)"),
    ],
    # NOTE: 'Size — distribution (shares)' dropped — share_size_*
    # columns in mun_size_formality.csv don't sum to 1 (mean 1.36, max
    # 3.08). The size-distribution counts above remain.
    "Formality": [
        ("formality_index",           "Formality index"),
        ("share_registered",          "Share registered"),
        ("share_tax_registered",      "Share tax-registered"),
        ("share_keeps_accounts",      "Share keeps accounts"),
        ("share_operates_year_round", "Share year-round"),
    ],
    "Credit & finance": [
        ("share_borrowed",            "Share borrowed"),
        ("share_uses_formal_credit",  "Share formal credit"),
        ("share_has_foreign_capital", "Share foreign capital"),
    ],
    "Female-led firms": [
        ("share_female_led",          "Share female-led"),
    ],
}


# =============================================================================
# 5.  Auto-log expansion: for count / value outcomes, also surface log(1+x)
# =============================================================================
import re
LOG_PATTERNS = [
    re.compile(r"^n_"),                # counts (n_*, n_firms_*, n_migrants_*, n_shocks, …)
    re.compile(r"_total$"),            # *_total
    re.compile(r"_amt$"),              # *_amt
    re.compile(r"_rs$"),               # *_rs
    re.compile(r"_12m$|_7day$|_30day$"),# expenditures / outcomes over a period
    re.compile(r"^revenue_|^expenses_|^profit_|^capex_"),  # enterprise rupee vars
    re.compile(r"^food_exp_|^nonfood_"),  # consumption rupee vars
    re.compile(r"^edu_spend_|^hlt_spend_"),# spending rupee vars
    re.compile(r"^new_firms_(?!.*_share)"),# nec_panel sectoral / size entry counts
    re.compile(r"^total_loss_"),       # shock loss
]
LOG_EXCLUDE = {                       # already-log or count-of-distinct
    "n_industries_present", "n_entry_years_observed",
    "n_extreme_finance_records",
}

def should_log(var):
    if var in LOG_EXCLUDE: return False
    if var.startswith("log_"): return False
    if "_share" in var or "share_" in var: return False
    if "_rate" in var or "_pct" in var or "_pp" in var: return False
    return any(p.search(var) for p in LOG_PATTERNS)

def expand_with_logs(groups):
    """Insert ('log_'+var, 'log('+label+')') after each loggable (var, label),
    unless log_var already appears explicitly anywhere in this group."""
    out = {}
    for g, items in groups.items():
        existing_vars = {v for v, _ in items}
        new_items = []
        for v, lab in items:
            new_items.append((v, lab))
            if should_log(v) and f"log_{v}" not in existing_vars:
                new_items.append((f"log_{v}", f"log({lab})"))
        out[g] = new_items
    return out

def _add_log_columns(df):
    """In-place: add log_<var> = log(1+<var>) for any column that should_log
    AND has no log_ companion AND has non-negative values."""
    new_cols = {}
    for c in list(df.columns):
        if not should_log(c): continue
        log_name = f"log_{c}"
        if log_name in df.columns: continue
        try:
            s = pd.to_numeric(df[c], errors="coerce")
            if s.dropna().min() < 0: continue   # log only on non-negative
            new_cols[log_name] = np.log1p(s)
        except Exception:
            continue
    if new_cols:
        df = pd.concat([df, pd.DataFrame(new_cols, index=df.index)], axis=1)
    return df


# =============================================================================
# 6.  Estimation helpers
# =============================================================================
def _build_year_interactions(d, treat_col, ref_year, prefix=None):
    if prefix is None: prefix = treat_col
    years_present = sorted(d["year"].unique())
    actual_ref = ref_year if ref_year in years_present else years_present[0]
    out_cols = []
    for y in years_present:
        if y == actual_ref: continue
        col = f"{prefix}_x_{int(y)}"
        d[col] = d[treat_col] * (d["year"] == y).astype(float)
        out_cols.append(col)
    return out_cols


def fit_panel(df, y, spec_blocks, entity_col, year_col, ref_year, cluster_col):
    """Panel fit with entity + year FE."""
    needed = [entity_col, year_col, y, cluster_col,
              "fx_z", "mig_int_z", "log_migint_z"] + (BASELINE_X_COLS or [])
    seen = set(); ucols = []
    for c in needed:
        if c not in seen and c in df.columns:
            seen.add(c); ucols.append(c)
    d = df[ucols].dropna(subset=[y, "fx_z"]).copy()
    if d[y].nunique() < 2 or len(d) < 50: return None
    if d[y].std(ddof=1) == 0: return None

    treatment = spec_blocks.get("treatment", "log_int")
    if treatment == "log_int":
        d["treatment"] = d["fx_z"] * d["log_migint_z"]
    elif treatment == "lin_int":
        d["treatment"] = d["fx_z"] * d["mig_int_z"]
    elif treatment == "fx_alone":
        d["treatment"] = d["fx_z"]
    else: return {"err": f"unknown treatment {treatment}"}
    rhs = ["treatment"]

    if spec_blocks.get("c_mig"):
        rhs += _build_year_interactions(d, "mig_int_z", ref_year, prefix="cmig")
    if spec_blocks.get("c_fx"):
        rhs += _build_year_interactions(d, "fx_z", ref_year, prefix="cfx")
    if spec_blocks.get("c_X") and BASELINE_X_COLS:
        for x in BASELINE_X_COLS:
            if x in d.columns:
                rhs += _build_year_interactions(d, x, ref_year, prefix=f"cX_{x}")

    d_idx = d.set_index([entity_col, year_col])
    cluster_series = pd.Series(d[cluster_col].values, index=d_idx.index, name="_cl")
    try:
        m = PanelOLS(d_idx[y], d_idx[rhs],
                     entity_effects=True, time_effects=True,
                     drop_absorbed=True
                    ).fit(cov_type="clustered", clusters=cluster_series)
    except Exception as e:
        return {"err": str(e)[:80]}
    if "treatment" not in m.params.index: return {"err": "treatment absorbed"}
    return {
        "beta": float(m.params["treatment"]),
        "se":   float(m.std_errors["treatment"]),
        "pval": float(m.pvalues["treatment"]),
        "n":    int(m.nobs),
        "n_unit": int(d_idx.index.get_level_values(entity_col).nunique()),
        "n_muni": int(d[cluster_col].nunique()),
        "mean_y": float(d[y].mean()),
        "sd_y":   float(d[y].std(ddof=1)),
    }


def fit_cs(df, y, spec_blocks, cluster_col="DIST"):
    """Cross-section fit with district FE only (NEC cross-section).
    Uses statsmodels OLS with cluster-robust SE.
    Treatment is fx_z × log(mig_int_z) at year=2018 (or whatever the data has).
    Controls (when on): mig_int_z, fx_z, X — entered as LEVELS (no year interaction).
    """
    cols = [y, "fx_z", "mig_int_z", "log_migint_z", cluster_col] + (BASELINE_X_COLS or [])
    d = df[[c for c in cols if c in df.columns]].dropna(subset=[y, "fx_z"]).copy()
    if d[y].nunique() < 2 or len(d) < 50: return None
    if d[y].std(ddof=1) == 0: return None

    treatment = spec_blocks.get("treatment", "log_int")
    if treatment == "log_int":
        d["treatment"] = d["fx_z"] * d["log_migint_z"]
    elif treatment == "lin_int":
        d["treatment"] = d["fx_z"] * d["mig_int_z"]
    elif treatment == "fx_alone":
        d["treatment"] = d["fx_z"]
    else: return {"err": f"unknown treatment {treatment}"}

    rhs = ["treatment"]
    if spec_blocks.get("c_mig"): rhs += ["mig_int_z"]
    if spec_blocks.get("c_fx"):  rhs += ["fx_z"] if "fx_z" not in rhs else []
    if spec_blocks.get("c_X") and BASELINE_X_COLS:
        rhs += [x for x in BASELINE_X_COLS if x in d.columns]
    rhs = list(dict.fromkeys(rhs))   # dedup, preserve order

    # District dummies as fixed effects
    dist_dummies = pd.get_dummies(d[cluster_col], prefix="DIST", drop_first=True, dtype=float)
    X = pd.concat([d[rhs], dist_dummies], axis=1)
    X = sm.add_constant(X)
    try:
        m = sm.OLS(d[y].astype(float), X.astype(float)).fit(
            cov_type="cluster", cov_kwds={"groups": d[cluster_col].values})
    except Exception as e:
        return {"err": str(e)[:80]}
    if "treatment" not in m.params.index: return {"err": "treatment absorbed"}
    return {
        "beta": float(m.params["treatment"]),
        "se":   float(m.bse["treatment"]),
        "pval": float(m.pvalues["treatment"]),
        "n":    int(m.nobs),
        "n_unit": int(len(d)),       # one obs per muni
        "n_muni": int(len(d)),
        "mean_y": float(d[y].mean()),
        "sd_y":   float(d[y].std(ddof=1)),
    }


# =============================================================================
# 6.  Compute datasets
# =============================================================================
def compute_panel(name, source_df, groups, entity_col, year_col, ref_year,
                  cluster_col="lgcode"):
    print(f"\n[{name}] {entity_col} FE, ref={ref_year}, cluster {cluster_col}")
    panel = source_df.merge(inst[INST_KEEP], on=["lgcode","year"], how="inner")
    if BASELINE_X is not None:
        panel = panel.merge(BASELINE_X, on="lgcode", how="left")
        for c in BASELINE_X_COLS:
            if c in panel.columns:
                panel[c] = panel[c].fillna(panel[c].mean())

    print(f"  panel: {len(panel):,} rows · {panel[entity_col].nunique():,} {entity_col}s · "
          f"{panel['lgcode'].nunique():,} munis · years {sorted(panel[year_col].unique().tolist())}")

    THRESHOLDS = [0, 25, 50, 100]
    estimates = {}
    for thr in THRESHOLDS:
        sub = panel if thr == 0 else panel[panel["total_migrants"] >= thr]
        if len(sub) < 50: continue
        muni_yr = sub[["lgcode","year","fxshock","mig_intensity","log_mig_intensity"]].drop_duplicates()
        muni_yr["fx_z"]         = _std(muni_yr["fxshock"])
        muni_yr["mig_int_z"]    = _std(muni_yr["mig_intensity"])
        muni_yr["log_migint_z"] = _std(muni_yr["log_mig_intensity"])
        sub = sub.drop(columns=["fx_z","mig_int_z","log_migint_z"], errors="ignore")
        sub = sub.merge(muni_yr[["lgcode","year","fx_z","mig_int_z","log_migint_z"]],
                        on=["lgcode","year"], how="left")
        estimates[str(thr)] = {}
        for fk, fam in FAMILIES.items():
            estimates[str(thr)][fk] = {}
            for sk, spec in fam["specs"].items():
                estimates[str(thr)][fk][sk] = {}
                for gname, items in groups.items():
                    for var, _label in items:
                        if var not in sub.columns: continue
                        r = fit_panel(sub, var, spec,
                                      entity_col=entity_col, year_col=year_col,
                                      ref_year=ref_year, cluster_col=cluster_col)
                        if r is None: continue
                        estimates[str(thr)][fk][sk][var] = r
        print(f"  thr={thr}: {sum(len(estimates[str(thr)][fk][sk]) for fk in FAMILIES for sk in FAMILIES[fk]['specs']):,} cells")

    outcomes_d = {var: {"label": label, "group": g}
                  for g, items in groups.items() for var, label in items}
    return {
        "label": name, "entity": entity_col, "year": year_col,
        "ref_year": ref_year, "cluster": cluster_col,
        "groups": list(groups.keys()),
        "outcomes": outcomes_d, "estimates": estimates,
    }


def compute_cross_section(name, source_df, groups, year_for_shock=2018, cluster_col="DIST"):
    """NEC cross-section: muni-level data observed once, district FE."""
    print(f"\n[{name}] district FE, shock at year={year_for_shock}, cluster {cluster_col}")
    inst_yr = inst.query("year == @year_for_shock")[["lgcode","fxshock","mig_intensity","log_mig_intensity",
                                                     "fx_z","mig_int_z","log_migint_z","total_migrants"]]
    panel = source_df.merge(inst_yr, on="lgcode", how="inner")
    if BASELINE_X is not None:
        panel = panel.merge(BASELINE_X, on="lgcode", how="left")
        for c in BASELINE_X_COLS:
            if c in panel.columns: panel[c] = panel[c].fillna(panel[c].mean())
    print(f"  panel: {len(panel):,} munis, {panel[cluster_col].nunique() if cluster_col in panel.columns else '?'} districts")
    if cluster_col not in panel.columns:
        print(f"  WARNING: {cluster_col} not found, falling back to single cluster")
        panel[cluster_col] = 1

    THRESHOLDS = [0, 25, 50, 100]
    estimates = {}
    for thr in THRESHOLDS:
        sub = panel if thr == 0 else panel[panel["total_migrants"] >= thr]
        if len(sub) < 50: continue
        # Re-z-score on working sample
        for raw, z in [("fxshock","fx_z"), ("mig_intensity","mig_int_z"), ("log_mig_intensity","log_migint_z")]:
            sub[z] = _std(sub[raw])
        estimates[str(thr)] = {}
        for fk, fam in FAMILIES.items():
            estimates[str(thr)][fk] = {}
            for sk, spec in fam["specs"].items():
                estimates[str(thr)][fk][sk] = {}
                for gname, items in groups.items():
                    for var, _label in items:
                        if var not in sub.columns: continue
                        r = fit_cs(sub, var, spec, cluster_col=cluster_col)
                        if r is None: continue
                        estimates[str(thr)][fk][sk][var] = r
        print(f"  thr={thr}: {sum(len(estimates[str(thr)][fk][sk]) for fk in FAMILIES for sk in FAMILIES[fk]['specs']):,} cells")

    outcomes_d = {var: {"label": label, "group": g}
                  for g, items in groups.items() for var, label in items}
    return {
        "label": name, "entity": "lgcode", "year": str(year_for_shock),
        "ref_year": year_for_shock, "cluster": cluster_col,
        "groups": list(groups.keys()),
        "outcomes": outcomes_d, "estimates": estimates,
    }


# =============================================================================
# 7.  Build per-dataset frames
# =============================================================================
def load_hh_master():
    """Merge all RVS HH-year files on (hhid, year). Brings vmun_code from
    agriculture (which always has it) and joins the rest."""
    base_path = ROOT / "data/clean/rvs_outcomes"
    agri = pd.read_csv(base_path / "agriculture_hh_year.csv").rename(columns={"vmun_code":"lgcode"})
    keep = ["hhid","year","lgcode"] + [c for c in agri.columns
                                       if c not in ("hhid","year","wt_hh","psu","vdc","lgname",
                                                    "district77","district_name","s00q03a","s00q03b","s00q03c","lgcode")]
    master = agri[keep].copy()

    extra_files = ["consumption_hh_year","education_hh_year","enterprise_hh_year",
                   "health_hh_year","social_protection_hh_year",
                   "shocks_coping_shocked_hh_year","migration_hh_year_migrant_only"]
    for f in extra_files:
        p = base_path / f"{f}.csv"
        if not p.exists(): print(f"  skip {f}"); continue
        df = pd.read_csv(p)
        # don't double-merge identifying columns
        skip = {"hhid","year","wt_hh","psu","vdc","vmun_code","lgname",
                "district77","district_name","s00q03a","s00q03b","s00q03c","district","member_id"}
        keep_cols = ["hhid","year"] + [c for c in df.columns if c not in skip]
        master = master.merge(df[keep_cols].drop_duplicates(["hhid","year"]),
                              on=["hhid","year"], how="left")
        print(f"  + {f}: now {master.shape[1]} cols, {len(master):,} rows")
    return master


def load_nec_panel():
    """Use the prebuilt panel (mun_entry_panel_new.csv) directly.
    14,326 rows = 754 munis × 19 years (2000-2018), 36 cols including
    sectoral & size-bin entry counts and shares.
    Keep years 2001-2018 to match instrument coverage."""
    src = ROOT / "data/clean/nec2018/mun_entry_panel_new.csv"
    if not src.exists():
        print(f"  WARN: {src} not found"); return None
    df = pd.read_csv(src)
    df = df.query("year >= 2001 and year <= 2018").reset_index(drop=True)
    return df


def load_nec_cs():
    """Merge the 3 compact NEC files + ensure DIST column.
    DIST is derived from lgcode prefix (lgcode // 100) — first 3 digits encode
    Nepal's 77 districts. This is the same partition municipality_analysis.csv
    used to provide; deriving from lgcode removes the dependency on that file."""
    base = ROOT / "data/clean/nec2018"
    parts = []
    for f in ["mun_industry_structure", "mun_productivity_profitability", "mun_size_formality"]:
        p = base / f"{f}.csv"
        if p.exists(): parts.append(pd.read_csv(p))
    if not parts: return None
    merged = parts[0]
    for p in parts[1:]:
        new_cols = [c for c in p.columns if c == "lgcode" or c not in merged.columns]
        merged = merged.merge(p[new_cols], on="lgcode", how="outer")
    # Prefer external DIST if available; else derive from lgcode prefix
    mun_p = base / "municipality_analysis.csv"
    if mun_p.exists():
        try:
            dist = pd.read_csv(mun_p, usecols=["lgcode","DIST"])
            merged = merged.merge(dist, on="lgcode", how="left")
        except Exception:
            merged["DIST"] = merged["lgcode"] // 100
    else:
        # Derive: lgcode //100 → district code (77 distinct values for Nepal)
        merged["DIST"] = merged["lgcode"] // 100
    n_dist = merged["DIST"].nunique() if "DIST" in merged.columns else 0
    print(f"  load_nec_cs: {len(merged):,} munis · DIST = {n_dist} districts")
    return merged


# =============================================================================
# 8.  Run datasets
# =============================================================================
def main():
    final = {
        "datasets_meta": {
            "census":    {"label": "Census panel — Population & Housing"},
            "hh":        {"label": "HRVS HH × year (all domains)"},
            "nec_panel": {"label": "NEC firm entry — muni × founding-year"},
            "nec_cs":    {"label": "NEC 2018 — firm cross-section"},
        },
        "families": {
            fk: {"label": fv["label"],
                 "specs": {sk: sv["label"] for sk, sv in fv["specs"].items()}}
            for fk, fv in FAMILIES.items()
        },
        "thresholds": {"0":"All munis","25":"≥25 migrants in 2001",
                       "50":"≥50 migrants in 2001","100":"≥100 migrants in 2001"},
        "datasets": {},
    }

    # --- Census ---
    cen = pd.read_csv(ROOT / "data/clean/census/census_outcomes_municipality.csv")
    cen = _add_log_columns(cen)
    final["datasets"]["census"] = compute_panel(
        "census", cen, expand_with_logs(CENSUS_GROUPS),
        entity_col="lgcode", year_col="year", ref_year=2001)

    # --- HH (all RVS domains) ---
    hh = load_hh_master()
    hh = _add_log_columns(hh)
    final["datasets"]["hh"] = compute_panel(
        "hh", hh, expand_with_logs(HH_GROUPS),
        entity_col="hhid", year_col="year", ref_year=2016)

    # --- NEC panel (firm entry, wide→long) ---
    nec_panel = load_nec_panel()
    if nec_panel is not None:
        nec_panel = _add_log_columns(nec_panel)
        final["datasets"]["nec_panel"] = compute_panel(
            "nec_panel", nec_panel, expand_with_logs(NEC_PANEL_GROUPS),
            entity_col="lgcode", year_col="year", ref_year=2001)

    # --- NEC cross-section ---
    nec_cs = load_nec_cs()
    if nec_cs is not None:
        nec_cs = _add_log_columns(nec_cs)
        final["datasets"]["nec_cs"] = compute_cross_section(
            "nec_cs", nec_cs, expand_with_logs(NEC_CS_GROUPS),
            year_for_shock=2018, cluster_col="DIST")

    n_total = 0
    for ds in final["datasets"].values():
        for thr in ds["estimates"].values():
            for fam in thr.values():
                for spec in fam.values():
                    n_total += sum(1 for c in spec.values() if "err" not in c)
    print(f"\nTotal estimate cells: {n_total:,}")

    out = ROOT / "docs/results.json"
    out.write_text(json.dumps(final, separators=(",",":")))
    print(f"Wrote {out}")

if __name__ == "__main__":
    main()
