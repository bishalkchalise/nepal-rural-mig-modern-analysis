"""
Build all reduced-form regression results into a JSON for the interactive HTML.

Combinations:
  - 3 panel/ref settings (P2 ref2011, P3 ref2001, P3 ref2011)
  - 2 shock-timing variants (contemporaneous t, lagged t-1)
  - 5 shock variants
  - 4 control sets
  - all outcomes in GROUPS

Note: "lagged FX" = lagging the destination FX shifter by one year. Because
SSIV = Σ_d (N_md0/Pop_m0) · Z_dt with time-invariant 2001 weights, lagging the
shifter is equivalent to lagging SSIV. We merge instrument values at year-1 to
implement this.
"""
import warnings, json, os
warnings.filterwarnings("ignore")
import numpy as np, pandas as pd
from linearmodels.panel import PanelOLS
from pathlib import Path

# Project root: walk upward from this script to find a "data/clean" sibling
def _find_root():
    here = Path(__file__).resolve().parent if "__file__" in globals() else Path.cwd()
    for cand in [here, *here.parents]:
        if (cand / "data" / "clean").is_dir():
            return cand
    return Path.cwd()

ROOT = _find_root()
inst = pd.read_csv(ROOT / "data/clean/instrument/instrument_mun.csv")
out  = pd.read_csv(ROOT / "data/clean/census/census_outcomes_municipality.csv")

SSIV_COLS = [c for c in inst.columns
             if c.startswith(("ssiv_","shareshock_","absexp_"))]

# --- 2001 baseline X stack, organized following Khanna §IIIB ------------------
#  A. Migrant flow characteristics (region shares — built from a per-municipality
#     destination-region file when available; otherwise empty).
#  B. Pre-shock development status (1990–95 in Khanna; we use 2001 census).
#  C. Pre-shock industrial structure.
#  D. Trade-shift-share (loaded from a separate file when available).
#
# All entries below MUST be columns present in census_outcomes_municipality.csv
# at year=2001 (with non-trivial coverage). Anything not present silently drops.

#  - Drop variables that are all-NA in 2001 (radio, TV, mobile, etc.) — they
#    can't serve as baseline X. We keep what 2001 census actually measured.
#  - Avoid linearly dependent groups (work_agri + work_nonagri ≈ work_lfp).

BASE_X_DEV = [    # Block B: development status
    "amen_lighting_electricity",   # electrification
    "amen_water_piped",            # infrastructure
    "ent_has_nonagro",             # entrepreneurship density
    "head_age_mean",               # demographic structure
    "edu_literate",                # human capital
]
BASE_X_IND = [    # Block C: industrial / sectoral structure (Khanna §IIIB)
    "work_share_agriculture",        # primary
    "work_share_nonagriculture",     # services + industry combined
    "ind_manufacturing",             # secondary
    "ind_finance_real_estate_prof",  # finance / business / professional
    "flfp_all",                      # baseline female labour force
]
BASE_X_BIG = BASE_X_DEV + BASE_X_IND   # for "mi_share_baseX" / "khanna" sets

# Optional: per-municipality destination-region shares produced by the patched
# instrument.R (file may not exist — silently skipped).
REGION_FILE     = str(ROOT / "data/clean/instrument/dest_region_shares_2001.csv")
TRADE_SSIV_FILE = str(ROOT / "data/clean/instrument/trade_ssiv.csv")
WDI_GDP_FILE    = str(ROOT / "data/clean/instrument/wdi_dest_gdp_2001.csv")
DEST_SHARE_FILE = str(ROOT / "data/clean/instrument/dest_mun_mig_share_2001.csv")

def _load_optional_csv(path):
    try:
        return pd.read_csv(path)
    except FileNotFoundError:
        return None

REGION_DF = _load_optional_csv(REGION_FILE)
TRADE_DF  = _load_optional_csv(TRADE_SSIV_FILE)
WDI_DF    = _load_optional_csv(WDI_GDP_FILE)
DEST_SHARE_DF = _load_optional_csv(DEST_SHARE_FILE)

# Khanna Block A — destination GDP weighted by baseline migrant share.
# Aggregates dest GDP per capita to muni level: dest_gdp_o0 = Σ_d s_md0 · GDP_d_2001.
# Result is a per-muni cross-section variable usable as a baseline X covariate.
DEST_GDP_DF = None
if WDI_DF is not None and DEST_SHARE_DF is not None:
    _wdi = WDI_DF[["country", "gdp_pc_2001"]].dropna()
    _agg = (DEST_SHARE_DF.merge(_wdi, on="country", how="inner")
                  .assign(prod=lambda d: d["mun_mig_share_2001"] * d["gdp_pc_2001"])
                  .groupby("lgcode", as_index=False)
                  .agg(dest_gdp_pc_2001=("prod", "sum"),
                       coverage=("mun_mig_share_2001", "sum")))
    # Renormalise by coverage (some destinations may lack WDI data)
    _agg["dest_gdp_pc_2001"] = (_agg["dest_gdp_pc_2001"] /
                                 _agg["coverage"].where(_agg["coverage"] > 0, 1))
    DEST_GDP_DF = _agg[["lgcode", "dest_gdp_pc_2001"]]
    print(f"Khanna Block A: dest GDP/cap aggregated for {len(DEST_GDP_DF)} munis "
          f"(mean = USD {DEST_GDP_DF['dest_gdp_pc_2001'].mean():,.0f})")

REGION_COLS = ([c for c in REGION_DF.columns if c.startswith("share_")]
               if REGION_DF is not None else [])
# Region shares sum to 1 across all destinations within each muni. Drop the
# largest region as the implicit reference category to avoid the collinearity
# when interacting all region shares with year FE.
if REGION_COLS and REGION_DF is not None:
    _ref_region = REGION_DF[REGION_COLS].mean().idxmax()
    REGION_COLS = [c for c in REGION_COLS if c != _ref_region]
    print(f"Region-share reference (omitted): {_ref_region}")
TRADE_COLS  = ([c for c in TRADE_DF.columns
                if c not in ("lgcode","year")]
               if TRADE_DF is not None else [])

def build_panel(years, lag, ref_year):
    """Return a panel for the given census years, with shocks at year-lag merged in."""
    inst_lag = inst.copy()
    inst_lag["year"] = inst_lag["year"] + lag      # so shock-at-(year-lag) lands on year
    inst_lag = inst_lag[["lgcode","year","geog_intensity_2001","geog_total_mig_2001"] + SSIV_COLS]

    # 2001 baseline characteristics — keep only those with actual variation in 2001
    cand = [c for c in (BASE_X_DEV + BASE_X_IND) if c in out.columns]
    y2001 = out.query("year==2001")
    cand = [c for c in cand if y2001[c].notna().sum() > 100 and y2001[c].std() > 1e-6]
    base2001 = y2001[["lgcode"] + cand].rename(
        columns=lambda c: c+"_2001" if c!="lgcode" else c)

    p = (out.merge(inst_lag, on=["lgcode","year"], how="left")
            .merge(base2001, on="lgcode", how="left")
            .query("year in @years").copy())

    if REGION_DF is not None:
        p = p.merge(REGION_DF, on="lgcode", how="left")
        for c in REGION_COLS: p[c] = p[c].fillna(p[c].mean())
    if TRADE_DF is not None:
        p = p.merge(TRADE_DF, on=["lgcode","year"], how="left")
        for c in TRADE_COLS: p[c] = p[c].fillna(0)
    if DEST_GDP_DF is not None:
        p = p.merge(DEST_GDP_DF, on="lgcode", how="left")
        p["dest_gdp_pc_2001"] = p["dest_gdp_pc_2001"].fillna(p["dest_gdp_pc_2001"].mean())

    p[SSIV_COLS] = p[SSIV_COLS].fillna(0)
    p["geog_intensity_2001"] = p["geog_intensity_2001"].fillna(0)
    p["log_mi"] = np.arcsinh(p["geog_intensity_2001"])

    # Build "× 1{t=k}" interactions for every non-reference year
    other_years = [y for y in years if y != ref_year]
    base_present = [f"{c}_2001" for c in BASE_X_BIG if f"{c}_2001" in p.columns]

    for y in other_years:
        ind = (p["year"] == y).astype(int)
        p[f"mi_x_{y}"]         = p["log_mi"] * ind
        p[f"shareshock_x_{y}"] = p["shareshock_index_2001"] * ind
        for c in base_present:
            v = p[c].fillna(p[c].mean())
            p[f"{c}_x_{y}"] = v * ind
        for c in REGION_COLS:
            p[f"{c}_x_{y}"] = p[c] * ind
        if DEST_GDP_DF is not None:
            p[f"dest_gdp_pc_2001_x_{y}"] = p["dest_gdp_pc_2001"] * ind
    # Trade SSIV varies by year naturally — include as plain panel covariate
    # rather than year-indicator interactions. Year-indicator + trade × year
    # interactions create perfect collinearity in the T=2 census panel.

    # Alternative shock variants (winsorize, asinh) computed on the panel-specific shock
    hi = p["ssiv_index_2001"].quantile(0.99)
    p["ssiv_w99"]   = p["ssiv_index_2001"].clip(upper=hi)
    p["ssiv_asinh"] = np.arcsinh(p["ssiv_index_2001"])

    # Standardize all shocks within this panel
    for k, col in SHOCK_RAW.items():
        sd = p[col].std()
        p[f"{k}_z"] = (p[col] - p[col].mean()) / sd if sd > 0 else 0.0

    return p, other_years, base_present

SHOCK_RAW = {
    "ssiv_index": "ssiv_index_2001",
    "ssiv_w99":   "ssiv_w99",
}
SHOCKS = {
    "ssiv_index": {"label": "SSIV (level index)",     "desc": "Per-capita SSIV; 2001=baseline. Main spec."},
    "ssiv_w99":   {"label": "SSIV winsorized at 99%", "desc": "Outlier-robust version of the main SSIV (clipped at 99th percentile)."},
}

# Control sets are defined by tags; the actual columns are built per-panel from
# the list of non-reference years and from whichever baseline X columns are
# present in the panel (base_present).
def control_cols(tag, other_years, base_present):
    yrs = other_years
    cols = []
    if tag == "none":
        return []
    if tag in ("mi", "khanna_full"):
        cols += [f"mi_x_{y}" for y in yrs]
    if tag == "khanna_full":
        cols += [f"shareshock_x_{y}" for y in yrs]
        cols += [f"{c}_x_{y}" for c in base_present for y in yrs]
        cols += [f"{c}_x_{y}" for c in REGION_COLS for y in yrs]
        if DEST_GDP_DF is not None:
            cols += [f"dest_gdp_pc_2001_x_{y}" for y in yrs]
        # Trade SSIV is muni-year already; include as plain panel covariate.
        cols += list(TRADE_COLS)
    return cols

# Used to filter columns that may not exist in a given panel
CURRENT_PANEL_COLS = set()

CONTROLS = {
    "none":        "No controls (just FEs)",
    "mi":          "+ MI × 1{t≠ref}  (basic Khanna baseline)",
    "khanna_full": "Khanna et al. (2026): + ShareShock + baseline X × t + region shares × t + dest GDP × t + trade SSIV  (needs optional inputs)",
}

PANELS = {
    "P2_ref2011":      ("2011, 2021 (ref 2011)",          [2011, 2021], 2011),
    "P3_ref2001":      ("2001, 2011, 2021 (ref 2001)",    [2001, 2011, 2021], 2001),
    "P3_ref2011":      ("2001, 2011, 2021 (ref 2011)",    [2001, 2011, 2021], 2011),
}

LAGS = {
    "lag0": ("Contemporaneous (Z_t)",   0),
    "lag1": ("Lagged 1 year (Z_{t-1})", 1),
    "lag2": ("Lagged 2 years (Z_{t-2})",2),
}

# Outcome groups
GROUPS = {
    "Amenities — water & toilet": [
        ("amen_water_piped","Piped water"),
        ("amen_water_traditional","Traditional water source"),
        ("amen_toilet_modern","Modern toilet"),
        ("amen_toilet_ordinary","Ordinary toilet"),
        ("amen_toilet_any","Any toilet"),
        ("amen_toilet_none","No toilet"),
    ],
    "Amenities — cooking & lighting": [
        ("amen_cooking_lpg","LPG (cooking)"),
        ("amen_cooking_wood","Wood (cooking)"),
        ("amen_cooking_kerosene","Kerosene (cooking)"),
        ("amen_cooking_biogas","Biogas (cooking)"),
        ("amen_cooking_electric","Electric (cooking)"),
        ("amen_cooking_modern","Modern fuel"),
        ("amen_cooking_traditional","Traditional fuel"),
        ("amen_lighting_electricity","Electric lighting"),
        ("amen_lighting_kerosene","Kerosene lighting"),
        ("amen_lighting_biogas","Biogas lighting"),
        ("amen_lighting_others","Other lighting"),
    ],
    "Assets": [
        ("amen_assets_radio","Radio"),
        ("amen_assets_tv","TV"),
        ("amen_assets_cycle","Bicycle"),
        ("amen_assets_motorcycle","Motorcycle"),
        ("amen_assets_car","Car"),
        ("amen_assets_fridge","Fridge"),
        ("amen_assets_landline","Landline"),
        ("amen_assets_mobile","Mobile"),
        ("amen_assets_computer","Computer"),
        ("amen_assets_internet","Internet"),
        ("amen_assets_none","No durable assets"),
        ("amen_asset_count_mean","Mean asset count"),
    ],
    "Housing": [
        ("housing_own","Own house"),
        ("housing_rented","Rented house"),
        ("housing_foundation_modern","Modern foundation"),
        ("housing_foundation_traditional","Traditional foundation"),
        ("housing_roof_modern","Modern roof"),
        ("housing_roof_traditional","Traditional roof"),
    ],
    "Labour (15-60)": [
        ("work_share_agriculture","Agri work"),
        ("work_share_nonagriculture","Non-agri work"),
        ("work_share_wage_nonagri","Wage non-agri"),
        ("work_share_own_nonagri","Own-account non-agri"),
        ("work_lfp","LFP"),
        ("work_share_student","Student"),
        ("work_share_household_chores","Household chores"),
        ("work_share_job_seeking","Job seeking"),
        ("work_share_no_work","No work"),
    ],
    "Employment type": [
        ("emp_share_employer","Employer"),
        ("emp_share_employee","Employee"),
        ("emp_share_self_employed","Self-employed"),
        ("emp_share_unpaid_family_worker","Unpaid family worker"),
    ],
    "Industry shares": [
        ("ind_agri_forestry_fish","Agriculture, forestry & fishing"),
        ("ind_manufacturing","Manufacturing"),
        ("ind_construction","Construction"),
        ("ind_wholesale_retail","Wholesale & retail trade"),
        ("ind_transport_accommodation","Transport & accommodation"),
        ("ind_finance_real_estate_prof","Finance, RE & professional"),
        ("ind_public_admin_defence","Public admin & defence"),
        ("ind_education","Education"),
        ("ind_health","Health"),
        ("ind_arts_recreation","Arts & recreation"),
        ("ind_others","Other industries"),
    ],
    "Occupation": [
        ("occ_share_armed_forces","Armed forces"),
        ("occ_share_managers","Managers"),
        ("occ_share_professionals","Professionals"),
        ("occ_share_technicians","Technicians"),
        ("occ_share_office_assistants","Office assistants"),
        ("occ_share_service_sales","Service & sales"),
        ("occ_share_agriculture","Agriculture workers"),
        ("occ_share_craft_trades","Craft & trades"),
        ("occ_share_machine_operators","Machine operators"),
        ("occ_share_elementary","Elementary"),
    ],
    "Education": [
        ("edu_literate","Literate"),
        ("edu_literate_female","Literate (female)"),
        ("edu_literate_male","Literate (male)"),
        ("edu_school_attend_6_16","School attendance (6-16)"),
        ("edu_school_attend_6_16_female","School attendance (6-16, F)"),
        ("edu_school_attend_6_16_male","School attendance (6-16, M)"),
        ("edu_attain_primary_plus","Primary+"),
        ("edu_attain_secondary_plus","Secondary+"),
        ("edu_attain_higher_secondary_plus","Higher secondary+"),
        ("edu_attain_tertiary","Tertiary"),
        ("edu_years_mean","Mean years schooling"),
    ],
    "In-migration": [
        ("mig_in_share","In-migrant share"),
        ("mig_in_domestic","Domestic in-migrants"),
        ("mig_in_international","International in-migrants"),
        ("mig_in_from_rural","From rural"),
        ("mig_in_from_urban","From urban"),
        ("mig_in_reason_economic","Reason: economic"),
        ("mig_in_reason_noneconomic","Reason: non-economic"),
        ("mig_in_reason_study","Reason: study"),
        ("mig_in_reason_marriage","Reason: marriage"),
    ],
    "Female labour & ownership": [
        ("flfp_all","Female LFP"),
        ("flfp_agri","Female LFP (agri)"),
        ("flfp_nonagri","Female LFP (non-agri)"),
        ("flfp_wage","Female LFP (wage)"),
        ("flfp_chores_only","Female chores only"),
        ("fem_employment_rate","Female employment rate"),
        ("fem_share_of_ag_workers","Female share of agri workers"),
        ("fem_ag_specialization_ratio","Female agri specialization"),
        ("fem_wage_share_of_employment","Female wage employment share"),
        ("fem_ownership_house","Women own house"),
        ("fem_ownership_land","Women own land"),
        ("fem_ownership_both","Women own house+land"),
    ],
    "Male labour": [
        ("mlfp_all","Male LFP"),
        ("mlfp_agri","Male LFP (agri)"),
        ("mlfp_nonagri","Male LFP (non-agri)"),
    ],
    "Gender gaps & shares": [
        ("share_women","Share women"),
        ("share_men","Share men"),
        ("gap_lfp_m_minus_f","LFP gap (M-F)"),
        ("gap_nonagri_m_minus_f","Non-agri LFP gap (M-F)"),
    ],
    "Household structure": [
        ("head_age_mean","Head age (mean)"),
        ("head_elderly_share","Elderly head"),
        ("head_young_share","Young head"),
        ("head_female_share","Female-headed HH"),
        ("head_female_elderly","Female elderly head"),
        ("absent_hh_share","HH with absentee"),
    ],
    "Children left behind": [
        ("left_not_with_both","Left without both parents"),
        ("left_mother_only","Left with mother only"),
        ("left_father_only","Left with father only"),
        ("left_with_relatives","Left with relatives"),
        ("left_without_parents","Left without parents"),
    ],
    "Marriage": [
        ("mar_ever_married_15_60","Ever married (15-60)"),
        ("mar_never_married_15_60","Never married (15-60)"),
        ("mar_female_age_first_mean","Female age at first marriage"),
        ("mar_female_married_by_18","Female married by 18"),
        ("mar_female_married_by_20","Female married by 20"),
    ],
    "Fertility & mortality": [
        ("fert_birth_mean","Births (mean)"),
        ("fert_birth_son_mean","Sons (mean)"),
        ("fert_birth_dau_mean","Daughters (mean)"),
        ("fert_births_last12m_share","Births last 12 months (share)"),
        ("fert_births_last12m_rate","Births last 12 months (rate)"),
        ("mort_children_dead_mean","Children dead (mean)"),
        ("mort_child_dead_any","Any child death"),
        ("mort_child_death_ratio","Child death ratio"),
    ],
}

def fit(df, y, shock_z, controls):
    d = df.dropna(subset=[y]).copy()
    if d[y].nunique()<2 or len(d)<50: return None
    rhs = [shock_z] + [c for c in controls if c in d.columns]
    d = d.set_index(["lgcode","year"])
    try:
        m = PanelOLS(d[y], d[rhs], entity_effects=True, time_effects=True,
                     drop_absorbed=True
                    ).fit(cov_type="clustered", cluster_entity=True)
    except Exception as e:
        return {"err": str(e)[:80]}
    if shock_z not in m.params.index:
        return {"err": "shock absorbed"}
    return {
        "beta": float(m.params[shock_z]),
        "se":   float(m.std_errors[shock_z]),
        "pval": float(m.pvalues[shock_z]),
        "n":    int(m.nobs),
        "n_mun": d.index.get_level_values("lgcode").nunique(),
        "mean_y": float(d[y].mean()),
        "r2_within": float(m.rsquared_within),
    }

# ---- Build everything --------------------------------------------------------
results = {"meta":{
    "panels":   {k: {"label": v[0], "years": v[1], "ref": v[2]} for k,v in PANELS.items()},
    "lags":     {k: v[0] for k,v in LAGS.items()},
    "shocks":   {k: dict(v) for k,v in SHOCKS.items()},
    "controls": dict(CONTROLS),
    "groups":   list(GROUPS.keys()),
}}

# Outcome -> group lookup
results["outcomes"] = {}
for gname, items in GROUPS.items():
    for y, lab in items:
        results["outcomes"][y] = {"label": lab, "group": gname}

# Estimates indexed by [panel][lag][shock][ctrl][outcome]
results["estimates"] = {}
results["panel_info"] = {}
# Skip control sets that need optional files we don't have
SKIPPED_CTRLS = []
if REGION_DF is None or TRADE_DF is None:
    SKIPPED_CTRLS += ["khanna_full"]
ACTIVE_CTRLS = [c for c in CONTROLS if c not in SKIPPED_CTRLS]
print(f"Active control sets: {ACTIVE_CTRLS}")
if SKIPPED_CTRLS:
    print(f"Skipping (missing optional files): {SKIPPED_CTRLS}")
results["meta"]["controls"] = {k: CONTROLS[k] for k in ACTIVE_CTRLS}

for panel_key, (_, years, ref_year) in PANELS.items():
    results["estimates"][panel_key] = {}
    results["panel_info"][panel_key] = {}
    for lag_key, (_, lag) in LAGS.items():
        df, other_years, base_present = build_panel(years, lag, ref_year)
        CURRENT_PANEL_COLS.clear()
        CURRENT_PANEL_COLS.update(df.columns)
        results["estimates"][panel_key][lag_key] = {}
        results["panel_info"][panel_key][lag_key] = {
            "n_obs": int(len(df)),
            "n_muni": int(df["lgcode"].nunique()),
            "years": sorted([int(y) for y in df["year"].unique()]),
        }
        for shock_key in SHOCKS:
            shock_z = f"{shock_key}_z"
            # Confirm shock has variation
            if df[shock_z].std() < 1e-8:
                continue
            results["estimates"][panel_key][lag_key][shock_key] = {}
            for ctrl_key in ACTIVE_CTRLS:
                ctrl_cols = control_cols(ctrl_key, other_years, base_present)
                results["estimates"][panel_key][lag_key][shock_key][ctrl_key] = {}
                for gname, items in GROUPS.items():
                    for y, lab in items:
                        if y not in df.columns: continue
                        r = fit(df, y, shock_z, ctrl_cols)
                        if r is None: continue
                        results["estimates"][panel_key][lag_key][shock_key][ctrl_key][y] = r
        print(f"  done panel={panel_key} lag={lag_key}")

# ---- Wrap census results under "datasets/census" and add 3 more datasets ----
census_dataset = {
    "label":      "Census · municipality × census-year (2001-2021)",
    "panels":     results["meta"]["panels"],
    "lags":       results["meta"]["lags"],
    "shocks":     results["meta"]["shocks"],
    "controls":   results["meta"]["controls"],
    "groups":     results["meta"]["groups"],
    "outcomes":   results["outcomes"],
    "panel_info": results["panel_info"],
    "estimates":  results["estimates"],
}
final = {
    "datasets_meta": {
        "census":    {"label": "Census (muni × census-year)",  "default": True},
        "rvs":       {"label": "HRVS (HH × year, 2016-2018)"},
        "nec_cs":    {"label": "NEC cross-section (firms 2018)"},
        "nec_panel": {"label": "NEC firm panel (founding-year × muni, 2001-2018)"},
    },
    "datasets": {"census": census_dataset},
}


# =============================================================================
# RVS dataset — household × year panel (HRVS, 2016-2018)
# =============================================================================
def _rvs(name): return pd.read_csv(ROOT / "data/clean/rvs_outcomes" / f"{name}.csv")

def compute_rvs():
    print("\n[RVS] Loading household survey panels...")
    ag   = _rvs("agriculture_hh_year")
    mig  = _rvs("migration_hh_year")
    lab  = _rvs("labour_hh_year")
    ent  = _rvs("enterprise_hh_year")
    cons = _rvs("consumption_hh_year")
    hlt  = _rvs("health_hh_year")
    edu  = _rvs("education_hh_year")
    shk  = _rvs("shocks_coping_hh_year")

    KEYS = ["hhid","year"]
    base = ag[["hhid","year","vmun_code"]].drop_duplicates(KEYS)
    def merge_outs(df, src, cols):
        cols = [c for c in cols if c in src.columns]
        s = src[KEYS + cols].drop_duplicates(KEYS)
        return df.merge(s, on=KEYS, how="left")

    R_GROUPS = {
        "Agriculture · Land, tenure & livestock": [
            ("agri_hh",                  "HH operates a farm",                False),
            ("owned_plots_n",            "# owned plots",                     False),
            ("owned_area_sqm",           "Owned area (sqm)",                  False),
            ("cultivated_area_sqm",      "Cultivated area (sqm)",             False),
            ("rented_in_any",            "Rented / sharecropped land in",     False),
            ("plot_wet_fallow_share",    "Wet-season fallow share",           False),
            ("plot_dry_fallow_share",    "Dry-season fallow share",           False),
            ("double_crop_share",        "Double-cropped plot share",         False),
            ("livestock_has",            "Owns livestock",                    False),
        ],
        "Agriculture · Wet season inputs": [
            ("wet_use_fertiliser",       "Uses fertiliser (wet)",             False),
            ("wet_use_pesticide",        "Uses pesticide (wet)",              False),
            ("wet_use_hired_labour",     "Hires labour (wet)",                False),
            ("wet_use_equipment",        "Hires equipment (wet)",             False),
        ],
        "Agriculture · Dry season inputs": [
            ("dry_use_fertiliser",       "Uses fertiliser (dry)",             False),
            ("dry_use_pesticide",        "Uses pesticide (dry)",              False),
            ("dry_use_hired_labour",     "Hires labour (dry)",                False),
            ("dry_use_equipment",        "Hires equipment (dry)",             False),
        ],
        "Agriculture · Equipment & inputs": [
            ("owns_tractor",             "Owns tractor / power tiller",       False),
            ("owns_pump",                "Owns water pump / tubewell",        False),
            ("owns_modern_equip",        "Owns any modern equipment",         False),
            ("n_modern_equip_types",     "# modern equipment types",          False),
            ("ag_equip_stock_value_rs",      "Equipment stock value (all HHs)",     False),
            ("ag_equip_stock_value_rs_pos",  "Equipment stock value (owners only)", False),
            ("input_total_12m_rs",           "Total input spend (all HHs)",          False),
            ("input_total_12m_rs_pos",       "Total input spend (cultivators only)", False),
            ("input_intensity_per_sqm",  "Input spend per cultivated sqm",    False),
        ],
        "Agriculture · Crops & sales": [
            ("n_crop_types",             "# distinct crops grown",            False),
            ("grows_staple",             "Grows any staple crop",             False),
            ("grows_cashcrop",           "Grows any cash crop",               False),
            ("grows_horticulture",       "Grows fruits / vegetables",         False),
            ("crop_sold_any",            "Sold any crop",                     False),
            ("crop_sales_12m_rs",        "Crop sales (all HHs)",              False),
            ("crop_sales_12m_rs_pos",    "Crop sales (sellers only)",         False),
            ("crop_sale_share",          "Crop sale share",                   False),
        ],
        "Migration · Migrants": [
            ("has_migrant",                  "HH has any migrant",            False),
            ("has_migrant_internal",         "HH has internal migrant",       False),
            ("mig_cost_financed_by_loan_any","Migration financed by loan",    False),
        ],
        "Migration · Remittance flows & use": [
            ("remit_received",               "Remittance received",           False),
            ("remit_amount_12m_rs",          "Remit amount (all HHs)",        False),
            ("remit_amount_12m_rs_pos",      "Remit amount (recipients only)",False),
            ("remit_amount_intl_12m_rs",     "Remit amount intl (all HHs)",   False),
            ("remit_amount_intl_12m_rs_pos", "Remit amount intl (recipients)",False),
            ("remit_via_formal_any",         "Remit via bank / IME",          False),
            ("remit_via_hundi_any",          "Remit via hundi",               False),
            ("remit_use_consumption_any",    "Remit → consumption",           False),
            ("remit_use_education_any",      "Remit → education",             False),
            ("remit_use_business_any",       "Remit → business / productive", False),
        ],
        "Labour": [
            ("n_jobs_total",                 "# jobs in HH",                     False),
            ("share_wage_agri",              "Share of jobs: wage agri",         False),
            ("share_wage_nonagri",           "Share of jobs: wage non-agri",     False),
            ("share_self_agri",              "Share of jobs: self-emp agri",     False),
            ("share_self_nonagri",           "Share of jobs: self-emp non-agri", False),
            ("hh_has_wage_job",              "Any wage job in HH",               False),
            ("n_wage_jobs_hh",               "# wage jobs in HH",                False),
            ("wage_total_income_12m_rs",     "Wage income (all HHs)",            False),
            ("wage_total_income_12m_rs_pos", "Wage income (earners only)",       False),
            ("wage_sector_primary_any",      "Any primary-sector wage job",      False),
            ("wage_sector_secondary_any",    "Any secondary-sector wage job",    False),
            ("wage_sector_tertiary_any",     "Any tertiary-sector wage job",     False),
        ],
        "Enterprise": [
            ("has_nonag_enterprise",          "Has non-farm enterprise",          False),
            ("n_nonag_enterprises",           "# non-farm enterprises",           False),
            ("enterprise_workers_total",      "Total enterprise workers",         False),
            ("enterprise_revenue_12m",        "Revenue (all HHs)",                False),
            ("enterprise_revenue_12m_pos",    "Revenue (operators only)",         False),
            ("enterprise_expenses_12m",       "Expenses (all HHs)",               False),
            ("enterprise_expenses_12m_pos",   "Expenses (operators only)",        False),
            ("enterprise_profit_12m",         "Profit (all HHs)",                 False),
            ("enterprise_profit_12m_pos",     "Profit (operators only)",          False),
            ("enterprise_capex_12m",          "Capex (all HHs)",                  False),
            ("enterprise_capex_12m_pos",      "Capex (operators with capex)",     False),
            ("enterprise_profit_margin",      "Profit margin (operators)",        False),
            ("sector_agriculture_share",                "Share in agri / forestry / fish",  False),
            ("sector_manufacturing_construction_share", "Share in manuf / construction",    False),
            ("sector_services_share",                   "Share in services",                False),
        ],
        "Consumption · Food": [
            ("food_total_7day",            "Food total (7d)",                False),
            ("food_purchased_7day",        "Food purchased (7d)",            False),
            ("food_homeprod_7day",         "Food from own production (7d)",  False),
            ("food_homeprod_share",        "Share of food from own production", False),
            ("food_staples_7day",          "Staples (7d)",                   False),
            ("food_protein_7day",          "Protein foods (7d)",             False),
            ("food_animal_7day",           "Animal-source foods (7d)",       False),
            ("food_vegfruit_7day",         "Vegetables / fruit (7d)",        False),
            ("food_groups_consumed",       "# food groups consumed",         False),
            ("perceived_food_insecurity",  "Perceived food insecurity",      False),
            ("food_insec_any",             "Any realized food insecurity",   False),
            ("food_insec_index",           "Food insecurity breadth index",  False),
            ("severe_food_insecurity",     "Severe food insecurity",         False),
        ],
        "Consumption · Non-food": [
            ("nonfood_exp_30day",          "Non-food exp (30d)",             False),
            ("nonfood_exp_12m",            "Non-food exp (12m)",             False),
            ("nonfood_basic_nonfood_12m",            "Basic non-food (12m)",             False),
            ("nonfood_energy_fuel_lighting_12m",     "Energy / fuel / lighting (12m)",   False),
            ("nonfood_clothing_personal_12m",        "Clothing / personal (12m)",        False),
            ("nonfood_transport_communication_12m",  "Transport / communication (12m)",  False),
            ("nonfood_housing_household_12m",        "Housing / household goods (all HHs)",      False),
            ("nonfood_housing_household_12m_pos",    "Housing / household goods (spenders only)",False),
            ("nonfood_education_leisure_12m",        "Education / leisure (12m)",        False),
            ("nonfood_social_ceremonial_financial_12m", "Social / ceremony / finance (12m)", False),
            ("nonfood_luxury_valuables_12m",         "Jewellery / luxury (all HHs)",     False),
            ("nonfood_luxury_valuables_12m_pos",     "Jewellery / luxury (buyers only)", False),
        ],
        "Consumption · Durables": [
            ("durables_stock_value",       "Durables stock value",           False),
            ("durables_use_value_12m",     "Imputed durables consumption (12m)", False),
        ],
        "Education": [
            ("any_enrolled",           "Any school enrollment",                 False),
            ("n_private_school",       "# in private school",                   False),
            ("n_scholarship",          "# scholarships",                        False),
            ("scholarship_amt_12m",    "Scholarship amount (12m)",              False),
            ("edu_spend_total_12m",     "Education spend (all HHs)",            False),
            ("edu_spend_total_12m_pos", "Education spend (spenders only)",      False),
            ("edu_spend_per_enrolled", "Education spend per enrolled child",    False),
        ],
        "Health": [
            ("any_insured",            "Any health card holder",                False),
            ("n_chronic",              "# members with chronic illness",        False),
            ("n_acute_illness",        "# members with acute illness (30d)",    False),
            ("any_health_spending",    "Any health spending",                   False),
            ("hlt_spend_total",        "Health spend total (all HHs)",          False),
            ("hlt_spend_total_pos",    "Health spend total (spenders only)",    False),
        ],
        "Shocks & coping": [
            ("any_shock",        "HH reports any shock",     False),
            ("coped_self",       "Cope: own resources",      False),
            ("coped_external",   "Cope: external help",      False),
        ],
    }

    # Build wide HH×year panel. Each sub-group reads from the same underlying
    # CSV; the dict below maps a per-group source key onto each new sub-group.
    panel0 = base.copy()
    src_map = {
        "Agriculture · Land, tenure & livestock": ag,
        "Agriculture · Wet season inputs":        ag,
        "Agriculture · Dry season inputs":        ag,
        "Agriculture · Equipment & inputs":       ag,
        "Agriculture · Crops & sales":            ag,
        "Migration · Migrants":                   mig,
        "Migration · Remittance flows & use":     mig,
        "Labour":                                 lab,
        "Enterprise":                             ent,
        "Consumption · Food":                     cons,
        "Consumption · Non-food":                 cons,
        "Consumption · Durables":                 cons,
        "Education":                              edu,
        "Health":                                 hlt,
        "Shocks & coping":                        shk,
    }
    for gname, items in R_GROUPS.items():
        cols = [y for y,_,_ in items]
        src = src_map[gname]
        if isinstance(src, list):
            for s in src: panel0 = merge_outs(panel0, s, cols)
        else:
            panel0 = merge_outs(panel0, src, cols)

    # Shocks file is restricted to HHs that reported a shock; HHs not present
    # are treated as zero so any_shock / coped_* are 0/1 over the full HH panel.
    for c in ("any_shock", "coped_self", "coped_external"):
        if c in panel0.columns:
            panel0[c] = panel0[c].fillna(0)

    def build_for_lag(lag):
        """Merge instrument at (vmun_code, year-lag) so each HH-year row carries Z_{m, t-lag}."""
        inst_lag = inst.copy()
        inst_lag["year"] = inst_lag["year"] + lag
        inst_keep = inst_lag[["lgcode","year","geog_intensity_2001"] + SSIV_COLS]\
                       .rename(columns={"lgcode":"vmun_code"})
        p = panel0.merge(inst_keep, on=["vmun_code","year"], how="inner").copy()
        p[SSIV_COLS] = p[SSIV_COLS].fillna(0)
        p["geog_intensity_2001"] = p["geog_intensity_2001"].fillna(0)
        p["log_mi"] = np.arcsinh(p["geog_intensity_2001"])
        p["ssiv_w99"]   = p["ssiv_index_2001"].clip(upper=p["ssiv_index_2001"].quantile(0.99))
        p["ssiv_asinh"] = np.arcsinh(p["ssiv_index_2001"])

        # Khanna Eq. 4: MI × D_t and Rshock × D_t use the FULL period-FE vector.
        # HRVS has 3 years (2016, 2017, 2018); 2016 is the within-sample reference.
        for y in (2017, 2018):
            ind = (p["year"] == y).astype(int)
            p[f"mi_x_{y}"]         = p["log_mi"] * ind
            p[f"shareshock_x_{y}"] = p["shareshock_index_2001"] * ind

        # Region shares × (year − 2001) trend; trade SSIV as panel covariate.
        if REGION_DF is not None:
            p = p.merge(REGION_DF.rename(columns={"lgcode":"vmun_code"}),
                        on="vmun_code", how="left")
            for c in REGION_COLS:
                p[c] = p[c].fillna(p[c].mean())
                p[f"{c}_x_t"] = p[c] * (p["year"] - 2001)
        if TRADE_DF is not None:
            p = p.merge(TRADE_DF.rename(columns={"lgcode":"vmun_code"}),
                        on=["vmun_code","year"], how="left")
            for c in TRADE_COLS: p[c] = p[c].fillna(0)
        # Block A dest GDP weighted by baseline migrant share
        if DEST_GDP_DF is not None:
            p = p.merge(DEST_GDP_DF.rename(columns={"lgcode":"vmun_code"}),
                        on="vmun_code", how="left")
            p["dest_gdp_pc_2001"] = p["dest_gdp_pc_2001"].fillna(p["dest_gdp_pc_2001"].mean())
            p["dest_gdp_pc_2001_x_t"] = p["dest_gdp_pc_2001"] * (p["year"] - 2001)

        # Conditional-on-positive variants for zero-heavy monetary outcomes:
        # raw value when y > 0, NaN otherwise. fit_rvs's dropna will then run
        # the same regression on the receiving subsample only.
        POS_VARS = [
            "ag_equip_stock_value_rs",
            "input_total_12m_rs",
            "crop_sales_12m_rs",
            "remit_amount_12m_rs",
            "remit_amount_intl_12m_rs",
            "wage_total_income_12m_rs",
            "enterprise_revenue_12m",
            "enterprise_expenses_12m",
            "enterprise_profit_12m",
            "enterprise_capex_12m",
            "nonfood_housing_household_12m",
            "nonfood_luxury_valuables_12m",
            "hlt_spend_total",
            "edu_spend_total_12m",
        ]
        for v in POS_VARS:
            if v in p.columns:
                p[f"{v}_pos"] = p[v].where(p[v] > 0)

        for k, col in SHOCK_RAW.items():
            sd = p[col].std()
            p[f"{k}_z"] = (p[col] - p[col].mean()) / sd if sd > 0 else 0.0
        return p

    # Fit one HH-FE regression
    def fit_rvs(df, y, shock_z, controls, asinh):
        d = df[["hhid","year",shock_z, "vmun_code", y] + controls].dropna(subset=[y, shock_z]).copy()
        if d[y].nunique() < 2 or len(d) < 200: return None
        if asinh: d[y] = np.arcsinh(d[y])
        d["vmun_code"] = d["vmun_code"].astype(int)
        d_idx = d.set_index(["hhid","year"])
        cl = pd.DataFrame({"v": d["vmun_code"].values}, index=d_idx.index)
        rhs = [shock_z] + [c for c in controls if c in d.columns]
        try:
            m = PanelOLS(d_idx[y], d_idx[rhs], entity_effects=True, time_effects=True,
                         drop_absorbed=True
                        ).fit(cov_type="clustered", clusters=cl)
        except Exception as e:
            return {"err": str(e)[:80]}
        if shock_z not in m.params.index: return {"err": "shock absorbed"}
        return {
            "beta": float(m.params[shock_z]),
            "se":   float(m.std_errors[shock_z]),
            "pval": float(m.pvalues[shock_z]),
            "n":    int(m.nobs),
            "n_unit": d_idx.index.get_level_values("hhid").nunique(),
            "mean_y": float(d[y].mean()),
            "r2_within": float(m.rsquared_within),
        }

    PANELS_R   = {"HRVS_2016_2018": {"label":"2016–2018 (ref-2001 trend)", "years":[2016,2017,2018], "ref":2001}}
    LAGS_R     = {
        "lag0": "Contemporaneous (Z_t)",
        "lag1": "Lagged 1 year (Z_{t-1})",
        "lag2": "Lagged 2 years (Z_{t-2})",
    }
    CONTROLS_R = {
        "none": "No controls (HH FE + year FE only)",
        "mi":   "+ MI × year FE  (basic Khanna baseline)",
    }
    if REGION_DF is not None and TRADE_DF is not None:
        CONTROLS_R["khanna_full"] = "+ MI + ShareShock + region × t + dest GDP × t + trade SSIV"

    def ctrl_cols_r(tag):
        cols = []
        if tag == "none":
            return cols
        # Basic Khanna baseline: MI × full period FE (Khanna Eq. 4).
        if tag in ("mi", "khanna_full"):
            cols += ["mi_x_2017", "mi_x_2018"]
        if tag == "khanna_full":
            cols += ["shareshock_x_2017", "shareshock_x_2018"]
            cols += [f"{c}_x_t" for c in REGION_COLS]
            if DEST_GDP_DF is not None:
                cols += ["dest_gdp_pc_2001_x_t"]
            cols += list(TRADE_COLS)
        return cols

    outcomes_d = {}
    for gname, items in R_GROUPS.items():
        for y, lab, _ in items:
            outcomes_d[y] = {"label": lab, "group": gname}

    estimates  = {"HRVS_2016_2018": {}}
    panel_info = {"HRVS_2016_2018": {}}
    LAG_VALS = {"lag0": 0, "lag1": 1, "lag2": 2}
    for lag_key, lag in LAG_VALS.items():
        panel = build_for_lag(lag)
        panel_info["HRVS_2016_2018"][lag_key] = {
            "n_obs":  int(len(panel)),
            "n_unit": int(panel["hhid"].nunique()),
            "n_muni": int(panel["vmun_code"].nunique()),
            "years":  sorted([int(y) for y in panel["year"].unique()]),
        }
        estimates["HRVS_2016_2018"][lag_key] = {}
        n_cells = 0
        for shock_key in SHOCKS:
            shock_z = f"{shock_key}_z"
            if panel[shock_z].std() < 1e-8: continue
            estimates["HRVS_2016_2018"][lag_key][shock_key] = {}
            for ctrl_key in CONTROLS_R:
                cc = ctrl_cols_r(ctrl_key)
                estimates["HRVS_2016_2018"][lag_key][shock_key][ctrl_key] = {}
                for gname, items in R_GROUPS.items():
                    for y, lab, asinh_flag in items:
                        if y not in panel.columns: continue
                        r = fit_rvs(panel, y, shock_z, cc, asinh_flag)
                        if r is None: continue
                        estimates["HRVS_2016_2018"][lag_key][shock_key][ctrl_key][y] = r
                        n_cells += 1
        print(f"  [RVS] {lag_key}: {n_cells} cells")

    return {
        "label":      "HRVS · household × year (2016-2018)",
        "panels":     {k: v for k,v in PANELS_R.items()},
        "lags":       LAGS_R,
        "shocks":     {k: SHOCKS[k] for k in SHOCKS},
        "controls":   CONTROLS_R,
        "groups":     list(R_GROUPS.keys()),
        "outcomes":   outcomes_d,
        "panel_info": panel_info,
        "estimates":  estimates,
    }


# =============================================================================
# NEC cross-section dataset — municipality, 2018 firm census
# =============================================================================
def compute_nec_cs():
    print("\n[NEC-CS] Loading firm census municipality file...")
    import statsmodels.api as sm
    mun = pd.read_csv(ROOT / "data/clean/nec2018/municipality_analysis.csv")

    def build_for_lag(lag):
        """Pull instrument values at year = 2018 - lag."""
        target_year = 2018 - lag
        inst_yr = inst.query("year==@target_year")[["lgcode","geog_intensity_2001"] + SSIV_COLS]
        d = mun.merge(inst_yr, on="lgcode", how="inner").copy()
        d["log_mi"] = np.arcsinh(d["geog_intensity_2001"])
        d["DIST"]   = d["DIST"].astype(str)
        # Standardise shocks (cross-section SD)
        d["ssiv_w99"]   = d["ssiv_index_2001"].clip(upper=d["ssiv_index_2001"].quantile(0.99))
        d["ssiv_asinh"] = np.arcsinh(d["ssiv_index_2001"])

        # Cross-section: region shares enter as plain covariates; trade SSIV
        # enters at the chosen lag's source year too.
        if REGION_DF is not None:
            d = d.merge(REGION_DF, on="lgcode", how="left")
            for c in REGION_COLS: d[c] = d[c].fillna(d[c].mean())
        if TRADE_DF is not None:
            t_yr = TRADE_DF.query("year == @target_year")[["lgcode"] + list(TRADE_COLS)]
            d = d.merge(t_yr, on="lgcode", how="left")
            for c in TRADE_COLS: d[c] = d[c].fillna(0)
        if DEST_GDP_DF is not None:
            d = d.merge(DEST_GDP_DF, on="lgcode", how="left")
            d["dest_gdp_pc_2001"] = d["dest_gdp_pc_2001"].fillna(d["dest_gdp_pc_2001"].mean())

        for k, col in SHOCK_RAW.items():
            sd = d[col].std()
            d[f"{k}_z"] = (d[col] - d[col].mean()) / sd if sd > 0 else 0.0
        return d, target_year

    NEC_CS_GROUPS = {
        "Firm presence & scale": [
            ("n_firms",           "# firms",              False),
            ("emp_total",         "Total employment",     False),
            ("mean_emp_per_firm", "Mean emp per firm",    False),
            ("p90_emp_per_firm",  "90th pct emp per firm",False),
        ],
        "Firm size composition": [
            ("share_firms_size_micro_1",      "Share micro firms (1)",     False),
            ("share_firms_size_small_2_9",    "Share small firms (2-9)",   False),
            ("share_firms_size_medium_10_50", "Share medium firms (10-50)",False),
            ("share_firms_size_large_51p",    "Share large firms (51+)",   False),
        ],
        "Formality": [
            ("share_registered",     "Share registered",     False),
            ("share_tax_registered", "Share tax-registered", False),
            ("share_keeps_accounts", "Share keeps accounts", False),
            ("share_incorporated",   "Share incorporated",   False),
            ("formality_index",      "Formality index",      False),
        ],
        "Sector composition": [
            ("share_firms_sec_sec_manuf",       "Share manufacturing",       False),
            ("share_firms_sec_sec_construct",   "Share construction",        False),
            ("share_firms_sec_sec_wholesale",   "Share wholesale & retail",  False),
            ("share_firms_sec_sec_hospitality", "Share hospitality",         False),
            ("share_firms_sec_sec_transport",   "Share transport",           False),
            ("share_firms_sec_sec_services",    "Share other services",      False),
            ("share_firms_sec_sec_health",      "Share health",              False),
            ("share_firms_sec_sec_education",   "Share education",           False),
        ],
        "Tradability": [
            ("share_trd_tradable_goods",        "Share tradable goods",      False),
            ("share_trd_tradable_services",     "Share tradable services",   False),
            ("share_trd_non_tradable_services", "Share non-tradable svc",    False),
        ],
        "Modernity": [
            ("share_modern_modern_services",      "Modern services",         False),
            ("share_modern_modern_manuf",         "Modern manufacturing",    False),
            ("share_modern_traditional_commerce", "Traditional commerce",    False),
            ("share_modern_traditional_services", "Traditional services",    False),
        ],
        "Productivity & capital": [
            ("rev_mean",                 "Mean revenue",                False),
            ("labor_prod_median",        "Median labour productivity",  False),
            ("value_added_pw_median",    "Median VA/worker",            False),
            ("capital_intensity_median", "Median capital intensity",    False),
            ("profit_margin_median",     "Median profit margin",        False),
        ],
        "Credit & finance": [
            ("share_borrowed_any",  "Share borrowed any",        False),
            ("share_formal_credit", "Share with formal credit",  False),
            ("interest_p50",        "Interest rate p50",         False),
        ],
        "Gender": [
            ("share_female_manager", "Share female manager",      False),
            ("share_female_owner",   "Share female owner",        False),
            ("share_female_led",     "Share female-led",          False),
            ("share_female_workers", "Share female workers",      False),
        ],
        "Firm age": [
            ("share_firms_young_5y",   "Share firms < 5 yrs old",  False),
            ("share_firms_mature_10y", "Share firms > 10 yrs old", False),
            ("median_firm_age",        "Median firm age",          False),
        ],
    }

    def fit_cs(df, y, shock_z, controls, asinh):
        d = df[[shock_z, "DIST", y] + controls].dropna(subset=[y, shock_z]).copy()
        if d[y].nunique() < 2 or len(d) < 30: return None
        if asinh: d[y] = np.arcsinh(d[y])
        X = pd.get_dummies(d[["DIST"]], drop_first=True).astype(float)
        X[shock_z] = d[shock_z].values
        for c in controls:
            if c in d.columns: X[c] = d[c].values
        X = sm.add_constant(X)
        try:
            res = sm.OLS(d[y].values, X).fit(
                cov_type="cluster", cov_kwds={"groups": d["DIST"].values})
        except Exception as e:
            return {"err": str(e)[:80]}
        if shock_z not in res.params.index: return {"err": "shock absorbed"}
        return {
            "beta": float(res.params[shock_z]),
            "se":   float(res.bse[shock_z]),
            "pval": float(res.pvalues[shock_z]),
            "n":    len(d),
            "n_unit": d["DIST"].nunique(),
            "mean_y": float(d[y].mean()),
            "r2_within": float(res.rsquared),
        }

    CONTROLS_NCS = {
        "none": "No controls",
        "mi":   "+ log(MI₀)  (basic Khanna baseline)",
    }
    if REGION_DF is not None and TRADE_DF is not None:
        CONTROLS_NCS["khanna_full"] = "+ log(MI₀) + region shares + trade SSIV"

    def ctrl_cols_ncs(tag):
        if tag == "none": return []
        if tag == "mi":   return ["log_mi"]
        if tag == "khanna_full": return ["log_mi"] + list(REGION_COLS) + list(TRADE_COLS)
        return []

    outcomes_d = {y: {"label": lab, "group": g}
                  for g, items in NEC_CS_GROUPS.items() for y, lab, _ in items}

    LAGS_NCS = {
        "lag0": "Z at 2018 (firm-survey year)",
        "lag1": "Z at 2017 (1y before survey)",
        "lag2": "Z at 2016 (2y before survey)",
    }
    LAG_VALS = {"lag0": 0, "lag1": 1, "lag2": 2}

    estimates  = {"NEC_2018": {}}
    panel_info = {"NEC_2018": {}}
    for lag_key, lag in LAG_VALS.items():
        df, src_year = build_for_lag(lag)
        panel_info["NEC_2018"][lag_key] = {
            "n_obs":  int(len(df)),
            "n_unit": int(df["DIST"].nunique()),
            "n_muni": int(df["lgcode"].nunique()),
            "years":  [src_year],
        }
        estimates["NEC_2018"][lag_key] = {}
        n_cells = 0
        for shock_key in SHOCKS:
            shock_z = f"{shock_key}_z"
            if df[shock_z].std() < 1e-8: continue
            estimates["NEC_2018"][lag_key][shock_key] = {}
            for ctrl_key in CONTROLS_NCS:
                cc = ctrl_cols_ncs(ctrl_key)
                estimates["NEC_2018"][lag_key][shock_key][ctrl_key] = {}
                for gname, items in NEC_CS_GROUPS.items():
                    for y, lab, asinh_flag in items:
                        if y not in df.columns: continue
                        r = fit_cs(df, y, shock_z, cc, asinh_flag)
                        if r is None: continue
                        estimates["NEC_2018"][lag_key][shock_key][ctrl_key][y] = r
                        n_cells += 1
        print(f"  [NEC-CS] {lag_key} (Z at {src_year}): {n_cells} cells")

    return {
        "label":      "NEC cross-section · firms 2018 (district FE)",
        "panels":     {"NEC_2018": {"label":"2018 cross-section", "years":[2018], "ref":None}},
        "lags":       LAGS_NCS,
        "shocks":     {k: SHOCKS[k] for k in SHOCKS},
        "controls":   CONTROLS_NCS,
        "groups":     list(NEC_CS_GROUPS.keys()),
        "outcomes":   outcomes_d,
        "panel_info": panel_info,
        "estimates":  estimates,
    }


# =============================================================================
# NEC entry-cohort panel — municipality × founding-year (2001-2018)
# =============================================================================
def compute_nec_panel():
    print("\n[NEC-Panel] Loading entry-cohort panel...")
    ep0 = pd.read_csv(ROOT / "data/clean/nec2018/entry_cohort_panel.csv")\
              .rename(columns={"founding_year_ad": "year"})

    def build_for_lag(lag):
        """Merge instrument at (lgcode, year-lag); each cohort row carries Z at founding year - lag."""
        inst_lag = inst.copy()
        inst_lag["year"] = inst_lag["year"] + lag
        df = ep0.merge(inst_lag[["lgcode","year","geog_intensity_2001"] + SSIV_COLS],
                       on=["lgcode","year"], how="inner")
        df = df.query("year >= 2001 and year <= 2018").copy()
        df[SSIV_COLS] = df[SSIV_COLS].fillna(0)
        df["geog_intensity_2001"] = df["geog_intensity_2001"].fillna(0)
        df["log_mi"]            = np.arcsinh(df["geog_intensity_2001"])
        df["ssiv_w99"]          = df["ssiv_index_2001"].clip(upper=df["ssiv_index_2001"].quantile(0.99))
        df["ssiv_asinh"]        = np.arcsinh(df["ssiv_index_2001"])

        # Khanna Eq. 4 — MI × D_t and Rshock × D_t use FULL period-FE vector.
        # NEC entry-cohort spans 2001-2018; ref = 2001 → 17 non-ref years.
        for y in range(2002, 2019):
            ind = (df["year"] == y).astype(int)
            df[f"mi_x_{y}"]         = df["log_mi"] * ind
            df[f"shareshock_x_{y}"] = df["shareshock_index_2001"] * ind
        # Post indicator for baseline X (Khanna Eq.4 X_o0 × Post_t)
        df["post"] = (df["year"] > 2001).astype(int)

        # Conditional-on-positive variants for entry-cohort counts: many
        # muni-year cells have 0 firms born in that year.
        POS_VARS = [
            "n_firms_surviving", "emp_surviving", "rev_surviving", "cap_surviving",
            "n_firms_surviving_size_micro_1",     "n_firms_surviving_size_small_2_9",
            "n_firms_surviving_size_medium_10_50","n_firms_surviving_size_large_51p",
        ]
        for v in POS_VARS:
            if v in df.columns:
                df[f"{v}_pos"] = df[v].where(df[v] > 0)

        # Block A region shares × Post; trade SSIV as muni-year covariate.
        if REGION_DF is not None:
            df = df.merge(REGION_DF, on="lgcode", how="left")
            for c in REGION_COLS:
                df[c] = df[c].fillna(df[c].mean())
                df[f"{c}_x_post"] = df[c] * df["post"]
        if TRADE_DF is not None:
            df = df.merge(TRADE_DF, on=["lgcode","year"], how="left")
            for c in TRADE_COLS: df[c] = df[c].fillna(0)
        if DEST_GDP_DF is not None:
            df = df.merge(DEST_GDP_DF, on="lgcode", how="left")
            df["dest_gdp_pc_2001"] = df["dest_gdp_pc_2001"].fillna(df["dest_gdp_pc_2001"].mean())
            df["dest_gdp_pc_2001_x_post"] = df["dest_gdp_pc_2001"] * df["post"]

        for k, col in SHOCK_RAW.items():
            sd = df[col].std()
            df[f"{k}_z"] = (df[col] - df[col].mean()) / sd if sd > 0 else 0.0
        return df

    P_GROUPS = {
        "Firm panel — total": [
            ("n_firms_surviving",          "# firms surviving (all cells)",      False),
            ("n_firms_surviving_pos",      "# firms surviving (cells > 0 only)", False),
            ("emp_surviving",              "Total emp (all cells)",              False),
            ("emp_surviving_pos",          "Total emp (cells > 0 only)",         False),
            ("rev_surviving",              "Total rev (all cells)",              False),
            ("rev_surviving_pos",          "Total rev (cells > 0 only)",         False),
            ("cap_surviving",              "Total capital (all cells)",          False),
            ("cap_surviving_pos",          "Total capital (cells > 0 only)",     False),
            ("median_firm_age_years",      "Median firm age (yrs)",              False),
        ],
        "By size": [
            ("n_firms_surviving_size_micro_1",         "# micro (1) — all cells",         False),
            ("n_firms_surviving_size_micro_1_pos",     "# micro (1) — cells > 0 only",    False),
            ("n_firms_surviving_size_small_2_9",       "# small (2-9) — all cells",       False),
            ("n_firms_surviving_size_small_2_9_pos",   "# small (2-9) — cells > 0 only",  False),
            ("n_firms_surviving_size_medium_10_50",    "# medium (10-50) — all cells",    False),
            ("n_firms_surviving_size_medium_10_50_pos","# medium (10-50) — cells > 0",    False),
            ("n_firms_surviving_size_large_51p",       "# large (51+) — all cells",       False),
            ("n_firms_surviving_size_large_51p_pos",   "# large (51+) — cells > 0",       False),
        ],
        "By sector": [
            ("n_firms_surviving_sec_manuf",       "Manufacturing",      False),
            ("n_firms_surviving_sec_construct",   "Construction",       False),
            ("n_firms_surviving_sec_wholesale",   "Wholesale & retail", False),
            ("n_firms_surviving_sec_hospitality", "Hospitality",        False),
            ("n_firms_surviving_sec_transport",   "Transport",          False),
            ("n_firms_surviving_sec_services",    "Services",           False),
            ("n_firms_surviving_sec_health",      "Health",             False),
            ("n_firms_surviving_sec_education",   "Education",          False),
            ("n_firms_surviving_sec_finance",     "Finance",            False),
            ("n_firms_surviving_sec_arts",        "Arts",               False),
        ],
        "By tradability": [
            ("n_firms_surviving_trd_tradable_goods",        "Tradable goods",         False),
            ("n_firms_surviving_trd_tradable_services",     "Tradable services",      False),
            ("n_firms_surviving_trd_non_tradable_services", "Non-tradable services",  False),
        ],
        "By modernity": [
            ("n_firms_surviving_modern_modern_services",        "Modern services",         False),
            ("n_firms_surviving_modern_modern_manuf",           "Modern manufacturing",    False),
            ("n_firms_surviving_modern_traditional_commerce",   "Traditional commerce",    False),
            ("n_firms_surviving_modern_traditional_services",   "Traditional services",    False),
            ("n_firms_surviving_modern_traditional_agriculture","Traditional agriculture", False),
        ],
    }

    def fit_panel(df, y, shock_z, controls, asinh):
        d = df[["lgcode","year",shock_z, y] + controls].dropna(subset=[y, shock_z]).copy()
        if d[y].nunique() < 2 or len(d) < 200: return None
        if asinh: d[y] = np.arcsinh(d[y])
        d_idx = d.set_index(["lgcode","year"])
        rhs = [shock_z] + [c for c in controls if c in d.columns]
        try:
            m = PanelOLS(d_idx[y], d_idx[rhs], entity_effects=True, time_effects=True,
                         drop_absorbed=True
                        ).fit(cov_type="clustered", cluster_entity=True)
        except Exception as e:
            return {"err": str(e)[:80]}
        if shock_z not in m.params.index: return {"err": "shock absorbed"}
        return {
            "beta": float(m.params[shock_z]),
            "se":   float(m.std_errors[shock_z]),
            "pval": float(m.pvalues[shock_z]),
            "n":    int(m.nobs),
            "n_unit": d_idx.index.get_level_values("lgcode").nunique(),
            "mean_y": float(d[y].mean()),
            "r2_within": float(m.rsquared_within),
        }

    PANELS_NP   = {"NEC_EC_2001_2018": {"label":"2001-2018 founding cohorts (ref-2001 trend)","years":list(range(2001,2019)),"ref":2001}}
    LAGS_NP     = {
        "lag0": "Contemporaneous (Z_t)",
        "lag1": "Lagged 1 year (Z_{t-1})",
        "lag2": "Lagged 2 years (Z_{t-2})",
    }
    CONTROLS_NP = {
        "none": "No controls",
        "mi":   "+ MI × D_t (basic Khanna baseline)",
    }
    if REGION_DF is not None and TRADE_DF is not None:
        CONTROLS_NP["khanna_full"] = "+ MI + ShareShock × D_t + region × Post + dest GDP × Post + trade SSIV"

    NP_YRS_NONREF = list(range(2002, 2019))  # ref = 2001

    def ctrl_cols_np(tag):
        cols = []
        if tag == "none":
            return cols
        if tag in ("mi", "khanna_full"):
            cols += [f"mi_x_{y}" for y in NP_YRS_NONREF]
        if tag == "khanna_full":
            cols += [f"shareshock_x_{y}" for y in NP_YRS_NONREF]
            cols += [f"{c}_x_post" for c in REGION_COLS]
            if DEST_GDP_DF is not None:
                cols.append("dest_gdp_pc_2001_x_post")
            cols += list(TRADE_COLS)
        return cols

    outcomes_d = {y: {"label": lab, "group": g}
                  for g, items in P_GROUPS.items() for y, lab, _ in items}

    LAG_VALS = {"lag0": 0, "lag1": 1, "lag2": 2}
    estimates  = {"NEC_EC_2001_2018": {}}
    panel_info = {"NEC_EC_2001_2018": {}}
    for lag_key, lag in LAG_VALS.items():
        df = build_for_lag(lag)
        panel_info["NEC_EC_2001_2018"][lag_key] = {
            "n_obs":  int(len(df)),
            "n_unit": int(df["lgcode"].nunique()),
            "years":  sorted([int(y) for y in df["year"].unique()]),
        }
        estimates["NEC_EC_2001_2018"][lag_key] = {}
        n_cells = 0
        for shock_key in SHOCKS:
            shock_z = f"{shock_key}_z"
            if df[shock_z].std() < 1e-8: continue
            estimates["NEC_EC_2001_2018"][lag_key][shock_key] = {}
            for ctrl_key in CONTROLS_NP:
                cc = ctrl_cols_np(ctrl_key)
                estimates["NEC_EC_2001_2018"][lag_key][shock_key][ctrl_key] = {}
                for gname, items in P_GROUPS.items():
                    for y, lab, asinh_flag in items:
                        if y not in df.columns: continue
                        r = fit_panel(df, y, shock_z, cc, asinh_flag)
                        if r is None: continue
                        estimates["NEC_EC_2001_2018"][lag_key][shock_key][ctrl_key][y] = r
                        n_cells += 1
        print(f"  [NEC-Panel] {lag_key}: {n_cells} cells")

    return {
        "label":      "NEC firm panel · muni × founding-year (2001-2018)",
        "panels":     PANELS_NP,
        "lags":       LAGS_NP,
        "shocks":     {k: SHOCKS[k] for k in SHOCKS},
        "controls":   CONTROLS_NP,
        "groups":     list(P_GROUPS.keys()),
        "outcomes":   outcomes_d,
        "panel_info": panel_info,
        "estimates":  estimates,
    }


final["datasets"]["rvs"]       = compute_rvs()
final["datasets"]["nec_cs"]    = compute_nec_cs()
final["datasets"]["nec_panel"] = compute_nec_panel()

with open(str(ROOT / "docs/results.json"),"w") as f:
    json.dump(final, f, separators=(",",":"))

n_total = 0
for ds_key, ds in final["datasets"].items():
    n_ds = 0
    for p in ds["estimates"].values():
        for l in p.values():
            for s in l.values():
                for c in s.values():
                    n_ds += len(c)
    n_total += n_ds
    print(f"  {ds_key}: {n_ds} cells, {len(ds['outcomes'])} outcomes")
print(f"\nTotal estimates: {n_total}")
print(f"File: {ROOT / 'docs/results.json'}")
