"""
Build docs/results.json — three datasets (census, hh, plot), three specs
(S1/S2/S3) with interaction-based treatment, four migration thresholds.

Mirrors script/estimate/{estimate_census, estimate_hh, estimate_plot}.R.

Specs (coefficient of interest in [brackets]):
  S1: y ~ [fx_z]              + i(year, mig_int_z, ref)     | FE
      (average FX shock effect, with linear year×migint trend)
  S2: y ~ [fx_z:log_migint_z] + i(year, mig_int_z, ref)     | FE   ← MAIN
      (heterogeneous slope by log migration intensity)
  S3: y ~ [fx_z:mig_int_z]    + i(year, log_migint_z, ref)  | FE
      (heterogeneous slope by linear migration intensity)

Datasets:
  census : lgcode × year (2001/2011/2021), FE = lgcode + year, ref = 2001
  hh     : hhid   × year (2016/2017/2018), FE = hhid   + year, ref = 2016
  plot   : plotid × year (2016/2017/2018), FE = hhid   + year, ref = 2016

Cluster: lgcode (level of treatment variation) for all three.
Reported coefficient: the interaction term (fx_z:mig_int_z or fx_z:log_migint_z).
Threshold drops munis with total_migrants < threshold (0 = no filter).
"""
import sys, json, os
import pandas as pd
import numpy as np
from pathlib import Path
from linearmodels import PanelOLS
import warnings
warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

VALIDATE_ONLY = os.environ.get("FULL", "0") != "1"

# =============================================================================
# 1.  Load instrument with column-name resilience
# =============================================================================
inst_raw = pd.read_csv(ROOT / "data/clean/instrument/instrument_mun.csv")

# Resolve canonical names (try intuitive → user's local R-style → technical)
COL_ALIAS = {
    "fxshock":        ["fxshock",        "avg_fx_shock_2001",       "shareshock_index_2001"],
    "mig_intensity":  ["mig_intensity",  "migrants_per_capita_2001","geog_intensity_2001"],
    "total_migrants": ["total_migrants", "total_migrants_2001",     "geog_total_mig_2001"],
}
inst = pd.DataFrame({"lgcode": inst_raw["lgcode"], "year": inst_raw["year"]})
for canon, candidates in COL_ALIAS.items():
    found = next((c for c in candidates if c in inst_raw.columns), None)
    if found is None:
        raise SystemExit(f"instrument file is missing one of {candidates}")
    inst[canon] = inst_raw[found]
    if canon != found:
        print(f"  Using '{found}' as '{canon}'")

inst["log_mig_intensity"] = np.log(inst["mig_intensity"] + 1e-8)


def _std(s):
    sd = s.std(ddof=1)
    return (s - s.mean()) / sd if sd > 0 else s * 0.0


# Z-score on the FULL instrument panel (matches estimate_census.R initial scale)
inst["fx_z"]         = _std(inst["fxshock"])
inst["mig_int_z"]    = _std(inst["mig_intensity"])
inst["log_migint_z"] = _std(inst["log_mig_intensity"])

print(f"Instrument: {len(inst):,} rows · {inst['lgcode'].nunique():,} munis · "
      f"years {inst['year'].min()}–{inst['year'].max()}")
print(f"  fx_z:         mean={inst['fx_z'].mean():+.4f}  sd={inst['fx_z'].std():.4f}")
print(f"  mig_int_z:    mean={inst['mig_int_z'].mean():+.4f}  sd={inst['mig_int_z'].std():.4f}")
print(f"  log_migint_z: mean={inst['log_migint_z'].mean():+.4f}  sd={inst['log_migint_z'].std():.4f}")

INST_KEEP = ["lgcode","year","total_migrants",
             "fxshock","mig_intensity","log_mig_intensity",
             "fx_z","mig_int_z","log_migint_z"]


# =============================================================================
# 2.  Outcome groups
# =============================================================================
CENSUS_GROUPS = {
    "Amenities": [
        "amen_water_piped","amen_water_traditional",
        "amen_cooking_wood","amen_cooking_kerosene","amen_cooking_lpg",
        "amen_cooking_biogas","amen_cooking_electric",
        "amen_cooking_modern","amen_cooking_traditional",
        "amen_lighting_electricity","amen_lighting_kerosene",
        "amen_lighting_biogas","amen_lighting_others",
        "amen_toilet_modern","amen_toilet_ordinary",
        "amen_toilet_none","amen_toilet_any",
    ],
    "Assets": [
        "amen_assets_radio","amen_assets_tv","amen_assets_landline",
        "amen_assets_mobile","amen_assets_computer","amen_assets_internet",
        "amen_assets_cycle","amen_assets_motorcycle","amen_assets_car",
        "amen_assets_fridge","amen_assets_none","amen_asset_count_mean",
    ],
    "Education": [
        "edu_literate","edu_literate_female","edu_literate_male",
        "edu_school_attend_6_16","edu_school_attend_6_16_female",
        "edu_school_attend_6_16_male",
        "edu_attain_primary_plus","edu_attain_secondary_plus",
        "edu_attain_higher_secondary_plus","edu_attain_tertiary",
        "edu_years_mean",
    ],
    "Housing": [
        "housing_own","housing_rented",
        "housing_foundation_modern","housing_foundation_traditional",
        "housing_roof_modern","housing_roof_traditional",
    ],
    "Female Ownership": [
        "fem_ownership_house","fem_ownership_land",
        "fem_ownership_both","fem_ownership_livestock",
    ],
    "Enterprise": [
        "ent_has_nonagro","ent_cottage","ent_trade","ent_transport",
        "ent_services","ent_other","ent_female_owner_share",
    ],
    "Absent HH": ["absent_hh_share","hh_death_12m"],
    "Marriage": [
        "mar_ever_married_15_60","mar_never_married_15_60",
        "mar_female_age_first_mean",
        "mar_female_married_by_18","mar_female_married_by_20",
    ],
    "Fertility": [
        "fert_birth_mean","fert_birth_son_mean","fert_birth_dau_mean",
        "fert_births_last12m_share","fert_births_last12m_rate",
    ],
    "Child Mortality": [
        "mort_children_dead_mean","mort_child_dead_any","mort_child_death_ratio",
    ],
    "Work Activity": [
        "work_share_agriculture","work_share_nonagriculture",
        "work_share_wage_nonagri","work_share_own_nonagri",
        "work_share_extended_econ","work_share_job_seeking",
        "work_share_household_chores","work_share_student",
        "work_share_no_work","work_lfp",
    ],
    "Occupation": [
        "occ_share_armed_forces","occ_share_managers",
        "occ_share_professionals","occ_share_technicians",
        "occ_share_office_assistants","occ_share_service_sales",
        "occ_share_agriculture","occ_share_craft_trades",
        "occ_share_machine_operators","occ_share_elementary",
    ],
    "Industry": [
        "ind_agri_forestry_fish","ind_manufacturing","ind_construction",
        "ind_wholesale_retail","ind_transport_accommodation",
        "ind_finance_real_estate_prof","ind_public_admin_defence",
        "ind_education","ind_health","ind_arts_recreation","ind_others",
    ],
    "Employment Status": [
        "emp_share_employer","emp_share_employee",
        "emp_share_self_employed","emp_share_unpaid_family_worker",
    ],
    "In-migration": [
        "mig_in_share","mig_in_domestic","mig_in_international",
        "mig_in_from_rural","mig_in_from_urban",
        "mig_in_reason_economic","mig_in_reason_noneconomic",
        "mig_in_reason_study","mig_in_reason_marriage","mig_in_return",
    ],
    "Female Labor": [
        "flfp_all","fem_employment_rate","flfp_agri","flfp_nonagri",
        "flfp_wage","flfp_chores_only",
    ],
    "Male Labor": ["mlfp_all","mlfp_agri","mlfp_nonagri"],
    "Gender Gaps": [
        "share_women","share_men","fem_share_of_ag_workers",
        "fem_ag_specialization_ratio","fem_wage_share_of_employment",
        "gap_lfp_m_minus_f","gap_nonagri_m_minus_f",
    ],
    "HH Head": [
        "head_female_share","head_age_mean","head_elderly_share",
        "head_female_elderly","head_young_share",
    ],
    "Left-behind children": [
        "left_not_with_both","left_mother_only","left_father_only",
        "left_with_relatives","left_without_parents",
    ],
}

HH_GROUPS = {
    "Land Portfolio": [
        "agro_hh","n_plots_owned","total_owned_area_sqm",
        "cultivated_area_sqm","cultivated_area_total_sqm","rented_in_area_sqm",
    ],
    "Land Use (HH)": [
        "share_self_wet","share_rented_out_wet","share_fallow_wet",
        "share_self_dry","share_fallow_dry","share_both_seasons",
    ],
    "Crop Choice": [
        "n_crops_total","n_crops_wet","n_crops_dry","multi_season",
        "grows_staple","grows_cashcrop","grows_horticulture",
    ],
}

PLOT_GROUPS = {
    "Land Use (Wet)": [
        "wet_self_cultivated","wet_rented_out","wet_fallow","wet_other_use",
    ],
    "Irrigation (Wet)": [
        "wet_irr_surface","wet_irr_groundwater","wet_irr_rainfed",
    ],
}


# =============================================================================
# 3.  Estimation helpers
# =============================================================================
def _build_year_interactions(d, treat_col, ref_year):
    """For years actually present in d (excluding ref_year), create
    treat_col × 1{year==y} interaction columns. Mutates d, returns column list."""
    out_cols = []
    for y in sorted(d["year"].unique()):
        if y == ref_year:
            continue
        col = f"{treat_col}_x_{int(y)}"
        d[col] = d[treat_col] * (d["year"] == y).astype(float)
        out_cols.append(col)
    return out_cols


def fit_one(df, y, spec, entity_col, year_col, ref_year, cluster_col):
    """One (outcome, spec) cell.

    Specs:
      S1: y = fx_z + [fx_z·mig_int_z]                                | FE
      S2: y = fx_z + [fx_z·log_migint_z] + i(year, mig_int_z, ref)   | FE
      S3: y = fx_z + [fx_z·mig_int_z]    + i(year, log_migint_z, ref)| FE

    The bracketed term is the reported coefficient.
    """
    needed = [entity_col, year_col, y, cluster_col,
              "fx_z", "mig_int_z", "log_migint_z"]
    seen = set(); ucols = []
    for c in needed:
        if c not in seen and c in df.columns:
            seen.add(c); ucols.append(c)
    d = df[ucols].dropna(subset=[y, "fx_z"]).copy()
    if d[y].nunique() < 2 or len(d) < 50: return None
    if d[y].std(ddof=1) == 0: return None

    # Build the spec — exactly as in estimate_census.R:
    #   S1: y ~ fx_z + i(year, mig_int_z, ref) | FE          → report fx_z
    #   S2: y ~ fx_z:log_migint_z + i(year, mig_int_z, ref)  → report interaction
    #   S3: y ~ fx_z:mig_int_z + i(year, log_migint_z, ref)  → report interaction
    if spec == "S1":
        rhs = ["fx_z"]
        report_var = "fx_z"
        ctrl_cols  = _build_year_interactions(d, "mig_int_z", ref_year)
    elif spec == "S2":
        d["fx_x_logmig"] = d["fx_z"] * d["log_migint_z"]
        rhs = ["fx_x_logmig"]
        report_var = "fx_x_logmig"
        ctrl_cols  = _build_year_interactions(d, "mig_int_z", ref_year)
    elif spec == "S3":
        d["fx_x_mig"]    = d["fx_z"] * d["mig_int_z"]
        rhs = ["fx_x_mig"]
        report_var = "fx_x_mig"
        ctrl_cols  = _build_year_interactions(d, "log_migint_z", ref_year)
    else:
        return {"err": f"unknown spec {spec}"}

    rhs += ctrl_cols

    d_idx = d.set_index([entity_col, year_col])
    cluster_series = pd.Series(d[cluster_col].values, index=d_idx.index, name="_cluster")
    try:
        m = PanelOLS(
            d_idx[y], d_idx[rhs],
            entity_effects=True, time_effects=True,
            drop_absorbed=True,
        ).fit(cov_type="clustered", clusters=cluster_series)
    except Exception as e:
        return {"err": str(e)[:80]}

    if report_var not in m.params.index:
        return {"err": f"{report_var} absorbed"}

    return {
        "beta":      float(m.params[report_var]),
        "se":        float(m.std_errors[report_var]),
        "pval":      float(m.pvalues[report_var]),
        "n":         int(m.nobs),
        "n_unit":    int(d_idx.index.get_level_values(entity_col).nunique()),
        "n_muni":    int(d[cluster_col].nunique()),
        "mean_y":    float(d[y].mean()),
        "sd_y":      float(d[y].std(ddof=1)),
        "r2_within": float(m.rsquared_within),
    }


# =============================================================================
# 4.  Per-dataset compute
# =============================================================================
def compute_dataset(name, source_df, groups, entity_col, year_col, ref_year,
                    cluster_col="lgcode"):
    print(f"\n[{name}] {entity_col} FE, ref={ref_year}, cluster {cluster_col}")
    panel = source_df.merge(inst[INST_KEEP], on=["lgcode","year"], how="inner")
    print(f"  panel: {len(panel):,} rows · {panel[entity_col].nunique():,} {entity_col}s · "
          f"{panel['lgcode'].nunique():,} munis · years {sorted(panel[year_col].unique().tolist())}")

    SPECS = ["S1","S2","S3"]
    THRESHOLDS = [0, 25, 50, 100]
    estimates = {}

    for thr in THRESHOLDS:
        sub = panel if thr == 0 else panel[panel["total_migrants"] >= thr]
        if len(sub) < 50:
            print(f"  thr={thr}: too few obs ({len(sub)}) — skipped"); continue
        n_units_thr = sub[entity_col].nunique()
        n_muni_thr  = sub["lgcode"].nunique()

        # Re-z-score treatments at MUNI-YEAR level on working sample
        muni_yr = sub[["lgcode","year","fxshock","mig_intensity","log_mig_intensity"]].drop_duplicates()
        muni_yr["fx_z"]         = _std(muni_yr["fxshock"])
        muni_yr["mig_int_z"]    = _std(muni_yr["mig_intensity"])
        muni_yr["log_migint_z"] = _std(muni_yr["log_mig_intensity"])
        sub = sub.drop(columns=["fx_z","mig_int_z","log_migint_z"], errors="ignore")
        sub = sub.merge(muni_yr[["lgcode","year","fx_z","mig_int_z","log_migint_z"]],
                        on=["lgcode","year"], how="left")

        print(f"  thr={thr}: {len(sub):,} obs · {n_units_thr:,} {entity_col}s · {n_muni_thr:,} munis")

        estimates[str(thr)] = {}
        for spec in SPECS:
            estimates[str(thr)][spec] = {}
            n_cells = 0; n_err = 0
            for gname, ys in groups.items():
                for y in ys:
                    if y not in sub.columns: continue
                    r = fit_one(sub, y, spec,
                                entity_col=entity_col, year_col=year_col,
                                ref_year=ref_year, cluster_col=cluster_col)
                    if r is None: continue
                    estimates[str(thr)][spec][y] = r
                    if "err" in r: n_err += 1
                    else:          n_cells += 1
            print(f"    {spec}: {n_cells} cells, {n_err} errors")

    outcomes_d = {y: {"label": y, "group": g}
                  for g, ys in groups.items() for y in ys}

    return {
        "label":     name,
        "entity":    entity_col,
        "year":      year_col,
        "ref_year":  ref_year,
        "cluster":   cluster_col,
        "groups":    list(groups.keys()),
        "outcomes":  outcomes_d,
        "estimates": estimates,
    }


# =============================================================================
# 5.  Run datasets
# =============================================================================
def main():
    final = {
        "datasets_meta": {
            "census": {"label": "Census panel — Population & Housing"},
            "hh":     {"label": "HRVS HH × year (agriculture)"},
            "plot":   {"label": "HRVS plot × year (agriculture)"},
        },
        "specs": {
            "S1": "fx + year × mig_intensity   (average FX effect)",
            "S2": "fx × log(mig_intensity) + year × mig_intensity   [main]",
            "S3": "fx × mig_intensity + year × log(mig_intensity)",
        },
        "thresholds": {
            "0":   "All munis",
            "25":  "≥25 migrants in 2001",
            "50":  "≥50 migrants in 2001",
            "100": "≥100 migrants in 2001",
        },
        "datasets": {},
    }

    # --- Census ---
    cen = pd.read_csv(ROOT / "data/clean/census/census_outcomes_municipality.csv")
    final["datasets"]["census"] = compute_dataset(
        "census", cen, CENSUS_GROUPS,
        entity_col="lgcode", year_col="year", ref_year=2001)

    # --- HH ---
    hh = pd.read_csv(ROOT / "data/clean/rvs_outcomes/agriculture_hh_year.csv")
    hh = hh.rename(columns={"vmun_code": "lgcode"})
    final["datasets"]["hh"] = compute_dataset(
        "hh", hh, HH_GROUPS,
        entity_col="hhid", year_col="year", ref_year=2016)

    # --- Plot (only if FULL=1) ---
    if not VALIDATE_ONLY:
        plot = pd.read_csv(ROOT / "data/clean/rvs_outcomes/agriculture_plot_year.csv")
        plot = plot.rename(columns={"vmun_code": "lgcode"})
        final["datasets"]["plot"] = compute_dataset(
            "plot", plot, PLOT_GROUPS,
            entity_col="hhid", year_col="year", ref_year=2016)

    n_total = 0
    for ds in final["datasets"].values():
        for thr in ds["estimates"].values():
            for spec in thr.values():
                n_total += sum(1 for c in spec.values() if "err" not in c)
    print(f"\nTotal estimate cells: {n_total:,}")

    out = ROOT / "docs/results.json"
    out.write_text(json.dumps(final, separators=(",",":")))
    print(f"Wrote {out}")


if __name__ == "__main__":
    main()
