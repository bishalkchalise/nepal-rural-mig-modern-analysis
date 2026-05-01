"""
Build docs/results.json — three datasets (census, hh, plot), three specs
(S1/S2/S3), four migration thresholds.

Mirrors the R scripts in script/estimate/{estimate_census, estimate_hh,
estimate_plot}.R.

Specs (with z-scored treatments and FE absorbed):
  S1: y ~ fxshock_z + i(year, log_mig_intensity_z, ref) | FE
  S2: y ~ fxshock_z + i(year,     mig_intensity_z, ref) | FE   ← MAIN
  S3: y ~ fxshock_x_mig_intensity_z + fxshock_z + both heterogeneity terms | FE

Datasets:
  census : lgcode × year (2001/2011/2021), FE = lgcode + year
  hh     : hhid   × year (2016/2017/2018), FE = hhid   + year
  plot   : plotid × year (2016/2017/2018), FE = hhid   + year   (no plot FE)

Cluster: lgcode (level of treatment variation) for all three.
Reported coefficient: fxshock_z (S1, S2) or fxshock_x_mig_intensity_z (S3).
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

# =============================================================================
# 1.  Validation subset (toggle to expand later)
# =============================================================================
VALIDATE_ONLY = os.environ.get("FULL", "0") != "1"

# =============================================================================
# 2.  Load instrument + standardise treatments at muni-year level
# =============================================================================
inst = pd.read_csv(ROOT / "data/clean/instrument/instrument_mun.csv")

def _std(s):
    sd = s.std(ddof=1)
    return (s - s.mean()) / sd if sd > 0 else s * 0.0

# Compute log_mig_intensity carefully (handle zeros)
inst["log_mig_intensity"] = np.log(inst["mig_intensity"] + 1e-8)

# Z-score the four treatments at the (lgcode, year) muni level on the FULL
# instrument panel.  Datasets get re-merged.
TREAT_RAW = {
    "fxshock":                 "fxshock_z",
    "mig_intensity":           "mig_intensity_z",
    "log_mig_intensity":       "log_mig_intensity_z",
    "fxshock_x_mig_intensity": "fxshock_x_mig_intensity_z",
}
for raw, z in TREAT_RAW.items():
    inst[z] = _std(inst[raw])

print(f"Instrument loaded: {len(inst)} muni-year obs · "
      f"{inst['lgcode'].nunique()} munis · "
      f"years {sorted(inst['year'].unique().tolist())[0]}-{sorted(inst['year'].unique().tolist())[-1]}")
print(f"  fxshock_z              : mean={inst['fxshock_z'].mean():+.4f} sd={inst['fxshock_z'].std():.4f}")
print(f"  mig_intensity_z        : mean={inst['mig_intensity_z'].mean():+.4f} sd={inst['mig_intensity_z'].std():.4f}")
print(f"  log_mig_intensity_z    : mean={inst['log_mig_intensity_z'].mean():+.4f} sd={inst['log_mig_intensity_z'].std():.4f}")
print(f"  fxshock_x_mig_intensity_z: mean={inst['fxshock_x_mig_intensity_z'].mean():+.4f} sd={inst['fxshock_x_mig_intensity_z'].std():.4f}")

# muni-year columns we carry along (raw + z, plus threshold filter input)
INST_KEEP = ["lgcode","year","total_migrants",
             "fxshock","mig_intensity","log_mig_intensity","fxshock_x_mig_intensity",
             "fxshock_z","mig_intensity_z","log_mig_intensity_z","fxshock_x_mig_intensity_z"]


# =============================================================================
# 3.  Outcome groups
# =============================================================================
CENSUS_GROUPS = {
    "Assets": [
        "amen_assets_radio","amen_assets_tv","amen_assets_cycle",
        "amen_assets_motorcycle","amen_assets_car","amen_assets_fridge",
        "amen_assets_landline","amen_assets_mobile","amen_assets_computer",
        "amen_assets_internet","amen_assets_none","amen_asset_count_mean",
    ],
    "Industry": [
        "ind_agri_forestry_fish","ind_manufacturing","ind_construction",
        "ind_wholesale_retail","ind_transport_accommodation",
        "ind_finance_real_estate_prof","ind_public_admin_defence",
        "ind_education","ind_health","ind_arts_recreation","ind_others",
    ],
    "Occupation": [
        "occ_share_armed_forces","occ_share_managers","occ_share_professionals",
        "occ_share_technicians","occ_share_office_assistants",
        "occ_share_service_sales","occ_share_agriculture",
        "occ_share_craft_trades","occ_share_machine_operators",
        "occ_share_elementary",
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
# 4.  Estimation helpers
# =============================================================================
def _build_year_interactions(d, treat_col, ref_year):
    """For years actually present in d (excluding ref_year), create
    treat_col × 1{year==y} interaction columns. Mutates d, returns list of
    new column names."""
    out_cols = []
    years_present = sorted(d["year"].unique())
    for y in years_present:
        if y == ref_year:
            continue
        col = f"{treat_col}_x_{int(y)}"
        d[col] = d[treat_col] * (d["year"] == y).astype(float)
        out_cols.append(col)
    return out_cols


def fit_one(df, y, spec, entity_col, year_col, ref_year, cluster_col):
    cols = [entity_col, year_col, y, cluster_col,
            "fxshock_z", "fxshock_x_mig_intensity_z",
            "mig_intensity_z", "log_mig_intensity_z"]
    seen = set(); ucols = []
    for c in cols:
        if c not in seen and c in df.columns:
            seen.add(c); ucols.append(c)
    d = df[ucols].dropna(subset=[y, "fxshock_z"]).copy()
    if d[y].nunique() < 2 or len(d) < 50:
        return None
    if d[y].std(ddof=1) == 0:
        return None

    # Build interactions on the surviving panel (years actually present after dropna).
    if spec in ("S1", "S3"):
        log_int_cols = _build_year_interactions(d, "log_mig_intensity_z", ref_year)
    else:
        log_int_cols = []
    if spec in ("S2", "S3"):
        lin_int_cols = _build_year_interactions(d, "mig_intensity_z", ref_year)
    else:
        lin_int_cols = []

    if spec == "S3":
        rhs = ["fxshock_x_mig_intensity_z", "fxshock_z"] + log_int_cols + lin_int_cols
        report_var = "fxshock_x_mig_intensity_z"
    else:
        rhs = ["fxshock_z"] + log_int_cols + lin_int_cols
        report_var = "fxshock_z"

    d_idx = d.set_index([entity_col, year_col])
    cluster_series = pd.Series(d[cluster_col].values, index=d_idx.index, name="_cluster")
    try:
        m = PanelOLS(
            d_idx[y], d_idx[rhs],
            entity_effects=True, time_effects=True,
            drop_absorbed=True, check_rank=False,
        ).fit(cov_type="clustered", clusters=cluster_series)
    except Exception as e:
        return {"err": str(e)[:80]}

    if report_var not in m.params.index:
        return {"err": f"{report_var} absorbed"}

    return {
        "beta":    float(m.params[report_var]),
        "se":      float(m.std_errors[report_var]),
        "pval":    float(m.pvalues[report_var]),
        "n":       int(m.nobs),
        "n_unit":  int(d_idx.index.get_level_values(entity_col).nunique()),
        "n_muni":  int(d[cluster_col].nunique()),
        "mean_y":  float(d[y].mean()),
        "sd_y":    float(d[y].std(ddof=1)),
        "r2_within": float(m.rsquared_within),
    }


# =============================================================================
# 5.  Per-dataset compute
# =============================================================================
def compute_dataset(name, source_df, groups, entity_col, year_col, ref_year,
                    cluster_col="lgcode"):
    print(f"\n[{name}] Computing — {entity_col} FE, ref={ref_year}, "
          f"cluster {cluster_col}")
    panel = source_df.merge(inst[INST_KEEP], on=["lgcode","year"], how="inner")
    print(f"  After instrument merge: {len(panel):,} rows · "
          f"{panel[entity_col].nunique():,} {entity_col}s · "
          f"{panel['lgcode'].nunique():,} munis · "
          f"years {sorted(panel[year_col].unique().tolist())}")

    SPECS = ["S1","S2","S3"]
    THRESHOLDS = [0, 25, 50, 100]

    estimates = {}      # threshold → spec → outcome → cell
    for thr in THRESHOLDS:
        sub = panel if thr == 0 else panel[panel["total_migrants"] >= thr]
        if len(sub) < 50:
            print(f"  thr={thr}: too few obs ({len(sub)})"); continue
        n_units_thr = sub[entity_col].nunique()
        n_muni_thr  = sub["lgcode"].nunique()
        print(f"  thr={thr}: {len(sub):,} obs · {n_units_thr:,} {entity_col}s · {n_muni_thr:,} munis")

        # Re-z-score treatments on the working sample, at the muni-year level
        muni_yr = sub[["lgcode","year","fxshock","mig_intensity",
                       "log_mig_intensity","fxshock_x_mig_intensity"]].drop_duplicates()
        for raw, z in TREAT_RAW.items():
            muni_yr[z] = _std(muni_yr[raw])
        sub = sub.drop(columns=[c for c in TREAT_RAW.values() if c in sub.columns],
                       errors="ignore")
        sub = sub.merge(muni_yr[["lgcode","year"] + list(TREAT_RAW.values())],
                        on=["lgcode","year"], how="left")

        estimates[str(thr)] = {}
        for spec in SPECS:
            estimates[str(thr)][spec] = {}
            n_cells = 0
            for gname, ys in groups.items():
                for y in ys:
                    if y not in sub.columns: continue
                    r = fit_one(sub, y, spec,
                                entity_col=entity_col, year_col=year_col,
                                ref_year=ref_year, cluster_col=cluster_col)
                    if r is None: continue
                    estimates[str(thr)][spec][y] = r
                    if "err" not in r: n_cells += 1
            print(f"    {spec}: {n_cells} cells")

    outcomes_d = {y: {"label": y, "group": g}
                  for g, ys in groups.items() for y in ys}

    return {
        "label":      name,
        "entity":     entity_col,
        "year":       year_col,
        "ref_year":   ref_year,
        "cluster":    cluster_col,
        "groups":     list(groups.keys()),
        "outcomes":   outcomes_d,
        "estimates":  estimates,
    }


# =============================================================================
# 6.  Run datasets
# =============================================================================
def main():
    final = {
        "datasets_meta": {
            "census": {"label": "Census panel — Population & Housing"},
            "hh":     {"label": "HRVS HH × year (agriculture)"},
            "plot":   {"label": "HRVS plot × year (agriculture)"},
        },
        "specs": {
            "S1": "fxshock + i(year, log mig-intensity, ref)",
            "S2": "fxshock + i(year,     mig-intensity, ref)   [main]",
            "S3": "SSIV + fxshock + both year×mig-intensity heterogeneity",
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

    # --- Plot (only if not validation-only mode) ---
    if not VALIDATE_ONLY:
        plot = pd.read_csv(ROOT / "data/clean/rvs_outcomes/agriculture_plot_year.csv")
        plot = plot.rename(columns={"vmun_code": "lgcode"})
        # Make derived plot outcomes (mirrors estimate_plot.R prepare_plot_outcomes)
        for c in ["wet_self_cultivated","wet_rented_out","wet_fallow",
                  "wet_other_use","wet_irr_surface","wet_irr_groundwater",
                  "wet_irr_rainfed"]:
            if c not in plot.columns: continue
        final["datasets"]["plot"] = compute_dataset(
            "plot", plot, PLOT_GROUPS,
            entity_col="hhid", year_col="year", ref_year=2016)

    # Total cell count
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
