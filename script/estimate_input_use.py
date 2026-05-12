"""
estimate input-use outcomes at the preferred anchor spec
(log_int, c_mig=True, c_fx=True, Block A only)  at four thresholds.

mirrors the fit_panel logic in script/build_results.py for HH-panel outcomes.
"""
import sys, json
import pandas as pd, numpy as np
from pathlib import Path
from linearmodels import PanelOLS
import warnings
warnings.filterwarnings("ignore")

ROOT = Path(".")
sys.path.insert(0, str(ROOT))

# --- 1. Instrument
inst_raw = pd.read_csv(ROOT / "data/clean/instrument/instrument_mun.csv")
COL_ALIAS = {
    "fxshock":        ["fxshock","avg_fx_shock_2001","shareshock_index_2001"],
    "mig_intensity":  ["mig_intensity","migrants_per_capita_2001","geog_intensity_2001"],
    "total_migrants": ["total_migrants","total_migrants_2001","geog_total_mig_2001"],
}
inst = pd.DataFrame({"lgcode": inst_raw["lgcode"], "year": inst_raw["year"]})
for canon, cands in COL_ALIAS.items():
    found = next((c for c in cands if c in inst_raw.columns), None)
    inst[canon] = inst_raw[found]
inst["log_mig_intensity"] = np.log(inst["mig_intensity"] + 1e-8)

def _std(s):
    sd = s.std(ddof=1)
    return (s - s.mean()) / sd if sd > 0 else s * 0.0

# --- 2. Block A (destination-weighted baseline X)
def build_baseline_X():
    region_p = ROOT / "data/clean/instrument/dest_region_shares_2001.csv"
    wdi_p    = ROOT / "data/clean/instrument/wdi_dest_gdp_2001.csv"
    share_p  = ROOT / "data/clean/instrument/dest_mun_mig_share_2001.csv"
    wdi = pd.read_csv(wdi_p)[["country","gdp_pc_2001"]].dropna()
    share = pd.read_csv(share_p)
    dest_gdp = (share.merge(wdi, on="country", how="inner")
                .assign(prod=lambda d: d["mun_mig_share_2001"] * d["gdp_pc_2001"])
                .groupby("lgcode", as_index=False)
                .agg(dest_gdp_pc_2001=("prod","sum"), coverage=("mun_mig_share_2001","sum")))
    dest_gdp["dest_gdp_pc_2001"] = (dest_gdp["dest_gdp_pc_2001"] /
                                    dest_gdp["coverage"].where(dest_gdp["coverage"]>0,1))
    dest_gdp = dest_gdp[["lgcode","dest_gdp_pc_2001"]]
    region = pd.read_csv(region_p)
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

# --- 3. HH master (mirror load_hh_master in build_results.py)
base = ROOT / "data/clean/rvs_outcomes"
agri = pd.read_csv(base / "agriculture_hh_year.csv").rename(columns={"vmun_code":"lgcode"})
skip = {"hhid","year","wt_hh","psu","vdc","vmun_code","lgname",
        "district77","district_name","s00q03a","s00q03b","s00q03c"}
keep = ["hhid","year","lgcode"] + [c for c in agri.columns if c not in skip and c != "lgcode"]
master = agri[keep].copy()

# Derived log columns for cost outcomes (handle long tail)
for col in ["total_input_cost_rs","wet_cost_seed","dry_cost_seed",
            "wet_cost_fert","dry_cost_fert","wet_cost_labour","dry_cost_labour"]:
    if col in master.columns:
        master[f"log_{col}"] = np.log1p(master[col].clip(lower=0))

# --- 4. fit one outcome at the anchor spec
def fit_one(df, y, thr, ref_year=2016, entity_col="hhid", cluster_col="lgcode"):
    sub = df.copy()
    sub = sub.merge(inst[["lgcode","year","fxshock","mig_intensity","log_mig_intensity","total_migrants"]],
                    on=["lgcode","year"], how="inner")
    if thr > 0:
        sub = sub[sub["total_migrants"] >= thr]
    if len(sub) < 50:
        return None
    # z-score on muni-year working sample
    muni_yr = sub[["lgcode","year","fxshock","mig_intensity","log_mig_intensity"]].drop_duplicates()
    muni_yr["fx_z"]         = _std(muni_yr["fxshock"])
    muni_yr["mig_int_z"]    = _std(muni_yr["mig_intensity"])
    muni_yr["log_migint_z"] = _std(muni_yr["log_mig_intensity"])
    sub = sub.drop(columns=["fx_z","mig_int_z","log_migint_z"], errors="ignore")
    sub = sub.merge(muni_yr[["lgcode","year","fx_z","mig_int_z","log_migint_z"]],
                    on=["lgcode","year"], how="left")
    if BASELINE_X is not None:
        sub = sub.merge(BASELINE_X, on="lgcode", how="left")
        for c in BASELINE_X_COLS:
            sub[c] = sub[c].fillna(sub[c].mean())

    d = sub.dropna(subset=[y, "fx_z"]).copy()
    if d[y].nunique() < 2 or len(d) < 50 or d[y].std(ddof=1) == 0:
        return None

    # treatment: fx_z * log_migint_z (log_int)
    d["treatment"] = d["fx_z"] * d["log_migint_z"]
    rhs = ["treatment"]
    years_present = sorted(d["year"].unique())
    actual_ref = ref_year if ref_year in years_present else min(years_present)

    # c_mig: year × mig_int_z
    for yr in years_present:
        if yr == actual_ref: continue
        c = f"cmig_{yr}"
        d[c] = d["mig_int_z"] * (d["year"] == yr).astype(float)
        rhs.append(c)
    # c_fx: year × fx_z
    for yr in years_present:
        if yr == actual_ref: continue
        c = f"cfx_{yr}"
        d[c] = d["fx_z"] * (d["year"] == yr).astype(float)
        rhs.append(c)
    # Block A: year × baseline X
    for k in BASELINE_X_COLS:
        for yr in years_present:
            if yr == actual_ref: continue
            c = f"cX_{k}_{yr}"
            d[c] = d[k] * (d["year"] == yr).astype(float)
            rhs.append(c)

    d_idx = d.set_index([entity_col, "year"])
    cluster_series = pd.Series(d[cluster_col].values, index=d_idx.index, name="_cl")
    try:
        m = PanelOLS(d_idx[y], d_idx[rhs],
                     entity_effects=True, time_effects=True,
                     drop_absorbed=True).fit(cov_type="clustered", clusters=cluster_series)
    except Exception as e:
        return {"err": str(e)[:80]}
    if "treatment" not in m.params.index:
        return {"err": "treatment absorbed"}
    return {
        "beta":   float(m.params["treatment"]),
        "se":     float(m.std_errors["treatment"]),
        "pval":   float(m.pvalues["treatment"]),
        "n":      int(m.nobs),
        "n_unit": int(d_idx.index.get_level_values(entity_col).nunique()),
        "n_muni": int(d[cluster_col].nunique()),
        "mean_y": float(d[y].mean()),
        "sd_y":   float(d[y].std(ddof=1)),
    }

# --- 5. Run for input-use outcomes
OUTCOMES = [
    "owns_plough",
    "owns_powered_machinery",
    "owns_irrigation_kit",
    "n_irrigation_types",
    "log_total_input_cost_rs",
    "input_intensity_per_sqm",
    "log_wet_cost_fert",
    "log_dry_cost_fert",
    "log_wet_cost_seed",
    "log_dry_cost_seed",
    "log_wet_cost_labour",
    "log_dry_cost_labour",
]
rows = []
for y in OUTCOMES:
    if y not in master.columns:
        print(f"  skip {y} (not in master)")
        continue
    for thr in [0, 25, 50, 100]:
        r = fit_one(master, y, thr=thr)
        if r is None:
            continue
        rows.append(dict(outcome=y, threshold=thr, **r))
out = pd.DataFrame(rows)
out["stars"] = pd.cut(out["pval"], bins=[-1, .01, .05, .10, 2],
                     labels=["***","**","*",""]).astype(str)
out["beta_pp"]     = out["beta"] * 100
out["pct_of_mean"] = 100 * out["beta"] / out["mean_y"].where(out["mean_y"] != 0, np.nan)

# save & print
out_path = ROOT / "output/tab/input_use_results.csv"
out_path.parent.mkdir(parents=True, exist_ok=True)
out.to_csv(out_path, index=False)
print(f"\nSaved: {out_path}")

print("\n" + "="*80)
for y in OUTCOMES:
    block = out[out.outcome == y]
    if not len(block): continue
    print(f"\n{y}:")
    for _, r in block.iterrows():
        print(f"  k={r.threshold:3d}  β={r.beta:11.4f} {r.stars:>3}  SE={r.se:10.4f}  pct={r.pct_of_mean:7.2f}%  mean={r.mean_y:10.4f}  n={int(r.n)}  n_HH={int(r.n_unit)}")
