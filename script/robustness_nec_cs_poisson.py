"""
script/robustness_nec_cs_poisson.py

PPML + OLS-on-log(1+y) on the 2018 NEC cross-section, by cohort.

Why:
  Cards 3 and 4 of the visualizations page show 2018-stock SHARES by
  cohort.  This script provides the missing comparator: lag profiles of
  raw FIRM COUNTS by industry and by size, run both as OLS on log(1+y)
  and as PPML on the raw count, so the methodological comparison
  (Q.1 in the visualizations page) extends from flow to stock.

Spec — matches fit_nec_cs in robustness_nec.R exactly:
  outcome  : raw n_firms_<industry|size>          (count, integer >= 0)
  link     : log (Poisson) for PPML; identity on log(1+y) for OLS
  treatment: fx_z * log_migint_z                  (log/lin)
  controls : mig_int_z + fx_z + Block A levels    (no year-interactions; cs)
  FE       : DIST  (district)
  cluster  : DIST
  sample   : 2018 cross-section, lag-merged with FX shifter at year 2018-L,
             filtered to munis with total_migrants_2001 >= k

Datasets (3):
  nec_cs_full    full 2018 stock     (data/clean/nec2018/...)
  nec_cs_2001    post-2001 cohort    (output/tab/mun_cohort_stock_post2001.csv)
  nec_cs_2011    post-2011 cohort    (output/tab/mun_cohort_stock_post2011.csv)

Outcomes (13):
  n_firms (aggregate)
  n_firms_size_1_worker  ..  n_firms_size_51plus_workers   (4 sizes)
  n_firms_manufacturing, _hospitality, _trade_retail, _construction,
  _transport, _agriculture, _finance_prof_info, _social_services (8 industries)

Output: output/tab/robustness_nec_cs_poisson.csv  (~860 rows = 13 × 11 × 3 × 2)

Run from repo root:
  python3 script/robustness_nec_cs_poisson.py
"""
from __future__ import annotations
import time, warnings
from pathlib import Path
import numpy as np
import pandas as pd
warnings.filterwarnings("ignore")
from pyfixest.estimation import feols, fepois

ROOT = Path(".")
OUT_PATH = ROOT / "output/tab/robustness_nec_cs_poisson.csv"
OUT_PATH.parent.mkdir(parents=True, exist_ok=True)

# -----------------------------------------------------------------------------
# 1. Outcomes
# -----------------------------------------------------------------------------
OUTCOMES = [
    "n_firms",
    "n_firms_size_1_worker", "n_firms_size_2_9_workers",
    "n_firms_size_10_50_workers", "n_firms_size_51plus_workers",
    "n_firms_manufacturing", "n_firms_hospitality", "n_firms_trade_retail",
    "n_firms_construction", "n_firms_transport", "n_firms_agriculture",
    "n_firms_finance_prof_info", "n_firms_social_services",
]
LAGS = list(range(0, 11))
THRESHOLD = 25
ESTIMATORS = ["ols_log", "ppml"]

# -----------------------------------------------------------------------------
# 2. Load instrument + Block A (mirrors load_instrument / build_block_A in R)
# -----------------------------------------------------------------------------
print("Loading data ...")
inst_raw = pd.read_csv("data/clean/instrument/instrument_mun.csv")
inst = inst_raw[["lgcode","year","fxshock","mig_intensity","total_migrants"]].copy()
inst["log_mig_intensity"] = np.log(inst["mig_intensity"] + 1e-8)

# Block A
region = pd.read_csv("data/clean/instrument/dest_region_shares_2001.csv")
wdi    = pd.read_csv("data/clean/instrument/wdi_dest_gdp_2001.csv")
share  = pd.read_csv("data/clean/instrument/dest_mun_mig_share_2001.csv")
wdi    = wdi.loc[wdi["gdp_pc_2001"].notna(), ["country","gdp_pc_2001"]]
sh = share.merge(wdi, on="country", how="inner")
sh["num"] = sh["mun_mig_share_2001"] * sh["gdp_pc_2001"]
dest_gdp = sh.groupby("lgcode").agg(num=("num","sum"),
                                     cov=("mun_mig_share_2001","sum")).reset_index()
dest_gdp["dest_gdp_pc_2001"] = dest_gdp["num"] / dest_gdp["cov"].clip(lower=1e-9)
dest_gdp = dest_gdp[["lgcode","dest_gdp_pc_2001"]]
region_cols = [c for c in region.columns if c.startswith("share_")]
if region_cols:
    drop = region[region_cols].mean().idxmax()
    region_cols = [c for c in region_cols if c != drop]
bx = region[["lgcode"] + region_cols].merge(dest_gdp, on="lgcode", how="outer")
BLOCK_A_COLS = region_cols + ["dest_gdp_pc_2001"]
for c in BLOCK_A_COLS:
    bx[c] = bx[c].fillna(bx[c].mean())
print(f"  Block A cols: {BLOCK_A_COLS}")

# -----------------------------------------------------------------------------
# 3. Load the 3 cross-sections, build a unified column schema
# -----------------------------------------------------------------------------
def load_full_2018_stock():
    ind = pd.read_csv("data/clean/nec2018/mun_industry_structure.csv")
    sz  = pd.read_csv("data/clean/nec2018/mun_size_formality.csv")
    keep_ind = [c for c in OUTCOMES if c in ind.columns]
    keep_sz  = [c for c in OUTCOMES if c in sz.columns and c not in keep_ind]
    cs = ind[["lgcode"] + keep_ind].merge(sz[["lgcode"] + keep_sz], on="lgcode",
                                          how="outer")
    cs["DIST"] = cs["lgcode"] // 100
    return cs

DATA = {
    "nec_cs_full": load_full_2018_stock(),
    "nec_cs_2001": pd.read_csv("output/tab/mun_cohort_stock_post2001.csv"),
    "nec_cs_2011": pd.read_csv("output/tab/mun_cohort_stock_post2011.csv"),
}
for name, df in DATA.items():
    if "DIST" not in df.columns:
        df["DIST"] = df["lgcode"] // 100
    missing = [o for o in OUTCOMES if o not in df.columns]
    print(f"  {name}: {len(df)} munis, missing outcomes: {missing or 'none'}")

# -----------------------------------------------------------------------------
# 4. Single fit
# -----------------------------------------------------------------------------
def zscore(x):
    x = np.asarray(x, dtype=float)
    s = np.nanstd(x, ddof=1)
    return np.zeros_like(x) if (not np.isfinite(s) or s == 0) else (x - np.nanmean(x)) / s

def stars(p):
    if not np.isfinite(p): return ""
    return "***" if p < .01 else "**" if p < .05 else "*" if p < .10 else ""

def fit_one(dataset, outcome, lag_L, estimator):
    yr = 2018 - lag_L
    inst_use = inst[inst["year"] == yr][["lgcode","fxshock","mig_intensity",
                                          "total_migrants","log_mig_intensity"]].copy()
    if inst_use.empty:
        return {"err": f"no FX for year {yr}"}
    cs = DATA[dataset].merge(inst_use, on="lgcode", how="inner")
    cs = cs[cs["total_migrants"] >= THRESHOLD].copy()
    if outcome not in cs.columns:
        return {"err": f"outcome {outcome} missing in {dataset}"}
    cs = cs[cs[outcome].notna() & cs["fxshock"].notna()]
    if len(cs) < 30 or cs[outcome].nunique() < 2:
        return {"err": "too few obs / no variation"}
    cs["fx_z"]         = zscore(cs["fxshock"].values)
    cs["mig_int_z"]    = zscore(cs["mig_intensity"].values)
    cs["log_migint_z"] = zscore(cs["log_mig_intensity"].values)
    cs = cs.merge(bx[["lgcode"] + BLOCK_A_COLS], on="lgcode", how="left")
    cs["treatment"] = cs["fx_z"].values * cs["log_migint_z"].values

    if estimator == "ols_log":
        y_col = f"log_{outcome}"
        cs[y_col] = np.log1p(np.clip(cs[outcome].values, 0, None))
        lhs = y_col
        fn = feols
    else:  # ppml
        lhs = outcome
        fn = fepois

    rhs = ["treatment", "fx_z", "log_migint_z"] + BLOCK_A_COLS
    formula = f"{lhs} ~ " + " + ".join(rhs) + " | DIST"
    try:
        fit = fn(formula, data=cs, vcov={"CRV1": "DIST"})
        tidy = fit.tidy()
        if "treatment" not in tidy.index:
            return {"err": "treatment absorbed"}
        row = tidy.loc["treatment"]
        return dict(
            beta=float(row["Estimate"]),
            se=float(row["Std. Error"]),
            pval=float(row["Pr(>|t|)"]),
            n=int(fit._N),
            n_muni=int(cs["lgcode"].nunique()),
            mean_y=float(cs[outcome].mean()),
            sd_y=float(cs[outcome].std()),
        )
    except Exception as e:
        return {"err": str(e)[:120]}

# -----------------------------------------------------------------------------
# 5. Loop
# -----------------------------------------------------------------------------
rows = []
t0 = time.time()
total = len(LAGS) * len(DATA) * len(OUTCOMES) * len(ESTIMATORS)
print(f"\n========== robustness_nec_cs_poisson ==========")
print(f"Outcomes: {len(OUTCOMES)}  ·  Lags: {LAGS}  ·  Cohorts: {list(DATA)}  ·  k={THRESHOLD}")
print(f"Total fits (OLS + PPML): {total}\n")

def classify(o):
    if o == "n_firms":
        return "Stock — aggregate"
    if o.startswith("n_firms_size_"):
        return "Stock — by size"
    return "Stock — by industry"

def save_partial():
    df = pd.DataFrame(rows)
    for c in ("beta","se","pval","mean_y","sd_y","n","n_muni"):
        if c not in df.columns: df[c] = np.nan
    if "err" not in df.columns: df["err"] = ""
    df["err"] = df["err"].fillna("")
    df["stars"] = df["pval"].apply(stars)
    df["outcome_group"] = df["outcome"].apply(classify)
    df.to_csv(OUT_PATH, index=False)

done = 0
for est in ESTIMATORS:
    for dataset in DATA:
        for L in LAGS:
            for oc in OUTCOMES:
                r = fit_one(dataset, oc, L, est)
                r.update({"dataset": dataset, "outcome": oc, "lag": L,
                          "estimator": est, "threshold": THRESHOLD})
                rows.append(r)
                done += 1
                if r.get("err"):
                    tag = f"ERR: {r['err'][:32]}"
                else:
                    tag = f"β={r['beta']:+.4f} {stars(r['pval'])} (n={r['n']})"
                print(f"  [{done:4d}/{total}] {est:7s} {dataset:12s} lag={L:2d} "
                      f"{oc:35s} {tag}")
            save_partial()

save_partial()
print(f"\nWall-clock: {(time.time()-t0)/60:.1f} min")
print(f"Saved: {OUT_PATH}")
