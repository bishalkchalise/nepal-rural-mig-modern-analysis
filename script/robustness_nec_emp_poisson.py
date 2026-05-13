"""
script/robustness_nec_emp_poisson.py

OLS-on-log(1+y) + PPML on `emp_total` (total municipal employment in 2018),
by cohort, lags 0-10.

Mirrors robustness_nec_cs_poisson.py but limited to the single
employment-total outcome.  Three cohorts:
  nec_cs_full   full 2018 stock         (data/clean/nec2018/mun_size_formality.csv)
  nec_cs_2001   post-2001 cohort        (output/tab/mun_cohort_stock_post2001.csv)
  nec_cs_2011   post-2011 cohort        (output/tab/mun_cohort_stock_post2011.csv)

Output: output/tab/robustness_nec_emp_poisson.csv  (~66 rows)

Run from repo root:
  python3 script/robustness_nec_emp_poisson.py
"""
from __future__ import annotations
import time, warnings
from pathlib import Path
import numpy as np
import pandas as pd
warnings.filterwarnings("ignore")
from pyfixest.estimation import feols, fepois

ROOT = Path(".")
OUT_PATH = ROOT / "output/tab/robustness_nec_emp_poisson.csv"
OUT_PATH.parent.mkdir(parents=True, exist_ok=True)

LAGS = list(range(0, 11))
THRESHOLD = 25
ESTIMATORS = ["ols_log", "ppml"]

print("Loading data ...")
inst_raw = pd.read_csv("data/clean/instrument/instrument_mun.csv")
inst = inst_raw[["lgcode","year","fxshock","mig_intensity","total_migrants"]].copy()
inst["log_mig_intensity"] = np.log(inst["mig_intensity"] + 1e-8)

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

DATA = {
    "nec_cs_full": pd.read_csv("data/clean/nec2018/mun_size_formality.csv"),
    "nec_cs_2001": pd.read_csv("output/tab/mun_cohort_stock_post2001.csv"),
    "nec_cs_2011": pd.read_csv("output/tab/mun_cohort_stock_post2011.csv"),
}
for name, df in DATA.items():
    if "DIST" not in df.columns:
        df["DIST"] = df["lgcode"] // 100
    assert "emp_total" in df.columns, f"{name} missing emp_total"
    z = (df["emp_total"] == 0).sum()
    print(f"  {name}: {len(df)} munis, zero emp_total = {z}, mean = {df['emp_total'].mean():.0f}")

def zscore(x):
    x = np.asarray(x, dtype=float)
    s = np.nanstd(x, ddof=1)
    return np.zeros_like(x) if (not np.isfinite(s) or s == 0) else (x - np.nanmean(x)) / s

def stars(p):
    if not np.isfinite(p): return ""
    return "***" if p < .01 else "**" if p < .05 else "*" if p < .10 else ""

def fit_one(dataset, lag_L, estimator):
    yr = 2018 - lag_L
    inst_use = inst[inst["year"] == yr][["lgcode","fxshock","mig_intensity",
                                          "total_migrants","log_mig_intensity"]].copy()
    if inst_use.empty:
        return {"err": f"no FX for year {yr}"}
    cs = DATA[dataset].merge(inst_use, on="lgcode", how="inner")
    cs = cs[cs["total_migrants"] >= THRESHOLD].copy()
    cs = cs[cs["emp_total"].notna() & cs["fxshock"].notna()]
    if len(cs) < 30 or cs["emp_total"].nunique() < 2:
        return {"err": "too few obs / no variation"}
    cs["fx_z"]         = zscore(cs["fxshock"].values)
    cs["mig_int_z"]    = zscore(cs["mig_intensity"].values)
    cs["log_migint_z"] = zscore(cs["log_mig_intensity"].values)
    cs = cs.merge(bx[["lgcode"] + BLOCK_A_COLS], on="lgcode", how="left")
    cs["treatment"] = cs["fx_z"].values * cs["log_migint_z"].values

    if estimator == "ols_log":
        cs["log_emp_total"] = np.log1p(np.clip(cs["emp_total"].values, 0, None))
        lhs, fn = "log_emp_total", feols
    else:
        lhs, fn = "emp_total", fepois

    rhs = ["treatment", "fx_z", "mig_int_z"] + BLOCK_A_COLS  # log/lin anchor
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
            mean_y=float(cs["emp_total"].mean()),
            sd_y=float(cs["emp_total"].std()),
        )
    except Exception as e:
        return {"err": str(e)[:120]}

rows = []
t0 = time.time()
total = len(LAGS) * len(DATA) * len(ESTIMATORS)
print(f"\n========== robustness_nec_emp_poisson ==========")
print(f"Outcome: emp_total  ·  Lags: {LAGS}  ·  Cohorts: {list(DATA)}  ·  k={THRESHOLD}")
print(f"Total fits (OLS + PPML): {total}\n")

done = 0
for est in ESTIMATORS:
    for dataset in DATA:
        for L in LAGS:
            r = fit_one(dataset, L, est)
            r.update({"dataset": dataset, "outcome": "emp_total", "lag": L,
                      "estimator": est, "threshold": THRESHOLD})
            rows.append(r)
            done += 1
            if r.get("err"):
                tag = f"ERR: {r['err'][:32]}"
            else:
                tag = f"β={r['beta']:+.4f} {stars(r['pval'])} (n={r['n']})"
            print(f"  [{done:3d}/{total}] {est:7s} {dataset:12s} lag={L:2d}  {tag}")

df = pd.DataFrame(rows)
for c in ("beta","se","pval","mean_y","sd_y","n","n_muni"):
    if c not in df.columns: df[c] = np.nan
if "err" not in df.columns: df["err"] = ""
df["err"] = df["err"].fillna("")
df["stars"] = df["pval"].apply(stars)
df.to_csv(OUT_PATH, index=False)

print(f"\nWall-clock: {(time.time()-t0):.1f} sec")
print(f"Saved: {OUT_PATH}")
