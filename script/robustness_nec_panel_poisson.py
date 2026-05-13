"""
script/robustness_nec_panel_poisson.py

PPML (Poisson pseudo-MLE) version of the NEC-panel entry robustness sweep.

Why:
  The OLS-on-log(1+new_firms) approach used in robustness_nec.R floors zero
  cells at log(1)=0 and treats them as if they were "1 firm".  For industry
  and size buckets with substantial zero-share (agriculture 57%, size 51+
  91%, utilities 85%, size 10-50 53%, etc.) this can bias the coefficient
  toward zero and is hard to interpret as an elasticity.

  Poisson pseudo-MLE with high-dimensional FE (Wooldridge 1999, Silva &
  Tenreyro 2006) handles zero cells natively, requires only that
  E[y|X] = exp(Xb) be correctly specified, and gives the same semi-elasticity
  interpretation as the OLS-log coefficient — so the two are directly
  comparable.

Spec — matches fit_nec_panel in robustness_nec.R exactly:
  outcome  : raw new_firms_<industry|size>           (count, integer >= 0)
  link     : log (Poisson)
  treatment: fx_z * log_migint_z                     (log/lin)
  controls : year * mig_int_z + year * fx_z + year * Block_A
             (Block A = 4 region shares + dest_gdp_pc, all baseline 2001)
  FE       : lgcode + year
  cluster  : lgcode
  sample   : panel of muni-founding-year cells, lag-merged with FX shock at
             year t-L, then filtered to munis with total_migrants_2001 >= k

Output: output/tab/robustness_nec_poisson.csv
Schema: outcome_group | dataset | outcome | spec | threshold | lag |
        treatment_kind | scale_form | beta | stars | se | pval |
        mean_y | sd_y | n | n_muni | interpret | err

  beta is the Poisson coefficient on treatment (semi-elasticity:
  beta = d log E[y] / d treatment).  Stars: *** 1%, ** 5%, * 10%.

Run from repo root:
  python3 script/robustness_nec_panel_poisson.py
"""
from __future__ import annotations
import sys, time, math, warnings
from pathlib import Path
import numpy as np
import pandas as pd

warnings.filterwarnings("ignore")

from pyfixest.estimation import fepois

ROOT = Path(".")
OUT_PATH = ROOT / "output/tab/robustness_nec_poisson.csv"
OUT_PATH.parent.mkdir(parents=True, exist_ok=True)

# -----------------------------------------------------------------------------
# 1. Outcomes — 12 per spec (aggregate + 4 sizes + 7 industries)
# -----------------------------------------------------------------------------
OUTCOMES = [
    "new_firms",
    "new_firms_size_1_worker", "new_firms_size_2_9_workers",
    "new_firms_size_10_50_workers", "new_firms_size_51plus_workers",
    "new_firms_agriculture", "new_firms_manufacturing",
    "new_firms_construction", "new_firms_trade_retail",
    "new_firms_hospitality_food", "new_firms_transport_storage",
    "new_firms_finance_prof_realestate",
]
LAGS = list(range(0, 11))
THRESHOLDS = [0, 25, 50, 100]

# -----------------------------------------------------------------------------
# 2. Load data + build Block A (mirrors load_instrument / build_block_A in R)
# -----------------------------------------------------------------------------
print("Loading data ...")
inst_raw = pd.read_csv("data/clean/instrument/instrument_mun.csv")
inst = inst_raw[["lgcode","year","fxshock","mig_intensity","total_migrants"]].copy()
inst["log_mig_intensity"] = np.log(inst["mig_intensity"] + 1e-8)

nec_p = pd.read_csv("data/clean/nec2018/mun_entry_panel_new.csv")
nec_p = nec_p[(nec_p["year"] >= 2001) & (nec_p["year"] <= 2018)].copy()

# Block A (destination-weighted GDP + region shares, all baseline 2001)
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
# Drop the largest share (modal category) as the reference, matching the R code
if region_cols:
    means = region[region_cols].mean()
    drop = means.idxmax()
    region_cols = [c for c in region_cols if c != drop]
bx = region[["lgcode"] + region_cols].merge(dest_gdp, on="lgcode", how="outer")
BLOCK_A_COLS = region_cols + ["dest_gdp_pc_2001"]
for c in BLOCK_A_COLS:
    bx[c] = bx[c].fillna(bx[c].mean())
print(f"  Block A cols: {BLOCK_A_COLS}")
print(f"  nec_p rows: {len(nec_p)}, inst rows: {len(inst)}")

# -----------------------------------------------------------------------------
# 3. Helpers
# -----------------------------------------------------------------------------
def zscore(x: np.ndarray) -> np.ndarray:
    x = np.asarray(x, dtype=float)
    s = np.nanstd(x, ddof=1)
    if not np.isfinite(s) or s == 0:
        return np.zeros_like(x)
    return (x - np.nanmean(x)) / s

def build_year_dummies(d: pd.DataFrame, x_col: str, prefix: str,
                       ref_year: int) -> list[str]:
    years = sorted(d["year"].unique())
    if len(years) < 2:
        return []
    ref = ref_year if ref_year in years else min(years)
    cols = []
    for y in years:
        if y == ref: continue
        nm = f"{prefix}_x_{y}"
        d[nm] = d[x_col].values * (d["year"].values == y).astype(float)
        cols.append(nm)
    return cols

def stars(p: float) -> str:
    if not np.isfinite(p): return ""
    if p < .01: return "***"
    if p < .05: return "**"
    if p < .10: return "*"
    return ""

def classify(o: str) -> str:
    if o == "new_firms" or o.startswith("new_firms_size_"):
        return "Firm entry — total + by size (PPML)"
    return "Firm entry — by industry (PPML)"

def interpret(beta: float, mean_y: float) -> str:
    if not np.isfinite(beta): return ""
    # PPML beta is semi-elasticity: d log E[y] / d treatment, so ~ %-change
    pct_str = ""
    if np.isfinite(mean_y) and mean_y > 1e-9:
        pct_str = f" (β·meanY ≈ {beta*mean_y:+.2f} firms)"
    return f"{beta*100:+.1f}% change in expected count{pct_str}"

# -----------------------------------------------------------------------------
# 4. Single fit
# -----------------------------------------------------------------------------
def fit_one(outcome: str, lag_L: int, threshold: int) -> dict:
    inst_use = inst.copy()
    if lag_L != 0:
        inst_use["year"] = inst_use["year"] + lag_L
    panel = nec_p.merge(
        inst_use[["lgcode","year","fxshock","mig_intensity","total_migrants",
                  "log_mig_intensity"]],
        on=["lgcode","year"], how="inner", suffixes=("","_inst"))
    panel = panel[panel["total_migrants"] >= threshold].copy()
    if len(panel) < 50:
        return {"err": "n<50 after threshold"}
    # z-score muni-year level (collapse to muni × year)
    my = panel[["lgcode","year","fxshock","mig_intensity","log_mig_intensity"]].drop_duplicates(["lgcode","year"]).copy()
    my["fx_z"]         = zscore(my["fxshock"].values)
    my["mig_int_z"]    = zscore(my["mig_intensity"].values)
    my["log_migint_z"] = zscore(my["log_mig_intensity"].values)
    panel = panel.merge(my[["lgcode","year","fx_z","mig_int_z","log_migint_z"]],
                        on=["lgcode","year"], how="left")
    panel = panel.merge(bx[["lgcode"] + BLOCK_A_COLS], on="lgcode", how="left")
    d = panel[panel[outcome].notna() & panel["fx_z"].notna()].copy()
    if len(d) < 50 or d[outcome].nunique() < 2:
        return {"err": "too few obs / no variation"}
    # treatment = log/lin = fx_z * log_migint_z
    d["treatment"] = d["fx_z"].values * d["log_migint_z"].values
    # year-interacted controls (log/lin uses log_migint_z for mig control)
    year_cols  = build_year_dummies(d, "log_migint_z", "cmig", 2001)
    year_cols += build_year_dummies(d, "fx_z",         "cfx",  2001)
    for k in BLOCK_A_COLS:
        year_cols += build_year_dummies(d, k, f"cA_{k}", 2001)
    rhs = ["treatment"] + year_cols
    formula = f"{outcome} ~ " + " + ".join(rhs) + " | lgcode + year"
    try:
        fit = fepois(formula, data=d, vcov={"CRV1": "lgcode"})
        tidy = fit.tidy()
        if "treatment" not in tidy.index:
            return {"err": "treatment absorbed"}
        row = tidy.loc["treatment"]
        return dict(
            beta=float(row["Estimate"]),
            se=float(row["Std. Error"]),
            pval=float(row["Pr(>|t|)"]),
            n=int(fit._N),
            n_muni=int(d["lgcode"].nunique()),
            mean_y=float(d[outcome].mean()),
            sd_y=float(d[outcome].std()),
        )
    except Exception as e:
        return {"err": str(e)[:120]}

# -----------------------------------------------------------------------------
# 5. Loop with incremental save
# -----------------------------------------------------------------------------
rows = []
t0 = time.time()
total = len(LAGS) * len(THRESHOLDS) * len(OUTCOMES)
print(f"\n========== robustness_nec_panel_poisson ==========")
print(f"Outcomes:   {len(OUTCOMES)}")
print(f"Lags:       {LAGS}")
print(f"Thresholds: {THRESHOLDS}")
print(f"Total fits: {total}\n")

def save_partial():
    df = pd.DataFrame(rows)
    for c in ("beta","se","pval","mean_y","sd_y","n","n_muni"):
        if c not in df.columns: df[c] = np.nan
    if "err" not in df.columns: df["err"] = ""
    df["err"] = df["err"].fillna("")
    df["stars"] = df["pval"].apply(stars)
    df["outcome_group"] = df["outcome"].apply(classify)
    df["interpret"] = df.apply(lambda r: interpret(r["beta"], r["mean_y"]), axis=1)
    df["dataset"] = "nec_panel_ppml"
    df["scale_form"] = "ppml"
    df["treatment_kind"] = "log_int"
    df["c_mig_log_flag"] = True
    df["spec"] = df.apply(lambda r: f"S_ppml_lag{int(r['lag'])}", axis=1)
    KEEP = ["outcome_group","dataset","outcome","spec","threshold","lag",
            "treatment_kind","c_mig_log_flag","scale_form",
            "beta","stars","se","pval","mean_y","sd_y","n","n_muni",
            "interpret","err"]
    df = df.reindex(columns=KEEP)
    df = df.sort_values(["outcome_group","outcome","lag","threshold"]).reset_index(drop=True)
    df.to_csv(OUT_PATH, index=False)

done = 0
for L in LAGS:
    for thr in THRESHOLDS:
        for oc in OUTCOMES:
            r = fit_one(oc, L, thr)
            r.update({"outcome": oc, "lag": L, "threshold": thr})
            rows.append(r)
            done += 1
            if "err" in r and r.get("err"):
                tag = f"ERR: {r['err'][:40]}"
            else:
                tag = f"β={r['beta']:+.4f} {stars(r['pval'])} (n={r['n']})"
            print(f"  [{done:4d}/{total}] lag={L} k={thr:3d} {oc:38s} {tag}")
        save_partial()
        elapsed = (time.time() - t0) / 60
        print(f"  --- lag={L} k={thr} done, elapsed {elapsed:.1f} min, saved partial ---")

save_partial()
print(f"\nWall-clock: {(time.time()-t0)/60:.1f} min")
print(f"Saved: {OUT_PATH}")
