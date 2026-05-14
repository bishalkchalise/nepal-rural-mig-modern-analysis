"""
Incremental first-stage: gradually add the four RHS components, mirroring the
portal's regression structure but applied to the district panel.

Components (added one at a time):
  C1  treatment:  fx_z * mig_int_z                                       [headline]
  C2  + c_mig:    mig_int_z * year_dummies                               (control)
  C3  + c_fx:     fx_z * year_dummies                                    (control)
  C4  + X:        destination-region 2001 shares * year_dummies          (control)

Always-on:  fxshock main effect (alpha * fx_z), dname FE, year FE, cluster ~dname.
Treatments z-scored on the working sample. Two share types: 2001 census + DOFE 2009-10.

Outcomes:
  - DOFE district permits  (log(permits+1), 74 districts x 15 years)
  - RVS district migrants  (log(n_intl_migrants+1), 48 districts x 3 years)
  - RVS district remit     (log(remit_intl_12m_rs+1))
  - RVS district share_hh  (share_hh_with_migrant)
"""

import numpy as np
import pandas as pd
from linearmodels.panel import PanelOLS

# ----------------------------------------------------------------------------
# Load
# ----------------------------------------------------------------------------

inst       = pd.read_csv("district-analysis/data/clean/instrument/instrument_forex_dist.csv")
inst_dofe  = pd.read_csv("district-analysis/data/clean/instrument/instrument_dofe_dist.csv")
region_sh  = pd.read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv")
dofe_raw   = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")

# DOFE district x year permits
dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s): return dofe_to_census.get(s, s.title())

dofe_panel = (dofe_raw.groupby(["district_rename","year"]).total_migrants.sum()
              .reset_index()
              .assign(dname = lambda d: d.district_rename.map(to_dname))
              .rename(columns={"total_migrants":"permits"})
              [["dname","year","permits"]])

# RVS district panel
try:
    rvs_d_raw = pd.read_csv("district-analysis/data/clean/rvs/migration_district_year.csv")
    def to_dname_rvs(s):
        s = str(s).strip().upper()
        if s in dofe_to_census: return dofe_to_census[s]
        return s.title()
    rvs_d = rvs_d_raw.assign(dname = lambda d: d.dname_raw.map(to_dname_rvs))
except Exception as e:
    print("WARNING - could not load RVS district:", e); rvs_d = None

# ----------------------------------------------------------------------------
# Build base panel with z-scored treatments + region shares
# ----------------------------------------------------------------------------

def zscore(s):
    mu, sd = s.mean(), s.std()
    return (s - mu) / sd if sd > 0 else pd.Series(0.0, index=s.index)

def build_base(panel_df, share_label):
    """panel_df has columns including fxshock or fxshock_dofe; pick the right one
    and z-score on the WORKING sample (i.e. after merge)."""
    df = panel_df.copy()
    if share_label == "2001":
        df["fx_raw"] = df["fxshock"]
    else:
        df["fx_raw"] = df["fxshock_dofe"]
    df["fx_z"]         = zscore(df["fx_raw"])
    df["mi_z"]         = zscore(df["geog_intensity_2001"])
    df["log_mi_z"]     = zscore(np.log(df["geog_intensity_2001"] + 1e-12))
    return df

# ----------------------------------------------------------------------------
# Helper: build year-interaction columns
# ----------------------------------------------------------------------------

def add_year_inter(df, col, prefix, ref_year):
    out = pd.DataFrame(index=df.index)
    for y in sorted(df.year.unique()):
        if y == ref_year: continue
        out[f"{prefix}_y{y}"] = df[col] * (df.year == y).astype(float)
    return out

# ----------------------------------------------------------------------------
# Run one regression
# ----------------------------------------------------------------------------

def fit_inc(df, outcome, ref_year, level=1, fxshock_col="fx_z", mi_col="mi_z"):
    """level=1: treatment only;  level=2: +c_mig;  level=3: +c_fx;  level=4: +X."""
    d = df.dropna(subset=[outcome, fxshock_col, mi_col, "dname", "year"]).copy()
    if len(d) < 30 or d[outcome].nunique() < 2: return None
    d["treatment"] = d[fxshock_col] * d[mi_col]

    exog_cols = ["treatment", fxshock_col]   # always include main fxshock too
    extra     = pd.DataFrame(index=d.index)

    if level >= 2:
        extra = pd.concat([extra,
            add_year_inter(d, mi_col, "cmig", ref_year)], axis=1)
    if level >= 3:
        extra = pd.concat([extra,
            add_year_inter(d, fxshock_col, "cfx", ref_year)], axis=1)
    if level >= 4:
        # region shares x year
        for col in ["share_e_asia","share_gulf","share_oecd_north",
                    "share_oecd_europe","share_s_asia","share_se_asia"]:
            if col in d.columns:
                extra = pd.concat([extra,
                    add_year_inter(d, col, f"cX_{col}", ref_year)], axis=1)

    exog = pd.concat([d[exog_cols], extra], axis=1)
    y    = d[outcome]
    idx  = pd.MultiIndex.from_arrays([d["dname"].values, d["year"].values],
                                     names=["dname","year"])
    exog.index = idx; y.index = idx

    try:
        m = PanelOLS(y, exog, entity_effects=True, time_effects=True,
                     drop_absorbed=True)
        r = m.fit(cov_type="clustered", cluster_entity=True)
        return {
            "beta":  float(r.params["treatment"]),
            "se":    float(r.std_errors["treatment"]),
            "t":     float(r.tstats["treatment"]),
            "p":     float(r.pvalues["treatment"]),
            "n":     int(r.nobs),
            "r2_w":  float(r.rsquared_within),
        }
    except Exception as e:
        return {"err": str(e)[:80]}

# ----------------------------------------------------------------------------
# Assemble panels + outcomes + run
# ----------------------------------------------------------------------------

# Merge for DOFE permits panel
dofe_full = (dofe_panel.merge(inst, on=["dname","year"])
                       .merge(inst_dofe[["dname","year","fxshock_dofe"]],
                              on=["dname","year"])
                       .merge(region_sh, on="dname", how="left"))
dofe_full["log_permits"] = np.log(dofe_full.permits + 1)

# Merge for RVS district panel
if rvs_d is not None:
    rvs_full = (rvs_d.merge(inst, on=["dname","year"])
                     .merge(inst_dofe[["dname","year","fxshock_dofe"]],
                            on=["dname","year"])
                     .merge(region_sh, on="dname", how="left"))
    rvs_full["log_n_intl"] = np.log(rvs_full["n_intl_migrants"] + 1)
    rvs_full["log_remit"]  = np.log(rvs_full["remit_amount_intl_12m_rs"] + 1)
    rvs_full["share_hh"]   = rvs_full["n_hh_with_intl_migrant"] / rvs_full["n_hh"].clip(lower=1)
else:
    rvs_full = None

REF_DOFE = 2016   # mid of 2009-2023
REF_RVS  = 2017   # mid of 2016-2018

datasets = [
    ("DOFE_permits",  "log_permits", dofe_full, REF_DOFE),
]
if rvs_full is not None:
    datasets += [
        ("RVS_n_intl",    "log_n_intl",  rvs_full,  REF_RVS),
        ("RVS_share_hh",  "share_hh",    rvs_full,  REF_RVS),
        ("RVS_remit_intl","log_remit",   rvs_full,  REF_RVS),
    ]

share_types = ["2001", "DOFE"]
levels      = [1, 2, 3, 4]
labels      = {1:"C1 (treatment only)",
               2:"C2 (+ c_mig)",
               3:"C3 (+ c_fx)",
               4:"C4 (+ region X)"}

rows = []
for share_label in share_types:
    for name, outcome, panel, ref_y in datasets:
        df_z = build_base(panel, share_label)
        for lvl in levels:
            r = fit_inc(df_z, outcome, ref_y, level=lvl)
            if r is None or "err" in (r or {}):
                rows.append({"dataset":name, "outcome":outcome,
                             "share":share_label, "spec":labels[lvl],
                             "beta":np.nan, "se":np.nan, "t":np.nan, "p":np.nan,
                             "n":np.nan, "r2_w":np.nan,
                             "note": (r or {}).get("err","NA")})
            else:
                rows.append({"dataset":name, "outcome":outcome,
                             "share":share_label, "spec":labels[lvl],
                             **{k:r[k] for k in ("beta","se","t","p","n","r2_w")},
                             "note":""})

res = pd.DataFrame(rows)

def stars(p):
    if pd.isna(p): return ""
    if p < 0.01: return "***"
    if p < 0.05: return "**"
    if p < 0.10: return "*"
    return ""

res["sig"] = res["p"].apply(stars)
res["beta"]= res["beta"].round(4)
res["se"]  = res["se"].round(4)
res["t"]   = res["t"].round(2)
res["p"]   = res["p"].round(4)
res["r2_w"]= res["r2_w"].round(4)

# Print compact per-outcome view
for name, outcome, _, _ in datasets:
    print(f"\n=== {name} ({outcome}) ===")
    sub = (res[res.dataset == name]
              .pivot_table(index="spec", columns="share",
                           values=["beta","t","sig","n"],
                           aggfunc="first"))
    # Reorder
    sub = sub[[c for c in [("beta","2001"),("t","2001"),("sig","2001"),
                            ("beta","DOFE"),("t","DOFE"),("sig","DOFE"),
                            ("n","2001")] if c in sub.columns]]
    print(sub.to_string())

out_path = "district-analysis/output/tab/first_stage_incremental.csv"
import os; os.makedirs(os.path.dirname(out_path), exist_ok=True)
res.to_csv(out_path, index=False)
print(f"\nSaved: {out_path}  ({len(res)} rows)")
