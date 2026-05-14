"""
Compare three SSIV construction conventions on the DOFE-permits first-stage.

For each (share_vintage, intensity_vintage) combo and each incremental control
level (C1-C4), run the regression with three treatment definitions:

  A_log  : z(fx) * z(log(mi))          ← portal & Khanna convention (recommended)
  A_lin  : z(fx) * z(mi)               ← linear interaction, Order A
  B_log  : z(fx * log(mi))             ← multiply first, then z (Order B)

Spec (always-on):
  alpha * fx_z + lambda controls when level >= 2/3/4 + dname FE + year FE
  cluster ~dname
"""

import numpy as np, pandas as pd
from linearmodels.panel import PanelOLS

inst       = pd.read_csv("district-analysis/data/clean/instrument/instrument_forex_dist.csv")
inst_dofe  = pd.read_csv("district-analysis/data/clean/instrument/instrument_dofe_dist.csv")
region_sh  = pd.read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv")
dofe_raw   = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file   = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

dofe = (dofe_raw.groupby(["district_rename","year"]).total_migrants.sum().reset_index()
                .assign(dname=lambda d: d.district_rename.map(to_dname))
                .rename(columns={"total_migrants":"permits"})[["dname","year","permits"]])

mi_dofe = (dofe[dofe.year.isin([2009,2010])]
           .groupby("dname").permits.mean().rename("num").reset_index())
pop_2011 = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
                    [["dname","district_population_2011"]]
                    .drop_duplicates("dname")
                    .rename(columns={"district_population_2011":"pop_2011"}))
mi_dofe = (mi_dofe.merge(pop_2011, on="dname")
                  .assign(mig_int_dofe=lambda d: d.num/d.pop_2011)
                  [["dname","mig_int_dofe"]])

panel = (dofe.merge(inst, on=["dname","year"])
              .merge(inst_dofe[["dname","year","fxshock_dofe"]], on=["dname","year"])
              .merge(mi_dofe, on="dname", how="left")
              .merge(region_sh, on="dname", how="left")
              .dropna(subset=["fxshock","fxshock_dofe","geog_intensity_2001","mig_int_dofe"]))
panel["log_permits"] = np.log(panel.permits + 1)

def z(s):
    sd = s.std(); return (s - s.mean())/sd if sd>0 else pd.Series(0., index=s.index)

def yr_inter(df, col, prefix, ref=2016):
    out = pd.DataFrame(index=df.index)
    for y in sorted(df.year.unique()):
        if y == ref: continue
        out[f"{prefix}_y{y}"] = df[col] * (df.year == y).astype(float)
    return out

def fit(panel, share, mi_vintage, construction, level):
    """construction in {A_log, A_lin, B_log}."""
    d = panel.copy()
    d["fx_raw"] = d["fxshock"] if share=="2001" else d["fxshock_dofe"]
    d["mi_raw"] = d["geog_intensity_2001"] if mi_vintage=="2001" else d["mig_int_dofe"]

    d["fx_z"]      = z(d.fx_raw)
    d["mi_z"]      = z(d.mi_raw)
    d["log_mi_z"]  = z(np.log(d.mi_raw.clip(lower=1e-12)))

    if construction == "A_log":
        d["treatment"] = d.fx_z * d.log_mi_z
        cmig_col       = "log_mi_z"
    elif construction == "A_lin":
        d["treatment"] = d.fx_z * d.mi_z
        cmig_col       = "mi_z"
    elif construction == "B_log":
        d["treatment"] = z(d.fx_raw * np.log(d.mi_raw.clip(lower=1e-12)))
        cmig_col       = "log_mi_z"
    else:
        raise ValueError(construction)

    exog_cols = ["treatment","fx_z"]
    extras = pd.DataFrame(index=d.index)
    if level >= 2: extras = pd.concat([extras, yr_inter(d, cmig_col, "cmig")], axis=1)
    if level >= 3: extras = pd.concat([extras, yr_inter(d, "fx_z",  "cfx")],  axis=1)
    if level >= 4:
        for c in ["share_e_asia","share_gulf","share_oecd_north",
                  "share_oecd_europe","share_s_asia","share_se_asia"]:
            if c in d.columns: extras = pd.concat([extras, yr_inter(d, c, f"cX_{c}")], axis=1)

    exog = pd.concat([d[exog_cols], extras], axis=1)
    y    = d["log_permits"]
    idx  = pd.MultiIndex.from_arrays([d.dname.values, d.year.values], names=["dname","year"])
    exog.index = idx; y.index = idx
    try:
        m = PanelOLS(y, exog, entity_effects=True, time_effects=True, drop_absorbed=True)
        r = m.fit(cov_type="clustered", cluster_entity=True)
        return float(r.params["treatment"]), float(r.tstats["treatment"]), float(r.pvalues["treatment"]), int(r.nobs)
    except Exception as e:
        return (np.nan, np.nan, np.nan, np.nan)

def stars(p):
    if pd.isna(p): return ""
    if p<0.01: return "***"
    if p<0.05: return "**"
    if p<0.10: return "*"
    return ""

CONFIGS = [
    ("2001sh+2001mi", "2001", "2001"),
    ("DOFEsh+DOFEmi", "DOFE", "DOFE"),
]
CONSTRUCTIONS = ["A_log", "A_lin", "B_log"]
LEVELS = [1,2,3,4]
LBL = {1:"C1 bare", 2:"C2 +cmig", 3:"C3 +cfx", 4:"C4 +X"}

rows = []
print(f"{'spec':<11} {'config':<16} {'construction':<8} {'beta':>10} {'t':>7} {'sig':>5} {'n':>6}")
print("-"*72)
for lvl in LEVELS:
    for cfg_name, sh, mi in CONFIGS:
        for con in CONSTRUCTIONS:
            b,t,p,n = fit(panel, sh, mi, con, lvl)
            s = stars(p)
            print(f"{LBL[lvl]:<11} {cfg_name:<16} {con:<8} {b:>10.4f} {t:>7.2f} {s:>5} {n if not pd.isna(n) else '':>6}")
            rows.append({"spec":LBL[lvl],"config":cfg_name,"construction":con,
                         "beta":round(b,4),"t_stat":round(t,2),
                         "p_val":round(p,4),"sig":s,
                         "n_obs": (int(n) if not pd.isna(n) else None)})

pd.DataFrame(rows).to_csv(
    "district-analysis/output/tab/diag_three_constructions.csv", index=False)
print("\nSaved: output/tab/diag_three_constructions.csv")
