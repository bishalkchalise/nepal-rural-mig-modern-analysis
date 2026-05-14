"""
Canonical first-stage, run in staggered fashion across all outcomes.

Treatment (locked):    z(fxshock) * z(log(mig_int))     [Order A, log transform]

Specifications (added gradually, one component at a time):
  M1 : treatment                                   | dname FE
  M2 : treatment + alpha*fx_z                      | dname FE
  M3 : treatment + alpha*fx_z                      | dname + year FE
  M4 : + i(year, log_mi_z)                         | dname + year FE     (C_mig)
  M5 : + i(year, fx_z)                             | dname + year FE     (C_fx)
  M6 : + i(year, share_X)  six region shares       | dname + year FE     (C_X)

All terms z-scored on the working sample. Cluster ~dname.

Two vintage configurations:
  V_2001 : fxshock (2001 census shares),  log(mig_int_2001)
  V_DOFE : fxshock_dofe (2009-10 DOFE shares),  log(DOFE_0910 / pop_2011)

Outcomes covered (panels only):
  - DOFE permits district-year     log(permits+1)
  - RVS district-year              log(n_intl_migrants+1), share_hh, log(remit+1)
  - RVS HH-level (migrant-only)    log(n_intl_migrants+1), log(remit+1)
  - RVS migrant-level (intl)       log(remit+1), remit_sent_flag, log(earn+1)
"""

import os, numpy as np, pandas as pd
from linearmodels.panel import PanelOLS

# ---------------------------------------------------------------------------
# 1. Load common inputs
# ---------------------------------------------------------------------------

inst       = pd.read_csv("district-analysis/data/clean/instrument/instrument_forex_dist.csv")
inst_dofe  = pd.read_csv("district-analysis/data/clean/instrument/instrument_dofe_dist.csv")
region_sh  = pd.read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv")
dofe       = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file   = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

# DOFE district-year permits
dofe_dy = (dofe.groupby(["district_rename","year"]).total_migrants.sum().reset_index()
               .assign(dname=lambda d: d.district_rename.map(to_dname))
               .rename(columns={"total_migrants":"permits"})[["dname","year","permits"]])

# DOFE-vintage intensity (2009-10 mean / pop_2011)
mi_dofe = (dofe_dy[dofe_dy.year.isin([2009,2010])]
           .groupby("dname").permits.mean().rename("num").reset_index())
pop_2011 = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
                    [["dname","district_population_2011"]]
                    .drop_duplicates("dname")
                    .rename(columns={"district_population_2011":"pop_2011"}))
mi_dofe = (mi_dofe.merge(pop_2011, on="dname")
                  .assign(mig_int_dofe=lambda d: d.num/d.pop_2011)
                  [["dname","mig_int_dofe"]])

# RVS panels (only if present locally; cloud may not have them)
def try_load(p):
    try: return pd.read_csv(p)
    except Exception: return None

rvs_dist_path  = "district-analysis/data/clean/rvs/migration_district_year.csv"
rvs_hh_path    = "district-analysis/data/clean/rvs/migration_hh_year.csv"
rvs_mig_path   = "district-analysis/data/clean/rvs/migration_migrant_year.csv"

rvs_dist = try_load(rvs_dist_path)
rvs_hh   = try_load(rvs_hh_path)
rvs_mig  = try_load(rvs_mig_path)

if rvs_dist is not None:
    rvs_dist = rvs_dist.assign(
        dname=lambda d: d.dname_raw.map(to_dname),
        log_n_intl=lambda d: np.log(d.n_intl_migrants + 1),
        log_remit =lambda d: np.log(d.remit_amount_intl_12m_rs + 1),
        share_hh  =lambda d: d.n_hh_with_intl_migrant / d.n_hh.clip(lower=1))

def _rvs_with_dname(df):
    if df is None: return None
    dist_col = next((c for c in ["district_name","district77","district"] if c in df.columns), None)
    if dist_col is None: return None
    return df.assign(dname=lambda d: d[dist_col].map(to_dname))

rvs_hh   = _rvs_with_dname(rvs_hh)
rvs_mig  = _rvs_with_dname(rvs_mig)

# ---------------------------------------------------------------------------
# 2. Helpers
# ---------------------------------------------------------------------------

def z(s):
    s = pd.Series(s); sd = s.std()
    return (s - s.mean()) / sd if sd > 0 else pd.Series(0., index=s.index)

def yr_inter(df, col, prefix, ref):
    out = pd.DataFrame(index=df.index)
    for y in sorted(df.year.unique()):
        if y == ref: continue
        out[f"{prefix}_y{y}"] = df[col] * (df.year == y).astype(float)
    return out

# ---------------------------------------------------------------------------
# 3. The runner: takes a panel + outcome + vintage + spec level, returns res
# ---------------------------------------------------------------------------

REGION_COLS = ["share_e_asia","share_gulf","share_oecd_north",
               "share_oecd_europe","share_s_asia","share_se_asia"]

def fit_stage(panel, outcome, vintage, level, ref_year, entity_col="dname"):
    """vintage in {'V_2001','V_DOFE'};  level in 1..6."""
    d = panel.dropna(subset=[outcome]).copy()
    if vintage == "V_2001":
        d["fx_raw"] = d["fxshock"]
        d["mi_raw"] = d["geog_intensity_2001"]
    else:
        d["fx_raw"] = d["fxshock_dofe"]
        d["mi_raw"] = d["mig_int_dofe"]
    d = d.dropna(subset=["fx_raw","mi_raw"]).copy()
    if len(d) < 30: return None

    d["fx_z"]      = z(d.fx_raw)
    d["log_mi_z"]  = z(np.log(d.mi_raw.clip(lower=1e-12)))
    d["treatment"] = d.fx_z * d.log_mi_z

    # Required FE columns
    if entity_col not in d.columns or "year" not in d.columns: return None

    exog_cols = ["treatment"]
    extras    = []

    if level >= 2: exog_cols.append("fx_z")
    if level >= 4: extras.append(yr_inter(d, "log_mi_z", "cmig", ref_year))
    if level >= 5: extras.append(yr_inter(d, "fx_z",     "cfx",  ref_year))
    if level >= 6:
        for c in REGION_COLS:
            if c in d.columns:
                extras.append(yr_inter(d, c, f"cX_{c}", ref_year))

    exog = pd.concat([d[exog_cols]] + extras, axis=1) if extras else d[exog_cols]
    y    = d[outcome]
    # entity index + year for FE
    idx  = pd.MultiIndex.from_arrays([d[entity_col].values, d.year.values],
                                     names=[entity_col, "year"])
    exog.index = idx; y.index = idx

    entity_effects = True
    time_effects   = (level >= 3)

    try:
        m = PanelOLS(y, exog, entity_effects=entity_effects,
                     time_effects=time_effects, drop_absorbed=True)
        r = m.fit(cov_type="clustered", cluster_entity=True)
        if "treatment" not in r.params.index: return {"err":"absorbed"}
        return {
            "beta":  float(r.params["treatment"]),
            "se":    float(r.std_errors["treatment"]),
            "t":     float(r.tstats["treatment"]),
            "p":     float(r.pvalues["treatment"]),
            "n":     int(r.nobs),
            "r2_w":  float(r.rsquared_within),
        }
    except Exception as e:
        return {"err":str(e)[:80]}

def stars(p):
    if p is None or pd.isna(p): return ""
    if p<0.01: return "***"
    if p<0.05: return "**"
    if p<0.10: return "*"
    return ""

# ---------------------------------------------------------------------------
# 4. Build panels
# ---------------------------------------------------------------------------

# DOFE permits panel
dofe_panel = (dofe_dy.merge(inst, on=["dname","year"])
                     .merge(inst_dofe[["dname","year","fxshock_dofe"]], on=["dname","year"])
                     .merge(mi_dofe, on="dname", how="left")
                     .merge(region_sh, on="dname", how="left"))
dofe_panel["log_permits"] = np.log(dofe_panel.permits + 1)

# RVS district-year panel
if rvs_dist is not None:
    rvs_d_panel = (rvs_dist.merge(inst, on=["dname","year"])
                            .merge(inst_dofe[["dname","year","fxshock_dofe"]], on=["dname","year"])
                            .merge(mi_dofe, on="dname", how="left")
                            .merge(region_sh, on="dname", how="left"))
else:
    rvs_d_panel = None

# RVS HH-level panel (migrant-only)
if rvs_hh is not None:
    rvs_hh_panel = (rvs_hh.merge(inst, on=["dname","year"])
                          .merge(inst_dofe[["dname","year","fxshock_dofe"]], on=["dname","year"])
                          .merge(mi_dofe, on="dname", how="left")
                          .merge(region_sh, on="dname", how="left"))
    if "has_migrant_intl" in rvs_hh_panel.columns:
        rvs_hh_panel = rvs_hh_panel[rvs_hh_panel.has_migrant_intl == 1].copy()
        rvs_hh_panel["log_n_intl"] = np.log(rvs_hh_panel.n_intl_migrants + 1)
        rvs_hh_panel["log_remit"]  = np.log(rvs_hh_panel.remit_amount_intl_12m_rs + 1)
else:
    rvs_hh_panel = None

# RVS migrant-level panel
if rvs_mig is not None:
    rvs_mig_panel = (rvs_mig.merge(inst, on=["dname","year"])
                            .merge(inst_dofe[["dname","year","fxshock_dofe"]], on=["dname","year"])
                            .merge(mi_dofe, on="dname", how="left")
                            .merge(region_sh, on="dname", how="left"))
    if "is_international" in rvs_mig_panel.columns:
        rvs_mig_panel = rvs_mig_panel[rvs_mig_panel.is_international == 1].copy()
        rvs_mig_panel["log_remit"]        = np.log(rvs_mig_panel.remit_amount_rs.fillna(0)    + 1)
        rvs_mig_panel["log_earn_primary"] = np.log(rvs_mig_panel.earning_primary_rs.fillna(0) + 1)
else:
    rvs_mig_panel = None

# ---------------------------------------------------------------------------
# 5. Spec grid
# ---------------------------------------------------------------------------

SPEC_LABELS = {
    1: "M1 treat | dname FE",
    2: "M2 +fx_z main",
    3: "M3 +year FE",
    4: "M4 +C_mig",
    5: "M5 +C_fx",
    6: "M6 +C_X",
}

OUTCOMES = []
if dofe_panel is not None:
    OUTCOMES.append(("DOFE_permits",   "log_permits", dofe_panel,    2016))
if rvs_d_panel is not None:
    OUTCOMES += [
        ("RVS_d_n_intl",  "log_n_intl",  rvs_d_panel, 2017),
        ("RVS_d_share",   "share_hh",    rvs_d_panel, 2017),
        ("RVS_d_remit",   "log_remit",   rvs_d_panel, 2017),
    ]
if rvs_hh_panel is not None:
    OUTCOMES += [
        ("RVS_hh_n_intl", "log_n_intl",  rvs_hh_panel, 2017),
        ("RVS_hh_remit",  "log_remit",   rvs_hh_panel, 2017),
    ]
if rvs_mig_panel is not None:
    OUTCOMES += [
        ("RVS_mig_remit",  "log_remit",        rvs_mig_panel, 2017),
        ("RVS_mig_sent",   "remit_sent_flag",  rvs_mig_panel, 2017),
        ("RVS_mig_earn",   "log_earn_primary", rvs_mig_panel, 2017),
    ]

print(f"Outcomes to test: {[o[0] for o in OUTCOMES]}")

# HH/migrant panels use hhid as entity instead of dname
def entity_for(name):
    return "hhid" if name.startswith("RVS_hh_") or name.startswith("RVS_mig_") else "dname"

rows = []
for (name, outcome, panel, ref) in OUTCOMES:
    ec = entity_for(name)
    if ec == "hhid" and "hhid" not in panel.columns:
        print(f"  skip {name}: no hhid column")
        continue
    for vintage in ["V_2001", "V_DOFE"]:
        for lvl in range(1, 7):
            r = fit_stage(panel, outcome, vintage, lvl, ref, entity_col=ec)
            if r is None:
                continue
            if "err" in r:
                rows.append({"outcome":name,"y":outcome,"vintage":vintage,
                             "spec":SPEC_LABELS[lvl],
                             "beta":np.nan,"se":np.nan,"t":np.nan,"p":np.nan,
                             "sig":"","n":None,"r2_w":np.nan,"note":r["err"]})
            else:
                rows.append({"outcome":name,"y":outcome,"vintage":vintage,
                             "spec":SPEC_LABELS[lvl],
                             "beta":round(r["beta"],4),"se":round(r["se"],4),
                             "t":round(r["t"],2),"p":round(r["p"],4),
                             "sig":stars(r["p"]),"n":r["n"],
                             "r2_w":round(r["r2_w"],4),"note":""})

res = pd.DataFrame(rows)
out_path = "district-analysis/output/tab/first_stage_staggered.csv"
res.to_csv(out_path, index=False)
print(f"\nSaved: {out_path} ({len(res)} rows)\n")

# Print compact view per outcome
for (name, outcome, _, _) in OUTCOMES:
    sub = res[res.outcome == name]
    if sub.empty:
        continue
    pv = (sub.pivot_table(index="spec", columns="vintage",
                          values=["beta","t","sig","n"],
                          aggfunc="first"))
    # consistent column order
    cols = []
    for v in ["V_2001","V_DOFE"]:
        for stat in ["beta","t","sig","n"]:
            if (stat, v) in pv.columns:
                cols.append((stat, v))
    pv = pv[cols]
    print(f"\n=== {name} ({outcome}) ===")
    print(pv.to_string())
