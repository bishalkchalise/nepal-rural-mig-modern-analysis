"""
Same staggered first-stage ladder as before, but with fxshock built in the
Khanna direction (LCU per NPR -- falls as NPR depreciates).

We rebuild fxshock from scratch in Python for both vintages, mirroring
instrument.R / instrument_dofe.R but with the LCU/NPR sign convention.

  fx_to_npr_LCU(c, t) = LCU_per_USD(c, t) / NPR_per_USD(t)        # Khanna
  fx_index(c, t)      = fx_to_npr_LCU(c, t) / fx_to_npr_LCU(c, baseline_year)

  fxshock_d,t = sum_c  share_dc(baseline_year)  *  fx_index(c, t)

For 2001 vintage   : baseline_year = 2001, shares from 2001 census
For DOFE  vintage  : baseline_year = 2009.5 (mid 2009-10), shares from DOFE 2009-10

Then run the same six-rung spec ladder on log(DOFE permits + 1).
"""

import os, numpy as np, pandas as pd
from linearmodels.panel import PanelOLS

# ---------------------------------------------------------------------------
# 1. Build LCU/NPR fxshock from raw inputs
# ---------------------------------------------------------------------------

forex     = pd.read_csv("district-analysis/data/clean/forex_2000_2023.csv")
dofe      = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")
inst_old  = pd.read_csv("district-analysis/data/clean/instrument/instrument_forex_dist.csv")  # need 2001 shares
region_sh = pd.read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv")

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

# Forex raw: build LCU/NPR for each (country, year). country=Nepal gives NPR/USD.
nepal_fx = (forex[forex.country == "Nepal"][["year","forex"]]
            .rename(columns={"forex":"npr_per_usd"}))

fx_lcu = (forex.rename(columns={"forex":"lcu_per_usd"})
                [["country","year","lcu_per_usd"]]
                .merge(nepal_fx, on="year", how="inner")
                .query("country != 'Nepal' & country != 'India'")
                .assign(fx_lcu_per_npr = lambda d: d.lcu_per_usd / d.npr_per_usd)
                [["country","year","fx_lcu_per_npr"]])

def fx_index_baseline(fx, baseline_years):
    """fx is (country, year, fx_lcu_per_npr). Index to mean over baseline_years."""
    base = (fx[fx.year.isin(baseline_years)]
              .groupby("country").fx_lcu_per_npr.mean()
              .rename("base").reset_index())
    out  = (fx.merge(base, on="country", how="inner")
              .assign(fx_index=lambda d: d.fx_lcu_per_npr / d.base)
              [["country","year","fx_index"]])
    return out

# 2001 baseline
fx_idx_2001 = fx_index_baseline(fx_lcu, [2001])
# 2009-2010 baseline
fx_idx_dofe = fx_index_baseline(fx_lcu, [2009, 2010])

# DOFE district x country panel (year aggregated)
dofe_dc = (dofe.assign(dname = lambda d: d.district_rename.map(to_dname))
                .query("country != 'India' & country != 'Nepal'"))

# 2001-vintage shares: we don't have raw 2001 census shares per country here on
# cloud (those live in the migrant micro-data). Approx: just use DOFE 2001's
# share (DOFE doesn't cover 2001 -- so fall back to using 2009 as the 2001
# proxy). Better: read existing instrument's geog-level info and fold in.
# For a clean Khanna comparison let's use ONE vintage at a time:
#   V_DOFE -- shares from DOFE 2009-10 sums (we have this)
#   V_2001 -- skipped here; would need raw 2001 census micro

shares_dofe_0910 = (dofe_dc[dofe_dc.year.isin([2009,2010])]
                    .groupby(["dname","country"]).total_migrants.sum().reset_index())
totals = shares_dofe_0910.groupby("dname").total_migrants.sum().rename("total")
shares_dofe_0910 = (shares_dofe_0910.merge(totals, on="dname")
                    .assign(share = lambda d: d.total_migrants / d.total)
                    [["dname","country","share"]])

# Build fxshock_LCU per (district, year) using DOFE 2009-10 shares + 2001 base
def build_fxshock(shares, fx_idx):
    merged = shares.merge(fx_idx, on="country", how="inner")
    out = (merged.assign(contrib = lambda d: d.share * d.fx_index)
                  .groupby(["dname","year"]).contrib.sum()
                  .rename("fxshock_LCU").reset_index())
    return out

fxshock_LCU_DOFE_2001base  = build_fxshock(shares_dofe_0910, fx_idx_2001)   # DOFE shares, 2001 baseline
fxshock_LCU_DOFE_DOFEbase  = build_fxshock(shares_dofe_0910, fx_idx_dofe)   # DOFE shares, 2009-10 baseline

print(f"fxshock_LCU panels: 2001-base = {len(fxshock_LCU_DOFE_2001base)}  "
      f"DOFE-base = {len(fxshock_LCU_DOFE_DOFEbase)}")

# ---------------------------------------------------------------------------
# 2. Build mig_int (DOFE 2009-10 / pop_2011)
# ---------------------------------------------------------------------------

dofe_dy = (dofe.groupby(["district_rename","year"]).total_migrants.sum().reset_index()
               .assign(dname=lambda d: d.district_rename.map(to_dname))
               .rename(columns={"total_migrants":"permits"})[["dname","year","permits"]])

mi_dofe = (dofe_dy[dofe_dy.year.isin([2009,2010])]
           .groupby("dname").permits.mean().rename("num").reset_index())

pop_2011 = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
                    [["dname","district_population_2011"]]
                    .drop_duplicates("dname")
                    .rename(columns={"district_population_2011":"pop_2011"}))

mi_dofe = (mi_dofe.merge(pop_2011, on="dname")
                  .assign(mig_int_dofe=lambda d: d.num/d.pop_2011)
                  [["dname","mig_int_dofe"]])

# ---------------------------------------------------------------------------
# 3. Build outcome panel and merge fxshock_LCU
# ---------------------------------------------------------------------------

panel_DOFEbase = (dofe_dy
                  .merge(fxshock_LCU_DOFE_DOFEbase, on=["dname","year"])
                  .merge(mi_dofe, on="dname", how="left")
                  .merge(region_sh, on="dname", how="left"))
panel_2001base = (dofe_dy
                  .merge(fxshock_LCU_DOFE_2001base, on=["dname","year"])
                  .merge(mi_dofe, on="dname", how="left")
                  .merge(region_sh, on="dname", how="left"))

for p in [panel_DOFEbase, panel_2001base]:
    p["log_permits"] = np.log(p.permits + 1)

print(f"panel_DOFEbase rows: {len(panel_DOFEbase)}  panel_2001base rows: {len(panel_2001base)}")

# Direction sanity check
print("\nMean fxshock_LCU by year (DOFE-base):")
print(panel_DOFEbase.groupby("year").fxshock_LCU.mean().round(4).to_string())
print("\nMean fxshock_LCU by year (2001-base):")
print(panel_2001base.groupby("year").fxshock_LCU.mean().round(4).to_string())

# ---------------------------------------------------------------------------
# 4. Staggered ladder
# ---------------------------------------------------------------------------

def z(s):
    s = pd.Series(s); sd = s.std()
    return (s - s.mean())/sd if sd>0 else pd.Series(0., index=s.index)

def yr_inter(df, col, prefix, ref):
    out = pd.DataFrame(index=df.index)
    for y in sorted(df.year.unique()):
        if y == ref: continue
        out[f"{prefix}_y{y}"] = df[col] * (df.year == y).astype(float)
    return out

REGION_COLS = ["share_e_asia","share_gulf","share_oecd_north",
               "share_oecd_europe","share_s_asia","share_se_asia"]

def fit_stage(panel, level, ref=2016):
    d = panel.dropna(subset=["fxshock_LCU","mig_int_dofe","log_permits"]).copy()
    d["fx_z"]      = z(d.fxshock_LCU)
    d["log_mi_z"]  = z(np.log(d.mig_int_dofe.clip(lower=1e-12)))
    d["treatment"] = d.fx_z * d.log_mi_z

    cols = ["treatment"]
    extras = []
    if level >= 2: cols.append("fx_z")
    if level >= 4: extras.append(yr_inter(d, "log_mi_z", "cmig", ref))
    if level >= 5: extras.append(yr_inter(d, "fx_z",     "cfx",  ref))
    if level >= 6:
        for c in REGION_COLS:
            if c in d.columns: extras.append(yr_inter(d, c, f"cX_{c}", ref))

    exog = pd.concat([d[cols]] + extras, axis=1) if extras else d[cols]
    y    = d.log_permits
    idx  = pd.MultiIndex.from_arrays([d["dname"].values, d.year.values], names=["dname","year"])
    exog.index = idx; y.index = idx
    m = PanelOLS(y, exog, entity_effects=True,
                 time_effects=(level >= 3), drop_absorbed=True)
    r = m.fit(cov_type="clustered", cluster_entity=True)
    return float(r.params["treatment"]), float(r.tstats["treatment"]), float(r.pvalues["treatment"]), int(r.nobs)

def stars(p):
    return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

LBL = {1:"M1 treat | dname FE", 2:"M2 +fx_z main", 3:"M3 +year FE",
       4:"M4 +C_mig", 5:"M5 +C_fx", 6:"M6 +C_X"}

print("\n" + "="*100)
print("DOFE permits first-stage with KHANNA-direction fxshock (LCU per NPR)")
print("Treatment: z(fxshock_LCU) * z(log(mig_int_dofe))")
print("="*100)

print(f"\n{'spec':<22} {'2001-baseline':<26} {'DOFE-baseline':<26}")
print("-"*72)
rows = []
for lvl in range(1,7):
    b1,t1,p1,n1 = fit_stage(panel_2001base, lvl)
    b2,t2,p2,n2 = fit_stage(panel_DOFEbase, lvl)
    print(f"{LBL[lvl]:<22} beta={b1:>8.4f} t={t1:>6.2f} {stars(p1):<3}  "
                              f"beta={b2:>8.4f} t={t2:>6.2f} {stars(p2):<3}")
    rows.append({"spec":LBL[lvl], "baseline":"2001",  "beta":round(b1,4),"t":round(t1,2),"p":round(p1,4),"sig":stars(p1),"n":n1})
    rows.append({"spec":LBL[lvl], "baseline":"DOFE",  "beta":round(b2,4),"t":round(t2,2),"p":round(p2,4),"sig":stars(p2),"n":n2})

pd.DataFrame(rows).to_csv("district-analysis/output/tab/first_stage_staggered_khanna.csv", index=False)
print("\nSaved: output/tab/first_stage_staggered_khanna.csv")
