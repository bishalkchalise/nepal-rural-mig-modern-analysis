"""
BHJ exposure first-stage at district x country x year (2011-2023).

Two baseline-share versions of the SSIV:
  v1 (2001):    share_dc from 2001 census migration  (dist_mig_pop_2001.csv)
  v2 (2009-10): share_dc from average DOFE permits in 2009-2010

Exposure shifter (built in the migrant's-eye direction):

  npr_per_lcu_{c,t}   = NPR you receive for 1 unit of country-c currency in year t
  rer_{c,t}           = log( npr_per_lcu_{c,t} / npr_per_lcu_{c,2010} )
                       = log NPR-per-LCU change since 2010, country c
                       ( > 0  =  NPR has depreciated since 2010, more NPR per LCU
                         < 0  =  NPR has appreciated, fewer NPR per LCU         )

  z_dct = share_dc(base) * rer_ct

  RER rises when migrants would receive more NPR for the same LCU earnings.
  Expected sign of beta on z_dct is POSITIVE.

Outcome:
  y_dct = log(dofe_permits_dct + 1)

Three regressions per version:
  A. Pooled 2011-2023 panel:  y_dct ~ z_dct + FE(district, country, year), cluster ~ dname
  B. Cross-section TOTAL:     log(sum_t permits_dct + 1) ~ share * mean_t(log_fx_ct), FE(d, c), cluster d
  C. Cross-section AVERAGE:   log(mean_t permits_dct + 1) ~ share * mean_t(log_fx_ct), FE(d, c), cluster d
  D. Year-wise cross-sections: separate beta_t for each year 2011-2023

Plot: yearwise beta_t with 95% CI, two colors (v1 vs v2).

No interaction with migration intensity in this first-stage probe (per user spec).
"""
import numpy as np, pandas as pd
import matplotlib.pyplot as plt
from linearmodels.panel import PanelOLS

# ------------------------------------------------------------------------------
# 1. Load & normalize
# ------------------------------------------------------------------------------

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

forex = pd.read_csv("district-analysis/data/clean/forex_2000_2023.csv").dropna(subset=["country"])
dofe_raw = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv").dropna(subset=["country"])
m01 = pd.read_csv("district-analysis/data/clean/dist_mig_pop_2001.csv")

# Build FX in migrant's-eye direction: NPR per LCU.
# Raw 'forex' table is LCU per USD.  So:
#   npr_per_usd_t  = forex_Nepal_t
#   lcu_per_usd_t  = forex_c_t
#   npr_per_lcu_ct = npr_per_usd_t / lcu_per_usd_t
nepal_fx = forex[forex.country == "Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
        .merge(nepal_fx, on="year")
        .query("country not in ['Nepal','India']")
        .assign(npr_per_lcu=lambda d: d.npr_per_usd / d.lcu_per_usd))
fx["log_npr_per_lcu"] = np.log(fx.npr_per_lcu)
fx = fx[["country","year","log_npr_per_lcu"]].dropna()

# Anchor each country's series to its 2010 value to get a clean depreciation index.
base2010 = (fx[fx.year == 2010].set_index("country")
              .log_npr_per_lcu.rename("base_2010"))
fx = fx.join(base2010, on="country").dropna(subset=["base_2010"])
fx["rer"] = fx["log_npr_per_lcu"] - fx["base_2010"]   # > 0 = NPR depreciated since 2010
fx = fx[["country","year","rer"]]

# DOFE annual permits at district x country x year
dofe = (dofe_raw.assign(dname=lambda d: d.district_rename.map(to_dname))
                .groupby(["dname","country","year"]).total_migrants.sum()
                .reset_index().rename(columns={"total_migrants":"permits"}))
dofe = dofe[~dofe.country.isin(["Nepal","India"])]

# 2001 census migration (already at district x country, India excluded by user)
m01 = m01.rename(columns={"dist_mig_pop_2001":"mig01"})

# Country intersection across all three
common = sorted(set(m01.country.unique()) & set(fx.country.unique()) & set(dofe.country.unique()))
print(f"Common countries (n={len(common)}): {common}\n")

m01  = m01[m01.country.isin(common)].copy()
fx   = fx[fx.country.isin(common)].copy()
dofe = dofe[dofe.country.isin(common)].copy()

# District intersection (74 in 2001 census, 75 in DOFE)
districts = sorted(set(m01.dname.unique()) & set(dofe.dname.unique()))
print(f"Common districts: {len(districts)}\n")
m01  = m01[m01.dname.isin(districts)].copy()
dofe = dofe[dofe.dname.isin(districts)].copy()

# ------------------------------------------------------------------------------
# 2. Build baseline shares (v1, v2)
# ------------------------------------------------------------------------------

# v1: 2001 census shares
sh01 = (m01.assign(tot=lambda d: d.groupby("dname").mig01.transform("sum"))
            .assign(share_v1=lambda d: d.mig01 / d.tot)
            [["dname","country","share_v1"]])

# v2: 2009-2010 average DOFE shares
sh11 = (dofe[dofe.year.isin([2009,2010])]
        .groupby(["dname","country"]).permits.sum()
        .reset_index()
        .assign(tot=lambda d: d.groupby("dname").permits.transform("sum"))
        .assign(share_v2=lambda d: d.permits / d.tot)
        [["dname","country","share_v2"]])

print(f"shares v1 (2001):    {len(sh01)} d-c rows")
print(f"shares v2 (2009-10): {len(sh11)} d-c rows\n")

# ------------------------------------------------------------------------------
# 3. Build d x c x y panel for 2011-2023 with balanced grid + log_fx
# ------------------------------------------------------------------------------

YRS = list(range(2011, 2024))
grid = pd.MultiIndex.from_product([districts, common, YRS],
                                  names=["dname","country","year"]).to_frame(index=False)
panel = (grid.merge(dofe, on=["dname","country","year"], how="left")
              .fillna({"permits":0})
              .merge(fx, on=["country","year"], how="left")
              .merge(sh01, on=["dname","country"], how="left")
              .merge(sh11, on=["dname","country"], how="left")
              .fillna({"share_v1":0., "share_v2":0.}))

panel["log_perm"] = np.log(panel.permits + 1)
panel["z_v1"] = panel.share_v1 * panel.rer
panel["z_v2"] = panel.share_v2 * panel.rer
panel = panel.dropna(subset=["rer"])

# Standardize the shifter for interpretability: 1-sd of share-weighted
# depreciation exposure.  Use the panel sd (across d x c x y) so that
# yearwise coefficients are comparable on the same scale.
SD_V1 = panel.z_v1.std(ddof=0)
SD_V2 = panel.z_v2.std(ddof=0)
panel["z_v1_std"] = panel.z_v1 / SD_V1
panel["z_v2_std"] = panel.z_v2 / SD_V2

print(f"Panel: {len(panel)} rows ({len(districts)} d x {len(common)} c x {len(YRS)} y)")
print(f"rer (log NPR/LCU change since 2010) range: "
      f"[{panel.rer.min():.3f}, {panel.rer.max():.3f}]  mean={panel.rer.mean():.3f}")
print(f"z_v1: mean={panel.z_v1.mean():.4f}, sd={SD_V1:.4f}")
print(f"z_v2: mean={panel.z_v2.mean():.4f}, sd={SD_V2:.4f}\n")

# ------------------------------------------------------------------------------
# 4. Regressions
# ------------------------------------------------------------------------------

def fit_panel(df, ycol, xcols, entity, time, cluster="dname"):
    """Two-way FE panel OLS clustered by entity."""
    d = df.dropna(subset=[ycol]+xcols).copy()
    d["_ent"] = d[entity].astype("category").cat.codes
    d["_tim"] = d[time].astype("category").cat.codes if isinstance(time, str) else None
    idx = pd.MultiIndex.from_arrays(
        [d[entity].astype(str)+"_"+d.country.astype(str), d.year.values],
        names=["dc","year"])
    X = d[xcols].copy(); X.index = idx
    y = d[ycol].copy(); y.index = idx
    m = PanelOLS(y, X, entity_effects=False, time_effects=False, drop_absorbed=True)
    # Manual absorption via dummies for d and c; year FE included separately
    return m

def fit_dummy_fe(df, ycol, xcols, fe_cols, cluster_col):
    """Manually demean via dummy regressors using statsmodels for clustered SE."""
    import statsmodels.api as sm
    d = df.dropna(subset=[ycol]+xcols+fe_cols+[cluster_col]).copy()
    # Build dummies for FE
    dummies = pd.concat([pd.get_dummies(d[c], prefix=c, drop_first=True, dtype=float)
                         for c in fe_cols], axis=1)
    X = pd.concat([d[xcols], dummies], axis=1)
    X = sm.add_constant(X, has_constant="add")
    m = sm.OLS(d[ycol].values, X.values.astype(float)).fit(
        cov_type="cluster", cov_kwds={"groups": d[cluster_col].values})
    out = {}
    for c in xcols:
        i = X.columns.get_loc(c)
        b  = float(m.params[i]); se = float(m.bse[i]); p = float(m.pvalues[i])
        out[c] = {"beta": b, "se": se, "t": b/se if se>0 else np.nan, "p": p,
                  "ci_lo": b - 1.96*se, "ci_hi": b + 1.96*se}
    out["n"] = int(m.nobs)
    return out

def stars(p):
    if p is None or np.isnan(p): return ""
    return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

# --- A. Pooled 2011-2023 with district, country, year FE
print("=" * 78)
print("A. POOLED 2011-2023, log(permits+1) on z_dct, FE(d,c,y), cluster ~ dname")
print("=" * 78)
pooled_rows = []
for ver, zcol in [("v1_2001", "z_v1_std"), ("v2_2009_10", "z_v2_std")]:
    r = fit_dummy_fe(panel, "log_perm", [zcol],
                     fe_cols=["dname","country","year"], cluster_col="dname")
    info = r[zcol]
    info.update({"version": ver, "n": r["n"]})
    pooled_rows.append(info)
    print(f"  {ver:<12}  beta = {info['beta']:>8.4f}  se = {info['se']:.4f}  "
          f"t = {info['t']:>6.2f}  p = {info['p']:.4f} {stars(info['p'])}  "
          f"n = {info['n']}")
pooled_df = pd.DataFrame(pooled_rows)[["version","beta","se","t","p","ci_lo","ci_hi","n"]]
pooled_df.to_csv("district-analysis/output/tab/dxc_first_stage_pooled.csv", index=False)

# --- B/C. Cross-section TOTAL and AVERAGE
print("\n" + "=" * 78)
print("B/C. CROSS-SECTION (collapse over 2011-2023): TOTAL and AVERAGE permits")
print("=" * 78)
mean_rer = (fx[fx.year.between(2011,2023)].groupby("country").rer.mean()
                .rename("mean_rer").reset_index())
cs_base = (panel.groupby(["dname","country"])
                .agg(perm_total=("permits","sum"), perm_mean=("permits","mean"))
                .reset_index()
                .merge(sh01, on=["dname","country"], how="left")
                .merge(sh11, on=["dname","country"], how="left")
                .merge(mean_rer, on="country", how="left")
                .fillna({"share_v1":0., "share_v2":0.}))
cs_base["log_total"] = np.log(cs_base.perm_total + 1)
cs_base["log_mean"]  = np.log(cs_base.perm_mean  + 1)
cs_base["z_v1"]      = cs_base.share_v1 * cs_base.mean_rer
cs_base["z_v2"]      = cs_base.share_v2 * cs_base.mean_rer
# Standardize using the cross-section's own sd (d x c, not d x c x y)
cs_base["z_v1_std"]  = cs_base.z_v1 / cs_base.z_v1.std(ddof=0)
cs_base["z_v2_std"]  = cs_base.z_v2 / cs_base.z_v2.std(ddof=0)

cs_rows = []
for outcome in ["log_total", "log_mean"]:
    for ver, zcol in [("v1_2001","z_v1_std"), ("v2_2009_10","z_v2_std")]:
        r = fit_dummy_fe(cs_base, outcome, [zcol],
                         fe_cols=["dname","country"], cluster_col="dname")
        info = r[zcol]
        info.update({"outcome": outcome, "version": ver, "n": r["n"]})
        cs_rows.append(info)
        print(f"  {outcome:<10}  {ver:<12}  beta = {info['beta']:>8.4f}  "
              f"se = {info['se']:.4f}  t = {info['t']:>6.2f}  "
              f"p = {info['p']:.4f} {stars(info['p'])}  n = {info['n']}")
cs_df = pd.DataFrame(cs_rows)[["outcome","version","beta","se","t","p","ci_lo","ci_hi","n"]]
cs_df.to_csv("district-analysis/output/tab/dxc_first_stage_crosssection.csv", index=False)

# --- D. Year-by-year cross-section
print("\n" + "=" * 78)
print("D. YEAR-WISE cross-section: log(perm_dct+1) on z_dct, FE(d,c), cluster d")
print("=" * 78)
yw_rows = []
for y in YRS:
    sub = panel[panel.year == y].copy()
    for ver, zcol in [("v1_2001","z_v1_std"), ("v2_2009_10","z_v2_std")]:
        r = fit_dummy_fe(sub, "log_perm", [zcol],
                         fe_cols=["dname","country"], cluster_col="dname")
        info = r[zcol]
        info.update({"year": y, "version": ver, "n": r["n"]})
        yw_rows.append(info)
        print(f"  {y}  {ver:<12}  beta = {info['beta']:>8.4f}  "
              f"se = {info['se']:.4f}  t = {info['t']:>6.2f}  "
              f"p = {info['p']:.4f} {stars(info['p'])}  n = {info['n']}")
yw_df = pd.DataFrame(yw_rows)[["year","version","beta","se","t","p","ci_lo","ci_hi","n"]]
yw_df.to_csv("district-analysis/output/tab/dxc_first_stage_yearwise.csv", index=False)

# ------------------------------------------------------------------------------
# 5. Plot yearwise coefficients with 95% CI, both versions overlaid
# ------------------------------------------------------------------------------
import os
os.makedirs("district-analysis/output/fig", exist_ok=True)

fig, ax = plt.subplots(figsize=(8, 5))
colors = {"v1_2001": "#1f77b4", "v2_2009_10": "#d62728"}
labels = {"v1_2001": "v1: 2001 census shares", "v2_2009_10": "v2: 2009-10 DOFE shares"}
offsets = {"v1_2001": -0.1, "v2_2009_10": 0.1}
for ver in ["v1_2001", "v2_2009_10"]:
    sub = yw_df[yw_df.version == ver].sort_values("year").reset_index(drop=True)
    xs = sub.year + offsets[ver]
    ax.errorbar(xs, sub.beta,
                yerr=[sub.beta - sub.ci_lo, sub.ci_hi - sub.beta],
                fmt="o", capsize=3, capthick=1.2, elinewidth=1.2, markersize=6,
                color=colors[ver], label=labels[ver])

ax.axhline(0, color="black", linewidth=0.8, linestyle="--", alpha=0.6)
ax.set_xlabel("Year", fontsize=11)
ax.set_ylabel(r"$\beta$ on standardized $z_{dct}$  (log permits per 1-sd of exposure shock)",
              fontsize=11)
ax.set_title("Yearwise BHJ first-stage: log(DOFE permits + 1) on 1-sd of share x $\\Delta$log(NPR/LCU)$_{c,t}$\n"
             "district + country FE, cluster ~ dname, 95% CI", fontsize=11)
ax.legend(loc="best", frameon=True, fontsize=10)
ax.set_xticks(YRS)
ax.grid(True, axis="y", linestyle=":", alpha=0.4)
plt.tight_layout()
plt.savefig("district-analysis/output/fig/dxc_first_stage_yearwise.png", dpi=160)
print("\nSaved: output/fig/dxc_first_stage_yearwise.png")
print("Saved: output/tab/dxc_first_stage_pooled.csv")
print("Saved: output/tab/dxc_first_stage_crosssection.csv")
print("Saved: output/tab/dxc_first_stage_yearwise.csv")
