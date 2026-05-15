"""
BHJ exposure first-stage at district x country x year (2011-2023).

Two baseline-share versions, each on its own natural destination set:
  v1 (2001):    share_dc from 2001 census migration
                destination set = 2001 census destinations (~20)
  v2 (2009-10): share_dc from average DOFE permits 2009-2010
                destination set = DOFE destinations with >= 50 permits in 2009-10

For a fair head-to-head, we run them on a UNIFIED outcome panel: the union
of both destination sets (intersected with the FX panel).  Each share is
normalized over ITS OWN base (so total share per district = 1 within v1's
base, and = 1 within v2's base).  Destinations not in a version's base get
share = 0.

Exposure shifter:
  rer_ct = log(NPR per LCU)_t  -  log(NPR per LCU)_c,2010
           > 0 = NPR depreciated since 2010
  z_dct  = share_dc(base) * rer_ct

Outcome: y_dct = log(DOFE permits_dct + 1)
SE: clustered at dname.
"""
import os, numpy as np, pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm

# ------------------------------------------------------------------------------
# 1. Load
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

# FX in migrant's-eye direction: NPR per LCU, anchored to 2010
nepal_fx = forex[forex.country == "Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
        .merge(nepal_fx, on="year")
        .query("country not in ['Nepal','India']")
        .assign(npr_per_lcu=lambda d: d.npr_per_usd / d.lcu_per_usd))
fx["log_npr_per_lcu"] = np.log(fx.npr_per_lcu)
fx = fx[["country","year","log_npr_per_lcu"]].dropna()
base2010 = (fx[fx.year == 2010].set_index("country")
              .log_npr_per_lcu.rename("base_2010"))
fx = fx.join(base2010, on="country").dropna(subset=["base_2010"])
fx["rer"] = fx["log_npr_per_lcu"] - fx["base_2010"]
fx_countries = set(fx.country.unique())

# DOFE long: dname x country x year
dofe = (dofe_raw.assign(dname=lambda d: d.district_rename.map(to_dname))
                .groupby(["dname","country","year"]).total_migrants.sum()
                .reset_index().rename(columns={"total_migrants":"permits"}))
dofe = dofe[~dofe.country.isin(["Nepal","India"])]

# 2001 census migration (district x country, India already excluded)
m01 = m01.rename(columns={"dist_mig_pop_2001":"mig01"})

# ------------------------------------------------------------------------------
# 2. Define destination sets
# ------------------------------------------------------------------------------
set_v1 = sorted(set(m01.country.unique()) & fx_countries)
# v2: ALL 2009-10 DOFE destinations with positive permits AND in FX (no threshold)
do2 = dofe[dofe.year.isin([2009,2010])]
v2_totals = do2.groupby("country").permits.sum()
set_v2 = sorted(set(v2_totals[v2_totals > 0].index) & fx_countries)

# Unified outcome panel = union of v1 and v2 destination sets
dest = sorted(set(set_v1) | set(set_v2))
print(f"v1 destination set (2001 census ∩ FX, n={len(set_v1)}):\n  {set_v1}")
print(f"\nv2 destination set (DOFE>=50 in 2009-10 ∩ FX, n={len(set_v2)}):\n  {set_v2}")
print(f"\nUnion (outcome panel) n={len(dest)}: \n  {dest}\n")
in_v1_only = sorted(set(set_v1) - set(set_v2))
in_v2_only = sorted(set(set_v2) - set(set_v1))
print(f"  v1-only (no DOFE flow):  {in_v1_only}")
print(f"  v2-only (not in 2001):   {in_v2_only}\n")

# Districts common to 2001 census and DOFE
districts = sorted(set(m01.dname.unique()) & set(dofe.dname.unique()))
print(f"Common districts: {len(districts)}\n")
m01 = m01[m01.dname.isin(districts)].copy()
dofe = dofe[dofe.dname.isin(districts)].copy()

# ------------------------------------------------------------------------------
# 3. Shares (each normalized to ITS OWN base, then merged for unified panel)
# ------------------------------------------------------------------------------
sh01 = (m01[m01.country.isin(set_v1)]
        .assign(tot=lambda d: d.groupby("dname").mig01.transform("sum"))
        .assign(share_v1=lambda d: d.mig01 / d.tot)
        [["dname","country","share_v1"]])

sh11 = (dofe[dofe.year.isin([2009,2010]) & dofe.country.isin(set_v2)]
        .groupby(["dname","country"]).permits.sum().reset_index()
        .assign(tot=lambda d: d.groupby("dname").permits.transform("sum"))
        .assign(share_v2=lambda d: d.permits / d.tot)
        [["dname","country","share_v2"]])

# ------------------------------------------------------------------------------
# 4. Build unified d x c x y panel for 2011-2023
# ------------------------------------------------------------------------------
YRS = list(range(2011, 2024))
grid = pd.MultiIndex.from_product([districts, dest, YRS],
                                  names=["dname","country","year"]).to_frame(index=False)
panel = (grid.merge(dofe, on=["dname","country","year"], how="left")
              .fillna({"permits":0})
              .merge(fx[["country","year","rer"]], on=["country","year"], how="left")
              .merge(sh01, on=["dname","country"], how="left")
              .merge(sh11, on=["dname","country"], how="left")
              .fillna({"share_v1":0., "share_v2":0.}))
panel["log_perm"] = np.log(panel.permits + 1)
panel["z_v1"]     = panel.share_v1 * panel.rer
panel["z_v2"]     = panel.share_v2 * panel.rer
panel = panel.dropna(subset=["rer"])

SD_V1 = panel.z_v1.std(ddof=0)
SD_V2 = panel.z_v2.std(ddof=0)
panel["z_v1_std"] = panel.z_v1 / SD_V1
panel["z_v2_std"] = panel.z_v2 / SD_V2

print(f"Panel: {len(panel)} rows ({len(districts)} d x {len(dest)} c x {len(YRS)} y)")
print(f"rer range: [{panel.rer.min():.3f}, {panel.rer.max():.3f}]")
print(f"z_v1 sd={SD_V1:.4f}, z_v2 sd={SD_V2:.4f}\n")

# ------------------------------------------------------------------------------
# 5. Regressions
# ------------------------------------------------------------------------------
def fit_dummy_fe(df, ycol, xcols, fe_cols, cluster_col):
    d = df.dropna(subset=[ycol]+xcols+fe_cols+[cluster_col]).copy()
    dummies = pd.concat([pd.get_dummies(d[c], prefix=c, drop_first=True, dtype=float)
                         for c in fe_cols], axis=1)
    X = pd.concat([d[xcols].astype(float).reset_index(drop=True),
                   dummies.reset_index(drop=True)], axis=1)
    X = sm.add_constant(X, has_constant="add")
    m = sm.OLS(d[ycol].values.astype(float), X.values.astype(float)).fit(
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

# --- A. Pooled
print("="*78); print("A. POOLED 2011-23, FE(d,c,y), cluster ~dname (standardized z)"); print("="*78)
pooled = []
for ver, z in [("v1_2001","z_v1_std"), ("v2_2009_10","z_v2_std")]:
    r = fit_dummy_fe(panel, "log_perm", [z], ["dname","country","year"], "dname")
    info = r[z]; info.update({"version":ver, "n":r["n"]})
    pooled.append(info)
    print(f"  {ver:<12}  beta={info['beta']:>7.4f}  se={info['se']:.4f}  "
          f"t={info['t']:>6.2f}  p={info['p']:.4f} {stars(info['p'])}  n={info['n']}")
pd.DataFrame(pooled)[["version","beta","se","t","p","ci_lo","ci_hi","n"]] \
  .to_csv("district-analysis/output/tab/dxc_first_stage_pooled.csv", index=False)

# --- B/C. Cross-section TOTAL and AVERAGE
print("\n"+"="*78); print("B/C. CROSS-SECTION (collapse over 2011-23)"); print("="*78)
mean_rer = (fx[fx.year.between(2011,2023)].groupby("country").rer.mean()
              .rename("mean_rer").reset_index())
cs = (panel.groupby(["dname","country"])
            .agg(perm_total=("permits","sum"), perm_mean=("permits","mean"))
            .reset_index()
            .merge(sh01, on=["dname","country"], how="left")
            .merge(sh11, on=["dname","country"], how="left")
            .merge(mean_rer, on="country", how="left")
            .fillna({"share_v1":0., "share_v2":0.}))
cs["log_total"] = np.log(cs.perm_total + 1)
cs["log_mean"]  = np.log(cs.perm_mean + 1)
cs["z_v1"] = cs.share_v1 * cs.mean_rer
cs["z_v2"] = cs.share_v2 * cs.mean_rer
cs["z_v1_std"] = cs.z_v1 / cs.z_v1.std(ddof=0)
cs["z_v2_std"] = cs.z_v2 / cs.z_v2.std(ddof=0)
cs_rows = []
for outcome in ["log_total","log_mean"]:
    for ver, z in [("v1_2001","z_v1_std"), ("v2_2009_10","z_v2_std")]:
        r = fit_dummy_fe(cs, outcome, [z], ["dname","country"], "dname")
        info = r[z]; info.update({"outcome":outcome,"version":ver,"n":r["n"]})
        cs_rows.append(info)
        print(f"  {outcome:<10}  {ver:<12}  beta={info['beta']:>7.4f}  "
              f"se={info['se']:.4f}  t={info['t']:>6.2f}  p={info['p']:.4f} {stars(info['p'])}  n={info['n']}")
pd.DataFrame(cs_rows)[["outcome","version","beta","se","t","p","ci_lo","ci_hi","n"]] \
  .to_csv("district-analysis/output/tab/dxc_first_stage_crosssection.csv", index=False)

# --- D. Year-by-year
print("\n"+"="*78); print("D. YEAR-WISE (standardized z, district + country FE)"); print("="*78)
yw_rows = []
for y in YRS:
    sub = panel[panel.year == y].copy()
    for ver, z in [("v1_2001","z_v1_std"), ("v2_2009_10","z_v2_std")]:
        r = fit_dummy_fe(sub, "log_perm", [z], ["dname","country"], "dname")
        info = r[z]; info.update({"year":y, "version":ver, "n":r["n"]})
        yw_rows.append(info)
yw_df = pd.DataFrame(yw_rows)[["year","version","beta","se","t","p","ci_lo","ci_hi","n"]]
yw_df.to_csv("district-analysis/output/tab/dxc_first_stage_yearwise.csv", index=False)

for _, r in yw_df.iterrows():
    print(f"  {int(r.year)}  {r.version:<12}  beta={r.beta:>7.4f}  se={r.se:.4f}  "
          f"t={r.t:>6.2f}  p={r.p:.4f} {stars(r.p)}  n={int(r.n)}")

# ------------------------------------------------------------------------------
# 6. Plot 2014-2023
# ------------------------------------------------------------------------------
os.makedirs("district-analysis/output/fig", exist_ok=True)
PLOT_YRS = [y for y in YRS if y >= 2014]
fig, ax = plt.subplots(figsize=(8, 5))
colors = {"v1_2001":"#1f77b4", "v2_2009_10":"#d62728"}
labels = {"v1_2001": f"v1: 2001 census shares ({len(set_v1)} dest)",
          "v2_2009_10": f"v2: 2009-10 DOFE shares (>=50 permits, {len(set_v2)} dest)"}
offsets = {"v1_2001":-0.1, "v2_2009_10":0.1}
for ver in ["v1_2001","v2_2009_10"]:
    sub = (yw_df[(yw_df.version==ver) & (yw_df.year.isin(PLOT_YRS))]
           .sort_values("year").reset_index(drop=True))
    xs = sub.year + offsets[ver]
    ax.errorbar(xs, sub.beta,
                yerr=[sub.beta-sub.ci_lo, sub.ci_hi-sub.beta],
                fmt="o", capsize=3, capthick=1.2, elinewidth=1.2, markersize=6,
                color=colors[ver], label=labels[ver])
ax.axhline(0, color="black", linewidth=0.8, linestyle="--", alpha=0.6)
ax.set_xlabel("Year", fontsize=11)
ax.set_ylabel(r"$\beta$ on standardized $z_{dct}$  (log permits per 1-sd of exposure shock)", fontsize=11)
ax.set_title("Yearwise BHJ first-stage (2014-2023): natural destination sets per version\n"
             "district + country FE, cluster ~ dname, 95% CI", fontsize=11)
ax.legend(loc="best", frameon=True, fontsize=10)
ax.set_xticks(PLOT_YRS)
ax.grid(True, axis="y", linestyle=":", alpha=0.4)
plt.tight_layout()
plt.savefig("district-analysis/output/fig/dxc_first_stage_yearwise.png", dpi=160)
print("\nSaved figure + 3 tables.")
