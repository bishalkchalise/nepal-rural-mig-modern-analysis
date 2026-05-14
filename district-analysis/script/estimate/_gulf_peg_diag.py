"""
Diagnose the consequence of Gulf currencies being USD-pegged for the SSIV
design.

  1. Correlation of FX-index time-series across top destinations
     -> Confirms Gulf countries (Qatar, Saudi, UAE, Kuwait, Bahrain, Oman)
        all move in lockstep with USD-vs-NPR.

  2. Variance decomposition of fxshock_d,t :
     how much is year-common  (alpha_t)
     vs district-specific     (alpha_d, alpha_dt)
     vs residual?
     If most of the variance is year-common, the SSIV has limited cross-
     district identifying variation -- the C_mig over-absorption we saw
     in D1 is a direct consequence of the Gulf peg.

  3. Recompute fxshock DROPPING the Gulf countries and re-run M4-M6.
     What's the SSIV like once the dominant pegged destinations are out?
"""

import os, numpy as np, pandas as pd
import matplotlib.pyplot as plt
from linearmodels.panel import PanelOLS

os.makedirs("district-analysis/output/fig", exist_ok=True)
os.makedirs("district-analysis/output/tab", exist_ok=True)

# ---------------- load ------------------------------------------------------

forex     = pd.read_csv("district-analysis/data/clean/forex_2000_2023.csv")
dofe      = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

# Build NPR-per-LCU FX index (NPR/LCU direction; rises with NPR depreciation)
nepal_fx = forex[forex.country=="Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
            .merge(nepal_fx, on="year")
            .query("country not in ['Nepal','India']")
            .assign(fx_npr_per_lcu=lambda d: d.npr_per_usd/d.lcu_per_usd)
            [["country","year","fx_npr_per_lcu"]])

# Index to 2009-10 mean
base = fx[fx.year.isin([2009,2010])].groupby("country").fx_npr_per_lcu.mean().rename("base").reset_index()
fx = fx.merge(base, on="country").assign(fx_index=lambda d: d.fx_npr_per_lcu/d.base)[["country","year","fx_index"]]

# ---------------- 1. correlation of top destinations ------------------------

TOP_DEST_REQ = ["Qatar","Malaysia","Saudi Arabia","United Arab Emirates","Kuwait",
                "Bahrain","Oman","Japan","Romania","South Korea","Cyprus","Israel"]
GULF = {"Qatar","Saudi Arabia","United Arab Emirates","Kuwait","Bahrain","Oman"}

fx_wide = fx[fx.country.isin(TOP_DEST_REQ)].pivot(index="year", columns="country", values="fx_index")
TOP_DEST = [c for c in TOP_DEST_REQ if c in fx_wide.columns]   # restrict to those with FX data
missing = sorted(set(TOP_DEST_REQ) - set(TOP_DEST))
if missing:
    print(f"FX data missing for: {missing}.  Dropping from correlation heatmap.")
corr = fx_wide[TOP_DEST].corr(method="pearson").round(3)

fig, ax = plt.subplots(figsize=(9, 8))
im = ax.imshow(corr.values, cmap="RdBu_r", vmin=-1, vmax=1)
ax.set_xticks(range(len(TOP_DEST))); ax.set_xticklabels(TOP_DEST, rotation=60, ha="right", fontsize=9)
ax.set_yticks(range(len(TOP_DEST))); ax.set_yticklabels(TOP_DEST, fontsize=9)
for i in range(len(TOP_DEST)):
    for j in range(len(TOP_DEST)):
        v = corr.values[i, j]
        ax.text(j, i, f"{v:.2f}", ha="center", va="center",
                color="white" if abs(v) > 0.6 else "black", fontsize=7)
ax.set_title("Pairwise correlation of NPR/LCU FX-index time-series across top destinations",
             fontweight="bold")
fig.colorbar(im, ax=ax, shrink=0.7, label="Pearson corr")
plt.tight_layout()
plt.savefig("district-analysis/output/fig/dofe_fx_index_correlation.png", dpi=140, bbox_inches="tight")
plt.close()

# Mean within-Gulf correlation vs Gulf-vs-non-Gulf
gulf_list = [c for c in TOP_DEST if c in GULF]
nongulf   = [c for c in TOP_DEST if c not in GULF]
within_gulf  = corr.loc[gulf_list, gulf_list].values
within_gulf  = within_gulf[np.triu_indices_from(within_gulf, k=1)]
within_ng    = corr.loc[nongulf, nongulf].values
within_ng    = within_ng[np.triu_indices_from(within_ng, k=1)]
cross        = corr.loc[gulf_list, nongulf].values.flatten()

print("=" * 70)
print("FX-index correlation summary across top-12 destinations")
print("=" * 70)
print(f"  Within Gulf  ({len(gulf_list)} countries): mean = {within_gulf.mean():.3f},  min = {within_gulf.min():.3f}")
print(f"  Within non-Gulf ({len(nongulf)} cn):       mean = {within_ng.mean():.3f},  min = {within_ng.min():.3f}")
print(f"  Gulf vs non-Gulf:                          mean = {cross.mean():.3f}")
print("  -> Gulf currencies move essentially in lockstep (their FX vs NPR is mostly NPR/USD).")

# ---------------- 2. variance decomposition of fxshock ---------------------

# Build DOFE 2009-10 share-weighted fxshock
dofe = dofe.groupby(["district_rename","country","year"]).total_migrants.sum().reset_index()
dofe["dname"] = dofe.district_rename.map(to_dname)
dofe = dofe[~dofe.country.isin(["Nepal","India"])]

shares = (dofe[dofe.year.isin([2009,2010])]
          .groupby(["dname","country"]).total_migrants.sum().reset_index()
          .assign(total=lambda d: d.groupby("dname").total_migrants.transform("sum"))
          .assign(share=lambda d: d.total_migrants/d.total)
          [["dname","country","share"]])

fxshock_full = (shares.merge(fx, on="country")
                       .assign(c=lambda d: d.share*d.fx_index)
                       .groupby(["dname","year"]).c.sum().rename("fxshock").reset_index())

# Drop-Gulf version: re-normalise shares to non-Gulf only
shares_ng = (shares[~shares.country.isin(GULF)].copy()
             .assign(total=lambda d: d.groupby("dname").share.transform("sum")))
shares_ng["share"] = shares_ng.share / shares_ng.total
fxshock_ng = (shares_ng.drop(columns="total").merge(fx, on="country")
                       .assign(c=lambda d: d.share*d.fx_index)
                       .groupby(["dname","year"]).c.sum().rename("fxshock_ng").reset_index())

def decompose(panel, col):
    """Variance partition: year effect, district effect, residual."""
    df = panel.copy()
    grand = df[col].mean()
    yr_mean = df.groupby("year")[col].transform("mean")
    di_mean = df.groupby("dname")[col].transform("mean")
    # Two-way demean
    resid = df[col] - yr_mean - di_mean + grand
    total_var = ((df[col] - grand) ** 2).sum()
    year_var  = ((yr_mean - grand) ** 2).sum()
    dist_var  = ((di_mean - grand) ** 2).sum()
    resid_var = (resid ** 2).sum()
    return {
        "total_SS":     total_var,
        "year_share":   year_var / total_var,
        "district_share": dist_var / total_var,
        "residual_share": resid_var / total_var,
    }

dec_full = decompose(fxshock_full, "fxshock")
dec_ng   = decompose(fxshock_ng, "fxshock_ng")

print("\n" + "=" * 70)
print("Variance decomposition of fxshock_d,t (two-way demean)")
print("=" * 70)
print(f"{'spec':<28} {'year %':>9} {'district %':>12} {'residual %':>12}")
print(f"{'full top destinations':<28} {dec_full['year_share']*100:>9.2f} "
      f"{dec_full['district_share']*100:>12.2f} {dec_full['residual_share']*100:>12.2f}")
print(f"{'non-Gulf destinations':<28} {dec_ng['year_share']*100:>9.2f} "
      f"{dec_ng['district_share']*100:>12.2f} {dec_ng['residual_share']*100:>12.2f}")
print("\n  'residual' is the share that survives after both year-mean and district-mean")
print("  partialling.  That is the within-FE variation the SSIV identifies off.")

# Time-series plot: mean fxshock vs mean fxshock_ng
mean_by_year = (fxshock_full.groupby("year").fxshock.mean()
                 .rename("Full top destinations"))
mean_by_year_ng = (fxshock_ng.groupby("year").fxshock_ng.mean()
                    .rename("Non-Gulf only"))
fig, ax = plt.subplots(figsize=(10, 5))
mean_by_year.plot(ax=ax, marker="o", label="Full (Gulf-dominated)")
mean_by_year_ng.plot(ax=ax, marker="s", label="Non-Gulf only (Malaysia, Japan, Korea, etc.)")
ax.set_xlabel("Year"); ax.set_ylabel("Mean fxshock across districts")
ax.set_title("Cross-district mean fxshock: full vs non-Gulf only", fontweight="bold")
ax.legend(); ax.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig("district-analysis/output/fig/dofe_fxshock_full_vs_nongulf.png",
            dpi=140, bbox_inches="tight")
plt.close()

# ---------------- 3. re-run M4-M6 with non-Gulf fxshock --------------------

# DOFE permits panel
dofe_dy = (dofe.groupby(["dname","year"]).total_migrants.sum().reset_index()
              .rename(columns={"total_migrants":"permits"}))
dofe_dy["log_permits"] = np.log(dofe_dy.permits + 1)

# DOFE-vintage intensity (full destinations -- gulf included for intensity)
mi_num = (dofe_dy[dofe_dy.year.isin([2009,2010])]
          .groupby("dname").permits.mean().rename("num").reset_index())
pop = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
              [["dname","district_population_2011"]].drop_duplicates("dname")
              .rename(columns={"district_population_2011":"pop_2011"}))
mi = mi_num.merge(pop, on="dname").assign(mig_int_dofe=lambda d: d.num/d.pop_2011)[["dname","mig_int_dofe"]]

def z(s):
    sd = s.std(); return (s-s.mean())/sd if sd>0 else pd.Series(0., index=s.index)

def yr_inter(df, col, prefix, ref=2016):
    out = pd.DataFrame(index=df.index)
    for y in sorted(df.year.unique()):
        if y == ref: continue
        out[f"{prefix}_y{y}"] = df[col] * (df.year == y).astype(float)
    return out

def fit(panel, level):
    d = panel.dropna(subset=["fxshock_use","mig_int_dofe","log_permits"]).copy()
    d["fx_z"] = z(d.fxshock_use); d["log_mi_z"] = z(np.log(d.mig_int_dofe.clip(lower=1e-12)))
    d["treatment"] = d.fx_z * d.log_mi_z
    cols = ["treatment","fx_z"]; extras = []
    if level >= 4: extras.append(yr_inter(d, "log_mi_z","cmig"))
    if level >= 5: extras.append(yr_inter(d, "fx_z","cfx"))
    exog = pd.concat([d[cols]] + extras, axis=1) if extras else d[cols]
    y = d.log_permits
    idx = pd.MultiIndex.from_arrays([d.dname.values, d.year.values], names=["dname","year"])
    exog.index = idx; y.index = idx
    m = PanelOLS(y, exog, entity_effects=True, time_effects=True, drop_absorbed=True)
    r = m.fit(cov_type="clustered", cluster_entity=True)
    return float(r.params["treatment"]), float(r.tstats["treatment"]), float(r.pvalues["treatment"]), int(r.nobs)

def stars(p):
    return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

panel_full = dofe_dy.merge(fxshock_full, on=["dname","year"]).merge(mi, on="dname")
panel_full = panel_full.rename(columns={"fxshock":"fxshock_use"})

panel_ng = dofe_dy.merge(fxshock_ng, on=["dname","year"]).merge(mi, on="dname")
panel_ng = panel_ng.rename(columns={"fxshock_ng":"fxshock_use"})

print("\n" + "=" * 70)
print("DOFE permits first-stage: full fxshock vs non-Gulf fxshock")
print("=" * 70)
print(f"\n{'spec':<7} {'config':<18} {'beta':>10} {'t':>8} {'sig':>5} {'n':>6}")
print("-" * 70)
rows = []
for lvl in [4,5,6]:
    bf,tf,pf,nf = fit(panel_full, lvl)
    print(f"M{lvl:<6} {'full top destinations':<18} {bf:>10.4f} {tf:>8.2f} {stars(pf):>5} {nf:>6}")
    rows.append({"spec":f"M{lvl}","config":"full","beta":round(bf,4),"t":round(tf,2),"p":round(pf,4),"sig":stars(pf),"n":nf})
    bn,tn,pn,nn = fit(panel_ng, lvl)
    print(f"M{lvl:<6} {'non-Gulf only':<18} {bn:>10.4f} {tn:>8.2f} {stars(pn):>5} {nn:>6}")
    rows.append({"spec":f"M{lvl}","config":"non_gulf","beta":round(bn,4),"t":round(tn,2),"p":round(pn,4),"sig":stars(pn),"n":nn})

pd.DataFrame(rows).to_csv("district-analysis/output/tab/diag_drop_gulf.csv", index=False)
print("\nSaved:")
print("  output/fig/dofe_fx_index_correlation.png")
print("  output/fig/dofe_fxshock_full_vs_nongulf.png")
print("  output/tab/diag_drop_gulf.csv")
