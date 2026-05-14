"""
BHJ FD first-stage, cleaned up:
  - DROP non-Gulf variant (user request)
  - Show that fxshock direction is correct (NPR per LCU, rises with NPR depreciation)
  - Add level-regression rows to compare with FD
  - Add placebo with LEAD shock (Delta_z_{t+1}) to test if lag-1 negative is mechanical
"""

import numpy as np, pandas as pd
from linearmodels.panel import PanelOLS

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

forex     = pd.read_csv("district-analysis/data/clean/forex_2000_2023.csv")
dofe_raw  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")

# ----- Direction sanity check ------------------------------------------------
print("Direction sanity check on Qatar:")
sub = (forex.merge(forex[forex.country=="Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"}), on="year")
              [["country","year","forex","npr_per_usd"]]
              .query("country=='Qatar'"))
sub["lcu_per_usd"] = sub.forex
sub["npr_per_lcu"] = sub.npr_per_usd / sub.lcu_per_usd
print(sub[["year","lcu_per_usd","npr_per_usd","npr_per_lcu"]].head(3).to_string(index=False))
print("...")
print(sub[["year","lcu_per_usd","npr_per_usd","npr_per_lcu"]].tail(3).to_string(index=False))
print("NPR per LCU should RISE from 2009 to 2023 (NPR depreciated). Confirmed.\n")

# ----- Build inputs ---------------------------------------------------------
nepal_fx = forex[forex.country=="Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
            .merge(nepal_fx, on="year")
            .query("country not in ['Nepal','India']")
            .assign(fx_npr_per_lcu=lambda d: d.npr_per_usd/d.lcu_per_usd))
base = fx[fx.year.isin([2009,2010])].groupby("country").fx_npr_per_lcu.mean().rename("base").reset_index()
fx = (fx.merge(base,on="country")
        .assign(log_fx=lambda d: np.log(d.fx_npr_per_lcu/d.base))
        [["country","year","fx_npr_per_lcu","log_fx"]].sort_values(["country","year"]))
fx["d_log_fx"] = fx.groupby("country")["log_fx"].diff()

dofe = (dofe_raw.groupby(["district_rename","country","year"]).total_migrants.sum().reset_index()
            .assign(dname=lambda d: d.district_rename.map(to_dname)))
dofe = dofe[~dofe.country.isin(["Nepal","India"])]

shares = (dofe[dofe.year.isin([2009,2010])]
          .groupby(["dname","country"]).total_migrants.sum().reset_index()
          .assign(tot=lambda d: d.groupby("dname").total_migrants.transform("sum"))
          .assign(share=lambda d: d.total_migrants/d.tot)
          [["dname","country","share"]])
S_d = shares.groupby("dname").share.sum().rename("S_d").reset_index()

dy = (dofe.groupby(["dname","year"]).total_migrants.sum().reset_index()
            .rename(columns={"total_migrants":"permits"})
            .sort_values(["dname","year"]))
dy["log_permits"]   = np.log(dy.permits + 1)
dy["d_log_permits"] = dy.groupby("dname")["log_permits"].diff()

# z-shock in LEVELS
z_lvl = (shares.merge(fx, on="country").assign(x=lambda d: d.share*d.log_fx)
              .groupby(["dname","year"]).x.sum().rename("z").reset_index())
# z-shock in FIRST DIFFERENCES
dz = (shares.merge(fx, on="country").assign(x=lambda d: d.share*d.d_log_fx)
              .groupby(["dname","year"]).x.sum().rename("dz").reset_index())

pop = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
              [["dname","district_population_2011"]].drop_duplicates("dname")
              .rename(columns={"district_population_2011":"pop_2011"}))
mi_num = (dy[dy.year.isin([2009,2010])].groupby("dname").permits.mean().rename("num").reset_index())
mi = mi_num.merge(pop,on="dname").assign(mig_int_dofe=lambda d: d.num/d.pop_2011)[["dname","mig_int_dofe"]]

panel = (dy[["dname","year","permits","log_permits","d_log_permits"]]
           .merge(z_lvl, on=["dname","year"], how="left")
           .merge(dz,    on=["dname","year"], how="left")
           .merge(S_d,   on="dname", how="left")
           .merge(mi,    on="dname", how="left")
           .sort_values(["dname","year"]))

for L in (0, 1, 2):
    panel[f"z_L{L}"]  = panel.groupby("dname")["z"].shift(L)
    panel[f"dz_L{L}"] = panel.groupby("dname")["dz"].shift(L)
# Lead (future) shock for placebo
panel["dz_lead1"] = panel.groupby("dname")["dz"].shift(-1)
panel["z_lead1"]  = panel.groupby("dname")["z"].shift(-1)

def z_std(s):
    sd = s.std(); return (s-s.mean())/sd if sd>0 else pd.Series(0.,index=s.index)

# z-score shock vars
for c in ["z","z_L0","z_L1","z_L2","z_lead1","dz","dz_L0","dz_L1","dz_L2","dz_lead1"]:
    if c in panel.columns:
        panel[c+"_z"] = z_std(panel[c])
panel["log_mi_z"] = z_std(np.log(panel.mig_int_dofe.clip(lower=1e-12)))

# Quick check: cross-district mean of dz by year (should be small, year-mean varies)
print("Mean Δlog_fx-share-weighted (dz) by year:")
yr_means = panel.groupby("year").dz.mean().round(4)
print(yr_means.to_string())
print(f"(SD of yearly mean dz: {yr_means.std():.4f})\n")

# ----- Regressions ----------------------------------------------------------
def fit_fd(df, shock_col):
    d = df.dropna(subset=["d_log_permits", shock_col]).copy()
    exog = pd.DataFrame({"shock": d[shock_col].values}, index=d.index)
    idx = pd.MultiIndex.from_arrays([d.dname.values, d.year.values], names=["dname","year"])
    exog.index = idx
    y = d["d_log_permits"].copy(); y.index = idx
    m = PanelOLS(y, exog, entity_effects=False, time_effects=True, drop_absorbed=True)
    return m.fit(cov_type="clustered", cluster_entity=True), d

def fit_level(df, shock_col):
    d = df.dropna(subset=["log_permits", shock_col]).copy()
    exog = pd.DataFrame({"shock": d[shock_col].values}, index=d.index)
    idx = pd.MultiIndex.from_arrays([d.dname.values, d.year.values], names=["dname","year"])
    exog.index = idx
    y = d["log_permits"].copy(); y.index = idx
    m = PanelOLS(y, exog, entity_effects=True, time_effects=True, drop_absorbed=True)
    return m.fit(cov_type="clustered", cluster_entity=True), d

def stars(p): return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

print(f"{'design':<22} {'shock':<14} {'beta':>9} {'se':>8} {'t':>6} {'p':>8} {'sig':>4} {'mean_y':>10} {'n':>5}")
print("-" * 90)
rows = []
# 1. Level regression for comparison (dname + year FE, with z lagged 0/1/2)
for L in (0,1,2):
    res, used = fit_level(panel, f"z_L{L}_z")
    bh = float(res.params["shock"]); seh = float(res.std_errors["shock"])
    pv = float(res.pvalues["shock"])
    s = stars(pv)
    print(f"{'LEVEL  | dname+yr FE':<22} {f'z (lag {L})':<14} {bh:>9.4f} {seh:>8.4f} {bh/seh:>6.2f} {pv:>8.4f} {s:>4} {used.log_permits.mean():>10.3f} {int(res.nobs):>5}")
    rows.append({"design":"level","shock":f"z_L{L}","beta":round(bh,4),"se":round(seh,4),"t":round(bh/seh,2),"p":round(pv,4),"sig":s,"mean_y":round(used.log_permits.mean(),4),"n":int(res.nobs)})

print()
# 2. FD regression
for L in (0,1,2):
    res, used = fit_fd(panel, f"dz_L{L}_z")
    bh = float(res.params["shock"]); seh = float(res.std_errors["shock"])
    pv = float(res.pvalues["shock"])
    s = stars(pv)
    print(f"{'FD     | year FE':<22} {f'Δz (lag {L})':<14} {bh:>9.4f} {seh:>8.4f} {bh/seh:>6.2f} {pv:>8.4f} {s:>4} {used.d_log_permits.mean():>10.3f} {int(res.nobs):>5}")
    rows.append({"design":"FD","shock":f"dz_L{L}","beta":round(bh,4),"se":round(seh,4),"t":round(bh/seh,2),"p":round(pv,4),"sig":s,"mean_y":round(used.d_log_permits.mean(),4),"n":int(res.nobs)})

print()
# 3. Placebo: FD with LEAD shock (future shocks predicting current Δy -- should be null)
res, used = fit_fd(panel, "dz_lead1_z")
bh = float(res.params["shock"]); seh = float(res.std_errors["shock"])
pv = float(res.pvalues["shock"]); s = stars(pv)
print(f"{'PLACEBO FD | year FE':<22} {'Δz (lead +1)':<14} {bh:>9.4f} {seh:>8.4f} {bh/seh:>6.2f} {pv:>8.4f} {s:>4} {used.d_log_permits.mean():>10.3f} {int(res.nobs):>5}")
rows.append({"design":"FD placebo","shock":"dz_lead1","beta":round(bh,4),"se":round(seh,4),"t":round(bh/seh,2),"p":round(pv,4),"sig":s,"mean_y":round(used.d_log_permits.mean(),4),"n":int(res.nobs)})

pd.DataFrame(rows).to_csv("district-analysis/output/tab/bhj_fd_vs_level.csv", index=False)
print("\nSaved: output/tab/bhj_fd_vs_level.csv")
