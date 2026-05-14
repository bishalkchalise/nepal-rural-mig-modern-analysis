"""
BHJ first-differenced first-stage on DOFE permits.

  Delta_log_permits_{d,t} ~ beta * Delta_z_{d, t - L}  | year FE,  cluster ~dname

  Delta_z_{d,t} = sum_c  share_dc(fixed @ 2009-10)  *  ( log_fx_index_{c,t} - log_fx_index_{c,t-1} )

Lag L in {0, 1, 2}.  No district FE (absorbed by differencing).
Three control variants per cell:
  bare  : just year FE
  S     : + i(year, S_d_z)            BHJ incomplete-shares year-trend
  mi    : + i(year, log_mig_int_z)    intensity-specific year-trend

Two share configs:
  full     : 2009-10 shares over all non-India non-Nepal destinations
  non_gulf : drop Gulf destinations, re-normalise shares over remaining
"""

import os, numpy as np, pandas as pd
from linearmodels.panel import PanelOLS

GULF = {"Qatar","Saudi Arabia","United Arab Emirates","Kuwait","Bahrain","Oman"}
dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

forex     = pd.read_csv("district-analysis/data/clean/forex_2000_2023.csv")
dofe_raw  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")

nepal_fx = forex[forex.country=="Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
            .merge(nepal_fx, on="year")
            .query("country not in ['Nepal','India']")
            .assign(fx_npr_per_lcu=lambda d: d.npr_per_usd/d.lcu_per_usd))
base = fx[fx.year.isin([2009,2010])].groupby("country").fx_npr_per_lcu.mean().rename("base").reset_index()
fx = (fx.merge(base,on="country")
        .assign(log_fx=lambda d: np.log(d.fx_npr_per_lcu/d.base))
        [["country","year","log_fx"]].sort_values(["country","year"]))
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
            .rename(columns={"total_migrants":"permits"}))
dy["log_permits"] = np.log(dy.permits + 1)
dy = dy.sort_values(["dname","year"])
dy["d_log_permits"] = dy.groupby("dname")["log_permits"].diff()

def build_dz(fx_p, sh):
    c = sh.merge(fx_p, on="country").assign(x=lambda d: d.share*d.d_log_fx)
    return c.groupby(["dname","year"]).x.sum().rename("dz").reset_index()

dz_full = build_dz(fx, shares)
shares_ng = (shares[~shares.country.isin(GULF)].copy()
             .assign(t=lambda d: d.groupby("dname").share.transform("sum"))
             .assign(share=lambda d: d.share/d.t))
shares_ng = shares_ng[["dname","country","share"]]
dz_ng = build_dz(fx, shares_ng)

pop = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
              [["dname","district_population_2011"]].drop_duplicates("dname")
              .rename(columns={"district_population_2011":"pop_2011"}))
mi_num = (dy[dy.year.isin([2009,2010])].groupby("dname").permits.mean().rename("num").reset_index())
mi = mi_num.merge(pop,on="dname").assign(mig_int_dofe=lambda d: d.num/d.pop_2011)[["dname","mig_int_dofe"]]

fd = (dy[["dname","year","d_log_permits"]]
        .merge(dz_full.rename(columns={"dz":"dz_full"}), on=["dname","year"], how="left")
        .merge(dz_ng.rename(columns={"dz":"dz_ng"}),     on=["dname","year"], how="left")
        .merge(S_d, on="dname").merge(mi, on="dname")
        .dropna(subset=["d_log_permits","dz_full"])
        .sort_values(["dname","year"]))

for L in (0,1,2):
    fd[f"dz_full_L{L}"] = fd.groupby("dname")["dz_full"].shift(L)
    fd[f"dz_ng_L{L}"]   = fd.groupby("dname")["dz_ng"].shift(L)

def z(s):
    sd=s.std(); return (s-s.mean())/sd if sd>0 else pd.Series(0., index=s.index)
for L in (0,1,2):
    fd[f"dz_full_L{L}_z"] = z(fd[f"dz_full_L{L}"])
    fd[f"dz_ng_L{L}_z"]   = z(fd[f"dz_ng_L{L}"])
fd["log_mi_z"] = z(np.log(fd.mig_int_dofe.clip(lower=1e-12)))
fd["S_d_z"]    = z(fd.S_d)

def yr_inter(df, col, prefix):
    out = pd.DataFrame(index=df.index)
    ref = int(np.median(sorted(df.year.unique())))
    for y in sorted(df.year.unique()):
        if y == ref: continue
        out[f"{prefix}_y{y}"] = df[col] * (df.year == y).astype(float)
    return out

def fit(df, shock_col, ctrl="bare"):
    d = df.dropna(subset=["d_log_permits", shock_col]).copy()
    if len(d) < 100: return None
    exog = pd.DataFrame({"shock": d[shock_col].values}, index=d.index)
    if ctrl == "S":  exog = pd.concat([exog, yr_inter(d,"S_d_z","cS")], axis=1)
    if ctrl == "mi": exog = pd.concat([exog, yr_inter(d,"log_mi_z","cmi")], axis=1)
    y = d["d_log_permits"]
    idx = pd.MultiIndex.from_arrays([d.dname.values, d.year.values], names=["dname","year"])
    exog.index = idx; y.index = idx
    m = PanelOLS(y, exog, entity_effects=False, time_effects=True, drop_absorbed=True)
    r = m.fit(cov_type="clustered", cluster_entity=True)
    return {"beta":float(r.params["shock"]),
            "se":float(r.std_errors["shock"]),
            "t":float(r.tstats["shock"]),
            "p":float(r.pvalues["shock"]),
            "n":int(r.nobs),
            "mean_y":float(d["d_log_permits"].mean())}

def stars(p): return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

print(f"FD panel: {len(fd)} obs, {fd.dname.nunique()} districts, "
      f"years {sorted(fd.year.unique())[0]}-{sorted(fd.year.unique())[-1]}\n")
print(f"{'lag':>3} {'config':<10} {'ctrl':<6} {'beta':>9} {'se':>8} {'t':>6} {'p':>8} {'sig':>4} "
      f"{'mean_y':>11} {'n':>5}")
print("-"*82)
rows = []
for L in (0,1,2):
    for cfg, col_pat in [("full","dz_full_L{}_z"),("non_gulf","dz_ng_L{}_z")]:
        col = col_pat.format(L)
        for ctrl in ["bare","S","mi"]:
            r = fit(fd, col, ctrl)
            if r is None: continue
            s = stars(r["p"])
            print(f"{L:>3} {cfg:<10} {ctrl:<6} {r['beta']:>9.4f} {r['se']:>8.4f} {r['t']:>6.2f} "
                  f"{r['p']:>8.4f} {s:>4} {r['mean_y']:>11.5f} {r['n']:>5}")
            rows.append({"lag":L,"config":cfg,"controls":ctrl,
                         "beta":round(r['beta'],4),"se":round(r['se'],4),
                         "t":round(r['t'],2),"p":round(r['p'],4),"sig":s,
                         "mean_y":round(r['mean_y'],5),"n":r["n"]})

pd.DataFrame(rows).to_csv("district-analysis/output/tab/bhj_fd_first_stage.csv", index=False)
print("\nSaved: output/tab/bhj_fd_first_stage.csv")
