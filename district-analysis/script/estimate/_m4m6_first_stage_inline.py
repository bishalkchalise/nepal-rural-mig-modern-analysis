"""
M4-M6 first-stage with wild cluster bootstrap for the DOFE permits panel,
sign-flipped so that POSITIVE beta = more destination-currency appreciation
(more NPR depreciation) -> more permits.

Treatment construction (Khanna-direction fxshock, intuitive sign):
   fx_z       = z(fxshock_LCU)                  # LCU per NPR, falls with NPR depreciation
   treatment  = ( -fx_z ) * z(log(mig_int))     # negated so beta > 0 = appreciation -> outcome up

For each spec in {M4, M5, M6} report:
   beta, cluster SE, wild-cluster-bootstrap SE, both p-values, N.
"""

import os, numpy as np, pandas as pd
from linearmodels.panel import PanelOLS
np.random.seed(2026)

# -------- 1.  build inputs (Khanna direction fxshock) -----------------------

forex     = pd.read_csv("district-analysis/data/clean/forex_2000_2023.csv")
dofe      = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")
region_sh = pd.read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv")

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

nepal_fx = forex[forex.country=="Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
# NPR per LCU direction: rises as NPR depreciates -> intuitive positive sign for beta
fx_npr_per_lcu = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
                .merge(nepal_fx, on="year")
                .query("country!='Nepal' & country!='India'")
                .assign(fx_npr_per_lcu=lambda d: d.npr_per_usd/d.lcu_per_usd)
                [["country","year","fx_npr_per_lcu"]])
base = fx_npr_per_lcu[fx_npr_per_lcu.year.isin([2009,2010])].groupby("country").fx_npr_per_lcu.mean().rename("base").reset_index()
fx_idx = fx_npr_per_lcu.merge(base, on="country").assign(fx_index=lambda d: d.fx_npr_per_lcu/d.base)[["country","year","fx_index"]]

dofe_dc = dofe.assign(dname=lambda d: d.district_rename.map(to_dname)).query("country!='India' & country!='Nepal'")
shares = (dofe_dc[dofe_dc.year.isin([2009,2010])]
          .groupby(["dname","country"]).total_migrants.sum().reset_index()
          .assign(total=lambda d: d.groupby("dname").total_migrants.transform("sum"))
          .assign(share=lambda d: d.total_migrants/d.total)
          [["dname","country","share"]])

fxshock = (shares.merge(fx_idx, on="country")
                  .assign(c=lambda d: d.share*d.fx_index)
                  .groupby(["dname","year"]).c.sum().rename("fxshock").reset_index())

dofe_dy = (dofe.groupby(["district_rename","year"]).total_migrants.sum().reset_index()
                .assign(dname=lambda d: d.district_rename.map(to_dname))
                .rename(columns={"total_migrants":"permits"}))
mi = dofe_dy[dofe_dy.year.isin([2009,2010])].groupby("dname").permits.mean().rename("num").reset_index()
pop = pop_file.assign(dname=lambda d: d.district.map(to_dname))[["dname","district_population_2011"]].drop_duplicates("dname").rename(columns={"district_population_2011":"pop_2011"})
mi = mi.merge(pop, on="dname").assign(mig_int_dofe=lambda d: d.num/d.pop_2011)[["dname","mig_int_dofe"]]

panel = (dofe_dy[["dname","year","permits"]]
         .merge(fxshock, on=["dname","year"])
         .merge(mi, on="dname")
         .merge(region_sh, on="dname")
         .dropna().copy())
panel["log_permits"] = np.log(panel.permits + 1)

def z(s):
    sd = s.std(); return (s-s.mean())/sd if sd>0 else pd.Series(0., index=s.index)

# fxshock = NPR per LCU index (rises with NPR depreciation = destination appreciation)
# Positive beta on fx_z * log_mi_z = "more appreciation -> more y"  (intuitive sign)
panel["fx_z"]      = z(panel.fxshock)
panel["log_mi_z"]  = z(np.log(panel.mig_int_dofe.clip(lower=1e-12)))
panel["treatment"] = panel.fx_z * panel.log_mi_z

# -------- 2.  spec builder + bootstrap --------------------------------------

REGION_COLS = ["share_e_asia","share_gulf","share_oecd_north",
               "share_oecd_europe","share_s_asia","share_se_asia"]

def yr_inter(df, col, prefix, ref=2016):
    out = pd.DataFrame(index=df.index)
    for y in sorted(df.year.unique()):
        if y == ref: continue
        out[f"{prefix}_y{y}"] = df[col] * (df.year == y).astype(float)
    return out

def build_exog(d, level, ref=2016):
    cols = ["treatment","fx_z"]
    extras = []
    if level >= 4: extras.append(yr_inter(d, "log_mi_z", "cmig", ref))
    if level >= 5: extras.append(yr_inter(d, "fx_z",     "cfx",  ref))
    if level >= 6:
        for c in REGION_COLS:
            if c in d.columns: extras.append(yr_inter(d, c, f"cX_{c}", ref))
    return pd.concat([d[cols]] + extras, axis=1) if extras else d[cols]

def fit_panel(df, level, time_effects=True):
    exog = build_exog(df, level)
    y = df["log_permits"]
    idx = pd.MultiIndex.from_arrays([df.dname.values, df.year.values], names=["dname","year"])
    exog = exog.copy(); exog.index = idx; y = y.copy(); y.index = idx
    return PanelOLS(y, exog, entity_effects=True, time_effects=time_effects,
                    drop_absorbed=True).fit(cov_type="clustered", cluster_entity=True)

def wild_boot(df, level, B=500, outcome_col="log_permits"):
    res = fit_panel(df, level)
    bh  = float(res.params["treatment"])
    seh = float(res.std_errors["treatment"])
    fitted = res.fitted_values.reset_index(); fitted.columns = ["dname","year","yhat"]
    resid  = res.resids.reset_index();        resid.columns  = ["dname","year","e"]
    db = df.merge(fitted, on=["dname","year"]).merge(resid, on=["dname","year"]).copy()
    dist = db.dname.unique()
    bs = np.zeros(B)
    for b in range(B):
        signs = pd.Series(np.random.choice([-1.,1.], size=len(dist)), index=dist)
        db["g"] = db.dname.map(signs)
        db["y_star"] = db["yhat"] + db["g"] * db["e"]
        exog = build_exog(db, level)
        idx = pd.MultiIndex.from_arrays([db.dname.values, db.year.values], names=["dname","year"])
        exog = exog.copy(); exog.index = idx; ys = db["y_star"].copy(); ys.index = idx
        try:
            r = PanelOLS(ys, exog, entity_effects=True, time_effects=True,
                         drop_absorbed=True).fit(cov_type="clustered", cluster_entity=True)
            bs[b] = float(r.params["treatment"])
        except: bs[b] = np.nan
    bs = bs[~np.isnan(bs)]
    centered = bs - bh
    p_wild = (np.abs(centered) >= np.abs(bh)).mean()
    se_wild = float(bs.std())
    return {"beta": bh, "se_cluster": seh,
            "t_cluster": bh/seh, "p_cluster": float(res.pvalues["treatment"]),
            "se_wild": se_wild,
            "t_wild": bh/se_wild,
            "p_wild": float(p_wild),
            "mean_y": float(df[outcome_col].mean()),
            "n": int(res.nobs), "B": int(len(bs))}

# -------- 3.  Run M4 M5 M6 with bootstrap -----------------------------------

def stars(p):
    return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

print("DOFE permits, intuitive sign convention (beta>0 = appreciation -> permits up):\n")
print(f"{'spec':<6} {'beta':>9} {'se_cl':>8} {'t_cl':>7} {'p_cl':>9} {'sig':>4} "
      f"{'se_wild':>8} {'t_wild':>7} {'p_wild':>9} {'mean_y':>8} {'n':>5}")
print("-"*94)
rows = []
for lvl in [4,5,6]:
    r = wild_boot(panel, lvl, B=500)
    s = stars(r["p_cluster"])
    print(f"M{lvl:<5} {r['beta']:>9.4f} {r['se_cluster']:>8.4f} {r['t_cluster']:>7.2f} "
          f"{r['p_cluster']:>9.4f} {s:>4} {r['se_wild']:>8.4f} {r['t_wild']:>7.2f} "
          f"{r['p_wild']:>9.4f} {r['mean_y']:>8.3f} {r['n']:>5}")
    rows.append({"spec":f"M{lvl}", **{k:round(v,4) if isinstance(v,float) else v for k,v in r.items()}, "sig": s})

pd.DataFrame(rows).to_csv("district-analysis/output/tab/m4m6_dofe_permits.csv", index=False)
print("\nSaved: output/tab/m4m6_dofe_permits.csv")
