"""
Drop-Gulf robustness: rebuild fxshock using only non-Gulf destinations,
run M3-M6 with cluster + wild bootstrap inference on DOFE permits.
Side-by-side with the full (Gulf+non-Gulf) SSIV.
"""

import os, numpy as np, pandas as pd
from linearmodels.panel import PanelOLS
np.random.seed(2026)

GULF = {"Qatar","Saudi Arabia","United Arab Emirates","Kuwait","Bahrain","Oman"}

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

forex     = pd.read_csv("district-analysis/data/clean/forex_2000_2023.csv")
dofe      = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")
region_sh = pd.read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv")

# NPR per LCU FX index, baseline 2009-10
nepal_fx = forex[forex.country=="Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
            .merge(nepal_fx, on="year")
            .query("country not in ['Nepal','India']")
            .assign(fx_npr_per_lcu=lambda d: d.npr_per_usd/d.lcu_per_usd))
base = fx[fx.year.isin([2009,2010])].groupby("country").fx_npr_per_lcu.mean().rename("base").reset_index()
fx = fx.merge(base,on="country").assign(fx_index=lambda d: d.fx_npr_per_lcu/d.base)[["country","year","fx_index"]]

dofe = (dofe.groupby(["district_rename","country","year"]).total_migrants.sum().reset_index()
              .assign(dname=lambda d: d.district_rename.map(to_dname)))
dofe = dofe[~dofe.country.isin(["Nepal","India"])]

# Shares (full and non-Gulf-renormalised)
shares = (dofe[dofe.year.isin([2009,2010])]
          .groupby(["dname","country"]).total_migrants.sum().reset_index()
          .assign(total=lambda d: d.groupby("dname").total_migrants.transform("sum"))
          .assign(share=lambda d: d.total_migrants/d.total)
          [["dname","country","share"]])

shares_ng = (shares[~shares.country.isin(GULF)].copy()
             .assign(tt=lambda d: d.groupby("dname").share.transform("sum")))
shares_ng["share"] = shares_ng.share / shares_ng.tt
shares_ng = shares_ng[["dname","country","share"]]

def build_fx(s):
    return (s.merge(fx, on="country").assign(c=lambda d: d.share*d.fx_index)
              .groupby(["dname","year"]).c.sum().rename("fxshock").reset_index())

fx_full = build_fx(shares)
fx_ng   = build_fx(shares_ng)

# mig_int (DOFE / pop_2011)
dy = (dofe.groupby(["dname","year"]).total_migrants.sum().reset_index()
            .rename(columns={"total_migrants":"permits"}))
mi_num = (dy[dy.year.isin([2009,2010])].groupby("dname").permits.mean().rename("num").reset_index())
pop = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
                  [["dname","district_population_2011"]].drop_duplicates("dname")
                  .rename(columns={"district_population_2011":"pop_2011"}))
mi = mi_num.merge(pop,on="dname").assign(mig_int_dofe=lambda d: d.num/d.pop_2011)[["dname","mig_int_dofe"]]
dy["log_permits"] = np.log(dy.permits + 1)

def z(s):
    sd = s.std(); return (s-s.mean())/sd if sd>0 else pd.Series(0., index=s.index)

REGION_COLS = ["share_e_asia","share_gulf","share_oecd_north",
               "share_oecd_europe","share_s_asia","share_se_asia"]

def yr_inter(df, col, prefix, ref=2016):
    out = pd.DataFrame(index=df.index)
    for y in sorted(df.year.unique()):
        if y == ref: continue
        out[f"{prefix}_y{y}"] = df[col] * (df.year == y).astype(float)
    return out

def build_panel(fx_panel):
    p = (dy.merge(fx_panel, on=["dname","year"])
            .merge(mi, on="dname")
            .merge(region_sh, on="dname", how="left")
            .dropna(subset=["fxshock","mig_int_dofe","log_permits"]).copy())
    p["fx_z"]      = z(p.fxshock)
    p["log_mi_z"]  = z(np.log(p.mig_int_dofe.clip(lower=1e-12)))
    p["treatment"] = p.fx_z * p.log_mi_z
    return p

def fit_and_boot(panel, level, B=500):
    p = panel.copy()
    cols = ["treatment","fx_z"]; extras = []
    if level >= 4: extras.append(yr_inter(p, "log_mi_z","cmig"))
    if level >= 5: extras.append(yr_inter(p, "fx_z","cfx"))
    if level >= 6:
        for c in REGION_COLS:
            if c in p.columns: extras.append(yr_inter(p, c, f"cX_{c}"))
    exog = pd.concat([p[cols]] + extras, axis=1) if extras else p[cols]
    y = p.log_permits
    idx = pd.MultiIndex.from_arrays([p.dname.values, p.year.values], names=["dname","year"])
    exog.index = idx; y.index = idx
    m = PanelOLS(y, exog, entity_effects=True, time_effects=(level>=3), drop_absorbed=True)
    r = m.fit(cov_type="clustered", cluster_entity=True)
    bh = float(r.params["treatment"]); seh = float(r.std_errors["treatment"])
    pcl = float(r.pvalues["treatment"])
    # wild cluster bootstrap
    yhat = np.array(predict_arr := r.fitted_values).flatten()
    # predict from fit and align
    p_full = p.copy()
    p_full = p_full.set_index(["dname","year"])
    p_full[".yhat"] = r.fitted_values
    p_full[".e"]    = r.resids
    p_full = p_full.reset_index()
    p_full = p_full.dropna(subset=[".yhat",".e"])
    ents = p_full.dname.unique()
    bs = np.zeros(B)
    for b in range(B):
        signs = pd.Series(np.random.choice([-1.,1.], size=len(ents)), index=ents)
        p_full[".g"] = p_full.dname.map(signs)
        p_full[".y_star"] = p_full[".yhat"] + p_full[".g"] * p_full[".e"]
        exog_b = pd.concat([p_full[cols]] + (
            [yr_inter(p_full,"log_mi_z","cmig")] if level>=4 else []) + (
            [yr_inter(p_full,"fx_z","cfx")] if level>=5 else []) + (
            [yr_inter(p_full, c, f"cX_{c}") for c in REGION_COLS if c in p_full.columns and level>=6]
            ), axis=1)
        idx_b = pd.MultiIndex.from_arrays([p_full.dname.values, p_full.year.values], names=["dname","year"])
        exog_b.index = idx_b
        ys = p_full[".y_star"].copy(); ys.index = idx_b
        try:
            rb = PanelOLS(ys, exog_b, entity_effects=True, time_effects=(level>=3), drop_absorbed=True).fit(cov_type="clustered", cluster_entity=True)
            bs[b] = float(rb.params["treatment"])
        except: bs[b] = np.nan
    bs = bs[~np.isnan(bs)]
    p_wild = (np.abs(bs - bh) >= np.abs(bh)).mean()
    se_wild = bs.std()
    return {"beta":bh, "se_cluster":seh, "t_cluster":bh/seh, "p_cluster":pcl,
            "se_wild":float(se_wild), "p_wild":float(p_wild),
            "mean_y": float(p.log_permits.mean()), "n":int(r.nobs)}

def stars(p): return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

panel_full = build_panel(fx_full)
panel_ng   = build_panel(fx_ng)

print(f"Panel full   N={len(panel_full)}, districts={panel_full.dname.nunique()}")
print(f"Panel non-G  N={len(panel_ng)},  districts={panel_ng.dname.nunique()}")
print()
print(f"{'spec':<5} {'config':<10} {'beta':>9} {'se_cl':>8} {'t_cl':>6} {'p_cl':>8} {'sig':>4} "
      f"{'se_wild':>9} {'t_wild':>7} {'p_wild':>8} {'mean_y':>8} {'n':>5}")
print("-"*100)
rows = []
for lvl in [3,4,5,6]:
    rf = fit_and_boot(panel_full, lvl, B=500)
    rn = fit_and_boot(panel_ng,   lvl, B=500)
    for cfg, r in [("full", rf), ("non_gulf", rn)]:
        s = stars(r["p_cluster"])
        print(f"M{lvl:<4} {cfg:<10} {r['beta']:>9.4f} {r['se_cluster']:>8.4f} "
              f"{r['t_cluster']:>6.2f} {r['p_cluster']:>8.4f} {s:>4} "
              f"{r['se_wild']:>9.4f} {r['beta']/r['se_wild']:>7.2f} "
              f"{r['p_wild']:>8.4f} {r['mean_y']:>8.3f} {r['n']:>5}")
        rows.append({"spec":f"M{lvl}", "config":cfg, **{k:round(v,4) if isinstance(v,float) else v for k,v in r.items()}, "sig_cl":s})

pd.DataFrame(rows).to_csv("district-analysis/output/tab/diag_drop_gulf_full.csv", index=False)
print("\nSaved: output/tab/diag_drop_gulf_full.csv")
