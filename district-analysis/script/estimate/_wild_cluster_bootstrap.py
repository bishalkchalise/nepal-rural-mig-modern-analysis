"""
Wild cluster bootstrap (Cameron-Gelbach-Miller / Roodman) for the M4-M6
spec on DOFE permits, in Khanna direction (LCU/NPR). Compares to the
standard cluster-robust SE to see whether the C_mig over-absorption
materially changes inference.

Procedure (Rademacher wild bootstrap):
  1. Estimate the model, save residuals e_dt and predicted yhat_dt.
  2. For B=500 reps:
       - draw g_d in {-1, +1} per district, prob 0.5 each
       - bootstrap y* = yhat_dt + g_d * e_dt
       - re-fit, store beta*
  3. Bootstrap SE = sd(beta*)
     Bootstrap p-value: 2 * min(P(beta* > 0), P(beta* < 0))   (symmetric)

This is the standard reference for shift-share regressions with limited
cluster-level variation; works under H0 with cluster dependence.
"""

import os, numpy as np, pandas as pd
from linearmodels.panel import PanelOLS
np.random.seed(20260514)

# --------------------------------------------------------------------------
# 1. Build Khanna-direction fxshock (LCU/NPR) from raw
# --------------------------------------------------------------------------

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

nepal_fx = (forex[forex.country == "Nepal"][["year","forex"]]
            .rename(columns={"forex":"npr_per_usd"}))

fx_lcu = (forex.rename(columns={"forex":"lcu_per_usd"})
                [["country","year","lcu_per_usd"]]
                .merge(nepal_fx, on="year")
                .query("country != 'Nepal' & country != 'India'")
                .assign(fx_lcu_per_npr = lambda d: d.lcu_per_usd / d.npr_per_usd)
                [["country","year","fx_lcu_per_npr"]])

# DOFE 2009-10 baseline FX index
base = (fx_lcu[fx_lcu.year.isin([2009,2010])]
        .groupby("country").fx_lcu_per_npr.mean().rename("base").reset_index())
fx_idx = (fx_lcu.merge(base, on="country")
                .assign(fx_index = lambda d: d.fx_lcu_per_npr / d.base)
                [["country","year","fx_index"]])

# DOFE 2009-10 shares per (district, country)
dofe_dc = (dofe.assign(dname = lambda d: d.district_rename.map(to_dname))
                .query("country != 'India' & country != 'Nepal'"))
shares = (dofe_dc[dofe_dc.year.isin([2009,2010])]
          .groupby(["dname","country"]).total_migrants.sum().reset_index())
shares = (shares.assign(total=lambda d: d.groupby("dname").total_migrants.transform("sum"))
                .assign(share=lambda d: d.total_migrants / d.total)
                [["dname","country","share"]])

fxshock_LCU = (shares.merge(fx_idx, on="country")
                     .assign(contrib = lambda d: d.share * d.fx_index)
                     .groupby(["dname","year"]).contrib.sum()
                     .rename("fxshock_LCU").reset_index())

# DOFE-vintage mig_int
dofe_dy = (dofe.groupby(["district_rename","year"]).total_migrants.sum().reset_index()
                .assign(dname=lambda d: d.district_rename.map(to_dname))
                .rename(columns={"total_migrants":"permits"}))
mi = (dofe_dy[dofe_dy.year.isin([2009,2010])]
       .groupby("dname").permits.mean().rename("num").reset_index())
pop = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
              [["dname","district_population_2011"]]
              .drop_duplicates("dname")
              .rename(columns={"district_population_2011":"pop_2011"}))
mi = mi.merge(pop, on="dname").assign(mig_int_dofe=lambda d: d.num/d.pop_2011)[["dname","mig_int_dofe"]]

# Final panel
panel = (dofe_dy[["dname","year","permits"]]
         .merge(fxshock_LCU, on=["dname","year"])
         .merge(mi, on="dname", how="left")
         .merge(region_sh, on="dname", how="left")
         .dropna()
         .copy())
panel["log_permits"] = np.log(panel.permits + 1)

def z(s):
    sd = s.std(); return (s - s.mean())/sd if sd>0 else pd.Series(0., index=s.index)
panel["fx_z"]      = z(panel.fxshock_LCU)
panel["log_mi_z"]  = z(np.log(panel.mig_int_dofe.clip(lower=1e-12)))
panel["treatment"] = panel.fx_z * panel.log_mi_z

print(f"Panel: {len(panel)} obs, {panel.dname.nunique()} districts, "
      f"{panel.year.nunique()} years")

# --------------------------------------------------------------------------
# 2. Helper: build year-interactions
# --------------------------------------------------------------------------

def yr_inter(df, col, prefix, ref=2016):
    out = pd.DataFrame(index=df.index)
    for y in sorted(df.year.unique()):
        if y == ref: continue
        out[f"{prefix}_y{y}"] = df[col] * (df.year == y).astype(float)
    return out

REGION_COLS = ["share_e_asia","share_gulf","share_oecd_north",
               "share_oecd_europe","share_s_asia","share_se_asia"]

def build_exog(d, level, ref=2016):
    cols = ["treatment"]
    extras = []
    if level >= 2: cols.append("fx_z")
    if level >= 4: extras.append(yr_inter(d, "log_mi_z", "cmig", ref))
    if level >= 5: extras.append(yr_inter(d, "fx_z",     "cfx",  ref))
    if level >= 6:
        for c in REGION_COLS:
            if c in d.columns:
                extras.append(yr_inter(d, c, f"cX_{c}", ref))
    exog = pd.concat([d[cols]] + extras, axis=1) if extras else d[cols]
    return exog

# --------------------------------------------------------------------------
# 3. Wild cluster bootstrap
# --------------------------------------------------------------------------

def fit_one(df, level):
    exog = build_exog(df, level)
    y    = df["log_permits"]
    idx  = pd.MultiIndex.from_arrays([df.dname.values, df.year.values],
                                     names=["dname","year"])
    exog = exog.copy(); exog.index = idx
    y = y.copy(); y.index = idx
    m = PanelOLS(y, exog, entity_effects=True, time_effects=(level>=3),
                 drop_absorbed=True)
    return m.fit(cov_type="clustered", cluster_entity=True)

def wild_cluster_bootstrap(df, level, B=500):
    """Rademacher wild cluster bootstrap on residuals at dname level."""
    res = fit_one(df, level)
    beta_hat = float(res.params["treatment"])
    se_std   = float(res.std_errors["treatment"])
    t_std    = float(res.tstats["treatment"])
    p_std    = float(res.pvalues["treatment"])

    # Get residuals + fitted at original index (entity, year)
    fitted = res.fitted_values.reset_index()
    fitted.columns = ["dname","year","yhat"]
    resid  = res.resids.reset_index()
    resid.columns  = ["dname","year","e"]
    df_b = (df.merge(fitted, on=["dname","year"])
              .merge(resid,  on=["dname","year"])
              .copy())

    districts = df_b.dname.unique()
    n_d = len(districts)
    boot_betas = np.zeros(B)

    for b in range(B):
        # Rademacher signs per district
        signs = pd.Series(np.random.choice([-1.0, 1.0], size=n_d), index=districts, name="g")
        df_b["g"] = df_b["dname"].map(signs)
        df_b["y_star"] = df_b["yhat"] + df_b["g"] * df_b["e"]

        # Refit using y_star
        exog = build_exog(df_b, level)
        idx  = pd.MultiIndex.from_arrays([df_b.dname.values, df_b.year.values],
                                         names=["dname","year"])
        exog = exog.copy(); exog.index = idx
        y_star = df_b["y_star"].copy(); y_star.index = idx
        try:
            m = PanelOLS(y_star, exog, entity_effects=True,
                         time_effects=(level>=3), drop_absorbed=True)
            r = m.fit(cov_type="clustered", cluster_entity=True)
            boot_betas[b] = float(r.params["treatment"])
        except Exception:
            boot_betas[b] = np.nan

    boot_betas = boot_betas[~np.isnan(boot_betas)]
    se_boot = boot_betas.std()
    # Bootstrap-t p-value (symmetric, two-sided)
    t_boot   = boot_betas / se_boot
    t_actual = beta_hat / se_boot
    p_boot   = (np.abs(t_boot) > np.abs(t_actual)).mean()

    return {
        "beta":         round(beta_hat, 4),
        "se_cluster":   round(se_std, 4),
        "t_cluster":    round(t_std, 2),
        "p_cluster":    round(p_std, 4),
        "se_wildboot":  round(float(se_boot), 4),
        "t_wildboot":   round(beta_hat / float(se_boot), 2),
        "p_wildboot":   round(float(p_boot), 4),
        "B_used":       int(len(boot_betas)),
    }

print("Running wild cluster bootstrap (B=500) for M3, M4, M5, M6...")
print("This will take ~1 minute per spec.\n")
rows = []
for lvl in [3, 4, 5, 6]:
    r = wild_cluster_bootstrap(panel, lvl, B=500)
    r["spec"] = f"M{lvl}"
    rows.append(r)
    print(f"  M{lvl}: beta={r['beta']:.4f}  cluster t={r['t_cluster']:.2f} p={r['p_cluster']:.4f}  "
          f"||  wild-boot t={r['t_wildboot']:.2f} p={r['p_wildboot']:.4f}")

print("\nResults table:")
out = pd.DataFrame(rows)[["spec","beta","se_cluster","t_cluster","p_cluster",
                          "se_wildboot","t_wildboot","p_wildboot","B_used"]]
print(out.to_string(index=False))
out.to_csv("district-analysis/output/tab/diag_wild_cluster_bootstrap.csv", index=False)
print("\nSaved: output/tab/diag_wild_cluster_bootstrap.csv")
