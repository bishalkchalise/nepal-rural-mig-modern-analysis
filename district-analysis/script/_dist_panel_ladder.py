"""
District-level panel first-stage with Khanna spec ladder (M1 -> M4) and lags 0-3.

Spec (matching paper image):

  y_{d,t} =  beta * [ fx_{d,t} * log(mig_int_d) ]
           + lambda_1' (log(mig_int_d) * tau_t)        # C_mig
           + lambda_2' (fx_{d,t}      * tau_t)         # C_fx (z bare with year FE)
           + delta'   (X_{d,0}        * tau_t)         # C_X (year x dest-region shares)
           + alpha_d + gamma_t + eps_{d,t}

  fx_{d,t}  =  district SSIV  =  sum_c share_dc(v) * rer_{c,t-L}     (lag = L)
  rer_{c,t} =  log(NPR/LCU)_{c,t} - log(NPR/LCU)_{c,2010}
  mig_int_d =  2009-10 DOFE permits / pop_2011           (district-constant)

Ladder of controls:
  M1: just  beta * z_dt * log_mi  +  alpha_d + gamma_t
  M2: + C_mig                                            (log_mi * year FE)
  M3: + C_fx                                              (bare z_dt)
  M4: + C_X                                               (year x 6 region shares)

Lags applied to the SSIV: L = 0, 1, 2, 3 (replace z_dt with z_{d,t-L}).

Outcome: log(DOFE permits_{d,t} + 1), panel 2011-2023, 75 districts x 13 years.
SE clustered at  ~ dname.

Run for v1 (2001 census shares, 20 dest) and v2 (2009-10 DOFE shares, 14 dest).
"""
import os, numpy as np, pandas as pd, statsmodels.api as sm

# ---------------- load ----------------
dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

forex = pd.read_csv("district-analysis/data/clean/forex_2000_2023.csv").dropna(subset=["country"])
dofe_raw = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv").dropna(subset=["country"])
m01 = pd.read_csv("district-analysis/data/clean/dist_mig_pop_2001.csv")
pop_file = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")
regions = pd.read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv")

# ---------------- FX panel: rer_{c,t} ----------------
nepal_fx = forex[forex.country == "Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
        .merge(nepal_fx, on="year")
        .query("country not in ['Nepal','India']")
        .assign(npr_per_lcu=lambda d: d.npr_per_usd / d.lcu_per_usd))
fx["log_npr_per_lcu"] = np.log(fx.npr_per_lcu)
fx = fx[["country","year","log_npr_per_lcu"]].dropna()
b2010 = fx[fx.year == 2010].set_index("country").log_npr_per_lcu.rename("base_2010")
fx = fx.join(b2010, on="country").dropna(subset=["base_2010"])
fx["rer"] = fx["log_npr_per_lcu"] - fx["base_2010"]
fx_countries = set(fx.country.unique())

# ---------------- shares (v1, v2) ----------------
set_v1 = sorted(set(m01.country.unique()) & fx_countries)
dofe = (dofe_raw.assign(dname=lambda d: d.district_rename.map(to_dname))
                .groupby(["dname","country","year"]).total_migrants.sum().reset_index()
                .rename(columns={"total_migrants":"permits"}))
dofe = dofe[~dofe.country.isin(["Nepal","India"])]
v2_tot = dofe[dofe.year.isin([2009,2010])].groupby("country").permits.sum()
set_v2 = sorted(set(v2_tot[v2_tot >= 50].index) & fx_countries)

m01 = m01.rename(columns={"dist_mig_pop_2001":"mig01"})
sh_v1 = (m01[m01.country.isin(set_v1)]
            .assign(tot=lambda d: d.groupby("dname").mig01.transform("sum"))
            .assign(share=lambda d: d.mig01 / d.tot)[["dname","country","share"]])
sh_v2 = (dofe[dofe.year.isin([2009,2010]) & dofe.country.isin(set_v2)]
            .groupby(["dname","country"]).permits.sum().reset_index()
            .assign(tot=lambda d: d.groupby("dname").permits.transform("sum"))
            .assign(share=lambda d: d.permits / d.tot)[["dname","country","share"]])

# ---------------- district SSIV at every year (incl. pre-2011 for lags) ----------------
def build_z_panel(shares):
    return (shares.merge(fx, on="country", how="inner")
                  .assign(x=lambda d: d.share * d.rer)
                  .groupby(["dname","year"]).x.sum().rename("z").reset_index())

z_v1 = build_z_panel(sh_v1).rename(columns={"z":"z_v1"})
z_v2 = build_z_panel(sh_v2).rename(columns={"z":"z_v2"})

# ---------------- mig intensity (district-constant) ----------------
mi_num = (dofe[dofe.year.isin([2009,2010])].groupby("dname").permits.mean()
              .rename("num").reset_index())
pop = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
              [["dname","district_population_2011"]].drop_duplicates("dname")
              .rename(columns={"district_population_2011":"pop_2011"}))
mi = mi_num.merge(pop, on="dname").assign(mig_int=lambda d: d.num/d.pop_2011)
mi["log_mi"] = np.log(mi.mig_int.clip(lower=1e-12))
mi["log_mi_z"] = (mi.log_mi - mi.log_mi.mean()) / mi.log_mi.std()
mi = mi[["dname","log_mi_z"]]

# ---------------- panel grid 2011-2023 ----------------
districts = sorted(set(z_v1.dname) & set(mi.dname))
YRS = list(range(2011, 2024))
grid = pd.MultiIndex.from_product([districts, YRS],
                                  names=["dname","year"]).to_frame(index=False)

# DOFE permits at district level (sum across canonical destinations: union of v1 and v2)
dest_union = sorted(set(set_v1) | set(set_v2))
perm_d = (dofe[dofe.country.isin(dest_union)]
            .groupby(["dname","year"]).permits.sum().reset_index())
perm_panel = (grid.merge(perm_d, on=["dname","year"], how="left").fillna({"permits":0}))
perm_panel["log_perm"] = np.log(perm_panel.permits + 1)

# Merge shifter at various lags, plus mig_int, regions
panel = perm_panel.merge(mi, on="dname", how="inner")

# Region controls (district-constant)
REGION_COLS = ["share_e_asia","share_gulf","share_oecd_north","share_s_asia",
               "share_se_asia","share_oecd_europe"]  # drop "share_other" as reference
panel = panel.merge(regions[["dname"] + REGION_COLS], on="dname", how="left")

# Build z at lags L=0..3 by remapping year -> year-L
def add_z_lag(panel, z_df, ver, L):
    tmp = z_df.copy().rename(columns={"year":"year_src"})
    tmp["year"] = tmp["year_src"] + L  # so z(t=2011, L=2) pulls year_src=2009
    tmp = tmp.rename(columns={f"z_{ver}": f"z_{ver}_L{L}"})
    return panel.merge(tmp[["dname","year",f"z_{ver}_L{L}"]], on=["dname","year"], how="left")

for ver, zdf in [("v1", z_v1), ("v2", z_v2)]:
    for L in range(4):
        panel = add_z_lag(panel, zdf, ver, L)

# Standardize each z within panel
for ver in ["v1","v2"]:
    for L in range(4):
        col = f"z_{ver}_L{L}"
        s = panel[col].std(ddof=0)
        panel[f"{col}_std"] = panel[col] / s

# ---------------- regression engine ----------------
def fit_with_fe(df, ycol, x_terms, fe_cols, cluster):
    d = df.dropna(subset=[ycol] + x_terms + fe_cols + [cluster]).copy()
    dummies = pd.concat([pd.get_dummies(d[c], prefix=c, drop_first=True, dtype=float)
                         for c in fe_cols], axis=1)
    X = pd.concat([d[x_terms].astype(float).reset_index(drop=True),
                   dummies.reset_index(drop=True)], axis=1)
    X = sm.add_constant(X, has_constant="add")
    m = sm.OLS(d[ycol].values.astype(float), X.values.astype(float)).fit(
        cov_type="cluster", cov_kwds={"groups": d[cluster].values})
    out = {}
    for c in x_terms:
        if c not in X.columns: continue
        i = X.columns.get_loc(c)
        b = float(m.params[i]); se = float(m.bse[i]); p = float(m.pvalues[i])
        out[c] = {"beta":b, "se":se, "t":b/se if se>0 else np.nan, "p":p}
    out["n"] = int(m.nobs); out["r2"] = float(m.rsquared)
    return out

def stars(p):
    if p is None or np.isnan(p): return ""
    return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

# Helper to build year-interaction columns (for C_mig, C_fx, C_X)
def year_interact(df, base_col, year_col="year", prefix=None):
    """Returns list of new column names; modifies df in place."""
    yrs = sorted(df[year_col].unique())[1:]  # drop first year as reference
    cols = []
    for y in yrs:
        name = f"{prefix or base_col}_x_y{y}"
        df[name] = df[base_col] * (df[year_col] == y).astype(float)
        cols.append(name)
    return cols

# Add C_mig: log_mi_z * year FE
C_MIG = year_interact(panel, "log_mi_z", prefix="logmiz")

# Add C_X: each region share * year FE
C_X = []
for r in REGION_COLS:
    C_X += year_interact(panel, r, prefix=r)

# ---------------- run ladder x lags ----------------
results = []
for ver in ["v1", "v2"]:
    for L in range(4):
        # Define C_fx for this version+lag (bare z, time-varying, w/ year FE only here means just z)
        # but z varies by d,t, so include as a single regressor (NOT year-interacted to avoid collinearity)
        bare_z   = f"z_{ver}_L{L}_std"
        inter_z  = f"z_{ver}_L{L}_std_x_logmi"
        panel[inter_z] = panel[bare_z] * panel.log_mi_z

        # M1: bare interaction only
        x1 = [inter_z]
        # M2: + C_mig
        x2 = x1 + C_MIG
        # M3: + C_fx (bare z)
        x3 = x2 + [bare_z]
        # M4: + C_X
        x4 = x3 + C_X

        for mlabel, xs in [("M1", x1), ("M2", x2), ("M3", x3), ("M4", x4)]:
            r = fit_with_fe(panel, "log_perm", xs, fe_cols=["dname","year"], cluster="dname")
            if inter_z not in r: continue
            info = r[inter_z]
            d_used = panel.dropna(subset=["log_perm"]+xs+["dname","year"])
            mean_y = float(d_used.log_perm.mean())
            beta   = info["beta"]
            results.append({
                "version": ver, "lag": L, "model": mlabel,
                "term": "z * log_mi_z",
                "beta": round(beta,4), "se": round(info["se"],4),
                "t": round(info["t"],2), "p": round(info["p"],4),
                "sig": stars(info["p"]),
                "mean_y": round(mean_y,4),
                "pct_of_mean": round(100*beta/mean_y,2) if mean_y!=0 else None,
                "n": r["n"], "r2": round(r["r2"],4),
                "n_controls": len(xs) - 1,
            })

out = pd.DataFrame(results)
out["beta_with_sig"] = out.apply(lambda r: f"{r.beta:.4f}{r.sig}", axis=1)

print(f"{'ver':<4} {'lag':>3} {'model':<5} {'beta(***)':<12} {'se':>8} {'t':>6} "
      f"{'mean_y':>9} {'b/Y_%':>8} {'n':>5} {'r2':>7} {'#ctrl':>6}")
print("-"*88)
for _, r in out.iterrows():
    bs = f"{r.beta:.4f}{r.sig}"
    print(f"{r.version:<4} {r.lag:>3} {r.model:<5} {bs:<12} {r.se:>8.4f} "
          f"{r.t:>6.2f} {r.mean_y:>9.4f} {r.pct_of_mean:>7.2f}% "
          f"{r.n:>5} {r.r2:>7.4f} {r.n_controls:>6}")

os.makedirs("district-analysis/output/tab", exist_ok=True)
out.to_csv("district-analysis/output/tab/dist_panel_ladder.csv", index=False)
print(f"\nSaved: output/tab/dist_panel_ladder.csv ({len(out)} rows)")
