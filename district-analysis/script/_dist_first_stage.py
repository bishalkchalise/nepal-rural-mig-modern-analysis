"""
District-level BHJ first-stage (cross-section).

District shifter:
  z_d,t^(v) = sum_{c in set_v} share_dc^(v) * rer_{c,t}

where rer_{c,t} = log(NPR per LCU)_t - log(NPR per LCU)_{c,2010}
                   ( > 0 = NPR depreciated since 2010 )

Two versions:
  v1: 20 dest, share_dc from 2001 census migration
  v2: 14 dest, share_dc from DOFE 2009-10 average (destinations with >=50 permits)

For each outcome group we pick the appropriate rer window:
  - Census 2021 outcomes      -> rer_{c,2011-2021} averaged
  - DOFE cumulative 2011-23   -> rer_{c,2011-2023} averaged
  - RVS 2016-18 outcomes      -> rer_{c,2016-2018} averaged

Regression:  outcome_d = alpha + beta * z_d_std + eps_d   (HC1 SE, 1 obs / dist)
"""
import os, numpy as np, pandas as pd
import statsmodels.api as sm

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
census = pd.read_csv("district-analysis/data/clean/census/outcomes_district.csv")
rvs = pd.read_csv("district-analysis/data/clean/rvs/migration_district_year.csv")
rvs["dname"] = rvs.dname_raw.map(to_dname)

# ---------------- FX panel: rer_ct ----------------
nepal_fx = forex[forex.country == "Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
        .merge(nepal_fx, on="year")
        .query("country not in ['Nepal','India']")
        .assign(npr_per_lcu=lambda d: d.npr_per_usd / d.lcu_per_usd))
fx["log_npr_per_lcu"] = np.log(fx.npr_per_lcu)
fx = fx[["country","year","log_npr_per_lcu"]].dropna()
base2010 = fx[fx.year == 2010].set_index("country").log_npr_per_lcu.rename("base_2010")
fx = fx.join(base2010, on="country").dropna(subset=["base_2010"])
fx["rer"] = fx["log_npr_per_lcu"] - fx["base_2010"]
fx_countries = set(fx.country.unique())

# ---------------- destination sets (canonical) ----------------
set_v1 = sorted(set(m01.country.unique()) & fx_countries)
dofe = (dofe_raw.assign(dname=lambda d: d.district_rename.map(to_dname))
                .groupby(["dname","country","year"]).total_migrants.sum().reset_index()
                .rename(columns={"total_migrants":"permits"}))
dofe = dofe[~dofe.country.isin(["Nepal","India"])]
v2_totals = dofe[dofe.year.isin([2009,2010])].groupby("country").permits.sum()
set_v2 = sorted(set(v2_totals[v2_totals >= 50].index) & fx_countries)
print(f"v1: {len(set_v1)} dest  |  v2: {len(set_v2)} dest")

# ---------------- shares ----------------
m01 = m01.rename(columns={"dist_mig_pop_2001":"mig01"})
sh_v1 = (m01[m01.country.isin(set_v1)]
            .assign(tot=lambda d: d.groupby("dname").mig01.transform("sum"))
            .assign(share=lambda d: d.mig01 / d.tot)
            [["dname","country","share"]])

sh_v2 = (dofe[dofe.year.isin([2009,2010]) & dofe.country.isin(set_v2)]
            .groupby(["dname","country"]).permits.sum().reset_index()
            .assign(tot=lambda d: d.groupby("dname").permits.transform("sum"))
            .assign(share=lambda d: d.permits / d.tot)
            [["dname","country","share"]])

# ---------------- district-level shifter z_d(version, window) ----------------
def build_zd(shares, window_years):
    """Return DataFrame[dname, z_d] = sum_c share_dc * mean_{t in window} rer_ct"""
    mean_rer = (fx[fx.year.isin(window_years)].groupby("country").rer
                  .mean().rename("mean_rer").reset_index())
    z = (shares.merge(mean_rer, on="country", how="inner")
                .assign(x=lambda d: d.share * d.mean_rer)
                .groupby("dname").x.sum().rename("z").reset_index())
    return z

# Three windows
windows = {
    "census_2011_2021": list(range(2011, 2022)),
    "dofe_2011_2023":   list(range(2011, 2024)),
    "rvs_2016_2018":    [2016, 2017, 2018],
}

zd = {}
for tag, yrs in windows.items():
    zv1 = build_zd(sh_v1, yrs).rename(columns={"z":"z_v1"})
    zv2 = build_zd(sh_v2, yrs).rename(columns={"z":"z_v2"})
    zd[tag] = zv1.merge(zv2, on="dname", how="outer").fillna(0.)

# ---------------- assemble outcomes ----------------
# Census 2021
c21 = census[census.year == 2021][["dname","absent_hh_share","mig_in_international","mig_in_share"]].copy()

# DOFE cumulative + mean across union destinations
dest_union = sorted(set(set_v1) | set(set_v2))
dofe_used = (dofe[dofe.country.isin(dest_union) & dofe.year.between(2011,2023)]
              .groupby("dname").permits.agg(perm_total="sum", perm_mean="mean")
              .reset_index())
dofe_used["log_perm_total_2011_23"] = np.log(dofe_used.perm_total + 1)
dofe_used["log_perm_mean_2011_23"]  = np.log(dofe_used.perm_mean  + 1)

# RVS 2016-18 averaged
rvs_used = (rvs.groupby("dname")
              .agg(n_hh=("n_hh","sum"),
                   n_hh_intl=("n_hh_with_intl_migrant","sum"),
                   n_intl_mig=("n_intl_migrants","sum"),
                   remit_intl=("remit_amount_intl_12m_rs","sum"),
                   wt_intl_mig=("wt_intl_migrants","sum"))
              .reset_index())
rvs_used["hh_intl_share"]      = rvs_used.n_hh_intl   / rvs_used.n_hh
rvs_used["intl_per_hh"]        = rvs_used.n_intl_mig  / rvs_used.n_hh
rvs_used["log_wt_intl_mig"]    = np.log(rvs_used.wt_intl_mig + 1)
rvs_used["log_remit_per_hh"]   = np.log((rvs_used.remit_intl/rvs_used.n_hh) + 1)

# ---------------- regression engine ----------------
def fit(y, x_std, df):
    d = df.dropna(subset=[y, x_std]).copy()
    if len(d) < 20: return None
    X = sm.add_constant(d[[x_std]].astype(float))
    m = sm.OLS(d[y].values.astype(float), X.values.astype(float)).fit(cov_type="HC1")
    b = float(m.params[1]); se = float(m.bse[1]); p = float(m.pvalues[1])
    return {"beta":round(b,4), "se":round(se,4), "t":round(b/se,2),
            "p":round(p,4), "ci_lo":round(b-1.96*se,4), "ci_hi":round(b+1.96*se,4),
            "mean_y":round(float(d[y].mean()),4), "n":int(m.nobs)}

def stars(p):
    if p is None or np.isnan(p): return ""
    return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

def run_block(label, outcomes, window_tag, base_df):
    z = zd[window_tag].copy()
    # Standardize within this window (one z per district)
    z["z_v1_std"] = z.z_v1 / z.z_v1.std(ddof=0)
    z["z_v2_std"] = z.z_v2 / z.z_v2.std(ddof=0)
    panel = base_df.merge(z, on="dname", how="inner")

    print(f"\n=== {label}  (n_dist = {len(panel)}, window = {window_tag}) ===")
    print(f"{'outcome':<28} {'ver':<4} {'beta':>8} {'se':>7} {'t':>6} {'sig':>4} "
          f"{'mean_y':>9} {'n':>4}")
    print("-" * 76)
    rows = []
    for y in outcomes:
        if y not in panel.columns: continue
        for ver, zcol in [("v1", "z_v1_std"), ("v2", "z_v2_std")]:
            r = fit(y, zcol, panel)
            if r is None: continue
            r.update({"outcome":y, "version":ver, "group":label})
            rows.append(r)
            print(f"{y:<28} {ver:<4} {r['beta']:>8.4f} {r['se']:>7.4f} "
                  f"{r['t']:>6.2f} {stars(r['p']):>4} {r['mean_y']:>9.4f} {r['n']:>4}")
    return rows

all_rows = []
# Census 2021
panel_c = c21.merge(zd["census_2011_2021"], on="dname", how="inner")
all_rows += run_block("CENSUS 2021",
                      ["absent_hh_share","mig_in_international","mig_in_share"],
                      "census_2011_2021", c21)

# DOFE cumulative + mean
all_rows += run_block("DOFE 2011-23 cumulative/mean",
                      ["log_perm_total_2011_23","log_perm_mean_2011_23"],
                      "dofe_2011_2023", dofe_used)

# RVS 2016-18
all_rows += run_block("RVS 2016-18 mean",
                      ["hh_intl_share","intl_per_hh","log_wt_intl_mig","log_remit_per_hh"],
                      "rvs_2016_2018", rvs_used)

# ---------------- save ----------------
os.makedirs("district-analysis/output/tab", exist_ok=True)
out = pd.DataFrame(all_rows)[["group","outcome","version","beta","se","t","p",
                              "ci_lo","ci_hi","mean_y","n"]]
out["sig"] = out.p.apply(stars)
out.to_csv("district-analysis/output/tab/dist_first_stage.csv", index=False)
print(f"\nSaved: output/tab/dist_first_stage.csv ({len(out)} rows)")
