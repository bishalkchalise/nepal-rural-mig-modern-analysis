"""
BHJ (Borusyak-Hull-Jaravel) exposure-design panel SSIV at the (district x
country x year) level.

Canonical spec:
  log(permits_dct + 1) ~ beta * z_ct                | dname x year + country
                                                       cluster two-way ~ dname, country

  z_ct = log(fx_npr_per_lcu_index_ct)  in Khanna/NPR-per-LCU convention.
         Rises when destination currency appreciates vs NPR.

Why BHJ here:
  - The district x year FE absorbs ALL district-year confounds (intensity,
    labour market, country-FE-by-year not at issue).
  - country FE absorbs destination base-attractiveness levels.
  - identification: residual variation in z_ct after partialling those FEs;
    i.e. how destination-specific currency dynamics shift permits within
    a district x year, relative to other destinations that district uses.
  - The Gulf-peg problem matters less here because we identify off ALL
    destinations' relative movements within district-year; we don't need
    cross-district variation in the shifter.

Variants:
  BHJ-1 : bare shock      log_fx_z | dname^year + country
  BHJ-2 : + country trend log_fx_z | dname^year + country + country:trend
  BHJ-3 : + interaction with district 2001/DOFE mig_int (intensity-scaled)
"""

import os, numpy as np, pandas as pd
from linearmodels.panel import PanelOLS
np.random.seed(2026)

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

forex     = pd.read_csv("district-analysis/data/clean/forex_2000_2023.csv")
dofe      = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")

# Destination FX panel: NPR per LCU, index to 2009-10 mean
nepal_fx = forex[forex.country=="Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
            .merge(nepal_fx, on="year")
            .query("country not in ['Nepal','India']")
            .assign(fx_npr_per_lcu = lambda d: d.npr_per_usd / d.lcu_per_usd))
base = fx[fx.year.isin([2009,2010])].groupby("country").fx_npr_per_lcu.mean().rename("base").reset_index()
fx = fx.merge(base, on="country").assign(fx_index = lambda d: d.fx_npr_per_lcu / d.base)
fx = fx.assign(log_fx_index = lambda d: np.log(d.fx_index)).dropna(subset=["log_fx_index"])
fx = fx[["country","year","fx_index","log_fx_index"]]

# District-country-year DOFE panel
dofe = (dofe.groupby(["district_rename","country","year"]).total_migrants.sum().reset_index()
              .assign(dname = lambda d: d.district_rename.map(to_dname)))
dofe = dofe[~dofe.country.isin(["Nepal","India"])]

# District mig_int (DOFE-vintage)
dy = (dofe.groupby(["dname","year"]).total_migrants.sum().reset_index()
            .rename(columns={"total_migrants":"permits"}))
mi_num = (dy[dy.year.isin([2009,2010])].groupby("dname").permits.mean().rename("num").reset_index())
pop = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
              [["dname","district_population_2011"]].drop_duplicates("dname")
              .rename(columns={"district_population_2011":"pop_2011"}))
mi = mi_num.merge(pop,on="dname").assign(mig_int_dofe=lambda d: d.num/d.pop_2011)[["dname","mig_int_dofe"]]

# Restrict to top-N destinations to keep estimation tractable + meaningful
TOP_N = 25
top_countries = (dofe.groupby("country").total_migrants.sum()
                  .sort_values(ascending=False).head(TOP_N).index.tolist())
print(f"Restricting to top {TOP_N} destinations:")
for i, c in enumerate(top_countries, 1):
    print(f"  {i:2}. {c}")

dofe_top = dofe[dofe.country.isin(top_countries)].copy()

# Build full panel (district x country x year), filling zeros for missing (so
# every district has a row for every top destination in every year)
all_years = sorted(dofe_top.year.unique())
all_dnames = sorted(dofe_top.dname.unique())
grid = pd.MultiIndex.from_product([all_dnames, top_countries, all_years],
                                  names=["dname","country","year"]).to_frame(index=False)
panel = (grid.merge(dofe_top[["dname","country","year","total_migrants"]],
                    on=["dname","country","year"], how="left")
              .fillna({"total_migrants":0}))
panel = panel.merge(fx, on=["country","year"], how="inner")
panel = panel.merge(mi, on="dname", how="left")
panel["permits"] = panel.total_migrants.astype(float)
panel["log_permits"] = np.log(panel.permits + 1)

print(f"\nFull panel: {len(panel):,} obs "
      f"({panel.dname.nunique()} districts x {panel.country.nunique()} dests x {panel.year.nunique()} years)")

def z(s):
    sd = s.std(); return (s - s.mean())/sd if sd>0 else pd.Series(0., index=s.index)

panel["log_fx_z"]  = z(panel.log_fx_index)
panel["log_mi_z"]  = z(np.log(panel.mig_int_dofe.clip(lower=1e-12)))

# Build dname_year for entity FE
panel["dname_year"] = panel.dname.astype(str) + ":" + panel.year.astype(str)

# -------------------------------------------------------------------
# BHJ-1:  log(permits) ~ log_fx_z   | dname x year + country
# -------------------------------------------------------------------
# In linearmodels, we want entity_effects = dname_year and one absorbed
# country FE.  Use PanelOLS with index (dname_year, country) so entity =
# dname_year is the high-dim FE, country is the second FE absorbed.

def fit_bhj(df, regressors, label):
    d = df.copy()
    # Add country dummies (drop one)
    # linearmodels can do time_effects but that's year; we want country as
    # second FE.  Trick: set the panel index to (dname_year, country) and
    # treat country as time_effects.
    country_codes = {c: i for i, c in enumerate(sorted(d.country.unique()))}
    d["country_code"] = d.country.map(country_codes)
    d = d.set_index(["dname_year","country_code"])
    y = d["log_permits"]
    X = d[regressors]
    mod = PanelOLS(y, X, entity_effects=True, time_effects=True, drop_absorbed=True)
    r = mod.fit(cov_type="clustered", cluster_entity=True)
    return r, label

specs = []
# Build interactions
panel["fx_x_logmi"] = panel.log_fx_z * panel.log_mi_z

# BHJ-1: bare shock
specs.append(("BHJ-1 bare shock", ["log_fx_z"]))
# BHJ-2: shock + intensity interaction
specs.append(("BHJ-2 +interact", ["log_fx_z","fx_x_logmi"]))

print("\nBHJ panel SSIV results (cluster ~dname):")
print(f"{'spec':<22}  {'regressor':<20} {'beta':>10} {'se':>8} {'t':>7} {'p':>9} {'n':>8}")
print("-"*80)

rows = []
for label, regs in specs:
    r, lab = fit_bhj(panel, regs, label)
    for v in regs:
        if v in r.params.index:
            bh = float(r.params[v]); seh = float(r.std_errors[v])
            tv = float(r.tstats[v]);  pv = float(r.pvalues[v])
            sig = "***" if pv<0.01 else "**" if pv<0.05 else "*" if pv<0.1 else ""
            print(f"{lab:<22}  {v:<20} {bh:>10.4f} {seh:>8.4f} {tv:>7.2f} {pv:>9.4f} {int(r.nobs):>8}")
            rows.append({"spec":lab, "regressor":v, "beta":round(bh,4),
                         "se":round(seh,4), "t":round(tv,2), "p":round(pv,4),
                         "sig":sig, "n":int(r.nobs),
                         "mean_y":round(float(panel["log_permits"].mean()),4)})

pd.DataFrame(rows).to_csv("district-analysis/output/tab/bhj_panel_ssiv.csv", index=False)
print("\nSaved: output/tab/bhj_panel_ssiv.csv")
