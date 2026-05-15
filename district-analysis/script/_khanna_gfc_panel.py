"""
Khanna GFC shifter embedded in DOFE permits panel (district x year, 2009-2023).

  y_dt = alpha_d + delta_t
       + b1 * (z_d_z * post_t)
       + b2 * (z_d_z * post_t * log_mi_z)
       + b3 * (log_mi_z * post_t)            # C_mig
       + eps_dt

post_t = I(year >= 2011).  z_d built from pre(2001-04) vs post(2009-10) FX gap.
Cluster ~ dname.

This kills the cross-sectional baseline confound: anything time-invariant about
high-Japan/Malaysia districts is absorbed by district FE.  The only thing that
identifies b1 is the differential post-2010 trajectory of permits as a function
of z_d.
"""
import numpy as np, pandas as pd
from linearmodels.panel import PanelOLS

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper(); return dofe_to_census.get(u, str(s).strip().title())

forex     = pd.read_csv("district-analysis/data/clean/forex_2000_2023.csv")
dofe_raw  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")

# FX in Khanna direction
nepal_fx = forex[forex.country=="Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
            .merge(nepal_fx, on="year")
            .query("country not in ['Nepal','India']")
            .assign(fx_lcu_per_npr=lambda d: d.lcu_per_usd/d.npr_per_usd))
fx["log_fx"] = np.log(fx.fx_lcu_per_npr)
fx = fx[["country","year","log_fx"]].dropna()

# g_c: pre 2001-2004 vs post 2009-2010
PRE_YRS  = [2001, 2002, 2003, 2004]
POST_YRS = [2009, 2010]
gc = (fx[fx.year.isin(PRE_YRS + POST_YRS)]
        .assign(period=lambda d: np.where(d.year.isin(PRE_YRS), "pre", "post"))
        .groupby(["country","period"]).log_fx.mean().unstack())
gc["g_c"] = gc["post"] - gc["pre"]
gc = gc.reset_index().dropna(subset=["g_c"])

# DOFE
dofe = (dofe_raw.groupby(["district_rename","country","year"]).total_migrants.sum().reset_index()
            .assign(dname=lambda d: d.district_rename.map(to_dname)))
dofe = dofe[~dofe.country.isin(["Nepal","India"])]

# Shares 2009-10
shares = (dofe[dofe.year.isin([2009,2010])]
          .groupby(["dname","country"]).total_migrants.sum().reset_index()
          .assign(tot=lambda d: d.groupby("dname").total_migrants.transform("sum"))
          .assign(share=lambda d: d.total_migrants/d.tot)
          [["dname","country","share"]])

z_d = (shares.merge(gc[["country","g_c"]], on="country", how="inner")
              .assign(x=lambda d: d.share * d.g_c)
              .groupby("dname").x.sum().rename("z_d").reset_index())

# mig_int (DOFE-vintage)
mi_num = (dofe.groupby(["dname","year"]).total_migrants.sum().reset_index()
              .rename(columns={"total_migrants":"permits"})
              .pipe(lambda d: d[d.year.isin([2009,2010])])
              .groupby("dname").permits.mean().rename("num").reset_index())
pop = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
              [["dname","district_population_2011"]].drop_duplicates("dname")
              .rename(columns={"district_population_2011":"pop_2011"}))
mi = mi_num.merge(pop, on="dname").assign(mig_int_dofe=lambda d: d.num/d.pop_2011)[["dname","mig_int_dofe"]]

base = z_d.merge(mi, on="dname", how="inner")
base["log_mi_z"] = (np.log(base.mig_int_dofe.clip(lower=1e-12))
                    - np.log(base.mig_int_dofe.clip(lower=1e-12)).mean()
                   ) / np.log(base.mig_int_dofe.clip(lower=1e-12)).std()
base["z_d_z"]    = (base.z_d - base.z_d.mean()) / base.z_d.std()

# DOFE permits panel
permits = (dofe.groupby(["dname","year"]).total_migrants.sum().reset_index()
                .rename(columns={"total_migrants":"permits"}))
permits = permits[permits.year.between(2009, 2023)]
# Balance: fill missing district-year with 0 permits so log(p+1)=0
all_d  = base.dname.unique()
all_y  = list(range(2009, 2024))
grid   = pd.MultiIndex.from_product([all_d, all_y], names=["dname","year"]).to_frame(index=False)
permits = grid.merge(permits, on=["dname","year"], how="left").fillna({"permits":0})
permits["log_p"] = np.log(permits.permits + 1)

panel = permits.merge(base[["dname","z_d_z","log_mi_z"]], on="dname", how="inner")
panel["post"]            = (panel.year >= 2011).astype(int)
panel["z_x_post"]        = panel.z_d_z * panel.post
panel["mi_x_post"]       = panel.log_mi_z * panel.post
panel["z_x_mi_x_post"]   = panel.z_d_z * panel.log_mi_z * panel.post

print(f"Panel: {panel.dname.nunique()} districts x {panel.year.nunique()} years = {len(panel)} obs")
print(f"Years: {sorted(panel.year.unique())}")
print(f"Pre years (post=0): {sorted(panel[panel.post==0].year.unique())}")
print(f"Post years (post=1): {sorted(panel[panel.post==1].year.unique())}\n")

def fit_panel(spec_name, exog_cols, y_col="log_p"):
    df = panel.dropna(subset=[y_col]+exog_cols).copy()
    idx = pd.MultiIndex.from_arrays([df.dname.values, df.year.values], names=["dname","year"])
    Xe = df[exog_cols].copy(); Xe.index = idx
    yd = df[y_col].copy();     yd.index = idx
    m = PanelOLS(yd, Xe, entity_effects=True, time_effects=True, drop_absorbed=True)
    r = m.fit(cov_type="clustered", cluster_entity=True)
    rows = []
    for c in exog_cols:
        if c not in r.params.index: continue
        b = float(r.params[c]); se = float(r.std_errors[c]); p = float(r.pvalues[c])
        rows.append({"spec":spec_name, "term":c,
                     "beta":round(b,4), "se":round(se,4),
                     "t":round(b/se,2), "p":round(p,4),
                     "n":int(r.nobs), "r2":round(float(r.rsquared),4)})
    return rows

def stars(p):
    if p is None or np.isnan(p): return ""
    return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

results = []
# Spec A: bare z*post (no controls, no interaction)
results += fit_panel("A_bare", ["z_x_post"])
# Spec B: bare + interaction
results += fit_panel("B_inter", ["z_x_post","z_x_mi_x_post"])
# Spec C: bare + C_mig
results += fit_panel("C_Cmig", ["z_x_post","mi_x_post"])
# Spec D: full spec (bare + interaction + C_mig)
results += fit_panel("D_full", ["z_x_post","z_x_mi_x_post","mi_x_post"])

out = pd.DataFrame(results)
out["sig"] = out.p.apply(stars)
print("DOFE permits panel, log(permits+1), district FE + year FE, cluster ~dname")
print(f"{'spec':<10} {'term':<18} {'beta':>9} {'se':>7} {'t':>6} {'sig':>4} {'n':>5} {'r2':>7}")
print("-"*75)
for _, r in out.iterrows():
    print(f"{r.spec:<10} {r.term:<18} {r.beta:>9.4f} {r.se:>7.4f} {r.t:>6.2f} {r.sig:>4} {r.n:>5} {r.r2:>7.4f}")

out.to_csv("district-analysis/output/tab/khanna_gfc_panel.csv", index=False)
print(f"\nSaved: output/tab/khanna_gfc_panel.csv ({len(out)} rows)")
