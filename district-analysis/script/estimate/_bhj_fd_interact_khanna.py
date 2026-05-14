"""
BHJ FD with intensity interaction — Khanna full-spec analogue:
  Delta_y_dt ~ beta * (Delta_z_{d,t-L} * log_mi_z_d) + gamma * Delta_z_{d,t-L}  | year FE
Where Delta_z is share-weighted Delta_log_fx in LCU/NPR direction (Khanna).
Lag L in {0, 1, 2}; cluster ~dname.
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

nepal_fx = forex[forex.country=="Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
# Khanna direction: LCU per NPR
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
            .merge(nepal_fx, on="year")
            .query("country not in ['Nepal','India']")
            .assign(fx_lcu_per_npr=lambda d: d.lcu_per_usd/d.npr_per_usd))
base = fx[fx.year.isin([2009,2010])].groupby("country").fx_lcu_per_npr.mean().rename("base").reset_index()
fx = (fx.merge(base,on="country")
        .assign(log_fx=lambda d: np.log(d.fx_lcu_per_npr/d.base))
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

dy = (dofe.groupby(["dname","year"]).total_migrants.sum().reset_index()
            .rename(columns={"total_migrants":"permits"}).sort_values(["dname","year"]))
dy["log_permits"]   = np.log(dy.permits + 1)
dy["d_log_permits"] = dy.groupby("dname")["log_permits"].diff()

z_lvl = (shares.merge(fx, on="country").assign(x=lambda d: d.share*d.log_fx)
              .groupby(["dname","year"]).x.sum().rename("z").reset_index())
dz    = (shares.merge(fx, on="country").assign(x=lambda d: d.share*d.d_log_fx)
              .groupby(["dname","year"]).x.sum().rename("dz").reset_index())

pop = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
              [["dname","district_population_2011"]].drop_duplicates("dname")
              .rename(columns={"district_population_2011":"pop_2011"}))
mi_num = (dy[dy.year.isin([2009,2010])].groupby("dname").permits.mean().rename("num").reset_index())
mi = mi_num.merge(pop,on="dname").assign(mig_int_dofe=lambda d: d.num/d.pop_2011)[["dname","mig_int_dofe"]]

panel = (dy[["dname","year","log_permits","d_log_permits"]]
           .merge(z_lvl, on=["dname","year"], how="left")
           .merge(dz,    on=["dname","year"], how="left")
           .merge(mi,    on="dname")
           .sort_values(["dname","year"]))

for L in (0,1,2):
    panel[f"z_L{L}"]  = panel.groupby("dname")["z"].shift(L)
    panel[f"dz_L{L}"] = panel.groupby("dname")["dz"].shift(L)

def zsc(s):
    sd=s.std(); return (s-s.mean())/sd if sd>0 else pd.Series(0.,index=s.index)

panel["log_mi_z"] = zsc(np.log(panel.mig_int_dofe.clip(lower=1e-12)))
for c in ["z","dz","z_L0","z_L1","z_L2","dz_L0","dz_L1","dz_L2"]:
    panel[c+"_z"] = zsc(panel[c])

# Build interaction columns
for L in (0,1,2):
    panel[f"dz_L{L}_x_logmi"] = panel[f"dz_L{L}_z"] * panel["log_mi_z"]
    panel[f"z_L{L}_x_logmi"]  = panel[f"z_L{L}_z"]  * panel["log_mi_z"]

def stars(p): return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

# Level: y ~ z + z*log_mi | dname+year FE
def fit_level(df, lag):
    d = df.dropna(subset=["log_permits", f"z_L{lag}_z"]).copy()
    exog = d[[f"z_L{lag}_z", f"z_L{lag}_x_logmi"]].copy()
    exog.columns = ["bare","inter"]
    idx = pd.MultiIndex.from_arrays([d.dname.values, d.year.values], names=["dname","year"])
    exog.index = idx
    y = d["log_permits"].copy(); y.index = idx
    m = PanelOLS(y, exog, entity_effects=True, time_effects=True, drop_absorbed=True)
    return m.fit(cov_type="clustered", cluster_entity=True), d

def fit_fd(df, lag):
    d = df.dropna(subset=["d_log_permits", f"dz_L{lag}_z"]).copy()
    exog = d[[f"dz_L{lag}_z", f"dz_L{lag}_x_logmi"]].copy()
    exog.columns = ["bare","inter"]
    idx = pd.MultiIndex.from_arrays([d.dname.values, d.year.values], names=["dname","year"])
    exog.index = idx
    y = d["d_log_permits"].copy(); y.index = idx
    m = PanelOLS(y, exog, entity_effects=False, time_effects=True, drop_absorbed=True)
    return m.fit(cov_type="clustered", cluster_entity=True), d

print(f"{'design':<20} {'lag':>3} {'term':<10} {'beta':>9} {'se':>8} {'t':>6} {'p':>8} {'sig':>4} {'mean_y':>10} {'n':>5}")
print("-"*92)
rows = []
for L in (0,1,2):
    res, d_used = fit_level(panel, L)
    for term in ["bare","inter"]:
        bh=float(res.params[term]); seh=float(res.std_errors[term]); pv=float(res.pvalues[term])
        s = stars(pv)
        print(f"{'LEVEL | dname+yr FE':<20} {L:>3} {term:<10} {bh:>9.4f} {seh:>8.4f} {bh/seh:>6.2f} {pv:>8.4f} {s:>4} {d_used.log_permits.mean():>10.3f} {int(res.nobs):>5}")
        rows.append({"design":"LEVEL","lag":L,"term":term,"beta":round(bh,4),"se":round(seh,4),"t":round(bh/seh,2),"p":round(pv,4),"sig":s,"mean_y":round(d_used.log_permits.mean(),4),"n":int(res.nobs)})

print()
for L in (0,1,2):
    res, d_used = fit_fd(panel, L)
    for term in ["bare","inter"]:
        bh=float(res.params[term]); seh=float(res.std_errors[term]); pv=float(res.pvalues[term])
        s = stars(pv)
        print(f"{'FD    | year FE':<20} {L:>3} {term:<10} {bh:>9.4f} {seh:>8.4f} {bh/seh:>6.2f} {pv:>8.4f} {s:>4} {d_used.d_log_permits.mean():>10.3f} {int(res.nobs):>5}")
        rows.append({"design":"FD","lag":L,"term":term,"beta":round(bh,4),"se":round(seh,4),"t":round(bh/seh,2),"p":round(pv,4),"sig":s,"mean_y":round(d_used.d_log_permits.mean(),4),"n":int(res.nobs)})

pd.DataFrame(rows).to_csv("district-analysis/output/tab/bhj_fd_with_interact.csv", index=False)
print("\nSaved: output/tab/bhj_fd_with_interact.csv")
