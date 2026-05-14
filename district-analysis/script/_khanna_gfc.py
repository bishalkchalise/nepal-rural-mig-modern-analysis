"""
Khanna-style cross-sectional SSIV using a single pre-vs-post-GFC shifter.

  g_c = avg log(LCU/NPR)_{c, 2009-2010}  -  avg log(LCU/NPR)_{c, 2001-2004}
        (Khanna direction: LCU per NPR.  g_c < 0  =  NPR depreciated against c.)

  z_d = sum_c share_dc(2009-10 DOFE)  *  g_c     (ONE number per district)

Three pieces:
  A. Per-destination g_c table  (top 12 destinations)
  B. Validation: does z_d predict POST-2010 cumulative FX trajectory?
                (Khanna's "shifter correlates with future FX")
  C. First-stage: cross-sectional regressions of district migration
     outcomes on z_d (and z_d * log_mig_int).

Outcomes tested:
  - cumulative DOFE permits over 2011-2023 (log)
  - cumulative DOFE permit GROWTH 2011-2023 vs 2009-2010 baseline
  - census migration variables (mig_in_*, absent_hh_share) at 2021
"""
import numpy as np, pandas as pd
import statsmodels.api as sm

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper(); return dofe_to_census.get(u, str(s).strip().title())

forex     = pd.read_csv("district-analysis/data/clean/forex_2000_2023.csv")
dofe_raw  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file  = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")
outcomes  = pd.read_csv("district-analysis/data/clean/census/outcomes_district.csv")

# FX in Khanna direction
nepal_fx = forex[forex.country=="Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
            .merge(nepal_fx, on="year")
            .query("country not in ['Nepal','India']")
            .assign(fx_lcu_per_npr=lambda d: d.lcu_per_usd/d.npr_per_usd))
fx["log_fx"] = np.log(fx.fx_lcu_per_npr)
fx = fx[["country","year","log_fx"]].dropna()

# === A. g_c per country: pre 2001-2004 vs post 2009-2010 ===================
PRE_YRS  = [2001, 2002, 2003, 2004]
POST_YRS = [2009, 2010]
gc = (fx[fx.year.isin(PRE_YRS + POST_YRS)]
        .assign(period=lambda d: np.where(d.year.isin(PRE_YRS), "pre", "post"))
        .groupby(["country","period"]).log_fx.mean().unstack())
gc["g_c"] = gc["post"] - gc["pre"]
gc = gc.reset_index()

# === Top destinations by DOFE total ========================================
dofe = (dofe_raw.groupby(["district_rename","country","year"]).total_migrants.sum().reset_index()
            .assign(dname=lambda d: d.district_rename.map(to_dname)))
dofe = dofe[~dofe.country.isin(["Nepal","India"])]
top12 = (dofe.groupby("country").total_migrants.sum().sort_values(ascending=False).head(12).index.tolist())
print("g_c (avg log LCU/NPR change pre-2001-04 -> post-2009-10) for top 12 destinations:")
print(gc[gc.country.isin(top12)].sort_values("g_c").to_string(index=False))

# === Construct z_d (one per district) ======================================
shares = (dofe[dofe.year.isin([2009,2010])]
          .groupby(["dname","country"]).total_migrants.sum().reset_index()
          .assign(tot=lambda d: d.groupby("dname").total_migrants.transform("sum"))
          .assign(share=lambda d: d.total_migrants/d.tot)
          [["dname","country","share"]])

z_d = (shares.merge(gc[["country","g_c"]], on="country", how="inner")
              .assign(x=lambda d: d.share * d.g_c)
              .groupby("dname").x.sum().rename("z_d").reset_index())
print(f"\nz_d distribution across {len(z_d)} districts:")
print(f"  min   = {z_d.z_d.min():.4f}")
print(f"  median= {z_d.z_d.median():.4f}")
print(f"  mean  = {z_d.z_d.mean():.4f}")
print(f"  max   = {z_d.z_d.max():.4f}")
print(f"  sd    = {z_d.z_d.std():.4f}")

# === B. Validation: does z_d predict POST-2010 cumulative FX? ==============
# Build "future cumulative FX shock" per district = sum_c share * (log_fx_2023 - log_fx_2010)
fx_2010 = fx[fx.year==2010].set_index("country").log_fx.rename("logfx_2010")
fx_2023 = fx[fx.year==2023].set_index("country").log_fx.rename("logfx_2023")
fut = pd.concat([fx_2010, fx_2023], axis=1).dropna()
fut["future_change"] = fut.logfx_2023 - fut.logfx_2010
future_d = (shares.merge(fut[["future_change"]].reset_index(), on="country")
                  .assign(x=lambda d: d.share*d.future_change)
                  .groupby("dname").x.sum().rename("future_z").reset_index())
val = z_d.merge(future_d, on="dname")
cor_val = val[["z_d","future_z"]].corr().iloc[0,1]
print(f"\nValidation: corr(z_d, future_z 2010->2023) = {cor_val:.4f}")
print("  Higher (less negative) z_d -> Higher future_z (less NPR depreciation expected)?")

# === C. First-stage and second-stage regressions ===========================

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
base["z_d_z"]      = (base.z_d - base.z_d.mean()) / base.z_d.std()
base["z_x_logmi"]  = base.z_d_z * base.log_mi_z

# C1. First-stage: cumulative log permits 2011-2023 -> z_d
permits_panel = (dofe.groupby(["dname","year"]).total_migrants.sum().reset_index()
                  .rename(columns={"total_migrants":"permits"}))
log_perm_2011 = (permits_panel[permits_panel.year==2011].set_index("dname").permits
                  .apply(lambda x: np.log(x+1)).rename("log_p_2011"))
log_perm_2023 = (permits_panel[permits_panel.year==2023].set_index("dname").permits
                  .apply(lambda x: np.log(x+1)).rename("log_p_2023"))
log_perm_total = (permits_panel[permits_panel.year.between(2011,2023)]
                  .groupby("dname").permits.sum()
                  .apply(lambda x: np.log(x+1)).rename("log_p_total_11_23"))

ms = base.merge(log_perm_2011.reset_index(), on="dname", how="left") \
         .merge(log_perm_2023.reset_index(), on="dname", how="left") \
         .merge(log_perm_total.reset_index(), on="dname", how="left")
ms["dlog_p_11_23"] = ms.log_p_2023 - ms.log_p_2011

def runcs(y, df, label):
    d = df.dropna(subset=[y, "z_d_z"]).copy()
    X = sm.add_constant(d[["z_d_z","z_x_logmi"]])
    m = sm.OLS(d[y], X).fit(cov_type="HC1")
    def stars(p): return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""
    return {
      "outcome": label,
      "beta_z":   round(float(m.params["z_d_z"]),4),
      "se_z":     round(float(m.bse["z_d_z"]),4),
      "t_z":      round(float(m.tvalues["z_d_z"]),2),
      "p_z":      round(float(m.pvalues["z_d_z"]),4),
      "sig_z":    stars(float(m.pvalues["z_d_z"])),
      "beta_int": round(float(m.params["z_x_logmi"]),4),
      "se_int":   round(float(m.bse["z_x_logmi"]),4),
      "t_int":    round(float(m.tvalues["z_x_logmi"]),2),
      "p_int":    round(float(m.pvalues["z_x_logmi"]),4),
      "sig_int":  stars(float(m.pvalues["z_x_logmi"])),
      "mean_y":   round(float(d[y].mean()),4),
      "n":        int(m.nobs),
    }

print("\n=== FIRST-STAGE: DOFE permits responses to z_d (cross-section, HC1) ===\n")
fs_rows = [
  runcs("log_p_2011",        ms, "log permits 2011 (level)"),
  runcs("log_p_2023",        ms, "log permits 2023 (level)"),
  runcs("dlog_p_11_23",      ms, "Delta log permits 2011->2023"),
  runcs("log_p_total_11_23", ms, "log total permits 2011-2023"),
]
hdr = f"{'outcome':<35} {'b_z':>9} {'t_z':>6} {'sig':>4} {'b_int':>9} {'t_int':>6} {'sig':>4} {'mean_y':>9} {'n':>4}"
print(hdr); print("-"*len(hdr))
for r in fs_rows:
    print(f"{r['outcome']:<35} {r['beta_z']:>9.4f} {r['t_z']:>6.2f} {r['sig_z']:>4} "
          f"{r['beta_int']:>9.4f} {r['t_int']:>6.2f} {r['sig_int']:>4} "
          f"{r['mean_y']:>9.4f} {r['n']:>4}")

# C2. Second-stage: census outcomes 2011->2021 change
groups = {
    "amenities": ["amen_water_piped","amen_water_traditional","amen_cooking_modern",
                  "amen_lighting_electricity","amen_toilet_modern","amen_toilet_any"],
    "assets":    ["amen_assets_motorcycle","amen_assets_fridge","amen_assets_mobile",
                  "amen_assets_computer","amen_assets_internet","amen_asset_count_mean"],
    "industry":  ["ind_agri_forestry_fish","ind_manufacturing","ind_construction",
                  "ind_education","ind_health"],
    "occupation":["occ_share_managers","occ_share_professionals","occ_share_craft_trades",
                  "occ_share_elementary","occ_share_agriculture"],
    "migration": ["mig_in_share","mig_in_international","absent_hh_share"],
}
avail = set(outcomes.columns)
for grp, lst in list(groups.items()):
    groups[grp] = [v for v in lst if v in avail]

ow = (outcomes[outcomes.year.isin([2011,2021])]
       .pivot(index="dname", columns="year").reset_index())
ow.columns = ["_".join(map(str,c)).rstrip("_") for c in ow.columns]
if "dname_" in ow.columns and "dname" not in ow.columns:
    ow = ow.rename(columns={"dname_":"dname"})

ms2 = base.merge(ow, on="dname", how="left")

print("\n=== SECOND-STAGE: census Delta_y 2011->2021 on z_d (cross-section, HC1) ===\n")
ss_rows = []
hdr2 = f"{'group':<11} {'outcome':<33} {'b_z':>8} {'t_z':>6} {'sig':>4} {'b_int':>8} {'t_int':>6} {'sig':>4} {'mean_dy':>9} {'n':>4}"
print(hdr2); print("-"*len(hdr2))
for grp, lst in groups.items():
    for o in lst:
        c11, c21 = f"{o}_2011", f"{o}_2021"
        if c11 not in ms2.columns or c21 not in ms2.columns: continue
        ms2["_dy"] = ms2[c21] - ms2[c11]
        r = runcs("_dy", ms2, o)
        r["group"] = grp
        ss_rows.append(r)
        print(f"{grp:<11} {o:<33} {r['beta_z']:>8.4f} {r['t_z']:>6.2f} {r['sig_z']:>4} "
              f"{r['beta_int']:>8.4f} {r['t_int']:>6.2f} {r['sig_int']:>4} "
              f"{r['mean_y']:>9.4f} {r['n']:>4}")
        ms2 = ms2.drop(columns=["_dy"])

# Save
import os
os.makedirs("district-analysis/output/tab", exist_ok=True)
pd.DataFrame(fs_rows).to_csv("district-analysis/output/tab/khanna_gfc_first_stage.csv", index=False)
pd.DataFrame(ss_rows).to_csv("district-analysis/output/tab/khanna_gfc_second_stage.csv", index=False)
gc[gc.country.isin(top12)].sort_values("g_c").to_csv("district-analysis/output/tab/khanna_gfc_g_c.csv", index=False)
z_d.to_csv("district-analysis/output/tab/khanna_gfc_z_d.csv", index=False)

print(f"\nSaved:")
print("  output/tab/khanna_gfc_first_stage.csv  (4 rows)")
print(f"  output/tab/khanna_gfc_second_stage.csv ({len(ss_rows)} rows)")
print("  output/tab/khanna_gfc_g_c.csv          (per-country shifter)")
print("  output/tab/khanna_gfc_z_d.csv          (per-district shifter)")
