"""
BHJ first-differenced census second-stage, simplified.

  ONE FD per district:  Delta_y_d = y_{d,2021} - y_{d,2011}
  Lag-2 shock:          Delta_z_d = z_{d,2019} - z_{d,2009}
                                  = sum_c share_dc * ( log_fx_{c,2019} - log_fx_{c,2009} )

Cross-sectional regression (one obs per district, no panel structure):

  Delta_y_d = beta * (Delta_z_d * log_mi_z_d) + gamma * Delta_z_d + eps_d

  SE: HC1 robust.

FX direction: LCU per NPR (Khanna).
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

# FX panel in Khanna direction (LCU per NPR)
nepal_fx = forex[forex.country=="Nepal"][["year","forex"]].rename(columns={"forex":"npr_per_usd"})
fx = (forex.rename(columns={"forex":"lcu_per_usd"})[["country","year","lcu_per_usd"]]
            .merge(nepal_fx, on="year")
            .query("country not in ['Nepal','India']")
            .assign(fx_lcu_per_npr=lambda d: d.lcu_per_usd/d.npr_per_usd))
# log of raw fx_lcu_per_npr (no baseline needed since we'll difference)
fx["log_fx"] = np.log(fx.fx_lcu_per_npr)
fx = fx[["country","year","log_fx"]].dropna()

# DOFE 2009-10 shares
dofe = (dofe_raw.groupby(["district_rename","country","year"]).total_migrants.sum().reset_index()
            .assign(dname=lambda d: d.district_rename.map(to_dname)))
dofe = dofe[~dofe.country.isin(["Nepal","India"])]
shares = (dofe[dofe.year.isin([2009,2010])]
          .groupby(["dname","country"]).total_migrants.sum().reset_index()
          .assign(tot=lambda d: d.groupby("dname").total_migrants.transform("sum"))
          .assign(share=lambda d: d.total_migrants/d.tot)
          [["dname","country","share"]])

# Shock at year t = sum_c share_dc * log_fx_{c,t}
fx_window = fx[fx.year.isin([2009, 2019])].copy()
zwin = (shares.merge(fx_window, on="country", how="inner")
              .assign(x=lambda d: d.share * d.log_fx)
              .groupby(["dname","year"]).x.sum().rename("z").reset_index())
zwide = zwin.pivot(index="dname", columns="year", values="z").reset_index()
zwide["dz_lag2"] = zwide[2019] - zwide[2009]    # 2009 -> 2019, ending 2 years before y_2021
zwide = zwide[["dname","dz_lag2"]]

# Mig intensity
dy_dy = (dofe.groupby(["dname","year"]).total_migrants.sum().reset_index()
            .rename(columns={"total_migrants":"permits"}))
mi_num = (dy_dy[dy_dy.year.isin([2009,2010])].groupby("dname").permits.mean().rename("num").reset_index())
pop = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
              [["dname","district_population_2011"]].drop_duplicates("dname")
              .rename(columns={"district_population_2011":"pop_2011"}))
mi = mi_num.merge(pop, on="dname").assign(mig_int_dofe=lambda d: d.num/d.pop_2011)[["dname","mig_int_dofe"]]

# Outcome wide form
GROUPS = {
    "amenities": ["amen_water_piped","amen_water_traditional","amen_cooking_modern",
                  "amen_cooking_traditional","amen_lighting_electricity",
                  "amen_toilet_modern","amen_toilet_any"],
    "assets":    ["amen_assets_radio","amen_assets_tv","amen_assets_cycle",
                  "amen_assets_motorcycle","amen_assets_car","amen_assets_fridge",
                  "amen_assets_landline","amen_assets_mobile","amen_assets_computer",
                  "amen_assets_internet","amen_asset_count_mean"],
    "industry":  ["ind_agri_forestry_fish","ind_manufacturing","ind_construction",
                  "ind_wholesale_retail","ind_transport_accommodation",
                  "ind_finance_real_estate_prof","ind_public_admin_defence",
                  "ind_education","ind_health","ind_arts_recreation","ind_others"],
    "occupation":["occ_share_managers","occ_share_professionals","occ_share_technicians",
                  "occ_share_office_assistants","occ_share_service_sales",
                  "occ_share_agriculture","occ_share_craft_trades",
                  "occ_share_machine_operators","occ_share_elementary","occ_share_armed_forces"],
}
avail = set(outcomes.columns)
for g, lst in list(GROUPS.items()):
    GROUPS[g] = [v for v in lst if v in avail]
    if not GROUPS[g]:
        print(f"  WARNING: no outcomes in {avail} match group {g}")

# Build base: one row per district, with dy = y_2021 - y_2011 per outcome
base = (outcomes[outcomes.year.isin([2011,2021])]
          .pivot(index="dname", columns="year")
          .reset_index())
# Flatten multiindex columns: (varname, year) -> varname_year
base.columns = ["_".join(map(str,c)).rstrip("_") for c in base.columns]
# dname column will become "dname_" or "dname"; normalise:
if "dname_" in base.columns and "dname" not in base.columns:
    base = base.rename(columns={"dname_":"dname"})

# Merge in dz_lag2 and mi
base = base.merge(zwide, on="dname", how="inner").merge(mi, on="dname", how="inner")
# z-score helpers
base["log_mi_z"] = (np.log(base.mig_int_dofe.clip(lower=1e-12))
                      - np.log(base.mig_int_dofe.clip(lower=1e-12)).mean()
                   ) / np.log(base.mig_int_dofe.clip(lower=1e-12)).std()
base["dz_z"] = (base.dz_lag2 - base.dz_lag2.mean()) / base.dz_lag2.std()
base["dz_x_logmi"] = base.dz_z * base.log_mi_z

print(f"Base panel: {len(base)} districts (one FD obs per district).")
print(f"dz_lag2 (2009->2019) range: [{base.dz_lag2.min():.3f}, {base.dz_lag2.max():.3f}],  "
      f"mean = {base.dz_lag2.mean():.3f}\n")

def fit(out_var):
    yc = f"{out_var}_2011"; yc21 = f"{out_var}_2021"
    if yc not in base.columns or yc21 not in base.columns: return None
    d = base[["dname", yc, yc21, "dz_z","dz_x_logmi","log_mi_z"]].copy()
    d["dy"] = d[yc21] - d[yc]
    d = d.dropna(subset=["dy","dz_z"])
    if len(d) < 30: return None
    X = sm.add_constant(d[["dz_z","dz_x_logmi"]])
    m = sm.OLS(d["dy"], X).fit(cov_type="HC1")
    return {
        "outcome":out_var,
        "beta_dz":   round(float(m.params["dz_z"]), 4),
        "se_dz":     round(float(m.bse["dz_z"]), 4),
        "t_dz":      round(float(m.tvalues["dz_z"]), 2),
        "p_dz":      round(float(m.pvalues["dz_z"]), 4),
        "beta_inter":round(float(m.params["dz_x_logmi"]), 4),
        "se_inter":  round(float(m.bse["dz_x_logmi"]), 4),
        "t_inter":   round(float(m.tvalues["dz_x_logmi"]), 2),
        "p_inter":   round(float(m.pvalues["dz_x_logmi"]), 4),
        "mean_dy":   round(float(d.dy.mean()), 4),
        "n":         int(m.nobs),
    }

def stars(p):
    if p is None or np.isnan(p): return ""
    return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

rows = []
print(f"{'group':<11} {'outcome':<33} {'b_dz':>8} {'t_dz':>6} {'sig':>4} "
      f"{'b_inter':>9} {'t_inter':>8} {'sig':>4} {'mean_dy':>9} {'n':>4}")
print("-"*102)
for g, lst in GROUPS.items():
    for o in lst:
        r = fit(o)
        if r is None: continue
        r["group"] = g; rows.append(r)
        print(f"{g:<11} {o:<33} {r['beta_dz']:>8.4f} {r['t_dz']:>6.2f} {stars(r['p_dz']):>4} "
              f"{r['beta_inter']:>9.4f} {r['t_inter']:>8.2f} {stars(r['p_inter']):>4} "
              f"{r['mean_dy']:>9.4f} {r['n']:>4}")

out_df = pd.DataFrame(rows)
cols = ["group","outcome","beta_dz","se_dz","t_dz","p_dz",
        "beta_inter","se_inter","t_inter","p_inter","mean_dy","n"]
out_df = out_df[cols]
out_df.to_csv("district-analysis/output/tab/bhj_fd_census_second_stage_lag2.csv", index=False)
print(f"\nSaved: output/tab/bhj_fd_census_second_stage_lag2.csv ({len(out_df)} outcomes)")
print(f"Significant at p<0.05 (interaction): {(out_df.p_inter < 0.05).sum()} / {len(out_df)}")
print(f"Significant at p<0.05 (bare dz):     {(out_df.p_dz    < 0.05).sum()} / {len(out_df)}")
