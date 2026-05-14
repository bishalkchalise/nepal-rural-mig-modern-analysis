"""
Three diagnostics on whether C_mig (year x log_mi_z) is justified.

  D1.  Variance decomposition: how much of the treatment fx_z*log_mi_z is
       absorbed by the C_mig + C_fx + dname/year FE controls?  If almost
       all of it, M4 has essentially no residual variation -> beta is
       fragile / weak-instrument territory.

  D2.  Trend plot by intensity quintile: are high-mi vs low-mi districts
       trending in parallel?  If yes, C_mig is over-controlling.  If they
       diverge, C_mig is essential to isolate shock-response.

  D3.  Placebo test using future shock to predict past outcome.  Regress
       2009 log(permits) on 2023 fxshock * log_mi_z (controls + FE).  If
       beta is significantly non-zero, the SSIV is picking up a pre-
       existing pattern not a causal shock-response.

Outputs:
  output/fig/diag_cmig_quintile_trends.png
  output/fig/diag_cmig_variance_partition.png
  output/tab/diag_cmig_results.csv
"""

import os, numpy as np, pandas as pd, matplotlib.pyplot as plt
from linearmodels.panel import PanelOLS

os.makedirs("district-analysis/output/fig", exist_ok=True)
os.makedirs("district-analysis/output/tab", exist_ok=True)

# ---------- Load + build panel (same as before) ---------------------------

inst       = pd.read_csv("district-analysis/data/clean/instrument/instrument_forex_dist.csv")
inst_dofe  = pd.read_csv("district-analysis/data/clean/instrument/instrument_dofe_dist.csv")
region_sh  = pd.read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv")
dofe_raw   = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file   = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

dofe = (dofe_raw.groupby(["district_rename","year"]).total_migrants.sum().reset_index()
                .assign(dname=lambda d: d.district_rename.map(to_dname))
                .rename(columns={"total_migrants":"permits"})[["dname","year","permits"]])

mi_dofe = (dofe[dofe.year.isin([2009,2010])]
           .groupby("dname").permits.mean().rename("num").reset_index())
pop_2011 = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
                    [["dname","district_population_2011"]]
                    .drop_duplicates("dname")
                    .rename(columns={"district_population_2011":"pop_2011"}))
mi_dofe = (mi_dofe.merge(pop_2011, on="dname")
                  .assign(mig_int_dofe=lambda d: d.num/d.pop_2011)
                  [["dname","mig_int_dofe"]])

panel = (dofe.merge(inst[["dname","year","fxshock"]], on=["dname","year"])
              .merge(inst_dofe[["dname","year","fxshock_dofe"]], on=["dname","year"])
              .merge(mi_dofe, on="dname", how="left")
              .merge(region_sh, on="dname", how="left")
              .dropna(subset=["fxshock_dofe","mig_int_dofe"]))
panel["log_permits"] = np.log(panel.permits + 1)

def z(s):
    sd = s.std(); return (s - s.mean())/sd if sd>0 else pd.Series(0., index=s.index)

panel["fx_z"]      = z(panel.fxshock_dofe)
panel["log_mi_z"]  = z(np.log(panel.mig_int_dofe.clip(lower=1e-12)))
panel["treatment"] = panel.fx_z * panel.log_mi_z

# ============================================================================
# D1.  Variance partition: how much of `treatment` does C_mig + FE explain?
# ============================================================================

def yr_inter(df, col, prefix, ref=2016):
    out = pd.DataFrame(index=df.index)
    for y in sorted(df.year.unique()):
        if y == ref: continue
        out[f"{prefix}_y{y}"] = df[col] * (df.year == y).astype(float)
    return out

# Demean treatment by dname + year (the FE component)
def demean_two_way(df, var, e1="dname", e2="year"):
    s = df[var]
    s = s - df.groupby(e1)[var].transform("mean")
    s = s - df.groupby(e2)[s.name].transform("mean") if False else s
    # Two-way demeaning is iterative; use a simple convergent approach
    x = df[var].values.copy()
    for _ in range(20):
        x = x - df.assign(_x=x).groupby(e1)._x.transform("mean").values
        x = x - df.assign(_x=x).groupby(e2)._x.transform("mean").values
    return pd.Series(x, index=df.index)

df = panel.copy().reset_index(drop=True)
df["t_demeaned"]  = demean_two_way(df, "treatment")    # treatment after dname+year FE
df["fx_demeaned"] = demean_two_way(df, "fx_z")
df["mi_demeaned"] = demean_two_way(df, "log_mi_z")

# Now regress t_demeaned on (year × log_mi_z) + (year × fx_z) + region X year
extras = []
extras.append(yr_inter(df, "log_mi_z", "cmig"))
extras.append(yr_inter(df, "fx_z",     "cfx"))
controls_cmig = pd.concat(extras, axis=1).reset_index(drop=True)

import statsmodels.api as sm
y = df.t_demeaned.values
X = sm.add_constant(controls_cmig.values)
mod = sm.OLS(y, X, missing="drop").fit()
r2_cmig_cfx = mod.rsquared

# Just C_mig
controls_cmig_only = yr_inter(df, "log_mi_z", "cmig").reset_index(drop=True)
mod2 = sm.OLS(y, sm.add_constant(controls_cmig_only.values), missing="drop").fit()
r2_cmig_only = mod2.rsquared

# Just C_fx
controls_cfx_only = yr_inter(df, "fx_z", "cfx").reset_index(drop=True)
mod3 = sm.OLS(y, sm.add_constant(controls_cfx_only.values), missing="drop").fit()
r2_cfx_only = mod3.rsquared

print("=" * 75)
print("D1.  How much of (fx_z * log_mi_z) -- after dname + year FE demeaning --")
print("     is explained by the C_mig and C_fx year-trend controls?")
print("=" * 75)
print(f"  R^2 of t_demeaned on i(year, log_mi_z) only      : {r2_cmig_only:.4f}")
print(f"  R^2 of t_demeaned on i(year, fx_z) only          : {r2_cfx_only:.4f}")
print(f"  R^2 of t_demeaned on both C_mig + C_fx           : {r2_cmig_cfx:.4f}")
print(f"  Residual share of identifying variation for M4-M5: {1 - r2_cmig_cfx:.4f}")

# ============================================================================
# D2.  Trend plot by mig_int_dofe quintile
# ============================================================================

district_mi = panel.drop_duplicates("dname")[["dname","mig_int_dofe"]].copy()
district_mi["mi_quintile"] = pd.qcut(district_mi.mig_int_dofe, q=5,
                                     labels=["Q1 (low)","Q2","Q3","Q4","Q5 (high)"])

panel_q = panel.merge(district_mi[["dname","mi_quintile"]], on="dname")

trends = (panel_q.groupby(["year","mi_quintile"]).log_permits.mean()
          .reset_index())

fig, ax = plt.subplots(figsize=(10, 6))
colors = plt.cm.viridis(np.linspace(0, 0.9, 5))
for (q, color) in zip(trends.mi_quintile.unique(), colors):
    sub = trends[trends.mi_quintile == q]
    ax.plot(sub.year, sub.log_permits, marker="o", color=color,
            label=str(q), linewidth=2, markersize=4)
ax.set_xlabel("Year"); ax.set_ylabel("Mean log(permits + 1) across districts in quintile")
ax.set_title("Permit trends by 2009-10 DOFE migration intensity quintile\n"
             "(parallel = C_mig over-controls; diverging = C_mig essential)",
             fontweight="bold", fontsize=11)
ax.legend(title="mig_int_dofe quintile", loc="lower right", fontsize=9)
ax.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig("district-analysis/output/fig/diag_cmig_quintile_trends.png",
            dpi=140, bbox_inches="tight")
plt.close()

# Also: the WITHIN-quintile trend slopes
slopes = []
for q in trends.mi_quintile.unique():
    sub = trends[trends.mi_quintile == q].sort_values("year")
    slope, intercept = np.polyfit(sub.year.astype(float), sub.log_permits, 1)
    slopes.append({"quintile": str(q), "trend_per_year": round(slope, 4),
                   "level_2023": round(sub.log_permits.iloc[-1], 3),
                   "level_2009": round(sub.log_permits.iloc[0], 3)})
slopes_df = pd.DataFrame(slopes)
print("\n" + "=" * 75)
print("D2.  Within-quintile trend slope (log permits per year)")
print("=" * 75)
print(slopes_df.to_string(index=False))

# ============================================================================
# D3.  Placebo test:  predict past log_permits with FUTURE fxshock
# ============================================================================
# Take 2009 (earliest) log_permits as outcome.  Regress on 2023 (latest)
# fxshock_dofe x log_mi_z plus controls, by district.  No panel FE needed
# since we collapse to one obs per district.

def placebo(panel, outcome_year, shock_year):
    base = panel[panel.year == outcome_year][["dname","log_permits","mig_int_dofe"]].copy()
    base = base.rename(columns={"log_permits":"y"})
    sho  = panel[panel.year == shock_year][["dname","fxshock_dofe"]].copy()
    sho  = sho.rename(columns={"fxshock_dofe":"fx_future"})
    d = base.merge(sho, on="dname").dropna()
    d["fx_z"]      = z(d.fx_future)
    d["log_mi_z"]  = z(np.log(d.mig_int_dofe.clip(lower=1e-12)))
    d["treatment"] = d.fx_z * d.log_mi_z

    # Cross-sectional OLS on d (74 districts)
    X = sm.add_constant(d[["treatment","fx_z","log_mi_z"]].values)
    y = d["y"].values
    mod = sm.OLS(y, X).fit(cov_type="HC1")
    return {"shock_year": shock_year, "outcome_year": outcome_year,
            "n": len(d),
            "beta_treatment":   round(float(mod.params[1]), 4),
            "se_treatment":     round(float(mod.bse[1]), 4),
            "t_treatment":      round(float(mod.tvalues[1]), 2),
            "p_treatment":      round(float(mod.pvalues[1]), 4),
            "beta_fx":          round(float(mod.params[2]), 4),
            "beta_logmi":       round(float(mod.params[3]), 4),
            "r2":               round(float(mod.rsquared), 4)}

placebo_rows = []
for sh in [2023, 2020, 2015, 2012]:
    placebo_rows.append(placebo(panel, outcome_year=2009, shock_year=sh))
# also do real check: contemporary (no placebo)
placebo_rows.append({**placebo(panel, outcome_year=2023, shock_year=2023),
                     "label":"NOT placebo: same-year"})
plac_df = pd.DataFrame(placebo_rows)

print("\n" + "=" * 75)
print("D3.  Placebo: regress 2009 log_permits on FUTURE fxshock x log_mi_z")
print("     (one obs per district, no panel FE, HC1)")
print("=" * 75)
print(plac_df.to_string(index=False))
print("\n     If beta_treatment is significant in the placebo rows (using")
print("     future shock to predict past outcome), the SSIV variation is")
print("     correlated with pre-existing district patterns -- supporting")
print("     C_mig as essential, not over-controlling.")

# Save all
all_results = {
    "D1_variance_partition": pd.DataFrame([
        {"R2_target":"t_demeaned ~ C_mig only",                "value":round(r2_cmig_only, 4)},
        {"R2_target":"t_demeaned ~ C_fx only",                 "value":round(r2_cfx_only, 4)},
        {"R2_target":"t_demeaned ~ C_mig + C_fx (both)",       "value":round(r2_cmig_cfx, 4)},
        {"R2_target":"residual share for M4/M5 identification","value":round(1 - r2_cmig_cfx, 4)},
    ]),
    "D2_quintile_slopes":   slopes_df,
    "D3_placebo":           plac_df,
}

with open("district-analysis/output/tab/diag_cmig_results.csv","w") as f:
    for nm, df_ in all_results.items():
        f.write(f"# {nm}\n")
        df_.to_csv(f, index=False)
        f.write("\n")
print(f"\nSaved combined: output/tab/diag_cmig_results.csv")
print(f"      figure  : output/fig/diag_cmig_quintile_trends.png")
