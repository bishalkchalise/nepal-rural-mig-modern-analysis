"""
Census second stage using DOFE-vintage SSIV
============================================
Treatment : fx_z * mi_z   where
              fx_z = zscore( fxshock_dofe )                           (NPR/LCU)
              mi_z = zscore( DOFE_2009_10_mean / pop_2011 )           (vintage-matched)

Specification (matches slide / portal full spec):
    y_dt = alpha * fx_z
         + beta  * (fx_z * mi_z)
         + lambda_mig * (mi_z * tau_t)
         + lambda_fx  * (fx_z * tau_t)
         + delta'     * (X * tau_t)
         + dname FE + year FE + epsilon

Cluster ~dname.  X = six 2001 destination-region shares.  tau_t relative to
year 2001 (so non-ref years 2011, 2021).

Runs the full-spec regression across census outcome groups; reports beta on
fx_z * mi_z plus stars.
"""

import numpy as np, pandas as pd
from linearmodels.panel import PanelOLS

# --------------------------------------------------------------------------
# 1. Load
# --------------------------------------------------------------------------

outcomes_df = pd.read_csv("district-analysis/data/clean/census/outcomes_district.csv")
inst        = pd.read_csv("district-analysis/data/clean/instrument/instrument_forex_dist.csv")
inst_dofe   = pd.read_csv("district-analysis/data/clean/instrument/instrument_dofe_dist.csv")
region_sh   = pd.read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv")
pop_file    = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")
dofe_raw    = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")

# DOFE 2009-10 mean migrants per district
dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

dofe_panel = (dofe_raw.groupby(["district_rename","year"]).total_migrants.sum()
                .reset_index()
                .assign(dname=lambda d: d.district_rename.map(to_dname))
                .rename(columns={"total_migrants":"permits"}))

mi_dofe = (dofe_panel[dofe_panel.year.isin([2009,2010])]
           .groupby("dname").permits.mean().rename("dofe_mig_0910").reset_index())

# pop_2011
pop_2011 = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
                    [["dname","district_population_2011"]]
                    .drop_duplicates("dname")
                    .rename(columns={"district_population_2011":"pop_2011"}))

mi_dofe = (mi_dofe.merge(pop_2011, on="dname")
                  .assign(mig_int_dofe=lambda d: d.dofe_mig_0910 / d.pop_2011)
                  [["dname","mig_int_dofe"]])

# --------------------------------------------------------------------------
# 2. Build panel and z-scored treatments
# --------------------------------------------------------------------------

panel = (outcomes_df.merge(inst_dofe[["dname","year","fxshock_dofe"]],
                            on=["dname","year"])
                     .merge(mi_dofe,    on="dname", how="left")
                     .merge(region_sh,  on="dname", how="left"))

panel = panel.dropna(subset=["fxshock_dofe","mig_int_dofe"]).copy()

def z(s):
    sd = s.std()
    return (s - s.mean())/sd if sd > 0 else pd.Series(0., index=s.index)

panel["fx_z"]      = z(panel["fxshock_dofe"])
panel["mi_z"]      = z(panel["mig_int_dofe"])
panel["log_mi_z"]  = z(np.log(panel["mig_int_dofe"] + 1e-12))

# Year-dummy interactions (ref = 2001)
def yr_inter(df, col, prefix, ref=2001):
    out = pd.DataFrame(index=df.index)
    for y in sorted(df.year.unique()):
        if y == ref: continue
        out[f"{prefix}_y{y}"] = df[col] * (df.year == y).astype(float)
    return out

# --------------------------------------------------------------------------
# 3. Outcome groups (drop missing ones gracefully)
# --------------------------------------------------------------------------

GROUPS = {
    "amenities": ["amen_water_piped","amen_water_traditional","amen_cooking_modern",
                  "amen_cooking_traditional","amen_lighting_electricity",
                  "amen_toilet_modern","amen_toilet_any"],
    "housing":   ["housing_own","housing_rented","housing_foundation_modern",
                  "housing_foundation_traditional","housing_roof_modern","housing_roof_traditional"],
    "work":      ["flfp_all","fem_employment_rate","flfp_agri","flfp_wage",
                  "mlfp_all","mlfp_agri","mlfp_nonagri"],
    "migration": ["mig_in_share","mig_in_domestic","mig_in_international",
                  "mig_in_from_rural","mig_in_from_urban",
                  "mig_in_reason_economic","mig_in_reason_noneconomic",
                  "mig_in_return","absent_hh_share"],
    "occupation":["occ_agri","occ_services","occ_trade","occ_industry","occ_other"],
    "household": ["head_female_share","head_age_mean","head_elderly_share","head_young_share"],
    "left_behind":["left_not_with_both","left_mother_only","left_father_only",
                   "left_with_relatives","left_without_parents"],
}

REGION_X = ["share_e_asia","share_gulf","share_oecd_north",
            "share_oecd_europe","share_s_asia","share_se_asia"]

# --------------------------------------------------------------------------
# 4. Fit one outcome under the full spec
# --------------------------------------------------------------------------

def fit_full(outcome):
    d = panel.dropna(subset=[outcome]).copy()
    if len(d) < 30 or d[outcome].nunique() < 2: return None
    d["treatment"] = d.fx_z * d.mi_z

    base = d[["treatment","fx_z"]].reset_index(drop=True)
    extras = [base,
              yr_inter(d, "mi_z", "cmig").reset_index(drop=True),
              yr_inter(d, "fx_z", "cfx").reset_index(drop=True)]
    for c in REGION_X:
        if c in d.columns:
            extras.append(yr_inter(d, c, f"cX_{c}").reset_index(drop=True))

    exog = pd.concat(extras, axis=1)
    y    = d[outcome].reset_index(drop=True)
    idx  = pd.MultiIndex.from_arrays([d["dname"].values, d["year"].values],
                                     names=["dname","year"])
    exog.index = idx; y.index = idx

    try:
        m = PanelOLS(y, exog, entity_effects=True, time_effects=True,
                     drop_absorbed=True)
        r = m.fit(cov_type="clustered", cluster_entity=True)
        if "treatment" not in r.params.index:
            return {"err":"absorbed"}
        return {
            "beta":  float(r.params["treatment"]),
            "se":    float(r.std_errors["treatment"]),
            "t":     float(r.tstats["treatment"]),
            "p":     float(r.pvalues["treatment"]),
            "n":     int(r.nobs),
            "mean":  float(d[outcome].mean()),
            "r2_w":  float(r.rsquared_within),
        }
    except Exception as e:
        return {"err":str(e)[:80]}

def stars(p): return "***" if p<0.01 else "**" if p<0.05 else "*" if p<0.1 else ""

rows = []
for grp, outs in GROUPS.items():
    for o in outs:
        if o not in panel.columns: continue
        r = fit_full(o)
        if r is None: continue
        if "err" in r:
            rows.append({"group":grp,"outcome":o,"beta":np.nan,"se":np.nan,
                         "t":np.nan,"p":np.nan,"sig":"","n":np.nan,
                         "mean_y":np.nan,"r2_w":np.nan,"note":r["err"]})
        else:
            rows.append({"group":grp,"outcome":o,
                         "beta":round(r["beta"],4),"se":round(r["se"],4),
                         "t":round(r["t"],2),"p":round(r["p"],4),
                         "sig":stars(r["p"]),
                         "n":r["n"],"mean_y":round(r["mean"],4),
                         "r2_w":round(r["r2_w"],4),"note":""})

res = pd.DataFrame(rows)
res.to_csv("district-analysis/output/tab/second_stage_census_dofe.csv", index=False)

# Console output
for grp, outs in GROUPS.items():
    sub = res[res.group == grp]
    if sub.empty: continue
    print(f"\n=== {grp} ===")
    print(sub[["outcome","beta","se","t","p","sig","mean_y","n"]].to_string(index=False))

print(f"\nSaved: output/tab/second_stage_census_dofe.csv ({len(res)} rows)")
print(f"Significant at p<0.05: {(res.p < 0.05).sum()} / {len(res)}")
