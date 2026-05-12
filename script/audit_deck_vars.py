"""
script/audit_deck_vars.py
Full audit of every variable currently displayed in the presentation:

For each variable, dumps:
  - Source dataset and variable name
  - Raw stats from the *base data file* (no merges, no thresholds): N, mean,
    sd, min, p25, p50, p75, max, %zero, %na
  - Stats from the *regression sample at k=0*: muni-year (census) or HH-year
    (HRVS) panel after merging with the instrument
  - Stats from the *regression sample at k=25* (anchor): same panel, threshold
    on baseline migrant count

Outputs: output/tab/audit_deck_variables.csv  + console summary of any
slide-mean / raw-mean / regression-mean discrepancies > 5%.
"""
import pandas as pd, numpy as np, os
from pathlib import Path

ROOT = Path(".")

# ----- variable registry -----
# (source_label, csv_relpath, variable_name, slide_label, slide_mean)
VARS = [
    # First-stage
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "mig_in_share",                    "Migrant-HH share (1st stage proxy)", None),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "absent_hh_share",                  "Migrant-HH share (slide 4)",         0.24),
    ("hh_mig",       "data/clean/rvs_outcomes/migration_hh_year_migrant_only.csv","remit_amount_intl_12m_rs", "log(intl remit, Rs) [base in Rs]",   None),
    ("hh_mig",       "data/clean/rvs_outcomes/migration_hh_year_migrant_only.csv","n_migrants_international", "log(# intl migrants) [base count]",  None),
    # HH investment
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "amen_asset_count_mean",            "Durables per household",             2.89),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "housing_foundation_modern",        "Modern housing",                     0.28),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "amen_toilet_any",                  "Toilet adoption",                    0.66),
    # Human capital
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "edu_literate",                     "Literate (pop 6+)",                  0.64),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "edu_literate_female",              "Literate, female",                   0.55),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "edu_school_attend_6_16",           "School attendance 6-16",             0.84),
    ("hh_health",    "data/clean/rvs_outcomes/health_hh_year.csv",         "hlt_spend_total",                  "Health spending total (Rs)",         12401),
    # Sectors
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "ind_agri_forestry_fish",           "Agriculture (primary)",              0.72),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "ind_manufacturing",                "Manufacturing",                      0.047),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "ind_construction",                 "Construction",                       0.040),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "ind_wholesale_retail",             "Trade & retail",                     0.064),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "ind_finance_real_estate_prof",     "Finance / RE / prof",                0.010),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "ind_public_admin_defence",         "Public admin & defence",             0.015),
    # Occupations
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "occ_share_managers",               "Managers",                           0.021),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "occ_share_professionals",          "Professionals",                      0.034),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "occ_share_technicians",            "Technicians",                        0.014),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "occ_share_service_sales",          "Service & sales",                    0.057),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "occ_share_craft_trades",           "Craft & trades",                     0.064),
    ("census",       "data/clean/census/census_outcomes_municipality.csv", "occ_share_machine_operators",      "Machine operators",                  0.018),
    # Land use
    ("hh_ag",        "data/clean/rvs_outcomes/agriculture_hh_year.csv",    "share_self_wet",                   "Own-cultivated, wet season",         0.855),
    ("hh_ag",        "data/clean/rvs_outcomes/agriculture_hh_year.csv",    "share_self_dry",                   "Own-cultivated, dry season",         0.697),
    ("hh_ag",        "data/clean/rvs_outcomes/agriculture_hh_year.csv",    "share_both_seasons",               "Cultivated both seasons",            0.691),
    ("hh_ag",        "data/clean/rvs_outcomes/agriculture_hh_year.csv",    "share_fallow_wet",                 "Fallow, wet season",                 0.07),
    ("hh_ag",        "data/clean/rvs_outcomes/agriculture_hh_year.csv",    "share_fallow_dry",                 "Fallow, dry season",                 0.27),
    ("hh_ag",        "data/clean/rvs_outcomes/agriculture_hh_year.csv",    "crop_simpson_diversity",           "Crop diversity (Simpson)",           0.223),
    ("hh_ag",        "data/clean/rvs_outcomes/agriculture_hh_year.csv",    "grows_horticulture",               "Grows any horticulture crop",        0.375),
    # Input use
    ("hh_ag",        "data/clean/rvs_outcomes/agriculture_hh_year.csv",    "owns_plough",                      "Owns plough",                        0.62),
    ("hh_ag",        "data/clean/rvs_outcomes/agriculture_hh_year.csv",    "owns_powered_machinery",           "Owns powered farm machinery",        0.03),
    ("hh_ag",        "data/clean/rvs_outcomes/agriculture_hh_year.csv",    "owns_irrigation_kit",              "Owns irrigation equipment",          0.10),
    ("hh_ag",        "data/clean/rvs_outcomes/agriculture_hh_year.csv",    "total_input_cost_rs",              "Total input cost (Rs, base)",        None),
    ("hh_ag",        "data/clean/rvs_outcomes/agriculture_hh_year.csv",    "dry_cost_seed",                    "Dry-season seed cost (Rs, base)",    None),
    # Consumption
    ("hh_cons",      "data/clean/rvs_outcomes/consumption_hh_year.csv",    "food_exp_total_7day",              "Food spending, weekly (Rs)",         2282),
    ("hh_cons",      "data/clean/rvs_outcomes/consumption_hh_year.csv",    "food_exp_protein_7day",            "Protein spending, weekly (Rs)",      724),
    ("hh_cons",      "data/clean/rvs_outcomes/consumption_hh_year.csv",    "nonfood_exp_12m",                  "Non-food spending, annual (Rs)",     97932),
    ("hh_cons",      "data/clean/rvs_outcomes/consumption_hh_year.csv",    "nonfood_clothing_footwear_12m",    "Clothing & footwear, annual (Rs)",   12151),
    ("hh_cons",      "data/clean/rvs_outcomes/consumption_hh_year.csv",    "nonfood_fuel_lighting_12m",        "Fuel & lighting, annual (Rs)",       4760),
    ("hh_cons",      "data/clean/rvs_outcomes/consumption_hh_year.csv",    "food_insec_score",                 "Food insecurity score (0-27)",       0.77),
    # NEC panel
    ("nec_panel",    "data/clean/nec2018/mun_entry_panel_new.csv",         "new_firms",                        "Total new firms (count, base)",      62),
    ("nec_panel",    "data/clean/nec2018/mun_entry_panel_new.csv",         "new_firms_size_1_worker",          "Size: 1-worker (count, base)",       24),
    ("nec_panel",    "data/clean/nec2018/mun_entry_panel_new.csv",         "new_firms_size_2_9_workers",       "Size: 2-9 workers (count, base)",    36),
    ("nec_panel",    "data/clean/nec2018/mun_entry_panel_new.csv",         "new_firms_size_10_50_workers",     "Size: 10-50 workers (count, base)",  2.0),
    ("nec_panel",    "data/clean/nec2018/mun_entry_panel_new.csv",         "new_firms_manufacturing",          "Manufacturing (count, base)",        7.0),
    ("nec_panel",    "data/clean/nec2018/mun_entry_panel_new.csv",         "new_firms_trade_retail",           "Trade & retail (count, base)",       35),
    ("nec_panel",    "data/clean/nec2018/mun_entry_panel_new.csv",         "new_firms_hospitality_food",       "Hospitality & food (count, base)",   9.3),
    # NEC cs 2018
    ("nec_cs",       "data/clean/nec2018/mun_size_formality.csv",          "n_firms",                          "# firms (count, base)",              None),
    ("nec_cs",       "data/clean/nec2018/mun_size_formality.csv",          "emp_total",                        "Total employment (count, base)",     None),
    ("nec_cs",       "data/clean/nec2018/mun_productivity_profitability.csv","rev_total",                      "Total revenue (Rs, base)",           None),
    ("nec_cs",       "data/clean/nec2018/mun_productivity_profitability.csv","cap_total",                      "Total capital stock (Rs, base)",     None),
    ("nec_cs",       "data/clean/nec2018/mun_industry_structure.csv",      "industry_diversity",               "Industry diversity (1-HHI)",         0.66),
    ("nec_cs",       "data/clean/nec2018/mun_size_formality.csv",          "share_female_led",                 "Share female-led",                   0.31),
]

# load instrument for the regression-sample stats
inst = pd.read_csv("data/clean/instrument/instrument_mun.csv",
                   usecols=["lgcode","year","mig_intensity","total_migrants"])

def summary(s):
    s = pd.to_numeric(s, errors='coerce')
    n_total = len(s); n_na = s.isna().sum()
    s_nn = s.dropna()
    if not len(s_nn):
        return dict(n=0, n_na=n_na, mean=np.nan, sd=np.nan, min=np.nan, p25=np.nan,
                    p50=np.nan, p75=np.nan, max=np.nan, pct_zero=np.nan)
    return dict(
        n=len(s_nn), n_na=int(n_na),
        mean=s_nn.mean(), sd=s_nn.std(),
        min=s_nn.min(), p25=s_nn.quantile(.25),
        p50=s_nn.median(), p75=s_nn.quantile(.75),
        max=s_nn.max(),
        pct_zero=100*(s_nn==0).mean(),
    )

# ----- loop -----
rows = []
file_cache = {}
for src, csv, var, label, slide_mean in VARS:
    if csv not in file_cache:
        file_cache[csv] = pd.read_csv(csv, low_memory=False)
    df = file_cache[csv]
    if var not in df.columns:
        rows.append({**{"source":src,"variable":var,"label":label,"slide_mean":slide_mean,
                        "raw_n":None,"raw_mean":None,"raw_sd":None,"raw_min":None,
                        "raw_p50":None,"raw_max":None,"raw_pct_zero":None,
                        "reg_k0_n":None,"reg_k0_mean":None,"reg_k25_n":None,"reg_k25_mean":None,
                        "note":"variable not in CSV"}})
        continue
    raw = summary(df[var])
    # regression-sample stats: merge with instrument on lgcode + year (if year exists)
    reg = df.copy()
    if 'lgcode' in reg.columns:
        if 'year' in reg.columns:
            reg = reg.merge(inst, on=['lgcode','year'], how='inner')
        else:
            # cross-section: use latest year (2018) instrument
            inst_yr = inst[inst['year']==2018][['lgcode','mig_intensity','total_migrants']]
            reg = reg.merge(inst_yr, on='lgcode', how='inner')
    else:
        reg = pd.DataFrame()
    if not reg.empty:
        rk0  = summary(reg[var])
        rk25 = summary(reg.loc[reg['total_migrants']>=25, var])
    else:
        rk0 = rk25 = {k: None for k in raw}
    rows.append({
        "source": src, "variable": var, "label": label, "slide_mean": slide_mean,
        "raw_n": raw["n"], "raw_mean": raw["mean"], "raw_sd": raw["sd"],
        "raw_min": raw["min"], "raw_p50": raw["p50"], "raw_max": raw["max"],
        "raw_pct_zero": raw["pct_zero"],
        "reg_k0_n":  rk0["n"],  "reg_k0_mean":  rk0["mean"],
        "reg_k25_n": rk25["n"], "reg_k25_mean": rk25["mean"],
        "note": ""
    })

audit = pd.DataFrame(rows)
# Discrepancy flag: slide_mean vs raw_mean
def flag(row):
    if row['slide_mean'] is None or pd.isna(row['raw_mean']):
        return ""
    s, r = row['slide_mean'], row['raw_mean']
    if s == 0: return "" if r == 0 else "DIFF"
    pct = abs(s - r) / abs(s) * 100
    if pct > 5: return f"DIFF {pct:.0f}%"
    return ""
audit['flag_raw_vs_slide'] = audit.apply(flag, axis=1)

out_path = ROOT / "output/tab/audit_deck_variables.csv"
out_path.parent.mkdir(parents=True, exist_ok=True)
audit.to_csv(out_path, index=False, float_format="%.4f")
print(f"Saved {out_path}  ({len(audit)} rows)")

# Console summary
print("\n=== Discrepancies (slide_mean vs raw_mean > 5%) ===")
disc = audit[audit['flag_raw_vs_slide']!=""][
    ['source','variable','label','slide_mean','raw_mean','reg_k0_mean','reg_k25_mean','flag_raw_vs_slide']
].sort_values('source')
print(disc.to_string(index=False))

print("\n=== High zero-mass variables (>50% zeros) ===")
zm = audit[audit['raw_pct_zero'] > 50][
    ['source','variable','label','raw_n','raw_mean','raw_p50','raw_max','raw_pct_zero']
]
print(zm.to_string(index=False))

print("\n=== Variables with very heavy right tail (max/mean > 50) ===")
audit['tail'] = audit['raw_max'] / audit['raw_mean'].replace(0, np.nan)
ht = audit[audit['tail'] > 50][['source','variable','label','raw_mean','raw_p50','raw_max','tail']]
print(ht.to_string(index=False))
