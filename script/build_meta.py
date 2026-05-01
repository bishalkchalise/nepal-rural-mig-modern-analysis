"""
Build docs/meta.json — variable definitions + summary stats for the explorer.

Reads:
  - data/clean/census/outcomes_census_codebook.csv  (long definitions)
  - data/clean/census/census_outcomes_municipality.csv  (data for stats)
  - docs/results.json   (outcome list + group structure)

Writes:
  - docs/meta.json with:
      "definitions": { y: {short, long, universe, src_2001, src_2011, src_2021, group} }
      "summary":     { y: {n_2001, n_2011, n_2021, mean_2001, mean_2011, mean_2021,
                          sd_2001, sd_2011, sd_2021, min_*, max_*} }
"""
import json, pandas as pd, numpy as np
from pathlib import Path

def _find_root():
    here = Path(__file__).resolve().parent if "__file__" in globals() else Path.cwd()
    for cand in [here, *here.parents]:
        if (cand / "data" / "clean").is_dir():
            return cand
    return Path.cwd()

ROOT = _find_root()
cb_census = pd.read_csv(ROOT / "data/clean/census/outcomes_census_codebook.csv")

# RVS codebook (data/rvs_codebook.csv) has its own schema:
#   category, variable, unit, reference, source, definition
# Reshape it to match the census codebook columns so a single cb_idx works.
def _load_rvs_codebook():
    p = ROOT / "data/rvs_codebook.csv"
    if not p.exists(): return pd.DataFrame()
    rvs_cb = pd.read_csv(p)
    rvs_cb = rvs_cb.rename(columns={
        "category":   "category",
        "variable":   "variable",
        "definition": "definition",
        "source":     "source_vars_2001",  # piggy-back: census schema has src_2001/11/21;
                                            # for RVS we only have one survey "source", repeat it.
    })
    rvs_cb["universe"]         = rvs_cb.get("reference", "")
    rvs_cb["source_vars_2011"] = rvs_cb["source_vars_2001"]
    rvs_cb["source_vars_2021"] = rvs_cb["source_vars_2001"]
    return rvs_cb[["variable","category","definition","universe",
                   "source_vars_2001","source_vars_2011","source_vars_2021"]]

cb_rvs = _load_rvs_codebook()
cb     = pd.concat([cb_census, cb_rvs], ignore_index=True) if len(cb_rvs) else cb_census

out = pd.read_csv(ROOT / "data/clean/census/census_outcomes_municipality.csv")
res = json.load(open(ROOT / "docs/results.json"))

# ── Raw summary for monetary / count outcomes that get asinh-transformed ────
# We compute mean/median (conditional on positive) so each definition can say
# "Raw mean ≈ NPR 62,000; conditional median ≈ NPR 125,000" rather than just
# "in rupees", which would be misleading when the explorer shows the
# asinh-transformed mean of e.g. 4.2.
def _read_optional(path):
    try:
        return pd.read_csv(path)
    except Exception:
        return None

_rvs = {f: _read_optional(ROOT / "data/clean/rvs_outcomes" / f"{f}.csv") for f in [
    "agriculture_hh_year","migration_hh_year","labour_hh_year","enterprise_hh_year",
    "consumption_hh_year","health_hh_year","education_hh_year","shocks_coping_hh_year"
]}
_nec_mun = _read_optional(ROOT / "data/clean/nec2018/municipality_analysis.csv")
_nec_ec  = _read_optional(ROOT / "data/clean/nec2018/entry_cohort_panel.csv")

# Map: variable → (source DataFrame, raw units string)
RAW_SRC = {
    # RVS monetary
    "ag_equip_stock_value_rs":             (_rvs["agriculture_hh_year"], "rupees"),
    "input_total_12m_rs":                  (_rvs["agriculture_hh_year"], "rupees"),
    "crop_sales_12m_rs":                   (_rvs["agriculture_hh_year"], "rupees"),
    "remit_amount_12m_rs":                 (_rvs["migration_hh_year"],   "rupees"),
    "remit_amount_intl_12m_rs":            (_rvs["migration_hh_year"],   "rupees"),
    "wage_total_income_12m_rs":            (_rvs["labour_hh_year"],      "rupees"),
    "enterprise_revenue_12m":              (_rvs["enterprise_hh_year"],  "rupees"),
    "enterprise_expenses_12m":             (_rvs["enterprise_hh_year"],  "rupees"),
    "enterprise_profit_12m":               (_rvs["enterprise_hh_year"],  "rupees"),
    "enterprise_capex_12m":                (_rvs["enterprise_hh_year"],  "rupees"),
    "food_total_7day":                     (_rvs["consumption_hh_year"], "rupees"),
    "food_purchased_7day":                 (_rvs["consumption_hh_year"], "rupees"),
    "food_homeprod_7day":                  (_rvs["consumption_hh_year"], "rupees"),
    "food_staples_7day":                   (_rvs["consumption_hh_year"], "rupees"),
    "food_protein_7day":                   (_rvs["consumption_hh_year"], "rupees"),
    "food_animal_7day":                    (_rvs["consumption_hh_year"], "rupees"),
    "food_vegfruit_7day":                  (_rvs["consumption_hh_year"], "rupees"),
    "nonfood_exp_30day":                   (_rvs["consumption_hh_year"], "rupees"),
    "nonfood_exp_12m":                     (_rvs["consumption_hh_year"], "rupees"),
    "nonfood_basic_nonfood_12m":           (_rvs["consumption_hh_year"], "rupees"),
    "nonfood_energy_fuel_lighting_12m":    (_rvs["consumption_hh_year"], "rupees"),
    "nonfood_clothing_personal_12m":       (_rvs["consumption_hh_year"], "rupees"),
    "nonfood_transport_communication_12m": (_rvs["consumption_hh_year"], "rupees"),
    "nonfood_housing_household_12m":       (_rvs["consumption_hh_year"], "rupees"),
    "nonfood_education_leisure_12m":       (_rvs["consumption_hh_year"], "rupees"),
    "nonfood_social_ceremonial_financial_12m": (_rvs["consumption_hh_year"], "rupees"),
    "nonfood_luxury_valuables_12m":        (_rvs["consumption_hh_year"], "rupees"),
    "durables_stock_value":                (_rvs["consumption_hh_year"], "rupees"),
    "durables_use_value_12m":              (_rvs["consumption_hh_year"], "rupees"),
    "hlt_spend_total":                     (_rvs["health_hh_year"],      "rupees"),
    "scholarship_amt_12m":                 (_rvs["education_hh_year"],   "rupees"),
    "edu_spend_total_12m":                 (_rvs["education_hh_year"],   "rupees"),
    "edu_spend_per_enrolled":              (_rvs["education_hh_year"],   "rupees"),
    # NEC cross-section
    "n_firms":                         (_nec_mun, "firms"),
    "emp_total":                       (_nec_mun, "workers"),
    "rev_mean":                        (_nec_mun, "rupees"),
    "labor_prod_median":               (_nec_mun, "rupees / worker"),
    "value_added_pw_median":           (_nec_mun, "rupees / worker"),
    "capital_intensity_median":        (_nec_mun, "rupees / worker"),
    # NEC entry cohort
    "n_firms_surviving":               (_nec_ec, "firms"),
    "emp_surviving":                   (_nec_ec, "workers"),
    "rev_surviving":                   (_nec_ec, "rupees"),
    "cap_surviving":                   (_nec_ec, "rupees"),
}

def _fmt_amt(x, units):
    if x is None or pd.isna(x): return "—"
    if units == "rupees" or units.startswith("rupees"):
        if x >= 10_000_000:  return f"NPR {x/10_000_000:.1f} crore"
        if x >= 100_000:     return f"NPR {x/100_000:.1f} lakh"
        if x >= 1_000:       return f"NPR {x/1_000:,.0f}k"
        return f"NPR {x:,.0f}"
    if x >= 1_000:           return f"{x:,.0f} {units}"
    return f"{x:,.1f} {units}"

raw_stats = {}   # variable → dict
for var, (df, units) in RAW_SRC.items():
    if df is None or var not in df.columns: continue
    s = df[var].dropna()
    if len(s) == 0: continue
    s_pos = s[s > 0]
    raw_stats[var] = {
        "units":      units,
        "mean":       float(s.mean()),
        "median":     float(s.median()),
        "median_pos": float(s_pos.median()) if len(s_pos) > 0 else None,
        "n":          int(len(s)),
        "share_pos":  float(len(s_pos) / len(s)),
    }
    # Conditional-on-positive variant: same source variable, but stats over s>0 only.
    if len(s_pos) > 0:
        raw_stats[f"{var}_pos"] = {
            "units":      units,
            "mean":       float(s_pos.mean()),
            "median":     float(s_pos.median()),
            "median_pos": float(s_pos.median()),
            "n":          int(len(s_pos)),
            "share_pos":  1.0,
        }

# Multi-dataset structure: collect outcomes from every dataset
ours = {}
if "datasets" in res:
    for ds_key, ds in res["datasets"].items():
        for y, meta in ds["outcomes"].items():
            # Last-write-wins, but census comes first so labels prefer census variants
            if y not in ours:
                ours[y] = dict(meta, dataset=ds_key)
else:
    ours = res["outcomes"]

# Index codebook by variable
cb_idx = cb.set_index("variable")

# Manual short-definition fallback for our outcomes that aren't in the codebook.
# RVS variables are all sourced from data/rvs_codebook.csv (loaded above into
# the merged cb), so this fallback only covers NEC and census-derived outcomes.
SHORT_FALLBACK = {
    # === NEC cross-section + entry-cohort panel ===
    "n_firms":                 "Number of firms in municipality (2018 NEC).",
    "emp_total":               "Total employment across all firms in muni.",
    "mean_emp_per_firm":       "Mean employment per firm.",
    "p90_emp_per_firm":        "90th percentile of employment per firm.",
    "share_firms_size_micro_1":"Share of firms with 1 worker.",
    "share_firms_size_small_2_9":"Share of firms with 2-9 workers.",
    "share_firms_size_medium_10_50":"Share of firms with 10-50 workers.",
    "share_firms_size_large_51p":"Share of firms with 51+ workers.",
    "share_registered":        "Share of firms officially registered.",
    "share_tax_registered":    "Share of firms tax-registered (VAT/PAN).",
    "share_keeps_accounts":    "Share of firms keeping written accounts.",
    "share_incorporated":      "Share of firms incorporated (limited company).",
    "formality_index":         "Composite formality index across registration measures.",
    "share_firms_sec_sec_manuf":"Share of firms in manufacturing.",
    "share_firms_sec_sec_construct":"Share of firms in construction.",
    "share_firms_sec_sec_wholesale":"Share of firms in wholesale & retail.",
    "share_firms_sec_sec_hospitality":"Share of firms in hospitality.",
    "share_firms_sec_sec_transport":"Share of firms in transport.",
    "share_firms_sec_sec_services":"Share of firms in other services.",
    "share_firms_sec_sec_health":"Share of firms in health services.",
    "share_firms_sec_sec_education":"Share of firms in education services.",
    "share_trd_tradable_goods":"Share of firms producing tradable goods.",
    "share_trd_tradable_services":"Share of firms providing tradable services.",
    "share_trd_non_tradable_services":"Share of firms providing non-tradable services.",
    "share_modern_modern_services":"Share in 'modern services' (finance, ICT, prof).",
    "share_modern_modern_manuf":"Share in 'modern' manufacturing tier.",
    "share_modern_traditional_commerce":"Share in traditional commerce.",
    "share_modern_traditional_services":"Share in traditional services.",
    "rev_mean":                "Mean firm revenue (rupees).",
    "labor_prod_median":       "Median labour productivity (revenue/worker).",
    "value_added_pw_median":   "Median value added per worker.",
    "capital_intensity_median":"Median capital per worker.",
    "profit_margin_median":    "Median profit margin.",
    "share_borrowed_any":      "Share of firms that borrowed any credit.",
    "share_formal_credit":     "Share of firms with formal credit.",
    "interest_p50":            "Median interest rate paid.",
    "share_female_manager":    "Share of firms with a female manager.",
    "share_female_owner":      "Share of firms with a female owner.",
    "share_female_led":        "Share of female-led firms.",
    "share_female_workers":    "Share of female workers across firms.",
    "share_emp_female":        "Share of total employment that is female.",
    "share_firms_young_5y":    "Share of firms < 5 years old.",
    "share_firms_mature_10y":  "Share of firms > 10 years old.",
    "median_firm_age":         "Median firm age.",
    "n_firms_surviving":       "Firms born in this year that survived to 2018.",
    "emp_surviving":           "Total employment in this year's surviving cohort.",
    "rev_surviving":           "Total revenue in this year's surviving cohort.",
    "cap_surviving":           "Total capital in this year's surviving cohort.",
    "median_firm_age_years":   "Median age of firms in this cohort (years to 2018).",
    "n_firms_surviving_size_micro_1":"Surviving micro firms (1 worker).",
    "n_firms_surviving_size_small_2_9":"Surviving small firms (2-9 workers).",
    "n_firms_surviving_size_medium_10_50":"Surviving medium firms (10-50 workers).",
    "n_firms_surviving_size_large_51p":"Surviving large firms (51+ workers).",
    "n_firms_surviving_sec_manuf":"Surviving cohort firms in manufacturing.",
    "n_firms_surviving_sec_construct":"Surviving cohort firms in construction.",
    "n_firms_surviving_sec_wholesale":"Surviving cohort firms in wholesale & retail.",
    "n_firms_surviving_sec_hospitality":"Surviving cohort firms in hospitality.",
    "n_firms_surviving_sec_transport":"Surviving cohort firms in transport.",
    "n_firms_surviving_sec_services":"Surviving cohort firms in services.",
    "n_firms_surviving_sec_health":"Surviving cohort firms in health.",
    "n_firms_surviving_sec_education":"Surviving cohort firms in education.",
    "n_firms_surviving_sec_finance":"Surviving cohort firms in finance.",
    "n_firms_surviving_sec_arts":"Surviving cohort firms in arts.",
    "n_firms_surviving_trd_tradable_goods":"Surviving cohort firms producing tradable goods.",
    "n_firms_surviving_trd_tradable_services":"Surviving cohort firms producing tradable services.",
    "n_firms_surviving_trd_non_tradable_services":"Surviving cohort firms in non-tradable services.",
    "n_firms_surviving_modern_modern_services":"Surviving cohort firms in modern services.",
    "n_firms_surviving_modern_modern_manuf":"Surviving cohort firms in modern manufacturing.",
    "n_firms_surviving_modern_traditional_commerce":"Surviving cohort firms in traditional commerce.",
    "n_firms_surviving_modern_traditional_services":"Surviving cohort firms in traditional services.",
    "n_firms_surviving_modern_traditional_agriculture":"Surviving cohort firms in traditional agriculture.",

    # === Census ===
    "amen_water_piped":         "Share of HHs with piped/tap drinking water.",
    "amen_water_traditional":   "Share of HHs with traditional water source (well, river, etc.).",
    "ind_arts_recreation":      "Workforce share in arts/recreation industry (ISIC R+S).",
    "ind_health":               "Workforce share in health/social work (ISIC Q).",
    "ind_transport_accommodation":"Workforce share in transport + accommodation (ISIC H+I).",
    "housing_roof_modern":      "Share of HHs with modern roof material (concrete/metal/tile).",
    "housing_roof_traditional": "Share of HHs with traditional roof (thatch/wood/mud).",
    "housing_foundation_modern":"Share of HHs with modern foundation (concrete/brick).",
    "housing_foundation_traditional":"Share of HHs with traditional foundation (mud/stone).",
    "housing_own":              "Share of HHs that own their dwelling.",
    "housing_rented":           "Share of HHs renting their dwelling.",
    "fem_employment_rate":      "Female employment rate (employed / female pop 15-60).",
    "fem_share_of_ag_workers":  "Female share among agricultural workers.",
    "fem_ag_specialization_ratio":"Ratio of female agri share to overall agri share.",
    "fem_wage_share_of_employment":"Share of female employment in wage work.",
    "mlfp_agri":                "Male LFP share in agriculture.",
    "mlfp_nonagri":             "Male LFP share in non-agriculture.",
    "flfp_agri":                "Female LFP in agriculture.",
    "flfp_nonagri":             "Female LFP in non-agriculture.",
    "flfp_wage":                "Female LFP in wage employment.",
    "flfp_chores_only":         "Share of women whose main activity is chores.",
    "gap_lfp_m_minus_f":        "Male - female LFP gap (percentage points).",
    "gap_nonagri_m_minus_f":    "Male - female non-agri LFP gap (pp).",
    "head_age_mean":            "Mean age of household head.",
    "head_elderly_share":       "Share of HHs with head aged 60+.",
    "head_young_share":         "Share of HHs with head aged < 30.",
    "head_female_share":        "Share of female-headed HHs.",
    "head_female_elderly":      "Share of HHs with female elderly head.",
    "absent_hh_share":          "Share of HHs with at least one absentee member.",
    "share_men":                "Male share of total population.",
    "share_women":              "Female share of total population.",
    "left_not_with_both":       "Share of children not living with both parents.",
    "left_mother_only":         "Share of children living with mother only.",
    "left_father_only":         "Share of children living with father only.",
    "left_with_relatives":      "Share of children living with relatives only.",
    "left_without_parents":     "Share of children living without either parent.",
}

definitions = {}
summary = {}

def _stat_block(series):
    s = series.dropna()
    return {
        "n":    int(len(s)),
        "mean": float(s.mean())   if len(s)    else None,
        "sd":   float(s.std())    if len(s) > 1 else None,
        "min":  float(s.min())    if len(s)    else None,
        "max":  float(s.max())    if len(s)    else None,
        "median": float(s.median()) if len(s)  else None,
    }

# Map RVS file → list of variables it owns (built from its columns)
_rvs_var_to_df = {}
for fname, df in _rvs.items():
    if df is None: continue
    for c in df.columns:
        _rvs_var_to_df.setdefault(c, df)

def _compute_summary(y, base_y, ds_key, census_df, rvs_dfs, nec_mun_df, nec_ec_df):
    """Return {"years":[...], "n_<yr>":..., "mean_<yr>":..., ...} keyed by year.

    Year ranges per dataset:
      census   → 2001, 2011, 2021
      rvs      → 2016, 2017, 2018
      nec_cs   → 2018 only
      nec_panel→ pooled summary (all founding years 2001-2018)
    """
    # _pos variants use the base variable's data, restricted to >0
    pos_only = y.endswith("_pos")
    src_var = base_y

    if ds_key == "census":
        years = [2001, 2011, 2021]
        out_blob = {"years": years}
        if src_var not in census_df.columns:
            for yr in years:
                out_blob.update({f"n_{yr}":0, f"mean_{yr}":None, f"sd_{yr}":None,
                                 f"min_{yr}":None, f"max_{yr}":None})
            return out_blob
        for yr in years:
            d = census_df.query("year==@yr")[src_var]
            if pos_only: d = d[d > 0]
            blk = _stat_block(d)
            out_blob[f"n_{yr}"]    = blk["n"]
            out_blob[f"mean_{yr}"] = blk["mean"]
            out_blob[f"sd_{yr}"]   = blk["sd"]
            out_blob[f"min_{yr}"]  = blk["min"]
            out_blob[f"max_{yr}"]  = blk["max"]
        return out_blob

    if ds_key == "rvs":
        years = [2016, 2017, 2018]
        out_blob = {"years": years}
        df = _rvs_var_to_df.get(src_var)
        if df is None or "year" not in df.columns:
            for yr in years:
                out_blob.update({f"n_{yr}":0, f"mean_{yr}":None, f"sd_{yr}":None,
                                 f"min_{yr}":None, f"max_{yr}":None})
            return out_blob
        for yr in years:
            d = df.query("year==@yr")[src_var]
            if pos_only: d = d[d > 0]
            blk = _stat_block(d)
            out_blob[f"n_{yr}"]    = blk["n"]
            out_blob[f"mean_{yr}"] = blk["mean"]
            out_blob[f"sd_{yr}"]   = blk["sd"]
            out_blob[f"min_{yr}"]  = blk["min"]
            out_blob[f"max_{yr}"]  = blk["max"]
        return out_blob

    if ds_key == "nec_cs":
        years = [2018]
        out_blob = {"years": years}
        if nec_mun_df is None or src_var not in nec_mun_df.columns:
            out_blob.update({"n_2018":0, "mean_2018":None, "sd_2018":None,
                             "min_2018":None, "max_2018":None})
            return out_blob
        d = nec_mun_df[src_var]
        if pos_only: d = d[d > 0]
        blk = _stat_block(d)
        out_blob.update({"n_2018":blk["n"], "mean_2018":blk["mean"],
                         "sd_2018":blk["sd"], "min_2018":blk["min"],
                         "max_2018":blk["max"]})
        return out_blob

    if ds_key == "nec_panel":
        # Founding-year × muni panel (2001-2018). For summary, pool all years
        # AND show start (2001-2005), middle (2006-2012), and recent (2013-2018)
        # cohort means so the user sees the cohort-time pattern.
        out_blob = {"years": ["2001-05", "2006-12", "2013-18"]}
        if nec_ec_df is None or src_var not in nec_ec_df.columns:
            for tag in out_blob["years"]:
                out_blob.update({f"n_{tag}":0, f"mean_{tag}":None, f"sd_{tag}":None,
                                 f"min_{tag}":None, f"max_{tag}":None})
            return out_blob
        yr_col = "founding_year_ad" if "founding_year_ad" in nec_ec_df.columns else "year"
        bins = [(2001,2005,"2001-05"), (2006,2012,"2006-12"), (2013,2018,"2013-18")]
        for lo, hi, tag in bins:
            sub = nec_ec_df[(nec_ec_df[yr_col] >= lo) & (nec_ec_df[yr_col] <= hi)]
            d = sub[src_var]
            if pos_only: d = d[d > 0]
            blk = _stat_block(d)
            out_blob[f"n_{tag}"]    = blk["n"]
            out_blob[f"mean_{tag}"] = blk["mean"]
            out_blob[f"sd_{tag}"]   = blk["sd"]
            out_blob[f"min_{tag}"]  = blk["min"]
            out_blob[f"max_{tag}"]  = blk["max"]
        return out_blob

    # Unknown dataset
    return {"years": []}

for y, meta in ours.items():
    label = meta["label"]
    group = meta["group"]

    # _pos variants inherit the base variable's definition. We then append
    # a "Subsample restricted to ..." note via the raw_blurb logic below.
    base_y = y[:-4] if y.endswith("_pos") else y

    if base_y in cb_idx.index:
        row = cb_idx.loc[base_y]
        if isinstance(row, pd.DataFrame): row = row.iloc[0]
        defn = str(row["definition"]) if "definition" in row else ""
        short = defn.split(".")[0] + "." if defn else (label + ".")
        long_ = defn or label
        universe = str(row["universe"]) if "universe" in row and pd.notna(row.get("universe")) else ""
        src01 = str(row.get("source_vars_2001","") or "")
        src11 = str(row.get("source_vars_2011","") or "")
        src21 = str(row.get("source_vars_2021","") or "")
    elif base_y in SHORT_FALLBACK:
        short = SHORT_FALLBACK[base_y]
        long_ = SHORT_FALLBACK[base_y]
        universe = ""
        src01 = src11 = src21 = ""
    else:
        short = label + "."
        long_ = label + " — definition derived from variable name; no codebook entry available."
        universe = ""
        src01 = src11 = src21 = ""
    # Append a raw-units note so monetary outcomes always make sense
    # in the explorer, even when most HHs have value 0.
    raw_blurb = ""
    raw_units = None
    if y in raw_stats:
        rs = raw_stats[y]
        raw_units = rs["units"]
        # _pos variants are conditional-on-positive subsamples
        if y.endswith("_pos"):
            raw_blurb = (f" Subsample restricted to HHs with raw > 0: "
                         f"mean = {_fmt_amt(rs['mean'], rs['units'])}, "
                         f"median = {_fmt_amt(rs['median'], rs['units'])}, "
                         f"N = {rs['n']:,}.")
        else:
            extras = []
            if rs.get('median') is not None:
                extras.append(f"median = {_fmt_amt(rs['median'], rs['units'])}")
            if 0 < rs['share_pos'] < 1:
                extras.append(f"{rs['share_pos']*100:.0f}% have raw > 0")
            if rs.get('median_pos') is not None and rs['median'] == 0:
                extras.append(f"conditional median = {_fmt_amt(rs['median_pos'], rs['units'])}")
            raw_blurb = f" Raw mean = {_fmt_amt(rs['mean'], rs['units'])}"
            if extras: raw_blurb += " (" + "; ".join(extras) + ")"
            raw_blurb += "."
        if not long_.endswith("."):
            long_ = long_ + "."
        long_ = long_ + raw_blurb

    definitions[y] = {
        "label": label,
        "group": group,
        "short": short,
        "long":  long_,
        "universe": universe,
        "src_2001": src01,
        "src_2011": src11,
        "src_2021": src21,
        "raw_units": raw_units,
        "raw_stats": raw_stats.get(y),
    }

    # Summary stats by year. Each dataset has its own year range, so we
    # tag the summary block with the dataset and the actual years observed.
    ds_key = meta.get("dataset", "census")
    base_y_for_data = base_y  # _pos variants point at the underlying base var
    s = _compute_summary(y, base_y_for_data, ds_key, out, _rvs, _nec_mun, _nec_ec)
    s["dataset"] = ds_key
    summary[y] = s

meta_out = {"definitions": definitions, "summary": summary, "groups": list(set(m["group"] for m in ours.values()))}
with open(str(ROOT / "docs/meta.json"),"w") as f:
    json.dump(meta_out, f, separators=(",",":"))
print(f"Wrote {len(definitions)} definitions, {len(summary)} summary entries.")
print(f"File: {ROOT / 'docs/meta.json'}")
