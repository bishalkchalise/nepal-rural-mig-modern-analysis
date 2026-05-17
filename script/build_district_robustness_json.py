#!/usr/bin/env python3
"""
Rebuild docs/district_robustness.json from existing district CSV,
reshaped to match muni portal conventions (groups, column names, simpler dropdowns).

- Outcome groups: muni-style names (capitalised, with em-dashes)
- Columns: A2/A3/A4 (was M2/M3/M4); drop M5
- Scalings: log + lin only (drop raw)
- Add Migration group containing in/out/net for both lifetime and 5-yr

Uses district-analysis/output/tab/district_robustness_grid.csv (no R re-run).
"""

import csv, json, os
from collections import defaultdict

GRID_MAIN = "district-analysis/output/tab/robustness_all_panels.csv"
GRID_DROP = "district-analysis/output/tab/robustness_drop_districts.csv"
OUT       = "docs/district_robustness.json"

# === Outcome -> (group, label) mapping ===
# Match muni group naming convention exactly. District-specific outcomes
# (e.g. mig_out_*, net_*, _temp_*) get explicit entries below.

CENSUS_MAP = {
    # MIGRATION (merges muni "In-migration" + new out/net)
    # -- permanent (lifetime / birth district)
    "mig_in_internal_share":             ("Migration", "In-migrant share (lifetime)"),
    "mig_out_internal_share":            ("Migration", "Out-migrant share (lifetime)"),
    "net_internal_mig_share":            ("Migration", "Net internal migration (lifetime)"),
    # -- temporary (5-year)
    "mig_in_temp_share":                 ("Migration", "In-mig (5-yr, temporary)"),
    "mig_out_temp_share":                ("Migration", "Out-mig (5-yr, temporary)"),
    "net_temp_mig_share":                ("Migration", "Net migration (5-yr, temporary)"),
    "mig_in_temp_economic_share":        ("Migration", "In-mig 5-yr, economic reason"),
    "mig_in_temp_noneconomic_share":     ("Migration", "In-mig 5-yr, non-economic reason"),
    "mig_out_temp_economic_share":       ("Migration", "Out-mig 5-yr, economic reason"),
    "mig_out_temp_noneconomic_share":    ("Migration", "Out-mig 5-yr, non-economic reason"),
    # NB: lifetime economic/noneconomic reason splits dropped (no 2011 data;
    #     all panel-fit cells are NaN -- outcome would show only mean_y)
    # ASSETS
    "amen_assets_radio":      ("Assets", "Radio"),
    "amen_assets_tv":         ("Assets", "Television"),
    "amen_assets_landline":   ("Assets", "Landline phone"),
    "amen_assets_mobile":     ("Assets", "Mobile phone"),
    "amen_assets_computer":   ("Assets", "Computer"),
    "amen_assets_internet":   ("Assets", "Internet"),
    "amen_assets_cycle":      ("Assets", "Bicycle"),
    "amen_assets_motorcycle": ("Assets", "Motorcycle"),
    "amen_assets_car":        ("Assets", "Car"),
    "amen_assets_fridge":     ("Assets", "Refrigerator"),
    "amen_asset_count_mean":  ("Assets", "Mean asset count"),
    # AMENITIES (merges cooking, lighting, water, toilet)
    "amen_water_piped":       ("Amenities", "Piped water"),
    "amen_water_traditional": ("Amenities", "Traditional water source"),
    "amen_cooking_wood":      ("Amenities", "Wood/firewood cooking"),
    "amen_cooking_kerosene":  ("Amenities", "Kerosene cooking"),
    "amen_cooking_lpg":       ("Amenities", "LPG cooking"),
    "amen_cooking_biogas":    ("Amenities", "Biogas cooking"),
    "amen_cooking_electric":  ("Amenities", "Electric cooking"),
    "amen_cooking_modern":    ("Amenities", "Modern cooking fuel"),
    "amen_cooking_traditional": ("Amenities", "Traditional cooking fuel"),
    "amen_lighting_electricity": ("Amenities", "Lighting: electricity"),
    "amen_lighting_kerosene": ("Amenities", "Lighting: kerosene"),
    "amen_lighting_biogas":   ("Amenities", "Lighting: biogas"),
    "amen_lighting_others":   ("Amenities", "Lighting: other"),
    "amen_toilet_modern":     ("Amenities", "Toilet: modern"),
    "amen_toilet_ordinary":   ("Amenities", "Toilet: ordinary"),
    "amen_toilet_any":        ("Amenities", "Toilet: any"),
    "amen_toilet_none":       ("Amenities", "Toilet: none"),
    # HOUSING
    "housing_own":                    ("Housing", "Own dwelling"),
    "housing_rented":                 ("Housing", "Rented dwelling"),
    "housing_foundation_modern":      ("Housing", "Modern foundation"),
    "housing_foundation_traditional": ("Housing", "Traditional foundation"),
    "housing_roof_modern":            ("Housing", "Modern roof"),
    "housing_roof_traditional":       ("Housing", "Traditional roof"),
    # INDUSTRY
    "ind_agri_forestry_fish":      ("Industry", "Agriculture, forestry, fishing"),
    "ind_manufacturing":           ("Industry", "Manufacturing"),
    "ind_construction":            ("Industry", "Construction"),
    "ind_wholesale_retail":        ("Industry", "Wholesale & retail"),
    "ind_transport_accommodation": ("Industry", "Transport & accommodation"),
    "ind_finance_real_estate_prof":("Industry", "Finance / RE / professional"),
    "ind_public_admin_defence":    ("Industry", "Public admin & defence"),
    "ind_education":               ("Industry", "Education"),
    "ind_health":                  ("Industry", "Health"),
    "ind_arts_recreation":         ("Industry", "Arts & recreation"),
    "ind_others":                  ("Industry", "Other industry"),
    # OCCUPATION
    "occ_share_armed_forces":      ("Occupation", "Armed forces"),
    "occ_share_managers":          ("Occupation", "Managers"),
    "occ_share_professionals":     ("Occupation", "Professionals"),
    "occ_share_technicians":       ("Occupation", "Technicians"),
    "occ_share_office_assistants": ("Occupation", "Office assistants"),
    "occ_share_service_sales":     ("Occupation", "Service & sales"),
    "occ_share_agriculture":       ("Occupation", "Agricultural occupations"),
    "occ_share_craft_trades":      ("Occupation", "Craft & trades"),
    "occ_share_machine_operators": ("Occupation", "Machine operators"),
    "occ_share_elementary":        ("Occupation", "Elementary occupations"),
    # EMPLOYMENT STATUS
    "emp_share_employee":              ("Employment Status", "Wage employee"),
    "emp_share_employer":              ("Employment Status", "Employer"),
    "emp_share_self_employed":         ("Employment Status", "Self-employed"),
    "emp_share_unpaid_family_worker":  ("Employment Status", "Unpaid family worker"),
    # EDUCATION
    "edu_literate":                  ("Education", "Literacy"),
    "edu_literate_female":           ("Education", "Female literacy"),
    "edu_literate_male":             ("Education", "Male literacy"),
    "edu_school_attend_6_16":        ("Education", "School attendance (6-16)"),
    "edu_school_attend_6_16_female": ("Education", "School attendance (6-16) — female"),
    "edu_school_attend_6_16_male":   ("Education", "School attendance (6-16) — male"),
    # DROPPED (per user): Marriage, Female Labor, Male Labor, Gender Gaps,
    #                     Work Activity, Female Ownership, Absent HH
}

HH_MAP = {
    # Migration -- HH
    "has_migrant_intl":            ("Migration — HH", "Has international migrant"),
    "n_intl_migrants":             ("Migration — HH", "# international migrants"),
    "remit_amount_intl_12m_rs":    ("Migration — HH", "Intl remittance, 12m (Rs)"),
    "remit_received":              ("Migration — HH", "Any remittance received"),
    # HH spending (merges food, non-food, education, health spending)
    "food_exp_total_7day":         ("HH spending", "Food exp, total 7d"),
    "food_exp_purchased_7day":     ("HH spending", "Food exp, purchased 7d"),
    "food_exp_homeprod_7day":      ("HH spending", "Food exp, home-produced 7d"),
    "nonfood_exp_12m":             ("HH spending", "Non-food exp, 12m"),
    "edu_spend_total_12m":         ("HH spending", "Education spend, 12m"),
    "hlt_spend_total":             ("HH spending", "Health spend"),
    # Land use -- wet/dry (ag)
    "share_self_wet":              ("Land use — wet/dry", "Own-cultivated, wet"),
    "share_self_dry":              ("Land use — wet/dry", "Own-cultivated, dry"),
    "share_both_seasons":          ("Land use — wet/dry", "Cultivated, both seasons"),
    "share_fallow_wet":            ("Land use — wet/dry", "Fallow, wet"),
    "share_fallow_dry":            ("Land use — wet/dry", "Fallow, dry"),
    "share_rented_out_wet":        ("Land use — wet/dry", "Rented out, wet"),
    # Land -- agriculture (assets/equipment + inputs)
    "owns_plough":                 ("Land — agriculture", "Owns plough"),
    "owns_powered_machinery":      ("Land — agriculture", "Owns powered machinery"),
    "owns_irrigation_kit":         ("Land — agriculture", "Owns irrigation kit"),
    "owns_storage_struct":         ("Land — agriculture", "Owns storage"),
    "owns_transport":              ("Land — agriculture", "Owns transport"),
    "n_equip_categories":          ("Land — agriculture", "# equipment categories"),
    "n_powered_types":             ("Land — agriculture", "# powered equipment types"),
    "equip_stock_value_rs":        ("Land — agriculture", "Equipment stock value (Rs)"),
    "total_input_cost_rs":         ("Land — agriculture", "Total input cost (Rs)"),
    "input_intensity_per_sqm":     ("Land — agriculture", "Input intensity / sqm"),
    "wet_cost_seed":               ("Land — agriculture", "Wet: seed cost"),
    "dry_cost_seed":               ("Land — agriculture", "Dry: seed cost"),
    "wet_cost_fert":               ("Land — agriculture", "Wet: fertilizer cost"),
    "dry_cost_fert":               ("Land — agriculture", "Dry: fertilizer cost"),
    "wet_cost_labour":             ("Land — agriculture", "Wet: labour cost"),
    "dry_cost_labour":             ("Land — agriculture", "Dry: labour cost"),
    "wet_cost_insect":             ("Land — agriculture", "Wet: insecticide cost"),
    "dry_cost_insect":             ("Land — agriculture", "Dry: insecticide cost"),
    # Enterprise
    "has_enterprise":              ("Enterprise", "HH operates enterprise"),
    "n_enterprises":               ("Enterprise", "# enterprises"),
    "n_workers_total":             ("Enterprise", "# workers"),
    "revenue_12m":                 ("Enterprise", "Revenue, 12m"),
    "profit_12m":                  ("Enterprise", "Profit, 12m"),
    "expenses_12m":                ("Enterprise", "Expenses, 12m"),
    "capex_12m":                   ("Enterprise", "Capex, 12m"),
    "sector_trade":                ("Enterprise", "Sector: trade"),
    "sector_manufacturing":        ("Enterprise", "Sector: manufacturing"),
    "sector_services":             ("Enterprise", "Sector: services"),
    "sector_hotels":               ("Enterprise", "Sector: hospitality"),
    "sector_transport":            ("Enterprise", "Sector: transport"),
}

NEC_CS_MAP = {
    "n_firms":             ("Size — counts (level + log)", "# firms"),
    "emp_total":           ("Size — counts (level + log)", "Total employment"),
    "mean_emp_per_firm":   ("Size — counts (level + log)", "Mean employment per firm"),
    "share_firms_size_micro_1":     ("Size — distribution (counts)", "Share micro (1 worker)"),
    "share_firms_size_small_2_9":   ("Size — distribution (counts)", "Share small (2-9)"),
    "share_firms_size_medium_10_50":("Size — distribution (counts)", "Share medium (10-50)"),
    "share_firms_size_large_51p":   ("Size — distribution (counts)", "Share large (51+)"),
    "formality_index":     ("Formality", "Formality index"),
    "share_registered":    ("Formality", "Share registered"),
    "share_tax_registered":("Formality", "Share tax-registered"),
    "share_keeps_accounts":("Formality", "Share keeps accounts"),
    "share_formal_credit": ("Credit & finance", "Share with formal credit"),
    "share_borrowed_any":  ("Credit & finance", "Share borrowed (any)"),
    "share_emp_female":    ("Female-led firms", "Share female employees"),
    "share_emp_foreign":   ("Size — counts (level + log)", "Share foreign employees"),
    "share_any_foreign_cap":("Credit & finance", "Share with foreign capital"),
}

NEC_PANEL_MAP = {
    "n_new_firms":     ("Firm entry — total (level + log)", "# new firms"),
    "log_n_new_firms": ("Firm entry — total (level + log)", "log(1 + new firms)"),
    "n_new_firms_size_micro_1":        ("Entry by size — counts", "Size: micro (1 worker)"),
    "log_n_new_firms_size_micro_1":    ("Entry by size — counts", "log: micro (1 worker)"),
    "n_new_firms_size_small_2_9":      ("Entry by size — counts", "Size: small (2-9)"),
    "log_n_new_firms_size_small_2_9":  ("Entry by size — counts", "log: small (2-9)"),
    "n_new_firms_size_medium_10_50":   ("Entry by size — counts", "Size: medium (10-50)"),
    "log_n_new_firms_size_medium_10_50":("Entry by size — counts", "log: medium (10-50)"),
    "n_new_firms_size_large_51p":      ("Entry by size — counts", "Size: large (51+)"),
    "log_n_new_firms_size_large_51p":  ("Entry by size — counts", "log: large (51+)"),
    "n_new_firms_agriculture":         ("Entry by sector — counts", "Agriculture"),
    "log_n_new_firms_agriculture":     ("Entry by sector — counts", "log Agriculture"),
    "n_new_firms_manufacturing":       ("Entry by sector — counts", "Manufacturing"),
    "log_n_new_firms_manufacturing":   ("Entry by sector — counts", "log Manufacturing"),
    "n_new_firms_construction":        ("Entry by sector — counts", "Construction"),
    "log_n_new_firms_construction":    ("Entry by sector — counts", "log Construction"),
    "n_new_firms_trade_retail":        ("Entry by sector — counts", "Trade & retail"),
    "log_n_new_firms_trade_retail":    ("Entry by sector — counts", "log Trade & retail"),
    "n_new_firms_hospitality_food":    ("Entry by sector — counts", "Hospitality & food"),
    "log_n_new_firms_hospitality_food":("Entry by sector — counts", "log Hospitality & food"),
    "n_new_firms_transport_storage":   ("Entry by sector — counts", "Transport & storage"),
    "log_n_new_firms_transport_storage":("Entry by sector — counts", "log Transport & storage"),
    "n_new_firms_finance_prof_realestate":("Entry by sector — counts", "Finance / RE / prof"),
    "log_n_new_firms_finance_prof_realestate":("Entry by sector — counts", "log Finance / RE / prof"),
    "n_new_firms_education_health_social":("Entry by sector — counts", "Education, health, social"),
    "log_n_new_firms_education_health_social":("Entry by sector — counts", "log Education, health, social"),
    "n_new_firms_other_services":      ("Entry by sector — counts", "Other services"),
    "log_n_new_firms_other_services":  ("Entry by sector — counts", "log Other services"),
}

DS_MAP = {"census": CENSUS_MAP, "hh": HH_MAP, "nec_cs": NEC_CS_MAP, "nec_panel": NEC_PANEL_MAP}

# ============================================================================
# Build
# ============================================================================
# Source CSVs (both written by the R pipeline)
rows_main = list(csv.DictReader(open(GRID_MAIN)))
for r in rows_main:
    r.setdefault("variant", "baseline")
rows_drop = []
if os.path.exists(GRID_DROP):
    rows_drop = list(csv.DictReader(open(GRID_DROP)))
    # drop CSV lacks `scaling` / `lag` columns -- the R script always runs
    # the drop variants at the baseline spec (scaling=log, lag=2). Fill in.
    for r in rows_drop:
        r["scaling"] = r.get("scaling") or "log"
        r["lag"]     = r.get("lag")     or "2"
    # Skip LOO rows (they're per-district drops, not the 3 portal variants)
    rows_drop = [r for r in rows_drop if r.get("variant") != "loo"]
print(f"Main rows: {len(rows_main):,} | drop rows: {len(rows_drop):,}")

# Build stats lookup from main CSV (mean_y, sd_y not yet in CSV columns
# uniformly -- pull whatever's there).
# The R aggregator stitches sd_y + n_unit into the merged grid; we don't
# have that on this side, but mean_y and n are available from the main CSV.
# sd_y and n_unit fall back to None when not available; the portal handles
# null mean/sd gracefully (skips % rescalings).
rows = rows_main + rows_drop

# Drop raw scaling. Rename M2->A2, M3->A3, M4->A4. KEEP M5 as A5 (the
# 2001-baseline control extension; census-only).
def map_model(m): return {"M2":"A2","M3":"A3","M4":"A4","M5":"A5"}.get(m)
keep = []
for r in rows:
    if r["scaling"] == "raw": continue
    nm = map_model(r["model"])
    if nm is None: continue
    r["model"] = nm
    keep.append(r)
print(f"After dropping raw: {len(keep)}")

# Filter to outcomes in our maps
def in_map(r):
    return r["outcome"] in DS_MAP.get(r["dataset"], {})
keep = [r for r in keep if in_map(r)]
print(f"After mapping filter: {len(keep)}")

# Aggregate stats per outcome
stats = {}
for r in keep:
    k = (r["dataset"], r["outcome"])
    if k in stats: continue
    try: my = float(r["mean_y"])
    except: my = None
    # sd_y / n_unit not present in raw source CSVs; default to None
    try: sdy = float(r.get("sd_y", ""))
    except: sdy = None
    try: nu = int(float(r.get("n_unit", "")))
    except: nu = None
    stats[k] = (my, sdy, nu)

# Cells
cells = defaultdict(dict)
for r in keep:
    k = (r["dataset"], r["outcome"])
    sk = f"{r['scaling']}|{r['lag']}|{r['model']}|{r['variant']}"
    c = {}
    for f in ("beta","se","p"):
        try: c[f] = float(r[f])
        except: c[f] = None
    try: c["n"] = int(float(r["n"]))
    except: c["n"] = None
    sig = r.get("sig","")
    c["sig"] = "" if sig in ("NA","NaN") else sig
    cells[k][sk] = c

# Build per-dataset
out_json = {
    "datasets_meta": {
        "census":    {"label": "Census district panel (2011, 2021)"},
        "hh":        {"label": "HRVS HH panel (2016-18, district x year residualised)"},
        "nec_cs":    {"label": "NEC 2018 district cross-section"},
        "nec_panel": {"label": "NEC entry-cohort panel (2011-2018)"}
    },
    "datasets": {}
}
for ds in ("census","hh","nec_cs","nec_panel"):
    outs = {}
    groups = set()
    for k, (g, lbl) in DS_MAP[ds].items():
        full_key = (ds, k)
        if full_key not in cells: continue
        # Drop outcomes whose cells are ALL null (no panel fits succeeded)
        any_nonnull = any(c.get("beta") is not None for c in cells[full_key].values())
        if not any_nonnull: continue
        my, sdy, nu = stats.get(full_key, (None, None, None))
        outs[k] = {
            "label": lbl, "group": g,
            "mean_y": my, "sd_y": sdy, "n_unit": nu,
            "cells": cells[full_key]
        }
        groups.add(g)
    out_json["datasets"][ds] = {"outcomes": outs, "groups": sorted(groups)}
    print(f"  {ds}: {len(outs)} outcomes in {len(groups)} groups")

with open(OUT, "w") as f:
    json.dump(out_json, f, separators=(",",":"))
print(f"\nWrote {OUT} ({os.path.getsize(OUT):,} bytes)")
