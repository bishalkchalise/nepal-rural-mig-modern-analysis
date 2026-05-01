"""
Build the Khanna §IIIC trade shift-share variable from raw Comtrade flows.

Inputs (produced by script/fetch_comtrade.py):
  - data/clean/instrument/trade_baseline_partner_industry.csv

Plus existing in repo:
  - data/clean/instrument/instrument_mun.csv  (for FX shifters Z_dt and 2001 weights)
  - data/clean/census/census_outcomes_municipality.csv  (for industry employment shares L_jo)

Output:
  - data/clean/instrument/trade_ssiv.csv
    columns: lgcode, year, trade_ssiv_imp, trade_ssiv_exp

Formula (Khanna eq. 5):
    Shiftshare^trade_ot = Σ_d Σ_j (L_jo / Pop_o) · M_jd^baseline · ΔR_dt

For Nepal we collapse SITC AG2 codes into ~10 broad industries that match
census ind_* shares. Crosswalk inline below — extend as needed.
"""
import sys
import pandas as pd
import numpy as np
from pathlib import Path

def _find_root():
    here = Path(__file__).resolve().parent if "__file__" in globals() else Path.cwd()
    for cand in [here, *here.parents]:
        if (cand / "data" / "clean").is_dir():
            return cand
    return Path.cwd()

ROOT = str(_find_root())

# Lightweight crosswalk: HS/SITC 2-digit chapter -> our census industry tag.
# Extend / refine after inspecting trade_baseline_partner_industry.csv.
HS2_TO_IND = {
    # Agriculture/food
    "01":"agri","02":"agri","03":"agri","04":"agri","05":"agri","06":"agri",
    "07":"agri","08":"agri","09":"agri","10":"agri","11":"agri","12":"agri",
    "13":"agri","14":"agri","15":"agri","16":"agri","17":"agri","18":"agri",
    "19":"manuf","20":"manuf","21":"manuf","22":"manuf","23":"agri","24":"agri",
    # Mining / energy
    "25":"manuf","26":"manuf","27":"manuf",
    # Chemicals / plastics
    "28":"manuf","29":"manuf","30":"manuf","31":"manuf","32":"manuf","33":"manuf",
    "34":"manuf","35":"manuf","36":"manuf","37":"manuf","38":"manuf",
    "39":"manuf","40":"manuf",
    # Hides / textiles / footwear
    "41":"manuf","42":"manuf","43":"manuf","44":"manuf","45":"manuf","46":"manuf",
    "47":"manuf","48":"manuf","49":"manuf",
    "50":"manuf","51":"manuf","52":"manuf","53":"manuf","54":"manuf","55":"manuf",
    "56":"manuf","57":"manuf","58":"manuf","59":"manuf","60":"manuf",
    "61":"manuf","62":"manuf","63":"manuf","64":"manuf","65":"manuf",
    # Metals / machinery
    "66":"manuf","67":"manuf","68":"manuf","69":"manuf","70":"manuf",
    "71":"manuf","72":"manuf","73":"manuf","74":"manuf","75":"manuf",
    "76":"manuf","78":"manuf","79":"manuf","80":"manuf","81":"manuf",
    "82":"manuf","83":"manuf","84":"manuf","85":"manuf",
    # Vehicles / transport equipment
    "86":"transport","87":"transport","88":"transport","89":"transport","90":"manuf",
    # Other
    "91":"manuf","92":"manuf","93":"manuf","94":"manuf","95":"manuf","96":"manuf","97":"manuf",
}
IND_COLS_FOR_LJO = {
    "agri":      "ind_agri_forestry_fish",
    "manuf":     "ind_manufacturing",
    "transport": "ind_transport_accommodation",
}

raw_path = f"{ROOT}/data/clean/instrument/trade_baseline_partner_industry.csv"
try:
    raw = pd.read_csv(raw_path)
except FileNotFoundError:
    print(f"Run script/fetch_comtrade.py first ({raw_path} not found).", file=sys.stderr)
    sys.exit(1)

# Tolerate both comtradr-native (cmd_code, primary_value, partner_desc, flow_desc)
# and legacy (cmdCode, primaryValue, partnerDesc, flow) column naming.
COL_ALIAS = {
    "cmd_code":      "cmdCode",
    "primary_value": "primaryValue",
    "partner_desc":  "partnerDesc",
    "flow_desc":     "flow_long",
}
for src, tgt in COL_ALIAS.items():
    if src in raw.columns and tgt not in raw.columns:
        raw[tgt] = raw[src]
# Map "Import"/"Export" → "M"/"X" if needed
if "flow" not in raw.columns and "flow_long" in raw.columns:
    raw["flow"] = raw["flow_long"].astype(str).str.lower().map({"import":"M","export":"X"})

missing = [c for c in ("cmdCode","partnerDesc","primaryValue","flow") if c not in raw.columns]
if missing:
    print(f"trade_baseline_partner_industry.csv missing columns: {missing}", file=sys.stderr)
    sys.exit(1)

# Aggregate to (partner, industry) average baseline 1995-2001
raw["hs2"] = raw["cmdCode"].astype(str).str.zfill(2)
raw["industry"] = raw["hs2"].map(HS2_TO_IND).fillna("manuf")

# Harmonise partner names so they match the FX panel (which uses
# countrycode-standardised names + "Eurozone" aggregation).
EUROZONE = {"Austria","Belgium","France","Germany","Italy","Spain","Netherlands",
            "Finland","Greece","Portugal","Ireland","Luxembourg","Slovakia",
            "Slovenia","Cyprus","Estonia","Latvia","Lithuania","Malta"}
DROP_AGG = {"World","Areas, nes","Other Asia, nes","Other Europe, nes",
            "Other Africa, nes","Special Categories","Free Zones","Bunkers",
            "Br. Antarctic Terr.","Other Asia"}
COMTRADE_RENAMES = {
    "China, Hong Kong SAR": "Hong Kong SAR China",
    "China, Macao SAR":     "Macao SAR China",
    "Rep. of Korea":        "South Korea",
    "USA":                  "United States",
    "Russian Federation":   "Russia",
}
raw["partnerStd"] = raw["partnerDesc"].map(lambda x: COMTRADE_RENAMES.get(x, x))
raw.loc[raw["partnerStd"].isin(EUROZONE), "partnerStd"] = "Eurozone"
raw = raw[~raw["partnerDesc"].isin(DROP_AGG)]
raw = raw.dropna(subset=["partnerStd"])

print(f"Partner harmonisation: {raw['partnerDesc'].nunique()} raw → "
      f"{raw['partnerStd'].nunique()} standardised", flush=True)

base = (raw.groupby(["partnerStd","industry","flow"])["primaryValue"]
            .mean().reset_index()
            .rename(columns={"partnerStd":"partner","primaryValue":"value_usd"}))

# Pivot into imp/exp by partner-industry
imp = base[base.flow=="M"].drop(columns="flow").rename(columns={"value_usd":"imp_usd"})
exp = base[base.flow=="X"].drop(columns="flow").rename(columns={"value_usd":"exp_usd"})
trade_pij = imp.merge(exp, on=["partner","industry"], how="outer").fillna(0)

# Merge with FX shifter Z_dt for each (partner,year)
inst = pd.read_csv(f"{ROOT}/data/clean/instrument/instrument_mun.csv")
# We need Z_dt by partner, but instrument_mun is already aggregated. Need
# the raw FX panel — load instead from the forex CSV.
fx = pd.read_csv(f"{ROOT}/data/clean/forex_2000_2023.csv")
nepal = fx.query("country=='Nepal'")[["year","forex"]].rename(columns={"forex":"npr_usd"})
fx = fx.merge(nepal, on="year", how="left")
fx["fx_to_npr"] = fx["forex"] / fx["npr_usd"]
fx_2001 = fx.query("year==2001")[["country","fx_to_npr"]].rename(columns={"fx_to_npr":"fx_2001"})
fx = fx.merge(fx_2001, on="country", how="left")
fx["fx_index_2001"] = fx["fx_to_npr"] / fx["fx_2001"]

trade_pij_yr = (trade_pij.merge(fx, left_on="partner", right_on="country", how="inner")
                          .assign(imp_x_z = lambda d: d["imp_usd"] * (d["fx_index_2001"]-1),
                                  exp_x_z = lambda d: d["exp_usd"] * (d["fx_index_2001"]-1)))

# Industry-year totals
ind_yr = (trade_pij_yr.groupby(["industry","year"])
          [["imp_x_z","exp_x_z"]].sum().reset_index())

# Apportion to municipality via 2001 employment shares (using 2001 census ind_* * geog_pop_2001)
out = pd.read_csv(f"{ROOT}/data/clean/census/census_outcomes_municipality.csv")
inst_mun = inst[["lgcode","year","geog_pop_2001"]].drop_duplicates("lgcode")
emp = (out.query("year==2001")[["lgcode"] + list(IND_COLS_FOR_LJO.values())]
          .merge(inst_mun, on="lgcode", how="inner"))
for ind, col in IND_COLS_FOR_LJO.items():
    emp[f"L_{ind}_o"] = emp[col].fillna(0) * emp["geog_pop_2001"]
emp_long = emp[["lgcode","geog_pop_2001"] + [f"L_{i}_o" for i in IND_COLS_FOR_LJO]]\
            .melt(id_vars=["lgcode","geog_pop_2001"],
                  var_name="industry", value_name="L_jo")
emp_long["industry"] = emp_long["industry"].str.replace(r"^L_|_o$", "", regex=True)

merged = ind_yr.merge(emp_long, on="industry", how="inner")
# Khanna Eq. (5): apportion industry-destination shocks to munis by the muni's
# share of NATIONAL industry employment (L_jo / L_j), then divide by population.
L_j = emp_long.groupby("industry", as_index=False)["L_jo"].sum().rename(
    columns={"L_jo": "L_j"})
merged = merged.merge(L_j, on="industry", how="left")
merged["emp_share_of_natl"] = np.where(merged["L_j"] > 0,
                                       merged["L_jo"] / merged["L_j"], 0.0)
merged["weight"]    = merged["emp_share_of_natl"] / merged["geog_pop_2001"]
merged["imp_share"] = merged["imp_x_z"] * merged["weight"]
merged["exp_share"] = merged["exp_x_z"] * merged["weight"]

trade_ssiv = (merged.groupby(["lgcode","year"])
                    [["imp_share","exp_share"]].sum().reset_index()
                    .rename(columns={"imp_share":"trade_ssiv_imp",
                                     "exp_share":"trade_ssiv_exp"}))
out_path = f"{ROOT}/data/clean/instrument/trade_ssiv.csv"
trade_ssiv.to_csv(out_path, index=False)
print(f"Wrote {len(trade_ssiv)} muni-year rows to {out_path}")
print(trade_ssiv.describe())
