"""
Build docs/meta.json — variable definitions + summary statistics for the
test pages (results_test.json).

Reads:
  - docs/results_test.json (outcome list per dataset)
  - data/clean/census/outcomes_census_codebook.csv (census defs)
  - data/clean/rvs_outcomes/*_codebook.csv (RVS defs)
  - data/clean/nec2018/compact_outputs_codebook.csv (NEC defs)
  - source CSVs for per-dataset summary stats
"""
import json, pandas as pd, numpy as np
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

# ---- Load outcome list from results_test.json (source of truth) ----
res_path = ROOT / "docs/results_test.json"
if not res_path.exists():
    res_path = ROOT / "docs/results.json"
res = json.load(open(res_path))
print(f"Reading outcome list from {res_path.name}")

# ---- Load codebooks ----
def _load(p):
    return pd.read_csv(p) if Path(p).exists() else pd.DataFrame()

cen_cb  = _load(ROOT / "data/clean/census/outcomes_census_codebook.csv")
nec_cb  = _load(ROOT / "data/clean/nec2018/compact_outputs_codebook.csv")
rvs_cbs = []   # combine all RVS codebooks
for f in ["agriculture_codebook","consumption_codebook","enterprise_codebook",
          "health_education_codebook","migration_hh_year_codebook","migration_migrant_codebook",
          "shocks_coping_codebook","social_protection_codebook"]:
    p = ROOT / "data/clean/rvs_outcomes" / f"{f}.csv"
    if p.exists():
        df = pd.read_csv(p)
        df["_source"] = f
        rvs_cbs.append(df)
rvs_cb = pd.concat(rvs_cbs, ignore_index=True) if rvs_cbs else pd.DataFrame()

cen_cb_idx = cen_cb.set_index("variable")  if "variable" in cen_cb.columns else None
rvs_cb_idx = rvs_cb.set_index("variable")  if "variable" in rvs_cb.columns else None
nec_cb_idx = nec_cb.set_index("variable")  if "variable" in nec_cb.columns else None

# ---- Load source data for summary stats ----
def _safe_read(p):
    try: return pd.read_csv(p)
    except Exception: return None

cen_data  = _safe_read(ROOT / "data/clean/census/census_outcomes_municipality.csv")
# HH master = merge of all HH-year files on (hhid, year), brings vmun_code from agri
def _build_hh_master():
    base = ROOT / "data/clean/rvs_outcomes"
    agri = _safe_read(base / "agriculture_hh_year.csv")
    if agri is None: return None
    if "vmun_code" in agri.columns:
        agri = agri.rename(columns={"vmun_code":"lgcode"})
    master = agri.copy()
    for f in ["consumption_hh_year","education_hh_year","enterprise_hh_year",
              "health_hh_year","social_protection_hh_year",
              "shocks_coping_shocked_hh_year","migration_hh_year_migrant_only"]:
        df = _safe_read(base / f"{f}.csv")
        if df is None: continue
        skip = {"hhid","year","wt_hh","psu","vdc","vmun_code","lgname",
                "district77","district_name","s00q03a","s00q03b","s00q03c","district","member_id"}
        keep_cols = ["hhid","year"] + [c for c in df.columns if c not in skip and c not in master.columns]
        master = master.merge(df[keep_cols].drop_duplicates(["hhid","year"]),
                              on=["hhid","year"], how="left")
    return master
hh_data   = _build_hh_master()
nec_panel = _safe_read(ROOT / "data/clean/nec2018/mun_entry_panel_new.csv")
# NEC cross-section: merge of 3 compact files
def _build_nec_cs():
    base = ROOT / "data/clean/nec2018"
    parts = [_safe_read(base / f"{f}.csv") for f in
             ["mun_industry_structure","mun_productivity_profitability","mun_size_formality"]]
    parts = [p for p in parts if p is not None]
    if not parts: return None
    m = parts[0]
    for p in parts[1:]:
        new_cols = [c for c in p.columns if c == "lgcode" or c not in m.columns]
        m = m.merge(p[new_cols], on="lgcode", how="outer")
    return m
nec_cs_data = _build_nec_cs()

# ---- Build definitions ----
def _row_text(idx, y, def_col="definition"):
    """Helper: pick the matching row for variable y from a codebook indexed by 'variable'.
    Returns (definition_text, universe_text) or ("","")."""
    if idx is None or y not in idx.index: return "", ""
    row = idx.loc[y]
    if isinstance(row, pd.DataFrame): row = row.iloc[0]
    defn = str(row[def_col]) if def_col in row.index and pd.notna(row.get(def_col)) else ""
    universe = ""
    if "level" in row.index and pd.notna(row.get("level")):
        unit = row.get("unit", "")
        universe = f"{row['level']} ({unit})" if pd.notna(unit) else str(row["level"])
    elif "universe" in row.index and pd.notna(row.get("universe")):
        universe = str(row["universe"])
    elif "unit" in row.index and pd.notna(row.get("unit")):
        universe = str(row["unit"])
    return defn, universe


def lookup_def(y, ds_key, fallback_label=None):
    """Return (label, short, long, universe) for outcome y in dataset ds_key.
    Falls back to fallback_label (curated label from results_test.json outcomes)
    when the codebook has no row.  long is the codebook definition; short is the
    first sentence (for tooltip)."""
    long_, universe = "", ""
    if ds_key == "census":
        long_, universe = _row_text(cen_cb_idx, y)
    elif ds_key in ("hh", "plot"):
        long_, universe = _row_text(rvs_cb_idx, y)
    elif ds_key in ("nec_panel", "nec_cs"):
        long_, universe = _row_text(nec_cb_idx, y, def_col="description")

    if not long_:
        long_ = fallback_label or y
    short = long_.split(".")[0] + "." if "." in long_ else long_
    label = fallback_label or y
    return label, short, long_, universe


def stats_block(series):
    s = series.dropna()
    return {
        "n":    int(len(s)),
        "mean": float(s.mean())   if len(s) > 0 else None,
        "sd":   float(s.std(ddof=1)) if len(s) > 1 else None,
        "min":  float(s.min())    if len(s) > 0 else None,
        "max":  float(s.max())    if len(s) > 0 else None,
        "median": float(s.median()) if len(s) > 0 else None,
    }


def per_year_stats(df, var, years):
    out = {"years": list(years), "dataset": None}
    if var not in df.columns:
        for y in years:
            out.update({f"n_{y}":0, f"mean_{y}":None, f"sd_{y}":None,
                        f"min_{y}":None, f"max_{y}":None})
        return out
    for y in years:
        d = df.query("year == @y")[var]
        b = stats_block(d)
        out[f"n_{y}"]    = b["n"]
        out[f"mean_{y}"] = b["mean"]
        out[f"sd_{y}"]   = b["sd"]
        out[f"min_{y}"]  = b["min"]
        out[f"max_{y}"]  = b["max"]
    return out


# ---- Iterate over outcomes from results.json ----
definitions = {}
summary = {}

DS_YEARS = {
    "census":    [2001, 2011, 2021],
    "hh":        [2016, 2017, 2018],
    "plot":      [2016, 2017, 2018],
    "nec_panel": [2001, 2010, 2018],   # representative subset
    "nec_cs":    [2018],
}
DS_DATA = {"census": cen_data, "hh": hh_data,
           "nec_panel": nec_panel, "nec_cs": nec_cs_data}

for ds_key, ds in res.get("datasets", {}).items():
    years = DS_YEARS.get(ds_key, [])
    src   = DS_DATA.get(ds_key)
    for y, meta in ds["outcomes"].items():
        # Pass the curated label from results_test.json as fallback
        label, short, long_, universe = lookup_def(y, ds_key,
                                                   fallback_label=meta.get("label"))
        definitions[y] = {
            "label":    meta.get("label") or label,
            "group":    meta["group"],
            "short":    short,
            "long":     long_,
            "universe": universe,
            "dataset":  ds_key,
        }
        # Summary stats: cross-section has no "year" column for the outcome
        if src is not None and "year" in (src.columns if hasattr(src, "columns") else []):
            s = per_year_stats(src, y, years)
        elif src is not None and ds_key == "nec_cs":
            # NEC cross-section: single year (2018)
            s = {"years": years, "dataset": ds_key}
            if y in src.columns:
                s_block = stats_block(src[y])
                s.update({f"n_{years[0]}": s_block["n"], f"mean_{years[0]}": s_block["mean"],
                          f"sd_{years[0]}": s_block["sd"], f"min_{years[0]}": s_block["min"],
                          f"max_{years[0]}": s_block["max"]})
            else:
                s.update({f"n_{years[0]}": 0, f"mean_{years[0]}": None, f"sd_{years[0]}": None,
                          f"min_{years[0]}": None, f"max_{years[0]}": None})
        else:
            s = {"years": years, "dataset": ds_key}
        s["dataset"] = ds_key
        summary[y] = s

groups = sorted({m["group"] for m in definitions.values()})

out = {"definitions": definitions, "summary": summary, "groups": groups}
out_path = ROOT / "docs/meta.json"
out_path.write_text(json.dumps(out, separators=(",",":")))
print(f"Wrote {len(definitions)} definitions, {len(summary)} summary entries.")
print(f"File: {out_path}")
