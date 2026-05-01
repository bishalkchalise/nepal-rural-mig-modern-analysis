"""
Build docs/meta.json — variable definitions + summary statistics.

Reads:
  - docs/results.json (outcome list per dataset)
  - data/clean/census/outcomes_census_codebook.csv (census defs)
  - data/clean/rvs_outcomes/agriculture_codebook.csv (HH/plot defs)
  - source CSVs for per-dataset summary stats
"""
import json, pandas as pd, numpy as np
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

# ---- Load outcome lists from results.json ----
res  = json.load(open(ROOT / "docs/results.json"))

# ---- Load codebooks ----
cen_cb_path  = ROOT / "data/clean/census/outcomes_census_codebook.csv"
agri_cb_path = ROOT / "data/clean/rvs_outcomes/agriculture_codebook.csv"
cen_cb  = pd.read_csv(cen_cb_path)  if cen_cb_path.exists()  else pd.DataFrame()
agri_cb = pd.read_csv(agri_cb_path) if agri_cb_path.exists() else pd.DataFrame()
cen_cb_idx  = cen_cb.set_index("variable")  if "variable" in cen_cb.columns  else None
agri_cb_idx = agri_cb.set_index("variable") if "variable" in agri_cb.columns else None

# ---- Load source data for summary stats ----
cen_data  = pd.read_csv(ROOT / "data/clean/census/census_outcomes_municipality.csv")
hh_data   = pd.read_csv(ROOT / "data/clean/rvs_outcomes/agriculture_hh_year.csv")
plot_data = pd.read_csv(ROOT / "data/clean/rvs_outcomes/agriculture_plot_year.csv")

# ---- Build definitions ----
def lookup_def(y, ds_key):
    """Return (label, short, long, universe) for outcome y in dataset ds_key."""
    label = y
    short = y
    long_ = y
    universe = ""

    if ds_key == "census" and cen_cb_idx is not None and y in cen_cb_idx.index:
        row = cen_cb_idx.loc[y]
        if isinstance(row, pd.DataFrame): row = row.iloc[0]
        long_ = str(row["definition"]) if "definition" in row else y
        short = (long_.split(".")[0] + ".") if long_ else y
        universe = str(row["universe"]) if "universe" in row and pd.notna(row.get("universe")) else ""
        if "label" in row and pd.notna(row.get("label")):
            label = str(row["label"])

    elif ds_key in ("hh","plot") and agri_cb_idx is not None and y in agri_cb_idx.index:
        row = agri_cb_idx.loc[y]
        if isinstance(row, pd.DataFrame): row = row.iloc[0]
        long_ = str(row["definition"]) if "definition" in row else y
        short = long_
        if "level" in row and pd.notna(row.get("level")):
            universe = f"{row['level']} ({row['unit']})" if pd.notna(row.get("unit")) else str(row["level"])
        # use variable name as label
        label = y

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
    "census": [2001, 2011, 2021],
    "hh":     [2016, 2017, 2018],
    "plot":   [2016, 2017, 2018],
}
DS_DATA = {"census": cen_data, "hh": hh_data, "plot": plot_data}

for ds_key, ds in res.get("datasets", {}).items():
    years = DS_YEARS.get(ds_key, [])
    src   = DS_DATA.get(ds_key)
    for y, meta in ds["outcomes"].items():
        label, short, long_, universe = lookup_def(y, ds_key)
        definitions[y] = {
            "label":    meta.get("label", label) or label,
            "group":    meta["group"],
            "short":    short,
            "long":     long_,
            "universe": universe,
            "dataset":  ds_key,
        }
        s = per_year_stats(src, y, years) if src is not None else {"years": years}
        s["dataset"] = ds_key
        summary[y] = s

groups = sorted({m["group"] for m in definitions.values()})

out = {"definitions": definitions, "summary": summary, "groups": groups}
out_path = ROOT / "docs/meta.json"
out_path.write_text(json.dumps(out, separators=(",",":")))
print(f"Wrote {len(definitions)} definitions, {len(summary)} summary entries.")
print(f"File: {out_path}")
