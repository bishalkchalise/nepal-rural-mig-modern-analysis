"""
Convert output/tab/robustness_final.csv -> docs/robustness.json with the
same nested shape used by docs/results.json (consumed by explorer.html),
so docs/robustness.html can reuse the same render pipeline.

Output shape:
{
  "datasets_meta": { ds: {label}, ... },
  "thresholds":    { "0": label, ... },
  "families":      { "robustness": { "specs": { S0_baseline: "...", ... } } },
  "spec_meta":     { spec_name: {lag, treatment_form, c_mig_form, desc}, ... },
  "datasets": {
    ds: {
      label, entity, year, ref_year, cluster,
      groups:  [outcome_group, ...],          # unique sorted
      outcomes: { var: {label, group} },      # label = var name (CSV doesn't carry a nicer label)
      estimates: {
        threshold: {
          "robustness": {
            spec: { var: {beta, se, pval, n, n_muni, mean_y, sd_y, interpret} }
          }
        }
      }
    }
  }
}
"""
import pandas as pd, json
from pathlib import Path

ROOT = Path(".")
CSV       = ROOT / "output/tab/robustness_final.csv"
CSV_FILL  = ROOT / "output/tab/robustness_final_fill.csv"   # optional add-on
OUT       = ROOT / "docs/robustness.json"

DATASET_META = {
    "census":    {"label": "Census municipality panel (2001, 2011, 2021)",
                  "entity": "lgcode", "ref_year": 2001, "cluster": "lgcode"},
    "hh":        {"label": "HRVS household panel (2016, 2017, 2018)",
                  "entity": "hhid",   "ref_year": 2016, "cluster": "lgcode"},
    "nec_panel": {"label": "NEC firm-entry panel (annual 2001–2018)",
                  "entity": "lgcode", "ref_year": 2001, "cluster": "lgcode"},
    "nec_cs":    {"label": "NEC 2018 firm cross-section",
                  "entity": "DIST",   "ref_year": 2018, "cluster": "DIST"},
}
THRESHOLD_LABELS = {
    "0":   "All munis",
    "25":  "≥25 migrants in 2001",
    "50":  "≥50 migrants in 2001",
    "100": "≥100 migrants in 2001",
}
# Spec catalogue with lag + scale axes
SPECS = [
    ("S0_baseline",        0,  "log", "lin", "anchor: fx × log(mig_int_z); year × mig_int_z"),
    ("S_lag1",             1,  "log", "lin", "FX shifter lagged 1 year (log/lin)"),
    ("S_lag2",             2,  "log", "lin", "FX shifter lagged 2 years (log/lin)"),
    ("S_lag3",             3,  "log", "lin", "FX shifter lagged 3 years (log/lin)"),
    ("S_lag4",             4,  "log", "lin", "FX shifter lagged 4 years (log/lin)"),
    ("S_lag5",             5,  "log", "lin", "FX shifter lagged 5 years (log/lin)"),
    ("S_lag10",            10, "log", "lin", "FX shifter lagged 10 years (log/lin)"),
    ("S_both_log",         0,  "log", "log", "log/log at lag 0"),
    ("S_both_log_lag1",    1,  "log", "log", "log/log at lag 1y"),
    ("S_both_log_lag2",    2,  "log", "log", "log/log at lag 2y"),
    ("S_both_log_lag3",    3,  "log", "log", "log/log at lag 3y"),
    ("S_both_log_lag4",    4,  "log", "log", "log/log at lag 4y"),
    ("S_both_log_lag5",    5,  "log", "log", "log/log at lag 5y"),
    ("S_both_log_lag10",   10, "log", "log", "log/log at lag 10y"),
    ("S_both_linear",      0,  "lin", "lin", "lin/lin at lag 0"),
    ("S_both_linear_lag1", 1,  "lin", "lin", "lin/lin at lag 1y"),
    ("S_both_linear_lag2", 2,  "lin", "lin", "lin/lin at lag 2y"),
    ("S_both_linear_lag3", 3,  "lin", "lin", "lin/lin at lag 3y"),
    ("S_both_linear_lag4", 4,  "lin", "lin", "lin/lin at lag 4y"),
    ("S_both_linear_lag5", 5,  "lin", "lin", "lin/lin at lag 5y"),
    ("S_both_linear_lag10",10, "lin", "lin", "lin/lin at lag 10y"),
]
SPEC_LABEL = {s[0]: s[4] for s in SPECS}
SPEC_META  = {s[0]: {"lag": s[1], "treatment_form": s[2], "c_mig_form": s[3], "desc": s[4]}
              for s in SPECS}

def load_results_groups():
    """Load the same outcome -> group mapping that explorer.html uses,
    so robustness groups line up with Results (separate 'Amenities' and
    'Assets', etc.) rather than the lumped 'Assets / amenities' label
    from the CSV's outcome_group column."""
    p = ROOT / "docs/results.json"
    if not p.exists(): return {}
    j = json.loads(p.read_text())
    mapping = {}  # (dataset, outcome) -> group
    for ds, dd in j.get("datasets", {}).items():
        for oc, info in dd.get("outcomes", {}).items():
            g = info.get("group")
            lbl = info.get("label")
            if g:
                mapping[(ds, oc)] = {"group": g, "label": lbl or oc}
    return mapping


def compute_n_unit_hh():
    """For HH dataset, count unique HHs in the regression sample
    (threshold-filtered, non-NA on outcome).  Iterate over each HRVS
    file separately to avoid messy merges — we only need
    (hhid, year, lgcode, outcome) per file.

    Returns dict: (threshold, outcome) -> n_unit (int).
    """
    import numpy as np
    base = ROOT / "data/clean/rvs_outcomes"
    inst_p = ROOT / "data/clean/instrument/instrument_mun.csv"
    if not inst_p.exists():
        return {}
    inst = pd.read_csv(inst_p, usecols=["lgcode","year","total_migrants"])

    SKIP = {"hhid","year","lgcode","vmun_code","wt_hh","psu","vdc","lgname",
            "district","district77","district_name","s00q03a","s00q03b","s00q03c",
            "member_id","fxshock","mig_intensity","log_mig_intensity",
            "total_migrants","fx_z","mig_int_z","log_migint_z"}

    out = {}
    files = ["agriculture_hh_year","consumption_hh_year","education_hh_year",
             "enterprise_hh_year","health_hh_year","social_protection_hh_year",
             "shocks_coping_shocked_hh_year","migration_hh_year_migrant_only"]

    # Build (hhid, year) -> lgcode lookup from any file that has lgcode
    hh_geo = []
    for f in files:
        p = base / (f + ".csv")
        if not p.exists(): continue
        df = pd.read_csv(p, usecols=lambda c: c in ("hhid","year","lgcode","vmun_code"))
        if "vmun_code" in df.columns and "lgcode" not in df.columns:
            df = df.rename(columns={"vmun_code": "lgcode"})
        if {"hhid","year","lgcode"}.issubset(df.columns):
            hh_geo.append(df[["hhid","year","lgcode"]].drop_duplicates(["hhid","year"]))
    if not hh_geo:
        return {}
    geo = pd.concat(hh_geo, ignore_index=True).drop_duplicates(["hhid","year"])

    for f in files:
        p = base / (f + ".csv")
        if not p.exists(): continue
        df = pd.read_csv(p)
        if "vmun_code" in df.columns and "lgcode" not in df.columns:
            df = df.rename(columns={"vmun_code": "lgcode"})
        if "hhid" not in df.columns or "year" not in df.columns:
            continue
        # ensure lgcode column via the geo lookup
        if "lgcode" not in df.columns:
            df = df.merge(geo, on=["hhid","year"], how="left")
        df = df.dropna(subset=["lgcode"])
        df["lgcode"] = df["lgcode"].astype(int)
        # merge in total_migrants
        df = df.merge(inst, on=["lgcode","year"], how="inner")
        outcomes = [c for c in df.columns if c not in SKIP]
        # also include derived log_ versions for migrant/remit
        log_derive = {}
        for v in ("n_migrants_total","n_migrants_international","n_migrants_male",
                  "n_migrants_female","remittance_amt","remit_amount_12m_rs",
                  "remit_amount_intl_12m_rs"):
            if v in df.columns:
                df["log_"+v] = np.log1p(pd.to_numeric(df[v], errors="coerce"))
                log_derive["log_"+v] = "log of "+v
                if ("log_"+v) not in outcomes:
                    outcomes.append("log_"+v)
        for thr in (0, 25, 50, 100):
            sub = df[df["total_migrants"] >= thr]
            for oc in outcomes:
                v = pd.to_numeric(sub[oc], errors="coerce")
                mask = v.notna()
                if mask.sum() < 50: continue
                # only set if not already set (HH master uses first file's record per outcome)
                if (thr, oc) not in out:
                    out[(thr, oc)] = int(sub.loc[mask, "hhid"].nunique())
    return out


def main():
    if not CSV.exists():
        raise FileNotFoundError(f"{CSV} not found — run script/robustness_final.R first.")
    df = pd.read_csv(CSV)
    if CSV_FILL.exists():
        df_fill = pd.read_csv(CSV_FILL)
        df = pd.concat([df, df_fill], ignore_index=True)
        print(f"  + merged {len(df_fill):,} fill rows from {CSV_FILL.name}")
    results_groups = load_results_groups()
    print("  Computing n_unit (unique HHs) for HH dataset…")
    hh_nunit = compute_n_unit_hh()
    print(f"    -> {len(hh_nunit)} (threshold, outcome) entries")

    # Numeric coercion
    for col in ["beta","se","pval","mean_y","sd_y","n","n_muni"]:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")
    df["threshold"] = df["threshold"].astype(int)

    # Drop rows with non-empty err (cell didn't produce an estimate)
    df["err"] = df["err"].fillna("")
    # We keep error rows but flag them; explorer-style page renders 'err' if no beta
    out = {
        "datasets_meta": {},
        "thresholds":    THRESHOLD_LABELS,
        "families":      {"robustness": {"specs": SPEC_LABEL}},
        "spec_meta":     SPEC_META,
        "datasets":      {},
    }
    for ds, meta in DATASET_META.items():
        sub = df[df["dataset"] == ds]
        if sub.empty:
            continue
        out["datasets_meta"][ds] = {"label": meta["label"]}
        # Outcomes: prefer the results.json (dataset, outcome) -> group
        # mapping so labels line up with the Results page (Amenities,
        # Assets, etc. instead of lumped 'Assets / amenities').
        outcomes = {}
        for v, g_csv in (sub[["outcome","outcome_group"]]
                            .drop_duplicates()
                            .itertuples(index=False)):
            override = results_groups.get((ds, v))
            if override:
                outcomes[v] = {"label": override["label"], "group": override["group"]}
            else:
                outcomes[v] = {"label": v, "group": g_csv}
        # Groups: unique sorted (over the resolved group labels)
        groups = sorted({o["group"] for o in outcomes.values() if o.get("group")})
        # Estimates nested
        est = {}
        for thr, df_thr in sub.groupby("threshold"):
            est_thr = {"robustness": {}}
            for spec, df_spec in df_thr.groupby("spec"):
                cells = {}
                for _, r in df_spec.iterrows():
                    rec = {}
                    if r["err"]:
                        rec["err"] = r["err"]
                    else:
                        if pd.notna(r["beta"]): rec["beta"] = float(r["beta"])
                        if pd.notna(r["se"]):   rec["se"]   = float(r["se"])
                        if pd.notna(r["pval"]): rec["pval"] = float(r["pval"])
                        if pd.notna(r["mean_y"]):rec["mean_y"]=float(r["mean_y"])
                        if pd.notna(r["sd_y"]): rec["sd_y"] = float(r["sd_y"])
                        if pd.notna(r["n"]):    rec["n"]    = int(r["n"])
                        if pd.notna(r["n_muni"]):rec["n_muni"]=int(r["n_muni"])
                        # n_unit: census/nec_panel/nec_cs use n_muni;
                        # HH gets the precomputed unique-HH count.
                        if ds == "hh":
                            nu = hh_nunit.get((int(r["threshold"]), r["outcome"]))
                            if nu is not None: rec["n_unit"] = nu
                        else:
                            if pd.notna(r["n_muni"]):
                                rec["n_unit"] = int(r["n_muni"])
                        if isinstance(r.get("interpret"), str) and r["interpret"]:
                            rec["interpret"] = r["interpret"]
                    cells[r["outcome"]] = rec
                est_thr["robustness"][spec] = cells
            est[str(int(thr))] = est_thr
        out["datasets"][ds] = {
            "label":    meta["label"],
            "entity":   meta["entity"],
            "ref_year": meta["ref_year"],
            "cluster":  meta["cluster"],
            "groups":   groups,
            "outcomes": outcomes,
            "estimates": est,
        }

    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(out, separators=(",", ":")))
    sz_kb = OUT.stat().st_size / 1024
    print(f"Wrote {OUT}  ({sz_kb:.0f} KB)")
    for ds, dsd in out["datasets"].items():
        print(f"  {ds}: {len(dsd['outcomes'])} outcomes, "
              f"{len(dsd['groups'])} groups, "
              f"{len(dsd['estimates'])} thresholds")

if __name__ == "__main__":
    main()
