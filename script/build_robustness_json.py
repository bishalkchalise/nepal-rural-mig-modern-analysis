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
CSV  = ROOT / "output/tab/robustness_final.csv"
OUT  = ROOT / "docs/robustness.json"

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
    ("S0_baseline",   0,  "log", "lin", "anchor: fx × log(mig_int_z); year × mig_int_z"),
    ("S_lag1",        1,  "log", "lin", "FX shifter lagged 1 year"),
    ("S_lag2",        2,  "log", "lin", "FX shifter lagged 2 years"),
    ("S_lag3",        3,  "log", "lin", "FX shifter lagged 3 years"),
    ("S_lag4",        4,  "log", "lin", "FX shifter lagged 4 years"),
    ("S_lag5",        5,  "log", "lin", "FX shifter lagged 5 years"),
    ("S_lag10",       10, "log", "lin", "FX shifter lagged 10 years"),
    ("S_both_log",    0,  "log", "log", "consistent log: year × log(mig_int_z) control"),
    ("S_both_linear", 0,  "lin", "lin", "consistent linear: fx × mig_int_z treatment"),
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

def main():
    if not CSV.exists():
        raise FileNotFoundError(f"{CSV} not found — run script/robustness_final.R first.")
    df = pd.read_csv(CSV)
    results_groups = load_results_groups()

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
