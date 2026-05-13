"""
script/export_outcome_map.py

Export the current curated outcome map to a human-editable CSV.

Workflow:
  1. python3 script/export_outcome_map.py
     -> writes output/tab/outcome_map.csv
  2. Open that CSV (Excel, numbers, sheets, whatever) and:
       - Set 'keep' to '' or 'N' to drop a variable from the portal
       - Edit the 'group' column to re-bucket a variable
       - Edit the 'label' column to rename the display string
       - DO NOT change the 'variable' column (that's the join key)
  3. Save the CSV (keep the same column order) and push.
  4. Re-run python3 script/build_robustness_json.py.
     The JSON builder picks up output/tab/outcome_map.csv if it exists
     and ignores the python map.
"""
import pandas as pd
import sys
from pathlib import Path

ROOT = Path(".")
sys.path.insert(0, str(ROOT / "script"))
from _outcome_map import CURATED

OUT = ROOT / "output/tab/outcome_map.csv"
OUT.parent.mkdir(parents=True, exist_ok=True)

rows = []
for ds, m in CURATED.items():
    for var, info in m.items():
        rows.append({
            "dataset":  ds,
            "variable": var,
            "group":    info["group"],
            "label":    info["label"],
            "keep":     "Y",
        })
df = pd.DataFrame(rows, columns=["dataset","variable","group","label","keep"])
# Preserve declaration order from _outcome_map.py.  No sorting — the JSON
# builder uses CSV row order to determine which group appears first in
# the portal dropdown.
df.to_csv(OUT, index=False)
print(f"Wrote {OUT}  ({len(df)} rows, {df['dataset'].nunique()} datasets)")
print("Edit the CSV (drop rows, change group/label, set keep=N to exclude),")
print("save it, then re-run: python3 script/build_robustness_json.py")
