"""
script/apply_outcome_edits_2026_05_13.py

One-off edit script that applies the curation instructions captured on
2026-05-13.  Edits output/tab/outcome_map.csv in place:

  REMOVE individual vars:
    hh_death_12m, edu_attain_primary_plus, edu_attain_secondary_plus,
    edu_attain_higher_secondary_plus, edu_attain_tertiary,
    edu_years_mean, amen_assets_none, work_lfp, gap_nonagri_m_minus_f.

  REMOVE whole groups:
    census: Left-behind children, HH demographics, Child mortality,
            Female autonomy, Fertility
    hh:     Social protection, Coping, Shocks, Cropping, Remittance use,
            Land area, Livestock, Irrigation

  KEEP & MERGE:
    HH 'Health' / 'Education' / 'Remittances' / 'Migrants' / 'Migration costs':
      -> retain only 7 specific vars under a single group
         'Migrants & remittances' (migrant + remit + cost rows)
         and a single group 'Health & education spending'
         (hlt_spend_total + edu_spend_total_12m).
    NEC panel:
      -> merge 'Firm entry — total' into 'Firm entry — by size' (rename
         to 'Firm entry — total + by size') so log_new_firms sits with
         the size rows.

After running this, re-run python3 script/build_robustness_json.py.
"""
import pandas as pd
from pathlib import Path

CSV = Path("output/tab/outcome_map.csv")
df = pd.read_csv(CSV)

# ---------------- 1. Drop individual variables ----------------
DROP_VARS = {
    "hh_death_12m",
    "edu_attain_primary_plus",
    "edu_attain_secondary_plus",
    "edu_attain_higher_secondary_plus",
    "edu_attain_tertiary",
    "edu_years_mean",
    "amen_assets_none",
    "work_lfp",
    "gap_nonagri_m_minus_f",
}
df.loc[df["variable"].isin(DROP_VARS), "keep"] = "N"

# ---------------- 2. Drop whole groups ----------------
DROP_GROUPS = {
    "census": {"Left-behind children", "HH demographics",
               "Child mortality", "Female autonomy", "Fertility"},
    "hh":     {"Social protection", "Coping", "Shocks",
               "Cropping", "Remittance use",
               "Land area", "Livestock", "Irrigation"},
}
for ds, groups in DROP_GROUPS.items():
    mask = (df["dataset"] == ds) & (df["group"].isin(groups))
    df.loc[mask, "keep"] = "N"

# ---------------- 3. HH: keep only the chosen vars across the merged groups ----------------
# Migrants & remittances (combined)
KEEP_HH_MIGRREMIT = {
    "has_migrant_international":     "HH with int'l migrant",
    "log_n_migrants_international":  "log(# int'l migrants)",
    "remit_received":                "Any remittance received",
    "remit_amount_intl_12m_rs":      "Intl remit, Rs (12m)",
    "log_remit_amount_intl_12m_rs":  "log(intl remit, Rs)",
    "mig_cost_12m_rs":               "Mig cost (12m, Rs)",
    "cost_per_migrant":              "Cost per migrant (Rs)",
}
GROUP_HH_MIGRREMIT = "Migrants & remittances"
# Drop everything in the old Migrants / Remittances / Migration costs / Remittance use
# groups EXCEPT the 7 keepers above.
OLD_MR_GROUPS = {"Migrants", "Remittances", "Migration costs", "Remittance use"}
mask = (df["dataset"] == "hh") & (df["group"].isin(OLD_MR_GROUPS))
df.loc[mask, "keep"] = "N"
# Re-enable + retag the keepers
for v, lab in KEEP_HH_MIGRREMIT.items():
    sel = (df["dataset"] == "hh") & (df["variable"] == v)
    df.loc[sel, ["keep","group","label"]] = ["Y", GROUP_HH_MIGRREMIT, lab]

# Health & education spending (2 vars combined)
KEEP_HH_HEALTHEDU = {
    "hlt_spend_total":      "Health spending total (Rs)",
    "edu_spend_total_12m":  "Total education spend (12m)",
}
GROUP_HH_HEALTHEDU = "Health & education spending"
# Drop the rest of the old Health and Education groups (HH)
mask = (df["dataset"] == "hh") & (df["group"].isin({"Health", "Education"}))
df.loc[mask, "keep"] = "N"
for v, lab in KEEP_HH_HEALTHEDU.items():
    sel = (df["dataset"] == "hh") & (df["variable"] == v)
    df.loc[sel, ["keep","group","label"]] = ["Y", GROUP_HH_HEALTHEDU, lab]

# ---------------- 4. NEC panel: merge total into size group ----------------
sel = (df["dataset"] == "nec_panel") & (df["variable"] == "log_new_firms")
df.loc[sel, "group"] = "Firm entry — total + by size"
# Rename the existing size group entries to the same merged name
sel = (df["dataset"] == "nec_panel") & (df["group"] == "Firm entry — by size")
df.loc[sel, "group"] = "Firm entry — total + by size"

# ---------------- 5. Save ----------------
df.to_csv(CSV, index=False)
kept = df["keep"].astype(str).str.upper().isin({"Y","YES","TRUE","1"})
print(f"Edited {CSV}")
print(f"  rows kept: {kept.sum()} / {len(df)}")
print(f"  groups by dataset (kept only):")
for ds, sub in df[kept].groupby("dataset"):
    gs = sub["group"].drop_duplicates().tolist()
    print(f"    {ds:9s} ({len(sub)} vars):")
    for g in gs:
        n = (sub["group"]==g).sum()
        print(f"      [{n:2d}] {g}")
