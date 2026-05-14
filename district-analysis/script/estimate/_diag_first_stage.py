"""
Diagnostics for the first-stage construction of fxshock * mig_int.

Questions to resolve:
  Q1.  Should fx be z-scored BEFORE multiplying, or AFTER multiplying?
        (i.e. z(fx) * z(mi)  vs  z( fx * mi ))
  Q2.  Is log of mig_int the right transform?  Mig_int is small (< 0.05)
        so log is always negative -- does that hurt?  Alternatives:
        linear, log(1000*x), asinh, sqrt, percentile-rank.
  Q3.  How skewed are these distributions?  Which transform symmetrises
        best without flipping signs across districts?

Produces:
  output/fig/diag_mig_int_distributions.png   raw + 5 transforms, histograms
  output/fig/diag_zscore_distributions.png    z-scored versions overlaid
  output/fig/diag_interaction_distributions.png  fx * mi distributions
  output/fig/diag_z_order_comparison.png      z(fx)z(mi) vs z(fx mi) scatter
  output/tab/diag_mig_int_summary.csv         summary stats per transform
  output/tab/diag_interaction_summary.csv     summary stats for interactions
"""

import numpy as np, pandas as pd
import matplotlib.pyplot as plt
import os
from scipy import stats

os.makedirs("district-analysis/output/fig", exist_ok=True)
os.makedirs("district-analysis/output/tab", exist_ok=True)

# ---------- Load -----------------------------------------------------------

inst       = pd.read_csv("district-analysis/data/clean/instrument/instrument_forex_dist.csv")
inst_dofe  = pd.read_csv("district-analysis/data/clean/instrument/instrument_dofe_dist.csv")
dofe       = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")
pop_file   = pd.read_csv("district-analysis/data/clean/foreign_migration_district_population.csv")

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

dofe_d = (dofe.groupby(["district_rename","year"]).total_migrants.sum().reset_index()
              .assign(dname=lambda d: d.district_rename.map(to_dname))
              .rename(columns={"total_migrants":"permits"})[["dname","year","permits"]])

# DOFE 2009-10 / pop_2011 intensity
mi_dofe_num = (dofe_d[dofe_d.year.isin([2009,2010])]
               .groupby("dname").permits.mean().rename("num").reset_index())
pop_2011 = (pop_file.assign(dname=lambda d: d.district.map(to_dname))
                    [["dname","district_population_2011"]]
                    .drop_duplicates("dname")
                    .rename(columns={"district_population_2011":"pop_2011"}))
mi_dofe = (mi_dofe_num.merge(pop_2011, on="dname")
                       .assign(mig_int_dofe = lambda d: d.num / d.pop_2011)
                       [["dname","mig_int_dofe"]])

# District-level table (one row per district) for distribution analysis
dist_tbl = (inst[["dname","geog_intensity_2001"]].drop_duplicates("dname")
            .rename(columns={"geog_intensity_2001":"mig_int_2001"})
            .merge(mi_dofe, on="dname", how="left"))

# Panel-level table (district x year) for interaction analysis
panel = (dofe_d
         .merge(inst[["dname","year","fxshock","geog_intensity_2001"]],
                on=["dname","year"])
         .merge(inst_dofe[["dname","year","fxshock_dofe"]],
                on=["dname","year"])
         .merge(mi_dofe, on="dname", how="left"))

print(f"Districts: {len(dist_tbl)}  |  Panel rows: {len(panel)}")

# ---------- Transforms ----------------------------------------------------

def transforms_of(x):
    """Apply 6 candidate transforms to a non-negative variable x."""
    x = pd.Series(x).clip(lower=1e-12)
    return {
        "linear":         x,
        "log":            np.log(x),
        "log(1000*x)":    np.log(1000*x),
        "asinh":          np.arcsinh(x),
        "sqrt":           np.sqrt(x),
        "rank_pct":       x.rank(pct=True),       # 0-1 percentile rank
    }

def zscore(s):
    s = pd.Series(s)
    sd = s.std()
    return (s - s.mean()) / sd if sd > 0 else pd.Series(0., index=s.index)

# ---------- Q2/Q3: mig_int distribution under each transform --------------

mi_var = "mig_int_2001"
mi_raw = dist_tbl[mi_var].dropna()
tx_mi  = transforms_of(mi_raw)

summary_rows = []
for nm, vals in tx_mi.items():
    summary_rows.append({
        "transform": nm,
        "n":           len(vals),
        "mean":        round(vals.mean(), 6),
        "sd":          round(vals.std(), 6),
        "min":         round(vals.min(), 6),
        "median":      round(vals.median(), 6),
        "max":         round(vals.max(), 6),
        "skew":        round(stats.skew(vals.dropna()), 3),
        "kurt":        round(stats.kurtosis(vals.dropna()), 3),
        "n_negative":  int((vals < 0).sum()),
        "n_positive":  int((vals > 0).sum()),
        "iqr":         round(vals.quantile(0.75) - vals.quantile(0.25), 6),
    })
mi_summary = pd.DataFrame(summary_rows)
mi_summary.to_csv("district-analysis/output/tab/diag_mig_int_summary.csv", index=False)

print("\n=== mig_int_2001 transform summary (74 districts) ===")
print(mi_summary.to_string(index=False))

# ---------- Plot mig_int distributions ------------------------------------

fig, axes = plt.subplots(2, 3, figsize=(13, 7))
for ax, (nm, vals) in zip(axes.flatten(), tx_mi.items()):
    ax.hist(vals, bins=20, color="steelblue", edgecolor="black", alpha=0.8)
    ax.axvline(0, color="red", linestyle="--", linewidth=1, alpha=0.6)
    ax.set_title(f"mig_int_2001 ({nm})\n"
                 f"skew={stats.skew(vals):.2f}, neg={int((vals<0).sum())}",
                 fontsize=10)
    ax.set_xlabel(nm); ax.set_ylabel("districts")
fig.suptitle("mig_int_2001 under different transforms (red = 0 line)",
             y=0.995, fontweight="bold")
plt.tight_layout()
plt.savefig("district-analysis/output/fig/diag_mig_int_distributions.png",
            dpi=140, bbox_inches="tight")
plt.close()

# z-scored versions
fig, axes = plt.subplots(2, 3, figsize=(13, 7))
for ax, (nm, vals) in zip(axes.flatten(), tx_mi.items()):
    z = zscore(vals)
    ax.hist(z, bins=20, color="darkorange", edgecolor="black", alpha=0.8)
    ax.axvline(0, color="black", linestyle="--", linewidth=1)
    ax.set_title(f"z({nm}(mi))    skew={stats.skew(z):.2f}", fontsize=10)
    ax.set_xlabel("z-score"); ax.set_ylabel("districts")
fig.suptitle("z-scored mig_int_2001 transforms (all centered at 0 by construction)",
             y=0.995, fontweight="bold")
plt.tight_layout()
plt.savefig("district-analysis/output/fig/diag_zscore_distributions.png",
            dpi=140, bbox_inches="tight")
plt.close()

# ---------- Q1: Interaction order -----------------------------------------
# Compare z(fx) * z(mi) versus z(fx * mi) on the full district-year panel.

p = panel.dropna(subset=["fxshock","geog_intensity_2001","fxshock_dofe","mig_int_dofe"]).copy()

p["fx2001_z"]    = zscore(p["fxshock"])
p["mi2001_z"]    = zscore(p["geog_intensity_2001"])
p["fxdofe_z"]    = zscore(p["fxshock_dofe"])
p["midofe_z"]    = zscore(p["mig_int_dofe"])

# Interaction order A: standardize first, then multiply
p["int_A_2001"] = p.fx2001_z * p.mi2001_z
p["int_A_dofe"] = p.fxdofe_z * p.midofe_z

# Interaction order B: multiply first, then standardize the product
p["int_B_2001"] = zscore(p.fxshock      * p.geog_intensity_2001)
p["int_B_dofe"] = zscore(p.fxshock_dofe * p.mig_int_dofe)

# Also for log transform
p["log_mi2001_z"]   = zscore(np.log(p.geog_intensity_2001.clip(lower=1e-12)))
p["log_midofe_z"]   = zscore(np.log(p.mig_int_dofe.clip(lower=1e-12)))
p["int_A_log2001"]  = p.fx2001_z * p.log_mi2001_z
p["int_A_logdofe"]  = p.fxdofe_z * p.log_midofe_z
p["int_B_log2001"]  = zscore(p.fxshock      * np.log(p.geog_intensity_2001.clip(lower=1e-12)))
p["int_B_logdofe"]  = zscore(p.fxshock_dofe * np.log(p.mig_int_dofe.clip(lower=1e-12)))

inter_summary = []
for label, col in [("linear_2001_A z(fx)*z(mi)",       "int_A_2001"),
                   ("linear_2001_B z(fx*mi)",          "int_B_2001"),
                   ("linear_dofe_A z(fx)*z(mi)",       "int_A_dofe"),
                   ("linear_dofe_B z(fx*mi)",          "int_B_dofe"),
                   ("log_2001_A z(fx)*z(logmi)",       "int_A_log2001"),
                   ("log_2001_B z(fx*logmi)",          "int_B_log2001"),
                   ("log_dofe_A z(fx)*z(logmi)",       "int_A_logdofe"),
                   ("log_dofe_B z(fx*logmi)",          "int_B_logdofe")]:
    v = p[col]
    inter_summary.append({
        "interaction": label,
        "mean":     round(v.mean(), 4),
        "sd":       round(v.std(),  4),
        "min":      round(v.min(),  4),
        "max":      round(v.max(),  4),
        "skew":     round(stats.skew(v.dropna()), 3),
        "n_neg":    int((v<0).sum()),
        "n_pos":    int((v>0).sum()),
    })
inter_df = pd.DataFrame(inter_summary)
inter_df.to_csv("district-analysis/output/tab/diag_interaction_summary.csv", index=False)
print("\n=== Interaction distributions ===")
print(inter_df.to_string(index=False))

# Compute correlations between order-A and order-B for each pair
print("\n=== Correlation: z(fx)*z(mi)  vs  z(fx*mi) ===")
for label, a, b in [("linear 2001",     "int_A_2001",    "int_B_2001"),
                    ("linear DOFE",     "int_A_dofe",    "int_B_dofe"),
                    ("log 2001",        "int_A_log2001", "int_B_log2001"),
                    ("log DOFE",        "int_A_logdofe", "int_B_logdofe")]:
    cor = p[[a, b]].corr().iloc[0,1]
    print(f"  {label:<14}  cor(A, B) = {cor:.4f}")

# ---------- Plot Q1 scatter: order-A vs order-B ---------------------------

fig, axes = plt.subplots(2, 2, figsize=(11, 9))
for ax, (lbl, a, b) in zip(axes.flatten(), [
    ("2001 shares, linear mi",  "int_A_2001",    "int_B_2001"),
    ("DOFE shares, linear mi",  "int_A_dofe",    "int_B_dofe"),
    ("2001 shares, log mi",     "int_A_log2001", "int_B_log2001"),
    ("DOFE shares, log mi",     "int_A_logdofe", "int_B_logdofe"),
]):
    ax.scatter(p[a], p[b], s=4, alpha=0.4)
    lo = min(p[a].min(), p[b].min())
    hi = max(p[a].max(), p[b].max())
    ax.plot([lo,hi], [lo,hi], color="red", linestyle="--", linewidth=1)
    cor = p[[a,b]].corr().iloc[0,1]
    ax.set_xlabel("z(fx)*z(mi)  [order A]")
    ax.set_ylabel("z(fx*mi)     [order B]")
    ax.set_title(f"{lbl}    cor = {cor:.3f}", fontsize=10)
fig.suptitle("Order A (z first, then multiply) vs Order B (multiply, then z)",
             y=0.995, fontweight="bold")
plt.tight_layout()
plt.savefig("district-analysis/output/fig/diag_z_order_comparison.png",
            dpi=140, bbox_inches="tight")
plt.close()

# Distributions of all interaction forms
fig, axes = plt.subplots(2, 4, figsize=(15, 7))
for ax, (lbl, col) in zip(axes.flatten(), [
    ("2001 lin A: z(fx)z(mi)",   "int_A_2001"),
    ("2001 lin B: z(fx*mi)",     "int_B_2001"),
    ("DOFE lin A",               "int_A_dofe"),
    ("DOFE lin B",               "int_B_dofe"),
    ("2001 log A: z(fx)z(logmi)","int_A_log2001"),
    ("2001 log B: z(fx*logmi)",  "int_B_log2001"),
    ("DOFE log A",               "int_A_logdofe"),
    ("DOFE log B",               "int_B_logdofe"),
]):
    v = p[col]
    ax.hist(v, bins=30, color="seagreen", edgecolor="black", alpha=0.8)
    ax.axvline(0, color="red", linestyle="--", linewidth=1)
    ax.set_title(f"{lbl}\nskew={stats.skew(v):.2f}  sd={v.std():.2f}", fontsize=9)
fig.suptitle("Distribution of fxshock × mig_int under each variant",
             y=0.995, fontweight="bold")
plt.tight_layout()
plt.savefig("district-analysis/output/fig/diag_interaction_distributions.png",
            dpi=140, bbox_inches="tight")
plt.close()

print("\nSaved plots:")
print("  output/fig/diag_mig_int_distributions.png")
print("  output/fig/diag_zscore_distributions.png")
print("  output/fig/diag_z_order_comparison.png")
print("  output/fig/diag_interaction_distributions.png")
print("And tables:")
print("  output/tab/diag_mig_int_summary.csv")
print("  output/tab/diag_interaction_summary.csv")
