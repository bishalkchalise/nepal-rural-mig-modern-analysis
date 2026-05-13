"""
script/plot_firm_lag_robustness.py

Generates 6 firm-side lag plots from the local robustness CSVs:

  1. firm_lag_full_vs_cohort        — firm-stock scale outcomes (n_firms, emp,
                                      rev, cap, mean_emp/firm, industry diversity,
                                      share size 1, share size 51+), 3 datasets
                                      overlaid (full 2018, post-2001, post-2011).
  2. firm_entry_lag_industry        — NEC panel: log(new firms) by 7 ISIC
                                      industries + aggregate, single line per
                                      industry.
  3. firm_entry_lag_size            — NEC panel: log(new firms) by 4 size
                                      categories + aggregate, single line per
                                      size with 95% CI bands.
  4. firm_industry_composition_lag  — NEC cs: 8 industry shares, 3 datasets
                                      overlaid (full / post-2001 / post-2011).
  5. firm_formality_credit_lag      — NEC cs: 8 formality / credit / demo
                                      outcomes, full 2018 stock only (cohort
                                      versions not available for these vars).
  6. firm_size_composition_lag      — NEC cs: 4 size shares, 3 datasets
                                      overlaid.

Inputs:
  output/tab/robustness_nec.csv
  output/tab/robustness_nec_cohort.csv

Outputs (PNG + SVG in docs/figs/):
  firm_lag_full_vs_cohort.{png,svg}
  firm_entry_lag_industry.{png,svg}
  firm_entry_lag_size.{png,svg}
  firm_industry_composition_lag.{png,svg}
  firm_formality_credit_lag.{png,svg}
  firm_size_composition_lag.{png,svg}

Run from repo root:
  python3 script/plot_firm_lag_robustness.py
"""
import pandas as pd
import numpy as np
from pathlib import Path
import matplotlib
matplotlib.rcParams.update({
    "font.family": "DejaVu Sans",
    "axes.spines.top": False,
    "axes.spines.right": False,
    "axes.edgecolor": "#1a202c",
    "axes.linewidth": 0.6,
})
import matplotlib.pyplot as plt

ROOT = Path(".")
OUT  = ROOT / "docs/figs"
OUT.mkdir(parents=True, exist_ok=True)

# --------------- Load data ---------------
co = pd.read_csv(ROOT / "output/tab/robustness_nec_cohort.csv")
co = co[co["err"].fillna("") == ""].copy()
nec = pd.read_csv(ROOT / "output/tab/robustness_nec.csv")
nec = nec[nec["err"].fillna("") == ""].copy()

# Anchor selections: scale = log/lin, threshold = 25
SCALE, THR = "log/lin", 25
all3 = pd.concat([
    nec[nec["dataset"] == "nec_cs"].assign(dataset="full 2018 stock"),
    co[co["dataset"]  == "nec_cs_post2001"].assign(dataset="post-2001 cohort"),
    co[co["dataset"]  == "nec_cs_post2011"].assign(dataset="post-2011 cohort"),
], ignore_index=True)
cs_a   = all3[(all3["scale_form"] == SCALE) & (all3["threshold"] == THR)]
panel  = nec[(nec["dataset"] == "nec_panel") & (nec["scale_form"] == SCALE) & (nec["threshold"] == THR)]
cs_full = nec[(nec["dataset"] == "nec_cs")   & (nec["scale_form"] == SCALE) & (nec["threshold"] == THR)]

COL3 = {"full 2018 stock": "#a0aec0",
        "post-2001 cohort": "#2c5282",
        "post-2011 cohort": "#c53030"}

def draw_cohort_panel(ax, oc, label, df):
    for ds in ["full 2018 stock", "post-2001 cohort", "post-2011 cohort"]:
        s = df[(df["dataset"] == ds) & (df["outcome"] == oc)].sort_values("lag")
        if s.empty: continue
        x, b, se = s["lag"].values, s["beta"].values, s["se"].values
        col = COL3[ds]
        ax.plot(x, b, marker="o", markersize=4, lw=1.6, color=col, label=ds)
        ax.fill_between(x, b - 1.96*se, b + 1.96*se, color=col, alpha=0.10)
    ax.axhline(0, color="#1a202c", lw=0.5, ls=":")
    ax.set_title(label, fontsize=10, fontweight="bold")
    ax.set_xlabel("FX shifter lag (years)", fontsize=8.5)
    ax.tick_params(labelsize=8)
    ax.grid(axis="y", lw=0.3, alpha=0.4)

def add_note(fig, text, y=0.02):
    fig.text(0.03, y, text, fontsize=8.5, color="#4a5568",
             va="bottom", ha="left", family="DejaVu Sans", linespacing=1.40)

def save(stem):
    plt.savefig(OUT / f"{stem}.svg", format="svg")
    plt.savefig(OUT / f"{stem}.png", format="png", dpi=150)
    print(f"  saved docs/figs/{stem}.{{svg,png}}")
    plt.close()

# ====================================================================
# Plot 1 — firm-stock scale outcomes (cohort comparison)
# ====================================================================
print("Plot 1: firm-stock scale outcomes ...")
OUT1 = [("log_n_firms",           "log(# firms)"),
        ("log_emp_total",         "log(total employment)"),
        ("log_rev_total",         "log(revenue, Rs)"),
        ("log_cap_total",         "log(capital stock, Rs)"),
        ("mean_emp_per_firm",     "Mean employment per firm"),
        ("industry_diversity",    "Industry diversity (1 − HHI)"),
        ("share_size_1_worker",   "Share: 1-worker firms"),
        ("share_size_51plus_workers","Share: 51+ worker firms")]
fig, axes = plt.subplots(2, 4, figsize=(15, 12), sharex=True)
for ax, (oc, lab) in zip(axes.flatten(), OUT1):
    draw_cohort_panel(ax, oc, lab, cs_a)
for ax in (axes[0,0], axes[1,0]):
    ax.set_ylabel("β  (95% CI)", fontsize=9)
hh, ll = axes[0,0].get_legend_handles_labels()
fig.suptitle("FIRM STOCK — coefficients vs FX-shifter lag, by cohort restriction",
             y=0.985, fontsize=12, fontweight="bold")
fig.legend(hh, ll, loc="upper center", ncol=3, frameon=False,
           bbox_to_anchor=(0.5, 0.955), fontsize=10)
fig.subplots_adjust(top=0.88, bottom=0.18, left=0.05, right=0.98,
                    hspace=0.40, wspace=0.28)
add_note(fig,
"Notes: Each panel plots β (with 95% CI bands) from the anchor robustness spec (treatment = fx_z × log(mig_int_z);\n"
"controls year × mig_int_z + year × fx_z + Block A levels + district FE) on the corresponding 2018 firm-stock outcome\n"
"(y-axis), across FX-shifter lags 0–10 years (x-axis). Sample: 475 munis with ≥25 baseline migrants in 2001. Three datasets\n"
"overlaid: full 2018 stock (all firms in 2018), post-2001 cohort (firms founded 2001–2018, surviving to 2018), and\n"
"post-2011 cohort (firms founded 2011–2018). Close overlap of the three lines indicates the lag-strengthening profile is\n"
"intrinsic to the shock-period cohort, not an artifact of pre-shock incumbents clearing out.")
save("firm_lag_full_vs_cohort")

# ====================================================================
# Plot 2 — Firm entry by industry (NEC panel)
# ====================================================================
print("Plot 2: firm entry by industry ...")
INDUSTRIES = [
    ("log_new_firms_hospitality_food",      "Hospitality & food",     "#c53030"),
    ("log_new_firms_manufacturing",         "Manufacturing",          "#2c5282"),
    ("log_new_firms_construction",          "Construction",           "#9c4221"),
    ("log_new_firms_transport_storage",     "Transport & storage",    "#22543d"),
    ("log_new_firms_trade_retail",          "Trade & retail",         "#744210"),
    ("log_new_firms_agriculture",           "Agriculture",            "#553c9a"),
    ("log_new_firms_finance_prof_realestate","Finance / prof / RE",   "#0987a0"),
    ("log_new_firms",                       "All entry",              "#1a202c"),
]
fig, ax = plt.subplots(1, 1, figsize=(10, 7.6))
for oc, lab, c in INDUSTRIES:
    s = panel[panel["outcome"] == oc].sort_values("lag")
    if s.empty: continue
    lw = 2.2 if oc == "log_new_firms" else 1.2
    ax.plot(s["lag"], s["beta"], marker="o", markersize=3.5, lw=lw,
            color=c, label=lab, alpha=0.95 if oc == "log_new_firms" else 0.7)
ax.axhline(0, color="#1a202c", lw=0.5, ls=":")
ax.set_xlabel("FX shifter lag (years)", fontsize=10)
ax.set_ylabel("β  on log(1 + # new firms)", fontsize=10)
ax.set_title("FIRM ENTRY by industry — annual flow response to lagged FX shock",
             fontsize=12, fontweight="bold")
ax.legend(loc="upper left", fontsize=8.5, frameon=False, ncol=2)
ax.grid(axis="y", lw=0.3, alpha=0.4)
fig.subplots_adjust(top=0.93, bottom=0.30, left=0.10, right=0.97)
add_note(fig,
"Notes: Lines plot β from the anchor spec (treatment = fx_z(t−L) × log(mig_int_z); controls: year × mig_int_z + year × fx_z\n"
"+ Block A × year; muni + year FE; SE clustered at muni) on log(1 + new firms in muni m, founded in year t) across lags 0–10.\n"
"Sample: muni × founding-year cells in NEC panel, k≥25 (8,550 cells). Each line is one ISIC industry; bold black = aggregate.\n"
"Hospitality & food shows the deepest contemporaneous-to-medium-run negative response (lag 2–5, β ≈ −0.04 to −0.06 ***);\n"
"transport, construction, and manufacturing follow a similar but milder pattern; agriculture and finance/RE trend positive\n"
"at mid-lags. Lag 10 estimates are small-sample (only 2011-onwards cohort survives the merge with pre-2008 FX) and noisy.")
save("firm_entry_lag_industry")

# ====================================================================
# Plot 3 — Firm entry by size (NEC panel)
# ====================================================================
print("Plot 3: firm entry by size ...")
SIZES = [
    ("log_new_firms_size_1_worker",      "1 worker",            "#c53030"),
    ("log_new_firms_size_2_9_workers",   "2–9 workers",         "#2c5282"),
    ("log_new_firms_size_10_50_workers", "10–50 workers",       "#22543d"),
    ("log_new_firms_size_51plus_workers","51+ workers",         "#744210"),
    ("log_new_firms",                    "All entry (any size)","#1a202c"),
]
fig, ax = plt.subplots(1, 1, figsize=(10, 7.6))
for oc, lab, c in SIZES:
    s = panel[panel["outcome"] == oc].sort_values("lag")
    if s.empty: continue
    lw = 2.2 if oc == "log_new_firms" else 1.4
    ax.plot(s["lag"], s["beta"], marker="o", markersize=4, lw=lw,
            color=c, label=lab, alpha=0.95 if oc == "log_new_firms" else 0.85)
    if oc != "log_new_firms":
        ax.fill_between(s["lag"], s["beta"] - 1.96*s["se"], s["beta"] + 1.96*s["se"],
                        color=c, alpha=0.08)
ax.axhline(0, color="#1a202c", lw=0.5, ls=":")
ax.set_xlabel("FX shifter lag (years)", fontsize=10)
ax.set_ylabel("β  on log(1 + # new firms)", fontsize=10)
ax.set_title("FIRM ENTRY by size — annual flow response to lagged FX shock",
             fontsize=12, fontweight="bold")
ax.legend(loc="upper left", fontsize=9, frameon=False)
ax.grid(axis="y", lw=0.3, alpha=0.4)
fig.subplots_adjust(top=0.93, bottom=0.28, left=0.10, right=0.97)
add_note(fig,
"Notes: β from anchor spec on log(1 + new firms in muni m, founded year t, in given size category). Sample: NEC panel\n"
"muni × founding-year, k≥25. 95% CI bands shown for the four size lines. Size-1 firms (solo entrants) are roughly flat\n"
"around zero; 2–9 worker firms (small employers) show a mild dip at mid-lags; 10–50 worker entries are too rare for clean\n"
"identification; 51+ worker entries trend slightly positive at lag 2 and lag 10 but never significant. The aggregate\n"
"(black) masks heterogeneity: 1-worker entries respond little, while 2–9 and 10–50 categories are slightly suppressed\n"
"at lag 4–5.")
save("firm_entry_lag_size")

# ====================================================================
# Plot 4 — Industry composition (cohort comparison)
# ====================================================================
print("Plot 4: industry composition (cohort) ...")
COMP = [
    ("share_agriculture",       "Agriculture"),
    ("share_manufacturing",     "Manufacturing"),
    ("share_construction",      "Construction"),
    ("share_trade_retail",      "Trade & retail"),
    ("share_hospitality",       "Hospitality"),
    ("share_transport",         "Transport"),
    ("share_finance_prof_info", "Finance / prof / info"),
    ("share_social_services",   "Social services"),
]
fig, axes = plt.subplots(2, 4, figsize=(15, 12), sharex=True)
for ax, (oc, lab) in zip(axes.flatten(), COMP):
    draw_cohort_panel(ax, oc, lab, cs_a)
for ax in (axes[0,0], axes[1,0]):
    ax.set_ylabel("β on share (pp)", fontsize=9)
hh, ll = axes[0,0].get_legend_handles_labels()
fig.suptitle("INDUSTRY COMPOSITION (2018 stock) — share-of-firms response to lagged FX shock",
             y=0.985, fontsize=12, fontweight="bold")
fig.legend(hh, ll, loc="upper center", ncol=3, frameon=False,
           bbox_to_anchor=(0.5, 0.955), fontsize=10)
fig.subplots_adjust(top=0.88, bottom=0.18, left=0.05, right=0.98,
                    hspace=0.40, wspace=0.28)
add_note(fig,
"Notes: Each panel plots β (with 95% CI bands) from the anchor cross-section spec on the muni-level share of firms in the\n"
"named industry. Three datasets overlaid as in firm-stock plot. Hospitality consistently negative (≈ −1.0pp), Agriculture\n"
"consistently positive (≈ +0.5pp). Manufacturing share roughly flat across lags. Most sector-share responses are stable\n"
"across the lag profile — composition effects are immediate, not cumulative. Cohort restriction produces small upward shift\n"
"for sectors that include public-sector firms (post-shock cohorts have fewer such firms).")
save("firm_industry_composition_lag")

# ====================================================================
# Plot 5 — Formality / credit / demographics (full 2018 stock only)
# ====================================================================
print("Plot 5: formality, credit, demographics ...")
FOR_OUT = [
    ("share_tax_registered",      "Share tax-registered"),
    ("share_keeps_accounts",      "Share keeps accounts"),
    ("share_registered",          "Share registered"),
    ("share_operates_year_round", "Share year-round operation"),
    ("share_borrowed",            "Share borrowed"),
    ("share_uses_formal_credit",  "Uses formal credit"),
    ("share_female_led",          "Share female-led"),
    ("formality_index",           "Formality index"),
]
fig, axes = plt.subplots(2, 4, figsize=(15, 10.5), sharex=True)
for ax, (oc, lab) in zip(axes.flatten(), FOR_OUT):
    s = cs_full[cs_full["outcome"] == oc].sort_values("lag")
    if not s.empty:
        x, b, se = s["lag"].values, s["beta"].values, s["se"].values
        col = "#2c5282"
        ax.plot(x, b, marker="o", markersize=4.5, lw=1.7, color=col)
        ax.fill_between(x, b - 1.96*se, b + 1.96*se, color=col, alpha=0.15)
    ax.axhline(0, color="#1a202c", lw=0.5, ls=":")
    ax.set_title(lab, fontsize=10, fontweight="bold")
    ax.set_xlabel("FX shifter lag (years)", fontsize=8.5)
    ax.tick_params(labelsize=8)
    ax.grid(axis="y", lw=0.3, alpha=0.4)
for ax in (axes[0,0], axes[1,0]):
    ax.set_ylabel("β  (95% CI)", fontsize=9)
fig.suptitle("FIRM FORMALITY, CREDIT, DEMOGRAPHICS (2018 full stock) — response to lagged FX shock",
             y=0.975, fontsize=12, fontweight="bold")
fig.subplots_adjust(top=0.92, bottom=0.18, left=0.05, right=0.98,
                    hspace=0.40, wspace=0.28)
add_note(fig,
"Notes: Each panel plots β (with 95% CI band) from the anchor cross-section spec on a 2018 firm-composition outcome (full\n"
"stock only — cohort-restricted versions of these vars are not available without firm-level microdata). Tax registration\n"
"drops with shock (≈ −3pp) consistently across lags; accounts and registration also negative but smaller. Female-led firm\n"
"share rises steadily (≈ +1pp). Formal credit usage barely moves. Sample: full 2018 stock at k≥25.")
save("firm_formality_credit_lag")

# ====================================================================
# Plot 6 — Firm size composition (2018 stock, cohort comparison)
# ====================================================================
print("Plot 6: firm size composition (cohort) ...")
SIZE_OUT = [
    ("share_size_1_worker",      "Share: 1 worker"),
    ("share_size_2_9_workers",   "Share: 2–9 workers"),
    ("share_size_10_50_workers", "Share: 10–50 workers"),
    ("share_size_51plus_workers","Share: 51+ workers"),
]
fig, axes = plt.subplots(1, 4, figsize=(16, 7.5), sharex=True)
for ax, (oc, lab) in zip(axes, SIZE_OUT):
    draw_cohort_panel(ax, oc, lab, cs_a)
axes[0].set_ylabel("β on share of firms (pp scale)", fontsize=9)
hh, ll = axes[0].get_legend_handles_labels()
fig.suptitle("FIRM SIZE COMPOSITION (2018 stock) — share-of-firms response to lagged FX shock, by cohort",
             y=0.97, fontsize=12, fontweight="bold")
fig.legend(hh, ll, loc="upper center", ncol=3, frameon=False,
           bbox_to_anchor=(0.5, 0.91), fontsize=10)
fig.subplots_adjust(top=0.83, bottom=0.34, left=0.06, right=0.98, wspace=0.30)
add_note(fig,
"Notes: β on muni-level share of 2018 firms in each size bucket (1 / 2–9 / 10–50 / 51+ workers). Anchor cross-section spec.\n"
"Three datasets: full 2018 stock; post-2001 cohort; post-2011 cohort. Shifts in size composition are mostly stable across\n"
"lags (composition effects are immediate, not cumulative). Cohort restriction lifts the 51+ worker share estimate above\n"
"zero — i.e., the shock-period cohort is slightly more represented in the 51+ category than the full 2018 stock implies —\n"
"but the effect remains tiny (≤ +0.1pp). The headline composition shift is the +1.5pp positive on 1-worker firms balanced\n"
"by a ≈ −1.5pp negative on 2–9 workers: shocks shift entry toward solo proprietors.")
save("firm_size_composition_lag")

print("\nAll 6 plots saved to docs/figs/.")
