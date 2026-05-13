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

# ====================================================================
# Plot 7 — Firm ENTRY (flow) by industry, multi-panel with CI bands
# ====================================================================
print("Plot 7: firm entry by industry (multi-panel) ...")
ENT_IND = [
    ("log_new_firms",                        "All entry (aggregate)"),
    ("log_new_firms_hospitality_food",       "Hospitality & food"),
    ("log_new_firms_manufacturing",          "Manufacturing"),
    ("log_new_firms_construction",           "Construction"),
    ("log_new_firms_transport_storage",      "Transport & storage"),
    ("log_new_firms_trade_retail",           "Trade & retail"),
    ("log_new_firms_agriculture",            "Agriculture"),
    ("log_new_firms_finance_prof_realestate","Finance / prof / RE"),
]
fig, axes = plt.subplots(2, 4, figsize=(15, 12), sharex=True)
for ax, (oc, lab) in zip(axes.flatten(), ENT_IND):
    s = panel[panel["outcome"] == oc].sort_values("lag")
    if not s.empty:
        x, b, se = s["lag"].values, s["beta"].values, s["se"].values
        col = "#1a202c" if oc == "log_new_firms" else "#2c5282"
        ax.plot(x, b, marker="o", markersize=4.5, lw=1.8, color=col, label="β (95% CI)")
        ax.fill_between(x, b - 1.96*se, b + 1.96*se, color=col, alpha=0.15)
    ax.axhline(0, color="#1a202c", lw=0.5, ls=":")
    ax.set_title(lab, fontsize=10, fontweight="bold")
    ax.set_xlabel("FX shifter lag (years)", fontsize=8.5)
    ax.tick_params(labelsize=8)
    ax.grid(axis="y", lw=0.3, alpha=0.4)
for ax in (axes[0,0], axes[1,0]):
    ax.set_ylabel("β  on log(1 + # new firms)", fontsize=9)
hh, ll = axes[0,0].get_legend_handles_labels()
fig.suptitle("FIRM ENTRY (flow) by industry — annual log(# new firms) response to lagged FX shock",
             y=0.985, fontsize=12, fontweight="bold")
fig.legend(hh, ll, loc="upper center", ncol=1, frameon=False,
           bbox_to_anchor=(0.5, 0.955), fontsize=10)
fig.subplots_adjust(top=0.88, bottom=0.18, left=0.05, right=0.98,
                    hspace=0.40, wspace=0.28)
add_note(fig,
"Notes: Each panel plots β (with 95% CI band) from the anchor flow spec (treatment = fx_z(t−L) × log(mig_int_z); controls:\n"
"year × mig_int_z + year × fx_z + Block A × year; muni + year FE; SE clustered at muni) on log(1 + new firms in muni m\n"
"founded in year t, in given industry) across lags 0–10. Sample: NEC panel of muni × founding-year cells, k≥25 (8,550\n"
"cells). Hospitality & food shows the deepest negative response (lag 2–5, β ≈ −0.04 to −0.06 ***); transport, construction\n"
"and manufacturing follow a milder but consistent pattern; agriculture and finance/prof/RE trend positive at mid-lags.\n"
"Lag 10 estimates are small-sample (only 2011+ cohort survives the merge with pre-2008 FX) and noisier.")
save("firm_entry_lag_by_industry_panels")

# ====================================================================
# Plot 8 — Firm ENTRY (flow) by size, multi-panel with CI bands
# ====================================================================
print("Plot 8: firm entry by size (multi-panel) ...")
ENT_SIZE = [
    ("log_new_firms",                   "All entry (aggregate)"),
    ("log_new_firms_size_1_worker",     "1 worker"),
    ("log_new_firms_size_2_9_workers",  "2–9 workers"),
    ("log_new_firms_size_10_50_workers","10–50 workers"),
    ("log_new_firms_size_51plus_workers","51+ workers"),
]
fig, axes = plt.subplots(1, 5, figsize=(18, 7.5), sharex=True)
for ax, (oc, lab) in zip(axes, ENT_SIZE):
    s = panel[panel["outcome"] == oc].sort_values("lag")
    if not s.empty:
        x, b, se = s["lag"].values, s["beta"].values, s["se"].values
        col = "#1a202c" if oc == "log_new_firms" else "#c53030"
        ax.plot(x, b, marker="o", markersize=4.5, lw=1.8, color=col, label="β (95% CI)")
        ax.fill_between(x, b - 1.96*se, b + 1.96*se, color=col, alpha=0.15)
    ax.axhline(0, color="#1a202c", lw=0.5, ls=":")
    ax.set_title(lab, fontsize=10, fontweight="bold")
    ax.set_xlabel("FX shifter lag (years)", fontsize=8.5)
    ax.tick_params(labelsize=8)
    ax.grid(axis="y", lw=0.3, alpha=0.4)
axes[0].set_ylabel("β  on log(1 + # new firms)", fontsize=9)
hh, ll = axes[0].get_legend_handles_labels()
fig.suptitle("FIRM ENTRY (flow) by size — annual log(# new firms) response to lagged FX shock",
             y=0.97, fontsize=12, fontweight="bold")
fig.legend(hh, ll, loc="upper center", ncol=1, frameon=False,
           bbox_to_anchor=(0.5, 0.91), fontsize=10)
fig.subplots_adjust(top=0.83, bottom=0.34, left=0.05, right=0.98, wspace=0.30)
add_note(fig,
"Notes: β (with 95% CI band) from the anchor flow spec on log(1 + new firms in muni m founded in year t, in given size\n"
"category) across lags 0–10. Sample: NEC panel of muni × founding-year cells, k≥25. Aggregate panel (left, black) shows\n"
"the overall lag profile; the four size-specific panels show how the response is allocated by firm size. Size-1 (solo)\n"
"entries are roughly flat near zero. 2–9 worker entries (small employers) show a mild but consistent dip at mid-lags.\n"
"10–50 worker entries are too rare for clean identification (wide CIs). 51+ worker entries trend slightly positive at\n"
"lag 2 and lag 10 but never significant. Bottom line: the aggregate response masks heterogeneity — the negative-entry\n"
"effect comes mainly from suppressed 2–9 worker firm formation, not solo proprietors.")
save("firm_entry_lag_by_size_panels")

# ====================================================================
# Plot 9 + 10 — OLS-on-log(1+y)  vs.  PPML  overlay (entry by industry / size)
# ====================================================================
PPML_PATH = ROOT / "output/tab/robustness_nec_poisson.csv"
if PPML_PATH.exists():
    ppml = pd.read_csv(PPML_PATH)
    ppml = ppml[ppml["err"].fillna("") == ""].copy()
    ppml_a = ppml[ppml["threshold"] == THR]   # anchor k=25
    # OLS panel rows at the anchor spec — already in `panel` above (log/lin, k=25)
    # PPML outcomes are the raw count names; map to the log_* names used in panel
    def ols_for(raw_oc):
        s = panel[panel["outcome"] == "log_" + raw_oc].sort_values("lag")
        return s
    def ppml_for(raw_oc):
        s = ppml_a[ppml_a["outcome"] == raw_oc].sort_values("lag")
        return s

    def draw_overlay(ax, raw_oc, label):
        ols  = ols_for(raw_oc)
        pp   = ppml_for(raw_oc)
        if not ols.empty:
            x, b, se = ols["lag"].values, ols["beta"].values, ols["se"].values
            ax.plot(x, b, marker="o", markersize=4, lw=1.6, color="#2c5282",
                    label="OLS on log(1+y)")
            ax.fill_between(x, b - 1.96*se, b + 1.96*se, color="#2c5282", alpha=0.10)
        if not pp.empty:
            x, b, se = pp["lag"].values, pp["beta"].values, pp["se"].values
            ax.plot(x, b, marker="s", markersize=4, lw=1.6, color="#c53030",
                    label="PPML on raw count")
            ax.fill_between(x, b - 1.96*se, b + 1.96*se, color="#c53030", alpha=0.10)
        ax.axhline(0, color="#1a202c", lw=0.5, ls=":")
        ax.set_title(label, fontsize=10, fontweight="bold")
        ax.set_xlabel("FX shifter lag (years)", fontsize=8.5)
        ax.tick_params(labelsize=8)
        ax.grid(axis="y", lw=0.3, alpha=0.4)

    # ---- 9: industry overlay ----
    print("Plot 9: PPML vs OLS — entry by industry ...")
    OVL_IND = [
        ("new_firms",                        "All entry (aggregate)"),
        ("new_firms_hospitality_food",       "Hospitality & food"),
        ("new_firms_manufacturing",          "Manufacturing"),
        ("new_firms_construction",           "Construction"),
        ("new_firms_transport_storage",      "Transport & storage"),
        ("new_firms_trade_retail",           "Trade & retail"),
        ("new_firms_agriculture",            "Agriculture"),
        ("new_firms_finance_prof_realestate","Finance / prof / RE"),
    ]
    fig, axes = plt.subplots(2, 4, figsize=(15, 12), sharex=True)
    for ax, (oc, lab) in zip(axes.flatten(), OVL_IND):
        draw_overlay(ax, oc, lab)
    for ax in (axes[0,0], axes[1,0]):
        ax.set_ylabel("β  (semi-elasticity)", fontsize=9)
    hh, ll = axes[0,0].get_legend_handles_labels()
    fig.suptitle("FIRM ENTRY by industry — OLS on log(1+y)  vs.  PPML on raw count",
                 y=0.985, fontsize=12, fontweight="bold")
    fig.legend(hh, ll, loc="upper center", ncol=2, frameon=False,
               bbox_to_anchor=(0.5, 0.955), fontsize=10)
    fig.subplots_adjust(top=0.88, bottom=0.18, left=0.05, right=0.98,
                        hspace=0.40, wspace=0.28)
    add_note(fig,
"Notes: Side-by-side comparison of OLS on log(1 + new firms) (blue, anchor spec from robustness_nec.csv) vs. PPML on raw\n"
"new-firms count (red, robustness_nec_poisson.csv). Both use identical controls (year × mig_int_z + year × fx_z + Block A;\n"
"muni + year FE; SE clustered at muni) and the same treatment (fx_z × log(mig_int_z)). Both coefficients are semi-elasticities,\n"
"so they are directly comparable. PPML treats zero cells natively (large-zero outcomes like agriculture 57%, finance/RE 60%);\n"
"OLS on log(1+y) floors zeros at log(1)=0. Where the two lines diverge sharply (e.g. agriculture, finance/RE), the OLS estimate\n"
"is contaminated by the zero-floor and PPML is the preferred estimator. Where they overlap (hospitality, trade-retail), the\n"
"OLS result is robust. PPML lifts the aggregate elasticity because counts are mass-weighted (large munis count more).")
    save("firm_entry_lag_industry_ppml_overlay")

    # ---- 10: size overlay ----
    print("Plot 10: PPML vs OLS — entry by size ...")
    OVL_SIZE = [
        ("new_firms",                    "All entry (aggregate)"),
        ("new_firms_size_1_worker",      "1 worker"),
        ("new_firms_size_2_9_workers",   "2–9 workers"),
        ("new_firms_size_10_50_workers", "10–50 workers"),
        ("new_firms_size_51plus_workers","51+ workers"),
    ]
    fig, axes = plt.subplots(1, 5, figsize=(18, 7.5), sharex=True)
    for ax, (oc, lab) in zip(axes, OVL_SIZE):
        draw_overlay(ax, oc, lab)
    axes[0].set_ylabel("β  (semi-elasticity)", fontsize=9)
    hh, ll = axes[0].get_legend_handles_labels()
    fig.suptitle("FIRM ENTRY by size — OLS on log(1+y)  vs.  PPML on raw count",
                 y=0.97, fontsize=12, fontweight="bold")
    fig.legend(hh, ll, loc="upper center", ncol=2, frameon=False,
               bbox_to_anchor=(0.5, 0.91), fontsize=10)
    fig.subplots_adjust(top=0.83, bottom=0.34, left=0.05, right=0.98, wspace=0.30)
    add_note(fig,
"Notes: OLS vs. PPML side-by-side on the same NEC entry panel. PPML matters most for the high-zero buckets: size 10–50\n"
"workers (52% zeros) and size 51+ workers (91% zeros). For the size-51+ panel, OLS-on-log(1+y) is essentially identifying\n"
"off the few non-zero cells, while PPML uses the full sample correctly. For the dominant 1-worker and 2–9 worker categories\n"
"(low zero-share), the two estimators agree closely — confirming the OLS result on the main entry response is robust.")
    save("firm_entry_lag_size_ppml_overlay")
else:
    print(f"PPML CSV not yet present at {PPML_PATH}; skipping plots 9-10.")
    print("Run: python3 script/robustness_nec_panel_poisson.py")

# ====================================================================
# Plot 11 + 12 — Stock-count OLS vs PPML overlay, by cohort
# ====================================================================
CS_PPML_PATH = ROOT / "output/tab/robustness_nec_cs_poisson.csv"
if CS_PPML_PATH.exists():
    cs_p = pd.read_csv(CS_PPML_PATH)
    cs_p = cs_p[cs_p["err"].fillna("") == ""].copy()
    cs_p_a = cs_p[cs_p["threshold"] == THR]

    COH_LBL = {"nec_cs_full": "full 2018 stock",
               "nec_cs_2001": "post-2001 cohort",
               "nec_cs_2011": "post-2011 cohort"}
    COH_COL_OLS  = {"nec_cs_full": "#a0aec0",
                    "nec_cs_2001": "#2c5282",
                    "nec_cs_2011": "#0987a0"}
    COH_COL_PPML = {"nec_cs_full": "#e9b196",
                    "nec_cs_2001": "#c53030",
                    "nec_cs_2011": "#742a2a"}

    def draw_cs_overlay(ax, raw_oc, label):
        for ds in ["nec_cs_full", "nec_cs_2001", "nec_cs_2011"]:
            for est, col_map, marker, ls in [
                ("ols_log",  COH_COL_OLS,  "o", "-"),
                ("ppml",     COH_COL_PPML, "s", "--")]:
                s = cs_p_a[(cs_p_a["dataset"] == ds) &
                            (cs_p_a["outcome"] == raw_oc) &
                            (cs_p_a["estimator"] == est)].sort_values("lag")
                if s.empty: continue
                x, b, se = s["lag"].values, s["beta"].values, s["se"].values
                lbl = f"{est.upper().replace('OLS_LOG','OLS log(1+y)')} · {COH_LBL[ds]}"
                ax.plot(x, b, marker=marker, markersize=3.5, lw=1.4, ls=ls,
                        color=col_map[ds], label=lbl, alpha=0.9)
        ax.axhline(0, color="#1a202c", lw=0.5, ls=":")
        ax.set_title(label, fontsize=10, fontweight="bold")
        ax.set_xlabel("FX shifter lag (years)", fontsize=8.5)
        ax.tick_params(labelsize=8)
        ax.grid(axis="y", lw=0.3, alpha=0.4)

    # ---- 11: industry stock overlay ----
    print("Plot 11: Stock count by industry — OLS vs PPML by cohort ...")
    CS_IND = [
        ("n_firms",                   "All firms (aggregate)"),
        ("n_firms_hospitality",       "Hospitality"),
        ("n_firms_manufacturing",     "Manufacturing"),
        ("n_firms_construction",      "Construction"),
        ("n_firms_transport",         "Transport"),
        ("n_firms_trade_retail",      "Trade & retail"),
        ("n_firms_agriculture",       "Agriculture"),
        ("n_firms_finance_prof_info", "Finance / prof / info"),
    ]
    fig, axes = plt.subplots(2, 4, figsize=(16, 12.5), sharex=True)
    for ax, (oc, lab) in zip(axes.flatten(), CS_IND):
        draw_cs_overlay(ax, oc, lab)
    for ax in (axes[0,0], axes[1,0]):
        ax.set_ylabel("β  (semi-elasticity)", fontsize=9)
    hh, ll = axes[0,0].get_legend_handles_labels()
    fig.suptitle("FIRM STOCK (2018) by industry — OLS on log(1+y)  vs.  PPML on raw count, by cohort",
                 y=0.985, fontsize=12, fontweight="bold")
    fig.legend(hh, ll, loc="upper center", ncol=3, frameon=False,
               bbox_to_anchor=(0.5, 0.955), fontsize=9.5)
    fig.subplots_adjust(top=0.88, bottom=0.18, left=0.05, right=0.98,
                        hspace=0.40, wspace=0.28)
    add_note(fig,
"Notes: 2018 cross-section, anchor cs spec (treatment = fx_z(2018-L) × log(mig_int_z); controls mig_int_z + fx_z + Block A;\n"
"district FE; SE clustered at district), k≥25. Each panel plots β (semi-elasticity) on raw firm counts in the named industry,\n"
"with three cohort overlays (full 2018 stock; post-2001 cohort; post-2011 cohort) and two estimators (OLS solid circles on\n"
"log(1+y); PPML dashed squares on raw y). Where OLS and PPML lines coincide within a cohort, the OLS-log result is robust.\n"
"Largest divergences appear for high-zero industries (construction 60%, transport 60%, agriculture 5–6% across cohorts).\n"
"PPML weights by count level so big-firm-count munis drive the elasticity — particularly visible on the aggregate panel.")
    save("firm_stock_industry_count_ppml_overlay")

    # ---- 12: size stock overlay ----
    print("Plot 12: Stock count by size — OLS vs PPML by cohort ...")
    CS_SIZE = [
        ("n_firms",                       "All firms (aggregate)"),
        ("n_firms_size_1_worker",         "1 worker"),
        ("n_firms_size_2_9_workers",      "2–9 workers"),
        ("n_firms_size_10_50_workers",    "10–50 workers"),
        ("n_firms_size_51plus_workers",   "51+ workers"),
    ]
    fig, axes = plt.subplots(1, 5, figsize=(20, 8.6), sharex=True)
    for ax, (oc, lab) in zip(axes, CS_SIZE):
        draw_cs_overlay(ax, oc, lab)
    axes[0].set_ylabel("β  (semi-elasticity)", fontsize=9)
    hh, ll = axes[0].get_legend_handles_labels()
    fig.suptitle("FIRM STOCK (2018) by size — OLS on log(1+y)  vs.  PPML on raw count, by cohort",
                 y=0.97, fontsize=12, fontweight="bold")
    fig.legend(hh, ll, loc="upper center", ncol=3, frameon=False,
               bbox_to_anchor=(0.5, 0.91), fontsize=9.5)
    fig.subplots_adjust(top=0.83, bottom=0.30, left=0.04, right=0.98, wspace=0.30)
    add_note(fig,
"Notes: 2018 cross-section, anchor cs spec. β (semi-elasticity) on raw firm counts in each size bucket. Three cohorts × two\n"
"estimators (six lines per panel). For size-51+ firms (54–65% zeros depending on cohort), PPML and OLS-log diverge sharply —\n"
"PPML preferred. For the dominant size 1 and size 2–9 categories (essentially no zeros), OLS-log and PPML track each other\n"
"closely, with PPML somewhat above OLS-log because of count-level weighting. The aggregate panel rolls up all four size\n"
"buckets and shows the PPML elasticity ≈ 2–3× the OLS-log estimate, consistent with big-muni mass weighting.")
    save("firm_stock_size_count_ppml_overlay")
else:
    print(f"Stock PPML CSV not yet present at {CS_PPML_PATH}; skipping plots 11-12.")
    print("Run: python3 script/robustness_nec_cs_poisson.py")

print("\nAll plots saved to docs/figs/.")
