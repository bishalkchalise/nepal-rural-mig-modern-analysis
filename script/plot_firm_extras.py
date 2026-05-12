"""
Two extra forest plots:
  A. Firm entry by SIZE bucket (NEC panel, log scale)
  B. Firm count by INDUSTRY in 2018 cross-section (NEC cs, log scale)

Same anchor spec (A4 = log_int + c_mig + c_fx + Block A).
SVG + PNG saved to docs/figs/.
"""
import json
from pathlib import Path
import matplotlib.pyplot as plt
import matplotlib as mpl

mpl.rcParams.update({
    "font.family": "DejaVu Sans, Helvetica, Arial, sans-serif",
    "font.size": 9, "axes.titlesize": 11, "axes.titleweight": "bold",
    "axes.spines.top": False, "axes.spines.right": False,
    "axes.edgecolor": "#1a202c", "axes.linewidth": 0.6,
})

ROOT = Path(".")
d = json.load(open(ROOT / "docs/results.json"))

def pull(ds, varlist, thr_str, fam='A', spec='A4'):
    cells = d['datasets'][ds]['estimates'][thr_str][fam][spec]
    rows = []
    for var, lab in varlist:
        c = cells.get(var, {})
        if 'beta' not in c: continue
        b, se = c['beta'], c['se']
        rows.append({'label': lab, 'beta': b, 'se': se,
                     'lo': b - 1.96*se, 'hi': b + 1.96*se,
                     'p': c.get('pval', 1.0), 'mean': c.get('mean_y', 0)})
    return rows

def two_panel(rows_left, rows_right, title_left, title_right,
              suptitle, xlabel, outfile):
    # Sort both panels by left-panel beta (most negative on top)
    order = sorted(range(len(rows_left)), key=lambda i: rows_left[i]['beta'])
    rows_left  = [rows_left[i]  for i in order]
    rows_right = [rows_right[i] for i in order]
    labels = [r['label'] for r in rows_left]
    fig, axes = plt.subplots(1, 2, figsize=(9.5, 4.2), sharey=True)
    y = list(range(len(labels)))
    extreme = max(abs(r['hi']) for r in rows_left + rows_right
                  if r['lo'] is not None) * 1.05
    for ax, rows, title in zip(axes, [rows_left, rows_right],
                              [title_left, title_right]):
        for i, r in enumerate(rows):
            color = '#2c5282' if r['p'] < 0.10 else '#a0aec0'
            ax.errorbar(r['beta'], i,
                        xerr=[[r['beta'] - r['lo']], [r['hi'] - r['beta']]],
                        fmt='o', color=color, ecolor=color, capsize=3,
                        markersize=6, linewidth=1.2)
        ax.axvline(0, color='#cbd5e0', linewidth=0.8, zorder=0)
        ax.set_yticks(y); ax.set_yticklabels(labels)
        ax.set_title(title, loc='left', pad=8)
        ax.set_xlabel(xlabel, fontsize=8)
        ax.invert_yaxis()
        ax.tick_params(axis='both', length=2)
        ax.grid(axis='x', linewidth=0.4, alpha=0.4)
        ax.set_xlim(-extreme, extreme)
    plt.suptitle(suptitle, fontsize=13, fontweight='bold',
                 x=0.06, y=0.98, ha='left')
    plt.tight_layout(rect=[0, 0, 1, 0.92])
    out = ROOT / "docs/figs" / outfile
    out.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(out, format='svg', bbox_inches='tight', dpi=300)
    plt.savefig(out.with_suffix('.png'), format='png', bbox_inches='tight', dpi=180)
    print(f"Saved: {out}")
    return rows_left, rows_right

# ===== A. NEC panel — firm entry by size =====
SIZES = [
    ("log_new_firms_size_1_worker",       "1 worker"),
    ("log_new_firms_size_2_9_workers",    "2–9 workers"),
    ("log_new_firms_size_10_50_workers",  "10–50 workers"),
    ("log_new_firms_size_51plus_workers", "51+ workers"),
]
print("\n=== Firm entry by SIZE (NEC panel) ===")
r0  = pull('nec_panel', SIZES, '0')
r25 = pull('nec_panel', SIZES, '25')
two_panel(r0, r25,
    'Full sample  ($k = 0$)', 'Threshold  $k \\geq 25$',
    'Firm entry by size — NEC panel, 2001–2018',
    'β  on log(1 + # new firms)  per 1 SD shock',
    'firm_entry_size.svg')
for r in r0:  print(f"  {r['label']:18s}  k=0:  β={r['beta']:+.4f}  CI=[{r['lo']:+.4f},{r['hi']:+.4f}]  p={r['p']:.3f}")
for r in r25: print(f"  {r['label']:18s}  k=25: β={r['beta']:+.4f}  CI=[{r['lo']:+.4f},{r['hi']:+.4f}]  p={r['p']:.3f}")

# ===== B. NEC cross-section — firm count by industry =====
NEC_CS_INDUSTRIES = [
    ("log_n_firms_agriculture",       "Agriculture"),
    ("log_n_firms_manufacturing",     "Manufacturing"),
    ("log_n_firms_construction",      "Construction"),
    ("log_n_firms_trade_retail",      "Trade & retail"),
    ("log_n_firms_transport",         "Transport"),
    ("log_n_firms_hospitality",       "Hospitality"),
    ("log_n_firms_finance_prof_info", "Finance / prof / info"),
    ("log_n_firms_social_services",   "Social services"),
    ("log_n_firms_other_services",    "Other services"),
]
print("\n=== # firms by INDUSTRY (NEC cross-section, 2018) ===")
r0  = pull('nec_cs', NEC_CS_INDUSTRIES, '0')
r25 = pull('nec_cs', NEC_CS_INDUSTRIES, '25')
two_panel(r0, r25,
    'Full sample  ($k = 0$)', 'Threshold  $k \\geq 25$',
    '# firms by industry — NEC 2018 cross-section',
    'β  on log(1 + # firms)  per 1 SD shock',
    'firms_count_industry.svg')
for r in r0:  print(f"  {r['label']:25s}  k=0:  β={r['beta']:+.4f}  p={r['p']:.3f}")
for r in r25: print(f"  {r['label']:25s}  k=25: β={r['beta']:+.4f}  p={r['p']:.3f}")
