"""
Forest plot: β on log(new firms) by industry, NEC panel.
Two thresholds shown side by side (k=0 and k=25) so the audience sees
the headline at full sample and the smaller-sample stability.
Saves an SVG to docs/figs/firm_entry_industry.svg for embedding in the deck.
"""
import json
from pathlib import Path
import matplotlib.pyplot as plt
import matplotlib as mpl

mpl.rcParams.update({
    "font.family": "IBM Plex Sans, Helvetica, Arial, sans-serif",
    "font.size": 9,
    "axes.titlesize": 11,
    "axes.titleweight": "bold",
    "axes.spines.top": False,
    "axes.spines.right": False,
    "axes.edgecolor": "#1a202c",
    "axes.linewidth": 0.6,
    "xtick.color": "#1a202c",
    "ytick.color": "#1a202c",
})

ROOT = Path(".")
d = json.load(open(ROOT / "docs/results.json"))

INDUSTRIES = [
    ("log_new_firms_manufacturing",            "Manufacturing"),
    ("log_new_firms_construction",             "Construction"),
    ("log_new_firms_trade_retail",             "Trade & retail"),
    ("log_new_firms_hospitality_food",         "Hospitality & food"),
    ("log_new_firms_transport_storage",        "Transport & storage"),
    ("log_new_firms_finance_prof_realestate",  "Finance / prof / RE"),
    ("log_new_firms_education_health_social",  "Education / health / social"),
    ("log_new_firms_other_services",           "Other services"),
]

def pull(thr_str):
    cells = d['datasets']['nec_panel']['estimates'][thr_str]['A']['A4']
    rows = []
    for var, lab in INDUSTRIES:
        c = cells.get(var, {})
        if 'beta' not in c: continue
        b, se = c['beta'], c['se']
        rows.append({'label': lab, 'beta': b, 'se': se,
                     'lo': b - 1.96*se, 'hi': b + 1.96*se,
                     'p': c.get('pval', 1.0)})
    return rows

rows_k0  = pull('0')
rows_k25 = pull('25')

# Sort by k=0 β magnitude (most negative on top)
order = sorted(range(len(rows_k0)), key=lambda i: rows_k0[i]['beta'])
rows_k0  = [rows_k0[i]  for i in order]
rows_k25 = [rows_k25[i] for i in order]
labels = [r['label'] for r in rows_k0]

# Two-panel side-by-side
fig, axes = plt.subplots(1, 2, figsize=(9.5, 4.2), sharey=True)
y = list(range(len(labels)))

for ax, rows, title in zip(axes, [rows_k0, rows_k25],
                          ['Full sample  ($k = 0$)', 'Threshold  $k \\geq 25$']):
    for i, r in enumerate(rows):
        color = '#2c5282' if r['p'] < 0.10 else '#a0aec0'
        ax.errorbar(r['beta'], i, xerr=[[r['beta'] - r['lo']], [r['hi'] - r['beta']]],
                    fmt='o', color=color, ecolor=color, capsize=3,
                    markersize=6, linewidth=1.2)
    ax.axvline(0, color='#cbd5e0', linewidth=0.8, zorder=0)
    ax.set_yticks(y)
    ax.set_yticklabels(labels)
    ax.set_ylim(len(labels) - 0.5, -0.5)   # padding top & bottom; inverted
    ax.set_title(title, loc='left', pad=8)
    ax.set_xlabel('β  on log(# new firms)  per 1 SD shock', fontsize=8)
    ax.tick_params(axis='both', length=2)
    ax.grid(axis='x', linewidth=0.4, alpha=0.4)
    # symmetric x-axis around 0 with generous padding so bars + ticks fit
    all_bounds = [abs(r['lo']) for r in rows_k0 + rows_k25] + \
                 [abs(r['hi']) for r in rows_k0 + rows_k25]
    extreme = max(all_bounds) * 1.18
    ax.set_xlim(-extreme, extreme)

plt.tight_layout()

out = ROOT / "docs/figs/firm_entry_industry.svg"
out.parent.mkdir(parents=True, exist_ok=True)
plt.savefig(out, format='svg', bbox_inches='tight', dpi=300)
plt.savefig(out.with_suffix('.png'), format='png', bbox_inches='tight', dpi=180)
print(f"Saved: {out}")
print(f"Saved: {out.with_suffix('.png')}")

# Print numeric summary
print("\nNumeric summary:")
for r0, r25 in zip(rows_k0, rows_k25):
    print(f"  {r0['label']:30s}  k=0: β={r0['beta']:+.4f} (p={r0['p']:.3f})  k=25: β={r25['beta']:+.4f} (p={r25['p']:.3f})")
