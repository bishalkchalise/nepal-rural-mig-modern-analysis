"""
Replace slide 12 (Firm entry) body with log-scale coefficients pulled from
output/tab/firm_heterogeneity_net.csv (full sample, 4 thresholds).

This brings slide 12 onto the same outcome scale as the forest plots (14-15)
and the heterogeneity slide (H2).
"""
import pandas as pd, re
from pathlib import Path

ROOT = Path(".")
HTML_PATH = ROOT / "docs/presentation.html"
CSV = ROOT / "output/tab/firm_heterogeneity_net.csv"

# Outcome → display label
OUTCOMES = [
    ("log_new_firms",                    "(1) Total new firms"),
    ("log_new_firms_size_1_worker",      "(2) Size: 1-worker"),
    ("log_new_firms_size_2_9_workers",   "(3) Size: 2–9 workers"),
    ("log_new_firms_size_10_50_workers", "(4) Size: 10–50 workers"),
    ("log_new_firms_manufacturing",      "(5) Industry: Manufacturing"),
    ("log_new_firms_trade_retail",       "(6) Industry: Trade &amp; retail"),
    ("log_new_firms_hospitality_food",   "(7) Industry: Hospitality &amp; food"),
]

# Load full sample at all 4 thresholds
df = pd.read_csv(CSV)
df = df[(df['sample'] == 'full') & (df['dataset'] == 'nec_panel')]

def stars_html(p):
    if pd.isna(p): return ""
    if p < .01: return ' <span class="stars">***</span>'
    if p < .05: return ' <span class="stars">**</span>'
    if p < .10: return ' <span class="stars">*</span>'
    return ""

def fmt_beta(b, p):
    """log-scale beta: 3 decimal places. Negative uses $-$ for HTML aesthetic."""
    s = stars_html(p)
    if pd.isna(b): return "&mdash;"
    sign = "$-$" if b < 0 else ""
    return f"{sign}{abs(b):.3f}{s}"

def fmt_se(se):
    if pd.isna(se): return ""
    return f"({se:.3f})"

def fmt_pct(b):
    """approx percent change for log outcomes."""
    if pd.isna(b): return ""
    pct = b * 100
    sign = "$-$" if pct < 0 else "+"
    return f"[{sign}{abs(pct):.1f}%]"

def fmt_n(n, n_muni):
    return f"{int(n):,} / {int(n_muni)}"

rows_html = []
# We need 4 columns: k=0 (full), 25, 50, 100
THR = [0, 25, 50, 100]
for code, label in OUTCOMES:
    sub = df[df['outcome'] == code].set_index('threshold')
    # build cells
    beta_cells, se_cells, pct_cells = [], [], []
    for thr in THR:
        if thr in sub.index:
            r = sub.loc[thr]
            beta_cells.append(f"<td>{fmt_beta(r['beta'], r['pval'])}</td>")
            se_cells  .append(f'<td class="se">{fmt_se(r["se"])}</td>')
            pct_cells .append(f"<td>{fmt_pct(r['beta'])}</td>")
        else:
            beta_cells.append("<td>&mdash;</td>")
            se_cells  .append('<td class="se"></td>')
            pct_cells .append("<td></td>")
    # mean (use k=25 sample mean — matches the het slide convention)
    mean_y = sub.loc[25, 'mean_y'] if 25 in sub.index else float('nan')
    mean_str = f"Mean {mean_y:.2f}" if not pd.isna(mean_y) else ""
    rows_html.append(
        f'      <tr class="outcomehead"><td><strong>{label}</strong></td>\n'
        f'          ' + ''.join(beta_cells) + '</tr>\n'
        f'      <tr class="outcomesub"><td>{mean_str}</td>\n'
        f'          ' + ''.join(se_cells) + '</tr>\n'
        f'      <tr class="pct"><td></td>' + ''.join(pct_cells) + '</tr>'
    )

# Build the N row: use log_new_firms (the comprehensive outcome) for N
n_lookup = df[df['outcome'] == 'log_new_firms'].set_index('threshold')
n_cells = []
for thr in THR:
    if thr in n_lookup.index:
        r = n_lookup.loc[thr]
        n_cells.append(f"<td>{fmt_n(r['n'], r['n_muni'])}</td>")
    else:
        n_cells.append("<td>&mdash;</td>")
n_row = (f'      <tr><td>$N$ (muni × cohort) / Clusters</td>\n'
         f'          ' + ''.join(n_cells) + '</tr>')

# New slide 12 body (from <section> through </section>)
new_section = f"""<section>
  <h2>Firm entry, 2001–2018 — by size and industry</h2>
  <table>
    <thead>
      <tr>
        <th rowspan="2" class="outcome-span">Outcome: log(1 + # new firms per muni-cohort)</th>
        <th colspan="4" class="sample-span">Municipality sample &mdash; 2001 migrant-count threshold</th>
      </tr>
      <tr class="threshold-row">
        <th>full</th><th>k ≥ 25</th><th>k ≥ 50</th><th>k ≥ 100</th>
      </tr>
    </thead>
    <tbody>

{chr(10).join(rows_html)}

{n_row}
    </tbody>
  </table>
  <p class="eq-cap">
    Reported $\\beta$: $\\mathrm{{fx}}_z \\times \\log(\\mathrm{{mig\\_int}}_z)$. SE in (), approximate % change in []
    (for log outcomes, the bracketed value is $100 \\cdot \\beta$, the proportional response).
    *** $p&lt;.01$, ** $p&lt;.05$, * $p&lt;.10$. SE clustered at municipality. FE: muni + cohort-year.
    Source: NEC 2018 firm census, panel of surviving firms by founding year 2001–2018.
    Outcome: $\\log(1 + \\text{{count}})$ so the coefficient is interpretable as an approximate proportional change
    (a $\\beta$ of $-0.05$ ≈ 5% fewer new firms per year per 1-SD shock to the shift-share treatment).
    Same outcome scale as the forest plots (slides 14–15) and the heterogeneity slide.
  </p>
</section>"""

# Replace slide 12 in HTML
html = HTML_PATH.read_text(encoding="utf-8")
# Anchor: the slide-12 comment block to next section break
beg_anchor = ('<!-- =============================================================\n'
              '     12. Firm entry — by size and industry (NEC panel)\n'
              '     ============================================================= -->\n'
              '<section>')
end_anchor = ('\n</section>\n\n<!-- =============================================================\n'
              '     13. Firm structure')

beg_idx = html.find(beg_anchor)
end_idx = html.find(end_anchor, beg_idx)
if beg_idx < 0 or end_idx < 0:
    raise RuntimeError(f"Could not locate slide 12 markers in HTML. beg={beg_idx}, end={end_idx}")

# Replace from <section> through </section> (keeping slide-12 comment but
# swapping the section body).
section_start = beg_idx + beg_anchor.find('<section>')
new_html = html[:section_start] + new_section + html[end_idx + 1:]  # +1 skip leading \n in end_anchor
HTML_PATH.write_text(new_html, encoding="utf-8")
print(f"Rewrote slide 12 in {HTML_PATH}")
print(f"  Outcomes:    {len(OUTCOMES)}")
print(f"  Thresholds:  {THR}")
print(f"  Mean baseline: k=0 sample mean_y (matches het slide k=0 if shown)")
