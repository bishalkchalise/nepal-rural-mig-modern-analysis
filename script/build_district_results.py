#!/usr/bin/env python3
"""Build docs/district_results.html (reveal.js deck) mirroring the muni
presentation.html structure but with district-level numbers.

Tables: rows = outcomes, columns = 4 specs:
    A2          all 75 districts -- + C_mig only
    A3          all 75 districts -- + C_fx
    A4          all 75 districts -- + C_X (saturated)
    A4 drop-KTM all 72 districts -- robustness: drop Kathmandu valley

Each outcome shows 4 rows in muni style:
    outcomehead: beta + stars
    outcomesub : panel description + (SE)
    pct        : [% of mean]
    N/clusters : N / N_dist

Data source: docs/district_robustness.json built by the R script.
"""

import json
from pathlib import Path

DATA = json.load(open("docs/district_robustness.json"))

# ============================================================================
# First-stage validation data (one row per period x model)
# Source: district-analysis/output/tab/first_stage_future_mig.csv
# ============================================================================
import csv
FIRST_STAGE = []
fs_path = "district-analysis/output/tab/first_stage_future_mig.csv"
if Path(fs_path).exists():
    FIRST_STAGE = list(csv.DictReader(open(fs_path)))
    print(f"Loaded {len(FIRST_STAGE)} first-stage rows from {fs_path}")

# ============================================================================
# Slide structure: one slide per (group, dataset). Title and outcome list
# explicit so the slide reads coherently.
# ============================================================================
SLIDES = [
    # (slide_title, dataset_key, group_name, [outcome_keys in order])
    ("Internal migration (in / out / net)", "census", "Migration", [
        "mig_in_internal_share", "mig_out_internal_share", "net_internal_mig_share",
        "mig_in_temp_share", "mig_out_temp_share", "net_temp_mig_share",
    ]),
    ("Migration by reason (5-year, temporary)", "census", "Migration", [
        "mig_in_temp_economic_share", "mig_in_temp_noneconomic_share",
        "mig_out_temp_economic_share", "mig_out_temp_noneconomic_share",
    ]),
    ("Industry employment shares", "census", "Industry", [
        "ind_agri_forestry_fish", "ind_manufacturing", "ind_construction",
        "ind_wholesale_retail", "ind_transport_accommodation",
        "ind_finance_real_estate_prof", "ind_education", "ind_health",
    ]),
    ("Occupational composition", "census", "Occupation", [
        "occ_share_managers", "occ_share_professionals", "occ_share_technicians",
        "occ_share_service_sales", "occ_share_agriculture",
        "occ_share_craft_trades", "occ_share_elementary",
    ]),
    ("Employment status", "census", "Employment Status", [
        "emp_share_employee", "emp_share_employer",
        "emp_share_self_employed", "emp_share_unpaid_family_worker",
    ]),
    ("Human capital -- education", "census", "Education", [
        "edu_literate", "edu_literate_female", "edu_literate_male",
        "edu_school_attend_6_16", "edu_school_attend_6_16_female",
        "edu_school_attend_6_16_male",
    ]),
    ("Household assets", "census", "Assets", [
        "amen_assets_mobile", "amen_assets_internet", "amen_assets_landline",
        "amen_assets_radio", "amen_assets_tv", "amen_assets_motorcycle",
        "amen_assets_car",
    ]),
    ("Amenities -- cooking & lighting", "census", "Amenities", [
        "amen_cooking_lpg", "amen_cooking_modern", "amen_cooking_traditional",
        "amen_lighting_electricity",
        "amen_water_piped", "amen_toilet_modern",
    ]),
    ("Housing structure", "census", "Housing", [
        "housing_own", "housing_rented", "housing_foundation_modern",
        "housing_foundation_traditional", "housing_roof_modern",
        "housing_roof_traditional",
    ]),
    ("HH spending (raw)", "hh", "HH spending", [
        "food_exp_total_7day", "nonfood_exp_12m",
        "edu_spend_total_12m", "hlt_spend_total",
    ]),
    ("HH spending (log)", "hh", "HH spending", [
        "log_food_exp_total_7day", "log_nonfood_exp_12m",
        "log_edu_spend_total_12m", "log_hlt_spend_total",
    ]),
    ("HH migration & remittance", "hh", "Migration -- HH", [
        "has_migrant_intl", "n_intl_migrants",
        "remit_received", "remit_amount_intl_12m_rs",
        "log_remit_amount_intl_12m_rs",
    ]),
    ("HH agriculture -- inputs & costs (log)", "hh", "Land -- agriculture", [
        "input_intensity_per_sqm", "log_total_input_cost_rs",
        "log_wet_cost_seed", "log_wet_cost_fert", "log_wet_cost_labour",
        "log_dry_cost_seed", "log_dry_cost_fert",
    ]),
    ("HH enterprise", "hh", "Enterprise", [
        "has_enterprise", "n_enterprises", "n_workers_total",
        "log_revenue_12m", "log_profit_12m", "log_capex_12m",
    ]),
    ("Firm structure & scale (NEC 2018)", "nec_cs", "Firm size & scale", [
        "n_firms", "emp_total", "mean_emp_per_firm",
        "share_firms_size_micro_1", "share_firms_size_small_2_9",
        "share_firms_size_medium_10_50", "share_firms_size_large_51p",
    ]),
    ("Firm formality & credit (NEC 2018)", "nec_cs", "Formality & credit", [
        "formality_index", "share_registered", "share_keeps_accounts",
        "share_formal_credit", "share_borrowed_any", "share_emp_female",
    ]),
    ("Firm entry by size (2011-2018)", "nec_panel", "Firm entry & size", [
        "log_n_new_firms",
        "log_n_new_firms_size_micro_1", "log_n_new_firms_size_small_2_9",
        "log_n_new_firms_size_medium_10_50", "log_n_new_firms_size_large_51p",
    ]),
    ("Firm entry by sector (2011-2018)", "nec_panel", "Entry by sector", [
        "log_n_new_firms_agriculture", "log_n_new_firms_manufacturing",
        "log_n_new_firms_construction", "log_n_new_firms_trade_retail",
        "log_n_new_firms_hospitality_food", "log_n_new_firms_transport_storage",
        "log_n_new_firms_education_health_social", "log_n_new_firms_other_services",
    ]),
]

# Columns: (label_for_threshold_row, variant_for_cells, model)
COLUMNS = [
    ("A2",                 "baseline",        "A2"),
    ("A3",                 "baseline",        "A3"),
    ("A4 (saturated)",     "baseline",        "A4"),
    ("A4, drop KTM",       "drop_ktm_valley", "A4"),
]
SCALING = "log"
LAG     = "2"

# Dataset -> entity description for the outcomesub row
DS_DESC = {
    "census":    "Census, district x year",
    "hh":        "HRVS HH x year",
    "nec_cs":    "NEC 2018 district cross-section",
    "nec_panel": "NEC entry-cohort, district x year",
}

# ============================================================================
# Cell rendering helpers (muni-style booktabs)
# ============================================================================
def fmt_num(x, big=False):
    if x is None: return "&mdash;"
    try: x = float(x)
    except: return "&mdash;"
    a = abs(x)
    if a >= 1e6:  return f"{x/1e6:.2f}M"
    if a >= 1e5:  return f"{x/1e3:.1f}k"
    if a >= 1e3:  return f"{round(x):,}"
    if a >= 100:  return f"{x:.1f}"
    if a >= 1:    return f"{x:.2f}"
    if a >= 0.01: return f"{x:.3f}"
    if a == 0:    return "0"
    return f"{x:.4f}"

def fmt_beta(b, p, digits=4):
    if b is None: return "&mdash;"
    try: b = float(b)
    except: return "&mdash;"
    a = abs(b)
    if a >= 1e3: s = f"{round(b):,}"
    elif a >= 100: s = f"{b:.2f}"
    elif a >= 1: s = f"{b:.3f}"
    else: s = f"{b:.{digits}f}"
    if b < 0: s = s.replace("-", "$-$")
    stars = ""
    try:
        p = float(p) if p is not None else None
        if p is not None:
            if p < 0.01: stars = '<span class="stars">***</span>'
            elif p < 0.05: stars = '<span class="stars">**</span>'
            elif p < 0.10: stars = '<span class="stars">*</span>'
    except: pass
    return f"{s} {stars}".strip()

def fmt_se(se, digits=4):
    if se is None: return ""
    try: se = float(se)
    except: return ""
    a = abs(se)
    if a >= 1e3: s = f"{round(se):,}"
    elif a >= 100: s = f"{se:.2f}"
    elif a >= 1: s = f"{se:.3f}"
    else: s = f"{se:.{digits}f}"
    return f'<span class="se">({s})</span>'

def fmt_pct(b, mean_y):
    if b is None or mean_y is None: return ""
    try:
        b = float(b); mean_y = float(mean_y)
        if abs(mean_y) < 1e-12: return ""
        pct = b / mean_y * 100
        sign = "+" if pct >= 0 else ""
        s = f"{sign}{pct:.1f}%"
        if pct < 0: s = s.replace("-", "$-$")
        return f"[{s}]"
    except: return ""

def fmt_n_clusters(n, n_unit):
    if n is None: n_s = "&mdash;"
    else:
        try: n_s = f"{int(n):,}"
        except: n_s = "&mdash;"
    if n_unit is None: u_s = ""
    else:
        try: u_s = f" / {int(n_unit):,}"
        except: u_s = ""
    return n_s + u_s

# ============================================================================
# Build a slide for one outcome group
# ============================================================================
def cell(outc_meta, variant, model):
    if outc_meta is None: return None
    k = f"{SCALING}|{LAG}|{model}|{variant}"
    return outc_meta.get("cells", {}).get(k)

def render_slide(title, ds_key, group, outcome_keys):
    ds_outc = DATA["datasets"][ds_key]["outcomes"]
    ds_desc = DS_DESC[ds_key]
    rows = []
    n_outc = 0
    for i, ok in enumerate(outcome_keys, 1):
        meta = ds_outc.get(ok)
        if meta is None: continue
        # Some outcomes may not have cells for drop_ktm_valley (e.g., for HH);
        # in that case fall back to baseline cell so the column still shows.
        mean_y = meta.get("mean_y")
        n_unit = meta.get("n_unit")
        label = meta.get("label", ok)
        # 4 columns of (beta, se, p, n) ; fall back to baseline if variant
        # cell missing for any column.
        cols = []
        for col_label, variant, model in COLUMNS:
            c = cell(meta, variant, model)
            if c is None or c.get("beta") is None:
                # Fall back to baseline cell for the same model
                c = cell(meta, "baseline", model)
            cols.append(c)
        # Skip outcome entirely if every column is empty
        if all(c is None or c.get("beta") is None for c in cols):
            continue
        n_outc += 1
        # outcomehead row
        beta_cells = "".join(
            f"<td>{fmt_beta(c.get('beta') if c else None, c.get('p') if c else None)}</td>"
            for c in cols)
        rows.append(
            f'<tr class="outcomehead"><td><strong>({n_outc}) {label}</strong></td>{beta_cells}</tr>'
        )
        # outcomesub row (panel description + SE)
        se_cells = "".join(
            f"<td>{fmt_se(c.get('se') if c else None)}</td>" for c in cols)
        rows.append(
            f'<tr class="outcomesub"><td>{ds_desc} &mdash; mean {fmt_num(mean_y)}</td>{se_cells}</tr>'
        )
        # pct row (% of mean)
        pct_cells = "".join(
            f"<td>{fmt_pct(c.get('beta') if c else None, mean_y)}</td>" for c in cols)
        rows.append(
            f'<tr class="pct"><td></td>{pct_cells}</tr>'
        )
        # N / clusters row
        n_cells = "".join(
            f"<td>{fmt_n_clusters(c.get('n') if c else None, n_unit)}</td>" for c in cols)
        rows.append(
            f'<tr><td>$N$ / Clusters</td>{n_cells}</tr>'
        )

    if not rows: return ""

    threshold_th = "".join(f"<th>{lbl}</th>" for lbl, _, _ in COLUMNS)
    return f"""
<section>
  <h2>{title}</h2>
  <table>
    <thead>
      <tr>
        <th rowspan="2" class="outcome-span">Outcome variables</th>
        <th colspan="{len(COLUMNS)}" class="sample-span">District sample &mdash; control progression + drop-KTM robustness</th>
      </tr>
      <tr class="threshold-row">
        {threshold_th}
      </tr>
    </thead>
    <tbody>
      {''.join(rows)}
    </tbody>
  </table>
  <p class="eq-cap" style="margin-top:0.6em">
    Reported $\\beta$: $\\mathrm{{fx}}_z \\times \\log(\\mathrm{{mig\\_int}}_z)$.
    Lag-2 FX shifter. SE in (), % of mean in []. *** $p<0.01$, ** $p<0.05$, * $p<0.10$.
    SE clustered at district. A2/A3/A4 add C_mig, C_fx, C_X sequentially.
  </p>
</section>
"""

# ============================================================================
# First-stage slide: 3 periods (rows) x 4 specs (cols)
# Uses FIRST_STAGE loaded from first_stage_future_mig.csv
# ============================================================================
def render_first_stage():
    if not FIRST_STAGE:
        return ""
    # Pivot by (period, model)
    by_pm = {}
    for r in FIRST_STAGE:
        by_pm[(r["period"], r["model"])] = r
    # Order: full window first, then the sub-periods chronologically
    PERIOD_ORDER = ["2011-2022", "2011-2015", "2015-2019", "2019-2022"]
    seen = {r["period"] for r in FIRST_STAGE}
    periods = [p for p in PERIOD_ORDER if p in seen] + \
              sorted(p for p in seen if p not in PERIOD_ORDER)
    col_specs = [("A2", "A2"), ("A3", "A3"),
                 ("A4 (saturated)", "A4"),
                 ("A4, drop KTM",   "A4_dropKTM")]
    rows = []
    for i, per in enumerate(periods, 1):
        cells = [by_pm.get((per, m)) for _, m in col_specs]
        mean_y = next((c["mean_y"] for c in cells if c), None)
        mean_p = next((c.get("mean_permits_per_1000") for c in cells if c), None)
        try: mean_p_disp = f"{float(mean_p):.1f}/1000" if mean_p else ""
        except: mean_p_disp = ""
        # outcomehead
        beta_cells = "".join(
            f"<td>{fmt_beta(c.get('beta') if c else None, c.get('p') if c else None)}</td>"
            for c in cells)
        rows.append(
            f'<tr class="outcomehead">'
            f'<td><strong>({i}) log(permits per 1000), {per}</strong></td>'
            f'{beta_cells}</tr>'
        )
        # outcomesub (mean of log permits + raw mean permits per 1000)
        se_cells = "".join(
            f"<td>{fmt_se(c.get('se') if c else None)}</td>" for c in cells)
        try: my_disp = f"mean log {float(mean_y):.2f}"
        except: my_disp = ""
        rows.append(
            f'<tr class="outcomesub"><td>District cross-section &mdash; {my_disp} &middot; raw {mean_p_disp}</td>'
            f'{se_cells}</tr>'
        )
        # pct (% of mean log)
        pct_cells = "".join(
            f"<td>{fmt_pct(c.get('beta') if c else None, mean_y)}</td>" for c in cells)
        rows.append(f'<tr class="pct"><td></td>{pct_cells}</tr>')
        # N row
        n_cells = "".join(
            f"<td>{fmt_n_clusters(c.get('n') if c else None, c.get('n') if c else None)}</td>"
            for c in cells)
        rows.append(f'<tr><td>$N$ districts</td>{n_cells}</tr>')
    threshold_th = "".join(f"<th>{lbl}</th>" for lbl, _ in col_specs)
    return f"""
<section>
  <h2>First-stage: future DOFE migration</h2>
  <p class="eq-cap" style="margin-bottom:0.4em">
    Outcome: $\\log\\!\\bigl(\\text{{DOFE permits per 1{{,}}000 pop}}\\bigr)$ summed
    over the indicated post-baseline window. Regressor: the same SSIV
    $z_d$ averaged over the window's years. A positive significant
    $\\beta$ says the 2009-10 shares $\\times$ FX shifter construction
    actually predicts where post-baseline migration flows.
  </p>
  <table>
    <thead>
      <tr>
        <th rowspan="2" class="outcome-span">Period (outcome window)</th>
        <th colspan="{len(col_specs)}" class="sample-span">District sample &mdash; control progression + drop-KTM robustness</th>
      </tr>
      <tr class="threshold-row">
        {threshold_th}
      </tr>
    </thead>
    <tbody>
      {''.join(rows)}
    </tbody>
  </table>
  <p class="eq-cap" style="margin-top:0.6em">
    Reported $\\beta$: SSIV $z$ (standardised within period). SE in (),
    \\% of log-mean in []. *** $p<0.01$, ** $p<0.05$, * $p<0.10$.
    HC1 SE (cross-section, one obs per district per period).
    A2 = $z$ + baseline mig intensity; A3 same in cross-section (kept for symmetry);
    A4 adds 6 destination-region shares; col 4 also drops Kathmandu valley.
  </p>
</section>
"""

slides_html = render_first_stage() + "\n" + \
              "\n".join(render_slide(t, ds, g, outs) for t, ds, g, outs in SLIDES)

# ============================================================================
# Title / spec / first-stage slides (modelled exactly on muni)
# ============================================================================
TITLE_AND_SPEC = """
<!-- Title slide -->
<section class="title-slide">
  <h1>District-level results<br/>International migration &amp; modernisation of rural Nepal</h1>
  <p class="authors">Bishal K. Chalise &nbsp;&middot;&nbsp; Ahmed Mushfiq Mobarak</p>
  <p class="pdf-link">
    <a href="?print-pdf" target="_blank" rel="noopener">&darr; Download as PDF</a>
  </p>
</section>

<!-- Shift-share construction -->
<section>
  <h2>Shift-share construction</h2>

  <h3>Step 1 &mdash; baseline destination shares (2009-10 DOFE)</h3>
  \\[
    s_{d,k,0} \\;=\\; \\frac{N_{d,k,\\,2009\\text{-}10}}{\\sum_{k'} N_{d,k',\\,2009\\text{-}10}}
  \\]
  <p class="eq-cap">$N_{d,k,2009\\text{-}10}$ = DOFE labour-permit count from district $d$ to destination $k$ averaged over 2009 and 2010. Shares sum to 1 within district.</p>

  <h3>Step 2 &mdash; destination shifter (annual)</h3>
  \\[
    \\mathrm{fx}_{k,t} \\;=\\; \\log\\!\\bigl(\\mathrm{NPR}/\\mathrm{LCU}_{k,t}\\bigr) - \\log\\!\\bigl(\\mathrm{NPR}/\\mathrm{LCU}_{k,\\,2010}\\bigr)
  \\]
  <p class="eq-cap">Bilateral nominal exchange rate of destination $k$'s currency vs NPR, anchored to 2010 (Khanna-style). Source: IMF FX panel, 2000-2023.</p>

  <h3>Step 3 &mdash; district-year share-weighted shock</h3>
  \\[
    z_{d,t} \\;=\\; \\sum_k s_{d,k,0}\\,\\mathrm{fx}_{k,t}
  \\]
  <p class="eq-cap">Pre-determined 2009-10 destination mix interacted with FX shifters. Standardised within sample before estimation. 26 destinations cover &gt;99% of DOFE permits.</p>
</section>

<!-- Specification -->
<section>
  <h2>Specification</h2>
  \\[
    \\begin{aligned}
      y_{dt} \\;=\\;& \\underbrace{\\beta\\bigl[\\mathrm{fx}_z \\cdot \\log(\\mathrm{mig\\_int}_z)\\bigr]}_{\\text{migration-intensity-scaled FX shock (reported)}}
        \\;+\\; \\underbrace{\\boldsymbol{\\lambda}_1'\\bigl(\\mathrm{mig\\_int}_z\\,\\boldsymbol{\\tau}_t\\bigr)}_{C_{\\text{mig}}\\,:\\,\\text{year}\\times\\text{mig-intensity trend}} \\\\[14pt]
        & +\\; \\underbrace{\\boldsymbol{\\lambda}_2'\\bigl(\\mathrm{fx}_z\\,\\boldsymbol{\\tau}_t\\bigr)}_{C_{\\text{fx}}\\,:\\,\\text{year}\\times\\text{fx trend}}
        \\;+\\; \\underbrace{\\boldsymbol{\\delta}'\\bigl(\\mathbf{X}_{d,0}\\,\\boldsymbol{\\tau}_t\\bigr)}_{C_X\\,:\\,\\text{year}\\times\\text{baseline X (destination-region shares)}} \\\\[14pt]
        & +\\; \\alpha_d \\;+\\; \\gamma_t \\;+\\; \\varepsilon_{dt}
    \\end{aligned}
  \\]
  <p class="eq-cap" style="margin-top:0.9em">
    $\\mathbf{X}_{d,0}$ = 2001 destination-region migrant shares (6 region indicators).
    Entity FE $\\alpha_d$: district. SE clustered at district.
    Columns A2 &rarr; A4 add the control blocks one at a time; column 4 reruns A4 dropping the Kathmandu valley (Kathmandu, Lalitpur, Bhaktapur) as a sample-selection robustness check.
  </p>
</section>
"""

# ============================================================================
# Assemble: head + body wrapper from muni; slides between
# ============================================================================
def render_doc():
    muni = Path("docs/presentation.html").read_text()
    # Take everything up to <div class="slides">
    head_end = muni.index('<div class="slides">') + len('<div class="slides">')
    head = muni[:head_end]
    # Replace title
    head = head.replace(
        "<title>Presentation — Nepal migration / FX shock</title>",
        "<title>District results — Nepal migration / FX shock</title>"
    )
    # Body close: from </div></div> wrapper + script block
    tail_start = muni.rindex("</div>\n</div>")
    tail = muni[tail_start:]
    return head + "\n\n" + TITLE_AND_SPEC + "\n\n" + slides_html + "\n\n" + tail

doc = render_doc()
out_path = "docs/district_results.html"
Path(out_path).write_text(doc)
print(f"Wrote {out_path}: {len(doc):,} chars, {doc.count('<section')} slides")
