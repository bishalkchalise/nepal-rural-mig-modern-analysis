"""
Generate two Reveal.js slides for heterogeneity (HH/census + firm) from
the four heterogeneity CSVs.  Each slide has:
  - vertical side tabs to pick outcome block
  - a top toggle to switch between net-flow and rural/urban splits
  - per-tab + per-sample table at threshold = 25 (anchor)

Output: writes the two <section>...</section> blocks into
docs/presentation.html, replacing any existing block between the markers
<!-- HET-SLIDES-BEGIN --> and <!-- HET-SLIDES-END -->.
"""
import pandas as pd
from pathlib import Path

ROOT  = Path(".")
DOCS  = ROOT / "docs/presentation.html"

# -------------------------------------------------------------- labels & blocks
# (outcome code, slide label, mean string for sub-row, value type)
#   value type: 'pct' = share x100 with 1 dp, 'rs' = rupee integer, 'log' = 3 dp
# Labels may include HTML tooltip attribute (title=) for hover-explanations.

TT_SIMPSON = ("Simpson crop diversity: 1 − Σpᵢ². "
              "0 = monoculture; ~0.9 = perfectly balanced across crops. "
              "Mean ≈0.22 means most HH concentrate in 1–2 crops.")
TT_HHI     = ("Industry diversity = 1 − HHI on muni-level industry employment shares. "
              "0 = all employment in one industry; close to 1 = perfectly diversified.")
TT_HFIAS   = ("Household Food Insecurity Access Scale (HFIAS): 9 indicator questions × 4 frequency levels "
              "(0–3 each), summed to 0–27. Median household scores 0 in most populations; "
              "any positive value indicates experienced food insecurity.")
HH_BLOCKS = [
    ("firststage", "First-stage", [
        ("absent_hh_share",                 "Migrant-HH share",                 ".24",      "pct"),
        ("log_remit_amount_intl_12m_rs",    "log(intl remit, Rs)",              "7.30",     "log"),
        ("log_n_migrants_international",    "log(# intl migrants)",             "0.53",     "log"),
    ]),
    ("investment", "Investment", [
        ("amen_asset_count_mean",           "Durables per household",           "2.89",     "log"),
        ("housing_foundation_modern",       "Modern housing",                   ".28",      "pct"),
        ("amen_toilet_any",                 "Toilet adoption",                  ".66",      "pct"),
    ]),
    ("humancap", "Human capital", [
        ("edu_literate",                    "Literate (pop 6+)",                ".64",      "pct"),
        ("edu_literate_female",             "Literate, female",                 ".55",      "pct"),
        ("edu_school_attend_6_16",          "School attendance 6–16",           ".84",      "pct"),
        ("hlt_spend_total",                 "Health spending — total (Rs)",     "Rs 12,401","rs"),
    ]),
    ("sector", "Sectors", [
        ("ind_agri_forestry_fish",          "Agriculture (primary)",            ".70",      "pct"),
        ("ind_manufacturing",               "Manufacturing (secondary)",        ".05",      "pct"),
        ("ind_construction",                "Construction (secondary)",         ".04",      "pct"),
        ("ind_wholesale_retail",            "Trade & retail (tertiary)",        ".07",      "pct"),
        ("ind_finance_real_estate_prof",    "Finance / RE / prof. (tertiary)",  ".013",     "pct"),
        ("ind_public_admin_defence",        "Public admin (tertiary)",          ".017",     "pct"),
    ]),
    ("occup", "Occupations", [
        ("occ_share_managers",              "Managers (high-skill)",            ".021",     "pct"),
        ("occ_share_professionals",         "Professionals (high-skill)",       ".034",     "pct"),
        ("occ_share_technicians",           "Technicians (mid-high)",           ".014",     "pct"),
        ("occ_share_service_sales",         "Service & sales (mid)",            ".057",     "pct"),
        ("occ_share_craft_trades",          "Craft & trades (manual)",          ".064",     "pct"),
        ("occ_share_machine_operators",     "Machine operators (manual)",       ".018",     "pct"),
    ]),
    ("landuse", "Land use", [
        ("share_self_wet",                  "Own-cultivated land, wet season",  ".84",      "pct"),
        ("share_self_dry",                  "Own-cultivated land, dry season",  ".65",      "pct"),
        ("share_both_seasons",              "Cultivated both seasons",          ".65",      "pct"),
        ("share_fallow_wet",                "Fallow land, wet season",          ".08",      "pct"),
        ("share_fallow_dry",                "Fallow land, dry season",          ".28",      "pct"),
        ("crop_simpson_diversity",          f"<span title='{TT_SIMPSON}' style='border-bottom:1px dotted #2c5282;cursor:help'>Crop diversity (Simpson)</span>", ".22", "pct"),
        ("grows_horticulture",              "Grows any horticulture crop",      ".38",      "pct"),
    ]),
    ("inputs", "Input use", [
        ("owns_plough",                     "Owns plough",                      ".52",      "pct"),
        ("owns_powered_machinery",          "Owns powered farm machinery",      ".03",      "pct"),
        ("owns_irrigation_kit",             "Owns irrigation equipment",        ".11",      "pct"),
        ("log_total_input_cost_rs",         "Total input cost (log Rs)",        "6.35",     "log"),
        ("log_dry_cost_seed",               "Seed cost, dry season (log Rs)",   "1.94",     "log"),
    ]),
    ("consump", "Consumption", [
        ("food_exp_total_7day",             "Food spending, weekly (Rs)",       "Rs 2,282", "rs"),
        ("food_exp_protein_7day",           "Protein spending, weekly (Rs)",    "Rs 724",   "rs"),
        ("nonfood_exp_12m",                 "Non-food spending, annual (Rs)",   "Rs 97,932","rs"),
        ("nonfood_clothing_footwear_12m",   "Clothing & footwear, annual (Rs)", "Rs 12,151","rs"),
        ("nonfood_fuel_lighting_12m",       "Fuel & lighting, annual (Rs)",     "Rs 4,760", "rs"),
        ("food_insec_score",                f"<span title='{TT_HFIAS}' style='border-bottom:1px dotted #2c5282;cursor:help'>Food insecurity score (HFIAS, 0–27)</span>", "0.77", "log"),
    ]),
]
FIRM_BLOCKS = [
    ("entry", "Firm entry 2001–18", [
        ("log_new_firms",                       "Total new firms",              "3.35", "log"),
        ("log_new_firms_size_1_worker",         "Size: 1-worker",               "2.40", "log"),
        ("log_new_firms_size_2_9_workers",      "Size: 2–9 workers",            "2.81", "log"),
        ("log_new_firms_size_10_50_workers",    "Size: 10–50 workers",          "0.74", "log"),
        ("log_new_firms_manufacturing",         "Industry: Manufacturing",      "1.50", "log"),
        ("log_new_firms_trade_retail",          "Industry: Trade & retail",     "2.70", "log"),
        ("log_new_firms_hospitality_food",      "Industry: Hospitality & food", "1.52", "log"),
    ]),
    ("structure", "Firm structure 2018 (cross-section)", [
        ("log_n_firms",                         "log(# firms)",                 "6.82", "log"),
        ("log_emp_total",                       "log(total employment)",        "7.92", "log"),
        ("log_rev_total",                       "log(revenue)",                 "21.25","log"),
        ("log_cap_total",                       "log(capital stock)",           "21.47","log"),
        ("industry_diversity",                  f"<span title='{TT_HHI}' style='border-bottom:1px dotted #2c5282;cursor:help'>Industry diversity (1 − HHI)</span>", ".67", "pct"),
        ("share_female_led",                    "Share female-led",             ".35",  "pct"),
    ]),
]

# -------------------------------------------------------------- formatters
def stars(p):
    if pd.isna(p): return ""
    if p < .01: return "***"
    if p < .05: return "**"
    if p < .10: return "*"
    return ""

def fmt_beta(b, p, kind):
    if pd.isna(b): return "."
    s = stars(p)
    if kind == "pct":
        return f"{b*100:+.2f}<span class='stars'>{s}</span>"
    if kind == "rs":
        return f"{b:+,.0f}<span class='stars'>{s}</span>"
    return f"{b:+.3f}<span class='stars'>{s}</span>"

def fmt_se(se, kind):
    if pd.isna(se): return ""
    if kind == "pct":
        return f"({se*100:.2f})"
    if kind == "rs":
        return f"({se:,.0f})"
    return f"({se:.3f})"

def fmt_effect_row(b, mean, kind):
    """Per-cell content for the row under the beta:
       - 'pct'  -> percentage-point change   '[+6.3pp]'
       - 'log'  -> approx percent change       '[+49.7%]'
       - 'rs'   -> Rs amount as % of mean      '[+22%]'
    """
    if pd.isna(b): return ""
    if kind == "pct":
        return f"[{b*100:+.1f}pp]"
    if kind == "log":
        return f"[{b*100:+.0f}%]"
    if pd.isna(mean) or mean == 0:
        return ""
    return f"[{100*b/mean:+.0f}%]"

# -------------------------------------------------------------- table builder
def build_table(df, sample_labels, block, slide_id, sample_key):
    # Map raw labels to header text — "Net-inflow" / "Net-outflow" become
    # "Internal in-flow" / "Internal out-flow" so the audience never has to
    # remember which "net" means which.
    HEADER = {
        "Net-inflow":  "Internal in-flow",
        "Net-outflow": "Internal out-flow",
    }
    sample_labels_disp = [HEADER.get(l, l) for l in sample_labels]
    """One <table> for one (slide, tab, sample-set) combination."""
    block_key, _, outcomes = block
    table_id = f"{slide_id}-{block_key}-{sample_key}"

    header_cols = "".join(f"<th>{lbl}</th>" for lbl in sample_labels_disp)
    rows = []
    for code, label, mean_str, kind in outcomes:
        sub = df[df['outcome'] == code]
        if sub.empty:
            continue
        # build three sample cells (assume order ['full', A, B])
        beta_cells, se_cells, pct_cells = [], [], []
        for samp in sample_labels_to_csv_samples(sample_labels):
            r = sub[sub['sample'] == samp]
            if r.empty:
                beta_cells.append("<td>.</td>"); se_cells.append("<td class='se'></td>"); pct_cells.append("<td></td>")
                continue
            b = float(r['beta'].iloc[0]); p = float(r['pval'].iloc[0])
            se= float(r['se'].iloc[0]);   m = float(r['mean_y'].iloc[0])
            beta_cells.append(f"<td>{fmt_beta(b,p,kind)}</td>")
            se_cells.append(f"<td class='se'>{fmt_se(se,kind)}</td>")
            pct_cells.append(f"<td>{fmt_effect_row(b,m,kind)}</td>")
        rows.append(
            f"<tr class='outcomehead'><td><strong>{label}</strong></td>{''.join(beta_cells)}</tr>"
            f"<tr class='outcomesub'><td>mean {mean_str}</td>{''.join(se_cells)}</tr>"
            f"<tr class='pct'><td></td>{''.join(pct_cells)}</tr>"
        )
    table = (
        f"<table id='{table_id}' class='het-table'>"
        f"<thead><tr><th style='width:42%'>Outcome</th>{header_cols}</tr></thead>"
        f"<tbody>{''.join(rows)}</tbody>"
        f"</table>"
    )
    return table

def sample_labels_to_csv_samples(labels):
    """Map header labels back to CSV sample codes."""
    mapping = {
        "Full": "full",
        "Net-inflow": "receiver",
        "Net-outflow": "sender",
        "Rural": "rural",
        "Urban": "urban",
    }
    return [mapping[l] for l in labels]

# -------------------------------------------------------------- slide builder
def build_slide(slide_id, title, blocks, csv_net, csv_urb, footnote):
    # subset both CSVs to k=25
    net = csv_net[csv_net['threshold'] == 25]
    urb = csv_urb[csv_urb['threshold'] == 25]

    # tab buttons
    tab_btns = []
    for i, (key, label, _) in enumerate(blocks):
        cls = "het-tab-btn" + (" active" if i == 0 else "")
        tab_btns.append(f"<button class='{cls}' data-tab='{key}' onclick=\"hetSetTab('{slide_id}','{key}')\">{label}</button>")
    tabs_html = "<div class='het-tabs'>" + "".join(tab_btns) + "</div>"

    # sample toggle
    sample_toggle = (
        f"<div class='het-sample-toggle'>"
        f"  <button class='het-sample-btn active' data-sample='net' onclick=\"hetSetSample('{slide_id}','net')\">Internal migration (net inflow / outflow)</button>"
        f"  <button class='het-sample-btn' data-sample='urb' onclick=\"hetSetSample('{slide_id}','urb')\">Rural / Urban</button>"
        f"</div>"
    )

    # tables: one per (tab × sample)
    tables = []
    for block in blocks:
        key = block[0]
        tables.append(build_table(net, ["Full","Net-inflow","Net-outflow"], block, slide_id, "net"))
        tables.append(build_table(urb, ["Full","Rural","Urban"],            block, slide_id, "urb"))
    tables_html = "<div class='het-content'>" + "".join(tables) + "</div>"

    sect = f'''<section id="{slide_id}" class="het-slide" data-tab="{blocks[0][0]}" data-sample="net">
  <h2>{title}</h2>
  {sample_toggle}
  <div class='het-grid'>
    {tabs_html}
    {tables_html}
  </div>
  <p class="eq-cap het-foot">{footnote}</p>
</section>'''
    return sect


# -------------------------------------------------------------- CSS / JS
EXTRA_HEAD = '''
<style>
  .het-grid { display: grid; grid-template-columns: 170px 1fr; gap: 36px; align-items: start; }
  .het-tabs {
    display: flex; flex-direction: column; gap: 4px;
    padding: 8px 12px 8px 0;
    border-right: 1px solid #cbd5e0;
  }
  .het-tab-btn, .het-sample-btn {
    padding: 6px 10px; font-size: 0.52em; line-height: 1.2;
    border: 1px solid transparent; background: transparent; color: #1a202c;
    cursor: pointer; font-family: inherit; text-align: left;
    border-radius: 3px;
  }
  .het-tab-btn:hover, .het-sample-btn:hover { background: #edf2f7; }
  .het-tab-btn.active, .het-sample-btn.active {
    background: #2c5282; color: white; border-color: #2c5282;
  }
  .het-sample-toggle {
    display: flex; gap: 4px; margin: 0 0 10px 0;
    padding-bottom: 8px; border-bottom: 1px solid #e2e8f0;
  }
  .het-sample-btn { padding: 4px 12px; border: 1px solid #cbd5e0; }
  .het-content .het-table { display: none; font-size: 0.42em; margin: 0; width: 100%; border-collapse: collapse; }
  .het-content .het-table.active { display: table; }
  .het-content .het-table td, .het-content .het-table th {
    padding: 1.5px 6px !important; line-height: 1.15; vertical-align: baseline;
  }
  .het-content .het-table tr.outcomesub td { padding-top: 0 !important; padding-bottom: 0 !important; }
  .het-content .het-table tr.pct td        { padding-top: 0 !important; padding-bottom: 4px !important; }
  .het-foot { font-size: 0.30em !important; margin-top: 12px; line-height: 1.3; }
  .reveal .het-slide h2 { margin-bottom: 8px; }
</style>
<script>
  function hetUpdate(slideId) {
    const slide = document.getElementById(slideId);
    if (!slide) return;
    const tab = slide.dataset.tab, samp = slide.dataset.sample;
    slide.querySelectorAll('.het-table').forEach(t => t.classList.remove('active'));
    const target = slide.querySelector('#' + slideId + '-' + tab + '-' + samp);
    if (target) target.classList.add('active');
    slide.querySelectorAll('.het-tab-btn').forEach(b => b.classList.toggle('active', b.dataset.tab === tab));
    slide.querySelectorAll('.het-sample-btn').forEach(b => b.classList.toggle('active', b.dataset.sample === samp));
  }
  function hetSetTab(slideId, key)    { document.getElementById(slideId).dataset.tab    = key;  hetUpdate(slideId); }
  function hetSetSample(slideId, key) { document.getElementById(slideId).dataset.sample = key;  hetUpdate(slideId); }
  document.addEventListener('DOMContentLoaded', () => {
    ['hetslide-hh','hetslide-firm'].forEach(hetUpdate);
  });
  if (window.Reveal) { Reveal.on('slidechanged', () => ['hetslide-hh','hetslide-firm'].forEach(hetUpdate)); }
</script>
'''

FOOTNOTE = (
    'All coefficients at $k\\geq 25$ baseline migrants per muni, anchor spec '
    '(treatment + year × mig_int + year × fx + year × Block A; muni + year FE). '
    'SE in parens (cluster: muni); *** $p<.01$, ** $p<.05$, * $p<.10$. '
    '<strong>Effect column</strong> [ ]: percentage points (<em>pp</em>) for share outcomes, '
    'approximate % change for log outcomes, % of sample mean for rupee amounts. '
    '<strong>Means</strong> shown are the regression-sample (k ≥ 25) means, not the raw-data population; '
    'k=25 munis are wealthier and more migration-exposed than the average Nepalese muni. '
    '<strong>Internal migration:</strong> classification uses 2001 census <em>internal</em> migration flows. '
    'Internal in-flow muni = more people moved <em>into</em> the muni from elsewhere in Nepal than moved out; '
    'internal out-flow = reverse. Fixed at 2001 baseline (predetermined). '
    '<strong>Rural / Urban:</strong> rural = Gaunpalika; '
    'urban = Nagar/Upamahanagar/Mahanagarpalika. '
    'NEC panel covers founding years 2001–2018 (annual); NEC cross-section is 2018 stock.'
)

# -------------------------------------------------------------- main
def main():
    net = pd.read_csv("output/tab/curated_heterogeneity_net.csv")
    urb = pd.read_csv("output/tab/curated_heterogeneity_urb.csv")
    fnet= pd.read_csv("output/tab/firm_heterogeneity_net.csv")
    furb= pd.read_csv("output/tab/firm_rural_urban.csv")

    # combine census + hh for the HH/census slide
    net['source'] = net['dataset']; urb['source'] = urb['dataset']
    fnet['source'] = fnet['dataset']; furb['source'] = furb['dataset']

    h1 = build_slide("hetslide-hh",   "Heterogeneity: household & population",
                     HH_BLOCKS, net, urb, FOOTNOTE)
    h2 = build_slide("hetslide-firm", "Heterogeneity: firm side",
                     FIRM_BLOCKS, fnet, furb, FOOTNOTE)

    block = "\n\n<!-- HET-SLIDES-BEGIN -->\n" + EXTRA_HEAD + "\n" + h1 + "\n\n" + h2 + "\n<!-- HET-SLIDES-END -->\n"

    html = DOCS.read_text(encoding='utf-8')
    BEG = "<!-- HET-SLIDES-BEGIN -->"
    END = "<!-- HET-SLIDES-END -->"
    if BEG in html and END in html:
        before = html.split(BEG)[0]
        after  = html.split(END, 1)[1]
        html = before + block + after
    else:
        # insert before the final </div></div> that closes reveal slides
        anchor = "</section>\n\n</div>"
        if anchor in html:
            html = html.replace(anchor, "</section>\n" + block + "\n</div>", 1)
        else:
            # fall back: append at very end before </body>
            html = html.replace("</body>", block + "\n</body>")

    DOCS.write_text(html, encoding='utf-8')
    print(f"Wrote heterogeneity slides into {DOCS}")
    print(f"  H1 outcome blocks: {[b[0] for b in HH_BLOCKS]}")
    print(f"  H2 outcome blocks: {[b[0] for b in FIRM_BLOCKS]}")

if __name__ == "__main__":
    main()
