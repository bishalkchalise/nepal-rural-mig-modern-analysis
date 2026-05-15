"""
script/audit_consistency.py
Match each main-slide outcome's k>=25 cell against the heterogeneity 'full' cell.
"""
import pandas as pd, re
from pathlib import Path

ROOT = Path(".")
HTML = (ROOT/"docs/presentation.html").read_text(encoding="utf-8")

# Strip het sections (same row classes would confuse the parser)
for beg, end in [("<!-- HET-HH-BEGIN -->","<!-- HET-HH-END -->"),
                 ("<!-- HET-FIRM-BEGIN -->","<!-- HET-FIRM-END -->")]:
    if beg in HTML and end in HTML:
        HTML = HTML.split(beg)[0] + HTML.split(end,1)[1]

# Walk through HTML and pull every outcomehead/outcomesub/pct triplet, in order.
# Use raw <tr ... </tr> matches.
tr_pat = re.compile(r'<tr[^>]*class="(outcomehead|outcomesub|pct)"[^>]*>(.*?)</tr>', re.S)
all_trs = [(m.group(1), m.group(2)) for m in tr_pat.finditer(HTML)]

def cells_of(txt):
    return re.findall(r'<td[^>]*>(.*?)</td>', txt, re.S)
def clean(s):
    return re.sub(r'<.*?>', '', s).replace('$-$','-').replace('−','-').replace('&mdash;','—').strip()

main_rows = []
k = 0
while k < len(all_trs) - 2:
    if all_trs[k][0] == 'outcomehead' \
       and all_trs[k+1][0] == 'outcomesub' \
       and all_trs[k+2][0] == 'pct':
        h = cells_of(all_trs[k][1])
        s = cells_of(all_trs[k+1][1])
        p = cells_of(all_trs[k+2][1])
        # main slides have 5 columns (label + 4 thresholds); het has 4 (label + 3 samples)
        if len(h) == 5 and len(s) == 5 and len(p) == 5:
            lab_m = re.search(r'<strong>([^<]+)</strong>', all_trs[k][1])
            if lab_m:
                label = lab_m.group(1).strip()
                main_rows.append({
                    'label':     label,
                    'mean':      clean(s[0]),
                    'beta_full': clean(h[1]),
                    'beta_k25':  clean(h[2]),
                    'se_k25':    clean(s[2]),
                    'pct_k25':   clean(p[2]),
                })
        k += 3
    else:
        k += 1

print(f"Parsed {len(main_rows)} main-slide outcomes")

# --- DECK_MAP: map het code to main label substring
DECK_MAP = [
    ("absent_hh_share",               "census",  "Migrant-HH share"),
    ("log_remit_amount_intl_12m_rs",  "hh",      "log(intl remit, Rs)"),
    ("log_n_migrants_international",  "hh",      "log(# intl migrants)"),
    ("amen_asset_count_mean",         "census",  "Durables per household"),
    ("housing_foundation_modern",     "census",  "Modern housing"),
    ("amen_toilet_any",               "census",  "Toilet adoption"),
    ("edu_literate",                  "census",  "Literate (pop 6+)"),
    ("edu_literate_female",           "census",  "Literate, female"),
    ("edu_school_attend_6_16",        "census",  "School attendance 6"),
    ("hlt_spend_total",               "hh",      "Health spending — total"),
    ("ind_agri_forestry_fish",        "census",  "(1) Agriculture"),
    ("ind_manufacturing",             "census",  "(2) Manufacturing"),
    ("ind_construction",              "census",  "(3) Construction"),
    ("ind_wholesale_retail",          "census",  "Trade & retail"),
    ("ind_finance_real_estate_prof",  "census",  "Finance"),
    ("ind_public_admin_defence",      "census",  "Public admin"),
    ("occ_share_managers",            "census",  "Managers"),
    ("occ_share_professionals",       "census",  "Professionals"),
    ("occ_share_technicians",         "census",  "Technicians"),
    ("occ_share_service_sales",       "census",  "Service & sales"),
    ("occ_share_craft_trades",        "census",  "Craft & trades"),
    ("occ_share_machine_operators",   "census",  "Machine operators"),
    ("share_self_wet",                "hh",      "Self-cultivated, wet"),
    ("share_self_dry",                "hh",      "Self-cultivated, dry"),
    ("share_both_seasons",            "hh",      "Cultivated both seasons"),
    ("share_fallow_wet",              "hh",      "Fallow, wet"),
    ("share_fallow_dry",              "hh",      "Fallow, dry"),
    ("crop_simpson_diversity",        "hh",      "Crop diversity"),
    ("grows_horticulture",            "hh",      "Grows any horticulture"),
    ("owns_plough",                   "hh",      "Owns plough"),
    ("owns_powered_machinery",        "hh",      "Owns powered farm machinery"),
    ("owns_irrigation_kit",           "hh",      "Owns irrigation equipment"),
    ("log_total_input_cost_rs",       "hh",      "Total input cost"),
    ("log_dry_cost_seed",             "hh",      "Seed cost"),
    ("food_exp_total_7day",           "hh",      "Food spending, weekly"),
    ("food_exp_protein_7day",         "hh",      "Protein spending"),
    ("nonfood_exp_12m",               "hh",      "Non-food spending"),
    ("nonfood_clothing_footwear_12m", "hh",      "Clothing & footwear"),
    ("nonfood_fuel_lighting_12m",     "hh",      "Fuel & lighting"),
    ("food_insec_score",              "hh",      "Food insecurity score"),
    ("log_new_firms",                 "nec_panel", "Total new firms"),
    ("log_new_firms_size_1_worker",   "nec_panel", "Size: 1-worker"),
    ("log_new_firms_size_2_9_workers","nec_panel", "Size: 2–9"),
    ("log_new_firms_size_10_50_workers","nec_panel","Size: 10–50"),
    ("log_new_firms_manufacturing",   "nec_panel", "Industry: Manufacturing"),
    ("log_new_firms_trade_retail",    "nec_panel", "Industry: Trade"),
    ("log_new_firms_hospitality_food","nec_panel", "Industry: Hospitality"),
    ("log_n_firms",                   "nec_cs",  "log(# firms)"),
    ("log_emp_total",                 "nec_cs",  "log(total employment)"),
    ("log_rev_total",                 "nec_cs",  "log(revenue)"),
    ("log_cap_total",                 "nec_cs",  "log(capital stock)"),
    ("industry_diversity",            "nec_cs",  "Industry diversity"),
    ("share_female_led",              "nec_cs",  "Share female-led"),
]

hetn = pd.read_csv("output/tab/curated_heterogeneity_net.csv")
fnet = pd.read_csv("output/tab/firm_heterogeneity_net.csv")
hetn = hetn[(hetn['sample']=='full') & (hetn['threshold']==25)]
fnet = fnet[(fnet['sample']=='full') & (fnet['threshold']==25)]

def het_lookup(code, source):
    if source in ("census","hh"):
        m = hetn[(hetn['dataset']==source) & (hetn['outcome']==code)]
    else:
        m = fnet[(fnet['dataset']==source) & (fnet['outcome']==code)]
    if m.empty: return None
    r = m.iloc[0]
    return dict(beta=float(r['beta']), se=float(r['se']),
                pval=float(r['pval']), mean=float(r['mean_y']),
                n=int(r['n']))

# Compare
rows = []
for code, source, lab in DECK_MAP:
    main = next((m for m in main_rows if lab in m['label']), None)
    h = het_lookup(code, source)
    rows.append({
        'code': code, 'source': source,
        'main_label': main['label'] if main else '(not found)',
        'main_β_k25': main['beta_k25'] if main else '',
        'main_SE_k25': main['se_k25'] if main else '',
        'main_mean': main['mean'] if main else '',
        'het_β': f"{h['beta']:+.4f}" if h else '',
        'het_SE': f"{h['se']:.4f}" if h else '',
        'het_mean': f"{h['mean']:.3f}" if h else '',
        'het_pval': f"{h['pval']:.3f}" if h else '',
        'het_n':    h['n'] if h else '',
    })

df = pd.DataFrame(rows)

# Try numeric comparison
def num(s):
    if not s: return None
    s = re.sub(r'<.*?>','', str(s))
    s = s.replace(',','').replace('Rs','').replace('$','').strip()
    s = re.sub(r'\s*\(.*\)','', s)
    s = s.replace('−','-').replace('−','-')
    try: return float(s)
    except: return None

df['m_beta'] = df['main_β_k25'].apply(num)
df['h_beta_num'] = df['het_β'].apply(lambda x: float(x) if x else None)
df['match_beta'] = df.apply(lambda r: (
    "OK" if (r['m_beta'] is not None and r['h_beta_num'] is not None
             and abs(r['m_beta']-r['h_beta_num'])/max(abs(r['m_beta']),1e-6) < 0.05) else "DIFF"
), axis=1)

# Mean comparison
df['m_mean_num'] = df['main_mean'].apply(num)
df['h_mean_num'] = df['het_mean'].apply(lambda x: float(x) if x else None)
def mean_match(r):
    if r['m_mean_num'] is None or r['h_mean_num'] is None: return ""
    if abs(r['m_mean_num']) < 1e-9: return "OK" if abs(r['h_mean_num']) < 1e-9 else "DIFF"
    diff_pct = abs(r['m_mean_num']-r['h_mean_num'])/abs(r['m_mean_num'])*100
    return "OK" if diff_pct < 5 else f"DIFF {diff_pct:.0f}%"
df['match_mean'] = df.apply(mean_match, axis=1)

pd.set_option("display.width", 220); pd.set_option("display.max_colwidth", 35)
print(df[['code','source','main_β_k25','het_β','match_beta','main_mean','het_mean','match_mean','het_n','het_pval']].to_string(index=False))

out = ROOT/"output/tab/audit_consistency.csv"
df.to_csv(out, index=False)
print(f"\nSaved {out}")

# Print clean summary
print("\n========== INCONSISTENCIES ==========")
bad = df[(df['match_beta']=='DIFF') | (df['match_mean'].str.startswith('DIFF', na=False))]
if bad.empty:
    print("None.")
else:
    print(bad[['code','main_β_k25','het_β','main_mean','het_mean','match_beta','match_mean']].to_string(index=False))
