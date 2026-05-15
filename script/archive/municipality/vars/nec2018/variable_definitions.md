# NEC 2018 — Variable Definitions

Every output column, grouped by deliverable, with its definition and the
literature source for each classification rule.

## Pipeline

Scripts live in `script/nec2018/`. Run in order from the project root:

```r
source("script/nec2018/01_classify_nsic.R")        # NSIC lookup map
source("script/nec2018/02_firm_level_prep.R")      # cleans + derives
source("script/nec2018/03_municipality_wide.R")    # main deliverable 1
source("script/nec2018/04_entry_cohort_panel.R")   # main deliverable 2
source("script/nec2018/05_district_aggregate.R")   # district roll-up
```

Outputs go to `data/clean/nec2018/`.

Input: `data/raw/Economic Census 2018/NEC_2018.dta` (923,356 firms, 97 cols).

---

## Classification schemes (from 01_classify_nsic.R)

Four independent classifications applied at NSIC 2-digit (division) level
to every firm. Each firm gets all four tags — they're different lenses on
the same universe.

### Scheme 1: Tradability

Column `tradability`. Six categories.

| Value | NSIC sections | Rationale |
|---|---|---|
| `tradable_goods` | A (agro), B (mining), C (manufacturing) | Goods ship across regions and borders |
| `tradable_services` | H (transport), J (info/comm), K (finance), M (prof services) | Functionally tradable services per Gervais (2014) |
| `non_tradable_services` | G (retail), I (hospitality), L (real estate), N (admin sup), R (arts), S (other services), T (hh prod), F (construction), P (education), Q (health) | Served where the customer is |
| `non_tradable_utilities` | D (electricity), E (water) | Local network goods |
| `public_admin` | O | Compulsory public sector |
| `extra_territorial` | U | Embassies, international orgs |

**Citations**:
- Mian, A. and Sufi, A. (2014). "What Explains the 2007–2009 Drop in Employment?" *Econometrica* 82(6): 2197–2223.
- Gervais, A. (2014). "Triangle inequalities and the absence of trade." *Journal of International Economics* 92(2): 222–236.

### Scheme 2: Agricultural orientation

Column `ag_orientation`. Distinguishes upstream primary activity from downstream agro-linked manufacturing.

| Value | NSIC 2-digit codes | Rationale |
|---|---|---|
| `crop_livestock_primary` | 01 | Combined crop + animal production. At 2-digit, subsistence vs market cannot be separated. |
| `forestry_primary` | 02 | Forestry and logging |
| `fishery_primary` | 03 | Fishing and aquaculture |
| `agro_processing` | 10, 11, 12 | Food, beverages, tobacco — first-stage transformation |
| `agro_downstream_manuf` | 13, 14, 15, 16 | Textiles, apparel, leather, wood — looser backward linkage to ag |
| `not_ag` | everything else | Non-ag activity |

**Citation**: Reardon, T., Timmer, C.P., Barrett, C.B., and Berdegué, J. (2012). "The Rapid Rise of Supermarkets in Developing Countries." *Annual Review of Resource Economics* 4: 39–63.

**Caveat**: Subsistence vs market-oriented cannot be separated at NSIC 2-digit level. To distinguish, use 3-digit NSIC (`NSIC_3digt` in raw data) or cross-tag with firm size and formality: `ag_orientation == "crop_livestock_primary"` AND `size_cat == "micro_1"` AND `is_registered == 0` is a reasonable subsistence proxy.

### Scheme 3: Manufacturing technology tier

Column `manuf_tier`. OECD-style tech classification adapted for developing countries.

| Value | NSIC 2-digit codes | Rationale |
|---|---|---|
| `low_tech` | 10, 11, 12, 13, 14, 15, 16, 17, 18, 31, 32 | Food, textiles, wood, paper, printing, furniture, other mfg |
| `medium_low_tech` | 19, 22, 23, 24, 25, 33 | Coke/petro, rubber/plastics, non-metallic mineral, basic/fabricated metals, machinery repair |
| `medium_high_tech` | 20, 27, 28, 29, 30 | Chemicals, electrical eq, machinery, motor vehicles, other transport eq |
| `high_tech` | 21, 26 | Pharmaceuticals, computer/electronics/optical |
| `not_manuf` | outside NSIC section C | Not manufacturing |

**Citations**:
- Hatzichronoglou, T. (1997). "Revision of the High-Technology Sector and Product Classification." *OECD STI Working Paper*.
- Galindo-Rueda, F. and Verger, F. (2016). "OECD Taxonomy of Economic Activities Based on R&D Intensity." *OECD STI Working Paper*.
- Lall, S. (2000). "The Technological Structure and Performance of Developing Country Manufactured Exports, 1985–98." *Oxford Development Studies* 28(3): 337–369.

**Handicraft caveat**: Not a distinct NSIC code. Spans low_tech divisions 13, 14, 15, 16, 32. Cross-tag as `manuf_tier == "low_tech"` AND `size_cat %in% c("micro_1", "small_2_9")` AND `is_registered == 0`.

### Scheme 4: Modernity (Lewis dual-economy)

Column `modernity`. Modern / traditional / public split.

| Value | NSIC codes / sections | Rationale |
|---|---|---|
| `modern_services` | K (finance), M (prof services), J (info/comm), 62 (IT) | Formal, contract-based, high-productivity |
| `modern_manuf` | 21, 26, 27, 28 | Modern manufacturing |
| `industrial_sector` | C (mfg, except modern), F (construction), D (utilities) | General industrial activity |
| `traditional_agriculture` | 01, 02 | Subsistence-heavy primary sector |
| `traditional_commerce` | G (retail), I (hospitality), T (hh prod) | Classic informal-sector activities |
| `traditional_services` | 95, 96 | Repair and personal services |
| `public_sector` | O, P, Q | Government-dominated in Nepal |
| `other` | residual | Extra-territorial etc. |

**Citations**:
- Lewis, W.A. (1954). "Economic Development with Unlimited Supplies of Labour." *The Manchester School* 22(2): 139–191.
- La Porta, R. and Shleifer, A. (2014). "Informality and Development." *Journal of Economic Perspectives* 28(3): 109–126.

---

## Firm-level columns (`firm_level.csv`)

### Identifiers

| Column | Source |
|---|---|
| `UNIQID` | NEC unique firm identifier |
| `lgcode` | First 5 characters of UNIQID — local government (municipality / rural municipality) code |
| `DIST` | NEC district code (1–75 with post-2015 E/W splits for Nawalparasi, Rukum = 77 total) |

### NSIC

| Column | Description |
|---|---|
| `NSIC_SEC` | ISIC Rev. 4 section letter (A–U) |
| `NSIC_2digt` | 2-digit division |
| `sector_short` | Short tag (agro, manuf, wholesale, ...) |

### Size

| Column | Definition |
|---|---|
| `pe_tot` | PE1TOT — total persons engaged |
| `pe_nm`, `pe_nf` | Nepali male / female workers |
| `pe_fm`, `pe_ff` | Foreign male / female workers |
| `size_cat` | `micro_1` (=1), `small_2_9` (2–9), `medium_10_50` (10–50), `large_51p` (51+) |

### Finance (annualized; raw IE1/IE2/IE21 are monthly averages)

| Column | Definition |
|---|---|
| `rev_annual` | IE1 × 12 — annual gross revenue (NRs.) |
| `exp_annual` | IE2 × 12 — annual operating expenses |
| `sal_annual` | IE21 × 12 — annual wage bill |
| `cap_total` | CI1 — total capital stock |
| `cap_fixed` | CI12 — fixed capital (land, buildings, plant) |
| `cap_foreign_ratio` | CI11 — share of capital from foreign sources (0–1) |
| `value_added` | `rev_annual - exp_annual` — imperfect proxy (IE2 includes both intermediate inputs and labor) |

### Productivity

| Column | Definition |
|---|---|
| `labor_productivity` | Revenue per worker: `rev_annual / pe_tot` |
| `value_added_pw` | Value added per worker: `value_added / pe_tot` |
| `capital_intensity` | Capital per worker: `cap_total / pe_tot` |
| `capital_productivity` | Revenue per capital: `rev_annual / cap_total` |
| `wage_share_of_exp` | `sal_annual / exp_annual` — labor share of costs |
| `profit_margin` | `value_added / rev_annual` |

### Behavior flags (0/1, NA where not applicable)

| Column | Source |
|---|---|
| `is_registered` | RI1 == 1 |
| `is_tax_registered` | RI2 == 1 |
| `keeps_accounts` | AR1 == 1 |
| `operates_year_round` | BO4 == 1 |
| `has_borrowed` | AC1 == 1 |
| `uses_formal_credit` | AC1 == 1 AND AC2 ∈ {bank, finance co, microfinance, coop} |
| `is_incorporated` | LS1 ∈ {3, 4} (Pvt. Ltd. or Public Ltd.) |
| `is_sole_prop` | LS1 == 1 |
| `is_cooperative` | LS1 == 5 |
| `is_multinational` | LS2 == 1 |
| `female_manager` | MO1 == 2 |
| `female_owner` | MO2 == 2 |
| `female_led` | pmax(female_manager, female_owner) |
| `has_foreign_capital` | cap_foreign_ratio > 0 |
| `has_branches` | HO1 > 0 |
| `has_parent` | PC1 == 1 |
| `owns_building` | BP1 == 1 |
| `owns_land` | BP2 == 1 |

### Age / cohort

| Column | Definition |
|---|---|
| `founding_year_ad` | BO8Y converted to AD. Detection: 1900–2025 treated as AD; 2026–2090 as BS (converted via `round(bs - 56.7)`) |
| `firm_age_years` | `2018 - founding_year_ad` |
| `cohort_5yr` | `pre_1985`, `1985_1989`, ..., `2015_2018` |
| `is_young_firm` | 1 if founded within 5 years of 2018 |

**BS/AD detection caveat**: The overlap zone 2000–2025 defaults to AD. After running 02, check the founding_year_ad distribution — median should be around 2005–2010. If median is unreasonably high or max exceeds 2018, a more conservative AD-threshold may be needed.

---

## Municipality-level wide file (`municipality_analysis.csv`)

One row per `lgcode` (~750 units). Column families:

- **Core**: `n_firms`, `emp_total`, `emp_nepali_*`, `emp_foreign_*`
- **Formalization**: `share_registered`, `share_tax_registered`, `share_keeps_accounts`, `share_incorporated`, `share_operates_yr_round`, `formality_index`
- **Size distribution**: `n_firms_size_*`, `share_firms_size_*`, `emp_size_*`, `share_emp_size_*` (4 buckets × 4 metrics)
- **Sector composition**: `n_firms_sec_*`, `share_firms_sec_*`
- **Classification shares**: `n_trd_*`, `share_trd_*`, `n_agorient_*`, `share_agorient_*`, `n_mtier_*`, `share_within_manuf_mtier_*`, `n_modern_*`, `share_modern_*`
- **Sector × size crosstab**: `n_firms_secsize_{sector}_{size}`, `share_in_mun_secsize_*`
- **Finance**: `share_borrowed_any`, `share_formal_credit`, `interest_p50/p90`, `rev_*`, `cap_*`
- **Productivity**: `labor_prod_median/p90`, `value_added_pw_*`, `capital_intensity_median`, `wage_share_median`, `profit_margin_median`
- **Gender**: `share_female_manager/owner/led/workers`, `gender_inclusion_index`
- **Firm age**: `share_firms_young_5y`, `share_firms_mature_10y`, `median_firm_age`, `p90_firm_age`
- **Tenure/structure**: `share_owns_building`, `share_owns_land`, `share_has_branches`, `share_has_parent`, `share_multinational`
- **DIST**: majority district code per municipality (for joining to district-level data)

---

## Entry cohort panel (`entry_cohort_panel.csv`)

Long format: one row per `(lgcode, founding_year_ad)`. Window: 1980–2018.

| Column | Definition |
|---|---|
| `lgcode`, `founding_year_ad` | Keys |
| `n_firms_surviving` | Count of firms founded that year in that municipality, still operating in 2018 |
| `emp_surviving` | Sum of PE1TOT across those firms |
| `rev_surviving`, `cap_surviving` | Sum of annual revenue and capital stock |
| `median_firm_age_years` | Redundant with `2018 - founding_year_ad`, kept for convenience |
| `n_firms_surviving_size_{size}` | By size bucket |
| `n_firms_surviving_sec_{sector}` | By short sector tag |
| `n_firms_surviving_trd_{tradability}` | By tradability |
| `n_firms_surviving_agor_{ag_orient}` | By ag orientation |
| `n_firms_surviving_mtier_{manuf_tier}` | By manufacturing tier (manufacturing firms only) |
| `n_firms_surviving_modern_{modernity}` | By modernity |

**IMPORTANT CAVEAT — survivor bias**: These are surviving stocks from each founding-year cohort, NOT true entry counts. Firms that exited before 2018 are not observed. Do not interpret cross-cohort comparisons as entry dynamics. Older cohorts appear artificially small.

---

## District-level file (`district_analysis.csv`)

Same column families as municipality file but grouped by `DIST` (77 districts). Extra column `n_lgcodes` = count of municipalities rolling up into each district (useful for sanity checks).

---

## Revision notes

- v1 — initial build. Classification schemes are defined in `01_classify_nsic.R`. To override any mapping, edit `data/clean/nec2018/nsic_classification_map.csv` directly after running 01, then re-run 02 onward.
