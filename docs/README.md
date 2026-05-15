# SSIV results site

A small static site reporting reduced-form shift-share evidence on the
local-economy effects of international migration in Nepal.

## Files

- `index.html`       — narrative landing page (the story).
- `explorer.html`    — interactive regression explorer (25,740 cells).
- `methods.html`     — equations, identification, robustness.
- `definitions.html` — variable definitions.
- `summary.html`     — mean / SD / N by census round.
- `style.css`        — shared styles.
- `results.json`     — pre-computed regression estimates.
- `meta.json`        — definitions + summary statistics.

## Run locally

```bash
cd docs && python3 -m http.server
# open http://localhost:8000
```

## Publish on GitHub Pages

1. **Settings → Pages → Source: Deploy from branch → branch → `/docs`**
2. URL: `https://<user>.github.io/<repo>/`

The selected (panel, lag, shock, ctrl, group, display) on the explorer page is
encoded in the URL hash, so views are shareable.

## Re-build `results.json` and `meta.json`

In Python:
```bash
python3 script/build_results.py
python3 script/build_meta.py
```

Or in RStudio (identical output):
```r
source("script/build_results.R")
source("script/build_meta.R")
```

The R version uses `fixest::feols` with cluster `~ lgcode`; coefficients match
the Python `linearmodels.PanelOLS` setup byte-for-byte.

Reads:
- `data/clean/instrument/instrument_mun.csv`
- `data/clean/census/census_outcomes_municipality.csv`

## Enabling the Khanna et al. (2026) control sets

Two control sets — `khanna` (region-share controls) and `khanna_full`
(adds the trade SSIV) — are silently hidden in the explorer until their
inputs exist. To enable, run any of the equivalent Python or R recipes
below.

### Python
```bash
# 1) Region shares (needs raw 2001 census .dta files):
Rscript script/vars/instrument.R

# 2) WDI destination GDP per capita 2001:
pip install wbgapi pandas
python3 script/fetch_wdi_dest_gdp.py
#    → data/clean/instrument/wdi_dest_gdp_2001.csv

# 3) Trade SSIV (Khanna §IIIC). Needs a free Comtrade API key.
pip install comtradeapicall
export COMTRADE_API_KEY=...
python3 script/fetch_comtrade.py
python3 script/build_trade_ssiv.py
#    → data/clean/instrument/trade_ssiv.csv
```

### R / RStudio (equivalent)
```r
# 2) WDI destination GDP per capita 2001:
source("script/optional/fetch_wdi_dest_gdp.R")

# 3) Trade SSIV. Set Sys.setenv(COMTRADE_API_KEY="…") if you have a key.
source("script/optional/fetch_comtrade.R")
source("script/optional/build_trade_ssiv.R")
```

Then re-run `build_results.py` (or `build_results.R`); the `khanna` /
`khanna_full` control sets appear automatically once their files are present.

## Specification

```
y_mt = α_m + γ_t + β · shock_{m,t-lag} + λ' · controls_{m,t≠ref} + ε_mt
```

- Two-way FE: municipality + year.
- CR1 cluster at municipality.
- `MI_{m,0}` from 2001 census; baseline X from 2001.
- Lagging the FX shifter ≡ lagging SSIV (2001 weights are time-invariant).

### Panels & reference year
- `P2_ref2011` — 2011, 2021 (your headline panel).
- `P3_ref2001` — 2001, 2011, 2021 anchored at pre-shock 2001.
- `P3_ref2011` — 2001, 2011, 2021 anchored at 2011.

### Shock variants
- `ssiv_index` — main per-capita SSIV (level index, 2001 = 1).
- `shareshock_index` — composition only (no `MI` scaling).
- `ssiv_log_dec` — decadal log change in FX × baseline weights.
- `ssiv_w99` — main SSIV, top 1% winsorized.
- `ssiv_asinh` — variance-stabilized SSIV.

All shocks pre-standardized within panel; β is per 1 SD.

### Control sets (Khanna et al. 2026 §IIIB mapping)

| Tag | What it adds | Khanna analog |
|---|---|---|
| `none` | only FEs | — |
| `mi` | + `MI × 1{t≠ref}` | `MigInc × D_t` (incomplete-share control) |
| `mi_share` | + `MI` + `ShareShock × 1{t≠ref}` | `MigInc × D_t` + `Rshock × D_t` (decomposed) |
| `mi_baseX` | + `MI` + 5 baseline X × `1{t≠ref}` | partial Block B/C |
| `mi_share_baseX` | + `MI` + `ShareShock` + full 12-var baseline X × `1{t≠ref}` | Blocks B + C |
| `khanna` | above + region shares × `1{t≠ref}` | + Block A |
| `khanna_full` | above + trade SSIV × `1{t≠ref}` | + Block D |

The 12-var "full baseline X" (used by `mi_share_baseX` and above) covers:
- **Block B (development)**: lighting/electricity, piped water, radio, TV, non-agro enterprise, head age mean, literacy.
- **Block C (industrial structure)**: agri share, non-agri share, manufacturing share, finance/RE/prof share, female LFP.

`khanna` and `khanna_full` only appear in the viewer once the corresponding optional CSVs exist.
