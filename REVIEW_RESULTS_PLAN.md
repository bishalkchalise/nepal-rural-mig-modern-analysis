# Code review + results communication plan

## 1) Code review of current branch

### A. Strengths
- The instrument script documents SSIV construction clearly (weights, shifters, identities) and aligns with BHJ-style diagnostics.
- Outcome construction is broad and policy-relevant, covering amenities, labor, industry, migration, education, and demography.
- The web explorer concept (results + definitions + summary + methods) is excellent for transparency and replication.

### B. Highest-priority technical issues to fix
1. **`census_est.R` mixes production logic with exploratory blocks**
   - The script contains repeated model runs and ad hoc regressions after the core batch run.
   - This increases risk of inconsistent reported results.
   - Recommendation: split into:
     - `01_build_panel.R`
     - `02_estimate_main.R`
     - `03_robustness.R`
     - `04_export_tables_for_web.R`

2. **Potential naming mismatch in regression batches**
   - `run_ssiv_regressions(outcomes = unname(labor), ...)` is called, but the outcome list file defines `work` and does not clearly define `labor` in the shown section.
   - Recommendation: enforce one canonical object per outcome group and assert existence before running.

3. **Control specification mismatch between code and methods page**
   - The methods page explains richer controls (`mi`, `mi_share`, `mi_baseX`, etc.), but `run_ssiv_regressions()` currently uses one reduced formula shape.
   - Recommendation: implement a spec registry (`spec_id -> formula fragments`) and ensure webpage controls map exactly to those formulas.

4. **Data-path inconsistency risk**
   - Similar files appear under multiple paths (`data/clean/census/...` and `data/clean/...`) and scripts point to both styles.
   - Recommendation: a single data contract file (`config/paths.yml`) + all scripts read from it.

5. **Hard reset style in scripts (`rm(list=ls())`)**
   - This is fragile in modular pipelines and makes function reuse/testing hard.
   - Recommendation: move logic into pure functions and run from a single driver script.

### C. Variable-definition and coding consistency checks to run
- Create a `validate_variables.R` script that checks:
  1. every variable shown on webpage exists in merged analysis panel,
  2. every variable has a codebook definition,
  3. every estimated outcome has summary stats for all available rounds,
  4. all control-set labels map to real formula objects.

### D. Reproducibility improvements
- Add `renv.lock` and a README workflow (`make data`, `make estimate`, `make web`).
- Export a machine-readable results bundle:
  - `results_main.parquet`
  - `results_robustness.parquet`
  - `spec_manifest.csv`
- Add a changelog entry whenever variable definitions or coding decisions change.

---

## 2) Best user-friendly design plan for results (census + RVS + NEC)

## Design principle
**One coherent story, three evidence layers:**
1. **Census (macro place change):** modernization and structural transformation at municipality level.
2. **RVS (household mechanisms):** consumption, labor allocation, coping, migration channels.
3. **NEC (firm-side response):** local enterprise composition, formality, productivity, scale.

## Proposed minimalist website structure

### Page 1 — "Start here" (for non-specialists)
- 5 bullets:
  - research question,
  - what the FX shock means in plain language,
  - where treatment variation comes from,
  - what datasets are used,
  - headline takeaways.
- 1 static visual: "How SSIV is built" diagram.

### Page 2 — "Headline findings"
- Show only 8–12 pre-registered headline outcomes (not all variables).
- Group cards:
  - Housing & amenities,
  - Labor & sector shift,
  - Human capital,
  - Gender/household,
  - In-migration response.
- Each card has:
  - effect size (z-score and % of mean),
  - 95% CI,
  - one-sentence interpretation,
  - link "see robustness".

### Page 3 — "Mechanisms: RVS"
- Household-level channels:
  - income/consumption smoothing,
  - occupational transitions,
  - education-health spending,
  - coping/shock responses.
- Present as mechanism ladder:
  - FX shock -> remittance purchasing power -> household decisions -> local demand/skills.

### Page 4 — "Firm response: NEC"
- Firm outcomes in a compact dashboard:
  - sectoral composition,
  - tradable/non-tradable split,
  - size distribution,
  - productivity proxies,
  - formality/finance.
- Explicit caveat on NEC survivor bias should appear as a persistent info badge.

### Page 5 — "Robustness lab"
- Keep all robustness dimensions but default-collapsed.
- Add a **traffic-light stability metric** per outcome:
  - Green: sign/magnitude stable across pre-defined robustness grid,
  - Yellow: sign stable but precision weak,
  - Red: unstable.
- This helps non-technical users avoid over-reading one specification.

### Page 6 — "Definitions & data provenance"
- Unified variable dictionary across Census/RVS/NEC.
- For each variable: universe, formula, source questionnaire code, missingness share, years available.

### Page 7 — "Replication"
- One-click download:
  - cleaned panels,
  - regression-ready file,
  - result tables,
  - script map.

---

## 3) Concrete result-display standards

- Always show both:
  - **Standardized beta (per 1 SD shock)** and
  - **Percent of baseline mean**.
- Add **sample metadata in every tooltip**: years, N municipalities/HH/firms, clustering level.
- Use one color language everywhere:
  - positive modernization effects = one hue,
  - adverse effects = contrasting hue,
  - insignificant = neutral gray.
- Avoid coefficient-only tables on landing pages; use ranked dot-whisker charts.

---

## 4) Suggested "best story" for literature contribution

## Working title narrative
**"External migration income shocks can accelerate rural structural transformation through both household demand channels and local firm reallocation."**

## Story arc
1. **Exogenous income shock:** destination-currency appreciation raises remittance purchasing power in origin municipalities with high pre-2001 destination exposure.
2. **Local modernization in census outcomes:** better housing/amenities, asset adoption, and a shift away from low-productivity agricultural employment shares.
3. **Mechanism evidence from RVS:** households adjust expenditures and labor choices in ways consistent with income effects and risk smoothing.
4. **Firm-side corroboration from NEC:** local enterprise mix and modern-sector indicators shift in the same direction, consistent with demand-linked local equilibrium adjustment.
5. **Robustness and decomposition:** results survive lag, controls, shock variants, and decomposition controls; strongest patterns are those stable across the full grid.

## Value-add relative to literature
- Extends the "abundance from abroad" framework to Nepal with municipality-level long-run modernization outcomes.
- Integrates **three data layers** (population census, household survey, enterprise census) to link reduced-form impacts to mechanisms and market-level responses.
- Offers a transparent public results interface with equation-level documentation and variable provenance.

---

## 5) 30-day execution roadmap (practical)

### Week 1
- Freeze variable dictionary + spec registry.
- Refactor estimation scripts into modular pipeline.

### Week 2
- Generate unified results bundle (Census/RVS/NEC).
- Build headline-outcome shortlist and predefine robustness grid.

### Week 3
- Implement redesigned pages + mechanism/firms pages.
- Add stability scoring and narrative annotations.

### Week 4
- Internal replication pass, external user test (non-economist), final polish.
- Publish versioned release notes with hash of analysis outputs.

