# Shift-Share IV for Nepal: Construction, Diagnostics, and Identification

A systematic record of construction choices, diagnostic findings, and identification considerations for a municipality-level shift-share IV (SSIV) measuring exposure to destination-country FX shocks transmitted through migration networks.

---

## 0. Setup

```r
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(stringr)
library(purrr)
library(fixest)

# Required data frames assumed loaded:
# - mun_dest_year:    municipality × destination × year panel
#                     columns: lgcode, country, year, mun_mig_pop_2001, 
#                              total_migrants_2001, destination_share, 
#                              relative_fx_2001, mun_pop_2001
# - mun_ssiv:         municipality × year panel of constructed SSIV
#                     columns: lgcode, year, population_2001, total_migrants_2001,
#                              migrants_per_capita_2001, avg_fx_shock_2001,
#                              ssiv_per_capita_2001, total_fx_exposure_2001
# - mig_share_2021:   municipality × destination shares for 2021
#                     columns: lgcode, country_name, migrants, total_migrants, dest_share
# - nepal_shape:      sf object with municipality boundaries
#                     columns: LGCODE, LGNAME, DNAME, ECOBELT, PNAME, geometry

# Helper: country name standardization
standardize_country <- function(x) {
  x <- toupper(str_trim(x))
  case_when(
    x %in% c("HONG KONG SAR CHINA", "HONG KONG")         ~ "HONG KONG",
    x %in% c("UNITED ARAB EMIRATES", "UAE")              ~ "UAE",
    x %in% c("UNITED KINGDOM", "UK", "GREAT BRITAIN")    ~ "UK",
    x %in% c("UNITED STATES", "USA", "US")               ~ "USA",
    x %in% c("SOUTH KOREA", "KOREA, REPUBLIC OF",
             "REPUBLIC OF KOREA", "KOREA SOUTH")         ~ "SOUTH KOREA",
    x %in% c("RUSSIA", "RUSSIAN FEDERATION")             ~ "RUSSIA",
    TRUE                                                  ~ x
  )
}
```

---

## 1. SSIV Construction: Conceptual Framework

Following Borusyak, Hull, and Jaravel (2022) and the Yang (2008) shift-share approach to migration-FX shocks, we define three related quantities at the municipality (`o`) × year (`t`) level:

### Definitions

For each municipality `o` and destination `d`:

- $\omega_{do,2001}$: pre-shock per-capita migrant income/count from destination $d$, fixed at 2001 baseline
- $\tilde{\Delta R}_{dt}$: fractional change in destination-$d$ exchange rate (FX shock)

The three quantities:

| Quantity | Formula | Interpretation |
|----------|---------|----------------|
| `Rshock` (FX shock, weighted avg) | $\sum_d (m_{do}/M_o) \tilde{\Delta R}_{dt}$ | Average FX shock faced by muni $o$'s migrants — proper weighted average, weights sum to 1 |
| `MigInc` (migration intensity) | $M_o / \text{Pop}_o$ | Per-capita migrant exposure |
| `Shiftshare` (the IV) | $\sum_d \omega_{do,2001} \tilde{\Delta R}_{dt}$ | Combined exposure: composition × scale |

The key identity from equation (3) in Yang (2008):
$$Shiftshare_o = Rshock_o \times MigInc_{o0}$$

### R Implementation

```r
# Per-row (muni × destination × year) contributions
mun_contributions <- mun_dest_year %>%
  mutate(
    share_times_fx       = destination_share * relative_fx_2001,
    migrants_times_fx    = mun_mig_pop_2001  * relative_fx_2001,
    pc_migrants_times_fx = (mun_mig_pop_2001 / mun_pop_2001) * relative_fx_2001
  )

# Collapse to municipality × year
mun_ssiv <- mun_contributions %>%
  group_by(lgcode, year) %>%
  summarise(
    population_2001          = first(mun_pop_2001),
    total_migrants_2001      = first(total_migrants_2001),
    migrants_per_capita_2001 = total_migrants_2001 / population_2001,
    
    avg_fx_shock_2001        = sum(share_times_fx,    na.rm = TRUE),  # Rshock_o
    ssiv_per_capita_2001     = sum(migrants_times_fx, na.rm = TRUE) / first(mun_pop_2001),  # Shiftshare_o
    total_fx_exposure_2001   = sum(migrants_times_fx, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Identity check: ssiv_per_capita = avg_fx_shock × migrants_per_capita
    ssiv_per_capita_check = avg_fx_shock_2001 * migrants_per_capita_2001,
    ssiv_per_capita_diff  = ssiv_per_capita_2001 - ssiv_per_capita_check
  )
```

### Identity Verification

All three relationships should hold to floating-point precision:

```r
mun_contributions %>%
  group_by(lgcode, year) %>%
  summarise(
    sum_share_fx    = sum(share_times_fx,       na.rm = TRUE),
    sum_mig_fx      = sum(migrants_times_fx,    na.rm = TRUE),
    sum_pc_mig_fx   = sum(pc_migrants_times_fx, na.rm = TRUE),
    M_o             = first(total_migrants_2001),
    Pop_o           = first(mun_pop_2001),
    
    check1 = sum_share_fx - sum_mig_fx / M_o,                          # Rshock identity
    check2 = sum_pc_mig_fx - sum_mig_fx / Pop_o,                       # SSIV identity
    check3 = sum_pc_mig_fx - sum_share_fx * (M_o / Pop_o),             # Decomposition
    
    .groups = "drop"
  ) %>%
  summarise(across(starts_with("check"), ~ max(abs(.), na.rm = TRUE)))

# All max values should be < 1e-15 (machine epsilon)
```

**Result observed in our data**: `max_check1 = 4.44e-16`, `max_check2 = 1.39e-17`, `max_check3 = 2.78e-17`. All identities hold exactly.

### Why these are/aren't weighted averages

| Quantity | Weighted average? | Why |
|----------|-------------------|-----|
| `avg_fx_shock_2001` (sum of `share_times_fx`) | ✅ Yes | Weights $m_{do}/M_o$ sum to 1 |
| `ssiv_per_capita_2001` (sum of `pc_migrants_times_fx`) | ❌ No | Weights $m_{do}/\text{Pop}_o$ sum to migrants per capita |
| `total_fx_exposure_2001` (sum of `migrants_times_fx`) | ❌ No | Raw migrant counts, sum to $M_o$ |

The shift-share IS NOT normalized — that's intentional, because it captures both **composition** (via shares) and **scale** (via per-capita migrant intensity).

---

## 2. India Exclusion Issue

### Problem

India was excluded from the destination set early in the pipeline (because the Nepalese rupee is pegged to the Indian rupee, making relative FX shock zero), but the denominator for `destination_share` still included Indian migrants. Result: shares didn't sum to 1 within municipality.

### Diagnostic

```r
# Check whether shares sum to 1 within each muni-year
share_check <- mun_dest_year %>%
  group_by(lgcode, year) %>%
  summarise(
    n_destinations = n(),
    share_sum      = sum(destination_share, na.rm = TRUE),
    share_sum_diff = abs(share_sum - 1),
    .groups = "drop"
  )

summary(share_check$share_sum)
# Result: 194 municipality-years had shares summing to less than 1
```

Quantification:
```r
# Total migrants in the data: 156,530 (excluding India)
# Total migrants in the denominator: 162,389 (including India)
# Difference: 5,859 Indian migrants → ~3.6% of total
```

### Decision and Resolution

**Approach taken**: Recognize that India's FX shock is genuinely zero (peg ⇒ no relative change), so the per-capita shift-share `ssiv_per_capita_2001` is *correct as computed* — India contributes zero to the sum regardless. The issue is only that `avg_fx_shock_2001` (the weighted average) is technically attenuated by the missing India share weight.

**Recommended fix** for cleanest interpretation:

```r
# Recompute denominator from non-India destinations
mun_dest_year <- mun_dest_year %>%
  group_by(lgcode) %>%
  mutate(
    total_migrants_2001 = sum(mun_mig_pop_2001, na.rm = TRUE),
    destination_share   = mun_mig_pop_2001 / total_migrants_2001
  ) %>%
  ungroup()

# Alternative: add India back with relative_fx_2001 = 0
# (gives same shift-share value, cleaner weighted-average interpretation)
```

---

## 3. Residualization for Visualization

To visualize identifying variation, residualize SSIV on muni and year FE:

```r
fe_model <- feols(ssiv_per_capita_2001 ~ 1 | lgcode + year, data = mun_ssiv)
mun_ssiv$ssiv_resid <- residuals(fe_model)
```

### Critical insight: averaging residuals across years gives ~0 by construction

```r
# This produces residuals on the order of 1e-17 (machine epsilon)
ssiv_avg <- mun_ssiv %>%
  group_by(lgcode) %>%
  summarise(ssiv_resid = mean(ssiv_resid, na.rm = TRUE))
```

The two-way FE forces $\frac{1}{T}\sum_t \hat{\varepsilon}_{ot} \approx 0$ for every muni. To visualize meaningful variation, plot specific years instead:

```r
years_to_plot <- c(2001, 2011, 2021)

panel_data <- nepal_shape %>%
  mutate(LGCODE = as.numeric(LGCODE)) %>%
  inner_join(
    mun_ssiv %>% filter(year %in% years_to_plot),
    by = c("LGCODE" = "lgcode")
  )

# Symmetric color limits for fair comparison across panels
lim <- panel_data %>%
  st_drop_geometry() %>%
  pull(ssiv_resid) %>%
  abs() %>%
  quantile(0.98, na.rm = TRUE)

ggplot(panel_data) +
  geom_sf(aes(fill = ssiv_resid), color = NA) +
  scale_fill_gradient2(
    low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
    midpoint = 0, limits = c(-lim, lim),
    oob = scales::squish, name = "SSIV residual"
  ) +
  facet_wrap(~ year) +
  theme_void()
```

---

## 4. Trajectory Analysis: Sign-Flip Pattern

### Discovery

Classifying munis by their residual sign trajectory across 2001 → 2011 → 2021 reveals near-universal sign flipping:

```r
mun_pattern <- mun_ssiv %>%
  filter(year %in% c(2001, 2011, 2021)) %>%
  select(lgcode, year, ssiv_resid) %>%
  pivot_wider(names_from = year, values_from = ssiv_resid, names_prefix = "resid_") %>%
  mutate(
    pattern = paste0(
      ifelse(resid_2001 > 0, "+", "−"),
      ifelse(resid_2011 > 0, "+", "−"),
      ifelse(resid_2021 > 0, "+", "−")
    ),
    category = case_when(
      pattern == "+++"            ~ "Persistently positive",
      pattern == "−−−"            ~ "Persistently negative",
      pattern == "−−+"            ~ "Late switcher (− to +)",
      pattern == "++−"            ~ "Late de-exposed (+ to −)",
      pattern == "−++"            ~ "Early switcher (− to +)",
      pattern == "+−−"            ~ "Early de-exposed (+ to −)",
      pattern %in% c("+−+", "−+−") ~ "Oscillating",
      TRUE                        ~ "Other"
    ),
    swing = resid_2021 - resid_2001
  )

mun_pattern %>% count(pattern, sort = TRUE) %>% mutate(pct = round(100 * n / sum(n), 1))
```

**Result observed**:
- `++−` (Late de-exposed): 476 munis (66.7%)
- `−−+` (Late switcher): 225 munis (31.5%)
- All other patterns: <2% combined
- **Zero persistent (`+++` or `−−−`) munis**

### Interpretation

The sign-flip is **mechanical, not pathological**:
- The year FE forces cross-sectional residuals to mean zero each year
- 2001/2011 had similar destination-shock structures
- 2021 had a fundamentally different structure
- → Munis "above average" in 2001/2011 were forced "below average" in 2021 by the demeaning

### Mechanism: FX shock structure changed regime

```r
# Verify the FX shock pattern flip
mun_dest_year %>%
  filter(year %in% c(2001, 2011, 2021)) %>%
  group_by(country, year) %>%
  summarise(fx = first(relative_fx_2001), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = fx, names_prefix = "fx_") %>%
  arrange(desc(fx_2021))
```

**Observed pattern**:
- 2001: All countries = 1.0 (baseline)
- 2011: Australia 1.97, Japan 1.50, Singapore 1.41 high; Gulf states ~0.99 (USD-pegged, flat); Bangladesh 0.74, Pakistan 0.71 low
- 2021: USD-pegged Gulf states JUMPED to 1.58; Australia 2.29, Japan 1.75 still high; Russia 0.62, Pakistan 0.60, Sri Lanka 0.71 collapsed

The Gulf states went from FX laggards to FX leaders, mechanically flipping which munis appeared "above-average exposed."

---

## 5. Variance Concentration Diagnostic

```r
# Variance share by year
mun_ssiv %>%
  group_by(year) %>%
  summarise(
    sd_resid     = sd(ssiv_resid, na.rm = TRUE),
    var_resid    = var(ssiv_resid, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(weight_share = var_resid / sum(var_resid))
```

**Observed**: 2020 and 2021 each carry ~7-8% of total variance; 2015-2017 each carry ~1-2%. Identifying variation is concentrated in late years.

### 2001 has nearly zero identifying variation

```r
summary(filter(mun_ssiv, year == 2001)$ssiv_resid)
#       Min:     -0.0121
#       1st Qu:  -0.000767
#       Median:   0.00107
#       3rd Qu:   0.00149
#       Max:      0.00166
```

**Why**: `relative_fx_2001` is normalized so all destinations = 1.0 in 2001. So in 2001:
$$Rshock_{o,2001} = \sum_d \omega_{do0} \cdot 1 = MigInc_{o0}$$

After absorbing muni FE (which absorbs `migrants_per_capita`), 2001 residuals are essentially zero. **Effective identifying variation is 2011 vs 2021**.

---

## 6. Trajectory Group Composition: Migration Intensity Story

### Diagnostic

```r
# Compare characteristics across trajectory groups
mun_pattern %>%
  left_join(
    mun_ssiv %>% filter(year == 2001) %>% 
      select(lgcode, population_2001, total_migrants_2001, migrants_per_capita_2001),
    by = "lgcode"
  ) %>%
  filter(category %in% c("Late de-exposed (+ to −)", "Late switcher (− to +)")) %>%
  group_by(category) %>%
  summarise(
    n                = n(),
    mean_pop         = mean(population_2001, na.rm = TRUE),
    mean_total_mig   = mean(total_migrants_2001, na.rm = TRUE),
    mean_mig_per_cap = mean(migrants_per_capita_2001, na.rm = TRUE)
  )
```

**Observed**:

| Category | n | Mean total migrants | Mean migrants/cap |
|----------|---|--------------------:|------------------:|
| Late de-exposed (`++−`) | 476 | 51.6 | 0.00152 |
| Late switcher (`−−+`) | 225 | 600.4 | 0.0184 |

**Critical finding**: The `−−+` group has ~12× higher migration intensity than the `++−` group.

### Geographic pattern

```r
mun_pattern %>%
  filter(category %in% c("Late de-exposed (+ to −)", "Late switcher (− to +)")) %>%
  inner_join(
    nepal_shape %>% st_drop_geometry() %>% select(LGCODE, ECOBELT),
    by = c("lgcode" = "LGCODE")
  ) %>%
  count(category, ECOBELT) %>%
  pivot_wider(names_from = category, values_from = n, values_fill = 0)
```

**Observed**: Terai overrepresented in `++−` (208 vs 58), reflecting that Terai munis send most migrants to India (excluded from IV), leaving small noisy non-India samples.

### Interpretation reframed

The `++−` munis aren't "diversified migrators" — they're munis with **very few non-India migrants overall**, where small-sample noise creates lumpy, scattered destination shares. The trajectory split reflects measurement quality, not economic structure.

---

## 7. Share Persistence Diagnostic (CRUCIAL)

### Cross-time correlation of destination shares

The IV's identifying assumption requires that 2001 shares meaningfully predict subsequent exposure. Test this directly:

```r
# Standardize country names across years
shares_2001_clean <- mun_dest_year %>%
  filter(year == 2001) %>%
  select(lgcode, country, share_2001 = destination_share) %>%
  mutate(country_std = standardize_country(country))

shares_2021_clean <- mig_share_2021 %>%
  select(lgcode, country_name, share_2021 = dest_share) %>%
  mutate(country_std = standardize_country(country_name))

# Verify cleaning
setdiff(shares_2001_clean$country_std, shares_2021_clean$country_std)
setdiff(shares_2021_clean$country_std, shares_2001_clean$country_std)

# Join (full join, fill missing with 0 — country absent ⇒ zero share)
shares_joined <- shares_2001_clean %>%
  select(lgcode, country_std, share_2001) %>%
  full_join(
    shares_2021_clean %>% select(lgcode, country_std, share_2021),
    by = c("lgcode", "country_std")
  ) %>%
  mutate(
    share_2001 = coalesce(share_2001, 0),
    share_2021 = coalesce(share_2021, 0)
  )

# Within-muni correlations
mun_correlations <- shares_joined %>%
  group_by(lgcode) %>%
  summarise(
    n_dest_either = sum(share_2001 > 0 | share_2021 > 0),
    cor_pearson   = if (n_dest_either >= 3) cor(share_2001, share_2021) else NA_real_,
    cor_spearman  = if (n_dest_either >= 3) cor(share_2001, share_2021, method = "spearman") else NA_real_,
    .groups = "drop"
  )
```

**Observed at municipality level**:
- Median Pearson correlation: 0.55
- 24.8% strong (≥0.7), 35.7% weak (<0.4), 15.9% negative

### Persistence vs migration intensity

```r
mun_correlations_intensity <- mun_correlations %>%
  left_join(
    mun_ssiv %>% filter(year == 2001) %>% 
      select(lgcode, total_migrants_2001, migrants_per_capita_2001),
    by = "lgcode"
  ) %>%
  filter(!is.na(cor_pearson))

mun_correlations_intensity %>%
  mutate(mig_bin = ntile(total_migrants_2001, 5)) %>%
  group_by(mig_bin) %>%
  summarise(
    n              = n(),
    mean_total_mig = mean(total_migrants_2001),
    median_cor     = median(cor_pearson),
    pct_strong     = mean(cor_pearson >= 0.7),
    pct_negative   = mean(cor_pearson < 0)
  )
```

**Observed**: Strong dose-response between 2001 migration intensity and share persistence:

| Quintile | Mean migrants | Median correlation | % strong | % negative |
|----------|--------------:|-------------------:|---------:|-----------:|
| Q1 | 4.3 | -0.06 | 3.5% | high |
| Q2 | 19.9 | 0.38 | 13.4% | moderate |
| Q3 | 71 | 0.62 | 31.7% | low |
| Q4 | 214 | 0.64 | 33.1% | low |
| Q5 | 834 | 0.67 | 42.3% | none |

Persistence stabilizes around Q3 (~50-70 migrants). Below this threshold, 2001 shares are statistically meaningless.

### Examining negative-correlation munis

```r
mun_correlations_intensity %>%
  filter(cor_pearson < 0) %>%
  mutate(intensity_bin = case_when(
    total_migrants_2001 < 10  ~ "Tiny (<10)",
    total_migrants_2001 < 50  ~ "Small (10-50)",
    total_migrants_2001 < 200 ~ "Medium (50-200)",
    TRUE                      ~ "Large (200+)"
  )) %>%
  count(intensity_bin)
```

**Observed**:
- Tiny (<10): 79
- Small (10-50): 29
- Medium (50-200): 5
- Large (200+): 0

**96% of negative correlations occur in munis with <50 migrants.** The 5 medium-migration exceptions were all Himalayan ethnic-minority munis (Sherpa, Thakali, Manangi, Hyolmo) with genuine network reorganization — not measurement error.

---

## 8. District-Level Diagnostic

### Aggregation construction

```r
# District-level shares from 2001
district_shares_2001 <- mun_dest_year %>%
  filter(year == 2001) %>%
  left_join(
    nepal_shape %>% st_drop_geometry() %>% 
      select(LGCODE, DNAME, ECOBELT),
    by = c("lgcode" = "LGCODE")
  ) %>%
  group_by(DNAME, country) %>%
  summarise(dist_migrants_2001 = sum(mun_mig_pop_2001, na.rm = TRUE), .groups = "drop") %>%
  group_by(DNAME) %>%
  mutate(
    dist_total_migrants_2001 = sum(dist_migrants_2001, na.rm = TRUE),
    share_2001 = dist_migrants_2001 / dist_total_migrants_2001
  ) %>%
  ungroup() %>%
  mutate(country_std = standardize_country(country))

# District-level shares from 2021
district_shares_2021 <- mig_share_2021 %>%
  left_join(
    nepal_shape %>% st_drop_geometry() %>% select(LGCODE, DNAME),
    by = c("lgcode" = "LGCODE")
  ) %>%
  group_by(DNAME, country_name) %>%
  summarise(dist_migrants_2021 = sum(migrants, na.rm = TRUE), .groups = "drop") %>%
  group_by(DNAME) %>%
  mutate(
    dist_total_migrants_2021 = sum(dist_migrants_2021, na.rm = TRUE),
    share_2021 = dist_migrants_2021 / dist_total_migrants_2021
  ) %>%
  ungroup() %>%
  mutate(country_std = standardize_country(country_name))

# Joined and correlations computed analogously to muni level
```

### Observed district-level results

| Metric | Municipality | District |
|--------|-------------:|---------:|
| n | 711 | 76 |
| Median correlation | 0.55 | 0.66 |
| % strong (≥0.7) | 24.8% | 36.8% |
| % weak (<0.4) | 35.7% | 15.8% |
| % negative | 15.9% | 6.6% |

### Persistent district-level problems

12 districts still have correlation < 0.4, including:
- **Kathmandu (corr 0.27, 13,492 migrants)** — best-measured district has weakest persistence
- **Lalitpur (corr 0.33, 3,729 migrants)**
- Far-western Pahad (Achham, Bajura, Bajhang, Doti)
- High-altitude Himal (Mustang, Manang, Dolpa, Humla, Jumla)

### Interpretation

District aggregation **only modestly** improves persistence. This reveals that the muni-level persistence problem is *not purely measurement noise* — there's genuine network reorganization in:
1. Urban centers (diversification into new high-skill destinations, post-2008 Korea EPS)
2. Far-western Pahad (delayed entry into international migration)
3. Himalayan ethnic minorities (distinctive networks reconfigured)

### Verdict on district aggregation

**Modest help, not transformational**. District-level analysis loses too much variation (76 units × 3 years = 228 obs) to justify as the main specification. Better to use it as a robustness check.

---

## 9. Sample Restriction Decisions

### Three candidate approaches

| Approach | What it does | Verdict |
|----------|--------------|---------|
| **Threshold cutoff (≥50 migrants)** | Restricts to munis where 2001 shares are reliably measured | **Recommended as main specification** |
| **Winsorize 5-95%** | Caps SSIV outliers but keeps all munis | **Not recommended** — addresses wrong problem (extreme values, not measurement noise) |
| **District aggregation** | Pools to 75 districts | **Robustness check only** |

### Recommended three-specification approach

```r
# === MAIN SPECIFICATION ===
mun_ssiv_main <- mun_ssiv %>% filter(total_migrants_2001 >= 50)

m_main <- feols(outcome ~ ssiv_per_capita_2001 | lgcode + year, 
                data = mun_ssiv_main, cluster = ~lgcode)

# === ROBUSTNESS 1: Direct correlation filter ===
mun_ssiv_high_persist <- mun_ssiv %>%
  inner_join(
    mun_correlations %>% filter(cor_pearson >= 0.5) %>% select(lgcode),
    by = "lgcode"
  )

m_persist <- feols(outcome ~ ssiv_per_capita_2001 | lgcode + year, 
                   data = mun_ssiv_high_persist, cluster = ~lgcode)

# === ROBUSTNESS 2: District aggregation ===
# (build district_ssiv analogously to mun_ssiv at district level)
m_district <- feols(outcome ~ ssiv_per_capita_2001 | DNAME + year,
                    data = district_ssiv, cluster = ~DNAME)

# === ROBUSTNESS 3: Threshold sensitivity ===
thresholds <- c(25, 50, 75, 100, 150)
threshold_robustness <- map_dfr(thresholds, function(t) {
  m <- feols(outcome ~ ssiv_per_capita_2001 | lgcode + year,
             data = mun_ssiv %>% filter(total_migrants_2001 >= t),
             cluster = ~lgcode)
  tibble(threshold = t, coef = coef(m)[1], se = se(m)[1])
})

etable(m_main, m_persist, m_district)
```

---

## 10. Control Variables: Adapting Khanna et al. (2026)

### Group 1: Migrant flow characteristics (2001 baseline)

Captures destination mix and skill composition. **Include all.**

```r
# Regional destination shares
mun_regional_shares <- mun_dest_year %>%
  filter(year == 2001) %>%
  mutate(region = case_when(
    country %in% c("Saudi Arabia", "Qatar", "United Arab Emirates", 
                   "Kuwait", "Bahrain", "Oman")          ~ "Gulf",
    country %in% c("Malaysia", "Singapore", "Thailand", 
                   "Hong Kong SAR China")                ~ "SE_Asia",
    country %in% c("South Korea", "Japan")               ~ "NE_Asia",
    country %in% c("United States", "United Kingdom",
                   "Australia", "Canada")                ~ "Western_OECD",
    country %in% c("Bangladesh", "Pakistan", "Sri Lanka",
                   "Bhutan", "Maldives")                 ~ "South_Asia",
    TRUE                                                 ~ "Other"
  )) %>%
  group_by(lgcode, region) %>%
  summarise(region_share_2001 = sum(destination_share, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = region, values_from = region_share_2001, 
              values_fill = 0, names_prefix = "share_2001_")

# Destination characteristics weighted by 2001 shares
# (requires external destination-level data: GDP, sector mix, wage levels)
mun_dest_chars <- mun_dest_year %>%
  filter(year == 2001) %>%
  left_join(destination_chars, by = "country") %>%  # external dataset
  group_by(lgcode) %>%
  summarise(
    mun_avg_dest_gdppc     = sum(destination_share * dest_gdppc_2001, na.rm = TRUE),
    mun_share_construction = sum(destination_share * sector_construction, na.rm = TRUE),
    mun_share_services     = sum(destination_share * sector_services, na.rm = TRUE),
    .groups = "drop"
  )
```

### Group 2: Pre-shock muni development (2001 baseline)

**Include with caution** — these may be similar to outcomes. Include the structural ones (rural share, literacy, infrastructure) but treat baseline-of-outcome variable as a separate robustness column.

```r
# From 2001 census, aggregated to LG boundaries
baseline_dev_2001 <- read_census_2001() %>%
  group_by(lgcode) %>%
  summarise(
    rural_share_2001       = mean(is_rural),
    literacy_rate_2001     = mean(literate),
    asset_index_2001       = mean(asset_pca),       # PCA on ownership variables
    electricity_share_2001 = mean(has_electricity),
    hh_size_2001           = mean(hh_size),
    ag_hh_share_2001       = mean(is_ag_household),
    .groups = "drop"
  )
```

### Group 3: Pre-shock industrial structure (2001 baseline)

**Include unless outcome IS labor market**. If labor market outcome, include only structural composition (sector shares), not levels (LFPR).

```r
industrial_2001 <- read_census_2001() %>%
  group_by(lgcode) %>%
  summarise(
    ag_workforce_share_2001  = mean(occupation_ag),
    mfg_workforce_share_2001 = mean(occupation_mfg),
    svc_workforce_share_2001 = mean(occupation_svc),
    lfp_rate_2001            = mean(in_labor_force),
    .groups = "drop"
  )
```

### Group 4: Alternative shift-share channels

**Critical**, especially the COVID and earthquake controls.

```r
# Trade shift-share (FX → consumer prices)
trade_imports_ssiv <- mun_dest_year %>%
  filter(year == 2001) %>%
  left_join(nepal_imports_by_country, by = c("country", "year")) %>%
  group_by(lgcode, year) %>%
  summarise(
    ssiv_imports = sum(destination_share * import_volume_pc * relative_fx_2001,
                       na.rm = TRUE),
    .groups = "drop"
  )

# COVID exposure shift-share — ESSENTIAL given 2020-21 variance concentration
covid_ssiv <- mun_dest_year %>%
  filter(year == 2001) %>%
  left_join(destination_covid_data, by = "country") %>%
  group_by(lgcode) %>%
  summarise(
    ssiv_covid_2020 = sum(destination_share * dest_covid_deaths_pc_2020, na.rm = TRUE),
    ssiv_covid_2021 = sum(destination_share * dest_covid_deaths_pc_2021, na.rm = TRUE),
    .groups = "drop"
  )

# Earthquake exposure (Nepal-specific, 2015)
earthquake_controls <- read_quake_data() %>%
  group_by(lgcode) %>%
  summarise(
    quake_destroyed_share_2015 = sum(houses_destroyed) / sum(total_houses)
  )
```

### Nepal-specific additions

| Control | Purpose |
|---------|---------|
| Earthquake damage × post-2015 | Major 2015 confounder for 2011→2021 comparisons |
| Distance to Indian border | Predicts unmeasured India migration intensity |
| Ecobelt × year FE | Allows belt-specific time trends (Himal/Pahad/Terai/KTM) |
| Pre-2001 migration trend (1991→2001) | If census data permits, addresses migration-on-trend bias |
| Number of pre-2017 VDCs amalgamated | Crosswalk quality control |

### Building the regression in stages

```r
# Stage 1: Pure FE
m1 <- feols(outcome ~ ssiv_per_capita_2001 | lgcode + year, 
            data = mun_ssiv_main, cluster = ~lgcode)

# Stage 2: + Migrant flow characteristics × year
m2 <- feols(outcome ~ ssiv_per_capita_2001 + 
              i(year, share_2001_Gulf) + i(year, share_2001_SE_Asia) +
              i(year, mun_avg_dest_gdppc) | 
              lgcode + year, 
            data = mun_ssiv_main, cluster = ~lgcode)

# Stage 3: + Development × year
m3 <- feols(outcome ~ ssiv_per_capita_2001 + 
              i(year, share_2001_Gulf) + i(year, share_2001_SE_Asia) +
              i(year, mun_avg_dest_gdppc) +
              i(year, rural_share_2001) + i(year, literacy_rate_2001) | 
              lgcode + year, 
            data = mun_ssiv_main, cluster = ~lgcode)

# Stage 4: + Industrial structure × year
m4 <- feols(outcome ~ ssiv_per_capita_2001 + 
              i(year, share_2001_Gulf) + i(year, share_2001_SE_Asia) +
              i(year, mun_avg_dest_gdppc) +
              i(year, rural_share_2001) + i(year, literacy_rate_2001) +
              i(year, ag_workforce_share_2001) + i(year, mfg_workforce_share_2001) | 
              lgcode + year, 
            data = mun_ssiv_main, cluster = ~lgcode)

# Stage 5: + Alternative shift-shares + earthquake
m5 <- feols(outcome ~ ssiv_per_capita_2001 + 
              i(year, share_2001_Gulf) + i(year, share_2001_SE_Asia) +
              i(year, mun_avg_dest_gdppc) +
              i(year, rural_share_2001) + i(year, literacy_rate_2001) +
              i(year, ag_workforce_share_2001) + i(year, mfg_workforce_share_2001) +
              ssiv_covid + ssiv_imports +
              quake_destroyed_share_2015:i(year >= 2015) | 
              lgcode + year, 
            data = mun_ssiv_main, cluster = ~lgcode)

etable(m1, m2, m3, m4, m5)
```

### On "bad controls" when baseline = outcome

**Including baseline 2001 value of outcome variable interacted with year is OK** because:
1. 2001 baseline is predetermined (cannot be affected by 2011/2021 treatment)
2. Muni FE absorbs the level; the interaction adds differential trends
3. This addresses convergence dynamics (poorer munis growing faster), a real threat

Footnote in paper:
> *"We control for baseline 2001 development indicators interacted with year fixed effects throughout. While these include variables conceptually related to our outcomes, they enter as predetermined baseline characteristics that allow differential trends rather than as contemporary mediators, and thus do not pose a 'bad control' concern in the sense of Angrist and Pischke (2009)."*

---

## 11. Identification Threats and How They Are Addressed

| Threat | Diagnostic finding | Mitigation |
|--------|-------------------|------------|
| Small-sample noise in 2001 shares | Q1-Q2 munis have median correlation < 0.4; 96% of negative correlations in <50-migrant munis | **Restrict to ≥50 migrant munis (main spec)** |
| Genuine network reorganization | Urban + remote munis have low persistence even when well-measured | **Direct correlation filter (≥0.5) as robustness** |
| 2020-21 variance concentration | These years carry disproportionate identifying weight | **COVID shift-share control; report by-period results** |
| Earthquake confounder (2015) | Earthquake-affected districts differ from rest | **Earthquake damage × post-2015 control** |
| India peg makes Terai munis low-information | Terai overrepresented in noisy `++−` group | **Implicit handling via migration-count threshold** |
| Convergence dynamics | High vs low baseline development on different trends | **Baseline development × year interactions** |
| Heterogeneous treatment effects | Trajectory groups have very different intensities | **Stratified regressions by migration intensity / ecobelt** |

---

## 12. Summary of Recommended Specification

### Main analysis design

> **Effective panel**: 2001 → 2011 → 2021, three observation periods
>
> **Effective identifying variation**: 2011→2021 difference (2001 contributes near-zero variation due to baseline normalization)
>
> **Main sample**: Municipalities with ≥50 overseas migrants in 2001 (~426 munis)
>
> **Specification**:
> $$Y_{ot} = \beta \cdot Shiftshare_{ot} + \alpha_o + \delta_t + X_{o,2001}' \gamma_t + Z_{ot}' \theta + u_{ot}$$
> 
> where:
> - $\alpha_o$: municipality FE
> - $\delta_t$: year FE  
> - $X_{o,2001}$: 2001 baseline characteristics (interacted with year)
> - $Z_{ot}$: time-varying alternative shift-share controls (COVID, trade) and earthquake
> - SEs clustered at municipality level

### Robustness checks to report

1. Threshold sensitivity: vary cutoff from 25 to 150 migrants
2. Direct correlation filter: include only munis with 2001-2021 share correlation ≥ 0.5
3. District-level aggregation: 76 districts, 3 years
4. Long-difference: collapse to 2011 vs 2021, run cross-section regression
5. Pre-COVID sample: drop 2020-2021 if outcomes permit
6. Stratified by ecobelt: separate Pahad / Himal / Terai / KTM regressions
7. Trajectory-group stratified: separate regressions for `++−` vs `−−+` munis (test for heterogeneous effects)

### Paper framing language

> *"Our identification strategy uses 2001 destination shares as exposure weights, predetermined relative to subsequent FX shocks. Within-municipality correlations between 2001 and 2021 destination shares average 0.65 in our main sample (≥50 migrants in 2001), indicating strong persistence of destination patterns. Identification leverages the differential exposure of municipalities to the late-2010s reconfiguration of remittance-source-country exchange rates: the USD-pegged Gulf bloc — which dominates Nepal's migration patterns — appreciated sharply against the NPR by 2021, while secondary destinations in South Asia and Russia experienced relative depreciations. Municipalities with concentrated Gulf migration thus received a more uniform FX windfall, while municipalities with diversified destination portfolios saw the windfall diluted by exposure to weaker-currency destinations. We exclude India from the destination set because the Nepalese rupee is pegged to the Indian rupee."*

---

## Appendix: Key R Functions for Diagnostics

### Function: full diagnostic suite

```r
run_ssiv_diagnostics <- function(mun_ssiv, mun_correlations, threshold = 50) {
  
  # 1. Identity verification
  identity_check <- mun_ssiv %>%
    summarise(
      max_diff = max(abs(ssiv_per_capita_diff), na.rm = TRUE)
    )
  cat("Identity check (should be ~1e-15):", identity_check$max_diff, "\n\n")
  
  # 2. Variance concentration
  var_share <- mun_ssiv %>%
    group_by(year) %>%
    summarise(var_resid = var(ssiv_resid, na.rm = TRUE), .groups = "drop") %>%
    mutate(weight_share = var_resid / sum(var_resid))
  cat("Variance share by year:\n")
  print(var_share)
  cat("\n")
  
  # 3. Persistence summary
  cat("Share persistence summary:\n")
  print(summary(mun_correlations$cor_pearson))
  cat("\nMedian by intensity quintile:\n")
  
  intensity_summary <- mun_correlations %>%
    left_join(mun_ssiv %>% filter(year == 2001) %>% 
                select(lgcode, total_migrants_2001),
              by = "lgcode") %>%
    filter(!is.na(cor_pearson)) %>%
    mutate(mig_bin = ntile(total_migrants_2001, 5)) %>%
    group_by(mig_bin) %>%
    summarise(
      n          = n(),
      median_cor = median(cor_pearson),
      pct_strong = mean(cor_pearson >= 0.7),
      pct_neg    = mean(cor_pearson < 0)
    )
  print(intensity_summary)
  cat("\n")
  
  # 4. Sample sizes by threshold
  cat("Sample sizes at different thresholds:\n")
  for (t in c(10, 25, 50, 75, 100, 150, 200)) {
    n <- length(unique(mun_ssiv$lgcode[mun_ssiv$total_migrants_2001 >= t]))
    cat("  ≥", t, "migrants:", n, "munis\n")
  }
  
  invisible(list(
    identity = identity_check,
    variance = var_share,
    intensity = intensity_summary
  ))
}

# Usage:
# run_ssiv_diagnostics(mun_ssiv, mun_correlations, threshold = 50)
```

---

*Document compiled from systematic diagnostic exercise on Nepal SSIV construction. Each section's findings reference specific outputs from the corresponding R diagnostic.*
