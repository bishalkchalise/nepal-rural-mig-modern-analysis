################################################################################
# Robustness across 4 datasets x 2 scalings (log_mi_z, mig_w90_z) x M2/M3/M4.
#
# Datasets:
#   - census   : district x year panel (2011, 2021)             FE: dname + year
#   - hh       : RVS HH x year panel (2016-2018, all sections)   FE: hhid  + year
#   - nec_cs   : NEC 2018 district cross-section                 no FE
#   - nec_panel: skipped (no district-level entry-cohort file)
#
# Outcomes are taken from the robustness portal (docs/robustness.json).
# Treatment-only M1 skipped per user request.
#
# Output: district-analysis/output/tab/robustness_all_panels.csv (long format)
#   columns: dataset, outcome, scaling, lag, model, beta, se, p, sig, mean_y, n
# Robustness grid:
#   scalings: log (baseline z of log(mig/1000)), lin (z linear), raw (no z)
#   lags:     0, 1, 2 (baseline), 3, 5 years
#   models:   M2 / M3 / M4 (Khanna control ladder)
#
# Run: source("district-analysis/script/_robustness_all_panels.R")
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

t0 <- Sys.time()

# ---- name normalization ----
dofe_to_census <- c(
  "CHITWAN" = "Chitawan", "DHANUSHA" = "Dhanusa", "KAPILVASTU" = "Kapilbastu",
  "MAKAWANPUR" = "Makwanpur", "TANAHUN" = "Tanahu", "TEHRATHUM" = "Terhathum",
  "KABHREPALANCHOK" = "Kavrepalanchok"
)
to_dname <- function(s) {
  u <- toupper(trimws(as.character(s)))
  ifelse(u %in% names(dofe_to_census),
         dofe_to_census[u],
         tools::toTitleCase(tolower(as.character(s))))
}

DIST_LOOKUP <- c(
  "101"="Taplejung","102"="Sankhuwasabha","103"="Solukhumbu","104"="Okhaldhunga",
  "105"="Khotang","106"="Bhojpur","107"="Dhankuta","108"="Terhathum",
  "109"="Panchthar","110"="Ilam","111"="Jhapa","112"="Morang","113"="Sunsari",
  "114"="Udayapur","201"="Saptari","202"="Siraha","203"="Dhanusa","204"="Mahottari",
  "205"="Sarlahi","206"="Rautahat","207"="Bara","208"="Parsa",
  "301"="Dolakha","302"="Sindhupalchok","303"="Rasuwa","304"="Dhading",
  "305"="Nuwakot","306"="Kathmandu","307"="Bhaktapur","308"="Lalitpur",
  "309"="Kavrepalanchok","310"="Ramechhap","311"="Sindhuli","312"="Makwanpur",
  "313"="Chitawan","401"="Gorkha","402"="Manang","403"="Mustang","404"="Myagdi",
  "405"="Kaski","406"="Lamjung","407"="Tanahu","408"="Nawalparasi_E","409"="Syangja",
  "410"="Parbat","411"="Baglung","501"="Rukum_E","502"="Rolpa","503"="Pyuthan",
  "504"="Gulmi","505"="Arghakhanchi","506"="Palpa","507"="Nawalparasi_W",
  "508"="Rupandehi","509"="Kapilbastu","510"="Dang","511"="Banke","512"="Bardiya",
  "601"="Dolpa","602"="Mugu","603"="Humla","604"="Jumla","605"="Kalikot",
  "606"="Dailekh","607"="Jajarkot","608"="Rukum_W","609"="Salyan","610"="Surkhet",
  "701"="Bajura","702"="Bajhang","703"="Darchula","704"="Baitadi","705"="Dadeldhura",
  "706"="Doti","707"="Achham","708"="Kailali","709"="Kanchanpur"
)

# ---- load ----
forex    <- read_csv("district-analysis/data/clean/forex_2000_2023.csv", show_col_types = FALSE)
dofe_raw <- read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv", show_col_types = FALSE)
pop_file <- read_csv("district-analysis/data/clean/foreign_migration_district_population.csv", show_col_types = FALSE)
regions  <- read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv", show_col_types = FALSE)
census   <- read_csv("district-analysis/data/clean/census/outcomes_district.csv", show_col_types = FALSE)

# Out-migration outcomes (built by district-analysis/script/vars/outcome_census_outmig.R)
outmig_path <- "district-analysis/data/clean/census/outmig_district_long.csv"
if (file.exists(outmig_path)) {
  outmig <- read_csv(outmig_path, show_col_types = FALSE)
  census <- census %>% left_join(outmig, by = c("dname", "year"))
  cat(sprintf("Joined out-migration outcomes from %s (%d rows)\n",
              outmig_path, nrow(outmig)))
} else {
  cat(sprintf("NOTE: %s not found — out-migration outcomes skipped\n", outmig_path))
}
nec_cs   <- if (file.exists("district-analysis/data/clean/nec/nec_2018_district.csv")) {
              read_csv("district-analysis/data/clean/nec/nec_2018_district.csv", show_col_types = FALSE)
            } else {
              NULL
            }
if (!is.null(nec_cs) && "DIST" %in% names(nec_cs) && !"dname" %in% names(nec_cs)) {
  nec_cs$dname <- DIST_LOOKUP[as.character(nec_cs$DIST)]
}

# Collapse split districts (Nawalparasi_E/W, Rukum_E/W) to base names so they
# match mi / z_v2 / regions / outcomes_district. Aggregate duplicates: sums for
# counts, weighted means (by n_firms) for shares/indices.
collapse_nec_splits <- function(df) {
  if (is.null(df) || !"dname" %in% names(df)) return(df)
  df <- df %>%
    mutate(dname = case_when(
      dname %in% c("Nawalparasi_E", "Nawalparasi_W") ~ "Nawalparasi",
      dname %in% c("Rukum_E", "Rukum_W")             ~ "Rukum",
      TRUE                                           ~ dname
    ))
  if (!"n_firms" %in% names(df)) return(df)
  share_cols <- grep("^share_|formality_index|^mean_|^p50_|^p90_|_per_firm$",
                     names(df), value = TRUE)
  count_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))],
                        c(share_cols, "DIST", "n_firms"))
  df %>%
    group_by(dname) %>%
    summarise(
      across(all_of(share_cols), ~ weighted.mean(.x, w = n_firms, na.rm = TRUE)),
      across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
      n_firms = sum(n_firms, na.rm = TRUE),
      .groups = "drop"
    )
}
nec_cs <- collapse_nec_splits(nec_cs)

# HH dname crosswalk
hh_dist <- read_csv("district-analysis/data/clean/rvs/migration_hh_year.csv",
                    show_col_types = FALSE) %>%
  mutate(dname = to_dname(district_name)) %>%
  select(hhid, year, dname) %>% distinct()

# ---- FX rer ----
nepal_fx <- forex %>% filter(country == "Nepal") %>%
  transmute(year, npr_per_usd = forex)
fx <- forex %>%
  filter(!country %in% c("Nepal", "India"), !is.na(forex)) %>%
  transmute(country, year, lcu_per_usd = forex) %>%
  inner_join(nepal_fx, by = "year") %>%
  mutate(log_npr_per_lcu = log(npr_per_usd / lcu_per_usd)) %>%
  filter(!is.na(log_npr_per_lcu)) %>%
  group_by(country) %>%
  mutate(base_2010 = log_npr_per_lcu[year == 2010][1]) %>%
  ungroup() %>%
  filter(!is.na(base_2010)) %>%
  mutate(rer = log_npr_per_lcu - base_2010) %>%
  select(country, year, rer)

# ---- shares + z ----
dofe <- dofe_raw %>%
  filter(!is.na(country)) %>%
  mutate(dname = to_dname(district_rename)) %>%
  group_by(dname, country, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  filter(!country %in% c("Nepal", "India"))

v2_tot <- dofe %>% filter(year %in% c(2009, 2010)) %>%
  group_by(country) %>% summarise(tot = sum(permits), .groups = "drop")
set_v2 <- sort(intersect(v2_tot$country[v2_tot$tot > 0], unique(fx$country)))

sh_v2 <- dofe %>%
  filter(year %in% c(2009, 2010), country %in% set_v2) %>%
  group_by(dname, country) %>%
  summarise(permits = sum(permits), .groups = "drop") %>%
  group_by(dname) %>%
  mutate(share = permits / sum(permits)) %>%
  ungroup() %>%
  select(dname, country, share)

z_v2 <- sh_v2 %>%
  inner_join(fx, by = "country", relationship = "many-to-many") %>%
  mutate(x = share * rer) %>%
  group_by(dname, year) %>%
  summarise(z_v2 = sum(x, na.rm = TRUE), .groups = "drop")

# ---- migration intensity: two scalings ----
winsor <- function(x, lo, hi) {
  q <- quantile(x, probs = c(lo, hi), na.rm = TRUE); pmin(pmax(x, q[1]), q[2])
}

mi <- dofe %>%
  filter(year %in% c(2009, 2010)) %>%
  group_by(dname) %>%
  summarise(num = sum(permits, na.rm = TRUE) / 2, .groups = "drop") %>%   # avg annual permits across 2009-10
  left_join(
    pop_file %>%
      mutate(dname = to_dname(district)) %>%
      select(dname, pop_2011 = district_population_2011) %>%
      distinct(dname, .keep_all = TRUE),
    by = "dname"
  ) %>%
  mutate(mig_per_1000 = (num / pop_2011) * 1000,
         mig_log      = log(pmax(mig_per_1000, 1e-6)),
         log_mi_z     = (mig_log - mean(mig_log)) / sd(mig_log),         # baseline scaling (z of log)
         lin_mi_z     = (mig_per_1000 - mean(mig_per_1000)) /
                        sd(mig_per_1000),                                 # robustness scaling: z linear
         mi_raw       = mig_per_1000) %>%                                 # robustness scaling: raw (mig per 1k pop)
  select(dname, log_mi_z, lin_mi_z, mi_raw)

REGION_COLS <- c("share_e_asia", "share_gulf", "share_oecd_north",
                 "share_s_asia", "share_se_asia", "share_oecd_europe")

# Districts with no 2001 foreign migrants (e.g. Kalikot) have NA region shares,
# which causes feols to silently drop them from M4. Fill with 0 so the row is
# kept (substantively: a district with no recorded 2001 destinations
# contributes 0 to all destination-region interactions).
fill_region_na <- function(df) {
  if (is.null(df)) return(df)
  cols <- intersect(REGION_COLS, names(df))
  if (!length(cols)) return(df)
  df %>% mutate(across(all_of(cols), ~ if_else(is.na(.x), 0, .x)))
}

stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p<0.01, "***", ifelse(p<0.05, "**", ifelse(p<0.10, "*", ""))))
}

# ---- robustness grid ----
SCALINGS <- c("log", "lin", "raw")   # mi scaling alternatives
LAGS     <- c(0L, 1L, 2L, 3L, 5L)    # lag-of-shock alternatives (years)
BASELINE_SCALING <- "log"
BASELINE_LAG     <- 2L

mi_col_for <- function(scaling) switch(scaling,
  "log" = "log_mi_z",
  "lin" = "lin_mi_z",
  "raw" = "mi_raw",
  stop("Unknown scaling: ", scaling)
)
zstd_col_for <- function(lag) sprintf("z_lag%d_std", lag)

# Attach `z_lagK` and standardised `z_lagK_std` columns for each lag in LAGS
# to a panel that already has (dname, year, ...) and joined `regions`.
attach_z_lags <- function(panel, z_v2_df) {
  for (k in LAGS) {
    zk <- z_v2_df %>% mutate(year = year + k) %>%
            rename(!!sprintf("z_lag%d", k) := z_v2)
    panel <- panel %>% left_join(zk, by = c("dname","year"))
    zcol <- sprintf("z_lag%d", k)
    if (zcol %in% names(panel)) {
      v <- panel[[zcol]]
      panel[[zstd_col_for(k)]] <-
        (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
    }
  }
  panel
}

# ---- regression dispatcher ----
# scaling:
#   "log" -> z of log(mig per 1000)   (baseline)
#   "lin" -> z of      mig per 1000
#   "raw" -> raw       mig per 1000   (no z)
# lag:
#   0, 1, 2 (baseline), 3, 5 — year-offset on the FX shifter relative to outcome
fit_one <- function(panel, ycol, scaling = BASELINE_SCALING,
                    lag = BASELINE_LAG, mode, refyr = NA) {
  mig_col  <- mi_col_for(scaling)
  zstd_col <- zstd_col_for(lag)
  if (!mig_col  %in% names(panel))  return(NULL)
  if (!zstd_col %in% names(panel))  return(NULL)
  panel$mig_var <- panel[[mig_col]]
  panel$z_L_std <- panel[[zstd_col]]
  panel <- panel[!is.na(panel$z_L_std) & !is.na(panel$mig_var), ]
  if (!nrow(panel)) return(NULL)
  panel$z_inter <- panel$z_L_std * panel$mig_var
  panel$z_bare  <- panel$z_L_std

  if (mode == "cs") {
    # cross-section, HC1 SE
    f_M2 <- as.formula(sprintf("%s ~ z_inter + mig_var", ycol))
    f_M3 <- as.formula(sprintf("%s ~ z_inter + mig_var + z_bare", ycol))
    f_M4 <- as.formula(sprintf("%s ~ z_inter + mig_var + z_bare + %s", ycol,
                               paste(REGION_COLS, collapse = " + ")))
    vcov_arg <- "hetero"
  } else {
    # panel; mode == "dname" or "hhid" (entity FE) + year FE, cluster ~dname
    region_terms <- paste(sprintf("i(year, %s, ref = %d)", REGION_COLS, refyr), collapse = " + ")
    f_M2 <- as.formula(sprintf("%s ~ z_inter + i(year, mig_var, ref = %d) | %s + year",
                               ycol, refyr, mode))
    f_M3 <- as.formula(sprintf("%s ~ z_inter + i(year, mig_var, ref = %d) + z_bare | %s + year",
                               ycol, refyr, mode))
    f_M4 <- as.formula(sprintf("%s ~ z_inter + i(year, mig_var, ref = %d) + z_bare + %s | %s + year",
                               ycol, refyr, region_terms, mode))
    vcov_arg <- NULL
  }

  do_fit <- function(f) {
    fit <- if (mode == "cs") {
      tryCatch(feols(f, data = panel, vcov = vcov_arg),
               error = function(e) NULL)
    } else {
      tryCatch(feols(f, data = panel, cluster = ~dname),
               error = function(e) NULL)
    }
    if (is.null(fit)) return(c(NA, NA, NA, NA))
    s <- summary(fit)$coeftable
    if (!"z_inter" %in% rownames(s)) return(c(NA, NA, NA, nobs(fit)))
    c(s["z_inter","Estimate"], s["z_inter","Std. Error"],
      s["z_inter","Pr(>|t|)"], nobs(fit))
  }

  m2 <- do_fit(f_M2); m3 <- do_fit(f_M3); m4 <- do_fit(f_M4)
  mean_y <- mean(panel[[ycol]], na.rm = TRUE)
  tibble(model = c("M2","M3","M4"),
         beta = c(m2[1], m3[1], m4[1]),
         se   = c(m2[2], m3[2], m4[2]),
         p    = c(m2[3], m3[3], m4[3]),
         mean_y = mean_y,
         n    = c(m2[4], m3[4], m4[4]))
}

run_outcomes <- function(panel, outcomes, mode, refyr, ds_label) {
  rows <- list()
  for (yc in outcomes) {
    if (!yc %in% names(panel)) next
    for (slbl in SCALINGS) {
      for (lag in LAGS) {
        r <- fit_one(panel, yc, slbl, lag, mode, refyr)
        if (is.null(r)) next
        r$dataset <- ds_label
        r$outcome <- yc
        r$scaling <- slbl
        r$lag     <- lag
        r$sig     <- stars(r$p)
        rows[[length(rows)+1]] <- r
      }
    }
  }
  bind_rows(rows)
}

# ---- outcome lists (trimmed per user request) ----
CENSUS_OUTCOMES <- c(
  # Out-migration (built by outcome_census_outmig.R; omitted automatically if CSV missing)
  # PERMANENT (lifetime / birth district) — 2011 Q16, 2021 q19/q20
  'mig_out_internal_share','mig_in_internal_share','net_internal_mig_share',
  'mig_out_economic_share','mig_out_noneconomic_share',
  'mig_in_economic_share','mig_in_noneconomic_share',
  # TEMPORARY (5-year window) — 2011 Q19A/Q19B/Q18, 2021 q21/q22/q25
  'mig_out_temp_share','mig_in_temp_share','net_temp_mig_share',
  'mig_out_temp_economic_share','mig_out_temp_noneconomic_share',
  'mig_in_temp_economic_share','mig_in_temp_noneconomic_share',
  # Note: `mig_in_share` (outcome_census.R) is DROPPED from the headline list
  #   because its definition differs across rounds (2011: 5-year residents only,
  #   age >= 5, codes {2,3}; 2021: full sample, codes {2,3,4} incl. abroad).
  # Headline industry/occupation/employment
  'ind_agri_forestry_fish','occ_share_managers','emp_share_employee',
  # Assets group (all amen_assets_* + count)
  'amen_asset_count_mean',
  'amen_assets_mobile','amen_assets_radio','amen_assets_tv','amen_assets_fridge',
  'amen_assets_computer','amen_assets_internet','amen_assets_landline',
  'amen_assets_cycle','amen_assets_motorcycle','amen_assets_car',
  # Amenities (non-assets amen_*)
  'amen_cooking_modern','amen_cooking_traditional','amen_cooking_lpg',
  'amen_cooking_electric','amen_cooking_biogas','amen_cooking_wood','amen_cooking_kerosene',
  'amen_lighting_electricity','amen_lighting_kerosene','amen_lighting_biogas','amen_lighting_others',
  'amen_water_piped','amen_water_traditional',
  'amen_toilet_any','amen_toilet_modern','amen_toilet_ordinary','amen_toilet_none')

NEC_CS_OUTCOMES <- c('n_firms','emp_total','mean_emp_per_firm','formality_index',
  'share_registered','share_tax_registered','share_keeps_accounts',
  'share_emp_female','share_emp_foreign',
  'share_firms_size_micro_1','share_firms_size_small_2_9',
  'share_firms_size_medium_10_50','share_firms_size_large_51p',
  'share_borrowed_any','share_formal_credit','share_any_foreign_cap')

HH_OUTCOMES <- c(
  # Migration / remittance
  'has_migrant_intl','n_intl_migrants',
  'remit_amount_intl_12m_rs','remit_received',
  # Agriculture (kept)
  'share_self_wet','share_self_dry','share_both_seasons','share_fallow_wet',
  'share_fallow_dry','share_rented_out_wet','owns_plough','owns_powered_machinery',
  'owns_irrigation_kit','owns_storage_struct','owns_transport',
  'n_equip_categories','n_powered_types','equip_stock_value_rs',
  'total_input_cost_rs','wet_cost_seed','dry_cost_seed','wet_cost_fert','dry_cost_fert',
  'wet_cost_labour','dry_cost_labour','wet_cost_insect','dry_cost_insect',
  'input_intensity_per_sqm',
  # Spending (trimmed to 6 specific items)
  'food_exp_total_7day','food_exp_purchased_7day','food_exp_homeprod_7day',
  'nonfood_exp_12m','edu_spend_total_12m','hlt_spend_total',
  # Enterprise (kept)
  'has_enterprise','n_enterprises','n_workers_total','revenue_12m','profit_12m',
  'expenses_12m','capex_12m',
  'sector_trade','sector_manufacturing','sector_services','sector_hotels','sector_transport')

# Source paths to look for HH files (district-analysis first, then archive)
HH_FILE_PATHS <- c(
  "district-analysis/data/clean/rvs/agriculture_hh_year.csv",
  "district-analysis/data/clean/rvs/enterprise_hh_year.csv",
  "district-analysis/data/clean/rvs/health_hh_year.csv",
  "district-analysis/data/clean/rvs/shocks_coping_shocked_hh_year.csv",
  "district-analysis/data/clean/rvs/social_protection_hh_year.csv",
  "district-analysis/data/clean/rvs/consumption_hh_year.csv",
  "district-analysis/data/clean/rvs/education_hh_year.csv",
  "district-analysis/data/clean/rvs/migration_hh_year.csv",
  # fallbacks in archive
  "data/clean/archive/municipality/rvs_outcomes/agriculture_hh_year.csv",
  "data/clean/archive/municipality/rvs_outcomes/enterprise_hh_year.csv",
  "data/clean/archive/municipality/rvs_outcomes/health_hh_year.csv",
  "data/clean/archive/municipality/rvs_outcomes/shocks_coping_shocked_hh_year.csv",
  "data/clean/archive/municipality/rvs_outcomes/social_protection_hh_year.csv",
  "data/clean/archive/municipality/rvs_outcomes/consumption_hh_year.csv",
  "data/clean/archive/municipality/rvs_outcomes/education_hh_year.csv"
)

# ---- prep each panel (attach multi-lag z columns for robustness grid) ----
# 1. census
cdf <- census %>% filter(year %in% c(2011, 2021)) %>%
  inner_join(mi, by = "dname") %>%
  left_join(regions, by = "dname") %>% fill_region_na() %>%
  attach_z_lags(z_v2)

cat(sprintf("Census panel: %d obs over %d districts\n",
            nrow(cdf), n_distinct(cdf$dname)))

# 2. NEC cs (cross-section: lag = years before NEC field-year 2018)
if (!is.null(nec_cs)) {
  # For cross-section, "lag k" means z at year 2018 - k.
  ncs <- nec_cs %>%
    inner_join(mi, by = "dname") %>%
    left_join(regions, by = "dname") %>% fill_region_na()
  for (k in LAGS) {
    yr <- 2018L - k
    zk <- z_v2 %>% filter(year == yr) %>% select(dname, z_v2) %>%
            rename(!!sprintf("z_lag%d", k) := z_v2)
    ncs <- ncs %>% left_join(zk, by = "dname")
    zcol <- sprintf("z_lag%d", k)
    if (zcol %in% names(ncs)) {
      v <- ncs[[zcol]]
      ncs[[zstd_col_for(k)]] <- (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
    }
  }
  cat(sprintf("NEC cs: %d districts\n", nrow(ncs)))
} else {
  ncs <- NULL
}

# 3. HH panel: merge across files; outer join on (hhid, year)
load_hh <- function() {
  hh <- NULL
  loaded <- character()
  for (p in HH_FILE_PATHS) {
    if (!file.exists(p)) next
    # avoid double-loading the same content if both locations exist
    base <- basename(p)
    if (base %in% loaded) next
    df <- read_csv(p, show_col_types = FALSE, progress = FALSE)
    keep <- intersect(c("hhid","year", HH_OUTCOMES), names(df))
    df <- df[, keep, drop = FALSE]
    if (is.null(hh)) hh <- df else hh <- full_join(hh, df, by = c("hhid","year"))
    loaded <- c(loaded, base)
  }
  hh
}

hh <- load_hh()
hh <- hh %>%
  inner_join(hh_dist, by = c("hhid","year")) %>%
  inner_join(mi, by = "dname") %>%
  left_join(regions, by = "dname") %>% fill_region_na() %>%
  attach_z_lags(z_v2)
cat(sprintf("HH panel: %d obs over %d districts, %d HH\n",
            nrow(hh), n_distinct(hh$dname), n_distinct(hh$hhid)))

# ---- RUN ----
out_rows <- list()
cat("\nRunning census panel...\n")
out_rows[[1]] <- run_outcomes(cdf, CENSUS_OUTCOMES, mode = "dname", refyr = 2011L, ds_label = "census")

cat("Running HH panel...\n")
out_rows[[2]] <- run_outcomes(hh, HH_OUTCOMES, mode = "hhid", refyr = 2016L, ds_label = "hh")

if (!is.null(ncs)) {
  cat("Running NEC cs...\n")
  out_rows[[3]] <- run_outcomes(ncs, NEC_CS_OUTCOMES, mode = "cs", refyr = NA_integer_, ds_label = "nec_cs")
}

# ---- NEC panel (entry-cohort: n_new_firms by founding year × district) ----
NEC_PANEL_FILE <- "district-analysis/data/clean/nec/nec_panel_district.csv"
NEC_PANEL_OUTCOMES <- c(
  "log_n_new_firms","log_emp_new_firms","log_rev_new_firms","log_cap_new_firms",
  "log_n_new_firms_size_micro_1","log_n_new_firms_size_small_2_9",
  "log_n_new_firms_size_medium_10_50","log_n_new_firms_size_large_51p"
)
if (file.exists(NEC_PANEL_FILE)) {
  npd <- read_csv(NEC_PANEL_FILE, show_col_types = FALSE) %>%
    filter(year >= 2011, year <= 2018) %>%   # restrict to post-2010 cohort years
    inner_join(mi, by = "dname") %>%
    left_join(regions, by = "dname") %>% fill_region_na() %>%
    attach_z_lags(z_v2)
  # Append all log_n_new_firms_<sector> outcomes that exist in the data
  sector_log_cols <- grep("^log_n_new_firms_(?!size_)", names(npd),
                          value = TRUE, perl = TRUE)
  NEC_PANEL_OUTCOMES_FULL <- c(NEC_PANEL_OUTCOMES, sector_log_cols)
  cat(sprintf("Running NEC panel (%d obs, %d outcomes)...\n",
              nrow(npd), length(NEC_PANEL_OUTCOMES_FULL)))
  out_rows[[4]] <- run_outcomes(npd, NEC_PANEL_OUTCOMES_FULL, mode = "dname",
                                refyr = 2011L, ds_label = "nec_panel")
} else {
  cat(sprintf("Skipping NEC panel: %s not found.\nRun district-analysis/script/_build_nec_panel_district.R first.\n", NEC_PANEL_FILE))
}

out <- bind_rows(out_rows) %>%
  select(dataset, outcome, scaling, lag, model, beta, se, p, sig, mean_y, n) %>%
  arrange(dataset, outcome, scaling, lag, model)

dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(out, "district-analysis/output/tab/robustness_all_panels.csv")

cat(sprintf("\nSaved %d rows to district-analysis/output/tab/robustness_all_panels.csv\n", nrow(out)))
cat(sprintf("Elapsed: %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))

cat("\nSummary by dataset / scaling / model:\n")
print(out %>% group_by(dataset, scaling, model) %>%
        summarise(n_outcomes = n_distinct(outcome),
                  pct_pos    = round(mean(beta > 0, na.rm = TRUE) * 100, 1),
                  pct_sig    = round(mean(p < 0.05, na.rm = TRUE) * 100, 1),
                  .groups = "drop"))
