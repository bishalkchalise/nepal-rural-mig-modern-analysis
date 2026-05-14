################################################################################
#
# DISTRICT-LEVEL SHIFT-SHARE INSTRUMENT (SSIV)
# ------------------------------------------------------------------------------
# Purpose : Build a district-year panel of forex-based shift-share instruments
#           for Nepali international migration, using 2001 Census baseline
#           destination shares and annual exchange-rate shocks (2001-2023).
#
# Geography : District (`dname`). The municipality-era version lives on
#             the `main` branch.
#
# Working directory : run from the repo root, e.g.
#                     source("district-analysis/script/vars/instrument.R")
#
# Inputs :
#   - data/raw/Full Census Data/Census 2001/fullmi01_full_absentee.dta
#   - data/raw/Full Census Data/Census 2011/censusid2011.xlsx
#   - data/raw/old vdc to local level.xlsx
#   - data/raw/Full Census Data/Census 2001/fullpi01_full.dta
#   - district-analysis/data/clean/forex_2000_2023.csv
#
# Outputs :
#   - district-analysis/data/clean/instrument/instrument_forex_dist.csv
#   - district-analysis/data/clean/instrument/dest_region_shares_2001.csv
#
# Methodology : identical to the archived municipality script - five forex
# shifters Z_{d,t} (level index, YoY growth, YoY log, decadal growth, decadal
# log) crossed with three weighting families (share-weighted shock, per-capita
# SSIV, absolute exposure).  See archive header for full derivation.
#
################################################################################

options(scipen = 999)

library(tidyverse)
library(haven)
library(readxl)
library(janitor)
library(countrycode)
library(stringr)
library(tidyr)
library(ggplot2)


################################################################################
# SECTION 1: LOAD RAW DATA AND BUILD ID -> dname CROSSWALK
################################################################################

mig_2001_raw <- haven::read_dta(
  "data/raw/Full Census Data/Census 2001/fullmi01_full_absentee.dta"
) %>%
  mutate(ddvvv = as.integer(batchid) %/% 100)

census_2011_id <- readxl::read_xlsx(
  "data/raw/Full Census Data/Census 2011/censusid2011.xlsx"
) %>%
  janitor::clean_names()

vdc_to_lg_map <- readxl::read_xlsx(
  "data/raw/old vdc to local level.xlsx"
) %>%
  rename(dcode  = dist,
         lgcode = vmun_code)

# Keep the lgcode map only to recover `dname` (district name) for each
# 2001 record - the new district pipeline does not use lgcode itself.
census_2011_mapped <- census_2011_id %>%
  left_join(
    vdc_to_lg_map,
    by = c("dname" = "dist_name", "vname" = "vname")
  ) %>%
  select(-dist) %>%
  filter(!is.na(dname))

mig_2001_final <- mig_2001_raw %>%
  left_join(
    census_2011_mapped,
    by = c("ddvvv"  = "ddvvvcbs11",
           "vdcmun" = "vdcmun",
           "ward"   = "ward")
  )


################################################################################
# SECTION 2: DESTINATION COUNTRY CODING
################################################################################

country_names <- c(
  "1"  = "India",     "2" = "Pakistan",   "3" = "Bangladesh",
  "4"  = "Bhutan",    "5" = "Sri Lanka",  "6" = "Maldives",
  "7"  = "China",     "8" = "Korea",
  "9"  = "Russia and Former States of USSR",
  "10" = "Japan",     "11" = "Hong Kong",
  "12" = "Singapore", "13" = "Malaysia",
  "14" = "Australia",
  "15" = "Saudi Arabia", "16" = "Qatar",  "17" = "Kuwait",
  "18" = "United Arab Emirates", "19" = "Bahrain",
  "21" = "United Kingdom", "22" = "Germany",
  "23" = "France",         "24" = "Other European Countries",
  "25" = "America, Canada and Mexico",
  "20" = "Other Asian Countries",
  "96" = "Other countries"
)

mig_2001_final <- mig_2001_final %>%
  mutate(q12_cnty = country_names[as.character(q12_cnty)])


################################################################################
# SECTION 3: CLEAN AND AGGREGATE MIGRATION FLOWS (DISTRICT)
################################################################################

mig_2001_final <- mig_2001_final %>%
  mutate(country = case_when(
    q12_cnty == "America, Canada and Mexico"                         ~ "United States",
    q12_cnty %in% c("Other Asian Countries",
                    "Other European Countries",
                    "Other countries")                               ~ "Others",
    TRUE                                                             ~ q12_cnty
  )) %>%
  mutate(country = ifelse(
    country != "Others",
    countrycode(country,
                origin      = "country.name",
                destination = "country.name",
                warn        = FALSE),
    country
  )) %>%
  mutate(country = ifelse(is.na(country), "Others", country))

# India dropped: NPR-INR peg + open-border corridor make it an unusable
# destination for forex-shifter SSIV identification.
dist_mig_pop_2001 <- mig_2001_final %>%
  filter(country != "India", !is.na(dname)) %>%
  filter(q12_rsn != 6) %>%  # drop absentees due to marriage
  group_by(dname, country) %>%
  summarise(dist_mig_pop_2001 = n(), .groups = "drop")


################################################################################
# SECTION 4: DISTRICT POPULATION + MIGRATION INTENSITY
################################################################################

census_ind_2001 <- read_dta(
  "data/raw/Full Census Data/Census 2001/fullpi01_full.dta"
) %>%
  mutate(ddvvv = as.integer(batchid) %/% 100)

census_ind_2001_geo <- census_ind_2001 %>%
  left_join(
    census_2011_mapped,
    by = c("dist"   = "dcode",
           "vdcmun" = "vdcmun",
           "ward"   = "ward")
  )

dist_pop <- census_ind_2001_geo %>%
  filter(!is.na(dname)) %>%
  group_by(dname) %>%
  summarise(dist_pop_2001 = n(), .groups = "drop")

gc()

dist_totals <- dist_mig_pop_2001 %>%
  group_by(dname) %>%
  summarise(total_mig_2001 = sum(dist_mig_pop_2001, na.rm = TRUE),
            .groups = "drop")

dist_mig_intensity <- dist_pop %>%
  left_join(dist_totals, by = "dname") %>%
  mutate(
    total_mig_2001      = replace_na(total_mig_2001, 0),
    dist_intensity_2001 = if_else(dist_pop_2001 > 0,
                                  round(total_mig_2001 / dist_pop_2001, 8),
                                  NA_real_)
  ) %>%
  rename(dist_total_migrants_2001 = total_mig_2001)


################################################################################
# SECTION 5: FOREX PANEL (SHIFTER) CONSTRUCTION
################################################################################

forex_raw <- read.csv("district-analysis/data/clean/forex_2000_2023.csv") %>%
  filter(year >= 1999)

nepal_fx <- forex_raw %>%
  filter(country == "Nepal") %>%
  transmute(year, npr_per_usd = as.numeric(forex))

fx_panel <- forex_raw %>%
  transmute(country, year, lcu_per_usd = as.numeric(forex)) %>%
  left_join(nepal_fx, by = "year") %>%
  # LCU per NPR (Khanna et al convention). Falls as NPR depreciates (1 NPR
  # buys fewer units of destination LCU). A more-negative shock = better for
  # migrant remittance value (each LCU converts to more NPR back home).
  mutate(fx_to_npr = lcu_per_usd / npr_per_usd) %>%
  filter(country != "Nepal", country != "India")

fx_base_2001 <- fx_panel %>%
  filter(year == 2001) %>%
  transmute(country, fx_to_npr_2001 = fx_to_npr)

fx_panel <- fx_panel %>%
  left_join(fx_base_2001, by = "country") %>%
  filter(!is.na(fx_to_npr), !is.na(fx_to_npr_2001)) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    fx_index_2001 = fx_to_npr / fx_to_npr_2001,
    fx_growth_yoy = (fx_to_npr / lag(fx_to_npr, 1)) - 1,
    fx_log_yoy    = log(fx_to_npr) - log(lag(fx_to_npr, 1)),

    .base_2001 = if (any(year == 2001)) fx_to_npr[year == 2001][1] else NA_real_,
    .base_2011 = if (any(year == 2011)) fx_to_npr[year == 2011][1] else NA_real_,

    fx_growth_dec = case_when(
      year == 2011 ~ (fx_to_npr / .base_2001) - 1,
      year == 2021 ~ (fx_to_npr / .base_2011) - 1,
      TRUE         ~ NA_real_
    ),
    fx_log_dec = case_when(
      year == 2011 ~ log(fx_to_npr) - log(.base_2001),
      year == 2021 ~ log(fx_to_npr) - log(.base_2011),
      TRUE         ~ NA_real_
    )
  ) %>%
  select(-.base_2001, -.base_2011) %>%
  ungroup() %>%
  filter(year >= 2000, year <= 2023) %>%
  mutate(across(c(lcu_per_usd, npr_per_usd, fx_to_npr, fx_to_npr_2001,
                  fx_index_2001, fx_growth_yoy, fx_log_yoy,
                  fx_growth_dec, fx_log_dec),
                ~ round(.x, 10))) %>%
  select(country, year,
         lcu_per_usd, npr_per_usd,
         fx_to_npr, fx_to_npr_2001,
         fx_index_2001,
         fx_growth_yoy, fx_log_yoy,
         fx_growth_dec, fx_log_dec)

cat("\n--- SHIFTER PANEL: years covered ---\n")
print(range(fx_panel$year, na.rm = TRUE))


################################################################################
# SECTION 6: SHIFT-SHARE PANEL BUILDER
################################################################################

build_ssiv_panel <- function(mig_counts, intensity, fx, id_cols,
                             pop_col, count_col) {

  mig_clean <- mig_counts %>%
    mutate(country = str_squish(country)) %>%
    filter(!is.na(country), country != "")

  fx_clean <- fx %>%
    mutate(country = str_squish(country)) %>%
    filter(!is.na(country), country != "")

  mig_shares <- mig_clean %>%
    group_by(across(all_of(id_cols))) %>%
    mutate(
      geog_total_2001 = sum(.data[[count_col]], na.rm = TRUE),
      mig_share_2001  = .data[[count_col]] / geog_total_2001
    ) %>%
    ungroup()

  dest_year <- mig_shares %>%
    tidyr::crossing(year = unique(fx_clean$year)) %>%
    inner_join(fx_clean, by = c("country", "year")) %>%
    left_join(intensity, by = id_cols)

  ssiv <- dest_year %>%
    group_by(across(all_of(c(id_cols, "year")))) %>%
    summarise(
      geog_pop_2001       = first(.data[[pop_col]]),
      geog_total_mig_2001 = first(geog_total_2001),
      geog_intensity_2001 = first(geog_total_mig_2001 / geog_pop_2001),

      shareshock_index_2001 = sum(mig_share_2001 * fx_index_2001, na.rm = TRUE),
      shareshock_growth_yoy = sum(mig_share_2001 * fx_growth_yoy, na.rm = TRUE),
      shareshock_log_yoy    = sum(mig_share_2001 * fx_log_yoy,    na.rm = TRUE),
      shareshock_growth_dec = sum(mig_share_2001 * fx_growth_dec, na.rm = TRUE),
      shareshock_log_dec    = sum(mig_share_2001 * fx_log_dec,    na.rm = TRUE),

      ssiv_index_2001 = sum(.data[[count_col]] * fx_index_2001, na.rm = TRUE)
                        / first(.data[[pop_col]]),
      ssiv_growth_yoy = sum(.data[[count_col]] * fx_growth_yoy, na.rm = TRUE)
                        / first(.data[[pop_col]]),
      ssiv_log_yoy    = sum(.data[[count_col]] * fx_log_yoy,    na.rm = TRUE)
                        / first(.data[[pop_col]]),
      ssiv_growth_dec = sum(.data[[count_col]] * fx_growth_dec, na.rm = TRUE)
                        / first(.data[[pop_col]]),
      ssiv_log_dec    = sum(.data[[count_col]] * fx_log_dec,    na.rm = TRUE)
                        / first(.data[[pop_col]]),

      absexp_index_2001 = sum(.data[[count_col]] * fx_index_2001, na.rm = TRUE),
      absexp_growth_yoy = sum(.data[[count_col]] * fx_growth_yoy, na.rm = TRUE),
      absexp_log_yoy    = sum(.data[[count_col]] * fx_log_yoy,    na.rm = TRUE),
      absexp_growth_dec = sum(.data[[count_col]] * fx_growth_dec, na.rm = TRUE),
      absexp_log_dec    = sum(.data[[count_col]] * fx_log_dec,    na.rm = TRUE),

      .groups = "drop"
    ) %>%
    mutate(
      check_ssiv_index_2001 = shareshock_index_2001 * geog_intensity_2001,
      check_ssiv_log_yoy    = shareshock_log_yoy    * geog_intensity_2001,
      check_ssiv_log_dec    = shareshock_log_dec    * geog_intensity_2001,
      diff_index_2001       = ssiv_index_2001 - check_ssiv_index_2001,
      diff_log_yoy          = ssiv_log_yoy    - check_ssiv_log_yoy,
      diff_log_dec          = ssiv_log_dec    - check_ssiv_log_dec
    )

  ssiv
}


################################################################################
# SECTION 7: BUILD DISTRICT PANEL
################################################################################

dist_ssiv <- build_ssiv_panel(
  mig_counts = dist_mig_pop_2001,
  intensity  = dist_mig_intensity,
  fx         = fx_panel,
  id_cols    = "dname",
  pop_col    = "dist_pop_2001",
  count_col  = "dist_mig_pop_2001"
)


################################################################################
# SECTION 8: DIAGNOSTICS
################################################################################

cat("\n=== IDENTITY CHECK: ssiv == MI * shareshock (max abs deviation, district) ===\n")
dist_ssiv %>%
  summarise(
    max_diff_index   = max(abs(diff_index_2001), na.rm = TRUE),
    max_diff_log_yoy = max(abs(diff_log_yoy),    na.rm = TRUE),
    max_diff_log_dec = max(abs(diff_log_dec),    na.rm = TRUE)
  ) %>% print()

cat("\n--- EFFECTIVE NUMBER OF SHOCKS (inverse Herfindahl, district avg shares) ---\n")
dist_shares_avg <- dist_mig_pop_2001 %>%
  group_by(dname) %>%
  mutate(share = dist_mig_pop_2001 / sum(dist_mig_pop_2001, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(avg_share = mean(share, na.rm = TRUE), .groups = "drop") %>%
  mutate(avg_share = avg_share / sum(avg_share))

cat("Effective N shocks:", round(1 / sum(dist_shares_avg$avg_share^2), 2), "\n")
cat("Raw N destinations:", nrow(dist_shares_avg), "\n\n")

cat("--- TOP 10 DESTINATIONS BY AVERAGE DISTRICT SHARE ---\n")
dist_shares_avg %>% arrange(desc(avg_share)) %>% slice_head(n = 10) %>% print()


################################################################################
# SECTION 9: VISUAL CHECK (mean district exposure over time)
################################################################################

plot_df <- dist_ssiv %>%
  group_by(year) %>%
  summarise(
    shareshock_index_2001 = mean(shareshock_index_2001, na.rm = TRUE),
    ssiv_index_2001       = mean(ssiv_index_2001,       na.rm = TRUE),
    ssiv_growth_yoy       = mean(ssiv_growth_yoy,       na.rm = TRUE),
    ssiv_log_yoy          = mean(ssiv_log_yoy,          na.rm = TRUE),
    ssiv_growth_dec       = mean(ssiv_growth_dec,       na.rm = TRUE),
    ssiv_log_dec          = mean(ssiv_log_dec,          na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "instrument", values_to = "value")

ggplot(plot_df, aes(x = year, y = value, color = instrument)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ instrument, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title    = "District-Level Mean SSIV over Time",
    subtitle = "Each panel on its own scale; baseline values reflect construction",
    y        = "SSIV value",
    x        = "Year"
  )


################################################################################
# SECTION 10: FINAL PANEL + EXPORT
################################################################################

dist_fx_panel <- dist_ssiv %>% select(-starts_with("check_"), -starts_with("diff_"))

# Intuitive aliases used by downstream estimation scripts
add_intuitive_aliases <- function(df) {
  df %>% mutate(
    fxshock                 = shareshock_index_2001,
    mig_intensity           = geog_intensity_2001,
    fxshock_x_mig_intensity = ssiv_index_2001,
    total_migrants          = geog_total_mig_2001
  )
}
dist_fx_panel <- add_intuitive_aliases(dist_fx_panel)

cat("\n--- FINAL DISTRICT PANEL DIMENSIONS ---\n")
cat("District:", nrow(dist_fx_panel), "rows,",
    n_distinct(dist_fx_panel$dname), "districts\n")

dir.create("district-analysis/data/clean/instrument",
           recursive = TRUE, showWarnings = FALSE)
write.csv(dist_fx_panel,
          "district-analysis/data/clean/instrument/instrument_forex_dist.csv",
          row.names = FALSE)


################################################################################
# SECTION 11: BASELINE DESTINATION-REGION SHARES (district x region)
################################################################################
# Used downstream as Khanna-style baseline X (region composition) controls.

country_to_region <- function(c) {
  gulf      <- c("Saudi Arabia","Qatar","United Arab Emirates",
                 "Kuwait","Bahrain","Oman")
  oth_wasia <- c("Iraq","Iran","Lebanon","Israel","Jordan","Yemen","Syria")
  e_asia    <- c("Korea","Japan","China","Hong Kong","Taiwan")
  se_asia   <- c("Malaysia","Singapore","Thailand","Indonesia","Philippines")
  s_asia    <- c("Pakistan","Bangladesh","Bhutan","Sri Lanka","Maldives","Afghanistan")
  oecd_n    <- c("United States","Canada","Mexico","Australia","New Zealand")
  oecd_eu   <- c("United Kingdom","Germany","France","Italy","Spain",
                 "Portugal","Netherlands","Belgium","Sweden","Norway",
                 "Denmark","Finland","Ireland","Austria","Switzerland",
                 "Greece","Poland","Czechia","Slovakia","Hungary",
                 "Romania","Croatia","Malta","Cyprus","Slovenia")
  case_when(
    c %in% gulf      ~ "gulf",
    c %in% oth_wasia ~ "oth_wasia",
    c %in% e_asia    ~ "e_asia",
    c %in% se_asia   ~ "se_asia",
    c %in% s_asia    ~ "s_asia",
    c %in% oecd_n    ~ "oecd_north",
    c %in% oecd_eu   ~ "oecd_europe",
    TRUE             ~ "other"
  )
}

dist_region_shares <- dist_mig_pop_2001 %>%
  mutate(region = country_to_region(country)) %>%
  group_by(dname, region) %>%
  summarise(n = sum(dist_mig_pop_2001), .groups = "drop") %>%
  group_by(dname) %>%
  mutate(share = n / sum(n)) %>%
  select(dname, region, share) %>%
  tidyr::pivot_wider(names_from   = region,
                     values_from  = share,
                     values_fill  = 0,
                     names_prefix = "share_")

cat("\n--- DESTINATION-REGION SHARES, DISTRICT ---\n")
cat("Districts:", nrow(dist_region_shares), "\n")
cat("Columns: ",
    paste(setdiff(names(dist_region_shares), "dname"), collapse = ", "),
    "\n\n")

write.csv(dist_region_shares,
          "district-analysis/data/clean/instrument/dest_region_shares_2001.csv",
          row.names = FALSE)

################################################################################
# END OF SCRIPT
################################################################################
