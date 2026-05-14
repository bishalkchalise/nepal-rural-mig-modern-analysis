################################################################################
#
# DOFE-BASED SHIFT-SHARE INSTRUMENT (alternative baseline shares)
# ------------------------------------------------------------------------------
# Purpose : Build alternative SSIV panels using DOFE (admin permit) destination
#           shares from different baseline years, for comparison against the
#           existing 2001 census-based SSIV.
#
# Baselines:
#   - 2009 share        (earliest DOFE year)
#   - 2010 share
#   - 2011 share
#   - 2009-2011 average share
#
# (DOFE coverage starts in 2009, so 2008 is not available - using 2009 as
#  the earliest.)
#
# Construction (each baseline year base):
#   share_dc(base) = mig_dc(base) / sum_c mig_dc(base)
#   fx_index_base(c,t) = fx_to_npr(c,t) / fx_to_npr(c,base)
#   fxshock_dt(base) = sum_c share_dc(base) * fx_index_base(c,t)
#
# India is dropped from the destination set (NPR-INR effectively pegged).
#
# Inputs :
#   - district-analysis/data/clean/foreign_migration_district_country_annual.csv
#   - district-analysis/data/clean/forex_2000_2023.csv
#
# Output :
#   - district-analysis/data/clean/instrument/instrument_dofe_dist.csv
#     columns: dname, year, fxshock_2009, fxshock_2010, fxshock_2011,
#              fxshock_avg
#
# Source : run from repo root,
#            source("district-analysis/script/vars/instrument_dofe.R")
#
################################################################################

rm(list = ls()); cat("\14")

suppressPackageStartupMessages({
  library(tidyverse)
})

# ------------------------------------------------------------------------------
# 1. Load FX panel (same construction as instrument.R)
# ------------------------------------------------------------------------------

forex_raw <- read.csv("district-analysis/data/clean/forex_2000_2023.csv") %>%
  filter(year >= 1999)

nepal_fx <- forex_raw %>%
  filter(country == "Nepal") %>%
  transmute(year, npr_per_usd = as.numeric(forex))

fx_panel <- forex_raw %>%
  transmute(country, year, lcu_per_usd = as.numeric(forex)) %>%
  left_join(nepal_fx, by = "year") %>%
  # LCU per NPR (Khanna et al convention). Falls as NPR depreciates.
  mutate(fx_to_npr = lcu_per_usd / npr_per_usd) %>%
  filter(country != "Nepal", country != "India") %>%
  select(country, year, fx_to_npr) %>%
  filter(!is.na(fx_to_npr))

# ------------------------------------------------------------------------------
# 2. Load DOFE, drop India, normalize district names
# ------------------------------------------------------------------------------

dofe_to_census <- c(
  "CHITWAN"    = "Chitawan",
  "DHANUSHA"   = "Dhanusa",
  "KAPILVASTU" = "Kapilbastu",
  "MAKAWANPUR" = "Makwanpur",
  "TANAHUN"    = "Tanahu",
  "TEHRATHUM"  = "Terhathum"
)

dofe <- read.csv(
  "district-analysis/data/clean/foreign_migration_district_country_annual.csv",
  stringsAsFactors = FALSE
) %>%
  filter(country != "India", !is.na(total_migrants)) %>%
  group_by(district_rename, country, year) %>%
  summarise(mig = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    dname = ifelse(!is.na(dofe_to_census[district_rename]),
                   dofe_to_census[district_rename],
                   str_to_title(tolower(district_rename)))
  ) %>%
  select(dname, country, year, mig)

# ------------------------------------------------------------------------------
# 3. Helper: build SSIV for a given baseline-year vector
# ------------------------------------------------------------------------------
# Compute district x country shares averaged over the supplied baseline years
# (could be a single year or multiple). Then apply the fx index normalized to
# the mid-year of the baseline window.

build_dofe_ssiv <- function(dofe_panel, fx_panel,
                            baseline_years, label) {

  # Aggregate baseline counts across the window
  base_counts <- dofe_panel %>%
    filter(year %in% baseline_years) %>%
    group_by(dname, country) %>%
    summarise(mig_base = sum(mig, na.rm = TRUE), .groups = "drop")

  # Per-district denominator
  base_shares <- base_counts %>%
    group_by(dname) %>%
    mutate(share = mig_base / sum(mig_base, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(share > 0)

  # FX index: normalize to the midpoint of the baseline window
  mid_base <- round(mean(baseline_years))
  fx_norm <- fx_panel %>%
    group_by(country) %>%
    mutate(
      fx_base = fx_to_npr[year == mid_base][1],
      fx_index = fx_to_npr / fx_base
    ) %>%
    ungroup() %>%
    filter(!is.na(fx_index)) %>%
    select(country, year, fx_index)

  # Combine shares and FX, sum across destinations
  share_fx <- base_shares %>%
    inner_join(fx_norm, by = "country") %>%
    group_by(dname, year) %>%
    summarise(
      fxshock = sum(share * fx_index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(!!paste0("fxshock_", label) := fxshock)

  share_fx
}

# ------------------------------------------------------------------------------
# 4. Build the four DOFE SSIVs
# ------------------------------------------------------------------------------

ssiv_2009    <- build_dofe_ssiv(dofe, fx_panel, 2009,      "2009")
ssiv_2010    <- build_dofe_ssiv(dofe, fx_panel, 2010,      "2010")
ssiv_2011    <- build_dofe_ssiv(dofe, fx_panel, 2011,      "2011")
ssiv_0910    <- build_dofe_ssiv(dofe, fx_panel, 2009:2010, "dofe")  # 2009-2010 avg

instrument_dofe <- ssiv_2009 %>%
  full_join(ssiv_2010, by = c("dname", "year")) %>%
  full_join(ssiv_2011, by = c("dname", "year")) %>%
  full_join(ssiv_0910, by = c("dname", "year")) %>%
  arrange(dname, year)
# `fxshock_dofe` column = SSIV built with 2009-2010 average DOFE shares.
# Used downstream as the alternative shifter alongside the 2001-census-share
# version (`fxshock` in instrument_forex_dist.csv).

# ------------------------------------------------------------------------------
# 5. Save
# ------------------------------------------------------------------------------

dir.create("district-analysis/data/clean/instrument",
           recursive = TRUE, showWarnings = FALSE)

write.csv(instrument_dofe,
          "district-analysis/data/clean/instrument/instrument_dofe_dist.csv",
          row.names = FALSE)

cat(sprintf(
  "Saved: instrument_dofe_dist.csv  (%d rows, %d districts, %d years)\n",
  nrow(instrument_dofe),
  length(unique(instrument_dofe$dname)),
  length(unique(instrument_dofe$year))
))
