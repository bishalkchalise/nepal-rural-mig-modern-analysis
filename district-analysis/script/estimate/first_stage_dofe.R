################################################################################
#
# FIRST-STAGE: DOFE PERMITS ON FOREX SSIV
# ------------------------------------------------------------------------------
# Purpose : Validate the forex shift-share instrument by regressing observed
#           foreign-migration flows (DOFE labour-permit administrative data)
#           on the SSIV shifters. Strong, significant first-stage = the SSIV
#           captures migration variation we observe in admin records.
#
# Inputs :
#   - district-analysis/data/clean/foreign_migration_district_country_annual.csv
#       schema: district_rename (UPPERCASE), country, year, Gender, total_migrants
#   - district-analysis/data/clean/instrument/instrument_forex_dist.csv
#       schema: dname, year, ssiv_index_2001, ssiv_log_yoy, shareshock_*,
#               absexp_*, geog_intensity_2001, ...
#
# Coverage : DOFE 2009-2024, instrument 2000-2023.
#            Inner join keeps 2009-2023 = 15 years * 74 districts = 1110 obs.
#
# Source : run from repo root,
#            source("district-analysis/script/estimate/first_stage_dofe.R")
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

# ------------------------------------------------------------------------------
# 1. Load DOFE permit records and aggregate to (district, year)
# ------------------------------------------------------------------------------

dofe_raw <- read.csv(
  "district-analysis/data/clean/foreign_migration_district_country_annual.csv",
  stringsAsFactors = FALSE
)

# DOFE uses ALL-CAPS district names with a few spellings that differ from the
# census/instrument convention. Map them explicitly:
dofe_to_census <- c(
  "CHITWAN"    = "Chitawan",
  "DHANUSHA"   = "Dhanusa",
  "KAPILVASTU" = "Kapilbastu",
  "MAKAWANPUR" = "Makwanpur",
  "TANAHUN"    = "Tanahu",
  "TEHRATHUM"      = "Terhathum",
  "KABHREPALANCHOK" = "Kavrepalanchok"
)

dofe_panel <- dofe_raw %>%
  group_by(district_rename, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    dname = dofe_to_census[district_rename],
    dname = ifelse(is.na(dname), str_to_title(tolower(district_rename)), dname)
  ) %>%
  select(dname, year, permits)

# ------------------------------------------------------------------------------
# 2. Load forex SSIV and join
# ------------------------------------------------------------------------------

instrument <- read.csv(
  "district-analysis/data/clean/instrument/instrument_forex_dist.csv",
  stringsAsFactors = FALSE
) %>%
  select(dname, year, fxshock, geog_intensity_2001)

instrument_dofe <- read.csv(
  "district-analysis/data/clean/instrument/instrument_dofe_dist.csv",
  stringsAsFactors = FALSE
) %>%
  select(dname, year, fxshock_dofe)

fs_df <- dofe_panel %>%
  inner_join(instrument,      by = c("dname", "year")) %>%
  inner_join(instrument_dofe, by = c("dname", "year")) %>%
  mutate(
    log_permits      = log(permits + 1),
    log_mig_int      = log(pmax(geog_intensity_2001, 1e-12)),
    fx_x_logmi       = fxshock      * log_mig_int,
    fxdofe_x_logmi   = fxshock_dofe * log_mig_int
  )

cat(sprintf(
  "First-stage panel: %d obs, %d districts, %d years (%d-%d)\n",
  nrow(fs_df),
  length(unique(fs_df$dname)),
  length(unique(fs_df$year)),
  min(fs_df$year), max(fs_df$year)
))

# Sanity: anyone fail to match?
unmatched <- setdiff(unique(dofe_panel$dname), unique(instrument$dname))
if (length(unmatched) > 0) {
  cat("WARNING - DOFE districts with no instrument match:\n")
  print(unmatched)
}

# ------------------------------------------------------------------------------
# 3. First-stage regressions: permits on SSIV shifters
# ------------------------------------------------------------------------------

# Five specs side-by-side:
#   m1 : bare fxshock_2001
#   m2 : fxshock_2001 x log(mig_int) interaction only
#   m3 : fxshock_2001 x log(mig_int) + i(year,log_mig_int) + i(year,fxshock)   [slide full spec]
#   m4 : bare fxshock_dofe (2009-2010 DOFE share baseline)
#   m5 : fxshock_dofe x log(mig_int) interaction only
m1 <- feols(log_permits ~ fxshock                                | dname + year,
            data = fs_df, cluster = ~dname)
m2 <- feols(log_permits ~ fx_x_logmi                             | dname + year,
            data = fs_df, cluster = ~dname)
m3 <- feols(log_permits ~ fx_x_logmi + i(year, log_mig_int) + i(year, fxshock)
            | dname + year,
            data = fs_df, cluster = ~dname)
m4 <- feols(log_permits ~ fxshock_dofe                           | dname + year,
            data = fs_df, cluster = ~dname)
m5 <- feols(log_permits ~ fxdofe_x_logmi                         | dname + year,
            data = fs_df, cluster = ~dname)

cat("\n=== First-stage: log(DOFE permits + 1) on fxshock (5 specs) ===\n")
print(etable(m1, m2, m3, m4, m5,
             cluster = ~dname,
             headers = c("fx_2001",
                         "fx_2001 x logmi",
                         "fx_2001 x logmi FULL",
                         "fx_dofe",
                         "fx_dofe x logmi"),
             keep = "%fx",   # show only fxshock-related coefs
             digits  = 4,
             fitstat = c("n", "r2", "wr2")))

# ------------------------------------------------------------------------------
# 4. Save coefficient table to output/tab/
# ------------------------------------------------------------------------------

dir.create("district-analysis/output/tab", recursive = TRUE,
           showWarnings = FALSE)

mods    <- list(m1, m2, m3, m4, m5)
regnms  <- c("fxshock", "fx_x_logmi", "fx_x_logmi",
             "fxshock_dofe", "fxdofe_x_logmi")
fs_summary <- tibble(
  spec      = c("fx_2001",
                "fx_2001 x logmi",
                "fx_2001 x logmi FULL",
                "fx_dofe",
                "fx_dofe x logmi"),
  coef      = mapply(function(m, nm) coef(m)[nm], mods, regnms),
  se        = mapply(function(m, nm)
                       sqrt(diag(vcov(m, cluster = ~dname)))[nm], mods, regnms),
  n_obs     = sapply(mods, nobs),
  r2_within = sapply(mods, function(m) fitstat(m, "wr2", simplify = TRUE))
) %>%
  mutate(t_stat = coef / se,
         p_val  = 2 * pnorm(-abs(t_stat)))

write.csv(fs_summary,
          "district-analysis/output/tab/first_stage_dofe.csv",
          row.names = FALSE)

cat("\nSaved: district-analysis/output/tab/first_stage_dofe.csv\n")
