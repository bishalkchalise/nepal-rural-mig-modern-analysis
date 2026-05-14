################################################################################
#
# ROBUSTNESS: intensity scaling for the SSIV interaction
# ------------------------------------------------------------------------------
# For each of the four datasets (DOFE permits, NRVS HH-level, migrant-level,
# district-aggregate RVS), run the first-stage with the headline interaction
# under four intensity transforms:
#
#   1. log(mig_int)            (the slide's original form, always negative)
#   2. log(1000 * mig_int)     (migration rate per 1,000 -- usually positive)
#   3. mig_int (linear)        (no log -- effect linear in intensity)
#   4. mig_int_dofe            (DOFE 2009-2010 avg per 2001 census population)
#
# Each transform is run twice: WITHOUT and WITH the slide's C_mig and C_fx
# year-trend controls.
#
# Run from repo root:
#     source("district-analysis/script/estimate/robustness_intensity_scaling.R")
#
# Output:
#     district-analysis/output/tab/robustness_intensity_scaling.csv
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

# ------------------------------------------------------------------------------
# 1. Load instruments and build all intensity transforms
# ------------------------------------------------------------------------------

instr <- read.csv(
  "district-analysis/data/clean/instrument/instrument_forex_dist.csv",
  stringsAsFactors = FALSE
)
instr_dofe <- read.csv(
  "district-analysis/data/clean/instrument/instrument_dofe_dist.csv",
  stringsAsFactors = FALSE
)

# DOFE district-year permit panel (also used to build a DOFE-based mig_int)
dofe_raw <- read.csv(
  "district-analysis/data/clean/foreign_migration_district_country_annual.csv",
  stringsAsFactors = FALSE
)
dofe_to_census <- c(
  "CHITWAN"    = "Chitawan", "DHANUSHA" = "Dhanusa",
  "KAPILVASTU" = "Kapilbastu","MAKAWANPUR" = "Makwanpur",
  "TANAHUN"    = "Tanahu",   "TEHRATHUM" = "Terhathum",
  "KABHREPALANCHOK" = "Kavrepalanchok"
)

dofe_panel <- dofe_raw %>%
  group_by(district_rename, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    dname = ifelse(!is.na(dofe_to_census[district_rename]),
                   dofe_to_census[district_rename],
                   str_to_title(tolower(district_rename)))
  ) %>%
  select(dname, year, permits)

# DOFE 2009-2010 mean migrants per district / 2001 population
mi_dofe <- dofe_panel %>%
  filter(year %in% 2009:2010) %>%
  group_by(dname) %>%
  summarise(dofe_mig_0910 = mean(permits), .groups = "drop") %>%
  left_join(distinct(instr[, c("dname", "geog_pop_2001")]), by = "dname") %>%
  mutate(mig_int_dofe = dofe_mig_0910 / geog_pop_2001) %>%
  select(dname, mig_int_dofe)

# Build a base panel with all the needed columns at (dname, year)
base <- instr %>%
  select(dname, year, fxshock, geog_intensity_2001, geog_pop_2001) %>%
  inner_join(instr_dofe %>% select(dname, year, fxshock_dofe),
             by = c("dname", "year")) %>%
  left_join(mi_dofe, by = "dname") %>%
  mutate(
    log_mig_int        = log(pmax(geog_intensity_2001, 1e-12)),
    log_mig_int_x1k    = log(pmax(1000   * geog_intensity_2001, 1e-12)),
    log_mig_int_x100k  = log(pmax(100000 * geog_intensity_2001, 1e-12)),
    mig_int_lin        = geog_intensity_2001,
    mig_int_dofe       = pmax(mig_int_dofe, 1e-12)
  )

# ------------------------------------------------------------------------------
# 2. Define the runner for one (dataset, intensity, fxshock, with_controls) cell
# ------------------------------------------------------------------------------

# A "dataset" is a (panel, outcome) pair. We'll iterate over four datasets.

run_cell <- function(panel_df, outcome,
                     intensity_col, fxshock_col,
                     with_controls,
                     cluster_var = "dname") {

  panel_df$main <- panel_df[[fxshock_col]] * panel_df[[intensity_col]]
  rhs <- "main"

  if (with_controls) {
    rhs <- paste(rhs,
                 sprintf("i(year, %s)", intensity_col),
                 sprintf("i(year, %s)", fxshock_col),
                 sep = " + ")
  }

  f <- as.formula(paste0(outcome, " ~ ", rhs, " | dname + year"))

  m <- tryCatch(
    feols(f, data = panel_df,
          cluster = as.formula(paste0("~", cluster_var))),
    error = function(e) NULL
  )

  if (is.null(m)) {
    return(tibble(coef = NA_real_, se = NA_real_,
                  t_stat = NA_real_, p_val = NA_real_,
                  n_obs = NA_integer_, r2_w = NA_real_))
  }

  ct <- as.data.frame(summary(m)$coeftable)
  if (!"main" %in% rownames(ct)) {
    return(tibble(coef = NA_real_, se = NA_real_,
                  t_stat = NA_real_, p_val = NA_real_,
                  n_obs = nobs(m), r2_w = NA_real_))
  }

  tibble(
    coef   = ct["main", "Estimate"],
    se     = ct["main", "Std. Error"],
    t_stat = ct["main", "t value"],
    p_val  = ct["main", "Pr(>|t|)"],
    n_obs  = nobs(m),
    r2_w   = fitstat(m, "wr2", simplify = TRUE)
  )
}

# ------------------------------------------------------------------------------
# 3. Assemble the four datasets
# ------------------------------------------------------------------------------

# (a) DOFE permits panel
ds_dofe <- dofe_panel %>%
  inner_join(base, by = c("dname", "year")) %>%
  mutate(y = log(permits + 1))

# (b) RVS district-level
rvs_dist <- read.csv("district-analysis/data/clean/rvs/migration_district_year.csv",
                     stringsAsFactors = FALSE) %>%
  mutate(
    upr   = toupper(str_squish(dname_raw)),
    dname = ifelse(!is.na(dofe_to_census[upr]),
                   dofe_to_census[upr],
                   str_to_title(tolower(upr)))
  ) %>%
  select(-upr)

ds_rvs_d <- rvs_dist %>%
  inner_join(base, by = c("dname", "year")) %>%
  mutate(y = log(remit_amount_intl_12m_rs + 1))

# (c) RVS HH-level, migrant-only
rvs_hh <- read.csv("district-analysis/data/clean/rvs/migration_hh_year.csv",
                   stringsAsFactors = FALSE)
dist_key <- if ("district_name" %in% names(rvs_hh)) "district_name" else
            if ("district77"    %in% names(rvs_hh)) "district77"    else "district"

rvs_hh <- rvs_hh %>%
  rename(dname_raw = all_of(dist_key)) %>%
  mutate(
    upr   = toupper(str_squish(dname_raw)),
    dname = ifelse(!is.na(dofe_to_census[upr]),
                   dofe_to_census[upr],
                   str_to_title(tolower(upr)))
  ) %>%
  select(-upr)

ds_rvs_hh <- rvs_hh %>%
  filter(has_migrant_intl == 1) %>%
  inner_join(base, by = c("dname", "year")) %>%
  mutate(y = log(remit_amount_intl_12m_rs + 1))

# (d) RVS migrant-level
rvs_mig <- read.csv("district-analysis/data/clean/rvs/migration_migrant_year.csv",
                    stringsAsFactors = FALSE)
dist_key <- if ("district_name" %in% names(rvs_mig)) "district_name" else
            if ("district77"    %in% names(rvs_mig)) "district77"    else "district"

rvs_mig <- rvs_mig %>%
  rename(dname_raw = all_of(dist_key)) %>%
  mutate(
    upr   = toupper(str_squish(dname_raw)),
    dname = ifelse(!is.na(dofe_to_census[upr]),
                   dofe_to_census[upr],
                   str_to_title(tolower(upr)))
  ) %>%
  select(-upr)

ds_rvs_mig <- rvs_mig %>%
  filter(is_international == 1) %>%
  inner_join(base, by = c("dname", "year")) %>%
  mutate(y = log(coalesce(remit_amount_rs, 0) + 1))

datasets <- list(
  list(name = "DOFE permits panel",         data = ds_dofe,    outcome = "y"),
  list(name = "RVS district",               data = ds_rvs_d,   outcome = "y"),
  list(name = "RVS HH-level (migrant-only)", data = ds_rvs_hh,  outcome = "y"),
  list(name = "RVS migrant-level",          data = ds_rvs_mig, outcome = "y")
)

# ------------------------------------------------------------------------------
# 4. Loop over intensity transforms x fxshock variants x controls
# ------------------------------------------------------------------------------

intensities <- list(
  "log(mig_int)"      = "log_mig_int",
  "log(1000*mi)"      = "log_mig_int_x1k",
  "mi_linear"         = "mig_int_lin",
  "mi_dofe"           = "mig_int_dofe"
)
fxshocks <- list(
  "fx_2001"  = "fxshock",
  "fx_dofe"  = "fxshock_dofe"
)
controls <- c(FALSE, TRUE)

cat("Running robustness cells...\n")

results <- map_dfr(datasets, function(ds) {
  map_dfr(names(fxshocks), function(fxlbl) {
    fxcol <- fxshocks[[fxlbl]]
    map_dfr(names(intensities), function(milbl) {
      micol <- intensities[[milbl]]
      map_dfr(controls, function(ctrl) {
        out <- run_cell(ds$data, ds$outcome,
                        intensity_col = micol,
                        fxshock_col   = fxcol,
                        with_controls = ctrl)
        out %>% mutate(
          dataset    = ds$name,
          fxshock    = fxlbl,
          intensity  = milbl,
          controls   = if (ctrl) "FULL" else "no",
          spec       = paste(fxlbl, "x", milbl)
        )
      })
    })
  })
})

results <- results %>%
  mutate(
    sig = case_when(
      is.na(p_val)  ~ "",
      p_val < 0.01  ~ "***",
      p_val < 0.05  ~ "**",
      p_val < 0.1   ~ "*",
      TRUE          ~ ""
    )
  ) %>%
  select(dataset, fxshock, intensity, controls, spec,
         coef, se, t_stat, p_val, sig, n_obs, r2_w)

dir.create("district-analysis/output/tab",
           recursive = TRUE, showWarnings = FALSE)

write.csv(results,
          "district-analysis/output/tab/robustness_intensity_scaling.csv",
          row.names = FALSE)

cat("\nSaved: district-analysis/output/tab/robustness_intensity_scaling.csv\n")
cat(sprintf("Total cells: %d (4 datasets x 2 fxshocks x 4 intensities x 2 control sets)\n",
            nrow(results)))

# Print a compact preview to the console
cat("\n=== Preview: DOFE panel, fx_2001 ===\n")
print(results %>%
        filter(dataset == "DOFE permits panel", fxshock == "fx_2001") %>%
        select(intensity, controls, coef, se, t_stat, p_val, sig, n_obs) %>%
        mutate(across(c(coef, se, t_stat), ~ round(., 4)),
               p_val = round(p_val, 4)),
      n = Inf)
