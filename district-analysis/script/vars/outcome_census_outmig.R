################################################################################
#
# OUT-MIGRATION OUTCOMES (CENSUS 2001 / 2011 / 2021) — BY ORIGIN DISTRICT
# ------------------------------------------------------------------------------
# Builds district-level out-migration outcomes by re-aggregating the same
# "where did you live 5 years ago" question used for mig_in_* — but grouping
# by the ORIGIN district (not the destination / current district).
#
# Output CSV columns:
#   dname, year,
#   n_out_internal_5y,                # gross out-mig count, internal only
#   n_stayers_origin_5y,              # people who stayed in d (resident in d, 5y-ago in d)
#   denom_origin_5y,                  # = n_out_internal + n_stayers (people in d 5y ago)
#   mig_out_internal_share,           # n_out / denom
#   mig_out_to_urban_share,           # of out-migrants, share whose CURRENT (dest) is urban
#   mig_out_to_rural_share,           # of out-migrants, share whose CURRENT (dest) is rural
#   mig_out_male_share,
#   mig_out_female_share,
#   mig_out_reason_economic_share,    # destination-pull reason (q18 / q25 / q6_rstay)
#   mig_out_reason_noneconomic_share,
#   mig_out_age_15_30_share,
#   net_internal_mig_share            # = mig_in_domestic - mig_out_internal_share
#
# Also writes a bilateral 77x77 origin->destination flow CSV per round, for
# later gravity / spatial-equilibrium work.
#
# Usage (from repo root):
#   source("district-analysis/script/vars/outcome_census_outmig.R")
#
# Outputs:
#   district-analysis/data/clean/census/outmig_district_long.csv
#   district-analysis/data/clean/census/outmig_bilateral_2011.csv
#   district-analysis/data/clean/census/outmig_bilateral_2021.csv
#
################################################################################

rm(list = ls()); cat("\14")

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(readxl)
  library(janitor)
})

OUT_DIR <- "district-analysis/data/clean/census"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Build dcode -> dname lookup (same source as outcome_census.R) ----
vdc_to_lg_map <- read_xlsx("data/raw/old vdc to local level.xlsx") %>%
  rename(dcode = dist, lgcode = vmun_code)

# District-level lookup: numeric dcode -> dname (current 77-district names)
dcode_lookup <- vdc_to_lg_map %>%
  distinct(dcode, dist_name) %>%
  rename(dname = dist_name) %>%
  filter(!is.na(dcode), !is.na(dname))

# Sanity: should be 77 (or up to 75 for older boundaries)
cat(sprintf("dcode_lookup: %d districts\n", nrow(dcode_lookup)))

# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  COLUMN NAME OVERRIDES — VERIFY LOCALLY                                  ║
# ║  After read_dta(..., n_max = 5) |> names(), update these constants if    ║
# ║  the origin-district column name in your raw file differs.               ║
# ╚══════════════════════════════════════════════════════════════════════════╝
ORIGIN_COL_2001 <- "q7a_dist"   # district lived 5y ago, when q7_li5ya == 2
ORIGIN_COL_2011 <- "q19b"       # district lived 5y ago, when q19a == 2
ORIGIN_COL_2021 <- "q22"        # district lived 5y ago, when q21 %in% c(2,3)

# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Helper — aggregate one micro file to origin-district level out-mig      ║
# ╚══════════════════════════════════════════════════════════════════════════╝
build_outmig <- function(ind, year,
                         col_origin,          # district code of place 5y ago
                         col_where,           # 1=here, 2=other dist, 3=abroad
                         where_internal_codes,# codes meaning "another Nepali district"
                         col_dest_urban,      # rural/urban code of CURRENT residence
                         col_sex,             # 1=male, 2=female
                         col_age,
                         col_reason,
                         reason_econ_codes,
                         reason_noecon_codes,
                         curr_dcode_col       # column with CURRENT district code
                         ) {

  # ---- 1. Out-migrants only (left another Nepali district) ----
  out_mig <- ind %>%
    filter(.data[[col_where]] %in% where_internal_codes,
           !is.na(.data[[col_origin]])) %>%
    mutate(orig_dcode = as.integer(.data[[col_origin]])) %>%
    left_join(dcode_lookup, by = c("orig_dcode" = "dcode")) %>%
    rename(orig_dname = dname) %>%
    filter(!is.na(orig_dname))

  cat(sprintf("  %d: %d out-migrant records mapped to %d origin dists\n",
              year, nrow(out_mig), n_distinct(out_mig$orig_dname)))

  # ---- 2. District-level out-migration outcomes ----
  # Two flavours:
  #   *_of_outmig_share  : share OF out-migrants (denom = n_out_internal_5y)
  #   *_pop_share        : count for that subgroup / origin denom (computed in step 4)
  out_agg <- out_mig %>%
    group_by(orig_dname) %>%
    summarise(
      n_out_internal_5y                = n(),
      # Subgroup counts — divided by population denom in step 4
      n_out_economic                   = sum(.data[[col_reason]] %in% reason_econ_codes,   na.rm = TRUE),
      n_out_noneconomic                = sum(.data[[col_reason]] %in% reason_noecon_codes, na.rm = TRUE),
      n_out_male                       = sum(.data[[col_sex]]    == 1,                     na.rm = TRUE),
      n_out_female                     = sum(.data[[col_sex]]    == 2,                     na.rm = TRUE),
      n_out_age_15_30                  = sum(.data[[col_age]]    >= 15 & .data[[col_age]] < 30, na.rm = TRUE),
      # Composition-of-out-migrants shares (denom = n_out_internal_5y)
      mig_out_to_urban_share           = mean(.data[[col_dest_urban]] == 2, na.rm = TRUE),
      mig_out_to_rural_share           = mean(.data[[col_dest_urban]] == 1, na.rm = TRUE),
      mig_out_of_outmig_male_share     = mean(.data[[col_sex]]    == 1, na.rm = TRUE),
      mig_out_of_outmig_female_share   = mean(.data[[col_sex]]    == 2, na.rm = TRUE),
      mig_out_of_outmig_econ_share     = mean(.data[[col_reason]] %in% reason_econ_codes,   na.rm = TRUE),
      mig_out_of_outmig_noecon_share   = mean(.data[[col_reason]] %in% reason_noecon_codes, na.rm = TRUE),
      mig_out_of_outmig_age_15_30      = mean(.data[[col_age]]    >= 15 & .data[[col_age]] < 30, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(dname = orig_dname)

  # ---- 3. Stayers: lived in d 5y ago AND currently in d ----
  stayers <- ind %>%
    filter(.data[[col_where]] == 1, !is.na(.data[[curr_dcode_col]])) %>%
    mutate(curr_dcode = as.integer(.data[[curr_dcode_col]])) %>%
    left_join(dcode_lookup, by = c("curr_dcode" = "dcode")) %>%
    filter(!is.na(dname)) %>%
    count(dname, name = "n_stayers_origin_5y")

  # ---- 4. Merge → compute denom and shares (denom = origin pop 5y ago) ----
  outmig_dist <- out_agg %>%
    full_join(stayers, by = "dname") %>%
    mutate(
      across(starts_with("n_out_"),       ~ coalesce(.x, 0L)),
      n_stayers_origin_5y   = coalesce(n_stayers_origin_5y, 0L),
      denom_origin_5y       = n_out_internal_5y + n_stayers_origin_5y,
      # Population-denominator shares — directly comparable to mig_in_*
      mig_out_internal_share           = if_else(denom_origin_5y > 0,
                                                 n_out_internal_5y / denom_origin_5y, NA_real_),
      mig_out_economic_share           = if_else(denom_origin_5y > 0,
                                                 n_out_economic     / denom_origin_5y, NA_real_),
      mig_out_noneconomic_share        = if_else(denom_origin_5y > 0,
                                                 n_out_noneconomic  / denom_origin_5y, NA_real_),
      mig_out_male_share               = if_else(denom_origin_5y > 0,
                                                 n_out_male         / denom_origin_5y, NA_real_),
      mig_out_female_share             = if_else(denom_origin_5y > 0,
                                                 n_out_female       / denom_origin_5y, NA_real_),
      mig_out_age_15_30_share          = if_else(denom_origin_5y > 0,
                                                 n_out_age_15_30    / denom_origin_5y, NA_real_),
      year = year
    )

  # ---- 5. Bilateral matrix (origin -> destination) for later gravity work ----
  bilat <- out_mig %>%
    mutate(curr_dcode = as.integer(.data[[curr_dcode_col]])) %>%
    left_join(dcode_lookup, by = c("curr_dcode" = "dcode")) %>%
    filter(!is.na(dname)) %>%
    rename(dest_dname = dname) %>%
    count(orig_dname, dest_dname, name = "n_flow") %>%
    mutate(year = year)

  list(district = outmig_dist, bilateral = bilat)
}

# ============================================================================
#  2001 ROUND
# ============================================================================
cat("=== 2001 ===\n")

ind2_01 <- read_dta(
  "data/raw/Full Census Data/Census 2001/fullpi02_full.dta",
  col_select = any_of(c("dist","vdcmun","ward",
                        "q2_sex","q3_age",
                        "q5_durtn","q6_rstay",
                        "q7_li5ya","q7b_vdcm",
                        ORIGIN_COL_2001))
)
stopifnot(ORIGIN_COL_2001 %in% names(ind2_01))

# In 2001 q6_rstay is the REASON to current place; reason codes:
# 1-3 = work/business/employment (economic); 4-6 = study/marriage/family (non-economic)
out_01 <- build_outmig(
  ind                  = ind2_01,
  year                 = 2001,
  col_origin           = ORIGIN_COL_2001,
  col_where            = "q7_li5ya",
  where_internal_codes = c(2),                     # 2 = "another Nepali district"
  col_dest_urban       = "q7b_vdcm",               # rural/urban of place 5y AGO — caveat below
  col_sex              = "q2_sex",
  col_age              = "q3_age",
  col_reason           = "q6_rstay",
  reason_econ_codes    = 1:3,
  reason_noecon_codes  = 4:6,
  curr_dcode_col       = "dist"
)

# NOTE: 2001 lacks a rural/urban code of the CURRENT district at individual level
#       (q7b_vdcm describes the place 5y ago, not the current dest). So
#       mig_out_to_urban_share in 2001 is the share whose ORIGIN was urban,
#       not whose destination is urban — drop for 2001 if you want strict
#       interpretation. Setting to NA here for clarity:
out_01$district <- out_01$district %>%
  mutate(mig_out_to_urban_share = NA_real_,
         mig_out_to_rural_share = NA_real_)

rm(ind2_01); gc(verbose = FALSE)

# ============================================================================
#  2011 ROUND
# ============================================================================
cat("=== 2011 ===\n")

ind11_2 <- read_dta(
  "data/raw/Full Census Data/Census 2011/individual02.dta",
  col_select = any_of(c("dist","vdcmun","ward",
                        "q04","q05",                  # sex, age (2011 codes — verify)
                        "q18",                         # reason (2011 codes — verify)
                        "q19a","q19c",
                        ORIGIN_COL_2011))
)
stopifnot(ORIGIN_COL_2011 %in% names(ind11_2))

# 2011 q19a: 1=same district, 2=other Nepali district, 3=abroad
# Reason q18 codes vary by round — most commonly:
#   1 = agriculture/work, 2 = trade/business, 3 = study, 4 = marriage, ...
#   You may need to adjust the econ vs non-econ split below to match your codebook.
out_11 <- build_outmig(
  ind                  = ind11_2,
  year                 = 2011,
  col_origin           = ORIGIN_COL_2011,
  col_where            = "q19a",
  where_internal_codes = c(2),
  col_dest_urban       = "q19c",                # NB this is rural/urban of ORIGIN, not dest
  col_sex              = "q04",
  col_age              = "q05",
  col_reason           = "q18",
  reason_econ_codes    = c(1,2,3),
  reason_noecon_codes  = c(4,5,6,7,8),
  curr_dcode_col       = "dist"
)

# Same caveat as 2001 — q19c is rural/urban of ORIGIN, not destination. Drop it.
out_11$district <- out_11$district %>%
  mutate(mig_out_to_urban_share = NA_real_,
         mig_out_to_rural_share = NA_real_)

rm(ind11_2); gc(verbose = FALSE)

# ============================================================================
#  2021 ROUND
# ============================================================================
cat("=== 2021 ===\n")

ind_21 <- read_dta(
  "data/raw/Full Census Data/Census 2021/Data/PCMS2021_Individual.dta",
  col_select = any_of(c("dist",
                        "q04","q05",                  # sex, age
                        "q21","q23","q25",
                        ORIGIN_COL_2021))
)
stopifnot(ORIGIN_COL_2021 %in% names(ind_21))

# 2021 q21: 1=same dist, 2=other dist (rural origin), 3=other dist (urban origin), 4=abroad
# q23 = rural/urban of place 5y ago.  q25 = reason for migrating.
#   Reason codes typical: 1=work, 2=trade, 3=study, 4=marriage, 5=family, 6=other,
#                         7=transfer, 8=return, 9=conflict — adjust as needed.
out_21 <- build_outmig(
  ind                  = ind_21,
  year                 = 2021,
  col_origin           = ORIGIN_COL_2021,
  col_where            = "q21",
  where_internal_codes = c(2, 3),               # both other-district codes
  col_dest_urban       = "q23",                  # rural/urban of ORIGIN — same caveat
  col_sex              = "q04",
  col_age              = "q05",
  col_reason           = "q25",
  reason_econ_codes    = c(1, 2, 7, 8),
  reason_noecon_codes  = c(3, 4, 5, 6, 9),
  curr_dcode_col       = "dist"
)

out_21$district <- out_21$district %>%
  mutate(mig_out_to_urban_share = NA_real_,
         mig_out_to_rural_share = NA_real_)

rm(ind_21); gc(verbose = FALSE)

# ============================================================================
#  STACK + JOIN net_internal_mig_share
# ============================================================================
cat("=== Stacking ===\n")

outmig_long <- bind_rows(out_01$district, out_11$district, out_21$district) %>%
  select(dname, year,
         # raw counts
         n_out_internal_5y, n_stayers_origin_5y, denom_origin_5y,
         n_out_economic, n_out_noneconomic,
         n_out_male, n_out_female, n_out_age_15_30,
         # population-denominator out-migration rates (comparable to mig_in_*)
         mig_out_internal_share,
         mig_out_economic_share, mig_out_noneconomic_share,
         mig_out_male_share, mig_out_female_share, mig_out_age_15_30_share,
         # composition-of-out-migrants shares (sum to 1 within group)
         mig_out_to_urban_share, mig_out_to_rural_share,
         mig_out_of_outmig_male_share, mig_out_of_outmig_female_share,
         mig_out_of_outmig_econ_share, mig_out_of_outmig_noecon_share,
         mig_out_of_outmig_age_15_30) %>%
  arrange(dname, year)

# Optional: pull mig_in_domestic from the existing district panel to form net
existing_panel_path <- "district-analysis/data/clean/census/census_outcomes_district.csv"
if (file.exists(existing_panel_path)) {
  pin <- read_csv(existing_panel_path, show_col_types = FALSE) %>%
    select(dname, year, mig_in_domestic)
  outmig_long <- outmig_long %>%
    left_join(pin, by = c("dname", "year")) %>%
    mutate(net_internal_mig_share = mig_in_domestic - mig_out_internal_share) %>%
    select(-mig_in_domestic)
}

write_csv(outmig_long, file.path(OUT_DIR, "outmig_district_long.csv"))
cat(sprintf("Wrote %s: %d rows\n",
            file.path(OUT_DIR, "outmig_district_long.csv"),
            nrow(outmig_long)))

# Bilateral matrices — keep separate per round for sparsity
write_csv(out_11$bilateral, file.path(OUT_DIR, "outmig_bilateral_2011.csv"))
write_csv(out_21$bilateral, file.path(OUT_DIR, "outmig_bilateral_2021.csv"))
write_csv(out_01$bilateral, file.path(OUT_DIR, "outmig_bilateral_2001.csv"))

cat("=== Sanity ===\n")
outmig_long %>%
  group_by(year) %>%
  summarise(
    n_dist            = n_distinct(dname),
    mean_out_share    = mean(mig_out_internal_share, na.rm = TRUE),
    sd_out_share      = sd(mig_out_internal_share,   na.rm = TRUE),
    total_out_count   = sum(n_out_internal_5y),
    .groups = "drop"
  ) %>%
  print()

cat("\nDone.  Next: add `mig_out_internal_share` (and friends) to OUTCOMES list\n",
    "in district-analysis/script/_robustness_all_panels.R, then re-run that\n",
    "script to extend robustness_all_panels.csv.\n", sep = "")
