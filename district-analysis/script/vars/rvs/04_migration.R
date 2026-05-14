################################################################################
#
# NRVS SECTION 11: MIGRATION + REMITTANCE - DISTRICT AGGREGATION
# ------------------------------------------------------------------------------
# Ported from the municipality-era script/vars/rvs/04_migration.R (archived on
# the `main` branch).  Same migrant-level construction logic, with one new
# aggregation step that rolls migrants up to a district-year panel for
# first-stage validation against the forex SSIV.
#
# Inputs :
#   - data/raw/RVS Data/clean/migration/section_11.csv      (migrant-level)
#   - data/raw/RVS Data/clean/id_match_long.csv             (HH -> district)
#
# Outputs (district-analysis/data/clean/rvs/) :
#   - migration_migrant_year.csv          (one row per migrant x year)
#   - migration_hh_year.csv               (one row per HH x year)
#   - migration_district_year.csv         (NEW: one row per district x year,
#                                          used by first_stage_rvs.R)
#
# Source : run from repo root,
#            source("district-analysis/script/vars/rvs/04_migration.R")
#
################################################################################

rm(list = ls()); cat("\14")

suppressPackageStartupMessages({
  library(tidyverse)
  library(fs)
})

base_in  <- "data/raw/RVS Data/clean"
base_out <- "district-analysis/data/clean/rvs"
dir_create(base_out, recurse = TRUE)

read_csv_q <- function(p) read_csv(p, show_col_types = FALSE, progress = FALSE)

sec11 <- read_csv_q(file.path(base_in, "migration", "section_11.csv"))

# ------------------------------------------------------------------------------
# 1. HELPERS
# ------------------------------------------------------------------------------

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

match_any <- function(x, patterns) {
  s <- str_squish(str_to_lower(as.character(x)))
  s <- coalesce(s, "")
  str_detect(s, paste(patterns, collapse = "|"))
}

yesno <- function(x) {
  s <- str_squish(str_to_lower(as.character(x)))
  case_when(
    is.na(s) | s == "" ~ NA_integer_,
    s == "yes" ~ 1L,
    s == "no"  ~ 0L,
    s == "1"   ~ 1L,
    s == "2"   ~ 0L,
    TRUE       ~ NA_integer_
  )
}

# ------------------------------------------------------------------------------
# 2. MIGRANT-LEVEL CLEANING
# ------------------------------------------------------------------------------

gulf_patterns <- c(
  "qatar", "saudi", "emirates", "\\buae\\b",
  "kuwait", "bahrain", "bahrai", "oman", "dubai"
)

migrant_year <- sec11 %>%
  mutate(
    is_internal      = as.integer(match_any(s11q02, "locally")),
    is_international = as.integer(match_any(s11q02, "overseas")),

    dest_india    = as.integer(match_any(s11q02b, "^india$")),
    dest_gulf     = as.integer(
      match_any(s11q02b, gulf_patterns) |
        match_any(s11q02d, gulf_patterns)
    ),
    dest_malaysia = as.integer(match_any(s11q02b, "malay")),
    dest_other_country = as.integer(
      is_international == 1 &
        dest_india == 0 & dest_gulf == 0 & dest_malaysia == 0
    ),

    migrant_male   = as.integer(match_any(s11q01c, "^male$")),
    migrant_female = as.integer(match_any(s11q01c, "^female$")),
    migrant_age    = as_num(s11q01d),
    months_away    = pmin(as_num(s11q03), 600, na.rm = FALSE),

    reason_work = as.integer(yesno(s11q04a_6) == 1 | yesno(s11q04a_7) == 1),
    reason_educ = as.integer(yesno(s11q04a_4) == 1 | yesno(s11q04a_5) == 1),
    reason_family = as.integer(
      yesno(s11q04a_1) == 1 | yesno(s11q04a_2) == 1 | yesno(s11q04a_3) == 1
    ),

    earning_primary_rs   = coalesce(as_num(s11q05c), 0),
    earning_secondary_rs = coalesce(as_num(s11q06c), 0),
    has_secondary_job    = as.integer(yesno(s11q06a) == 1),

    remit_sent_flag = yesno(s11q07a),
    remit_amount_rs = coalesce(as_num(s11q07c), 0),
    remit_freq_per_year = if_else(
      as_num(s11q07b) < 998 & !is.na(as_num(s11q07b)),
      pmin(as_num(s11q07b), 12),
      NA_real_
    ),
    remit_via_bank_ime = yesno(s11q07d_1),
    remit_via_friends  = yesno(s11q07d_2),
    remit_via_hundi    = yesno(s11q07d_3),
    remit_via_other    = yesno(s11q07d_4),

    remit_use_consumption = as.integer(
      yesno(s11q07e_1) == 1 | yesno(s11q07e_12) == 1
    ),
    remit_use_education = as.integer(yesno(s11q07e_3) == 1),
    remit_use_business = as.integer(
      yesno(s11q07e_5) == 1 | yesno(s11q07e_6) == 1 |
        yesno(s11q07e_7) == 1 | yesno(s11q07e_8) == 1 |
        yesno(s11q07e_9) == 1
    ),

    mig_cost_any       = yesno(s11q08a),
    mig_cost_rs        = coalesce(as_num(s11q08b), 0),
    mig_cost_loan_flag = yesno(s11q08c_1)
  )

# ------------------------------------------------------------------------------
# 3. MIGRANT-YEAR OUTPUT
# ------------------------------------------------------------------------------

migrant_year_out <- migrant_year %>%
  select(
    hhid, year,
    any_of(c("migrationid", "member_id", "wt_hh",
             "vmun_code", "lgname", "district77", "district_name")),
    is_internal, is_international,
    dest_india, dest_gulf, dest_malaysia, dest_other_country,
    s11q02b_raw = s11q02b,
    s11q02d_raw = s11q02d,
    s11q02a_raw = s11q02a,
    migrant_male, migrant_female, migrant_age, months_away,
    reason_work, reason_educ, reason_family,
    earning_primary_rs, earning_secondary_rs, has_secondary_job,
    remit_sent_flag, remit_amount_rs, remit_freq_per_year,
    remit_via_bank_ime, remit_via_friends, remit_via_hundi, remit_via_other,
    remit_use_consumption, remit_use_education, remit_use_business,
    mig_cost_any, mig_cost_rs, mig_cost_loan_flag
  )

write_csv(migrant_year_out,
          file.path(base_out, "migration_migrant_year.csv"), na = "")

# ------------------------------------------------------------------------------
# 4. HH-YEAR AGGREGATION
# ------------------------------------------------------------------------------

hh_agg <- migrant_year %>%
  group_by(hhid, year) %>%
  summarise(
    has_migrant          = 1L,
    has_migrant_internal = as.integer(any(is_internal == 1, na.rm = TRUE)),
    has_migrant_intl     = as.integer(any(is_international == 1, na.rm = TRUE)),

    n_migrants           = n(),
    n_intl_migrants      = sum(is_international == 1, na.rm = TRUE),

    remit_received       = as.integer(any(remit_sent_flag == 1, na.rm = TRUE)),
    remit_amount_12m_rs  = sum(remit_amount_rs, na.rm = TRUE),
    remit_amount_intl_12m_rs = sum(remit_amount_rs[is_international == 1],
                                   na.rm = TRUE),

    remit_via_formal_any = as.integer(any(remit_via_bank_ime == 1, na.rm = TRUE)),
    remit_via_hundi_any  = as.integer(any(remit_via_hundi == 1, na.rm = TRUE)),

    .groups = "drop"
  )

# Bring district info onto the HH-year frame via id_match_long
idmap_path <- file.path(base_in, "id_match_long.csv")
if (!file_exists(idmap_path)) {
  stop("id_match_long.csv not found at: ", idmap_path,
       "\nCannot build district aggregation without it.")
}

id_match <- read_csv_q(idmap_path) %>%
  distinct(hhid, year, .keep_all = TRUE) %>%
  select(hhid, year,
         any_of(c("wt_hh", "psu", "district", "vdc",
                  "vmun_code", "lgname", "district77", "district_name")))

migration_hh_year <- id_match %>%
  left_join(hh_agg, by = c("hhid", "year")) %>%
  mutate(across(
    c(has_migrant, has_migrant_internal, has_migrant_intl,
      n_migrants, n_intl_migrants,
      remit_received, remit_amount_12m_rs, remit_amount_intl_12m_rs,
      remit_via_formal_any, remit_via_hundi_any),
    ~ coalesce(.x, 0)
  ))

write_csv(migration_hh_year,
          file.path(base_out, "migration_hh_year.csv"), na = "")

# ------------------------------------------------------------------------------
# 5. DISTRICT-YEAR AGGREGATION (for first-stage)
# ------------------------------------------------------------------------------

dist_key <- if ("district_name" %in% names(migration_hh_year)) {
  "district_name"
} else if ("district77" %in% names(migration_hh_year)) {
  "district77"
} else if ("district" %in% names(migration_hh_year)) {
  "district"
} else {
  stop("No district column found in id_match_long.csv")
}

cat(sprintf("Aggregating to district-year using key: %s\n", dist_key))

has_wt <- "wt_hh" %in% names(migration_hh_year)

migration_district_year <- migration_hh_year %>%
  filter(!is.na(.data[[dist_key]])) %>%
  rename(dname_raw = all_of(dist_key)) %>%
  group_by(dname_raw, year) %>%
  summarise(
    n_hh                      = dplyr::n(),
    n_hh_with_intl_migrant    = sum(has_migrant_intl, na.rm = TRUE),
    n_total_migrants          = sum(n_migrants, na.rm = TRUE),
    n_intl_migrants           = sum(n_intl_migrants, na.rm = TRUE),
    remit_amount_intl_12m_rs  = sum(remit_amount_intl_12m_rs, na.rm = TRUE),
    remit_amount_total_12m_rs = sum(remit_amount_12m_rs,      na.rm = TRUE),
    wt_intl_migrants = if (has_wt)
      sum(n_intl_migrants * wt_hh, na.rm = TRUE) else NA_real_,
    wt_remit_intl    = if (has_wt)
      sum(remit_amount_intl_12m_rs * wt_hh, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )

write_csv(migration_district_year,
          file.path(base_out, "migration_district_year.csv"), na = "")

cat(sprintf(
  "Saved: %s/migration_district_year.csv  (%d district-year rows, years %s)\n",
  base_out,
  nrow(migration_district_year),
  paste(sort(unique(migration_district_year$year)), collapse = ", ")
))
