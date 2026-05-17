################################################################################
#
# OUT-MIGRATION OUTCOMES — CENSUS 2011 + 2021 (LIFETIME / BIRTH-DISTRICT BASIS)
# ------------------------------------------------------------------------------
# District-level out-migration counts and shares, aggregated by ORIGIN (birth)
# district. Mirrors the user's working pipeline (Q16B in 2011; q19/q20/q25 in
# 2021), with paths/output aligned to the project's `dname` + `year` schema.
#
# Output:
#   district-analysis/data/clean/census/outmig_district_long.csv
#     dname, year,
#     native_pop, inmig_count, outmig_count, net_internal_mig_count,
#     mig_in_internal_share,                # inmig_count  / native_pop
#     mig_out_internal_share,               # outmig_count / native_pop
#     net_internal_mig_share,
#     # economic / non-economic splits (2021 only; NA for 2011)
#     mig_out_economic_share,
#     mig_out_noneconomic_share,
#     mig_in_economic_share,
#     mig_in_noneconomic_share
#
# Source: run from repo root
#   source("district-analysis/script/vars/outcome_census_outmig.R")
#
################################################################################

rm(list = ls()); cat("\14")

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(forcats)
})

CENSUS_FOLDER <- "data/raw/Full Census Data"
OUT_DIR       <- "district-analysis/data/clean/census"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Helper — collapse post-2014 splits to pre-split district names so 2011 and
# 2021 share the same 75-district codeframe.
collapse_splits <- function(fct) {
  fct_collapse(fct,
    "Nawalparasi" = c("Nawalparasi West", "Nawalparasi East"),
    "Rukum"       = c("Rukum West", "Rukum East")
  )
}

# Helper — convert district label strings to Title Case so 2011 (which arrives
# as 'kathmandu') matches 2021 / outcomes_district.csv ('Kathmandu').
tc_dname <- function(x) {
  out <- tools::toTitleCase(tolower(as.character(x)))
  # toTitleCase doesn't capitalise after spaces in older R; fix explicitly.
  gsub("(^|[[:space:]/-])([a-z])", "\\1\\U\\2", out, perl = TRUE)
}

# Helper — find the first existing path among candidates (case-insensitive).
# Searches recursively inside `dir` for files matching any of `pats`.
find_first <- function(dir, pats) {
  if (!dir.exists(dir)) stop("Folder does not exist: ", dir)
  hits <- list.files(dir, recursive = TRUE, full.names = TRUE,
                     ignore.case = TRUE, pattern = paste(pats, collapse = "|"))
  if (length(hits) == 0)
    stop("No file matching {", paste(pats, collapse = ", "), "} under ", dir)
  hits[[1]]
}

# Helper — fetch a column by name, case-insensitive (handles DIST vs dist).
col <- function(df, name) {
  hit <- match(tolower(name), tolower(names(df)))
  if (is.na(hit))
    stop("Column not found (case-insensitive): ", name,
         ". Available columns: ", paste(names(df), collapse = ", "))
  df[[hit]]
}

# ============================================================================
#  CENSUS 2011 — lifetime out-migration by birth district
# ============================================================================
cat("=== 2011 ===\n")

path_11 <- find_first(file.path(CENSUS_FOLDER, "Census 2011"),
                     c("^individual02\\.dta$"))
cat(sprintf("  reading %s\n", path_11))
census_ind_11 <- read_dta(path_11)

cat("  available columns matching Q16/Q19/DIST (case-insens.):\n  ")
cat(grep("^q1[69]|^dist", names(census_ind_11),
         ignore.case = TRUE, value = TRUE), sep = " | ")
cat("\n")

q16a_raw <- col(census_ind_11, "Q16A")
q16a_fct <- as_factor(q16a_raw)
cat("  Q16A value levels (first 10): ",
    paste(head(levels(q16a_fct), 10), collapse = " | "), "\n")
cat("  Q16A value counts:\n")
print(head(sort(table(as.character(q16a_fct), useNA = "ifany"),
                decreasing = TRUE), 10))

# Flexible "Other district" matcher — case/whitespace tolerant
is_other_dist_11 <- grepl("other.*district", as.character(q16a_fct),
                          ignore.case = TRUE)
cat(sprintf("  Rows flagged as 'Other district' (flexible match): %d (of %d)\n",
            sum(is_other_dist_11), length(is_other_dist_11)))

c11 <- tibble(
    district       = tc_dname(as_factor(col(census_ind_11, "DIST"))),
    birth_district = tc_dname(as_factor(col(census_ind_11, "Q16B"))),
    is_other_dist  = is_other_dist_11
  ) %>%
  mutate(
    district       = as.character(collapse_splits(factor(district))),
    birth_district = as.character(collapse_splits(factor(birth_district))),
    # When Q16A is "same district", Q16B is typically blank/NA -> treat as native.
    birth_district = if_else(!is_other_dist & is.na(birth_district),
                             district, birth_district)
  ) %>%
  filter(!is.na(district), district != "Not Stated")

cat(sprintf("  2011 sample dname after title-case: %s\n",
            paste(head(sort(unique(c11$district)), 5), collapse = ", ")))

# Native population (denominator) = STAYERS in district d (Q16A says
# "Same district" / equivalent — i.e., currently in d AND not an inter-district
# migrant). Conceptually matches 2021's
# `place_birth %in% c("Same local unit", "Other local unit (same district)")`.
native_pop_11 <- c11 %>%
  filter(!is_other_dist) %>%
  count(district, name = "native_pop")
cat(sprintf("  2011 native_pop range: min=%d  median=%d  max=%d\n",
            min(native_pop_11$native_pop),
            as.integer(median(native_pop_11$native_pop)),
            max(native_pop_11$native_pop)))

# Inter-district migrants only — drop birth_district labels that are unreliable
# placeholders ("Don't Know", "Not Reported", numeric refusal codes).
JUNK_DNAMES <- c("Don't Know", "Don'T Know", "Not Reported", "Not Stated",
                 "Refused", "997")
mig_11 <- c11 %>%
  filter(is_other_dist,
         district       != birth_district,
         !district       %in% JUNK_DNAMES,
         !birth_district %in% JUNK_DNAMES)
cat(sprintf("  2011: %d inter-district migrant rows after filter\n", nrow(mig_11)))

inmig_11 <- mig_11 %>%
  count(district, name = "inmig_count")

outmig_11 <- mig_11 %>%
  count(birth_district, name = "outmig_count") %>%
  rename(district = birth_district)

mig_out_2011 <- native_pop_11 %>%
  full_join(inmig_11,  by = "district") %>%
  full_join(outmig_11, by = "district") %>%
  mutate(across(c(native_pop, inmig_count, outmig_count),
                ~ replace_na(.x, 0L))) %>%
  mutate(
    year                   = 2011L,
    net_internal_mig_count = inmig_count - outmig_count,
    mig_in_internal_share  = if_else(native_pop > 0, inmig_count  / native_pop, NA_real_),
    mig_out_internal_share = if_else(native_pop > 0, outmig_count / native_pop, NA_real_),
    net_internal_mig_share = if_else(native_pop > 0,
                                     net_internal_mig_count / native_pop,
                                     NA_real_),
    # Reason splits not available with Q16 (no paired reason question);
    # filled below from the 5-year (Q19A/Q19B/Q18) build instead.
    mig_out_economic_share    = NA_real_,
    mig_out_noneconomic_share = NA_real_,
    mig_in_economic_share     = NA_real_,
    mig_in_noneconomic_share  = NA_real_
  ) %>%
  rename(dname = district)

cat(sprintf("  2011: %d districts, total inter-dist migrants = %s\n",
            nrow(mig_out_2011), format(sum(mig_out_2011$outmig_count), big.mark = ",")))

# ----- 2011: 5-YEAR (TEMPORARY) MIGRATION via Q19A / Q19B / Q18 -------------
# Different concept from Q16: Q19A asks place lived 5 years ago; this picks up
# recent moves only. Q18 gives reason -> economic / noneconomic split.
cat("  -- 2011 5-yr (temp) build --\n")
q19a_fct <- as_factor(col(census_ind_11, "Q19A"))
is_other_dist_5yr <- grepl("other.*district", as.character(q19a_fct),
                           ignore.case = TRUE)
q18_raw <- tryCatch(col(census_ind_11, "Q18"), error = function(e) NA_integer_)

c11_5yr <- tibble(
    district       = tc_dname(as_factor(col(census_ind_11, "DIST"))),
    birth_district = tc_dname(as_factor(col(census_ind_11, "Q19B"))),
    is_other_dist  = is_other_dist_5yr,
    reason_code    = suppressWarnings(as.integer(q18_raw))
  ) %>%
  mutate(
    district       = as.character(collapse_splits(factor(district))),
    birth_district = as.character(collapse_splits(factor(birth_district))),
    birth_district = if_else(!is_other_dist & is.na(birth_district),
                             district, birth_district),
    reason_cat = case_when(
      reason_code %in% c(1L, 2L, 3L)              ~ "economic",
      reason_code %in% c(4L, 5L, 6L, 7L, 8L)      ~ "noneconomic",
      TRUE                                        ~ NA_character_
    )
  ) %>%
  filter(!is.na(district), district != "Not Stated")

mig_11_5yr <- c11_5yr %>%
  filter(is_other_dist,
         district       != birth_district,
         !district       %in% JUNK_DNAMES,
         !birth_district %in% JUNK_DNAMES)

# Denominator: stayers in d (Q19A = same district)
native_pop_11_5yr <- c11_5yr %>%
  filter(!is_other_dist) %>%
  count(district, name = "native_pop_5yr")

# In/out totals
inmig_11_5yr  <- mig_11_5yr %>% count(district,       name = "inmig_count_5yr")
outmig_11_5yr <- mig_11_5yr %>% count(birth_district, name = "outmig_count_5yr") %>%
                 rename(district = birth_district)

# Reason splits
inmig_reason_11_5yr <- mig_11_5yr %>%
  filter(!is.na(reason_cat)) %>%
  count(district, reason_cat) %>%
  pivot_wider(names_from = reason_cat, values_from = n,
              values_fill = 0, names_prefix = "inmig_5yr_")
outmig_reason_11_5yr <- mig_11_5yr %>%
  filter(!is.na(reason_cat)) %>%
  count(birth_district, reason_cat) %>%
  pivot_wider(names_from = reason_cat, values_from = n,
              values_fill = 0, names_prefix = "outmig_5yr_") %>%
  rename(district = birth_district)

mig_out_2011_5yr <- native_pop_11_5yr %>%
  full_join(inmig_11_5yr,         by = "district") %>%
  full_join(outmig_11_5yr,        by = "district") %>%
  full_join(inmig_reason_11_5yr,  by = "district") %>%
  full_join(outmig_reason_11_5yr, by = "district") %>%
  mutate(across(c(native_pop_5yr, inmig_count_5yr, outmig_count_5yr,
                  matches("^inmig_5yr_(economic|noneconomic)$"),
                  matches("^outmig_5yr_(economic|noneconomic)$")),
                ~ replace_na(.x, 0L))) %>%
  mutate(
    year                          = 2011L,
    net_temp_mig_count            = inmig_count_5yr - outmig_count_5yr,
    mig_in_temp_share             = if_else(native_pop_5yr > 0, inmig_count_5yr  / native_pop_5yr, NA_real_),
    mig_out_temp_share            = if_else(native_pop_5yr > 0, outmig_count_5yr / native_pop_5yr, NA_real_),
    net_temp_mig_share            = if_else(native_pop_5yr > 0,
                                            net_temp_mig_count / native_pop_5yr, NA_real_),
    mig_out_temp_economic_share   = if_else(native_pop_5yr > 0,
                                            outmig_5yr_economic    / native_pop_5yr, NA_real_),
    mig_out_temp_noneconomic_share= if_else(native_pop_5yr > 0,
                                            outmig_5yr_noneconomic / native_pop_5yr, NA_real_),
    mig_in_temp_economic_share    = if_else(native_pop_5yr > 0,
                                            inmig_5yr_economic     / native_pop_5yr, NA_real_),
    mig_in_temp_noneconomic_share = if_else(native_pop_5yr > 0,
                                            inmig_5yr_noneconomic  / native_pop_5yr, NA_real_)
  ) %>%
  rename(dname = district) %>%
  select(dname, year,
         mig_in_temp_share, mig_out_temp_share, net_temp_mig_share,
         mig_out_temp_economic_share, mig_out_temp_noneconomic_share,
         mig_in_temp_economic_share,  mig_in_temp_noneconomic_share)

mig_out_2011 <- mig_out_2011 %>% left_join(mig_out_2011_5yr, by = c("dname","year"))

cat(sprintf("  2011 5-yr: %d districts, total temp migrants = %s\n",
            nrow(mig_out_2011_5yr),
            format(sum(mig_11_5yr$is_other_dist, na.rm = TRUE), big.mark = ",")))

rm(census_ind_11, c11, c11_5yr, mig_11, mig_11_5yr,
   inmig_11, outmig_11, native_pop_11,
   inmig_11_5yr, outmig_11_5yr, native_pop_11_5yr,
   inmig_reason_11_5yr, outmig_reason_11_5yr); gc(verbose = FALSE)

# ============================================================================
#  CENSUS 2021 — weighted lifetime out-migration by birth district
# ============================================================================
cat("=== 2021 ===\n")

path_21 <- find_first(file.path(CENSUS_FOLDER, "Census 2021"),
                     c("^PCMS2021_Individual\\.dta$"))
cat(sprintf("  reading %s\n", path_21))
census_ind_21 <- read_dta(path_21)

# Pull the weight column case-insensitively (some exports name it INDIVIDUAL_WT)
wt_21 <- tryCatch(col(census_ind_21, "individual_wt"),
                  error = function(e) rep(1, nrow(census_ind_21)))

c21 <- tibble(
    district       = as.character(collapse_splits(as_factor(col(census_ind_21, "dist")))),
    place_birth    = as_factor(col(census_ind_21, "q19")),
    birth_district = as.character(collapse_splits(as_factor(col(census_ind_21, "q20")))),
    reason         = as_factor(col(census_ind_21, "q25")),
    individual_wt  = wt_21
  ) %>%
  mutate(
    reason_category = case_when(
      reason %in% c("Work/employment", "Trade/business", "Agriculture")        ~ "economic",
      reason %in% c("Marriage", "Study/training", "Natural calamities",
                    "Returning back", "Dependent")                              ~ "noneconomic",
      reason %in% c("Don't know", "Not reported") | is.na(reason)               ~ NA_character_,
      TRUE                                                                      ~ "other"
    )
  )

# Native pop (lived in same local unit / same district)
native_pop_21 <- c21 %>%
  filter(place_birth %in% c("Same local unit",
                            "Other local unit (same district)")) %>%
  group_by(district) %>%
  summarise(native_pop = sum(individual_wt, na.rm = TRUE), .groups = "drop")

# Inter-district migrants (weighted) — drop refusal / unknown birth-district
# labels so they don't inflate outmig_count for real districts via misallocation.
mig_21 <- c21 %>%
  filter(place_birth == "Other district", district != birth_district,
         !is.na(district), !is.na(birth_district),
         !district       %in% JUNK_DNAMES,
         !birth_district %in% JUNK_DNAMES)

inmig_21 <- mig_21 %>%
  group_by(district) %>%
  summarise(inmig_count = sum(individual_wt, na.rm = TRUE), .groups = "drop")

outmig_21 <- mig_21 %>%
  group_by(birth_district) %>%
  summarise(outmig_count = sum(individual_wt, na.rm = TRUE), .groups = "drop") %>%
  rename(district = birth_district)

# Reason splits — outmig
outmig_reason_21 <- mig_21 %>%
  filter(!is.na(reason_category), reason_category != "other") %>%
  group_by(birth_district, reason_category) %>%
  summarise(n = sum(individual_wt, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = reason_category, values_from = n,
              values_fill = 0, names_prefix = "outmig_") %>%
  rename(district = birth_district)

# Reason splits — inmig
inmig_reason_21 <- mig_21 %>%
  filter(!is.na(reason_category), reason_category != "other") %>%
  group_by(district, reason_category) %>%
  summarise(n = sum(individual_wt, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = reason_category, values_from = n,
              values_fill = 0, names_prefix = "inmig_")

mig_out_2021 <- native_pop_21 %>%
  full_join(inmig_21,         by = "district") %>%
  full_join(outmig_21,        by = "district") %>%
  full_join(outmig_reason_21, by = "district") %>%
  full_join(inmig_reason_21,  by = "district") %>%
  filter(str_detect(district, "^[A-Za-z ]+$")) %>%
  mutate(across(c(native_pop, inmig_count, outmig_count,
                  matches("^outmig_(economic|noneconomic)$"),
                  matches("^inmig_(economic|noneconomic)$")),
                ~ replace_na(.x, 0))) %>%
  mutate(
    year                   = 2021L,
    net_internal_mig_count = inmig_count - outmig_count,
    mig_in_internal_share  = if_else(native_pop > 0, inmig_count  / native_pop, NA_real_),
    mig_out_internal_share = if_else(native_pop > 0, outmig_count / native_pop, NA_real_),
    net_internal_mig_share = if_else(native_pop > 0,
                                     net_internal_mig_count / native_pop,
                                     NA_real_),
    mig_out_economic_share    = if_else(native_pop > 0, outmig_economic    / native_pop, NA_real_),
    mig_out_noneconomic_share = if_else(native_pop > 0, outmig_noneconomic / native_pop, NA_real_),
    mig_in_economic_share     = if_else(native_pop > 0, inmig_economic     / native_pop, NA_real_),
    mig_in_noneconomic_share  = if_else(native_pop > 0, inmig_noneconomic  / native_pop, NA_real_)
  ) %>%
  rename(dname = district) %>%
  select(dname, year, native_pop, inmig_count, outmig_count, net_internal_mig_count,
         mig_in_internal_share, mig_out_internal_share, net_internal_mig_share,
         mig_out_economic_share, mig_out_noneconomic_share,
         mig_in_economic_share,  mig_in_noneconomic_share)

cat(sprintf("  2021: %d districts, total inter-dist migrants (wt) = %s\n",
            nrow(mig_out_2021), format(round(sum(mig_out_2021$outmig_count)), big.mark = ",")))

# ----- 2021: 5-YEAR (TEMPORARY) MIGRATION via q21 / q22 / q25 ---------------
# Parallel to 2011 Q19A/B/Q18. q21 = "place lived X years ago", q22 = origin
# district code, q25 = reason. If q21/q22 are absent from this export, the
# temp outcomes are filled with NA.
cat("  -- 2021 5-yr (temp) build --\n")
has_q21 <- "q21" %in% tolower(names(census_ind_21))
has_q22 <- "q22" %in% tolower(names(census_ind_21))
if (has_q21 && has_q22) {
  q21_fct <- as_factor(col(census_ind_21, "q21"))
  is_other_dist_21_5yr <- grepl("other.*district", as.character(q21_fct),
                                ignore.case = TRUE)
  c21_5yr <- tibble(
      district       = as.character(collapse_splits(as_factor(col(census_ind_21, "dist")))),
      place_5yr      = q21_fct,
      birth_district = as.character(collapse_splits(as_factor(col(census_ind_21, "q22")))),
      reason         = as_factor(col(census_ind_21, "q25")),
      individual_wt  = wt_21,
      is_other_dist  = is_other_dist_21_5yr
    ) %>%
    mutate(
      reason_cat = case_when(
        reason %in% c("Work/employment", "Trade/business", "Agriculture")    ~ "economic",
        reason %in% c("Marriage", "Study/training", "Natural calamities",
                      "Returning back", "Dependent")                          ~ "noneconomic",
        TRUE                                                                  ~ NA_character_
      )
    )

  mig_21_5yr <- c21_5yr %>%
    filter(is_other_dist, district != birth_district,
           !is.na(district), !is.na(birth_district),
           !district       %in% JUNK_DNAMES,
           !birth_district %in% JUNK_DNAMES)

  native_pop_21_5yr <- c21_5yr %>%
    filter(!is_other_dist) %>%
    group_by(district) %>%
    summarise(native_pop_5yr = sum(individual_wt, na.rm = TRUE), .groups = "drop")

  inmig_21_5yr <- mig_21_5yr %>%
    group_by(district) %>%
    summarise(inmig_count_5yr = sum(individual_wt, na.rm = TRUE), .groups = "drop")

  outmig_21_5yr <- mig_21_5yr %>%
    group_by(birth_district) %>%
    summarise(outmig_count_5yr = sum(individual_wt, na.rm = TRUE), .groups = "drop") %>%
    rename(district = birth_district)

  outmig_reason_21_5yr <- mig_21_5yr %>%
    filter(!is.na(reason_cat)) %>%
    group_by(birth_district, reason_cat) %>%
    summarise(n = sum(individual_wt, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = reason_cat, values_from = n,
                values_fill = 0, names_prefix = "outmig_5yr_") %>%
    rename(district = birth_district)
  inmig_reason_21_5yr <- mig_21_5yr %>%
    filter(!is.na(reason_cat)) %>%
    group_by(district, reason_cat) %>%
    summarise(n = sum(individual_wt, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = reason_cat, values_from = n,
                values_fill = 0, names_prefix = "inmig_5yr_")

  mig_out_2021_5yr <- native_pop_21_5yr %>%
    full_join(inmig_21_5yr,         by = "district") %>%
    full_join(outmig_21_5yr,        by = "district") %>%
    full_join(outmig_reason_21_5yr, by = "district") %>%
    full_join(inmig_reason_21_5yr,  by = "district") %>%
    filter(str_detect(district, "^[A-Za-z ]+$")) %>%
    mutate(across(c(native_pop_5yr, inmig_count_5yr, outmig_count_5yr,
                    matches("^outmig_5yr_(economic|noneconomic)$"),
                    matches("^inmig_5yr_(economic|noneconomic)$")),
                  ~ replace_na(.x, 0))) %>%
    mutate(
      year                          = 2021L,
      net_temp_mig_count            = inmig_count_5yr - outmig_count_5yr,
      mig_in_temp_share             = if_else(native_pop_5yr > 0, inmig_count_5yr  / native_pop_5yr, NA_real_),
      mig_out_temp_share            = if_else(native_pop_5yr > 0, outmig_count_5yr / native_pop_5yr, NA_real_),
      net_temp_mig_share            = if_else(native_pop_5yr > 0,
                                              net_temp_mig_count / native_pop_5yr, NA_real_),
      mig_out_temp_economic_share   = if_else(native_pop_5yr > 0,
                                              outmig_5yr_economic    / native_pop_5yr, NA_real_),
      mig_out_temp_noneconomic_share= if_else(native_pop_5yr > 0,
                                              outmig_5yr_noneconomic / native_pop_5yr, NA_real_),
      mig_in_temp_economic_share    = if_else(native_pop_5yr > 0,
                                              inmig_5yr_economic     / native_pop_5yr, NA_real_),
      mig_in_temp_noneconomic_share = if_else(native_pop_5yr > 0,
                                              inmig_5yr_noneconomic  / native_pop_5yr, NA_real_)
    ) %>%
    rename(dname = district) %>%
    select(dname, year,
           mig_in_temp_share, mig_out_temp_share, net_temp_mig_share,
           mig_out_temp_economic_share, mig_out_temp_noneconomic_share,
           mig_in_temp_economic_share,  mig_in_temp_noneconomic_share)

  mig_out_2021 <- mig_out_2021 %>% left_join(mig_out_2021_5yr, by = c("dname","year"))
  cat(sprintf("  2021 5-yr: %d districts attached\n", nrow(mig_out_2021_5yr)))
  rm(c21_5yr, mig_21_5yr, native_pop_21_5yr,
     inmig_21_5yr, outmig_21_5yr,
     outmig_reason_21_5yr, inmig_reason_21_5yr,
     mig_out_2021_5yr); gc(verbose = FALSE)
} else {
  cat("  q21/q22 not found in 2021 file; temp outcomes set to NA.\n")
  mig_out_2021 <- mig_out_2021 %>%
    mutate(mig_in_temp_share = NA_real_, mig_out_temp_share = NA_real_,
           net_temp_mig_share = NA_real_,
           mig_out_temp_economic_share = NA_real_,
           mig_out_temp_noneconomic_share = NA_real_,
           mig_in_temp_economic_share = NA_real_,
           mig_in_temp_noneconomic_share = NA_real_)
}

rm(census_ind_21, c21, mig_21, inmig_21, outmig_21,
   outmig_reason_21, inmig_reason_21, native_pop_21); gc(verbose = FALSE)

# ============================================================================
#  STACK + WRITE
# ============================================================================

outmig_long <- bind_rows(mig_out_2011, mig_out_2021) %>%
  select(dname, year, native_pop, inmig_count, outmig_count, net_internal_mig_count,
         # Permanent (lifetime / birth district) - Q16 in 2011, q19/q20 in 2021
         mig_in_internal_share, mig_out_internal_share, net_internal_mig_share,
         mig_out_economic_share, mig_out_noneconomic_share,
         mig_in_economic_share,  mig_in_noneconomic_share,
         # Temporary (5-year) - Q19A/Q19B/Q18 in 2011, q21/q22/q25 in 2021
         mig_in_temp_share, mig_out_temp_share, net_temp_mig_share,
         mig_out_temp_economic_share, mig_out_temp_noneconomic_share,
         mig_in_temp_economic_share,  mig_in_temp_noneconomic_share) %>%
  arrange(dname, year)

write_csv(outmig_long, file.path(OUT_DIR, "outmig_district_long.csv"))
cat(sprintf("\nWrote %s: %d rows x %d cols\n",
            file.path(OUT_DIR, "outmig_district_long.csv"),
            nrow(outmig_long), ncol(outmig_long)))

cat("\n--- Per-year sanity ---\n")
outmig_long %>%
  group_by(year) %>%
  summarise(
    n_dist          = n_distinct(dname),
    mean_out_share  = mean(mig_out_internal_share, na.rm = TRUE),
    sd_out_share    = sd(mig_out_internal_share,   na.rm = TRUE),
    median_out_count= median(outmig_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

cat("\nDone.  Next: re-run district-analysis/script/_robustness_all_panels.R\n")
