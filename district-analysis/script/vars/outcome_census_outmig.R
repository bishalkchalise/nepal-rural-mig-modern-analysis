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
    district       = as.character(collapse_splits(as_factor(col(census_ind_11, "DIST")))),
    birth_district = as.character(collapse_splits(as_factor(col(census_ind_11, "Q16B")))),
    is_other_dist  = is_other_dist_11
  ) %>%
  filter(!is.na(district), !is.na(birth_district),
         district != "Not Stated", birth_district != "Not Stated")

# Native population (born here OR lived here always) — denominator for shares.
# In 2011 Q16A categories that are "non-migrant" relative to current district:
#   "Same district / Born in this district"  (exact label may vary)
# We approximate native pop as everyone whose birth_district == current district.
native_pop_11 <- c11 %>%
  filter(district == birth_district) %>%
  count(district, name = "native_pop")

# Inter-district migrants only
mig_11 <- c11 %>%
  filter(is_other_dist, district != birth_district)
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
    # Reason splits not available in 2011 with this pipeline
    mig_out_economic_share    = NA_real_,
    mig_out_noneconomic_share = NA_real_,
    mig_in_economic_share     = NA_real_,
    mig_in_noneconomic_share  = NA_real_
  ) %>%
  rename(dname = district)

cat(sprintf("  2011: %d districts, total inter-dist migrants = %s\n",
            nrow(mig_out_2011), format(sum(mig_out_2011$outmig_count), big.mark = ",")))

rm(census_ind_11, c11, mig_11, inmig_11, outmig_11, native_pop_11); gc(verbose = FALSE)

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

# Inter-district migrants (weighted)
mig_21 <- c21 %>%
  filter(place_birth == "Other district", district != birth_district,
         !is.na(district), !is.na(birth_district),
         district != "997", birth_district != "997")

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

rm(census_ind_21, c21, mig_21, inmig_21, outmig_21,
   outmig_reason_21, inmig_reason_21, native_pop_21); gc(verbose = FALSE)

# ============================================================================
#  STACK + WRITE
# ============================================================================

outmig_long <- bind_rows(mig_out_2011, mig_out_2021) %>%
  select(dname, year, native_pop, inmig_count, outmig_count, net_internal_mig_count,
         mig_in_internal_share, mig_out_internal_share, net_internal_mig_share,
         mig_out_economic_share, mig_out_noneconomic_share,
         mig_in_economic_share,  mig_in_noneconomic_share) %>%
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
