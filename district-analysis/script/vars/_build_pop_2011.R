################################################################################
#
# Build district-level 2011 census population and append to the instrument
# ----------------------------------------------------------------------------
# We need `geog_pop_2011` to compute a DOFE-vintage migration intensity using
# a contemporaneous denominator:  mig_int_2008_10 = DOFE_2009_10 / pop_2011.
#
# Inputs (read from raw):
#   - data/raw/Full Census Data/Census 2011/Data/individual_2011.dta
#     (or whatever the actual 2011 individual file is; this script uses the
#      same path the existing instrument.R / outcome_census.R uses)
#
# Output:
#   - district-analysis/data/clean/instrument/pop_2011_dist.csv
#     columns: dname, pop_2011
#
#   (Optional) also appends pop_2011 to instrument_forex_dist.csv in-place.
#
# Run from repo root:
#     source("district-analysis/script/vars/_build_pop_2011.R")
#
################################################################################

rm(list = ls()); cat("\14")

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
})

# The 2011 individual file path used elsewhere in the project
RAW_2011_PATH <- "data/raw/Full Census Data/Census 2011/Data/individual_2011.dta"

if (!file.exists(RAW_2011_PATH)) {
  stop("2011 individual census file not found at: ", RAW_2011_PATH,
       "\nAdjust RAW_2011_PATH in this script to point at the right file.")
}

cat("Reading 2011 census individual file...\n")
ind_11 <- read_dta(RAW_2011_PATH,
                   col_select = any_of(c("district", "district77",
                                         "district_name", "dname",
                                         "lgcode", "vmun_code")))

# Identify the district key in 2011
dist_col <- if ("dname"         %in% names(ind_11)) "dname" else
            if ("district_name" %in% names(ind_11)) "district_name" else
            if ("district77"    %in% names(ind_11)) "district77" else
            if ("district"      %in% names(ind_11)) "district" else
            stop("No district column in 2011 individual file")

# Convert labelled doubles to character if needed
if (inherits(ind_11[[dist_col]], "haven_labelled") ||
    is.numeric(ind_11[[dist_col]])) {
  ind_11$dname <- as.character(as_factor(ind_11[[dist_col]]))
} else {
  ind_11$dname <- as.character(ind_11[[dist_col]])
}
ind_11$dname <- str_squish(str_to_title(tolower(ind_11$dname)))

pop_2011 <- ind_11 %>%
  filter(!is.na(dname), dname != "") %>%
  count(dname, name = "pop_2011")

dir.create("district-analysis/data/clean/instrument",
           recursive = TRUE, showWarnings = FALSE)

write.csv(pop_2011,
          "district-analysis/data/clean/instrument/pop_2011_dist.csv",
          row.names = FALSE)

cat(sprintf("Saved: pop_2011_dist.csv  (%d districts, total pop %d)\n",
            nrow(pop_2011), sum(pop_2011$pop_2011, na.rm = TRUE)))

# Optionally merge into instrument_forex_dist.csv so downstream scripts pick
# it up without an extra read
INSTR_PATH <- "district-analysis/data/clean/instrument/instrument_forex_dist.csv"
if (file.exists(INSTR_PATH)) {
  instr <- read.csv(INSTR_PATH, stringsAsFactors = FALSE)
  if (!"geog_pop_2011" %in% names(instr)) {
    instr <- instr %>%
      left_join(pop_2011 %>% rename(geog_pop_2011 = pop_2011), by = "dname")
    write.csv(instr, INSTR_PATH, row.names = FALSE)
    cat("Appended geog_pop_2011 to: ", INSTR_PATH, "\n", sep = "")
  } else {
    cat("instrument_forex_dist.csv already has geog_pop_2011 - skipping merge.\n")
  }
}
