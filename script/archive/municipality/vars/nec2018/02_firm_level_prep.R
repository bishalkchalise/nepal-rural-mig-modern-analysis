##############################################################################
# script/nec2018/02_firm_level_prep.R
##############################################################################
#
# Reads raw NEC_2018.dta, cleans, attaches classifications from 01,
# derives productivity and behavior flags, writes firm-level CSV.
#
# Input:  data/raw/Economic Census 2018/NEC_2018.dta
# Input:  data/clean/nec2018/nsic_classification_map.csv (from 01)
# Output: data/clean/nec2018/firm_level.csv
#
##############################################################################

library(tidyverse)
library(haven)

dir.create("data/clean/nec2018", showWarnings = FALSE, recursive = TRUE)

# ---- Constants ------------------------------------------------------------
REFERENCE_YEAR_AD <- 2018
BS_AD_THRESHOLD   <- 2025     # BO8Y values above this must be BS

# ---- Helpers --------------------------------------------------------------
to_num <- function(x) suppressWarnings(as.numeric(x))
nz     <- function(x) replace(x, is.na(x), 0)

size_bucket <- function(n) {
  case_when(
    is.na(n)           ~ NA_character_,
    n == 1             ~ "micro_1",
    n >= 2  & n <= 9   ~ "small_2_9",
    n >= 10 & n <= 50  ~ "medium_10_50",
    n >= 51            ~ "large_51p"
  )
}

# BS to AD: Nepal Bikram Sambat is ~56.7 years ahead of AD
bs_to_ad <- function(bs_year) as.integer(round(bs_year - 56.7))

# Detect and convert BO8Y to AD
detect_and_convert_year <- function(y) {
  y <- to_num(y)
  case_when(
    is.na(y)                                ~ NA_integer_,
    y >= 1900 & y <= BS_AD_THRESHOLD        ~ as.integer(y),  # AD
    y >  BS_AD_THRESHOLD & y <= 2090        ~ bs_to_ad(y),    # BS
    TRUE                                     ~ NA_integer_
  )
}

# ---- Read raw census ------------------------------------------------------
cat("Reading NEC_2018.dta ...\n")
NEC_2018 <- read_dta("data/raw/Economic Census 2018/NEC_2018.dta")
cat("  rows:", nrow(NEC_2018), " cols:", ncol(NEC_2018), "\n")

nsic_map <- read_csv("data/clean/nec2018/nsic_classification_map.csv",
                     show_col_types = FALSE) |>
  mutate(nsic_2digt = str_pad(as.character(nsic_2digt), 2, pad = "0"))

# ---- Clean and derive firm-level columns ---------------------------------
cat("Cleaning and deriving ...\n")

firm_level <- NEC_2018 |>
  mutate(
    # IDs
    UNIQID = str_trim(str_replace_all(UNIQID, "\\s+", "")),
    lgcode = str_trim(str_sub(UNIQID, 1, 5)),
    DIST   = as.integer(to_num(DIST)),

    # NSIC keys
    NSIC_SEC   = str_trim(toupper(as.character(NSIC_SEC))),
    NSIC_2digt = str_pad(as.character(NSIC_2digt), 2, pad = "0"),

    # Coerce labelled -> numeric for behavioral fields
    ri1 = to_num(RI1), ri2 = to_num(RI2), ls1 = to_num(LS1), ls2 = to_num(LS2),
    mo1 = to_num(MO1), mo2 = to_num(MO2), ow1 = to_num(OW1), bo4 = to_num(BO4),
    bp1 = to_num(BP1), bp2 = to_num(BP2), ar1 = to_num(AR1),
    ac1 = to_num(AC1), ac2 = to_num(AC2), ac3 = to_num(AC3), ac4 = to_num(AC4),
    pc1 = to_num(PC1), ho1 = to_num(HO1), ho2 = to_num(HO2),

    # Employment
    pe_tot = to_num(PE1TOT), pe_nm = to_num(PE1NM), pe_nf = to_num(PE1NF),
    pe_fm  = to_num(PE1FM),  pe_ff = to_num(PE1FF),

    # Finance (monthly -> annual)
    rev_monthly = to_num(IE1), exp_monthly = to_num(IE2), sal_monthly = to_num(IE21),
    rev_annual  = rev_monthly * 12,
    exp_annual  = exp_monthly * 12,
    sal_annual  = sal_monthly * 12,
    value_added = rev_annual - exp_annual,

    cap_total         = to_num(CI1),
    cap_fixed         = to_num(CI12),
    cap_foreign_ratio = to_num(CI11),

    # Size
    size_cat = size_bucket(pe_tot),

    # Founding year
    bo8y_raw         = to_num(BO8Y),
    founding_year_ad = detect_and_convert_year(bo8y_raw),
    firm_age_years   = REFERENCE_YEAR_AD - founding_year_ad
  ) |>
  # Attach classifications
  left_join(
    nsic_map |> select(nsic_2digt, sector_short,
                       tradability, ag_orientation, manuf_tier, modernity),
    by = c("NSIC_2digt" = "nsic_2digt")
  ) |>
  # Productivity & behavior
  mutate(
    labor_productivity    = ifelse(pe_tot > 0, rev_annual / pe_tot, NA_real_),
    value_added_pw        = ifelse(pe_tot > 0, value_added / pe_tot, NA_real_),
    capital_intensity     = ifelse(pe_tot > 0, cap_total / pe_tot, NA_real_),
    capital_productivity  = ifelse(cap_total > 0, rev_annual / cap_total, NA_real_),
    wage_share_of_exp     = ifelse(exp_annual > 0, sal_annual / exp_annual, NA_real_),
    profit_margin         = ifelse(rev_annual > 0, value_added / rev_annual, NA_real_),

    is_registered         = case_when(ri1 == 1 ~ 1L, ri1 == 2 ~ 0L, TRUE ~ NA_integer_),
    is_tax_registered     = case_when(ri2 == 1 ~ 1L, ri2 == 2 ~ 0L, TRUE ~ NA_integer_),
    keeps_accounts        = case_when(ar1 == 1 ~ 1L, ar1 == 2 ~ 0L, TRUE ~ NA_integer_),
    operates_year_round   = case_when(bo4 == 1 ~ 1L, bo4 == 2 ~ 0L, TRUE ~ NA_integer_),
    has_borrowed          = case_when(ac1 == 1 ~ 1L, ac1 == 2 ~ 0L, TRUE ~ NA_integer_),
    uses_formal_credit    = case_when(
      ac1 == 1 & ac2 %in% c(1, 2, 3, 4)   ~ 1L,
      ac1 == 1 & ac2 %in% c(5, 6)         ~ 0L,
      ac1 == 2                             ~ 0L,
      TRUE                                 ~ NA_integer_
    ),

    is_incorporated       = case_when(ls1 %in% c(3, 4)              ~ 1L,
                                      ls1 %in% c(1, 2, 5, 6, 7, 8, 9, 10) ~ 0L,
                                      TRUE ~ NA_integer_),
    is_sole_prop          = case_when(ls1 == 1 ~ 1L, ls1 %in% 2:10 ~ 0L, TRUE ~ NA_integer_),
    is_cooperative        = case_when(ls1 == 5 ~ 1L, ls1 %in% c(1:4, 6:10) ~ 0L, TRUE ~ NA_integer_),

    is_multinational      = case_when(ls2 == 1 ~ 1L, ls2 == 2 ~ 0L, TRUE ~ NA_integer_),
    female_manager        = case_when(mo1 == 2 ~ 1L, mo1 == 1 ~ 0L, TRUE ~ NA_integer_),
    female_owner          = case_when(mo2 == 2 ~ 1L, mo2 == 1 ~ 0L, TRUE ~ NA_integer_),
    female_led            = pmax(female_manager, female_owner, na.rm = FALSE),
    has_foreign_capital   = case_when(!is.na(cap_foreign_ratio) & cap_foreign_ratio > 0 ~ 1L,
                                      !is.na(cap_foreign_ratio) & cap_foreign_ratio == 0 ~ 0L,
                                      TRUE ~ NA_integer_),

    has_branches          = case_when(!is.na(ho1) & ho1 > 0  ~ 1L,
                                      !is.na(ho1) & ho1 == 0 ~ 0L,
                                      TRUE ~ NA_integer_),
    has_parent            = case_when(pc1 == 1 ~ 1L, pc1 == 2 ~ 0L, TRUE ~ NA_integer_),

    owns_building         = case_when(bp1 == 1 ~ 1L, bp1 %in% 2:4 ~ 0L, TRUE ~ NA_integer_),
    owns_land             = case_when(bp2 == 1 ~ 1L, bp2 %in% 2:4 ~ 0L, TRUE ~ NA_integer_),

    cohort_5yr = case_when(
      is.na(founding_year_ad)                                   ~ NA_character_,
      founding_year_ad < 1985                                   ~ "pre_1985",
      founding_year_ad < 1990                                   ~ "1985_1989",
      founding_year_ad < 1995                                   ~ "1990_1994",
      founding_year_ad < 2000                                   ~ "1995_1999",
      founding_year_ad < 2005                                   ~ "2000_2004",
      founding_year_ad < 2010                                   ~ "2005_2009",
      founding_year_ad < 2015                                   ~ "2010_2014",
      founding_year_ad <= 2018                                  ~ "2015_2018",
      TRUE                                                       ~ NA_character_
    ),
    is_young_firm = as.integer(firm_age_years <= 5 & firm_age_years >= 0)
  ) |>
  # Final column selection
  select(
    UNIQID, lgcode, DIST,
    NSIC_SEC, NSIC_2digt, sector_short,
    tradability, ag_orientation, manuf_tier, modernity,
    pe_tot, pe_nm, pe_nf, pe_fm, pe_ff, size_cat,
    rev_annual, exp_annual, sal_annual, cap_total, cap_fixed,
    cap_foreign_ratio, value_added,
    labor_productivity, value_added_pw, capital_intensity,
    capital_productivity, wage_share_of_exp, profit_margin,
    is_registered, is_tax_registered, keeps_accounts, operates_year_round,
    has_borrowed, uses_formal_credit, is_incorporated, is_sole_prop,
    is_cooperative, is_multinational, female_manager, female_owner,
    female_led, has_foreign_capital, has_branches, has_parent,
    owns_building, owns_land,
    founding_year_ad, firm_age_years, cohort_5yr, is_young_firm,
    ac3_interest_rate = ac3
  )

write_csv(firm_level, "data/clean/nec2018/firm_level.csv")
cat("Wrote data/clean/nec2018/firm_level.csv  |  ",
    nrow(firm_level), "rows  x  ", ncol(firm_level), "cols\n")

# ---- Sanity ---------------------------------------------------------------
cat("\n-- Firms by size --\n")
print(firm_level |> count(size_cat, sort = TRUE))

cat("\n-- Firms by tradability --\n")
print(firm_level |> count(tradability, sort = TRUE))

cat("\n-- Founding year (AD after BS/AD detection) --\n")
print(firm_level |> summarise(
  n_with_year = sum(!is.na(founding_year_ad)),
  min_year    = min(founding_year_ad, na.rm = TRUE),
  median_year = median(founding_year_ad, na.rm = TRUE),
  max_year    = max(founding_year_ad, na.rm = TRUE),
  pct_post_2010 = round(100 * mean(founding_year_ad >= 2010, na.rm = TRUE), 1)
))
