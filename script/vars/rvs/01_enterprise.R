##############################################################################
# NRVS STAGE 2: NON-AG ENTERPRISE OUTCOMES (HH × YEAR)
##############################################################################
#
# Reads the Stage-1 raw CSV for Section 10 (non-ag enterprise, one row per
# enterprise × year) and produces one row per (hhid, year) with enterprise
# activity, revenue, costs, profit, and sector indicators.
#
# Input:
#   <base>/nonag_enterprise/section_10.csv   (one row per enterprise, keyed by
#                                             hhid + nonagid/enterpriseid)
#
# Output:
#   <out>/enterprise_hh_year.csv
#   <out>/enterprise_codebook.csv
#
# Definitions:
#   Operating expenses = s10q05 (wages) + s10q06 (fuel/utilities)
#                      + s10q07 (raw materials) + s10q08 (other operations)
#   Profit (operating) = s10q04 (gross revenue) − Operating expenses
#   Capex              = s10q09  (kept separate — investment, not a cost)
#   Asset sales        = s10q10  (kept separate)
#
# Sector codes (s10q02, 18 categories):
#   1=Agriculture, 2=Fishing, 3=Mining, 4=Manufacturing,
#   5=Electricity/Gas/Water, 6=Construction,
#   7=Wholesale/Retail, 8=Hotels/Restaurants,
#   9=Transport/Storage/Comm, 10=Financial, 11=Real Estate/Business,
#   12=Public Admin, 13=Education, 14=Health, 15=Community/Social,
#   16=Private Households, 17=Extra-territorial, 18=Other
#
# Output columns (15 outcomes + identifiers):
#   has_enterprise          1 if HH had any enterprise this year
#   n_enterprises           count of enterprises
#   n_workers_total         sum of workers across all enterprises
#   revenue_12m             sum of gross revenue (Rs.)
#   expenses_12m            sum of operating expenses (Rs.)
#   profit_12m              revenue − expenses (Rs.)
#   capex_12m               sum of s10q09 across enterprises
#   asset_sales_12m         sum of s10q10 across enterprises
#   n_partial_owned         count of enterprises with partial ownership
#   sector_manufacturing    1 if any enterprise in sector 4
#   sector_trade            1 if any in sector 7 (wholesale/retail)
#   sector_hotels           1 if any in sector 8
#   sector_transport        1 if any in sector 9
#   sector_services         1 if any in sectors 13/14/15 (education/health/community)
#   sector_other            1 if any in remaining sectors
#
##############################################################################

library(tidyverse)
library(fs)

base_in  <- "data/raw/RVS Data/clean"
base_out <- "data/clean/rvs_outcomes"
dir_create(base_out, recurse = TRUE)

# The Stage-1 file is section_10.csv (the rename-to-zero-pad was optional;
# use whichever filename exists in your clean folder).
ent_path <- file.path(base_in, "nonag_enterprise", "section_10.csv")
if (!file_exists(ent_path)) {
  ent_path <- file.path(base_in, "nonag_enterprise", "section_10.csv")  # fallback same
}
sec10 <- read_csv(ent_path, show_col_types = FALSE, progress = FALSE)

# Some waves use "nonagid" and others "enterpriseid" for the same concept.
# Harmonise to `entid` for use below.
if ("enterpriseid" %in% names(sec10) && !"nonagid" %in% names(sec10)) {
  sec10 <- sec10 %>% rename(nonagid = enterpriseid)
}


##############################################################################
# 1. ENTERPRISE-LEVEL PREP
##############################################################################
#
# Ensure numeric conversion (Stage 1 wrote labelled factors to CSV as strings).
# For s10q02 (sector), values are the factor-label text — map back to codes.

# Sector label → code (from schema labels)
sector_label_to_code <- c(
  "Agriculture"                             = 1,
  "Fishing"                                 = 2,
  "Mining And Quarrying"                    = 3,
  "Manufacturing"                           = 4,
  "Electricity, Gas And Water Supply"       = 5,
  "Construction"                            = 6,
  # Literal label varies slightly — capture both forms safely below via str_detect
  "Hotels And Restaurants"                  = 8,
  "Financial Intermediation"                = 10,
  "Real Estate, Renting And Business Activities" = 11,
  "Public Administration And Defence, Compulsory Social Security" = 12,
  "Education"                               = 13,
  "Health And Social Work"                  = 14,
  "Other Community, Social And Personal Service Activities" = 15,
  "Private Households With Employed Persons" = 16,
  "Extra Territorial Organizations And Bodies" = 17,
  "Other"                                   = 18
)

# Robust sector-to-code mapping using regex — handles the two tricky labels
# (wholesale/retail with slashes; transport/storage/communications) and is
# resilient to minor wording drift across waves.
sector_code <- function(x) {
  s <- tolower(as.character(x))
  dplyr::case_when(
    is.na(s)                                           ~ NA_integer_,
    stringr::str_detect(s, "^agriculture")             ~ 1L,
    stringr::str_detect(s, "^fishing")                 ~ 2L,
    stringr::str_detect(s, "^mining")                  ~ 3L,
    stringr::str_detect(s, "^manufacturing")           ~ 4L,
    stringr::str_detect(s, "electricity.*gas.*water")  ~ 5L,
    stringr::str_detect(s, "^construction")            ~ 6L,
    stringr::str_detect(s, "wholesale|retail")         ~ 7L,
    stringr::str_detect(s, "hotels|restaurant")        ~ 8L,
    stringr::str_detect(s, "transport.*storage|transport.*communicat") ~ 9L,
    stringr::str_detect(s, "financial")                ~ 10L,
    stringr::str_detect(s, "real estate")              ~ 11L,
    stringr::str_detect(s, "public admin")             ~ 12L,
    stringr::str_detect(s, "^education")               ~ 13L,
    stringr::str_detect(s, "health.*social")           ~ 14L,
    stringr::str_detect(s, "community.*social|personal service") ~ 15L,
    stringr::str_detect(s, "private households")      ~ 16L,
    stringr::str_detect(s, "extra territorial|extraterritorial") ~ 17L,
    stringr::str_detect(s, "^other$") | s == "other"   ~ 18L,
    TRUE                                               ~ NA_integer_
  )
}

ent <- sec10 %>%
  mutate(
    sector_code = sector_code(s10q02),
    ownership_full    = tolower(as.character(s10q02a)) == "full",
    ownership_partial = tolower(as.character(s10q02a)) == "partial",
    n_workers  = coalesce(as.numeric(s10q03), 0),
    revenue_12m  = coalesce(as.numeric(s10q04), 0),
    wages        = coalesce(as.numeric(s10q05), 0),
    fuel         = coalesce(as.numeric(s10q06), 0),
    raw_mat      = coalesce(as.numeric(s10q07), 0),
    other_exp    = coalesce(as.numeric(s10q08), 0),
    expenses_12m = wages + fuel + raw_mat + other_exp,
    profit_12m   = revenue_12m - expenses_12m,
    capex_12m    = coalesce(as.numeric(s10q09), 0),
    asset_sales_12m = coalesce(as.numeric(s10q10), 0)
  )


##############################################################################
# 2. AGGREGATE TO HH × YEAR
##############################################################################

ent_hh <- ent %>%
  group_by(hhid, year) %>%
  summarise(
    has_enterprise   = 1L,
    n_enterprises    = dplyr::n(),
    n_workers_total  = sum(n_workers, na.rm = TRUE),
    revenue_12m      = sum(revenue_12m,    na.rm = TRUE),
    expenses_12m     = sum(expenses_12m,   na.rm = TRUE),
    profit_12m       = sum(profit_12m,     na.rm = TRUE),
    capex_12m        = sum(capex_12m,      na.rm = TRUE),
    asset_sales_12m  = sum(asset_sales_12m, na.rm = TRUE),
    n_partial_owned  = sum(ownership_partial, na.rm = TRUE),
    
    sector_manufacturing = as.integer(any(sector_code == 4,  na.rm = TRUE)),
    sector_trade         = as.integer(any(sector_code == 7,  na.rm = TRUE)),
    sector_hotels        = as.integer(any(sector_code == 8,  na.rm = TRUE)),
    sector_transport     = as.integer(any(sector_code == 9,  na.rm = TRUE)),
    sector_services      = as.integer(any(sector_code %in% c(13, 14, 15), na.rm = TRUE)),
    sector_other         = as.integer(any(!sector_code %in% c(4, 7, 8, 9, 13, 14, 15),
                                          na.rm = TRUE)),
    .groups = "drop"
  )


##############################################################################
# 3. BALANCE TO ALL HH × YEAR (fill zeros for HHs with no enterprise)
##############################################################################
#
# Non-enterprise HHs are absent from Section 10 entirely. Any consumption of
# the output with a subsequent "share of HHs with enterprise" calculation will
# want those HHs present with zeros. We reconstruct the HH universe from the
# id_match produced in Stage 1.

idmap_path <- file.path(base_in, "id_match_long.csv")
if (file_exists(idmap_path)) {
  id_match <- read_csv(idmap_path, show_col_types = FALSE, progress = FALSE)
} else {
  # Fallback: build HH×year universe from section_10 hhids plus any other
  # section you have. Minimally it covers enterprise HHs only.
  warning("id_match_long.csv not found; balanced panel will cover only ",
          "HHs observed in Section 10.")
  id_match <- ent %>% distinct(hhid, year)
}

# Keep id_match column set minimal for this output
id_min <- id_match %>%
  select(hhid, year,
         any_of(c("wt_hh", "psu", "district", "vdc",
                  "vmun_code", "lgname", "district77", "district_name",
                  "s00q03a", "s00q03b", "s00q03c")))

enterprise_hh_year <- id_min %>%
  left_join(ent_hh, by = c("hhid", "year")) %>%
  mutate(
    has_enterprise = coalesce(has_enterprise, 0L),
    across(c(n_enterprises, n_workers_total,
             revenue_12m, expenses_12m, profit_12m,
             capex_12m, asset_sales_12m, n_partial_owned,
             sector_manufacturing, sector_trade, sector_hotels,
             sector_transport, sector_services, sector_other),
           ~ coalesce(., 0))
  ) %>%
  arrange(hhid, year)


##############################################################################
# 4. SAVE + SANITY REPORT
##############################################################################

write_csv(enterprise_hh_year,
          file.path(base_out, "enterprise_hh_year.csv"), na = "")

# Codebook
codebook <- tribble(
  ~variable,                ~unit,       ~reference,   ~source,              ~definition,
  "has_enterprise",         "HH × year", "past year",  "sec 10 presence",    "1 if HH ran any non-ag enterprise this year; 0 otherwise.",
  "n_enterprises",          "HH × year", "past year",  "count sec 10 rows",  "Number of non-ag enterprises operated by HH.",
  "n_workers_total",        "HH × year", "past year",  "sum s10q03",         "Total workers across all HH enterprises (includes unpaid family).",
  "revenue_12m",            "HH × year", "12 months",  "sum s10q04",         "Total gross revenue (Rs.) across all HH enterprises, past 12 months.",
  "expenses_12m",           "HH × year", "12 months",  "sum s10q05..08",     "Total operating expenses = wages + fuel/utilities + raw materials + other operations.",
  "profit_12m",             "HH × year", "12 months",  "revenue − expenses", "Operating profit. Capex (s10q09) NOT subtracted.",
  "capex_12m",              "HH × year", "12 months",  "sum s10q09",         "Capital expenditure: cash or in-kind value spent on assets for the business.",
  "asset_sales_12m",        "HH × year", "12 months",  "sum s10q10",         "Value received from selling business assets.",
  "n_partial_owned",        "HH × year", "past year",  "count s10q02a=Partial","Count of enterprises with partial ownership (vs full). Missing in 2017.",
  "sector_manufacturing",   "HH × year", "past year",  "s10q02 == 4",        "1 if HH operated any manufacturing enterprise.",
  "sector_trade",           "HH × year", "past year",  "s10q02 == 7",        "1 if HH operated any wholesale/retail enterprise.",
  "sector_hotels",          "HH × year", "past year",  "s10q02 == 8",        "1 if HH operated any hotel/restaurant enterprise.",
  "sector_transport",       "HH × year", "past year",  "s10q02 == 9",        "1 if HH operated any transport/storage/communications enterprise.",
  "sector_services",        "HH × year", "past year",  "s10q02 ∈ {13,14,15}", "1 if HH operated any education, health, or community/social service.",
  "sector_other",           "HH × year", "past year",  "s10q02 other",       "1 if HH operated any enterprise in sectors 1,2,3,5,6,10,11,12,16,17,18."
)
write_csv(codebook, file.path(base_out, "enterprise_codebook.csv"))

# Sanity
cat("\n=============================================================\n")
cat("enterprise_hh_year.csv:", nrow(enterprise_hh_year), "rows,",
    ncol(enterprise_hh_year), "cols\n")
cat("Rows per year: ",
    paste0(enterprise_hh_year %>% count(year) %>%
             mutate(x = paste0(year, "=", n)) %>% pull(x), collapse = "  "),
    "\n")

cat("\n---- Enterprise ownership by year ----\n")
enterprise_hh_year %>%
  group_by(year) %>%
  summarise(
    n_hh            = dplyr::n(),
    share_with_ent  = round(mean(has_enterprise == 1), 3),
    mean_n_ent      = round(mean(n_enterprises), 3),
    .groups = "drop"
  ) %>% print()

cat("\n---- Outcome means (among HHs with has_enterprise = 1) ----\n")
enterprise_hh_year %>%
  filter(has_enterprise == 1) %>%
  summarise(
    n_ent_hh     = dplyr::n(),
    mean_workers = mean(n_workers_total,  na.rm = TRUE),
    mean_revenue = mean(revenue_12m,      na.rm = TRUE),
    mean_expense = mean(expenses_12m,     na.rm = TRUE),
    mean_profit  = mean(profit_12m,       na.rm = TRUE),
    mean_capex   = mean(capex_12m,        na.rm = TRUE)
  ) %>% print()

cat("\n---- Sector prevalence (share of HHs with enterprise in each sector) ----\n")
enterprise_hh_year %>%
  filter(has_enterprise == 1) %>%
  summarise(across(starts_with("sector_"),
                   ~ round(mean(.x), 3))) %>%
  print(width = Inf)

cat("\n---- Overall outcome summary ----\n")
enterprise_hh_year %>%
  select(starts_with("has_"), starts_with("n_"),
         ends_with("_12m"), starts_with("sector_")) %>%
  summarise(across(everything(),
                   list(n_nonNA = ~sum(!is.na(.x)),
                        median  = ~median(.x, na.rm = TRUE),
                        mean    = ~mean(.x, na.rm = TRUE)),
                   .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_sep = "__", names_to = c("var", "stat")) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  print(n = 40)

cat("=============================================================\n")