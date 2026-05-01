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


library(tidyverse)

##############################################################################
# 1. CLEAN + MAP NON-AGRICULTURAL ENTERPRISE DATA
##############################################################################

sector_code <- function(x) {
  s <- str_squish(str_to_lower(as.character(x)))
  
  case_when(
    is.na(s) | s == "" ~ NA_integer_,
    
    str_detect(s, "^agriculture|^fishing") ~ 1L,
    str_detect(s, "^mining") ~ 2L,
    str_detect(s, "^manufacturing") ~ 3L,
    str_detect(s, "electricity|gas.*water supply") ~ 4L,
    str_detect(s, "water supply|waste") ~ 5L,
    str_detect(s, "^construction") ~ 6L,
    str_detect(s, "wholesale|retail") ~ 7L,
    str_detect(s, "hotel|restaurant") ~ 8L,
    str_detect(s, "transport|storage") ~ 9L,
    str_detect(s, "communication") ~ 10L,
    str_detect(s, "financial") ~ 11L,
    str_detect(s, "real estate|renting") ~ 12L,
    str_detect(s, "professional|business activit") ~ 13L,
    str_detect(s, "public administration|defence") ~ 14L,
    str_detect(s, "^education") ~ 15L,
    str_detect(s, "health|social work") ~ 16L,
    TRUE ~ 17L
  )
}

ent <- sec10 %>%
  mutate(
    sector_code = sector_code(s10q02),
    
    has_valid_enterprise = !is.na(s10q02),
    
    ownership_full = str_to_lower(as.character(s10q02a)) == "full",
    ownership_partial = str_to_lower(as.character(s10q02a)) == "partial",
    
    n_workers = coalesce(as.numeric(s10q03), 0),
    revenue_12m = coalesce(as.numeric(s10q04), 0),
    wages_12m = coalesce(as.numeric(s10q05), 0),
    fuel_12m = coalesce(as.numeric(s10q06), 0),
    rawmat_12m = coalesce(as.numeric(s10q07), 0),
    otherexp_12m = coalesce(as.numeric(s10q08), 0),
    capex_12m = coalesce(as.numeric(s10q09), 0),
    asset_sales_12m = coalesce(as.numeric(s10q10), 0),
    
    expenses_12m = wages_12m + fuel_12m + rawmat_12m + otherexp_12m,
    profit_12m = revenue_12m - expenses_12m,
    
    valid_sector = !is.na(sector_code)
  ) %>%
  filter(has_valid_enterprise)

##############################################################################
# 2. AGGREGATE ENTERPRISE DATA TO HH × YEAR
##############################################################################

ent_hh <- ent %>%
  group_by(hhid, year) %>%
  summarise(
    has_nonag_enterprise = 1L,
    n_nonag_enterprises = n(),
    n_enterprises_valid_sector = sum(valid_sector, na.rm = TRUE),
    
    n_full_owned = sum(ownership_full, na.rm = TRUE),
    n_partial_owned = sum(ownership_partial, na.rm = TRUE),
    
    enterprise_workers_total = sum(n_workers, na.rm = TRUE),
    enterprise_workers_mean = mean(n_workers, na.rm = TRUE),
    
    enterprise_revenue_12m = sum(revenue_12m, na.rm = TRUE),
    enterprise_expenses_12m = sum(expenses_12m, na.rm = TRUE),
    enterprise_profit_12m = sum(profit_12m, na.rm = TRUE),
    enterprise_capex_12m = sum(capex_12m, na.rm = TRUE),
    enterprise_asset_sales_12m = sum(asset_sales_12m, na.rm = TRUE),
    
    enterprise_rev_per_firm = mean(revenue_12m, na.rm = TRUE),
    enterprise_profit_per_firm = mean(profit_12m, na.rm = TRUE),
    
    enterprise_rev_per_worker = if_else(
      sum(n_workers, na.rm = TRUE) > 0,
      sum(revenue_12m, na.rm = TRUE) / sum(n_workers, na.rm = TRUE),
      NA_real_
    ),
    
    enterprise_profit_margin = if_else(
      sum(revenue_12m, na.rm = TRUE) > 0,
      sum(profit_12m, na.rm = TRUE) / sum(revenue_12m, na.rm = TRUE),
      NA_real_
    ),
    
    ind_agri_forestry_fish = sum(sector_code == 1, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_mining_quarrying = sum(sector_code == 2, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_manufacturing = sum(sector_code == 3, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_electricity_gas_water = sum(sector_code == 4, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_water_waste = sum(sector_code == 5, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_construction = sum(sector_code == 6, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_wholesale_retail = sum(sector_code == 7, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_accommodation_food = sum(sector_code == 8, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_transport_storage = sum(sector_code == 9, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_information_comm = sum(sector_code == 10, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_financial_insurance = sum(sector_code == 11, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_real_estate = sum(sector_code == 12, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_professional_technical = sum(sector_code == 13, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_public_admin_defence = sum(sector_code == 14, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_education = sum(sector_code == 15, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_health_social = sum(sector_code == 16, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    ind_others = sum(sector_code == 17, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    
    sector_agriculture_share =
      sum(sector_code == 1, na.rm = TRUE) / pmax(sum(valid_sector), 1),
    
    sector_manufacturing_construction_share =
      sum(sector_code %in% c(2, 3, 4, 5, 6), na.rm = TRUE) / pmax(sum(valid_sector), 1),
    
    sector_services_share =
      sum(sector_code %in% c(7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), na.rm = TRUE) / pmax(sum(valid_sector), 1),
    
    .groups = "drop"
  )

##############################################################################
# 3. CREATE HH × YEAR UNIVERSE
##############################################################################

idmap_path <- file.path(base_in, "id_match_long.csv")

if (file.exists(idmap_path)) {
  id_match <- read_csv(idmap_path, show_col_types = FALSE, progress = FALSE)
} else {
  stop("id_match_long.csv not found. Need HH universe to assign zeros to non-enterprise HHs.")
}

id_min <- id_match %>%
  distinct(hhid, year, .keep_all = TRUE) %>%
  select(
    hhid, year,
    any_of(c(
      "wt_hh", "psu", "district", "vdc",
      "vmun_code", "lgname",
      "district77", "district_name",
      "s00q03a", "s00q03b", "s00q03c"
    ))
  )

##############################################################################
# 4. FINAL HH × YEAR ENTERPRISE PANEL
##############################################################################

zero_vars <- c(
  "n_nonag_enterprises",
  "n_enterprises_valid_sector",
  "n_full_owned",
  "n_partial_owned",
  "enterprise_workers_total",
  "enterprise_workers_mean",
  "enterprise_revenue_12m",
  "enterprise_expenses_12m",
  "enterprise_profit_12m",
  "enterprise_capex_12m",
  "enterprise_asset_sales_12m",
  "sector_agriculture_share",
  "sector_manufacturing_construction_share",
  "sector_services_share",
  "ind_agri_forestry_fish",
  "ind_mining_quarrying",
  "ind_manufacturing",
  "ind_electricity_gas_water",
  "ind_water_waste",
  "ind_construction",
  "ind_wholesale_retail",
  "ind_accommodation_food",
  "ind_transport_storage",
  "ind_information_comm",
  "ind_financial_insurance",
  "ind_real_estate",
  "ind_professional_technical",
  "ind_public_admin_defence",
  "ind_education",
  "ind_health_social",
  "ind_others"
)

enterprise_hh_year <- id_min %>%
  left_join(ent_hh, by = c("hhid", "year")) %>%
  mutate(
    has_nonag_enterprise = coalesce(has_nonag_enterprise, 0L),
    
    across(
      any_of(zero_vars),
      ~ coalesce(.x, 0)
    ),
    
    enterprise_rev_per_firm = if_else(
      has_nonag_enterprise == 1,
      enterprise_rev_per_firm,
      NA_real_
    ),
    
    enterprise_profit_per_firm = if_else(
      has_nonag_enterprise == 1,
      enterprise_profit_per_firm,
      NA_real_
    ),
    
    enterprise_rev_per_worker = if_else(
      has_nonag_enterprise == 1 & enterprise_workers_total > 0,
      enterprise_rev_per_worker,
      NA_real_
    ),
    
    enterprise_profit_margin = if_else(
      has_nonag_enterprise == 1 & enterprise_revenue_12m > 0,
      enterprise_profit_margin,
      NA_real_
    )
  ) %>%
  arrange(hhid, year)

##############################################################################
# 5. SAVE
##############################################################################

write_csv(
  enterprise_hh_year,
  file.path(base_out, "enterprise_hh_year.csv"),
  na = ""
)

##############################################################################
# 6. CODEBOOK
##############################################################################

enterprise_codebook <- tribble(
  ~variable, ~unit, ~reference, ~source, ~definition,
  
  "has_nonag_enterprise", "HH × year", "past year", "Section 10 presence",
  "1 if household operated any non-agricultural enterprise; 0 otherwise.",
  
  "n_nonag_enterprises", "HH × year", "past year", "count Section 10 rows",
  "Number of non-agricultural enterprises operated by household.",
  
  "n_enterprises_valid_sector", "HH × year", "past year", "s10q02 non-missing",
  "Number of household enterprises with a valid non-missing sector code.",
  
  "n_full_owned", "HH × year", "past year", "s10q02a",
  "Number of household enterprises reported as fully owned by the household.",
  
  "n_partial_owned", "HH × year", "past year", "s10q02a",
  "Number of household enterprises reported as partially owned by the household.",
  
  "enterprise_workers_total", "HH × year", "past year", "sum s10q03",
  "Total workers across all household non-agricultural enterprises.",
  
  "enterprise_workers_mean", "enterprise-owner HH × year", "past year", "mean s10q03",
  "Average number of workers per enterprise, defined for households with enterprises.",
  
  "enterprise_revenue_12m", "HH × year", "12 months", "sum s10q04",
  "Total gross revenue across all household non-agricultural enterprises.",
  
  "enterprise_expenses_12m", "HH × year", "12 months", "sum s10q05–s10q08",
  "Total operating expenses: wages, fuel/utilities, raw materials, and other expenses.",
  
  "enterprise_profit_12m", "HH × year", "12 months", "revenue minus expenses",
  "Operating profit across all household enterprises. Capital expenditure is not subtracted.",
  
  "enterprise_capex_12m", "HH × year", "12 months", "sum s10q09",
  "Capital expenditure on business assets.",
  
  "enterprise_asset_sales_12m", "HH × year", "12 months", "sum s10q10",
  "Value received from selling business assets.",
  
  "enterprise_rev_per_firm", "enterprise-owner HH × year", "12 months", "derived",
  "Average revenue per enterprise, defined only for households with enterprises.",
  
  "enterprise_profit_per_firm", "enterprise-owner HH × year", "12 months", "derived",
  "Average profit per enterprise, defined only for households with enterprises.",
  
  "enterprise_rev_per_worker", "enterprise-owner HH × year", "12 months", "derived",
  "Revenue per worker, defined only for enterprise households with positive workers.",
  
  "enterprise_profit_margin", "enterprise-owner HH × year", "ratio", "derived",
  "Profit divided by revenue, defined only for enterprise households with positive revenue.",
  
  "ind_agri_forestry_fish", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in agriculture, forestry, or fishing.",
  
  "ind_mining_quarrying", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in mining and quarrying.",
  
  "ind_manufacturing", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in manufacturing.",
  
  "ind_electricity_gas_water", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in electricity, gas, and water supply.",
  
  "ind_water_waste", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in water or waste-related services.",
  
  "ind_construction", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in construction.",
  
  "ind_wholesale_retail", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in wholesale and retail trade, including repair of vehicles and household goods.",
  
  "ind_accommodation_food", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in hotels, restaurants, accommodation, and food services.",
  
  "ind_transport_storage", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in transport and storage.",
  
  "ind_information_comm", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in communications or information-related activities.",
  
  "ind_financial_insurance", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in financial intermediation, finance, or insurance.",
  
  "ind_real_estate", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in real estate, renting, or business activities.",
  
  "ind_professional_technical", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in professional, technical, or business service activities.",
  
  "ind_public_admin_defence", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in public administration, defence, or compulsory social security.",
  
  "ind_education", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in education.",
  
  "ind_health_social", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in health and social work.",
  
  "ind_others", "HH × year", "share", "derived from s10q02",
  "Residual share of household enterprises in other sectors.",
  
  "sector_agriculture_share", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in agriculture, forestry, or fishing.",
  
  "sector_manufacturing_construction_share", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in mining, manufacturing, utilities, water/waste, or construction.",
  
  "sector_services_share", "HH × year", "share", "derived from s10q02",
  "Share of household enterprises in trade, accommodation/food, transport, communication, finance, real estate, professional services, public administration, education, health, and other services."
)

write_csv(
  enterprise_codebook,
  file.path(base_out, "enterprise_codebook.csv")
)

##############################################################################
# 7. SANITY REPORT
##############################################################################

cat("\n=============================================================\n")
cat("enterprise_hh_year.csv:", nrow(enterprise_hh_year), "rows,",
    ncol(enterprise_hh_year), "cols\n")

cat("Rows per year:",
    paste0(
      enterprise_hh_year %>%
        count(year) %>%
        mutate(x = paste0(year, "=", n)) %>%
        pull(x),
      collapse = "  "
    ),
    "\n"
)

cat("\n---- Enterprise ownership by year ----\n")
enterprise_hh_year %>%
  group_by(year) %>%
  summarise(
    n_hh = n(),
    share_with_nonag_enterprise = round(mean(has_nonag_enterprise, na.rm = TRUE), 3),
    mean_n_enterprises_all_hh = round(mean(n_nonag_enterprises, na.rm = TRUE), 3),
    mean_n_enterprises_conditional = round(
      mean(n_nonag_enterprises[has_nonag_enterprise == 1], na.rm = TRUE),
      3
    ),
    .groups = "drop"
  ) %>%
  print()

cat("\n---- Outcome means among enterprise households only ----\n")
enterprise_hh_year %>%
  filter(has_nonag_enterprise == 1) %>%
  summarise(
    n_ent_hh = n(),
    mean_workers = mean(enterprise_workers_total, na.rm = TRUE),
    mean_revenue = mean(enterprise_revenue_12m, na.rm = TRUE),
    mean_expense = mean(enterprise_expenses_12m, na.rm = TRUE),
    mean_profit = mean(enterprise_profit_12m, na.rm = TRUE),
    mean_capex = mean(enterprise_capex_12m, na.rm = TRUE),
    mean_rev_per_worker = mean(enterprise_rev_per_worker, na.rm = TRUE),
    mean_profit_margin = mean(enterprise_profit_margin, na.rm = TRUE)
  ) %>%
  print()

cat("\n---- Broad sector shares among enterprise households ----\n")
enterprise_hh_year %>%
  filter(has_nonag_enterprise == 1) %>%
  summarise(
    agriculture = mean(sector_agriculture_share, na.rm = TRUE),
    manufacturing_construction = mean(sector_manufacturing_construction_share, na.rm = TRUE),
    services = mean(sector_services_share, na.rm = TRUE)
  ) %>%
  print()

cat("\n---- Detailed industry shares among enterprise households ----\n")
enterprise_hh_year %>%
  filter(has_nonag_enterprise == 1) %>%
  summarise(
    across(
      starts_with("ind_"),
      ~ round(mean(.x, na.rm = TRUE), 3)
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "industry",
    values_to = "mean_share"
  ) %>%
  arrange(desc(mean_share)) %>%
  print(n = Inf)

cat("\n---- Overall enterprise outcome summary ----\n")
enterprise_hh_year %>%
  select(
    has_nonag_enterprise,
    n_nonag_enterprises,
    enterprise_workers_total,
    enterprise_revenue_12m,
    enterprise_expenses_12m,
    enterprise_profit_12m,
    enterprise_capex_12m,
    enterprise_rev_per_firm,
    enterprise_profit_per_firm,
    enterprise_rev_per_worker,
    enterprise_profit_margin,
    sector_agriculture_share,
    sector_manufacturing_construction_share,
    sector_services_share
  ) %>%
  summarise(
    across(
      everything(),
      list(
        n_nonNA = ~ sum(!is.na(.x)),
        median = ~ median(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_sep = "__",
    names_to = c("var", "stat")
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  print(n = Inf)

cat("=============================================================\n")