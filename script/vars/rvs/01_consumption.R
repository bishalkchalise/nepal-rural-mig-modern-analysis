##############################################################################
# NRVS STAGE 2: CONSUMPTION OUTCOMES (HH × YEAR) — v2
##############################################################################
#
# Differences from v1:
#   - Food-security (5b) now uses s05q10 gateway logic correctly; three
#     food-security outcomes instead of two (food_insec_worried added).
#   - 13 nonfood activity-based categories added (12-month each), from 6a+6b
#     combined. Categories sum to total_12m minus ambiguous-match residual.
#   - Categorisation uses case-insensitive substring matching on item labels
#     to be robust to minor wording drift across waves.
#
# Final HH × year output has 13 (original) + 13 (category) + 1 (validation)
# = 27 outcome columns, plus identifiers and the weight.
#
##############################################################################

library(tidyverse)
library(fs)

base_in  <- "data/raw/RVS Data/clean"
base_out <- "data/clean/rvs_outcomes"
dir_create(base_out, recurse = TRUE)

read_csv_q <- function(p) read_csv(p, show_col_types = FALSE, progress = FALSE)

sec5a <- read_csv_q(file.path(base_in, "food_consumption",    "section_5a.csv"))
sec5b <- read_csv_q(file.path(base_in, "food_consumption",    "section_5b.csv"))
sec6a <- read_csv_q(file.path(base_in, "nonfood_consumption", "section_6a.csv"))
sec6b <- read_csv_q(file.path(base_in, "nonfood_consumption", "section_6b.csv"))
sec6c <- read_csv_q(file.path(base_in, "nonfood_consumption", "section_6c.csv"))
sec6d <- read_csv_q(file.path(base_in, "nonfood_consumption", "section_6d.csv"))


##############################################################################
# FOOD-ITEM CATEGORY MAPPING (Section 5a) — same as v1
##############################################################################

food_category <- tribble(
  ~foodid,                                             ~group,
  "Rice",                                              "staples",
  "Wheat flour",                                       "staples",
  "Maize flour",                                       "staples",
  "Maize",                                             "staples",
  "Millet",                                            "staples",
  "Barley",                                            "staples",
  "Beaten, flattened rice",                            "staples",
  "Lentil (Black gram)",                               "pulses",
  "Beans (Green gram, masyaura)",                      "pulses",
  "Horse Gram",                                        "pulses",
  "Chicken",                                           "meat_fish",
  "Mutton",                                            "meat_fish",
  "Buffalo meat",                                      "meat_fish",
  "Other meats (Pig, Boar, Duck)",                     "meat_fish",
  "Fish",                                              "meat_fish",
  "Milk",                                              "dairy",
  "Curd / Whey",                                       "dairy",
  "Ghee",                                              "dairy",
  "Powder milk",                                       "dairy",
  "Other milk products (cheese, paneer)",              "dairy",
  "Eggs",                                              "eggs",
  "All Type of Alcohol",                               "vice",
  "Cigarettes",                                        "vice",
  "Tobacco,jarda, khan, beetle",                       "vice"
)


##############################################################################
# NONFOOD ACTIVITY-BASED CATEGORY MAPPING (Sections 6a + 6b)
##############################################################################
# Substring matches on lowercased item labels. First match wins; items falling
# to the default go in 'other_nonfood'. Ordering matters where categories
# overlap (e.g. appliance items that might match both 'electronics' and
# 'household_goods' resolve by whichever substring appears first below).

classify_nonfood <- function(item_label) {
  s <- tolower(as.character(item_label))
  dplyr::case_when(
    is.na(s) ~ NA_character_,
    
    # Transport: vehicles, fares, fuel for vehicles
    str_detect(s, "public transportation|petrol|diesel|motor oil|registration, fines|bicycle|motorcycle|motor car|scooter|bullock") ~ "transport",
    
    # Communication: phones, postal
    str_detect(s, "telephone|mobile|postal|telegram|fax|cordless") ~ "communication",
    
    # Entertainment & leisure: cinema, books, toys, holidays, media equipment
    str_detect(s, "entertainment|cinema|cd/cassette|newspaper|book|stationery|toys|sport|excursion|holiday|television|vcr|vcd|cassette recorder|radio|camera") ~ "entertainment_leisure",
    
    # Ceremonies: religious, marriage, funeral
    str_detect(s, "religious ceremon|marriage|birth|funeral|death related") ~ "ceremonies",
    
    # Taxes
    str_detect(s, "\\btax|taxes") ~ "taxes",
    
    # Fuel & lighting: cooking gas/fuel, kerosene, wood, candles
    str_detect(s, "wood|lpg|cylinder gas|kerosene|coal|charcoal|matches|candle|lighter|lantern|light bulb|batter") ~ "fuel_lighting",
    
    # Personal care: hygiene, cleaning, personal services
    str_detect(s, "personal care|household cleaning|haircut|shaving|shoeshine|dry cleaning|washing expenses|personal services") ~ "personal_care",
    
    # Clothing & footwear
    str_detect(s, "shoes|slipper|sandal|clothing|apparel") ~ "clothing_footwear",
    
    # Household goods: furniture, kitchenware, bedding, small appliances
    str_detect(s, "crockery|cutlery|kitchen utensil|pillow|mattress|blanket|kitchen appliance|refrigerator|cooking range|blender|furniture|fixture|electric fan|heater|pressure lamp|petromax|iron|sewing machine|washing machine|repair and servicing of household") ~ "household_goods",
    
    # Housing improvement: repairs and additions to the house itself
    str_detect(s, "home improvement|repair and maintenance of the house") ~ "housing_improvement",
    
    # Electronics / computing
    str_detect(s, "computer|printer") ~ "electronics_tech",
    
    # Jewellery / luxury
    str_detect(s, "jewelry|jewellery|watch") ~ "jewellery_luxury",
    
    # Legal / insurance
    str_detect(s, "legal|insurance") ~ "legal_insurance",
    
    # Everything else
    TRUE ~ "other_nonfood"
  )
}


##############################################################################
# 1. FOOD — SECTION 5a (7-day values by source and category)
##############################################################################

food_5a <- sec5a %>%
  left_join(food_category, by = "foodid") %>%
  mutate(
    group         = coalesce(group, "other_food"),
    val_purchased = coalesce(as.numeric(s05q03), 0),
    val_homeprod  = coalesce(as.numeric(s05q06), 0),
    val_gift      = coalesce(as.numeric(s05q09), 0),
    val_total     = val_purchased + val_homeprod + val_gift,
    is_protein    = group %in% c("pulses", "meat_fish", "dairy", "eggs"),
    is_vice       = group == "vice",
    is_staple     = group == "staples"
  )

food_hh <- food_5a %>%
  group_by(hhid, year) %>%
  summarise(
    food_exp_purchased_7day = sum(val_purchased, na.rm = TRUE),
    food_exp_homeprod_7day  = sum(val_homeprod,  na.rm = TRUE),
    food_exp_total_7day     = sum(val_total,     na.rm = TRUE),
    food_exp_staples_7day   = sum(val_total[is_staple],  na.rm = TRUE),
    food_exp_protein_7day   = sum(val_total[is_protein], na.rm = TRUE),
    food_exp_vice_7day      = sum(val_total[is_vice],    na.rm = TRUE),
    .groups = "drop"
  )


##############################################################################
# 2. FOOD SECURITY — SECTION 5b (gateway + Likert)
##############################################################################

likert_score <- function(x) {
  s <- tolower(as.character(x))
  dplyr::case_when(
    is.na(s)                             ~ NA_integer_,
    stringr::str_detect(s, "^never")     ~ 0L,
    stringr::str_detect(s, "^rarely")    ~ 1L,
    stringr::str_detect(s, "^sometimes") ~ 2L,
    stringr::str_detect(s, "^often")     ~ 3L,
    TRUE                                 ~ NA_integer_
  )
}

fs_items <- c("s05q11b", "s05q12", "s05q13", "s05q14", "s05q15",
              "s05q16",  "s05q17", "s05q18", "s05q19")
fs_cols_present <- intersect(fs_items, names(sec5b))

food_sec_hh <- sec5b %>%
  mutate(
    gateway_yes = dplyr::case_when(
      is.na(s05q10)                          ~ NA,
      tolower(as.character(s05q10)) == "yes" ~ TRUE,
      tolower(as.character(s05q10)) == "no"  ~ FALSE,
      TRUE                                   ~ NA
    ),
    across(all_of(fs_cols_present), likert_score, .names = "{.col}_s")
  ) %>%
  rowwise() %>%
  mutate(
    .sev_sum = sum(c_across(ends_with("_s")), na.rm = TRUE),
    .sev_any = as.integer(any(c_across(ends_with("_s")) > 0, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(
    food_insec_worried = dplyr::case_when(
      is.na(gateway_yes)   ~ NA_integer_,
      gateway_yes == TRUE  ~ 1L,
      gateway_yes == FALSE ~ 0L
    ),
    food_insec_any = dplyr::case_when(
      is.na(gateway_yes)   ~ NA_integer_,
      gateway_yes == FALSE ~ 0L,
      gateway_yes == TRUE  ~ .sev_any
    ),
    food_insec_score = dplyr::case_when(
      is.na(gateway_yes)   ~ NA_real_,
      gateway_yes == FALSE ~ 0,
      gateway_yes == TRUE  ~ as.numeric(.sev_sum)
    )
  ) %>%
  select(hhid, year, food_insec_worried, food_insec_any, food_insec_score)


##############################################################################
# 3. NONFOOD — SECTIONS 6a + 6b, categorised by activity
##############################################################################
#
# Combine frequent (6a, 12-month recall via s06q01b) and infrequent (6b,
# 12-month via s06q02) into one long table, classify each row by activity,
# then widen to one column per activity.

nonfood_long <- bind_rows(
  sec6a %>%
    mutate(
      item   = nonfoodid,
      value  = coalesce(as.numeric(s06q01b), 0),
      source = "6a_12m"
    ) %>%
    select(hhid, year, item, value, source),
  sec6b %>%
    mutate(
      item   = nonfoodid,
      value  = coalesce(as.numeric(s06q02), 0),
      source = "6b_12m"
    ) %>%
    select(hhid, year, item, value, source)
) %>%
  mutate(category = classify_nonfood(item))

# Diagnostic: report share of Rupee value landing in 'other_nonfood' per year.
# If this is large, the substring matching missed items and the mapping needs
# tightening. Printed at the end.
other_share <- nonfood_long %>%
  group_by(year) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),
    other_value = sum(value[category == "other_nonfood"], na.rm = TRUE),
    other_share_pct = round(100 * other_value / pmax(total_value, 1), 1),
    .groups = "drop"
  )

# Aggregate to HH × year × category, then pivot wide
nonfood_cat_hh <- nonfood_long %>%
  group_by(hhid, year, category) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(colname = paste0("nonfood_", category, "_12m")) %>%
  select(-category) %>%
  pivot_wider(names_from = colname, values_from = value, values_fill = 0)

# Also preserve the two existing totals from v1
nonfood_totals_hh <- sec6a %>%
  group_by(hhid, year) %>%
  summarise(
    nonfood_exp_30day        = sum(coalesce(as.numeric(s06q01a), 0), na.rm = TRUE),
    nonfood_exp_12m_from_6a  = sum(coalesce(as.numeric(s06q01b), 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  full_join(
    sec6b %>%
      group_by(hhid, year) %>%
      summarise(
        nonfood_exp_12m_from_6b = sum(coalesce(as.numeric(s06q02), 0), na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("hhid", "year")
  ) %>%
  mutate(
    nonfood_exp_30day       = coalesce(nonfood_exp_30day, 0),
    nonfood_exp_12m_from_6a = coalesce(nonfood_exp_12m_from_6a, 0),
    nonfood_exp_12m_from_6b = coalesce(nonfood_exp_12m_from_6b, 0),
    nonfood_exp_12m         = nonfood_exp_12m_from_6a + nonfood_exp_12m_from_6b
  ) %>%
  select(hhid, year, nonfood_exp_30day, nonfood_exp_12m)


##############################################################################
# 4. DURABLES — SECTION 6c
##############################################################################

durables_hh <- sec6c %>%
  group_by(hhid, year) %>%
  summarise(
    durables_stock_value = sum(coalesce(as.numeric(s06q03b), 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(durables_use_value_12m = 0.10 * durables_stock_value)


##############################################################################
# 5. SELF-PRODUCED NONFOOD — SECTION 6d
##############################################################################

selfprod_hh <- sec6d %>%
  mutate(self_yes = as.integer(
    !is.na(s06q04a) & str_to_lower(as.character(s06q04a)) == "yes"
  )) %>%
  group_by(hhid, year) %>%
  summarise(
    selfprod_nonfood_count = sum(self_yes, na.rm = TRUE),
    .groups = "drop"
  )


##############################################################################
# 6. MERGE
##############################################################################

geog_hh <- sec5a %>%
  distinct(hhid, year, .keep_all = TRUE) %>%
  select(hhid, year, wt_hh,
         any_of(c("psu", "district", "vdc",
                  "vmun_code", "lgname", "district77", "district_name",
                  "s00q03a", "s00q03b", "s00q03c")))

consumption_hh_year <- geog_hh %>%
  full_join(food_hh,           by = c("hhid", "year")) %>%
  full_join(food_sec_hh,       by = c("hhid", "year")) %>%
  full_join(nonfood_totals_hh, by = c("hhid", "year")) %>%
  full_join(nonfood_cat_hh,    by = c("hhid", "year")) %>%
  full_join(durables_hh,       by = c("hhid", "year")) %>%
  full_join(selfprod_hh,       by = c("hhid", "year"))

# Validation: sum of 13 category columns should ≈ nonfood_exp_12m (equal if
# all items classified, less than total if 'other_nonfood' absorbs residuals).
cat_col_names <- grep("^nonfood_[a-z_]+_12m$", names(consumption_hh_year),
                      value = TRUE)
cat_col_names <- setdiff(cat_col_names, "nonfood_exp_12m")  # exclude the total

consumption_hh_year <- consumption_hh_year %>%
  rowwise() %>%
  mutate(
    nonfood_cat_sum_12m = sum(c_across(all_of(cat_col_names)), na.rm = TRUE)
  ) %>%
  ungroup()

# Final column order
id_cols <- c("hhid", "year", "wt_hh",
             intersect(c("psu", "district", "vdc",
                         "vmun_code", "lgname", "district77", "district_name",
                         "s00q03a", "s00q03b", "s00q03c"),
                       names(consumption_hh_year)))

food_cols <- c("food_exp_purchased_7day", "food_exp_homeprod_7day",
               "food_exp_total_7day", "food_exp_staples_7day",
               "food_exp_protein_7day", "food_exp_vice_7day",
               "food_insec_worried", "food_insec_any", "food_insec_score")

nonfood_hdr <- c("nonfood_exp_30day", "nonfood_exp_12m", "nonfood_cat_sum_12m")
# category columns in the order they appear in classify_nonfood
cat_order <- c("transport", "communication", "entertainment_leisure",
               "ceremonies", "taxes", "fuel_lighting", "personal_care",
               "clothing_footwear", "household_goods", "housing_improvement",
               "electronics_tech", "jewellery_luxury", "legal_insurance",
               "other_nonfood")
nonfood_cat_cols <- paste0("nonfood_", cat_order, "_12m")
nonfood_cat_cols <- intersect(nonfood_cat_cols, names(consumption_hh_year))

durable_cols  <- c("durables_stock_value", "durables_use_value_12m")
selfprod_cols <- c("selfprod_nonfood_count")

consumption_hh_year <- consumption_hh_year %>%
  select(all_of(id_cols),
         all_of(food_cols),
         all_of(nonfood_hdr),
         all_of(nonfood_cat_cols),
         all_of(durable_cols),
         all_of(selfprod_cols)) %>%
  arrange(hhid, year)


##############################################################################
# 7. SAVE + SANITY REPORT
##############################################################################

write_csv(consumption_hh_year,
          file.path(base_out, "consumption_hh_year.csv"), na = "")

# Codebook
codebook <- tribble(
  ~variable,                      ~unit,           ~reference,   ~source,                   ~definition,
  
  # Food expenditure
  "food_exp_purchased_7day",      "HH × year",     "7 days",     "5a s05q03",               "Rs. value of food purchased in past 7 days (all items summed).",
  "food_exp_homeprod_7day",       "HH × year",     "7 days",     "5a s05q06",               "Rs. market-value of food consumed from own production in past 7 days.",
  "food_exp_total_7day",          "HH × year",     "7 days",     "5a s05q03+06+09",         "Total food value (purchased + home-produced + received as gift), 7-day recall.",
  "food_exp_staples_7day",        "HH × year",     "7 days",     "5a subset",               "Rs. value of cereals/grains (rice, wheat, maize, millet, barley, beaten rice) across all sources.",
  "food_exp_protein_7day",        "HH × year",     "7 days",     "5a subset",               "Rs. value of protein foods: meat/fish, dairy, eggs, pulses — all sources.",
  "food_exp_vice_7day",           "HH × year",     "7 days",     "5a subset",               "Rs. value of alcohol + cigarettes + tobacco/jarda/khan/beetle — all sources.",
  
  # Food security
  "food_insec_worried",           "HH × year",     "12 months",  "5b s05q10",               "1 if HH worried about food in any month in past 12 (gateway question); 0 if no worry; NA if not asked.",
  "food_insec_any",               "HH × year",     "12 months",  "5b s05q11b..s05q19",      "1 if any of 9 severity items reported above 'Never'; 0 if gateway=No or all 'Never'; NA if gateway missing.",
  "food_insec_score",             "HH × year",     "12 months",  "5b s05q11b..s05q19",      "0–27 HFIAS-style severity sum (Never=0, Rarely=1, Sometimes=2, Often=3). Gateway=No → 0.",
  
  # Nonfood headline totals
  "nonfood_exp_30day",            "HH × year",     "30 days",    "6a s06q01a",              "Total frequent-nonfood spending reported over past 30 days.",
  "nonfood_exp_12m",              "HH × year",     "12 months",  "6a s06q01b + 6b s06q02",  "Total nonfood spending over past 12 months (frequent annual-recall + infrequent).",
  "nonfood_cat_sum_12m",          "HH × year",     "12 months",  "derived",                 "Sum of the 13 activity-category columns below; validates against nonfood_exp_12m.",
  
  # Nonfood activity categories (12-month)
  "nonfood_transport_12m",        "HH × year",     "12 months",  "6a+6b subset",            "Public transport, petrol/diesel, bicycles, motorcycles, cars, vehicle repair/registration.",
  "nonfood_communication_12m",    "HH × year",     "12 months",  "6a+6b subset",            "Telephone sets, mobile phones, postal/telegram/fax expenses.",
  "nonfood_entertainment_leisure_12m","HH × year", "12 months",  "6a+6b subset",            "Cinema, CDs, newspapers, books, stationery, toys, sports, holidays, TV/VCR/radio/camera.",
  "nonfood_ceremonies_12m",       "HH × year",     "12 months",  "6a+6b subset",            "Religious ceremonies, marriage/birth, funeral/death expenses.",
  "nonfood_taxes_12m",            "HH × year",     "12 months",  "6b subset",               "Income, land, housing, property taxes.",
  "nonfood_fuel_lighting_12m",    "HH × year",     "12 months",  "6a subset",               "Wood, LPG, kerosene, coal, matches, candles, lanterns, light bulbs, batteries.",
  "nonfood_personal_care_12m",    "HH × year",     "12 months",  "6a subset",               "Personal care items, household cleaning, haircut/shaving, dry cleaning.",
  "nonfood_clothing_footwear_12m","HH × year",     "12 months",  "6a subset",               "Shoes/slippers/sandals, ready-made clothing.",
  "nonfood_household_goods_12m",  "HH × year",     "12 months",  "6b subset",               "Crockery/cutlery/utensils, bedding, furniture, kitchen appliances, small electrics, repair of HH effects.",
  "nonfood_housing_improvement_12m","HH × year",   "12 months",  "6b subset",               "Home improvements, repair and maintenance of the house itself.",
  "nonfood_electronics_tech_12m", "HH × year",     "12 months",  "6b subset",               "Computer/printer.",
  "nonfood_jewellery_luxury_12m", "HH × year",     "12 months",  "6b subset",               "Jewellery, watches.",
  "nonfood_legal_insurance_12m",  "HH × year",     "12 months",  "6b subset",               "Legal expenses, insurance (life, car, etc.).",
  "nonfood_other_nonfood_12m",    "HH × year",     "12 months",  "6a+6b residual",          "Items not matched by any activity category (pocket money, wages to help, misc). Large value here = mapping gap.",
  
  # Durables / self-produced
  "durables_stock_value",         "HH × year",     "stock",      "6c s06q03b",              "Rs. current market value of all durable goods owned by HH (wealth).",
  "durables_use_value_12m",       "HH × year",     "12 months",  "derived from 6c",         "Imputed annual consumption flow from durables = 0.10 × durables_stock_value.",
  "selfprod_nonfood_count",       "HH × year",     "past year",  "6d s06q04a",              "Count of nonfood items where HH reported self-producing (Yes)."
)
write_csv(codebook, file.path(base_out, "consumption_codebook.csv"))

# Sanity
cat("\n=============================================================\n")
cat("consumption_hh_year.csv:", nrow(consumption_hh_year), "rows,",
    ncol(consumption_hh_year), "cols\n")
cat("Rows per year:",
    paste0(consumption_hh_year %>% count(year) %>%
             mutate(x = paste0(year, "=", n)) %>% pull(x), collapse = "  "),
    "\n")
cat("HHs with municipality:", sum(!is.na(consumption_hh_year$vmun_code)),
    "/", nrow(consumption_hh_year), "\n\n")

cat("---- Classification residual ('other_nonfood' share) ----\n")
print(other_share)

cat("\n---- Category sum vs. nonfood_exp_12m (sanity) ----\n")
consumption_hh_year %>%
  summarise(
    mean_total_12m   = mean(nonfood_exp_12m, na.rm = TRUE),
    mean_cat_sum_12m = mean(nonfood_cat_sum_12m, na.rm = TRUE),
    diff_pct         = round(100 * (mean_cat_sum_12m - mean_total_12m) /
                               pmax(mean_total_12m, 1), 2)
  ) %>% print()

cat("\n---- Full outcome summary ----\n")
consumption_hh_year %>%
  select(starts_with("food_"), starts_with("nonfood_"),
         starts_with("durables_"), starts_with("selfprod_")) %>%
  summarise(across(everything(),
                   list(n_nonNA = ~sum(!is.na(.x)),
                        median  = ~median(.x, na.rm = TRUE),
                        mean    = ~mean(.x, na.rm = TRUE)),
                   .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_sep = "__", names_to = c("var", "stat")) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  print(n = 40)

cat("=============================================================\n")