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

library(tidyverse)

food_category <- tribble(
  ~foodid,                                             ~group,
  # Staples / cereals / tubers
  "Rice",                                              "staples",
  "Wheat flour",                                       "staples",
  "Maize flour",                                       "staples",
  "Maize",                                             "staples",
  "Millet",                                            "staples",
  "Barley",                                            "staples",
  "Beaten, flattened rice",                            "staples",
  "Potatoes",                                          "staples",
  
  # Pulses / legumes
  "Lentil (Black gram)",                               "pulses",
  "Beans (Green gram, masyaura)",                      "pulses",
  "Horse Gram",                                        "pulses",
  
  # Meat and fish
  "Chicken",                                           "meat_fish",
  "Mutton",                                            "meat_fish",
  "Buffalo meat",                                      "meat_fish",
  "Other meats (Pig, Boar, Duck)",                     "meat_fish",
  "Fish",                                              "meat_fish",
  
  # Dairy
  "Milk",                                              "dairy",
  "Curd / Whey",                                       "dairy",
  "Ghee",                                              "dairy",
  "Powder milk",                                       "dairy",
  "Other milk products (cheese, paneer)",              "dairy",
  
  # Eggs
  "Eggs",                                              "eggs",
  
  # Vegetables and fruits
  "Green leafy vegetables",                            "vegetables",
  "Other vegetables",                                  "vegetables",
  "Onions",                                            "vegetables",
  "Tomatoes",                                          "vegetables",
  "Fruits",                                            "fruits",
  "Dried fruits and nuts (coconut, cashew, dates etc)", "fruits_nuts",
  
  # Oils and fats
  "Mustard oil",                                       "oils_fats",
  "Vegetable oil",                                     "oils_fats",
  "Other oil (soya, sunflower, corn etc)",             "oils_fats",
  
  # Sugar / sweets
  "Sugar",                                             "sugar_sweets",
  "Gur (Sakhar)",                                      "sugar_sweets",
  "Sweets (Mithai)",                                   "sugar_sweets",
  
  # Drinks / beverages
  "Tea (dried leaves)/Coffee (ground, instant)",       "beverages",
  "Other Non Alcohoblic (Fruit Juice,Coca cola, Pepsi)", "beverages",
  
  # Condiments
  "Salt",                                              "condiments",
  "Other spices and condiments (Coriander, nutmeg)",   "condiments",
  
  # Vice / temptation
  "All Type of Alcohol",                               "vice",
  "Cigarettes",                                        "vice",
  "Tobacco,jarda, khan, beetle",                       "vice",
  
  # Miscellaneous
  "Misc. other food expenditures",                     "misc",
  "Other",                                             "misc"
)

# Check zero omission
sec5a %>%
  distinct(foodid) %>%
  anti_join(food_category, by = "foodid")


##############################################################################
# NONFOOD ACTIVITY-BASED CATEGORY MAPPING (Sections 6a + 6b)
##############################################################################
# Substring matches on lowercased item labels. First match wins; items falling
# to the default go in 'other_nonfood'. Ordering matters where categories
# overlap (e.g. appliance items that might match both 'electronics' and
# 'household_goods' resolve by whichever substring appears first below).

nonfood_category <- tribble(
  ~nonfoodid, ~group,
  
  # Frequent non-food
  "Personal care items like shampoo, cosmetics, soap", "personal_care",
  "Household cleaning articles (soap, bleach, washing powder)", "household_cleaning",
  "Personal services (haircuts, shaving, shoeshine)", "personal_services",
  "Dry cleaning and washing expenses", "clothing_services",
  "Ready-made clothing and apparel", "clothing_footwear",
  "Shoes, slippers, sandals, etc.", "clothing_footwear",
  "Pocket money to children", "transfers_pocket_money",
  "Other frequent expenses not mentioned", "other_nonfood",
  
  # Fuel / lighting
  "Matches, candles, lighters., lanterns, etc", "fuel_lighting",
  "Light bulbs, shades, batteries, etc", "fuel_lighting",
  "Cylinder gas (LPG)", "fuel_lighting",
  "Wood (bundle wood, logwood, sawdust)", "fuel_lighting",
  "Kerosene oil", "fuel_lighting",
  "Coal, charcoal", "fuel_lighting",
  
  # Transport
  "Public transportation (buses, taxis, rickshaws, train tickets", "transport",
  "Petrol, diesel, motor oil (for personal vehicle only)", "transport",
  
  # Education / media / leisure
  "Newspapers, books, stationery  supplies(except educational expenses)", "education_media",
  "Entertainment (cinema, CD/cassette rentals, etc.)", "entertainment_leisure",
  
  # Services / labour
  "Wages paid to watchman, servant, gardener, driver, etc", "domestic_services",
  
  # Ceremonies
  "Expenditure on religious ceremonies", "ceremonies",
  "Marriages, births, and other ceremonies", "ceremonies",
  "Funeral and death related expenses", "ceremonies",
  
  # Housing
  "Repair and maintenance of the house", "housing_repair",
  "Home improvements and additions", "housing_improvement",
  
  # Household durables / goods
  "Kitchen appliances (refrigerator, cooking range, blenders,etc.)", "household_durables",
  "Pillows, mattresses, blankets, etc", "household_goods",
  "Crockery, cutlery and kitchen utensils (household use)", "household_goods",
  "Repair and servicing of household effects", "household_repair",
  "Electric fans", "household_durables",
  "Furniture and fixtures", "household_durables",
  "Pressure lamps/petromax", "household_durables",
  "Heaters (electric, gas, kerosene)", "household_durables",
  "Sewing machine", "household_durables",
  "Iron (electric or other", "household_durables",
  "Washing machine", "household_durables",
  
  # Communication / technology / electronics
  "Postal expenses, telegrams, fax, telephone", "communication",
  "Telephone Set, Curdless, Mobile", "communication_durable",
  "Television/VCR", "electronics_entertainment",
  "Cassette recorder or player, radio, etc", "electronics_entertainment",
  "Computer / Printer", "electronics_tech",
  "Camera, camcorder, etc", "electronics_entertainment",
  
  # Transport durables / vehicle costs
  "Repair and other expenses for personal vehicle (registration, fines)", "transport_repair",
  "Bicycle", "transport_durable",
  "Motorcycle", "transport_durable",
  "Motor car or other such vehicle", "transport_durable",
  "Other durable goods (bullock/he buffalo carts, etc.)", "transport_durable",
  
  # Taxes / legal / insurance / valuables
  "Income taxes, land taxes, housing and property taxes", "taxes",
  "Legal expenses and insurance (life, car, etc", "legal_insurance",
  "Jewelry, watches", "jewellery_luxury",
  
  # Leisure / travel
  "Toys, sports goods", "entertainment_leisure",
  "Excursion, holiday, (including travel and lodging)", "travel_leisure"
)

# Check omissions in sec6a
sec6a %>%
  distinct(nonfoodid) %>%
  anti_join(nonfood_category, by = "nonfoodid")

# Check omissions in sec6b
sec6b %>%
  distinct(nonfoodid) %>%
  anti_join(nonfood_category, by = "nonfoodid")


##############################################################################
# 1. FOOD — SECTION 5a (7-day values by source and category)
##############################################################################

library(tidyverse)

food_5a <- sec5a %>%
  left_join(food_category, by = "foodid") %>%
  mutate(
    group = coalesce(group, "other_food"),
    
    # Correct values from codebook
    val_homeprod  = coalesce(as.numeric(s05q03), 0),
    val_purchased = coalesce(as.numeric(s05q06), 0),
    val_gift      = coalesce(as.numeric(s05q09), 0),
    
    # Main total: excluding in-kind for now
    val_total = val_homeprod + val_purchased,
    
    # Main groupings
    diet_group = case_when(
      group == "staples" ~ "staples",
      group %in% c("pulses", "meat_fish", "dairy", "eggs") ~ "protein",
      group %in% c("meat_fish", "dairy", "eggs") ~ "animal_source",
      group %in% c("vegetables", "fruits", "fruits_nuts") ~ "vegfruit",
      group %in% c("oils_fats", "sugar_sweets") ~ "oils_sugar",
      group %in% c("vice", "sugar_sweets", "beverages") ~ "temptation",
      TRUE ~ "other_food"
    ),
    
    is_staple       = group == "staples",
    is_protein      = group %in% c("pulses", "meat_fish", "dairy", "eggs"),
    is_animal       = group %in% c("meat_fish", "dairy", "eggs"),
    is_vegfruit     = group %in% c("vegetables", "fruits", "fruits_nuts"),
    is_oils_sugar   = group %in% c("oils_fats", "sugar_sweets"),
    is_temptation   = group %in% c("vice", "sugar_sweets", "beverages")
  )

food_hh <- food_5a %>%
  group_by(hhid, year) %>%
  summarise(
    # Total food values
    food_homeprod_7day  = sum(val_homeprod, na.rm = TRUE),
    food_purchased_7day = sum(val_purchased, na.rm = TRUE),
    food_total_7day     = sum(val_total, na.rm = TRUE),
    food_gift_7day      = sum(val_gift, na.rm = TRUE),
    
    # Market dependence
    food_homeprod_share = if_else(
      food_total_7day > 0,
      food_homeprod_7day / food_total_7day,
      NA_real_
    ),
    food_purchased_share = if_else(
      food_total_7day > 0,
      food_purchased_7day / food_total_7day,
      NA_real_
    ),
    
    # Diet values
    food_staples_7day = sum(val_total[is_staple], na.rm = TRUE),
    food_protein_7day = sum(val_total[is_protein], na.rm = TRUE),
    food_animal_7day = sum(val_total[is_animal], na.rm = TRUE),
    food_vegfruit_7day = sum(val_total[is_vegfruit], na.rm = TRUE),
    food_oils_sugar_7day = sum(val_total[is_oils_sugar], na.rm = TRUE),
    food_temptation_7day = sum(val_total[is_temptation], na.rm = TRUE),
    
    # Diet shares
    food_staples_share = if_else(
      food_total_7day > 0,
      food_staples_7day / food_total_7day,
      NA_real_
    ),
    food_protein_share = if_else(
      food_total_7day > 0,
      food_protein_7day / food_total_7day,
      NA_real_
    ),
    food_animal_share = if_else(
      food_total_7day > 0,
      food_animal_7day / food_total_7day,
      NA_real_
    ),
    food_vegfruit_share = if_else(
      food_total_7day > 0,
      food_vegfruit_7day / food_total_7day,
      NA_real_
    ),
    food_oils_sugar_share = if_else(
      food_total_7day > 0,
      food_oils_sugar_7day / food_total_7day,
      NA_real_
    ),
    food_temptation_share = if_else(
      food_total_7day > 0,
      food_temptation_7day / food_total_7day,
      NA_real_
    ),
    
    # Simple diversity measure
    food_groups_consumed = n_distinct(group[val_total > 0]),
    
    .groups = "drop"
  )

# Check
food_hh %>%
  summarise(
    n_hh = n(),
    mean_food_total = mean(food_total_7day, na.rm = TRUE),
    mean_purchased_share = mean(food_purchased_share, na.rm = TRUE),
    mean_homeprod_share = mean(food_homeprod_share, na.rm = TRUE),
    mean_staple_share = mean(food_staples_share, na.rm = TRUE),
    mean_protein_share = mean(food_protein_share, na.rm = TRUE),
    mean_temptation_share = mean(food_temptation_share, na.rm = TRUE)
  )


##############################################################################
# 2. FOOD SECURITY — SECTION 5b (gateway + Likert)
##############################################################################



#--------------------------------------------------
# Helpers
#--------------------------------------------------
likert_score <- function(x) {
  s <- str_squish(str_to_lower(as.character(x)))
  
  case_when(
    is.na(s) | s == "" ~ NA_integer_,
    str_detect(s, "^never") ~ 0L,
    str_detect(s, "^rarely") ~ 1L,
    str_detect(s, "^sometimes") ~ 2L,
    str_detect(s, "^often") ~ 3L,
    TRUE ~ suppressWarnings(as.integer(as.numeric(s)))
  )
}

yes_no <- function(x) {
  s <- str_squish(str_to_lower(as.character(x)))
  
  case_when(
    is.na(s) | s == "" ~ NA,
    str_detect(s, "^yes") ~ TRUE,
    str_detect(s, "^no") ~ FALSE,
    TRUE ~ NA
  )
}

#--------------------------------------------------
# Food insecurity items
#--------------------------------------------------
fs_items <- c(
  "s05q12","s05q13","s05q14","s05q15",
  "s05q16","s05q17","s05q18","s05q19"
)

#--------------------------------------------------
# Household-level food security outcomes
#--------------------------------------------------
food_sec_hh <- sec5b %>%
  mutate(
    gateway_yes = yes_no(s05q10),
    
    # score items 0-3
    across(all_of(fs_items), likert_score, .names = "{.col}_s")
  ) %>%
  rowwise() %>%
  mutate(
    
    # answered items
    valid_n = sum(!is.na(c_across(ends_with("_s")))),
    
    # count of affirmed dimensions (>0)
    affirmed_n = sum(c_across(ends_with("_s")) > 0, na.rm = TRUE),
    
    # severity points (0-24 max if all 8 items answered)
    sev_sum = sum(c_across(ends_with("_s")), na.rm = TRUE),
    
    # severe hardship items
    severe_flag = as.integer(
      any(c_across(c(s05q17_s, s05q18_s, s05q19_s)) > 0, na.rm = TRUE)
    )
    
  ) %>%
  ungroup() %>%
  mutate(
    
    #----------------------------------------------
    # FOUR MAIN OUTCOMES
    #----------------------------------------------
    
    # 1. Perceived insecurity (worry)
    perceived_food_insecurity = case_when(
      is.na(gateway_yes) ~ NA_real_,
      gateway_yes ~ 1,
      !gateway_yes ~ 0
    ),
    
    # 2. Any realized insecurity
    food_insec_any = case_when(
      gateway_yes == FALSE ~ 0,
      gateway_yes == TRUE & valid_n == 0 ~ NA_real_,
      affirmed_n > 0 ~ 1,
      TRUE ~ 0
    ),
    
    # 3. Breadth index (0-1)
    food_insec_index = case_when(
      gateway_yes == FALSE ~ 0,
      gateway_yes == TRUE & valid_n == 0 ~ NA_real_,
      TRUE ~ affirmed_n / valid_n
    ),
    
    # 4. Severe hardship
    severe_food_insecurity = case_when(
      gateway_yes == FALSE ~ 0,
      gateway_yes == TRUE & valid_n == 0 ~ NA_real_,
      TRUE ~ as.numeric(severe_flag)
    ),
    
    #----------------------------------------------
    # Optional severity-frequency score (0-1)
    #----------------------------------------------
    food_insec_severity = case_when(
      gateway_yes == FALSE ~ 0,
      gateway_yes == TRUE & valid_n == 0 ~ NA_real_,
      TRUE ~ sev_sum / (3 * valid_n)
    ),
    
    #----------------------------------------------
    # FAO-style 4-tier category
    #----------------------------------------------
    food_security_status = case_when(
      perceived_food_insecurity == 0 &
        food_insec_any == 0 ~ "Food secure",
      
      perceived_food_insecurity == 1 &
        food_insec_any == 0 ~ "Vulnerable / worried",
      
      food_insec_any == 1 &
        severe_food_insecurity == 0 ~ "Moderate insecurity",
      
      severe_food_insecurity == 1 ~ "Severe insecurity",
      
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    hhid, year,
    perceived_food_insecurity,
    food_insec_any,
    food_insec_index,
    severe_food_insecurity,
    food_insec_severity,
    food_security_status
  )

#--------------------------------------------------
# Year summary
#--------------------------------------------------
food_sec_summary <- food_sec_hh %>%
  group_by(year) %>%
  summarise(
    n_hh = n(),
    perceived_share = mean(perceived_food_insecurity, na.rm = TRUE),
    any_share = mean(food_insec_any, na.rm = TRUE),
    mean_index = mean(food_insec_index, na.rm = TRUE),
    severe_share = mean(severe_food_insecurity, na.rm = TRUE),
    mean_severity = mean(food_insec_severity, na.rm = TRUE),
    .groups = "drop"
  )

print(food_sec_summary)

#--------------------------------------------------
# Distribution of 4-tier status
#--------------------------------------------------
food_sec_hh %>%
  count(year, food_security_status) %>%
  group_by(year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  print(n = Inf)


library(tidyverse)

##############################################################################
# 3. NONFOOD — SECTIONS 6a + 6b, broad meaningful categories
##############################################################################

# Broad category mapping
nonfood_category_broad <- tribble(
  ~nonfoodid, ~category,
  
  # Basic recurrent non-food
  "Personal care items like shampoo, cosmetics, soap", "basic_nonfood",
  "Household cleaning articles (soap, bleach, washing powder)", "basic_nonfood",
  "Matches, candles, lighters., lanterns, etc", "energy_fuel_lighting",
  "Light bulbs, shades, batteries, etc", "energy_fuel_lighting",
  "Cylinder gas (LPG)", "energy_fuel_lighting",
  "Wood (bundle wood, logwood, sawdust)", "energy_fuel_lighting",
  "Kerosene oil", "energy_fuel_lighting",
  "Coal, charcoal", "energy_fuel_lighting",
  
  # Clothing and personal services
  "Ready-made clothing and apparel", "clothing_personal",
  "Shoes, slippers, sandals, etc.", "clothing_personal",
  "Personal services (haircuts, shaving, shoeshine)", "clothing_personal",
  "Dry cleaning and washing expenses", "clothing_personal",
  
  # Transport and communication
  "Public transportation (buses, taxis, rickshaws, train tickets", "transport_communication",
  "Petrol, diesel, motor oil (for personal vehicle only)", "transport_communication",
  "Postal expenses, telegrams, fax, telephone", "transport_communication",
  "Telephone Set, Curdless, Mobile", "transport_communication",
  "Repair and other expenses for personal vehicle (registration, fines)", "transport_communication",
  "Bicycle", "transport_communication",
  "Motorcycle", "transport_communication",
  "Motor car or other such vehicle", "transport_communication",
  "Other durable goods (bullock/he buffalo carts, etc.)", "transport_communication",
  
  # Housing and household goods
  "Repair and maintenance of the house", "housing_household",
  "Home improvements and additions", "housing_household",
  "Kitchen appliances (refrigerator, cooking range, blenders,etc.)", "housing_household",
  "Pillows, mattresses, blankets, etc", "housing_household",
  "Crockery, cutlery and kitchen utensils (household use)", "housing_household",
  "Repair and servicing of household effects", "housing_household",
  "Electric fans", "housing_household",
  "Furniture and fixtures", "housing_household",
  "Pressure lamps/petromax", "housing_household",
  "Heaters (electric, gas, kerosene)", "housing_household",
  "Sewing machine", "housing_household",
  "Iron (electric or other", "housing_household",
  "Washing machine", "housing_household",
  
  # Education, media, leisure, electronics
  "Newspapers, books, stationery  supplies(except educational expenses)", "education_leisure",
  "Entertainment (cinema, CD/cassette rentals, etc.)", "education_leisure",
  "Toys, sports goods", "education_leisure",
  "Excursion, holiday, (including travel and lodging)", "education_leisure",
  "Television/VCR", "education_leisure",
  "Cassette recorder or player, radio, etc", "education_leisure",
  "Computer / Printer", "education_leisure",
  "Camera, camcorder, etc", "education_leisure",
  
  # Social, ceremonial, legal, financial
  "Expenditure on religious ceremonies", "social_ceremonial_financial",
  "Marriages, births, and other ceremonies", "social_ceremonial_financial",
  "Funeral and death related expenses", "social_ceremonial_financial",
  "Income taxes, land taxes, housing and property taxes", "social_ceremonial_financial",
  "Legal expenses and insurance (life, car, etc", "social_ceremonial_financial",
  "Wages paid to watchman, servant, gardener, driver, etc", "social_ceremonial_financial",
  "Pocket money to children", "social_ceremonial_financial",
  
  # Luxury / valuables / residual
  "Jewelry, watches", "luxury_valuables",
  "Other frequent expenses not mentioned", "other_nonfood"
)

# Combine sections 6a and 6b
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
  left_join(nonfood_category_broad, by = c("item" = "nonfoodid")) %>%
  mutate(category = coalesce(category, "other_nonfood"))

# Diagnostic: check missing classifications
nonfood_long %>%
  distinct(item, category) %>%
  arrange(category, item) %>%
  print(n = Inf)

# Diagnostic: share in other_nonfood
other_share <- nonfood_long %>%
  group_by(year) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),
    other_value = sum(value[category == "other_nonfood"], na.rm = TRUE),
    other_share_pct = round(100 * other_value / pmax(total_value, 1), 1),
    .groups = "drop"
  )

print(other_share)

# Aggregate HH x year x broad category
nonfood_cat_hh <- nonfood_long %>%
  group_by(hhid, year, category) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(colname = paste0("nonfood_", category, "_12m")) %>%
  select(hhid, year, colname, value) %>%
  pivot_wider(
    names_from = colname,
    values_from = value,
    values_fill = 0
  )

# Totals from sections 6a and 6b
nonfood_totals_hh <- sec6a %>%
  group_by(hhid, year) %>%
  summarise(
    nonfood_exp_30day       = sum(coalesce(as.numeric(s06q01a), 0), na.rm = TRUE),
    nonfood_exp_12m_from_6a = sum(coalesce(as.numeric(s06q01b), 0), na.rm = TRUE),
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

# Final HH-level non-food data
nonfood_hh <- nonfood_totals_hh %>%
  left_join(nonfood_cat_hh, by = c("hhid", "year")) %>%
  mutate(
    across(starts_with("nonfood_") & ends_with("_12m"), ~ coalesce(.x, 0)),
    
    nonfood_basic_share = if_else(
      nonfood_exp_12m > 0,
      nonfood_basic_nonfood_12m / nonfood_exp_12m,
      NA_real_
    ),
    
    nonfood_energy_share = if_else(
      nonfood_exp_12m > 0,
      nonfood_energy_fuel_lighting_12m / nonfood_exp_12m,
      NA_real_
    ),
    
    nonfood_transport_comm_share = if_else(
      nonfood_exp_12m > 0,
      nonfood_transport_communication_12m / nonfood_exp_12m,
      NA_real_
    ),
    
    nonfood_housing_household_share = if_else(
      nonfood_exp_12m > 0,
      nonfood_housing_household_12m / nonfood_exp_12m,
      NA_real_
    ),
    
    nonfood_education_leisure_share = if_else(
      nonfood_exp_12m > 0,
      nonfood_education_leisure_12m / nonfood_exp_12m,
      NA_real_
    ),
    
    nonfood_social_financial_share = if_else(
      nonfood_exp_12m > 0,
      nonfood_social_ceremonial_financial_12m / nonfood_exp_12m,
      NA_real_
    )
  )

##############################################################################
# 4. DURABLES — SECTION 6c
##############################################################################

durables_hh <- sec6c %>%
  group_by(hhid, year) %>%
  summarise(
    durables_stock_value = sum(coalesce(as.numeric(s06q03b), 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    durables_use_value_12m = 0.10 * durables_stock_value
  )

# Quick summary
nonfood_hh %>%
  group_by(year) %>%
  summarise(
    n_hh = n(),
    mean_nonfood_12m = mean(nonfood_exp_12m, na.rm = TRUE),
    mean_basic_share = mean(nonfood_basic_share, na.rm = TRUE),
    mean_energy_share = mean(nonfood_energy_share, na.rm = TRUE),
    mean_transport_comm_share = mean(nonfood_transport_comm_share, na.rm = TRUE),
    mean_housing_household_share = mean(nonfood_housing_household_share, na.rm = TRUE),
    mean_education_leisure_share = mean(nonfood_education_leisure_share, na.rm = TRUE),
    mean_social_financial_share = mean(nonfood_social_financial_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()
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

##############################################################################
# 6. COMBINE HOUSEHOLD-YEAR CONSUMPTION DATA
##############################################################################

geog_hh <- sec5a %>%
  distinct(hhid, year, .keep_all = TRUE) %>%
  select(
    hhid, year, wt_hh,
    any_of(c(
      "psu", "district", "vdc",
      "vmun_code", "lgname", "district77", "district_name",
      "s00q03a", "s00q03b", "s00q03c"
    ))
  )

consumption_hh_year <- geog_hh %>%
  full_join(food_hh,     by = c("hhid", "year")) %>%
  full_join(food_sec_hh, by = c("hhid", "year")) %>%
  full_join(nonfood_hh,  by = c("hhid", "year")) %>%
  full_join(durables_hh, by = c("hhid", "year")) %>%
  full_join(selfprod_hh, by = c("hhid", "year"))

# Non-food category sum validation
cat_col_names <- grep("^nonfood_[a-z_]+_12m$", names(consumption_hh_year), value = TRUE)
cat_col_names <- setdiff(cat_col_names, "nonfood_exp_12m")

consumption_hh_year <- consumption_hh_year %>%
  rowwise() %>%
  mutate(
    nonfood_cat_sum_12m = sum(c_across(all_of(cat_col_names)), na.rm = TRUE)
  ) %>%
  ungroup()

# Final column order
id_cols <- c(
  "hhid", "year", "wt_hh",
  intersect(
    c("psu", "district", "vdc",
      "vmun_code", "lgname", "district77", "district_name",
      "s00q03a", "s00q03b", "s00q03c"),
    names(consumption_hh_year)
  )
)

food_cols <- c(
  "food_homeprod_7day",
  "food_purchased_7day",
  "food_total_7day",
  "food_gift_7day",
  "food_homeprod_share",
  "food_purchased_share",
  "food_staples_7day",
  "food_protein_7day",
  "food_animal_7day",
  "food_vegfruit_7day",
  "food_oils_sugar_7day",
  "food_temptation_7day",
  "food_staples_share",
  "food_protein_share",
  "food_animal_share",
  "food_vegfruit_share",
  "food_oils_sugar_share",
  "food_temptation_share",
  "food_groups_consumed"
)

food_sec_cols <- c(
  "perceived_food_insecurity",
  "food_insec_any",
  "food_insec_index",
  "severe_food_insecurity",
  "food_insec_severity",
  "food_security_status"
)

nonfood_hdr <- c(
  "nonfood_exp_30day",
  "nonfood_exp_12m",
  "nonfood_cat_sum_12m"
)

nonfood_cat_order <- c(
  "basic_nonfood",
  "energy_fuel_lighting",
  "clothing_personal",
  "transport_communication",
  "housing_household",
  "education_leisure",
  "social_ceremonial_financial",
  "luxury_valuables",
  "other_nonfood"
)

nonfood_cat_cols <- paste0("nonfood_", nonfood_cat_order, "_12m")
nonfood_cat_cols <- intersect(nonfood_cat_cols, names(consumption_hh_year))

nonfood_share_cols <- c(
  "nonfood_basic_share",
  "nonfood_energy_share",
  "nonfood_transport_comm_share",
  "nonfood_housing_household_share",
  "nonfood_education_leisure_share",
  "nonfood_social_financial_share"
)
nonfood_share_cols <- intersect(nonfood_share_cols, names(consumption_hh_year))

durable_cols <- c(
  "durables_stock_value",
  "durables_use_value_12m"
)

selfprod_cols <- c(
  "selfprod_nonfood_count"
)

consumption_hh_year <- consumption_hh_year %>%
  select(
    any_of(id_cols),
    any_of(food_cols),
    any_of(food_sec_cols),
    any_of(nonfood_hdr),
    any_of(nonfood_cat_cols),
    any_of(nonfood_share_cols),
    any_of(durable_cols),
    any_of(selfprod_cols)
  ) %>%
  arrange(hhid, year)


##############################################################################
# 7. SAVE + SANITY REPORT
##############################################################################

write_csv(
  consumption_hh_year,
  file.path(base_out, "consumption_hh_year.csv"),
  na = ""
)

codebook <- tribble(
  ~variable, ~unit, ~reference, ~source, ~definition,
  
  # Food expenditure and sourcing
  "food_homeprod_7day", "HH × year", "7 days", "5a s05q03",
  "Rs. market value of food consumed from household own production in the past 7 days.",
  
  "food_purchased_7day", "HH × year", "7 days", "5a s05q06",
  "Rs. value of food purchased from the market in the past 7 days.",
  
  "food_total_7day", "HH × year", "7 days", "5a s05q03 + s05q06",
  "Total food value excluding gifts/in-kind: home-produced plus purchased food.",
  
  "food_gift_7day", "HH × year", "7 days", "5a s05q09",
  "Rs. value of food received as gifts or in-kind transfers in the past 7 days.",
  
  "food_homeprod_share", "HH × year", "share", "derived",
  "Share of food value sourced from own production: home-produced / (home-produced + purchased). Excludes gifts/in-kind.",
  
  "food_purchased_share", "HH × year", "share", "derived",
  "Share of food value sourced from market purchases: purchased / (home-produced + purchased). Excludes gifts/in-kind.",
  
  # Diet composition
  "food_staples_7day", "HH × year", "7 days", "5a subset",
  "Rs. value of staples: rice, wheat flour, maize, maize flour, millet, barley, beaten rice, potatoes.",
  
  "food_protein_7day", "HH × year", "7 days", "5a subset",
  "Rs. value of protein foods: pulses, meat/fish, dairy, and eggs.",
  
  "food_animal_7day", "HH × year", "7 days", "5a subset",
  "Rs. value of animal-source foods: meat/fish, dairy, and eggs.",
  
  "food_vegfruit_7day", "HH × year", "7 days", "5a subset",
  "Rs. value of vegetables, fruits, and dried fruits/nuts.",
  
  "food_oils_sugar_7day", "HH × year", "7 days", "5a subset",
  "Rs. value of oils/fats and sugar/sweets.",
  
  "food_temptation_7day", "HH × year", "7 days", "5a subset",
  "Rs. value of vice, sugar/sweets, and beverages.",
  
  "food_staples_share", "HH × year", "share", "derived",
  "Share of total food value spent on staples.",
  
  "food_protein_share", "HH × year", "share", "derived",
  "Share of total food value spent on protein foods.",
  
  "food_animal_share", "HH × year", "share", "derived",
  "Share of total food value spent on animal-source foods.",
  
  "food_vegfruit_share", "HH × year", "share", "derived",
  "Share of total food value spent on vegetables, fruits, and dried fruits/nuts.",
  
  "food_oils_sugar_share", "HH × year", "share", "derived",
  "Share of total food value spent on oils/fats and sugar/sweets.",
  
  "food_temptation_share", "HH × year", "share", "derived",
  "Share of total food value spent on vice, sugar/sweets, and beverages.",
  
  "food_groups_consumed", "HH × year", "count", "derived",
  "Number of detailed food groups with positive consumption value.",
  
  # Food security
  "perceived_food_insecurity", "HH × year", "12 months", "5b s05q10",
  "Binary indicator equal to 1 if household worried about food availability during the reference period; 0 otherwise.",
  
  "food_insec_any", "HH × year", "12 months", "5b s05q12–s05q19",
  "Binary indicator equal to 1 if household reported any realized food insecurity experience at least rarely on one or more of the 8 items.",
  
  "food_insec_index", "HH × year", "0–1 index", "derived from 5b s05q12–s05q19",
  "Breadth index: number of affirmed food insecurity items divided by number of valid answered items. Ranges from 0 to 1.",
  
  "severe_food_insecurity", "HH × year", "12 months", "5b s05q17–s05q19",
  "Binary indicator equal to 1 if household reported severe hardship: no food, went to sleep hungry, or went a whole day/night without eating.",
  
  "food_insec_severity", "HH × year", "0–1 index", "derived from 5b s05q12–s05q19",
  "Severity-frequency index using Never=0, Rarely=1, Sometimes=2, Often=3, divided by maximum possible score.",
  
  "food_security_status", "HH × year", "category", "derived",
  "Four-category status: Food secure, Vulnerable/worried, Moderate insecurity, Severe insecurity.",
  
  # Non-food totals
  "nonfood_exp_30day", "HH × year", "30 days", "6a s06q01a",
  "Total frequent non-food spending over the past 30 days.",
  
  "nonfood_exp_12m", "HH × year", "12 months", "6a s06q01b + 6b s06q02",
  "Total non-food spending over the past 12 months.",
  
  "nonfood_cat_sum_12m", "HH × year", "12 months", "derived",
  "Sum of broad non-food category columns; used to validate against total non-food spending.",
  
  # Broad non-food categories
  "nonfood_basic_nonfood_12m", "HH × year", "12 months", "6a+6b subset",
  "Basic recurrent non-food goods: personal care and household cleaning items.",
  
  "nonfood_energy_fuel_lighting_12m", "HH × year", "12 months", "6a+6b subset",
  "Energy, fuel, and lighting: LPG, kerosene, wood, coal, candles, matches, bulbs, batteries.",
  
  "nonfood_clothing_personal_12m", "HH × year", "12 months", "6a subset",
  "Clothing, footwear, personal grooming, and clothing-related services.",
  
  "nonfood_transport_communication_12m", "HH × year", "12 months", "6a+6b subset",
  "Transport, fuel for vehicles, vehicle purchase/repair, public transport, telephone, mobile, postal, fax.",
  
  "nonfood_housing_household_12m", "HH × year", "12 months", "6b subset",
  "Housing repairs, home improvement, household goods, furniture, appliances, utensils, bedding, and household repairs.",
  
  "nonfood_education_leisure_12m", "HH × year", "12 months", "6a+6b subset",
  "Books, newspapers, stationery, entertainment, toys, sports, holidays, TV/radio/camera/computer-related leisure goods.",
  
  "nonfood_social_ceremonial_financial_12m", "HH × year", "12 months", "6a+6b subset",
  "Religious ceremonies, marriage/birth/funeral expenses, taxes, legal/insurance, domestic wages, and pocket money.",
  
  "nonfood_luxury_valuables_12m", "HH × year", "12 months", "6b subset",
  "Jewelry and watches.",
  
  "nonfood_other_nonfood_12m", "HH × year", "12 months", "6a+6b residual",
  "Residual non-food category for unclassified or miscellaneous non-food items.",
  
  # Durables / self-produced
  "durables_stock_value", "HH × year", "stock", "6c s06q03b",
  "Rs. current market value of all durable goods owned by household.",
  
  "durables_use_value_12m", "HH × year", "12 months", "derived from 6c",
  "Imputed annual consumption flow from durables, calculated as 10 percent of durable stock value.",
  
  "selfprod_nonfood_count", "HH × year", "past year", "6d s06q04a",
  "Count of non-food items self-produced by the household."
)

write_csv(codebook, file.path(base_out, "consumption_codebook.csv"))

# Sanity report
cat("\n=============================================================\n")
cat("consumption_hh_year.csv:", nrow(consumption_hh_year), "rows,",
    ncol(consumption_hh_year), "cols\n")

cat("Rows per year:",
    paste0(
      consumption_hh_year %>%
        count(year) %>%
        mutate(x = paste0(year, "=", n)) %>%
        pull(x),
      collapse = "  "
    ),
    "\n"
)

cat("HHs with municipality:", sum(!is.na(consumption_hh_year$vmun_code)),
    "/", nrow(consumption_hh_year), "\n\n")

cat("---- Classification residual: other_nonfood share ----\n")
print(other_share)

cat("\n---- Category sum vs. nonfood_exp_12m sanity ----\n")
consumption_hh_year %>%
  summarise(
    mean_total_12m = mean(nonfood_exp_12m, na.rm = TRUE),
    mean_cat_sum_12m = mean(nonfood_cat_sum_12m, na.rm = TRUE),
    diff_pct = round(
      100 * (mean_cat_sum_12m - mean_total_12m) / pmax(mean_total_12m, 1),
      2
    )
  ) %>%
  print()

cat("\n---- Full outcome summary ----\n")
consumption_hh_year %>%
  select(
    starts_with("food_"),
    starts_with("perceived_"),
    starts_with("severe_"),
    starts_with("nonfood_"),
    starts_with("durables_"),
    starts_with("selfprod_")
  ) %>%
  select(where(is.numeric)) %>%
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
  print(n = 80)

cat("=============================================================\n")