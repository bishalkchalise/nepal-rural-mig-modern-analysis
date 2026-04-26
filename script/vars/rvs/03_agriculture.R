##############################################################################
# NRVS STAGE 2: AGRICULTURE OUTCOMES (HH × YEAR)
##############################################################################
#
# Reads Stage-1 raw CSVs for the agriculture module (Sections 9a1, 9a2, 9a3,
# 9a4, 9b1, 9b2, 9c, 9d, 9e, 9f) and produces one row per (hhid, year) with
# 24 behavioural outcomes organised around four questions:
#
#   (A) What happens to the land? (use / deployment)
#   (B) Does the HH trade land? (land market)
#   (C) How intensively do they farm? (input use)
#   (D) What do they grow and do they sell it? (crop choice + market)
#   (E) What technology do they use? (equipment adoption)
#
# The unit of each sub-section is not uniform:
#   - 9a1: one row per owned plot × HH × year
#   - 9a2: one row per rented/shared-in plot × HH × year
#   - 9a3: one row per HH × year (land transactions)
#   - 9a4: one row per HH × year (wide crop-grown flags, 167 cols)
#   - 9b1: one row per HH × year × crop (wet season)
#   - 9b2: one row per HH × year × crop (dry season)
#   - 9c:  one row per HH × year (input use & costs)
#   - 9d:  one row per HH × year × livestock type
#   - 9e:  one row per HH × year (livestock product revenue)
#   - 9f:  one row per HH × year × equipment type
#
# We aggregate each to HH × year, merge by (hhid, year), and balance to the
# full HH universe so non-agricultural HHs appear with agro_hh = 0.
#
# Output:
#   <out>/agriculture_hh_year.csv
#   <out>/agriculture_codebook.csv
#
##############################################################################

library(tidyverse)
library(fs)

base_in  <- "data/raw/RVS Data/clean"
base_out <- "data/clean/rvs_outcomes"
dir_create(base_out, recurse = TRUE)

read_csv_q <- function(p) read_csv(p, show_col_types = FALSE, progress = FALSE)

# Sub-section files (paths depend on your Stage-1 folder structure)
p9a1 <- file.path(base_in, "land/section_9a1.csv")
p9a2 <- file.path(base_in, "land/section_9a2.csv")
p9a3 <- file.path(base_in, "land/section_9a3.csv")
p9a4 <- file.path(base_in, "land/section_9a4.csv")
p9b1 <- file.path(base_in, "crop_production/section_9b1.csv")
p9b2 <- file.path(base_in, "crop_production/section_9b2.csv")
p9c  <- file.path(base_in, "ag_inputs/section_9c.csv")
p9d  <- file.path(base_in, "livestock/section_9d.csv")
p9f  <- file.path(base_in, "ag_equipment/section_9f.csv")

sec9a1 <- read_csv_q(p9a1)
sec9a2 <- read_csv_q(p9a2)
sec9a3 <- read_csv_q(p9a3)
sec9a4 <- read_csv_q(p9a4)
sec9b1 <- read_csv_q(p9b1)
sec9b2 <- read_csv_q(p9b2)
sec9c  <- read_csv_q(p9c)
sec9d  <- read_csv_q(p9d)
sec9f  <- read_csv_q(p9f)


##############################################################################
# HELPER: robust case-insensitive substring match against a factor-string col
##############################################################################

match_any <- function(x, patterns) {
  s <- tolower(as.character(x))
  s <- dplyr::coalesce(s, "")
  pattern <- paste(patterns, collapse = "|")
  stringr::str_detect(s, pattern)
}


##############################################################################
# (A) LAND USE — from Section 9a1 (owned plots)
##############################################################################
#
# s09q07  = land use in LAST WET season (factor labels: self-cropped /
#           sharecropped out / rented out / fallow)
# s09q11  = land use in LAST DRY season (same labels)
#
# For each owned plot we classify its WET-season use and DRY-season use, then
# aggregate to HH × year: count of plots by use type.

land_9a1 <- sec9a1 %>%
  mutate(
    # Wet-season use categories (priority order: fallow → rented/shared out → self)
    use_wet_self   = match_any(s09q07, c("self.?crop", "self.?cultivat", "own farm")),
    use_wet_shared = match_any(s09q07, c("share.?crop")),
    use_wet_rented = match_any(s09q07, c("rent")) & !use_wet_shared,
    use_wet_fallow = match_any(s09q07, c("fallow")),
    # Dry-season use categories
    use_dry_self   = match_any(s09q11, c("self.?crop", "self.?cultivat", "own farm")),
    use_dry_shared = match_any(s09q11, c("share.?crop")),
    use_dry_rented = match_any(s09q11, c("rent")) & !use_dry_shared,
    use_dry_fallow = match_any(s09q11, c("fallow")),
    # Area
    plot_area_sqm  = coalesce(as.numeric(area_sqm), 0)
  ) %>%
  mutate(
    # Plot-level summary flags (is plot cultivated by HH in either season?)
    plot_self_cropped   = use_wet_self | use_dry_self,
    plot_rented_out     = use_wet_shared | use_wet_rented |
      use_dry_shared | use_dry_rented,
    plot_fallow_any     = use_wet_fallow | use_dry_fallow,
    plot_cultivated_any = plot_self_cropped,
    plot_both_seasons   = use_wet_self & use_dry_self
  )

land_use_hh <- land_9a1 %>%
  group_by(hhid, year) %>%
  summarise(
    owned_plots_n          = dplyr::n(),
    owned_area_sqm         = sum(plot_area_sqm, na.rm = TRUE),
    plots_self_cropped_n   = sum(plot_self_cropped, na.rm = TRUE),
    plots_rented_out_n     = sum(plot_rented_out,  na.rm = TRUE),
    plots_fallow_n         = sum(plot_fallow_any,  na.rm = TRUE),
    area_self_cropped_sqm  = sum(plot_area_sqm * plot_self_cropped, na.rm = TRUE),
    plots_both_seasons_n   = sum(plot_both_seasons, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    plots_abandoned_share    = (plots_rented_out_n + plots_fallow_n) /
      pmax(owned_plots_n, 1),
    plots_both_seasons_share = plots_both_seasons_n /
      pmax(owned_plots_n, 1)
  ) %>%
  select(-plots_both_seasons_n)  # keep only the share

# Rented/shared IN area — from 9a2 (separate concept)
rented_in_hh <- sec9a2 %>%
  mutate(area_in_sqm = coalesce(as.numeric(area_sqm), 0)) %>%
  group_by(hhid, year) %>%
  summarise(rented_in_area_sqm = sum(area_in_sqm, na.rm = TRUE),
            .groups = "drop")


##############################################################################
# (B) LAND MARKET — from Section 9a3
##############################################################################
# s09q26 = sold any land in past 12m (Yes/No)
# s09q28a = amount received from sale
# s09q29 = bought any land in past 12m
# s09q31a = amount paid for land bought

land_market_hh <- sec9a3 %>%
  mutate(
    land_sold_flag      = as.integer(match_any(s09q26, c("^yes"))),
    land_bought_flag    = as.integer(match_any(s09q29, c("^yes"))),
    land_sold_rs_row    = coalesce(as.numeric(s09q28a), 0),
    land_bought_rs_row  = coalesce(as.numeric(s09q31a), 0)
  ) %>%
  group_by(hhid, year) %>%
  summarise(
    land_sold_any      = as.integer(any(land_sold_flag == 1, na.rm = TRUE)),
    land_bought_any    = as.integer(any(land_bought_flag == 1, na.rm = TRUE)),
    land_sold_12m_rs   = sum(land_sold_rs_row,   na.rm = TRUE),
    land_bought_12m_rs = sum(land_bought_rs_row, na.rm = TRUE),
    .groups = "drop"
  )


##############################################################################
# (C) FARMING INTENSITY — from Section 9c (inputs, wet + dry combined)
##############################################################################
# s09q52a/c/e/g/i = used input (Y/N) WET season  [seed/fert/insect/equip/labour]
# s09q52b/d/f/h/j = amount spent WET
# s09q53* = parallel for DRY season

intensity_hh <- sec9c %>%
  mutate(
    use_seed_flag       = as.integer(match_any(s09q52a, "^yes")) |
      as.integer(match_any(s09q53a, "^yes")),
    use_fert_flag       = as.integer(match_any(s09q52c, "^yes")) |
      as.integer(match_any(s09q53c, "^yes")),
    use_insect_flag     = as.integer(match_any(s09q52e, "^yes")) |
      as.integer(match_any(s09q53e, "^yes")),
    use_equip_flag      = as.integer(match_any(s09q52g, "^yes")) |
      as.integer(match_any(s09q53g, "^yes")),
    use_labour_flag     = as.integer(match_any(s09q52i, "^yes")) |
      as.integer(match_any(s09q53i, "^yes")),
    
    input_seed_rs       = coalesce(as.numeric(s09q52b), 0) +
      coalesce(as.numeric(s09q53b), 0),
    input_fert_rs       = coalesce(as.numeric(s09q52d), 0) +
      coalesce(as.numeric(s09q53d), 0),
    input_insect_rs     = coalesce(as.numeric(s09q52f), 0) +
      coalesce(as.numeric(s09q53f), 0),
    input_equip_hire_rs = coalesce(as.numeric(s09q52h), 0) +
      coalesce(as.numeric(s09q53h), 0),
    input_hired_labour_rs = coalesce(as.numeric(s09q52j), 0) +
      coalesce(as.numeric(s09q53j), 0),
    input_total_12m_rs  = input_seed_rs + input_fert_rs + input_insect_rs +
      input_equip_hire_rs + input_hired_labour_rs
  ) %>%
  select(hhid, year,
         use_fertiliser = use_fert_flag,
         use_insecticide = use_insect_flag,
         use_hired_equipment = use_equip_flag,
         use_hired_labour = use_labour_flag,
         input_total_12m_rs)


##############################################################################
# (D) CROP CHOICE — from Sections 9b1 (wet) + 9b2 (dry)
##############################################################################
# Each row = HH × year × crop × season. cropid is the crop name (string).
# Staples / cash crops / horticulture categorisation on cropid.
# Market orientation:
#   s09q41e (wet) / s09q50e (dry) = quantity sold
#   s09q42  (wet) / s09q51  (dry) = price per unit → sales_rs = qty × price
#   s09q41a (wet) / s09q50a (dry) = quantity harvested (production)
# crop_sale_share = mean of per-crop (sold / harvested) ratios.

# Crop category mapping on cropid strings
staple_crops <- c("paddy", "rice", "wheat", "maize", "millet", "barley",
                  "beaten.?rice", "flattened.?rice")
cashcrop_crops <- c("sugarcane", "jute", "tobacco", "cardamom", "tea",
                    "cash crop", "oilseed", "mustard", "linseed", "sesame",
                    "ground.?nut")
horti_crops   <- c("vegetable", "fruit", "onion", "garlic", "chilli",
                   "chilie", "tomato", "potato", "ginger", "turmeric",
                   "cauliflower", "cabbage", "cucumber", "orange", "lemon",
                   "mango", "banana", "apple", "guava")

classify_crop <- function(crop_label) {
  s <- tolower(as.character(crop_label))
  dplyr::case_when(
    is.na(s)                                       ~ NA_character_,
    stringr::str_detect(s, paste(staple_crops,   collapse = "|")) ~ "staple",
    stringr::str_detect(s, paste(cashcrop_crops, collapse = "|")) ~ "cashcrop",
    stringr::str_detect(s, paste(horti_crops,    collapse = "|")) ~ "horticulture",
    TRUE                                           ~ "other_crop"
  )
}

# Prepare wet and dry crop rows with harmonised column names
crops_wet <- sec9b1 %>%
  transmute(
    hhid, year, cropid,
    crop_type   = classify_crop(cropid),
    harvest_qty = suppressWarnings(as.numeric(s09q41a)),
    sold_qty    = suppressWarnings(as.numeric(s09q41e)),
    price_unit  = suppressWarnings(as.numeric(s09q42))
  )

crops_dry <- sec9b2 %>%
  transmute(
    hhid, year, cropid,
    crop_type   = classify_crop(cropid),
    harvest_qty = suppressWarnings(as.numeric(s09q50a)),
    sold_qty    = suppressWarnings(as.numeric(s09q50e)),
    price_unit  = suppressWarnings(as.numeric(s09q51))
  )

crops_all <- bind_rows(
  crops_wet %>% mutate(season = "wet"),
  crops_dry %>% mutate(season = "dry")
) %>%
  mutate(
    harvest_qty = coalesce(harvest_qty, 0),
    sold_qty    = coalesce(sold_qty,    0),
    price_unit  = coalesce(price_unit,  0),
    sold_rs_row = sold_qty * price_unit,
    # Per-crop-row share (guard divide-by-zero; 0 harvest = NA share)
    sale_share_row = dplyr::if_else(harvest_qty > 0,
                                    pmin(sold_qty / harvest_qty, 1),
                                    NA_real_)
  )

crops_hh <- crops_all %>%
  group_by(hhid, year) %>%
  summarise(
    n_crop_types       = n_distinct(cropid[!is.na(cropid)]),
    grows_staple       = as.integer(any(crop_type == "staple",       na.rm = TRUE)),
    grows_cashcrop     = as.integer(any(crop_type == "cashcrop",     na.rm = TRUE)),
    grows_horticulture = as.integer(any(crop_type == "horticulture", na.rm = TRUE)),
    any_crop_sold      = as.integer(any(sold_qty > 0, na.rm = TRUE)),
    crop_sale_rs_12m   = sum(sold_rs_row, na.rm = TRUE),
    crop_sale_share    = mean(sale_share_row, na.rm = TRUE),
    .groups = "drop"
  )


##############################################################################
# (E) TECHNOLOGY — from Section 9f
##############################################################################
# equipmentid labels equipment type; s09q65 = number owned.

modern_equip <- c("tractor", "water pump", "tubewell", "borewell",
                  "thresher", "drip", "sprinkler", "generator",
                  "diesel engine")

equip_hh <- sec9f %>%
  mutate(
    n_owned      = coalesce(as.numeric(s09q65), 0),
    is_plough    = match_any(equipmentid, "plough"),
    is_modern    = match_any(equipmentid, modern_equip),
    stock_value  = coalesce(as.numeric(s09q66), 0)
  ) %>%
  group_by(hhid, year) %>%
  summarise(
    owns_plough         = as.integer(any(is_plough & n_owned > 0, na.rm = TRUE)),
    owns_modern_equip   = as.integer(any(is_modern & n_owned > 0, na.rm = TRUE)),
    n_modern_equip_types = sum(is_modern & n_owned > 0, na.rm = TRUE),
    equip_stock_value_rs = sum(stock_value, na.rm = TRUE),
    .groups = "drop"
  )


##############################################################################
# (F) AGRICULTURAL HOUSEHOLD FLAG
##############################################################################
# agro_hh = 1 if HH has any plot (9a1), any crop row (9b1/9b2), or any
# livestock (9d shows livestockid != "None").

livestock_has_hh <- sec9d %>%
  mutate(
    lvst_n = coalesce(as.numeric(s09q57a), 0),
    has_lvst = !match_any(livestockid, "^none") & lvst_n > 0
  ) %>%
  group_by(hhid, year) %>%
  summarise(livestock_has = as.integer(any(has_lvst, na.rm = TRUE)),
            .groups = "drop")

agro_flag <- dplyr::bind_rows(
  sec9a1 %>% distinct(hhid, year) %>% mutate(from_plot = 1),
  sec9b1 %>% distinct(hhid, year) %>% mutate(from_crop = 1),
  livestock_has_hh %>% filter(livestock_has == 1) %>%
    transmute(hhid, year, from_lvst = 1)
) %>%
  group_by(hhid, year) %>%
  summarise(agro_hh = 1L, .groups = "drop")


##############################################################################
# (G) MERGE ALL BLOCKS TO HH × YEAR, BALANCE AGAINST ID_MATCH
##############################################################################

idmap_path <- file.path(base_in, "id_match_long.csv")
if (file.exists(idmap_path)) {
  id_match <- read_csv_q(idmap_path)
} else {
  warning("id_match_long.csv not found at ", idmap_path,
          "; output will only cover HHs that appear in any ag sub-section.")
  id_match <- agro_flag %>% distinct(hhid, year)
}

id_min <- id_match %>%
  select(hhid, year,
         any_of(c("wt_hh", "psu", "district", "vdc",
                  "vmun_code", "lgname", "district77", "district_name",
                  "s00q03a", "s00q03b", "s00q03c")))

agriculture_hh_year <- id_min %>%
  left_join(agro_flag,         by = c("hhid", "year")) %>%
  left_join(land_use_hh,       by = c("hhid", "year")) %>%
  left_join(rented_in_hh,      by = c("hhid", "year")) %>%
  left_join(land_market_hh,    by = c("hhid", "year")) %>%
  left_join(intensity_hh,      by = c("hhid", "year")) %>%
  left_join(crops_hh,          by = c("hhid", "year")) %>%
  left_join(equip_hh,          by = c("hhid", "year")) %>%
  left_join(livestock_has_hh,  by = c("hhid", "year")) %>%
  mutate(
    # Derived: total cultivated area = own-self-cropped + rented-in
    cultivated_area_sqm = coalesce(area_self_cropped_sqm, 0) +
      coalesce(rented_in_area_sqm, 0),
    # Input intensity per sqm (NA when no land cultivated)
    input_intensity_per_sqm = dplyr::if_else(
      cultivated_area_sqm > 0,
      coalesce(input_total_12m_rs, 0) / cultivated_area_sqm,
      NA_real_
    ),
    # Zero-fill key indicators for non-ag HHs
    agro_hh            = coalesce(agro_hh, 0L),
    livestock_has      = coalesce(livestock_has, 0L),
    across(c(owned_plots_n, owned_area_sqm,
             plots_self_cropped_n, plots_rented_out_n, plots_fallow_n,
             plots_abandoned_share, plots_both_seasons_share,
             land_sold_any, land_bought_any,
             land_sold_12m_rs, land_bought_12m_rs,
             use_fertiliser, use_insecticide,
             use_hired_equipment, use_hired_labour,
             input_total_12m_rs,
             n_crop_types, grows_staple, grows_cashcrop, grows_horticulture,
             any_crop_sold, crop_sale_rs_12m,
             owns_plough, owns_modern_equip, n_modern_equip_types,
             equip_stock_value_rs),
           ~ coalesce(., 0))
  )

# Final column order
id_cols <- c("hhid", "year", "wt_hh",
             intersect(c("psu", "district", "vdc",
                         "vmun_code", "lgname", "district77", "district_name",
                         "s00q03a", "s00q03b", "s00q03c"),
                       names(agriculture_hh_year)))

outcome_cols <- c(
  # (A) Land use
  "agro_hh", "owned_plots_n", "cultivated_area_sqm",
  "plots_self_cropped_n", "plots_rented_out_n", "plots_fallow_n",
  "plots_abandoned_share", "plots_both_seasons_share",
  # (B) Land market
  "land_sold_any", "land_bought_any",
  "land_sold_12m_rs", "land_bought_12m_rs",
  # (C) Intensity
  "input_total_12m_rs", "input_intensity_per_sqm",
  "use_fertiliser", "use_insecticide",
  "use_hired_equipment", "use_hired_labour",
  # (D) Crop choice & market
  "n_crop_types", "grows_staple", "grows_cashcrop", "grows_horticulture",
  "any_crop_sold", "crop_sale_rs_12m", "crop_sale_share",
  # (E) Technology
  "owns_plough", "owns_modern_equip", "n_modern_equip_types",
  "equip_stock_value_rs",
  # (F) Livestock
  "livestock_has"
)

agriculture_hh_year <- agriculture_hh_year %>%
  select(all_of(id_cols), all_of(outcome_cols)) %>%
  arrange(hhid, year)


##############################################################################
# (H) SAVE + SANITY REPORT
##############################################################################

write_csv(agriculture_hh_year,
          file.path(base_out, "agriculture_hh_year.csv"), na = "")

# Codebook
codebook <- tribble(
  ~variable,                    ~unit,       ~reference,    ~source,              ~definition,
  "agro_hh",                   "HH × year", "past year",   "9a1/9b/9d presence", "1 if HH has any plot, any crop row, or any livestock > 0; 0 otherwise.",
  "owned_plots_n",             "HH × year", "current",     "count 9a1 rows",     "Number of owned plots reported by HH.",
  "cultivated_area_sqm",       "HH × year", "past year",   "9a1 + 9a2 area_sqm", "Total area cultivated = area of own self-cropped plots + area of rented/shared-in plots.",
  "plots_self_cropped_n",      "HH × year", "past year",   "9a1 s09q07/s09q11",  "Count of owned plots cultivated by HH themselves in wet OR dry season.",
  "plots_rented_out_n",        "HH × year", "past year",   "9a1 s09q07/s09q11",  "Count of owned plots sharecropped OR rented out to others.",
  "plots_fallow_n",            "HH × year", "past year",   "9a1 s09q07/s09q11",  "Count of owned plots reported fallow in wet OR dry season.",
  "plots_abandoned_share",     "HH × year", "past year",   "derived",            "(rented_out + fallow) / owned — disengagement index (0-1).",
  "plots_both_seasons_share",  "HH × year", "past year",   "derived",            "Share of owned plots self-cropped in BOTH wet and dry seasons — intensity index.",
  "land_sold_any",             "HH × year", "12 months",   "9a3 s09q26",         "1 if HH sold or gave away any land in past 12 months.",
  "land_bought_any",           "HH × year", "12 months",   "9a3 s09q29",         "1 if HH bought or received any land in past 12 months.",
  "land_sold_12m_rs",          "HH × year", "12 months",   "9a3 s09q28a",        "Rs. received from land sales in past 12 months.",
  "land_bought_12m_rs",        "HH × year", "12 months",   "9a3 s09q31a",        "Rs. paid for land bought in past 12 months.",
  "input_total_12m_rs",        "HH × year", "12 months",   "9c s09q52/53 b/d/f/h/j", "Total Rs. on agricultural inputs (seed + fertiliser + insecticide + equipment hire + hired labour), wet+dry combined.",
  "input_intensity_per_sqm",   "HH × year", "12 months",   "derived",            "input_total_12m_rs / cultivated_area_sqm. NA when cultivated area = 0.",
  "use_fertiliser",            "HH × year", "past year",   "9c s09q52c/q53c",    "1 if HH paid for fertiliser in either season.",
  "use_insecticide",           "HH × year", "past year",   "9c s09q52e/q53e",    "1 if HH paid for insecticide in either season.",
  "use_hired_equipment",       "HH × year", "past year",   "9c s09q52g/q53g",    "1 if HH paid to hire equipment in either season.",
  "use_hired_labour",          "HH × year", "past year",   "9c s09q52i/q53i",    "1 if HH paid for hired labour in either season.",
  "n_crop_types",              "HH × year", "past year",   "9b1+9b2 cropid",     "Count of distinct crops grown (wet + dry seasons combined).",
  "grows_staple",              "HH × year", "past year",   "9b1+9b2 cropid",     "1 if HH grew any of: paddy/rice, wheat, maize, millet, barley, beaten rice.",
  "grows_cashcrop",            "HH × year", "past year",   "9b1+9b2 cropid",     "1 if HH grew any of: sugarcane, jute, tobacco, cardamom, tea, oilseeds.",
  "grows_horticulture",        "HH × year", "past year",   "9b1+9b2 cropid",     "1 if HH grew any vegetables or fruits.",
  "any_crop_sold",             "HH × year", "past year",   "9b1 s09q41e + 9b2 s09q50e", "1 if HH sold any portion of any crop.",
  "crop_sale_rs_12m",          "HH × year", "past year",   "qty × price",        "Total Rs. from crop sales = Σ (sold_qty × price_per_unit) across crops and seasons.",
  "crop_sale_share",           "HH × year", "past year",   "derived",            "Mean across crops of (sold_qty / harvested_qty). Unit-invariant. NA if no harvest.",
  "owns_plough",               "HH × year", "current",     "9f equipmentid",     "1 if HH owns a plough (n_owned > 0).",
  "owns_modern_equip",         "HH × year", "current",     "9f equipmentid",     "1 if HH owns any of: tractor, water pump, tubewell, thresher, drip/sprinkler, generator.",
  "n_modern_equip_types",      "HH × year", "current",     "9f equipmentid",     "Count of distinct modern equipment types owned.",
  "equip_stock_value_rs",      "HH × year", "current",     "9f s09q66",          "Rs. total value of agricultural equipment owned (label says 'sales' but data pattern indicates stock value).",
  "livestock_has",             "HH × year", "current",     "9d livestockid",     "1 if HH has any livestock (excluding 'None' rows)."
)
write_csv(codebook, file.path(base_out, "agriculture_codebook.csv"))

# Sanity
cat("\n=============================================================\n")
cat("agriculture_hh_year.csv:", nrow(agriculture_hh_year), "rows,",
    ncol(agriculture_hh_year), "cols\n")
cat("Rows per year:",
    paste0(agriculture_hh_year %>% count(year) %>%
             mutate(x = paste0(year, "=", n)) %>% pull(x), collapse = "  "),
    "\n")
cat("HHs with municipality:", sum(!is.na(agriculture_hh_year$vmun_code)),
    "/", nrow(agriculture_hh_year), "\n\n")

cat("---- Agro household share by year ----\n")
agriculture_hh_year %>%
  group_by(year) %>%
  summarise(
    n_hh            = dplyr::n(),
    share_agro      = round(mean(agro_hh),       3),
    share_livestock = round(mean(livestock_has), 3),
    mean_cult_sqm   = round(mean(cultivated_area_sqm, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>% print()

cat("\n---- Selected outcome summaries (among agro HHs) ----\n")
agriculture_hh_year %>%
  filter(agro_hh == 1) %>%
  summarise(
    n                     = dplyr::n(),
    share_fertiliser      = round(mean(use_fertiliser), 3),
    share_insecticide     = round(mean(use_insecticide), 3),
    share_hired_labour    = round(mean(use_hired_labour), 3),
    share_modern_equip    = round(mean(owns_modern_equip), 3),
    share_grows_cashcrop  = round(mean(grows_cashcrop), 3),
    share_grows_horticult = round(mean(grows_horticulture), 3),
    share_any_crop_sold   = round(mean(any_crop_sold), 3),
    mean_n_crop_types     = round(mean(n_crop_types), 2),
    mean_crop_sale_share  = round(mean(crop_sale_share, na.rm = TRUE), 3),
    mean_input_12m_rs     = round(mean(input_total_12m_rs), 0)
  ) %>%
  pivot_longer(everything()) %>% print(n = 30)

cat("\n---- Land market activity ----\n")
agriculture_hh_year %>%
  summarise(
    land_sold_share   = round(mean(land_sold_any),   3),
    land_bought_share = round(mean(land_bought_any), 3),
    mean_sold_rs_cond = round(mean(land_sold_12m_rs[land_sold_any == 1],
                                   na.rm = TRUE), 0),
    mean_bought_rs_cond = round(mean(land_bought_12m_rs[land_bought_any == 1],
                                     na.rm = TRUE), 0)
  ) %>% print()

cat("\n---- Full outcome summary ----\n")
agriculture_hh_year %>%
  select(all_of(outcome_cols)) %>%
  summarise(across(everything(),
                   list(n_nonNA = ~sum(!is.na(.x)),
                        median  = ~median(.x, na.rm = TRUE),
                        mean    = ~mean(.x, na.rm = TRUE)),
                   .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_sep = "__", names_to = c("var", "stat")) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  print(n = 40)

cat("=============================================================\n")