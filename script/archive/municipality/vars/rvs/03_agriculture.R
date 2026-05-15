      ##############################################################################
      # NRVS STAGE 2: AGRICULTURE OUTCOMES — SHORT CORE VERSION
      # Output: HH × year
      ##############################################################################
      
      library(tidyverse)
      library(fs)
      
      base_in  <- "data/raw/RVS Data/clean"
      base_out <- "data/clean/rvs_outcomes"
      dir_create(base_out, recurse = TRUE)
      
      read_csv_q <- function(p) read_csv(p, show_col_types = FALSE, progress = FALSE)
      
      # Paths
      p9a1 <- file.path(base_in, "land/section_9a1.csv")
      p9a2 <- file.path(base_in, "land/section_9a2.csv")
      p9b1 <- file.path(base_in, "crop_production/section_9b1.csv")
      p9b2 <- file.path(base_in, "crop_production/section_9b2.csv")
      p9c  <- file.path(base_in, "ag_inputs/section_9c.csv")
      p9d  <- file.path(base_in, "livestock/section_9d.csv")
      p9f  <- file.path(base_in, "ag_equipment/section_9f.csv")
      
      sec9a1 <- read_csv_q(p9a1)
      sec9a2 <- read_csv_q(p9a2)
      sec9b1 <- read_csv_q(p9b1)
      sec9b2 <- read_csv_q(p9b2)
      sec9c  <- read_csv_q(p9c)
      sec9d  <- read_csv_q(p9d)
      sec9f  <- read_csv_q(p9f)
      
      ##############################################################################
      # Helpers
      ##############################################################################
      
      match_any <- function(x, patterns) {
        s <- str_squish(str_to_lower(as.character(x)))
        s <- coalesce(s, "")
        str_detect(s, paste(patterns, collapse = "|"))
      }
      
      yes_flag <- function(x) {
        as.integer(match_any(x, "^yes"))
      }
      
      safe_num <- function(x) {
        coalesce(suppressWarnings(as.numeric(x)), 0)
      }
      
      ##############################################################################
      # 1. LAND USE: owned plots, wet and dry season
      ##############################################################################
      
      land_plot <- sec9a1 %>%
        mutate(
          area_sqm_clean = safe_num(area_sqm),
          
          wet_farmed = match_any(s09q07, c("self.?crop", "self.?cultivat", "own farm")),
          wet_shared = match_any(s09q07, c("share.?crop")),
          wet_rented = match_any(s09q07, c("rent")) & !wet_shared,
          wet_fallow = match_any(s09q07, c("fallow")),
          
          dry_farmed = match_any(s09q11, c("self.?crop", "self.?cultivat", "own farm")),
          dry_shared = match_any(s09q11, c("share.?crop")),
          dry_rented = match_any(s09q11, c("rent")) & !dry_shared,
          dry_fallow = match_any(s09q11, c("fallow")),
          
          double_cropped = wet_farmed & dry_farmed
        )
      
      land_hh <- land_plot %>%
        group_by(hhid, year) %>%
        summarise(
          owned_plots_n = n(),
          owned_area_sqm = sum(area_sqm_clean, na.rm = TRUE),
          
          plot_wet_farmed_share = mean(wet_farmed, na.rm = TRUE),
          plot_wet_rented_share = mean(wet_rented, na.rm = TRUE),
          plot_wet_shared_share = mean(wet_shared, na.rm = TRUE),
          plot_wet_fallow_share = mean(wet_fallow, na.rm = TRUE),
          
          plot_dry_farmed_share = mean(dry_farmed, na.rm = TRUE),
          plot_dry_rented_share = mean(dry_rented, na.rm = TRUE),
          plot_dry_shared_share = mean(dry_shared, na.rm = TRUE),
          plot_dry_fallow_share = mean(dry_fallow, na.rm = TRUE),
          
          double_crop_share = mean(double_cropped, na.rm = TRUE),
          
          own_cultivated_area_sqm = sum(area_sqm_clean * (wet_farmed | dry_farmed), na.rm = TRUE),
          
          .groups = "drop"
        )
      
      ##############################################################################
      # 2. RENTED / SHARECROPPED-IN LAND
      ##############################################################################
      
      rented_in_hh <- sec9a2 %>%
        mutate(rented_in_area_sqm = safe_num(area_sqm)) %>%
        group_by(hhid, year) %>%
        summarise(
          rented_in_area_sqm = sum(rented_in_area_sqm, na.rm = TRUE),
          rented_in_any = as.integer(rented_in_area_sqm > 0),
          .groups = "drop"
        )
      
      ##############################################################################
      # 3. INPUT USE: wet and dry season
      ##############################################################################
      
      inputs_hh <- sec9c %>%
        transmute(
          hhid, year,
          
          wet_use_seed         = yes_flag(s09q52a),
          wet_use_fertiliser   = yes_flag(s09q52c),
          wet_use_pesticide    = yes_flag(s09q52e),
          wet_use_equipment    = yes_flag(s09q52g),
          wet_use_hired_labour = yes_flag(s09q52i),
          
          dry_use_seed         = yes_flag(s09q53a),
          dry_use_fertiliser   = yes_flag(s09q53c),
          dry_use_pesticide    = yes_flag(s09q53e),
          dry_use_equipment    = yes_flag(s09q53g),
          dry_use_hired_labour = yes_flag(s09q53i),
          
          input_total_12m_rs =
            safe_num(s09q52b) + safe_num(s09q52d) + safe_num(s09q52f) +
            safe_num(s09q52h) + safe_num(s09q52j) +
            safe_num(s09q53b) + safe_num(s09q53d) + safe_num(s09q53f) +
            safe_num(s09q53h) + safe_num(s09q53j)
        ) %>%
        group_by(hhid, year) %>%
        summarise(
          across(starts_with("wet_use_"), ~ as.integer(any(.x == 1, na.rm = TRUE))),
          across(starts_with("dry_use_"), ~ as.integer(any(.x == 1, na.rm = TRUE))),
          input_total_12m_rs = sum(input_total_12m_rs, na.rm = TRUE),
          .groups = "drop"
        )
      
      ##############################################################################
      # 4. CROP CHOICE AND COMMERCIALISATION
      ##############################################################################
      
      staple_crops <- c(
        "paddy", "rice", "wheat", "maize", "millet", "barley"
      )
      
      cashcrop_crops <- c(
        "sugarcane", "jute", "tobacco", "cardamom", "tea",
        "oilseed", "mustard", "linseed", "sesame", "ground.?nut"
      )
      
      horti_crops <- c(
        "vegetable", "fruit", "onion", "garlic", "chilli", "chilie",
        "tomato", "potato", "ginger", "turmeric", "cauliflower",
        "cabbage", "cucumber", "orange", "lemon", "mango", "banana",
        "apple", "guava"
      )
      
      classify_crop <- function(crop_label) {
        s <- str_squish(str_to_lower(as.character(crop_label)))
        
        case_when(
          is.na(s) | s == "" ~ NA_character_,
          str_detect(s, paste(staple_crops, collapse = "|")) ~ "staple",
          str_detect(s, paste(cashcrop_crops, collapse = "|")) ~ "cashcrop",
          str_detect(s, paste(horti_crops, collapse = "|")) ~ "horticulture",
          TRUE ~ "other_crop"
        )
      }
      
      crops_wet <- sec9b1 %>%
        transmute(
          hhid, year, cropid,
          season = "wet",
          crop_type = classify_crop(cropid),
          harvest_qty = safe_num(s09q41a),
          sold_qty = safe_num(s09q41e),
          price_unit = safe_num(s09q42)
        )
      
      crops_dry <- sec9b2 %>%
        transmute(
          hhid, year, cropid,
          season = "dry",
          crop_type = classify_crop(cropid),
          harvest_qty = safe_num(s09q50a),
          sold_qty = safe_num(s09q50e),
          price_unit = safe_num(s09q51)
        )
      
      crops_all <- bind_rows(crops_wet, crops_dry) %>%
        mutate(
          crop_sales_rs_row = sold_qty * price_unit,
          crop_sale_share_row = if_else(
            harvest_qty > 0,
            pmin(sold_qty / harvest_qty, 1),
            NA_real_
          )
        )
      
      crops_hh <- crops_all %>%
        group_by(hhid, year) %>%
        summarise(
          n_crop_types = n_distinct(cropid[!is.na(cropid)]),
          
          grows_staple = as.integer(any(crop_type == "staple", na.rm = TRUE)),
          grows_cashcrop = as.integer(any(crop_type == "cashcrop", na.rm = TRUE)),
          grows_horticulture = as.integer(any(crop_type == "horticulture", na.rm = TRUE)),
          
          wet_grows_staple = as.integer(any(season == "wet" & crop_type == "staple", na.rm = TRUE)),
          dry_grows_staple = as.integer(any(season == "dry" & crop_type == "staple", na.rm = TRUE)),
          
          crop_sold_any = as.integer(any(sold_qty > 0, na.rm = TRUE)),
          crop_sales_12m_rs = sum(crop_sales_rs_row, na.rm = TRUE),
          crop_sale_share = mean(crop_sale_share_row, na.rm = TRUE),
          
          .groups = "drop"
        )
      
      ##############################################################################
      # 5. AGRICULTURAL TECHNOLOGY / EQUIPMENT
      ##############################################################################
      
      modern_equip <- c(
        "tractor", "power tiller", "water pump", "pump", "tubewell",
        "borewell", "thresher", "drip", "sprinkler", "generator",
        "diesel engine", "harvester"
      )
      
      equip_hh <- sec9f %>%
        mutate(
          n_owned = safe_num(s09q65),
          stock_value = safe_num(s09q66),
          
          owns_tractor_row = match_any(equipmentid, c("tractor", "power tiller")),
          owns_pump_row = match_any(equipmentid, c("water pump", "pump", "tubewell", "borewell")),
          owns_modern_row = match_any(equipmentid, modern_equip)
        ) %>%
        group_by(hhid, year) %>%
        summarise(
          owns_tractor = as.integer(any(owns_tractor_row & n_owned > 0, na.rm = TRUE)),
          owns_pump = as.integer(any(owns_pump_row & n_owned > 0, na.rm = TRUE)),
          owns_modern_equip = as.integer(any(owns_modern_row & n_owned > 0, na.rm = TRUE)),
          n_modern_equip_types = sum(owns_modern_row & n_owned > 0, na.rm = TRUE),
          ag_equip_stock_value_rs = sum(stock_value, na.rm = TRUE),
          .groups = "drop"
        )
      
      ##############################################################################
      # 6. LIVESTOCK FLAG
      ##############################################################################
      
      livestock_hh <- sec9d %>%
        mutate(
          livestock_n = safe_num(s09q57a),
          has_livestock_row = !match_any(livestockid, "^none") & livestock_n > 0
        ) %>%
        group_by(hhid, year) %>%
        summarise(
          livestock_has = as.integer(any(has_livestock_row, na.rm = TRUE)),
          .groups = "drop"
        )
      
      ##############################################################################
      # 7. AGRICULTURE HOUSEHOLD FLAG
      ##############################################################################
      
      agri_flag <- bind_rows(
        sec9a1 %>% distinct(hhid, year) %>% mutate(flag = 1L),
        sec9a2 %>% distinct(hhid, year) %>% mutate(flag = 1L),
        sec9b1 %>% distinct(hhid, year) %>% mutate(flag = 1L),
        sec9b2 %>% distinct(hhid, year) %>% mutate(flag = 1L),
        livestock_hh %>% filter(livestock_has == 1) %>% transmute(hhid, year, flag = 1L)
      ) %>%
        group_by(hhid, year) %>%
        summarise(agri_hh = 1L, .groups = "drop")
      
      ##############################################################################
      # 8. BALANCE TO FULL HH × YEAR UNIVERSE
      ##############################################################################
      
      idmap_path <- file.path(base_in, "id_match_long.csv")
      
      if (file.exists(idmap_path)) {
        id_match <- read_csv_q(idmap_path)
      } else {
        warning("id_match_long.csv not found; output covers HHs in agriculture modules only.")
        id_match <- agri_flag %>% distinct(hhid, year)
      }
      
      id_min <- id_match %>%
        distinct(hhid, year, .keep_all = TRUE) %>%
        select(
          hhid, year,
          any_of(c(
            "wt_hh", "psu", "district", "vdc",
            "vmun_code", "lgname", "district77", "district_name",
            "s00q03a", "s00q03b", "s00q03c"
          ))
        )
      
      agriculture_hh_year <- id_min %>%
        left_join(agri_flag,    by = c("hhid", "year")) %>%
        left_join(land_hh,      by = c("hhid", "year")) %>%
        left_join(rented_in_hh, by = c("hhid", "year")) %>%
        left_join(inputs_hh,    by = c("hhid", "year")) %>%
        left_join(crops_hh,     by = c("hhid", "year")) %>%
        left_join(equip_hh,     by = c("hhid", "year")) %>%
        left_join(livestock_hh, by = c("hhid", "year")) %>%
        mutate(
          agri_hh = coalesce(agri_hh, 0L),
          livestock_has = coalesce(livestock_has, 0L),
          
          # Unconditional/extensive variables: zero is meaningful
          across(
            any_of(c(
              "owned_plots_n",
              "owned_area_sqm",
              "own_cultivated_area_sqm",
              "rented_in_area_sqm",
              "rented_in_any",
              "input_total_12m_rs",
              "n_crop_types",
              "grows_staple",
              "grows_cashcrop",
              "grows_horticulture",
              "wet_grows_staple",
              "dry_grows_staple",
              "crop_sold_any",
              "crop_sales_12m_rs",
              "owns_tractor",
              "owns_pump",
              "owns_modern_equip",
              "n_modern_equip_types",
              "ag_equip_stock_value_rs"
            )),
            ~ coalesce(.x, 0)
          ),
          
          cultivated_area_sqm = own_cultivated_area_sqm + rented_in_area_sqm,
          
          # Conditional variables: keep NA for non-ag / no-land HHs
          input_intensity_per_sqm = if_else(
            cultivated_area_sqm > 0,
            input_total_12m_rs / cultivated_area_sqm,
            NA_real_
          ),
          
          across(
            any_of(c(
              "plot_wet_farmed_share",
              "plot_wet_rented_share",
              "plot_wet_shared_share",
              "plot_wet_fallow_share",
              "plot_dry_farmed_share",
              "plot_dry_rented_share",
              "plot_dry_shared_share",
              "plot_dry_fallow_share",
              "double_crop_share",
              "crop_sale_share"
            )),
            ~ if_else(agri_hh == 1, .x, NA_real_)
          )
        )
      
      ##############################################################################
      # 9. FINAL COLUMN ORDER
      ##############################################################################
      
      id_cols <- c(
        "hhid", "year", "wt_hh",
        intersect(
          c("psu", "district", "vdc",
            "vmun_code", "lgname", "district77", "district_name",
            "s00q03a", "s00q03b", "s00q03c"),
          names(agriculture_hh_year)
        )
      )
      
      outcome_cols <- c(
        "agri_hh",
        
        "owned_plots_n",
        "owned_area_sqm",
        "cultivated_area_sqm",
        "rented_in_area_sqm",
        "rented_in_any",
        
        "plot_wet_farmed_share",
        "plot_wet_rented_share",
        "plot_wet_shared_share",
        "plot_wet_fallow_share",
        
        "plot_dry_farmed_share",
        "plot_dry_rented_share",
        "plot_dry_shared_share",
        "plot_dry_fallow_share",
        
        "double_crop_share",
        
        "wet_use_seed",
        "wet_use_fertiliser",
        "wet_use_pesticide",
        "wet_use_equipment",
        "wet_use_hired_labour",
        
        "dry_use_seed",
        "dry_use_fertiliser",
        "dry_use_pesticide",
        "dry_use_equipment",
        "dry_use_hired_labour",
        
        "input_total_12m_rs",
        "input_intensity_per_sqm",
        
        "n_crop_types",
        "grows_staple",
        "grows_cashcrop",
        "grows_horticulture",
        "wet_grows_staple",
        "dry_grows_staple",
        
        "crop_sold_any",
        "crop_sales_12m_rs",
        "crop_sale_share",
        
        "owns_tractor",
        "owns_pump",
        "owns_modern_equip",
        "n_modern_equip_types",
        "ag_equip_stock_value_rs",
        
        "livestock_has"
      )
      
      agriculture_hh_year <- agriculture_hh_year %>%
        select(any_of(id_cols), any_of(outcome_cols)) %>%
        arrange(hhid, year)
      
      ##############################################################################
      # 10. SAVE
      ##############################################################################
      
      write_csv(
        agriculture_hh_year,
        file.path(base_out, "agriculture_hh_year.csv"),
        na = ""
      )
      
      ##############################################################################
      # 11. CODEBOOK
      ##############################################################################
      
      agriculture_codebook <- tribble(
        ~variable, ~unit, ~reference, ~source, ~definition,
        
        "agri_hh", "HH × year", "past year", "9a/9b/9d presence",
        "1 if household appears in land, crop, or livestock agriculture modules; 0 otherwise.",
        
        "owned_plots_n", "HH × year", "current", "9a1",
        "Number of owned plots reported by household.",
        
        "owned_area_sqm", "HH × year", "current", "9a1 area_sqm",
        "Total area of owned plots in square metres.",
        
        "cultivated_area_sqm", "HH × year", "past year", "9a1 + 9a2",
        "Total cultivated area: own self-cultivated area plus rented/sharecropped-in area.",
        
        "rented_in_area_sqm", "HH × year", "past year", "9a2 area_sqm",
        "Total area rented or sharecropped in by household.",
        
        "rented_in_any", "HH × year", "past year", "9a2",
        "1 if household rented or sharecropped in any agricultural land.",
        
        "plot_wet_farmed_share", "HH × year", "wet season", "9a1 s09q07",
        "Share of owned plots self-cultivated by household in the wet season.",
        
        "plot_wet_rented_share", "HH × year", "wet season", "9a1 s09q07",
        "Share of owned plots rented out in the wet season.",
        
        "plot_wet_shared_share", "HH × year", "wet season", "9a1 s09q07",
        "Share of owned plots sharecropped out in the wet season.",
        
        "plot_wet_fallow_share", "HH × year", "wet season", "9a1 s09q07",
        "Share of owned plots kept fallow in the wet season.",
        
        "plot_dry_farmed_share", "HH × year", "dry season", "9a1 s09q11",
        "Share of owned plots self-cultivated by household in the dry season.",
        
        "plot_dry_rented_share", "HH × year", "dry season", "9a1 s09q11",
        "Share of owned plots rented out in the dry season.",
        
        "plot_dry_shared_share", "HH × year", "dry season", "9a1 s09q11",
        "Share of owned plots sharecropped out in the dry season.",
        
        "plot_dry_fallow_share", "HH × year", "dry season", "9a1 s09q11",
        "Share of owned plots kept fallow in the dry season.",
        
        "double_crop_share", "HH × year", "past year", "9a1 s09q07/s09q11",
        "Share of owned plots self-cultivated in both wet and dry seasons.",
        
        "wet_use_seed", "HH × year", "wet season", "9c s09q52a",
        "1 if household used purchased seed in the wet season.",
        
        "wet_use_fertiliser", "HH × year", "wet season", "9c s09q52c",
        "1 if household used fertiliser in the wet season.",
        
        "wet_use_pesticide", "HH × year", "wet season", "9c s09q52e",
        "1 if household used pesticide/insecticide in the wet season.",
        
        "wet_use_equipment", "HH × year", "wet season", "9c s09q52g",
        "1 if household hired agricultural equipment in the wet season.",
        
        "wet_use_hired_labour", "HH × year", "wet season", "9c s09q52i",
        "1 if household hired agricultural labour in the wet season.",
        
        "dry_use_seed", "HH × year", "dry season", "9c s09q53a",
        "1 if household used purchased seed in the dry season.",
        
        "dry_use_fertiliser", "HH × year", "dry season", "9c s09q53c",
        "1 if household used fertiliser in the dry season.",
        
        "dry_use_pesticide", "HH × year", "dry season", "9c s09q53e",
        "1 if household used pesticide/insecticide in the dry season.",
        
        "dry_use_equipment", "HH × year", "dry season", "9c s09q53g",
        "1 if household hired agricultural equipment in the dry season.",
        
        "dry_use_hired_labour", "HH × year", "dry season", "9c s09q53i",
        "1 if household hired agricultural labour in the dry season.",
        
        "input_total_12m_rs", "HH × year", "12 months", "9c s09q52/53",
        "Total agricultural input spending across wet and dry seasons.",
        
        "input_intensity_per_sqm", "HH × year", "12 months", "derived",
        "Agricultural input spending per square metre of cultivated area. NA if cultivated area is zero.",
        
        "n_crop_types", "HH × year", "past year", "9b1/9b2 cropid",
        "Number of distinct crops grown across wet and dry seasons.",
        
        "grows_staple", "HH × year", "past year", "9b1/9b2 cropid",
        "1 if household grew any staple crop such as rice/paddy, wheat, maize, millet, or barley.",
        
        "grows_cashcrop", "HH × year", "past year", "9b1/9b2 cropid",
        "1 if household grew any cash crop such as sugarcane, jute, tobacco, cardamom, tea, mustard, or oilseed.",
        
        "grows_horticulture", "HH × year", "past year", "9b1/9b2 cropid",
        "1 if household grew any fruits or vegetables.",
        
        "wet_grows_staple", "HH × year", "wet season", "9b1 cropid",
        "1 if household grew any staple crop in the wet season.",
        
        "dry_grows_staple", "HH × year", "dry season", "9b2 cropid",
        "1 if household grew any staple crop in the dry season.",
        
        "crop_sold_any", "HH × year", "past year", "9b1/9b2 sold quantity",
        "1 if household sold any crop output in either season.",
        
        "crop_sales_12m_rs", "HH × year", "past year", "9b1/9b2 sold quantity × price",
        "Total crop sales value across wet and dry seasons.",
        
        "crop_sale_share", "HH × year", "past year", "derived",
        "Mean crop-level share of harvest sold. NA if no crop harvest quantity is observed.",
        
        "owns_tractor", "HH × year", "current", "9f equipmentid/s09q65",
        "1 if household owns a tractor or power tiller.",
        
        "owns_pump", "HH × year", "current", "9f equipmentid/s09q65",
        "1 if household owns a water pump, tubewell, or borewell equipment.",
        
        "owns_modern_equip", "HH × year", "current", "9f equipmentid/s09q65",
        "1 if household owns any modern agricultural equipment.",
        
        "n_modern_equip_types", "HH × year", "current", "9f equipmentid/s09q65",
        "Number of modern agricultural equipment types owned.",
        
        "ag_equip_stock_value_rs", "HH × year", "current", "9f s09q66",
        "Total value of agricultural equipment owned.",
        
        "livestock_has", "HH × year", "current", "9d livestockid/s09q57a",
        "1 if household owns any livestock."
      )
      
      write_csv(
        agriculture_codebook,
        file.path(base_out, "agriculture_codebook.csv")
      )
      
      ##############################################################################
      # 12. CHECKS
      ##############################################################################
      
      cat("\n=============================================================\n")
      cat("agriculture_hh_year.csv:", nrow(agriculture_hh_year), "rows,",
          ncol(agriculture_hh_year), "cols\n")
      
      cat("Rows per year:",
          paste0(
            agriculture_hh_year %>%
              count(year) %>%
              mutate(x = paste0(year, "=", n)) %>%
              pull(x),
            collapse = "  "
          ),
          "\n"
      )
      
      cat("HHs with municipality:", sum(!is.na(agriculture_hh_year$vmun_code)),
          "/", nrow(agriculture_hh_year), "\n\n")
      
      cat("---- Agriculture participation by year ----\n")
      agriculture_hh_year %>%
        group_by(year) %>%
        summarise(
          n_hh = n(),
          share_agri_hh = round(mean(agri_hh, na.rm = TRUE), 3),
          share_livestock = round(mean(livestock_has, na.rm = TRUE), 3),
          mean_owned_plots = round(mean(owned_plots_n, na.rm = TRUE), 2),
          mean_cultivated_area_sqm = round(mean(cultivated_area_sqm, na.rm = TRUE), 0),
          .groups = "drop"
        ) %>%
        print()
      
      cat("\n---- Land use shares among agriculture HHs ----\n")
      agriculture_hh_year %>%
        filter(agri_hh == 1) %>%
        group_by(year) %>%
        summarise(
          wet_farmed = round(mean(plot_wet_farmed_share, na.rm = TRUE), 3),
          wet_rented = round(mean(plot_wet_rented_share, na.rm = TRUE), 3),
          wet_shared = round(mean(plot_wet_shared_share, na.rm = TRUE), 3),
          wet_fallow = round(mean(plot_wet_fallow_share, na.rm = TRUE), 3),
          dry_farmed = round(mean(plot_dry_farmed_share, na.rm = TRUE), 3),
          dry_rented = round(mean(plot_dry_rented_share, na.rm = TRUE), 3),
          dry_shared = round(mean(plot_dry_shared_share, na.rm = TRUE), 3),
          dry_fallow = round(mean(plot_dry_fallow_share, na.rm = TRUE), 3),
          double_crop = round(mean(double_crop_share, na.rm = TRUE), 3),
          .groups = "drop"
        ) %>%
        print(width = Inf)
      
      cat("\n---- Input and technology use among agriculture HHs ----\n")
      agriculture_hh_year %>%
        filter(agri_hh == 1) %>%
        group_by(year) %>%
        summarise(
          wet_fert = round(mean(wet_use_fertiliser, na.rm = TRUE), 3),
          dry_fert = round(mean(dry_use_fertiliser, na.rm = TRUE), 3),
          wet_labour = round(mean(wet_use_hired_labour, na.rm = TRUE), 3),
          dry_labour = round(mean(dry_use_hired_labour, na.rm = TRUE), 3),
          owns_tractor = round(mean(owns_tractor, na.rm = TRUE), 3),
          owns_pump = round(mean(owns_pump, na.rm = TRUE), 3),
          owns_modern_equip = round(mean(owns_modern_equip, na.rm = TRUE), 3),
          mean_input_rs = round(mean(input_total_12m_rs, na.rm = TRUE), 0),
          .groups = "drop"
        ) %>%
        print(width = Inf)
      
      cat("\n---- Crop orientation and commercialisation among agriculture HHs ----\n")
      agriculture_hh_year %>%
        filter(agri_hh == 1) %>%
        group_by(year) %>%
        summarise(
          grows_staple = round(mean(grows_staple, na.rm = TRUE), 3),
          grows_cashcrop = round(mean(grows_cashcrop, na.rm = TRUE), 3),
          grows_horticulture = round(mean(grows_horticulture, na.rm = TRUE), 3),
          crop_sold_any = round(mean(crop_sold_any, na.rm = TRUE), 3),
          mean_crop_sales_rs = round(mean(crop_sales_12m_rs, na.rm = TRUE), 0),
          mean_crop_sale_share = round(mean(crop_sale_share, na.rm = TRUE), 3),
          mean_n_crop_types = round(mean(n_crop_types, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        print(width = Inf)
      
      cat("\n---- Outcome summary ----\n")
      agriculture_hh_year %>%
        select(any_of(outcome_cols)) %>%
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