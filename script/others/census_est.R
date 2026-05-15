      rm(list = ls())
      cat("\14")
      # setwd("C:/Users/s222385015/OneDrive - Deakin University/PHD/Third Chapter/Analysis")
      
      options(scipen = 999)
      
      library(fixest)
      library(tidyverse)
      library(broom)
      
      # =========================================================
      # 0. LOAD DATA + BUILD PANEL
      # =========================================================
      
      instrument      <- read.csv("data/clean/instrument/instrument_mun.csv")
      census_outcome  <- read.csv("data/clean/census/census_outcomes_municipality.csv")
      source("script/estimation_display.R")
      
      census_panel <- census_outcome %>%
        left_join(instrument, by = c("lgcode", "year")) %>%
        left_join(
          instrument %>% mutate(year = year + 1) %>% 
            rename_with(~ paste0(., "_lag1"), -c(lgcode, year)),
          by = c("lgcode", "year")
        ) %>%
        left_join(
          instrument %>% mutate(year = year + 2) %>% 
            rename_with(~ paste0(., "_lag2"), -c(lgcode, year)),
          by = c("lgcode", "year")
        ) %>%
        mutate(
          across(
            c(starts_with("ssiv_"), starts_with("shareshock_"), 
              starts_with("absexp_"), starts_with("check_"), starts_with("diff_"),
              geog_total_mig_2001, geog_intensity_2001),
            ~ coalesce(., 0)
          )
        )
      
      panel_df <- census_panel %>%
        mutate(
          lgcode = as.integer(lgcode),
          year   = as.integer(year)
        ) %>%
        mutate(log_mi = asinh(geog_intensity_2001)) %>%
        arrange(lgcode, year) %>%
        mutate(fxshock = as.numeric(scale(ssiv_index_2001))) %>% 
        filter(year != 2001)
        
      
      panel_df %>%
        filter(year != 2001) %>%
        summarise(
          n_distinct_lg = n_distinct(lgcode),
          n_fx_na       = sum(is.na(fxshock)),
          n_logmi_na    = sum(is.na(log_mi)),
          n_logmi_zero  = sum(log_mi == 0, na.rm = TRUE),
          n_ssiv_zero   = sum(ssiv_index_2001 == 0, na.rm = TRUE)
        )
      
      m1 <- feols(amen_water_piped ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      nobs(m1)
      
      
      # =========================================================
      # 3. ESTIMATION PANEL: 2011 + 2021
      # =========================================================
      
      
      run_ssiv_regressions(outcomes = unname(amenities), data = panel_df)
      run_ssiv_regressions(outcomes = unname(assets), data = panel_df)
      run_ssiv_regressions(outcomes = unname(housing), data = panel_df)
      
      run_ssiv_regressions(outcomes = unname(labor), data = panel_df)
      run_ssiv_regressions(outcomes = unname(occupation), data = panel_df)
      run_ssiv_regressions(outcomes = unname(industry), data = panel_df)
      run_ssiv_regressions(outcomes = unname(enterprise), data = panel_df)
      run_ssiv_regressions(outcomes = unname(work), data = panel_df)
      
      run_ssiv_regressions(outcomes = unname(migration), data = panel_df)
      run_ssiv_regressions(outcomes = unname(education), data = panel_df)
      
      run_ssiv_regressions(outcomes = unname(demography), data = panel_df)
      run_ssiv_regressions(outcomes = unname(mortality), data = panel_df)
      run_ssiv_regressions(outcomes = unname(gender), data = panel_df)
      run_ssiv_regressions(outcomes = unname(household), data = panel_df)
      
      #--------------------------------------------------
      # Industry
      #--------------------------------------------------
      industry <- c(
        "Agriculture, forestry & fishing"   = "ind_agri_forestry_fish",
        "Manufacturing"                    = "ind_manufacturing",
        "Construction"                     = "ind_construction",
        "Wholesale & retail trade"         = "ind_wholesale_retail",
        "Transport & accommodation"        = "ind_transport_accommodation",
        "Finance, real estate & professional" = "ind_finance_real_estate_prof",
        "Public admin & defence"           = "ind_public_admin_defence",
        "Education"                        = "ind_education",
        "Health"                           = "ind_health",
        "Arts & recreation"                = "ind_arts_recreation",
        "Other industries"                 = "ind_others"
      )
      
      #---------------------------------------------------------------
      
      m1 <- feols(ind_agri_forestry_fish ~ fxshock * mig_in_share+ i(year, log_mi, ref = 2011) | 
                  lgcode + year, data = panel_df)
      m2 <- feols(ind_manufacturing ~ fxshock * mig_in_share + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m3 <- feols(ind_construction ~ fxshock * mig_in_share + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m4 <- feols(ind_wholesale_retail ~ fxshock * mig_in_share + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m5 <- feols(ind_transport_accommodation ~ fxshock * mig_in_share + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m6 <- feols(ind_public_admin_defence ~ fxshock * mig_in_share + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m7 <- feols(ind_education ~ fxshock * mig_in_share + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      
      etable(m1, m2, m3, m4, m5, m6, m7, cluster = ~lgcode)
      
      m1 <- feols(ind_agri_forestry_fish ~ fxshock * mig_in_from_rural+ i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m2 <- feols(ind_manufacturing ~ fxshock * mig_in_from_rural + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m3 <- feols(ind_construction ~ fxshock * mig_in_from_rural + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m4 <- feols(ind_wholesale_retail ~ fxshock * mig_in_from_rural + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m5 <- feols(ind_transport_accommodation ~ fxshock * mig_in_from_rural + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m6 <- feols(ind_public_admin_defence ~ fxshock * mig_in_from_rural + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m7 <- feols(ind_education ~ fxshock * mig_in_from_rural + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      
      etable(m1, m2, m3, m4, m5, m6, m7, cluster = ~lgcode)
      
      m1 <- feols(housing_own ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m2 <- feols(housing_rented ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m3 <- feols(housing_foundation_modern ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m4 <- feols(housing_foundation_traditional ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m5 <- feols(housing_roof_modern ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m6 <- feols(housing_roof_traditional ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      
      etable(m1, m2, m3, m4, m5, m6, cluster = ~lgcode)
      
      m1 <- feols(mig_in_share ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m2 <- feols(mig_in_domestic ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m3 <- feols(mig_in_international ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)

      m4 <- feols(mig_in_from_rural ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m5 <- feols(mig_in_from_urban ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m6 <- feols(mig_in_reason_economic ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m7 <- feols(mig_in_reason_noneconomic ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m8 <- feols(mig_in_reason_study ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m9 <- feols(mig_in_reason_marriage ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      
      etable(m1, m2, m3, m4, m5, m6, m7, m8, m9, cluster = ~lgcode)
      
      
      m1 <- feols(ent_has_nonagro ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m2 <- feols(ent_cottage ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m3 <- feols(ent_trade ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m4 <- feols(housing_foundation_traditional ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m5 <- feols(housing_roof_modern ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m6 <- feols(housing_roof_traditional ~ fxshock + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      
      etable(m1, m2, m3, m4, m5, m6, cluster = ~lgcode)
      
      
      panel_df %>% 
        select(contains("ent")) %>% 
        names()
      
      #======================================
      

      
     run_ssiv_regressions(outcomes = unname(occupation), data = panel_df)
     run_ssiv_regressions(outcomes = unname(amenities), data = panel_df)
     run_ssiv_regressions(outcomes = unname(labor), data = panel_df)
     run_ssiv_regressions(outcomes = unname(assets), data = panel_df)
     run_ssiv_regressions(outcomes = unname(education), data = panel_df)
     run_ssiv_regressions(outcomes = unname(industry), data = panel_df)
     
      
      m1 <- feols(amen_water_piped ~ ssiv_log_yoy + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m2 <- feols(amen_water_traditional ~ ssiv_log_yoy + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m3 <- feols(amen_cooking_wood ~ ssiv_log_yoy + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m4 <- feols(amen_cooking_kerosene ~ ssiv_log_yoy + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m5 <- feols(amen_cooking_lpg ~ ssiv_log_yoy + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m6 <- feols(amen_cooking_traditional ~ ssiv_log_yoy + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      m7 <- feols(amen_lighting_electricity ~ ssiv_log_yoy + i(year, log_mi, ref = 2011) | 
                    lgcode + year, data = panel_df)
      
      etable(m1, m2, m3, m4, m5, m6, m7, cluster = ~lgcode)
      
      # Default: per-capita FX exposure level
      res <- estimate_outcomes("children_left", spec = c("main", "baseline"))
      
      # Same outcomes, but using the growth version
      res_g <- estimate_outcomes("industry",
                                 spec       = c("main", "baseline, std"),
                                 instrument = "pcexposure_fx_index_2001")
      
      # Absolute exposure growth, standardized treatment
      res_abs <- estimate_outcomes("industry",
                                   spec       = "std",
                                   instrument = "pcexposure_fx_index_2001")
      
      # Compare level vs growth side by side by eye
      res_lvl <- estimate_outcomes("fert_mort",
                                   instrument = "pcexposure_fx_index_2001")
      res_gr  <- estimate_outcomes("fert_mort",
                                   instrument = "pcexposure_fx_growth_since_2001")
      
      # Save if desired
      # write.csv(res_assets$publish_table, "results_assets.csv", row.names = FALSE)