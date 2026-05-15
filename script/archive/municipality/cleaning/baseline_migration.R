      
      # Baseline migration 

      # Environment setup
      rm(list = ls()); cat("\14") 
      #setwd("C:/Users/s222385015/OneDrive - Deakin University/PHD/Third Chapter/Analysis")
      
      # Load required packages
      library(tidyverse)    # Data manipulation and visualization
      library(fixest)       # Fast fixed effects estimation
      library(haven)        # Import Stata files
      library(readxl)       # Import Excel files
      library(janitor)      # Data cleaning utilities
      library(countrycode)  # Country name standardization
      
      
      #####################################################################################################
      # SECTION 1: DATA LOADING AND PREPARATION
      #####################################################################################################
      
      #---------------------------------------------------------------------------------------------------
      # 1.1 LOAD RAW MIGRATION DATA (2001 CENSUS)
      #---------------------------------------------------------------------------------------------------
      
      # 2001 absentee (international migration) data
      mig_2001_raw <- haven::read_dta(
        "data/raw/Full Census Data/Census 2001/fullmi01_full_absentee.dta"
      ) %>%
        mutate(ddvvv = as.integer(batchid) %/% 100)
      
      # 2011 census ID crosswalk
      census_2011_id <- readxl::read_xlsx(
        "data/raw/Full Census Data/Census 2011/censusid2011.xlsx"
      ) %>%
        janitor::clean_names()
      
      # Old VDC → new local level mapping
      vdc_to_lg_map <- readxl::read_xlsx(
        "data/raw/old vdc to local level.xlsx"
      ) %>%
        rename(dcode = dist)
      
      # Attach mapping to 2011 census IDs
      census_2011_mapped <- census_2011_id %>%
        left_join(
          vdc_to_lg_map,
          by = c("dname" = "dist_name", "vname" = "vname")
        ) %>%
        select(-dist)
      

      # Final merged dataset (2001 migration + IDs)
      mig_2001_final <- mig_2001_raw %>%
        left_join(
          census_2011_mapped,
          by = c("ddvvv" = "ddvvvcbs11", "vdcmun" = "vdcmun", "ward" = "ward")
        )
     
    
      
      #####################################################################################################
      # SECTION 2: REFERENCE DATA CREATION
      #####################################################################################################
      
      
      
      #---------------------------------------------------------------------------------------------------
      # 2.2 DESTINATION COUNTRY MAPPING
      #---------------------------------------------------------------------------------------------------
      
      # Create destination country lookup (based on 2001 census categories)
      country_names <- c(
        # South Asian neighbors
        "1" = "India", "2" = "Pakistan", "3" = "Bangladesh", "4" = "Bhutan", 
        "5" = "Sri Lanka", "6" = "Maldives",
        
        # East Asian countries
        "7" = "China", "8" = "Korea", "9" = "Russia and Former States of USSR", 
        "10" = "Japan", "11" = "Hong Kong", "12" = "Singapore", "13" = "Malaysia",
        
        # Other destinations
        "14" = "Australia", 
        
        # Gulf countries (major destination for Nepali workers)
        "15" = "Saudi Arabia", "16" = "Qatar", "17" = "Kuwait", 
        "18" = "United Arab Emirates", "19" = "Bahrain",
        
        # Western countries
        "21" = "United Kingdom", "22" = "Germany",
        "23" = "France", "24" = "Other European Countries", 
        "25" = "America, Canada and Mexico", 
        
        # Regional aggregates
        "20" = "Other Asian Countries",  "96" = "Other countries"
      )
      
      # Apply name mappings to migration data
      mig_2001_final <- mig_2001_final %>%
        mutate(
          q12_cnty = country_names[as.character(q12_cnty)]
        )
      
      #####################################################################################################
      # SECTION 3: MIGRATION FLOW PROCESSING
      #####################################################################################################
      
      #---------------------------------------------------------------------------------------------------
      # 3.1 FILTER AND STANDARDIZE MIGRATION DATA
      #---------------------------------------------------------------------------------------------------
      
      # Process 2001 census migration data for analysis
      mig_2001_final <- mig_2001_final %>% 
      
        # # Apply analysis filters
        # filter(
        #   !q12_rsn %in% c("5", "6", "7"),     # Exclude: study/training, marriage, other non-economic reasons
        #   q12_cnty != "India",                # Exclude: migration to India)
        #   !is.na(vmun_code)                   # Keep only valid municipality identifier
        # ) %>%
        # 
        # Standardize destination country names
        mutate(country = case_when(
          q12_cnty == "America, Canada and Mexico" ~ "United States",
          q12_cnty %in% c("Other Asian Countries", "Other European Countries", "Other countries") ~ "Others",
          TRUE ~ q12_cnty
        )) %>%
        
        # Apply international country name standardization
        mutate(country = ifelse(
          country != "Others",
          countrycode(country, origin = "country.name", destination = "country.name", warn = FALSE),
          country
        )) %>%
        
        # Handle any remaining unmatched countries
        mutate(country = ifelse(is.na(country), "Others", country)) 
      
      
      write.csv(mig_2001_final, "data/clean/migration_2001.csv", row.names = FALSE)

      #   summarise(migrant2001 = n(), .groups = "drop")