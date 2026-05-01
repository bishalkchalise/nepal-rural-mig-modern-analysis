      ################################################################################
      #
      # BASELINE MIGRATION: SHIFT-SHARE INSTRUMENT (SSIV) CONSTRUCTION
      # ------------------------------------------------------------------------------
      # Purpose : Build ward-, municipality-, and district-year panels of forex-based
      #           shift-share instruments for Nepali international migration, using
      #           2001 census baseline destination shares and annual exchange-rate
      #           shocks (2001-2023).
      #
      # Geography levels (all three produced, annual panel 2001-2023):
      #   - Ward          (lgcode x ward)
      #   - Municipality  (lgcode)
      #   - District      (dname)
      #
      # Inputs :
      #   - 2001 Census absentee (international migrant) micro-data
      #   - 2011 Census ID crosswalk + old-VDC -> new-local-level map
      #   - 2001 Census individual-level population data
      #   - Forex panel (NPR and destination LCU per USD, 2000-2023)
      #
      # Outputs :
      #   - data/clean/instrument_ward.csv  (ward-year panel)
      #   - data/clean/instrument_mun.csv   (municipality-year panel)
      #   - data/clean/instrument_dist.csv  (district-year panel)
      #
      # ------------------------------------------------------------------------------
      # METHODOLOGY (what gets computed and why)
      # ------------------------------------------------------------------------------
      #
      # STEP 1 - DESTINATION-YEAR SHIFTER Z_{d,t}
      # For each destination d in each year t we build the exchange rate
      #   fx_to_npr_{d,t} = (NPR/USD)_t / (LCU/USD)_{d,t}   (NPR per 1 unit of LCU)
      #
      # Economic logic: a Nepali migrant earns in destination LCU and remits home.
      # A RISE in fx_to_npr means the destination currency appreciated against the
      # NPR - the migrant's LCU earnings now convert into MORE rupees. This is a
      # FAVORABLE remittance shock for the household.
      #
      # From this level we construct FIVE shifter versions per (d, t):
      #   (a) fx_index_2001  = fx_to_npr_{d,t} / fx_to_npr_{d,2001}
      #                        -> index: 1.00 at baseline, >1 = appreciation vs NPR
      #   (b) fx_growth_yoy  = fx_to_npr_{d,t} / fx_to_npr_{d,t-1}  - 1
      #                        -> year-on-year growth rate (x2-x1)/x1
      #   (c) fx_log_yoy     = log(fx_to_npr_{d,t}) - log(fx_to_npr_{d,t-1})
      #                        -> year-on-year log change (approx. small-growth %)
      #   (d) fx_growth_dec  = fx_to_npr_{d,t} / fx_to_npr_{d,t-10} - 1
      #                        -> 10-year rolling growth rate (x2-x1)/x1
      #   (e) fx_log_dec     = log(fx_to_npr_{d,t}) - log(fx_to_npr_{d,t-10})
      #                        -> 10-year rolling log change
      #
      # STEP 2 - BASELINE WEIGHTS (from 2001 Census absentee data)
      # For each geography g (ward/mun/dist) and destination d:
      #   N_{g,d,2001} = count of 2001 absentees from g who went to d
      #   Pop_{g,2001} = 2001 total population of g
      #   s_{g,d,2001} = N_{g,d,2001} / sum_d N_{g,d,2001}    (destination share)
      #   MI_{g,2001}  = sum_d N_{g,d,2001} / Pop_{g,2001}    (migration intensity)
      #
      # STEP 3 - INSTRUMENT FAMILIES (three weighting schemes x five shifters = 15
      # instrument columns per geography x year)
      #
      #   (A) Share-weighted shock (no scale - interpretation: "typical migrant shock")
      #       shareshock_{g,t} = sum_d s_{g,d,2001} * Z_{d,t}
      #       BASELINE BEHAVIOUR: when Z = fx_index_2001, shareshock_{g,2001} = 1.0
      #       at every geography (because shares sum to 1 and every shifter = 1).
      #       When Z is a growth/log shifter, shareshock_{g,2001} = 0.
      #
      #   (B) Per-capita exposure (MAIN SSIV - standard Bartik/BHJ form)
      #       ssiv_{g,t} = sum_d (N_{g,d,2001} / Pop_{g,2001}) * Z_{d,t}
      #                  = MI_{g,2001} * shareshock_{g,t}
      #       -> This is the main regressor for stocks/flows normalized by pop.
      #       BASELINE BEHAVIOUR: when Z = fx_index_2001, ssiv_{g,2001} = MI_{g,2001},
      #       NOT 1.0 - the SSIV in the baseline year collapses to baseline migration
      #       intensity. This is why `ssiv_index_2001` looks like a small number
      #       (~0.007 at the district level) in 2001 and grows over time: the shifter
      #       is 1 in 2001 and drifts up, while MI_{g,2001} is a fixed scale factor.
      #       For growth shifters (yoy/dec), ssiv_{g,2001} = 0 automatically.
      #
      #   (C) Absolute exposure (levels - total migrant-weighted shock)
      #       abs_{g,t} = sum_d N_{g,d,2001} * Z_{d,t}
      #       BASELINE BEHAVIOUR (Z = index): abs_{g,2001} = total_migrants_{g,2001}.
      #
      #   Identity used as a sanity check: ssiv = MI * shareshock (for any Z).
      #
      # STEP 4 - DIAGNOSTICS
      #   - Effective number of shocks (inverse Herfindahl of avg shares) for BHJ.
      #   - Identity check (ssiv vs MI*shareshock) should be ~0.
      #   - Destination matching between migration data and fx panel reported.
      #
      ################################################################################
      
      
      # ==============================================================================
      # 0. ENVIRONMENT SETUP
      # ==============================================================================
      
      rm(list = ls())
      cat("\14")
      # setwd("C:/Users/s222385015/OneDrive - Deakin University/PHD/Third Chapter/Analysis")
      
      options(scipen = 999)
      
      # ---- Load required packages --------------------------------------------------
      library(tidyverse)    # Data manipulation and visualization
      library(fixest)       # Fast fixed-effects estimation (used downstream)
      library(haven)        # Import Stata (.dta) files
      library(readxl)       # Import Excel files
      library(janitor)      # Data cleaning utilities
      library(countrycode)  # Country-name standardization
      library(stringr)      # String utilities
      library(tidyr)        # Reshape helpers (pivot_longer)
      library(ggplot2)      # Plotting
      
      
      ################################################################################
      # SECTION 1: LOAD RAW DATA AND BUILD ID CROSSWALK
      ################################################################################
      
      # ------------------------------------------------------------------------------
      # 1.1 Load 2001 Census absentee (international migration) micro-data
      # ------------------------------------------------------------------------------
      # `batchid` encodes district+VDC+ward; dividing by 100 drops the ward digits,
      # giving the 5-digit district-VDC identifier `ddvvv` used for merging.
      
      mig_2001_raw <- haven::read_dta(
        "data/raw/Full Census Data/Census 2001/fullmi01_full_absentee.dta"
      ) %>%
        mutate(ddvvv = as.integer(batchid) %/% 100)
      
      
      # ------------------------------------------------------------------------------
      # 1.2 Load crosswalks to map 2001 VDC/ward codes to modern local levels
      # ------------------------------------------------------------------------------
      # (a) 2011 Census ID file provides standardized codes linking to 2001 IDs.
      # (b) VDC -> local-government (lgcode) map reflects Nepal's 2017 restructuring.
      
      census_2011_id <- readxl::read_xlsx(
        "data/raw/Full Census Data/Census 2011/censusid2011.xlsx"
      ) %>%
        janitor::clean_names()
      
      vdc_to_lg_map <- readxl::read_xlsx(
        "data/raw/old vdc to local level.xlsx"
      ) %>%
        rename(dcode  = dist,
               lgcode = vmun_code)
      
      
      # ------------------------------------------------------------------------------
      # 1.3 Attach the VDC -> local-level mapping to the 2011 ID file
      # ------------------------------------------------------------------------------
      
      census_2011_mapped <- census_2011_id %>%
        left_join(
          vdc_to_lg_map,
          by = c("dname" = "dist_name", "vname" = "vname")
        ) %>%
        select(-dist) %>%
        filter(!is.na(dname))
      
      
      # ------------------------------------------------------------------------------
      # 1.4 Merge migration micro-data with the ID crosswalk
      # ------------------------------------------------------------------------------
      # Each migrant record now carries `dname` (district name), `lgcode`
      # (local-government code), and `ward` (ward within the new local level).
      
      mig_2001_final <- mig_2001_raw %>%
        left_join(
          census_2011_mapped,
          by = c("ddvvv"  = "ddvvvcbs11",
                 "vdcmun" = "vdcmun",
                 "ward"   = "ward")
        )
      
      
      ################################################################################
      # SECTION 2: DESTINATION COUNTRY CODING
      ################################################################################
      
      # ------------------------------------------------------------------------------
      # 2.1 Map 2001 Census numeric country codes to country names
      # ------------------------------------------------------------------------------
      
      country_names <- c(
        # --- South Asian neighbours -------------------------------------------------
        "1"  = "India",     "2" = "Pakistan",   "3" = "Bangladesh",
        "4"  = "Bhutan",    "5" = "Sri Lanka",  "6" = "Maldives",
        
        # --- East / Southeast Asia --------------------------------------------------
        "7"  = "China",     "8" = "Korea",
        "9"  = "Russia and Former States of USSR",
        "10" = "Japan",     "11" = "Hong Kong",
        "12" = "Singapore", "13" = "Malaysia",
        
        # --- Oceania ----------------------------------------------------------------
        "14" = "Australia",
        
        # --- Gulf (largest labour-migration corridor for Nepal) ---------------------
        "15" = "Saudi Arabia", "16" = "Qatar",  "17" = "Kuwait",
        "18" = "United Arab Emirates", "19" = "Bahrain",
        
        # --- Western countries ------------------------------------------------------
        "21" = "United Kingdom", "22" = "Germany",
        "23" = "France",         "24" = "Other European Countries",
        "25" = "America, Canada and Mexico",
        
        # --- Residual aggregates ----------------------------------------------------
        "20" = "Other Asian Countries",
        "96" = "Other countries"
      )
      
      mig_2001_final <- mig_2001_final %>%
        mutate(q12_cnty = country_names[as.character(q12_cnty)])
      
      
      ################################################################################
      # SECTION 3: CLEAN AND AGGREGATE MIGRATION FLOWS
      ################################################################################
      
      # ------------------------------------------------------------------------------
      # 3.1 Standardize country names
      # ------------------------------------------------------------------------------
      # NOTE: analysis filters (excluding study/marriage migrants etc.) are kept
      #       commented so raw flows are preserved. Uncomment to impose them.
      
      mig_2001_final <- mig_2001_final %>%
        
        # # ---- Optional analysis filters -------------------------------------------
      # filter(
      #   !q12_rsn %in% c("5", "6", "7"),  # drop study/training, marriage, other
      #   !is.na(lgcode)                   # keep only records with a valid lg code
      # ) %>%
      
      # Collapse a few aggregates to cleaner labels
      mutate(country = case_when(
        q12_cnty == "America, Canada and Mexico"                         ~ "United States",
        q12_cnty %in% c("Other Asian Countries",
                        "Other European Countries",
                        "Other countries")                               ~ "Others",
        TRUE                                                             ~ q12_cnty
      )) %>%
        
        # Pass everything except "Others" through countrycode for standard names
        mutate(country = ifelse(
          country != "Others",
          countrycode(country,
                      origin      = "country.name",
                      destination = "country.name",
                      warn        = FALSE),
          country
        )) %>%
        
        # Anything countrycode could not resolve is relabelled "Others"
        mutate(country = ifelse(is.na(country), "Others", country))
      
      
      # ------------------------------------------------------------------------------
      # 3.2 Aggregate migrant counts by geography x destination
      # ------------------------------------------------------------------------------
      # India is excluded from destinations because:
      #   (i) NPR-INR peg makes India's shifter collinear with Nepal-side variation;
      #   (ii) India is an open-border corridor, not a formal labour-migration market.
      
      # Ward-level counts (lgcode + ward)
      ward_mig_pop_2001 <- mig_2001_final %>%
        filter(country != "India", !is.na(lgcode), !is.na(ward)) %>%
        filter(q12_rsn != 6) %>%  # Remove absentee due to marriage 
        group_by(lgcode, new_ward, country) %>%
        summarise(ward_mig_pop_2001 = n(), .groups = "drop")
      
      # Municipality-level counts (lgcode)
      mun_mig_pop_2001 <- mig_2001_final %>%
        filter(country != "India", !is.na(lgcode)) %>%
        filter(q12_rsn != 6) %>% # Remove absentee due to marriage 
        group_by(lgcode, country) %>%
        summarise(mun_mig_pop_2001 = n(), .groups = "drop")
      
      # District-level counts (dname) -- India retained here if you want it; mirror
      # the municipality treatment by excluding it, for consistency.
      dist_mig_pop_2001 <- mig_2001_final %>%
        filter(country != "India", !is.na(dname)) %>%
        filter(q12_rsn != 6) %>%  # Remove absentee due to marriage 
        group_by(dname, country) %>%
        summarise(dist_mig_pop_2001 = n(), .groups = "drop")
      
      
      ################################################################################
      # SECTION 4: POPULATION DENOMINATORS AND MIGRATION INTENSITY
      ################################################################################
      
      # ------------------------------------------------------------------------------
      # 4.1 Load 2001 Census individual-level data and attach geography crosswalk
      # ------------------------------------------------------------------------------
      
      census_ind_2001 <- read_dta(
        "data/raw/Full Census Data/Census 2001/fullpi01_full.dta"
      ) %>%
        mutate(ddvvv = as.integer(batchid) %/% 100)
      
      census_ind_2001_geo <- census_ind_2001 %>%
        left_join(
          census_2011_mapped,
          by = c("dist"  = "dcode",
                 "vdcmun" = "vdcmun",
                 "ward"   = "ward")
        )
      
      
      # Ward populations (lgcode + new_ward)
      ward_pop <- census_ind_2001_geo %>%
        filter(!is.na(lgcode), !is.na(new_ward)) %>%
        group_by(lgcode, new_ward) %>%
        summarise(ward_pop_2001 = n(), .groups = "drop")
      
      
      gc()
      
      # Municipality populations (lgcode)
      mun_pop <- census_ind_2001_geo %>%
        filter(!is.na(lgcode)) %>%
        group_by(lgcode) %>%
        summarise(mun_pop_2001 = n(), .groups = "drop")
      
      gc()
      
      # District populations
      dist_pop <- census_ind_2001_geo %>%
        filter(!is.na(dname)) %>%
        group_by(dname) %>%
        summarise(dist_pop_2001 = n(), .groups = "drop")
      
      
      # ------------------------------------------------------------------------------
      # 4.2 Migration intensity = total migrants / population (both 2001)
      # ------------------------------------------------------------------------------
      # Total migrants = sum across destinations (India already excluded upstream).
      
      # Ward intensity
      ward_totals <- ward_mig_pop_2001 %>%
        group_by(lgcode, new_ward) %>%
        summarise(total_mig_2001 = sum(ward_mig_pop_2001, na.rm = TRUE),
                  .groups = "drop")
      
      ward_mig_intensity <- ward_pop %>%
        left_join(ward_totals, by = c("lgcode", "new_ward")) %>%
        mutate(
          total_mig_2001      = replace_na(total_mig_2001, 0),
          ward_intensity_2001 = if_else(ward_pop_2001 > 0,
                                        round(total_mig_2001 / ward_pop_2001, 8),
                                        NA_real_)
        ) %>%
        rename(ward_total_migrants_2001 = total_mig_2001)
      
      # Municipality intensity
      mun_totals <- mun_mig_pop_2001 %>%
        group_by(lgcode) %>%
        summarise(total_mig_2001 = sum(mun_mig_pop_2001, na.rm = TRUE),
                  .groups = "drop")
      
      mun_mig_intensity <- mun_pop %>%
        left_join(mun_totals, by = "lgcode") %>%
        mutate(
          total_mig_2001     = replace_na(total_mig_2001, 0),
          mun_intensity_2001 = if_else(mun_pop_2001 > 0,
                                       round(total_mig_2001 / mun_pop_2001, 8),
                                       NA_real_)
        ) %>%
        rename(mun_total_migrants_2001 = total_mig_2001)
      
      # District intensity
      dist_totals <- dist_mig_pop_2001 %>%
        group_by(dname) %>%
        summarise(total_mig_2001 = sum(dist_mig_pop_2001, na.rm = TRUE),
                  .groups = "drop")
      
      dist_mig_intensity <- dist_pop %>%
        left_join(dist_totals, by = "dname") %>%
        mutate(
          total_mig_2001      = replace_na(total_mig_2001, 0),
          dist_intensity_2001 = if_else(dist_pop_2001 > 0,
                                        round(total_mig_2001 / dist_pop_2001, 8),
                                        NA_real_)
        ) %>%
        rename(dist_total_migrants_2001 = total_mig_2001)
      
      
      ################################################################################
      # SECTION 5: FOREX PANEL (SHIFTER) CONSTRUCTION
      ################################################################################
      # Produces an annual panel 2001-2023 with FIVE shifter versions per country:
      #   fx_index_2001  : level index (2001 = 1.00)  -- for "multiplier" interp
      #   fx_growth_yoy  : year-on-year growth rate (x2 - x1) / x1   (2001 = NA)
      #   fx_log_yoy     : year-on-year log change                   (2001 = NA)
      #   fx_growth_dec  : 10-year rolling growth rate (t vs t-10)
      #   fx_log_dec     : 10-year rolling log change     (t vs t-10)
      # ------------------------------------------------------------------------------
      
      # ---- 5.1 Load raw forex data --------------------------------------------------
      # Keep 1991 onward so that the 10-year lag is available from 2001.
      forex_raw <- read.csv("data/clean/forex_2000_2023.csv") %>%
        filter(year >= 1999)
      
      # ---- 5.2 Nepal NPR/USD series -------------------------------------------------
      nepal_fx <- forex_raw %>%
        filter(country == "Nepal") %>%
        transmute(year, npr_per_usd = as.numeric(forex))
      
      # ---- 5.3 Build NPR-per-destination cross rate ---------------------------------
      # fx_to_npr_{d,t} = (NPR/USD)_t / (LCU/USD)_{d,t} = NPR per 1 LCU
      fx_panel <- forex_raw %>%
        transmute(country, year, lcu_per_usd = as.numeric(forex)) %>%
        left_join(nepal_fx, by = "year") %>%
        mutate(fx_to_npr =  lcu_per_usd/npr_per_usd) %>%
        filter(country != "Nepal", country != "India")   # drop Nepal (identity) + India
      
      # ---- 5.4 Anchor to 2001 baseline ---------------------------------------------
      fx_base_2001 <- fx_panel %>%
        filter(year == 2001) %>%
        transmute(country, fx_to_npr_2001 = fx_to_npr)
      
      fx_panel <- fx_panel %>%
        left_join(fx_base_2001, by = "country")
      
      # ---- 5.5 Compute the five shifter versions -----------------------------------
      fx_panel <- fx_panel %>%
        filter(!is.na(fx_to_npr), !is.na(fx_to_npr_2001)) %>%
        arrange(country, year) %>%
        group_by(country) %>%
        mutate(
          fx_index_2001 = fx_to_npr / fx_to_npr_2001,
          fx_growth_yoy = (fx_to_npr / lag(fx_to_npr, 1)) - 1,
          fx_log_yoy    = log(fx_to_npr) - log(lag(fx_to_npr, 1)),
          
          # Pull benchmark values; NA_real_ if that year doesn't exist for this country
          .base_2001 = if (any(year == 2001)) fx_to_npr[year == 2001][1] else NA_real_,
          .base_2011 = if (any(year == 2011)) fx_to_npr[year == 2011][1] else NA_real_,
          
          fx_growth_dec = case_when(
            year == 2011 ~ (fx_to_npr / .base_2001) - 1,
            year == 2021 ~ (fx_to_npr / .base_2011) - 1,
            TRUE ~ NA_real_
          ),
          fx_log_dec = case_when(
            year == 2011 ~ log(fx_to_npr) - log(.base_2001),
            year == 2021 ~ log(fx_to_npr) - log(.base_2011),
            TRUE ~ NA_real_
          )
        ) %>%
        select(-.base_2001, -.base_2011) %>%
        ungroup() %>%
        filter(year >= 2000, year <= 2023)
      
      # Tidy rounding + column order
      fx_panel <- fx_panel %>%
        mutate(across(c(lcu_per_usd, npr_per_usd, fx_to_npr, fx_to_npr_2001,
                        fx_index_2001,
                        fx_growth_yoy, fx_log_yoy,
                        fx_growth_dec, fx_log_dec),
                      ~ round(.x, 10))) %>%
        select(country, year,
               lcu_per_usd, npr_per_usd,
               fx_to_npr, fx_to_npr_2001,
               fx_index_2001,
               fx_growth_yoy, fx_log_yoy,
               fx_growth_dec, fx_log_dec)
      
      # ---- 5.6 Diagnostics ---------------------------------------------------------
      cat("\n--- SHIFTER PANEL: years covered ---\n")
      print(range(fx_panel$year, na.rm = TRUE))
      
      cat("\n--- SHIFTER DISTRIBUTION at 2011 and 2021 ---\n")
      fx_panel %>%
        filter(year %in% c(2011, 2021)) %>%
        group_by(year) %>%
        summarise(
          n_countries     = n(),
          mean_idx        = mean(fx_index_2001, na.rm = TRUE),
          mean_growth_yoy = mean(fx_growth_yoy, na.rm = TRUE),
          mean_log_dec    = mean(fx_log_dec,    na.rm = TRUE),
          sd_log_dec      = sd(fx_log_dec,      na.rm = TRUE),
          .groups = "drop"
        ) %>%
        print()
      
      
      ################################################################################
      # SECTION 6: SHIFT-SHARE PANEL BUILDER (reusable helper)
      ################################################################################
      # Takes (geography-destination counts, geography intensity, forex panel) and
      # returns the full 15-column instrument panel at (geog_id x year) level.
      # ------------------------------------------------------------------------------
      
      build_ssiv_panel <- function(mig_counts, intensity, fx, id_cols,
                                   pop_col, count_col) {
        
        # --- Clean join keys --------------------------------------------------------
        mig_clean <- mig_counts %>%
          mutate(country = str_squish(country)) %>%
          filter(!is.na(country), country != "")
        
        
        
        fx_clean <- fx %>%
          mutate(country = str_squish(country)) %>%
          filter(!is.na(country), country != "")
        
        
        # --- Baseline destination shares within each geography ----------------------
        mig_shares <- mig_clean %>%
          group_by(across(all_of(id_cols))) %>%
          mutate(
            geog_total_2001 = sum(.data[[count_col]], na.rm = TRUE),
            mig_share_2001  = .data[[count_col]] / geog_total_2001
          ) %>%
          ungroup()
        
        dest_year <- mig_shares %>%
          tidyr::crossing(year = unique(fx_clean$year)) %>%
          inner_join(fx_clean, by = c("country", "year")) %>%
          left_join(intensity, by = id_cols)
        
        # # --- Merge baseline shares with shifter panel (geog x dest x year) ----------
        # dest_year <- mig_shares %>%
        #   inner_join(fx_clean, by = "country", relationship = "many-to-many") %>%
        #   left_join(intensity, by = id_cols)
        
        
        # --- Collapse to geog-year, build 3 weighting families x 5 shifters ---------
        ssiv <- dest_year %>%
          group_by(across(all_of(c(id_cols, "year")))) %>%
          summarise(
            # Carry intensity metadata
            geog_pop_2001       = first(.data[[pop_col]]),
            geog_total_mig_2001 = first(geog_total_2001),
            geog_intensity_2001 = first(geog_total_mig_2001 / geog_pop_2001),
            
            # --- (A) Share-weighted shock -----------------------------------------
            # BASELINE: shareshock_index_2001 = 1 in 2001; growth/log shocks = 0.
            shareshock_index_2001 = sum(mig_share_2001 * fx_index_2001, na.rm = TRUE),
            shareshock_growth_yoy = sum(mig_share_2001 * fx_growth_yoy, na.rm = TRUE),
            shareshock_log_yoy    = sum(mig_share_2001 * fx_log_yoy,    na.rm = TRUE),
            shareshock_growth_dec = sum(mig_share_2001 * fx_growth_dec, na.rm = TRUE),
            shareshock_log_dec    = sum(mig_share_2001 * fx_log_dec,    na.rm = TRUE),
            
            # --- (B) Per-capita exposure (MAIN SSIV) ------------------------------
            # BASELINE: ssiv_index_2001 = MI_{g,2001} (NOT 1); growth/log ssiv = 0.
            ssiv_index_2001 = sum(.data[[count_col]] * fx_index_2001, na.rm = TRUE)
            / first(.data[[pop_col]]),
            ssiv_growth_yoy = sum(.data[[count_col]] * fx_growth_yoy, na.rm = TRUE)
            / first(.data[[pop_col]]),
            ssiv_log_yoy    = sum(.data[[count_col]] * fx_log_yoy,    na.rm = TRUE)
            / first(.data[[pop_col]]),
            ssiv_growth_dec = sum(.data[[count_col]] * fx_growth_dec, na.rm = TRUE)
            / first(.data[[pop_col]]),
            ssiv_log_dec    = sum(.data[[count_col]] * fx_log_dec,    na.rm = TRUE)
            / first(.data[[pop_col]]),
            
            # --- (C) Absolute exposure (levels) -----------------------------------
            # BASELINE: absexp_index_2001 = total_migrants_2001; growth/log = 0.
            absexp_index_2001 = sum(.data[[count_col]] * fx_index_2001, na.rm = TRUE),
            absexp_growth_yoy = sum(.data[[count_col]] * fx_growth_yoy, na.rm = TRUE),
            absexp_log_yoy    = sum(.data[[count_col]] * fx_log_yoy,    na.rm = TRUE),
            absexp_growth_dec = sum(.data[[count_col]] * fx_growth_dec, na.rm = TRUE),
            absexp_log_dec    = sum(.data[[count_col]] * fx_log_dec,    na.rm = TRUE),
            
            .groups = "drop"
          ) %>%
          # Identity checks: ssiv = MI * shareshock
          mutate(
            check_ssiv_index_2001 = shareshock_index_2001 * geog_intensity_2001,
            check_ssiv_log_yoy    = shareshock_log_yoy    * geog_intensity_2001,
            check_ssiv_log_dec    = shareshock_log_dec    * geog_intensity_2001,
            diff_index_2001       = ssiv_index_2001 - check_ssiv_index_2001,
            diff_log_yoy          = ssiv_log_yoy    - check_ssiv_log_yoy,
            diff_log_dec          = ssiv_log_dec    - check_ssiv_log_dec
          )
        
        return(ssiv)
      }
      
      
      ################################################################################
      # SECTION 7: BUILD THE THREE PANELS
      ################################################################################
      
      # ---- 7.1 Ward-level panel (lgcode + ward) ------------------------------------
      ward_ssiv <- build_ssiv_panel(
        mig_counts = ward_mig_pop_2001,
        intensity  = ward_mig_intensity,
        fx         = fx_panel,
        id_cols    = c("lgcode", "new_ward"),
        pop_col    = "ward_pop_2001",
        count_col  = "ward_mig_pop_2001"
      )
      
      # ---- 7.2 Municipality-level panel (lgcode) -----------------------------------
      mun_ssiv <- build_ssiv_panel(
        mig_counts = mun_mig_pop_2001,
        intensity  = mun_mig_intensity,
        fx         = fx_panel,
        id_cols    = "lgcode",
        pop_col    = "mun_pop_2001",
        count_col  = "mun_mig_pop_2001"
      )
      
      # ---- 7.3 District-level panel (dname) ----------------------------------------
      dist_ssiv <- build_ssiv_panel(
        mig_counts = dist_mig_pop_2001,
        intensity  = dist_mig_intensity,
        fx         = fx_panel,
        id_cols    = "dname",
        pop_col    = "dist_pop_2001",
        count_col  = "dist_mig_pop_2001"
      )
      
      
      ################################################################################
      # SECTION 8: DIAGNOSTICS
      ################################################################################
      
      cat("\n================================================================\n")
      cat("IDENTITY CHECKS: ssiv == MI * shareshock  (max abs deviation)\n")
      cat("================================================================\n")
      
      identity_report <- function(df, label) {
        df %>%
          summarise(
            level              = label,
            max_diff_index     = max(abs(diff_index_2001), na.rm = TRUE),
            max_diff_log_yoy   = max(abs(diff_log_yoy),    na.rm = TRUE),
            max_diff_log_dec   = max(abs(diff_log_dec),    na.rm = TRUE)
          )
      }
      
      bind_rows(
        identity_report(ward_ssiv, "ward"),
        identity_report(mun_ssiv,  "municipality"),
        identity_report(dist_ssiv, "district")
      ) %>% print()
      
      
      # ---- Effective number of shocks (BHJ diagnostic) at municipality level ------
      cat("\n--- EFFECTIVE NUMBER OF SHOCKS (inverse Herfindahl, mun-level) ---\n")
      mun_shares_avg <- mun_mig_pop_2001 %>%
        group_by(lgcode) %>%
        mutate(share = mun_mig_pop_2001 / sum(mun_mig_pop_2001, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(country) %>%
        summarise(avg_share = mean(share, na.rm = TRUE), .groups = "drop") %>%
        mutate(avg_share = avg_share / sum(avg_share))
      
      cat("Effective N shocks:", round(1 / sum(mun_shares_avg$avg_share^2), 2), "\n")
      cat("Raw N destinations:", nrow(mun_shares_avg), "\n\n")
      
      cat("--- TOP 10 DESTINATIONS BY AVERAGE MUN SHARE ---\n")
      mun_shares_avg %>% arrange(desc(avg_share)) %>% slice_head(n = 10) %>% print()
      
      
      ################################################################################
      # SECTION 9: VISUAL CHECK - mean district exposure over time
      ################################################################################
      # Shown side-by-side since baseline behaviour differs:
      #   - shareshock_index_2001: = 1 in 2001 (shifter is 1, shares sum to 1)
      #   - ssiv_index_2001:       = MI in 2001 (per-capita form scales by intensity)
      #   - ssiv_growth_dec:       = 0 in 2001 (growth-based)
      #   - ssiv_log_dec:          = 0 in 2001 (log-based)
      
      plot_df <- dist_ssiv %>%
        group_by(year) %>%
        summarise(
          shareshock_index_2001 = mean(shareshock_index_2001, na.rm = TRUE),
          ssiv_index_2001       = mean(ssiv_index_2001,       na.rm = TRUE),
          ssiv_growth_yoy       = mean(ssiv_growth_yoy,       na.rm = TRUE),
          ssiv_log_yoy          = mean(ssiv_log_yoy,          na.rm = TRUE),
          ssiv_growth_dec       = mean(ssiv_growth_dec,       na.rm = TRUE),
          ssiv_log_dec          = mean(ssiv_log_dec,          na.rm = TRUE),
          .groups = "drop"
        ) %>%
        pivot_longer(-year, names_to = "instrument", values_to = "value")
      
      ggplot(plot_df, aes(x = year, y = value, color = instrument)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        facet_wrap(~ instrument, scales = "free_y") +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(
          title    = "District-Level Mean SSIV over Time",
          subtitle = "Each panel on its own scale; baseline values reflect construction",
          y        = "SSIV value",
          x        = "Year"
        )
      
      
      ################################################################################
      # SECTION 10: FINAL PANELS + EXPORT
      ################################################################################
      # Drop the identity-check helper columns from the exported panels.
      
      ward_fx_panel <- ward_ssiv %>% select(-starts_with("check_"), -starts_with("diff_"))
      mun_fx_panel  <- mun_ssiv  %>% select(-starts_with("check_"), -starts_with("diff_"))
      dist_fx_panel <- dist_ssiv %>% select(-starts_with("check_"), -starts_with("diff_"))

      # ---- Add intuitive aliases used by script/estimate/*.R ----------------
      # These are 1:1 aliases of the technical names, exposed under names that
      # the estimation scripts and explorer use (clearer about what each term
      # represents — and SSIV is literally fxshock × mig_intensity).
      add_intuitive_aliases <- function(df) {
        df %>% mutate(
          fxshock                 = shareshock_index_2001,
          mig_intensity           = geog_intensity_2001,
          fxshock_x_mig_intensity = ssiv_index_2001,
          total_migrants          = geog_total_mig_2001
        )
      }
      ward_fx_panel <- add_intuitive_aliases(ward_fx_panel)
      mun_fx_panel  <- add_intuitive_aliases(mun_fx_panel)
      dist_fx_panel <- add_intuitive_aliases(dist_fx_panel)

      cat("\n--- FINAL PANEL DIMENSIONS ---\n")
      cat("Ward:         ", nrow(ward_fx_panel), "rows,",
          n_distinct(ward_fx_panel$lgcode, ward_fx_panel$new_ward), "wards\n")
      cat("Municipality: ", nrow(mun_fx_panel),  "rows,",
          n_distinct(mun_fx_panel$lgcode), "municipalities\n")
      cat("District:     ", nrow(dist_fx_panel), "rows,",
          n_distinct(dist_fx_panel$dname), "districts\n")
      
      write.csv(ward_fx_panel, "data/clean/instrument/instrument_ward.csv", row.names = FALSE)
      write.csv(mun_fx_panel,  "data/clean/instrument/instrument_mun.csv",  row.names = FALSE)
      write.csv(dist_fx_panel, "data/clean/instrument/instrument_dist.csv", row.names = FALSE)


      ################################################################################
      # SECTION 11: BASELINE DESTINATION-REGION SHARES (for Khanna-style controls)
      ################################################################################
      # Produces: data/clean/instrument/dest_region_shares_2001.csv
      # Used by build_results.py as baseline X (region composition) controls.
      # Region groupings follow Khanna et al. (2026) §IIIB roughly: Gulf, Other
      # West Asia, East Asia, Southeast Asia, OECD-North, OECD-Europe, South Asia
      # (excl. India, which is already dropped from migrant counts), Other.

      country_to_region <- function(c) {
        gulf      <- c("Saudi Arabia","Qatar","United Arab Emirates",
                       "Kuwait","Bahrain","Oman")
        oth_wasia <- c("Iraq","Iran","Lebanon","Israel","Jordan","Yemen","Syria")
        e_asia    <- c("Korea","Japan","China","Hong Kong","Taiwan")
        se_asia   <- c("Malaysia","Singapore","Thailand","Indonesia","Philippines")
        s_asia    <- c("Pakistan","Bangladesh","Bhutan","Sri Lanka","Maldives","Afghanistan")
        oecd_n    <- c("United States","Canada","Mexico","Australia","New Zealand")
        oecd_eu   <- c("United Kingdom","Germany","France","Italy","Spain",
                       "Portugal","Netherlands","Belgium","Sweden","Norway",
                       "Denmark","Finland","Ireland","Austria","Switzerland",
                       "Greece","Poland","Czechia","Slovakia","Hungary",
                       "Romania","Croatia","Malta","Cyprus","Slovenia")
        case_when(
          c %in% gulf      ~ "gulf",
          c %in% oth_wasia ~ "oth_wasia",
          c %in% e_asia    ~ "e_asia",
          c %in% se_asia   ~ "se_asia",
          c %in% s_asia    ~ "s_asia",
          c %in% oecd_n    ~ "oecd_north",
          c %in% oecd_eu   ~ "oecd_europe",
          TRUE             ~ "other"
        )
      }

      mun_region_shares <- mun_mig_pop_2001 %>%
        mutate(region = country_to_region(country)) %>%
        group_by(lgcode, region) %>%
        summarise(n = sum(mun_mig_pop_2001), .groups = "drop") %>%
        group_by(lgcode) %>%
        mutate(share = n / sum(n)) %>%
        select(lgcode, region, share) %>%
        tidyr::pivot_wider(names_from = region, values_from = share,
                           values_fill = 0,
                           names_prefix = "share_")

      cat("\n--- DESTINATION-REGION SHARES, MUNICIPALITY ---\n")
      cat("Municipalities:", nrow(mun_region_shares), "\n")
      cat("Columns: ", paste(setdiff(names(mun_region_shares), "lgcode"), collapse=", "), "\n\n")

      write.csv(mun_region_shares,
                "data/clean/instrument/dest_region_shares_2001.csv",
                row.names = FALSE)

      ################################################################################
      # END OF SCRIPT
      ################################################################################