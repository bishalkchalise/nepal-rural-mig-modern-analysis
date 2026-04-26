      ##############################################################################
      # NRVS STAGE 2: MIGRATION OUTCOMES (MIGRANT × YEAR  and  HH × YEAR)
      ##############################################################################
      #
      # Reads the Stage-1 raw CSV for Section 11 (migrants, one row per migrant ×
      # year). Produces two outputs:
      #
      #   1. migration_migrant_year.csv  — one row per (hhid, migrationid, year)
      #        Preserves all per-migrant detail: destination, reason, earnings,
      #        remittance sent by this specific migrant, migration cost.
      #
      #   2. migration_hh_year.csv       — one row per (hhid, year)
      #        29 behavioural outcomes aggregated to the household. Designed to
      #        merge with consumption, enterprise, and agriculture hh-year files.
      #
      # Value-code verification (from user's diagnostic on the real data):
      #   s11q02              "Overseas" / "Locally (in Nepal)" strings
      #   s11q02a             District name (local migrant) — 74 distinct values
      #   s11q02b             Country (overseas) — "India", "Malayasia" [sic],
      #                       "Qatar", "Saudi Arabia", "United Arab Emirates",
      #                       plus 9 smaller destinations. NB: spelling typos exist.
      #   s11q02d             Free-text country, wave-3 only. Used to refine Gulf
      #                       matching (catches "Kuwait", "Bahrain", "Oman" which
      #                       fall into the s11q02b "Other" bucket in 2016/17).
      #   s11q07a             "Yes" / "No" string — gateway "Did migrant send?"
      #   s11q07b             NUMERIC months/year (1–15; 998 = "don't know").
      #                       12 = monthly, 4 = quarterly, etc. Capped at 12.
      #   s11q07c             Numeric Rs. (range 0–5M; median 100k).
      #   s11q07d_1..4        "Yes" / "No" strings per channel:
      #                       _1 = Bank/IME (formal), _2 = friends/family,
      #                       _3 = Hundi (informal), _4 = Other.
      #   s11q01c             "Male"/"Female"/"Third Gender"
      #   s11q03              Numeric months since first left; cap at 600 (50 yrs)
      #                       since max reported was 3,600 (data-entry error).
      #
      ##############################################################################
      
      library(tidyverse)
      library(fs)
      
      base_in  <- "data/raw/RVS Data/clean"
      base_out <- "data/clean/rvs_outcomes"
      dir_create(base_out, recurse = TRUE)
      
      read_csv_q <- function(p) read_csv(p, show_col_types = FALSE, progress = FALSE)
      
      sec11 <- read_csv_q(file.path(base_in, "migration", "section_11.csv"))
      
      
      ##############################################################################
      # 1. HELPERS
      ##############################################################################
      
      # Safe numeric coerce (labelled → raw number; garbage → NA)
      as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
      
      # Case-insensitive substring pattern match with NA safety
      match_any <- function(x, patterns) {
        s <- tolower(as.character(x))
        s <- dplyr::coalesce(s, "")
        pattern <- paste(patterns, collapse = "|")
        stringr::str_detect(s, pattern)
      }
      
      yesno <- function(x) {
        s <- tolower(as.character(x))
        dplyr::case_when(
          is.na(s)     ~ NA_integer_,
          s == "yes"   ~ 1L,
          s == "no"    ~ 0L,
          s == "1"     ~ 1L,
          s == "2"     ~ 0L,
          TRUE         ~ NA_integer_
        )
      }
      
      
      ##############################################################################
      # 2. MIGRANT-LEVEL FILE (clean per-migrant rows)
      ##############################################################################
      
      # Gulf countries: s11q02b canonical ("Qatar", "Saudi Arabia", "United Arab
      # Emirates") PLUS s11q02d free-text for 2018 where migrants may have said
      # Kuwait / Bahrain / Oman (which fall into s11q02b = "Other" in 2016/17).
      gulf_patterns <- c("qatar", "saudi", "emirates", "\\buae\\b",
                         "kuwait", "bahrain", "bahrai", "oman", "dubai")
      
      migrant_year <- sec11 %>%
        mutate(
          # Type
          is_internal      = as.integer(match_any(s11q02, "locally")),
          is_international = as.integer(match_any(s11q02, "overseas")),
          
          # Destination region / country for overseas
          dest_india     = as.integer(match_any(s11q02b, "^india$")),
          dest_gulf      = as.integer(match_any(s11q02b, gulf_patterns) |
                                        match_any(s11q02d, gulf_patterns)),
          dest_malaysia  = as.integer(match_any(s11q02b, "malay")),     # incl. "Malayasia" typo
          dest_other_country = as.integer(is_international == 1 &
                                            match_any(s11q02b, "^india$") == FALSE &
                                            (!match_any(s11q02b, gulf_patterns)) &
                                            (!match_any(s11q02b, "malay"))),
          
          # Demographics
          migrant_male        = as.integer(match_any(s11q01c, "^male$")),
          migrant_female      = as.integer(match_any(s11q01c, "^female$")),
          migrant_third_gender= as.integer(match_any(s11q01c, "third")),
          migrant_age         = as_num(s11q01d),
          months_away         = pmin(as_num(s11q03), 600, na.rm = FALSE),  # cap at 50 yrs
          
          # Reasons (11 categories, many binaries → grouped)
          reason_work   = as.integer(yesno(s11q04a_6) == 1 | yesno(s11q04a_7) == 1),
          reason_educ   = as.integer(yesno(s11q04a_4) == 1 | yesno(s11q04a_5) == 1),
          reason_family = as.integer(yesno(s11q04a_1) == 1 | yesno(s11q04a_2) == 1 |
                                       yesno(s11q04a_3) == 1),
          reason_disaster = as.integer(yesno(s11q04a_10) == 1),
          
          # Earnings at destination
          earning_primary_rs    = coalesce(as_num(s11q05c), 0),
          earning_secondary_rs  = coalesce(as_num(s11q06c), 0),
          has_secondary_job     = as.integer(yesno(s11q06a) == 1),
          
          # Remittance (by this migrant)
          remit_sent_flag       = yesno(s11q07a),
          remit_amount_rs       = coalesce(as_num(s11q07c), 0),
          # Frequency: numeric sends per year, treat 998 as "don't know" → NA.
          # Cap at 12 (monthly) to guard against data-entry errors (max was 15).
          remit_freq_per_year   = dplyr::if_else(
            as_num(s11q07b) < 998 & !is.na(as_num(s11q07b)),
            pmin(as_num(s11q07b), 12), NA_real_
          ),
          remit_via_bank_ime    = yesno(s11q07d_1),
          remit_via_friends     = yesno(s11q07d_2),
          remit_via_hundi       = yesno(s11q07d_3),
          remit_via_other       = yesno(s11q07d_4),
          
          # Remittance use (15 binaries → consolidated to 6 analytic flags)
          remit_use_consumption = as.integer(
            yesno(s11q07e_1) == 1 | yesno(s11q07e_12) == 1
          ),  # non-durable + ceremonies
          remit_use_education   = as.integer(yesno(s11q07e_3)  == 1),
          remit_use_health      = as.integer(yesno(s11q07e_4)  == 1),
          remit_use_productive  = as.integer(
            yesno(s11q07e_5)  == 1 |  # business inputs
              yesno(s11q07e_6)  == 1 |  # business equipment
              yesno(s11q07e_7)  == 1 |  # land purchase
              yesno(s11q07e_8)  == 1 |  # livestock
              yesno(s11q07e_9)  == 1    # other business
          ),
          remit_use_housing     = as.integer(
            yesno(s11q07e_10) == 1 | yesno(s11q07e_11) == 1
          ),  # purchase or improve dwelling
          remit_use_debt        = as.integer(yesno(s11q07e_14) == 1),
          
          # Migration cost
          mig_cost_any          = yesno(s11q08a),
          mig_cost_rs           = coalesce(as_num(s11q08b), 0),
          mig_cost_loan_flag    = yesno(s11q08c_1)
        )
      
      # Write migrant-level output (trim to useful columns, drop raw s11q* duplicates)
      migrant_year_out <- migrant_year %>%
        select(
          hhid, year,
          any_of(c("migrationid", "member_id", "wt_hh", "vmun_code", "lgname",
                   "district77", "district_name")),
          
          # Raw per-migrant fields preserved for analysis-stage use
          is_internal, is_international,
          dest_india, dest_gulf, dest_malaysia, dest_other_country,
          s11q02b_raw = s11q02b, s11q02d_raw = s11q02d, s11q02a_raw = s11q02a,
          
          migrant_male, migrant_female, migrant_third_gender, migrant_age,
          months_away,
          
          reason_work, reason_educ, reason_family, reason_disaster,
          
          earning_primary_rs, earning_secondary_rs, has_secondary_job,
          
          remit_sent_flag, remit_amount_rs, remit_freq_per_year,
          remit_via_bank_ime, remit_via_friends, remit_via_hundi, remit_via_other,
          remit_use_consumption, remit_use_education, remit_use_health,
          remit_use_productive, remit_use_housing, remit_use_debt,
          
          mig_cost_any, mig_cost_rs, mig_cost_loan_flag
        )
      
      write_csv(migrant_year_out,
                file.path(base_out, "migration_migrant_year.csv"), na = "")
      
      
      ##############################################################################
      # 3. HH × YEAR AGGREGATION (29 columns)
      ##############################################################################
      
      hh_agg <- migrant_year %>%
        group_by(hhid, year) %>%
        summarise(
          # Status
          n_migrants_total         = dplyr::n(),
          has_migrant              = 1L,
          has_migrant_internal     = as.integer(any(is_internal == 1,      na.rm = TRUE)),
          has_migrant_international= as.integer(any(is_international == 1, na.rm = TRUE)),
          n_migrants_international = sum(is_international == 1, na.rm = TRUE),
          
          # Demographics
          n_migrants_male          = sum(migrant_male   == 1, na.rm = TRUE),
          n_migrants_female        = sum(migrant_female == 1, na.rm = TRUE),
          migrant_mean_age         = mean(migrant_age,  na.rm = TRUE),
          migrant_mean_months_away = mean(months_away,  na.rm = TRUE),
          
          # Reasons (any migrant)
          mig_reason_work          = as.integer(any(reason_work     == 1, na.rm = TRUE)),
          mig_reason_education     = as.integer(any(reason_educ     == 1, na.rm = TRUE)),
          mig_reason_marriage_family = as.integer(any(reason_family == 1, na.rm = TRUE)),
          mig_reason_disaster      = as.integer(any(reason_disaster == 1, na.rm = TRUE)),
          
          # Destination shares (within international migrants)
          n_dest_india             = sum(dest_india    == 1, na.rm = TRUE),
          n_dest_gulf              = sum(dest_gulf     == 1, na.rm = TRUE),
          n_dest_malaysia          = sum(dest_malaysia == 1, na.rm = TRUE),
          
          # Remittance
          remit_received           = as.integer(any(remit_sent_flag == 1, na.rm = TRUE)),
          remit_amount_12m_rs      = sum(remit_amount_rs, na.rm = TRUE),
          remit_amount_intl_12m_rs = sum(remit_amount_rs[is_international == 1], na.rm = TRUE),
          remit_frequency_avg      = mean(remit_freq_per_year, na.rm = TRUE),
          remit_via_formal_any     = as.integer(any(remit_via_bank_ime == 1, na.rm = TRUE)),
          remit_via_hundi_any      = as.integer(any(remit_via_hundi    == 1, na.rm = TRUE)),
          
          # Remittance use
          remit_use_consumption_any = as.integer(any(remit_use_consumption == 1, na.rm = TRUE)),
          remit_use_education_any   = as.integer(any(remit_use_education   == 1, na.rm = TRUE)),
          remit_use_health_any      = as.integer(any(remit_use_health      == 1, na.rm = TRUE)),
          remit_use_productive_any  = as.integer(any(remit_use_productive  == 1, na.rm = TRUE)),
          
          # Migration cost
          mig_cost_12m_rs                = sum(mig_cost_rs, na.rm = TRUE),
          mig_cost_financed_by_loan_any  = as.integer(any(mig_cost_loan_flag == 1, na.rm = TRUE)),
          
          # Earnings at destination
          migrant_monthly_earning_total_rs = sum(earning_primary_rs + earning_secondary_rs,
                                                 na.rm = TRUE),
          migrant_has_secondary_job_any    = as.integer(any(has_secondary_job == 1,
                                                            na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        mutate(
          # Destination SHARES within international migrants (guard div/0)
          dest_india_share    = dplyr::if_else(n_migrants_international > 0,
                                               n_dest_india    / n_migrants_international,
                                               NA_real_),
          dest_gulf_share     = dplyr::if_else(n_migrants_international > 0,
                                               n_dest_gulf     / n_migrants_international,
                                               NA_real_),
          dest_malaysia_share = dplyr::if_else(n_migrants_international > 0,
                                               n_dest_malaysia / n_migrants_international,
                                               NA_real_)
        )
      
      
      ##############################################################################
      # 4. BALANCE TO FULL HH UNIVERSE — non-migrant HHs get zeros
      ##############################################################################
      
      idmap_path <- file.path(base_in, "id_match_long.csv")
      if (file.exists(idmap_path)) {
        id_match <- read_csv_q(idmap_path)
      } else {
        warning("id_match_long.csv not found; output covers only migrant HHs.")
        id_match <- hh_agg %>% distinct(hhid, year)
      }
      
      id_min <- id_match %>%
        select(hhid, year,
               any_of(c("wt_hh", "psu", "district", "vdc",
                        "vmun_code", "lgname", "district77", "district_name",
                        "s00q03a", "s00q03b", "s00q03c")))
      
      migration_hh_year <- id_min %>%
        left_join(hh_agg, by = c("hhid", "year")) %>%
        mutate(
          has_migrant               = coalesce(has_migrant, 0L),
          has_migrant_internal      = coalesce(has_migrant_internal, 0L),
          has_migrant_international = coalesce(has_migrant_international, 0L),
          remit_received            = coalesce(remit_received, 0L),
          across(c(n_migrants_total, n_migrants_international,
                   n_migrants_male, n_migrants_female,
                   n_dest_india, n_dest_gulf, n_dest_malaysia,
                   mig_reason_work, mig_reason_education,
                   mig_reason_marriage_family, mig_reason_disaster,
                   remit_amount_12m_rs, remit_amount_intl_12m_rs,
                   remit_via_formal_any, remit_via_hundi_any,
                   remit_use_consumption_any, remit_use_education_any,
                   remit_use_health_any, remit_use_productive_any,
                   mig_cost_12m_rs, mig_cost_financed_by_loan_any,
                   migrant_monthly_earning_total_rs,
                   migrant_has_secondary_job_any),
                 ~ coalesce(., 0))
          # migrant_mean_age, migrant_mean_months_away, remit_frequency_avg,
          # and the *_share columns stay NA for HHs with no migrants (correct)
        )
      
      # Final column order
      id_cols <- c("hhid", "year", "wt_hh",
                   intersect(c("psu", "district", "vdc",
                               "vmun_code", "lgname", "district77", "district_name",
                               "s00q03a", "s00q03b", "s00q03c"),
                             names(migration_hh_year)))
      
      outcome_cols <- c(
        # (1) Status (5)
        "has_migrant", "has_migrant_internal", "has_migrant_international",
        "n_migrants_total", "n_migrants_international",
        # (2) Demographics (4)
        "n_migrants_male", "n_migrants_female",
        "migrant_mean_age", "migrant_mean_months_away",
        # (3) Reasons (4)
        "mig_reason_work", "mig_reason_education",
        "mig_reason_marriage_family", "mig_reason_disaster",
        # (4) Destination (3)
        "dest_india_share", "dest_gulf_share", "dest_malaysia_share",
        # (5) Remittance — core (7)
        "remit_received", "remit_amount_12m_rs", "remit_amount_intl_12m_rs",
        "remit_frequency_avg", "remit_via_formal_any", "remit_via_hundi_any",
        # (6) Remittance use (4)
        "remit_use_consumption_any", "remit_use_education_any",
        "remit_use_health_any", "remit_use_productive_any",
        # (7) Migration cost (2)
        "mig_cost_12m_rs", "mig_cost_financed_by_loan_any",
        # (8) Earnings (2)
        "migrant_monthly_earning_total_rs", "migrant_has_secondary_job_any"
      )
      
      migration_hh_year <- migration_hh_year %>%
        select(all_of(id_cols), all_of(outcome_cols)) %>%
        arrange(hhid, year) %>%
        mutate(across(where(is.numeric),
                      ~ dplyr::if_else(is.nan(.x), NA_real_, .x)))
      
      write_csv(migration_hh_year,
                file.path(base_out, "migration_hh_year.csv"), na = "")
      
      
      ##############################################################################
      # 5. CODEBOOK
      ##############################################################################
      
      codebook <- tribble(
        ~variable,                  ~unit,       ~reference,  ~source,                 ~definition,
        
        # Status
        "has_migrant",              "HH × year", "present",   "s11q02 non-empty",      "1 if HH has at least one migrant (internal or overseas).",
        "has_migrant_internal",     "HH × year", "present",   "s11q02 = 'Locally'",    "1 if HH has at least one migrant within Nepal.",
        "has_migrant_international","HH × year", "present",   "s11q02 = 'Overseas'",   "1 if HH has at least one migrant abroad.",
        "n_migrants_total",         "HH × year", "present",   "count Section 11 rows", "Total migrants from this HH.",
        "n_migrants_international", "HH × year", "present",   "count overseas",        "Number of international migrants.",
        
        # Demographics
        "n_migrants_male",          "HH × year", "present",   "s11q01c = Male",        "Count of male migrants.",
        "n_migrants_female",        "HH × year", "present",   "s11q01c = Female",      "Count of female migrants.",
        "migrant_mean_age",         "HH × year", "present",   "mean(s11q01d)",         "Mean age of migrants at survey. NA if no migrants.",
        "migrant_mean_months_away", "HH × year", "present",   "mean(s11q03)",          "Mean months since first leaving, capped at 600. NA if no migrants.",
        
        # Reasons
        "mig_reason_work",          "HH × year", "12 months", "s11q04a_6 OR _7",       "1 if any migrant moved for 'look for work' or 'start new job/business'.",
        "mig_reason_education",     "HH × year", "12 months", "s11q04a_4 OR _5",       "1 if any migrant moved for education or training.",
        "mig_reason_marriage_family","HH × year","12 months", "s11q04a_1/2/3",         "1 if any migrant moved for marriage, to follow family, or family reasons.",
        "mig_reason_disaster",      "HH × year", "12 months", "s11q04a_10",            "1 if any migrant moved due to natural disaster.",
        
        # Destination
        "dest_india_share",         "HH × year", "present",   "s11q02b = India",       "Share of international migrants in India. NA if no international migrants.",
        "dest_gulf_share",          "HH × year", "present",   "s11q02b/d Gulf",        "Share in Gulf countries: Qatar, Saudi Arabia, UAE, Kuwait, Bahrain, Oman (s11q02b + s11q02d free-text in 2018).",
        "dest_malaysia_share",      "HH × year", "present",   "s11q02b Malaysia",      "Share in Malaysia (catches 'Malayasia' typo in data).",
        
        # Remittance core
        "remit_received",           "HH × year", "12 months", "s11q07a = Yes",         "1 if any migrant sent remittance in past 12 months.",
        "remit_amount_12m_rs",      "HH × year", "12 months", "sum s11q07c",           "Total remittance received (Rs.) across all migrants.",
        "remit_amount_intl_12m_rs", "HH × year", "12 months", "sum s11q07c if intl",   "Remittance received from international migrants only.",
        "remit_frequency_avg",      "HH × year", "12 months", "mean s11q07b",          "Mean frequency of remittance sends per year, across migrants who sent (capped at 12 = monthly; excludes '998' don't-know).",
        "remit_via_formal_any",     "HH × year", "12 months", "s11q07d_1 = Yes",       "1 if any migrant used bank / IME (formal channel).",
        "remit_via_hundi_any",      "HH × year", "12 months", "s11q07d_3 = Yes",       "1 if any migrant used hundi (informal transfer).",
        
        # Remittance use
        "remit_use_consumption_any","HH × year", "12 months", "s11q07e_1/12",          "1 if remittance used for non-durable consumption or ceremonies.",
        "remit_use_education_any",  "HH × year", "12 months", "s11q07e_3",             "1 if remittance used for education costs.",
        "remit_use_health_any",     "HH × year", "12 months", "s11q07e_4",             "1 if remittance used for health costs.",
        "remit_use_productive_any", "HH × year", "12 months", "s11q07e_5..9",          "1 if remittance used for productive investment (business inputs/equipment, land, livestock, other business).",
        
        # Migration cost
        "mig_cost_12m_rs",          "HH × year", "past",      "sum s11q08b",           "Total Rs. spent on migration costs (across all HH migrants).",
        "mig_cost_financed_by_loan_any","HH × year","past",   "s11q08c_1 = Yes",       "1 if any HH migrant financed migration cost via a loan.",
        
        # Earnings
        "migrant_monthly_earning_total_rs","HH × year","present","s11q05c + s11q06c",  "Sum of monthly earnings (primary + secondary job) across all HH migrants.",
        "migrant_has_secondary_job_any","HH × year","present",  "s11q06a = Yes",       "1 if any HH migrant reported a secondary job."
      )
      write_csv(codebook, file.path(base_out, "migration_codebook.csv"))
      
      
      ##############################################################################
      # 6. SANITY REPORT
      ##############################################################################
      
      cat("\n=============================================================\n")
      cat("migration_migrant_year.csv:", nrow(migrant_year_out), "rows (one per migrant × year)\n")
      cat("migration_hh_year.csv:      ", nrow(migration_hh_year), "rows (one per HH × year)\n\n")
      
      cat("---- Migrant counts by year ----\n")
      migrant_year_out %>% count(year) %>% print()
      
      cat("\n---- HH migration prevalence ----\n")
      migration_hh_year %>%
        group_by(year) %>%
        summarise(
          n_hh                   = dplyr::n(),
          share_has_migrant      = round(mean(has_migrant),               3),
          share_has_intl_migrant = round(mean(has_migrant_international), 3),
          mean_n_migrants        = round(mean(n_migrants_total),          2),
          share_remit_received   = round(mean(remit_received),            3),
          mean_remit_rs_cond     = round(mean(remit_amount_12m_rs[remit_received == 1],
                                              na.rm = TRUE), 0),
          .groups = "drop"
        ) %>% print()
      
      cat("\n---- Destination breakdown (of international migrants) ----\n")
      migrant_year_out %>%
        filter(is_international == 1) %>%
        summarise(
          n_intl        = dplyr::n(),
          share_india   = round(mean(dest_india == 1),        3),
          share_gulf    = round(mean(dest_gulf == 1),         3),
          share_malaysia= round(mean(dest_malaysia == 1),     3),
          share_other   = round(mean(dest_other_country == 1),3)
        ) %>% print()
      
      cat("\n---- Remittance channel uptake (among senders) ----\n")
      migrant_year_out %>%
        filter(remit_sent_flag == 1) %>%
        summarise(
          n_senders    = dplyr::n(),
          share_bank   = round(mean(remit_via_bank_ime == 1, na.rm = TRUE), 3),
          share_friend = round(mean(remit_via_friends  == 1, na.rm = TRUE), 3),
          share_hundi  = round(mean(remit_via_hundi    == 1, na.rm = TRUE), 3),
          share_other  = round(mean(remit_via_other    == 1, na.rm = TRUE), 3),
          mean_freq    = round(mean(remit_freq_per_year,  na.rm = TRUE), 2),
          mean_amount  = round(mean(remit_amount_rs,      na.rm = TRUE), 0)
        ) %>% print()
      
      cat("\n---- Remittance use (share among receivers) ----\n")
      migration_hh_year %>%
        filter(remit_received == 1) %>%
        summarise(
          share_consumption = round(mean(remit_use_consumption_any), 3),
          share_education   = round(mean(remit_use_education_any),   3),
          share_health      = round(mean(remit_use_health_any),      3),
          share_productive  = round(mean(remit_use_productive_any),  3)
        ) %>% print()
      
      cat("\n---- Full HH × year outcome summary ----\n")
      migration_hh_year %>%
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