    ##############################################################################
    # NRVS STAGE 2: MIGRATION OUTCOMES
    # Outputs:
    #   1. migration_migrant_year.csv
    #   2. migration_hh_year.csv
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
    
    as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
    
    match_any <- function(x, patterns) {
      s <- str_squish(str_to_lower(as.character(x)))
      s <- coalesce(s, "")
      str_detect(s, paste(patterns, collapse = "|"))
    }
    
    yesno <- function(x) {
      s <- str_squish(str_to_lower(as.character(x)))
      
      case_when(
        is.na(s) | s == "" ~ NA_integer_,
        s == "yes" ~ 1L,
        s == "no"  ~ 0L,
        s == "1"   ~ 1L,
        s == "2"   ~ 0L,
        TRUE ~ NA_integer_
      )
    }
    
    ##############################################################################
    # 2. MIGRANT-LEVEL CLEANING
    ##############################################################################
    
    gulf_patterns <- c(
      "qatar", "saudi", "emirates", "\\buae\\b",
      "kuwait", "bahrain", "bahrai", "oman", "dubai"
    )
    
    migrant_year <- sec11 %>%
      mutate(
        # Migration type
        is_internal      = as.integer(match_any(s11q02, "locally")),
        is_international = as.integer(match_any(s11q02, "overseas")),
        
        # Destination
        dest_india = as.integer(match_any(s11q02b, "^india$")),
        dest_gulf = as.integer(
          match_any(s11q02b, gulf_patterns) |
            match_any(s11q02d, gulf_patterns)
        ),
        dest_malaysia = as.integer(match_any(s11q02b, "malay")),
        dest_other_country = as.integer(
          is_international == 1 &
            dest_india == 0 &
            dest_gulf == 0 &
            dest_malaysia == 0
        ),
        
        # Demographics
        migrant_male   = as.integer(match_any(s11q01c, "^male$")),
        migrant_female = as.integer(match_any(s11q01c, "^female$")),
        migrant_age    = as_num(s11q01d),
        months_away    = pmin(as_num(s11q03), 600, na.rm = FALSE),
        
        # Reasons
        reason_work = as.integer(
          yesno(s11q04a_6) == 1 | yesno(s11q04a_7) == 1
        ),
        reason_educ = as.integer(
          yesno(s11q04a_4) == 1 | yesno(s11q04a_5) == 1
        ),
        reason_family = as.integer(
          yesno(s11q04a_1) == 1 |
            yesno(s11q04a_2) == 1 |
            yesno(s11q04a_3) == 1
        ),
        
        # Earnings
        earning_primary_rs   = coalesce(as_num(s11q05c), 0),
        earning_secondary_rs = coalesce(as_num(s11q06c), 0),
        has_secondary_job    = as.integer(yesno(s11q06a) == 1),
        
        # Remittance
        remit_sent_flag = yesno(s11q07a),
        remit_amount_rs = coalesce(as_num(s11q07c), 0),
        
        remit_freq_per_year = if_else(
          as_num(s11q07b) < 998 & !is.na(as_num(s11q07b)),
          pmin(as_num(s11q07b), 12),
          NA_real_
        ),
        
        remit_via_bank_ime = yesno(s11q07d_1),
        remit_via_friends  = yesno(s11q07d_2),
        remit_via_hundi    = yesno(s11q07d_3),
        remit_via_other    = yesno(s11q07d_4),
        
        # Remittance use
        remit_use_consumption = as.integer(
          yesno(s11q07e_1) == 1 | yesno(s11q07e_12) == 1
        ),
        
        remit_use_education = as.integer(
          yesno(s11q07e_3) == 1
        ),
        
        remit_use_business = as.integer(
          yesno(s11q07e_5) == 1 |
            yesno(s11q07e_6) == 1 |
            yesno(s11q07e_7) == 1 |
            yesno(s11q07e_8) == 1 |
            yesno(s11q07e_9) == 1
        ),
        
        # Migration cost
        mig_cost_any = yesno(s11q08a),
        mig_cost_rs  = coalesce(as_num(s11q08b), 0),
        mig_cost_loan_flag = yesno(s11q08c_1)
      )
    
    ##############################################################################
    # 3. MIGRANT-YEAR OUTPUT
    ##############################################################################
    
    migrant_year_out <- migrant_year %>%
      select(
        hhid, year,
        any_of(c(
          "migrationid", "member_id", "wt_hh",
          "vmun_code", "lgname", "district77", "district_name"
        )),
        
        is_internal,
        is_international,
        dest_india,
        dest_gulf,
        dest_malaysia,
        dest_other_country,
        s11q02b_raw = s11q02b,
        s11q02d_raw = s11q02d,
        s11q02a_raw = s11q02a,
        
        migrant_male,
        migrant_female,
        migrant_age,
        months_away,
        
        reason_work,
        reason_educ,
        reason_family,
        
        earning_primary_rs,
        earning_secondary_rs,
        has_secondary_job,
        
        remit_sent_flag,
        remit_amount_rs,
        remit_freq_per_year,
        remit_via_bank_ime,
        remit_via_friends,
        remit_via_hundi,
        remit_via_other,
        
        remit_use_consumption,
        remit_use_education,
        remit_use_business,
        
        mig_cost_any,
        mig_cost_rs,
        mig_cost_loan_flag
      )
    
    write_csv(
      migrant_year_out,
      file.path(base_out, "migration_migrant_year.csv"),
      na = ""
    )
    
    ##############################################################################
    # 4. HH-YEAR AGGREGATION
    ##############################################################################
    
    hh_agg <- migrant_year %>%
      group_by(hhid, year) %>%
      summarise(
        has_migrant = 1L,
        
        has_migrant_internal = as.integer(
          any(is_internal == 1, na.rm = TRUE)
        ),
        
        remit_received = as.integer(
          any(remit_sent_flag == 1, na.rm = TRUE)
        ),
        
        remit_amount_12m_rs = sum(
          remit_amount_rs,
          na.rm = TRUE
        ),
        
        remit_amount_intl_12m_rs = sum(
          remit_amount_rs[is_international == 1],
          na.rm = TRUE
        ),
        
        remit_via_formal_any = as.integer(
          any(remit_via_bank_ime == 1, na.rm = TRUE)
        ),
        
        remit_via_hundi_any = as.integer(
          any(remit_via_hundi == 1, na.rm = TRUE)
        ),
        
        remit_use_consumption_any = as.integer(
          any(remit_use_consumption == 1, na.rm = TRUE)
        ),
        
        remit_use_education_any = as.integer(
          any(remit_use_education == 1, na.rm = TRUE)
        ),
        
        remit_use_business_any = as.integer(
          any(remit_use_business == 1, na.rm = TRUE)
        ),
        
        mig_cost_financed_by_loan_any = as.integer(
          any(mig_cost_loan_flag == 1, na.rm = TRUE)
        ),
        
        .groups = "drop"
      )
    
    ##############################################################################
    # 5. BALANCE TO FULL HH-YEAR UNIVERSE
    ##############################################################################
    
    idmap_path <- file.path(base_in, "id_match_long.csv")
    
    if (file.exists(idmap_path)) {
      id_match <- read_csv_q(idmap_path)
    } else {
      warning("id_match_long.csv not found; output covers only migrant HHs.")
      id_match <- hh_agg %>% distinct(hhid, year)
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
    
    migration_hh_year <- id_min %>%
      left_join(hh_agg, by = c("hhid", "year")) %>%
      mutate(
        has_migrant = coalesce(has_migrant, 0L),
        has_migrant_internal = coalesce(has_migrant_internal, 0L),
        
        remit_received = coalesce(remit_received, 0L),
        remit_amount_12m_rs = coalesce(remit_amount_12m_rs, 0),
        remit_amount_intl_12m_rs = coalesce(remit_amount_intl_12m_rs, 0),
        
        remit_via_formal_any = coalesce(remit_via_formal_any, 0L),
        remit_via_hundi_any = coalesce(remit_via_hundi_any, 0L),
        
        remit_use_consumption_any = coalesce(remit_use_consumption_any, 0L),
        remit_use_education_any = coalesce(remit_use_education_any, 0L),
        remit_use_business_any = coalesce(remit_use_business_any, 0L),
        
        mig_cost_financed_by_loan_any = coalesce(
          mig_cost_financed_by_loan_any,
          0L
        )
      )
    
    ##############################################################################
    # 6. FINAL COLUMN ORDER
    ##############################################################################
    
    id_cols <- c(
      "hhid", "year", "wt_hh",
      intersect(
        c(
          "psu", "district", "vdc",
          "vmun_code", "lgname", "district77", "district_name",
          "s00q03a", "s00q03b", "s00q03c"
        ),
        names(migration_hh_year)
      )
    )
    
    outcome_cols <- c(
      "has_migrant",
      "has_migrant_internal",
      
      "remit_received",
      "remit_amount_12m_rs",
      "remit_amount_intl_12m_rs",
      
      "remit_via_formal_any",
      "remit_via_hundi_any",
      
      "remit_use_consumption_any",
      "remit_use_education_any",
      "remit_use_business_any",
      
      "mig_cost_financed_by_loan_any"
    )
    
    migration_hh_year <- migration_hh_year %>%
      select(any_of(id_cols), any_of(outcome_cols)) %>%
      arrange(hhid, year) %>%
      mutate(
        across(
          where(is.numeric),
          ~ if_else(is.nan(.x), NA_real_, .x)
        )
      )
    
    write_csv(
      migration_hh_year,
      file.path(base_out, "migration_hh_year.csv"),
      na = ""
    )
    
    ##############################################################################
    # 7. CODEBOOK
    ##############################################################################
    
    migration_codebook <- tribble(
      ~variable, ~unit, ~reference, ~source, ~definition,
      
      "has_migrant", "HH × year", "present", "Section 11 presence",
      "1 if household has at least one migrant, either internal or overseas; 0 otherwise.",
      
      "has_migrant_internal", "HH × year", "present", "s11q02 = Locally",
      "1 if household has at least one internal migrant within Nepal; 0 otherwise.",
      
      "remit_received", "HH × year", "12 months", "s11q07a",
      "1 if any migrant sent remittance to the household in the past 12 months; 0 otherwise.",
      
      "remit_amount_12m_rs", "HH × year", "12 months", "sum s11q07c",
      "Total remittance amount received by the household from all migrants in the past 12 months, in rupees.",
      
      "remit_amount_intl_12m_rs", "HH × year", "12 months", "sum s11q07c for overseas migrants",
      "Total remittance amount received from international migrants only in the past 12 months, in rupees.",
      
      "remit_via_formal_any", "HH × year", "12 months", "s11q07d_1",
      "1 if any migrant sent remittance through bank or IME/formal channel; 0 otherwise.",
      
      "remit_via_hundi_any", "HH × year", "12 months", "s11q07d_3",
      "1 if any migrant sent remittance through hundi/informal channel; 0 otherwise.",
      
      "remit_use_consumption_any", "HH × year", "12 months", "s11q07e_1 or s11q07e_12",
      "1 if any remittance was used for non-durable consumption or ceremonies; 0 otherwise.",
      
      "remit_use_education_any", "HH × year", "12 months", "s11q07e_3",
      "1 if any remittance was used for education expenses; 0 otherwise.",
      
      "remit_use_business_any", "HH × year", "12 months", "s11q07e_5 to s11q07e_9",
      "1 if any remittance was used for productive investment, including business inputs, business equipment, land, livestock, or other business purposes; 0 otherwise.",
      
      "mig_cost_financed_by_loan_any", "HH × year", "past migration episode", "s11q08c_1",
      "1 if any migrant in the household financed migration costs using a loan; 0 otherwise."
    )
    
    write_csv(
      migration_codebook,
      file.path(base_out, "migration_codebook.csv")
    )
    
    ##############################################################################
    # 8. CHECKS
    ##############################################################################
    
    cat("\n=============================================================\n")
    cat("migration_migrant_year.csv:", nrow(migrant_year_out),
        "rows, one per migrant × year\n")
    cat("migration_hh_year.csv:", nrow(migration_hh_year),
        "rows, one per HH × year\n\n")
    
    cat("Rows per year:\n")
    migration_hh_year %>%
      count(year) %>%
      print()
    
    cat("\n---- HH migration and remittance prevalence ----\n")
    migration_hh_year %>%
      group_by(year) %>%
      summarise(
        n_hh = n(),
        share_has_migrant = round(mean(has_migrant, na.rm = TRUE), 3),
        share_has_internal_migrant = round(mean(has_migrant_internal, na.rm = TRUE), 3),
        share_remit_received = round(mean(remit_received, na.rm = TRUE), 3),
        mean_remit_rs_all = round(mean(remit_amount_12m_rs, na.rm = TRUE), 0),
        mean_remit_rs_cond = round(
          mean(remit_amount_12m_rs[remit_received == 1], na.rm = TRUE),
          0
        ),
        share_formal = round(mean(remit_via_formal_any, na.rm = TRUE), 3),
        share_hundi = round(mean(remit_via_hundi_any, na.rm = TRUE), 3),
        share_mig_cost_loan = round(mean(mig_cost_financed_by_loan_any, na.rm = TRUE), 3),
        .groups = "drop"
      ) %>%
      print(width = Inf)
    
    cat("\n---- Remittance use among remittance receivers ----\n")
    migration_hh_year %>%
      filter(remit_received == 1) %>%
      summarise(
        n_receivers = n(),
        share_consumption = round(mean(remit_use_consumption_any, na.rm = TRUE), 3),
        share_education = round(mean(remit_use_education_any, na.rm = TRUE), 3),
        share_business = round(mean(remit_use_business_any, na.rm = TRUE), 3)
      ) %>%
      print(width = Inf)
    
    cat("\n---- Migrant-level destination check ----\n")
    migrant_year_out %>%
      summarise(
        n_migrants = n(),
        share_internal = round(mean(is_internal == 1, na.rm = TRUE), 3),
        share_international = round(mean(is_international == 1, na.rm = TRUE), 3),
        share_india = round(mean(dest_india == 1, na.rm = TRUE), 3),
        share_gulf = round(mean(dest_gulf == 1, na.rm = TRUE), 3),
        share_malaysia = round(mean(dest_malaysia == 1, na.rm = TRUE), 3),
        share_other_country = round(mean(dest_other_country == 1, na.rm = TRUE), 3)
      ) %>%
      print(width = Inf)
    
    cat("\n---- Full HH-year outcome summary ----\n")
    migration_hh_year %>%
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