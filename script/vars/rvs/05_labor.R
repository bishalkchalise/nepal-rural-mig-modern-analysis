    ##############################################################################
    # NRVS STAGE 2: LABOUR OUTCOMES (MEMBER × YEAR  and  HH × YEAR)
    ##############################################################################
    #
    # Reads Stage-1 raw CSVs for Section 1 (roster), Section 7 (jobs & time use),
    # and Section 8 (wage jobs). Produces two outputs:
    #
    #   1. labour_member_year.csv — one row per (hhid, member_id, year)
    #        Member demographics (age, sex) + their labour-market engagement
    #        aggregated across this member's jobs that year.
    #
    #   2. labour_hh_year.csv — one row per (hhid, year), 36 outcome columns.
    #        Aggregated across all members. Mergeable with consumption /
    #        enterprise / agriculture / migration outputs.
    #
    # Value-code verification (from user's diagnostic on real data):
    #   s07q01a     "Yes"/"No" — wave 2/3 only (2016 has no such column)
    #   s07q06      "Self Employment in Agriculture" / "Wage Employment in
    #               Agriculture" / "Self Employment Not in Agriculture" /
    #               "Wage Employment Not in Agriculture" — all three waves
    #   s07q03_1..14   integer 1/NA flags for worked in each of 14 Nepali months
    #   s07q04_1..14   integer days worked in each month
    #   s07q05      numeric hours per day
    #   s07q03a     "In Nepal" / "Outside Nepal" — wave 2/3 only
    #   s07q03b_i   factor — reason for returning from foreign work (rare)
    #
    #   s08q02      18-sector factor labels (Agriculture, Manufacturing, etc.)
    #   s08q05      "Daily Basis" / "Long Term Basis" / "Contract/Piece Rate"
    #   s08q06      daily wage (Rs.) — only daily-basis workers
    #   s08q12a..e  monthly salary + allowances — only long-term workers
    #   s08q13      contract value — only piece-rate workers
    #   s08q10      near-empty (53 rows across 3 waves) → NOT used
    #
    #   Roster s01q02 = Sex ("Male"/"Female"/"Third Gender"), s01q03 = Age.
    #
    # Design decisions:
    #   - "Worker" in 2016 is anyone with a Section 7 row (since s07q01a
    #     doesn't exist in 2016; appearing in Section 7 implies working).
    #   - Foreign-work columns are NA for 2016 (no data).
    #   - s08q10 skipped — nearly empty, would be noise.
    #   - Long-term wage income = 12 × s08q12a + s08q12b + s08q12c + s08q12d + s08q12e
    #     (annualised monthly + allowances + bonus + uniform + other payment).
    #   - Daily wage income = s08q06 × days_worked (where days_worked comes from
    #     the matching Section 7 job via hhid + member_id + jobid).
    #   - Contract income = s08q13.
    #   - Child worker = anyone in Section 7 whose roster age is 5–14.
    #   - Female LFP = share of female 15–64 members who worked.
    #
    ##############################################################################
    
    library(tidyverse)
    library(fs)
    
    base_in  <- "data/raw/RVS Data/clean"
    base_out <- "data/clean/rvs_outcomes"
    dir_create(base_out, recurse = TRUE)
    
    read_csv_q <- function(p) read_csv(p, show_col_types = FALSE, progress = FALSE)
    
    sec1 <- read_csv_q(file.path(base_in, "roster",     "section_1.csv"))
    sec7 <- read_csv_q(file.path(base_in, "labor_jobs", "section_7.csv"))
    sec8 <- read_csv_q(file.path(base_in, "wage_jobs",  "section_8.csv"))
    
    
    ##############################################################################
    # HELPERS
    ##############################################################################
    
    as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
    
    match_any <- function(x, patterns) {
      s <- tolower(as.character(x))
      s <- dplyr::coalesce(s, "")
      pattern <- paste(patterns, collapse = "|")
      stringr::str_detect(s, pattern)
    }
    
    # NaN-to-NA sweep (mean(na.rm=TRUE) of empty group returns NaN)
    nan_to_na <- function(df) {
      df %>% mutate(across(where(is.numeric),
                           ~ dplyr::if_else(is.nan(.x), NA_real_, .x)))
    }
    
    
    ##############################################################################
    # 1. ROSTER PREP — keep minimal demographic columns
    ##############################################################################
    
    roster <- sec1 %>%
      mutate(
        age         = as_num(s01q03),
        sex         = as.character(s01q02),
        is_male     = as.integer(match_any(sex, "^male$")),
        is_female   = as.integer(match_any(sex, "^female$")),
        is_working_age  = as.integer(!is.na(age) & age >= 15 & age <= 64),
        is_child_5_14   = as.integer(!is.na(age) & age >= 5  & age <= 14),
        is_female_wa    = as.integer(is_female == 1 & is_working_age == 1)
      ) %>%
      select(hhid, year, member_id, age, sex, is_male, is_female,
             is_working_age, is_child_5_14, is_female_wa)
    
    
    ##############################################################################
    # 2. SECTION 7 PREP — jobs with days/hours + classification
    ##############################################################################
    
    # Sum days across 14 month-columns and compute hours = days × s07q05 per job.
    month_day_cols <- paste0("s07q04_", 1:14)
    month_flag_cols <- paste0("s07q03_", 1:14)
    
    jobs <- sec7 %>%
      mutate(
        # Days worked: sum of s07q04_1..14
        days_worked = rowSums(across(any_of(month_day_cols), as_num), na.rm = TRUE),
        # Months worked: sum of non-zero s07q03_1..14 flags
        months_worked = rowSums(across(any_of(month_flag_cols),
                                       ~ as.integer(!is.na(.x) & as_num(.x) > 0)),
                                na.rm = TRUE),
        hours_per_day = as_num(s07q05),
        hours_worked  = days_worked * coalesce(hours_per_day, 0),
        
        # Job type from s07q06
        is_wage_agri    = as.integer(match_any(s07q06, "wage.*agricultur")) &
          !as.integer(match_any(s07q06, "not in agricultur")),
        is_wage_nonagri = as.integer(match_any(s07q06, "wage.*not in agricultur")),
        is_self_agri    = as.integer(match_any(s07q06, "self.*in agricultur")) &
          !as.integer(match_any(s07q06, "not in agricultur")),
        is_self_nonagri = as.integer(match_any(s07q06, "self.*not in agricultur")),
        
        # Foreign work (wave 2/3 only)
        is_foreign_work = as.integer(match_any(s07q03a, "outside")),
        is_returned_foreign = as.integer(!is.na(s07q03b_i))
      )
    
    # Fix the and-not logic: str_detect with "not in" substring breaks simple
    # regex approach. Explicit remap:
    jobs <- jobs %>%
      mutate(
        is_wage_agri    = as.integer(tolower(s07q06) == "wage employment in agriculture"),
        is_wage_nonagri = as.integer(tolower(s07q06) == "wage employment not in agriculture"),
        is_self_agri    = as.integer(tolower(s07q06) == "self employment in agriculture"),
        is_self_nonagri = as.integer(tolower(s07q06) == "self employment not in agriculture")
      )
    
    
    ##############################################################################
    # 3. SECTION 8 PREP — wage job income
    ##############################################################################
    
    # Per-row wage income by contract type
    wages <- sec8 %>%
      mutate(
        contract     = tolower(as.character(s08q05)),
        is_daily     = as.integer(stringr::str_detect(contract, "daily")),
        is_longterm  = as.integer(stringr::str_detect(contract, "long")),
        is_piece     = as.integer(stringr::str_detect(contract, "contract|piece")),
        
        daily_wage   = coalesce(as_num(s08q06),  0),
        monthly_base = coalesce(as_num(s08q12a), 0),
        travel_al    = coalesce(as_num(s08q12b), 0),
        bonus        = coalesce(as_num(s08q12c), 0),
        uniform_al   = coalesce(as_num(s08q12d), 0),
        other_pay    = coalesce(as_num(s08q12e), 0),
        contract_value = coalesce(as_num(s08q13), 0),
        
        # Long-term annual income formula
        longterm_income_rs = dplyr::if_else(
          is_longterm == 1,
          12 * monthly_base + travel_al + bonus + uniform_al + other_pay,
          0
        ),
        # Contract income
        contract_income_rs = dplyr::if_else(is_piece == 1, contract_value, 0),
        
        # Sector classification — 11 specific buckets + 3 composites
        sec_label = tolower(as.character(s08q02)),
        sec_agri         = as.integer(match_any(s08q02, "^agriculture")),
        sec_fishing_mining = as.integer(match_any(s08q02, "fishing|mining")),
        sec_manufacturing  = as.integer(match_any(s08q02, "manufacturing")),
        sec_utilities      = as.integer(match_any(s08q02, "electricity|gas.*water")),
        sec_construction   = as.integer(match_any(s08q02, "construction")),
        sec_trade          = as.integer(match_any(s08q02, "wholesale|retail")),
        sec_hotels         = as.integer(match_any(s08q02, "hotel|restaurant")),
        sec_transport      = as.integer(match_any(s08q02, "transport")),
        sec_finance        = as.integer(match_any(s08q02, "financial|real estate|business activities")),
        sec_public         = as.integer(match_any(s08q02, "public admin|^education|health.*social")),
        sec_community      = as.integer(match_any(s08q02, "community.*social|personal service"))
      )
    
    
    ##############################################################################
    # 4. DAILY-WAGE INCOME — JOIN SECTION 7 DAYS TO SECTION 8 WAGES
    ##############################################################################
    #
    # Daily-basis wage-income = daily_wage × days_worked.
    # Section 8 gives daily_wage, Section 7 gives days_worked per job.
    # Keys: (hhid, member_id, jobid).
    
    if ("jobid" %in% names(sec7) && "wagejobid" %in% names(sec8)) {
      # wagejobid in sec8 links to jobid in sec7 via (hhid, member_id)
      daily_income <- wages %>%
        filter(is_daily == 1) %>%
        select(hhid, year, member_id, wagejobid, daily_wage) %>%
        left_join(
          jobs %>% select(hhid, year, member_id, jobid, days_worked),
          by = c("hhid", "year", "member_id", "wagejobid" = "jobid")
        ) %>%
        mutate(daily_income_rs = daily_wage * coalesce(days_worked, 0)) %>%
        group_by(hhid, year, member_id) %>%
        summarise(daily_income_rs_member = sum(daily_income_rs, na.rm = TRUE),
                  .groups = "drop")
    } else {
      # Fallback: daily_income unavailable; use zero and note in codebook.
      warning("Cannot link Section 7 days to Section 8 wages by jobid; ",
              "daily wage income will be 0.")
      daily_income <- wages %>% distinct(hhid, year, member_id) %>%
        mutate(daily_income_rs_member = 0)
    }
    
    ##############################################################################
    # 4b. JOB-YEAR FILE — one row per (hhid, member_id, jobid, year)
    ##############################################################################
    #
    # Richest level of detail: preserves the full Section 7 job record plus the
    # Section 8 wage-job attributes (sector, contract type, computed income) for
    # jobs that have a matching wage record. Also joins roster age/sex.
    
    idmap_path <- file.path(base_in, "id_match_long.csv")
    if (file.exists(idmap_path)) {
      id_match <- read_csv_q(idmap_path)
    } else {
      warning("id_match_long.csv not found; output may be incomplete.")
      id_match <- tibble(hhid = integer(), year = integer())
    }
    
    
    # Section 8 minimal view for joining onto Section 7 by (hhid, year, member_id,
    # jobid = wagejobid)
    wage_join <- wages %>%
      select(hhid, year, member_id,
             wagejobid, sec_label, contract,
             is_daily, is_longterm, is_piece,
             daily_wage, longterm_income_rs, contract_income_rs,
             sec_agri, sec_fishing_mining, sec_manufacturing,
             sec_utilities, sec_construction, sec_trade, sec_hotels,
             sec_transport, sec_finance, sec_public, sec_community) %>%
      rename(jobid = wagejobid)
    
    labour_job_year <- jobs %>%
      select(hhid, year, member_id, jobid,
             s07q06,
             is_wage_agri, is_wage_nonagri, is_self_agri, is_self_nonagri,
             days_worked, months_worked, hours_per_day, hours_worked,
             is_foreign_work, is_returned_foreign) %>%
      left_join(wage_join, by = c("hhid", "year", "member_id", "jobid")) %>%
      left_join(
        roster %>% select(hhid, year, member_id, age, sex,
                          is_working_age, is_child_5_14),
        by = c("hhid", "year", "member_id")
      ) %>%
      # Compute daily-wage income per job row for jobs that match a wage record
      mutate(
        daily_income_rs = dplyr::if_else(
          coalesce(is_daily, 0L) == 1,
          coalesce(daily_wage, 0) * coalesce(days_worked, 0),
          0
        ),
        job_total_income_rs = coalesce(daily_income_rs, 0) +
          coalesce(longterm_income_rs, 0) +
          coalesce(contract_income_rs, 0)
      ) %>%
      # Attach geography + weight from id_match (by hhid + year)
      left_join(
        id_match %>% select(hhid, year,
                            any_of(c("wt_hh", "psu", "district",
                                     "vmun_code", "lgname", "district77",
                                     "district_name"))),
        by = c("hhid", "year")
      ) %>%
      arrange(hhid, year, member_id, jobid)
    
    write_csv(labour_job_year, file.path(base_out, "labour_job_year.csv"), na = "")
    
    
    
    ##############################################################################
    # 5. MEMBER-YEAR AGGREGATION
    ##############################################################################
    
    # A. From Section 7: for each member × year, aggregate across their jobs.
    member_jobs <- jobs %>%
      group_by(hhid, year, member_id) %>%
      summarise(
        n_jobs            = dplyr::n(),
        days_worked_total = sum(days_worked, na.rm = TRUE),
        hours_worked_total= sum(hours_worked, na.rm = TRUE),
        has_wage_agri     = as.integer(any(is_wage_agri == 1,    na.rm = TRUE)),
        has_wage_nonagri  = as.integer(any(is_wage_nonagri == 1, na.rm = TRUE)),
        has_self_agri     = as.integer(any(is_self_agri == 1,    na.rm = TRUE)),
        has_self_nonagri  = as.integer(any(is_self_nonagri == 1, na.rm = TRUE)),
        foreign_work_any  = as.integer(any(is_foreign_work == 1, na.rm = TRUE)),
        returned_foreign_any = as.integer(any(is_returned_foreign == 1, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(worked = 1L)
    
    # B. From Section 8: for each member × year, aggregate wage income + sectors.
    member_wages <- wages %>%
      group_by(hhid, year, member_id) %>%
      summarise(
        n_wage_jobs = dplyr::n(),
        longterm_income_rs_member = sum(longterm_income_rs, na.rm = TRUE),
        contract_income_rs_member = sum(contract_income_rs, na.rm = TRUE),
        has_sec_agri       = as.integer(any(sec_agri           == 1, na.rm = TRUE)),
        has_sec_fish_mine  = as.integer(any(sec_fishing_mining == 1, na.rm = TRUE)),
        has_sec_manuf      = as.integer(any(sec_manufacturing  == 1, na.rm = TRUE)),
        has_sec_utilities  = as.integer(any(sec_utilities      == 1, na.rm = TRUE)),
        has_sec_constr     = as.integer(any(sec_construction   == 1, na.rm = TRUE)),
        has_sec_trade      = as.integer(any(sec_trade          == 1, na.rm = TRUE)),
        has_sec_hotels     = as.integer(any(sec_hotels         == 1, na.rm = TRUE)),
        has_sec_transport  = as.integer(any(sec_transport      == 1, na.rm = TRUE)),
        has_sec_finance    = as.integer(any(sec_finance        == 1, na.rm = TRUE)),
        has_sec_public     = as.integer(any(sec_public         == 1, na.rm = TRUE)),
        has_sec_community  = as.integer(any(sec_community      == 1, na.rm = TRUE)),
        .groups = "drop"
      )
    
    # C. Merge: roster × jobs × wages × daily_income
    member_year <- roster %>%
      left_join(member_jobs,   by = c("hhid", "year", "member_id")) %>%
      left_join(member_wages,  by = c("hhid", "year", "member_id")) %>%
      left_join(daily_income,  by = c("hhid", "year", "member_id")) %>%
      mutate(
        worked = coalesce(worked, 0L),
        across(c(n_jobs, days_worked_total, hours_worked_total,
                 has_wage_agri, has_wage_nonagri, has_self_agri, has_self_nonagri,
                 foreign_work_any, returned_foreign_any,
                 n_wage_jobs,
                 longterm_income_rs_member, contract_income_rs_member,
                 daily_income_rs_member,
                 has_sec_agri, has_sec_fish_mine, has_sec_manuf, has_sec_utilities,
                 has_sec_constr, has_sec_trade, has_sec_hotels, has_sec_transport,
                 has_sec_finance, has_sec_public, has_sec_community),
               ~ coalesce(., 0)),
        wage_total_income_member = longterm_income_rs_member +
          contract_income_rs_member +
          daily_income_rs_member
      )
    
    write_csv(member_year, file.path(base_out, "labour_member_year.csv"), na = "")
    
    
    ##############################################################################
    # 6. HH × YEAR AGGREGATION (36 columns)
    ##############################################################################
    
    hh_agg <- member_year %>%
      group_by(hhid, year) %>%
      summarise(
        # Labour force & demographics
        hh_n_working_age       = sum(is_working_age, na.rm = TRUE),
        hh_n_workers           = sum(worked, na.rm = TRUE),
        hh_female_workers      = sum(worked == 1 & is_female == 1, na.rm = TRUE),
        hh_female_working_age  = sum(is_female_wa, na.rm = TRUE),
        hh_child_workers       = sum(worked == 1 & is_child_5_14 == 1, na.rm = TRUE),
        
        # Job portfolio
        n_jobs_total           = sum(n_jobs, na.rm = TRUE),
        n_jobs_wage_agri       = sum(has_wage_agri == 1, na.rm = TRUE),
        n_jobs_wage_nonagri    = sum(has_wage_nonagri == 1, na.rm = TRUE),
        n_jobs_self_agri       = sum(has_self_agri == 1, na.rm = TRUE),
        n_jobs_self_nonagri    = sum(has_self_nonagri == 1, na.rm = TRUE),
        
        # Intensity
        hh_total_days_worked_12m  = sum(days_worked_total, na.rm = TRUE),
        hh_total_hours_12m        = sum(hours_worked_total, na.rm = TRUE),
        
        # Wage income
        n_wage_jobs_hh          = sum(n_wage_jobs, na.rm = TRUE),
        wage_daily_income_12m_rs    = sum(daily_income_rs_member,    na.rm = TRUE),
        wage_longterm_income_12m_rs = sum(longterm_income_rs_member, na.rm = TRUE),
        wage_contract_income_12m_rs = sum(contract_income_rs_member, na.rm = TRUE),
        wage_total_income_12m_rs    = sum(wage_total_income_member,  na.rm = TRUE),
        
        # Sectors (any member with a wage job in that sector)
        wage_sector_agri          = as.integer(any(has_sec_agri      == 1, na.rm = TRUE)),
        wage_sector_fish_mine     = as.integer(any(has_sec_fish_mine == 1, na.rm = TRUE)),
        wage_sector_manufacturing = as.integer(any(has_sec_manuf     == 1, na.rm = TRUE)),
        wage_sector_utilities     = as.integer(any(has_sec_utilities == 1, na.rm = TRUE)),
        wage_sector_construction  = as.integer(any(has_sec_constr    == 1, na.rm = TRUE)),
        wage_sector_trade         = as.integer(any(has_sec_trade     == 1, na.rm = TRUE)),
        wage_sector_hotels        = as.integer(any(has_sec_hotels    == 1, na.rm = TRUE)),
        wage_sector_transport     = as.integer(any(has_sec_transport == 1, na.rm = TRUE)),
        wage_sector_finance       = as.integer(any(has_sec_finance   == 1, na.rm = TRUE)),
        wage_sector_public        = as.integer(any(has_sec_public    == 1, na.rm = TRUE)),
        wage_sector_community     = as.integer(any(has_sec_community == 1, na.rm = TRUE)),
        
        # Foreign work
        hh_foreign_worker_any      = as.integer(any(foreign_work_any == 1,
                                                    na.rm = TRUE)),
        hh_returned_foreign_any    = as.integer(any(returned_foreign_any == 1,
                                                    na.rm = TRUE)),
        
        .groups = "drop"
      ) %>%
      mutate(
        # Derived outcomes
        hh_any_worker = as.integer(hh_n_workers > 0),
        hh_has_wage_job         = as.integer(n_wage_jobs_hh > 0),
        hh_has_wage_nonagri     = as.integer(n_jobs_wage_nonagri > 0),
        hh_lfp_rate = dplyr::if_else(
          hh_n_working_age > 0,
          hh_n_workers / hh_n_working_age,
          NA_real_
        ),
        hh_female_lfp_rate = dplyr::if_else(
          hh_female_working_age > 0,
          hh_female_workers / hh_female_working_age,
          NA_real_
        ),
        hh_work_intensity_per_worker = dplyr::if_else(
          hh_n_workers > 0,
          hh_total_days_worked_12m / hh_n_workers,
          NA_real_
        ),
        # Composite sector flags. Two orthogonal classifications:
        #   Classical 3-sector (primary / secondary / tertiary): Clark-Fisher.
        #   Tradable / non-tradable: cross-location exchangeability. Agri is
        #   kept OUT of tradable because rural ag wage labour is hyper-local.
        wage_sector_primary_any = as.integer(
          wage_sector_agri == 1 | wage_sector_fish_mine == 1
        ),
        wage_sector_secondary_any = as.integer(
          wage_sector_manufacturing == 1 | wage_sector_utilities == 1 |
            wage_sector_construction == 1
        ),
        wage_sector_tertiary_any = as.integer(
          wage_sector_trade == 1 | wage_sector_hotels == 1 |
            wage_sector_transport == 1 | wage_sector_finance == 1 |
            wage_sector_public == 1 | wage_sector_community == 1
        ),
        wage_sector_tradable_any = as.integer(
          wage_sector_manufacturing == 1 | wage_sector_fish_mine == 1 |
            wage_sector_finance == 1
        ),
        wage_sector_nontradable_any = as.integer(
          wage_sector_utilities == 1 | wage_sector_construction == 1 |
            wage_sector_trade == 1 | wage_sector_hotels == 1 |
            wage_sector_transport == 1 | wage_sector_public == 1 |
            wage_sector_community == 1
        )
      )
    
    
    ##############################################################################
    # 7. BALANCE TO FULL HH UNIVERSE
    ##############################################################################
    
    idmap_path <- file.path(base_in, "id_match_long.csv")
    if (file.exists(idmap_path)) {
      id_match <- read_csv_q(idmap_path)
    } else {
      warning("id_match_long.csv not found; output covers only HHs in roster.")
      id_match <- hh_agg %>% distinct(hhid, year)
    }
    
    id_min <- id_match %>%
      select(hhid, year,
             any_of(c("wt_hh", "psu", "district", "vdc",
                      "vmun_code", "lgname", "district77", "district_name",
                      "s00q03a", "s00q03b", "s00q03c")))
    
    labour_hh_year <- id_min %>%
      left_join(hh_agg, by = c("hhid", "year")) %>%
      mutate(
        across(c(hh_n_working_age, hh_n_workers,
                 hh_female_workers, hh_female_working_age, hh_child_workers,
                 hh_any_worker,
                 n_jobs_total, n_jobs_wage_agri, n_jobs_wage_nonagri,
                 n_jobs_self_agri, n_jobs_self_nonagri,
                 hh_total_days_worked_12m, hh_total_hours_12m,
                 n_wage_jobs_hh, hh_has_wage_job, hh_has_wage_nonagri,
                 wage_daily_income_12m_rs, wage_longterm_income_12m_rs,
                 wage_contract_income_12m_rs, wage_total_income_12m_rs,
                 wage_sector_agri, wage_sector_fish_mine,
                 wage_sector_manufacturing, wage_sector_utilities,
                 wage_sector_construction, wage_sector_trade,
                 wage_sector_hotels, wage_sector_transport,
                 wage_sector_finance, wage_sector_public, wage_sector_community,
                 wage_sector_primary_any, wage_sector_secondary_any,
                 wage_sector_tertiary_any,
                 wage_sector_tradable_any, wage_sector_nontradable_any,
                 hh_foreign_worker_any, hh_returned_foreign_any),
               ~ coalesce(., 0))
      ) %>%
      nan_to_na()
    
    # Force foreign-work columns to NA for 2016 (data truly missing, not zero)
    labour_hh_year <- labour_hh_year %>%
      mutate(
        hh_foreign_worker_any   = dplyr::if_else(year == 2016, NA_real_,
                                                 as.numeric(hh_foreign_worker_any)),
        hh_returned_foreign_any = dplyr::if_else(year == 2016, NA_real_,
                                                 as.numeric(hh_returned_foreign_any))
      )
    
    # Final column order
    id_cols <- c("hhid", "year", "wt_hh",
                 intersect(c("psu", "district", "vdc",
                             "vmun_code", "lgname", "district77", "district_name",
                             "s00q03a", "s00q03b", "s00q03c"),
                           names(labour_hh_year)))
    
    outcome_cols <- c(
      # (1) LFP & demographics (6)
      "hh_any_worker", "hh_n_workers", "hh_n_working_age", "hh_lfp_rate",
      "hh_female_lfp_rate", "hh_child_workers",
      # (2) Job portfolio (5)
      "n_jobs_total",
      "n_jobs_wage_agri", "n_jobs_wage_nonagri",
      "n_jobs_self_agri", "n_jobs_self_nonagri",
      "hh_has_wage_nonagri",
      # (3) Intensity (3)
      "hh_total_days_worked_12m", "hh_total_hours_12m",
      "hh_work_intensity_per_worker",
      # (4) Wage income (4)
      "hh_has_wage_job",
      "wage_daily_income_12m_rs", "wage_longterm_income_12m_rs",
      "wage_contract_income_12m_rs", "wage_total_income_12m_rs",
      # (5) Sector dummies — 11 specific (11)
      "wage_sector_agri", "wage_sector_fish_mine",
      "wage_sector_manufacturing", "wage_sector_utilities",
      "wage_sector_construction", "wage_sector_trade",
      "wage_sector_hotels", "wage_sector_transport",
      "wage_sector_finance", "wage_sector_public", "wage_sector_community",
      # (6) Composite sectors (5)
      "wage_sector_primary_any", "wage_sector_secondary_any",
      "wage_sector_tertiary_any",
      "wage_sector_tradable_any", "wage_sector_nontradable_any",
      # (7) Foreign work (2)
      "hh_foreign_worker_any", "hh_returned_foreign_any"
    )
    
    labour_hh_year <- labour_hh_year %>%
      select(all_of(id_cols), all_of(outcome_cols)) %>%
      arrange(hhid, year)
    
    write_csv(labour_hh_year, file.path(base_out, "labour_hh_year.csv"), na = "")
    
    
    ##############################################################################
    # 8. CODEBOOK
    ##############################################################################
    
    codebook <- tribble(
      ~variable,                        ~unit,        ~reference,   ~source,                  ~definition,
      
      # LFP & demographics
      "hh_any_worker",                 "HH × year", "12 months",  "sec 7 presence",         "1 if any HH member has any Section 7 job row this year.",
      "hh_n_workers",                  "HH × year", "12 months",  "count worked members",   "Number of HH members who worked (any job) this year.",
      "hh_n_working_age",              "HH × year", "current",    "roster s01q03",          "Number of HH members aged 15–64 (working-age population denominator).",
      "hh_lfp_rate",                   "HH × year", "12 months",  "derived",                "hh_n_workers / hh_n_working_age. NA if no working-age members.",
      "hh_female_lfp_rate",            "HH × year", "12 months",  "derived",                "Share of female 15–64 members who worked. NA if no female working-age members.",
      "hh_child_workers",              "HH × year", "12 months",  "roster + s07",           "Count of workers aged 5–14 (child labour indicator).",
      
      # Job portfolio
      "n_jobs_total",                  "HH × year", "12 months",  "sum Section 7 rows",     "Total number of jobs held across all HH members.",
      "n_jobs_wage_agri",              "HH × year", "12 months",  "s07q06 category",        "Number of jobs that are wage employment in agriculture.",
      "n_jobs_wage_nonagri",           "HH × year", "12 months",  "s07q06 category",        "Number of jobs that are wage employment not in agriculture.",
      "n_jobs_self_agri",              "HH × year", "12 months",  "s07q06 category",        "Number of jobs that are self-employment in agriculture.",
      "n_jobs_self_nonagri",           "HH × year", "12 months",  "s07q06 category",        "Number of jobs that are self-employment not in agriculture.",
      "hh_has_wage_nonagri",           "HH × year", "12 months",  "derived",                "1 if any member has a non-agricultural wage job.",
      
      # Intensity
      "hh_total_days_worked_12m",      "HH × year", "12 months",  "sum s07q04_1..14",       "Total person-days of work across all members and jobs.",
      "hh_total_hours_12m",            "HH × year", "12 months",  "days × s07q05",          "Total person-hours = Σ (days × hours-per-day). Approximate; assumes hours-per-day constant.",
      "hh_work_intensity_per_worker",  "HH × year", "12 months",  "derived",                "Mean days worked per worker = hh_total_days_worked_12m / hh_n_workers. NA if no workers.",
      
      # Wage income
      "hh_has_wage_job",               "HH × year", "12 months",  "sec 8 presence",         "1 if any HH member has a Section 8 wage job record.",
      "wage_daily_income_12m_rs",      "HH × year", "12 months",  "s08q06 × days",          "Total Rs. from daily-wage jobs = s08q06 × days worked (Section 7).",
      "wage_longterm_income_12m_rs",   "HH × year", "12 months",  "s08q12a..e",             "Total Rs. from long-term jobs = 12·s08q12a + s08q12b + s08q12c + s08q12d + s08q12e.",
      "wage_contract_income_12m_rs",   "HH × year", "12 months",  "s08q13",                 "Total Rs. from contract/piece-rate jobs.",
      "wage_total_income_12m_rs",      "HH × year", "12 months",  "sum of above",           "Sum of daily + long-term + contract wage income.",
      
      # Sector dummies
      "wage_sector_agri",              "HH × year", "12 months",  "s08q02 = Agriculture",   "1 if any HH member has a wage job in agriculture.",
      "wage_sector_fish_mine",         "HH × year", "12 months",  "s08q02",                 "1 if any member in Fishing OR Mining sector.",
      "wage_sector_manufacturing",     "HH × year", "12 months",  "s08q02",                 "1 if any member in Manufacturing.",
      "wage_sector_utilities",         "HH × year", "12 months",  "s08q02",                 "1 if any member in Electricity/Gas/Water Supply.",
      "wage_sector_construction",      "HH × year", "12 months",  "s08q02",                 "1 if any member in Construction.",
      "wage_sector_trade",             "HH × year", "12 months",  "s08q02",                 "1 if any member in Wholesale/Retail Trade.",
      "wage_sector_hotels",            "HH × year", "12 months",  "s08q02",                 "1 if any member in Hotels/Restaurants.",
      "wage_sector_transport",         "HH × year", "12 months",  "s08q02",                 "1 if any member in Transport/Storage/Communications.",
      "wage_sector_finance",           "HH × year", "12 months",  "s08q02",                 "1 if any member in Financial Intermediation OR Real Estate/Business.",
      "wage_sector_public",            "HH × year", "12 months",  "s08q02",                 "1 if any member in Public Admin, Education, or Health/Social Work.",
      "wage_sector_community",         "HH × year", "12 months",  "s08q02",                 "1 if any member in Other Community/Social/Personal Services.",
      
      # Composite sectors
      "wage_sector_primary_any",       "HH × year", "12 months",  "derived",                "1 if any member in primary sector (Agriculture, Fishing, Mining) — extraction from nature.",
      "wage_sector_secondary_any",     "HH × year", "12 months",  "derived",                "1 if any member in secondary sector (Manufacturing, Utilities, Construction) — transformation of materials.",
      "wage_sector_tertiary_any",      "HH × year", "12 months",  "derived",                "1 if any member in tertiary sector (services: Trade, Hotels, Transport, Finance, Public, Community).",
      "wage_sector_tradable_any",      "HH × year", "12 months",  "derived",                "1 if any member in internationally-tradable sector: Manufacturing, Mining, Finance. Agriculture kept OUT since rural ag wage labour is hyper-local.",
      "wage_sector_nontradable_any",   "HH × year", "12 months",  "derived",                "1 if any member in location-bound sector: Utilities, Construction, Trade, Hotels, Transport, Public, Community.",
      
      # Foreign work
      "hh_foreign_worker_any",         "HH × year", "12 months",  "s07q03a",                "1 if any member worked outside Nepal this year. NA for 2016 (variable not in wave 1).",
      "hh_returned_foreign_any",       "HH × year", "12 months",  "s07q03b_i",              "1 if any member reported a reason for returning from foreign work. NA for 2016."
    )
    write_csv(codebook, file.path(base_out, "labour_codebook.csv"))
    
    
    ##############################################################################
    # 9. SANITY REPORT
    ##############################################################################
    
    cat("\n=============================================================\n")
    cat("labour_member_year.csv:", nrow(member_year), "rows (one per member × year)\n")
    cat("labour_hh_year.csv:    ", nrow(labour_hh_year), "rows (one per HH × year)\n\n")
    
    cat("---- LFP by year ----\n")
    labour_hh_year %>%
      group_by(year) %>%
      summarise(
        n_hh           = dplyr::n(),
        share_worker   = round(mean(hh_any_worker == 1, na.rm = TRUE), 3),
        mean_lfp       = round(mean(hh_lfp_rate, na.rm = TRUE), 3),
        mean_flfp      = round(mean(hh_female_lfp_rate, na.rm = TRUE), 3),
        share_child_wk = round(mean(hh_child_workers > 0, na.rm = TRUE), 3),
        share_wage_job = round(mean(hh_has_wage_job == 1, na.rm = TRUE), 3),
        mean_wage_rs   = round(mean(wage_total_income_12m_rs[hh_has_wage_job == 1],
                                    na.rm = TRUE), 0),
        .groups = "drop"
      ) %>% print()
    
    cat("\n---- Sector prevalence (of HHs with a wage job) ----\n")
    labour_hh_year %>%
      filter(hh_has_wage_job == 1) %>%
      summarise(across(starts_with("wage_sector_"),
                       ~ round(mean(.x, na.rm = TRUE), 3))) %>%
      pivot_longer(everything()) %>% print(n = 20)
    
    cat("\n---- Foreign work (2017/18 only) ----\n")
    labour_hh_year %>%
      filter(year > 2016) %>%
      group_by(year) %>%
      summarise(
        share_foreign_worker   = round(mean(hh_foreign_worker_any, na.rm = TRUE), 3),
        share_returned_foreign = round(mean(hh_returned_foreign_any, na.rm = TRUE), 3),
        .groups = "drop"
      ) %>% print()
    
    cat("\n---- Full outcome summary ----\n")
    labour_hh_year %>%
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