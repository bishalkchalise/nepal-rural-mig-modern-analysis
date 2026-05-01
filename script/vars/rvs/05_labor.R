      ##############################################################################
      # NRVS STAGE 2: LABOUR OUTCOMES — TRIMMED
      ##############################################################################
      # Keeps:
      #   - s07q06 employment-mix shares (4 categories, as HH-level shares of jobs)
      #   - s08 total wage income (daily + long-term + contract combined)
      #   - s08q02 sector shares, grouped into composite buckets
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
        stringr::str_detect(s, paste(patterns, collapse = "|"))
      }
      
      nan_to_na <- function(df) {
        df %>% mutate(across(where(is.numeric),
                             ~ dplyr::if_else(is.nan(.x), NA_real_, .x)))
      }
      
      ##############################################################################
      # 1. ROSTER — minimal (just to identify HHs and count members)
      ##############################################################################
      
      roster <- sec1 %>%
        select(hhid, year, member_id)
      
      ##############################################################################
      # 2. SECTION 7 — job classification (the 4 s07q06 categories)
      ##############################################################################
      
      # Days worked per job (needed for daily-wage income calculation)
      month_day_cols <- paste0("s07q04_", 1:14)
      
      jobs <- sec7 %>%
        mutate(
          days_worked = rowSums(across(any_of(month_day_cols), as_num), na.rm = TRUE),
          is_wage_agri    = as.integer(tolower(s07q06) == "wage employment in agriculture"),
          is_wage_nonagri = as.integer(tolower(s07q06) == "wage employment not in agriculture"),
          is_self_agri    = as.integer(tolower(s07q06) == "self employment in agriculture"),
          is_self_nonagri = as.integer(tolower(s07q06) == "self employment not in agriculture")
        )
      
      ##############################################################################
      # 3. SECTION 8 — wage income (combined) + sector grouping
      ##############################################################################
      
      wages <- sec8 %>%
        mutate(
          contract     = tolower(as.character(s08q05)),
          is_daily     = as.integer(stringr::str_detect(contract, "daily")),
          is_longterm  = as.integer(stringr::str_detect(contract, "long")),
          is_piece     = as.integer(stringr::str_detect(contract, "contract|piece")),
          
          daily_wage     = coalesce(as_num(s08q06),  0),
          monthly_base   = coalesce(as_num(s08q12a), 0),
          travel_al      = coalesce(as_num(s08q12b), 0),
          bonus          = coalesce(as_num(s08q12c), 0),
          uniform_al     = coalesce(as_num(s08q12d), 0),
          other_pay      = coalesce(as_num(s08q12e), 0),
          contract_value = coalesce(as_num(s08q13),  0),
          
          longterm_income_rs = dplyr::if_else(
            is_longterm == 1,
            12 * monthly_base + travel_al + bonus + uniform_al + other_pay,
            0
          ),
          contract_income_rs = dplyr::if_else(is_piece == 1, contract_value, 0),
          
          # Sector grouping — straight to composites (Clark-Fisher 3-sector)
          sec_primary = as.integer(match_any(s08q02, "^agriculture|fishing|mining")),
          sec_secondary = as.integer(match_any(s08q02,
                                               "manufacturing|electricity|gas.*water|construction")),
          sec_tertiary = as.integer(match_any(s08q02,
                                              "wholesale|retail|hotel|restaurant|transport|financial|real estate|business activities|public admin|^education|health.*social|community.*social|personal service"))
        )
      
      ##############################################################################
      # 4. DAILY-WAGE INCOME — link Section 7 days × Section 8 daily wage
      ##############################################################################
      
      if ("jobid" %in% names(sec7) && "wagejobid" %in% names(sec8)) {
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
        warning("Cannot link Section 7 days to Section 8 wages by jobid; ",
                "daily wage income will be 0.")
        daily_income <- wages %>% distinct(hhid, year, member_id) %>%
          mutate(daily_income_rs_member = 0)
      }
      
      ##############################################################################
      # 5. MEMBER-YEAR AGGREGATION
      ##############################################################################
      
      member_jobs <- jobs %>%
        group_by(hhid, year, member_id) %>%
        summarise(
          n_jobs           = dplyr::n(),
          n_wage_agri      = sum(is_wage_agri,    na.rm = TRUE),
          n_wage_nonagri   = sum(is_wage_nonagri, na.rm = TRUE),
          n_self_agri      = sum(is_self_agri,    na.rm = TRUE),
          n_self_nonagri   = sum(is_self_nonagri, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(worked = 1L)
      
      member_wages <- wages %>%
        group_by(hhid, year, member_id) %>%
        summarise(
          n_wage_jobs               = dplyr::n(),
          longterm_income_rs_member = sum(longterm_income_rs, na.rm = TRUE),
          contract_income_rs_member = sum(contract_income_rs, na.rm = TRUE),
          has_sec_primary   = as.integer(any(sec_primary   == 1, na.rm = TRUE)),
          has_sec_secondary = as.integer(any(sec_secondary == 1, na.rm = TRUE)),
          has_sec_tertiary  = as.integer(any(sec_tertiary  == 1, na.rm = TRUE)),
          .groups = "drop"
        )
      
      member_year <- roster %>%
        left_join(member_jobs,  by = c("hhid", "year", "member_id")) %>%
        left_join(member_wages, by = c("hhid", "year", "member_id")) %>%
        left_join(daily_income, by = c("hhid", "year", "member_id")) %>%
        mutate(
          worked = coalesce(worked, 0L),
          across(c(n_jobs, n_wage_agri, n_wage_nonagri, n_self_agri, n_self_nonagri,
                   n_wage_jobs, longterm_income_rs_member, contract_income_rs_member,
                   daily_income_rs_member,
                   has_sec_primary, has_sec_secondary, has_sec_tertiary),
                 ~ coalesce(., 0)),
          wage_total_income_member = longterm_income_rs_member +
            contract_income_rs_member +
            daily_income_rs_member
        )
      
      write_csv(member_year, file.path(base_out, "labour_member_year.csv"), na = "")
      
      ##############################################################################
      # 6. HH × YEAR AGGREGATION — shares + total wage income
      ##############################################################################
      
      hh_agg <- member_year %>%
        group_by(hhid, year) %>%
        summarise(
          # Total jobs and counts by s07q06 category
          n_jobs_total       = sum(n_jobs,         na.rm = TRUE),
          n_jobs_wage_agri   = sum(n_wage_agri,    na.rm = TRUE),
          n_jobs_wage_nonagri= sum(n_wage_nonagri, na.rm = TRUE),
          n_jobs_self_agri   = sum(n_self_agri,    na.rm = TRUE),
          n_jobs_self_nonagri= sum(n_self_nonagri, na.rm = TRUE),
          
          # Total wage income (combined: daily + long-term + contract)
          wage_total_income_12m_rs = sum(wage_total_income_member, na.rm = TRUE),
          n_wage_jobs_hh           = sum(n_wage_jobs, na.rm = TRUE),
          
          # Sector flags (any member in each composite)
          wage_sector_primary_any   = as.integer(any(has_sec_primary   == 1, na.rm = TRUE)),
          wage_sector_secondary_any = as.integer(any(has_sec_secondary == 1, na.rm = TRUE)),
          wage_sector_tertiary_any  = as.integer(any(has_sec_tertiary  == 1, na.rm = TRUE)),
          
          .groups = "drop"
        ) %>%
        mutate(
          # SHARES of HH's job portfolio in each s07q06 category
          share_wage_agri    = dplyr::if_else(n_jobs_total > 0, n_jobs_wage_agri    / n_jobs_total, NA_real_),
          share_wage_nonagri = dplyr::if_else(n_jobs_total > 0, n_jobs_wage_nonagri / n_jobs_total, NA_real_),
          share_self_agri    = dplyr::if_else(n_jobs_total > 0, n_jobs_self_agri    / n_jobs_total, NA_real_),
          share_self_nonagri = dplyr::if_else(n_jobs_total > 0, n_jobs_self_nonagri / n_jobs_total, NA_real_),
          
          hh_has_wage_job = as.integer(n_wage_jobs_hh > 0)
        )
      
      ##############################################################################
      # 7. BALANCE TO FULL HH UNIVERSE + GEOGRAPHY
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
          across(c(n_jobs_total, n_jobs_wage_agri, n_jobs_wage_nonagri,
                   n_jobs_self_agri, n_jobs_self_nonagri,
                   n_wage_jobs_hh, hh_has_wage_job,
                   wage_total_income_12m_rs,
                   wage_sector_primary_any, wage_sector_secondary_any,
                   wage_sector_tertiary_any),
                 ~ coalesce(., 0))
        ) %>%
        nan_to_na()
      
      # Final column order
      id_cols <- c("hhid", "year", "wt_hh",
                   intersect(c("psu", "district", "vdc",
                               "vmun_code", "lgname", "district77", "district_name",
                               "s00q03a", "s00q03b", "s00q03c"),
                             names(labour_hh_year)))
      
      outcome_cols <- c(
        # Job portfolio shares (s07q06)
        "n_jobs_total",
        "share_wage_agri", "share_wage_nonagri",
        "share_self_agri", "share_self_nonagri",
        # Wage income (combined)
        "hh_has_wage_job", "n_wage_jobs_hh", "wage_total_income_12m_rs",
        # Sector composites (s08q02)
        "wage_sector_primary_any", "wage_sector_secondary_any", "wage_sector_tertiary_any"
      )
      
      labour_hh_year <- labour_hh_year %>%
        select(all_of(id_cols), all_of(outcome_cols)) %>%
        arrange(hhid, year)
      
      write_csv(labour_hh_year, file.path(base_out, "labour_hh_year.csv"), na = "")
      
      ##############################################################################
      # 8. CODEBOOK
      ##############################################################################
      
      codebook <- tribble(
        ~variable,                    ~unit,        ~reference,   ~source,         ~definition,
        "n_jobs_total",               "HH × year", "12 months",  "Section 7",     "Total number of jobs across all HH members.",
        "share_wage_agri",            "HH × year", "12 months",  "s07q06",        "Share of HH's jobs that are wage employment in agriculture.",
        "share_wage_nonagri",         "HH × year", "12 months",  "s07q06",        "Share of HH's jobs that are wage employment not in agriculture.",
        "share_self_agri",            "HH × year", "12 months",  "s07q06",        "Share of HH's jobs that are self-employment in agriculture.",
        "share_self_nonagri",         "HH × year", "12 months",  "s07q06",        "Share of HH's jobs that are self-employment not in agriculture.",
        "hh_has_wage_job",            "HH × year", "12 months",  "Section 8",     "1 if any HH member has a Section 8 wage job record.",
        "n_wage_jobs_hh",             "HH × year", "12 months",  "Section 8",     "Total number of Section 8 wage jobs in the HH.",
        "wage_total_income_12m_rs",   "HH × year", "12 months",  "s08q06/12/13",  "Total wage Rs. = daily (s08q06×days) + long-term (12·s08q12a + s08q12b..e) + contract (s08q13).",
        "wage_sector_primary_any",    "HH × year", "12 months",  "s08q02",        "1 if any member has a wage job in primary sector (Agriculture, Fishing, Mining).",
        "wage_sector_secondary_any",  "HH × year", "12 months",  "s08q02",        "1 if any member in secondary sector (Manufacturing, Utilities, Construction).",
        "wage_sector_tertiary_any",   "HH × year", "12 months",  "s08q02",        "1 if any member in tertiary sector (Trade, Hotels, Transport, Finance, Public, Community)."
      )
      write_csv(codebook, file.path(base_out, "labour_codebook.csv"))
      
      ##############################################################################
      # 9. SANITY REPORT
      ##############################################################################
      
      cat("\n=============================================================\n")
      cat("labour_member_year.csv:", nrow(member_year), "rows\n")
      cat("labour_hh_year.csv:    ", nrow(labour_hh_year), "rows\n\n")
      
      cat("---- Mean job-portfolio shares by year ----\n")
      labour_hh_year %>%
        group_by(year) %>%
        summarise(
          n_hh             = dplyr::n(),
          mn_share_w_agri  = round(mean(share_wage_agri,    na.rm = TRUE), 3),
          mn_share_w_nonag = round(mean(share_wage_nonagri, na.rm = TRUE), 3),
          mn_share_s_agri  = round(mean(share_self_agri,    na.rm = TRUE), 3),
          mn_share_s_nonag = round(mean(share_self_nonagri, na.rm = TRUE), 3),
          .groups = "drop"
        ) %>% print()
      
      cat("\n---- Wage income (HHs with a wage job) ----\n")
      labour_hh_year %>%
        group_by(year) %>%
        summarise(
          share_wage_job = round(mean(hh_has_wage_job == 1, na.rm = TRUE), 3),
          mean_wage_rs   = round(mean(wage_total_income_12m_rs[hh_has_wage_job == 1],
                                      na.rm = TRUE), 0),
          median_wage_rs = round(median(wage_total_income_12m_rs[hh_has_wage_job == 1],
                                        na.rm = TRUE), 0),
          .groups = "drop"
        ) %>% print()
      
      cat("\n---- Sector prevalence (share of HHs with a wage job) ----\n")
      labour_hh_year %>%
        filter(hh_has_wage_job == 1) %>%
        group_by(year) %>%
        summarise(
          share_primary   = round(mean(wage_sector_primary_any,   na.rm = TRUE), 3),
          share_secondary = round(mean(wage_sector_secondary_any, na.rm = TRUE), 3),
          share_tertiary  = round(mean(wage_sector_tertiary_any,  na.rm = TRUE), 3),
          .groups = "drop"
        ) %>% print()
      cat("=============================================================\n")