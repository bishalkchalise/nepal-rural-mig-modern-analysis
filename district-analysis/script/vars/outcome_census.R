    #####################################################################################################
    # NEPAL CENSUS PANEL — DISTRICT (DNAME) LEVEL
    # Produces: district-analysis/data/clean/census/outcomes_district.csv
    #
    # Source: run from repo root, e.g.
    #   source("district-analysis/script/vars/outcome_census.R")
    #
    # Structure:
    #   - Load each census, attach dname
    #   - For each topic, compute 3 per-year frames, bind them
    #   - Join all topic frames at the end
    #   - Free memory between years
    #####################################################################################################
    
    rm(list = ls()); cat("\14")
    
    suppressPackageStartupMessages({
      library(tidyverse)
      library(haven)
      library(readxl)
      library(janitor)
    })
    
    DIR_CLEAN <- "district-analysis/data/clean/census"
    dir.create(DIR_CLEAN, recursive = TRUE, showWarnings = FALSE)
    
    
    #####################################################################################################
    # SECTION 1 — Geographic mapping
    #####################################################################################################
    
    vdc_to_lg_map <- read_xlsx("data/raw/old vdc to local level.xlsx") %>%
      rename(dcode = dist)
    
    census_2011_id <- read_xlsx("data/raw/Full Census Data/Census 2011/censusid2011.xlsx") %>%
      clean_names()
    
    census_2011_mapped <- census_2011_id %>%
      left_join(vdc_to_lg_map, by = c("dname" = "dist_name", "vname" = "vname")) %>%
      select(-dist) %>%
      filter(!is.na(dname))
    
    
    #####################################################################################################
    # SECTION 2 — Load 2001 files (attach dname via VDC crosswalk)
    #####################################################################################################
    
    cat("Loading 2001...\n")
    
    ind1_01 <- read_dta(
      "data/raw/Full Census Data/Census 2001/fullpi01_full.dta",
      col_select = any_of(c("dist","vdcmun","ward",
                            "q3_sex","q4_age","q5_caste","q6_reltn","q7_relgn","q10_dibl"))
    ) %>%
      left_join(census_2011_mapped,
                by = c("dist"="dcode","vdcmun"="vdcmun","ward"="ward")) %>%
      filter(!is.na(dname))
    
    ind2_01 <- read_dta(
      "data/raw/Full Census Data/Census 2001/fullpi02_full.dta",
      col_select = any_of(c("dist","vdcmun","ward",
                            "q2_sex","q3_age",
                            "q4_bplce","q4b_vdcm","q5_durtn","q6_rstay",
                            "q7_li5ya","q7b_vdcm",
                            "q8_edutn","q9a_elvl","q10_catt","q11_msta","q12_fage",
                            "q13_stot","q13_dtot","q13_sded","q13_dded",
                            "q14_livb","q14_1sex","q14_2sex",
                            "q15_work","q17_occ1","q18_ind1","q19_esta",
                            "q21_liar"))
    ) %>%
      left_join(census_2011_mapped,
                by = c("dist"="dcode","vdcmun"="vdcmun","ward"="ward")) %>%
      filter(!is.na(dname))
    
    hh1_01 <- read_dta(
      "data/raw/Full Census Data/Census 2001/fullhi01_full.dta",
      col_select = any_of(c("dist","vdcmun","ward",
                            "q01_htyp","q02_otyp","q07_hfem","q07_lfem","q08_lvfe",
                            "q09_heco","q10_mact","q11_abst"))
    ) %>%
      left_join(census_2011_mapped,
                by = c("dist"="dcode","vdcmun"="vdcmun","ward"="ward")) %>%
      filter(!is.na(dname))
    
    hh2_01 <- read_dta(
      "data/raw/Full Census Data/Census 2001/fullhi02_full.dta",
      col_select = any_of(c("dist","vdcmun","ward",
                            "q1_wsorc","q2_cookf","q3_lighf","q4_toilf",
                            "q5a_hhfa","q5b_hhfa","q6_death"))
    ) %>%
      left_join(census_2011_mapped,
                by = c("dist"="dcode","vdcmun"="vdcmun","ward"="ward")) %>%
      filter(!is.na(dname))
    
    gc(verbose = FALSE)
    
    
    #####################################################################################################
    # SECTION 3 — Compute 2001 topics (all at dname level)
    #####################################################################################################
    
    cat("2001 topics...\n")
    
    # ---- Amenities (household q-file 2) ----
    amen_01 <- hh2_01 %>%
      group_by(dname) %>%
      summarise(
        amen_water_piped          = mean(q1_wsorc == 1, na.rm = TRUE),
        amen_water_traditional    = mean(q1_wsorc %in% c(2,3,4,5), na.rm = TRUE),
        amen_cooking_wood         = mean(q2_cookf == 1, na.rm = TRUE),
        amen_cooking_kerosene     = mean(q2_cookf == 2, na.rm = TRUE),
        amen_cooking_lpg          = mean(q2_cookf == 3, na.rm = TRUE),
        amen_cooking_biogas       = mean(q2_cookf == 4, na.rm = TRUE),
        amen_cooking_electric     = NA_real_,
        amen_cooking_modern       = mean(q2_cookf %in% c(3,4), na.rm = TRUE),
        amen_cooking_traditional  = mean(q2_cookf %in% c(1,2,5,6), na.rm = TRUE),
        amen_lighting_electricity = mean(q3_lighf == 1, na.rm = TRUE),
        amen_lighting_kerosene    = mean(q3_lighf == 2, na.rm = TRUE),
        amen_lighting_biogas      = mean(q3_lighf == 3, na.rm = TRUE),
        amen_lighting_others      = mean(q3_lighf == 4, na.rm = TRUE),
        amen_toilet_modern        = mean(q4_toilf == 1, na.rm = TRUE),
        amen_toilet_ordinary      = mean(q4_toilf == 2, na.rm = TRUE),
        amen_toilet_none          = mean(q4_toilf == 3, na.rm = TRUE),
        amen_toilet_any           = mean(q4_toilf %in% c(1,2), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2001)
    
    # ---- Housing structure (household q-file 1) ----
    housing_01 <- hh1_01 %>%
      group_by(dname) %>%
      summarise(
        housing_own                    = mean(q02_otyp == 1, na.rm = TRUE),
        housing_rented                 = mean(q02_otyp == 2, na.rm = TRUE),
        housing_foundation_modern      = NA_real_,
        housing_foundation_traditional = NA_real_,
        housing_roof_modern            = NA_real_,
        housing_roof_traditional       = NA_real_,
        .groups = "drop"
      ) %>%
      mutate(year = 2001)
    
    # ---- Female ownership + enterprise ----
    femx_01 <- hh1_01 %>%
      group_by(dname) %>%
      summarise(
        fem_ownership_house     = mean(q07_hfem == 1, na.rm = TRUE),
        fem_ownership_land      = mean(q07_lfem == 1, na.rm = TRUE),
        fem_ownership_both      = mean(q07_hfem == 1 & q07_lfem == 1, na.rm = TRUE),
        fem_ownership_livestock = mean(q08_lvfe == 1, na.rm = TRUE),
        ent_has_nonagro         = mean(q09_heco == 1, na.rm = TRUE),
        ent_cottage             = mean(q10_mact == 1, na.rm = TRUE),
        ent_trade               = mean(q10_mact == 2, na.rm = TRUE),
        ent_transport           = mean(q10_mact == 3, na.rm = TRUE),
        ent_services            = mean(q10_mact == 4, na.rm = TRUE),
        ent_other               = mean(q10_mact == 5, na.rm = TRUE),
        ent_female_owner_share  = NA_real_,
        .groups = "drop"
      ) %>%
      mutate(year = 2001)
    
    # ---- Household shock (absentee + death) ----
    shock_01 <- hh1_01 %>%
      group_by(dname) %>%
      summarise(absent_hh_share = mean(q11_abst == 1, na.rm = TRUE), .groups = "drop") %>%
      full_join(
        hh2_01 %>%
          group_by(dname) %>%
          summarise(hh_death_12m = mean(q6_death == 1, na.rm = TRUE), .groups = "drop"),
        by = "dname"
      ) %>%
      mutate(year = 2001)
    
    # Household files no longer needed after this point
    rm(hh1_01, hh2_01); gc(verbose = FALSE)
    
    # ---- Education: literacy (pop 6+) ----
    edu_lit_01 <- ind2_01 %>%
      filter(q3_age >= 6, q3_age < 150) %>%
      group_by(dname) %>%
      summarise(
        edu_literate        = mean(q8_edutn == 2, na.rm = TRUE),
        edu_literate_female = mean(q8_edutn[q2_sex == 2] == 2, na.rm = TRUE),
        edu_literate_male   = mean(q8_edutn[q2_sex == 1] == 2, na.rm = TRUE),
        .groups = "drop"
      )
    
    # ---- Education: school attendance (6-16) ----
    edu_att_01 <- ind2_01 %>%
      filter(q3_age >= 6, q3_age <= 16) %>%
      mutate(attending = coalesce(q10_catt == 1, FALSE)) %>%
      group_by(dname) %>%
      summarise(
        edu_school_attend_6_16        = mean(attending, na.rm = TRUE),
        edu_school_attend_6_16_female = mean(attending[q2_sex == 2], na.rm = TRUE),
        edu_school_attend_6_16_male   = mean(attending[q2_sex == 1], na.rm = TRUE),
        .groups = "drop"
      )
    
    # ---- Education: attainment (25+) ----
    edu_at2_01 <- ind2_01 %>%
      filter(q3_age >= 25, q3_age < 150, !is.na(q9a_elvl)) %>%
      mutate(
        # Years of schooling — academic track only
        yrs = case_when(
          q9a_elvl == 0       ~ 0,             # literate, no formal
          q9a_elvl %in% 1:10  ~ as.numeric(q9a_elvl),
          q9a_elvl == 11      ~ 10,            # SLC = completed class 10
          q9a_elvl == 12      ~ 12,            # Intermediate = +2
          q9a_elvl == 13      ~ 15,            # Bachelor's
          q9a_elvl == 14      ~ 17,            # Master's
          q9a_elvl == 15      ~ 20,            # PhD
          q9a_elvl %in% c(16, 17, 18, 19, 99) ~ NA_real_,
          TRUE                ~ NA_real_
        )
      ) %>%
      group_by(dname) %>%
      summarise(
        # Four tiers: primary / secondary (inc. lower) / higher secondary / tertiary
        edu_attain_primary_plus         = mean(q9a_elvl %in% 1:15, na.rm = TRUE),
        edu_attain_secondary_plus       = mean(q9a_elvl %in% c(6:11, 12:15), na.rm = TRUE),
        edu_attain_higher_secondary_plus = mean(q9a_elvl %in% 12:15, na.rm = TRUE),
        edu_attain_tertiary             = mean(q9a_elvl %in% 13:15, na.rm = TRUE),
        edu_years_mean                  = mean(yrs, na.rm = TRUE),
        .groups = "drop"
      )
    
    edu_01 <- edu_lit_01 %>%
      full_join(edu_att_01, by = "dname") %>%
      full_join(edu_at2_01, by = "dname") %>%
      mutate(year = 2001)
    
    rm(edu_lit_01, edu_att_01, edu_at2_01)
    
    
    # ---- Marriage ----
    mar_base_01 <- ind2_01 %>%
      filter(q3_age >= 15, q3_age < 60) %>%
      group_by(dname) %>%
      summarise(
        mar_ever_married_15_60  = mean(q11_msta %in% 2:7, na.rm = TRUE),
        mar_never_married_15_60 = mean(q11_msta == 1,    na.rm = TRUE),
        .groups = "drop"
      )
    mar_fem_01 <- ind2_01 %>%
      filter(q2_sex == 2, q3_age >= 20, q3_age <= 24,
             q11_msta %in% 2:7, !is.na(q12_fage)) %>%
      group_by(dname) %>%
      summarise(
        mar_female_age_first_mean = mean(q12_fage, na.rm = TRUE),
        mar_female_married_by_18  = mean(q12_fage < 18, na.rm = TRUE),
        mar_female_married_by_20  = mean(q12_fage < 20, na.rm = TRUE),
        .groups = "drop"
      )
    mar_01 <- mar_base_01 %>% full_join(mar_fem_01, by = "dname") %>% mutate(year = 2001)
    rm(mar_base_01, mar_fem_01)
    
    
    # ---- Fertility + mortality ----
    fert_01 <- ind2_01 %>%
      filter(q2_sex == 2, q3_age >= 15, q3_age <= 49, q11_msta %in% 2:7) %>%
      mutate(
        birth_son   = coalesce(q13_stot, 0),
        birth_dau   = coalesce(q13_dtot, 0),
        birth_total = birth_son + birth_dau,
        dead_son    = coalesce(q13_sded, 0),
        dead_dau    = coalesce(q13_dded, 0),
        dead_tot    = dead_son + dead_dau,
        birth_12m   = as.integer(q14_livb == 1)
      ) %>%
      group_by(dname) %>%
      summarise(
        fert_birth_mean           = mean(birth_total, na.rm = TRUE),
        fert_birth_son_mean       = mean(birth_son,   na.rm = TRUE),
        fert_birth_dau_mean       = mean(birth_dau,   na.rm = TRUE),
        fert_births_last12m_share = mean(birth_12m,   na.rm = TRUE),
        fert_births_last12m_rate  = mean(birth_12m,   na.rm = TRUE),
        mort_children_dead_mean   = mean(dead_tot,    na.rm = TRUE),
        mort_child_dead_any       = mean(dead_tot > 0, na.rm = TRUE),
        mort_child_death_ratio    = sum(dead_tot, na.rm = TRUE) /
          pmax(sum(birth_total, na.rm = TRUE), 1e-9),
        .groups = "drop"
      ) %>%
      mutate(year = 2001)
    
    
    # ---- Work shares ----
    work_01 <- ind2_01 %>%
      filter(q3_age > 14, q3_age < 60) %>%
      group_by(dname) %>%
      summarise(
        work_share_agriculture      = mean(q15_work == 1,     na.rm = TRUE),
        work_share_nonagriculture   = mean(q15_work %in% 2:4, na.rm = TRUE),
        work_share_wage_nonagri     = mean(q15_work == 2,     na.rm = TRUE),
        work_share_own_nonagri      = mean(q15_work == 3,     na.rm = TRUE),
        work_share_extended_econ    = mean(q15_work == 4,     na.rm = TRUE),
        work_share_job_seeking      = mean(q15_work == 5,     na.rm = TRUE),
        work_share_household_chores = mean(q15_work == 6,     na.rm = TRUE),
        work_share_student          = mean(q15_work == 7,     na.rm = TRUE),
        work_share_no_work          = mean(q15_work == 8,     na.rm = TRUE),
        work_lfp                    = mean(q15_work %in% 1:4, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2001)
    
    
    # ---- Occupation (ISCO 1-digit; 2001 lacks armed forces category) ----
    occ_01 <- ind2_01 %>%
      filter(q3_age >= 15, q3_age < 60, !is.na(q17_occ1), q17_occ1 %in% 1:9) %>%
      group_by(dname) %>%
      summarise(
        occ_share_armed_forces      = 0,
        occ_share_managers          = mean(q17_occ1 == 1, na.rm = TRUE),
        occ_share_professionals     = mean(q17_occ1 == 2, na.rm = TRUE),
        occ_share_technicians       = mean(q17_occ1 == 3, na.rm = TRUE),
        occ_share_office_assistants = mean(q17_occ1 == 4, na.rm = TRUE),
        occ_share_service_sales     = mean(q17_occ1 == 5, na.rm = TRUE),
        occ_share_agriculture       = mean(q17_occ1 == 6, na.rm = TRUE),
        occ_share_craft_trades      = mean(q17_occ1 == 7, na.rm = TRUE),
        occ_share_machine_operators = mean(q17_occ1 == 8, na.rm = TRUE),
        occ_share_elementary        = mean(q17_occ1 == 9, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2001)
    
    
    # ---- Industry (11 harmonized groups) ----
    industry_01 <- ind2_01 %>%
      filter(q3_age >= 15, q3_age < 60, !is.na(q18_ind1), q18_ind1 != 99) %>%
      group_by(dname) %>%
      summarise(
        ind_agri_forestry_fish       = mean(q18_ind1 %in% 1:2, na.rm = TRUE),
        ind_manufacturing            = mean(q18_ind1 == 4,   na.rm = TRUE),
        ind_construction             = mean(q18_ind1 == 6,   na.rm = TRUE),
        ind_wholesale_retail         = mean(q18_ind1 == 7,   na.rm = TRUE),
        ind_transport_accommodation  = mean(q18_ind1 %in% 8:9, na.rm = TRUE),
        ind_finance_real_estate_prof = mean(q18_ind1 %in% 10:11, na.rm = TRUE),
        ind_public_admin_defence     = mean(q18_ind1 == 12,  na.rm = TRUE),
        ind_education                = mean(q18_ind1 == 13,  na.rm = TRUE),
        ind_health                   = mean(q18_ind1 == 14,  na.rm = TRUE),
        ind_arts_recreation          = 0,   # bundled with "others" in 2001
        ind_others                   = mean(q18_ind1 %in% c(3, 5, 15, 16, 17), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2001)
    
    
    # ---- Employment status ----
    emp_01 <- ind2_01 %>%
      filter(q3_age >= 15, q3_age < 60, q19_esta %in% 1:4) %>%
      group_by(dname) %>%
      summarise(
        emp_share_employer             = mean(q19_esta == 1, na.rm = TRUE),
        emp_share_employee             = mean(q19_esta == 2, na.rm = TRUE),
        emp_share_self_employed        = mean(q19_esta == 3, na.rm = TRUE),
        emp_share_unpaid_family_worker = mean(q19_esta == 4, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2001)
    
    
    # ---- Migration (5-year reference period) ----
    mig_01 <- ind2_01 %>%
      filter(q3_age >= 5) %>%
      group_by(dname) %>%
      summarise(
        mig_in_share              = mean(q7_li5ya %in% c(2,3), na.rm = TRUE),
        mig_in_domestic           = mean(q7_li5ya == 2, na.rm = TRUE),
        mig_in_international      = mean(q7_li5ya == 3, na.rm = TRUE),
        mig_in_from_rural         = mean(q7_li5ya %in% c(2,3) & q7b_vdcm == 1, na.rm = TRUE),
        mig_in_from_urban         = mean(q7_li5ya %in% c(2,3) & q7b_vdcm == 2, na.rm = TRUE),
        mig_in_reason_economic    = mean(q7_li5ya %in% c(2,3) & q6_rstay %in% 1:3, na.rm = TRUE),
        mig_in_reason_noneconomic = mean(q7_li5ya %in% c(2,3) & q6_rstay %in% 4:6, na.rm = TRUE),
        mig_in_reason_study       = mean(q7_li5ya %in% c(2,3) & q6_rstay == 4, na.rm = TRUE),
        mig_in_reason_marriage    = mean(q7_li5ya %in% c(2,3) & q6_rstay == 5, na.rm = TRUE),
        mig_in_return             = NA_real_,
        .groups = "drop"
      ) %>%
      mutate(year = 2001)
    
    
    # ---- FLFP + gender gaps ----
    flfp_01 <- ind2_01 %>%
      filter(q3_age >= 15, q3_age < 60) %>%
      group_by(dname) %>%
      summarise(
        denom_f = pmax(mean(q2_sex == 2, na.rm = TRUE), 1e-9),
        denom_m = pmax(mean(q2_sex == 1, na.rm = TRUE), 1e-9),
        flfp_all            = mean(q15_work %in% 1:5 & q2_sex == 2, na.rm = TRUE) / denom_f,
        fem_employment_rate = mean(q15_work %in% 1:4 & q2_sex == 2, na.rm = TRUE) / denom_f,
        flfp_agri           = mean(q15_work == 1     & q2_sex == 2, na.rm = TRUE) / denom_f,
        flfp_nonagri        = mean(q15_work %in% 2:4 & q2_sex == 2, na.rm = TRUE) / denom_f,
        flfp_wage           = mean(q15_work == 2     & q2_sex == 2, na.rm = TRUE) / denom_f,
        flfp_chores_only    = mean(q15_work == 6     & q2_sex == 2, na.rm = TRUE) / denom_f,
        mlfp_all            = mean(q15_work %in% 1:5 & q2_sex == 1, na.rm = TRUE) / denom_m,
        mlfp_agri           = mean(q15_work == 1     & q2_sex == 1, na.rm = TRUE) / denom_m,
        mlfp_nonagri        = mean(q15_work %in% 2:4 & q2_sex == 1, na.rm = TRUE) / denom_m,
        share_women         = denom_f,
        share_men           = denom_m,
        .groups = "drop"
      ) %>%
      select(-denom_f, -denom_m) %>%
      mutate(year = 2001)
    
    
    # ---- Headship (household heads only) ----
    head_01 <- ind1_01 %>%
      filter(q6_reltn == 1) %>%
      group_by(dname) %>%
      summarise(
        head_female_share   = mean(q3_sex == 2, na.rm = TRUE),
        head_age_mean       = mean(q4_age, na.rm = TRUE),
        head_elderly_share  = mean(q4_age >= 60, na.rm = TRUE),
        head_female_elderly = mean(q3_sex == 2 & q4_age >= 60, na.rm = TRUE),
        head_young_share    = mean(q4_age < 30, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2001)
    
    
    # ---- Left-behind children ----
    left_01 <- ind2_01 %>%
      filter(q3_age < 16, !is.na(q21_liar)) %>%
      group_by(dname) %>%
      summarise(
        left_not_with_both   = mean(q21_liar != 1, na.rm = TRUE),
        left_mother_only     = mean(q21_liar == 2, na.rm = TRUE),
        left_father_only     = mean(q21_liar == 3, na.rm = TRUE),
        left_with_relatives  = mean(q21_liar == 6, na.rm = TRUE),
        left_without_parents = mean(q21_liar %in% 6:8, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2001)
    
    
    # Free 2001 individual files
    rm(ind1_01, ind2_01); gc(verbose = FALSE)
    cat("2001 done.\n")
    
    
    #####################################################################################################
    # SECTION 4 — Load 2011 files
    #####################################################################################################
    
    cat("Loading 2011...\n")
    
    ind11_1 <- read_dta("data/raw/Full Census Data/Census 2011/individual01.dta") %>%
      left_join(census_2011_mapped,
                by = c("dist"="dcode","vdcmun"="vdcmun","ward"="ward")) %>%
      filter(!is.na(dname))
    
    ind11_2 <- read_dta("data/raw/Full Census Data/Census 2011/individual02.dta") %>%
      left_join(census_2011_mapped,
                by = c("dist"="dcode","vdcmun"="vdcmun","ward"="ward")) %>%
      filter(!is.na(dname))
    
    hh_11 <- read_dta("data/raw/Full Census Data/Census 2011/household.dta") %>%
      left_join(census_2011_mapped,
                by = c("dist"="dcode","vdcmun"="vdcmun","ward"="ward")) %>%
      filter(!is.na(dname))
    if (!"h11" %in% names(hh_11)) hh_11$h11 <- NA_real_
    gc(verbose = FALSE)
    
    
    #####################################################################################################
    # SECTION 5 — Compute 2011 topics
    #####################################################################################################
    
    cat("2011 topics...\n")
    
    # ---- Amenities (2011 HH file — h07 list columns, h03/h04/h05/h06 single-value) ----
    amen_11 <- hh_11 %>%
      mutate(
        has_radio      = if_any(starts_with("h07"), ~ replace_na(.x == 1,  FALSE)),
        has_tv         = if_any(starts_with("h07"), ~ replace_na(.x == 2,  FALSE)),
        has_computer   = if_any(starts_with("h07"), ~ replace_na(.x == 4,  FALSE)),
        has_internet   = if_any(starts_with("h07"), ~ replace_na(.x == 5,  FALSE)),
        has_landline   = if_any(starts_with("h07"), ~ replace_na(.x == 6,  FALSE)),
        has_mobile     = if_any(starts_with("h07"), ~ replace_na(.x == 7,  FALSE)),
        has_car        = if_any(starts_with("h07"), ~ replace_na(.x == 8,  FALSE)),
        has_motorcycle = if_any(starts_with("h07"), ~ replace_na(.x == 9,  FALSE)),
        has_cycle      = if_any(starts_with("h07"), ~ replace_na(.x == 10, FALSE)),
        has_fridge     = if_any(starts_with("h07"), ~ replace_na(.x == 12, FALSE)),
        asset_count    = rowSums(across(starts_with("h07"), ~ .x %in% 1:12), na.rm = TRUE)
      ) %>%
      group_by(dname) %>%
      summarise(
        amen_water_piped          = mean(h03 == 1, na.rm = TRUE),
        amen_water_traditional    = mean(h03 %in% c(2,3,4,5,6), na.rm = TRUE),
        amen_cooking_wood         = mean(h04 == 1, na.rm = TRUE),
        amen_cooking_kerosene     = mean(h04 == 2, na.rm = TRUE),
        amen_cooking_lpg          = mean(h04 == 3, na.rm = TRUE),
        amen_cooking_biogas       = mean(h04 == 5, na.rm = TRUE),
        amen_cooking_electric     = mean(h04 == 6, na.rm = TRUE),
        amen_cooking_modern       = mean(h04 %in% c(3,5,6), na.rm = TRUE),
        amen_cooking_traditional  = mean(h04 %in% c(1,2,4,7), na.rm = TRUE),
        amen_lighting_electricity = mean(h05 %in% c(1,4), na.rm = TRUE),
        amen_lighting_kerosene    = mean(h05 == 2, na.rm = TRUE),
        amen_lighting_biogas      = mean(h05 == 3, na.rm = TRUE),
        amen_lighting_others      = mean(h05 %in% c(4,5), na.rm = TRUE),
        amen_toilet_modern        = mean(h06 %in% c(1,2), na.rm = TRUE),
        amen_toilet_ordinary      = mean(h06 == 3, na.rm = TRUE),
        amen_toilet_none          = mean(h06 == 4, na.rm = TRUE),
        amen_toilet_any           = mean(h06 %in% c(1,2,3), na.rm = TRUE),
        amen_assets_radio         = mean(has_radio),
        amen_assets_tv            = mean(has_tv),
        amen_assets_cycle         = mean(has_cycle),
        amen_assets_motorcycle    = mean(has_motorcycle),
        amen_assets_car           = mean(has_car),
        amen_assets_fridge        = mean(has_fridge),
        amen_assets_landline      = mean(has_landline),
        amen_assets_mobile        = mean(has_mobile),
        amen_assets_computer      = mean(has_computer),
        amen_assets_internet      = mean(has_internet),
        amen_assets_none          = mean(h07a == 13, na.rm = TRUE),
        amen_asset_count_mean     = mean(asset_count),
        .groups = "drop"
      ) %>%
      mutate(year = 2011)
    
    
    # ---- Housing structure ----
    housing_11 <- hh_11 %>%
      group_by(dname) %>%
      summarise(
        housing_own                    = mean(h01 == 1, na.rm = TRUE),
        housing_rented                 = mean(h01 == 2, na.rm = TRUE),
        housing_foundation_modern      = mean(h021 %in% c(2, 3), na.rm = TRUE),
        housing_foundation_traditional = mean(h021 %in% c(1, 4, 5), na.rm = TRUE),
        housing_roof_modern            = mean(h023 %in% c(2, 4), na.rm = TRUE),
        housing_roof_traditional       = mean(h023 == 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2011)
    
    
    # ---- Female ownership (no enterprise in 2011) ----
    femx_11 <- hh_11 %>%
      group_by(dname) %>%
      summarise(
        fem_ownership_house     = mean(h08 == 1, na.rm = TRUE),
        fem_ownership_land      = mean(h09 == 1, na.rm = TRUE),
        fem_ownership_both      = mean(h08 == 1 & h09 == 1, na.rm = TRUE),
        fem_ownership_livestock = NA_real_,
        ent_has_nonagro         = NA_real_,
        ent_cottage             = NA_real_,
        ent_trade               = NA_real_,
        ent_transport           = NA_real_,
        ent_services            = NA_real_,
        ent_other               = NA_real_,
        ent_female_owner_share  = NA_real_,
        .groups = "drop"
      ) %>%
      mutate(year = 2011)
    
    
    # ---- Shock (absentee + death) ----
    shock_11 <- hh_11 %>%
      group_by(dname) %>%
      summarise(
        absent_hh_share = mean(h13 == 1, na.rm = TRUE),
        hh_death_12m    = mean(h11 == 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2011)
    
    rm(hh_11); gc(verbose = FALSE)
    
    
    # ---- Education: literacy (pop 6+) ----
    edu_lit_11 <- ind11_1 %>%
      filter(!is.na(q05), q05 >= 6, q05 < 150) %>%
      group_by(dname) %>%
      summarise(
        edu_literate        = mean(q13 == 1, na.rm = TRUE),
        edu_literate_female = mean(q13[q04 == 2] == 1, na.rm = TRUE),
        edu_literate_male   = mean(q13[q04 == 1] == 1, na.rm = TRUE),
        .groups = "drop"
      )
    
    # ---- Education: attendance (6-16) ----
    edu_att_11 <- ind11_1 %>%
      filter(!is.na(q05), q05 >= 6, q05 <= 16) %>%
      group_by(dname) %>%
      summarise(
        edu_school_attend_6_16        = mean(q14 == 1, na.rm = TRUE),
        edu_school_attend_6_16_female = mean(q14[q04 == 2] == 1, na.rm = TRUE),
        edu_school_attend_6_16_male   = mean(q14[q04 == 1] == 1, na.rm = TRUE),
        .groups = "drop"
      )
    
    # ---- Education: attainment (25+) ----
    edu_at2_11 <- ind11_1 %>%
      filter(!is.na(q05), q05 >= 25, q05 < 150, !is.na(q15_1)) %>%
      mutate(
        yrs = case_when(
          q15_1 == 0       ~ 0,       # literate, no formal schooling
          q15_1 %in% 1:10  ~ as.numeric(q15_1),
          q15_1 == 11      ~ 10,      # SLC = completed class 10
          q15_1 == 12      ~ 12,      # Intermediate / +2
          q15_1 == 13      ~ 15,      # Bachelor's
          q15_1 == 14      ~ 17,      # Master's
          q15_1 == 15      ~ 20,      # PhD
          TRUE             ~ NA_real_
        )
      ) %>%
      group_by(dname) %>%
      summarise(
        edu_attain_primary_plus          = mean(q15_1 %in% 1:15, na.rm = TRUE),
        edu_attain_secondary_plus        = mean(q15_1 %in% 6:15, na.rm = TRUE),
        edu_attain_higher_secondary_plus = mean(q15_1 %in% 12:15, na.rm = TRUE),
        edu_attain_tertiary              = mean(q15_1 %in% 13:15, na.rm = TRUE),
        edu_years_mean                   = mean(yrs, na.rm = TRUE),
        .groups = "drop"
      )
    
    edu_11 <- edu_lit_11 %>%
      full_join(edu_att_11, by = "dname") %>%
      full_join(edu_at2_11, by = "dname") %>%
      mutate(year = 2011)
    
    rm(edu_lit_11, edu_att_11, edu_at2_11)
    
    
    # ---- Marriage ----
    mar_base_11 <- ind11_1 %>%
      filter(!is.na(q05), q05 >= 15, q05 < 60) %>%
      group_by(dname) %>%
      summarise(
        mar_ever_married_15_60  = mean(q07 %in% 2:7, na.rm = TRUE),
        mar_never_married_15_60 = mean(q07 == 1,    na.rm = TRUE),
        .groups = "drop"
      )
    mar_fem_11 <- ind11_1 %>%
      filter(q04 == 2, !is.na(q05), q05 >= 20, q05 <= 24,
             q07 %in% 2:7, !is.na(q08), q08 < 99) %>%
      group_by(dname) %>%
      summarise(
        mar_female_age_first_mean = mean(q08, na.rm = TRUE),
        mar_female_married_by_18  = mean(q08 < 18, na.rm = TRUE),
        mar_female_married_by_20  = mean(q08 < 20, na.rm = TRUE),
        .groups = "drop"
      )
    mar_11 <- mar_base_11 %>% full_join(mar_fem_11, by = "dname") %>% mutate(year = 2011)
    rm(mar_base_11, mar_fem_11)
    
    
    # ---- Fertility ----
    fert_11 <- ind11_2 %>%
      filter(q04x == 2, !is.na(q05x), q05x >= 15, q05x <= 49, q07x %in% 2:7) %>%
      mutate(
        birth_son   = coalesce(q20_4son, 0),
        birth_dau   = coalesce(q20_4dau, 0),
        birth_total = birth_son + birth_dau,
        dead_son    = coalesce(q20_3son, 0),
        dead_dau    = coalesce(q20_3dau, 0),
        dead_tot    = dead_son + dead_dau,
        birth_12m   = as.integer(q21 == 1)
      ) %>%
      group_by(dname) %>%
      summarise(
        fert_birth_mean           = mean(birth_total, na.rm = TRUE),
        fert_birth_son_mean       = mean(birth_son,   na.rm = TRUE),
        fert_birth_dau_mean       = mean(birth_dau,   na.rm = TRUE),
        fert_births_last12m_share = mean(birth_12m,   na.rm = TRUE),
        fert_births_last12m_rate  = mean(birth_12m,   na.rm = TRUE),
        mort_children_dead_mean   = mean(dead_tot,    na.rm = TRUE),
        mort_child_dead_any       = mean(dead_tot > 0, na.rm = TRUE),
        mort_child_death_ratio    = sum(dead_tot, na.rm = TRUE) /
          pmax(sum(birth_total, na.rm = TRUE), 1e-9),
        .groups = "drop"
      ) %>%
      mutate(year = 2011)
    
    
    # ---- Work shares (primary activity = argmax of q22_1..q22_8 months) ----
    # Vectorized primary activity — avoids rowwise for speed on full sample.
    ind11_2_work <- ind11_2 %>%
      filter(!is.na(q05x), q05x > 14, q05x < 60) %>%
      filter(!if_all(starts_with("q22_"), is.na)) %>%
      mutate(across(starts_with("q22_"), ~ coalesce(.x, 0L))) %>%
      mutate(
        mmax = pmax(q22_1, q22_2, q22_3, q22_4, q22_5, q22_6, q22_7, q22_8),
        primary_activity = case_when(
          mmax == 0     ~ NA_integer_,
          q22_1 == mmax ~ 1L, q22_2 == mmax ~ 2L, q22_3 == mmax ~ 3L,
          q22_4 == mmax ~ 4L, q22_5 == mmax ~ 5L, q22_6 == mmax ~ 6L,
          q22_7 == mmax ~ 7L, q22_8 == mmax ~ 8L,
          TRUE ~ NA_integer_)
      )
    
    work_11 <- ind11_2_work %>%
      group_by(dname) %>%
      summarise(
        work_share_agriculture      = mean(primary_activity == 1, na.rm = TRUE),
        work_share_wage_nonagri     = mean(primary_activity == 2, na.rm = TRUE),
        work_share_own_nonagri      = mean(primary_activity == 3, na.rm = TRUE),
        work_share_extended_econ    = mean(primary_activity == 4, na.rm = TRUE),
        work_share_job_seeking      = mean(primary_activity == 5, na.rm = TRUE),
        work_share_household_chores = mean(primary_activity == 6, na.rm = TRUE),
        work_share_student          = mean(primary_activity == 7, na.rm = TRUE),
        work_share_no_work          = mean(primary_activity == 8, na.rm = TRUE),
        work_share_nonagriculture   = mean(primary_activity %in% 2:4, na.rm = TRUE),
        work_lfp                    = mean(primary_activity %in% 1:4, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2011)
    
    
    # ---- FLFP + gender gaps (reuses primary_activity) ----
    flfp_11 <- ind11_2_work %>%
      group_by(dname) %>%
      summarise(
        denom_f = pmax(mean(q04x == 2, na.rm = TRUE), 1e-9),
        denom_m = pmax(mean(q04x == 1, na.rm = TRUE), 1e-9),
        flfp_all            = mean(primary_activity %in% 1:5 & q04x == 2, na.rm = TRUE) / denom_f,
        fem_employment_rate = mean(primary_activity %in% 1:4 & q04x == 2, na.rm = TRUE) / denom_f,
        flfp_agri           = mean(primary_activity == 1     & q04x == 2, na.rm = TRUE) / denom_f,
        flfp_nonagri        = mean(primary_activity %in% 2:4 & q04x == 2, na.rm = TRUE) / denom_f,
        flfp_wage           = mean(primary_activity == 2     & q04x == 2, na.rm = TRUE) / denom_f,
        flfp_chores_only    = mean(primary_activity == 6     & q04x == 2, na.rm = TRUE) / denom_f,
        mlfp_all            = mean(primary_activity %in% 1:5 & q04x == 1, na.rm = TRUE) / denom_m,
        mlfp_agri           = mean(primary_activity == 1     & q04x == 1, na.rm = TRUE) / denom_m,
        mlfp_nonagri        = mean(primary_activity %in% 2:4 & q04x == 1, na.rm = TRUE) / denom_m,
        share_women         = denom_f,
        share_men           = denom_m,
        .groups = "drop"
      ) %>%
      select(-denom_f, -denom_m) %>%
      mutate(year = 2011)
    
    rm(ind11_2_work); gc(verbose = FALSE)
    
    
    # ---- Occupation (derive 1-digit from 3-digit q23) ----
    occ_11 <- ind11_2 %>%
      filter(!is.na(q05x), q05x > 14, q05x < 60, !is.na(q23)) %>%
      mutate(occ1 = case_when(
        q23 %in% c(11, 21, 31)  ~ 0L,
        q23 %in% c(998, 999)    ~ NA_integer_,
        q23 >= 100 & q23 <= 999 ~ as.integer(q23 %/% 100),
        TRUE                    ~ NA_integer_)) %>%
      filter(!is.na(occ1)) %>%
      group_by(dname) %>%
      summarise(
        occ_share_armed_forces      = mean(occ1 == 0, na.rm = TRUE),
        occ_share_managers          = mean(occ1 == 1, na.rm = TRUE),
        occ_share_professionals     = mean(occ1 == 2, na.rm = TRUE),
        occ_share_technicians       = mean(occ1 == 3, na.rm = TRUE),
        occ_share_office_assistants = mean(occ1 == 4, na.rm = TRUE),
        occ_share_service_sales     = mean(occ1 == 5, na.rm = TRUE),
        occ_share_agriculture       = mean(occ1 == 6, na.rm = TRUE),
        occ_share_craft_trades      = mean(occ1 == 7, na.rm = TRUE),
        occ_share_machine_operators = mean(occ1 == 8, na.rm = TRUE),
        occ_share_elementary        = mean(occ1 == 9, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2011)
    
    
    # ---- Industry (q24 is 2-digit ISIC-4; map to 11 harmonized groups) ----
    industry_11 <- ind11_2 %>%
      filter(!is.na(q05x), q05x > 14, q05x < 60, !is.na(q24), !q24 %in% c(998, 999)) %>%
      group_by(dname) %>%
      summarise(
        ind_agri_forestry_fish       = mean(q24 %in% 1:3, na.rm = TRUE),
        ind_manufacturing            = mean(q24 %in% 10:33, na.rm = TRUE),
        ind_construction             = mean(q24 %in% 41:43, na.rm = TRUE),
        ind_wholesale_retail         = mean(q24 %in% 45:47, na.rm = TRUE),
        ind_transport_accommodation  = mean(q24 %in% c(49:53, 55:56), na.rm = TRUE),
        ind_finance_real_estate_prof = mean(q24 %in% c(58:66, 68, 69:82), na.rm = TRUE),
        ind_public_admin_defence     = mean(q24 == 84, na.rm = TRUE),
        ind_education                = mean(q24 == 85, na.rm = TRUE),
        ind_health                   = mean(q24 %in% 86:88, na.rm = TRUE),
        ind_arts_recreation          = mean(q24 %in% 90:93, na.rm = TRUE),
        ind_others                   = mean(q24 %in% c(5:9, 35, 36:39, 94:96, 97:98, 99), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2011)
    
    
    # ---- Employment status ----
    emp_11 <- ind11_2 %>%
      filter(!is.na(q05x), q05x > 14, q05x < 60, q25 %in% 1:4) %>%
      group_by(dname) %>%
      summarise(
        emp_share_employer             = mean(q25 == 1, na.rm = TRUE),
        emp_share_employee             = mean(q25 == 2, na.rm = TRUE),
        emp_share_self_employed        = mean(q25 == 3, na.rm = TRUE),
        emp_share_unpaid_family_worker = mean(q25 == 4, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2011)
    
    
    # ---- Migration (5-year reference period) ----
    mig_11 <- ind11_2 %>%
      filter(!is.na(q05x), q05x >= 5) %>%
      group_by(dname) %>%
      summarise(
        mig_in_share              = mean(q19a %in% c(2,3), na.rm = TRUE),
        mig_in_domestic           = mean(q19a == 2, na.rm = TRUE),
        mig_in_international      = mean(q19a == 3, na.rm = TRUE),
        mig_in_from_rural         = mean(q19a %in% c(2,3) & q19c == 1, na.rm = TRUE),
        mig_in_from_urban         = mean(q19a %in% c(2,3) & q19c == 2, na.rm = TRUE),
        mig_in_reason_economic    = mean(q19a %in% c(2,3) & q18 %in% 1:3, na.rm = TRUE),
        mig_in_reason_noneconomic = mean(q19a %in% c(2,3) & q18 %in% 4:8, na.rm = TRUE),
        mig_in_reason_study       = mean(q19a %in% c(2,3) & q18 == 4, na.rm = TRUE),
        mig_in_reason_marriage    = mean(q19a %in% c(2,3) & q18 == 5, na.rm = TRUE),
        mig_in_return             = NA_real_,
        .groups = "drop"
      ) %>%
      mutate(year = 2011)
    
    
    # ---- Headship ----
    head_11 <- ind11_1 %>%
      filter(q03 == 1, !is.na(q05), q05 < 150) %>%
      group_by(dname) %>%
      summarise(
        head_female_share   = mean(q04 == 2, na.rm = TRUE),
        head_age_mean       = mean(q05, na.rm = TRUE),
        head_elderly_share  = mean(q05 >= 60, na.rm = TRUE),
        head_female_elderly = mean(q04 == 2 & q05 >= 60, na.rm = TRUE),
        head_young_share    = mean(q05 < 30, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2011)
    
    
    # ---- Left-behind children ----
    left_11 <- ind11_2 %>%
      filter(!is.na(q05x), q05x < 16, !is.na(q27), q27 %in% 1:8) %>%
      group_by(dname) %>%
      summarise(
        left_not_with_both   = mean(q27 != 1, na.rm = TRUE),
        left_mother_only     = mean(q27 == 2, na.rm = TRUE),
        left_father_only     = mean(q27 == 3, na.rm = TRUE),
        left_with_relatives  = mean(q27 == 6, na.rm = TRUE),
        left_without_parents = mean(q27 %in% 6:8, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2011)
    
    
    rm(ind11_1, ind11_2); gc(verbose = FALSE)
    cat("2011 done.\n")
    
    
    #####################################################################################################
    # SECTION 6 — Load 2021 files (mnid = dname directly)
    #####################################################################################################
    
    cat("Loading 2021...\n")
    
    ind_21 <- read_dta("data/raw/Full Census Data/Census 2021/Data/PCMS2021_Individual.dta") %>%
      mutate(dname = as.integer(mnid))
    
    hh_21 <- read_dta("data/raw/Full Census Data/Census 2021/Data/PCMS2021_Household.dta") %>%
      mutate(dname = as.integer(mnid))
    gc(verbose = FALSE)
    
    
    #####################################################################################################
    # SECTION 7 — Compute 2021 topics
    #####################################################################################################
    
    cat("2021 topics...\n")
    
    # ---- Amenities ----
    amen_21 <- hh_21 %>%
      group_by(dname) %>%
      summarise(
        amen_water_piped          = mean(h06 %in% c(1,2), na.rm = TRUE),
        amen_water_traditional    = mean(h06 %in% c(3,4,5,6,7), na.rm = TRUE),
        amen_cooking_wood         = mean(h07 == 1, na.rm = TRUE),
        amen_cooking_kerosene     = mean(h07 == 6, na.rm = TRUE),
        amen_cooking_lpg          = mean(h07 == 2, na.rm = TRUE),
        amen_cooking_biogas       = mean(h07 == 5, na.rm = TRUE),
        amen_cooking_electric     = mean(h07 == 3, na.rm = TRUE),
        amen_cooking_modern       = mean(h07 %in% c(2,3,5), na.rm = TRUE),
        amen_cooking_traditional  = mean(h07 %in% c(1,4,6,7), na.rm = TRUE),
        amen_lighting_electricity = mean(h08 %in% c(1,2), na.rm = TRUE),
        amen_lighting_kerosene    = mean(h08 == 3, na.rm = TRUE),
        amen_lighting_biogas      = mean(h08 == 4, na.rm = TRUE),
        amen_lighting_others      = mean(h08 == 5, na.rm = TRUE),
        amen_toilet_modern        = mean(h09 %in% c(1,2), na.rm = TRUE),
        amen_toilet_ordinary      = mean(h09 == 3, na.rm = TRUE),
        amen_toilet_none          = mean(h09 == 5, na.rm = TRUE),
        amen_toilet_any           = mean(h09 %in% c(1,2,3,4), na.rm = TRUE),
        amen_assets_radio         = mean(h10_A == 1, na.rm = TRUE),
        amen_assets_tv            = mean(h10_B == 1, na.rm = TRUE),
        amen_assets_cycle         = mean(h10_J == 1, na.rm = TRUE),
        amen_assets_motorcycle    = mean(h10_I == 1, na.rm = TRUE),
        amen_assets_car           = mean(h10_H == 1, na.rm = TRUE),
        amen_assets_fridge        = mean(h10_L == 1, na.rm = TRUE),
        amen_assets_landline      = mean(h10_C == 1, na.rm = TRUE),
        amen_assets_mobile        = mean(if_any(c(h10_D, h10_E), ~ .x == 1), na.rm = TRUE),
        amen_assets_computer      = mean(h10_F == 1, na.rm = TRUE),
        amen_assets_internet      = mean(h10_G == 1, na.rm = TRUE),
        amen_assets_none          = mean(rowSums(
          across(c(h10_A,h10_B,h10_J,h10_I,h10_H,h10_L,h10_C,h10_D,h10_E,h10_F,h10_G),
                 ~ .x == 1), na.rm = TRUE) == 0, na.rm = TRUE),
        amen_asset_count_mean     = mean(rowSums(
          across(c(h10_A,h10_B,h10_J,h10_I,h10_H,h10_L,h10_C,h10_D,h10_E,h10_F,h10_G,h10_K,h10_M,h10_N),
                 ~ .x == 1), na.rm = TRUE), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2021)
    
    
    # ---- Housing ----
    housing_21 <- hh_21 %>%
      group_by(dname) %>%
      summarise(
        housing_own                    = mean(h01 == 1, na.rm = TRUE),
        housing_rented                 = mean(h01 == 2, na.rm = TRUE),
        housing_foundation_modern      = mean(h02 %in% c(2, 3), na.rm = TRUE),
        housing_foundation_traditional = mean(h02 %in% c(1, 4, 5), na.rm = TRUE),
        housing_roof_modern            = mean(h04 %in% c(1, 2), na.rm = TRUE),
        housing_roof_traditional       = mean(h04 == 3, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2021)
    
    
    # ---- Female ownership + enterprise ----
    femx_21 <- hh_21 %>%
      group_by(dname) %>%
      summarise(
        fem_ownership_house     = mean(h11 %in% c(1,3), na.rm = TRUE),
        fem_ownership_land      = mean(h11 %in% c(2,3), na.rm = TRUE),
        fem_ownership_both      = mean(h11 == 3,        na.rm = TRUE),
        fem_ownership_livestock = NA_real_,
        ent_has_nonagro         = mean(h12 %in% 1:5, na.rm = TRUE),
        ent_cottage             = mean(h12 == 1, na.rm = TRUE),
        ent_trade               = mean(h12 == 2, na.rm = TRUE),
        ent_transport           = mean(h12 == 3, na.rm = TRUE),
        ent_services            = mean(h12 == 4, na.rm = TRUE),
        ent_other               = mean(h12 == 5, na.rm = TRUE),
        ent_female_owner_share  = mean(h13 == 2 & h12 %in% 1:5, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2021)
    
    
    # ---- Shock ----
    shock_21 <- hh_21 %>%
      group_by(dname) %>%
      summarise(
        absent_hh_share = mean(h16 == 1, na.rm = TRUE),
        hh_death_12m    = mean(h14 == 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2021)
    
    rm(hh_21); gc(verbose = FALSE)
    
    
    # ---- Education: literacy (pop 6+) ----
    edu_lit_21 <- ind_21 %>%
      filter(!is.na(q05), q05 >= 6, q05 < 150) %>%
      group_by(dname) %>%
      summarise(
        edu_literate        = mean(q15 == 1, na.rm = TRUE),
        edu_literate_female = mean(q15[q04 == 2] == 1, na.rm = TRUE),
        edu_literate_male   = mean(q15[q04 == 1] == 1, na.rm = TRUE),
        .groups = "drop"
      )
    
    # ---- Education: school attendance (6-16) ----
    edu_att_21 <- ind_21 %>%
      filter(!is.na(q05), q05 >= 6, q05 <= 16) %>%
      mutate(attending = q16 %in% c(1, 2, 3)) %>%
      group_by(dname) %>%
      summarise(
        edu_school_attend_6_16        = mean(attending, na.rm = TRUE),
        edu_school_attend_6_16_female = mean(attending[q04 == 2], na.rm = TRUE),
        edu_school_attend_6_16_male   = mean(attending[q04 == 1], na.rm = TRUE),
        .groups = "drop"
      )
    
    # ---- Education: attainment (25+) ----
    edu_at2_21 <- ind_21 %>%
      filter(!is.na(q05), q05 >= 25, q05 < 150, !is.na(q17)) %>%
      mutate(
        yrs = case_when(
          q17 == 0       ~ 0,       # literate, no formal schooling
          q17 %in% 1:10  ~ as.numeric(q17),
          q17 == 11      ~ 10,      # SLC
          q17 == 12      ~ 12,      # Intermediate / +2
          q17 == 13      ~ 15,      # Bachelor's
          q17 == 14      ~ 17,      # Master's
          q17 == 15      ~ 20,      # PhD
          TRUE           ~ NA_real_
        )
      ) %>%
      group_by(dname) %>%
      summarise(
        edu_attain_primary_plus          = mean(q17 %in% 1:15, na.rm = TRUE),
        edu_attain_secondary_plus        = mean(q17 %in% 6:15, na.rm = TRUE),
        edu_attain_higher_secondary_plus = mean(q17 %in% 12:15, na.rm = TRUE),
        edu_attain_tertiary              = mean(q17 %in% 13:15, na.rm = TRUE),
        edu_years_mean                   = mean(yrs, na.rm = TRUE),
        .groups = "drop"
      )
    
    edu_21 <- edu_lit_21 %>%
      full_join(edu_att_21, by = "dname") %>%
      full_join(edu_at2_21, by = "dname") %>%
      mutate(year = 2021)
    
    rm(edu_lit_21, edu_att_21, edu_at2_21)
    
    
    # ---- Marriage ----
    mar_base_21 <- ind_21 %>%
      filter(q05 >= 15, q05 < 60) %>%
      group_by(dname) %>%
      summarise(
        mar_ever_married_15_60  = mean(q13 %in% 2:5, na.rm = TRUE),
        mar_never_married_15_60 = mean(q13 == 1,    na.rm = TRUE),
        .groups = "drop"
      )
    mar_fem_21 <- ind_21 %>%
      filter(q04 == 2, q05 >= 20, q05 <= 24, !is.na(q14)) %>%
      group_by(dname) %>%
      summarise(
        mar_female_age_first_mean = mean(q14, na.rm = TRUE),
        mar_female_married_by_18  = mean(q14 < 18 & q13 != 1, na.rm = TRUE),
        mar_female_married_by_20  = mean(q14 < 20 & q13 != 1, na.rm = TRUE),
        .groups = "drop"
      )
    mar_21 <- mar_base_21 %>% full_join(mar_fem_21, by = "dname") %>% mutate(year = 2021)
    rm(mar_base_21, mar_fem_21)
    
    
    # ---- Fertility ----
    fert_21 <- ind_21 %>%
      filter(q04 == 2, q05 >= 15, q05 <= 49, q13 %in% 2:5) %>%
      mutate(
        birth_son      = coalesce(q26son, 0),
        birth_dau      = coalesce(q26dau, 0),
        birth_total    = birth_son + birth_dau,
        dead_son       = coalesce(q27son, 0),
        dead_dau       = coalesce(q27dau, 0),
        dead_tot       = dead_son + dead_dau,
        births_12m_son = coalesce(q28son, 0),
        births_12m_dau = coalesce(q28dau, 0),
        births_12m     = births_12m_son + births_12m_dau
      ) %>%
      group_by(dname) %>%
      summarise(
        fert_birth_mean           = mean(birth_total, na.rm = TRUE),
        fert_birth_son_mean       = mean(birth_son,   na.rm = TRUE),
        fert_birth_dau_mean       = mean(birth_dau,   na.rm = TRUE),
        fert_births_last12m_share = mean(births_12m > 0, na.rm = TRUE),
        fert_births_last12m_rate  = mean(births_12m, na.rm = TRUE),
        mort_children_dead_mean   = mean(dead_tot, na.rm = TRUE),
        mort_child_dead_any       = mean(dead_tot > 0, na.rm = TRUE),
        mort_child_death_ratio    = sum(dead_tot, na.rm = TRUE) /
          pmax(sum(birth_total, na.rm = TRUE), 1e-9),
        .groups = "drop"
      ) %>%
      mutate(year = 2021)
    
    
    # ---- Work shares (q31/q32/q34/q36/q37 skip-pattern denominator fix) ----
    work_21 <- ind_21 %>%
      filter(q05 > 14, q05 < 60) %>%
      group_by(dname) %>%
      summarise(
        work_lfp                    = mean(q31 %in% 1:3, na.rm = TRUE),
        work_share_no_work          = sum(q31 == 4 & !(q36 %in% 1:3) & !(q37 %in% 1:2), na.rm = TRUE) / n(),
        work_share_agriculture      = sum(q32 == 6, na.rm = TRUE) / n(),
        work_share_nonagriculture   = sum(q32 %in% c(0,1,2,3,4,5,7,8,9), na.rm = TRUE) / n(),
        work_share_wage_nonagri     = sum(q34 == 1 & q32 != 6, na.rm = TRUE) / n(),
        work_share_own_nonagri      = sum(q34 == 3 & q32 != 6, na.rm = TRUE) / n(),
        work_share_extended_econ    = NA_real_,
        work_share_job_seeking      = sum(q37 %in% 1:2, na.rm = TRUE) / n(),
        work_share_household_chores = sum(q36 %in% 2:3, na.rm = TRUE) / n(),
        work_share_student          = sum(q36 == 1, na.rm = TRUE) / n(),
        .groups = "drop"
      ) %>%
      mutate(year = 2021)
    
    
    # ---- Occupation ----
    occ_21 <- ind_21 %>%
      filter(q05 > 14, q05 < 60, !is.na(q32)) %>%
      group_by(dname) %>%
      summarise(
        occ_share_armed_forces      = mean(q32 == 0, na.rm = TRUE),
        occ_share_managers          = mean(q32 == 1, na.rm = TRUE),
        occ_share_professionals     = mean(q32 == 2, na.rm = TRUE),
        occ_share_technicians       = mean(q32 == 3, na.rm = TRUE),
        occ_share_office_assistants = mean(q32 == 4, na.rm = TRUE),
        occ_share_service_sales     = mean(q32 == 5, na.rm = TRUE),
        occ_share_agriculture       = mean(q32 == 6, na.rm = TRUE),
        occ_share_craft_trades      = mean(q32 == 7, na.rm = TRUE),
        occ_share_machine_operators = mean(q32 == 8, na.rm = TRUE),
        occ_share_elementary        = mean(q32 == 9, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2021)
    
    
    # ---- Industry ----
    industry_21 <- ind_21 %>%
      filter(q05 > 14, q05 < 60, !is.na(q33), !q33 %in% c(98, 99)) %>%
      group_by(dname) %>%
      summarise(
        ind_agri_forestry_fish       = mean(q33 == 1, na.rm = TRUE),
        ind_manufacturing            = mean(q33 == 3, na.rm = TRUE),
        ind_construction             = mean(q33 == 6, na.rm = TRUE),
        ind_wholesale_retail         = mean(q33 == 7, na.rm = TRUE),
        ind_transport_accommodation  = mean(q33 %in% 8:9, na.rm = TRUE),
        ind_finance_real_estate_prof = mean(q33 %in% 10:14, na.rm = TRUE),
        ind_public_admin_defence     = mean(q33 == 15, na.rm = TRUE),
        ind_education                = mean(q33 == 16, na.rm = TRUE),
        ind_health                   = mean(q33 == 17, na.rm = TRUE),
        ind_arts_recreation          = mean(q33 == 18, na.rm = TRUE),
        ind_others                   = mean(q33 %in% c(2, 4, 5, 19:21), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2021)
    # NOTE: ind_21 is now the industry summary, not the individual file.
    # Reload ind_21 if needed below — we've finished with it.
    
    # ---- Employment status (code 1 = Employee, 2 = Employer — swapped vs 2001/2011) ----
    ind_21_full <- read_dta("data/raw/Full Census Data/Census 2021/Data/PCMS2021_Individual.dta") %>%
      mutate(dname = as.integer(mnid))
    
    emp_21 <- ind_21_full %>%
      filter(q05 > 14, q05 < 60, q34 %in% 1:4) %>%
      group_by(dname) %>%
      summarise(
        emp_share_employee             = mean(q34 == 1, na.rm = TRUE),
        emp_share_employer             = mean(q34 == 2, na.rm = TRUE),
        emp_share_self_employed        = mean(q34 == 3, na.rm = TRUE),
        emp_share_unpaid_family_worker = mean(q34 == 4, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2021)
    
    
    # ---- Migration ----
    mig_21 <- ind_21_full %>%
      group_by(dname) %>%
      summarise(
        mig_in_share              = mean(q21 %in% c(2,3,4), na.rm = TRUE),
        mig_in_domestic           = mean(q21 %in% c(2,3),   na.rm = TRUE),
        mig_in_international      = mean(q21 == 4,          na.rm = TRUE),
        mig_in_from_rural         = mean(q21 %in% c(2,3,4) & q23 == 1, na.rm = TRUE),
        mig_in_from_urban         = mean(q21 %in% c(2,3,4) & q23 == 2, na.rm = TRUE),
        mig_in_reason_economic    = mean(q21 %in% c(2,3,4) & q25 %in% c(1,2,7,8), na.rm = TRUE),
        mig_in_reason_noneconomic = mean(q21 %in% c(2,3,4) & q25 %in% c(3,4,5,6,9), na.rm = TRUE),
        mig_in_reason_study       = mean(q21 %in% c(2,3,4) & q25 == 3, na.rm = TRUE),
        mig_in_reason_marriage    = mean(q21 %in% c(2,3,4) & q25 == 4, na.rm = TRUE),
        mig_in_return             = mean(q21 %in% c(2,3,4) & q25 == 8, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2021)
    
    
    # ---- FLFP + gender gaps (ag/non-ag by industry q33) ----
    flfp_21 <- ind_21_full %>%
      filter(q05 > 14, q05 < 60) %>%
      group_by(dname) %>%
      summarise(
        denom_f = pmax(mean(q04 == 2, na.rm = TRUE), 1e-9),
        denom_m = pmax(mean(q04 == 1, na.rm = TRUE), 1e-9),
        flfp_all            = mean((q31 %in% 1:3 | q37 %in% 1:2) & q04 == 2, na.rm = TRUE) / denom_f,
        fem_employment_rate = mean(q31 %in% 1:3 & q04 == 2, na.rm = TRUE) / denom_f,
        flfp_agri           = mean(q31 %in% 1:3 & q33 == 1 & q04 == 2, na.rm = TRUE) / denom_f,
        flfp_nonagri        = mean(q31 %in% 1:3 & !is.na(q33) & q33 != 1 & q04 == 2, na.rm = TRUE) / denom_f,
        flfp_wage           = mean(q31 %in% 1:3 & q34 == 1 & q04 == 2, na.rm = TRUE) / denom_f,
        flfp_chores_only    = mean(q31 == 4 & q36 %in% 2:3 & q04 == 2, na.rm = TRUE) / denom_f,
        mlfp_all            = mean((q31 %in% 1:3 | q37 %in% 1:2) & q04 == 1, na.rm = TRUE) / denom_m,
        mlfp_agri           = mean(q31 %in% 1:3 & q33 == 1 & q04 == 1, na.rm = TRUE) / denom_m,
        mlfp_nonagri        = mean(q31 %in% 1:3 & !is.na(q33) & q33 != 1 & q04 == 1, na.rm = TRUE) / denom_m,
        share_women         = denom_f,
        share_men           = denom_m,
        .groups = "drop"
      ) %>%
      select(-denom_f, -denom_m) %>%
      mutate(year = 2021)
    
    
    # ---- Headship ----
    head_21 <- ind_21_full %>%
      filter(q03 == 1) %>%
      group_by(dname) %>%
      summarise(
        head_female_share   = mean(q04 == 2, na.rm = TRUE),
        head_age_mean       = mean(q05, na.rm = TRUE),
        head_elderly_share  = mean(q05 >= 60, na.rm = TRUE),
        head_female_elderly = mean(q04 == 2 & q05 >= 60, na.rm = TRUE),
        head_young_share    = mean(q05 < 30, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2021)
    
    
    # ---- Left-behind children ----
    left_21 <- ind_21_full %>%
      filter(q05 < 16, !is.na(q29)) %>%
      group_by(dname) %>%
      summarise(
        left_not_with_both   = mean(q29 != 1, na.rm = TRUE),
        left_mother_only     = mean(q29 == 2, na.rm = TRUE),
        left_father_only     = mean(q29 == 3, na.rm = TRUE),
        left_with_relatives  = mean(q29 == 6, na.rm = TRUE),
        left_without_parents = mean(q29 %in% 6:8, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(year = 2021)
    
    
    rm(ind_21_full, ind_21); gc(verbose = FALSE)
    cat("2021 done.\n")
    
    
    #####################################################################################################
    # SECTION 8 — Bind topics across years, add post-FLFP derived measures, join into panel
    #####################################################################################################
    
    cat("Binding and joining...\n")
    
    # Bind each topic across 3 years
    amen_df    <- bind_rows(amen_01,    amen_11,    amen_21)
    housing_df <- bind_rows(housing_01, housing_11, housing_21)
    femx_df    <- bind_rows(femx_01,    femx_11,    femx_21)
    shock_df   <- bind_rows(shock_01,   shock_11,   shock_21)
    edu_df     <- bind_rows(edu_01,     edu_11,     edu_21)
    mar_df     <- bind_rows(mar_01,     mar_11,     mar_21)
    fert_df    <- bind_rows(fert_01,    fert_11,    fert_21)
    work_df    <- bind_rows(work_01,    work_11,    work_21)
    occ_df     <- bind_rows(occ_01,     occ_11,     occ_21)
    industry_df     <- bind_rows(industry_01,     industry_11,     industry_21)   # ind_21 now = industry summary
    emp_df     <- bind_rows(emp_01,     emp_11,     emp_21)
    mig_df     <- bind_rows(mig_01,     mig_11,     mig_21)
    flfp_df    <- bind_rows(flfp_01,    flfp_11,    flfp_21) %>%
      mutate(
        fem_share_of_ag_workers       = (flfp_agri * share_women) /
          pmax(flfp_agri * share_women + mlfp_agri * share_men, 1e-9),
        fem_ag_specialization_ratio   = flfp_agri / pmax(mlfp_agri, 1e-9),
        fem_wage_share_of_employment  = flfp_wage / pmax(fem_employment_rate, 1e-9),
        gap_lfp_m_minus_f             = mlfp_all - flfp_all,
        gap_nonagri_m_minus_f         = mlfp_nonagri - flfp_nonagri
      )
    head_df <- bind_rows(head_01, head_11, head_21)
    left_df <- bind_rows(left_01, left_11, left_21)
    
    # Join everything at (dname, year)
    panel_dist <- amen_df %>%
      full_join(housing_df, by = c("dname","year")) %>%
      full_join(femx_df,    by = c("dname","year")) %>%
      full_join(shock_df,   by = c("dname","year")) %>%
      full_join(edu_df,     by = c("dname","year")) %>%
      full_join(mar_df,     by = c("dname","year")) %>%
      full_join(fert_df,    by = c("dname","year")) %>%
      full_join(work_df,    by = c("dname","year")) %>%
      full_join(occ_df,     by = c("dname","year")) %>%
      full_join(industry_df,     by = c("dname","year")) %>%
      full_join(emp_df,     by = c("dname","year")) %>%
      full_join(mig_df,     by = c("dname","year")) %>%
      full_join(flfp_df,    by = c("dname","year")) %>%
      full_join(head_df,    by = c("dname","year")) %>%
      full_join(left_df,    by = c("dname","year")) %>%
      arrange(dname, year)
    
    # NaN (from mean-of-all-NA groups) -> NA
    panel_dist <- panel_dist %>%
      mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))
    
    cat(sprintf("Final: %d rows x %d cols\n", nrow(panel_dist), ncol(panel_dist)))
    
    write.csv(panel_dist, file.path(DIR_CLEAN, "outcomes_district.csv"), row.names = FALSE)
    cat("Saved: district-analysis/data/clean/census/outcomes_district.csv\n")
