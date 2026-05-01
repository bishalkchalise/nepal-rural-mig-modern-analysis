      ##############################################################################
      # NRVS STAGE 2: HEALTH & EDUCATION OUTCOMES (HH x YEAR)
      ##############################################################################
      #
      # Inputs (person-level, one row per member x year):
      #   <base>/education/section_2.csv
      #   <base>/health/section_3.csv
      #
      # Outputs:
      #   <out>/education_hh_year.csv
      #   <out>/health_hh_year.csv
      #   <out>/health_education_codebook.csv
      #
      # Key definitions:
      # ---------------------------------------------------------------------------
      # EDUCATION (Section 2)
      #   Coverage questions:
      #     s02q01 = enrollment status (Never / Currently / Previously attended)
      #     s02q04 = school type (Govt / Private / Other)
      #     s02q05 = received scholarship (Yes/No)
      #     s02q08 = scholarship amount (Rs.)
      #   Expenditure (past 12 months, per enrolled person), standard NLSS mapping:
      #     s02q09a = tuition / admission fees
      #     s02q09b = books & stationery
      #     s02q09c = uniforms & clothing
      #     s02q09d = transport
      #     s02q09e = food & lodging (hostel)
      #     s02q09f = other education expenses
      #
      # HEALTH (Section 3)
      #   Coverage questions:
      #     s03q01  = has health insurance card
      #     s03q02a = has chronic illness
      #     s03q04  = acute illness in past 30 days
      #   Expenditure (past ~30 days acute + longer window for chronic / hospital),
      #   standard NLSS mapping:
      #     s03q06a = consultation / doctor fees
      #     s03q06b = medicines
      #     s03q06c = hospital / inpatient costs
      #     s03q06d = diagnostic tests
      #     s03q06e = other medical expenses
      #
      # OUTPUT STRUCTURE (one row per hhid x year):
      #   Education (14 outcomes):
      #     n_enrolled                   # of HH members currently attending school
      #     n_school_age                 # of HH members who ever attended or are currently attending
      #     any_enrolled                 1 if any HH member currently attending
      #     n_private_school             # in private schools (s02q04)
      #     n_scholarship                # receiving scholarship (s02q05 == "Yes")
      #     scholarship_amt_12m          sum of s02q08
      #     edu_spend_tuition_12m        sum of s02q09a
      #     edu_spend_books_12m          sum of s02q09b
      #     edu_spend_uniforms_12m       sum of s02q09c
      #     edu_spend_transport_12m      sum of s02q09d
      #     edu_spend_food_lodging_12m   sum of s02q09e
      #     edu_spend_other_12m          sum of s02q09f
      #     edu_spend_total_12m          sum of all six subcategories
      #     edu_spend_per_enrolled       total / n_enrolled (NA if n_enrolled == 0)
      #
      #   Health (11 outcomes):
      #     n_insured                    # with health card
      #     any_insured                  1 if any HH member has health card
      #     n_chronic                    # with chronic illness
      #     n_acute_illness              # with acute illness past 30d
      #     any_health_spending          1 if household had any medical spending
      #     hlt_spend_fees               sum of s03q06a
      #     hlt_spend_medicines          sum of s03q06b
      #     hlt_spend_hospital           sum of s03q06c
      #     hlt_spend_tests              sum of s03q06d
      #     hlt_spend_other              sum of s03q06e
      #     hlt_spend_total              sum of all five subcategories
      #
      ##############################################################################
      
      library(tidyverse)
      library(fs)
      
      # ---- Paths -----------------------------------------------------------------
      # Adjust `base_in` for your machine if the working dir differs
      base_in  <- "data/raw/RVS Data/clean"
      base_out <- "data/clean/rvs_outcomes"
      dir_create(base_out, recurse = TRUE)
      
      # ---- Helpers ---------------------------------------------------------------
      # Coerce "Yes"/"No"/NA to 1/0/NA; leave numerics alone
      yn01 <- function(x) {
        if (is.numeric(x)) return(as.integer(x > 0))
        x <- as.character(x)
        case_when(
          is.na(x)                         ~ NA_integer_,
          str_detect(tolower(x), "^yes")   ~ 1L,
          str_detect(tolower(x), "^no")    ~ 0L,
          TRUE                              ~ NA_integer_
        )
      }
      
      # Safe numeric: treat NA as 0 when summing spending (but only after we've
      # filtered to observed rows, so "NA because not-enrolled" stays excluded).
      na0 <- function(x) ifelse(is.na(x), 0, x)
      
      ##############################################################################
      # 1. EDUCATION
      ##############################################################################
      
      edu_raw <- read_csv(file.path(base_in, "education/section_2.csv"),
                          show_col_types = FALSE)
      
      cat("Education raw rows:", nrow(edu_raw), "\n")
      cat("Years present     :", paste(sort(unique(edu_raw$year)), collapse = ", "), "\n")
      
      edu_person <- edu_raw %>%
        transmute(
          year, hhid, member_id,
          # Enrollment status
          is_current = str_detect(tolower(as.character(s02q01)), "currently"),
          is_ever    = str_detect(tolower(as.character(s02q01)), "currently|previously"),
          # School type (only meaningful for currently-attending)
          is_private = str_detect(tolower(as.character(s02q04)), "private|institutional"),
          # Scholarship
          has_schol        = yn01(s02q05),
          schol_amt        = na0(as.numeric(s02q08)),
          # Expenditure sub-categories (Rs., past 12 months)
          sp_tuition       = na0(as.numeric(s02q09a)),
          sp_books         = na0(as.numeric(s02q09b)),
          sp_uniforms      = na0(as.numeric(s02q09c)),
          sp_transport     = na0(as.numeric(s02q09d)),
          sp_food_lodging  = na0(as.numeric(s02q09e)),
          sp_other         = na0(as.numeric(s02q09f))
        )
      
      edu_hh <- edu_person %>%
        group_by(hhid, year) %>%
        summarise(
          n_enrolled                 = sum(is_current, na.rm = TRUE),
          n_school_age               = sum(is_ever,    na.rm = TRUE),
          any_enrolled               = as.integer(n_enrolled > 0),
          n_private_school           = sum(is_private & is_current, na.rm = TRUE),
          n_scholarship              = sum(has_schol == 1, na.rm = TRUE),
          scholarship_amt_12m        = sum(schol_amt),
          edu_spend_tuition_12m      = sum(sp_tuition),
          edu_spend_books_12m        = sum(sp_books),
          edu_spend_uniforms_12m     = sum(sp_uniforms),
          edu_spend_transport_12m    = sum(sp_transport),
          edu_spend_food_lodging_12m = sum(sp_food_lodging),
          edu_spend_other_12m        = sum(sp_other),
          .groups = "drop"
        ) %>%
        mutate(
          edu_spend_total_12m  = edu_spend_tuition_12m + edu_spend_books_12m +
            edu_spend_uniforms_12m + edu_spend_transport_12m +
            edu_spend_food_lodging_12m + edu_spend_other_12m,
          edu_spend_per_enrolled = if_else(n_enrolled > 0,
                                           edu_spend_total_12m / n_enrolled,
                                           NA_real_)
        )
      
      write_csv(edu_hh, file.path(base_out, "education_hh_year.csv"))
      cat("Wrote", file.path(base_out, "education_hh_year.csv"),
          "-- rows:", nrow(edu_hh), "\n")
      
      ##############################################################################
      # 2. HEALTH
      ##############################################################################
      
      hlt_raw <- read_csv(file.path(base_in, "health/section_3.csv"),
                          show_col_types = FALSE)
      
      cat("\nHealth raw rows:", nrow(hlt_raw), "\n")
      cat("Years present  :", paste(sort(unique(hlt_raw$year)), collapse = ", "), "\n")
      
      hlt_person <- hlt_raw %>%
        transmute(
          year, hhid, member_id,
          # Coverage indicators
          has_card    = as.integer(str_detect(tolower(as.character(s03q01)), "^yes")),
          has_chronic = yn01(s03q02a),
          has_acute   = yn01(s03q04),
          # Expenditure sub-categories (Rs.)
          sp_fees      = na0(as.numeric(s03q06a)),
          sp_meds      = na0(as.numeric(s03q06b)),
          sp_hospital  = na0(as.numeric(s03q06c)),
          sp_tests     = na0(as.numeric(s03q06d)),
          sp_other     = na0(as.numeric(s03q06e))
        )
      
      hlt_hh <- hlt_person %>%
        group_by(hhid, year) %>%
        summarise(
          n_insured           = sum(has_card == 1, na.rm = TRUE),
          any_insured         = as.integer(n_insured > 0),
          n_chronic           = sum(has_chronic == 1, na.rm = TRUE),
          n_acute_illness     = sum(has_acute == 1, na.rm = TRUE),
          hlt_spend_fees      = sum(sp_fees),
          hlt_spend_medicines = sum(sp_meds),
          hlt_spend_hospital  = sum(sp_hospital),
          hlt_spend_tests     = sum(sp_tests),
          hlt_spend_other     = sum(sp_other),
          .groups = "drop"
        ) %>%
        mutate(
          hlt_spend_total     = hlt_spend_fees + hlt_spend_medicines +
            hlt_spend_hospital + hlt_spend_tests + hlt_spend_other,
          any_health_spending = as.integer(hlt_spend_total > 0)
        ) %>%
        select(hhid, year,
               n_insured, any_insured, n_chronic, n_acute_illness,
               any_health_spending,
               hlt_spend_fees, hlt_spend_medicines, hlt_spend_hospital,
               hlt_spend_tests, hlt_spend_other, hlt_spend_total)
      
      write_csv(hlt_hh, file.path(base_out, "health_hh_year.csv"))
      cat("Wrote", file.path(base_out, "health_hh_year.csv"),
          "-- rows:", nrow(hlt_hh), "\n")
      
      ##############################################################################
      # 3. CODEBOOK
      ##############################################################################
      
      codebook <- tribble(
        ~variable,                      ~source,         ~definition,
        # Education
        "n_enrolled",                   "s02q01",        "Count of HH members currently attending school",
        "n_school_age",                 "s02q01",        "Count of HH members who ever attended or currently attend",
        "any_enrolled",                 "derived",       "1 if any HH member currently attending",
        "n_private_school",             "s02q04",        "Count of currently-attending members in private/institutional schools",
        "n_scholarship",                "s02q05",        "Count of members receiving scholarship",
        "scholarship_amt_12m",          "s02q08",        "Sum of scholarship amounts (Rs., 12m)",
        "edu_spend_tuition_12m",        "s02q09a",       "Tuition / admission fees (Rs., 12m)",
        "edu_spend_books_12m",          "s02q09b",       "Books & stationery (Rs., 12m)",
        "edu_spend_uniforms_12m",       "s02q09c",       "Uniforms & clothing (Rs., 12m)",
        "edu_spend_transport_12m",      "s02q09d",       "Transport to school (Rs., 12m)",
        "edu_spend_food_lodging_12m",   "s02q09e",       "Food & lodging / hostel (Rs., 12m)",
        "edu_spend_other_12m",          "s02q09f",       "Other education expenses (Rs., 12m)",
        "edu_spend_total_12m",          "derived",       "Sum of all six education sub-categories",
        "edu_spend_per_enrolled",       "derived",       "Total education spend / n_enrolled (NA if 0)",
        # Health
        "n_insured",                    "s03q01",        "Count of members with health card",
        "any_insured",                  "derived",       "1 if any member has health card",
        "n_chronic",                    "s03q02a",       "Count of members with chronic illness",
        "n_acute_illness",              "s03q04",        "Count of members with acute illness (past 30d)",
        "any_health_spending",          "derived",       "1 if HH had any medical spending",
        "hlt_spend_fees",               "s03q06a",       "Consultation / doctor fees (Rs.)",
        "hlt_spend_medicines",          "s03q06b",       "Medicines (Rs.)",
        "hlt_spend_hospital",           "s03q06c",       "Hospital / inpatient costs (Rs.)",
        "hlt_spend_tests",              "s03q06d",       "Diagnostic tests (Rs.)",
        "hlt_spend_other",              "s03q06e",       "Other medical expenses (Rs.)",
        "hlt_spend_total",              "derived",       "Sum of all five health sub-categories"
      )
      
      write_csv(codebook, file.path(base_out, "health_education_codebook.csv"))
      cat("Wrote", file.path(base_out, "health_education_codebook.csv"), "\n")
      
      ##############################################################################
      # 4. QUICK SANITY CHECKS
      ##############################################################################
      
      cat("\n==== EDUCATION HH x YEAR: summary ====\n")
      edu_hh %>%
        group_by(year) %>%
        summarise(
          n_hh              = n(),
          pct_any_enrolled  = round(100 * mean(any_enrolled), 1),
          mean_n_enrolled   = round(mean(n_enrolled), 2),
          mean_spend_total  = round(mean(edu_spend_total_12m), 0),
          med_spend_total   = round(median(edu_spend_total_12m), 0),
          pct_private       = round(100 * mean(n_private_school > 0), 1)
        ) %>% print()
      
      cat("\n==== HEALTH HH x YEAR: summary ====\n")
      hlt_hh %>%
        group_by(year) %>%
        summarise(
          n_hh                 = n(),
          pct_any_insured      = round(100 * mean(any_insured), 1),
          mean_n_chronic       = round(mean(n_chronic), 2),
          mean_n_acute         = round(mean(n_acute_illness), 2),
          mean_spend_total     = round(mean(hlt_spend_total), 0),
          med_spend_total      = round(median(hlt_spend_total), 0),
          pct_any_spending     = round(100 * mean(any_health_spending), 1)
        ) %>% print()
      
      cat("\n==== Sub-category means (validate sub-category labels) ====\n")
      cat("Education spending by sub-category (mean Rs. per HH):\n")
      edu_hh %>%
        summarise(across(starts_with("edu_spend_") & !matches("total|per_enrolled"),
                         ~ round(mean(.), 0))) %>%
        pivot_longer(everything(), names_to = "category", values_to = "mean_rs") %>%
        arrange(desc(mean_rs)) %>% print()
      
      cat("\nHealth spending by sub-category (mean Rs. per HH):\n")
      hlt_hh %>%
        summarise(across(starts_with("hlt_spend_") & !matches("total"),
                         ~ round(mean(.), 0))) %>%
        pivot_longer(everything(), names_to = "category", values_to = "mean_rs") %>%
        arrange(desc(mean_rs)) %>% print()
      
      cat("\nDone.\n")