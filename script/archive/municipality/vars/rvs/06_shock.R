      ##############################################################################
      # NRVS STAGE 2: SHOCKS & COPING — MINIMAL
      ##############################################################################
      # Inputs:
      #   <base>/shocks_coping/section_15a.csv
      #
      # Outputs:
      #   <out>/shocks_coping_hh_year.csv
      #   <out>/shocks_codebook.csv
      #
      # Outcomes (per HH × year, only for HHs that reported a shock):
      #   any_shock       — 1 if HH had any shock in past 12m
      #   coped_self      — 1 if used own resources (savings/reduced consumption,
      #                     selling assets, borrowing)
      #   coped_external  — 1 if used outside sources (private transfers from
      #                     family/community, govt/NGO assistance, remittances)
      #   cope_category   — 4-level: self_only / external_only / both / neither
      ##############################################################################
      
      library(tidyverse)
      library(fs)
      
      base_in  <- "data/raw/RVS Data/clean"
      base_out <- "data/clean/rvs_outcomes"
      dir_create(base_out, recurse = TRUE)
      
      # ---- helpers --------------------------------------------------------------
      yn01 <- function(x) {
        if (is.numeric(x)) return(as.integer(x > 0))
        x <- as.character(x)
        case_when(
          is.na(x)                       ~ NA_integer_,
          str_detect(tolower(x), "^yes") ~ 1L,
          str_detect(tolower(x), "^no")  ~ 0L,
          TRUE                           ~ NA_integer_
        )
      }
      
      # ---- read ------------------------------------------------------------------
      sh15a <- read_csv(file.path(base_in, "shocks_coping/section_15a.csv"),
                        show_col_types = FALSE)
      
      # ---- per-shock-row coping flags -------------------------------------------
      shocks_per_row <- sh15a %>%
        transmute(
          hhid, year,
          # Self-reliant coping
          cope_savings     = yn01(s15q08a),     # savings / reduced consumption
          cope_sell_assets = yn01(s15q07a),
          cope_borrow      = yn01(s15q09a),
          # External coping
          cope_private     = yn01(s15q06a_1) | yn01(s15q06a_2) |
            yn01(s15q06b_1) | yn01(s15q06b_2),
          cope_gov         = yn01(s15q10a),
          cope_remittance  = yn01(s15q11a)
        ) %>%
        mutate(across(starts_with("cope_"), ~ as.integer(replace_na(., FALSE))))
      
      # ---- aggregate to HH × year -----------------------------------------------
      shocks_hh <- shocks_per_row %>%
        group_by(hhid, year) %>%
        summarise(
          any_shock      = 1L,
          coped_self     = as.integer(any(cope_savings     == 1 |
                                            cope_sell_assets == 1 |
                                            cope_borrow      == 1, na.rm = TRUE)),
          coped_external = as.integer(any(cope_private    == 1 |
                                            cope_gov        == 1 |
                                            cope_remittance == 1, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        mutate(
          cope_category = case_when(
            coped_self == 1 & coped_external == 0 ~ "self_only",
            coped_self == 0 & coped_external == 1 ~ "external_only",
            coped_self == 1 & coped_external == 1 ~ "both",
            TRUE                                  ~ "neither"
          )
        ) %>%
        select(hhid, year, any_shock, coped_self, coped_external, cope_category)
      
      # ---- write -----------------------------------------------------------------
      write_csv(shocks_hh, file.path(base_out, "shocks_coping_hh_year.csv"))
      
      codebook <- tribble(
        ~variable,        ~source,               ~definition,
        "any_shock",      "15a presence",        "1 if HH reported any shock in past 12m.",
        "coped_self",     "15a s15q07a/08a/09a", "1 if HH used own resources (savings/reduced consumption, selling assets, or borrowing) for any shock.",
        "coped_external", "15a s15q06/10a/11a",  "1 if HH used outside resources (private transfers from family/community, govt/NGO assistance, or remittances) for any shock.",
        "cope_category",  "derived",             "4-level coping summary: self_only / external_only / both / neither."
      )
      write_csv(codebook, file.path(base_out, "shocks_codebook.csv"))
      
      # ---- sanity check ---------------------------------------------------------
      cat("\n==== Coping shares by year (% of HHs reporting a shock) ====\n")
      shocks_hh %>%
        group_by(year) %>%
        summarise(
          n_hh        = n(),
          pct_self    = round(100*mean(cope_category == "self_only"),     1),
          pct_ext     = round(100*mean(cope_category == "external_only"), 1),
          pct_both    = round(100*mean(cope_category == "both"),          1),
          pct_neither = round(100*mean(cope_category == "neither"),       1),
          .groups = "drop"
        ) %>% print()
      
      cat("\nDone.\n")