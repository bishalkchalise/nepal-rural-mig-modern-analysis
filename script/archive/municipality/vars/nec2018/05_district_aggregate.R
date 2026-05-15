##############################################################################
# script/nec2018/05_district_aggregate.R
##############################################################################
#
# Same structure as 03_municipality_wide.R but grouped by DIST (77 districts
# instead of ~750 municipalities). Built directly from firm_level.csv (not
# from municipality file) to avoid weight-averaging issues.
#
# Input:  data/clean/nec2018/firm_level.csv
# Output: data/clean/nec2018/district_analysis.csv
#
##############################################################################

library(tidyverse)

# ---- Helpers --------------------------------------------------------------
nz         <- function(x) replace(x, is.na(x), 0)
mean01     <- function(x) if (sum(!is.na(x)) == 0) NA_real_ else mean(x, na.rm = TRUE)
q_val      <- function(x, p) { x <- x[!is.na(x)]; if (!length(x)) NA_real_ else unname(quantile(x, p, type = 7)) }
median_pos <- function(x) { x <- x[!is.na(x) & x > 0]; if (!length(x)) NA_real_ else median(x) }
mean_pos   <- function(x) { x <- x[!is.na(x) & x > 0]; if (!length(x)) NA_real_ else mean(x) }
p90_pos    <- function(x) { x <- x[!is.na(x) & x > 0]; if (!length(x)) NA_real_ else q_val(x, 0.90) }

SIZE_LEVELS <- c("micro_1", "small_2_9", "medium_10_50", "large_51p")

# ---- Read firm-level ------------------------------------------------------
firm <- read_csv("data/clean/nec2018/firm_level.csv",
                 show_col_types = FALSE, guess_max = 1e5)
cat("Firm-level rows:", nrow(firm), "\n")

# ---- 1. Core counts + formalization --------------------------------------
dist_core <- firm |>
  group_by(DIST) |>
  summarise(
    n_firms                 = n(),
    n_lgcodes               = n_distinct(lgcode),
    share_registered        = mean01(is_registered),
    share_tax_registered    = mean01(is_tax_registered),
    share_keeps_accounts    = mean01(keeps_accounts),
    share_incorporated      = mean01(is_incorporated),
    share_sole_prop         = mean01(is_sole_prop),
    share_cooperative       = mean01(is_cooperative),
    share_operates_yr_round = mean01(operates_year_round),
    .groups = "drop"
  ) |>
  rowwise() |>
  mutate(formality_index = mean(c(share_registered, share_tax_registered,
                                   share_keeps_accounts, share_incorporated,
                                   share_operates_yr_round), na.rm = TRUE)) |>
  ungroup()

# ---- 2. Employment -------------------------------------------------------
dist_emp <- firm |>
  group_by(DIST) |>
  summarise(
    emp_total          = sum(nz(pe_tot)),
    emp_nepali_male    = sum(nz(pe_nm)),
    emp_nepali_female  = sum(nz(pe_nf)),
    emp_foreign_male   = sum(nz(pe_fm)),
    emp_foreign_female = sum(nz(pe_ff)),
    mean_emp_per_firm  = mean_pos(pe_tot),
    p50_emp_per_firm   = median_pos(pe_tot),
    p90_emp_per_firm   = p90_pos(pe_tot),
    .groups = "drop"
  ) |>
  mutate(
    emp_nepali_total  = emp_nepali_male + emp_nepali_female,
    emp_foreign_total = emp_foreign_male + emp_foreign_female,
    share_emp_foreign = ifelse(emp_total > 0, emp_foreign_total / emp_total, NA_real_),
    share_emp_female  = ifelse(emp_total > 0, emp_nepali_female / emp_total, NA_real_)
  )

# ---- 3. Size distribution ------------------------------------------------
dist_size <- firm |>
  filter(!is.na(size_cat)) |>
  mutate(size_cat = factor(size_cat, levels = SIZE_LEVELS)) |>
  group_by(DIST, size_cat) |>
  summarise(n_firms_size = n(), emp_size = sum(nz(pe_tot)), .groups = "drop") |>
  group_by(DIST) |>
  mutate(
    share_firms_size = n_firms_size / sum(n_firms_size),
    share_emp_size   = ifelse(sum(emp_size) > 0, emp_size / sum(emp_size), NA_real_)
  ) |>
  ungroup() |>
  pivot_wider(id_cols = DIST, names_from = size_cat,
              values_from = c(n_firms_size, share_firms_size, emp_size, share_emp_size),
              values_fill = 0)

# ---- 4. Sector composition -----------------------------------------------
dist_sector <- firm |>
  filter(!is.na(sector_short)) |>
  count(DIST, sector_short, name = "n_firms_sec") |>
  group_by(DIST) |>
  mutate(share_firms_sec = n_firms_sec / sum(n_firms_sec)) |>
  ungroup() |>
  pivot_wider(id_cols = DIST, names_from = sector_short,
              values_from = c(n_firms_sec, share_firms_sec),
              names_glue = "{.value}_sec_{sector_short}", values_fill = 0)

# ---- 5. Classification shares --------------------------------------------
dist_trade <- firm |>
  filter(!is.na(tradability)) |>
  count(DIST, tradability, name = "n") |>
  group_by(DIST) |> mutate(share = n / sum(n)) |> ungroup() |>
  pivot_wider(id_cols = DIST, names_from = tradability,
              values_from = c(n, share),
              names_glue = "{.value}_trd_{tradability}", values_fill = 0)

dist_ag <- firm |>
  filter(!is.na(ag_orientation)) |>
  count(DIST, ag_orientation, name = "n") |>
  group_by(DIST) |> mutate(share = n / sum(n)) |> ungroup() |>
  pivot_wider(id_cols = DIST, names_from = ag_orientation,
              values_from = c(n, share),
              names_glue = "{.value}_agorient_{ag_orientation}", values_fill = 0)

dist_mtier <- firm |>
  filter(!is.na(manuf_tier), manuf_tier != "not_manuf") |>
  count(DIST, manuf_tier, name = "n") |>
  group_by(DIST) |> mutate(share_within_manuf = n / sum(n)) |> ungroup() |>
  pivot_wider(id_cols = DIST, names_from = manuf_tier,
              values_from = c(n, share_within_manuf),
              names_glue = "{.value}_mtier_{manuf_tier}", values_fill = 0)

dist_modern <- firm |>
  filter(!is.na(modernity)) |>
  count(DIST, modernity, name = "n") |>
  group_by(DIST) |> mutate(share = n / sum(n)) |> ungroup() |>
  pivot_wider(id_cols = DIST, names_from = modernity,
              values_from = c(n, share),
              names_glue = "{.value}_modern_{modernity}", values_fill = 0)

# ---- 6. Sector x size crosstab -------------------------------------------
dist_sector_size <- firm |>
  filter(!is.na(sector_short), !is.na(size_cat)) |>
  mutate(size_cat = factor(size_cat, levels = SIZE_LEVELS)) |>
  count(DIST, sector_short, size_cat, name = "n_firms") |>
  group_by(DIST) |>
  mutate(share_in_dist = n_firms / sum(n_firms)) |>
  ungroup() |>
  pivot_wider(id_cols = DIST, names_from = c(sector_short, size_cat),
              values_from = c(n_firms, share_in_dist),
              names_glue = "{.value}_secsize_{sector_short}_{size_cat}",
              values_fill = 0)

# ---- 7. Finance ----------------------------------------------------------
dist_finance <- firm |>
  group_by(DIST) |>
  summarise(
    share_borrowed_any    = mean01(has_borrowed),
    share_formal_credit   = mean01(uses_formal_credit),
    interest_p50          = { x <- ac3_interest_rate[has_borrowed == 1 & !is.na(ac3_interest_rate) & ac3_interest_rate > 0]; median_pos(x) },
    interest_p90          = { x <- ac3_interest_rate[has_borrowed == 1 & !is.na(ac3_interest_rate) & ac3_interest_rate > 0]; p90_pos(x) },
    rev_mean              = mean_pos(rev_annual),
    rev_median            = median_pos(rev_annual),
    rev_p90               = p90_pos(rev_annual),
    rev_log_mean          = mean01(log1p(rev_annual[!is.na(rev_annual) & rev_annual > 0])),
    exp_median            = median_pos(exp_annual),
    cap_mean              = mean_pos(cap_total),
    cap_median            = median_pos(cap_total),
    cap_p90               = p90_pos(cap_total),
    fixcap_median         = median_pos(cap_fixed),
    share_any_foreign_cap = mean01(has_foreign_capital),
    mean_foreign_cap_ratio = mean01(pmin(pmax(cap_foreign_ratio, 0), 1)),
    .groups = "drop"
  )

# ---- 8. Productivity -----------------------------------------------------
dist_prod <- firm |>
  group_by(DIST) |>
  summarise(
    labor_prod_median            = median_pos(labor_productivity),
    labor_prod_p90               = p90_pos(labor_productivity),
    value_added_pw_median        = median_pos(value_added_pw),
    value_added_pw_p90           = p90_pos(value_added_pw),
    capital_intensity_median     = median_pos(capital_intensity),
    capital_productivity_median  = median_pos(capital_productivity),
    wage_share_median            = median_pos(wage_share_of_exp),
    profit_margin_median         = { x <- profit_margin[!is.na(profit_margin)]; if (!length(x)) NA_real_ else median(x) },
    .groups = "drop"
  )

# ---- 9. Gender ------------------------------------------------------------
dist_gender <- firm |>
  group_by(DIST) |>
  summarise(
    share_female_manager = mean01(female_manager),
    share_female_owner   = mean01(female_owner),
    share_female_led     = mean01(female_led),
    share_female_workers = { tot <- sum(nz(pe_tot)); if (tot > 0) sum(nz(pe_nf)) / tot else NA_real_ },
    .groups = "drop"
  ) |>
  rowwise() |>
  mutate(gender_inclusion_index = mean(c(share_female_manager, share_female_owner,
                                          share_female_workers), na.rm = TRUE)) |>
  ungroup()

# ---- 10. Firm age --------------------------------------------------------
dist_age <- firm |>
  group_by(DIST) |>
  summarise(
    share_firms_young_5y   = mean01(is_young_firm),
    share_firms_mature_10y = mean01(firm_age_years > 10),
    median_firm_age        = median_pos(firm_age_years),
    p90_firm_age           = p90_pos(firm_age_years),
    .groups = "drop"
  )

# ---- 11. Tenure / structure -----------------------------------------------
dist_struct <- firm |>
  group_by(DIST) |>
  summarise(
    share_owns_building = mean01(owns_building),
    share_owns_land     = mean01(owns_land),
    share_has_branches  = mean01(has_branches),
    share_has_parent    = mean01(has_parent),
    share_multinational = mean01(is_multinational),
    .groups = "drop"
  )

# ---- Assemble wide -------------------------------------------------------
district <- dist_core |>
  left_join(dist_emp,         by = "DIST") |>
  left_join(dist_size,        by = "DIST") |>
  left_join(dist_sector,      by = "DIST") |>
  left_join(dist_trade,       by = "DIST") |>
  left_join(dist_ag,          by = "DIST") |>
  left_join(dist_mtier,       by = "DIST") |>
  left_join(dist_modern,      by = "DIST") |>
  left_join(dist_sector_size, by = "DIST") |>
  left_join(dist_finance,     by = "DIST") |>
  left_join(dist_prod,        by = "DIST") |>
  left_join(dist_gender,      by = "DIST") |>
  left_join(dist_age,         by = "DIST") |>
  left_join(dist_struct,      by = "DIST") |>
  arrange(DIST)

write_csv(district, "data/clean/nec2018/district_analysis.csv")
cat("Wrote data/clean/nec2018/district_analysis.csv  |  ",
    nrow(district), "rows x ", ncol(district), "cols\n")

# ---- Sanity --------------------------------------------------------------
cat("\n-- Top 10 districts by firm count --\n")
print(district |>
  arrange(desc(n_firms)) |>
  select(DIST, n_firms, n_lgcodes, emp_total, share_registered, labor_prod_median) |>
  head(10))
