##############################################################################
# script/nec2018/04_entry_cohort_panel.R
##############################################################################
#
# Builds a long-format panel: one row per (lgcode, founding_year_ad) with
# survival counts broken out by size, sector, and four classification schemes.
#
# IMPORTANT CAVEAT — survivor bias:
# NEC 2018 only observes firms still operating in 2018. Therefore counts
# of firms "founded in year X" are actually SURVIVORS from that cohort,
# NOT true entry counts. Column names use `n_firms_surviving_*` to make
# this explicit. Do not interpret cross-cohort comparisons as entry rates.
#
# Cohort window: 1980-2018 (configurable below).
#
# Input:  data/clean/nec2018/firm_level.csv
# Output: data/clean/nec2018/entry_cohort_panel.csv
#
##############################################################################

library(tidyverse)

# ---- Cohort window --------------------------------------------------------
COHORT_YEAR_MIN <- 1980
COHORT_YEAR_MAX <- 2018

# ---- Helpers --------------------------------------------------------------
nz <- function(x) replace(x, is.na(x), 0)

# ---- Read firm-level ------------------------------------------------------
firm <- read_csv("data/clean/nec2018/firm_level.csv",
                 show_col_types = FALSE, guess_max = 1e5)
cat("Firm-level rows:", nrow(firm), "\n")

# ---- Filter to cohort window ---------------------------------------------
firm_cohort <- firm |>
  filter(!is.na(founding_year_ad),
         founding_year_ad >= COHORT_YEAR_MIN,
         founding_year_ad <= COHORT_YEAR_MAX,
         !is.na(lgcode))

cat("Firms in cohort window (", COHORT_YEAR_MIN, "-", COHORT_YEAR_MAX, "): ",
    nrow(firm_cohort), " / ", nrow(firm),
    " (", round(100 * nrow(firm_cohort) / nrow(firm), 1), "%)\n", sep = "")

# ---- Core counts per (lgcode, founding_year_ad) --------------------------
panel_core <- firm_cohort |>
  group_by(lgcode, founding_year_ad) |>
  summarise(
    n_firms_surviving     = n(),
    emp_surviving         = sum(nz(pe_tot)),
    rev_surviving         = sum(nz(rev_annual)),
    cap_surviving         = sum(nz(cap_total)),
    median_firm_age_years = median(firm_age_years, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Size breakdown -------------------------------------------------------
panel_size <- firm_cohort |>
  filter(!is.na(size_cat)) |>
  count(lgcode, founding_year_ad, size_cat) |>
  pivot_wider(names_from = size_cat, values_from = n,
              names_prefix = "n_firms_surviving_size_", values_fill = 0)

# ---- Sector breakdown (short sector tags) --------------------------------
panel_sector <- firm_cohort |>
  filter(!is.na(sector_short)) |>
  count(lgcode, founding_year_ad, sector_short) |>
  pivot_wider(names_from = sector_short, values_from = n,
              names_prefix = "n_firms_surviving_sec_", values_fill = 0)

# ---- Tradability breakdown ------------------------------------------------
panel_trade <- firm_cohort |>
  filter(!is.na(tradability)) |>
  count(lgcode, founding_year_ad, tradability) |>
  pivot_wider(names_from = tradability, values_from = n,
              names_prefix = "n_firms_surviving_trd_", values_fill = 0)

# ---- Ag orientation breakdown --------------------------------------------
panel_ag <- firm_cohort |>
  filter(!is.na(ag_orientation)) |>
  count(lgcode, founding_year_ad, ag_orientation) |>
  pivot_wider(names_from = ag_orientation, values_from = n,
              names_prefix = "n_firms_surviving_agor_", values_fill = 0)

# ---- Manufacturing tier breakdown (manufacturing firms only) -------------
panel_mtier <- firm_cohort |>
  filter(!is.na(manuf_tier), manuf_tier != "not_manuf") |>
  count(lgcode, founding_year_ad, manuf_tier) |>
  pivot_wider(names_from = manuf_tier, values_from = n,
              names_prefix = "n_firms_surviving_mtier_", values_fill = 0)

# ---- Modernity breakdown --------------------------------------------------
panel_modern <- firm_cohort |>
  filter(!is.na(modernity)) |>
  count(lgcode, founding_year_ad, modernity) |>
  pivot_wider(names_from = modernity, values_from = n,
              names_prefix = "n_firms_surviving_modern_", values_fill = 0)

# ---- Assemble long panel --------------------------------------------------
entry_cohort <- panel_core |>
  left_join(panel_size,   by = c("lgcode", "founding_year_ad")) |>
  left_join(panel_sector, by = c("lgcode", "founding_year_ad")) |>
  left_join(panel_trade,  by = c("lgcode", "founding_year_ad")) |>
  left_join(panel_ag,     by = c("lgcode", "founding_year_ad")) |>
  left_join(panel_mtier,  by = c("lgcode", "founding_year_ad")) |>
  left_join(panel_modern, by = c("lgcode", "founding_year_ad")) |>
  mutate(across(starts_with("n_firms_surviving_"), ~ replace_na(., 0L))) |>
  arrange(lgcode, founding_year_ad)

dir.create("data/clean/nec2018", recursive = TRUE, showWarnings = FALSE)
write_csv(entry_cohort, "data/clean/nec2018/entry_cohort_panel.csv")
cat("Wrote data/clean/nec2018/entry_cohort_panel.csv  |  ",
    nrow(entry_cohort), "rows x ", ncol(entry_cohort), "cols\n")

# ---- Sanity --------------------------------------------------------------
cat("\n-- Panel summary --\n")
cat("Unique municipalities:", n_distinct(entry_cohort$lgcode), "\n")
cat("Unique founding years:", n_distinct(entry_cohort$founding_year_ad), "\n")
cat("Year range: ", min(entry_cohort$founding_year_ad), " - ",
    max(entry_cohort$founding_year_ad), "\n", sep = "")
cat("Total surviving firms in panel:", sum(entry_cohort$n_firms_surviving), "\n")

cat("\n-- Surviving firms by founding year (head and tail) --\n")
yr_totals <- entry_cohort |>
  group_by(founding_year_ad) |>
  summarise(total_surviving = sum(n_firms_surviving),
            total_emp       = sum(emp_surviving),
            .groups = "drop") |>
  arrange(founding_year_ad)

n_yr <- nrow(yr_totals)
print(bind_rows(head(yr_totals, 5),
                tibble(founding_year_ad = NA, total_surviving = NA, total_emp = NA),
                tail(yr_totals, 10)))
