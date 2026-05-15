################################################################################
# Build mun_entry_panel_new.csv from entry_cohort_panel.csv.
# Renames n_firms_surviving_* columns to the shorter new_firms_* names that
# robustness_final.R / robustness_final_fill.R expect.
#
# Run AFTER 04_entry_cohort_panel.R:
#   source("script/build_mun_entry_panel_new.R")
################################################################################

suppressPackageStartupMessages({ library(tidyverse) })

IN  <- "data/clean/nec2018/entry_cohort_panel.csv"
OUT <- "data/clean/nec2018/mun_entry_panel_new.csv"

if (!file.exists(IN))
  stop("Run script/archive/municipality/vars/nec2018/04_entry_cohort_panel.R first to create ", IN)

ec <- read_csv(IN, show_col_types = FALSE)
cat(sprintf("Read %s: %d rows x %d cols\n", IN, nrow(ec), ncol(ec)))

# Sector short tag -> portal sector label (matches robustness_final.R + portal JSON)
SECTOR_MAP <- c(
  agro          = "agriculture",
  manuf         = "manufacturing",
  construct     = "construction",
  wholesale     = "trade_retail",
  hospitality   = "hospitality_food",
  transport     = "transport_storage",
  finance       = "finance_prof_realestate",
  real_estate   = "finance_prof_realestate",
  prof_tech     = "finance_prof_realestate",
  info_comm     = "finance_prof_realestate",
  education     = "education_health_social",
  health        = "education_health_social",
  public_admin  = "education_health_social",
  # everything else -> other_services
  mining        = "other_services",
  energy        = "other_services",
  water_waste   = "other_services",
  admin_sup     = "other_services",
  arts          = "other_services",
  services      = "other_services",
  hh_prod       = "other_services",
  extra_orgs    = "other_services"
)

# Size column rename map
SIZE_MAP <- c(
  "n_firms_surviving_size_micro_1"      = "new_firms_size_1_worker",
  "n_firms_surviving_size_small_2_9"    = "new_firms_size_2_9_workers",
  "n_firms_surviving_size_medium_10_50" = "new_firms_size_10_50_workers",
  "n_firms_surviving_size_large_51p"    = "new_firms_size_51plus_workers"
)

# Start the output: rename year column and core counts
out <- ec %>%
  rename(year = founding_year_ad,
         new_firms = n_firms_surviving,
         emp_total = emp_surviving,
         rev_total = rev_surviving,
         cap_total = cap_surviving)

# Rename size columns where present
for (oldn in names(SIZE_MAP)) {
  if (oldn %in% names(out)) {
    names(out)[names(out) == oldn] <- SIZE_MAP[[oldn]]
  }
}

# Collapse sectors into portal groupings
sec_cols <- grep("^n_firms_surviving_sec_", names(out), value = TRUE)
if (length(sec_cols)) {
  # Long form
  sec_long <- out %>%
    select(lgcode, year, all_of(sec_cols)) %>%
    pivot_longer(cols = -c(lgcode, year),
                 names_to = "sec_raw", values_to = "n", values_drop_na = TRUE) %>%
    mutate(sec_short = sub("^n_firms_surviving_sec_", "", sec_raw),
           sec_grp   = SECTOR_MAP[sec_short]) %>%
    filter(!is.na(sec_grp))
  # Aggregate to portal sector groups
  sec_wide <- sec_long %>%
    group_by(lgcode, year, sec_grp) %>%
    summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = sec_grp, values_from = n,
                names_prefix = "new_firms_", values_fill = 0)
  out <- out %>% select(-all_of(sec_cols)) %>%
    left_join(sec_wide, by = c("lgcode", "year"))
}

# Drop the rest of the surviving_* columns we don't need for the portal
drop_cols <- grep("^n_firms_surviving", names(out), value = TRUE)
if (length(drop_cols)) out <- out %>% select(-all_of(drop_cols))

dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
write_csv(out, OUT)
cat(sprintf("Wrote %s: %d rows x %d cols\n", OUT, nrow(out), ncol(out)))
cat("Columns:\n"); print(names(out))
