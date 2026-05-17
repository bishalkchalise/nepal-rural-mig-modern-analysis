################################################################################
# Build NEC entry-cohort panel at DISTRICT level (from firm_level.csv).
# Mirrors script/archive/municipality/vars/nec2018/04_entry_cohort_panel.R but
# groups by DIST instead of lgcode.
#
# Output: district-analysis/data/clean/nec/nec_panel_district.csv
#   one row per (DIST, dname, founding_year_ad)
#
# CAVEAT: NEC 2018 only observes firms still operating in 2018, so cohort
# counts are SURVIVORS, not true entry counts.  Names use n_new_firms_*.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
})

FIRM_FILE <- "data/clean/nec2018/firm_level.csv"
if (!file.exists(FIRM_FILE)) {
  stop(sprintf("firm_level.csv not found at %s\nRun script/archive/municipality/vars/nec2018/02_firm_level_prep.R first.", FIRM_FILE))
}

COHORT_YEAR_MIN <- 1980
COHORT_YEAR_MAX <- 2018

DIST_LOOKUP <- c(
  "101"="Taplejung","102"="Sankhuwasabha","103"="Solukhumbu","104"="Okhaldhunga",
  "105"="Khotang","106"="Bhojpur","107"="Dhankuta","108"="Terhathum",
  "109"="Panchthar","110"="Ilam","111"="Jhapa","112"="Morang","113"="Sunsari",
  "114"="Udayapur","201"="Saptari","202"="Siraha","203"="Dhanusa","204"="Mahottari",
  "205"="Sarlahi","206"="Rautahat","207"="Bara","208"="Parsa",
  "301"="Dolakha","302"="Sindhupalchok","303"="Rasuwa","304"="Dhading",
  "305"="Nuwakot","306"="Kathmandu","307"="Bhaktapur","308"="Lalitpur",
  "309"="Kavrepalanchok","310"="Ramechhap","311"="Sindhuli","312"="Makwanpur",
  "313"="Chitawan","401"="Gorkha","402"="Manang","403"="Mustang","404"="Myagdi",
  "405"="Kaski","406"="Lamjung","407"="Tanahu","408"="Nawalparasi","409"="Syangja",
  "410"="Parbat","411"="Baglung","501"="Rukum","502"="Rolpa","503"="Pyuthan",
  "504"="Gulmi","505"="Arghakhanchi","506"="Palpa","507"="Nawalparasi",
  "508"="Rupandehi","509"="Kapilbastu","510"="Dang","511"="Banke","512"="Bardiya",
  "601"="Dolpa","602"="Mugu","603"="Humla","604"="Jumla","605"="Kalikot",
  "606"="Dailekh","607"="Jajarkot","608"="Rukum","609"="Salyan","610"="Surkhet",
  "701"="Bajura","702"="Bajhang","703"="Darchula","704"="Baitadi","705"="Dadeldhura",
  "706"="Doti","707"="Achham","708"="Kailali","709"="Kanchanpur"
)

nz <- function(x) replace(x, is.na(x), 0)

firm <- read_csv(FIRM_FILE, show_col_types = FALSE, guess_max = 1e5)
cat("Firm-level rows:", nrow(firm), "\n")

firm_cohort <- firm %>%
  filter(!is.na(founding_year_ad),
         founding_year_ad >= COHORT_YEAR_MIN,
         founding_year_ad <= COHORT_YEAR_MAX,
         !is.na(DIST))
cat("Firms in cohort window:", nrow(firm_cohort), "\n")

# Core counts per (DIST, founding_year_ad)
panel_core <- firm_cohort %>%
  group_by(DIST, founding_year_ad) %>%
  summarise(
    n_new_firms = n(),
    emp_new_firms     = sum(nz(pe_tot)),
    rev_new_firms     = sum(nz(rev_annual)),
    cap_new_firms     = sum(nz(cap_total)),
    .groups = "drop"
  )

# By size category
SIZE_LEVELS <- c("micro_1", "small_2_9", "medium_10_50", "large_51p")
panel_size <- firm_cohort %>%
  filter(!is.na(size_cat)) %>%
  mutate(size_cat = factor(size_cat, levels = SIZE_LEVELS)) %>%
  group_by(DIST, founding_year_ad, size_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(id_cols = c(DIST, founding_year_ad),
              names_from = size_cat,
              names_glue = "n_new_firms_size_{size_cat}",
              values_from = n, values_fill = 0)

# Sector groupings matching the robustness portal (9 broad sectors)
SECTOR_GROUP <- c(
  agro          = "agriculture",
  mining        = "other_services",
  manuf         = "manufacturing",
  energy        = "other_services",
  water_waste   = "other_services",
  construct     = "construction",
  wholesale     = "trade_retail",
  transport     = "transport_storage",
  hospitality   = "hospitality_food",
  info_comm     = "finance_prof_realestate",
  finance       = "finance_prof_realestate",
  real_estate   = "finance_prof_realestate",
  prof_tech     = "finance_prof_realestate",
  admin_sup     = "other_services",
  public_admin  = "education_health_social",
  education     = "education_health_social",
  health        = "education_health_social",
  arts          = "other_services",
  services      = "other_services",
  hh_prod       = "other_services",
  extra_orgs    = "other_services"
)

# By sector (using broad groupings)
panel_sector <- firm_cohort %>%
  filter(!is.na(sector_short)) %>%
  mutate(sector_grp = SECTOR_GROUP[sector_short]) %>%
  filter(!is.na(sector_grp)) %>%
  group_by(DIST, founding_year_ad, sector_grp) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(id_cols = c(DIST, founding_year_ad),
              names_from = sector_grp,
              names_glue = "n_new_firms_{sector_grp}",
              values_from = n, values_fill = 0)

district_panel <- panel_core %>%
  left_join(panel_size,   by = c("DIST","founding_year_ad")) %>%
  left_join(panel_sector, by = c("DIST","founding_year_ad")) %>%
  arrange(DIST, founding_year_ad) %>%
  mutate(dname = DIST_LOOKUP[as.character(DIST)],
         year  = founding_year_ad)

# Collapse split-district duplicates (Nawalparasi 408+507 -> "Nawalparasi",
# Rukum 501+608 -> "Rukum") so the panel matches the 75-district codeframe
# used by mi / z_v2 / regions / outcomes_district.
count_cols <- setdiff(names(district_panel)[vapply(district_panel, is.numeric, logical(1))],
                      c("DIST", "founding_year_ad", "year"))
district_panel <- district_panel %>%
  group_by(dname, year) %>%
  summarise(
    DIST              = first(DIST),
    founding_year_ad  = first(founding_year_ad),
    across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# Add log versions
log_cols <- c("n_new_firms","emp_new_firms","rev_new_firms","cap_new_firms",
              grep("^n_new_firms_size_", names(district_panel), value = TRUE),
              grep("^n_new_firms_", names(district_panel), value = TRUE))
log_cols <- intersect(log_cols, names(district_panel))
for (c in log_cols) {
  district_panel[[paste0("log_", c)]] <- log(pmax(district_panel[[c]], 0) + 1)
}

dir.create("district-analysis/data/clean/nec", showWarnings = FALSE, recursive = TRUE)
write_csv(district_panel, "district-analysis/data/clean/nec/nec_panel_district.csv")
cat(sprintf("Saved: district-analysis/data/clean/nec/nec_panel_district.csv (%d rows, %d districts, %d cohort years)\n",
            nrow(district_panel),
            n_distinct(district_panel$DIST),
            n_distinct(district_panel$founding_year_ad)))
