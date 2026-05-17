################################################################################
# Build docs/district_robustness.json for the District results portal.
# ---------------------------------------------------------------------------
# Reads source CSVs directly (no R aggregator dependency):
#   - district-analysis/output/tab/robustness_all_panels.csv
#   - district-analysis/output/tab/robustness_drop_districts.csv  (optional)
#
# Output:
#   - docs/district_robustness.json   (consumed by docs/district_robustness.html)
#
# Run: source("district-analysis/script/build_district_robustness_json.R")
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})

t0 <- Sys.time()

# ---- Outcome -> (group, label) maps ---------------------------------------
# Groups follow muni-style naming (capitalised, em-dash). Outcomes outside
# these maps are dropped from the portal. To add/remove an outcome, edit
# the relevant DS_MAP entry.

CENSUS_MAP <- list(
  # MIGRATION (in/out/net, lifetime + 5-yr; reason splits for 5-yr only)
  mig_in_internal_share          = c("Migration", "In-migrant share (lifetime)"),
  mig_out_internal_share         = c("Migration", "Out-migrant share (lifetime)"),
  net_internal_mig_share         = c("Migration", "Net internal migration (lifetime)"),
  mig_in_temp_share              = c("Migration", "In-mig (5-yr, temporary)"),
  mig_out_temp_share             = c("Migration", "Out-mig (5-yr, temporary)"),
  net_temp_mig_share             = c("Migration", "Net migration (5-yr, temporary)"),
  mig_in_temp_economic_share     = c("Migration", "In-mig 5-yr, economic reason"),
  mig_in_temp_noneconomic_share  = c("Migration", "In-mig 5-yr, non-economic reason"),
  mig_out_temp_economic_share    = c("Migration", "Out-mig 5-yr, economic reason"),
  mig_out_temp_noneconomic_share = c("Migration", "Out-mig 5-yr, non-economic reason"),
  # ASSETS
  amen_assets_radio        = c("Assets", "Radio"),
  amen_assets_tv           = c("Assets", "Television"),
  amen_assets_landline     = c("Assets", "Landline phone"),
  amen_assets_mobile       = c("Assets", "Mobile phone"),
  amen_assets_computer     = c("Assets", "Computer"),
  amen_assets_internet     = c("Assets", "Internet"),
  amen_assets_cycle        = c("Assets", "Bicycle"),
  amen_assets_motorcycle   = c("Assets", "Motorcycle"),
  amen_assets_car          = c("Assets", "Car"),
  amen_assets_fridge       = c("Assets", "Refrigerator"),
  amen_asset_count_mean    = c("Assets", "Mean asset count"),
  # AMENITIES
  amen_water_piped         = c("Amenities", "Piped water"),
  amen_water_traditional   = c("Amenities", "Traditional water source"),
  amen_cooking_wood        = c("Amenities", "Wood/firewood cooking"),
  amen_cooking_kerosene    = c("Amenities", "Kerosene cooking"),
  amen_cooking_lpg         = c("Amenities", "LPG cooking"),
  amen_cooking_biogas      = c("Amenities", "Biogas cooking"),
  amen_cooking_electric    = c("Amenities", "Electric cooking"),
  amen_cooking_modern      = c("Amenities", "Modern cooking fuel"),
  amen_cooking_traditional = c("Amenities", "Traditional cooking fuel"),
  amen_lighting_electricity= c("Amenities", "Lighting: electricity"),
  amen_lighting_kerosene   = c("Amenities", "Lighting: kerosene"),
  amen_lighting_biogas     = c("Amenities", "Lighting: biogas"),
  amen_lighting_others     = c("Amenities", "Lighting: other"),
  amen_toilet_modern       = c("Amenities", "Toilet: modern"),
  amen_toilet_ordinary     = c("Amenities", "Toilet: ordinary"),
  amen_toilet_any          = c("Amenities", "Toilet: any"),
  amen_toilet_none         = c("Amenities", "Toilet: none"),
  # HOUSING
  housing_own                    = c("Housing", "Own dwelling"),
  housing_rented                 = c("Housing", "Rented dwelling"),
  housing_foundation_modern      = c("Housing", "Modern foundation"),
  housing_foundation_traditional = c("Housing", "Traditional foundation"),
  housing_roof_modern            = c("Housing", "Modern roof"),
  housing_roof_traditional       = c("Housing", "Traditional roof"),
  # INDUSTRY
  ind_agri_forestry_fish       = c("Industry", "Agriculture, forestry, fishing"),
  ind_manufacturing            = c("Industry", "Manufacturing"),
  ind_construction             = c("Industry", "Construction"),
  ind_wholesale_retail         = c("Industry", "Wholesale & retail"),
  ind_transport_accommodation  = c("Industry", "Transport & accommodation"),
  ind_finance_real_estate_prof = c("Industry", "Finance / RE / professional"),
  ind_public_admin_defence     = c("Industry", "Public admin & defence"),
  ind_education                = c("Industry", "Education"),
  ind_health                   = c("Industry", "Health"),
  ind_arts_recreation          = c("Industry", "Arts & recreation"),
  ind_others                   = c("Industry", "Other industry"),
  # OCCUPATION
  occ_share_armed_forces      = c("Occupation", "Armed forces"),
  occ_share_managers          = c("Occupation", "Managers"),
  occ_share_professionals     = c("Occupation", "Professionals"),
  occ_share_technicians       = c("Occupation", "Technicians"),
  occ_share_office_assistants = c("Occupation", "Office assistants"),
  occ_share_service_sales     = c("Occupation", "Service & sales"),
  occ_share_agriculture       = c("Occupation", "Agricultural occupations"),
  occ_share_craft_trades      = c("Occupation", "Craft & trades"),
  occ_share_machine_operators = c("Occupation", "Machine operators"),
  occ_share_elementary        = c("Occupation", "Elementary occupations"),
  # EMPLOYMENT STATUS
  emp_share_employee             = c("Employment Status", "Wage employee"),
  emp_share_employer             = c("Employment Status", "Employer"),
  emp_share_self_employed        = c("Employment Status", "Self-employed"),
  emp_share_unpaid_family_worker = c("Employment Status", "Unpaid family worker"),
  # EDUCATION
  edu_literate                  = c("Education", "Literacy"),
  edu_literate_female           = c("Education", "Female literacy"),
  edu_literate_male             = c("Education", "Male literacy"),
  edu_school_attend_6_16        = c("Education", "School attendance (6-16)"),
  edu_school_attend_6_16_female = c("Education", "School attendance (6-16) -- female"),
  edu_school_attend_6_16_male   = c("Education", "School attendance (6-16) -- male")
)

HH_MAP <- list(
  # Migration -- HH
  has_migrant_intl           = c("Migration -- HH", "Has international migrant"),
  n_intl_migrants            = c("Migration -- HH", "# international migrants"),
  remit_amount_intl_12m_rs   = c("Migration -- HH", "Intl remittance, 12m (Rs)"),
  remit_received             = c("Migration -- HH", "Any remittance received"),
  # HH spending (merges food / non-food / education / health)
  food_exp_total_7day        = c("HH spending", "Food exp, total 7d"),
  food_exp_purchased_7day    = c("HH spending", "Food exp, purchased 7d"),
  food_exp_homeprod_7day     = c("HH spending", "Food exp, home-produced 7d"),
  nonfood_exp_12m            = c("HH spending", "Non-food exp, 12m"),
  edu_spend_total_12m        = c("HH spending", "Education spend, 12m"),
  hlt_spend_total            = c("HH spending", "Health spend"),
  # Land use -- wet/dry
  share_self_wet             = c("Land use -- wet/dry", "Own-cultivated, wet"),
  share_self_dry             = c("Land use -- wet/dry", "Own-cultivated, dry"),
  share_both_seasons         = c("Land use -- wet/dry", "Cultivated, both seasons"),
  share_fallow_wet           = c("Land use -- wet/dry", "Fallow, wet"),
  share_fallow_dry           = c("Land use -- wet/dry", "Fallow, dry"),
  share_rented_out_wet       = c("Land use -- wet/dry", "Rented out, wet"),
  # Land -- agriculture
  owns_plough                = c("Land -- agriculture", "Owns plough"),
  owns_powered_machinery     = c("Land -- agriculture", "Owns powered machinery"),
  owns_irrigation_kit        = c("Land -- agriculture", "Owns irrigation kit"),
  owns_storage_struct        = c("Land -- agriculture", "Owns storage"),
  owns_transport             = c("Land -- agriculture", "Owns transport"),
  n_equip_categories         = c("Land -- agriculture", "# equipment categories"),
  n_powered_types            = c("Land -- agriculture", "# powered equipment types"),
  equip_stock_value_rs       = c("Land -- agriculture", "Equipment stock value (Rs)"),
  total_input_cost_rs        = c("Land -- agriculture", "Total input cost (Rs)"),
  input_intensity_per_sqm    = c("Land -- agriculture", "Input intensity / sqm"),
  wet_cost_seed              = c("Land -- agriculture", "Wet: seed cost"),
  dry_cost_seed              = c("Land -- agriculture", "Dry: seed cost"),
  wet_cost_fert              = c("Land -- agriculture", "Wet: fertilizer cost"),
  dry_cost_fert              = c("Land -- agriculture", "Dry: fertilizer cost"),
  wet_cost_labour            = c("Land -- agriculture", "Wet: labour cost"),
  dry_cost_labour            = c("Land -- agriculture", "Dry: labour cost"),
  wet_cost_insect            = c("Land -- agriculture", "Wet: insecticide cost"),
  dry_cost_insect            = c("Land -- agriculture", "Dry: insecticide cost"),
  # Enterprise
  has_enterprise             = c("Enterprise", "HH operates enterprise"),
  n_enterprises              = c("Enterprise", "# enterprises"),
  n_workers_total            = c("Enterprise", "# workers"),
  revenue_12m                = c("Enterprise", "Revenue, 12m"),
  profit_12m                 = c("Enterprise", "Profit, 12m"),
  expenses_12m               = c("Enterprise", "Expenses, 12m"),
  capex_12m                  = c("Enterprise", "Capex, 12m"),
  sector_trade               = c("Enterprise", "Sector: trade"),
  sector_manufacturing       = c("Enterprise", "Sector: manufacturing"),
  sector_services            = c("Enterprise", "Sector: services"),
  sector_hotels              = c("Enterprise", "Sector: hospitality"),
  sector_transport           = c("Enterprise", "Sector: transport")
)

NEC_CS_MAP <- list(
  # Size & scale (merge Size -- counts + Size -- distribution into one)
  n_firms                       = c("Firm size & scale", "# firms"),
  emp_total                     = c("Firm size & scale", "Total employment"),
  mean_emp_per_firm             = c("Firm size & scale", "Mean employment per firm"),
  share_firms_size_micro_1      = c("Firm size & scale", "Share micro (1 worker)"),
  share_firms_size_small_2_9    = c("Firm size & scale", "Share small (2-9)"),
  share_firms_size_medium_10_50 = c("Firm size & scale", "Share medium (10-50)"),
  share_firms_size_large_51p    = c("Firm size & scale", "Share large (51+)"),
  share_emp_foreign             = c("Firm size & scale", "Share foreign employees"),
  # Formality & credit (merge Formality + Credit & finance + Female-led firms)
  formality_index               = c("Formality & credit", "Formality index"),
  share_registered              = c("Formality & credit", "Share registered"),
  share_tax_registered          = c("Formality & credit", "Share tax-registered"),
  share_keeps_accounts          = c("Formality & credit", "Share keeps accounts"),
  share_formal_credit           = c("Formality & credit", "Share with formal credit"),
  share_borrowed_any            = c("Formality & credit", "Share borrowed (any)"),
  share_emp_female              = c("Formality & credit", "Share female employees"),
  share_any_foreign_cap         = c("Formality & credit", "Share with foreign capital")
)

NEC_PANEL_MAP <- list(
  # Firm entry (merges 'total' + 'by size' to avoid the singleton total group)
  n_new_firms                          = c("Firm entry & size", "# new firms"),
  log_n_new_firms                      = c("Firm entry & size", "log(1 + new firms)"),
  n_new_firms_size_micro_1             = c("Firm entry & size", "Size: micro (1 worker)"),
  log_n_new_firms_size_micro_1         = c("Firm entry & size", "log: micro (1 worker)"),
  n_new_firms_size_small_2_9           = c("Firm entry & size", "Size: small (2-9)"),
  log_n_new_firms_size_small_2_9       = c("Firm entry & size", "log: small (2-9)"),
  n_new_firms_size_medium_10_50        = c("Firm entry & size", "Size: medium (10-50)"),
  log_n_new_firms_size_medium_10_50    = c("Firm entry & size", "log: medium (10-50)"),
  n_new_firms_size_large_51p           = c("Firm entry & size", "Size: large (51+)"),
  log_n_new_firms_size_large_51p       = c("Firm entry & size", "log: large (51+)"),
  # Entry by sector
  n_new_firms_agriculture              = c("Entry by sector", "Agriculture"),
  log_n_new_firms_agriculture          = c("Entry by sector", "log Agriculture"),
  n_new_firms_manufacturing            = c("Entry by sector", "Manufacturing"),
  log_n_new_firms_manufacturing        = c("Entry by sector", "log Manufacturing"),
  n_new_firms_construction             = c("Entry by sector", "Construction"),
  log_n_new_firms_construction         = c("Entry by sector", "log Construction"),
  n_new_firms_trade_retail             = c("Entry by sector", "Trade & retail"),
  log_n_new_firms_trade_retail         = c("Entry by sector", "log Trade & retail"),
  n_new_firms_hospitality_food         = c("Entry by sector", "Hospitality & food"),
  log_n_new_firms_hospitality_food     = c("Entry by sector", "log Hospitality & food"),
  n_new_firms_transport_storage        = c("Entry by sector", "Transport & storage"),
  log_n_new_firms_transport_storage    = c("Entry by sector", "log Transport & storage"),
  n_new_firms_finance_prof_realestate  = c("Entry by sector", "Finance / RE / prof"),
  log_n_new_firms_finance_prof_realestate = c("Entry by sector", "log Finance / RE / prof"),
  n_new_firms_education_health_social  = c("Entry by sector", "Education, health, social"),
  log_n_new_firms_education_health_social = c("Entry by sector", "log Education, health, social"),
  n_new_firms_other_services           = c("Entry by sector", "Other services"),
  log_n_new_firms_other_services       = c("Entry by sector", "log Other services")
)

DS_MAPS <- list(census = CENSUS_MAP, hh = HH_MAP, nec_cs = NEC_CS_MAP, nec_panel = NEC_PANEL_MAP)

# ---- Load source CSVs ------------------------------------------------------
GRID_MAIN <- "district-analysis/output/tab/robustness_all_panels.csv"
GRID_DROP <- "district-analysis/output/tab/robustness_drop_districts.csv"

rows_main <- read_csv(GRID_MAIN, show_col_types = FALSE) %>%
  mutate(variant = "baseline")
cat(sprintf("Main rows: %d\n", nrow(rows_main)))

rows_drop <- if (file.exists(GRID_DROP)) {
  read_csv(GRID_DROP, show_col_types = FALSE) %>%
    filter(variant != "loo") %>%
    mutate(scaling = "log", lag = 2L)
} else {
  cat("NOTE: drop_districts CSV not found; skipping drop variants.\n")
  tibble()
}
cat(sprintf("Drop rows: %d\n", nrow(rows_drop)))

# Combine
rows <- bind_rows(rows_main, rows_drop)

# ---- Apply portal transforms ----------------------------------------------
# 1) Drop "raw" scaling (keep log + lin only)
# 2) Drop M5 entirely (user request: 2001-baseline column removed)
# 3) Rename M2 -> A2, M3 -> A3, M4 -> A4
rows <- rows %>%
  filter(scaling != "raw", model != "M5") %>%
  mutate(model = case_when(model == "M2" ~ "A2",
                           model == "M3" ~ "A3",
                           model == "M4" ~ "A4",
                           TRUE ~ model))
cat(sprintf("After drop raw + drop M5 + rename: %d\n", nrow(rows)))

# Filter to outcomes in the portal maps
in_scope <- mapply(function(ds, oc) oc %in% names(DS_MAPS[[ds]]),
                   rows$dataset, rows$outcome)
rows <- rows[in_scope, ]
cat(sprintf("In-scope rows: %d\n", nrow(rows)))

# ---- Build JSON shape ------------------------------------------------------
clean_sig <- function(s) ifelse(is.na(s) | s == "NA" | s == "NaN", "", s)

build_dataset <- function(ds_label, mp) {
  sub <- rows[rows$dataset == ds_label, ]
  outs <- list()
  for (oc in names(mp)) {
    s <- sub[sub$outcome == oc, ]
    if (!nrow(s)) next
    # Cells dictionary
    cells <- list()
    any_nonnull <- FALSE
    for (i in seq_len(nrow(s))) {
      k <- sprintf("%s|%s|%s|%s", s$scaling[i], s$lag[i], s$model[i], s$variant[i])
      b <- s$beta[i]; se <- s$se[i]; p <- s$p[i]; n <- s$n[i]
      cells[[k]] <- list(
        beta = if (is.na(b))  NULL else unbox(b),
        se   = if (is.na(se)) NULL else unbox(se),
        p    = if (is.na(p))  NULL else unbox(p),
        n    = if (is.na(n))  NULL else unbox(as.integer(n)),
        sig  = unbox(clean_sig(s$sig[i]))
      )
      if (!is.na(b)) any_nonnull <- TRUE
    }
    # Skip outcomes whose every cell is null
    if (!any_nonnull) next
    my <- suppressWarnings(as.numeric(s$mean_y[1]))
    # Number of unique entities (districts/HHs) backing the regression.
    # Census/NEC panel: n / 2 (district x year, 2 years for census, ~8 for nec_panel)
    # HH: too noisy to back out -- omit
    # NEC cs: equals N
    sample_n <- suppressWarnings(as.integer(s$n[1]))
    n_unit_val <- NA_integer_
    if (!is.na(sample_n) && sample_n > 0) {
      if (ds_label == "census")    n_unit_val <- as.integer(sample_n / 2)
      else if (ds_label == "nec_cs") n_unit_val <- sample_n
      # else leave as NA (hh, nec_panel: not reliably derivable)
    }
    outs[[oc]] <- list(
      label  = unbox(mp[[oc]][2]),
      group  = unbox(mp[[oc]][1]),
      mean_y = if (is.na(my)) NA else unbox(my),
      cells  = cells
    )
    if (!is.na(n_unit_val)) outs[[oc]]$n_unit <- unbox(n_unit_val)
  }
  groups <- sort(unique(vapply(outs, function(o) as.character(o$group), character(1))))
  list(outcomes = outs, groups = groups)
}

out_json <- list(
  datasets_meta = list(
    census    = list(label = unbox("Census district panel (2011, 2021)")),
    hh        = list(label = unbox("HRVS HH panel (2016-18, district x year residualised)")),
    nec_cs    = list(label = unbox("NEC 2018 district cross-section")),
    nec_panel = list(label = unbox("NEC entry-cohort panel (2011-2018)"))
  ),
  datasets = list(
    census    = build_dataset("census",    CENSUS_MAP),
    hh        = build_dataset("hh",        HH_MAP),
    nec_cs    = build_dataset("nec_cs",    NEC_CS_MAP),
    nec_panel = build_dataset("nec_panel", NEC_PANEL_MAP)
  )
)

for (ds in names(out_json$datasets)) {
  cat(sprintf("  %s: %d outcomes in %d groups\n",
              ds,
              length(out_json$datasets[[ds]]$outcomes),
              length(out_json$datasets[[ds]]$groups)))
}

dir.create("docs", showWarnings = FALSE)
write_json(out_json, "docs/district_robustness.json",
           auto_unbox = FALSE, na = "null", pretty = FALSE)
cat(sprintf("\nWrote docs/district_robustness.json (%s bytes)\n",
            format(file.size("docs/district_robustness.json"), big.mark = ",")))
cat(sprintf("Elapsed: %.1f s\n", as.numeric(Sys.time() - t0, units = "secs")))
