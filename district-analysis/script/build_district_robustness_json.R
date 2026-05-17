################################################################################
# Build unified portal CSV + JSON for the district robustness page
# ---------------------------------------------------------------------------
# Inputs:
#   - district-analysis/output/tab/robustness_all_panels.csv
#       (scaling x lag x model grid, baseline 75 districts)
#   - district-analysis/output/tab/robustness_drop_districts.csv
#       (per-variant cells: baseline / drop_ktm_valley / drop_low_mig + LOO)
#
# Computes:
#   - sd_y  per (dataset, outcome) from the source panels (SKIP_RUN sourcing)
#   - n_unit per (dataset, outcome) = distinct dnames/hhids in regression sample
#
# Outputs:
#   - district-analysis/output/tab/district_robustness_grid.csv
#       Single flat CSV. Columns:
#         dataset, outcome, label, group,
#         scaling, lag, model, variant,
#         beta, se, p, sig, mean_y, sd_y, n, n_unit
#       Used both as the portal's single source of truth and for downstream
#       diagnostics / paper tables.
#   - docs/district_robustness.json
#       Same data restructured for the portal page consumer.
#
# Run: source("district-analysis/script/build_district_robustness_json.R")
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

t0 <- Sys.time()

SKIP_RUN <- TRUE
suppressMessages(suppressWarnings({
  source("district-analysis/script/_robustness_all_panels.R")
}))

# ---- Outcome -> group + label dictionary ---------------------------------
# Hand-curated to match the analysis story; extend as needed.
GROUP_OF <- function(o, ds) {
  if (ds == "census") {
    if (startsWith(o, "mig_") && grepl("temp_", o)) return("migration_temp")
    if (startsWith(o, "mig_") || startsWith(o, "net_") || o == "absent_hh_share") return("migration_perm")
    if (startsWith(o, "ind_"))   return("industry")
    if (startsWith(o, "occ_"))   return("occupation")
    if (startsWith(o, "work_share_") || startsWith(o, "emp_share_")) return("work_status")
    if (startsWith(o, "flfp_") || startsWith(o, "mlfp_") || startsWith(o, "fem_") ||
        o == "gap_lfp_m_minus_f") return("lfp")
    if (startsWith(o, "edu_"))   return("education")
    if (startsWith(o, "mar_"))   return("marriage")
    if (startsWith(o, "amen_assets_") || o == "amen_asset_count_mean") return("assets")
    if (startsWith(o, "amen_lighting_")) return("amenities_lighting")
    if (startsWith(o, "amen_cooking_")) return("amenities_cooking")
    if (startsWith(o, "amen_water_") || startsWith(o, "amen_toilet_")) return("amenities_water_toilet")
    if (startsWith(o, "housing_")) return("housing")
    return("census_other")
  }
  if (ds == "hh") {
    if (o %in% c("has_migrant_intl","n_intl_migrants","remit_received",
                  "remit_amount_intl_12m_rs")) return("hh_migration")
    if (startsWith(o, "share_self_") || startsWith(o, "share_fallow_") ||
        startsWith(o, "share_rented_") || startsWith(o, "share_both_") ||
        startsWith(o, "owns_") || startsWith(o, "n_equip") || startsWith(o, "n_powered") ||
        grepl("^(wet|dry)_cost", o) || o %in% c("equip_stock_value_rs",
        "total_input_cost_rs","input_intensity_per_sqm")) return("hh_ag")
    if (grepl("^food_", o) || grepl("^nonfood_", o) || o == "edu_spend_total_12m" ||
        o == "hlt_spend_total") return("hh_consumption")
    if (o %in% c("has_enterprise","n_enterprises","n_workers_total","revenue_12m",
                 "profit_12m","expenses_12m","capex_12m") || startsWith(o, "sector_"))
      return("hh_enterprise")
    return("hh_other")
  }
  if (ds == "nec_cs") {
    if (startsWith(o, "share_firms_size_") || o %in% c("n_firms","emp_total","mean_emp_per_firm"))
      return("nec_scale")
    if (startsWith(o, "share_emp_") || startsWith(o, "share_any_foreign_")) return("nec_workforce")
    if (o %in% c("share_registered","share_tax_registered","share_keeps_accounts",
                 "share_formal_credit","share_borrowed_any","formality_index")) return("nec_formality")
    return("nec_other")
  }
  if (ds == "nec_panel") {
    if (grepl("_size_", o)) return("nec_cohort_size")
    if (o == "log_n_new_firms" || o == "log_emp_new_firms" ||
        o == "log_rev_new_firms" || o == "log_cap_new_firms") return("nec_cohort_aggregate")
    return("nec_cohort_sector")
  }
  "other"
}

LABEL_OVERRIDES <- list(
  "mig_in_internal_share"          = "In-migration share (lifetime, born elsewhere)",
  "mig_out_internal_share"         = "Out-migration share (lifetime, born here, now elsewhere)",
  "net_internal_mig_share"         = "Net internal migration share (lifetime)",
  "mig_in_temp_share"              = "In-migration share (5-yr, recent flow)",
  "mig_out_temp_share"             = "Out-migration share (5-yr)",
  "net_temp_mig_share"             = "Net internal migration share (5-yr)",
  "mig_in_temp_economic_share"     = "In-mig 5-yr, economic reason",
  "mig_in_temp_noneconomic_share"  = "In-mig 5-yr, non-economic reason",
  "mig_out_temp_economic_share"    = "Out-mig 5-yr, economic reason",
  "mig_out_temp_noneconomic_share" = "Out-mig 5-yr, non-economic reason",
  "mig_in_economic_share"          = "In-mig lifetime, economic reason",
  "mig_in_noneconomic_share"       = "In-mig lifetime, non-economic reason",
  "mig_out_economic_share"         = "Out-mig lifetime, economic reason",
  "mig_out_noneconomic_share"      = "Out-mig lifetime, non-economic reason",
  "absent_hh_share"                = "HH with absent member share",
  "ind_agri_forestry_fish"         = "Industry: agriculture/forestry/fish",
  "occ_share_managers"             = "Occupation: managers",
  "occ_share_professionals"        = "Occupation: professionals",
  "occ_share_technicians"          = "Occupation: technicians",
  "occ_share_office_assistants"    = "Occupation: office assistants",
  "occ_share_service_sales"        = "Occupation: service & sales",
  "occ_share_agriculture"          = "Occupation: skilled agriculture",
  "occ_share_craft_trades"         = "Occupation: craft & trades",
  "occ_share_machine_operators"    = "Occupation: machine operators",
  "occ_share_elementary"           = "Occupation: elementary",
  "occ_share_armed_forces"         = "Occupation: armed forces",
  "emp_share_employee"             = "Employment: wage employee",
  "emp_share_employer"             = "Employment: employer",
  "emp_share_self_employed"        = "Employment: self-employed",
  "emp_share_unpaid_family_worker" = "Employment: unpaid family worker",
  "amen_assets_landline"           = "Asset: landline phone",
  "amen_assets_mobile"             = "Asset: mobile phone",
  "amen_assets_car"                = "Asset: car",
  "amen_assets_internet"           = "Asset: internet",
  "amen_assets_computer"           = "Asset: computer",
  "amen_assets_tv"                 = "Asset: television",
  "amen_assets_radio"              = "Asset: radio",
  "amen_assets_fridge"             = "Asset: refrigerator",
  "amen_assets_motorcycle"         = "Asset: motorcycle",
  "amen_assets_cycle"              = "Asset: bicycle",
  "amen_asset_count_mean"          = "Mean asset count",
  "amen_cooking_modern"            = "Cooking fuel: modern (LPG/electric/biogas)",
  "amen_cooking_lpg"               = "Cooking fuel: LPG",
  "amen_cooking_biogas"            = "Cooking fuel: biogas",
  "amen_cooking_electric"          = "Cooking fuel: electric",
  "amen_cooking_kerosene"          = "Cooking fuel: kerosene",
  "amen_cooking_wood"              = "Cooking fuel: wood",
  "amen_cooking_traditional"       = "Cooking fuel: traditional (wood/kerosene)",
  "amen_lighting_electricity"      = "Lighting: electricity",
  "amen_lighting_kerosene"         = "Lighting: kerosene",
  "amen_lighting_biogas"           = "Lighting: biogas",
  "amen_lighting_others"           = "Lighting: other",
  "amen_water_piped"               = "Water: piped",
  "amen_water_traditional"         = "Water: traditional source",
  "amen_toilet_modern"             = "Toilet: modern",
  "amen_toilet_ordinary"           = "Toilet: ordinary",
  "amen_toilet_any"                = "Toilet: any",
  "amen_toilet_none"               = "Toilet: none",
  "n_firms"                        = "NEC: # firms",
  "emp_total"                      = "NEC: total employment",
  "mean_emp_per_firm"              = "NEC: mean employment / firm",
  "share_firms_size_micro_1"       = "NEC: share micro (1 worker)",
  "share_firms_size_small_2_9"     = "NEC: share small (2-9)",
  "share_firms_size_medium_10_50"  = "NEC: share medium (10-50)",
  "share_firms_size_large_51p"     = "NEC: share large (51+)",
  "formality_index"                = "NEC: formality index",
  "share_registered"               = "NEC: share registered",
  "share_keeps_accounts"           = "NEC: share keeps accounts",
  "share_tax_registered"           = "NEC: share tax-registered",
  "share_emp_female"               = "NEC: share female employees",
  "share_emp_foreign"              = "NEC: share foreign employees",
  "share_any_foreign_cap"          = "NEC: share with foreign capital",
  "edu_spend_total_12m"            = "HH: education spend (12m, Rs)",
  "hlt_spend_total"                = "HH: health spend (Rs)",
  "input_intensity_per_sqm"        = "HH: ag input intensity / sqm",
  "nonfood_exp_12m"                = "HH: non-food expenditure (12m, Rs)",
  "profit_12m"                     = "HH: enterprise profit (12m, Rs)",
  "n_workers_total"                = "HH: # workers in enterprise"
)
make_label <- function(o) {
  if (!is.null(LABEL_OVERRIDES[[o]])) return(LABEL_OVERRIDES[[o]])
  # auto-prettify
  o |>
    gsub("_", " ", x = _) |>
    sub("^(\\w)", "\\U\\1", x = _, perl = TRUE)
}

GROUP_LABEL <- c(
  migration_perm     = "Migration — permanent (lifetime)",
  migration_temp     = "Migration — temporary (5-year)",
  industry           = "Industry employment shares",
  occupation         = "Occupation shares",
  work_status        = "Work status & employment type",
  lfp                = "LFP — female & male",
  education          = "Education",
  marriage           = "Marriage",
  assets             = "Household assets",
  amenities_lighting = "Amenities — lighting",
  amenities_cooking  = "Amenities — cooking",
  amenities_water_toilet = "Amenities — water & toilet",
  housing            = "Housing",
  census_other       = "Other (census)",
  hh_migration       = "HH migration & remittance",
  hh_ag              = "HH agriculture",
  hh_consumption     = "HH consumption (food / non-food)",
  hh_enterprise      = "HH enterprise",
  hh_other           = "HH other",
  nec_scale          = "Firm scale & size",
  nec_workforce      = "Firm workforce & capital",
  nec_formality      = "Firm formality & credit",
  nec_other          = "Firm other",
  nec_cohort_aggregate = "Firm entry cohort — aggregates",
  nec_cohort_size    = "Firm entry cohort — by size",
  nec_cohort_sector  = "Firm entry cohort — by sector",
  other              = "Other"
)

# ---- compute sd_y per (dataset, outcome) ---------------------------------
sd_y <- function(panel, outs) {
  out <- lapply(outs, function(o) {
    if (!o %in% names(panel)) return(NULL)
    v <- panel[[o]]; v <- v[!is.na(v)]
    if (!length(v)) return(NULL)
    tibble(outcome = o, sd_y = sd(v), n_unit = NA)
  })
  bind_rows(out)
}
sd_dict <- bind_rows(
  sd_y(cdf, CENSUS_OUTCOMES) %>% mutate(dataset = "census"),
  sd_y(hh,  HH_OUTCOMES)     %>% mutate(dataset = "hh"),
  if (!is.null(ncs)) sd_y(ncs, NEC_CS_OUTCOMES) %>% mutate(dataset = "nec_cs") else NULL,
  if (exists("npd") && !is.null(npd)) sd_y(npd, NEC_PANEL_OUTCOMES_FULL) %>% mutate(dataset = "nec_panel") else NULL
)

# ---- n_unit per spec (distinct dnames/hhids that survive the regression) -
n_unit_by_outcome <- function(panel, outs, unit_col) {
  sapply(outs, function(o) {
    if (!o %in% names(panel)) return(NA_integer_)
    sub <- panel[, c(unit_col, o)]
    sub <- sub[!is.na(sub[[o]]), ]
    length(unique(sub[[unit_col]]))
  })
}
n_unit_dict <- bind_rows(
  tibble(dataset = "census",    outcome = CENSUS_OUTCOMES,         n_unit = n_unit_by_outcome(cdf, CENSUS_OUTCOMES, "dname")),
  tibble(dataset = "hh",        outcome = HH_OUTCOMES,             n_unit = n_unit_by_outcome(hh,  HH_OUTCOMES,     "hhid")),
  if (!is.null(ncs)) tibble(dataset = "nec_cs", outcome = NEC_CS_OUTCOMES, n_unit = n_unit_by_outcome(ncs, NEC_CS_OUTCOMES, "dname")) else NULL,
  if (exists("npd") && !is.null(npd))
    tibble(dataset = "nec_panel", outcome = NEC_PANEL_OUTCOMES_FULL, n_unit = n_unit_by_outcome(npd, NEC_PANEL_OUTCOMES_FULL, "dname")) else NULL
)

# ---- Load the grids ------------------------------------------------------
grid <- read_csv("district-analysis/output/tab/robustness_all_panels.csv",
                 show_col_types = FALSE) %>%
  mutate(variant = "baseline") %>%
  left_join(sd_dict %>% select(dataset, outcome, sd_y), by = c("dataset","outcome")) %>%
  left_join(n_unit_dict, by = c("dataset","outcome"))

drop_path <- "district-analysis/output/tab/robustness_drop_districts.csv"
if (file.exists(drop_path)) {
  drop_df <- read_csv(drop_path, show_col_types = FALSE) %>%
    # the new drop CSV already has model column from the M2/M3/M4 sweep
    mutate(scaling = "log", lag = 2L) %>%
    select(dataset, outcome, scaling, lag, model, beta, se, p, sig, mean_y, n, variant) %>%
    left_join(sd_dict %>% select(dataset, outcome, sd_y), by = c("dataset","outcome")) %>%
    left_join(n_unit_dict, by = c("dataset","outcome"))
  # Keep only non-baseline variants from drop_df (baseline already in grid)
  drop_df <- drop_df %>% filter(variant != "baseline")
  combined <- bind_rows(grid, drop_df)
} else {
  combined <- grid
}

# Add group + label
combined <- combined %>%
  mutate(group = mapply(GROUP_OF, outcome, dataset),
         label = sapply(outcome, make_label)) %>%
  select(dataset, outcome, label, group,
         scaling, lag, model, variant,
         beta, se, p, sig,
         mean_y, sd_y, n, n_unit) %>%
  arrange(dataset, group, outcome, scaling, lag, variant, model)

# ---- emit unified CSV (single source of truth for portal) ----------------
dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(combined, "district-analysis/output/tab/district_robustness_grid.csv")
cat(sprintf("Wrote district-analysis/output/tab/district_robustness_grid.csv (%d rows)\n",
            nrow(combined)))

# ---- emit JSON -----------------------------------------------------------
# Structure (matches existing portal JSON style closely):
# {
#   datasets_meta: { census: { label }, ... },
#   groups_meta: { migration_perm: { label, outcomes: [...] }, ... },
#   datasets: {
#     census: {
#       outcomes: {
#         <key>: { label, group, mean_y, sd_y, n_unit, cells: { <spec_key>: {beta, se, p, sig, n} } }
#       }
#     }
#   }
# }
spec_key <- function(scaling, lag, model, variant) {
  sprintf("%s|%d|%s|%s", scaling, lag, model, variant)
}

ds_list <- list()
for (ds in unique(combined$dataset)) {
  sub_ds <- combined %>% filter(dataset == ds)
  outs <- unique(sub_ds$outcome)
  out_list <- list()
  for (oc in outs) {
    sub <- sub_ds %>% filter(outcome == oc)
    if (!nrow(sub)) next
    # one mean_y / sd_y / n_unit per outcome (use first non-NA)
    mean_y_val <- first(na.omit(sub$mean_y))
    sd_y_val   <- first(na.omit(sub$sd_y))
    n_unit_val <- first(na.omit(sub$n_unit))
    cells <- list()
    for (i in seq_len(nrow(sub))) {
      k <- spec_key(sub$scaling[i], sub$lag[i], sub$model[i], sub$variant[i])
      cells[[k]] <- list(
        beta = if (is.na(sub$beta[i])) NULL else unbox(sub$beta[i]),
        se   = if (is.na(sub$se[i]))   NULL else unbox(sub$se[i]),
        p    = if (is.na(sub$p[i]))    NULL else unbox(sub$p[i]),
        sig  = unbox(sub$sig[i] %||% ""),
        n    = if (is.na(sub$n[i]))    NULL else unbox(as.integer(sub$n[i]))
      )
    }
    out_list[[oc]] <- list(
      label  = unbox(sub$label[1]),
      group  = unbox(sub$group[1]),
      mean_y = if (is.na(mean_y_val)) NULL else unbox(mean_y_val),
      sd_y   = if (is.na(sd_y_val))   NULL else unbox(sd_y_val),
      n_unit = if (is.na(n_unit_val)) NULL else unbox(as.integer(n_unit_val)),
      cells  = cells
    )
  }
  ds_list[[ds]] <- list(outcomes = out_list)
}

# Groups meta — only emit groups that actually have outcomes in the data
present_groups <- unique(combined$group)
groups_meta <- list()
for (g in present_groups) {
  outcomes_in_g <- combined %>% filter(group == g) %>% pull(outcome) %>% unique() %>% sort()
  groups_meta[[g]] <- list(
    label    = unbox(GROUP_LABEL[g] %||% g),
    outcomes = outcomes_in_g
  )
}

datasets_meta <- list(
  census    = list(label = unbox("Census district panel (2011, 2021)")),
  hh        = list(label = unbox("HRVS HH panel (2016-18, district x year residualised)")),
  nec_cs    = list(label = unbox("NEC 2018 district cross-section")),
  nec_panel = list(label = unbox("NEC entry-cohort panel (2011-2018)"))
)

out <- list(
  datasets_meta = datasets_meta,
  groups_meta   = groups_meta,
  datasets      = ds_list
)

dir.create("docs", showWarnings = FALSE)
write_json(out, "docs/district_robustness.json", pretty = FALSE,
           auto_unbox = FALSE, na = "null")
cat(sprintf("Wrote docs/district_robustness.json (%d datasets, %d outcomes total)\n",
            length(ds_list),
            sum(sapply(ds_list, function(d) length(d$outcomes)))))
cat(sprintf("Elapsed: %.1f s\n", as.numeric(Sys.time() - t0, units = "secs")))
