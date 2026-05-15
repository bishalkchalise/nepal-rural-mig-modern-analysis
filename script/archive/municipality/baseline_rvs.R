###############################################################################
# baseline_rvs.R
# ─────────────────────────────────────────────────────────────────────────────
# HRVS household × year panel (2016–2018), reduced-form SSIV.
#
# Spec:
#   y_imt = α_i + γ_t + β·SSIV_z + λ·log(MI₀)·(year - 2001) + ε_imt
#   ↪ baseline anchor 2001 (linear trend, since 2001 not in HRVS panel)
#   ↪ standardised SSIV (mean 0, sd 1) over the panel pool
#   ↪ FE: hhid + year   ;   cluster ~ vmun_code
#
# Run from project root:
#   source("script/baseline_rvs.R")
###############################################################################
source(file.path("script", "_shared.R"))

# ── User options ─────────────────────────────────────────────────────────────
LAG  <- 0       # 0 = contemporaneous Z_t, 1 = Z_{t-1}, 2 = Z_{t-2}
CTRL <- "mi"    # "none", "mi", "khanna_full"
                # mi          = + MI × year FE   (basic Khanna baseline)
                # khanna_full = + MI + ShareShock + region × t + dest GDP × t + trade SSIV
                # khanna_full silently falls back to "mi" if optional input CSVs
                # (region shares, trade SSIV) aren't present locally.
# ─────────────────────────────────────────────────────────────────────────────

# Optional Khanna inputs (regions, trade SSIV). NULL if files are missing.
KH <- load_khanna_inputs()

# ---- Load HRVS pieces --------------------------------------------------------
rvs <- function(name) read_csv(data_path("rvs_outcomes", paste0(name, ".csv")),
                                show_col_types = FALSE)

ag   <- rvs("agriculture_hh_year")
mig  <- rvs("migration_hh_year")
lab  <- rvs("labour_hh_year")
ent  <- rvs("enterprise_hh_year")
cons <- rvs("consumption_hh_year")
hlt  <- rvs("health_hh_year")
edu  <- rvs("education_hh_year")
shk  <- rvs("shocks_coping_hh_year")

# Base keys (HH × year)
base <- ag %>% select(hhid, year, vmun_code) %>% distinct(hhid, year, .keep_all = TRUE)

merge_outs <- function(df, src, cols) {
  cols <- intersect(cols, names(src))
  src_red <- src %>% select(hhid, year, all_of(cols)) %>%
                     distinct(hhid, year, .keep_all = TRUE)
  df %>% left_join(src_red, by = c("hhid", "year"))
}

# ---- Outcome groups + which file they come from ------------------------------
GROUPS <- list(
  list(name = "AGRICULTURE · Land, tenure & livestock", src = ag, items = list(
    c("agri_hh",                  "HH operates a farm",                "FALSE"),
    c("owned_plots_n",            "# owned plots",                     "FALSE"),
    c("owned_area_sqm",           "Owned area (sqm)",                  "FALSE"),
    c("cultivated_area_sqm",      "Cultivated area (sqm)",             "FALSE"),
    c("rented_in_any",            "Rented / sharecropped land in",     "FALSE"),
    c("plot_wet_fallow_share",    "Wet-season fallow share",           "FALSE"),
    c("plot_dry_fallow_share",    "Dry-season fallow share",           "FALSE"),
    c("double_crop_share",        "Double-cropped plot share",         "FALSE"),
    c("livestock_has",            "Owns livestock",                    "FALSE")
  )),
  list(name = "AGRICULTURE · Wet season inputs", src = ag, items = list(
    c("wet_use_fertiliser",       "Uses fertiliser (wet)",             "FALSE"),
    c("wet_use_pesticide",        "Uses pesticide (wet)",              "FALSE"),
    c("wet_use_hired_labour",     "Hires labour (wet)",                "FALSE"),
    c("wet_use_equipment",        "Hires equipment (wet)",             "FALSE")
  )),
  list(name = "AGRICULTURE · Dry season inputs", src = ag, items = list(
    c("dry_use_fertiliser",       "Uses fertiliser (dry)",             "FALSE"),
    c("dry_use_pesticide",        "Uses pesticide (dry)",              "FALSE"),
    c("dry_use_hired_labour",     "Hires labour (dry)",                "FALSE"),
    c("dry_use_equipment",        "Hires equipment (dry)",             "FALSE")
  )),
  list(name = "AGRICULTURE · Equipment & inputs", src = ag, items = list(
    c("owns_tractor",             "Owns tractor / power tiller",       "FALSE"),
    c("owns_pump",                "Owns water pump / tubewell",        "FALSE"),
    c("owns_modern_equip",        "Owns any modern equipment",         "FALSE"),
    c("n_modern_equip_types",     "# modern equipment types",          "FALSE"),
    c("ag_equip_stock_value_rs",  "Equipment stock value",             "TRUE"),
    c("input_total_12m_rs",       "Total input spend",                 "TRUE"),
    c("input_intensity_per_sqm",  "Input spend per cultivated sqm",    "FALSE")
  )),
  list(name = "AGRICULTURE · Crops & sales", src = ag, items = list(
    c("n_crop_types",             "# distinct crops grown",            "FALSE"),
    c("grows_staple",             "Grows any staple crop",             "FALSE"),
    c("grows_cashcrop",           "Grows any cash crop",               "FALSE"),
    c("grows_horticulture",       "Grows fruits / vegetables",         "FALSE"),
    c("crop_sold_any",            "Sold any crop",                     "FALSE"),
    c("crop_sales_12m_rs",        "Crop sales (rs)",                   "TRUE"),
    c("crop_sale_share",          "Crop sale share",                   "FALSE")
  )),
  list(name = "MIGRATION · Migrants", src = mig, items = list(
    c("has_migrant",                  "HH has any migrant",            "FALSE"),
    c("has_migrant_internal",         "HH has internal migrant",       "FALSE"),
    c("mig_cost_financed_by_loan_any","Migration financed by loan",    "FALSE")
  )),
  list(name = "MIGRATION · Remittance flows & use", src = mig, items = list(
    c("remit_received",               "Remittance received",           "FALSE"),
    c("remit_amount_12m_rs",          "Remit amount",                  "TRUE"),
    c("remit_amount_intl_12m_rs",     "Remit amount intl",             "TRUE"),
    c("remit_via_formal_any",         "Remit via bank / IME",          "FALSE"),
    c("remit_via_hundi_any",          "Remit via hundi",               "FALSE"),
    c("remit_use_consumption_any",    "Remit → consumption",           "FALSE"),
    c("remit_use_education_any",      "Remit → education",             "FALSE"),
    c("remit_use_business_any",       "Remit → business / productive", "FALSE")
  )),
  list(name = "LABOUR", src = lab, items = list(
    c("n_jobs_total",                 "# jobs in HH",                     "FALSE"),
    c("share_wage_agri",              "Share of jobs: wage agri",         "FALSE"),
    c("share_wage_nonagri",           "Share of jobs: wage non-agri",     "FALSE"),
    c("share_self_agri",              "Share of jobs: self-emp agri",     "FALSE"),
    c("share_self_nonagri",           "Share of jobs: self-emp non-agri", "FALSE"),
    c("hh_has_wage_job",              "Any wage job in HH",               "FALSE"),
    c("n_wage_jobs_hh",               "# wage jobs in HH",                "FALSE"),
    c("wage_total_income_12m_rs",     "Wage income",                      "TRUE"),
    c("wage_sector_primary_any",      "Any primary-sector wage job",      "FALSE"),
    c("wage_sector_secondary_any",    "Any secondary-sector wage job",    "FALSE"),
    c("wage_sector_tertiary_any",     "Any tertiary-sector wage job",     "FALSE")
  )),
  list(name = "ENTERPRISE", src = ent, items = list(
    c("has_nonag_enterprise",          "Has non-farm enterprise",         "FALSE"),
    c("n_nonag_enterprises",           "# non-farm enterprises",          "FALSE"),
    c("enterprise_workers_total",      "Total enterprise workers",        "FALSE"),
    c("enterprise_revenue_12m",        "Revenue",                         "TRUE"),
    c("enterprise_expenses_12m",       "Expenses",                        "TRUE"),
    c("enterprise_profit_12m",         "Profit",                          "TRUE"),
    c("enterprise_capex_12m",          "Capex",                           "TRUE"),
    c("enterprise_profit_margin",      "Profit margin",                   "FALSE"),
    c("sector_agriculture_share",                "Share in agri / forestry / fish",  "FALSE"),
    c("sector_manufacturing_construction_share", "Share in manuf / construction",    "FALSE"),
    c("sector_services_share",                   "Share in services",                "FALSE")
  )),
  list(name = "CONSUMPTION · Food", src = cons, items = list(
    c("food_total_7day",            "Food total (7d)",                "TRUE"),
    c("food_purchased_7day",        "Food purchased (7d)",            "TRUE"),
    c("food_homeprod_7day",         "Food from own production (7d)",  "TRUE"),
    c("food_homeprod_share",        "Share of food from own production","FALSE"),
    c("food_staples_7day",          "Staples (7d)",                   "TRUE"),
    c("food_protein_7day",          "Protein foods (7d)",             "TRUE"),
    c("food_animal_7day",           "Animal-source foods (7d)",       "TRUE"),
    c("food_vegfruit_7day",         "Vegetables / fruit (7d)",        "TRUE"),
    c("food_groups_consumed",       "# food groups consumed",         "FALSE"),
    c("perceived_food_insecurity",  "Perceived food insecurity",      "FALSE"),
    c("food_insec_any",             "Any realized food insecurity",   "FALSE"),
    c("food_insec_index",           "Food insecurity breadth index",  "FALSE"),
    c("severe_food_insecurity",     "Severe food insecurity",         "FALSE")
  )),
  list(name = "CONSUMPTION · Non-food", src = cons, items = list(
    c("nonfood_exp_30day",          "Non-food exp (30d)",             "TRUE"),
    c("nonfood_exp_12m",            "Non-food exp (12m)",             "TRUE"),
    c("nonfood_basic_nonfood_12m",            "Basic non-food (12m)",            "TRUE"),
    c("nonfood_energy_fuel_lighting_12m",     "Energy / fuel / lighting (12m)",  "TRUE"),
    c("nonfood_clothing_personal_12m",        "Clothing / personal (12m)",       "TRUE"),
    c("nonfood_transport_communication_12m",  "Transport / communication (12m)", "TRUE"),
    c("nonfood_housing_household_12m",        "Housing / household goods (12m)", "TRUE"),
    c("nonfood_education_leisure_12m",        "Education / leisure (12m)",       "TRUE"),
    c("nonfood_social_ceremonial_financial_12m","Social / ceremony / finance (12m)", "TRUE"),
    c("nonfood_luxury_valuables_12m",         "Jewellery / luxury (12m)",        "TRUE")
  )),
  list(name = "CONSUMPTION · Durables", src = cons, items = list(
    c("durables_stock_value",       "Durables stock value",           "TRUE"),
    c("durables_use_value_12m",     "Imputed durables consumption (12m)", "TRUE")
  )),
  list(name = "EDUCATION", src = edu, items = list(
    c("any_enrolled",           "Any school enrollment",                 "FALSE"),
    c("n_private_school",       "# in private school",                   "FALSE"),
    c("n_scholarship",          "# scholarships",                        "FALSE"),
    c("scholarship_amt_12m",    "Scholarship amount (12m)",              "TRUE"),
    c("edu_spend_total_12m",    "Education spend total",                 "TRUE"),
    c("edu_spend_per_enrolled", "Education spend per enrolled child",    "TRUE")
  )),
  list(name = "HEALTH", src = hlt, items = list(
    c("any_insured",            "Any health card holder",                "FALSE"),
    c("n_chronic",              "# members with chronic illness",        "FALSE"),
    c("n_acute_illness",        "# members with acute illness (30d)",    "FALSE"),
    c("any_health_spending",    "Any health spending",                   "FALSE"),
    c("hlt_spend_total",        "Health spend total",                    "TRUE")
  )),
  list(name = "SHOCKS & COPING", src = shk, items = list(
    c("any_shock",       "HH reports any shock",   "FALSE"),
    c("coped_self",      "Cope: own resources",    "FALSE"),
    c("coped_external",  "Cope: external help",    "FALSE")
  ))
)

# Merge all needed columns onto base
panel <- base
for (g in GROUPS) {
  cols <- vapply(g$items, `[`, "", 1)
  if (is.list(g$src) && !is.data.frame(g$src)) {
    for (s in g$src) panel <- merge_outs(panel, s, cols)
  } else {
    panel <- merge_outs(panel, g$src, cols)
  }
}

# Shocks file is restricted to HHs that reported a shock; HHs not present are
# treated as zero so any_shock / coped_* are 0/1 over the full HH panel.
for (c in c("any_shock", "coped_self", "coped_external")) {
  if (c %in% names(panel)) panel[[c]][is.na(panel[[c]])] <- 0
}

# ---- Merge instrument by (vmun_code, year-LAG) -------------------------------
inst <- read_csv(data_path("instrument", "instrument_mun.csv"), show_col_types = FALSE)
ssiv_cols <- grep("^(ssiv_|shareshock_|absexp_)", names(inst), value = TRUE)
# Shift the instrument's `year` forward by LAG so a row tagged year=t carries
# Z_{m, t-LAG}.
inst_keep <- inst %>%
  mutate(year = year + LAG) %>%
  select(lgcode, year, geog_intensity_2001, all_of(ssiv_cols)) %>%
  rename(vmun_code = lgcode)

panel <- panel %>% inner_join(inst_keep, by = c("vmun_code", "year")) %>%
  mutate(across(all_of(ssiv_cols), ~ replace_na(., 0)),
         geog_intensity_2001 = replace_na(geog_intensity_2001, 0),
         log_mi  = asinh(geog_intensity_2001),
         fxshock = zscore(ssiv_index_2001))

# Khanna Eq. (4), footnote 12: MI and ShareShock interact with the FULL period
# fixed effects, not a linear trend. HRVS spans 2016–2018 (3 years; ref = 2016).
for (y in c(2017, 2018)) {
  ind <- as.integer(panel$year == y)
  panel[[paste0("mi_x_",         y)]] <- panel$log_mi * ind
  panel[[paste0("shareshock_x_", y)]] <- panel$shareshock_index_2001 * ind
}

# Khanna inputs (optional): merge by lgcode (region shares are time-invariant)
# and by lgcode × year (trade SSIV is muni-year). Region shares × t (linear
# trend on 3 years is fine; year-FE interaction would saturate degrees of
# freedom for ~7 regions). Same for dest GDP.
have_khanna <- !is.null(KH$region_df)
have_full   <- have_khanna && !is.null(KH$trade_df)
if (have_khanna) {
  panel <- panel %>%
    left_join(KH$region_df %>% rename(vmun_code = lgcode), by = "vmun_code")
  for (c in KH$region_cols) {
    panel[[c]] <- ifelse(is.na(panel[[c]]), mean(panel[[c]], na.rm = TRUE), panel[[c]])
    panel[[paste0(c, "_x_t")]] <- panel[[c]] * (panel$year - 2001)
  }
  if (!is.null(KH$dest_gdp_df)) {
    panel <- panel %>%
      left_join(KH$dest_gdp_df %>% rename(vmun_code = lgcode), by = "vmun_code")
    panel$dest_gdp_pc_2001 <- ifelse(is.na(panel$dest_gdp_pc_2001),
                                     mean(panel$dest_gdp_pc_2001, na.rm = TRUE),
                                     panel$dest_gdp_pc_2001)
    panel$dest_gdp_pc_2001_x_t <- panel$dest_gdp_pc_2001 * (panel$year - 2001)
  }
}
if (have_full) {
  panel <- panel %>%
    left_join(KH$trade_df %>% rename(vmun_code = lgcode),
              by = c("vmun_code", "year"))
  for (c in KH$trade_cols) panel[[c]] <- replace_na(panel[[c]], 0)
}

MI_COLS         <- c("mi_x_2017", "mi_x_2018")
SHARESHOCK_COLS <- c("shareshock_x_2017", "shareshock_x_2018")

# Resolve CTRL → list of column names. Three options now:
#   "none"        — HH FE + year FE only.
#   "mi"          — basic Khanna baseline: + MI × year FE.
#   "khanna_full" — full Khanna: + MI + ShareShock + region × t + dest GDP × t + trade SSIV.
build_ctrl <- function(tag) {
  if (tag == "none") return(character(0))
  if (tag == "mi")   return(MI_COLS)
  if (tag == "khanna_full") {
    if (!have_full) {
      message("CTRL='khanna_full' but optional inputs (region shares / trade SSIV) are missing; falling back to 'mi'.")
      return(MI_COLS)
    }
    cols <- c(MI_COLS, SHARESHOCK_COLS, paste0(KH$region_cols, "_x_t"))
    if (!is.null(KH$dest_gdp_df)) cols <- c(cols, "dest_gdp_pc_2001_x_t")
    return(c(cols, KH$trade_cols))
  }
  warning("Unknown CTRL='", tag, "'; using 'mi'.")
  MI_COLS
}
ctrl_cols <- build_ctrl(CTRL)

cat("HRVS panel:", format(nrow(panel), big.mark = ","), "obs ·",
    format(n_distinct(panel$hhid), big.mark = ","), "HHs ·",
    n_distinct(panel$vmun_code), "munis · 2016-2018 · lag=", LAG,
    " · ctrl=", CTRL, "\n", sep = "")
cat("  Standardised SSIV (panel pool): mean=", round(mean(panel$ssiv_index_2001), 5),
    " sd=", round(sd(panel$ssiv_index_2001), 5), "\n", sep = "")
cat("  Controls: ", if (length(ctrl_cols)) paste(ctrl_cols, collapse=", ") else "(none)", "\n", sep="")

rows <- build_table(
  groups   = GROUPS,
  panel_df = panel,
  fit_fn   = fit_one,
  shock    = "fxshock",
  controls = ctrl_cols,
  fe          = c("hhid", "year"),
  cluster_var = "vmun_code"
)

print_table(
  rows, title = sprintf("═══ HRVS BASELINE — household panel 2016-2018 (lag=%d, ctrl=%s) ═══",
                       LAG, CTRL),
  n_obs = nrow(panel), n_unit = n_distinct(panel$hhid),
  unit_label = "HH-year",
  spec_str = paste0(
    "Spec: y_imt = α_i + γ_t + β·SSIV_z + λ'·controls + ε   ;  cluster ~ vmun_code"
  )
)
print_notes("HH FE absorb time-invariant household traits; year FE absorb common shocks. SSIV standardised on the panel pool.")
