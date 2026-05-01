###############################################################################
# baseline_census.R
# ─────────────────────────────────────────────────────────────────────────────
# Census municipality panel (2011 + 2021), reduced-form SSIV.
#
# Spec:
#   y_mt = α_m + γ_t + β·SSIV_z(t-LAG) + λ·log(MI₀) × 1{t = 2021} + ε_mt
#   ↪ ref year 2011 (within sample); MI₀ from 2001
#   ↪ standardised SSIV (mean 0, sd 1) over the panel pool
#   ↪ FE: lgcode + year   ;   cluster ~ lgcode
#
# Run from project root:
#   source("script/baseline_census.R")
###############################################################################
source(file.path("script", "_shared.R"))

# ── User options ─────────────────────────────────────────────────────────────
LAG  <- 0       # 0 = contemporaneous Z_t, 1 = Z_{t-1}, 2 = Z_{t-2}
CTRL <- "mi"    # "none", "mi", "khanna_full"
                # mi          = + MI × 1{year=2021}  (basic Khanna baseline)
                # khanna_full = + MI + ShareShock + region × t + dest GDP × t + trade SSIV
                # khanna_full silently falls back to "mi" if the optional input
                # CSVs (region shares / trade SSIV) aren't present locally.
# ─────────────────────────────────────────────────────────────────────────────

KH <- load_khanna_inputs()

# ---- Load data ---------------------------------------------------------------
inst <- read_csv(data_path("instrument", "instrument_mun.csv"), show_col_types = FALSE)
cen  <- read_csv(data_path("census",     "census_outcomes_municipality.csv"),
                 show_col_types = FALSE)

ssiv_cols <- grep("^(ssiv_|shareshock_|absexp_)", names(inst), value = TRUE)
# Lag the instrument by shifting its `year` forward by LAG so that a row tagged
# year=t carries Z_{t-LAG}.
inst_keep <- inst %>%
  mutate(year = year + LAG) %>%
  select(lgcode, year, geog_intensity_2001, all_of(ssiv_cols))

# ---- Build panel: 2011 & 2021 ------------------------------------------------
panel <- cen %>%
  inner_join(inst_keep, by = c("lgcode", "year")) %>%
  filter(year %in% c(2011, 2021)) %>%
  mutate(across(all_of(ssiv_cols), ~ replace_na(., 0)),
         geog_intensity_2001 = replace_na(geog_intensity_2001, 0),
         log_mi              = asinh(geog_intensity_2001),
         mi_x_2021           = log_mi * (year == 2021),
         shareshock_x_2021   = shareshock_index_2001 * (year == 2021),
         fxshock             = zscore(ssiv_index_2001))

# Optional Khanna inputs (region shares × 1{2021}, trade SSIV × 1{2021}).
have_khanna <- !is.null(KH$region_df)
have_full   <- have_khanna && !is.null(KH$trade_df)
if (have_khanna) {
  panel <- panel %>% left_join(KH$region_df, by = "lgcode")
  for (c in KH$region_cols) {
    panel[[c]] <- ifelse(is.na(panel[[c]]), mean(panel[[c]], na.rm = TRUE), panel[[c]])
    panel[[paste0(c, "_x_2021")]] <- panel[[c]] * (panel$year == 2021)
  }
  if (!is.null(KH$dest_gdp_df)) {
    panel <- panel %>% left_join(KH$dest_gdp_df, by = "lgcode")
    panel$dest_gdp_pc_2001 <- ifelse(is.na(panel$dest_gdp_pc_2001),
                                     mean(panel$dest_gdp_pc_2001, na.rm = TRUE),
                                     panel$dest_gdp_pc_2001)
    panel$dest_gdp_pc_2001_x_2021 <- panel$dest_gdp_pc_2001 * (panel$year == 2021)
  }
}
if (have_full) {
  panel <- panel %>% left_join(KH$trade_df, by = c("lgcode","year"))
  for (c in KH$trade_cols) panel[[c]] <- replace_na(panel[[c]], 0)
  # Trade SSIV varies by year naturally — include as plain panel covariate
  # rather than × 1{year=2021}; the year-indicator interaction is collinear
  # with the year FE in T=2 and creates rank deficiency.
}

build_ctrl <- function(tag) {
  if (tag == "none") return(character(0))
  if (tag == "mi")   return("mi_x_2021")
  if (tag == "khanna_full") {
    if (!have_full) {
      message("CTRL='khanna_full' but optional inputs (region shares / trade SSIV) are missing; falling back to 'mi'.")
      return("mi_x_2021")
    }
    base <- c("mi_x_2021", "shareshock_x_2021",
              paste0(KH$region_cols, "_x_2021"))
    if (!is.null(KH$dest_gdp_df)) base <- c(base, "dest_gdp_pc_2001_x_2021")
    return(c(base, KH$trade_cols))   # trade SSIV: panel-varying covariate
  }
  warning("Unknown CTRL='", tag, "'; using 'mi'.")
  "mi_x_2021"
}
ctrl_cols <- build_ctrl(CTRL)

cat("Census P2 panel:", nrow(panel), "obs ·",
    n_distinct(panel$lgcode), "munis · years 2011-2021 · lag=", LAG,
    " · ctrl=", CTRL, "\n", sep = "")
cat("  Standardised SSIV: mean=", round(mean(panel$ssiv_index_2001), 5),
    " sd=", round(sd(panel$ssiv_index_2001), 5), "\n", sep = "")
cat("  Controls: ", if (length(ctrl_cols)) paste(ctrl_cols, collapse=", ") else "(none)", "\n", sep="")

# ---- Outcome groups (mirrors explorer's most-watched groups) -----------------
GROUPS <- list(
  list(name = "AMENITIES & ASSETS", items = list(
    c("amen_water_piped",          "Piped water",                  "FALSE"),
    c("amen_cooking_lpg",          "LPG (cooking)",                "FALSE"),
    c("amen_cooking_modern",       "Modern fuel",                  "FALSE"),
    c("amen_lighting_electricity", "Electric lighting",            "FALSE"),
    c("amen_toilet_modern",        "Modern toilet",                "FALSE"),
    c("amen_assets_motorcycle",    "Motorcycle",                   "FALSE"),
    c("amen_assets_car",           "Car",                          "FALSE"),
    c("amen_assets_fridge",        "Fridge",                       "FALSE"),
    c("amen_assets_mobile",        "Mobile",                       "FALSE"),
    c("amen_assets_computer",      "Computer",                     "FALSE"),
    c("amen_assets_internet",      "Internet",                     "FALSE")
  )),
  list(name = "LABOUR (15-60)", items = list(
    c("work_share_agriculture",    "Agri work",                    "FALSE"),
    c("work_share_nonagriculture", "Non-agri work",                "FALSE"),
    c("work_share_wage_nonagri",   "Wage non-agri",                "FALSE"),
    c("work_share_own_nonagri",    "Own-account non-agri",         "FALSE"),
    c("work_lfp",                  "LFP",                          "FALSE")
  )),
  list(name = "EMPLOYMENT TYPE", items = list(
    c("emp_share_employer",         "Employer",                    "FALSE"),
    c("emp_share_employee",         "Employee",                    "FALSE"),
    c("emp_share_self_employed",    "Self-employed",               "FALSE"),
    c("emp_share_unpaid_family_worker", "Unpaid family worker",    "FALSE")
  )),
  list(name = "INDUSTRY", items = list(
    c("ind_agri_forestry_fish",      "Agriculture, forestry & fishing", "FALSE"),
    c("ind_manufacturing",           "Manufacturing",                  "FALSE"),
    c("ind_construction",            "Construction",                   "FALSE"),
    c("ind_wholesale_retail",        "Wholesale & retail",             "FALSE"),
    c("ind_transport_accommodation", "Transport & accommodation",      "FALSE"),
    c("ind_finance_real_estate_prof","Finance / RE / professional",    "FALSE"),
    c("ind_education",               "Education",                      "FALSE"),
    c("ind_health",                  "Health",                         "FALSE")
  )),
  list(name = "OCCUPATION", items = list(
    c("occ_share_managers",        "Managers",                     "FALSE"),
    c("occ_share_professionals",   "Professionals",                "FALSE"),
    c("occ_share_technicians",     "Technicians",                  "FALSE"),
    c("occ_share_service_sales",   "Service & sales",              "FALSE"),
    c("occ_share_agriculture",     "Agriculture workers",          "FALSE"),
    c("occ_share_elementary",      "Elementary",                   "FALSE")
  )),
  list(name = "EDUCATION", items = list(
    c("edu_literate",                  "Literate",                 "FALSE"),
    c("edu_school_attend_6_16",        "School attend (6-16)",     "FALSE"),
    c("edu_attain_secondary_plus",     "Secondary+",               "FALSE"),
    c("edu_attain_higher_secondary_plus","Higher secondary+",      "FALSE"),
    c("edu_attain_tertiary",           "Tertiary",                 "FALSE"),
    c("edu_years_mean",                "Mean years schooling",     "FALSE")
  )),
  list(name = "MIGRATION & FLFP", items = list(
    c("mig_in_share",          "In-migrant share",                 "FALSE"),
    c("mig_in_international",  "International in-migrants",        "FALSE"),
    c("flfp_all",              "Female LFP",                       "FALSE"),
    c("mlfp_all",              "Male LFP",                         "FALSE"),
    c("head_female_share",     "Female-headed HH",                 "FALSE")
  ))
)

rows <- build_table(
  groups   = GROUPS,
  panel_df = panel,
  fit_fn   = fit_one,
  shock    = "fxshock",
  controls = ctrl_cols,
  fe          = c("lgcode", "year"),
  cluster_var = "lgcode"
)

print_table(
  rows, title = sprintf("═══ CENSUS BASELINE — P2 panel (2011, 2021), ref year 2011, lag=%d, ctrl=%s ═══",
                       LAG, CTRL),
  n_obs = nrow(panel), n_unit = n_distinct(panel$lgcode),
  unit_label = "muni-year",
  spec_str = paste0(
    "Spec: y_mt = α_m + γ_t + β·SSIV_z + λ·log(MI₀)·1{t=2021} + ε   ;  cluster ~ lgcode"
  )
)
print_notes("Census waves matched to instrument by lgcode × year. SSIV standardised on the panel pool.")
