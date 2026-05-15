###############################################################################
# baseline_nec_cs.R
# ─────────────────────────────────────────────────────────────────────────────
# NEC 2018 firm census, cross-section. Reduced-form SSIV.
#
# Spec:
#   y_m = α + β·SSIV_z(2018-LAG) + λ·log(MI₀) + δ_district + ε_m
#   ↪ standardised SSIV (mean 0, sd 1) across the 714 munis
#   ↪ FE: district (DIST)   ;   cluster ~ DIST
#
# Run from project root:
#   source("script/baseline_nec_cs.R")
###############################################################################
source(file.path("script", "_shared.R"))

# ── User options ─────────────────────────────────────────────────────────────
LAG  <- 0       # 0 = Z at 2018, 1 = Z at 2017, 2 = Z at 2016
CTRL <- "mi"    # "none", "mi", "khanna_full"
                # mi          = + log(MI₀)  (basic Khanna baseline)
                # khanna_full = + log(MI₀) + region shares + dest GDP + trade SSIV
                # khanna_full silently falls back to "mi" if optional inputs missing.
# ─────────────────────────────────────────────────────────────────────────────

KH <- load_khanna_inputs()

# ---- Load --------------------------------------------------------------------
mun  <- read_csv(data_path("nec2018", "municipality_analysis.csv"),
                 show_col_types = FALSE)
inst <- read_csv(data_path("instrument", "instrument_mun.csv"),
                 show_col_types = FALSE)
ssiv_cols <- grep("^(ssiv_|shareshock_|absexp_)", names(inst), value = TRUE)

# Cross-section: instrument value at year (2018 - LAG)
target_year <- 2018 - LAG
inst_yr <- inst %>% filter(year == target_year) %>%
  select(lgcode, geog_intensity_2001, all_of(ssiv_cols))

panel <- mun %>%
  inner_join(inst_yr, by = "lgcode") %>%
  mutate(log_mi    = asinh(geog_intensity_2001),
         fxshock_z = zscore(ssiv_index_2001),
         DIST      = as.character(DIST))

# Optional Khanna inputs (cross-section: regions as covariates, trade SSIV at target_year)
have_khanna <- !is.null(KH$region_df)
have_full   <- have_khanna && !is.null(KH$trade_df)
if (have_khanna) {
  panel <- panel %>% left_join(KH$region_df, by = "lgcode")
  for (c in KH$region_cols)
    panel[[c]] <- ifelse(is.na(panel[[c]]), mean(panel[[c]], na.rm = TRUE), panel[[c]])
  if (!is.null(KH$dest_gdp_df)) {
    panel <- panel %>% left_join(KH$dest_gdp_df, by = "lgcode")
    panel$dest_gdp_pc_2001 <- ifelse(is.na(panel$dest_gdp_pc_2001),
                                     mean(panel$dest_gdp_pc_2001, na.rm = TRUE),
                                     panel$dest_gdp_pc_2001)
  }
}
if (have_full) {
  trade_yr <- KH$trade_df %>% filter(year == target_year)
  panel <- panel %>% left_join(trade_yr, by = "lgcode")
  for (c in KH$trade_cols) panel[[c]] <- replace_na(panel[[c]], 0)
}

build_ctrl <- function(tag) {
  if (tag == "none") return(character(0))
  if (tag == "mi")   return("log_mi")
  if (tag == "khanna_full") {
    if (!have_full) {
      message("CTRL='khanna_full' but optional inputs (region shares / trade SSIV) are missing; falling back to 'mi'.")
      return("log_mi")
    }
    base <- c("log_mi", KH$region_cols)
    if (!is.null(KH$dest_gdp_df)) base <- c(base, "dest_gdp_pc_2001")
    return(c(base, KH$trade_cols))
  }
  warning("Unknown CTRL='", tag, "'; using 'mi'.")
  "log_mi"
}
ctrl_cols <- build_ctrl(CTRL)

cat("NEC cross-section:", nrow(panel), "munis ·",
    n_distinct(panel$DIST), "districts · Z at year", target_year,
    "(lag=", LAG, ", ctrl=", CTRL, ")\n")
cat("  Standardised SSIV: mean=", round(mean(panel$ssiv_index_2001), 5),
    " sd=", round(sd(panel$ssiv_index_2001), 5), "\n", sep = "")
cat("  Controls: ", if (length(ctrl_cols)) paste(ctrl_cols, collapse=", ") else "(none)", "\n", sep="")

# ---- Outcome groups ----------------------------------------------------------
GROUPS <- list(
  list(name = "FIRM PRESENCE & SCALE", items = list(
    c("n_firms",           "# firms",              "TRUE"),
    c("emp_total",         "Total employment",     "TRUE"),
    c("mean_emp_per_firm", "Mean emp per firm",    "FALSE"),
    c("p90_emp_per_firm",  "90th pct emp per firm","FALSE")
  )),
  list(name = "FIRM SIZE COMPOSITION", items = list(
    c("share_firms_size_micro_1",     "Share micro firms (1)",     "FALSE"),
    c("share_firms_size_small_2_9",   "Share small firms (2-9)",   "FALSE"),
    c("share_firms_size_medium_10_50","Share medium firms (10-50)","FALSE"),
    c("share_firms_size_large_51p",   "Share large firms (51+)",   "FALSE")
  )),
  list(name = "FORMALITY", items = list(
    c("share_registered",      "Share registered",        "FALSE"),
    c("share_tax_registered",  "Share tax-registered",    "FALSE"),
    c("share_keeps_accounts",  "Share keeps accounts",    "FALSE"),
    c("share_incorporated",    "Share incorporated",      "FALSE"),
    c("formality_index",       "Formality index",         "FALSE")
  )),
  list(name = "SECTOR COMPOSITION", items = list(
    c("share_firms_sec_sec_manuf",       "Share manufacturing",     "FALSE"),
    c("share_firms_sec_sec_construct",   "Share construction",      "FALSE"),
    c("share_firms_sec_sec_wholesale",   "Share wholesale & retail","FALSE"),
    c("share_firms_sec_sec_hospitality", "Share hospitality",       "FALSE"),
    c("share_firms_sec_sec_transport",   "Share transport",         "FALSE"),
    c("share_firms_sec_sec_services",    "Share other services",    "FALSE"),
    c("share_firms_sec_sec_health",      "Share health",            "FALSE"),
    c("share_firms_sec_sec_education",   "Share education",         "FALSE")
  )),
  list(name = "TRADABILITY", items = list(
    c("share_trd_tradable_goods",        "Share tradable goods",       "FALSE"),
    c("share_trd_tradable_services",     "Share tradable services",    "FALSE"),
    c("share_trd_non_tradable_services", "Share non-tradable services","FALSE")
  )),
  list(name = "MODERNITY", items = list(
    c("share_modern_modern_services",      "Modern services",        "FALSE"),
    c("share_modern_modern_manuf",         "Modern manufacturing",   "FALSE"),
    c("share_modern_traditional_commerce", "Traditional commerce",   "FALSE"),
    c("share_modern_traditional_services", "Traditional services",   "FALSE")
  )),
  list(name = "PRODUCTIVITY & CAPITAL", items = list(
    c("rev_mean",                 "Mean revenue",                "TRUE"),
    c("labor_prod_median",        "Median labour productivity",  "TRUE"),
    c("value_added_pw_median",    "Median VA/worker",            "TRUE"),
    c("capital_intensity_median", "Median capital intensity",    "TRUE"),
    c("profit_margin_median",     "Median profit margin",        "FALSE")
  )),
  list(name = "CREDIT & FINANCE", items = list(
    c("share_borrowed_any",  "Share borrowed any",        "FALSE"),
    c("share_formal_credit", "Share with formal credit",  "FALSE"),
    c("interest_p50",        "Interest rate p50",         "FALSE")
  )),
  list(name = "GENDER", items = list(
    c("share_female_manager", "Share female manager",      "FALSE"),
    c("share_female_owner",   "Share female owner",        "FALSE"),
    c("share_female_led",     "Share female-led",          "FALSE"),
    c("share_female_workers", "Share female workers",      "FALSE"),
    c("share_emp_female",     "Share employment female",   "FALSE")
  )),
  list(name = "FIRM AGE", items = list(
    c("share_firms_young_5y",   "Share firms < 5 yrs old",  "FALSE"),
    c("share_firms_mature_10y", "Share firms > 10 yrs old", "FALSE"),
    c("median_firm_age",        "Median firm age",          "FALSE")
  ))
)

rows <- build_table(
  groups   = GROUPS,
  panel_df = panel,
  fit_fn   = fit_cs,
  shock    = "fxshock_z",
  controls = ctrl_cols,
  fe       = "DIST",
  cluster_var = "DIST"
)

print_table(
  rows, title = sprintf("═══ NEC CROSS-SECTION (firms 2018, Z at %d, ctrl=%s) — district FE ═══",
                       target_year, CTRL),
  n_obs = nrow(panel), n_unit = n_distinct(panel$DIST),
  unit_label = "muni", # "obs"
  spec_str = "Spec: y_m = α + β·SSIV_z + λ·log(MI₀) + δ_district + ε   ;  cluster ~ district"
)
print_notes("Cross-section at survey year 2018; MI₀ from 2001. SSIV standardised across municipalities.")
