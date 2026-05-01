###############################################################################
# baseline_nec_panel.R
# ─────────────────────────────────────────────────────────────────────────────
# NEC firm panel (municipality × founding-year, 2001-2018).
#
# Each row is the cohort of firms born in year `t` in municipality `m` that
# survived to the 2018 census.  We pair each cohort with the FX shock in its
# founding year, controlling for differential MI-trends from 2001 onward.
#
# Spec:
#   y_mt = α_m + γ_t + β·SSIV_z + λ·log(MI₀)·(t - 2001) + ε_mt
#   ↪ baseline anchor 2001 (linear trend; 2001 IS in the founding-year sample)
#   ↪ standardised SSIV (mean 0, sd 1) over the panel pool
#   ↪ FE: lgcode + year   ;   cluster ~ lgcode
#
# Run from project root:
#   source("script/baseline_nec_panel.R")
###############################################################################
source(file.path("script", "_shared.R"))

# ── User options ─────────────────────────────────────────────────────────────
LAG  <- 0       # 0 = contemporaneous Z_t (founding year), 1 = Z_{t-1}, 2 = Z_{t-2}
CTRL <- "mi"    # "none", "mi", "khanna_full"
                # mi          = + MI × D_t  (basic Khanna baseline)
                # khanna_full = + MI + ShareShock × D_t + region × Post + dest GDP × Post + trade SSIV
                # khanna_full silently falls back to "mi" if optional inputs missing.
# ─────────────────────────────────────────────────────────────────────────────

KH <- load_khanna_inputs()

# ---- Load --------------------------------------------------------------------
ep   <- read_csv(data_path("nec2018",   "entry_cohort_panel.csv"),
                 show_col_types = FALSE)
inst <- read_csv(data_path("instrument","instrument_mun.csv"),
                 show_col_types = FALSE)
ssiv_cols <- grep("^(ssiv_|shareshock_|absexp_)", names(inst), value = TRUE)

# Use founding year as the time index, restrict to instrument coverage.
# Shift inst.year forward by LAG so a row tagged year=t carries Z_{m, t-LAG}.
ep <- ep %>% rename(year = founding_year_ad)
inst_keep <- inst %>%
  mutate(year = year + LAG) %>%
  select(lgcode, year, geog_intensity_2001, all_of(ssiv_cols))

panel <- ep %>%
  inner_join(inst_keep, by = c("lgcode", "year")) %>%
  filter(year >= 2001, year <= 2018) %>%
  mutate(across(all_of(ssiv_cols), ~ replace_na(., 0)),
         geog_intensity_2001 = replace_na(geog_intensity_2001, 0),
         log_mi          = asinh(geog_intensity_2001),
         fxshock         = zscore(ssiv_index_2001),
         post            = as.integer(year > 2001))

# Khanna Eq. (4) footnote 12: MI and ShareShock × full year FE vector D_t
# (ref = 2001, so non-ref years are 2002…2018).
NP_YRS_NONREF <- 2002:2018
for (y in NP_YRS_NONREF) {
  ind <- as.integer(panel$year == y)
  panel[[paste0("mi_x_",         y)]] <- panel$log_mi * ind
  panel[[paste0("shareshock_x_", y)]] <- panel$shareshock_index_2001 * ind
}

# Optional Khanna inputs: region shares × Post; dest GDP × Post; trade SSIV by year.
have_khanna <- !is.null(KH$region_df)
have_full   <- have_khanna && !is.null(KH$trade_df)
if (have_khanna) {
  panel <- panel %>% left_join(KH$region_df, by = "lgcode")
  for (c in KH$region_cols) {
    panel[[c]] <- ifelse(is.na(panel[[c]]), mean(panel[[c]], na.rm = TRUE), panel[[c]])
    panel[[paste0(c, "_x_post")]] <- panel[[c]] * panel$post
  }
  if (!is.null(KH$dest_gdp_df)) {
    panel <- panel %>% left_join(KH$dest_gdp_df, by = "lgcode")
    panel$dest_gdp_pc_2001 <- ifelse(is.na(panel$dest_gdp_pc_2001),
                                     mean(panel$dest_gdp_pc_2001, na.rm = TRUE),
                                     panel$dest_gdp_pc_2001)
    panel$dest_gdp_pc_2001_x_post <- panel$dest_gdp_pc_2001 * panel$post
  }
}
if (have_full) {
  panel <- panel %>% left_join(KH$trade_df, by = c("lgcode","year"))
  for (c in KH$trade_cols) panel[[c]] <- replace_na(panel[[c]], 0)
}

MI_COLS         <- paste0("mi_x_",         NP_YRS_NONREF)
SHARESHOCK_COLS <- paste0("shareshock_x_", NP_YRS_NONREF)

build_ctrl <- function(tag) {
  if (tag == "none") return(character(0))
  if (tag == "mi")   return(MI_COLS)
  if (tag == "khanna_full") {
    if (!have_full) {
      message("CTRL='khanna_full' but optional inputs (region shares / trade SSIV) are missing; falling back to 'mi'.")
      return(MI_COLS)
    }
    base <- c(MI_COLS, SHARESHOCK_COLS, paste0(KH$region_cols, "_x_post"))
    if (!is.null(KH$dest_gdp_df)) base <- c(base, "dest_gdp_pc_2001_x_post")
    return(c(base, KH$trade_cols))
  }
  warning("Unknown CTRL='", tag, "'; using 'mi'.")
  MI_COLS
}
ctrl_cols <- build_ctrl(CTRL)

cat("NEC firm panel:", format(nrow(panel), big.mark = ","), "obs ·",
    format(n_distinct(panel$lgcode), big.mark = ","), "munis · founding years",
    min(panel$year), "-", max(panel$year), "· lag=", LAG, "· ctrl=", CTRL, "\n", sep = "")
cat("  Controls: ", if (length(ctrl_cols)) paste(ctrl_cols, collapse=", ") else "(none)", "\n", sep="")
cat("  Standardised SSIV (panel pool): mean=", round(mean(panel$ssiv_index_2001), 5),
    " sd=", round(sd(panel$ssiv_index_2001), 5), "\n", sep = "")

# ---- Outcome groups ----------------------------------------------------------
GROUPS <- list(
  list(name = "FIRM PANEL — TOTAL", items = list(
    c("n_firms_surviving",      "# firms surviving",      "TRUE"),
    c("emp_surviving",          "Total emp surviving",    "TRUE"),
    c("rev_surviving",          "Total rev surviving",    "TRUE"),
    c("cap_surviving",          "Total capital surviving","TRUE"),
    c("median_firm_age_years",  "Median firm age (yrs)",  "FALSE")
  )),
  list(name = "BY SIZE", items = list(
    c("n_firms_surviving_size_micro_1",      "# micro (1)",       "TRUE"),
    c("n_firms_surviving_size_small_2_9",    "# small (2-9)",     "TRUE"),
    c("n_firms_surviving_size_medium_10_50", "# medium (10-50)",  "TRUE"),
    c("n_firms_surviving_size_large_51p",    "# large (51+)",     "TRUE")
  )),
  list(name = "BY SECTOR", items = list(
    c("n_firms_surviving_sec_manuf",       "Manufacturing",      "TRUE"),
    c("n_firms_surviving_sec_construct",   "Construction",       "TRUE"),
    c("n_firms_surviving_sec_wholesale",   "Wholesale & retail", "TRUE"),
    c("n_firms_surviving_sec_hospitality", "Hospitality",        "TRUE"),
    c("n_firms_surviving_sec_transport",   "Transport",          "TRUE"),
    c("n_firms_surviving_sec_services",    "Services",           "TRUE"),
    c("n_firms_surviving_sec_health",      "Health",             "TRUE"),
    c("n_firms_surviving_sec_education",   "Education",          "TRUE"),
    c("n_firms_surviving_sec_finance",     "Finance",            "TRUE"),
    c("n_firms_surviving_sec_arts",        "Arts",               "TRUE")
  )),
  list(name = "BY TRADABILITY", items = list(
    c("n_firms_surviving_trd_tradable_goods",        "Tradable goods",          "TRUE"),
    c("n_firms_surviving_trd_tradable_services",     "Tradable services",       "TRUE"),
    c("n_firms_surviving_trd_non_tradable_services", "Non-tradable services",   "TRUE")
  )),
  list(name = "BY MODERNITY", items = list(
    c("n_firms_surviving_modern_modern_services",        "Modern services",         "TRUE"),
    c("n_firms_surviving_modern_modern_manuf",           "Modern manufacturing",    "TRUE"),
    c("n_firms_surviving_modern_traditional_commerce",   "Traditional commerce",    "TRUE"),
    c("n_firms_surviving_modern_traditional_services",   "Traditional services",    "TRUE"),
    c("n_firms_surviving_modern_traditional_agriculture","Traditional agriculture", "TRUE")
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
  rows, title = sprintf("═══ NEC FIRM PANEL (founding-year × muni, 2001-2018, lag=%d, ctrl=%s) ═══",
                       LAG, CTRL),
  n_obs = nrow(panel), n_unit = n_distinct(panel$lgcode),
  unit_label = "muni-cohort",
  spec_str = "Spec: y_mt = α_m + γ_t + β·SSIV_z + λ·log(MI₀)·(t-2001) + ε   ;  cluster ~ lgcode"
)
print_notes("`year` is the founding year of the surviving cohort. SSIV standardised on the panel pool.")
