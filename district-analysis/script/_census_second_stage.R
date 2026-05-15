################################################################################
# Census 2011-2021 panel SECOND-STAGE on v2 SSIV.
#
# Spec (same as first-stage ladder, only outcome changes):
#   y_{d,t} = beta * [ z_{d,t-2} * mig_per_1000_z ]
#           + C_mig  (mig_per_1000_z * year FE)
#           + C_fx   (bare z_{d,t-2})
#           + C_X    (year x 6 dest-region shares)
#           + alpha_d + gamma_t + eps_{d,t}
#
#   z_{d,t}    = sum_c share_dc(v2) * rer_{c,t}
#   rer_{c,t}  = log(NPR/LCU)_{c,t} - log(NPR/LCU)_{c,2010}
#
# Panel: 75 districts x {2011, 2021}, 150 obs.
# Lag = 2: shifter at 2011 uses rer_{c,2009}; shifter at 2021 uses rer_{c,2019}.
# SE clustered ~dname.  Lag fixed at 2 (matches first-stage headline).
#
# Outcome groups:
#   industry   (11 vars)  ind_*
#   occupation (10 vars)  occ_share_*
#   assets     (11 vars)  amen_assets_*
#   amenities  ( 7 vars)  water/cooking/lighting/toilet
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

# ---- name normalization (same as first-stage) ----
dofe_to_census <- c(
  "CHITWAN" = "Chitawan", "DHANUSHA" = "Dhanusa", "KAPILVASTU" = "Kapilbastu",
  "MAKAWANPUR" = "Makwanpur", "TANAHUN" = "Tanahu", "TEHRATHUM" = "Terhathum",
  "KABHREPALANCHOK" = "Kavrepalanchok"
)
to_dname <- function(s) {
  u <- toupper(trimws(as.character(s)))
  ifelse(u %in% names(dofe_to_census),
         dofe_to_census[u],
         tools::toTitleCase(tolower(as.character(s))))
}

# ---- load ----
forex    <- read_csv("district-analysis/data/clean/forex_2000_2023.csv", show_col_types = FALSE)
dofe_raw <- read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv", show_col_types = FALSE)
pop_file <- read_csv("district-analysis/data/clean/foreign_migration_district_population.csv", show_col_types = FALSE)
regions  <- read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv", show_col_types = FALSE)
census   <- read_csv("district-analysis/data/clean/census/outcomes_district.csv", show_col_types = FALSE)

# ---- FX panel (same as first-stage) ----
nepal_fx <- forex %>% filter(country == "Nepal") %>%
  transmute(year, npr_per_usd = forex)
fx <- forex %>%
  filter(!country %in% c("Nepal", "India"), !is.na(forex)) %>%
  transmute(country, year, lcu_per_usd = forex) %>%
  inner_join(nepal_fx, by = "year") %>%
  mutate(log_npr_per_lcu = log(npr_per_usd / lcu_per_usd)) %>%
  filter(!is.na(log_npr_per_lcu)) %>%
  group_by(country) %>%
  mutate(base_2010 = log_npr_per_lcu[year == 2010][1]) %>%
  ungroup() %>%
  filter(!is.na(base_2010)) %>%
  mutate(rer = log_npr_per_lcu - base_2010) %>%
  select(country, year, rer)
fx_countries <- unique(fx$country)

# ---- v2 SSIV ----
dofe <- dofe_raw %>%
  filter(!is.na(country)) %>%
  mutate(dname = to_dname(district_rename)) %>%
  group_by(dname, country, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  filter(!country %in% c("Nepal", "India"))

v2_tot <- dofe %>% filter(year %in% c(2009, 2010)) %>%
  group_by(country) %>% summarise(tot = sum(permits), .groups = "drop")
set_v2 <- sort(intersect(v2_tot$country[v2_tot$tot > 0], fx_countries))
cat(sprintf("v2 destinations: %d\n", length(set_v2)))

sh_v2 <- dofe %>%
  filter(year %in% c(2009, 2010), country %in% set_v2) %>%
  group_by(dname, country) %>%
  summarise(permits = sum(permits), .groups = "drop") %>%
  group_by(dname) %>%
  mutate(share = permits / sum(permits)) %>%
  ungroup() %>%
  select(dname, country, share)

z_v2 <- sh_v2 %>%
  inner_join(fx, by = "country", relationship = "many-to-many") %>%
  mutate(x = share * rer) %>%
  group_by(dname, year) %>%
  summarise(z = sum(x, na.rm = TRUE), .groups = "drop") %>%
  rename(z_v2 = z)

# ---- mig intensity ----
mi <- dofe %>%
  filter(year %in% c(2009, 2010)) %>%
  group_by(dname) %>%
  summarise(num = mean(permits), .groups = "drop") %>%
  left_join(
    pop_file %>%
      mutate(dname = to_dname(district)) %>%
      select(dname, pop_2011 = district_population_2011) %>%
      distinct(dname, .keep_all = TRUE),
    by = "dname"
  ) %>%
  mutate(mig_int      = num / pop_2011,
         mig_per_1000 = mig_int * 1000,
         mig_per_1000_z = (mig_per_1000 - mean(mig_per_1000)) / sd(mig_per_1000)) %>%
  select(dname, mig_per_1000, mig_per_1000_z)

# ---- census 2011-2021 panel ----
census_panel <- census %>%
  filter(year %in% c(2011, 2021)) %>%
  inner_join(mi, by = "dname") %>%
  left_join(regions, by = "dname")

# Attach z_v2 at lag 2: panel year 2011 -> rer at 2009; 2021 -> rer at 2019
z_v2_lag2 <- z_v2 %>% mutate(year = year + 2) %>% rename(z_v2_L2 = z_v2)
census_panel <- census_panel %>%
  left_join(z_v2_lag2, by = c("dname", "year"))

# Standardize z (mean 0, sd 1 across the panel)
m_z <- mean(census_panel$z_v2_L2, na.rm = TRUE)
s_z <- sd(census_panel$z_v2_L2,   na.rm = TRUE)
census_panel$z_v2_L2_std <- (census_panel$z_v2_L2 - m_z) / s_z

# ---- regression engine ----
REGION_COLS <- c("share_e_asia", "share_gulf", "share_oecd_north",
                 "share_s_asia", "share_se_asia", "share_oecd_europe")

stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))
}

run_ladder_cs <- function(panel, ycol, label, group) {
  refyr <- 2011
  region_terms <- paste(sprintf("i(year, %s, ref = %d)", REGION_COLS, refyr),
                        collapse = " + ")

  panel$z_inter <- panel$z_v2_L2_std * panel$mig_per_1000_z
  panel$z_bare  <- panel$z_v2_L2_std

  f_M1 <- as.formula(sprintf("%s ~ z_inter | dname + year", ycol))
  f_M2 <- as.formula(sprintf("%s ~ z_inter + i(year, mig_per_1000_z, ref = %d) | dname + year",
                             ycol, refyr))
  f_M3 <- as.formula(sprintf("%s ~ z_inter + i(year, mig_per_1000_z, ref = %d) + z_bare | dname + year",
                             ycol, refyr))
  f_M4 <- as.formula(sprintf("%s ~ z_inter + i(year, mig_per_1000_z, ref = %d) + z_bare + %s | dname + year",
                             ycol, refyr, region_terms))

  rows_long <- list()
  cells <- list(); cells_se <- list()
  for (mlabel in c("M1","M2","M3","M4")) {
    f <- switch(mlabel, M1=f_M1, M2=f_M2, M3=f_M3, M4=f_M4)
    fit <- tryCatch(feols(f, data = panel, cluster = ~dname),
                    error = function(e) NULL)
    if (is.null(fit)) { cells[[mlabel]] <- "—"; cells_se[[mlabel]] <- ""; next }
    s <- summary(fit)$coeftable
    if (!"z_inter" %in% rownames(s)) { cells[[mlabel]] <- "—"; cells_se[[mlabel]] <- ""; next }
    b  <- s["z_inter","Estimate"]; se <- s["z_inter","Std. Error"]
    pv <- s["z_inter","Pr(>|t|)"]
    mean_y <- mean(predict(fit) + residuals(fit), na.rm = TRUE)
    cells[[mlabel]]    <- sprintf("%.4f%s", b, stars(pv))
    cells_se[[mlabel]] <- sprintf("(%.4f)", se)
    rows_long[[length(rows_long)+1]] <- tibble(
      group=group, outcome=label, model=mlabel,
      beta=round(b,4), se=round(se,4),
      t=round(s["z_inter","t value"],2), p=round(pv,4),
      sig=stars(pv), mean_y=round(mean_y,4),
      pct_of_mean=round(100*b/mean_y,2), n=nobs(fit))
  }
  list(rows = bind_rows(rows_long),
       cells = cells, cells_se = cells_se,
       mean_y = mean(panel[[ycol]], na.rm = TRUE))
}

# ---- outcome groups ----
GROUPS <- list(
  industry = c("ind_agri_forestry_fish","ind_manufacturing","ind_construction",
               "ind_wholesale_retail","ind_transport_accommodation",
               "ind_finance_real_estate_prof","ind_public_admin_defence",
               "ind_education","ind_health","ind_arts_recreation","ind_others"),
  occupation = c("occ_share_armed_forces","occ_share_managers","occ_share_professionals",
                 "occ_share_technicians","occ_share_office_assistants",
                 "occ_share_service_sales","occ_share_agriculture",
                 "occ_share_craft_trades","occ_share_machine_operators",
                 "occ_share_elementary"),
  assets = c("amen_assets_radio","amen_assets_tv","amen_assets_cycle",
             "amen_assets_motorcycle","amen_assets_car","amen_assets_fridge",
             "amen_assets_landline","amen_assets_mobile","amen_assets_computer",
             "amen_assets_internet","amen_asset_count_mean"),
  amenities = c("amen_water_piped","amen_water_traditional","amen_cooking_modern",
                "amen_cooking_traditional","amen_lighting_electricity",
                "amen_toilet_modern","amen_toilet_any")
)

all_rows <- list()
for (grp in names(GROUPS)) {
  cat(sprintf("\n==== %s outcomes  (panel 2011-2021, 150 obs, lag 2) ====\n", toupper(grp)))
  cat(sprintf("%-32s | %-13s | %-13s | %-13s | %-13s | %-9s\n",
              "outcome", "M1 (bare)", "M2 (+C_mig)", "M3 (+C_fx)", "M4 (+C_X)", "mean Y"))
  cat(strrep("-", 110), "\n", sep = "")
  for (yc in GROUPS[[grp]]) {
    if (!yc %in% names(census_panel)) next
    res <- run_ladder_cs(census_panel, yc, yc, grp)
    cat(sprintf("%-32s | %-13s | %-13s | %-13s | %-13s | %9.4f\n",
                yc, res$cells$M1, res$cells$M2, res$cells$M3, res$cells$M4, res$mean_y))
    cat(sprintf("%-32s | %-13s | %-13s | %-13s | %-13s |\n",
                "", res$cells_se$M1, res$cells_se$M2, res$cells_se$M3, res$cells_se$M4))
    all_rows[[length(all_rows)+1]] <- res$rows
  }
}

out <- bind_rows(all_rows)
dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(out, "district-analysis/output/tab/census_second_stage.csv")
cat(sprintf("\nSaved: district-analysis/output/tab/census_second_stage.csv (%d rows)\n", nrow(out)))
