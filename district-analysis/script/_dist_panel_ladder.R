################################################################################
# Khanna-spec panel first-stage ladder, v2 SSIV.
#
# Spec:
#   y_{i,t} = beta * [ fx_{d(i),t-L} * log(mig_int_{d(i)}) ]
#           + C_mig  ( log(mig_int_{d(i)}) * year FE )
#           + C_fx   ( bare fx_{d(i),t-L} )
#           + C_X    ( year x 6 dest-region shares_{d(i),0} )
#           + alpha_i + gamma_t + eps_{i,t}
#
#   fx_{d,t}  = sum_c share_dc(v2) * rer_{c,t}
#   rer_{c,t} = log(NPR/LCU)_{c,t} - log(NPR/LCU)_{c,2010}
#   mig_int_d = mean DOFE permits 2009-10 / pop_2011  (district-constant)
#
# Two panels:
#   1. DOFE permits  : alpha_i = dname,  i = district, t = year (2011-23)
#                      outcome = log(permits + 1)
#   2. RVS HH panel  : alpha_i = hhid,   i = household, t = year (2016-18)
#                      outcomes = has_migrant_intl, n_intl_migrants,
#                                 log(remit_amount_intl + 1)
#
# SE clustered at ~dname (= "municipality" in paper notation) in both cases.
# Columns = ladder M1->M4, rows = lag L in {0,1,2,3}.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

# ---- name normalization ----
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
rvs_hh   <- read_csv("district-analysis/data/clean/rvs/migration_hh_year.csv", show_col_types = FALSE) %>%
              mutate(dname = to_dname(district_name))

# ---- FX: rer_ct ----
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

# ---- v2 = ALL 2009-10 DOFE dest with positive permits & in FX ----
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

# ---- shares ----
sh_v2 <- dofe %>%
  filter(year %in% c(2009, 2010), country %in% set_v2) %>%
  group_by(dname, country) %>%
  summarise(permits = sum(permits), .groups = "drop") %>%
  group_by(dname) %>%
  mutate(share = permits / sum(permits)) %>%
  ungroup() %>%
  select(dname, country, share)

# ---- district SSIV at every year ----
z_v2 <- sh_v2 %>%
  inner_join(fx, by = "country", relationship = "many-to-many") %>%
  mutate(x = share * rer) %>%
  group_by(dname, year) %>%
  summarise(z = sum(x, na.rm = TRUE), .groups = "drop") %>%
  rename(z_v2 = z)

# ---- migration intensity ----
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
  mutate(mig_int  = num / pop_2011,
         log_mi   = log(pmax(mig_int, 1e-12)),
         log_mi_z = (log_mi - mean(log_mi)) / sd(log_mi)) %>%
  select(dname, log_mi_z)

# ---- DOFE district x year panel 2011-2023 ----
districts_dofe <- sort(intersect(unique(z_v2$dname), mi$dname))
YRS_DOFE       <- 2011:2023
grid_dofe      <- expand_grid(dname = districts_dofe, year = YRS_DOFE)

perm_d <- dofe %>%
  filter(country %in% set_v2) %>%
  group_by(dname, year) %>%
  summarise(permits = sum(permits), .groups = "drop")

dofe_panel <- grid_dofe %>%
  left_join(perm_d, by = c("dname", "year")) %>%
  replace_na(list(permits = 0)) %>%
  mutate(log_perm = log(permits + 1)) %>%
  inner_join(mi, by = "dname") %>%
  left_join(regions, by = "dname")

# ---- RVS HH x year panel 2016-2018 ----
rvs_hh_panel <- rvs_hh %>%
  inner_join(mi, by = "dname") %>%
  left_join(regions, by = "dname") %>%
  mutate(across(c(has_migrant, has_migrant_internal, has_migrant_intl,
                  n_migrants, n_intl_migrants, remit_received,
                  remit_amount_12m_rs, remit_amount_intl_12m_rs),
                as.numeric),
         log_n_migrants     = log(n_migrants + 1),
         log_n_intl_mig     = log(n_intl_migrants + 1),
         log_remit          = log(remit_amount_12m_rs + 1),
         log_remit_intl     = log(remit_amount_intl_12m_rs + 1))

# ---- helper: attach z at lags 0..3 and standardize ----
attach_z <- function(panel) {
  for (L in 0:3) {
    out_col <- paste0("z_v2_L", L)
    tmp <- z_v2 %>% mutate(year = year + L) %>%
      select(dname, year, !!out_col := z_v2)
    panel <- panel %>% left_join(tmp, by = c("dname", "year"))
  }
  for (L in 0:3) {
    col <- paste0("z_v2_L", L)
    m  <- mean(panel[[col]], na.rm = TRUE)
    panel[[paste0(col, "_std")]] <- (panel[[col]] - m) / sd(panel[[col]], na.rm = TRUE)
  }
  panel
}
dofe_panel    <- attach_z(dofe_panel)
rvs_hh_panel  <- attach_z(rvs_hh_panel)

# ---- regression engine ----
REGION_COLS <- c("share_e_asia", "share_gulf", "share_oecd_north",
                 "share_s_asia", "share_se_asia", "share_oecd_europe")

stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))
}

run_ladder <- function(panel, ycol, entity_fe, label, lags = 0:3) {
  refyr <- min(panel$year, na.rm = TRUE)
  region_terms <- paste(sprintf("i(year, %s, ref = %d)", REGION_COLS, refyr),
                        collapse = " + ")

  fe_str <- paste(entity_fe, "year", sep = " + ")
  rows_long <- list()
  tab <- list()

  for (L in lags) {
    z_std <- paste0("z_v2_L", L, "_std")
    panel$z_inter <- panel[[z_std]] * panel$log_mi_z
    panel$z_bare  <- panel[[z_std]]

    f_M1 <- as.formula(sprintf("%s ~ z_inter | %s", ycol, fe_str))
    f_M2 <- as.formula(sprintf("%s ~ z_inter + i(year, log_mi_z, ref = %d) | %s",
                               ycol, refyr, fe_str))
    f_M3 <- as.formula(sprintf("%s ~ z_inter + i(year, log_mi_z, ref = %d) + z_bare | %s",
                               ycol, refyr, fe_str))
    f_M4 <- as.formula(sprintf("%s ~ z_inter + i(year, log_mi_z, ref = %d) + z_bare + %s | %s",
                               ycol, refyr, region_terms, fe_str))

    row <- list(lag = L)
    for (mlabel in c("M1","M2","M3","M4")) {
      f <- switch(mlabel, M1 = f_M1, M2 = f_M2, M3 = f_M3, M4 = f_M4)
      fit <- tryCatch(feols(f, data = panel, cluster = ~dname),
                      error = function(e) NULL)
      if (is.null(fit)) { row[[mlabel]] <- "—"; next }
      s <- summary(fit)$coeftable
      if (!"z_inter" %in% rownames(s)) { row[[mlabel]] <- "—"; next }
      b  <- s["z_inter", "Estimate"]
      se <- s["z_inter", "Std. Error"]
      pv <- s["z_inter", "Pr(>|t|)"]
      mean_y <- mean(predict(fit) + residuals(fit), na.rm = TRUE)
      row[[mlabel]] <- sprintf("%.4f%s\n(%.4f)", b, stars(pv), se)
      rows_long[[length(rows_long)+1]] <- tibble(
        outcome = label, lag = L, model = mlabel,
        beta = round(b,4), se = round(se,4),
        t = round(s["z_inter","t value"],2), p = round(pv,4),
        sig = stars(pv),
        mean_y = round(mean_y,4),
        pct_of_mean = round(100*b/mean_y,2),
        n = nobs(fit)
      )
    }
    tab[[length(tab)+1]] <- row
  }

  cat(sprintf("\n==== %s    entity FE = %s,  cluster = ~dname    ====\n",
              label, entity_fe))
  cat(sprintf("n_obs (any-lag): %d   |   mean(Y) = %.4f\n\n",
              nrow(panel %>% filter(!is.na(.data[[ycol]]))),
              mean(panel[[ycol]], na.rm = TRUE)))
  cat(sprintf("%-5s | %-13s | %-13s | %-13s | %-13s\n",
              "lag", "M1 (bare)", "M2 (+C_mig)", "M3 (+C_fx)", "M4 (+C_X)"))
  cat(strrep("-", 76), "\n", sep = "")
  for (row in tab) {
    cells   <- sapply(c("M1","M2","M3","M4"), function(m) strsplit(row[[m]], "\n")[[1]][1])
    cell_se <- sapply(c("M1","M2","M3","M4"), function(m) strsplit(row[[m]], "\n")[[1]][2])
    cat(sprintf("L=%-2d  | %-13s | %-13s | %-13s | %-13s\n",
                row$lag, cells["M1"], cells["M2"], cells["M3"], cells["M4"]))
    cat(sprintf("      | %-13s | %-13s | %-13s | %-13s\n",
                cell_se["M1"], cell_se["M2"], cell_se["M3"], cell_se["M4"]))
  }
  cat("\n")
  bind_rows(rows_long)
}

# ---- run ----
all_rows <- list()

# DOFE: all 4 lags
all_rows[[length(all_rows)+1]] <- run_ladder(dofe_panel, "log_perm", "dname",
                                             "DOFE log(permits+1)")

# RVS: lag 2 only, all ladder
# Full sample - binary 0/1 (LPM) and counts
for (yc in c("has_migrant", "has_migrant_internal", "has_migrant_intl",
             "n_migrants", "n_intl_migrants", "remit_received",
             "log_n_migrants", "log_n_intl_mig")) {
  all_rows[[length(all_rows)+1]] <- run_ladder(rvs_hh_panel, yc, "hhid",
                                               paste0("RVS ", yc), lags = 2)
}

# remit_amount_12m_rs : conditional on has_migrant == 1
rvs_amt <- rvs_hh_panel %>% filter(has_migrant == 1)
all_rows[[length(all_rows)+1]] <- run_ladder(rvs_amt, "remit_amount_12m_rs", "hhid",
                                             "RVS remit_amount (if has_migrant=1)",
                                             lags = 2)
all_rows[[length(all_rows)+1]] <- run_ladder(rvs_amt, "log_remit", "hhid",
                                             "RVS log(remit+1) (if has_migrant=1)",
                                             lags = 2)

# remit_amount_intl_12m_rs : conditional on has_migrant_intl == 1
rvs_intl_amt <- rvs_hh_panel %>% filter(has_migrant_intl == 1)
all_rows[[length(all_rows)+1]] <- run_ladder(rvs_intl_amt, "remit_amount_intl_12m_rs", "hhid",
                                             "RVS remit_amount_intl (if has_migrant_intl=1)",
                                             lags = 2)
all_rows[[length(all_rows)+1]] <- run_ladder(rvs_intl_amt, "log_remit_intl", "hhid",
                                             "RVS log(remit_intl+1) (if has_migrant_intl=1)",
                                             lags = 2)

# ---- Census 2021 cross-section: 75 districts, single year ----
# Outcome year = 2021, shifter year = 2021 - L (lag 2 -> z built from rer_{c,2019})
census <- read_csv("district-analysis/data/clean/census/outcomes_district.csv",
                   show_col_types = FALSE) %>%
  filter(year == 2021) %>%
  select(dname, absent_hh_share, mig_in_international, mig_in_share, mig_in_domestic)

absentee21 <- read_csv("district-analysis/data/clean/census/absentee_2021_non_india_dist.csv",
                       show_col_types = FALSE) %>%
  transmute(dname,
            log_n_absentees    = log(n_absentees + 1),
            log_n_absentees_wt = log(n_absentees_weighted + 1),
            log_n_male         = log(n_male + 1),
            log_n_female       = log(n_female + 1))

cs21 <- census %>% full_join(absentee21, by = "dname") %>%
  inner_join(mi, by = "dname") %>%
  left_join(regions, by = "dname")

# Build z at year 2021-L per lag from the panel z_v2 series
for (L in 0:3) {
  zsub <- z_v2 %>% filter(year == 2021 - L) %>%
    transmute(dname, !!paste0("z_v2_L", L) := z_v2)
  cs21 <- cs21 %>% left_join(zsub, by = "dname")
}
for (L in 0:3) {
  col <- paste0("z_v2_L", L)
  m <- mean(cs21[[col]], na.rm = TRUE)
  cs21[[paste0(col, "_std")]] <- (cs21[[col]] - m) / sd(cs21[[col]], na.rm = TRUE)
}

run_cs_ladder <- function(panel, ycol, label, L = 2) {
  z_std <- paste0("z_v2_L", L, "_std")
  panel$z_inter <- panel[[z_std]] * panel$log_mi_z
  panel$z_bare  <- panel[[z_std]]

  region_terms <- paste(REGION_COLS, collapse = " + ")
  f_M1 <- as.formula(sprintf("%s ~ z_inter", ycol))
  f_M2 <- as.formula(sprintf("%s ~ z_inter + log_mi_z", ycol))
  f_M3 <- as.formula(sprintf("%s ~ z_inter + log_mi_z + z_bare", ycol))
  f_M4 <- as.formula(sprintf("%s ~ z_inter + log_mi_z + z_bare + %s", ycol, region_terms))

  rows_long <- list(); cells <- list(); cells_se <- list()
  for (mlabel in c("M1","M2","M3","M4")) {
    f <- switch(mlabel, M1=f_M1, M2=f_M2, M3=f_M3, M4=f_M4)
    fit <- tryCatch(feols(f, data = panel, vcov = "hetero"),
                    error = function(e) NULL)
    if (is.null(fit)) { cells[[mlabel]] <- "—"; cells_se[[mlabel]] <- ""; next }
    s <- summary(fit)$coeftable
    if (!"z_inter" %in% rownames(s)) { cells[[mlabel]] <- "—"; cells_se[[mlabel]] <- ""; next }
    b  <- s["z_inter","Estimate"]; se <- s["z_inter","Std. Error"]
    pv <- s["z_inter","Pr(>|t|)"]
    mean_y <- mean(panel[[ycol]], na.rm = TRUE)
    cells[[mlabel]]    <- sprintf("%.4f%s", b, stars(pv))
    cells_se[[mlabel]] <- sprintf("(%.4f)", se)
    rows_long[[length(rows_long)+1]] <- tibble(
      outcome=label, lag=L, model=mlabel,
      beta=round(b,4), se=round(se,4),
      t=round(s["z_inter","t value"],2), p=round(pv,4),
      sig=stars(pv), mean_y=round(mean_y,4),
      pct_of_mean=round(100*b/mean_y,2), n=nobs(fit))
  }

  cat(sprintf("\n==== %s    cross-section (75 districts, lag %d), HC1 SE ====\n",
              label, L))
  cat(sprintf("mean(Y) = %.4f\n\n", mean(panel[[ycol]], na.rm = TRUE)))
  cat(sprintf("%-5s | %-13s | %-13s | %-13s | %-13s\n",
              "lag", "M1 (bare)", "M2 (+log_mi)", "M3 (+z bare)", "M4 (+regions)"))
  cat(strrep("-", 76), "\n", sep = "")
  cat(sprintf("L=%-2d  | %-13s | %-13s | %-13s | %-13s\n",
              L, cells$M1, cells$M2, cells$M3, cells$M4))
  cat(sprintf("      | %-13s | %-13s | %-13s | %-13s\n",
              cells_se$M1, cells_se$M2, cells_se$M3, cells_se$M4))
  cat("\n")
  bind_rows(rows_long)
}

for (yc in c("absent_hh_share", "mig_in_international", "mig_in_share", "mig_in_domestic",
             "log_n_absentees", "log_n_absentees_wt", "log_n_male", "log_n_female")) {
  all_rows[[length(all_rows)+1]] <- run_cs_ladder(cs21, yc, paste0("Census 2021 ", yc))
}

out <- bind_rows(all_rows)
dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(out, "district-analysis/output/tab/dist_panel_ladder.csv")
cat(sprintf("\nSaved: district-analysis/output/tab/dist_panel_ladder.csv (%d rows)\n", nrow(out)))
