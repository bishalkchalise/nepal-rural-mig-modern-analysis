################################################################################
# District-level panel first-stage ladder for v2 SSIV only.
#
# Spec (Khanna):
#   y_{d,t} = beta * [ fx_{d,t-L} * log(mig_int_d) ]
#           + C_mig  (log(mig_int_d) * year FE)
#           + C_fx   (bare fx_{d,t-L})
#           + C_X    (year x 6 dest-region shares)
#           + alpha_d + gamma_t + eps_{d,t}
#
#   fx_{d,t}  = sum_c share_dc(v2) * rer_{c,t}        ( v2 = all DOFE 2009-10 dest with positive permits )
#   rer_{c,t} = log(NPR/LCU)_{c,t} - log(NPR/LCU)_{c,2010}
#   mig_int_d = mean DOFE permits 2009-10 / pop_2011  ( district-constant )
#
# Output: for each outcome, a 4-row x 4-col table where
#   rows = lag L in {0,1,2,3}
#   cols = M1 (bare), M2 (+C_mig), M3 (+C_fx), M4 (+C_X)
#
# Outcomes:
#   - DOFE panel: log(permits + 1)                      ( 75 dist x 13 yr )
#   - RVS panel:  hh_intl_share, intl_per_hh,
#                 log_wt_intl_mig, log_remit_per_hh     ( 50 dist x 3 yr  )
#
# SE clustered at ~dname.  Run with: source("district-analysis/script/_dist_panel_ladder.R")
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
rvs      <- read_csv("district-analysis/data/clean/rvs/migration_district_year.csv", show_col_types = FALSE) %>%
              mutate(dname = to_dname(dname_raw))

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

# ---- v2 destination set: ALL 2009-10 DOFE dest with positive permits & in FX ----
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

# ---- shares (v2 only) ----
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

# ---- migration intensity (district-constant) ----
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

# ---- DOFE panel 2011-2023 ----
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

# ---- RVS panel 2016-2018 ----
rvs_panel <- rvs %>%
  inner_join(mi, by = "dname") %>%
  left_join(regions, by = "dname") %>%
  mutate(hh_intl_share    = n_hh_with_intl_migrant / n_hh,
         intl_per_hh      = n_intl_migrants       / n_hh,
         log_wt_intl_mig  = log(wt_intl_migrants + 1),
         log_remit_per_hh = log((remit_amount_intl_12m_rs / n_hh) + 1))

# ---- helper: add z at lags 0..3 and standardize ----
attach_z <- function(panel) {
  for (L in 0:3) {
    out_col <- paste0("z_v2_L", L)
    tmp <- z_v2 %>% mutate(year = year + L) %>%
      select(dname, year, !!out_col := z_v2)
    panel <- panel %>% left_join(tmp, by = c("dname", "year"))
  }
  # standardize across the panel
  for (L in 0:3) {
    col <- paste0("z_v2_L", L)
    panel[[paste0(col, "_std")]] <- panel[[col]] / sd(panel[[col]], na.rm = TRUE)
  }
  panel
}

dofe_panel <- attach_z(dofe_panel)
rvs_panel  <- attach_z(rvs_panel)

# ---- regression engine ----
REGION_COLS <- c("share_e_asia", "share_gulf", "share_oecd_north",
                 "share_s_asia", "share_se_asia", "share_oecd_europe")

stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))
}

run_ladder <- function(panel, ycol, label) {
  refyr <- min(panel$year, na.rm = TRUE)
  region_terms <- paste(sprintf("i(year, %s, ref = %d)", REGION_COLS, refyr),
                        collapse = " + ")

  tab <- list()
  rows_long <- list()

  for (L in 0:3) {
    z_std <- paste0("z_v2_L", L, "_std")
    panel$z_inter <- panel[[z_std]] * panel$log_mi_z
    panel$z_bare  <- panel[[z_std]]

    f_M1 <- as.formula(sprintf("%s ~ z_inter | dname + year", ycol))
    f_M2 <- as.formula(sprintf("%s ~ z_inter + i(year, log_mi_z, ref = %d) | dname + year", ycol, refyr))
    f_M3 <- as.formula(sprintf("%s ~ z_inter + i(year, log_mi_z, ref = %d) + z_bare | dname + year", ycol, refyr))
    f_M4 <- as.formula(sprintf("%s ~ z_inter + i(year, log_mi_z, ref = %d) + z_bare + %s | dname + year",
                               ycol, refyr, region_terms))

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

  # ---- print column-format table ----
  cat(sprintf("\n==== Outcome: %s   (n_dist=%d, years=%d-%d) ====\n",
              label, length(unique(panel$dname)),
              min(panel$year), max(panel$year)))
  cat(sprintf("\nbeta on z_v2*log_mi_z   (clustered SE in parens, stars: *p<0.10, **p<0.05, ***p<0.01)\n\n"))

  mean_y <- mean(panel[[ycol]], na.rm = TRUE)
  cat(sprintf("Mean(Y) = %.4f\n\n", mean_y))

  cat(sprintf("%-5s | %-14s | %-14s | %-14s | %-14s\n",
              "lag", "M1 (bare)", "M2 (+C_mig)", "M3 (+C_fx)", "M4 (+C_X)"))
  cat(strrep("-", 80), "\n", sep = "")
  for (row in tab) {
    cells <- sapply(c("M1","M2","M3","M4"),
                    function(m) strsplit(row[[m]], "\n")[[1]][1])
    cat(sprintf("L=%-2d  | %-14s | %-14s | %-14s | %-14s\n",
                row$lag, cells["M1"], cells["M2"], cells["M3"], cells["M4"]))
    cells_se <- sapply(c("M1","M2","M3","M4"),
                       function(m) strsplit(row[[m]], "\n")[[1]][2])
    cat(sprintf("      | %-14s | %-14s | %-14s | %-14s\n",
                cells_se["M1"], cells_se["M2"], cells_se["M3"], cells_se["M4"]))
  }
  cat("\n")
  bind_rows(rows_long)
}

# ---- run all outcomes ----
all_rows <- list()
all_rows[[length(all_rows)+1]] <- run_ladder(dofe_panel, "log_perm",         "DOFE log(permits+1)")
all_rows[[length(all_rows)+1]] <- run_ladder(rvs_panel,  "hh_intl_share",    "RVS hh_intl_share")
all_rows[[length(all_rows)+1]] <- run_ladder(rvs_panel,  "intl_per_hh",      "RVS intl_per_hh")
all_rows[[length(all_rows)+1]] <- run_ladder(rvs_panel,  "log_wt_intl_mig",  "RVS log(wt_intl_mig+1)")
all_rows[[length(all_rows)+1]] <- run_ladder(rvs_panel,  "log_remit_per_hh", "RVS log(remit_per_hh+1)")

out <- bind_rows(all_rows)
dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(out, "district-analysis/output/tab/dist_panel_ladder.csv")
cat(sprintf("\nSaved: district-analysis/output/tab/dist_panel_ladder.csv (%d rows)\n", nrow(out)))
