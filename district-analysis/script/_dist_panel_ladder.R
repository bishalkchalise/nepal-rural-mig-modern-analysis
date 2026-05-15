################################################################################
# District-level panel first-stage ladder (M1 -> M4) with lag 0-3.
#
# Spec:
#   y_{d,t} = beta * [ fx_{d,t-L} * log(mig_int_d) ]
#           + C_mig      (log(mig_int_d) * year FE)
#           + C_fx       (bare fx_{d,t-L})
#           + C_X        (year x 6 dest-region shares)
#           + alpha_d + gamma_t + eps_{d,t}
#
#   fx_{d,t}  = sum_c share_dc(v) * rer_{c,t}
#   rer_{c,t} = log(NPR/LCU)_{c,t} - log(NPR/LCU)_{c,2010}
#   mig_int_d = mean DOFE permits 2009-10 / pop_2011  (district-constant)
#
# Versions: v1 (2001 census, 20 dest), v2 (2009-10 DOFE >=50, 14 dest).
# Outcome:  log(DOFE permits + 1), 75 districts x 13 years (2011-2023).
# SE clustered at ~dname.  Run from repo root.
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
m01      <- read_csv("district-analysis/data/clean/dist_mig_pop_2001.csv", show_col_types = FALSE)
pop_file <- read_csv("district-analysis/data/clean/foreign_migration_district_population.csv", show_col_types = FALSE)
regions  <- read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv", show_col_types = FALSE)

# ---- FX: rer_ct = log(NPR/LCU) - log(NPR/LCU)_{c,2010} ----
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

# ---- destination sets ----
dofe <- dofe_raw %>%
  filter(!is.na(country)) %>%
  mutate(dname = to_dname(district_rename)) %>%
  group_by(dname, country, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  filter(!country %in% c("Nepal", "India"))

set_v1 <- sort(intersect(unique(m01$country), fx_countries))
v2_tot <- dofe %>% filter(year %in% c(2009, 2010)) %>%
  group_by(country) %>% summarise(tot = sum(permits), .groups = "drop")
set_v2 <- sort(intersect(v2_tot$country[v2_tot$tot >= 50], fx_countries))
cat(sprintf("v1: %d destinations | v2: %d destinations\n", length(set_v1), length(set_v2)))

# ---- shares ----
sh_v1 <- m01 %>%
  filter(country %in% set_v1) %>%
  rename(mig01 = dist_mig_pop_2001) %>%
  group_by(dname) %>%
  mutate(share = mig01 / sum(mig01)) %>%
  ungroup() %>%
  select(dname, country, share)

sh_v2 <- dofe %>%
  filter(year %in% c(2009, 2010), country %in% set_v2) %>%
  group_by(dname, country) %>%
  summarise(permits = sum(permits), .groups = "drop") %>%
  group_by(dname) %>%
  mutate(share = permits / sum(permits)) %>%
  ungroup() %>%
  select(dname, country, share)

# ---- district SSIV at every year (incl pre-2011 for lags) ----
build_z <- function(shares) {
  shares %>%
    inner_join(fx, by = "country") %>%
    mutate(x = share * rer) %>%
    group_by(dname, year) %>%
    summarise(z = sum(x, na.rm = TRUE), .groups = "drop")
}
z_v1 <- build_z(sh_v1) %>% rename(z_v1 = z)
z_v2 <- build_z(sh_v2) %>% rename(z_v2 = z)

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

# ---- panel 2011-2023 ----
districts  <- sort(intersect(unique(z_v1$dname), mi$dname))
YRS        <- 2011:2023
grid       <- expand_grid(dname = districts, year = YRS)
dest_union <- union(set_v1, set_v2)

perm_d <- dofe %>%
  filter(country %in% dest_union) %>%
  group_by(dname, year) %>%
  summarise(permits = sum(permits), .groups = "drop")

panel <- grid %>%
  left_join(perm_d, by = c("dname", "year")) %>%
  replace_na(list(permits = 0)) %>%
  mutate(log_perm = log(permits + 1)) %>%
  inner_join(mi, by = "dname") %>%
  left_join(regions, by = "dname")

# ---- add z at lags 0..3 ----
for (ver in c("v1", "v2")) {
  zdf  <- if (ver == "v1") z_v1 else z_v2
  zcol <- paste0("z_", ver)
  for (L in 0:3) {
    out_col <- paste0(zcol, "_L", L)
    tmp <- zdf %>% mutate(year = year + L)
    tmp <- tmp %>% select(dname, year, !!out_col := all_of(zcol))
    panel <- panel %>% left_join(tmp, by = c("dname", "year"))
  }
}

# Standardize each z (sd across panel, ignoring NAs)
for (ver in c("v1", "v2")) {
  for (L in 0:3) {
    col <- paste0("z_", ver, "_L", L)
    sd_col <- sd(panel[[col]], na.rm = TRUE)
    panel[[paste0(col, "_std")]] <- panel[[col]] / sd_col
  }
}

# ---- ladder x lags ----
REGION_COLS <- c("share_e_asia", "share_gulf", "share_oecd_north",
                 "share_s_asia", "share_se_asia", "share_oecd_europe")

stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))
}

results <- list()
for (ver in c("v1", "v2")) {
  for (L in 0:3) {
    z_std_col <- paste0("z_", ver, "_L", L, "_std")
    panel$z_inter <- panel[[z_std_col]] * panel$log_mi_z
    panel$z_bare  <- panel[[z_std_col]]

    region_terms <- paste(sprintf("i(year, %s, ref = 2011)", REGION_COLS),
                          collapse = " + ")

    f_M1 <- as.formula("log_perm ~ z_inter | dname + year")
    f_M2 <- as.formula("log_perm ~ z_inter + i(year, log_mi_z, ref = 2011) | dname + year")
    f_M3 <- as.formula("log_perm ~ z_inter + i(year, log_mi_z, ref = 2011) + z_bare | dname + year")
    f_M4 <- as.formula(paste0("log_perm ~ z_inter + i(year, log_mi_z, ref = 2011) + z_bare + ",
                              region_terms, " | dname + year"))

    for (mlabel in c("M1", "M2", "M3", "M4")) {
      f <- switch(mlabel, M1 = f_M1, M2 = f_M2, M3 = f_M3, M4 = f_M4)
      fit <- tryCatch(feols(f, data = panel, cluster = ~dname),
                      error = function(e) NULL)
      if (is.null(fit)) next
      s <- summary(fit)$coeftable
      if (!"z_inter" %in% rownames(s)) next
      b  <- s["z_inter", "Estimate"]
      se <- s["z_inter", "Std. Error"]
      tv <- s["z_inter", "t value"]
      pv <- s["z_inter", "Pr(>|t|)"]
      mean_y <- mean(model.frame(fit)$log_perm, na.rm = TRUE)
      results[[length(results) + 1]] <- tibble(
        version = ver, lag = L, model = mlabel,
        beta = round(b, 4), se = round(se, 4),
        t = round(tv, 2), p = round(pv, 4),
        sig = stars(pv),
        mean_y = round(mean_y, 4),
        pct_of_mean = round(100 * b / mean_y, 2),
        n = nobs(fit),
        r2 = round(fitstat(fit, "wr2", simplify = TRUE), 4)
      )
    }
  }
}

out <- bind_rows(results)

# ---- pretty print ----
cat(sprintf("\n%-4s %3s %-5s %-12s %8s %7s %9s %9s %5s %7s\n",
            "ver", "lag", "model", "beta(***)", "se", "t", "mean_y", "b/Y_%", "n", "r2"))
cat(strrep("-", 80), "\n", sep = "")
for (i in seq_len(nrow(out))) {
  r <- out[i, ]
  cat(sprintf("%-4s %3d %-5s %-12s %8.4f %7.2f %9.4f %8.2f%% %5d %7.4f\n",
              r$version, r$lag, r$model,
              sprintf("%.4f%s", r$beta, r$sig),
              r$se, r$t, r$mean_y, r$pct_of_mean, r$n, r$r2))
}

dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(out, "district-analysis/output/tab/dist_panel_ladder.csv")
cat(sprintf("\nSaved: district-analysis/output/tab/dist_panel_ladder.csv (%d rows)\n",
            nrow(out)))
