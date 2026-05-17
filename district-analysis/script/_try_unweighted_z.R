################################################################################
# TRY: unweighted-average shifter instead of share-weighted.
#
# z_d_alt = mean(rer_{c,t}) over c with share_dc > 0
#           (= simple average across the district's positive-share destinations,
#            ignoring share magnitudes)
#
# Compare first-stage M1-M4 vs the share-weighted version on log(permits+1).
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

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

forex    <- read_csv("district-analysis/data/clean/forex_2000_2023.csv", show_col_types = FALSE)
dofe_raw <- read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv", show_col_types = FALSE)
pop_file <- read_csv("district-analysis/data/clean/foreign_migration_district_population.csv", show_col_types = FALSE)
regions  <- read_csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv", show_col_types = FALSE)

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

dofe <- dofe_raw %>%
  filter(!is.na(country)) %>%
  mutate(dname = to_dname(district_rename)) %>%
  group_by(dname, country, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  filter(!country %in% c("Nepal", "India"))

v2_tot <- dofe %>% filter(year %in% c(2009, 2010)) %>%
  group_by(country) %>% summarise(tot = sum(permits), .groups = "drop")
set_v2 <- sort(intersect(v2_tot$country[v2_tot$tot > 0], unique(fx$country)))

# Identify positive-share destinations per district
pos_pairs <- dofe %>%
  filter(year %in% c(2009, 2010), country %in% set_v2) %>%
  group_by(dname, country) %>%
  summarise(permits = sum(permits), .groups = "drop") %>%
  filter(permits > 0) %>%
  select(dname, country)

# Share-weighted z (= current)
sh_v2 <- dofe %>%
  filter(year %in% c(2009, 2010), country %in% set_v2) %>%
  group_by(dname, country) %>%
  summarise(permits = sum(permits), .groups = "drop") %>%
  group_by(dname) %>%
  mutate(share = permits / sum(permits)) %>%
  ungroup() %>%
  select(dname, country, share)

z_share <- sh_v2 %>%
  inner_join(fx, by = "country", relationship = "many-to-many") %>%
  mutate(x = share * rer) %>%
  group_by(dname, year) %>%
  summarise(z_share = sum(x, na.rm = TRUE), .groups = "drop")

# Unweighted mean z_alt: mean(rer) across positive-share destinations
z_unwt <- pos_pairs %>%
  inner_join(fx, by = "country", relationship = "many-to-many") %>%
  group_by(dname, year) %>%
  summarise(z_unwt = mean(rer, na.rm = TRUE), .groups = "drop")

# Mig intensity (log)
mi <- dofe %>%
  filter(year %in% c(2009, 2010)) %>%
  group_by(dname) %>%
  summarise(num = sum(permits, na.rm = TRUE) / 2, .groups = "drop") %>%
  left_join(
    pop_file %>%
      mutate(dname = to_dname(district)) %>%
      select(dname, pop_2011 = district_population_2011) %>%
      distinct(dname, .keep_all = TRUE),
    by = "dname"
  ) %>%
  mutate(mig_per_1000 = (num / pop_2011) * 1000,
         log_mi       = log(pmax(mig_per_1000, 1e-6)),
         log_mi_z     = (log_mi - mean(log_mi)) / sd(log_mi)) %>%
  select(dname, log_mi_z)

# DOFE outcome panel
districts <- sort(intersect(unique(z_share$dname), mi$dname))
YRS <- 2011:2023
grid <- expand_grid(dname = districts, year = YRS)
perm_d <- dofe %>% filter(country %in% set_v2) %>%
  group_by(dname, year) %>% summarise(permits = sum(permits), .groups = "drop")

panel <- grid %>% left_join(perm_d, by = c("dname","year")) %>%
  replace_na(list(permits = 0)) %>%
  mutate(log_perm = log(permits + 1)) %>%
  inner_join(mi, by = "dname") %>%
  left_join(regions, by = "dname")

# Attach both z variants at lag 2
z_share_L2 <- z_share %>% mutate(year = year + 2) %>% rename(z_share_L2 = z_share)
z_unwt_L2  <- z_unwt  %>% mutate(year = year + 2) %>% rename(z_unwt_L2  = z_unwt)
panel <- panel %>%
  left_join(z_share_L2, by = c("dname","year")) %>%
  left_join(z_unwt_L2,  by = c("dname","year"))

# Standardize each
m_s <- mean(panel$z_share_L2, na.rm=TRUE); s_s <- sd(panel$z_share_L2, na.rm=TRUE)
m_u <- mean(panel$z_unwt_L2,  na.rm=TRUE); s_u <- sd(panel$z_unwt_L2,  na.rm=TRUE)
panel$z_share_L2_std <- (panel$z_share_L2 - m_s) / s_s
panel$z_unwt_L2_std  <- (panel$z_unwt_L2  - m_u) / s_u

# Show how the two z's compare
cat("\n--- z_share vs z_unwt (district means) ---\n")
print(panel %>% group_by(dname) %>% summarise(
  mean_z_share = mean(z_share_L2, na.rm=TRUE),
  mean_z_unwt  = mean(z_unwt_L2,  na.rm=TRUE)
) %>% head(10))

cat(sprintf("\nCorrelation z_share_L2 vs z_unwt_L2: %.3f\n",
            cor(panel$z_share_L2, panel$z_unwt_L2, use = "complete.obs")))

# ---- Ladder regressions ----
REGION_COLS <- c("share_e_asia","share_gulf","share_oecd_north",
                 "share_s_asia","share_se_asia","share_oecd_europe")

stars <- function(p) ifelse(is.na(p), "",
                            ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*",""))))

run_ladder_on <- function(z_std_col, label) {
  panel$z_inter <- panel[[z_std_col]] * panel$log_mi_z
  panel$z_bare  <- panel[[z_std_col]]
  region_terms <- paste(sprintf("i(year, %s, ref = 2011)", REGION_COLS), collapse = " + ")

  f_M1 <- as.formula("log_perm ~ z_inter | dname + year")
  f_M2 <- as.formula("log_perm ~ z_inter + i(year, log_mi_z, ref = 2011) | dname + year")
  f_M3 <- as.formula("log_perm ~ z_inter + i(year, log_mi_z, ref = 2011) + z_bare | dname + year")
  f_M4 <- as.formula(paste0("log_perm ~ z_inter + i(year, log_mi_z, ref = 2011) + z_bare + ",
                            region_terms, " | dname + year"))

  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("%-5s | %-12s | %-8s | %-7s | %s\n", "model","beta","SE","p","n"))
  cat(strrep("-", 60), "\n", sep = "")
  for (mlabel in c("M1","M2","M3","M4")) {
    f <- switch(mlabel, M1=f_M1, M2=f_M2, M3=f_M3, M4=f_M4)
    fit <- feols(f, data = panel, cluster = ~dname)
    s <- summary(fit)$coeftable
    b <- s["z_inter","Estimate"]; se <- s["z_inter","Std. Error"]
    pv <- s["z_inter","Pr(>|t|)"]
    cat(sprintf("%-5s | %.4f%-3s | %8.4f | %7.4f | %d\n",
                mlabel, b, stars(pv), se, pv, nobs(fit)))
  }
}

run_ladder_on("z_share_L2_std", "Share-weighted z (current)")
run_ladder_on("z_unwt_L2_std",  "Unweighted mean z (over positive shares)")
