################################################################################
# DIAGNOSTIC: SSIV interaction term construction.
#
# Tests whether the persistent negative sign on z * mig_int comes from:
#   (a) The interaction multiplication itself
#   (b) Centering / scaling order
#   (c) Within-FE variation being too thin
#   (d) Confounding with controls
#
# For each panel (DOFE, census 2011-21, RVS, NEC if available):
#   1. Pairwise correlations of {y, z, mig, z*mig} (signs and magnitudes)
#   2. Reduced-form OLS:  y ~ z          (no interaction, FE)
#   3. Reduced-form OLS:  y ~ mig        (cross-section)
#   4. Bare interaction:  y ~ z*mig | FE
#   5. Four variants of the interaction term (all should give SAME sign,
#      different magnitudes -- if any flips, centering is the culprit):
#        (i)   z * mig                  (raw both)
#        (ii)  z_std * mig              (std z, raw mig)
#        (iii) z * mig_z                (raw z, std mig)
#        (iv)  z_std * mig_z            (current spec, both std)
#
# Outcome: console summary + output/tab/diag_interaction.csv
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

# ---- shared setup (copied from first-stage) ----
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
census   <- read_csv("district-analysis/data/clean/census/outcomes_district.csv",  show_col_types = FALSE)

# FX
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

# ============================================================================
# DIAGNOSTIC FUNCTIONS
# ============================================================================
stars <- function(p) ifelse(is.na(p), "",
                            ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*",""))))

run_one <- function(df, ycol, xcol, fe_str = NULL, cluster = "dname") {
  if (is.null(fe_str)) {
    f <- as.formula(sprintf("%s ~ %s", ycol, xcol))
    fit <- tryCatch(feols(f, data = df, vcov = "hetero"),
                    error = function(e) NULL)
  } else {
    f <- as.formula(sprintf("%s ~ %s | %s", ycol, xcol, fe_str))
    fit <- tryCatch(feols(f, data = df, cluster = ~dname),
                    error = function(e) NULL)
  }
  if (is.null(fit)) return(tibble(beta=NA,se=NA,p=NA,n=NA))
  s <- summary(fit)$coeftable
  if (!xcol %in% rownames(s)) return(tibble(beta=NA,se=NA,p=NA,n=nobs(fit)))
  b <- s[xcol,"Estimate"]; se <- s[xcol,"Std. Error"]; pv <- s[xcol,"Pr(>|t|)"]
  tibble(beta=round(b,4), se=round(se,4), p=round(pv,4),
         sig=stars(pv), n=nobs(fit))
}

diagnose_panel <- function(panel, ycol, panel_label, fe_str) {
  cat(sprintf("\n\n##################  %s  ##################\n", panel_label))
  cat(sprintf("outcome = %s, FE = %s\n", ycol, fe_str %||% "(none)"))

  # 1. Correlations
  cat("\n--- 1. Pairwise correlations (Pearson) ---\n")
  cm <- panel %>%
    select(all_of(c(ycol, "z_v2_L2", "z_v2_L2_std", "mig_per_1000",
                    "mig_per_1000_z", "inter_raw", "inter_std_x_std"))) %>%
    drop_na() %>% cor() %>% round(3)
  print(cm)

  # 2. Within-FE residual variance check: how much variation survives FE absorption?
  if (!is.null(fe_str)) {
    cat("\n--- 2. SD of inter_std_x_std before vs after FE absorption ---\n")
    f_dm <- as.formula(sprintf("inter_std_x_std ~ 1 | %s", fe_str))
    fit_dm <- tryCatch(feols(f_dm, data = panel),
                       error = function(e) NULL)
    if (!is.null(fit_dm)) {
      raw_sd <- sd(panel$inter_std_x_std, na.rm = TRUE)
      res_sd <- sd(residuals(fit_dm), na.rm = TRUE)
      cat(sprintf("  raw sd: %.4f   |   within-FE residual sd: %.4f   |   shrinkage: %.1f%%\n",
                  raw_sd, res_sd, 100 * (1 - res_sd/raw_sd)))
    }
  }

  # 3. Bare regressions
  cat("\n--- 3. Reduced-form regressions ---\n")
  cat(sprintf("%-30s | %-10s | %-7s | %-7s | %s\n", "spec", "beta", "SE", "p", "n"))
  cat(strrep("-", 80), "\n", sep = "")

  specs <- list(
    list(label = "y ~ z_std (no FE)",            x = "z_v2_L2_std", fe = NULL),
    list(label = "y ~ mig_per_1000_z (no FE)",   x = "mig_per_1000_z", fe = NULL),
    list(label = "y ~ z*mig (raw, no FE)",       x = "inter_raw",  fe = NULL),
    list(label = "y ~ z_std*mig_z (no FE)",      x = "inter_std_x_std", fe = NULL),
    list(label = "y ~ z_std (FE)",               x = "z_v2_L2_std", fe = fe_str),
    list(label = "y ~ z*mig raw (FE)",           x = "inter_raw", fe = fe_str),
    list(label = "y ~ z_std*mig raw (FE)",       x = "inter_std_x_mig", fe = fe_str),
    list(label = "y ~ z*mig_z (FE)",             x = "inter_z_x_std", fe = fe_str),
    list(label = "y ~ z_std*mig_z (FE) [CUR]",   x = "inter_std_x_std", fe = fe_str)
  )
  for (sp in specs) {
    r <- run_one(panel, ycol, sp$x, fe_str = sp$fe)
    cat(sprintf("%-30s | %9.4f%s | %7.4f | %7.4f | %d\n",
                sp$label, r$beta, ifelse(is.na(r$sig),"",r$sig),
                r$se, r$p, r$n))
  }

  # 4. If all four interaction variants flip sign vs raw, that points to centering
  cat("\n--- 4. Sign comparison: 4 interaction variants (all should agree if no bug) ---\n")
  for (sp in specs[3:9]) {
    if (grepl("\\bz", sp$label)) next # skip non-interaction
  }
  cat("  See rows above (FE=NULL block, last two columns):\n")
  cat("  (raw)        (std*std)\n")
  cat("  if signs match -> no bug; if they disagree -> centering interacting with controls\n")
}

# ============================================================================
# PANEL 1: DOFE district-year 2011-2023
# ============================================================================
districts <- sort(intersect(unique(z_v2$dname), mi$dname))
YRS <- 2011:2023
grid <- expand_grid(dname = districts, year = YRS)

perm_d <- dofe %>% filter(country %in% set_v2) %>%
  group_by(dname, year) %>% summarise(permits = sum(permits), .groups = "drop")

dofe_panel <- grid %>% left_join(perm_d, by = c("dname","year")) %>%
  replace_na(list(permits = 0)) %>%
  mutate(log_perm = log(permits + 1)) %>%
  inner_join(mi, by = "dname")

# Attach z at lag 2
z_lag2 <- z_v2 %>% mutate(year = year + 2) %>% rename(z_v2_L2 = z_v2)
dofe_panel <- dofe_panel %>% left_join(z_lag2, by = c("dname","year"))
dofe_panel$z_v2_L2_std <- with(dofe_panel,
  (z_v2_L2 - mean(z_v2_L2, na.rm = TRUE)) / sd(z_v2_L2, na.rm = TRUE))
dofe_panel <- dofe_panel %>%
  mutate(
    inter_raw         = z_v2_L2 * mig_per_1000,
    inter_std_x_mig   = z_v2_L2_std * mig_per_1000,
    inter_z_x_std     = z_v2_L2 * mig_per_1000_z,
    inter_std_x_std   = z_v2_L2_std * mig_per_1000_z
  )

diagnose_panel(dofe_panel, "log_perm", "DOFE 75d x 13y", "dname + year")

# ============================================================================
# PANEL 2: census 2011-2021 panel
# ============================================================================
census_panel <- census %>% filter(year %in% c(2011, 2021)) %>%
  inner_join(mi, by = "dname")
census_panel <- census_panel %>% left_join(z_lag2, by = c("dname","year"))
census_panel$z_v2_L2_std <- with(census_panel,
  (z_v2_L2 - mean(z_v2_L2, na.rm = TRUE)) / sd(z_v2_L2, na.rm = TRUE))
census_panel <- census_panel %>%
  mutate(
    inter_raw         = z_v2_L2 * mig_per_1000,
    inter_std_x_mig   = z_v2_L2_std * mig_per_1000,
    inter_z_x_std     = z_v2_L2 * mig_per_1000_z,
    inter_std_x_std   = z_v2_L2_std * mig_per_1000_z
  )

# Use absent_hh_share as the diagnostic outcome
diagnose_panel(census_panel, "absent_hh_share", "Census 75d x 2y", "dname + year")
diagnose_panel(census_panel, "amen_lighting_electricity", "Census amen_lighting", "dname + year")
diagnose_panel(census_panel, "ind_construction", "Census ind_construction", "dname + year")

cat("\n\nDone.\n")
