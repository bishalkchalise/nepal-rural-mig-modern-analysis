################################################################################
#
# CANONICAL FIRST-STAGE -- STAGGERED LADDER ACROSS ALL OUTCOMES
# ------------------------------------------------------------------------------
# Treatment (locked):  z(fxshock) * z(log(mig_int))     [Order A, log transform]
# z-scoring on the working sample.  Cluster ~entity (dname or hhid).
#
# Spec ladder (six rungs, components added one at a time):
#   M1 : treatment                          | entity FE
#   M2 : + alpha * fx_z                     | entity FE
#   M3 : + alpha * fx_z                     | entity FE + year FE
#   M4 : + i(year, log_mi_z)                | entity FE + year FE      (C_mig)
#   M5 : + i(year, fx_z)                    | entity FE + year FE      (C_fx)
#   M6 : + i(year, share_X)   six regions   | entity FE + year FE      (C_X)
#
# Vintages:
#   V_2001 : fxshock (2001 census shares) + log(geog_intensity_2001)
#   V_DOFE : fxshock_dofe (2009-10 shares) + log(DOFE_0910 / pop_2011)
#
# Outcomes (panels):
#   DOFE permits district-year       log(permits + 1)
#   RVS district panel                log(n_intl_migrants + 1), share_hh_with_migrant, log(remit_intl + 1)
#   RVS HH-level (migrant-only)       log(n_intl_migrants + 1), log(remit_intl + 1)
#   RVS migrant-level (intl only)     log(remit + 1), remit_sent_flag, log(earn_primary + 1)
#
# Output: district-analysis/output/tab/first_stage_staggered.csv
#
# Run from repo root:
#     source("district-analysis/script/estimate/first_stage_staggered.R")
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

# ----------------------------------------------------------------------------
# 1. Load common inputs
# ----------------------------------------------------------------------------

inst       <- read.csv("district-analysis/data/clean/instrument/instrument_forex_dist.csv",
                       stringsAsFactors = FALSE)
inst_dofe  <- read.csv("district-analysis/data/clean/instrument/instrument_dofe_dist.csv",
                       stringsAsFactors = FALSE)
region_sh  <- read.csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv",
                       stringsAsFactors = FALSE)
dofe_raw   <- read.csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv",
                       stringsAsFactors = FALSE)
pop_file   <- read.csv("district-analysis/data/clean/foreign_migration_district_population.csv",
                       stringsAsFactors = FALSE)

dofe_to_census <- c("CHITWAN"="Chitawan","DHANUSHA"="Dhanusa","KAPILVASTU"="Kapilbastu",
                    "MAKAWANPUR"="Makwanpur","TANAHUN"="Tanahu","TEHRATHUM"="Terhathum",
                    "KABHREPALANCHOK"="Kavrepalanchok")

to_dname <- function(x) {
  u <- toupper(str_squish(x))
  ifelse(!is.na(dofe_to_census[u]),
         dofe_to_census[u],
         str_to_title(tolower(u)))
}

# DOFE district-year permits
dofe_dy <- dofe_raw %>%
  group_by(district_rename, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  mutate(dname = to_dname(district_rename)) %>%
  select(dname, year, permits)

# DOFE-vintage intensity (2009-10 mean / pop_2011)
mi_dofe <- dofe_dy %>%
  filter(year %in% 2009:2010) %>%
  group_by(dname) %>%
  summarise(num = mean(permits), .groups = "drop")

pop_2011 <- pop_file %>%
  mutate(dname = to_dname(district)) %>%
  distinct(dname, district_population_2011) %>%
  rename(pop_2011 = district_population_2011)

mi_dofe <- mi_dofe %>%
  left_join(pop_2011, by = "dname") %>%
  mutate(mig_int_dofe = num / pop_2011) %>%
  select(dname, mig_int_dofe)

# Helper to safely load
try_load <- function(p) if (file.exists(p)) read.csv(p, stringsAsFactors = FALSE) else NULL

rvs_dist <- try_load("district-analysis/data/clean/rvs/migration_district_year.csv")
rvs_hh   <- try_load("district-analysis/data/clean/rvs/migration_hh_year.csv")
rvs_mig  <- try_load("district-analysis/data/clean/rvs/migration_migrant_year.csv")

if (!is.null(rvs_dist)) {
  rvs_dist <- rvs_dist %>%
    mutate(dname = to_dname(dname_raw),
           log_n_intl = log(n_intl_migrants + 1),
           log_remit  = log(remit_amount_intl_12m_rs + 1),
           share_hh   = n_hh_with_intl_migrant / pmax(n_hh, 1))
}

add_dname_rvs <- function(df) {
  if (is.null(df)) return(NULL)
  k <- intersect(c("district_name","district77","district"), names(df))[1]
  if (is.na(k)) return(df)
  df %>% mutate(dname = to_dname(.data[[k]]))
}
rvs_hh  <- add_dname_rvs(rvs_hh)
rvs_mig <- add_dname_rvs(rvs_mig)

# ----------------------------------------------------------------------------
# 2. Helpers
# ----------------------------------------------------------------------------

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

REGION_COLS <- c("share_e_asia","share_gulf","share_oecd_north",
                 "share_oecd_europe","share_s_asia","share_se_asia")

# ----------------------------------------------------------------------------
# 3. The runner
# ----------------------------------------------------------------------------

fit_stage <- function(panel, outcome, vintage, level, ref_year,
                      entity_col = "dname") {
  d <- panel
  if (vintage == "V_2001") {
    d$fx_raw <- d$fxshock
    d$mi_raw <- d$geog_intensity_2001
  } else {
    d$fx_raw <- d$fxshock_dofe
    d$mi_raw <- d$mig_int_dofe
  }
  d <- d[!is.na(d[[outcome]]) & !is.na(d$fx_raw) & !is.na(d$mi_raw), ]
  if (nrow(d) < 30) return(NULL)

  d$fx_z     <- zscore(d$fx_raw)
  d$log_mi_z <- zscore(log(pmax(d$mi_raw, 1e-12)))
  d$treatment <- d$fx_z * d$log_mi_z

  rhs <- "treatment"
  if (level >= 2) rhs <- c(rhs, "fx_z")
  if (level >= 4) rhs <- c(rhs, "i(year, log_mi_z)")
  if (level >= 5) rhs <- c(rhs, "i(year, fx_z)")
  if (level >= 6) {
    for (c in REGION_COLS) if (c %in% names(d)) rhs <- c(rhs, sprintf("i(year, %s)", c))
  }

  fe <- if (level >= 3) sprintf("%s + year", entity_col) else entity_col
  fml <- as.formula(sprintf("%s ~ %s | %s", outcome, paste(rhs, collapse = " + "), fe))
  m <- tryCatch(feols(fml, data = d, cluster = as.formula(paste0("~", entity_col)),
                      warn = FALSE, notes = FALSE),
                error = function(e) e)
  if (inherits(m, "error")) return(list(err = substr(conditionMessage(m), 1, 100)))
  ct <- as.data.frame(summary(m)$coeftable)
  if (!"treatment" %in% rownames(ct)) return(list(err = "treatment absorbed"))
  list(beta = ct["treatment","Estimate"],
       se   = ct["treatment","Std. Error"],
       t    = ct["treatment","t value"],
       p    = ct["treatment","Pr(>|t|)"],
       n    = nobs(m),
       r2_w = fitstat(m, "wr2", simplify = TRUE))
}

stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
}

SPEC_LABELS <- c("1" = "M1 treat | dname FE",
                 "2" = "M2 +fx_z main",
                 "3" = "M3 +year FE",
                 "4" = "M4 +C_mig",
                 "5" = "M5 +C_fx",
                 "6" = "M6 +C_X")

# ----------------------------------------------------------------------------
# 4. Build outcome-specific panels
# ----------------------------------------------------------------------------

mk_panel <- function(left, by_year_too) {
  out <- left %>%
    inner_join(inst, by = c("dname","year")) %>%
    inner_join(inst_dofe %>% select(dname, year, fxshock_dofe),
               by = c("dname","year")) %>%
    left_join(mi_dofe, by = "dname") %>%
    left_join(region_sh, by = "dname")
  out
}

dofe_panel <- dofe_dy %>%
  mutate(log_permits = log(permits + 1)) %>%
  mk_panel(by_year_too = TRUE)

OUTS <- list(
  list(name = "DOFE_permits", outcome = "log_permits", panel = dofe_panel,
       ref = 2016, entity = "dname")
)

if (!is.null(rvs_dist)) {
  rvs_d_panel <- mk_panel(rvs_dist, TRUE)
  OUTS <- c(OUTS, list(
    list(name="RVS_d_n_intl", outcome="log_n_intl", panel=rvs_d_panel, ref=2017, entity="dname"),
    list(name="RVS_d_share",  outcome="share_hh",   panel=rvs_d_panel, ref=2017, entity="dname"),
    list(name="RVS_d_remit",  outcome="log_remit",  panel=rvs_d_panel, ref=2017, entity="dname")
  ))
}

if (!is.null(rvs_hh) && "has_migrant_intl" %in% names(rvs_hh)) {
  rvs_hh_panel <- mk_panel(rvs_hh %>% filter(has_migrant_intl == 1), TRUE) %>%
    mutate(log_n_intl = log(n_intl_migrants + 1),
           log_remit  = log(remit_amount_intl_12m_rs + 1))
  OUTS <- c(OUTS, list(
    list(name="RVS_hh_n_intl", outcome="log_n_intl", panel=rvs_hh_panel, ref=2017, entity="hhid"),
    list(name="RVS_hh_remit",  outcome="log_remit",  panel=rvs_hh_panel, ref=2017, entity="hhid")
  ))
}

if (!is.null(rvs_mig) && "is_international" %in% names(rvs_mig)) {
  rvs_mig_panel <- mk_panel(rvs_mig %>% filter(is_international == 1), TRUE) %>%
    mutate(log_remit        = log(coalesce(remit_amount_rs, 0) + 1),
           log_earn_primary = log(coalesce(earning_primary_rs, 0) + 1))
  OUTS <- c(OUTS, list(
    list(name="RVS_mig_remit", outcome="log_remit",        panel=rvs_mig_panel, ref=2017, entity="hhid"),
    list(name="RVS_mig_sent",  outcome="remit_sent_flag",  panel=rvs_mig_panel, ref=2017, entity="hhid"),
    list(name="RVS_mig_earn",  outcome="log_earn_primary", panel=rvs_mig_panel, ref=2017, entity="hhid")
  ))
}

cat(sprintf("Outcomes to run: %d\n", length(OUTS)))
for (o in OUTS) cat("  -", o$name, "\n")

# ----------------------------------------------------------------------------
# 5. Loop over (outcome, vintage, spec level)
# ----------------------------------------------------------------------------

rows <- list(); i <- 0L
for (o in OUTS) {
  for (vintage in c("V_2001","V_DOFE")) {
    for (lvl in 1:6) {
      r <- fit_stage(o$panel, o$outcome, vintage, lvl, o$ref, entity_col = o$entity)
      i <- i + 1L
      rows[[i]] <- tibble(
        outcome = o$name, y = o$outcome, vintage = vintage,
        spec    = SPEC_LABELS[as.character(lvl)],
        beta    = if (is.list(r) && is.null(r$err)) r$beta  else NA_real_,
        se      = if (is.list(r) && is.null(r$err)) r$se    else NA_real_,
        t_stat  = if (is.list(r) && is.null(r$err)) r$t     else NA_real_,
        p_val   = if (is.list(r) && is.null(r$err)) r$p     else NA_real_,
        n_obs   = if (is.list(r) && is.null(r$err)) r$n     else NA_integer_,
        r2_w    = if (is.list(r) && is.null(r$err)) r$r2_w  else NA_real_,
        note    = if (is.list(r) && !is.null(r$err)) r$err  else ""
      )
    }
  }
}

results <- bind_rows(rows) %>%
  mutate(sig = sapply(p_val, stars))

dir.create("district-analysis/output/tab",
           recursive = TRUE, showWarnings = FALSE)
write.csv(results,
          "district-analysis/output/tab/first_stage_staggered.csv",
          row.names = FALSE)
cat(sprintf("Saved: output/tab/first_stage_staggered.csv (%d rows)\n",
            nrow(results)))

# Console preview per outcome
for (o in OUTS) {
  sub <- results %>% filter(outcome == o$name)
  cat(sprintf("\n=== %s (%s) ===\n", o$name, o$outcome))
  pv <- sub %>%
    select(spec, vintage, beta, t_stat, sig, n_obs) %>%
    mutate(across(c(beta, t_stat), ~ round(., 4))) %>%
    pivot_wider(names_from = vintage,
                values_from = c(beta, t_stat, sig, n_obs))
  print(as.data.frame(pv))
}
