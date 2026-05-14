################################################################################
#
# M4-M6 FIRST-STAGE WITH WILD CLUSTER BOOTSTRAP, ALL OUTCOMES
# ------------------------------------------------------------------------------
# For each first-stage outcome runs M4 / M5 / M6 specs with:
#   - cluster-robust SE (canonical)
#   - wild cluster bootstrap SE (Rademacher signs at entity level, B reps)
# Reports mean_y in the output table for every estimate.
#
# Spec recall (entity FE + year FE; cluster ~entity):
#   M4 : treatment + alpha*fx_z + i(year, log_mi_z)                      | C_mig
#   M5 : + i(year, fx_z)                                                  | + C_fx
#   M6 : + i(year, share_X)   six 2001 destination-region shares          | + C_X
#
# Treatment: z(fxshock) * z(log(mig_int))  using DOFE 2009-10 share + DOFE
#            2009-10 / pop_2011 intensity. fxshock is NPR per LCU after the
#            instrument.R direction flip -- positive beta = "more
#            destination-currency appreciation vs NPR -> more y" (intuitive).
#
# Outcomes:
#   - DOFE permits district-year       log(permits + 1)
#   - RVS district-year                 log_n_intl, share_hh, log_remit_intl
#   - RVS HH-level (migrant-only)       log_n_intl, log_remit_intl
#   - RVS migrant-level (intl)          log_remit, remit_sent_flag, log_earn
#
# Output: district-analysis/output/tab/first_stage_M4M6_boot.csv
#
# Run from repo root:
#     source("district-analysis/script/estimate/first_stage_M4M6_boot.R")
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

set.seed(2026)
B_BOOT <- 200       # wild bootstrap reps -- raise to 500 if you can wait

# ----------------------------------------------------------------------------
# 1. Load
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
  ifelse(!is.na(dofe_to_census[u]), dofe_to_census[u], str_to_title(tolower(u)))
}

# DOFE district-year permits + DOFE-vintage intensity
dofe_dy <- dofe_raw %>%
  group_by(district_rename, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  mutate(dname = to_dname(district_rename)) %>%
  select(dname, year, permits)

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

# RVS panels (auto-skip if missing locally)
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

build_rhs <- function(level) {
  rhs <- c("treatment","fx_z")
  if (level >= 4) rhs <- c(rhs, "i(year, log_mi_z)")
  if (level >= 5) rhs <- c(rhs, "i(year, fx_z)")
  if (level >= 6) for (c in REGION_COLS) rhs <- c(rhs, sprintf("i(year, %s)", c))
  rhs
}

fit_one <- function(df, outcome, level, entity_col) {
  rhs <- paste(build_rhs(level), collapse = " + ")
  fml <- as.formula(sprintf("%s ~ %s | %s + year", outcome, rhs, entity_col))
  feols(fml, data = df, cluster = as.formula(paste0("~", entity_col)),
        warn = FALSE, notes = FALSE)
}

# Wild cluster bootstrap on residuals -------------------------------------
wild_boot <- function(df, outcome, level, entity_col, B = B_BOOT) {
  fit <- tryCatch(fit_one(df, outcome, level, entity_col), error = function(e) e)
  if (inherits(fit, "error")) return(NULL)
  ct <- as.data.frame(summary(fit)$coeftable)
  if (!"treatment" %in% rownames(ct)) return(NULL)
  bh  <- ct["treatment","Estimate"]
  seh <- ct["treatment","Std. Error"]
  pcl <- ct["treatment","Pr(>|t|)"]

  # residuals + fitted
  fitted_v <- fit$fitted.values
  resid_v  <- residuals(fit)
  d <- df
  d$.yhat <- fitted_v
  d$.e    <- resid_v
  ents    <- unique(d[[entity_col]])

  bs <- numeric(B)
  for (b in seq_len(B)) {
    g <- setNames(sample(c(-1, 1), size = length(ents), replace = TRUE), ents)
    d$.g <- g[as.character(d[[entity_col]])]
    d$.y_star <- d$.yhat + d$.g * d$.e
    fbo <- as.formula(sprintf(".y_star ~ %s | %s + year",
                              paste(build_rhs(level), collapse = " + "),
                              entity_col))
    rb <- tryCatch(feols(fbo, data = d, cluster = as.formula(paste0("~", entity_col)),
                         warn = FALSE, notes = FALSE),
                   error = function(e) NULL)
    if (is.null(rb)) { bs[b] <- NA_real_; next }
    cb <- coef(rb)
    bs[b] <- if ("treatment" %in% names(cb)) cb["treatment"] else NA_real_
  }
  bs <- bs[!is.na(bs)]
  centered <- bs - bh
  p_wild <- mean(abs(centered) >= abs(bh))
  se_wild <- sd(bs)
  list(beta = bh, se_cluster = seh, p_cluster = pcl,
       se_wild = se_wild, p_wild = p_wild,
       n = nobs(fit),
       mean_y = mean(df[[outcome]], na.rm = TRUE),
       B_used = length(bs))
}

stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
}

# ----------------------------------------------------------------------------
# 3. Per-outcome runner
# ----------------------------------------------------------------------------

mk_panel <- function(left) {
  left %>%
    inner_join(inst,                                          by = c("dname","year")) %>%
    inner_join(inst_dofe %>% select(dname, year, fxshock_dofe), by = c("dname","year")) %>%
    left_join(mi_dofe,    by = "dname") %>%
    left_join(region_sh,  by = "dname")
}
add_treatment <- function(df) {
  df %>%
    mutate(fx_raw    = fxshock_dofe,
           mi_raw    = mig_int_dofe,
           fx_z      = zscore(fx_raw),
           log_mi_z  = zscore(log(pmax(mi_raw, 1e-12))),
           treatment = fx_z * log_mi_z)
}

run_outcome <- function(name, outcome, panel_in, entity_col) {
  panel_z <- panel_in %>% add_treatment()
  panel_z <- panel_z[!is.na(panel_z[[outcome]]) & !is.na(panel_z$treatment), ]
  out <- list()
  for (lvl in 4:6) {
    cat(sprintf("  %s  %s  M%d ...\n", name, outcome, lvl))
    r <- wild_boot(panel_z, outcome, lvl, entity_col, B = B_BOOT)
    if (is.null(r)) next
    out[[length(out)+1]] <- tibble(
      outcome = name, y = outcome, spec = sprintf("M%d", lvl),
      beta = round(r$beta, 4),
      se_cluster = round(r$se_cluster, 4),
      t_cluster  = round(r$beta / r$se_cluster, 2),
      p_cluster  = round(r$p_cluster, 4),
      sig_cluster= stars(r$p_cluster),
      se_wild    = round(r$se_wild, 4),
      t_wild     = round(r$beta / r$se_wild, 2),
      p_wild     = round(r$p_wild, 4),
      sig_wild   = stars(r$p_wild),
      mean_y     = round(r$mean_y, 4),
      n_obs      = r$n,
      B_boot     = r$B_used
    )
  }
  bind_rows(out)
}

# ----------------------------------------------------------------------------
# 4. Build outcome list and run
# ----------------------------------------------------------------------------

dofe_panel <- mk_panel(dofe_dy %>% mutate(log_permits = log(permits + 1)))
results <- list()
results[["DOFE_permits"]] <- run_outcome("DOFE_permits", "log_permits",
                                         dofe_panel, "dname")

if (!is.null(rvs_dist)) {
  rvs_d_panel <- mk_panel(rvs_dist)
  for (oc in c("log_n_intl","share_hh","log_remit")) {
    nm <- paste0("RVS_d_", oc)
    results[[nm]] <- run_outcome(nm, oc, rvs_d_panel, "dname")
  }
}
if (!is.null(rvs_hh) && "has_migrant_intl" %in% names(rvs_hh)) {
  rvs_hh_panel <- mk_panel(rvs_hh %>% filter(has_migrant_intl == 1)) %>%
    mutate(log_n_intl = log(n_intl_migrants + 1),
           log_remit  = log(remit_amount_intl_12m_rs + 1))
  for (oc in c("log_n_intl","log_remit")) {
    nm <- paste0("RVS_hh_", oc)
    results[[nm]] <- run_outcome(nm, oc, rvs_hh_panel, "hhid")
  }
}
if (!is.null(rvs_mig) && "is_international" %in% names(rvs_mig)) {
  rvs_mig_panel <- mk_panel(rvs_mig %>% filter(is_international == 1)) %>%
    mutate(log_remit        = log(coalesce(remit_amount_rs, 0) + 1),
           log_earn_primary = log(coalesce(earning_primary_rs, 0) + 1))
  for (oc in c("log_remit","remit_sent_flag","log_earn_primary")) {
    nm <- paste0("RVS_mig_", oc)
    results[[nm]] <- run_outcome(nm, oc, rvs_mig_panel, "hhid")
  }
}

results_df <- bind_rows(results)

dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write.csv(results_df,
          "district-analysis/output/tab/first_stage_M4M6_boot.csv",
          row.names = FALSE)
cat(sprintf("\nSaved: output/tab/first_stage_M4M6_boot.csv (%d rows)\n",
            nrow(results_df)))

# Console preview per outcome
for (nm in unique(results_df$outcome)) {
  cat(sprintf("\n=== %s ===\n", nm))
  print(results_df %>%
          filter(outcome == nm) %>%
          select(spec, beta, se_cluster, t_cluster, p_cluster, sig_cluster,
                 se_wild, t_wild, p_wild, sig_wild, mean_y, n_obs) %>%
          as.data.frame())
}
