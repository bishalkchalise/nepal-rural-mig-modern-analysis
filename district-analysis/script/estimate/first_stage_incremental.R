################################################################################
#
# INCREMENTAL FIRST STAGE  (matches portal regression structure)
# ------------------------------------------------------------------------------
# Four RHS components, added one at a time, to see how the headline beta
# behaves as the portal's controls come in. All variables z-scored on the
# working sample before interactions are built.
#
#   C1  treatment only :  fx_z * mi_z
#   C2  + c_mig        :  + mi_z * year_dummies
#   C3  + c_fx         :  + fx_z * year_dummies
#   C4  + region X     :  + (six 2001 destination-region shares) * year_dummies
#
# Always-on:
#   - main alpha * fx_z term (so the constant absorption identity in our
#     log/log discussion holds)
#   - dname FE + year FE
#   - cluster ~dname
#
# Two share types compared:
#   2001 share : fxshock      column from instrument_forex_dist.csv
#   DOFE share : fxshock_dofe column from instrument_dofe_dist.csv (2009-2010 avg)
#
# Four outcomes:
#   DOFE_permits      : log(permits + 1)
#   RVS_n_intl        : log(n_intl_migrants + 1)
#   RVS_share_hh      : n_hh_with_intl_migrant / n_hh
#   RVS_remit_intl    : log(remit_amount_intl_12m_rs + 1)
#
# Output: district-analysis/output/tab/first_stage_incremental.csv
#
# Run from repo root:
#     source("district-analysis/script/estimate/first_stage_incremental.R")
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

# ------------------------------------------------------------------------------
# 1. Load inputs
# ------------------------------------------------------------------------------

inst       <- read.csv("district-analysis/data/clean/instrument/instrument_forex_dist.csv",
                       stringsAsFactors = FALSE)
inst_dofe  <- read.csv("district-analysis/data/clean/instrument/instrument_dofe_dist.csv",
                       stringsAsFactors = FALSE)
region_sh  <- read.csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv",
                       stringsAsFactors = FALSE)

# DOFE district-year permits
dofe_to_census <- c("CHITWAN"="Chitawan","DHANUSHA"="Dhanusa","KAPILVASTU"="Kapilbastu",
                    "MAKAWANPUR"="Makwanpur","TANAHUN"="Tanahu","TEHRATHUM"="Terhathum",
                    "KABHREPALANCHOK"="Kavrepalanchok")
dofe_raw <- read.csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv",
                     stringsAsFactors = FALSE)
dofe_panel <- dofe_raw %>%
  group_by(district_rename, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  mutate(dname = ifelse(!is.na(dofe_to_census[district_rename]),
                        dofe_to_census[district_rename],
                        str_to_title(tolower(district_rename)))) %>%
  select(dname, year, permits)

# RVS district panel (if present)
rvs_path <- "district-analysis/data/clean/rvs/migration_district_year.csv"
have_rvs <- file.exists(rvs_path)
if (have_rvs) {
  rvs <- read.csv(rvs_path, stringsAsFactors = FALSE) %>%
    mutate(upr = toupper(str_squish(dname_raw)),
           dname = ifelse(!is.na(dofe_to_census[upr]),
                          dofe_to_census[upr],
                          str_to_title(tolower(upr)))) %>%
    select(-upr)
}

# ------------------------------------------------------------------------------
# 2. Build the analysis panels (each outcome has its own panel)
# ------------------------------------------------------------------------------

# DOFE 2009-2010 avg migrants per district (numerator for the DOFE-vintage intensity)
mi_dofe <- dofe_panel %>%
  filter(year %in% 2009:2010) %>%
  group_by(dname) %>%
  summarise(dofe_mig_0910 = mean(permits), .groups = "drop")

# Use pop_2011 if available (contemporaneous denominator); else fall back to pop_2001
pop_2011_path <- "district-analysis/data/clean/instrument/pop_2011_dist.csv"
if (file.exists(pop_2011_path)) {
  pop_use <- read.csv(pop_2011_path, stringsAsFactors = FALSE)
  pop_use$pop_denom <- pop_use$pop_2011
  pop_label <- "pop_2011"
} else {
  pop_use <- distinct(inst[, c("dname", "geog_pop_2001")])
  pop_use$pop_denom <- pop_use$geog_pop_2001
  pop_label <- "pop_2001 (fallback - run _build_pop_2011.R for 2011 denom)"
}
cat(sprintf("Using denominator: %s\n", pop_label))

mi_dofe_full <- mi_dofe %>%
  left_join(pop_use %>% select(dname, pop_denom), by = "dname") %>%
  mutate(mig_int_dofe = dofe_mig_0910 / pop_denom) %>%
  select(dname, mig_int_dofe)

dofe_full <- dofe_panel %>%
  inner_join(inst,       by = c("dname", "year")) %>%
  inner_join(inst_dofe %>% select(dname, year, fxshock_dofe),
             by = c("dname", "year")) %>%
  left_join(mi_dofe_full, by = "dname") %>%
  left_join(region_sh,   by = "dname") %>%
  mutate(log_permits = log(permits + 1))

if (have_rvs) {
  rvs_full <- rvs %>%
    inner_join(inst,       by = c("dname", "year")) %>%
    inner_join(inst_dofe %>% select(dname, year, fxshock_dofe),
               by = c("dname", "year")) %>%
    left_join(region_sh,   by = "dname") %>%
    mutate(log_n_intl = log(n_intl_migrants + 1),
           log_remit  = log(remit_amount_intl_12m_rs + 1),
           share_hh   = n_hh_with_intl_migrant / pmax(n_hh, 1))
}

# ------------------------------------------------------------------------------
# 3. z-scoring helper
# ------------------------------------------------------------------------------

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# ------------------------------------------------------------------------------
# 4. Fit one cell at a given incremental level
# ------------------------------------------------------------------------------

fit_inc <- function(panel_df, outcome, share_label, level,
                    mi_vintage = c("matched", "2001")) {

  mi_vintage <- match.arg(mi_vintage)
  df <- panel_df
  df$fx_raw <- if (share_label == "2001") df$fxshock else df$fxshock_dofe

  # Pick the intensity vintage to match the share vintage
  if (mi_vintage == "matched") {
    if (share_label == "2001") {
      df$mi_raw <- df$geog_intensity_2001
    } else if ("mig_int_dofe" %in% names(df)) {
      df$mi_raw <- df$mig_int_dofe
    } else {
      df$mi_raw <- df$geog_intensity_2001       # fallback
    }
  } else {
    df$mi_raw <- df$geog_intensity_2001
  }

  # z-score on the working sample
  df$fx_z     <- zscore(df$fx_raw)
  df$mi_z     <- zscore(df$mi_raw)

  # Treatment = fx_z * mi_z (linear interaction; switch to log_mi_z if you prefer)
  df$treatment <- df$fx_z * df$mi_z

  # Always include fx_z as a main term (matches portal's alpha*fxshock)
  rhs <- c("treatment", "fx_z")

  if (level >= 2) {
    rhs <- c(rhs, "i(year, mi_z)")          # c_mig
  }
  if (level >= 3) {
    rhs <- c(rhs, "i(year, fx_z)")          # c_fx
  }
  if (level >= 4) {
    for (c in c("share_e_asia", "share_gulf", "share_oecd_north",
                "share_oecd_europe", "share_s_asia", "share_se_asia")) {
      if (c %in% names(df)) rhs <- c(rhs, sprintf("i(year, %s)", c))
    }
  }

  fml <- as.formula(paste0(outcome, " ~ ", paste(rhs, collapse = " + "),
                           " | dname + year"))
  m <- tryCatch(feols(fml, data = df, cluster = ~dname,
                      warn = FALSE, notes = FALSE),
                error = function(e) e)
  if (inherits(m, "error")) return(list(err = substr(conditionMessage(m), 1, 100)))
  ct <- as.data.frame(summary(m)$coeftable)
  if (!"treatment" %in% rownames(ct)) return(list(err = "treatment absorbed"))
  list(
    beta = ct["treatment", "Estimate"],
    se   = ct["treatment", "Std. Error"],
    t    = ct["treatment", "t value"],
    p    = ct["treatment", "Pr(>|t|)"],
    n    = nobs(m),
    r2_w = fitstat(m, "wr2", simplify = TRUE)
  )
}

# ------------------------------------------------------------------------------
# 5. Run the grid
# ------------------------------------------------------------------------------

LEVEL_LABELS <- c("1" = "C1 treatment only",
                  "2" = "C2 + c_mig",
                  "3" = "C3 + c_fx",
                  "4" = "C4 + region X")

datasets <- list(
  list(name = "DOFE_permits", outcome = "log_permits", df = dofe_full)
)
if (have_rvs) {
  datasets <- c(datasets, list(
    list(name = "RVS_n_intl",    outcome = "log_n_intl", df = rvs_full),
    list(name = "RVS_share_hh",  outcome = "share_hh",   df = rvs_full),
    list(name = "RVS_remit_intl",outcome = "log_remit",  df = rvs_full)
  ))
}

rows <- list(); i <- 0L
for (d in datasets) {
  for (sh in c("2001", "DOFE")) {
    for (lvl in 1:4) {
      r <- fit_inc(d$df, d$outcome, sh, lvl)
      i <- i + 1L
      rows[[i]] <- tibble(
        dataset    = d$name,
        outcome    = d$outcome,
        share      = sh,
        spec       = LEVEL_LABELS[as.character(lvl)],
        beta       = if (is.list(r) && is.null(r$err)) r$beta else NA_real_,
        se         = if (is.list(r) && is.null(r$err)) r$se   else NA_real_,
        t_stat     = if (is.list(r) && is.null(r$err)) r$t    else NA_real_,
        p_val      = if (is.list(r) && is.null(r$err)) r$p    else NA_real_,
        n_obs      = if (is.list(r) && is.null(r$err)) r$n    else NA_integer_,
        r2_within  = if (is.list(r) && is.null(r$err)) r$r2_w else NA_real_,
        note       = if (is.list(r) && !is.null(r$err)) r$err else ""
      )
    }
  }
}

results <- bind_rows(rows) %>%
  mutate(sig = case_when(
    is.na(p_val) ~ "",
    p_val < 0.01 ~ "***",
    p_val < 0.05 ~ "**",
    p_val < 0.1  ~ "*",
    TRUE         ~ ""
  ))

dir.create("district-analysis/output/tab",
           recursive = TRUE, showWarnings = FALSE)
write.csv(results,
          "district-analysis/output/tab/first_stage_incremental.csv",
          row.names = FALSE)

cat("Saved: district-analysis/output/tab/first_stage_incremental.csv\n")
cat(sprintf("Rows: %d (%d datasets x 2 shares x 4 specs)\n",
            nrow(results), length(datasets)))

# Console preview
for (d in datasets) {
  cat(sprintf("\n=== %s (%s) ===\n", d$name, d$outcome))
  print(results %>%
          filter(dataset == d$name) %>%
          select(spec, share, beta, t_stat, sig, n_obs) %>%
          mutate(across(c(beta, t_stat), ~ round(., 4))) %>%
          pivot_wider(names_from = share,
                      values_from = c(beta, t_stat, sig, n_obs)) %>%
          as.data.frame())
}
