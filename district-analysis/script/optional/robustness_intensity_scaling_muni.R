################################################################################
#
# MUNI ROBUSTNESS: per-thousand scaling within the published S1/S2/S3 grid
# ------------------------------------------------------------------------------
# Mirrors the methods documented on the published portal:
#
#   S1 : y = alpha*fxshock + beta*(fxshock x mig_intensity)
#                          + alpha_i + gamma_t + eps                 [no controls]
#   S2 : y = alpha*fxshock + beta*(fxshock x LOG(mig_intensity))
#                          + lambda*(mig_intensity x tau_t)
#                          + alpha_i + gamma_t + eps                 [main spec]
#   S3 : y = alpha*fxshock + beta*(fxshock x mig_intensity)
#                          + lambda*(LOG(mig_intensity) x tau_t)
#                          + alpha_i + gamma_t + eps
#
# Plus:
#   - Sample threshold k in {0, 25, 50, 100} total migrants in 2001
#   - Lags: 0, 1, 2
#   - Each treatment term z-scored on the working sample
#   - FE: lgcode + year, cluster ~lgcode
#
# Robustness dimension added: the scaling of mig_intensity in BOTH the
# main interaction (S2) and the year-trend control (S2, S3):
#
#   "log(mi)"             portal default for S2 main interaction
#   "log(1000 * mi)"      per-thousand log
#   "log(100000 * mi)"    per-100k log (uniformly positive)
#   "asinh(mi)"           old build_results.R helper
#
# In S2 the math says swapping log(mi) -> log(1000*mi) is absorbed by the
# alpha*fxshock term, so beta should be unchanged. This script empirically
# verifies that.
#
# Output:
#   data/clean/robustness_intensity_scaling_muni.csv
#
# Run from the muni-era project root (NOT under district-analysis/).
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

# ------------------------------------------------------------------------------
# 1. Config (matches portal)
# ------------------------------------------------------------------------------

REF_YEAR     <- 2001
PANEL_YEARS  <- c(2001, 2011, 2021)   # drop 2001 below since SSIV=1 there
THRESHOLDS   <- c(0, 25, 50, 100)
LAGS         <- c(0L, 1L, 2L)

SPECS <- c("S1", "S2", "S3")

# How to build the LOG transform of mig_intensity. The portal uses log(mi);
# the robustness sweeps four variants.
LOG_VARIANTS <- list(
  "log(mi)"        = function(x) log(pmax(x,         1e-12)),
  "log(1000*mi)"   = function(x) log(pmax(x * 1e3,   1e-12)),
  "log(100000*mi)" = function(x) log(pmax(x * 1e5,   1e-12)),
  "asinh(mi)"      = function(x) asinh(x)
)

OUTCOMES <- c(
  "amen_water_piped", "amen_cooking_modern", "amen_lighting_electricity",
  "amen_toilet_modern", "amen_assets_mobile", "amen_assets_motorcycle",
  "housing_own", "housing_foundation_modern",
  "mig_in_share", "mig_in_international",
  "ent_has_nonagro", "head_female_share", "flfp_all"
)

# ------------------------------------------------------------------------------
# 2. Load
# ------------------------------------------------------------------------------

instrument <- read.csv(
  "data/clean/instrument/instrument_mun.csv",
  stringsAsFactors = FALSE
)
outcomes_df <- read.csv(
  "data/clean/census/census_outcomes_municipality.csv",
  stringsAsFactors = FALSE
)
OUTCOMES <- intersect(OUTCOMES, names(outcomes_df))
cat(sprintf("Outcomes available: %d\n", length(OUTCOMES)))

# ------------------------------------------------------------------------------
# 3. Helpers
# ------------------------------------------------------------------------------

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

build_panel <- function(lag_yrs, k_thresh, log_fn) {
  # Lag the instrument
  inst_lag <- instrument %>%
    mutate(year = year + lag_yrs)

  p <- outcomes_df %>%
    inner_join(inst_lag, by = c("lgcode", "year")) %>%
    filter(year %in% PANEL_YEARS, year != 2001)

  # Threshold: drop munis with fewer than k total 2001 migrants
  p$geog_total_mig_2001 <- replace_na(p$geog_total_mig_2001, 0)
  if (k_thresh > 0) {
    p <- p %>% filter(geog_total_mig_2001 >= k_thresh)
  }
  if (nrow(p) == 0) return(NULL)

  p$geog_intensity_2001 <- replace_na(p$geog_intensity_2001, 0)
  p$log_mi              <- log_fn(p$geog_intensity_2001)

  # z-score the building blocks on the working sample
  p$fxshock_z    <- zscore(p$fxshock)
  p$mi_z         <- zscore(p$geog_intensity_2001)
  p$log_mi_z     <- zscore(p$log_mi)

  # Interaction terms
  p$inter_lin    <- p$fxshock_z * p$mi_z
  p$inter_log    <- p$fxshock_z * p$log_mi_z
  # Don't re-z-score the products themselves -- the portal z-scores the
  # building blocks before estimation, not the interaction.

  # year × control terms
  other_years <- setdiff(unique(p$year), REF_YEAR)
  for (y in other_years) {
    ind <- as.integer(p$year == y)
    p[[paste0("mi_z_x_",      y)]] <- p$mi_z      * ind
    p[[paste0("log_mi_z_x_",  y)]] <- p$log_mi_z  * ind
  }

  list(panel = p, other_years = other_years)
}

fit_spec <- function(df, y, spec, other_years) {
  d <- df[!is.na(df[[y]]), ]
  if (length(unique(d[[y]])) < 2 || nrow(d) < 50) return(NULL)

  base_rhs <- c("fxshock_z")
  if (spec == "S1") {
    main      <- "inter_lin"
    ctrl_cols <- character(0)
  } else if (spec == "S2") {
    main      <- "inter_log"
    ctrl_cols <- paste0("mi_z_x_", other_years)
  } else if (spec == "S3") {
    main      <- "inter_lin"
    ctrl_cols <- paste0("log_mi_z_x_", other_years)
  } else stop("unknown spec: ", spec)

  rhs_terms <- c(base_rhs, main, ctrl_cols[ctrl_cols %in% names(d)])
  fml <- as.formula(paste0(
    "`", y, "` ~ ", paste0("`", rhs_terms, "`", collapse = " + "),
    " | lgcode + year"
  ))
  m <- tryCatch(
    feols(fml, data = d, cluster = "lgcode", warn = FALSE, notes = FALSE),
    error = function(e) e
  )
  if (inherits(m, "error")) return(list(err = substr(conditionMessage(m), 1, 80)))

  ct <- m$coeftable
  if (!main %in% rownames(ct)) return(list(err = "main term absorbed"))

  list(
    beta      = unname(ct[main, "Estimate"]),
    se        = unname(ct[main, "Std. Error"]),
    pval      = unname(ct[main, "Pr(>|t|)"]),
    n         = nrow(d),
    n_mun     = length(unique(d$lgcode)),
    mean_y    = mean(d[[y]], na.rm = TRUE),
    r2_within = unname(fitstat(m, "wr2", verbose = FALSE)$wr2)
  )
}

stars <- function(p) {
  if (is.na(p)) ""
  else if (p < 0.01) "***"
  else if (p < 0.05) "**"
  else if (p < 0.10) "*"
  else ""
}

# ------------------------------------------------------------------------------
# 4. Loop
# ------------------------------------------------------------------------------

cat(sprintf(
  "Grid: %d outcomes x %d lags x %d thresholds x %d log-variants x %d specs = %d cells\n",
  length(OUTCOMES), length(LAGS), length(THRESHOLDS),
  length(LOG_VARIANTS), length(SPECS),
  length(OUTCOMES) * length(LAGS) * length(THRESHOLDS) *
    length(LOG_VARIANTS) * length(SPECS)
))

rows <- list(); i <- 0L

for (lag_v in LAGS) {
  for (k in THRESHOLDS) {
    for (log_label in names(LOG_VARIANTS)) {
      bp <- build_panel(lag_v, k, LOG_VARIANTS[[log_label]])
      if (is.null(bp)) next
      df <- bp$panel
      for (spec in SPECS) {
        # S1 is invariant to the log-variant (no log term anywhere).
        # Still recorded for completeness so each (k, lag, log_label, spec)
        # has an entry, but the S1 values will be identical across log labels.
        for (y in OUTCOMES) {
          r <- fit_spec(df, y, spec, bp$other_years)
          i <- i + 1L
          rows[[i]] <- tibble(
            outcome     = y,
            spec        = spec,
            lag         = lag_v,
            threshold   = k,
            log_variant = log_label,
            beta        = if (!is.null(r) && is.null(r$err)) r$beta else NA_real_,
            se          = if (!is.null(r) && is.null(r$err)) r$se   else NA_real_,
            t_stat      = if (!is.null(r) && is.null(r$err)) r$beta / r$se else NA_real_,
            p_val       = if (!is.null(r) && is.null(r$err)) r$pval else NA_real_,
            sig         = if (!is.null(r) && is.null(r$err)) stars(r$pval) else "",
            n_obs       = if (!is.null(r) && is.null(r$err)) r$n    else NA_integer_,
            n_mun       = if (!is.null(r) && is.null(r$err)) r$n_mun else NA_integer_,
            mean_y      = if (!is.null(r) && is.null(r$err)) r$mean_y else NA_real_,
            r2_within   = if (!is.null(r) && is.null(r$err)) r$r2_within else NA_real_,
            note        = if (!is.null(r) && !is.null(r$err)) r$err else ""
          )
        }
      }
      cat(sprintf("  done: lag=%d, k=%d, log=%s\n", lag_v, k, log_label))
    }
  }
}

results <- bind_rows(rows)

write.csv(results,
          "data/clean/robustness_intensity_scaling_muni.csv",
          row.names = FALSE)

cat(sprintf("\nSaved: data/clean/robustness_intensity_scaling_muni.csv (%d rows)\n",
            nrow(results)))

# ------------------------------------------------------------------------------
# 5. Preview the headline robustness question
# ------------------------------------------------------------------------------
# Within (outcome, lag, threshold, spec), how does beta change across the
# four log-variants? In S2 the math says it should be ~identical.

cat("\n=== S2 stability across log-variants (lag=0, k=0) ===\n")
print(
  results %>%
    filter(spec == "S2", lag == 0, threshold == 0) %>%
    select(outcome, log_variant, beta, t_stat, sig) %>%
    mutate(beta = round(beta, 4), t_stat = round(t_stat, 2)) %>%
    pivot_wider(names_from = log_variant, values_from = c(beta, t_stat, sig)) %>%
    as.data.frame()
)

cat("\n=== S3 stability across log-variants (lag=0, k=0) ===\n")
print(
  results %>%
    filter(spec == "S3", lag == 0, threshold == 0) %>%
    select(outcome, log_variant, beta, t_stat, sig) %>%
    mutate(beta = round(beta, 4), t_stat = round(t_stat, 2)) %>%
    pivot_wider(names_from = log_variant, values_from = c(beta, t_stat, sig)) %>%
    as.data.frame()
)
