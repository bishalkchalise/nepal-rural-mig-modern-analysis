################################################################################
#
# ROBUSTNESS: intensity scaling for the MUNICIPALITY second-stage
# ------------------------------------------------------------------------------
# Mirrors the OLD muni `build_results.R` skeleton exactly (lags, shock variants,
# control sets, asinh transform, z-scoring), but adds a robustness dimension:
# how the intensity variable in the `mi` control is scaled.
#
# Note on the OLD muni structure:
#   Headline shock        : ssiv_index_2001 (per-capita; intensity is baked in
#                            LINEARLY -- ssiv = shareshock x mig_int_linear)
#   Intensity in controls : log_mi = asinh(geog_intensity_2001)
#   Shock variants        : ssiv_index, ssiv_w99 (winsorized 99%)
#   Lags                  : 0, 1, 2 years
#   Control sets          : none, mi, khanna_full
#
# Robustness dimension added here -- the scaling of mig_int in the `mi`
# control term:
#
#   asinh(mi)            old muni default
#   log(1000   * mi)     per-thousand log
#   log(100000 * mi)     per-100k log (uniformly positive in muni data)
#   mi_linear            no transform, just intensity
#
# Inputs (archived muni paths) :
#   - data/clean/instrument/instrument_mun.csv
#   - data/clean/census/census_outcomes_municipality.csv
#
# Output :
#   - data/clean/robustness_intensity_scaling_muni.csv
#     (one row per outcome x lag x shock x control set x mi-scaling)
#
# Run from the original (municipality-era) project root.
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

# ------------------------------------------------------------------------------
# Configuration (matches build_results.R from main)
# ------------------------------------------------------------------------------

REF_YEAR <- 2011
YEARS    <- c(2001, 2011, 2021)   # drop 2001 below

LAGS <- list(
  lag0 = 0L,
  lag1 = 1L,
  lag2 = 2L
)

SHOCKS <- list(
  ssiv_index = "ssiv_index_2001",
  ssiv_w99   = "ssiv_w99"
)

CONTROL_SETS <- c("none", "mi", "khanna_full")

# Baseline X (must match the columns present in census_outcomes_municipality)
BASE_X_DEV <- c("amen_lighting_electricity", "amen_water_piped",
                "ent_has_nonagro",           "head_age_mean", "edu_literate")
BASE_X_IND <- c("work_share_agriculture", "ind_manufacturing", "flfp_all")
BASE_X_BIG <- c(BASE_X_DEV, BASE_X_IND)

INTENSITY_SCALINGS <- list(
  "asinh(mi)        [OLD default]" = function(x) asinh(x),
  "log(1000 * mi)"                 = function(x) log(pmax(1000   * x, 1e-12)),
  "log(100000 * mi)"               = function(x) log(pmax(100000 * x, 1e-12)),
  "mi_linear"                      = function(x) x
)

OUTCOMES <- c(
  "amen_water_piped", "amen_cooking_modern", "amen_lighting_electricity",
  "amen_toilet_modern", "amen_assets_mobile", "amen_assets_motorcycle",
  "housing_own", "housing_foundation_modern",
  "mig_in_share", "mig_in_international",
  "ent_has_nonagro", "head_female_share", "flfp_all"
)

# ------------------------------------------------------------------------------
# Load
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
cat(sprintf("Outcomes available: %d\n  %s\n",
            length(OUTCOMES), paste(OUTCOMES, collapse = ", ")))

# ------------------------------------------------------------------------------
# Panel builder (mirrors build_results.R::build_panel())
# ------------------------------------------------------------------------------

build_panel <- function(lag_yrs, intensity_fn) {
  inst_lag <- instrument %>%
    mutate(year = year + lag_yrs)

  p <- outcomes_df %>%
    inner_join(inst_lag, by = c("lgcode", "year")) %>%
    filter(year %in% YEARS, year != 2001)

  p$geog_intensity_2001 <- replace_na(p$geog_intensity_2001, 0)
  p$log_mi              <- intensity_fn(p$geog_intensity_2001)

  # Winsorized SSIV at 99th percentile
  hi          <- quantile(p$ssiv_index_2001, 0.99, na.rm = TRUE)
  p$ssiv_w99  <- pmin(p$ssiv_index_2001, hi)

  # Standardize each shock
  for (k in names(SHOCKS)) {
    col <- SHOCKS[[k]]
    sd_ <- sd(p[[col]], na.rm = TRUE)
    p[[paste0(k, "_z")]] <- if (!is.na(sd_) && sd_ > 0)
      (p[[col]] - mean(p[[col]], na.rm = TRUE)) / sd_ else 0
  }

  # year × X interactions
  other_years  <- setdiff(YEARS, REF_YEAR)
  other_years  <- other_years[other_years != 2001]
  base_present <- intersect(paste0(BASE_X_BIG, "_2001"), names(p))

  for (y in other_years) {
    ind <- as.integer(p$year == y)
    p[[paste0("mi_x_",         y)]] <- p$log_mi                  * ind
    p[[paste0("shareshock_x_", y)]] <- p$shareshock_index_2001   * ind
    for (c in base_present) {
      v <- replace_na(p[[c]], mean(p[[c]], na.rm = TRUE))
      p[[paste0(c, "_x_", y)]] <- v * ind
    }
  }

  list(panel = p, other_years = other_years, base_present = base_present)
}

control_cols <- function(tag, other_years, base_present, panel_cols) {
  cols <- character(0)
  if (tag == "none") return(cols)
  if (tag %in% c("mi", "khanna_full"))
    cols <- c(cols, paste0("mi_x_", other_years))
  if (tag == "khanna_full") {
    cols <- c(cols, paste0("shareshock_x_", other_years))
    full <- as.vector(outer(base_present, other_years,
                            function(c, y) paste0(c, "_x_", y)))
    cols <- c(cols, intersect(full, panel_cols))
  }
  cols
}

fit_one <- function(df, y, shock_z, controls) {
  d <- df[!is.na(df[[y]]), ]
  if (length(unique(d[[y]])) < 2 || nrow(d) < 50) return(NULL)
  rhs_terms <- c(shock_z, controls[controls %in% names(d)])
  fml <- as.formula(paste0("`", y, "` ~ ",
                           paste0("`", rhs_terms, "`", collapse = " + "),
                           " | lgcode + year"))
  m <- tryCatch(
    feols(fml, data = d, cluster = "lgcode", warn = FALSE, notes = FALSE),
    error = function(e) e
  )
  if (inherits(m, "error")) return(list(err = substr(conditionMessage(m), 1, 80)))
  ct <- m$coeftable
  if (!shock_z %in% rownames(ct)) return(list(err = "shock absorbed"))
  list(
    beta      = unname(ct[shock_z, "Estimate"]),
    se        = unname(ct[shock_z, "Std. Error"]),
    pval      = unname(ct[shock_z, "Pr(>|t|)"]),
    n         = nrow(d),
    n_mun     = length(unique(d$lgcode)),
    mean_y    = mean(d[[y]], na.rm = TRUE),
    r2_within = unname(fitstat(m, "wr2", verbose = FALSE)$wr2)
  )
}

# ------------------------------------------------------------------------------
# Loop over the full grid
# ------------------------------------------------------------------------------

cat(sprintf(
  "Running %d outcomes x %d lags x %d shocks x %d controls x %d mi-scalings = %d cells\n",
  length(OUTCOMES), length(LAGS), length(SHOCKS), length(CONTROL_SETS),
  length(INTENSITY_SCALINGS),
  length(OUTCOMES) * length(LAGS) * length(SHOCKS) *
    length(CONTROL_SETS) * length(INTENSITY_SCALINGS)
))

rows <- list()
i <- 0L

for (lag_key in names(LAGS)) {
  for (mi_label in names(INTENSITY_SCALINGS)) {
    bp <- build_panel(LAGS[[lag_key]], INTENSITY_SCALINGS[[mi_label]])
    df <- bp$panel
    for (shock_key in names(SHOCKS)) {
      shock_z <- paste0(shock_key, "_z")
      for (ctrl_key in CONTROL_SETS) {
        cc <- control_cols(ctrl_key, bp$other_years, bp$base_present, names(df))
        for (y in OUTCOMES) {
          r <- fit_one(df, y, shock_z, cc)
          i <- i + 1L
          row <- tibble(
            outcome       = y,
            lag           = lag_key,
            shock         = shock_key,
            controls      = ctrl_key,
            mi_scaling    = mi_label,
            beta          = if (!is.null(r) && is.null(r$err)) r$beta else NA_real_,
            se            = if (!is.null(r) && is.null(r$err)) r$se   else NA_real_,
            t_stat        = if (!is.null(r) && is.null(r$err)) r$beta / r$se else NA_real_,
            p_val         = if (!is.null(r) && is.null(r$err)) r$pval else NA_real_,
            n_obs         = if (!is.null(r) && is.null(r$err)) r$n    else NA_integer_,
            n_mun         = if (!is.null(r) && is.null(r$err)) r$n_mun else NA_integer_,
            mean_y        = if (!is.null(r) && is.null(r$err)) r$mean_y else NA_real_,
            r2_within     = if (!is.null(r) && is.null(r$err)) r$r2_within else NA_real_,
            note          = if (!is.null(r) && !is.null(r$err)) r$err else ""
          )
          rows[[i]] <- row
        }
      }
    }
    cat(sprintf("  done: lag=%s, mi_scaling=%s\n", lag_key, mi_label))
  }
}

results <- bind_rows(rows) %>%
  mutate(sig = case_when(
    is.na(p_val)  ~ "",
    p_val < 0.01  ~ "***",
    p_val < 0.05  ~ "**",
    p_val < 0.1   ~ "*",
    TRUE          ~ ""
  ))

write.csv(results,
          "data/clean/robustness_intensity_scaling_muni.csv",
          row.names = FALSE)

cat(sprintf("\nSaved: data/clean/robustness_intensity_scaling_muni.csv (%d rows)\n",
            nrow(results)))

# Quick console preview: how sign / sig of the main shock changes across
# mi-scalings, for lag=0 main spec, no controls vs full controls
cat("\n=== PREVIEW (lag=0, shock=ssiv_index): is sign/sig stable across mi-scaling? ===\n")
print(
  results %>%
    filter(lag == "lag0", shock == "ssiv_index") %>%
    select(outcome, controls, mi_scaling, beta, t_stat, sig) %>%
    mutate(beta = round(beta, 4), t_stat = round(t_stat, 2)) %>%
    arrange(outcome, controls, mi_scaling) %>%
    as.data.frame()
)
