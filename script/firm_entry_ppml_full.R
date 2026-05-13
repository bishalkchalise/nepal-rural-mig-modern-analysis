# =============================================================================
# script/firm_entry_ppml_full.R
#
# All-in-one R script to reproduce the firm-entry PPML vs OLS-on-log(1+y)
# analysis locally:
#   1. estimates PPML (fepois) and OLS-on-log(1+y) (feols) for each of the
#      12 entry outcomes × 11 lags × 2 estimators = 264 fits,
#   2. writes a tidy CSV with β/SE/p/stars,
#   3. prints a coefficient table at lags 0/2/4/6/8/10,
#   4. produces two ggplot2 overlay figures (by industry, by size) saved
#      to docs/figs/.
#
# Spec mirrors fit_nec_panel in robustness_nec.R exactly:
#   treatment : fx_z(t-L) × log_migint_z(t-L)
#   controls  : year × log_migint_z + year × fx_z + year × Block A
#   FE        : lgcode + year
#   cluster   : lgcode
#   sample    : NEC panel of (muni × founding-year) cells, lag-merged with FX
#               shock at year t-L, filtered to total_migrants_2001 >= k (k=25).
#
# Output:
#   output/tab/robustness_nec_poisson_r.csv
#   docs/figs/firm_entry_lag_industry_ppml_overlay_R.{png,svg}
#   docs/figs/firm_entry_lag_size_ppml_overlay_R.{png,svg}
#
# Wall-clock: ~5 minutes (PPML is the slow side; 132 fits × ~2 sec).
#
# Run from repo root:
#   source("script/firm_entry_ppml_full.R")
# Requirements: fixest, data.table, ggplot2
# =============================================================================

suppressPackageStartupMessages({
  library(fixest); library(data.table); library(ggplot2)
})
source("script/_specs_lib.R")     # provides load_instrument(), build_block_A()
options(scipen = 999); setDTthreads(0)

# -----------------------------------------------------------------------------
# 1. Outcomes / lag grid / threshold
# -----------------------------------------------------------------------------
OUTCOMES <- c(
  "new_firms",
  "new_firms_size_1_worker",  "new_firms_size_2_9_workers",
  "new_firms_size_10_50_workers", "new_firms_size_51plus_workers",
  "new_firms_hospitality_food",   "new_firms_manufacturing",
  "new_firms_construction",       "new_firms_trade_retail",
  "new_firms_transport_storage",  "new_firms_agriculture",
  "new_firms_finance_prof_realestate"
)
LAGS       <- 0:10
THRESHOLD  <- 25L
ESTIMATORS <- c("ols_log", "ppml")

# -----------------------------------------------------------------------------
# 2. Load NEC panel + Block A + instrument
# -----------------------------------------------------------------------------
nec_p <- fread("data/clean/nec2018/mun_entry_panel_new.csv")
nec_p <- nec_p[year >= 2001 & year <= 2018]
for (v in OUTCOMES)
  if (v %in% names(nec_p) && !(paste0("log_", v) %in% names(nec_p)))
    nec_p[, (paste0("log_", v)) := log1p(pmax(get(v), 0))]

inst <- load_instrument()
bxA  <- build_block_A()
BLOCK_A      <- bxA$bx
BLOCK_A_COLS <- bxA$cols

# -----------------------------------------------------------------------------
# 3. Helpers
# -----------------------------------------------------------------------------
zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}
build_year_dummies <- function(d, X_col, prefix, ref_year = 2001L) {
  yrs <- sort(unique(d$year))
  if (length(yrs) < 2) return(character(0))
  ref_use <- if (ref_year %in% yrs) ref_year else min(yrs)
  cols <- character(0)
  for (yr in yrs) {
    if (yr == ref_use) next
    cnm <- sprintf("%s_x_%s", prefix, yr)
    set(d, j = cnm, value = d[[X_col]] * as.numeric(d$year == yr))
    cols <- c(cols, cnm)
  }
  cols
}
stars_fn <- function(p) fifelse(is.na(p), "",
                fifelse(p < .01, "***",
                fifelse(p < .05, "**",
                fifelse(p < .10, "*", ""))))

# -----------------------------------------------------------------------------
# 4. Single fit (estimator switches between feols and fepois)
# -----------------------------------------------------------------------------
fit_one <- function(outcome, lag_L, estimator) {
  inst_use <- inst[, .(lgcode, year, fxshock, mig_intensity, total_migrants,
                       log_mig_intensity)]
  if (lag_L != 0L) inst_use[, year := year + as.integer(lag_L)]
  panel <- merge(nec_p, inst_use, by = c("lgcode","year"), suffixes = c("",".inst"))
  panel <- panel[total_migrants >= THRESHOLD]
  if (nrow(panel) < 50) return(list(err = "n<50 after threshold"))
  muni_yr <- unique(panel[, .(lgcode, year, fxshock, mig_intensity, log_mig_intensity)])
  muni_yr[, fx_z         := zscore(fxshock)]
  muni_yr[, mig_int_z    := zscore(mig_intensity)]
  muni_yr[, log_migint_z := zscore(log_mig_intensity)]
  panel <- merge(panel, muni_yr[, .(lgcode, year, fx_z, mig_int_z, log_migint_z)],
                 by = c("lgcode","year"))
  if (!is.null(BLOCK_A))
    panel <- merge(panel, BLOCK_A, by = "lgcode", all.x = TRUE)

  y <- if (estimator == "ols_log") paste0("log_", outcome) else outcome
  d <- panel[!is.na(get(y)) & !is.na(fx_z)]
  if (nrow(d) < 50 || uniqueN(d[[y]]) < 2)
    return(list(err = "too few obs / no variation"))
  d <- copy(d)
  d[, treatment := fx_z * log_migint_z]

  year_cols <- character(0)
  year_cols <- c(year_cols, build_year_dummies(d, "log_migint_z", "cmig", 2001L))
  year_cols <- c(year_cols, build_year_dummies(d, "fx_z",         "cfx",  2001L))
  for (k in BLOCK_A_COLS)
    year_cols <- c(year_cols, build_year_dummies(d, k, paste0("cA_", k), 2001L))

  rhs <- c("treatment", year_cols)
  fml <- as.formula(sprintf("%s ~ %s | lgcode + year",
                            y, paste(rhs, collapse = " + ")))
  fit <- tryCatch({
    if (estimator == "ols_log")
      feols (fml, data = d, cluster = ~lgcode, notes = FALSE, warn = FALSE)
    else
      fepois(fml, data = d, cluster = ~lgcode, notes = FALSE, warn = FALSE)
  }, error = function(e) e)
  if (inherits(fit, "error"))
    return(list(err = substr(conditionMessage(fit), 1, 120)))
  cf <- coef(fit); se_ <- se(fit); pv <- pvalue(fit)
  if (!("treatment" %in% names(cf))) return(list(err = "treatment absorbed"))
  list(beta = unname(cf["treatment"]),
       se   = unname(se_["treatment"]),
       pval = unname(pv["treatment"]),
       n    = as.integer(fit$nobs),
       n_muni = uniqueN(d$lgcode),
       mean_y = mean(d[[outcome]], na.rm = TRUE),
       sd_y   = sd  (d[[outcome]], na.rm = TRUE))
}

# -----------------------------------------------------------------------------
# 5. Main loop with progress
# -----------------------------------------------------------------------------
dir.create("output/tab", recursive = TRUE, showWarnings = FALSE)
dir.create("docs/figs",  recursive = TRUE, showWarnings = FALSE)
rows <- list()
t0   <- Sys.time()
total <- length(ESTIMATORS) * length(LAGS) * length(OUTCOMES)
cat(sprintf("Running %d fits ...\n", total))

i <- 0
for (est in ESTIMATORS) {
  for (L in LAGS) {
    for (oc in OUTCOMES) {
      i <- i + 1
      r <- fit_one(oc, as.integer(L), est)
      r$estimator <- est; r$lag <- as.integer(L)
      r$outcome <- oc;    r$threshold <- THRESHOLD
      rows[[length(rows) + 1]] <- r
      if (!is.null(r$err)) {
        cat(sprintf("[%3d/%d] %-7s lag=%2d %-38s  ERR: %s\n",
                    i, total, est, L, oc, r$err))
      } else {
        cat(sprintf("[%3d/%d] %-7s lag=%2d %-38s  β=%+.4f%-3s (n=%d)\n",
                    i, total, est, L, oc, r$beta, stars_fn(r$pval), r$n))
      }
    }
  }
}
res <- rbindlist(rows, fill = TRUE)
res[, stars := stars_fn(pval)]
KEEP <- c("estimator","outcome","lag","threshold","beta","stars","se","pval",
          "mean_y","sd_y","n","n_muni","err")
res <- res[, intersect(KEEP, names(res)), with = FALSE]
fwrite(res, "output/tab/robustness_nec_poisson_r.csv")
cat(sprintf("\nSaved: output/tab/robustness_nec_poisson_r.csv\n"))
cat(sprintf("Wall-clock: %.1f min\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

# -----------------------------------------------------------------------------
# 6. Pretty-print coefficient tables at key lags
# -----------------------------------------------------------------------------
fmt_beta <- function(r) {
  if (nrow(r) == 0 || is.na(r$beta)) return(sprintf("%-10s", "—"))
  sprintf("%+.4f%-3s", r$beta, r$stars)
}
key_lags <- c(0, 2, 4, 6, 8, 10)
cat("\n=================== OLS vs PPML at key lags ===================\n")
cat(sprintf("%-38s | %-3s | %-10s | %-10s\n",
            "outcome","lag","OLS-log","PPML"))
cat(paste0(strrep("-", 78), "\n"))
for (oc in OUTCOMES) {
  for (L in key_lags) {
    ol <- res[outcome == oc & lag == L & estimator == "ols_log"]
    pp <- res[outcome == oc & lag == L & estimator == "ppml"]
    cat(sprintf("%-38s | %-3d | %-10s | %-10s\n",
                oc, L, fmt_beta(ol), fmt_beta(pp)))
  }
  cat("\n")
}

# -----------------------------------------------------------------------------
# 7. Plots (ggplot2): by industry, by size
# -----------------------------------------------------------------------------
res_plt <- res[!is.na(beta)]
res_plt[, lwr := beta - 1.96 * se]
res_plt[, upr := beta + 1.96 * se]
res_plt[, est_lbl := factor(estimator,
                             levels = c("ols_log","ppml"),
                             labels = c("OLS on log(1+y)","PPML on raw count"))]

IND_LBLS <- c(
  "new_firms"                          = "All entry (aggregate)",
  "new_firms_hospitality_food"         = "Hospitality & food",
  "new_firms_manufacturing"            = "Manufacturing",
  "new_firms_construction"             = "Construction",
  "new_firms_transport_storage"        = "Transport & storage",
  "new_firms_trade_retail"             = "Trade & retail",
  "new_firms_agriculture"              = "Agriculture",
  "new_firms_finance_prof_realestate"  = "Finance / prof / RE"
)
SIZE_LBLS <- c(
  "new_firms"                          = "All entry (aggregate)",
  "new_firms_size_1_worker"            = "1 worker",
  "new_firms_size_2_9_workers"         = "2–9 workers",
  "new_firms_size_10_50_workers"       = "10–50 workers",
  "new_firms_size_51plus_workers"      = "51+ workers"
)

plot_panel <- function(lbl_map, title_main, file_stem, ncol_facets,
                       width_in, height_in) {
  d <- res_plt[outcome %in% names(lbl_map)]
  d[, panel := factor(lbl_map[outcome], levels = unname(lbl_map))]
  p <- ggplot(d, aes(x = lag, y = beta, color = est_lbl, fill = est_lbl)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(aes(shape = est_lbl), size = 1.6) +
    facet_wrap(~ panel, ncol = ncol_facets, scales = "free_y") +
    scale_color_manual(values = c("OLS on log(1+y)" = "#2c5282",
                                  "PPML on raw count" = "#c53030")) +
    scale_fill_manual (values = c("OLS on log(1+y)" = "#2c5282",
                                  "PPML on raw count" = "#c53030")) +
    scale_shape_manual(values = c("OLS on log(1+y)" = 16,
                                  "PPML on raw count" = 15)) +
    scale_x_continuous(breaks = 0:10) +
    labs(x = "FX shifter lag (years)",
         y = "β  (semi-elasticity)",
         title = title_main,
         color = NULL, fill = NULL, shape = NULL,
         caption = paste(
           "Anchor flow spec: treatment = fx_z(t-L) × log(mig_int_z);",
           "controls year × mig + year × fx + year × Block A; muni + year FE;",
           "SE clustered at muni. k = 25. Both β are semi-elasticities (% change in expected entry)."
         )) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top",
          plot.title    = element_text(face = "bold", size = 13),
          plot.caption  = element_text(hjust = 0, size = 8.5, color = "grey30"),
          strip.text    = element_text(face = "bold", size = 10),
          panel.grid.minor = element_blank())
  ggsave(filename = paste0("docs/figs/", file_stem, ".png"), p,
         width = width_in, height = height_in, dpi = 150)
  ggsave(filename = paste0("docs/figs/", file_stem, ".svg"), p,
         width = width_in, height = height_in)
  cat(sprintf("Saved: docs/figs/%s.{png,svg}\n", file_stem))
}

plot_panel(IND_LBLS,
           "Firm entry by industry — OLS on log(1+y) vs. PPML on raw count",
           "firm_entry_lag_industry_ppml_overlay_R",
           ncol_facets = 4,  width_in = 14, height_in = 8.5)
plot_panel(SIZE_LBLS,
           "Firm entry by size — OLS on log(1+y) vs. PPML on raw count",
           "firm_entry_lag_size_ppml_overlay_R",
           ncol_facets = 5,  width_in = 16, height_in = 5.2)

cat("\nDone.\n")
