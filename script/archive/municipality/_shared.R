###############################################################################
# _shared.R — common helpers for the baseline reduced-form table scripts.
# Sourced by:
#   - baseline_census.R     (municipality × census-year panel, 2011–2021)
#   - baseline_rvs.R        (HH × year HRVS panel, 2016–2018)
#   - baseline_nec_cs.R     (municipality cross-section, 2018)
#   - baseline_nec_panel.R  (municipality × founding-year panel, 2001–2018)
#
# All scripts run from the project root (autodetected via `here::here()`).
###############################################################################

# ---- Packages ----------------------------------------------------------------
.req <- c("tidyverse", "fixest", "here")
.miss <- setdiff(.req, rownames(installed.packages()))
if (length(.miss)) {
  message("Installing required packages: ", paste(.miss, collapse = ", "))
  install.packages(.miss, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(here)
})

# ---- Project root ------------------------------------------------------------
ROOT <- tryCatch(here::here(), error = function(e) getwd())
data_path <- function(...) file.path(ROOT, "data", "clean", ...)

# ---- Standardise -------------------------------------------------------------
zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# ---- Optional inputs for Khanna et al. (2026) controls -------------------------------
# Returns list(region_df, region_cols, trade_df, trade_cols, dest_gdp_df).
# Files are silently absent → khanna controls just disabled.
load_khanna_inputs <- function() {
  region_path  <- data_path("instrument", "dest_region_shares_2001.csv")
  trade_path   <- data_path("instrument", "trade_ssiv.csv")
  wdi_path     <- data_path("instrument", "wdi_dest_gdp_2001.csv")
  share_path   <- data_path("instrument", "dest_mun_mig_share_2001.csv")
  region_df <- if (file.exists(region_path))
                 readr::read_csv(region_path, show_col_types = FALSE) else NULL
  trade_df  <- if (file.exists(trade_path))
                 readr::read_csv(trade_path, show_col_types = FALSE) else NULL
  region_cols <- if (!is.null(region_df))
                   grep("^share_", names(region_df), value = TRUE) else character(0)
  # Drop the largest region as the implicit reference (avoids the 7-share
  # collinearity with year FE).
  if (length(region_cols) > 0) {
    means <- vapply(region_cols, function(c) mean(region_df[[c]], na.rm = TRUE), 0)
    ref <- names(which.max(means))
    region_cols <- setdiff(region_cols, ref)
    message(sprintf("Khanna region reference (omitted): %s", ref))
  }
  trade_cols <- if (!is.null(trade_df))
                  setdiff(names(trade_df), c("lgcode", "year")) else character(0)

  # Khanna Block A: dest GDP/cap aggregated to muni via 2001 baseline shares
  # dest_gdp_o0 = Σ_d s_md0 · GDP_d_2001
  dest_gdp_df <- NULL
  if (file.exists(wdi_path) && file.exists(share_path)) {
    wdi   <- readr::read_csv(wdi_path,   show_col_types = FALSE) |>
               dplyr::select(country, gdp_pc_2001) |> tidyr::drop_na()
    share <- readr::read_csv(share_path, show_col_types = FALSE)
    dest_gdp_df <- share |>
      dplyr::inner_join(wdi, by = "country") |>
      dplyr::mutate(prod = mun_mig_share_2001 * gdp_pc_2001) |>
      dplyr::group_by(lgcode) |>
      dplyr::summarise(dest_gdp_pc_2001 = sum(prod, na.rm = TRUE),
                       coverage         = sum(mun_mig_share_2001, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::mutate(dest_gdp_pc_2001 = dest_gdp_pc_2001 /
                                       dplyr::if_else(coverage > 0, coverage, 1)) |>
      dplyr::select(lgcode, dest_gdp_pc_2001)
    message(sprintf("Khanna Block A: dest GDP/cap aggregated for %d munis (mean = USD %s)",
                    nrow(dest_gdp_df),
                    format(round(mean(dest_gdp_df$dest_gdp_pc_2001, na.rm = TRUE)),
                           big.mark = ",")))
  }

  list(region_df = region_df, region_cols = region_cols,
       trade_df  = trade_df,  trade_cols  = trade_cols,
       dest_gdp_df = dest_gdp_df)
}

# ---- Stars from p-values -----------------------------------------------------
stars_pval <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE     ~ ""
  )
}

# ---- One-shot fit using fixest::feols ---------------------------------------
# `controls` is a character vector of column names to add to the formula.
# Cluster column comes from `cluster_var` in the data frame.
fit_one <- function(df, y, shock = "fxshock", controls = character(0),
                    fe = c("lgcode", "year"), cluster_var = "lgcode",
                    asinh = FALSE) {
  d <- df %>% drop_na(all_of(c(y, shock, cluster_var)))
  if (nrow(d) < 50 || dplyr::n_distinct(d[[y]]) < 2) return(NULL)
  if (asinh) d[[y]] <- asinh(d[[y]])

  rhs <- c(shock, controls)
  rhs <- intersect(rhs, names(d))
  fml <- as.formula(
    paste0("`", y, "` ~ ", paste0("`", rhs, "`", collapse = " + "),
           " | ", paste(fe, collapse = " + "))
  )
  m <- tryCatch(
    feols(fml, data = d, cluster = as.formula(paste0("~", cluster_var)),
          warn = FALSE, notes = FALSE),
    error = function(e) e
  )
  if (inherits(m, "error")) return(NULL)
  ct <- m$coeftable
  if (!shock %in% rownames(ct)) return(NULL)
  list(
    beta   = unname(ct[shock, "Estimate"]),
    se     = unname(ct[shock, "Std. Error"]),
    pval   = unname(ct[shock, "Pr(>|t|)"]),
    n      = nrow(d),
    n_unit = dplyr::n_distinct(d[[fe[1]]]),
    mean_y = mean(d[[y]], na.rm = TRUE)
  )
}

# Cross-section variant (no entity FE; use district FE via formula).
fit_cs <- function(df, y, shock = "fxshock_z", controls = character(0),
                   fe = "DIST", cluster_var = "DIST", asinh = FALSE) {
  d <- df %>% drop_na(all_of(c(y, shock, cluster_var)))
  if (nrow(d) < 30 || dplyr::n_distinct(d[[y]]) < 2) return(NULL)
  if (asinh) d[[y]] <- asinh(d[[y]])

  rhs <- intersect(c(shock, controls), names(d))
  fml <- as.formula(
    paste0("`", y, "` ~ ", paste0("`", rhs, "`", collapse = " + "),
           " | ", paste(fe, collapse = " + "))
  )
  m <- tryCatch(
    feols(fml, data = d, cluster = as.formula(paste0("~", cluster_var)),
          warn = FALSE, notes = FALSE),
    error = function(e) e
  )
  if (inherits(m, "error")) return(NULL)
  ct <- m$coeftable
  if (!shock %in% rownames(ct)) return(NULL)
  list(
    beta   = unname(ct[shock, "Estimate"]),
    se     = unname(ct[shock, "Std. Error"]),
    pval   = unname(ct[shock, "Pr(>|t|)"]),
    n      = nrow(d),
    n_unit = dplyr::n_distinct(d[[fe[1]]]),
    mean_y = mean(d[[y]], na.rm = TRUE)
  )
}

# ---- Pretty table renderer ---------------------------------------------------
# `groups` is a list of `list(name = "...", items = list(c(y, label, asinh)))`.
# `fit_fn` is fit_one (panels) or fit_cs (cross-section).
# Returns the data frame of formatted rows; also prints to console.
build_table <- function(groups, panel_df, fit_fn, ...) {
  rows <- list()
  for (g in groups) {
    rows[[length(rows) + 1L]] <- list(group = g$name)
    for (it in g$items) {
      y     <- it[[1]]; lab <- it[[2]]; ash <- as.logical(it[[3]])
      if (!y %in% names(panel_df)) {
        rows[[length(rows) + 1L]] <- list(
          Outcome = lab, mean = "—", coef = "NF", se = "", pct = "—", N = "—"
        ); next
      }
      r <- fit_fn(panel_df, y, asinh = ash, ...)
      if (is.null(r)) {
        rows[[length(rows) + 1L]] <- list(
          Outcome = lab, mean = "—", coef = "—", se = "", pct = "—", N = "—"
        ); next
      }
      pct <- if (!is.na(r$mean_y) && r$mean_y != 0) sprintf("%+.1f%%", r$beta / r$mean_y * 100) else "—"
      rows[[length(rows) + 1L]] <- list(
        Outcome = paste0(lab, if (ash) " (asinh)" else ""),
        mean    = sprintf("%.3f", r$mean_y),
        coef    = sprintf("%+.4f%s", r$beta, stars_pval(r$pval)),
        se      = sprintf("(%.4f)", r$se),
        pct     = pct,
        N       = format(r$n, big.mark = ",")
      )
    }
  }
  rows
}

print_table <- function(rows, title, n_obs, n_unit, unit_label, spec_str) {
  W <- c(Outcome = 40, mean = 12, coef = 14, se = 12, pct = 10, N = 10)
  pad <- function(x, w) formatC(as.character(x), width = w, flag = "-")
  cat("\n", title, "\n", sep = "")
  cat(format(n_obs, big.mark = ","), unit_label, "obs ·",
      format(n_unit, big.mark = ","), "unit\n", sep = " ")
  cat(spec_str, "\n\n", sep = "")
  cat(strrep("─", sum(W)), "\n", sep = "")
  cat(pad("Outcome", W["Outcome"]),
      pad("mean(y)", W["mean"]),
      pad("β (per 1 SD)", W["coef"]),
      pad("SE", W["se"]),
      pad("% of mean", W["pct"]),
      pad("N", W["N"]), "\n", sep = " ")
  cat(strrep("─", sum(W)), "\n", sep = "")
  for (r in rows) {
    if (!is.null(r$group)) {
      cat("\n  ", r$group, "\n  ", strrep("─", 38), "\n", sep = "")
    } else {
      cat(pad(r$Outcome, W["Outcome"]),
          pad(r$mean,    W["mean"]),
          pad(r$coef,    W["coef"]),
          pad(r$se,      W["se"]),
          pad(r$pct,     W["pct"]),
          pad(r$N,       W["N"]), "\n", sep = " ")
    }
  }
  cat(strrep("─", sum(W)), "\n", sep = "")
}

print_notes <- function(spec_notes = NULL) {
  cat("\nNotes: β reported per 1 SD of standardised SSIV. ",
      "Standard errors clustered as indicated.\n",
      "       *** p<0.01, ** p<0.05, * p<0.10. ",
      "(asinh) outcomes use inverse-hyperbolic-sine.\n", sep = "")
  if (!is.null(spec_notes)) cat("       ", spec_notes, "\n", sep = "")
}
