################################################################################
# Robustness: drop-district checks
# ---------------------------------------------------------------------------
# Runs the headline spec (scaling = log, lag = 2, model = M4) under two
# district-set variants, on a short list of headline outcomes:
#
#   1) thin_cov: drop districts in the bottom quartile by # destinations
#                appearing in the 2009-10 DOFE share data
#   2) loo     : leave-one-out (drop each of 75 districts in turn, fit, record)
#
# Output:
#   district-analysis/output/tab/robustness_drop_districts.csv
#     cols: dataset, outcome, variant, dropped_dname, beta, se, p, sig, n
#
# Run: source("district-analysis/script/_robustness_drop_districts.R")
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

t0 <- Sys.time()

# ---- Reuse panel build from main script -----------------------------------
# Defer to the main robustness script to construct mi, z_v2, regions, cdf,
# hh, ncs, npd in one consistent way. The script writes the standard grid
# CSV too, but the in-memory objects are what we need.
SKIP_RUN <- TRUE   # tells main script to build panels + helpers but skip the regression loop
suppressMessages(suppressWarnings({
  source("district-analysis/script/_robustness_all_panels.R")
}))

# After sourcing, we have: cdf, hh, ncs (maybe NULL), npd (maybe NULL),
# mi, z_v2, sh_v2, regions, attach_z_lags(), fit_one(), LAGS, SCALINGS,
# mi_col_for(), zstd_col_for(), BASELINE_SCALING, BASELINE_LAG, REGION_COLS.

stars <- function(p) ifelse(is.na(p), "",
  ifelse(p<0.01,"***", ifelse(p<0.05,"**", ifelse(p<0.10,"*",""))))
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Thin-coverage flag ---------------------------------------------------
# Count distinct 2009-10 destinations per district (after positivity filter)
n_dest <- sh_v2 %>%
  filter(share > 0) %>%
  group_by(dname) %>%
  summarise(n_dest = n_distinct(country), .groups = "drop")
q1 <- quantile(n_dest$n_dest, 0.25, na.rm = TRUE)
thin_districts <- n_dest %>% filter(n_dest <= q1) %>% pull(dname)
cat(sprintf("Thin-coverage cutoff (Q1 of n_dest): %.0f\n", q1))
cat(sprintf("Thin-coverage districts (%d): %s\n",
            length(thin_districts), paste(thin_districts, collapse = ", ")))

# ---- Build dynamic HEADLINE list from outcomes that are significant at the
# ---- baseline (M4, scaling=log, lag=2, p<0.10) in the main robustness CSV.
sig_outcomes <- list()
main_csv <- "district-analysis/output/tab/robustness_all_panels.csv"
if (file.exists(main_csv)) {
  rg <- read_csv(main_csv, show_col_types = FALSE) %>%
    filter(model == "M4", scaling == "log", lag == 2L, !is.na(p), p < 0.10)
  for (ds in unique(rg$dataset)) {
    sig_outcomes[[ds]] <- rg %>% filter(dataset == ds) %>% pull(outcome) %>% unique()
  }
  cat("Outcomes per dataset significant at baseline M4/log/lag2 (p<0.10):\n")
  for (ds in names(sig_outcomes)) cat(sprintf("  %s: %d\n", ds, length(sig_outcomes[[ds]])))
}

HEADLINE <- list(
  list(ds = "census",    panel = cdf, mode = "dname", refyr = 2011L, outs = sig_outcomes$census    %||% character()),
  list(ds = "hh",        panel = hh,  mode = "hhid",  refyr = 2016L, outs = sig_outcomes$hh        %||% character()),
  list(ds = "nec_cs",    panel = ncs, mode = "cs",    refyr = NA_integer_, outs = sig_outcomes$nec_cs %||% character()),
  list(ds = "nec_panel", panel = if (exists("npd")) npd else NULL, mode = "dname", refyr = 2011L, outs = sig_outcomes$nec_panel %||% character())
)

run_one <- function(panel, ycol, mode, refyr) {
  # Single-cell wrapper around fit_one for baseline spec (log, lag=2, M4).
  out <- tryCatch(
    fit_one(panel, ycol, BASELINE_SCALING, BASELINE_LAG, mode, refyr),
    error = function(e) NULL)
  if (is.null(out) || nrow(out) == 0) return(NULL)
  m4 <- out[out$model == "M4", ]
  if (nrow(m4) == 0) return(NULL)
  tibble(beta = m4$beta, se = m4$se, p = m4$p, n = m4$n)
}

out_rows <- list()

cat("\n========== Variant: thin_cov (drop thin-coverage districts) ==========\n")
for (h in HEADLINE) {
  if (is.null(h$panel)) next
  pn <- h$panel %>% filter(!dname %in% thin_districts)
  for (yc in h$outs) {
    if (!yc %in% names(pn)) next
    r <- run_one(pn, yc, h$mode, h$refyr)
    if (is.null(r)) next
    out_rows[[length(out_rows)+1]] <- tibble(
      dataset = h$ds, outcome = yc, variant = "thin_cov",
      dropped_dname = paste(thin_districts, collapse = "|"),
      beta = r$beta, se = r$se, p = r$p, sig = stars(r$p), n = r$n
    )
  }
}

cat("\n========== Variant: baseline (all districts) ==========\n")
for (h in HEADLINE) {
  if (is.null(h$panel)) next
  for (yc in h$outs) {
    if (!yc %in% names(h$panel)) next
    r <- run_one(h$panel, yc, h$mode, h$refyr)
    if (is.null(r)) next
    out_rows[[length(out_rows)+1]] <- tibble(
      dataset = h$ds, outcome = yc, variant = "baseline",
      dropped_dname = NA_character_,
      beta = r$beta, se = r$se, p = r$p, sig = stars(r$p), n = r$n
    )
  }
}

cat("\n========== Variant: loo (leave-one-out) ==========\n")
# Get district list (use mi dnames as the canonical set)
all_dist <- sort(unique(mi$dname))
cat(sprintf("LOO across %d districts...\n", length(all_dist)))
i <- 0
for (d in all_dist) {
  i <- i + 1
  if (i %% 10 == 0) cat(sprintf("  ... %d/%d (%s)\n", i, length(all_dist), d))
  for (h in HEADLINE) {
    if (is.null(h$panel)) next
    pn <- h$panel %>% filter(dname != d)
    for (yc in h$outs) {
      if (!yc %in% names(pn)) next
      r <- run_one(pn, yc, h$mode, h$refyr)
      if (is.null(r)) next
      out_rows[[length(out_rows)+1]] <- tibble(
        dataset = h$ds, outcome = yc, variant = "loo",
        dropped_dname = d,
        beta = r$beta, se = r$se, p = r$p, sig = stars(r$p), n = r$n
      )
    }
  }
}

out <- bind_rows(out_rows)
dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(out, "district-analysis/output/tab/robustness_drop_districts.csv")

cat(sprintf("\nWrote %d rows to district-analysis/output/tab/robustness_drop_districts.csv\n",
            nrow(out)))
cat(sprintf("Elapsed: %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))
