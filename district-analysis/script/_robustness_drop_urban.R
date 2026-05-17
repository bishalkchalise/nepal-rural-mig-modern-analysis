################################################################################
# Robustness: drop urban migration-receiver districts
# ---------------------------------------------------------------------------
# Kathmandu shows up as the dominant LOO driver for NEC firm-count and
# employment outcomes (60-65% swing on a single drop). This script runs the
# headline spec under three urban-drop variants:
#   1) drop_ktm        : drop Kathmandu only
#   2) drop_ktm_valley : drop Kathmandu + Lalitpur + Bhaktapur (KTM valley)
#   3) drop_top_urban  : drop the 6 top urban receivers
#                        (KTM valley + Morang, Rupandehi, Kaski)
# vs.
#   0) baseline (all 75 districts)
#
# Focus on NEC firm outcomes; also include the migration headlines so we
# can show the migration story isn't a "urban districts" artefact.
#
# Output: district-analysis/output/tab/robustness_drop_urban.csv
#
# Run: source("district-analysis/script/_robustness_drop_urban.R")
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

t0 <- Sys.time()
SKIP_RUN <- TRUE
suppressMessages(suppressWarnings({
  source("district-analysis/script/_robustness_all_panels.R")
}))

stars <- function(p) ifelse(is.na(p), "",
  ifelse(p<0.01,"***", ifelse(p<0.05,"**", ifelse(p<0.10,"*",""))))
`%||%` <- function(a, b) if (is.null(a)) b else a

# Drop variants
DROP_SETS <- list(
  baseline        = character(),
  drop_ktm        = c("Kathmandu"),
  drop_ktm_valley = c("Kathmandu", "Lalitpur", "Bhaktapur"),
  drop_top_urban  = c("Kathmandu", "Lalitpur", "Bhaktapur",
                      "Morang", "Rupandehi", "Kaski")
)

# Headline list = baseline-significant outcomes (same logic as drop_districts)
main_csv <- "district-analysis/output/tab/robustness_all_panels.csv"
sig_outcomes <- list()
if (file.exists(main_csv)) {
  rg <- read_csv(main_csv, show_col_types = FALSE) %>%
    filter(model == "M4", scaling == "log", lag == 2L, !is.na(p), p < 0.10)
  for (ds in unique(rg$dataset)) {
    sig_outcomes[[ds]] <- rg %>% filter(dataset == ds) %>% pull(outcome) %>% unique()
  }
  for (ds in names(sig_outcomes))
    cat(sprintf("  %s: %d baseline-sig outcomes\n", ds, length(sig_outcomes[[ds]])))
}

HEADLINE <- list(
  list(ds="census",    panel=cdf, mode="dname", refyr=2011L,        outs=sig_outcomes$census    %||% character()),
  list(ds="hh",        panel=hh,  mode="hhid",  refyr=2016L,        outs=sig_outcomes$hh        %||% character()),
  list(ds="nec_cs",    panel=ncs, mode="cs",    refyr=NA_integer_,  outs=sig_outcomes$nec_cs    %||% character()),
  list(ds="nec_panel", panel=if (exists("npd")) npd else NULL, mode="dname", refyr=2011L, outs=sig_outcomes$nec_panel %||% character())
)

run_one <- function(panel, ycol, mode, refyr) {
  out <- tryCatch(
    fit_one(panel, ycol, BASELINE_SCALING, BASELINE_LAG, mode, refyr),
    error = function(e) NULL)
  if (is.null(out) || nrow(out) == 0) return(NULL)
  m4 <- out[out$model == "M4", ]
  if (nrow(m4) == 0) return(NULL)
  tibble(beta = m4$beta, se = m4$se, p = m4$p, n = m4$n)
}

out_rows <- list()
for (variant in names(DROP_SETS)) {
  drop <- DROP_SETS[[variant]]
  cat(sprintf("\n========== Variant: %s (drop %d districts) ==========\n",
              variant, length(drop)))
  for (h in HEADLINE) {
    if (is.null(h$panel)) next
    pn <- if (length(drop)) h$panel %>% filter(!dname %in% drop) else h$panel
    for (yc in h$outs) {
      if (!yc %in% names(pn)) next
      r <- run_one(pn, yc, h$mode, h$refyr)
      if (is.null(r)) next
      out_rows[[length(out_rows)+1]] <- tibble(
        dataset = h$ds, outcome = yc, variant = variant,
        dropped = paste(drop, collapse = "|"),
        beta = r$beta, se = r$se, p = r$p, sig = stars(r$p), n = r$n)
    }
  }
}

out <- bind_rows(out_rows)
write_csv(out, "district-analysis/output/tab/robustness_drop_urban.csv")
cat(sprintf("\nWrote %d rows to robustness_drop_urban.csv\n", nrow(out)))
cat(sprintf("Elapsed: %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))
