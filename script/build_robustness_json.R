################################################################################
# R port of build_robustness_json.py.
#
# Reads:
#   output/tab/robustness_final.csv
#   output/tab/robustness_final_fill.csv   (optional)
#   output/tab/outcome_map.csv             (curated outcome -> group/label)
#   docs/results.json                      (optional, for group labels)
#
# Writes: docs/robustness.json
#
# Run from repo root: source("script/build_robustness_json.R")
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})

CSV       <- "output/tab/robustness_final.csv"
CSV_FILL  <- "output/tab/robustness_final_fill.csv"
MAP_CSV   <- "output/tab/outcome_map.csv"
OUT       <- "docs/robustness.json"

if (!file.exists(CSV)) stop(sprintf("%s not found - run robustness_final.R first", CSV))
if (!file.exists(MAP_CSV)) stop(sprintf("%s not found", MAP_CSV))

# ----- Metadata catalogues -----
DATASET_META <- list(
  census    = list(label = "Census municipality panel (2001, 2011, 2021)",
                   entity = "lgcode", ref_year = 2001L, cluster = "lgcode"),
  hh        = list(label = "HRVS household panel (2016, 2017, 2018)",
                   entity = "hhid",   ref_year = 2016L, cluster = "lgcode"),
  nec_panel = list(label = "NEC firm-entry panel (annual 2001-2018)",
                   entity = "lgcode", ref_year = 2001L, cluster = "lgcode"),
  nec_cs    = list(label = "NEC 2018 firm cross-section",
                   entity = "DIST",   ref_year = 2018L, cluster = "DIST")
)
THRESHOLD_LABELS <- list(
  "0"   = "All munis",
  "25"  = ">=25 migrants in 2001",
  "50"  = ">=50 migrants in 2001",
  "100" = ">=100 migrants in 2001"
)

SPECS <- tribble(
  ~spec,                    ~lag, ~treatment_form, ~c_mig_form, ~desc,
  "S0_baseline",            0L,   "log", "log",  "anchor: fx x log(mig_int_z); year x log(mig_int_z)  (log/log)",
  "S_lag1",                 1L,   "log", "log",  "FX shifter lagged 1 year (log/log)",
  "S_lag2",                 2L,   "log", "log",  "FX shifter lagged 2 years (log/log)",
  "S_lag3",                 3L,   "log", "log",  "FX shifter lagged 3 years (log/log)",
  "S_lag4",                 4L,   "log", "log",  "FX shifter lagged 4 years (log/log)",
  "S_lag5",                 5L,   "log", "log",  "FX shifter lagged 5 years (log/log)",
  "S_lag10",                10L,  "log", "log",  "FX shifter lagged 10 years (log/log)",
  "S_both_log",             0L,   "log", "log",  "log/log at lag 0",
  "S_both_log_lag1",        1L,   "log", "log",  "log/log at lag 1y",
  "S_both_log_lag2",        2L,   "log", "log",  "log/log at lag 2y",
  "S_both_log_lag3",        3L,   "log", "log",  "log/log at lag 3y",
  "S_both_log_lag4",        4L,   "log", "log",  "log/log at lag 4y",
  "S_both_log_lag5",        5L,   "log", "log",  "log/log at lag 5y",
  "S_both_log_lag10",       10L,  "log", "log",  "log/log at lag 10y",
  "S_both_linear",          0L,   "lin", "lin",  "lin/lin at lag 0",
  "S_both_linear_lag1",     1L,   "lin", "lin",  "lin/lin at lag 1y",
  "S_both_linear_lag2",     2L,   "lin", "lin",  "lin/lin at lag 2y",
  "S_both_linear_lag3",     3L,   "lin", "lin",  "lin/lin at lag 3y",
  "S_both_linear_lag4",     4L,   "lin", "lin",  "lin/lin at lag 4y",
  "S_both_linear_lag5",     5L,   "lin", "lin",  "lin/lin at lag 5y",
  "S_both_linear_lag10",    10L,  "lin", "lin",  "lin/lin at lag 10y"
)
SPEC_LABEL <- setNames(as.list(SPECS$desc), SPECS$spec)
SPEC_META  <- setNames(
  lapply(seq_len(nrow(SPECS)), function(i) {
    s <- SPECS[i, ]
    list(lag = s$lag, treatment_form = s$treatment_form,
         c_mig_form = s$c_mig_form, desc = s$desc)
  }),
  SPECS$spec
)

# ----- Curated outcome map -----
cmap <- read_csv(MAP_CSV, show_col_types = FALSE) %>%
  mutate(keep_flag = toupper(trimws(as.character(keep))) %in% c("Y","YES","TRUE","1")) %>%
  filter(keep_flag) %>%
  select(dataset, variable, group, label) %>%
  mutate(variable = trimws(variable),
         group    = trimws(group),
         label    = ifelse(is.na(label) | label == "", variable, label))
cat(sprintf("Curated map: %d rows kept\n", nrow(cmap)))

# ----- Optional: dataset-specific group order from results.json -----
results_groups <- list()
rj <- "docs/results.json"
if (file.exists(rj)) {
  rj_data <- jsonlite::fromJSON(rj, simplifyVector = FALSE)
  for (ds in names(rj_data$datasets %||% list())) {
    outc <- rj_data$datasets[[ds]]$outcomes %||% list()
    for (oc in names(outc)) {
      info <- outc[[oc]]
      if (!is.null(info$group))
        results_groups[[paste(ds, oc, sep = "::")]] <-
          list(group = info$group, label = info$label %||% oc)
    }
  }
}

# ----- Load result CSVs -----
df <- read_csv(CSV, show_col_types = FALSE)
if (file.exists(CSV_FILL)) {
  dff <- read_csv(CSV_FILL, show_col_types = FALSE)
  df  <- bind_rows(df, dff)
  cat(sprintf("  + merged %d fill rows from %s\n", nrow(dff), basename(CSV_FILL)))
}
# Numeric coercion
for (c in c("beta","se","pval","mean_y","sd_y","n","n_muni")) {
  if (c %in% names(df)) df[[c]] <- suppressWarnings(as.numeric(df[[c]]))
}
df$threshold <- suppressWarnings(as.integer(df$threshold))
df$err       <- ifelse(is.na(df$err), "", as.character(df$err))

# ----- Assemble output -----
out <- list(
  datasets_meta = setNames(list(), character()),
  thresholds    = THRESHOLD_LABELS,
  families      = list(robustness = list(specs = SPEC_LABEL)),
  spec_meta     = SPEC_META,
  datasets      = setNames(list(), character())
)

for (ds in names(DATASET_META)) {
  meta <- DATASET_META[[ds]]
  sub  <- df %>% filter(dataset == ds)
  if (nrow(sub) == 0) next

  out$datasets_meta[[ds]] <- list(label = meta$label)

  # Curated outcomes for this dataset (preserve declaration order)
  ds_map <- cmap %>% filter(dataset == ds) %>% distinct(variable, .keep_all = TRUE)
  ds_map <- ds_map[ds_map$variable %in% unique(sub$outcome), ]

  outcomes <- setNames(
    lapply(seq_len(nrow(ds_map)), function(i) {
      list(label = ds_map$label[i], group = ds_map$group[i])
    }),
    ds_map$variable
  )
  groups <- unique(ds_map$group)

  # Restrict result rows to curated outcomes
  sub <- sub %>% filter(outcome %in% names(outcomes))

  # Drop outcomes that errored in every spec
  if (nrow(sub) > 0) {
    all_err <- sub %>% group_by(outcome) %>%
      summarise(all_err = all(err != ""), .groups = "drop")
    drop_o <- all_err$outcome[all_err$all_err]
    if (length(drop_o)) {
      outcomes <- outcomes[!names(outcomes) %in% drop_o]
      sub      <- sub %>% filter(!outcome %in% drop_o)
      groups   <- unique(sapply(outcomes, function(x) x$group))
      cat(sprintf("  %s: dropped %d always-error outcomes\n", ds, length(drop_o)))
    }
  }

  # Build estimates: threshold -> robustness -> spec -> outcome -> rec
  est <- list()
  for (thr in sort(unique(sub$threshold))) {
    sub_thr <- sub %>% filter(threshold == thr)
    spec_block <- list()
    for (sp in sort(unique(sub_thr$spec))) {
      sub_sp <- sub_thr %>% filter(spec == sp)
      cells <- list()
      for (i in seq_len(nrow(sub_sp))) {
        r <- sub_sp[i, ]
        rec <- list()
        if (r$err != "") {
          rec$err <- r$err
        } else {
          if (!is.na(r$beta))   rec$beta   <- r$beta
          if (!is.na(r$se))     rec$se     <- r$se
          if (!is.na(r$pval))   rec$pval   <- r$pval
          if (!is.na(r$mean_y)) rec$mean_y <- r$mean_y
          if ("sd_y" %in% names(r) && !is.na(r$sd_y)) rec$sd_y <- r$sd_y
          if (!is.na(r$n))      rec$n      <- as.integer(r$n)
          if (!is.na(r$n_muni)) {
            rec$n_muni <- as.integer(r$n_muni)
            rec$n_unit <- as.integer(r$n_muni)
          }
          if ("interpret" %in% names(r) && !is.na(r$interpret) && nzchar(r$interpret))
            rec$interpret <- r$interpret
        }
        cells[[r$outcome]] <- rec
      }
      spec_block[[sp]] <- cells
    }
    est[[as.character(thr)]] <- list(robustness = spec_block)
  }

  out$datasets[[ds]] <- list(
    label     = meta$label,
    entity    = meta$entity,
    ref_year  = meta$ref_year,
    cluster   = meta$cluster,
    groups    = groups,
    outcomes  = outcomes,
    estimates = est
  )
}

# Write JSON (compact, like the Python version)
dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
write_json(out, OUT, auto_unbox = TRUE, null = "null", na = "null", digits = NA)

sz_kb <- round(file.info(OUT)$size / 1024, 1)
cat(sprintf("Wrote %s  (%.1f KB)\n", OUT, sz_kb))
for (ds in names(out$datasets)) {
  dsd <- out$datasets[[ds]]
  cat(sprintf("  %s: %d outcomes, %d groups, %d thresholds\n",
              ds, length(dsd$outcomes), length(dsd$groups), length(dsd$estimates)))
}
