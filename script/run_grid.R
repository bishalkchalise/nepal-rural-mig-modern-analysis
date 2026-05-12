# =============================================================================
# script/run_grid.R — run a full grid of specifications and aggregate the
# results into one master CSV per dataset.  Outputs:
#
#   output/tab/individual/   <- one CSV per (spec, dataset, threshold)
#   output/tab/grid_census.csv   <- ALL census specs stacked (long format)
#   output/tab/grid_hh.csv       <- ALL hh     specs stacked (long format)
#
# Each master CSV has the same column shape as a single run_spec output,
# with the `spec` column distinguishing the runs.  Sort / pivot / filter on
# (outcome, spec, threshold) to compare stability across specifications.
#
# RUNTIME: census grid ~5-15 min, hh grid ~20-40 min on a laptop.
# Edit DATASETS / GRID below to subset before re-running.
# =============================================================================

source("script/_specs_lib.R")

# ---------------------------------------------------------------------------
# Datasets to run.  Comment out one if you only want the other.
# ---------------------------------------------------------------------------
DATASETS <- c("census", "hh")

# ---------------------------------------------------------------------------
# Grid of specifications to estimate.  Edit freely.
#
#   threshold  : migrant-count cutoff (4 values)
#   c_mig      : year x mig_intensity term on/off (the "second term")
#   block_set  : preset bundles of control blocks
#                  "none"        -- treatment + (optional C_mig) + FE only
#                  "blockA_only" -- + dest-weighted baseline X x year
#                  "khanna_full" -- + Block A + Block B + Block C (trade SSIV)
#   treatment  : functional form of the interaction
#                  "log_int"  -- fx_z * log(mig_int_z)   <- main
#                  "lin_int"  -- fx_z * mig_int_z        <- robustness
#
# Full grid below = 4 x 2 x 3 x 2 = 48 specs per dataset.
# c_fx is forced ON in every spec (year x fx trend always partialled out).
# ---------------------------------------------------------------------------
GRID <- expand.grid(
  threshold = c(0L, 25L, 50L, 100L),
  c_mig     = c(FALSE, TRUE),
  block_set = c("none", "blockA_only", "khanna_full"),
  treatment = c("log_int", "lin_int"),
  stringsAsFactors = FALSE
)

# Map block_set -> three booleans.
block_flags <- function(set) {
  list(
    "none"        = c(c_block_a = FALSE, c_block_b = FALSE, c_block_c = FALSE),
    "blockA_only" = c(c_block_a = TRUE,  c_block_b = FALSE, c_block_c = FALSE),
    "khanna_full" = c(c_block_a = TRUE,  c_block_b = TRUE,  c_block_c = TRUE)
  )[[set]]
}

# Build a stable, machine-readable spec_label per grid row.
make_label <- function(treatment, c_mig, c_fx, block_set) {
  paste0(
    "tx_", treatment,
    if (c_mig)  "_cmig" else "_nocmig",
    if (c_fx)   "_cfx"  else "_nocfx",
    "_blk_", block_set
  )
}

# Individual CSVs go here (one per spec, fast to grep through).
INDIV_DIR <- file.path(ROOT, "output", "tab", "individual")
dir.create(INDIV_DIR, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Run the grid for each dataset.
# ---------------------------------------------------------------------------
master <- list()   # collects every result table

for (ds in DATASETS) {
  cat("\n\n##############################################################\n")
  cat("##   DATASET = ", ds, "\n", sep = "")
  cat("##   grid    = ", nrow(GRID), " specs\n", sep = "")
  cat("##############################################################\n\n")

  ds_rows <- vector("list", nrow(GRID))
  t_start <- Sys.time()

  for (i in seq_len(nrow(GRID))) {
    row   <- GRID[i, ]
    flags <- block_flags(row$block_set)
    label <- make_label(row$treatment, row$c_mig, c_fx = TRUE, row$block_set)
    out_path <- file.path(INDIV_DIR,
                          sprintf("%s_%s_thr%d.csv", label, ds, row$threshold))

    cat(sprintf("\n[%d/%d] %s | %s | thr=%d\n",
                i, nrow(GRID), ds, label, row$threshold))

    res <- tryCatch(
      run_spec(
        spec_label  = label,
        dataset     = ds,
        threshold   = row$threshold,
        treatment   = row$treatment,
        c_mig       = row$c_mig,
        c_fx        = TRUE,
        c_block_a   = flags["c_block_a"],
        c_block_b   = flags["c_block_b"],
        c_block_c   = flags["c_block_c"],
        outcomes    = NULL,           # full default catalogue
        output_path = out_path
      ),
      error = function(e) {
        message("  run_spec failed: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(res)) {
      # Stamp grid metadata so we can recover the dial settings from the master
      res[, `:=`(grid_c_mig     = row$c_mig,
                 grid_c_fx      = TRUE,
                 grid_block_set = row$block_set,
                 grid_treatment = row$treatment)]
      ds_rows[[i]] <- res
    }
  }

  combined <- rbindlist(Filter(Negate(is.null), ds_rows), fill = TRUE)
  master_path <- file.path(ROOT, "output", "tab",
                           sprintf("grid_%s.csv", ds))
  fwrite(combined, master_path)
  master[[ds]] <- combined

  cat(sprintf("\n>>>  %s grid done: %d rows -> %s\n",
              ds, nrow(combined), master_path))
  cat(sprintf("     wallclock = %.1f min\n",
              as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
}

cat("\n\n==============================================================\n")
cat("  GRID RUN COMPLETE\n")
cat("==============================================================\n")
for (ds in names(master))
  cat(sprintf("  %-7s  %5d cells   ->  output/tab/grid_%s.csv\n",
              ds, nrow(master[[ds]]), ds))
cat(sprintf("  Individual CSVs:  %s/\n", INDIV_DIR))
cat("\nTo push the results to git, in RStudio Terminal or Git Bash:\n\n")
cat("  git add output/tab/grid_*.csv output/tab/individual/\n")
cat("  git commit -m \"Grid run: all specs x thresholds, both datasets\"\n")
cat("  git push\n\n")
