##############################################################################
# script/nec2018/00_run_all.R
##############################################################################
#
# Master runner: sets the project working directory and sources scripts
# 01-05 in order. Each script depends on the previous one's output, so
# running them individually also works — this just saves a few keystrokes.
#
# HOW TO USE:
#   1. Edit PROJECT_ROOT below to point to your project root (the folder
#      that contains data/raw/ and script/).
#   2. Source this file:
#        source("path/to/00_run_all.R")
#      Or open it in RStudio and click Source.
#
##############################################################################

# ---- EDIT THIS ------------------------------------------------------------
# Path to your project root — the folder that contains data/raw/ and script/
PROJECT_ROOT <- "C:/Users/s222385015/OneDrive - Deakin University (1)\nepal-rural-mig-modern-analysis"




# ---- DON'T EDIT BELOW -----------------------------------------------------

setwd(PROJECT_ROOT)
cat("Working directory set to:\n  ", getwd(), "\n\n")

# Verify the raw data file exists before we go any further
raw_path <- "data/raw/Economic Census 2018/NEC_2018.dta"
if (!file.exists(raw_path)) {
  stop("Cannot find ", raw_path, "\n",
       "Current working dir: ", getwd(), "\n",
       "Edit PROJECT_ROOT at the top of this script.")
}
cat("Found raw data:", raw_path, "\n\n")

# Verify the scripts exist
scripts <- c(
  "script/vars/nec2018/01_classify_nsic.R",
  "script/vars/nec2018/02_firm_level_prep.R",
  "script/vars/nec2018/03_municipality_wide.R",
  "script/vars/nec2018/04_entry_cohort_panel.R",
  "script/vars/nec2018/05_district_aggregate.R"
)
missing <- scripts[!file.exists(scripts)]
if (length(missing)) {
  stop("Missing script files:\n  ", paste(missing, collapse = "\n  "))
}

# ---- Run each in sequence, timing and catching errors --------------------
run_script <- function(path) {
  cat("\n", strrep("=", 76), "\n", sep = "")
  cat("RUNNING: ", path, "\n", sep = "")
  cat(strrep("=", 76), "\n", sep = "")
  t_start <- Sys.time()

  tryCatch(
    source(path, echo = FALSE),
    error = function(e) {
      cat("\n!!! ERROR in", path, ":\n  ", conditionMessage(e), "\n")
      stop("Halting pipeline. Fix the error above and re-run from ",
           path, " onward.", call. = FALSE)
    }
  )

  elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "secs")), 1)
  cat("\nFinished ", path, " in ", elapsed, "s\n", sep = "")
}

pipeline_start <- Sys.time()

for (s in scripts) run_script(s)

total <- round(as.numeric(difftime(Sys.time(), pipeline_start, units = "mins")), 2)
cat("\n", strrep("=", 76), "\n", sep = "")
cat("PIPELINE COMPLETE in ", total, " minutes\n", sep = "")
cat(strrep("=", 76), "\n", sep = "")

cat("\nOutputs written to data/clean/nec2018/:\n")
out_files <- list.files("data/clean/nec2018", full.names = FALSE)
for (f in out_files) {
  size_mb <- round(file.info(file.path("data/clean/nec2018", f))$size / 1e6, 2)
  cat(sprintf("  %-40s %s MB\n", f, size_mb))
}
