################################################################################
# Bootstrap: create all output directories that the muni pipeline writes to.
# Run this once before sourcing any of the variable-creation or robustness
# scripts to avoid the "cannot open file" / "No such file or directory" errors.
#
# Usage: source("script/_setup_dirs.R")
################################################################################

dirs <- c(
  "data/clean/census",
  "data/clean/nec2018",
  "data/clean/instrument",
  "data/clean/rvs_outcomes",
  "data/clean/archive/municipality/rvs_outcomes",   # archive sometimes used
  "output/tab",
  "output/fig",
  "docs"
)

created <- character()
for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    created <- c(created, d)
  }
}

if (length(created)) {
  cat("Created directories:\n")
  for (d in created) cat("  +", d, "\n")
} else {
  cat("All output directories already exist.\n")
}
