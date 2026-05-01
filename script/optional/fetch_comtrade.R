###############################################################################
# fetch_comtrade.R
# ─────────────────────────────────────────────────────────────────────────────
# Fetch UN Comtrade Nepal trade flows by partner × HS-2-digit, 1995-2001
# (the baseline period for the Khanna-style trade SSIV).
#
# Usage:
#   1. Get a free API key at https://uncomtrade.org/ (recommended)
#      and store it as the env var COMTRADE_API_KEY.
#   2. Run from project root:
#         source("script/optional/fetch_comtrade.R")
#
# Output:
#   data/clean/instrument/trade_baseline_partner_industry.csv
#     columns: partner, partner_iso3, cmdCode (HS2), period (year),
#              flow ("M" import / "X" export), primaryValue (USD)
#
# Then run script/optional/build_trade_ssiv.R to aggregate to municipality-year.
###############################################################################

.req <- c("comtradr", "tidyverse", "here")
.miss <- setdiff(.req, rownames(installed.packages()))
if (length(.miss)) {
  message("Installing required packages: ", paste(.miss, collapse = ", "))
  install.packages(.miss, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(comtradr)
  library(tidyverse)
  library(here)
})

ROOT <- tryCatch(here::here(), error = function(e) getwd())

API_KEY  <- Sys.getenv("COMTRADE_API_KEY",          unset = "")
API_KEY2 <- Sys.getenv("COMTRADE_API_KEY_SECONDARY", unset = "")
if (nzchar(API_KEY)) {
  set_primary_comtrade_key(API_KEY)
  if (nzchar(API_KEY2)) {
    message("Primary Comtrade key set. (comtradr has no built-in secondary-key ",
            "fallback — keep your secondary key in COMTRADE_API_KEY_SECONDARY ",
            "and re-source this script with it as primary if the primary is ",
            "rate-limited.)")
  } else {
    message("Using primary Comtrade key.")
  }
} else {
  message("No COMTRADE_API_KEY env var set — using public anonymous endpoint ",
          "(low rate limit; some series may be unavailable). Set the env var ",
          "for a much higher rate limit.")
}

REPORTER <- "NPL"   # ISO3 code for Nepal — comtradr's new backend rejects "Nepal"
YEARS    <- 1995:2001

# HS2 codes 01-97 (HS chapter list; 77 is reserved/unused but harmless to include)
HS2 <- sprintf("%02d", 1:97)

# comtradr's new (v0.4+) backend rejects "AG2" as a commodity code, so we have
# to pass HS2 codes explicitly. Batch into chunks of ~20 to avoid URL-length /
# rate-limit issues.
HS2_CHUNKS <- split(HS2, ceiling(seq_along(HS2) / 20))

cat("Fetching Nepal Comtrade flows (HS2) for years ",
    paste(range(YEARS), collapse = "-"), " ",
    "(", length(HS2_CHUNKS), " HS2 chunks × ", length(YEARS), " years × 2 flows)\n",
    sep = "")

frames <- list()
for (yr in YEARS) {
  for (flow in c("Import", "Export")) {
    for (chunk in HS2_CHUNKS) {
      res <- tryCatch(
        ct_get_data(
          type            = "goods",
          frequency       = "A",
          commodity_classification = "HS",
          commodity_code  = chunk,
          flow_direction  = flow,
          reporter        = REPORTER,
          partner         = "everything",
          start_date      = yr,
          end_date        = yr,
          verbose         = FALSE
        ),
        error = function(e) e
      )
      if (inherits(res, "error")) {
        message(sprintf("  %d %s [%s..%s]: ERROR %s",
                        yr, flow, chunk[1], tail(chunk, 1),
                        conditionMessage(res)))
        next
      }
      if (is.null(res) || nrow(res) == 0) next
      res$year <- as.integer(yr)
      res$flow <- if (flow == "Import") "M" else "X"
      frames[[length(frames) + 1L]] <- res
      Sys.sleep(0.5)  # polite pacing
    }
    cat(sprintf("  %d %s done\n", yr, flow))
  }
}

if (length(frames) == 0) {
  stop("No data fetched. Check API key and rate limits.")
}

raw <- dplyr::bind_rows(frames)

# build_trade_ssiv.R handles comtradr's native column names directly,
# so we don't need to rename here. Save raw output as-is.

out_path <- file.path(ROOT, "data/clean/instrument/trade_baseline_partner_industry.csv")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write.csv(raw, out_path, row.names = FALSE)
cat(sprintf("Wrote %d raw rows to %s\n", nrow(raw), out_path))
cat("Next: source('script/optional/build_trade_ssiv.R') to aggregate to muni-year.\n")
