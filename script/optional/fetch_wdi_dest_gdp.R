###############################################################################
# fetch_wdi_dest_gdp.R
# ─────────────────────────────────────────────────────────────────────────────
# Fetch 2001 destination-country GDP per capita (constant 2015 USD) from the
# World Bank API and save to data/clean/instrument/wdi_dest_gdp_2001.csv.
#
# Once produced, this file (combined with optional/build_trade_ssiv.R + the
# patched script/vars/instrument.R region-shares output) unlocks the
# `khanna` / `khanna_full` control sets in build_results.py / .R.
#
# Run from project root:
#   source("script/optional/fetch_wdi_dest_gdp.R")
###############################################################################

.req <- c("WDI", "tidyverse", "here")
.miss <- setdiff(.req, rownames(installed.packages()))
if (length(.miss)) {
  message("Installing required packages: ", paste(.miss, collapse = ", "))
  install.packages(.miss, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(WDI)
  library(tidyverse)
  library(here)
})

ROOT <- tryCatch(here::here(), error = function(e) getwd())

# Country list mirrors instrument.R country_names + the destinations Khanna's
# block A wants conditioned on (region group, OECD-status, etc.).
DEST_ISO3 <- c(
  # Gulf
  "Saudi Arabia"          = "SAU", "Qatar"             = "QAT",
  "United Arab Emirates"  = "ARE", "Kuwait"            = "KWT",
  "Bahrain"               = "BHR", "Oman"              = "OMN",
  # Other West Asia
  "Israel"                = "ISR", "Lebanon"           = "LBN",
  "Jordan"                = "JOR",
  # East Asia
  "Korea, Rep."           = "KOR", "Japan"             = "JPN",
  "China"                 = "CHN", "Hong Kong"         = "HKG",
  # Southeast Asia
  "Malaysia"              = "MYS", "Singapore"         = "SGP",
  "Thailand"              = "THA",
  # South Asia
  "Pakistan"              = "PAK", "Bangladesh"        = "BGD",
  "Bhutan"                = "BTN", "Sri Lanka"         = "LKA",
  "Maldives"              = "MDV",
  # OECD-North & Pacific
  "United States"         = "USA", "Canada"            = "CAN",
  "Mexico"                = "MEX", "Australia"         = "AUS",
  "New Zealand"           = "NZL",
  # OECD-Europe
  "United Kingdom"        = "GBR", "Germany"           = "DEU",
  "France"                = "FRA", "Italy"             = "ITA",
  "Spain"                 = "ESP", "Portugal"          = "PRT",
  "Netherlands"           = "NLD", "Belgium"           = "BEL",
  "Sweden"                = "SWE", "Romania"           = "ROU",
  "Croatia"               = "HRV", "Malta"             = "MLT",
  "Poland"                = "POL",
  # Other
  "Russian Federation"    = "RUS"
)

cat("Fetching WDI NY.GDP.PCAP.KD (constant 2015 USD) for ",
    length(DEST_ISO3), " countries, year 2001...\n", sep = "")

raw <- WDI::WDI(country   = unname(DEST_ISO3),
                indicator = "NY.GDP.PCAP.KD",
                start     = 2001,
                end       = 2001,
                extra     = FALSE)

inv <- setNames(names(DEST_ISO3), unname(DEST_ISO3))
df <- raw |>
  rename(gdp_pc_2001 = NY.GDP.PCAP.KD, iso3 = iso3c) |>
  mutate(country = inv[iso3]) |>
  select(country, iso3, gdp_pc_2001) |>
  arrange(country)

out_path <- file.path(ROOT, "data/clean/instrument/wdi_dest_gdp_2001.csv")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write.csv(df, out_path, row.names = FALSE)
cat(sprintf("Wrote %d rows to %s\n", nrow(df), out_path))
print(head(df, 10))
