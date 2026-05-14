###############################################################################
# build_trade_ssiv.R
# ─────────────────────────────────────────────────────────────────────────────
# Build the Khanna §IIIC trade shift-share variable from raw Comtrade flows.
#
# Inputs (produced by script/optional/fetch_comtrade.R):
#   data/clean/instrument/trade_baseline_partner_industry.csv
#
# Plus existing in repo:
#   data/clean/instrument/instrument_mun.csv     (FX shifters Z_dt + 2001 weights)
#   data/clean/forex_2000_2023.csv               (raw FX panel)
#   data/clean/census/census_outcomes_municipality.csv  (industry employment shares)
#
# Output:
#   data/clean/instrument/trade_ssiv.csv
#     columns: lgcode, year, trade_ssiv_imp, trade_ssiv_exp
#
# Formula (Khanna eq. 5):
#   Shiftshare^trade_ot = Σ_d Σ_j (L_jo / Pop_o) · M_jd^baseline · ΔR_dt
#
# Run from project root:
#   source("script/optional/build_trade_ssiv.R")
###############################################################################

.req <- c("tidyverse", "here", "countrycode")
.miss <- setdiff(.req, rownames(installed.packages()))
if (length(.miss)) {
  install.packages(.miss, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(countrycode)
})

ROOT <- tryCatch(here::here(), error = function(e) getwd())
data_path <- function(...) file.path(ROOT, "data", "clean", ...)

# ── Lightweight HS-2-digit → broad industry crosswalk ──────────────────────
# Mirrors script/build_trade_ssiv.py exactly.
HS2_TO_IND <- tibble::tribble(
  ~hs2,  ~industry,
  "01","agri","02","agri","03","agri","04","agri","05","agri","06","agri",
  "07","agri","08","agri","09","agri","10","agri","11","agri","12","agri",
  "13","agri","14","agri","15","agri","16","agri","17","agri","18","agri",
  "19","manuf","20","manuf","21","manuf","22","manuf","23","agri","24","agri",
  "25","manuf","26","manuf","27","manuf",
  "28","manuf","29","manuf","30","manuf","31","manuf","32","manuf","33","manuf",
  "34","manuf","35","manuf","36","manuf","37","manuf","38","manuf",
  "39","manuf","40","manuf",
  "41","manuf","42","manuf","43","manuf","44","manuf","45","manuf","46","manuf",
  "47","manuf","48","manuf","49","manuf",
  "50","manuf","51","manuf","52","manuf","53","manuf","54","manuf","55","manuf",
  "56","manuf","57","manuf","58","manuf","59","manuf","60","manuf",
  "61","manuf","62","manuf","63","manuf","64","manuf","65","manuf",
  "66","manuf","67","manuf","68","manuf","69","manuf","70","manuf",
  "71","manuf","72","manuf","73","manuf","74","manuf","75","manuf",
  "76","manuf","78","manuf","79","manuf","80","manuf","81","manuf",
  "82","manuf","83","manuf","84","manuf","85","manuf",
  "86","transport","87","transport","88","transport","89","transport","90","manuf",
  "91","manuf","92","manuf","93","manuf","94","manuf","95","manuf","96","manuf","97","manuf"
)

IND_COLS_FOR_LJO <- c(
  agri      = "ind_agri_forestry_fish",
  manuf     = "ind_manufacturing",
  transport = "ind_transport_accommodation"
)

# ── 1. Aggregate raw Comtrade flows to (partner, industry) baseline averages
raw_path <- data_path("instrument", "trade_baseline_partner_industry.csv")
if (!file.exists(raw_path)) {
  stop("Run script/optional/fetch_comtrade.R first; ", raw_path, " not found.")
}
raw <- read_csv(raw_path, show_col_types = FALSE)

# Tolerate both comtradr's native names (partner_desc, cmd_code, primary_value,
# flow_desc) and the Python pipeline's trade_baseline_partner_industry.csv
# (partnerDesc, cmdCode, primaryValue, flow as M/X).
to_canon <- function(df) {
  alias <- c(
    partner_desc   = "partnerDesc",
    cmd_code       = "cmdCode",
    primary_value  = "primaryValue",
    flow_desc      = "flow_long",
    flowDesc       = "flow_long",
    flow           = "flow",     # already short M/X
    period         = "year"
  )
  for (src in names(alias)) {
    if (src %in% names(df) && !(alias[[src]] %in% names(df))) {
      df[[alias[[src]]]] <- df[[src]]
    }
  }
  # Map comtradr's flow_desc ("Import"/"Export") to short M/X if needed
  if (!"flow" %in% names(df) && "flow_long" %in% names(df)) {
    df$flow <- ifelse(tolower(df$flow_long) == "import", "M",
                ifelse(tolower(df$flow_long) == "export", "X", NA_character_))
  }
  df
}
raw <- to_canon(raw)

# Sanity check
need <- c("partnerDesc","cmdCode","primaryValue","flow")
miss <- setdiff(need, names(raw))
if (length(miss)) {
  stop("trade_baseline_partner_industry.csv is missing columns: ",
       paste(miss, collapse = ", "), ". Got: ",
       paste(names(raw), collapse = ", "))
}

# ── Harmonise partner names so they match the FX panel ─────────────────────
#   countrycode handles most cases; manual fixups for Comtrade quirks +
#   Eurozone aggregation (the FX panel uses "Eurozone", not per-country).
EUROZONE <- c("Austria","Belgium","France","Germany","Italy","Spain",
              "Netherlands","Finland","Greece","Portugal","Ireland",
              "Luxembourg","Slovakia","Slovenia","Cyprus","Estonia",
              "Latvia","Lithuania","Malta")
DROP_AGG <- c("World","Areas, nes","Other Asia, nes","Other Europe, nes",
              "Other Africa, nes","Special Categories","Free Zones",
              "Bunkers","Br. Antarctic Terr.","Other Asia")

raw <- raw |>
  mutate(partnerStd = countrycode(partnerDesc,
                                  origin      = "country.name",
                                  destination = "country.name",
                                  warn        = FALSE),
         # Manual overrides (countrycode misses some Comtrade names)
         partnerStd = case_when(
           partnerDesc == "China, Hong Kong SAR" ~ "Hong Kong SAR China",
           partnerDesc == "China, Macao SAR"     ~ "Macao SAR China",
           partnerDesc == "Rep. of Korea"        ~ "South Korea",
           TRUE ~ partnerStd
         ),
         # Eurozone aggregation
         partnerStd = if_else(partnerStd %in% EUROZONE, "Eurozone", partnerStd)) |>
  filter(!partnerDesc %in% DROP_AGG, !is.na(partnerStd))

cat("Partner harmonisation: ", n_distinct(raw$partnerDesc), " raw → ",
    n_distinct(raw$partnerStd), " standardised\n", sep = "")

raw <- raw |>
  mutate(hs2 = stringr::str_pad(as.character(cmdCode), 2, pad = "0")) |>
  left_join(HS2_TO_IND, by = "hs2") |>
  mutate(industry = tidyr::replace_na(industry, "manuf"))

base <- raw |>
  group_by(partner = partnerStd, industry, flow) |>
  summarise(value_usd = mean(primaryValue, na.rm = TRUE), .groups = "drop")

imp <- base |> filter(flow == "M") |> select(-flow) |> rename(imp_usd = value_usd)
exp <- base |> filter(flow == "X") |> select(-flow) |> rename(exp_usd = value_usd)
trade_pij <- full_join(imp, exp, by = c("partner", "industry")) |>
  mutate(across(c(imp_usd, exp_usd), ~ tidyr::replace_na(., 0)))

# ── 2. Build FX shifter Z_dt for each (partner, year)
fx <- read_csv(data_path("forex_2000_2023.csv"), show_col_types = FALSE)
nepal <- fx |> filter(country == "Nepal") |>
  select(year, npr_usd = forex)
fx <- fx |>
  left_join(nepal, by = "year") |>
  mutate(fx_to_npr = forex / npr_usd)
fx_2001 <- fx |> filter(year == 2001) |>
  select(country, fx_2001 = fx_to_npr)
fx <- fx |>
  left_join(fx_2001, by = "country") |>
  mutate(fx_index_2001 = fx_to_npr / fx_2001)

trade_pij_yr <- trade_pij |>
  inner_join(fx, by = c("partner" = "country")) |>
  mutate(imp_x_z = imp_usd * (fx_index_2001 - 1),
         exp_x_z = exp_usd * (fx_index_2001 - 1))

# ── 3. Aggregate to (industry, year) totals
ind_yr <- trade_pij_yr |>
  group_by(industry, year) |>
  summarise(imp_x_z = sum(imp_x_z, na.rm = TRUE),
            exp_x_z = sum(exp_x_z, na.rm = TRUE),
            .groups = "drop")

# ── 4. Apportion to municipality via 2001 employment levels
inst <- read_csv(data_path("instrument", "instrument_mun.csv"), show_col_types = FALSE) |>
  distinct(lgcode, .keep_all = TRUE) |>
  select(lgcode, geog_pop_2001)
out <- read_csv(data_path("census",     "census_outcomes_municipality.csv"),
                show_col_types = FALSE)

emp <- out |> filter(year == 2001) |>
  select(lgcode, all_of(unname(IND_COLS_FOR_LJO))) |>
  inner_join(inst, by = "lgcode")

# Build long format with L_<industry>_o columns
for (i in seq_along(IND_COLS_FOR_LJO)) {
  ind_tag <- names(IND_COLS_FOR_LJO)[i]
  src_col <- IND_COLS_FOR_LJO[i]
  emp[[paste0("L_", ind_tag, "_o")]] <- tidyr::replace_na(emp[[src_col]], 0) * emp$geog_pop_2001
}
emp_long <- emp |>
  select(lgcode, geog_pop_2001, starts_with("L_")) |>
  pivot_longer(starts_with("L_"),
               names_to  = "industry",
               values_to = "L_jo") |>
  mutate(industry = stringr::str_replace_all(industry, "^L_|_o$", ""))

# Khanna (Eq. 5) apportions industry-destination shocks to provinces by the
# province's SHARE of NATIONAL industry employment (L_jo / L_j), divided by
# population (1/Pop_o). So compute the national industry totals first.
L_j <- emp_long |>
  group_by(industry) |>
  summarise(L_j = sum(L_jo, na.rm = TRUE), .groups = "drop")

emp_long <- emp_long |>
  left_join(L_j, by = "industry") |>
  mutate(emp_share_of_natl = if_else(L_j > 0, L_jo / L_j, 0),
         weight            = emp_share_of_natl / geog_pop_2001)
            # weight = (L_jo / L_j) · (1 / Pop_o)  per Khanna Eq. (5)

merged <- ind_yr |>
  inner_join(emp_long, by = "industry") |>
  mutate(imp_share = imp_x_z * weight,
         exp_share = exp_x_z * weight)

trade_ssiv <- merged |>
  group_by(lgcode, year) |>
  summarise(trade_ssiv_imp = sum(imp_share, na.rm = TRUE),
            trade_ssiv_exp = sum(exp_share, na.rm = TRUE),
            .groups = "drop")

out_path <- data_path("instrument", "trade_ssiv.csv")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write.csv(trade_ssiv, out_path, row.names = FALSE)
cat(sprintf("Wrote %d muni-year rows to %s\n", nrow(trade_ssiv), out_path))
print(summary(trade_ssiv |> select(trade_ssiv_imp, trade_ssiv_exp)))
