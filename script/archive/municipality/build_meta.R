###############################################################################
# build_meta.R — produces docs/meta.json (definitions + summary statistics)
# R/RStudio companion to script/build_meta.py.
#
# Run: source("script/build_meta.R")
###############################################################################

.req <- c("tidyverse","jsonlite","here")
.miss <- setdiff(.req, rownames(installed.packages()))
if (length(.miss)) install.packages(.miss, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(here)
})

ROOT <- tryCatch(here::here(), error = function(e) getwd())

cb_census <- read_csv(file.path(ROOT, "data/clean/census/outcomes_census_codebook.csv"),
                      show_col_types = FALSE)

# RVS codebook (data/rvs_codebook.csv): category, variable, unit, reference,
# source, definition. Reshape to match census-codebook columns so a single
# cb_idx works for both datasets.
rvs_cb_path <- file.path(ROOT, "data/rvs_codebook.csv")
if (file.exists(rvs_cb_path)) {
  cb_rvs <- read_csv(rvs_cb_path, show_col_types = FALSE) |>
    transmute(variable,
              category,
              definition,
              universe         = reference,
              source_vars_2001 = source,
              source_vars_2011 = source,
              source_vars_2021 = source)
  cb <- bind_rows(cb_census, cb_rvs)
} else {
  cb <- cb_census
}

out <- read_csv(file.path(ROOT, "data/clean/census/census_outcomes_municipality.csv"),
                show_col_types = FALSE)
res <- jsonlite::read_json(file.path(ROOT, "docs/results.json"))
ours <- res$outcomes

cb_idx <- cb |> distinct(variable, .keep_all = TRUE) |> column_to_rownames("variable")

SHORT_FALLBACK <- c(
  amen_water_piped         = "Share of HHs with piped/tap drinking water.",
  amen_water_traditional   = "Share of HHs with traditional water source (well, river, etc.).",
  ind_arts_recreation      = "Workforce share in arts/recreation industry (ISIC R+S).",
  ind_health               = "Workforce share in health/social work (ISIC Q).",
  ind_transport_accommodation = "Workforce share in transport + accommodation (ISIC H+I).",
  housing_roof_modern      = "Share of HHs with modern roof material (concrete/metal/tile).",
  housing_roof_traditional = "Share of HHs with traditional roof (thatch/wood/mud).",
  housing_foundation_modern= "Share of HHs with modern foundation (concrete/brick).",
  housing_foundation_traditional = "Share of HHs with traditional foundation (mud/stone).",
  housing_own              = "Share of HHs that own their dwelling.",
  housing_rented           = "Share of HHs renting their dwelling.",
  fem_employment_rate      = "Female employment rate (employed / female pop 15-60).",
  fem_share_of_ag_workers  = "Female share among agricultural workers.",
  fem_ag_specialization_ratio = "Ratio of female agri share to overall agri share.",
  fem_wage_share_of_employment = "Share of female employment in wage work.",
  mlfp_agri                = "Male LFP share in agriculture.",
  mlfp_nonagri             = "Male LFP share in non-agriculture.",
  flfp_agri                = "Female LFP in agriculture.",
  flfp_nonagri             = "Female LFP in non-agriculture.",
  flfp_wage                = "Female LFP in wage employment.",
  flfp_chores_only         = "Share of women whose main activity is chores.",
  gap_lfp_m_minus_f        = "Male - female LFP gap (percentage points).",
  gap_nonagri_m_minus_f    = "Male - female non-agri LFP gap (pp).",
  head_age_mean            = "Mean age of household head.",
  head_elderly_share       = "Share of HHs with head aged 60+.",
  head_young_share         = "Share of HHs with head aged < 30.",
  head_female_share        = "Share of female-headed HHs.",
  head_female_elderly      = "Share of HHs with female elderly head.",
  absent_hh_share          = "Share of HHs with at least one absentee member.",
  share_men                = "Male share of total population.",
  share_women              = "Female share of total population.",
  left_not_with_both       = "Share of children not living with both parents.",
  left_mother_only         = "Share of children living with mother only.",
  left_father_only         = "Share of children living with father only.",
  left_with_relatives      = "Share of children living with relatives only.",
  left_without_parents     = "Share of children living without either parent."
)

definitions <- list(); summary_stats <- list()

for (y in names(ours)) {
  meta <- ours[[y]]
  label <- meta$label; group <- meta$group

  if (y %in% rownames(cb_idx)) {
    row <- cb_idx[y, ]
    defn <- as.character(row$definition)
    short <- if (!is.na(defn) && nchar(defn))
                paste0(strsplit(defn, "\\.")[[1]][1], ".") else paste0(label, ".")
    long_ <- if (!is.na(defn) && nchar(defn)) defn else label
    universe <- as.character(row$universe); if (is.na(universe)) universe <- ""
    src01 <- as.character(row$source_vars_2001); if (is.na(src01)) src01 <- ""
    src11 <- as.character(row$source_vars_2011); if (is.na(src11)) src11 <- ""
    src21 <- as.character(row$source_vars_2021); if (is.na(src21)) src21 <- ""
  } else {
    short <- if (y %in% names(SHORT_FALLBACK)) SHORT_FALLBACK[[y]] else paste0(label, ".")
    long_ <- short
    universe <- ""; src01 <- ""; src11 <- ""; src21 <- ""
  }
  definitions[[y]] <- list(
    label    = label,  group    = group,
    short    = short,  long     = long_,
    universe = universe,
    src_2001 = src01, src_2011 = src11, src_2021 = src21
  )

  # Summary stats per year
  s <- list()
  for (yr in c(2001, 2011, 2021)) {
    d <- out |> filter(year == yr) |> pull(.data[[y]])
    s[[paste0("n_", yr)]]    <- sum(!is.na(d))
    s[[paste0("mean_", yr)]] <- if (any(!is.na(d))) mean(d, na.rm = TRUE) else NA
    s[[paste0("sd_", yr)]]   <- if (sum(!is.na(d)) > 1) sd(d, na.rm = TRUE) else NA
    s[[paste0("min_", yr)]]  <- if (any(!is.na(d))) min(d, na.rm = TRUE) else NA
    s[[paste0("max_", yr)]]  <- if (any(!is.na(d))) max(d, na.rm = TRUE) else NA
  }
  summary_stats[[y]] <- s
}

meta_out <- list(
  definitions = definitions,
  summary     = summary_stats,
  groups      = unique(unlist(lapply(ours, `[[`, "group")))
)
write_json(meta_out, file.path(ROOT, "docs/meta.json"),
           auto_unbox = TRUE, na = "null", null = "null", digits = NA)
cat(sprintf("Wrote %d definitions, %d summary entries.\nFile: %s\n",
            length(definitions), length(summary_stats),
            file.path(ROOT, "docs/meta.json")))
