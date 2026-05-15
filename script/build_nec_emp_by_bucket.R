# =============================================================================
# script/build_nec_emp_by_bucket.R
#
# Build muni-level EMPLOYMENT aggregates by industry and by size from
# firm-level NEC 2018 microdata.  Currently the clean muni files only have
# emp_total (and emp_surviving, in the cohort panel) — no breakdown by
# industry or size.  PPML on employment-by-bucket therefore can't run
# without this step.
#
# Mirrors the column-naming convention used by:
#   data/clean/nec2018/mun_industry_structure.csv  (n_firms_<industry>)
#   data/clean/nec2018/mun_size_formality.csv      (n_firms_size_<bucket>)
#   output/tab/mun_cohort_stock_post*.csv          (cohort-restricted)
#
# Outputs (4 files, same schema as the inputs they extend):
#   data/clean/nec2018/mun_emp_by_industry.csv
#   data/clean/nec2018/mun_emp_by_size.csv
#   output/tab/mun_emp_by_bucket_post2001.csv
#   output/tab/mun_emp_by_bucket_post2011.csv
#
# Run from repo root (after you've adjusted the INPUT_* constants below):
#   source("script/build_nec_emp_by_bucket.R")
# Wall-clock: < 2 minutes on the firm-level NEC.
# =============================================================================

suppressPackageStartupMessages({ library(data.table) })
options(scipen = 999)
ROOT <- normalizePath(".")

# -----------------------------------------------------------------------------
# 0. CONFIGURE — substitute the actual local paths / column names here.
# -----------------------------------------------------------------------------
# Path to the firm-level NEC microdata (one row per firm).
INPUT_FIRMS_CSV <- "data/raw/nec2018/nec_firms_2018.csv"   # <-- CHANGE

# Required columns in INPUT_FIRMS_CSV:
COL_LGCODE      <- "lgcode"          # 6-digit muni code
COL_EMP         <- "n_workers"       # firm's total employment in 2018
COL_FOUND_YEAR  <- "founding_year_ad"   # 4-digit AD founding year
COL_NSIC        <- "nsic_section"    # NSIC/ISIC section code, e.g. "C","G","I"
                                     # (single letter)  OR change to a numeric code
                                     # and map it in NSIC_TO_GROUP below
COL_STATUS_2018 <- "is_operating_2018"  # logical / 1-0: still operating in 2018

# Industry grouping — maps NSIC section → 10-bucket scheme used in muni files.
# Edit this if your column codes industries differently.
NSIC_TO_GROUP <- c(
  A = "agriculture", B = "utilities_mining",  C = "manufacturing",
  D = "utilities_mining", E = "utilities_mining", F = "construction",
  G = "trade_retail",  H = "transport",         I = "hospitality",
  J = "finance_prof_info", K = "finance_prof_info",
  L = "finance_prof_info", M = "finance_prof_info",
  N = "other_services", O = "social_services", P = "social_services",
  Q = "social_services", R = "other_services",  S = "other_services",
  T = "other_services", U = "other_services"
)

# Size bucket from employment headcount (same thresholds as muni_size_formality)
size_bucket <- function(n) {
  fcase(
    is.na(n) | n < 1,    NA_character_,
    n == 1,              "size_1_worker",
    n >= 2  & n <= 9,    "size_2_9_workers",
    n >= 10 & n <= 50,   "size_10_50_workers",
    n >= 51,             "size_51plus_workers"
  )
}

# -----------------------------------------------------------------------------
# 1. Load firm-level data + classify each firm
# -----------------------------------------------------------------------------
firms <- fread(INPUT_FIRMS_CSV)
setnames(firms,
         c(COL_LGCODE, COL_EMP, COL_FOUND_YEAR, COL_NSIC, COL_STATUS_2018),
         c("lgcode", "emp", "founding_year_ad", "nsic", "is_operating_2018"))
firms[, lgcode := as.integer(lgcode)]
firms[, emp    := as.numeric(emp)]
firms[, emp    := pmax(emp, 0, na.rm = TRUE)]
firms[, founding_year_ad := as.integer(founding_year_ad)]

# Industry grouping
firms[, industry := NSIC_TO_GROUP[as.character(nsic)]]
firms[is.na(industry), industry := "other_services"]
# Composite buckets matching muni_industry_structure.csv:
firms[, industry_grp := industry]   # already grouped above

# Size bucket
firms[, size_grp := size_bucket(emp)]

cat(sprintf("Loaded %s firms.  Industries: %s.  Size buckets: %s.\n",
            format(nrow(firms), big.mark = ","),
            paste(unique(firms$industry_grp), collapse = ", "),
            paste(unique(firms$size_grp),     collapse = ", ")))

# -----------------------------------------------------------------------------
# 2. Helper — pivot+sum employment to muni × bucket
# -----------------------------------------------------------------------------
emp_by_bucket <- function(d, bucket_col, prefix) {
  # rows: lgcode  ·  cols: prefix_<bucket>  ·  values: sum(emp)
  long <- d[!is.na(get(bucket_col)),
            .(emp = sum(emp, na.rm = TRUE)),
            by = .(lgcode, bucket = get(bucket_col))]
  wide <- dcast(long, lgcode ~ bucket, value.var = "emp",
                fun.aggregate = sum, fill = 0)
  setnames(wide, setdiff(names(wide), "lgcode"),
                  paste0(prefix, setdiff(names(wide), "lgcode")))
  # Also add an overall total to match the existing emp_total convention
  tot <- d[, .(emp_total = sum(emp, na.rm = TRUE)), by = lgcode]
  merge(wide, tot, by = "lgcode", all = TRUE)
}

# -----------------------------------------------------------------------------
# 3. Full-stock aggregates (all firms operating in 2018)
# -----------------------------------------------------------------------------
stock <- firms[is_operating_2018 == 1 | is_operating_2018 == TRUE]

emp_ind  <- emp_by_bucket(stock, "industry_grp", prefix = "emp_")
emp_sz   <- emp_by_bucket(stock, "size_grp",     prefix = "emp_")

# Composite industry groups (match muni_industry_structure.csv naming)
emp_ind[, emp_finance_prof_info  := emp_finance_prof_info]   # already grouped
# (other groups are 1-to-1 with NSIC_TO_GROUP output)

dir.create(file.path(ROOT, "data/clean/nec2018"), recursive = TRUE, showWarnings = FALSE)
fwrite(emp_ind, "data/clean/nec2018/mun_emp_by_industry.csv")
fwrite(emp_sz,  "data/clean/nec2018/mun_emp_by_size.csv")
cat("Wrote: data/clean/nec2018/mun_emp_by_industry.csv\n")
cat("Wrote: data/clean/nec2018/mun_emp_by_size.csv\n")

# -----------------------------------------------------------------------------
# 4. Cohort-restricted versions — same recipe as build_nec_cohort_stocks.R
# -----------------------------------------------------------------------------
build_cohort_emp <- function(min_year, out_path) {
  d <- firms[is_operating_2018 == 1 | is_operating_2018 == TRUE]
  d <- d[founding_year_ad >= min_year]
  ind <- emp_by_bucket(d, "industry_grp", prefix = "emp_")
  sz  <- emp_by_bucket(d, "size_grp",     prefix = "emp_")
  # merge industry + size aggregates on lgcode
  out <- merge(ind, sz, by = c("lgcode", "emp_total"), all = TRUE)
  out[, DIST := lgcode %/% 100]
  fwrite(out, out_path)
  cat(sprintf("Wrote: %s   (%d munis, post-%d cohort)\n",
              out_path, nrow(out), min_year))
}

dir.create(file.path(ROOT, "output/tab"), recursive = TRUE, showWarnings = FALSE)
build_cohort_emp(2001L, "output/tab/mun_emp_by_bucket_post2001.csv")
build_cohort_emp(2011L, "output/tab/mun_emp_by_bucket_post2011.csv")

# -----------------------------------------------------------------------------
# 5. Quick summary
# -----------------------------------------------------------------------------
cat("\n========== mun_emp_by_industry summary ==========\n")
print(emp_ind[, lapply(.SD, function(x) sum(x, na.rm = TRUE)),
              .SDcols = patterns("^emp_")])

cat("\n========== mun_emp_by_size summary ==========\n")
print(emp_sz[, lapply(.SD, function(x) sum(x, na.rm = TRUE)),
             .SDcols = patterns("^emp_")])

cat("\nDone.\n")
