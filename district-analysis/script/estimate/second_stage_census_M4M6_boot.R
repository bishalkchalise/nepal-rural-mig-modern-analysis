################################################################################
#
# CENSUS SECOND-STAGE WITH M4-M6 + WILD CLUSTER BOOTSTRAP
# ------------------------------------------------------------------------------
# Same M4-M6 spec ladder as first_stage_M4M6_boot.R, applied to the district
# census panel (74 districts x 2-3 census years).
#
# Treatment : z(fxshock_dofe) * z(log(mig_int_dofe))
# fxshock direction: NPR per LCU (positive beta = "appreciation -> y up")
# Cluster: dname; Bootstrap: wild cluster Rademacher, B reps.
#
# Spec :
#   M4 : treatment + alpha*fx_z + i(year, log_mi_z)                       | C_mig
#   M5 : + i(year, fx_z)                                                   | + C_fx
#   M6 : + i(year, share_X)  six 2001 destination-region shares            | + C_X
#
# Output: district-analysis/output/tab/second_stage_census_M4M6_boot.csv
# One row per (outcome, spec).  mean_y always reported.
#
# Run from repo root:
#     source("district-analysis/script/estimate/second_stage_census_M4M6_boot.R")
#
################################################################################

rm(list = ls()); cat("\14")

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

set.seed(2026)
B_BOOT <- 200    # raise to 500 for production, slower

# ----------------------------------------------------------------------------
# 1. Load
# ----------------------------------------------------------------------------

outcomes_df <- read.csv("district-analysis/data/clean/census/outcomes_district.csv",
                        stringsAsFactors = FALSE)
inst_dofe   <- read.csv("district-analysis/data/clean/instrument/instrument_dofe_dist.csv",
                        stringsAsFactors = FALSE)
region_sh   <- read.csv("district-analysis/data/clean/instrument/dest_region_shares_2001.csv",
                        stringsAsFactors = FALSE)
dofe_raw    <- read.csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv",
                        stringsAsFactors = FALSE)
pop_file    <- read.csv("district-analysis/data/clean/foreign_migration_district_population.csv",
                        stringsAsFactors = FALSE)

dofe_to_census <- c("CHITWAN"="Chitawan","DHANUSHA"="Dhanusa","KAPILVASTU"="Kapilbastu",
                    "MAKAWANPUR"="Makwanpur","TANAHUN"="Tanahu","TEHRATHUM"="Terhathum",
                    "KABHREPALANCHOK"="Kavrepalanchok")
to_dname <- function(x) {
  u <- toupper(str_squish(x))
  ifelse(!is.na(dofe_to_census[u]), dofe_to_census[u], str_to_title(tolower(u)))
}

dofe_dy <- dofe_raw %>%
  group_by(district_rename, year) %>%
  summarise(permits = sum(total_migrants, na.rm = TRUE), .groups = "drop") %>%
  mutate(dname = to_dname(district_rename)) %>%
  select(dname, year, permits)

mi_dofe <- dofe_dy %>%
  filter(year %in% 2009:2010) %>%
  group_by(dname) %>%
  summarise(num = mean(permits), .groups = "drop")
pop_2011 <- pop_file %>%
  mutate(dname = to_dname(district)) %>%
  distinct(dname, district_population_2011) %>%
  rename(pop_2011 = district_population_2011)
mi_dofe <- mi_dofe %>%
  left_join(pop_2011, by = "dname") %>%
  mutate(mig_int_dofe = num / pop_2011) %>%
  select(dname, mig_int_dofe)

# ----------------------------------------------------------------------------
# 2. Build panel + treatment
# ----------------------------------------------------------------------------

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

panel <- outcomes_df %>%
  inner_join(inst_dofe %>% select(dname, year, fxshock_dofe),
             by = c("dname", "year")) %>%
  left_join(mi_dofe,   by = "dname") %>%
  left_join(region_sh, by = "dname") %>%
  filter(!is.na(fxshock_dofe), !is.na(mig_int_dofe))

panel <- panel %>%
  mutate(fx_z      = zscore(fxshock_dofe),
         log_mi_z  = zscore(log(pmax(mig_int_dofe, 1e-12))),
         treatment = fx_z * log_mi_z)

cat(sprintf("Census panel: %d obs, %d districts, %d years\n",
            nrow(panel),
            length(unique(panel$dname)),
            length(unique(panel$year))))

# ----------------------------------------------------------------------------
# 3. Outcome groups
# ----------------------------------------------------------------------------

GROUPS <- list(
  amenities = c("amen_water_piped","amen_water_traditional",
                "amen_cooking_modern","amen_cooking_traditional",
                "amen_lighting_electricity","amen_toilet_modern","amen_toilet_any"),
  housing   = c("housing_own","housing_rented",
                "housing_foundation_modern","housing_foundation_traditional",
                "housing_roof_modern","housing_roof_traditional"),
  work      = c("flfp_all","fem_employment_rate","flfp_agri","flfp_wage",
                "mlfp_all","mlfp_agri","mlfp_nonagri"),
  migration = c("mig_in_share","mig_in_domestic","mig_in_international",
                "mig_in_from_rural","mig_in_from_urban",
                "mig_in_reason_economic","mig_in_reason_noneconomic",
                "mig_in_return","absent_hh_share"),
  household = c("head_female_share","head_age_mean","head_elderly_share",
                "head_young_share"),
  left_behind = c("left_not_with_both","left_mother_only","left_father_only",
                  "left_with_relatives","left_without_parents")
)
OUTCOMES <- intersect(unlist(unname(GROUPS)), names(panel))

REGION_COLS <- c("share_e_asia","share_gulf","share_oecd_north",
                 "share_oecd_europe","share_s_asia","share_se_asia")

build_rhs <- function(level) {
  rhs <- c("treatment","fx_z")
  if (level >= 4) rhs <- c(rhs, "i(year, log_mi_z)")
  if (level >= 5) rhs <- c(rhs, "i(year, fx_z)")
  if (level >= 6) for (c in REGION_COLS) rhs <- c(rhs, sprintf("i(year, %s)", c))
  rhs
}

fit_one <- function(df, outcome, level) {
  rhs <- paste(build_rhs(level), collapse = " + ")
  fml <- as.formula(sprintf("%s ~ %s | dname + year", outcome, rhs))
  feols(fml, data = df, cluster = ~dname, warn = FALSE, notes = FALSE)
}

wild_boot <- function(df, outcome, level, B = B_BOOT) {
  fit <- tryCatch(fit_one(df, outcome, level), error = function(e) e)
  if (inherits(fit, "error")) return(NULL)
  ct <- as.data.frame(summary(fit)$coeftable)
  if (!"treatment" %in% rownames(ct)) return(NULL)
  bh  <- ct["treatment","Estimate"]
  seh <- ct["treatment","Std. Error"]
  pcl <- ct["treatment","Pr(>|t|)"]

  d <- df
  d$.yhat <- fit$fitted.values
  d$.e    <- residuals(fit)
  ents    <- unique(d$dname)

  bs <- numeric(B)
  for (b in seq_len(B)) {
    g <- setNames(sample(c(-1, 1), size = length(ents), replace = TRUE), ents)
    d$.g      <- g[as.character(d$dname)]
    d$.y_star <- d$.yhat + d$.g * d$.e
    fbo <- as.formula(sprintf(".y_star ~ %s | dname + year",
                              paste(build_rhs(level), collapse = " + ")))
    rb <- tryCatch(feols(fbo, data = d, cluster = ~dname,
                         warn = FALSE, notes = FALSE),
                   error = function(e) NULL)
    if (is.null(rb)) { bs[b] <- NA_real_; next }
    cb <- coef(rb)
    bs[b] <- if ("treatment" %in% names(cb)) cb["treatment"] else NA_real_
  }
  bs <- bs[!is.na(bs)]
  centered <- bs - bh
  p_wild <- mean(abs(centered) >= abs(bh))
  list(beta = bh, se_cluster = seh, p_cluster = pcl,
       se_wild = sd(bs), p_wild = p_wild,
       n = nobs(fit), mean_y = mean(df[[outcome]], na.rm = TRUE),
       B_used = length(bs))
}

stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
}

# ----------------------------------------------------------------------------
# 4. Loop and report
# ----------------------------------------------------------------------------

cat(sprintf("Running %d outcomes x 3 specs (M4-M6) x B=%d wild bootstrap reps\n",
            length(OUTCOMES), B_BOOT))

rows <- list(); i <- 0L
for (oc in OUTCOMES) {
  for (lvl in 4:6) {
    cat(sprintf("  %s  M%d ...\n", oc, lvl))
    r <- wild_boot(panel, oc, lvl, B = B_BOOT)
    if (is.null(r)) next
    grp <- names(GROUPS)[sapply(GROUPS, function(g) oc %in% g)][1]
    i <- i + 1L
    rows[[i]] <- tibble(
      group = grp, outcome = oc, spec = sprintf("M%d", lvl),
      beta       = round(r$beta, 4),
      se_cluster = round(r$se_cluster, 4),
      t_cluster  = round(r$beta / r$se_cluster, 2),
      p_cluster  = round(r$p_cluster, 4),
      sig_cluster= stars(r$p_cluster),
      se_wild    = round(r$se_wild, 4),
      t_wild     = round(r$beta / r$se_wild, 2),
      p_wild     = round(r$p_wild, 4),
      sig_wild   = stars(r$p_wild),
      mean_y     = round(r$mean_y, 4),
      n_obs      = r$n,
      B_boot     = r$B_used
    )
  }
}

results <- bind_rows(rows)

dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write.csv(results,
          "district-analysis/output/tab/second_stage_census_M4M6_boot.csv",
          row.names = FALSE)
cat(sprintf("\nSaved: output/tab/second_stage_census_M4M6_boot.csv (%d rows)\n",
            nrow(results)))

# Per-group summaries
for (grp in unique(results$group)) {
  cat(sprintf("\n=== %s ===\n", grp))
  print(results %>%
          filter(group == grp) %>%
          select(outcome, spec, beta, t_cluster, p_cluster, sig_cluster,
                 t_wild, p_wild, sig_wild, mean_y) %>%
          as.data.frame())
}

cat(sprintf("\nSignificant at p_wild<0.05 (out of %d cells): %d\n",
            nrow(results), sum(results$p_wild < 0.05, na.rm = TRUE)))
