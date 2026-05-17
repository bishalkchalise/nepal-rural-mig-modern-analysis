################################################################################
# First-stage validation: does the SSIV predict FUTURE DOFE migration outflows?
# ---------------------------------------------------------------------------
# For each district d and time-period T, compute:
#
#   y_{d,T} = log(permits_{d,T} / pop_{d,2011}  *  1000)
#
# and regress on the SSIV z_d averaged over the same period years, with
# the standard control progression. A positive, significant beta says
# the SSIV's baseline-share-times-FX-shifter construction actually
# predicts where Nepali migration permits flow over the post-baseline
# decade -- the "first stage" of the IV.
#
# Periods (configurable below):
#   2011-2015, 2015-2019, 2019-2022
#
# Output:
#   district-analysis/output/tab/first_stage_future_mig.csv
#     columns: period, model (A2/A3/A4), beta, se, p, sig, n, mean_y
#
# Run (fresh R session):
#   source("district-analysis/script/_first_stage_future_mig.R")
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

t0 <- Sys.time()

# Reuse the main panel build (mi, z_v2, regions, dofe, pop_file, REGION_COLS,
# fill_region_na, to_dname) without running the regression loop.
SKIP_RUN <- TRUE
suppressMessages(suppressWarnings({
  source("district-analysis/script/_robustness_all_panels.R")
}))

stars <- function(p) ifelse(is.na(p), "",
  ifelse(p<0.01,"***", ifelse(p<0.05,"**", ifelse(p<0.10,"*",""))))

# ---- Periods --------------------------------------------------------------
PERIODS <- list(
  "2011-2015" = 2011:2015,
  "2015-2019" = 2016:2019,
  "2019-2022" = 2020:2022,
  "2011-2022" = 2011:2022     # full post-baseline window (summary check)
)

# ---- Outcome: district-period log permits per 1000 baseline population ----
pop <- pop_file %>%
  mutate(dname = to_dname(district)) %>%
  select(dname, pop_2011 = district_population_2011) %>%
  distinct(dname, .keep_all = TRUE)

# dofe is (dname, country, year, permits) from the main script;
# aggregate to (dname, period) sums.
period_data <- bind_rows(lapply(names(PERIODS), function(p) {
  yrs <- PERIODS[[p]]
  dofe %>%
    filter(year %in% yrs) %>%
    group_by(dname) %>%
    summarise(permits_period = sum(permits, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(period = p, n_years = length(yrs))
}))

# z averaged over each period's years -- aligns the shifter with the
# same window the outcome is built from.
z_by_period <- bind_rows(lapply(names(PERIODS), function(p) {
  yrs <- PERIODS[[p]]
  z_v2 %>%
    filter(year %in% yrs) %>%
    group_by(dname) %>%
    summarise(z = mean(z_v2, na.rm = TRUE), .groups = "drop") %>%
    mutate(period = p)
}))

# ---- Build cross-section panel (one row per dname x period) ---------------
panel <- period_data %>%
  inner_join(pop, by = "dname") %>%
  inner_join(mi,  by = "dname") %>%
  inner_join(z_by_period, by = c("dname","period")) %>%
  left_join(regions, by = "dname") %>%
  fill_region_na() %>%
  mutate(
    permits_per_1000 = permits_period / n_years / pop_2011 * 1000,
    log_permits_per_1000 = log(pmax(permits_per_1000, 1e-6))
  ) %>%
  group_by(period) %>%
  mutate(z_std = (z - mean(z, na.rm = TRUE)) / sd(z, na.rm = TRUE)) %>%
  ungroup()

cat(sprintf("Panel: %d (dname x period) rows | districts: %d | periods: %d\n",
            nrow(panel), n_distinct(panel$dname), n_distinct(panel$period)))

# ---- Fit per-period regressions at A2/A3/A4 -------------------------------
# A2: y ~ z (SSIV) + baseline mig intensity
# A3: y ~ z + baseline mig intensity   (same as A2 in cross-section --
#      there is no panel year so the year x z control collapses; reported
#      identically for layout symmetry with the rest of the deck)
# A4: y ~ z + baseline mig intensity + region shares
# Loop over both mig-intensity scalings (log + lin) so the portal's
# "Mig intensity" dropdown has cells under both options.
SCALING_COLS <- c(log = "log_mi_z", lin = "lin_mi_z")

results <- list()
for (sc in names(SCALING_COLS)) {
  mig_col <- SCALING_COLS[[sc]]
  for (p in names(PERIODS)) {
    pp <- panel %>% filter(period == p)
    if (!nrow(pp)) next
    pp$mig_var <- pp[[mig_col]]
    pp$z_inter <- pp$z_std
    f_A2 <- log_permits_per_1000 ~ z_inter + mig_var
    f_A3 <- log_permits_per_1000 ~ z_inter + mig_var
    f_A4 <- as.formula(paste("log_permits_per_1000 ~ z_inter + mig_var +",
                             paste(REGION_COLS, collapse = " + ")))
    for (mdl in c("A2","A3","A4")) {
      f <- list(A2 = f_A2, A3 = f_A3, A4 = f_A4)[[mdl]]
      fit <- tryCatch(feols(f, data = pp, vcov = "hetero"),
                      error = function(e) NULL)
      if (is.null(fit)) next
      s <- summary(fit)$coeftable
      if (!"z_inter" %in% rownames(s)) next
      results[[length(results)+1]] <- tibble(
        period = p, scaling = sc, model = mdl,
        beta   = s["z_inter","Estimate"],
        se     = s["z_inter","Std. Error"],
        p      = s["z_inter","Pr(>|t|)"],
        sig    = stars(s["z_inter","Pr(>|t|)"]),
        n      = nobs(fit),
        mean_y = mean(pp$log_permits_per_1000, na.rm = TRUE),
        mean_permits_per_1000 = mean(pp$permits_per_1000, na.rm = TRUE)
      )
    }
  }
}

# A4 under district-drop variants (for the portal's variant filter), both scalings
ktm_valley <- c("Kathmandu","Lalitpur","Bhaktapur")
low_mig_districts <- mi %>% arrange(log_mi_z) %>%
  slice_head(n = 7) %>% pull(dname)
DROP_VARIANTS <- list(
  A4_dropKTM    = ktm_valley,
  A4_dropLowMig = low_mig_districts
)
for (sc in names(SCALING_COLS)) {
  mig_col <- SCALING_COLS[[sc]]
  for (variant_model in names(DROP_VARIANTS)) {
    drop <- DROP_VARIANTS[[variant_model]]
    for (p in names(PERIODS)) {
      pp <- panel %>% filter(period == p, !dname %in% drop)
      if (!nrow(pp)) next
      pp$mig_var <- pp[[mig_col]]
      pp$z_inter <- pp$z_std
      f_A4 <- as.formula(paste("log_permits_per_1000 ~ z_inter + mig_var +",
                               paste(REGION_COLS, collapse = " + ")))
      fit <- tryCatch(feols(f_A4, data = pp, vcov = "hetero"),
                      error = function(e) NULL)
      if (is.null(fit)) next
      s <- summary(fit)$coeftable
      if (!"z_inter" %in% rownames(s)) next
      results[[length(results)+1]] <- tibble(
        period = p, scaling = sc, model = variant_model,
        beta   = s["z_inter","Estimate"],
        se     = s["z_inter","Std. Error"],
        p      = s["z_inter","Pr(>|t|)"],
        sig    = stars(s["z_inter","Pr(>|t|)"]),
        n      = nobs(fit),
        mean_y = mean(pp$log_permits_per_1000, na.rm = TRUE),
        mean_permits_per_1000 = mean(pp$permits_per_1000, na.rm = TRUE)
      )
    }
  }
}

# ============================================================================
# Annual panel 2011-2022 (district x year, with year FE)
# Stronger first-stage spec: within-time variation. Treats permits as annual
# rather than period-summed. SE clustered at district.
# ============================================================================
ann_data <- dofe %>%
  filter(year >= 2011, year <= 2022) %>%
  group_by(dname, year) %>%
  summarise(permits_year = sum(permits, na.rm = TRUE), .groups = "drop") %>%
  inner_join(pop, by = "dname") %>%
  inner_join(mi,  by = "dname") %>%
  left_join(regions, by = "dname") %>%
  fill_region_na() %>%
  inner_join(z_v2, by = c("dname","year")) %>%
  mutate(
    permits_per_1000 = permits_year / pop_2011 * 1000,
    log_permits_per_1000 = log(pmax(permits_per_1000, 1e-6)),
    z_std = (z_v2 - mean(z_v2, na.rm = TRUE)) / sd(z_v2, na.rm = TRUE)
  )

cat(sprintf("\nAnnual panel: %d (dname x year) rows | districts: %d | years: %d\n",
            nrow(ann_data), n_distinct(ann_data$dname),
            n_distinct(ann_data$year)))

run_annual_panel <- function(panel, drop_districts = NULL,
                              scaling_col = "log_mi_z") {
  if (length(drop_districts)) panel <- panel %>% filter(!dname %in% drop_districts)
  panel$mig_var <- panel[[scaling_col]]
  panel$z_inter <- panel$z_std
  # A2: y ~ z + mig_var + year FE
  # A3: y ~ z + mig_var + year FE  (cluster ~ dname; no district FE so z varies)
  # A4: + region shares interacted with year (Block A)
  region_terms <- paste(sprintf("i(year, %s, ref = 2011L)", REGION_COLS),
                        collapse = " + ")
  f_A2 <- as.formula("log_permits_per_1000 ~ z_inter + mig_var | year")
  f_A3 <- as.formula("log_permits_per_1000 ~ z_inter + mig_var | year")
  f_A4 <- as.formula(sprintf(
    "log_permits_per_1000 ~ z_inter + mig_var + %s | year",
    region_terms))
  out <- list()
  for (mdl in c("A2","A3","A4")) {
    f <- list(A2 = f_A2, A3 = f_A3, A4 = f_A4)[[mdl]]
    fit <- tryCatch(feols(f, data = panel, cluster = ~dname),
                    error = function(e) NULL)
    if (is.null(fit)) next
    s <- summary(fit)$coeftable
    if (!"z_inter" %in% rownames(s)) next
    out[[length(out)+1]] <- tibble(
      model = mdl,
      beta  = s["z_inter","Estimate"],
      se    = s["z_inter","Std. Error"],
      p     = s["z_inter","Pr(>|t|)"],
      sig   = stars(s["z_inter","Pr(>|t|)"]),
      n     = nobs(fit),
      mean_y = mean(panel$log_permits_per_1000, na.rm = TRUE),
      mean_permits_per_1000 = mean(panel$permits_per_1000, na.rm = TRUE)
    )
  }
  bind_rows(out)
}

# Loop over both scalings + 3 variants (baseline + 2 drops)
annual_results <- list()
for (sc in names(SCALING_COLS)) {
  mig_col <- SCALING_COLS[[sc]]
  # baseline
  r <- run_annual_panel(ann_data, scaling_col = mig_col) %>%
    mutate(period = "2011-2022 (annual panel, year FE)", scaling = sc)
  annual_results[[length(annual_results)+1]] <- r
  # A4 drop variants
  for (variant_model in names(DROP_VARIANTS)) {
    drop <- DROP_VARIANTS[[variant_model]]
    r <- run_annual_panel(ann_data, drop_districts = drop, scaling_col = mig_col)
    r <- r %>% filter(model == "A4") %>%
      mutate(model = variant_model,
             period = "2011-2022 (annual panel, year FE)", scaling = sc)
    annual_results[[length(annual_results)+1]] <- r
  }
}
annual_out <- bind_rows(annual_results)

out <- bind_rows(results, annual_out) %>% arrange(period, scaling, model)
dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(out, "district-analysis/output/tab/first_stage_future_mig.csv")

cat("\nResults (first-stage validation):\n")
print(out %>% select(period, model, beta, se, sig, n, mean_y))
cat(sprintf("\nWrote district-analysis/output/tab/first_stage_future_mig.csv\n"))
cat(sprintf("Elapsed: %.1f s\n", as.numeric(Sys.time() - t0, units = "secs")))
