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
results <- list()
for (p in names(PERIODS)) {
  pp <- panel %>% filter(period == p)
  if (!nrow(pp)) next
  pp$mig_var <- pp$log_mi_z
  pp$z_inter <- pp$z_std    # in CS this is just z (no interaction needed
                            # for first-stage validation)
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
      period = p, model = mdl,
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

# Also run A4 with drop-KTM-valley sample (4th column)
ktm_valley <- c("Kathmandu","Lalitpur","Bhaktapur")
for (p in names(PERIODS)) {
  pp <- panel %>% filter(period == p, !dname %in% ktm_valley)
  if (!nrow(pp)) next
  pp$mig_var <- pp$log_mi_z
  pp$z_inter <- pp$z_std
  f_A4 <- as.formula(paste("log_permits_per_1000 ~ z_inter + mig_var +",
                           paste(REGION_COLS, collapse = " + ")))
  fit <- tryCatch(feols(f_A4, data = pp, vcov = "hetero"),
                  error = function(e) NULL)
  if (is.null(fit)) next
  s <- summary(fit)$coeftable
  if (!"z_inter" %in% rownames(s)) next
  results[[length(results)+1]] <- tibble(
    period = p, model = "A4_dropKTM",
    beta   = s["z_inter","Estimate"],
    se     = s["z_inter","Std. Error"],
    p      = s["z_inter","Pr(>|t|)"],
    sig    = stars(s["z_inter","Pr(>|t|)"]),
    n      = nobs(fit),
    mean_y = mean(pp$log_permits_per_1000, na.rm = TRUE),
    mean_permits_per_1000 = mean(pp$permits_per_1000, na.rm = TRUE)
  )
}

out <- bind_rows(results) %>% arrange(period, model)
dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(out, "district-analysis/output/tab/first_stage_future_mig.csv")

cat("\nResults (first-stage validation):\n")
print(out %>% select(period, model, beta, se, sig, n, mean_y))
cat(sprintf("\nWrote district-analysis/output/tab/first_stage_future_mig.csv\n"))
cat(sprintf("Elapsed: %.1f s\n", as.numeric(Sys.time() - t0, units = "secs")))
