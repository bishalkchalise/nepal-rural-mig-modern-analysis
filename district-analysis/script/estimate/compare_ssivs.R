################################################################################
#
# COMPARE SHIFT-SHARE INSTRUMENTS (2001 vs DOFE-baseline alternatives)
# ------------------------------------------------------------------------------
# Reads the existing 2001-baseline SSIV (instrument_forex_dist.csv) and the
# four DOFE-baseline alternatives (instrument_dofe_dist.csv) and plots their
# distributions side by side so we can see whether they look similar or
# materially different.
#
# Plots produced :
#   1. Cross-district density of fxshock in a single year (default 2021)
#      with one curve per baseline
#   2. Time-series of cross-district mean fxshock by year, one line per
#      baseline
#   3. Scatter plot matrix of each DOFE baseline vs the 2001 baseline
#
# Inputs :
#   - district-analysis/data/clean/instrument/instrument_forex_dist.csv
#   - district-analysis/data/clean/instrument/instrument_dofe_dist.csv
#
# Outputs (district-analysis/output/fig/) :
#   - ssiv_density_2021.png
#   - ssiv_time_series_mean.png
#   - ssiv_scatter_vs_2001.png
#   - ssiv_corr_matrix.csv  (correlation table, in output/tab/)
#
# Source : run from repo root,
#            source("district-analysis/script/estimate/compare_ssivs.R")
#
################################################################################

rm(list = ls()); cat("\14")

suppressPackageStartupMessages({
  library(tidyverse)
})

# ------------------------------------------------------------------------------
# 1. Load both instruments and assemble a tall comparison frame
# ------------------------------------------------------------------------------

ssiv_2001 <- read.csv(
  "district-analysis/data/clean/instrument/instrument_forex_dist.csv",
  stringsAsFactors = FALSE
) %>%
  select(dname, year, fxshock_2001 = fxshock)

ssiv_dofe <- read.csv(
  "district-analysis/data/clean/instrument/instrument_dofe_dist.csv",
  stringsAsFactors = FALSE
)

ssiv_all <- ssiv_2001 %>%
  inner_join(ssiv_dofe, by = c("dname", "year"))

cat(sprintf("Compare panel: %d obs, %d districts, %d years (%s)\n",
            nrow(ssiv_all),
            length(unique(ssiv_all$dname)),
            length(unique(ssiv_all$year)),
            paste(range(ssiv_all$year), collapse = "-")))

# Tall: one row per (dname, year, baseline)
ssiv_long <- ssiv_all %>%
  pivot_longer(starts_with("fxshock_"),
               names_to  = "baseline",
               names_prefix = "fxshock_",
               values_to = "fxshock") %>%
  mutate(baseline = factor(baseline,
                           levels = c("2001", "2009", "2010", "2011", "avg")))

dir.create("district-analysis/output/fig",
           recursive = TRUE, showWarnings = FALSE)
dir.create("district-analysis/output/tab",
           recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 2. Density at a single year (default 2021)
# ------------------------------------------------------------------------------

snapshot_year <- 2021

p1 <- ssiv_long %>%
  filter(year == snapshot_year) %>%
  ggplot(aes(x = fxshock, colour = baseline, fill = baseline)) +
  geom_density(alpha = 0.20, linewidth = 0.8) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = sprintf(
         "Cross-district density of fxshock at year %d", snapshot_year),
       subtitle = "One curve per baseline-share year (2001 census vs DOFE 2009/10/11/avg)",
       x = "fxshock", y = "Density",
       colour = "Baseline", fill = "Baseline") +
  theme_minimal(base_size = 12)

ggsave("district-analysis/output/fig/ssiv_density_2021.png",
       plot = p1, width = 8, height = 5, dpi = 150)

# ------------------------------------------------------------------------------
# 3. Time series of cross-district mean fxshock
# ------------------------------------------------------------------------------

p2 <- ssiv_long %>%
  group_by(year, baseline) %>%
  summarise(mean_fxshock = mean(fxshock, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_fxshock,
             colour = baseline, group = baseline)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "Mean fxshock across districts, by year",
       subtitle = "One line per baseline-share year",
       x = "Year", y = "Mean fxshock",
       colour = "Baseline") +
  theme_minimal(base_size = 12)

ggsave("district-analysis/output/fig/ssiv_time_series_mean.png",
       plot = p2, width = 8, height = 5, dpi = 150)

# ------------------------------------------------------------------------------
# 4. Scatter: each DOFE baseline vs 2001 (at snapshot_year)
# ------------------------------------------------------------------------------

scatter_df <- ssiv_all %>%
  filter(year == snapshot_year) %>%
  pivot_longer(c(fxshock_2009, fxshock_2010, fxshock_2011, fxshock_avg),
               names_to = "baseline_dofe",
               names_prefix = "fxshock_",
               values_to = "fxshock_dofe")

p3 <- ggplot(scatter_df,
             aes(x = fxshock_2001, y = fxshock_dofe)) +
  geom_point(alpha = 0.6, size = 1.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_smooth(method = "lm", se = FALSE, colour = "steelblue",
              formula = y ~ x) +
  facet_wrap(~ baseline_dofe, scales = "free", nrow = 2) +
  labs(title = sprintf("DOFE-baseline fxshock vs 2001-baseline fxshock (year = %d)",
                       snapshot_year),
       subtitle = "Each point = one district; dashed = 45-degree line; blue = OLS fit",
       x = "fxshock (2001 baseline)", y = "fxshock (DOFE baseline)") +
  theme_minimal(base_size = 12)

ggsave("district-analysis/output/fig/ssiv_scatter_vs_2001.png",
       plot = p3, width = 9, height = 7, dpi = 150)

# ------------------------------------------------------------------------------
# 5. Correlation matrix at snapshot_year
# ------------------------------------------------------------------------------

corr_mat <- ssiv_all %>%
  filter(year == snapshot_year) %>%
  select(starts_with("fxshock_")) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)

cat(sprintf("\nCorrelation matrix of fxshock across baselines (year = %d):\n",
            snapshot_year))
print(corr_mat)

write.csv(corr_mat,
          "district-analysis/output/tab/ssiv_corr_matrix.csv",
          row.names = TRUE)

cat("\nSaved:\n")
cat("  district-analysis/output/fig/ssiv_density_2021.png\n")
cat("  district-analysis/output/fig/ssiv_time_series_mean.png\n")
cat("  district-analysis/output/fig/ssiv_scatter_vs_2001.png\n")
cat("  district-analysis/output/tab/ssiv_corr_matrix.csv\n")
