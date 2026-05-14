################################################################################
#
# PLOT: fxshock variables (bare and intensity-scaled) for both share types
# ------------------------------------------------------------------------------
# Just visualize the four candidate first-stage regressors so we can see what
# they actually look like, before getting tangled in regression signs.
#
#   1. fxshock_2001                   bare, 2001 census shares
#   2. fxshock_2001 x log(mig_int)    intensity-scaled, 2001 shares
#   3. fxshock_dofe                   bare, 2009-2010 DOFE shares
#   4. fxshock_dofe x log(mig_int)    intensity-scaled, DOFE shares
#
# Outputs (district-analysis/output/fig/):
#   - fxshock_variables_timeseries.png   cross-district mean by year
#   - fxshock_variables_density.png      cross-district density at year 2021
#   - fxshock_variables_boxplot.png      year-by-year district boxplots
#
# Source: source("district-analysis/script/estimate/plot_fxshock_variables.R")
#
################################################################################

rm(list = ls()); cat("\14")

suppressPackageStartupMessages({
  library(tidyverse)
})

# ------------------------------------------------------------------------------
# 1. Load both SSIVs and build the four variables
# ------------------------------------------------------------------------------

ssiv_2001 <- read.csv(
  "district-analysis/data/clean/instrument/instrument_forex_dist.csv",
  stringsAsFactors = FALSE
) %>%
  select(dname, year, fxshock, geog_intensity_2001)

ssiv_dofe <- read.csv(
  "district-analysis/data/clean/instrument/instrument_dofe_dist.csv",
  stringsAsFactors = FALSE
) %>%
  select(dname, year, fxshock_dofe)

df <- ssiv_2001 %>%
  inner_join(ssiv_dofe, by = c("dname", "year")) %>%
  mutate(
    log_mig_int    = log(pmax(geog_intensity_2001, 1e-12)),
    fx_x_logmi     = fxshock      * log_mig_int,
    fxdofe_x_logmi = fxshock_dofe * log_mig_int
  )

long <- df %>%
  select(dname, year,
         `fxshock_2001`              = fxshock,
         `fxshock_2001 x log_mi`     = fx_x_logmi,
         `fxshock_dofe`              = fxshock_dofe,
         `fxshock_dofe x log_mi`     = fxdofe_x_logmi) %>%
  pivot_longer(-c(dname, year),
               names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable,
                           levels = c("fxshock_2001",
                                      "fxshock_2001 x log_mi",
                                      "fxshock_dofe",
                                      "fxshock_dofe x log_mi")))

dir.create("district-analysis/output/fig",
           recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 2. Time-series of cross-district mean
# ------------------------------------------------------------------------------

p1 <- long %>%
  group_by(year, variable) %>%
  summarise(mean_val = mean(value, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_val,
             colour = variable, group = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  facet_wrap(~ variable, scales = "free_y", nrow = 2) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "Mean fxshock across districts, by year",
       subtitle = "Four candidate first-stage regressors",
       x = "Year", y = "Cross-district mean",
       colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

ggsave("district-analysis/output/fig/fxshock_variables_timeseries.png",
       plot = p1, width = 9, height = 6, dpi = 150)

# ------------------------------------------------------------------------------
# 3. Cross-district density at a single year (default 2021)
# ------------------------------------------------------------------------------

snapshot_year <- 2021

p2 <- long %>%
  filter(year == snapshot_year) %>%
  ggplot(aes(x = value, fill = variable, colour = variable)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  facet_wrap(~ variable, scales = "free", nrow = 2) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = sprintf("Cross-district density of fxshock variables (year = %d)",
                       snapshot_year),
       x = "value", y = "Density",
       colour = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

ggsave("district-analysis/output/fig/fxshock_variables_density.png",
       plot = p2, width = 9, height = 6, dpi = 150)

# ------------------------------------------------------------------------------
# 4. Year-by-year boxplots showing cross-district spread
# ------------------------------------------------------------------------------

p3 <- long %>%
  ggplot(aes(x = factor(year), y = value, fill = variable)) +
  geom_boxplot(outlier.size = 0.5, linewidth = 0.3) +
  facet_wrap(~ variable, scales = "free_y", nrow = 2) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Year-by-year cross-district distribution of fxshock variables",
       x = "Year", y = "value", fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        strip.text = element_text(face = "bold"))

ggsave("district-analysis/output/fig/fxshock_variables_boxplot.png",
       plot = p3, width = 11, height = 7, dpi = 150)

cat("Saved:\n")
cat("  district-analysis/output/fig/fxshock_variables_timeseries.png\n")
cat("  district-analysis/output/fig/fxshock_variables_density.png\n")
cat("  district-analysis/output/fig/fxshock_variables_boxplot.png\n")
