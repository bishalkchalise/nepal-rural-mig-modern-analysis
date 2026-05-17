################################################################################
# Plot M3 coefficients for NEC panel: new firm entry by industry and firm size.
# Reads output/tab/robustness_all_panels.csv after _robustness_all_panels.R has
# been run.  Shows only "log" scaling (w90 dropped per user request).
#
# Output:
#   output/fig/nec_panel_industry_M3.png
#   output/fig/nec_panel_size_M3.png
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
})

OUT_FILE <- "district-analysis/output/tab/robustness_all_panels.csv"
if (!file.exists(OUT_FILE)) {
  stop(sprintf("Run _robustness_all_panels.R first; %s not found", OUT_FILE))
}

df <- read_csv(OUT_FILE, show_col_types = FALSE) %>%
  filter(dataset == "nec_panel", model == "M3", scaling == "log")

# Robustness grid added (scaling, lag). Keep only baseline lag for plotting.
if ("lag" %in% names(df)) df <- df %>% filter(lag == 2L)

if (nrow(df) == 0) stop("No nec_panel/M3/log rows in output CSV.")

df <- df %>%
  mutate(ci_lo = beta - 1.96 * se,
         ci_hi = beta + 1.96 * se)

# ---- Firm-size plot: explicit micro -> small -> medium -> large order ----
SIZE_ORDER <- c(
  "log_n_new_firms_size_micro_1"     = "Micro (1 worker)",
  "log_n_new_firms_size_small_2_9"   = "Small (2-9)",
  "log_n_new_firms_size_medium_10_50"= "Medium (10-50)",
  "log_n_new_firms_size_large_51p"   = "Large (51+)"
)
size_df <- df %>% filter(outcome %in% names(SIZE_ORDER)) %>%
  mutate(label = factor(SIZE_ORDER[outcome], levels = unname(SIZE_ORDER)))

caption_txt <- paste0(
  "Notes: Each point is the coefficient on the share-weighted FX shock interacted with district baseline\n",
  "migration intensity, from a district x founding-year panel regression with district + year fixed effects,\n",
  "controls for differential year trends by migration intensity and baseline destination region shares,\n",
  "and standard errors clustered at the district level. FX shock uses 2009-10 DOFE destination shares\n",
  "applied to year-over-year change in log NPR-per-LCU since 2010; lagged by two years.\n",
  "Whiskers are 95% confidence intervals."
)

p_size <- ggplot(size_df, aes(x = beta, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0, linewidth = 0.7, color = "#1f77b4") +
  geom_point(size = 3, color = "#1f77b4") +
  labs(x = "Effect on (log) new firm entry per 1-SD of share-weighted FX shock",
       y = NULL,
       title = "New Firm Entry, 2011-2018, by firm size") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

# ---- Industry plot: intuitive labels, order by sectoral logic ----
IND_ORDER <- c(
  "log_n_new_firms_agriculture"             = "Agriculture",
  "log_n_new_firms_manufacturing"           = "Manufacturing",
  "log_n_new_firms_construction"            = "Construction",
  "log_n_new_firms_trade_retail"            = "Trade & retail",
  "log_n_new_firms_hospitality_food"        = "Hospitality & food",
  "log_n_new_firms_transport_storage"       = "Transport & storage",
  "log_n_new_firms_finance_prof_realestate" = "Finance / prof. / real estate",
  "log_n_new_firms_education_health_social" = "Education / health / social",
  "log_n_new_firms_other_services"          = "Other services"
)
ind_df <- df %>% filter(outcome %in% names(IND_ORDER)) %>%
  mutate(label = unname(IND_ORDER[outcome])) %>%
  arrange(desc(beta)) %>%                       # positive at top, negative at bottom
  mutate(label = factor(label, levels = rev(label)))

p_ind <- ggplot(ind_df, aes(x = beta, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0, linewidth = 0.7, color = "#1f77b4") +
  geom_point(size = 3, color = "#1f77b4") +
  labs(x = "Effect on (log) new firm entry per 1-SD of share-weighted FX shock",
       y = NULL,
       title = "New Firm Entry, 2011-2018, by industry") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

dir.create("district-analysis/output/fig", recursive = TRUE, showWarnings = FALSE)
ggsave("district-analysis/output/fig/nec_panel_size_M3.png",
       p_size, width = 8, height = 3.5, dpi = 160)
ggsave("district-analysis/output/fig/nec_panel_industry_M3.png",
       p_ind, width = 9, height = 5, dpi = 160)

cat("Saved:\n",
    "  district-analysis/output/fig/nec_panel_size_M3.png\n",
    "  district-analysis/output/fig/nec_panel_industry_M3.png\n", sep = "")
