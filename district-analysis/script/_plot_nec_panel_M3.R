################################################################################
# Plot M3 coefficients for NEC panel: new firm entry by industry and firm size.
# Reads output/tab/robustness_all_panels.csv after _robustness_all_panels.R has
# been run.
#
# Output: output/fig/nec_panel_industry_size_M3.png
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
})

OUT_FILE <- "district-analysis/output/tab/robustness_all_panels.csv"
if (!file.exists(OUT_FILE)) {
  stop(sprintf("Run _robustness_all_panels.R first; %s not found", OUT_FILE))
}

df <- read_csv(OUT_FILE, show_col_types = FALSE) %>%
  filter(dataset == "nec_panel", model == "M3")

if (nrow(df) == 0) stop("No nec_panel M3 rows in output CSV.")

# Categorize outcomes into 'size' vs 'industry'
df <- df %>%
  mutate(
    category = case_when(
      grepl("size_", outcome) ~ "Firm size",
      outcome %in% c("log_n_new_firms","log_emp_new_firms",
                     "log_rev_new_firms","log_cap_new_firms") ~ "Aggregate",
      TRUE ~ "Industry"
    ),
    # Strip prefix for nicer labels
    label = gsub("^log_n_new_firms_size_", "size: ", outcome) |>
            gsub("^log_n_new_firms_", "ind: ", x = _) |>
            gsub("^log_", "", x = _)
  ) %>%
  mutate(ci_lo = beta - 1.96 * se,
         ci_hi = beta + 1.96 * se)

# Two-panel plot, one for industry one for size
plot_block <- function(d, title) {
  d <- d %>% arrange(beta) %>% mutate(label = factor(label, levels = label))
  ggplot(d, aes(x = beta, y = label, color = scaling)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
    geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                   height = 0, linewidth = 0.6,
                   position = position_dodge(width = 0.5)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
    scale_color_manual(values = c(log = "#1f77b4", w90 = "#d62728"),
                       labels = c(log = "log (outcome raw)",
                                  w90 = "w90 (outcome winsorized p5/p95)")) +
    labs(x = expression(beta * " on " * z[d*","*t-2]^{std} %*% log_mi[z]),
         y = NULL, color = "Outcome scaling",
         title = title) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "bottom")
}

p_ind  <- plot_block(df %>% filter(category == "Industry"),
                     "NEC panel M3: new firm entry by industry (log_n_new_firms_{sector})")
p_size <- plot_block(df %>% filter(category == "Firm size"),
                     "NEC panel M3: new firm entry by firm size (log_n_new_firms_size_{size})")

dir.create("district-analysis/output/fig", recursive = TRUE, showWarnings = FALSE)
ggsave("district-analysis/output/fig/nec_panel_industry_M3.png",
       p_ind, width = 9, height = 5, dpi = 160)
ggsave("district-analysis/output/fig/nec_panel_size_M3.png",
       p_size, width = 8, height = 3.5, dpi = 160)

cat("Saved:\n",
    "  district-analysis/output/fig/nec_panel_industry_M3.png\n",
    "  district-analysis/output/fig/nec_panel_size_M3.png\n", sep = "")
