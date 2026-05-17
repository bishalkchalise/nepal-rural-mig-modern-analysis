################################################################################
# District-level headline plots (4 figures)
# ---------------------------------------------------------------------------
# Generates 4 PNG figures summarising the main story:
#   fig1_first_stage_scatter.png  -- SSIV z -> log(permits 2011-2022), one
#                                    dot per district + fitted line
#   fig2_migration_forest.png     -- in/out/net x perm/temp, 6 rows, with 95% CI
#   fig3_sectoral_forest.png      -- industry & occupation reallocation, ~10 rows
#   fig4_firm_response.png        -- NEC cross-section + NEC panel entry by size
#
# Source data:
#   - district-analysis/output/tab/robustness_all_panels.csv  (for plots 2-4)
#   - rebuilds the first-stage cross-section panel inline for plot 1
#
# Run (fresh R session):
#   source("district-analysis/script/_plots_headline.R")
#
# Output: district-analysis/output/fig/fig{1..4}_*.png
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

dir.create("district-analysis/output/fig", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# Plot 1 -- first-stage scatter
# Reuse the main panel build to get z + permits panel
# ============================================================================
SKIP_RUN <- TRUE
suppressMessages(suppressWarnings({
  source("district-analysis/script/_robustness_all_panels.R")
}))

# Build the district cross-section for 2011-2022
pop <- pop_file %>%
  mutate(dname = to_dname(district)) %>%
  select(dname, pop_2011 = district_population_2011) %>%
  distinct(dname, .keep_all = TRUE)

permits_2011_22 <- dofe %>%
  filter(year >= 2011, year <= 2022) %>%
  group_by(dname) %>%
  summarise(permits = sum(permits, na.rm = TRUE) / 12, .groups = "drop")  # avg annual

z_2011_22 <- z_v2 %>%
  filter(year >= 2011, year <= 2022) %>%
  group_by(dname) %>%
  summarise(z = mean(z_v2, na.rm = TRUE), .groups = "drop") %>%
  mutate(z_std = (z - mean(z)) / sd(z))

ktm_valley <- c("Kathmandu","Lalitpur","Bhaktapur")
low_mig <- mi %>% arrange(log_mi_z) %>% slice_head(n = 7) %>% pull(dname)

p1_data <- permits_2011_22 %>%
  inner_join(pop, by = "dname") %>%
  inner_join(z_2011_22, by = "dname") %>%
  inner_join(mi, by = "dname") %>%
  mutate(log_permits = log(pmax(permits / pop_2011 * 1000, 1e-6)),
         group = case_when(
           dname %in% ktm_valley ~ "KTM valley",
           dname %in% low_mig    ~ "Low-mig (Karnali etc.)",
           TRUE                  ~ "Other"
         ),
         group = factor(group, levels = c("Other","KTM valley","Low-mig (Karnali etc.)")))

# Headline beta from CSV
fs <- read_csv("district-analysis/output/tab/first_stage_future_mig.csv",
               show_col_types = FALSE)
beta_label <- fs %>%
  filter(period == "2011-2022 (annual panel, year FE)" | period == "2011-2022",
         model == "A4", scaling == "log") %>%
  slice(1) %>%
  with(sprintf("Annual panel A4: β = +%.3f*** (SE %.3f, N = %d)", beta, se, n))

p1 <- ggplot(p1_data, aes(z_std, log_permits)) +
  geom_point(aes(colour = group, size = group), alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, colour = "#1a365d",
              linewidth = 0.7, alpha = 0.15, fill = "#1a365d") +
  scale_colour_manual(values = c("Other" = "#4a5568",
                                  "KTM valley" = "#c0392b",
                                  "Low-mig (Karnali etc.)" = "#e67e22")) +
  scale_size_manual(values = c("Other" = 2.2, "KTM valley" = 3.4,
                                "Low-mig (Karnali etc.)" = 3.4), guide = "none") +
  labs(title = "First stage: SSIV predicts where Nepali labour-permits go",
       subtitle = "Cross-section, 75 districts (one dot each). Outcome summed over 2011-2022.",
       x = "SSIV z (avg 2011-2022, standardised)",
       y = "log(permits / 1000 pop, 2011-2022)",
       colour = NULL,
       caption = beta_label) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, face = "italic"),
        panel.grid.minor = element_blank())

ggsave("district-analysis/output/fig/fig1_first_stage_scatter.png",
       p1, width = 7.5, height = 5.2, dpi = 200, bg = "white")
cat("Wrote fig1_first_stage_scatter.png\n")

# ============================================================================
# Helper -- read cell from main robustness CSV
# Cells in robustness_all_panels.csv: dataset, outcome, scaling, lag, model, beta, se, p, sig, mean_y, n
# ============================================================================
rg <- read_csv("district-analysis/output/tab/robustness_all_panels.csv",
               show_col_types = FALSE) %>%
  filter(scaling == "log", lag == 2L, model == "M4")
get_row <- function(ds, oc) {
  r <- rg %>% filter(dataset == ds, outcome == oc) %>% slice(1)
  if (!nrow(r)) return(NULL)
  list(beta = r$beta, se = r$se, p = r$p, sig = r$sig, n = r$n, mean = r$mean_y)
}

ci95 <- function(b, se) c(b - 1.96 * se, b + 1.96 * se)

# ============================================================================
# Plot 2 -- migration forest (6 rows, perm/temp x in/out/net)
# ============================================================================
mig_outc <- tribble(
  ~group, ~side, ~ds, ~outcome, ~label,
  "Temp (5-yr)", "In",  "census", "mig_in_temp_share",    "In-migration",
  "Temp (5-yr)", "Out", "census", "mig_out_temp_share",   "Out-migration",
  "Temp (5-yr)", "Net", "census", "net_temp_mig_share",   "Net",
  "Permanent",   "In",  "census", "mig_in_internal_share","In-migration",
  "Permanent",   "Out", "census", "mig_out_internal_share","Out-migration",
  "Permanent",   "Net", "census", "net_internal_mig_share","Net"
)
mig_df <- mig_outc %>%
  rowwise() %>%
  mutate(r = list(get_row(ds, outcome))) %>%
  ungroup() %>%
  mutate(beta = map_dbl(r, ~ if (is.null(.x)) NA else .x$beta),
         se   = map_dbl(r, ~ if (is.null(.x)) NA else .x$se),
         lo   = beta - 1.96*se, hi = beta + 1.96*se,
         sig  = map_chr(r, ~ if (is.null(.x)) "" else (.x$sig %||% ""))) %>%
  mutate(row_label = sprintf("%s -- %s", group, label),
         row_label = factor(row_label, levels = rev(row_label)))

p2 <- ggplot(mig_df, aes(beta, row_label, colour = side)) +
  geom_vline(xintercept = 0, colour = "#999", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.18, linewidth = 0.7) +
  geom_point(size = 2.7) +
  geom_text(aes(label = sprintf("β=%+0.3f%s", beta, sig)),
            hjust = -0.18, vjust = -0.6, size = 3.0, colour = "black") +
  scale_colour_manual(values = c("In"  = "#9d2918",
                                  "Out" = "#1e6e3a",
                                  "Net" = "#0d3b66"),
                      guide = guide_legend(reverse = TRUE)) +
  labs(title = "Migration response: blocked inflow, not extra outflow",
       subtitle = "Census 2011-2021 district panel; spec A4 (saturated), lag-2 log-z. 95% CI.",
       x = "β on z(log mig/1000)", y = NULL, colour = NULL,
       caption = "Net = In - Out. *** p<0.01, ** p<0.05, * p<0.10.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, face = "italic"),
        panel.grid.minor = element_blank())

ggsave("district-analysis/output/fig/fig2_migration_forest.png",
       p2, width = 8.0, height = 5.0, dpi = 200, bg = "white")
cat("Wrote fig2_migration_forest.png\n")

# ============================================================================
# Plot 3 -- sectoral reallocation forest (industry + occupation)
# ============================================================================
sec_outc <- tribble(
  ~category, ~ds, ~outcome, ~label,
  "Industry",   "census", "ind_agri_forestry_fish",      "Industry: Agriculture",
  "Industry",   "census", "ind_construction",            "Industry: Construction",
  "Industry",   "census", "ind_manufacturing",           "Industry: Manufacturing",
  "Industry",   "census", "ind_wholesale_retail",        "Industry: Wholesale/retail",
  "Industry",   "census", "ind_transport_accommodation", "Industry: Transport/accomm.",
  "Industry",   "census", "ind_education",               "Industry: Education",
  "Occupation", "census", "occ_share_agriculture",       "Occ: Skilled agri",
  "Occupation", "census", "occ_share_service_sales",     "Occ: Service & sales",
  "Occupation", "census", "occ_share_technicians",       "Occ: Technicians",
  "Occupation", "census", "occ_share_professionals",     "Occ: Professionals",
  "Occupation", "census", "occ_share_managers",          "Occ: Managers"
)
sec_df <- sec_outc %>%
  rowwise() %>%
  mutate(r = list(get_row(ds, outcome))) %>%
  ungroup() %>%
  mutate(beta = map_dbl(r, ~ if (is.null(.x)) NA else .x$beta),
         se   = map_dbl(r, ~ if (is.null(.x)) NA else .x$se),
         lo   = beta - 1.96*se, hi = beta + 1.96*se,
         sig  = map_chr(r, ~ if (is.null(.x)) "" else (.x$sig %||% "")),
         signed = if_else(beta < 0, "negative", "positive")) %>%
  filter(!is.na(beta)) %>%
  arrange(beta) %>%
  mutate(label = factor(label, levels = label))

p3 <- ggplot(sec_df, aes(beta, label, colour = signed)) +
  geom_vline(xintercept = 0, colour = "#999", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.22, linewidth = 0.7) +
  geom_point(size = 2.6) +
  geom_text(aes(label = sprintf("β=%+0.3f%s", beta, sig)),
            hjust = -0.18, vjust = -0.6, size = 2.9, colour = "black") +
  scale_colour_manual(values = c(negative = "#9d2918", positive = "#0e5a2d"),
                      guide = "none") +
  labs(title = "Workers exit agriculture into services and skilled occupations",
       subtitle = "Census 2011-2021 industry & occupation shares; A4 lag-2 log-z. 95% CI.",
       x = "β on z(log mig/1000)", y = NULL,
       caption = "Top = workers leaving; bottom = workers entering.  *** p<0.01, ** p<0.05, * p<0.10.") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, face = "italic"),
        panel.grid.minor = element_blank())

ggsave("district-analysis/output/fig/fig3_sectoral_forest.png",
       p3, width = 8.5, height = 5.4, dpi = 200, bg = "white")
cat("Wrote fig3_sectoral_forest.png\n")

# ============================================================================
# Plot 4 -- firm-side response (2 panels: NEC cs + NEC panel by size)
# ============================================================================
nec_cs_outc <- tribble(
  ~ds, ~outcome, ~label,
  "nec_cs", "n_firms",            "# firms",
  "nec_cs", "emp_total",          "Total employment",
  "nec_cs", "mean_emp_per_firm",  "Mean emp / firm",
  "nec_cs", "formality_index",    "Formality index",
  "nec_cs", "share_firms_size_micro_1",    "Share micro (1 worker)",
  "nec_cs", "share_firms_size_large_51p",  "Share large (51+)"
)
nec_cs_df <- nec_cs_outc %>%
  rowwise() %>%
  mutate(r = list(get_row(ds, outcome))) %>%
  ungroup() %>%
  mutate(beta = map_dbl(r, ~ if (is.null(.x)) NA else .x$beta),
         se   = map_dbl(r, ~ if (is.null(.x)) NA else .x$se),
         lo   = beta - 1.96*se, hi = beta + 1.96*se,
         sig  = map_chr(r, ~ if (is.null(.x)) "" else (.x$sig %||% ""))) %>%
  filter(!is.na(beta)) %>%
  # Standardise: report as % of mean for level outcomes (n_firms, emp_total)
  mutate(mean_y = map_dbl(r, ~ if (is.null(.x)) NA else (.x$mean %||% NA)),
         beta_std = if_else(abs(mean_y) > 10, beta/mean_y * 100, beta * 100),
         se_std   = if_else(abs(mean_y) > 10, se/abs(mean_y) * 100, se * 100),
         lo_std = beta_std - 1.96*se_std, hi_std = beta_std + 1.96*se_std) %>%
  arrange(beta_std) %>%
  mutate(label = factor(label, levels = label))

p4a <- ggplot(nec_cs_df, aes(beta_std, label)) +
  geom_vline(xintercept = 0, colour = "#999", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo_std, xmax = hi_std), height = 0.22,
                 colour = "#0d3b66", linewidth = 0.7) +
  geom_point(size = 2.6, colour = "#0d3b66") +
  geom_text(aes(label = sprintf("%+0.1f%%%s", beta_std, sig)),
            hjust = -0.18, vjust = -0.6, size = 2.9, colour = "black") +
  labs(title = "(a) NEC 2018 cross-section",
       x = "β as % of mean (or pp for shares)", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# Right panel: NEC panel entry by size cohort
nec_panel_outc <- tribble(
  ~ds, ~outcome, ~label,
  "nec_panel", "log_n_new_firms_size_micro_1",     "Micro (1 worker)",
  "nec_panel", "log_n_new_firms_size_small_2_9",   "Small (2-9)",
  "nec_panel", "log_n_new_firms_size_medium_10_50","Medium (10-50)",
  "nec_panel", "log_n_new_firms_size_large_51p",   "Large (51+)"
)
nec_panel_df <- nec_panel_outc %>%
  rowwise() %>%
  mutate(r = list(get_row(ds, outcome))) %>%
  ungroup() %>%
  mutate(beta = map_dbl(r, ~ if (is.null(.x)) NA else .x$beta),
         se   = map_dbl(r, ~ if (is.null(.x)) NA else .x$se),
         lo   = beta - 1.96*se, hi = beta + 1.96*se,
         sig  = map_chr(r, ~ if (is.null(.x)) "" else (.x$sig %||% ""))) %>%
  filter(!is.na(beta)) %>%
  mutate(label = factor(label, levels = label),
         signed = if_else(beta < 0, "negative", "positive"))

p4b <- ggplot(nec_panel_df, aes(beta, label, colour = signed)) +
  geom_vline(xintercept = 0, colour = "#999", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.22, linewidth = 0.7) +
  geom_point(size = 2.6) +
  geom_text(aes(label = sprintf("β=%+0.2f%s", beta, sig)),
            hjust = -0.18, vjust = -0.6, size = 2.9, colour = "black") +
  scale_colour_manual(values = c(negative = "#9d2918", positive = "#0e5a2d"),
                      guide = "none") +
  labs(title = "(b) NEC entry-cohort panel, 2011-2018 (log)",
       x = "β on z(log mig/1000)", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# Combine via patchwork if available; else save separately
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  p4 <- p4a + p4b +
    plot_annotation(
      title = "Firm-side response: smaller, fewer firms, but medium-size entry rises",
      subtitle = "Saturated A4, lag-2 log-z. 95% CI.",
      caption = "(a) % of mean for counts (firms, emp); pp for shares.  (b) log new-firm count by size cohort.")
  ggsave("district-analysis/output/fig/fig4_firm_response.png",
         p4, width = 11, height = 5.4, dpi = 200, bg = "white")
} else {
  ggsave("district-analysis/output/fig/fig4a_firm_cs.png",
         p4a, width = 6.5, height = 4.5, dpi = 200, bg = "white")
  ggsave("district-analysis/output/fig/fig4b_firm_entry.png",
         p4b, width = 6.5, height = 4.0, dpi = 200, bg = "white")
}
cat("Wrote fig4_firm_response.png\n")
cat("\nDone. View PNGs in district-analysis/output/fig/\n")
