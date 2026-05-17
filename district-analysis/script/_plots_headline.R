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
  filter(scaling == "log", lag == 2L, model == "M4") %>%
  mutate(sig = if_else(is.na(sig) | sig %in% c("NA","NaN"), "", sig))
get_row <- function(ds, oc) {
  r <- rg %>% filter(dataset == ds, outcome == oc) %>% slice(1)
  if (!nrow(r)) return(NULL)
  list(beta = r$beta, se = r$se, p = r$p, sig = r$sig, n = r$n, mean = r$mean_y)
}

# Significance-aware colour helper: red=neg-sig, green=pos-sig, grey=not-sig.
sig_colour <- function(beta, sig_str) {
  case_when(
    is.na(beta) ~ "grey",
    sig_str == "" ~ "grey",
    beta < 0      ~ "negative",
    TRUE          ~ "positive"
  )
}

# β label that omits the "NA" suffix when not significant
beta_lab <- function(beta, sig_str, fmt = "β=%+0.3f") {
  s <- sprintf(fmt, beta)
  if_else(is.na(sig_str) | sig_str == "", s, paste0(s, sig_str))
}

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
  mutate(beta  = map_dbl(r, ~ if (is.null(.x)) NA else .x$beta),
         se    = map_dbl(r, ~ if (is.null(.x)) NA else .x$se),
         lo    = beta - 1.96*se, hi = beta + 1.96*se,
         sig   = map_chr(r, ~ if (is.null(.x)) "" else (.x$sig %||% "")),
         sig   = if_else(is.na(sig) | sig %in% c("NA","NaN"), "", sig),
         colour_grp = sig_colour(beta, sig)) %>%
  # Order panels: In, Out, Net (clear left-to-right reading)
  mutate(side = factor(side, levels = c("In","Out","Net")),
         group = factor(group, levels = c("Permanent","Temp (5-yr)")))

p2 <- ggplot(mig_df, aes(beta, group, colour = colour_grp)) +
  facet_wrap(~ side, ncol = 1, scales = "free_y",
             strip.position = "top") +
  geom_vline(xintercept = 0, colour = "#999", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25, linewidth = 0.7) +
  geom_point(size = 3) +
  geom_text(aes(label = beta_lab(beta, sig)),
            hjust = -0.15, vjust = -0.7, size = 3.1, colour = "black") +
  scale_colour_manual(values = c(positive = "#0e5a2d",
                                  negative = "#9d2918",
                                  grey     = "#888888"),
                      guide = "none") +
  labs(title = "Migration response: blocked inflow, not extra outflow",
       subtitle = "Census 2011-2021 district panel; spec A4 (saturated), lag-2 log-z. 95% CI.",
       x = "β on z(log mig/1000)", y = NULL,
       caption = "Net = In - Out.  Grey = not significant (p≥0.10). *** p<0.01, ** p<0.05, * p<0.10.") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, face = "italic"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0))

ggsave("district-analysis/output/fig/fig2_migration_forest.png",
       p2, width = 8.0, height = 5.6, dpi = 200, bg = "white")
cat("Wrote fig2_migration_forest.png\n")

# ============================================================================
# Plot 3 -- sectoral reallocation: Industry & Occupation in SEPARATE panels
# ============================================================================
ind_outc <- tribble(
  ~ds, ~outcome, ~label,
  "census", "ind_agri_forestry_fish",      "Agriculture",
  "census", "ind_manufacturing",           "Manufacturing",
  "census", "ind_construction",            "Construction",
  "census", "ind_wholesale_retail",        "Wholesale & retail",
  "census", "ind_transport_accommodation", "Transport & accomm.",
  "census", "ind_finance_real_estate_prof","Finance / RE / prof.",
  "census", "ind_public_admin_defence",    "Public admin & defence",
  "census", "ind_education",               "Education",
  "census", "ind_health",                  "Health",
  "census", "ind_arts_recreation",         "Arts & recreation",
  "census", "ind_others",                  "Other industry"
)
occ_outc <- tribble(
  ~ds, ~outcome, ~label,
  "census", "occ_share_managers",          "Managers",
  "census", "occ_share_professionals",     "Professionals",
  "census", "occ_share_technicians",       "Technicians",
  "census", "occ_share_office_assistants", "Office assistants",
  "census", "occ_share_service_sales",     "Service & sales",
  "census", "occ_share_agriculture",       "Skilled agri",
  "census", "occ_share_craft_trades",      "Craft & trades",
  "census", "occ_share_machine_operators", "Machine operators",
  "census", "occ_share_elementary",        "Elementary",
  "census", "occ_share_armed_forces",      "Armed forces"
)

make_coef <- function(tbl) {
  tbl %>%
    rowwise() %>%
    mutate(r = list(get_row(ds, outcome))) %>%
    ungroup() %>%
    mutate(beta = map_dbl(r, ~ if (is.null(.x)) NA else .x$beta),
           se   = map_dbl(r, ~ if (is.null(.x)) NA else .x$se),
           lo   = beta - 1.96*se, hi = beta + 1.96*se,
           sig  = map_chr(r, ~ if (is.null(.x)) "" else (.x$sig %||% "")),
           sig  = if_else(is.na(sig) | sig %in% c("NA","NaN"), "", sig),
           colour_grp = sig_colour(beta, sig)) %>%
    filter(!is.na(beta)) %>%
    arrange(beta) %>%
    mutate(label = factor(label, levels = label))
}

ind_df <- make_coef(ind_outc)
occ_df <- make_coef(occ_outc)

forest <- function(df, title) {
  ggplot(df, aes(beta, label, colour = colour_grp)) +
    geom_vline(xintercept = 0, colour = "#999", linewidth = 0.4) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.22, linewidth = 0.65) +
    geom_point(size = 2.4) +
    geom_text(aes(label = beta_lab(beta, sig)),
              hjust = -0.18, vjust = -0.55, size = 2.7, colour = "black") +
    scale_colour_manual(values = c(negative = "#9d2918",
                                    positive = "#0e5a2d",
                                    grey     = "#888888"),
                        guide = "none") +
    labs(title = title, x = "β on z(log mig/1000)", y = NULL) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())
}

# Compute shared x range for fair comparison
xrange_pad <- function(...) {
  dfs <- list(...)
  all_lo <- min(map_dbl(dfs, ~ min(.x$lo, na.rm = TRUE)))
  all_hi <- max(map_dbl(dfs, ~ max(.x$hi, na.rm = TRUE)))
  pad <- (all_hi - all_lo) * 0.15
  c(all_lo - pad, all_hi + pad)
}
xr <- xrange_pad(ind_df, occ_df)

p3a <- forest(ind_df, "Industry shares") +
       coord_cartesian(xlim = xr) +
       theme(plot.title = element_text(size = 12))
p3b <- forest(occ_df, "Occupation shares") +
       coord_cartesian(xlim = xr) +
       theme(plot.title = element_text(size = 12))

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  p3 <- p3a + p3b +
    plot_annotation(
      title = "Workers reallocate out of agriculture into services & skilled occupations",
      subtitle = "Census 2011-2021 district panel; spec A4 (saturated), lag-2 log-z. 95% CI.",
      caption = "*** p<0.01, ** p<0.05, * p<0.10.")
  ggsave("district-analysis/output/fig/fig3_sectoral_forest.png",
         p3, width = 12, height = 5.6, dpi = 200, bg = "white")
} else {
  ggsave("district-analysis/output/fig/fig3a_industry.png",  p3a,
         width = 6.5, height = 5.0, dpi = 200, bg = "white")
  ggsave("district-analysis/output/fig/fig3b_occupation.png",p3b,
         width = 6.5, height = 5.0, dpi = 200, bg = "white")
}
cat("Wrote fig3_sectoral_forest.png\n")

# ============================================================================
# Plot 4 -- firm-side response: 4 separate panels (don't mix metric types)
#   4a: NEC scale         -- n_firms, emp_total, mean_emp_per_firm
#   4b: NEC size mix      -- share micro/small/medium/large
#   4c: NEC formality     -- formality_index, share registered/tax/accounts/credit
#   4d: NEC panel entry   -- log new firms by size cohort
# ============================================================================
nec_scale <- tribble(
  ~ds, ~outcome, ~label,
  "nec_cs", "n_firms",           "# firms",
  "nec_cs", "emp_total",         "Total employment"
  # mean_emp_per_firm dropped from this panel -- its mean (~3.2) makes the
  # % of mean misleadingly tiny next to # firms / total emp. Goes into (b).
)
nec_size <- tribble(
  ~ds, ~outcome, ~label,
  "nec_cs", "share_firms_size_micro_1",     "Micro (1)",
  "nec_cs", "share_firms_size_small_2_9",   "Small (2-9)",
  "nec_cs", "share_firms_size_medium_10_50","Medium (10-50)",
  "nec_cs", "share_firms_size_large_51p",   "Large (51+)"
)
nec_formal <- tribble(
  ~ds, ~outcome, ~label,
  "nec_cs", "formality_index",     "Formality index",
  "nec_cs", "share_registered",    "Share registered",
  "nec_cs", "share_tax_registered","Share tax-registered",
  "nec_cs", "share_keeps_accounts","Share keeps accounts",
  "nec_cs", "share_formal_credit", "Share formal credit",
  "nec_cs", "share_borrowed_any",  "Share borrowed (any)"
)
nec_entry_size <- tribble(
  ~ds, ~outcome, ~label,
  "nec_panel", "log_n_new_firms_size_micro_1",     "Micro (1)",
  "nec_panel", "log_n_new_firms_size_small_2_9",   "Small (2-9)",
  "nec_panel", "log_n_new_firms_size_medium_10_50","Medium (10-50)",
  "nec_panel", "log_n_new_firms_size_large_51p",   "Large (51+)"
)

# For NEC scale, rescale as % of mean so all three fit on a comparable axis
make_coef_scaled <- function(tbl) {
  tbl %>%
    rowwise() %>%
    mutate(r = list(get_row(ds, outcome))) %>%
    ungroup() %>%
    mutate(beta = map_dbl(r, ~ if (is.null(.x)) NA else .x$beta),
           se   = map_dbl(r, ~ if (is.null(.x)) NA else .x$se),
           mean_y = map_dbl(r, ~ if (is.null(.x)) NA else (.x$mean %||% NA)),
           sig  = map_chr(r, ~ if (is.null(.x)) "" else (.x$sig %||% "")),
           sig  = if_else(is.na(sig) | sig %in% c("NA","NaN"), "", sig),
           beta_pct = beta / mean_y * 100,
           se_pct   = se / abs(mean_y) * 100,
           lo = beta_pct - 1.96*se_pct, hi = beta_pct + 1.96*se_pct,
           colour_grp = sig_colour(beta_pct, sig)) %>%
    filter(!is.na(beta_pct)) %>%
    arrange(beta_pct) %>%
    mutate(label = factor(label, levels = label))
}

scale_df  <- make_coef_scaled(nec_scale)
size_df   <- make_coef(nec_size)
formal_df <- make_coef(nec_formal)
entry_df  <- make_coef(nec_entry_size)

forest_pct <- function(df, title) {
  ggplot(df, aes(beta_pct, label, colour = colour_grp)) +
    geom_vline(xintercept = 0, colour = "#999", linewidth = 0.4) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.22, linewidth = 0.7) +
    geom_point(size = 2.5) +
    geom_text(aes(label = beta_lab(beta_pct, sig, "%+0.0f%%")),
              hjust = -0.18, vjust = -0.55, size = 2.8, colour = "black") +
    scale_colour_manual(values = c(negative = "#9d2918",
                                    positive = "#0e5a2d",
                                    grey     = "#888888"),
                        guide = "none") +
    labs(title = title, x = "β as % of mean", y = NULL) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 12),
          panel.grid.minor = element_blank())
}

p4a <- forest_pct(scale_df,   "(a) NEC scale (cross-section)")
p4b <- forest(size_df,        "(b) NEC size mix (pp)") +
       theme(plot.title = element_text(size = 12))
p4c <- forest(formal_df,      "(c) NEC formality (pp)") +
       theme(plot.title = element_text(size = 12))
p4d <- forest(entry_df,       "(d) NEC entry by size (log, 2011-2018 panel)") +
       theme(plot.title = element_text(size = 12))

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  p4 <- (p4a + p4b) / (p4c + p4d) +
    plot_annotation(
      title = "Firm-side response: fewer, smaller, less formal firms",
      subtitle = "NEC 2018 cross-section + NEC panel 2011-2018; A4 saturated, lag-2 log-z. 95% CI.",
      caption = "(a) % of mean.  (b,c) percentage points (pp).  (d) log new-firm count by cohort.")
  ggsave("district-analysis/output/fig/fig4_firm_response.png",
         p4, width = 13, height = 8.8, dpi = 200, bg = "white")
} else {
  ggsave("district-analysis/output/fig/fig4a_scale.png",   p4a,
         width = 6.5, height = 3.2, dpi = 200, bg = "white")
  ggsave("district-analysis/output/fig/fig4b_size.png",    p4b,
         width = 6.5, height = 3.2, dpi = 200, bg = "white")
  ggsave("district-analysis/output/fig/fig4c_formal.png",  p4c,
         width = 6.5, height = 4.0, dpi = 200, bg = "white")
  ggsave("district-analysis/output/fig/fig4d_entry.png",   p4d,
         width = 6.5, height = 3.2, dpi = 200, bg = "white")
}
cat("Wrote fig4_firm_response.png\n")
cat("\nDone. View PNGs in district-analysis/output/fig/\n")
