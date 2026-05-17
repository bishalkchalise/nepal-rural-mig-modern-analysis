################################################################################
# Diagnostic: characterise the thin-coverage districts identified by the
# drop-thin-cov robustness check. Are they systematically extreme on the FX
# shock z? Concentrated in a particular region?
#
# Output: district-analysis/output/tab/thin_cov_diagnose.csv
#         district-analysis/output/tab/thin_cov_district_list.csv
#
# Run: source("district-analysis/script/_diagnose_thin_cov.R")
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
})

SKIP_RUN <- TRUE
suppressMessages(suppressWarnings({
  source("district-analysis/script/_robustness_all_panels.R")
}))

# 1) destination count per district from sh_v2 (2009-10 shares)
n_dest <- sh_v2 %>%
  filter(share > 0) %>%
  group_by(dname) %>%
  summarise(n_dest = n_distinct(country), .groups = "drop")
q1 <- quantile(n_dest$n_dest, 0.25, na.rm = TRUE)
thin_districts <- n_dest %>% filter(n_dest <= q1) %>% pull(dname)

# 2) z values per district (averaged across the lag-2 panel years)
z_by_dist <- z_v2 %>%
  group_by(dname) %>%
  summarise(z_mean = mean(z_v2, na.rm = TRUE),
            z_sd   = sd(z_v2, na.rm = TRUE),
            .groups = "drop")

# 3) mi values per district
mi_by_dist <- mi %>% select(dname, log_mi_z, mi_raw)

# 4) Top destination share per district (how concentrated is the share?)
top_share <- sh_v2 %>%
  filter(share > 0) %>%
  group_by(dname) %>%
  arrange(desc(share), .by_group = TRUE) %>%
  summarise(top_dest         = country[1],
            top_share        = share[1],
            top3_share       = sum(share[1:min(3, n())]),
            top5_share       = sum(share[1:min(5, n())]),
            .groups = "drop")

# 5) Region assignment (uses 6 regional shares; pick max as primary region)
region_main <- regions %>%
  pivot_longer(-dname, names_to = "region", values_to = "share") %>%
  group_by(dname) %>%
  slice_max(share, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(dname, main_region = region, main_region_share = share)

# Combine
combined <- n_dest %>%
  left_join(z_by_dist,  by = "dname") %>%
  left_join(mi_by_dist, by = "dname") %>%
  left_join(top_share,  by = "dname") %>%
  left_join(region_main,by = "dname") %>%
  mutate(thin = if_else(dname %in% thin_districts, "thin", "well-covered")) %>%
  arrange(n_dest)

# Stats by thin / well-covered
cat("=== Counts ===\n")
print(combined %>% count(thin))

cat("\n=== Z (FX shock) distribution by thin status ===\n")
print(combined %>%
        group_by(thin) %>%
        summarise(across(c(z_mean, z_sd, log_mi_z, mi_raw, n_dest, top_share, top5_share),
                         list(mean = ~ mean(.x, na.rm = TRUE),
                              sd   = ~ sd(.x,   na.rm = TRUE)),
                         .names = "{.col}_{.fn}"),
                  .groups = "drop"))

cat("\n=== T-test: z_mean and log_mi_z thin vs well-covered ===\n")
print(t.test(z_mean  ~ thin, data = combined))
print(t.test(log_mi_z ~ thin, data = combined))

cat("\n=== Region composition by thin status ===\n")
print(combined %>% count(thin, main_region) %>% pivot_wider(names_from = thin, values_from = n, values_fill = 0))

# Write outputs
dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(combined, "district-analysis/output/tab/thin_cov_diagnose.csv")
write_csv(combined %>% filter(thin == "thin") %>% select(dname, n_dest, z_mean, log_mi_z, top_dest, top_share, main_region),
          "district-analysis/output/tab/thin_cov_district_list.csv")

cat(sprintf("\nWrote thin_cov diagnostics to district-analysis/output/tab/\n"))
cat("\nThin-coverage districts (sorted by n_dest):\n")
print(combined %>% filter(thin == "thin") %>%
        select(dname, n_dest, log_mi_z, top_dest, top_share, main_region) %>%
        arrange(n_dest))
