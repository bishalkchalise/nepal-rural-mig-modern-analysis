################################################################################
# Robustness: Goldsmith-Pinkham / Rotemberg destination weights
# ---------------------------------------------------------------------------
# For an SSIV z(d,t) = Σ_k sh(d,k) * fx_k(t), the aggregate β decomposes
# (approximately) as Σ_k α_k β_k where:
#   α_k = variance-share weight of destination k in the aggregate z
#   β_k = "just-identified" estimate using only destination k's shifter
#
# Diagnostic: if a few destinations carry most of α_k, the SSIV identifies
# off those destinations. The exclusion restriction must hold for those
# destinations in particular (Goldsmith-Pinkham, Sorkin & Swift 2020).
#
# Output:
#   district-analysis/output/tab/rotemberg_weights.csv
#     destination, alpha_k (variance share), n_districts_share>0
#   district-analysis/output/tab/rotemberg_beta_k.csv
#     destination, outcome, dataset, beta_k, se_k, p_k, sig_k, n
#
# Run: source("district-analysis/script/_robustness_rotemberg.R")
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

t0 <- Sys.time()

SKIP_RUN <- TRUE
suppressMessages(suppressWarnings({
  source("district-analysis/script/_robustness_all_panels.R")
}))

stars <- function(p) ifelse(is.na(p), "",
  ifelse(p<0.01,"***", ifelse(p<0.05,"**", ifelse(p<0.10,"*",""))))

# ---- Build destination-specific z_k panels --------------------------------
# z_k(d,t) = sh(d,k) * fx_k(t) for each destination k in the 26-country set.
#
# The aggregate z(d,t) used in the main robustness is Σ_k z_k(d,t).
# We reconstruct z_k from sh_v2 (district × country shares, 2009-10 averaged
# already) and the fx panel (country × year rer).

# sh_v2: (dname, country, share)
# fx: created inside the main script as a tibble (country, year, rer)
# Need to reconstruct fx (defined inside main script; pull it out)
fx_panel <- forex %>%
  filter(!country %in% c("Nepal", "India"), !is.na(forex)) %>%
  transmute(country, year, lcu_per_usd = forex) %>%
  inner_join(forex %>% filter(country == "Nepal") %>%
               transmute(year, npr_per_usd = forex),
             by = "year") %>%
  mutate(log_npr_per_lcu = log(npr_per_usd / lcu_per_usd)) %>%
  filter(!is.na(log_npr_per_lcu)) %>%
  group_by(country) %>%
  mutate(base_2010 = log_npr_per_lcu[year == 2010][1]) %>%
  ungroup() %>%
  filter(!is.na(base_2010)) %>%
  mutate(rer = log_npr_per_lcu - base_2010) %>%
  select(country, year, rer)

dest_set <- intersect(unique(sh_v2$country), unique(fx_panel$country))
cat(sprintf("Destinations in Rotemberg decomposition: %d\n", length(dest_set)))
cat(sprintf("Destinations: %s\n", paste(sort(dest_set), collapse = ", ")))

# Build z_k(d, t) for each destination
zk_list <- list()
for (k in dest_set) {
  shk <- sh_v2 %>% filter(country == k) %>% select(dname, share)
  fxk <- fx_panel %>% filter(country == k) %>% select(year, rer)
  zk <- expand.grid(dname = unique(sh_v2$dname),
                    year  = unique(fx_panel$year),
                    stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    left_join(shk, by = "dname") %>%
    left_join(fxk, by = "year") %>%
    mutate(share = replace_na(share, 0),
           rer   = replace_na(rer, 0),
           z_k   = share * rer) %>%
    select(dname, year, z_k)
  zk_list[[k]] <- zk
}

# ---- Compute variance-share weights α_k -----------------------------------
# Restrict to the union of years actually used by the panels (2009 ≤ t ≤ 2021,
# given census 2011 lag-2 -> 2009 and census 2021 lag-0 -> 2021).
yrs_use <- 2009:2021
total_ss <- 0
ss_by_k  <- numeric(length(dest_set)); names(ss_by_k) <- dest_set
ndist_by_k <- numeric(length(dest_set)); names(ndist_by_k) <- dest_set
for (k in dest_set) {
  zk <- zk_list[[k]] %>% filter(year %in% yrs_use)
  ss <- sum(zk$z_k^2, na.rm = TRUE)
  ss_by_k[k] <- ss
  total_ss <- total_ss + ss
  ndist_by_k[k] <- sum(sh_v2$country == k & sh_v2$share > 0, na.rm = TRUE)
}
alpha <- ss_by_k / total_ss

rotemberg_w <- tibble(
  destination = dest_set,
  alpha_k     = alpha[dest_set],
  alpha_k_pct = 100 * alpha[dest_set],
  n_districts_share_positive = ndist_by_k[dest_set]
) %>% arrange(desc(alpha_k))

dir.create("district-analysis/output/tab", recursive = TRUE, showWarnings = FALSE)
write_csv(rotemberg_w, "district-analysis/output/tab/rotemberg_weights.csv")

cat("\n=== Rotemberg variance-share weights (top 15) ===\n")
print(head(rotemberg_w, 15))

cat(sprintf("\nTop destination carries %.1f%% of total z-variance\n", 100 * alpha[which.max(alpha)]))
cat(sprintf("Top 3 destinations carry %.1f%% combined\n",
            100 * sum(sort(alpha, decreasing = TRUE)[1:3])))
cat(sprintf("Top 5 destinations carry %.1f%% combined\n",
            100 * sum(sort(alpha, decreasing = TRUE)[1:5])))

# ---- Compute β_k for headline outcomes -----------------------------------
# For each headline outcome, run M4 with z_inter = (z_k * log_mi_z), i.e.
# replace the aggregate z with destination-k's contribution. Use baseline
# lag = 2, scaling = log. Cluster ~ dname.

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Dynamic HEADLINE list = outcomes significant at baseline (M4, log, lag2)
sig_outcomes <- list()
main_csv <- "district-analysis/output/tab/robustness_all_panels.csv"
if (file.exists(main_csv)) {
  rg <- read_csv(main_csv, show_col_types = FALSE) %>%
    filter(model == "M4", scaling == "log", lag == 2L, !is.na(p), p < 0.10)
  for (ds in unique(rg$dataset)) {
    sig_outcomes[[ds]] <- rg %>% filter(dataset == ds) %>% pull(outcome) %>% unique()
  }
  cat("Outcomes per dataset significant at baseline:\n")
  for (ds in names(sig_outcomes)) cat(sprintf("  %s: %d\n", ds, length(sig_outcomes[[ds]])))
}

HEADLINE <- list(
  list(ds="census",    panel=cdf, mode="dname", refyr=2011L,        outs=sig_outcomes$census    %||% character()),
  list(ds="hh",        panel=hh,  mode="hhid",  refyr=2016L,        outs=sig_outcomes$hh        %||% character()),
  list(ds="nec_cs",    panel=ncs, mode="cs",    refyr=NA_integer_,  outs=sig_outcomes$nec_cs    %||% character()),
  list(ds="nec_panel", panel=if (exists("npd")) npd else NULL, mode="dname", refyr=2011L, outs=sig_outcomes$nec_panel %||% character())
)

run_destk_one <- function(panel, ycol, mode, refyr, zk_panel) {
  # Replace z columns in panel with destination-k specific z.
  pn <- panel
  pn$z_L_k <- NA_real_

  if (mode == "cs") {
    # Cross-section (no year column on panel). Pick z_k at fixed year
    # 2018 - BASELINE_LAG (same convention as the main NEC cs build).
    yr <- 2018L - BASELINE_LAG
    zk_at <- zk_panel %>% filter(year == yr) %>%
               select(dname, z_k_lagged = z_k)
    pn <- pn %>% left_join(zk_at, by = "dname")
  } else {
    # Panel: match z_k by (dname, year - BASELINE_LAG)
    zk_lag <- zk_panel %>% mutate(year = year + BASELINE_LAG) %>%
                select(dname, year, z_k_lagged = z_k)
    pn <- pn %>% left_join(zk_lag, by = c("dname","year"))
  }

  if (!"mig_var" %in% names(pn)) {
    pn$mig_var <- pn[[mi_col_for("log")]]
  }
  if (all(is.na(pn$z_k_lagged))) return(NULL)
  pn$z_L_std <- (pn$z_k_lagged - mean(pn$z_k_lagged, na.rm = TRUE)) /
                sd(pn$z_k_lagged, na.rm = TRUE)
  pn$z_inter <- pn$z_L_std * pn$mig_var
  pn$z_bare  <- pn$z_L_std
  pn <- pn[!is.na(pn$z_L_std) & !is.na(pn$mig_var) & !is.na(pn[[ycol]]), ]
  if (nrow(pn) < 20) return(NULL)
  if (mode == "cs") {
    f <- as.formula(sprintf("%s ~ z_inter + mig_var + z_bare + %s",
                            ycol, paste(REGION_COLS, collapse = " + ")))
    fit <- tryCatch(feols(f, data = pn, vcov = "hetero"), error = function(e) NULL)
  } else {
    region_terms <- paste(sprintf("i(year, %s, ref = %d)", REGION_COLS, refyr),
                          collapse = " + ")
    f <- as.formula(sprintf(
      "%s ~ z_inter + i(year, mig_var, ref = %d) + z_bare + %s | %s + year",
      ycol, refyr, region_terms, mode))
    fit <- tryCatch(feols(f, data = pn, cluster = ~dname), error = function(e) NULL)
  }
  if (is.null(fit)) return(NULL)
  s <- summary(fit)$coeftable
  if (!"z_inter" %in% rownames(s)) return(NULL)
  tibble(beta_k = s["z_inter","Estimate"],
         se_k   = s["z_inter","Std. Error"],
         p_k    = s["z_inter","Pr(>|t|)"],
         n      = nobs(fit))
}

cat("\n========== Running destination-specific betas (M4 baseline) ==========\n")
bk_rows <- list()
for (k in dest_set) {
  zk <- zk_list[[k]]
  for (h in HEADLINE) {
    if (is.null(h$panel)) next
    for (yc in h$outs) {
      if (!yc %in% names(h$panel)) next
      r <- run_destk_one(h$panel, yc, h$mode, h$refyr, zk)
      if (is.null(r)) next
      bk_rows[[length(bk_rows)+1]] <- tibble(
        destination = k, dataset = h$ds, outcome = yc,
        beta_k = r$beta_k, se_k = r$se_k, p_k = r$p_k,
        sig_k = stars(r$p_k), n = r$n)
    }
  }
  cat(sprintf("  %s done (%d outcomes)\n", k,
              sum(map_lgl(bk_rows, ~ .x$destination == k))))
}

beta_k_df <- bind_rows(bk_rows) %>%
  left_join(rotemberg_w %>% select(destination, alpha_k_pct), by = "destination") %>%
  arrange(dataset, outcome, desc(alpha_k_pct))

write_csv(beta_k_df,
          "district-analysis/output/tab/rotemberg_beta_k.csv")

cat(sprintf("\nWrote %d (dest x outcome) rows to rotemberg_beta_k.csv\n",
            nrow(beta_k_df)))
cat(sprintf("Elapsed: %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))
