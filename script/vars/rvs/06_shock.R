##############################################################################
# NRVS STAGE 2: SHOCKS & COPING — MINIMAL (3 vars)
##############################################################################
# Inputs:
#   <base>/shocks_coping/section_15a.csv
#
# Outputs:
#   <out>/shocks_coping_hh_year.csv  (3 outcomes)
#   <out>/shocks_codebook.csv
#
# Outcomes:
#   any_shock         — 1 if HH had any shock in 12m
#   coped_self        — 1 if used only own resources (savings, sell assets,
#                       borrowing). 0 if needed outside help. NA if no shock.
#   coped_external    — 1 if used any outside source (govt/NGO, private
#                       transfers, remittances). NA if no shock.
#
# Note: coped_self and coped_external are NOT mutually exclusive — a HH can
# use both. They're separate flags so you can study substitution. If you'd
# rather have a single 3-level variable {self_only, external_any, none},
# that's a one-line tweak.
##############################################################################

library(tidyverse)
library(fs)

base_in  <- "data/raw/RVS Data/clean"
base_out <- "data/clean/rvs_outcomes"
dir_create(base_out, recurse = TRUE)

yn01 <- function(x) {
  if (is.numeric(x)) return(as.integer(x > 0))
  x <- as.character(x)
  case_when(
    is.na(x)                       ~ NA_integer_,
    str_detect(tolower(x), "^yes") ~ 1L,
    str_detect(tolower(x), "^no")  ~ 0L,
    TRUE                           ~ NA_integer_
  )
}

sh15a <- read_csv(file.path(base_in, "shocks_coping/section_15a.csv"),
                  show_col_types = FALSE)

# Per-shock-row coping flags
shocks_per_row <- sh15a %>%
  transmute(
    hhid, year,
    # Self-reliant coping
    cope_savings     = yn01(s14q08a <- s15q08a),  # savings/reduced consumption
    cope_sell_assets = yn01(s15q07a),
    cope_borrow      = yn01(s15q09a),
    # External coping
    cope_private     = yn01(s15q06a_1) | yn01(s15q06a_2) |
      yn01(s15q06b_1) | yn01(s15q06b_2),
    cope_gov         = yn01(s15q10a),
    cope_remittance  = yn01(s15q11a)
  ) %>%
  mutate(across(starts_with("cope_"), ~ as.integer(replace_na(., FALSE))))

# Aggregate to HH × year
shocks_hh <- shocks_per_row %>%
  group_by(hhid, year) %>%
  summarise(
    any_shock = 1L,
    used_self = as.integer(any(cope_savings == 1 |
                                 cope_sell_assets == 1 |
                                 cope_borrow == 1, na.rm = TRUE)),
    used_ext  = as.integer(any(cope_private == 1 |
                                 cope_gov == 1 |
                                 cope_remittance == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    cope_category = case_when(
      used_self == 1 & used_ext == 0 ~ "self_only",
      used_self == 0 & used_ext == 1 ~ "external_only",
      used_self == 1 & used_ext == 1 ~ "both",
      TRUE                           ~ "neither"
    )
  )

shocks_hh %>%
  group_by(year) %>%
  summarise(
    n_hh        = n(),
    pct_self    = round(100*mean(cope_category == "self_only"), 1),
    pct_ext     = round(100*mean(cope_category == "external_only"), 1),
    pct_both    = round(100*mean(cope_category == "both"), 1),
    pct_neither = round(100*mean(cope_category == "neither"), 1),
    .groups = "drop"
  ) %>% print()

write_csv(shocks_hh, file.path(base_out, "shocks_coping_hh_year.csv"))

# Codebook
codebook <- tribble(
  ~variable,        ~source,                ~definition,
  "any_shock",      "15a presence",         "1 if HH reported any shock in past 12m.",
  "coped_self",     "15a s15q07a/08a/09a",  "1 if HH used own resources to cope (savings/reduced consumption, selling assets, or borrowing) for any shock.",
  "coped_external", "15a s15q06/10a/11a",   "1 if HH used outside resources to cope (private transfers from family/community, govt/NGO assistance, or remittances) for any shock."
)
write_csv(codebook, file.path(base_out, "shocks_codebook.csv"))


