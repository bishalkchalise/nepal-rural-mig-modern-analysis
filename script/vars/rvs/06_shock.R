##############################################################################
# NRVS STAGE 2: SOCIAL PROTECTION & SHOCKS/COPING OUTCOMES (HH x YEAR)
# v2 — patched pubcashid classifier covering all 14 observed programs
##############################################################################
#
# Inputs:
#   <base>/social_protection/section_14a.csv   cash transfers by pubcashid
#   <base>/social_protection/section_14b.csv   in-kind by pubkindid
#   <base>/social_protection/section_14c.csv   public works by publicworkid
#   <base>/transfers/section_13a.csv           private gifts received
#   <base>/transfers/section_13c.csv           NGO assistance
#   <base>/transfers/section_13d.csv           remittances
#   <base>/shocks_coping/section_15a.csv       shock list + coping strategies
#
# Outputs:
#   <out>/social_protection_hh_year.csv
#   <out>/shocks_coping_hh_year.csv
#   <out>/shocks_socprot_codebook.csv
#
# =============================================================================
# SOCIAL PROTECTION CATEGORIES (from 14a pubcashid labels)
# =============================================================================
# Regular demographic social protection:
#   oldage              — Old Age Pension
#   widow               — Widow Pension
#   disability          — Disability Allowance
#   child_grant         — Child Grant
#   endangered_ethnic   — Endangered Ethnicities
#   maternal_incentive  — Maternal Incentive Scheme (CCT)
#   scholarship         — Government cash scholarship (if any)
#
# Disaster / emergency-response:
#   eq_relief_gov        — Earthquake Relief (govt)
#   eq_relief_ngo        — Earthquake Relief (non-govt)
#   emergency_adhoc_gov  — Emergency ad-hoc (health/house, govt)
#   emergency_adhoc_ngo  — Emergency ad-hoc (health/house, NGO)
#
# Catastrophic / political-legacy:
#   health_assistance    — Heart / cancer / alzheimers coverage
#   martyr_benefit       — Martyr's Family Benefits
#   movement_victim      — People's Movement Victim Benefits
#
# Residual:
#   other_cash           — anything else
#
# =============================================================================

library(tidyverse)
library(fs)

base_in  <- "data/raw/RVS Data/clean"
base_out <- "data/clean/rvs_outcomes"
dir_create(base_out, recurse = TRUE)

# ---- helpers -------------------------------------------------------------
yn01 <- function(x) {
  if (is.numeric(x)) return(as.integer(x > 0))
  x <- as.character(x)
  case_when(
    is.na(x)                        ~ NA_integer_,
    str_detect(tolower(x), "^yes")  ~ 1L,
    str_detect(tolower(x), "^no")   ~ 0L,
    TRUE                             ~ NA_integer_
  )
}
na0 <- function(x) ifelse(is.na(x), 0, x)

##############################################################################
# 1. SOCIAL PROTECTION: CASH TRANSFERS (14a)
##############################################################################
sp14a <- read_csv(file.path(base_in, "social_protection/section_14a.csv"),
                  show_col_types = FALSE)

cat("\n==== 14a programs (pubcashid) ====\n")
print(sp14a %>% count(pubcashid, sort = TRUE), n = Inf)

# Patched classifier — matches all 14 observed programs.
# Order matters: more specific patterns BEFORE more general ones.
classify_cash <- function(p) {
  p_low <- tolower(p)
  case_when(
    str_detect(p_low, "aware of none")                     ~ NA_character_,
    str_detect(p_low, "old age|senior|elder")              ~ "oldage",
    str_detect(p_low, "widow|single women")                ~ "widow",
    str_detect(p_low, "disab")                              ~ "disability",
    str_detect(p_low, "child grant")                       ~ "child_grant",
    str_detect(p_low, "endangered")                        ~ "endangered_ethnic",
    str_detect(p_low, "maternal")                          ~ "maternal_incentive",
    str_detect(p_low, "scholar")                           ~ "scholarship",
    str_detect(p_low, "earthquake relief.*non-gov")        ~ "eq_relief_ngo",
    str_detect(p_low, "earthquake relief")                 ~ "eq_relief_gov",
    str_detect(p_low, "emergency ad-hoc.*non-gov")         ~ "emergency_adhoc_ngo",
    str_detect(p_low, "emergency ad-hoc")                  ~ "emergency_adhoc_gov",
    str_detect(p_low, "health assistance")                 ~ "health_assistance",
    str_detect(p_low, "martyr")                            ~ "martyr_benefit",
    str_detect(p_low, "people.*movement|movement victim")   ~ "movement_victim",
    TRUE                                                    ~ "other_cash"
  )
}

sp14a_long <- sp14a %>%
  transmute(
    hhid, year, pubcashid,
    received     = yn01(s14q02),
    amount       = na0(as.numeric(s14q04a)),
    installments = na0(as.numeric(s14q04c))
  ) %>%
  mutate(program = classify_cash(pubcashid)) %>%
  filter(!is.na(program))   # drop "Aware of None"

cat("\n==== 14a classifier mapping check ====\n")
sp14a_long %>% count(program, pubcashid) %>% arrange(program) %>% print(n = Inf)

# Collapse to HH-year with program-specific flags and amounts
sp_cash <- sp14a_long %>%
  group_by(hhid, year, program) %>%
  summarise(rec = as.integer(max(received, na.rm = TRUE) == 1),
            amt = sum(amount),
            .groups = "drop") %>%
  pivot_wider(
    names_from  = program,
    values_from = c(rec, amt),
    values_fill = 0,
    names_glue  = "sp_cash_{program}_{.value}"
  )

# HH-year top-level aggregates
sp_cash_agg <- sp14a_long %>%
  group_by(hhid, year) %>%
  summarise(sp_cash_any_rec   = as.integer(any(received == 1, na.rm = TRUE)),
            sp_cash_total_amt = sum(amount, na.rm = TRUE),
            .groups = "drop")

sp_cash <- sp_cash %>% left_join(sp_cash_agg, by = c("hhid", "year"))

# Composite disaster vs demographic buckets (handles columns that may not
# exist if a program is absent in a given year)
cols_exist <- function(df, nms) intersect(nms, names(df))

disaster_rec_cols <- cols_exist(sp_cash, c(
  "sp_cash_eq_relief_gov_rec", "sp_cash_eq_relief_ngo_rec",
  "sp_cash_emergency_adhoc_gov_rec", "sp_cash_emergency_adhoc_ngo_rec"
))
disaster_amt_cols <- cols_exist(sp_cash, c(
  "sp_cash_eq_relief_gov_amt", "sp_cash_eq_relief_ngo_amt",
  "sp_cash_emergency_adhoc_gov_amt", "sp_cash_emergency_adhoc_ngo_amt"
))
demo_rec_cols <- cols_exist(sp_cash, c(
  "sp_cash_oldage_rec", "sp_cash_widow_rec", "sp_cash_disability_rec",
  "sp_cash_child_grant_rec", "sp_cash_maternal_incentive_rec",
  "sp_cash_endangered_ethnic_rec"
))
demo_amt_cols <- cols_exist(sp_cash, c(
  "sp_cash_oldage_amt", "sp_cash_widow_amt", "sp_cash_disability_amt",
  "sp_cash_child_grant_amt", "sp_cash_maternal_incentive_amt",
  "sp_cash_endangered_ethnic_amt"
))

sp_cash <- sp_cash %>%
  mutate(
    sp_cash_disaster_rec    = if (length(disaster_rec_cols)) as.integer(rowSums(across(all_of(disaster_rec_cols))) > 0) else 0L,
    sp_cash_disaster_amt    = if (length(disaster_amt_cols)) rowSums(across(all_of(disaster_amt_cols))) else 0,
    sp_cash_demographic_rec = if (length(demo_rec_cols))     as.integer(rowSums(across(all_of(demo_rec_cols))) > 0) else 0L,
    sp_cash_demographic_amt = if (length(demo_amt_cols))     rowSums(across(all_of(demo_amt_cols))) else 0
  )

##############################################################################
# 2. SOCIAL PROTECTION: IN-KIND TRANSFERS (14b)
##############################################################################
sp14b <- read_csv(file.path(base_in, "social_protection/section_14b.csv"),
                  show_col_types = FALSE)

sp_kind <- sp14b %>%
  transmute(
    hhid, year,
    received = yn01(s14q11),
    amount   = na0(as.numeric(s14q13a_q)) + na0(as.numeric(s14q13b_q))
  ) %>%
  group_by(hhid, year) %>%
  summarise(sp_kind_rec = as.integer(any(received == 1, na.rm = TRUE)),
            sp_kind_amt = sum(amount), .groups = "drop")

##############################################################################
# 3. SOCIAL PROTECTION: PUBLIC WORKS (14c)
##############################################################################
sp14c <- read_csv(file.path(base_in, "social_protection/section_14c.csv"),
                  show_col_types = FALSE)

sp_pw <- sp14c %>%
  transmute(
    hhid, year, publicworkid,
    participated = yn01(s14q17),
    days         = na0(as.numeric(s14q19)),
    earnings     = na0(as.numeric(s14q21a)) +
      na0(as.numeric(s14q22a)) +
      na0(as.numeric(s14q22b))
  ) %>%
  filter(!is.na(publicworkid),
         !str_detect(tolower(publicworkid), "aware of none")) %>%
  group_by(hhid, year) %>%
  summarise(sp_pw_rec        = as.integer(any(participated == 1, na.rm = TRUE)),
            sp_pw_days       = sum(days),
            sp_pw_earnings   = sum(earnings),
            sp_pw_n_programs = sum(participated, na.rm = TRUE),
            .groups = "drop")

##############################################################################
# 4. PRIVATE GIFTS RECEIVED (13a)
##############################################################################
tr13a <- read_csv(file.path(base_in, "transfers/section_13a.csv"),
                  show_col_types = FALSE)

tr_gift <- tr13a %>%
  transmute(
    hhid, year,
    cash   = na0(as.numeric(s13q07a)),
    inkind = na0(as.numeric(s13q07b))
  ) %>%
  group_by(hhid, year) %>%
  summarise(gift_rec        = 1L,
            gift_cash_amt   = sum(cash),
            gift_inkind_amt = sum(inkind),
            gift_total_amt  = sum(cash) + sum(inkind),
            gift_n_episodes = n(),
            .groups = "drop")

##############################################################################
# 5. NGO ASSISTANCE (13c)
##############################################################################
tr13c <- read_csv(file.path(base_in, "transfers/section_13c.csv"),
                  show_col_types = FALSE)

tr_ngo <- tr13c %>%
  transmute(
    hhid, year,
    cash   = na0(as.numeric(s13q19a)),
    inkind = na0(as.numeric(s13q19c))
  ) %>%
  group_by(hhid, year) %>%
  summarise(ngo_rec        = 1L,
            ngo_cash_amt   = sum(cash),
            ngo_inkind_amt = sum(inkind),
            ngo_total_amt  = sum(cash) + sum(inkind),
            .groups = "drop")

##############################################################################
# 6. REMITTANCES (13d)
##############################################################################
tr13d <- read_csv(file.path(base_in, "transfers/section_13d.csv"),
                  show_col_types = FALSE)

tr_remit <- tr13d %>%
  transmute(
    hhid, year,
    remit_cash   = yn01(s13q22),
    remit_inkind = yn01(s13q24a),
    amount       = na0(as.numeric(s13q23))
  ) %>%
  group_by(hhid, year) %>%
  summarise(remit_rec        = as.integer(any(remit_cash == 1, na.rm = TRUE)),
            remit_inkind_rec = as.integer(any(remit_inkind == 1, na.rm = TRUE)),
            remit_amt        = sum(amount),
            .groups = "drop")

##############################################################################
# 7. ASSEMBLE SOCIAL PROTECTION PANEL
##############################################################################
spine <- bind_rows(
  sp_cash  %>% select(hhid, year),
  sp_kind  %>% select(hhid, year),
  sp_pw    %>% select(hhid, year),
  tr_gift  %>% select(hhid, year),
  tr_ngo   %>% select(hhid, year),
  tr_remit %>% select(hhid, year)
) %>% distinct()

sp_panel <- spine %>%
  left_join(sp_cash,  by = c("hhid", "year")) %>%
  left_join(sp_kind,  by = c("hhid", "year")) %>%
  left_join(sp_pw,    by = c("hhid", "year")) %>%
  left_join(tr_gift,  by = c("hhid", "year")) %>%
  left_join(tr_ngo,   by = c("hhid", "year")) %>%
  left_join(tr_remit, by = c("hhid", "year")) %>%
  mutate(across(-c(hhid, year), ~ replace_na(., 0)))

sp_panel <- sp_panel %>%
  mutate(
    any_public_support  = pmax(sp_cash_any_rec, sp_kind_rec, sp_pw_rec),
    any_private_support = pmax(gift_rec, ngo_rec),
    any_remittance      = remit_rec,
    any_support         = pmax(any_public_support, any_private_support, any_remittance)
  )

write_csv(sp_panel, file.path(base_out, "social_protection_hh_year.csv"))
cat("\nWrote", file.path(base_out, "social_protection_hh_year.csv"),
    "-- rows:", nrow(sp_panel), "\n")

##############################################################################
# 8. SHOCKS & COPING (15a)
##############################################################################
sh15a <- read_csv(file.path(base_in, "shocks_coping/section_15a.csv"),
                  show_col_types = FALSE)

classify_cope <- function(txt) {
  t <- tolower(as.character(txt))
  case_when(
    is.na(t) | t == "none" | t == ""                                         ~ "none",
    str_detect(t, "savings|spent savings|reduced consum|home.?grown|less food") ~ "savings",
    str_detect(t, "borrow|loan|credit")                                      ~ "borrow",
    str_detect(t, "sold|sell|livestock sale|asset")                          ~ "sell_assets",
    str_detect(t, "government|ngo|aid|assistance|relief|grant")              ~ "gov_transfer",
    str_detect(t, "relative|friend|neighbor|family|community|gift")          ~ "private_transfer",
    str_detect(t, "migrat|abroad|foreign|india|remit|sent.*work")            ~ "remittance",
    TRUE                                                                      ~ "other"
  )
}

shocks_per_row <- sh15a %>%
  transmute(
    hhid, year, shockid,
    loss_value            = na0(as.numeric(s15q03)),
    cope_private_transfer = yn01(s15q06a_1) | yn01(s15q06a_2) |
      yn01(s15q06b_1) | yn01(s15q06b_2),
    cope_sell_assets      = yn01(s15q07a),
    cope_savings          = yn01(s15q08a),
    cope_borrow           = yn01(s15q09a),
    cope_gov_transfer     = yn01(s15q10a),
    cope_remittance       = yn01(s15q11a),
    cope_other_mech       = yn01(s15q12a),
    primary_cope          = classify_cope(s15q13a),
    secondary_cope        = classify_cope(s15q13b)
  ) %>%
  mutate(across(starts_with("cope_"), ~ as.integer(replace_na(., FALSE))))

shocks_hh <- shocks_per_row %>%
  group_by(hhid, year) %>%
  summarise(
    any_shock     = 1L,
    n_shocks      = n(),
    total_loss_rs = sum(loss_value),
    across(starts_with("cope_"), ~ as.integer(any(. == 1, na.rm = TRUE))),
    primary_savings          = as.integer(any(primary_cope == "savings"          | secondary_cope == "savings")),
    primary_borrow           = as.integer(any(primary_cope == "borrow"           | secondary_cope == "borrow")),
    primary_sell_assets      = as.integer(any(primary_cope == "sell_assets"      | secondary_cope == "sell_assets")),
    primary_gov_transfer     = as.integer(any(primary_cope == "gov_transfer"     | secondary_cope == "gov_transfer")),
    primary_private_transfer = as.integer(any(primary_cope == "private_transfer" | secondary_cope == "private_transfer")),
    primary_remittance       = as.integer(any(primary_cope == "remittance"       | secondary_cope == "remittance")),
    primary_none             = as.integer(all(primary_cope == "none")),
    .groups = "drop"
  )

shock_types <- shocks_per_row %>%
  mutate(type = case_when(
    str_detect(tolower(shockid), "disease|injury|illness")                               ~ "illness",
    str_detect(tolower(shockid), "death")                                                 ~ "death",
    str_detect(tolower(shockid), "earthquake|landslide|flood|hail|lightening|fire")       ~ "natural_disaster",
    str_detect(tolower(shockid), "drought|pest|plant|livestock|post harvest")             ~ "agricultural",
    str_detect(tolower(shockid), "price|fuel|blockag|riot")                               ~ "economic_price",
    str_detect(tolower(shockid), "job|bankruptcy|contract|default|displace")              ~ "economic_job",
    TRUE                                                                                   ~ "other_shock"
  )) %>%
  distinct(hhid, year, type) %>%
  mutate(val = 1L) %>%
  pivot_wider(names_from = type, values_from = val, values_fill = 0L,
              names_prefix = "shock_type_")

shocks_hh <- shocks_hh %>% left_join(shock_types, by = c("hhid", "year"))

write_csv(shocks_hh, file.path(base_out, "shocks_coping_hh_year.csv"))
cat("Wrote", file.path(base_out, "shocks_coping_hh_year.csv"),
    "-- rows:", nrow(shocks_hh), "\n")

##############################################################################
# 9. CODEBOOK
##############################################################################
codebook <- tribble(
  ~variable,                              ~source,          ~definition,
  # --- 14a per-program flags/amounts ---
  "sp_cash_oldage_rec",                   "14a",            "1 if HH received Old Age Pension",
  "sp_cash_oldage_amt",                   "14a s14q04a",    "Old-age pension annual amount (Rs.)",
  "sp_cash_widow_rec",                    "14a",            "1 if HH received Widow Pension",
  "sp_cash_widow_amt",                    "14a s14q04a",    "Widow pension annual amount (Rs.)",
  "sp_cash_disability_rec",               "14a",            "1 if HH received Disability Allowance",
  "sp_cash_disability_amt",               "14a s14q04a",    "Disability allowance amount (Rs.)",
  "sp_cash_child_grant_rec",              "14a",            "1 if HH received Child Grant",
  "sp_cash_child_grant_amt",              "14a s14q04a",    "Child grant amount (Rs.)",
  "sp_cash_endangered_ethnic_rec",        "14a",            "1 if HH received Endangered Ethnicities allowance",
  "sp_cash_endangered_ethnic_amt",        "14a s14q04a",    "Endangered-ethnic amount (Rs.)",
  "sp_cash_maternal_incentive_rec",       "14a",            "1 if HH received Maternal Incentive Scheme",
  "sp_cash_maternal_incentive_amt",       "14a s14q04a",    "Maternal incentive amount (Rs.)",
  "sp_cash_scholarship_rec",              "14a",            "1 if HH received govt scholarship (14a)",
  "sp_cash_scholarship_amt",              "14a s14q04a",    "Govt scholarship amount (Rs.)",
  "sp_cash_eq_relief_gov_rec",            "14a",            "1 if HH received earthquake relief (govt)",
  "sp_cash_eq_relief_gov_amt",            "14a s14q04a",    "EQ relief from government (Rs.)",
  "sp_cash_eq_relief_ngo_rec",            "14a",            "1 if HH received earthquake relief (NGO)",
  "sp_cash_eq_relief_ngo_amt",            "14a s14q04a",    "EQ relief from NGO (Rs.)",
  "sp_cash_emergency_adhoc_gov_rec",      "14a",            "1 if HH received emergency ad-hoc (govt)",
  "sp_cash_emergency_adhoc_gov_amt",      "14a s14q04a",    "Emergency ad-hoc (govt, Rs.)",
  "sp_cash_emergency_adhoc_ngo_rec",      "14a",            "1 if HH received emergency ad-hoc (NGO)",
  "sp_cash_emergency_adhoc_ngo_amt",      "14a s14q04a",    "Emergency ad-hoc (NGO, Rs.)",
  "sp_cash_health_assistance_rec",        "14a",            "1 if HH received catastrophic health assistance",
  "sp_cash_health_assistance_amt",        "14a s14q04a",    "Health assistance amount (Rs.)",
  "sp_cash_martyr_benefit_rec",           "14a",            "1 if HH received Martyr's Family Benefits",
  "sp_cash_martyr_benefit_amt",           "14a s14q04a",    "Martyr benefit amount (Rs.)",
  "sp_cash_movement_victim_rec",          "14a",            "1 if HH received People's Movement Victim Benefits",
  "sp_cash_movement_victim_amt",          "14a s14q04a",    "Movement victim amount (Rs.)",
  "sp_cash_other_cash_rec",               "14a",            "1 if HH received other unclassified public cash",
  "sp_cash_other_cash_amt",               "14a s14q04a",    "Other public cash amount (Rs.)",
  "sp_cash_any_rec",                      "14a",            "1 if HH received any public cash transfer",
  "sp_cash_total_amt",                    "14a",            "Total public cash transfers (Rs.)",
  "sp_cash_disaster_rec",                 "derived",        "1 if any disaster-triggered cash (EQ/emergency ad-hoc)",
  "sp_cash_disaster_amt",                 "derived",        "Sum of disaster-triggered cash (Rs.)",
  "sp_cash_demographic_rec",              "derived",        "1 if any demographic cash (pension/widow/disab/child/maternal/ethnic)",
  "sp_cash_demographic_amt",              "derived",        "Sum of demographic cash (Rs.)",
  # --- 14b/c + 13 + remit ---
  "sp_kind_rec",                          "14b s14q11",     "1 if HH received public in-kind transfer",
  "sp_kind_amt",                          "14b s14q13a_q+b_q", "In-kind cash value (Rs.)",
  "sp_pw_rec",                            "14c s14q17",     "1 if HH participated in public works",
  "sp_pw_days",                           "14c s14q19",     "Days worked in public works",
  "sp_pw_earnings",                       "14c",            "Public works total earnings (Rs.)",
  "sp_pw_n_programs",                     "14c",            "Number of public works programs",
  "gift_rec",                             "13a",            "1 if HH received private gift",
  "gift_cash_amt",                        "13a s13q07a",    "Private gift cash (Rs.)",
  "gift_inkind_amt",                      "13a s13q07b",    "Private gift in-kind value (Rs.)",
  "gift_total_amt",                       "13a derived",    "Total private gifts (Rs.)",
  "gift_n_episodes",                      "13a",            "Number of gift episodes",
  "ngo_rec",                              "13c",            "1 if HH received NGO assistance",
  "ngo_cash_amt",                         "13c s13q19a",    "NGO cash amount (Rs.)",
  "ngo_inkind_amt",                       "13c s13q19c",    "NGO in-kind (Rs.)",
  "ngo_total_amt",                        "13c derived",    "Total NGO assistance (Rs.)",
  "remit_rec",                            "13d s13q22",     "1 if HH received remittance",
  "remit_inkind_rec",                     "13d s13q24a",    "1 if HH received in-kind remittance",
  "remit_amt",                            "13d s13q23",     "Remittance amount summed per HH-year (Rs.)",
  "any_public_support",                   "derived",        "1 if any of {14a, 14b, 14c} received",
  "any_private_support",                  "derived",        "1 if any of {13a gift, 13c NGO} received",
  "any_remittance",                       "derived",        "1 if HH received remittance (13d)",
  "any_support",                          "derived",        "1 if any support of any kind received",
  # --- shocks ---
  "any_shock",                            "15a",            "1 if HH experienced any shock in 12m",
  "n_shocks",                             "15a",            "Number of shocks experienced",
  "total_loss_rs",                        "15a s15q03",     "Sum of shock loss values (Rs.)",
  "cope_private_transfer",                "15a s15q06",     "1 if coped via community/family support for any shock",
  "cope_sell_assets",                     "15a s15q07a",    "1 if coped by selling assets",
  "cope_savings",                         "15a s15q08a",    "1 if coped via savings/reduced consumption",
  "cope_borrow",                          "15a s15q09a",    "1 if coped by borrowing",
  "cope_gov_transfer",                    "15a s15q10a",    "1 if coped via gov/NGO assistance",
  "cope_remittance",                      "15a s15q11a",    "1 if coped via migration/remittance",
  "cope_other_mech",                      "15a s15q12a",    "1 if coped via other mechanism",
  "primary_savings",                      "15a s15q13a/b",  "'savings' named as primary/secondary",
  "primary_borrow",                       "15a s15q13a/b",  "'borrow' named as primary/secondary",
  "primary_sell_assets",                  "15a s15q13a/b",  "'sell assets' named as primary/secondary",
  "primary_gov_transfer",                 "15a s15q13a/b",  "'gov transfer' named as primary/secondary",
  "primary_private_transfer",             "15a s15q13a/b",  "'private transfer' named as primary/secondary",
  "primary_remittance",                   "15a s15q13a/b",  "'remittance/migration' named as primary/secondary",
  "primary_none",                         "15a s15q13a/b",  "'none' named for all shocks",
  "shock_type_illness",                   "15a shockid",    "1 if illness/injury shock",
  "shock_type_death",                     "15a shockid",    "1 if death in family",
  "shock_type_natural_disaster",          "15a shockid",    "1 if earthquake/flood/landslide/fire/hail",
  "shock_type_agricultural",              "15a shockid",    "1 if drought/pest/livestock/harvest loss",
  "shock_type_economic_price",            "15a shockid",    "1 if price/fuel/blockage shock",
  "shock_type_economic_job",              "15a shockid",    "1 if job loss/bankruptcy/displacement",
  "shock_type_other_shock",               "15a shockid",    "1 if other shock type"
)
write_csv(codebook, file.path(base_out, "shocks_socprot_codebook.csv"))
cat("Wrote", file.path(base_out, "shocks_socprot_codebook.csv"), "\n")

##############################################################################
# 10. SANITY CHECKS
##############################################################################
cat("\n==== SOCIAL PROTECTION: HH x YEAR summary ====\n")
sp_panel %>%
  group_by(year) %>%
  summarise(
    n_hh                 = n(),
    pct_any_support      = round(100*mean(any_support), 1),
    pct_any_public       = round(100*mean(any_public_support), 1),
    pct_any_private      = round(100*mean(any_private_support), 1),
    pct_any_remit        = round(100*mean(any_remittance), 1),
    pct_demographic_cash = round(100*mean(sp_cash_demographic_rec), 1),
    pct_disaster_cash    = round(100*mean(sp_cash_disaster_rec), 1),
    pct_oldage           = round(100*mean(sp_cash_oldage_rec), 1),
    pct_widow            = round(100*mean(sp_cash_widow_rec), 1),
    pct_child_grant      = round(100*mean(sp_cash_child_grant_rec), 1),
    pct_maternal         = round(100*mean(sp_cash_maternal_incentive_rec), 1),
    pct_eq_relief_gov    = round(100*mean(sp_cash_eq_relief_gov_rec), 1),
    mean_remit_amt       = round(mean(remit_amt)),
    mean_pension_amt     = round(mean(sp_cash_oldage_amt)),
    .groups = "drop"
  ) %>% print()

cat("\n==== SHOCKS & COPING: HH x YEAR summary ====\n")
shocks_hh %>%
  group_by(year) %>%
  summarise(
    n_hh                  = n(),
    mean_n_shocks         = round(mean(n_shocks), 2),
    mean_loss_rs          = round(mean(total_loss_rs)),
    pct_cope_savings      = round(100*mean(cope_savings), 1),
    pct_cope_borrow       = round(100*mean(cope_borrow), 1),
    pct_cope_sell_assets  = round(100*mean(cope_sell_assets), 1),
    pct_cope_gov_transfer = round(100*mean(cope_gov_transfer), 1),
    pct_cope_private      = round(100*mean(cope_private_transfer), 1),
    pct_cope_remittance   = round(100*mean(cope_remittance), 1),
    .groups = "drop"
  ) %>% print()

cat("\nDone.\n")