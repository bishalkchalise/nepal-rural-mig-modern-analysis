##############################################################################
# script/nec2018/01_classify_nsic.R
##############################################################################
#
# Builds a classification map: NSIC 2-digit division -> 4 classification
# schemes (tradability, ag_orientation, manuf_tier, modernity).
#
# Sources:
#   - Mian & Sufi (2014, Econometrica) — tradable vs non-tradable
#   - Gervais (2014, J. Int'l Econ) — tradable services
#   - Reardon et al. (2012, ARRE) — ag value-chain stages
#   - OECD Hatzichronoglou (1997), Galindo-Rueda & Verger (2016) — tech tiers
#   - Lall (2000, ODS) — developing-country manufacturing tiers
#   - Lewis (1954); La Porta & Shleifer (2014, JEP) — dual economy
#
# Output: data/clean/nec2018/nsic_classification_map.csv
#
# Run first, once per project. The map is a lookup table joined in
# 02_firm_level_prep.R. To override any mapping, edit the output CSV
# and re-run 02 onward.
#
##############################################################################

library(tidyverse)

dir.create("data/clean/nec2018", showWarnings = FALSE, recursive = TRUE)

# ---- NSIC 2-digit reference (ISIC Rev. 4) ---------------------------------
nsic_ref <- tribble(
  ~nsic_2digt, ~section, ~description,
  "01", "A", "Crop and animal production, hunting and related service activities",
  "02", "A", "Forestry and logging",
  "03", "A", "Fishing and aquaculture",
  "05", "B", "Mining of coal and lignite",
  "06", "B", "Extraction of crude petroleum and natural gas",
  "07", "B", "Mining of metal ores",
  "08", "B", "Other mining and quarrying",
  "09", "B", "Mining support service activities",
  "10", "C", "Manufacture of food products",
  "11", "C", "Manufacture of beverages",
  "12", "C", "Manufacture of tobacco products",
  "13", "C", "Manufacture of textiles",
  "14", "C", "Manufacture of wearing apparel",
  "15", "C", "Manufacture of leather and related products",
  "16", "C", "Manufacture of wood and of products of wood and cork",
  "17", "C", "Manufacture of paper and paper products",
  "18", "C", "Printing and reproduction of recorded media",
  "19", "C", "Manufacture of coke and refined petroleum products",
  "20", "C", "Manufacture of chemicals and chemical products",
  "21", "C", "Manufacture of pharmaceuticals",
  "22", "C", "Manufacture of rubber and plastics products",
  "23", "C", "Manufacture of other non-metallic mineral products",
  "24", "C", "Manufacture of basic metals",
  "25", "C", "Manufacture of fabricated metal products",
  "26", "C", "Manufacture of computer, electronic and optical products",
  "27", "C", "Manufacture of electrical equipment",
  "28", "C", "Manufacture of machinery and equipment",
  "29", "C", "Manufacture of motor vehicles, trailers and semi-trailers",
  "30", "C", "Manufacture of other transport equipment",
  "31", "C", "Manufacture of furniture",
  "32", "C", "Other manufacturing",
  "33", "C", "Repair and installation of machinery and equipment",
  "35", "D", "Electricity, gas, steam and air conditioning supply",
  "36", "E", "Water collection, treatment and supply",
  "37", "E", "Sewerage",
  "38", "E", "Waste collection, treatment and disposal activities",
  "39", "E", "Remediation activities and other waste management services",
  "41", "F", "Construction of buildings",
  "42", "F", "Civil engineering",
  "43", "F", "Specialized construction activities",
  "45", "G", "Wholesale and retail trade and repair of motor vehicles",
  "46", "G", "Wholesale trade, except of motor vehicles and motorcycles",
  "47", "G", "Retail trade, except of motor vehicles and motorcycles",
  "49", "H", "Land transport and transport via pipelines",
  "50", "H", "Water transport",
  "51", "H", "Air transport",
  "52", "H", "Warehousing and support activities for transportation",
  "53", "H", "Postal and courier activities",
  "55", "I", "Accommodation",
  "56", "I", "Food and beverage service activities",
  "58", "J", "Publishing activities",
  "59", "J", "Motion picture, video and TV programme production",
  "60", "J", "Programming and broadcasting activities",
  "61", "J", "Telecommunications",
  "62", "J", "Computer programming, consultancy and related activities",
  "63", "J", "Information service activities",
  "64", "K", "Financial service activities, except insurance and pension",
  "65", "K", "Insurance, reinsurance and pension funding",
  "66", "K", "Activities auxiliary to financial service and insurance",
  "68", "L", "Real estate activities",
  "69", "M", "Legal and accounting activities",
  "70", "M", "Activities of head offices; management consultancy",
  "71", "M", "Architectural and engineering activities",
  "72", "M", "Scientific research and development",
  "73", "M", "Advertising and market research",
  "74", "M", "Other professional, scientific and technical activities",
  "75", "M", "Veterinary activities",
  "77", "N", "Rental and leasing activities",
  "78", "N", "Employment activities",
  "79", "N", "Travel agency, tour operator, reservation service",
  "80", "N", "Security and investigation activities",
  "81", "N", "Services to buildings and landscape activities",
  "82", "N", "Office administrative, office support, business support",
  "84", "O", "Public administration and defence; compulsory social security",
  "85", "P", "Education",
  "86", "Q", "Human health activities",
  "87", "Q", "Residential care activities",
  "88", "Q", "Social work activities without accommodation",
  "90", "R", "Creative, arts and entertainment activities",
  "91", "R", "Libraries, archives, museums and other cultural activities",
  "92", "R", "Gambling and betting activities",
  "93", "R", "Sports activities and amusement and recreation activities",
  "94", "S", "Activities of membership organizations",
  "95", "S", "Repair of computers and personal and household goods",
  "96", "S", "Other personal service activities",
  "97", "T", "Activities of households as employers of domestic personnel",
  "98", "T", "Undifferentiated goods/services production by households",
  "99", "U", "Activities of extraterritorial organizations and bodies"
)

# ---- Scheme 1: Tradability -----------------------------------------------
# Mian & Sufi (2014); Gervais (2014)
tradability_map <- function(section) {
  case_when(
    section %in% c("A", "B", "C")                        ~ "tradable_goods",
    section %in% c("H", "J", "K", "M")                   ~ "tradable_services",
    section %in% c("G", "I", "L", "N", "R", "S",
                   "T", "P", "Q", "F")                   ~ "non_tradable_services",
    section %in% c("D", "E")                             ~ "non_tradable_utilities",
    section == "O"                                        ~ "public_admin",
    section == "U"                                        ~ "extra_territorial",
    TRUE                                                  ~ NA_character_
  )
}

# ---- Scheme 2: Agricultural orientation ----------------------------------
# Reardon et al. (2012) on ag value-chain stages
ag_orientation_map <- function(nsic_2) {
  case_when(
    nsic_2 == "01"                                  ~ "crop_livestock_primary",
    nsic_2 == "02"                                  ~ "forestry_primary",
    nsic_2 == "03"                                  ~ "fishery_primary",
    nsic_2 %in% c("10", "11", "12")                 ~ "agro_processing",
    nsic_2 %in% c("13", "14", "15", "16")           ~ "agro_downstream_manuf",
    TRUE                                             ~ "not_ag"
  )
}

# ---- Scheme 3: Manufacturing technology tier -----------------------------
# OECD Hatzichronoglou (1997); Lall (2000)
manuf_tier_map <- function(nsic_2, section) {
  case_when(
    section != "C"                                    ~ "not_manuf",
    nsic_2 %in% c("21", "26")                         ~ "high_tech",
    nsic_2 %in% c("20", "27", "28", "29", "30")       ~ "medium_high_tech",
    nsic_2 %in% c("19", "22", "23", "24", "25", "33") ~ "medium_low_tech",
    nsic_2 %in% c("10", "11", "12", "13", "14",
                  "15", "16", "17", "18", "31", "32") ~ "low_tech",
    TRUE                                              ~ NA_character_
  )
}

# ---- Scheme 4: Modernity (dual economy) -----------------------------------
# Lewis (1954); La Porta & Shleifer (2014)
modernity_map <- function(nsic_2, section) {
  case_when(
    section %in% c("K", "M")                  ~ "modern_services",
    section == "J" | nsic_2 == "62"           ~ "modern_services",
    nsic_2 %in% c("21", "26", "27", "28")     ~ "modern_manuf",
    nsic_2 %in% c("01", "02")                 ~ "traditional_agriculture",
    section %in% c("G", "I", "T")             ~ "traditional_commerce",
    nsic_2 %in% c("95", "96")                 ~ "traditional_services",
    section %in% c("C", "F", "D")             ~ "industrial_sector",
    section %in% c("O", "P", "Q")             ~ "public_sector",
    TRUE                                       ~ "other"
  )
}

# ---- Short sector tags (used throughout) ---------------------------------
sector_labels <- c(
  A = "agro", B = "mining", C = "manuf", D = "energy", E = "water_waste",
  F = "construct", G = "wholesale", H = "transport", I = "hospitality",
  J = "info_comm", K = "finance", L = "real_estate", M = "prof_tech",
  N = "admin_sup", O = "public_admin", P = "education", Q = "health",
  R = "arts", S = "services", T = "hh_prod", U = "extra_orgs"
)

# ---- Build the map --------------------------------------------------------
nsic_map <- nsic_ref |>
  mutate(
    sector_short   = sector_labels[section],
    tradability    = tradability_map(section),
    ag_orientation = ag_orientation_map(nsic_2digt),
    manuf_tier     = manuf_tier_map(nsic_2digt, section),
    modernity      = modernity_map(nsic_2digt, section)
  )

dir.create("data/clean/nec2018", recursive = TRUE, showWarnings = FALSE)
write_csv(nsic_map, "data/clean/nec2018/nsic_classification_map.csv")

cat("Wrote data/clean/nec2018/nsic_classification_map.csv  |  ",
    nrow(nsic_map), "rows\n")

cat("\n-- Scheme distributions --\n")
cat("\nTradability:\n");    print(count(nsic_map, tradability, sort = TRUE))
cat("\nAg orientation:\n"); print(count(nsic_map, ag_orientation, sort = TRUE))
cat("\nManuf tier:\n");     print(count(nsic_map, manuf_tier, sort = TRUE))
cat("\nModernity:\n");      print(count(nsic_map, modernity, sort = TRUE))
