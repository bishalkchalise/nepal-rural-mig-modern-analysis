"""
Fetch 2001 destination-country GDP per capita (constant 2015 USD) from the
World Bank API and save to data/clean/instrument/wdi_dest_gdp_2001.csv.

Run locally:
    pip install wbgapi pandas
    python3 script/fetch_wdi_dest_gdp.py

Output schema: country, iso3, gdp_pc_2001
"""
import sys, csv
import pandas as pd

try:
    import wbgapi as wb
except ImportError:
    print("Install with: pip install wbgapi pandas", file=sys.stderr)
    sys.exit(1)

# Country list mirrors instrument.R country_names + later additions.
# Use ISO3 codes for the WB API.
DEST_ISO3 = {
    # Gulf
    "Saudi Arabia":"SAU", "Qatar":"QAT", "United Arab Emirates":"ARE",
    "Kuwait":"KWT", "Bahrain":"BHR", "Oman":"OMN",
    # Other West Asia
    "Israel":"ISR", "Lebanon":"LBN", "Jordan":"JOR",
    # East Asia
    "Korea, Rep.":"KOR", "Japan":"JPN", "China":"CHN", "Hong Kong":"HKG",
    # Southeast Asia
    "Malaysia":"MYS", "Singapore":"SGP", "Thailand":"THA",
    # South Asia
    "Pakistan":"PAK", "Bangladesh":"BGD", "Bhutan":"BTN",
    "Sri Lanka":"LKA", "Maldives":"MDV",
    # OECD-North & Pacific
    "United States":"USA", "Canada":"CAN", "Mexico":"MEX",
    "Australia":"AUS", "New Zealand":"NZL",
    # OECD-Europe (frequent Nepali destinations)
    "United Kingdom":"GBR", "Germany":"DEU", "France":"FRA",
    "Italy":"ITA", "Spain":"ESP", "Portugal":"PRT",
    "Netherlands":"NLD", "Belgium":"BEL", "Sweden":"SWE",
    "Romania":"ROU", "Croatia":"HRV", "Malta":"MLT", "Poland":"POL",
    # Other
    "Russian Federation":"RUS",
}

print(f"Fetching WDI NY.GDP.PCAP.KD (constant 2015 USD) for {len(DEST_ISO3)} countries, year 2001...")
df = wb.data.DataFrame("NY.GDP.PCAP.KD",
                       economy=list(DEST_ISO3.values()),
                       time=2001, skipBlanks=True)
df = df.reset_index().rename(columns={"economy":"iso3", "YR2001":"gdp_pc_2001"})
inv = {v:k for k,v in DEST_ISO3.items()}
df["country"] = df["iso3"].map(inv)
df = df[["country","iso3","gdp_pc_2001"]].sort_values("country")

out_path = "data/clean/instrument/wdi_dest_gdp_2001.csv"
df.to_csv(out_path, index=False)
print(f"Wrote {len(df)} rows to {out_path}")
print(df.head(10).to_string(index=False))
