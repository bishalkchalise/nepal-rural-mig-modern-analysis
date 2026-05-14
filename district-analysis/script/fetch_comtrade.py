"""
Fetch UN Comtrade Nepal trade flows by partner × ISIC industry, 1995-2001
(baseline period for the Khanna et al. (2026) trade SSIV).

Run locally:
    pip install comtradeapicall pandas
    # Optional but recommended: get a free API key at https://uncomtrade.org/
    export COMTRADE_API_KEY=...
    python3 script/fetch_comtrade.py

Output:
  data/clean/instrument/trade_baseline_partner_industry.csv
    columns: partner, partner_iso3, industry_isic2, year, imp_usd, exp_usd
  data/clean/instrument/trade_ssiv.csv
    municipality-year trade SSIV (per Khanna §IIIC equation 5):
      Shiftshare^trade_ot = Σ_d Σ_j (L_jo / Pop_o) · M_jd · ΔR_dt

Inputs needed beyond Comtrade:
  - Per-municipality 2001 employment by ISIC industry. Currently
    `census_outcomes_municipality.csv` has industry SHARES (ind_*); we need
    levels = share × workforce-size. Workforce can be approximated from the
    same census file's `geog_pop_2001` × LFP.
"""
import os, sys, time
import pandas as pd

try:
    import comtradeapicall as ct
except ImportError:
    print("Install with: pip install comtradeapicall pandas", file=sys.stderr)
    sys.exit(1)

API_KEY = os.environ.get("COMTRADE_API_KEY", "")
if not API_KEY:
    print("WARNING: no COMTRADE_API_KEY set — using public anonymous endpoint")
    print("         (low rate limit; some series may be unavailable)")

# Nepal reporter code (M49 = 524)
REPORTER = "524"
YEARS = [str(y) for y in range(1995, 2002)]    # 1995-2001 baseline

# We pull at the SITC Rev.3 2-digit level (cmdCode AG2) — coarser than ISIC
# but matches what's freely accessible. Aggregate to 36 ISIC2 industries via a
# lightweight crosswalk in a follow-up script.
print(f"Fetching Nepal Comtrade flows for {YEARS} ...")

frames = []
for yr in YEARS:
    for flow in ("M","X"):       # M=imports, X=exports
        try:
            df = ct.getFinalData(API_KEY,
                                 typeCode="C", freqCode="A", clCode="HS",
                                 period=yr, reporterCode=REPORTER,
                                 partnerCode=None,
                                 cmdCode="AG2",
                                 flowCode=flow,
                                 maxRecords=100000)
        except Exception as e:
            print(f"  {yr} {flow}: ERROR {e}")
            continue
        if df is None or df.empty:
            print(f"  {yr} {flow}: empty")
            continue
        df["year"] = int(yr); df["flow"] = flow
        frames.append(df)
        time.sleep(1)

if not frames:
    print("No data fetched. Check API key and rate limits.", file=sys.stderr)
    sys.exit(1)

raw = pd.concat(frames, ignore_index=True)
out_raw = "data/clean/instrument/trade_baseline_partner_industry.csv"
raw.to_csv(out_raw, index=False)
print(f"Wrote {len(raw)} raw rows to {out_raw}")
print()
print("Next step: run script/build_trade_ssiv.py to aggregate to municipality-year.")
