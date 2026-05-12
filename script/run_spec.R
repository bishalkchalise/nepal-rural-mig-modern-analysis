# =============================================================================
# script/run_spec.R — your spec-runner cockpit.
#
# Edit the run_spec(...) calls below to define the regressions you want, then:
#   source("script/run_spec.R")
# Each call writes its own CSV to output/tab/ and prints a summary.
#
# Toggles per run_spec() call:
#   spec_label  -- string written to the `spec` column in the output CSV.
#                  Use this to identify each run (e.g. "main_noXorigin").
#   dataset     -- "census" or "hh".
#   threshold   -- 0 / 25 / 50 / 100  (single integer; one call per threshold).
#   treatment   -- "log_int"  : fx_z * log(mig_int_z)         <- main
#                  "lin_int"  : fx_z * mig_int_z              <- linear variant
#                  "fx_alone" : fx_z (no interaction)
#   c_mig       -- TRUE/FALSE  add year x mig_intensity      (the "second term")
#   c_fx        -- TRUE/FALSE  add year x fx
#   c_block_a   -- TRUE/FALSE  Block A: dest-weighted baseline X x year
#   c_block_b   -- TRUE/FALSE  Block B: origin (2001 census) baseline X x year
#   c_block_c   -- TRUE/FALSE  Block C: trade SSIV (level controls)
#   outcomes    -- NULL              : run the full catalogue for the dataset
#                  c("y1","y2",...)  : just these outcomes
#                  list(MyGrp = c(...), Other = c(...))  : custom group set
#   ref_year    -- NULL means 2001 (census) / 2016 (hh).  Override if needed.
#   output_path -- NULL means output/tab/<label>_<ds>_thr<thr>_results.csv
#
# When an outcome only has data in 2 of 3 waves (e.g. some census outcomes
# weren't measured in 2001), the engine quietly drops the missing year and
# estimates on the remaining years.  No more "in i(factor..." errors.
# =============================================================================

source("script/_specs_lib.R")

# ----------------------------------------------------------------------------
# Examples — edit, comment in/out, or add your own.
# ----------------------------------------------------------------------------

# (1) Main spec minus C_mig, census, all thresholds.
run_spec(spec_label = "main_no_cmig",
         dataset   = "census",
         threshold = 0L,
         c_mig = FALSE, c_fx = TRUE,
         c_block_a = TRUE, c_block_b = FALSE, c_block_c = FALSE)

# (2) Same, threshold = 25, with Khanna Blocks B and C added.
# run_spec(spec_label = "main_no_cmig_khanna",
#          dataset   = "census",
#          threshold = 25L,
#          c_mig = FALSE, c_fx = TRUE,
#          c_block_a = TRUE, c_block_b = TRUE, c_block_c = TRUE)

# (3) HH panel, threshold = 25, full Khanna controls.
# run_spec(spec_label = "main_no_cmig_khanna",
#          dataset   = "hh",
#          threshold = 25L,
#          c_mig = FALSE, c_fx = TRUE,
#          c_block_a = TRUE, c_block_b = TRUE, c_block_c = TRUE)

# (4) Just a handful of headline census outcomes, no Khanna controls.
# run_spec(spec_label = "headline_lean",
#          dataset   = "census",
#          threshold = 0L,
#          outcomes  = c("amen_assets_car","amen_assets_internet","amen_assets_mobile",
#                        "amen_lighting_electricity","edu_attain_higher_secondary_plus",
#                        "edu_attain_tertiary","work_share_agriculture","flfp_all"),
#          c_mig = FALSE, c_fx = TRUE,
#          c_block_a = FALSE, c_block_b = FALSE, c_block_c = FALSE)

# (5) Custom outcome groups.
# run_spec(spec_label = "assets_and_education",
#          dataset   = "census",
#          threshold = 0L,
#          outcomes  = list(
#            "My assets"    = c("amen_assets_car","amen_assets_internet","amen_assets_mobile"),
#            "My education" = c("edu_attain_secondary_plus","edu_attain_tertiary")
#          ),
#          c_mig = FALSE, c_fx = TRUE,
#          c_block_a = TRUE, c_block_b = TRUE, c_block_c = TRUE)
