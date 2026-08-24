# memo1_02_acs_pulls.R
#
# [CONSOLIDATED 2026-08-24, per Nicholas's request to reduce script count]
# Merges what were three files (memo1_02a_acs_pull_5yr.R,
# memo1_02b_acs_pull_1yr.R, memo1_02c_acs_pull_1yr_supplements.R -- the
# last already a 2026-08-23 consolidation of three earlier scripts in its
# own right) into one, run top-to-bottom -- all three are independent ACS
# PUMS pulls with their own per-year/per-vintage checkpointing, so merging
# them changes nothing about execution behavior, only file count. No logic
# changed from any of the three original scripts. Shared boilerplate
# (library() calls, load_dot_env/directory/data_dir setup, the Census API
# key, and one `log_step()` helper -- Section 2 and Section 3 both defined
# an identical one) is hoisted to the top, once. Each section keeps its
# own section-specific constants (PUMS_YEAR, MAX_AGE, YEARS, etc.) inline,
# even where a name is reused across sections (e.g. MAX_AGE=65 in both
# Section 1 and Section 2) -- each section only ever reads its own
# just-set value, so this is harmless, and preserving it avoids touching
# working logic just to deduplicate a constant.
#
# SECTION 1 (was memo1_02a_acs_pull_5yr.R): ACS 5-year PUMS pull +
# population-cell totals for Memo 1's reweighting of Column 2 and its
# ACS-benchmark comparison column. Extracted from Code/acs_reweight.R's
# Sections 0/3/4/5 (that script's PUMS-pull/recode/cell-building logic,
# reused essentially verbatim -- see that file, now in Code/Old/, for the
# original JOLE-referee-response context it was written for). Feeds
# Code/memo1_05_reweight_column2.R's raking cells and
# Code/memo1_06_column1_reweight.R's Stage 1. Re-run only if PUMS_YEAR
# changes.
#
# SECTION 2 (was memo1_02b_acs_pull_1yr.R): ACS 1-year PUMS pull across
# many calendar years, for the calendar-year migration-rate chart
# (Code/memo1_08_calibration_charts.R Section 1) and metro-tier-share
# chart (Section 2 of the same file). A sibling to Section 1 above, NOT a
# parameterization of it -- Section 1's whole design (one static 5-year
# vintage, one checkpoint) stays untouched. Duplicates (does not share
# code with) Section 1's MIGSP-vs-ST mover-flag logic; same codebook
# concept, but 1-year files need real per-year handling, so it isn't a
# literal copy-paste. Real codebook-drift findings across 2008-2023 ACS
# 1-year vintages (SCHL/MIGSP/PUMA padding, the ST->STATE rename in 2023,
# MIGSP/MIGPUMA non-mover sentinels differing by vintage) are documented
# inline below, each one confirmed live before being coded around, not
# assumed.
#
# SECTION 3 (was memo1_02c_acs_pull_1yr_supplements.R, itself a
# 2026-08-23 consolidation of memo1_02c_acs_pull_1yr_race2015.R,
# memo1_02d_acs_pull_1yr_occp2015.R, and
# memo1_02e_acs_pull_1yr_occp_allyears.R -- all three now in Code/Old/):
# three small supplemental ACS 1-year pulls (race/sex for 2015 only,
# occupation for 2015 only, occupation for all calibration years) that
# Sections 1-2 above deliberately never carried, via one parameterized
# puller. Output filenames are UNCHANGED (multiple downstream scripts
# read them by name): pums_1yr_race2015.rds, pums_1yr_occp2015.rds,
# pums_1yr_occp_allyears.rds.

library(tidycensus)
library(httr)
library(data.table)
library(dotenv)
library(here)

load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
tidycensus::census_api_key(Sys.getenv("CENSUS_KEY"))

# [ADDED 2026-08-11, after a real hang] tidycensus's get_pums() calls go
# through httr, which has NO default timeout -- a stalled connection (seen
# for real on the first full-scale Section 2 run: one state's download
# stopped advancing entirely, CPU time and memory both flat for 20+ minutes
# with no error ever raised) hangs forever instead of erroring. A finite
# timeout converts a silent hang into a catchable "Timeout was reached"
# error that a retry wrapper can act on.
httr::set_config(httr::timeout(120))

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

set.seed(49)

## ===========================================================================
## SECTION 1: ACS 5-year PUMS pull + Stage-1 raking cells
## ===========================================================================
log_step("SECTION 1: ACS 5-year PUMS pull")

## ---- SECTION 1.0: config, parameters (acs_reweight.R Section 0, as-is) ----

# ACS 5-year vintage (end year of the 5-year window) -- keep in sync with
# Code/memo1_01b_covariates.R's pums_year (used there for the LI-side age
# bucket, since both sides need the same "as-of" year to be comparable).
# 2022 (2018-2022) is a placeholder; confirm with
# tidycensus::load_variables(2022, "acs5", cache = TRUE) or by letting
# get_pums() below error if the year isn't live yet.
PUMS_YEAR <- 2022

# [CHANGED 2026-08-10] Trimmed to under-65 at Nicholas's request -- the
# eventual research focus is younger workers anyway, and this also removes
# the [65,70] bucket that was driving reweight_column2.R's most extreme
# ratio weights (LI/Revelio's professional-network sample has very few
# near-retirement-age users, so that cell was blowing up to match ACS's
# much larger real population there). MAX_AGE/AGE_BREAKS matches
# Code/memo1_01b_covariates.R's exactly (both sides must use identical
# breaks for the age-bucket raking margin to mean anything) -- the actual
# under-65 filter is applied to pums_filt right after it's loaded, below.
MAX_AGE <- 65
AGE_BREAKS <- seq(20, MAX_AGE, by = 5)

# Below this unweighted cell N, a PUMS cell is flagged as thin in the
# diagnostics printout (does not drop the cell).
SMALL_CELL_THRESHOLD <- 30

# 6-category race scheme + *_prob column names, reused verbatim from
# Code/acs_reweight.R Section 0 (itself from Code/Old/06_census.R, archived).
RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob",
                     "native_prob", "multiple_prob", "hispanic_prob")

# Census-Bureau-consistent region labels, copied verbatim from
# Code/demographics.R L23-40 / Code/memo1_01b_covariates.R -- keeps the ACS
# side's census_region directly comparable to the LI side's hs_region/
# col_region (both built from the same crosswalk).
state_region_crosswalk <- data.table(
  state_abbr = c(
    "CT", "ME", "MA", "NH", "RI", "VT",
    "NJ", "NY", "PA",
    "IL", "IN", "MI", "OH", "WI",
    "IA", "KS", "MN", "MO", "NE", "ND", "SD",
    "DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV",
    "AL", "KY", "MS", "TN",
    "AR", "LA", "OK", "TX",
    "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
    "AK", "CA", "HI", "OR", "WA"
  ),
  census_region = c(
    rep("Northeast", 9),
    rep("Midwest", 12),
    rep("South", 17),
    rep("West", 13)
  )
)

# PUMS's ST/MIGSP come back as numeric-string FIPS codes; origin_state
# (built in Section 1.2 below) is also FIPS. state_region_crosswalk is
# keyed by 2-letter abbreviation, so a FIPS -> abbreviation crosswalk is
# needed to attach census_region on the ACS side.
data(fips_codes, package = "tidycensus")
fips_to_abb <- unique(fips_codes[, c("state", "state_code")])
setDT(fips_to_abb)
setnames(fips_to_abb, c("state", "state_code"), c("state_abbr", "state_fips"))

## ---- SECTION 1.1: pull ACS 5-year PUMS, one state at a time
## (acs_reweight.R Section 3, as-is) ----
## Looping and filtering per-state instead of one national call, since the
## national file with replicate weights is large (order 15-20M rows) --
## bounding peak memory to one state's raw pull at a time.

# Checkpointed separately from Section 1.2's derived columns (race/sex/
# origin_state/age_bucket/etc.) -- 2026-08-10's first run found two real
# bugs in that derivation logic (see Section 1.2's notes) that had nothing
# to do with the raw pull itself. Re-deriving from a cached raw pull avoids
# re-hitting the Census API (51 states x 80 replicate weights each, real
# wall-clock time) every time the derivation logic needs a fix.
pums_raw_path <- file.path(data_dir, "intermediate/pums_raw_pull.rds")

if (!file.exists(pums_raw_path)) {

state_list <- c(state.abb, "DC")
pums_parts <- vector("list", length(state_list))

for (i in seq_along(state_list)) {
  st <- state_list[i]
  cat(sprintf("Pulling PUMS for %s (%d/%d)...\n", st, i, length(state_list)))

  pull <- get_pums(
    # [CHANGED 2026-08-10] added PUMA20/MIGPUMA20/POWPUMA20/POWSP.
    # PUMA20 (not plain "PUMA" -- tidycensus errors on that for the
    # 2018-2022 5-year file: "PUMAs are not available ... due to
    # inconsistent PUMA boundary definitions" across the 2020 redistricting
    # -- PUMA20 is the 2020-vintage-consistent variable, matching
    # Code/memo1_03_geo_crosswalks.R Section 1's 2020-vintage tract/PUMA
    # crosswalk) is needed to assign each respondent a metro-tier via
    # Data/intermediate/puma_cbsa_tier_crosswalk.rds for the metro-tier-
    # share-over-time chart. MIGPUMA20 (PUMA of residence 1 year ago) and
    # POWPUMA20/POWSP (place of work) are pulled now too, at Nicholas's
    # request, since re-pulling is already required and the marginal cost
    # of extra columns is small -- MIGPUMA20 in particular sets up real
    # PUMA-to-PUMA migration flow tables as a future follow-on (see
    # HANDOFF.md), not built out in this pass.
    variables   = c("ST", "MIGSP", "MIG", "SCHL", "AGEP", "SEX", "RAC2P", "HISP", "ESR",
                     "PUMA20", "MIGPUMA20", "POWPUMA20", "POWSP"),
    state       = st,
    survey      = "acs5",
    year        = PUMS_YEAR,
    rep_weights = "person",   # fetches PWGTP1-80; PWGTP comes back by default
    show_call   = FALSE
  )
  setDT(pull)

  # Filter immediately, matching Code/acs_reweight.R's existing thresholds:
  # esr %in% c(1,2,3) (currently employed), SCHL > 20 (BA+), PWGTP < 10000
  # (drops a known PUMS weight-outlier artifact).
  pull[, esr := as.integer(ESR)]
  pull <- pull[esr %in% c(1, 2, 3) & SCHL > 20 & PWGTP < 10000]

  pums_parts[[i]] <- pull
}

pums_filt <- rbindlist(pums_parts, use.names = TRUE, fill = TRUE)
rm(pums_parts)

cat(sprintf("PUMS filtered sample: %d rows across %d states (expect 51)\n",
            nrow(pums_filt), uniqueN(pums_filt$ST)))

saveRDS(pums_filt, pums_raw_path)

} else {
  cat("Raw pull checkpoint (pums_raw_pull.rds) already exists -- skipping the Census API loop\n")
  pums_filt <- readRDS(pums_raw_path)
}

# Under-65 trim (see MAX_AGE's note above) -- applied here, after the raw
# pull/checkpoint load, rather than baked into pums_raw_pull.rds itself, so
# a future change to MAX_AGE doesn't require re-pulling from the Census API.
n_before_age_trim <- nrow(pums_filt)
pums_filt <- pums_filt[as.integer(AGEP) < MAX_AGE]
cat(sprintf("Under-%d age trim: %d of %d rows kept (%.1f%%)\n",
            MAX_AGE, nrow(pums_filt), n_before_age_trim, 100 * nrow(pums_filt) / n_before_age_trim))

## ---- SECTION 1.2: race recode + origin-state construction
## (acs_reweight.R Section 4, as-is) ----

# [FIXED 2026-08-10] Code/acs_reweight.R's Section 4 (copied here initially,
# itself from Code/Old/06_census.R, archived) assumed RAC2P used a 4-digit
# code scheme ("1000"/"3000"/etc.) -- that script was, per HANDOFF.md, never
# actually run, and this assumption is simply wrong for the current PUMS
# vintage's actual RAC2P codebook (confirmed directly via
# tidycensus::pums_variables for year=2022/survey="acs5"): RAC2P is 2-digit,
# 01=White alone, 02=Black alone, 03-37=American Indian/Alaska Native
# alone/combinations, 38-59=Asian alone/combinations, 60-66=Native
# Hawaiian/Pacific Islander alone/combinations (folded into "asian" per
# RACE_PROB_COLS's own "api_prob" = Asian/Pacific Islander naming
# convention), 67=Some Other Race alone, 68=Two or More Races (both folded
# into "multiple" -- there's no clean single-race bucket for "Some Other
# Race" in this 6-category scheme). HISP != "01" overriding to "hispanic"
# regardless of RAC2P was already correct and is unchanged.
pums_filt[, rac2p_int := as.integer(RAC2P)]
pums_filt[, race := fcase(
  HISP != "01", "hispanic",
  rac2p_int == 1L, "white",
  rac2p_int == 2L, "black",
  rac2p_int >= 3L & rac2p_int <= 37L, "native",
  rac2p_int >= 38L & rac2p_int <= 66L, "asian",
  default = "multiple"
)]

pums_filt[, sex := fifelse(SEX == "1", "male", "female")]

# [FIXED 2026-08-10] MIGSP is populated for every row here, not just movers
# (confirmed: 0% NA in the actual pull -- the "populated only for movers"
# assumption inherited from acs_reweight.R was wrong), and is zero-padded to
# 3 characters ("001" = Alabama) vs. ST's 2 ("01") -- comparing them as
# strings made `MIGSP != ST` true almost universally (confirmed: 100.00%
# "movers" on the first run), corrupting origin_state and, downstream, the
# census_region join (0% match). Fixed by comparing as integers. MIGSP's
# codebook (tidycensus::pums_variables) numbers US states/DC 001-056 (same
# numbering as FIPS, just 3-digit padded) and Puerto Rico/foreign countries
# 072+ -- those aren't a US "origin state" in any sense comparable to ST, so
# they fall back to the current state, same as a non-mover (consistent with
# the original design intent, just not the original -- broken -- mechanism).
pums_filt[, migsp_int := as.integer(MIGSP)]
pums_filt[, st_int := as.integer(ST)]
pums_filt[, moved_out_of_state := fifelse(!is.na(migsp_int) & migsp_int <= 56L & migsp_int != st_int, 1L, 0L)]
pums_filt[, origin_state := fifelse(moved_out_of_state == 1L, sprintf("%02d", migsp_int), ST)]

pums_filt[, age := as.integer(AGEP)]
pums_filt[, age_bucket := cut(age, breaks = AGE_BREAKS, right = FALSE, include.lowest = TRUE)]

cat(sprintf("Interstate movers in PUMS sample: %.2f%% (sanity-check this is a\n",
            100 * mean(pums_filt$moved_out_of_state)))
cat("small share, not e.g. 50%+, which would indicate the MIGSP/ST logic above is wrong)\n")
cat("Race distribution (sanity-check no single category dominates ~100%):\n")
print(pums_filt[, .N, by = race][, share := N / sum(N)][order(-share)])

## ---- SECTION 1.3: characteristics-table covariates (census_region,
## graduate-degree attainment) ----

pums_filt <- merge(pums_filt, fips_to_abb, by.x = "origin_state", by.y = "state_fips", all.x = TRUE)
pums_filt <- merge(pums_filt, state_region_crosswalk, by = "state_abbr", all.x = TRUE)
cat(sprintf("census_region match rate: %.1f%%\n", 100 * mean(!is.na(pums_filt$census_region))))

# SCHL: 21 = Bachelor's, 22 = Master's, 23 = Professional degree,
# 24 = Doctorate (all rows already SCHL > 20 per the BA+ filter above).
# [FIXED 2026-08-10] integer 0/1, not logical -- matches the 0/1 convention
# every Revelio-side binary covariate uses (Code/memo1_01b_covariates.R).
pums_filt[, grad_degree := as.integer(as.integer(SCHL) >= 22)]

# No ACS PUMS analog exists for transfer status (no question asks whether a
# respondent transferred between undergraduate institutions) -- deliberately
# not derived here. Code/memo1_05_reweight_column2.R's final characteristics
# table should leave that cell blank for the ACS column and say so,
# consistent with this memo's established pattern of documenting
# asymmetries (e.g. Column 1's missing HS-side geography) rather than
# papering over them.

## ---- SECTION 1.4: PUMS population-cell totals (acs_reweight.R Section 5,
## as-is) ----

cell_state <- pums_filt[, .(pop = sum(PWGTP)), by = .(origin_state)]
cell_state_age <- pums_filt[, .(pop = sum(PWGTP)), by = .(origin_state, age_bucket)]
cell_state_age_race_sex <- pums_filt[, .(pop = sum(PWGTP)), by = .(origin_state, age_bucket, race, sex)]

pums_cells <- list(
  state             = cell_state,
  state_age         = cell_state_age,
  state_age_racesex = cell_state_age_race_sex
)
saveRDS(pums_cells, file.path(data_dir, "intermediate/pums_cells.rds"))
saveRDS(pums_filt, file.path(data_dir, "intermediate/pums_acs5_filt.rds"))

flag_thin <- cell_state_age_race_sex[pop < SMALL_CELL_THRESHOLD]
cat(sprintf("%d of %d state x age x race x sex cells are thin (pop-weighted; re-check\n",
            nrow(flag_thin), nrow(cell_state_age_race_sex)))
cat("with an unweighted N if this looks too permissive):\n")
print(head(flag_thin[order(pop)], 10))

## ---- SECTION 1.5: benchmark covariate summary (convenience artifact,
## not load-bearing -- memo1_05_reweight_column2.R's final characteristics
## table recomputes these shares directly from pums_acs5_filt.rds rather
## than depending on this file) ----

pums_benchmark_summary <- list(
  race   = pums_filt[, .(share = sum(PWGTP)), by = race][, share := share / sum(share)][order(-share)],
  sex    = pums_filt[, .(share = sum(PWGTP)), by = sex][, share := share / sum(share)][order(-share)],
  region = pums_filt[, .(share = sum(PWGTP)), by = census_region][, share := share / sum(share)][order(-share)],
  age_bucket   = pums_filt[, .(share = sum(PWGTP)), by = age_bucket][, share := share / sum(share)][order(age_bucket)],
  grad_degree  = pums_filt[, .(share = sum(PWGTP)), by = grad_degree][, share := share / sum(share)][order(-share)]
)
saveRDS(pums_benchmark_summary, file.path(data_dir, "intermediate/pums_benchmark_summary.rds"))
print(pums_benchmark_summary)

rm(pums_filt, pums_cells, cell_state, cell_state_age, cell_state_age_race_sex,
   flag_thin, pums_benchmark_summary)
gc()

log_step("SECTION 1 done.")

## ===========================================================================
## SECTION 2: ACS 1-year PUMS pull, many calendar years
## ===========================================================================
log_step("SECTION 2: ACS 1-year PUMS pull (multi-year)")

## Only pulls what the calendar-year charts actually consume: MIGSP/ST/SCHL/
## AGEP/ESR/PWGTP every year, plus PUMA for 2012-2021 (the metro-tier
## chart's 2010-PUMA-vintage window -- see Section 3 of
## Code/memo1_03_geo_crosswalks.R). Race/sex/region are deliberately NOT
## pulled here -- neither chart uses them, and a 2026-08-11 live-data check
## (not assumed) found RAC2P's own coding scheme drifts across vintages
## even within 2019-2023 (68/67/65/64 categories in successive years, and
## 2023 unexpectedly reverts to an old 4-digit code scheme) -- pulling race
## would import a real, separate verification burden this pull doesn't
## need to take on.
##
## [Real codebook-drift findings, confirmed 2026-08-11 by trial-pulling DC
## across 2008/2012/2015/2019/2021/2022/2023 -- NOT assumed from a static
## reference table, since tidycensus::pums_variables' bundled metadata only
## covers 2019-2023 for survey="acs1" and doesn't reach the older years this
## pull needs:]
##   1. SCHL/MIGSP are NOT zero-padded before ~2018 ("9" not "09"; "1" not
##      "001"), but ARE zero-padded from ~2019 on. Section 1's existing
##      `SCHL > 20` filter relies on same-width string comparison working by
##      accident for its one fixed (already-padded) vintage -- that would
##      silently misclassify rows here across a 16-year range with mixed
##      padding (e.g. the unpadded string "9" > "20" lexicographically,
##      which is wrong numerically). Fixed by explicit as.integer()
##      coercion before every numeric-context comparison, never string
##      comparison.
##   2. PUMA padding is similarly inconsistent (DC's PUMA came back
##      3-digit unpadded in 2008/2012/2015, 5-digit zero-padded in
##      2019/2021/2022/2023) -- zero-padded to 5 digits via an explicit
##      integer round-trip (sprintf("%05d", as.integer(PUMA))) before
##      building the state_puma join key, regardless of what the API
##      happened to return for that particular year.
##   3. The state-FIPS variable was renamed from "ST" to "STATE" starting
##      with the 2023 vintage (confirmed via tidycensus::pums_variables --
##      a real Census data-dictionary change, not a package bug). Resolved
##      per-year below and renamed back to "ST" uniformly post-pull.
##   4. get_pums() calls occasionally fail with a transient connection
##      reset (seen once for 2008, succeeded immediately on retry) -- each
##      state-year pull gets up to 3 attempts with a short backoff.
##   6. [CAUGHT BY memo1_08_calibration_charts.R Section 2's (Section 2 of
##      memo1_08_calibration_charts.R's) match-rate check, NOT ASSUMED] ST
##      itself is inconsistently zero-padded, same bug class as PUMA/SCHL/
##      MIGSP (header note #1/#2) but on the ONE column this script hadn't
##      defensively re-padded. Confirmed directly against pums_1yr_filt.rds:
##      years 2012-2016 return single-digit FIPS states (AL/AK/AZ/AR/CA/CO/
##      CT = codes 1/2/4/5/6/8/9) as "1" not "01"; 2017+ zero-pads. This
##      alone explained that match-rate check landing at 89.4% instead of
##      ~100% (those 7 states, weighted by population -- California alone
##      is a huge share -- account for almost exactly the missing ~10.6%).
##      Fixed below with the same integer-round-trip pattern already used
##      for PUMA10.
##   5. [CAUGHT BY A SMOKE TEST, NOT ASSUMED -- see below] MIGSP's "not a
##      mover" sentinel differs by vintage. Section 1's own comment
##      ("MIGSP is populated for every row, not just movers") is correct
##      for its one 2018-2022-vintage pull, where non-movers get MIGSP set
##      to their OWN current state (migsp_int == st_int). A 2-state/2-year
##      smoke test of THIS section (DC+RI, 2012 vs 2019) caught that this is
##      NOT true for the 2012 file: there, non-movers get MIGSP=0 (not
##      their own state), and treating 0 as a valid "moved from state 0"
##      origin inflated the 2012 mover rate to 89% (vs. a sane ~7% once
##      excluded) -- confirmed directly against the raw pull, not assumed.
##      Fixed below by requiring migsp_int >= 1, which is harmless for
##      vintages where 0 never appears.
##   7. [ADDED 2026-08-12, Phase B -- migration-behavior plan] MIGPUMA now
##      pulled alongside PUMA for the same PUMA_YEARS window (Phase B of
##      D:\Users\martensn\.claude\plans\nope-i-had-something-logical-aurora.md
##      needs each ACS respondent's migration-PUMA of residence one year
##      ago, matched via Section 4 of Code/memo1_03_geo_crosswalks.R).
##      Smoke-tested first (RI, 2012/2019/2021), not assumed clean: (a) same
##      padding drift as every other geography variable here -- unpadded
##      variable-width in 2012 ("100","3200"), 5-digit zero-padded from
##      ~2019 on -- fixed with the same sprintf("%05d", as.integer(...))
##      round-trip already used for PUMA10; (b) a NEW wrinkle not seen on
##      MIGSP/PUMA/ST/SCHL: some rows carry non-numeric sentinel strings
##      ("bbbbb", "0000N" observed directly in a live pull) that
##      as.integer() silently NAs out -- counted and reported explicitly
##      below, not silently absorbed; (c) non-movers get a real MIGPUMA
##      value (their own current-area code), not NA -- same convention as
##      MIGSP, no special-casing needed.
##
## Checkpointing: one raw file per YEAR (not one monolithic checkpoint) --
## this is a ~16x-cost pull relative to Section 1's single vintage (16
## years x 51 states), so an interrupted run or a later range extension
## must never re-touch already-pulled years. Same per-unit-checkpoint
## philosophy as Code/memo1_01a_column1_construct.R's per-chunk checkpoints
## (which survived a mid-run reboot).

# 2020 excluded: COVID-era data-collection problems make the 2020 ACS 1-year
# file "experimental," not directly comparable to other years (Census's own
# guidance). 2005-2007 excluded by default: SCHL==21="Bachelor's" and this
# pull's other codebook assumptions are only confirmed stable from 2008 on
# (see the live-data findings above) -- extending back further would need
# the same trial-pull verification repeated for those years first.
YEARS <- c(2008:2019, 2021:2023)

# PUMA requested only for windows where this repo has a matching crosswalk
# vintage (see Section 1/3 of Code/memo1_03_geo_crosswalks.R). 2010-vintage
# PUMA boundaries covered ACS 1-year survey years 2012-2021; 2020-vintage
# took over starting with the 2022 file. [EXTENDED 2026-08-13, per
# Nicholas's request to widen the flow-calibration window] PUMA_YEARS_2020
# added -- confirmed live that the 1-year file's raw PUMA/MIGPUMA variable
# names don't change with vintage (still plain "PUMA"/"MIGPUMA", not
# "PUMA20"), only their MEANING does -- so the two windows are derived into
# separately-named columns below (PUMA10/MIGPUMA10 vs PUMA20/MIGPUMA20)
# rather than one column silently holding two different vintages of code.
# Pre-2012 (2000-vintage) deliberately excluded -- see Section 4 of
# Code/memo1_03_geo_crosswalks.R for why.
PUMA_YEARS_2010 <- 2012:2021
PUMA_YEARS_2020 <- 2022:2023
PUMA_YEARS <- c(PUMA_YEARS_2010, PUMA_YEARS_2020)

MAX_AGE <- 65  # matches Section 1's under-65 trim, for a comparable population

state_list <- c(state.abb, "DC")
raw_dir <- file.path(data_dir, "intermediate/pums_1yr_raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

# [WIDENED 2026-08-11] 3 attempts / flat 5s backoff wasn't enough for a
# real observed failure mode: WI-2009 hit the 120s httr timeout 3 times in
# a row (0 bytes received each time) under sustained 51-state-per-year API
# load, halting the whole run. 6 attempts with growing backoff (10s, 20s,
# 40s...) gives the Census API more room to recover from what looks like
# transient rate-limiting/load, without masking a truly persistent failure
# (still errors out and halts after 6 attempts, doesn't retry forever).
pull_one_state <- function(st, year, variables, max_attempts = 6) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch({
      d <- get_pums(variables = variables, state = st, survey = "acs1", year = year, show_call = FALSE)
      setDT(d)
      d
    }, error = function(e) {
      cat(sprintf("  %s %d attempt %d/%d FAILED: %s\n", st, year, attempt, max_attempts, conditionMessage(e)))
      NULL
    })
    if (!is.null(result)) return(result)
    if (attempt < max_attempts) Sys.sleep(10 * attempt)
  }
  stop(sprintf("get_pums() failed for state=%s year=%d after %d attempts", st, year, max_attempts))
}

## ---- SECTION 2.1: per-year pull, checkpointed one file per year ----

for (y in YEARS) {
  raw_path_y <- file.path(raw_dir, sprintf("pums_1yr_raw_%d.rds", y))
  if (file.exists(raw_path_y)) {
    cat(sprintf("Year %d already pulled -- skipping\n", y))
    next
  }

  # [FOUND 2026-08-11] state-FIPS variable renamed ST -> STATE starting
  # 2023 (confirmed via tidycensus::pums_variables against live 2022 vs
  # 2023 metadata, not assumed).
  state_var <- if (y >= 2023) "STATE" else "ST"
  include_puma <- y %in% PUMA_YEARS
  variables <- c(state_var, "MIGSP", "SCHL", "AGEP", "ESR", if (include_puma) c("PUMA", "MIGPUMA"))

  log_step(sprintf("Pulling ACS 1-year PUMS for %d (%d states, PUMA/MIGPUMA %s)",
                    y, length(state_list), if (include_puma) "included" else "not requested"))

  year_parts <- vector("list", length(state_list))
  for (i in seq_along(state_list)) {
    st <- state_list[i]
    # [ADDED 2026-08-11] per-state progress line, flushed immediately -- the
    # only way to tell "slow" from "hung" during a 51-state loop is to see
    # which specific state a run stopped advancing on (a real hang, found on
    # the first full-scale run, looked identical to a slow large-state
    # download until checked against CPU time over several minutes).
    cat(sprintf("  [%d/%d] %s...", i, length(state_list), st)); flush(stdout())
    t0 <- Sys.time()
    pull <- pull_one_state(st, y, variables)
    cat(sprintf(" %d rows (%.0fs)\n", nrow(pull), as.numeric(Sys.time() - t0, units = "secs"))); flush(stdout())
    setnames(pull, state_var, "ST", skip_absent = TRUE)

    # [FIXED 2026-08-11] SCHL is character and NOT consistently zero-padded
    # across vintages (see header note #1) -- coerce to integer before
    # comparing, never rely on string comparison. Filter matches Section
    # 1's thresholds: employed, BA+, drop the PWGTP<10000 outlier artifact.
    pull[, esr_int := as.integer(ESR)]
    pull[, schl_int := as.integer(SCHL)]
    pull <- pull[esr_int %in% c(1, 2, 3) & !is.na(schl_int) & schl_int > 20 & PWGTP < 10000]
    pull[, c("esr_int", "schl_int") := NULL]

    year_parts[[i]] <- pull
  }
  year_dt <- rbindlist(year_parts, use.names = TRUE, fill = TRUE)
  year_dt[, survey_year := y]

  cat(sprintf("  %d: %d rows across %d states (expect 51)\n", y, nrow(year_dt), uniqueN(year_dt$ST)))
  saveRDS(year_dt, raw_path_y)
}

## ---- SECTION 2.2: combine + derive (cheap, rebuilt every run from raw
## checkpoints -- no separate checkpoint, matching Section 1's own
## established precedent) ----

log_step("Combining per-year raw checkpoints")
raw_files <- list.files(raw_dir, pattern = "^pums_1yr_raw_\\d{4}\\.rds$", full.names = TRUE)
stopifnot(length(raw_files) == length(YEARS))
pums_1yr <- rbindlist(lapply(raw_files, readRDS), use.names = TRUE, fill = TRUE)

# [FIXED 2026-08-11, see header note #6] ST itself is inconsistently
# zero-padded across vintages (2012-2016 return single-digit FIPS states
# as "1" not "01") -- pad to 2 digits via the same integer-round-trip
# pattern used for PUMA10, so downstream string joins (state_puma in
# Section 2 of memo1_08_calibration_charts.R) are safe regardless of which
# year's raw formatting produced a given row. Harmless for already-padded
# years.
pums_1yr[, ST := sprintf("%02d", as.integer(ST))]

n_before_age_trim <- nrow(pums_1yr)
pums_1yr[, age := as.integer(AGEP)]
pums_1yr <- pums_1yr[age < MAX_AGE]
cat(sprintf("Under-%d age trim: %d of %d rows kept (%.1f%%)\n",
            MAX_AGE, nrow(pums_1yr), n_before_age_trim, 100 * nrow(pums_1yr) / n_before_age_trim))

# MIGSP-vs-ST mover flag -- identical concept to Section 1's (MIGSP is
# populated for every row, 3-digit-equivalent state/DC codes <=56;
# comparing as integers is robust to the padding inconsistency documented
# above, since as.integer("001")==as.integer("1")).
# [FIXED 2026-08-11, see header note #5] migsp_int >= 1L excludes the
# pre-2019-vintage "not a mover" sentinel (MIGSP=0), which is NOT a valid
# state FIPS code -- treating it as one inflated the 2012 mover rate to
# 89% in testing. Harmless for vintages where MIGSP=0 never appears.
pums_1yr[, migsp_int := as.integer(MIGSP)]
pums_1yr[, st_int := as.integer(ST)]
pums_1yr[, moved_out_of_state := fifelse(!is.na(migsp_int) & migsp_int >= 1L & migsp_int <= 56L & migsp_int != st_int, 1L, 0L)]

# PUMA/MIGPUMA -> zero-padded 5-digit join keys, split into vintage-scoped
# columns (PUMA10/MIGPUMA10 for the 2010-vintage window, PUMA20/MIGPUMA20
# for the 2020-vintage window) rather than one column silently holding two
# different vintages of code under the same name -- the raw API variable
# name doesn't change with vintage (confirmed live: 2022/2023 still return
# plain "PUMA"/"MIGPUMA", not "PUMA20"), only its meaning does, so this
# split has to happen here, not upstream. [EXTENDED 2026-08-13] Same
# derivation logic runs twice (once per vintage window) via a small
# closure, rather than duplicating the block -- lower risk of the two
# copies drifting apart. [FIXED 2026-08-11] explicit integer round-trip
# handles the padding inconsistency documented above (header note #2)
# regardless of which raw format a given year's API response used.
derive_puma_migpuma <- function(dt, years, puma_col, migpuma_col) {
  rows <- dt$survey_year %in% years
  if (!any(rows)) return(invisible(NULL))
  if ("PUMA" %in% names(dt)) {
    idx <- rows & !is.na(dt$PUMA)
    dt[idx, (puma_col) := sprintf("%05d", as.integer(PUMA))]
  }
  # [ADDED 2026-08-12, Phase B, see header note #7] MIGPUMA also carries
  # non-numeric sentinel strings ("bbbbb", "0000N", confirmed via live
  # smoke test across multiple vintages) that as.integer() silently turns
  # into NA -- counted and reported explicitly rather than letting them
  # vanish into an unexplained match-rate gap downstream.
  if ("MIGPUMA" %in% names(dt)) {
    idx_present <- rows & !is.na(dt$MIGPUMA)
    n_migpuma_present <- sum(idx_present)
    if (n_migpuma_present > 0) {
      migpuma_int <- suppressWarnings(as.integer(dt$MIGPUMA[idx_present]))
      n_migpuma_nonnumeric <- sum(is.na(migpuma_int))
      cat(sprintf("MIGPUMA (%d-%d window): %d rows had a value; %d of those (%.2f%%) were non-numeric sentinels, dropped to NA\n",
                  min(years), max(years), n_migpuma_present, n_migpuma_nonnumeric,
                  100 * n_migpuma_nonnumeric / n_migpuma_present))
      # sprintf("%05d", NA_integer_) returns the literal string "NA" (not
      # an error) for the non-numeric-sentinel rows already counted above
      # -- explicitly null those back out to a real NA rather than leaving
      # "NA" as a silently-wrong string value.
      dt[idx_present, (migpuma_col) := suppressWarnings(sprintf("%05d", as.integer(MIGPUMA)))]
      dt[rows & get(migpuma_col) == "NA", (migpuma_col) := NA_character_]
      dt[rows & grepl("[^0-9]", get(migpuma_col)), (migpuma_col) := NA_character_]
      # [FOUND 2026-08-12, same bug class as header note #5's MIGSP=0 fix]
      # Pre-blank-convention vintages (confirmed: 2012, presumably
      # 2008-2016) encode "did not move" as MIGPUMA=0 ("00000" once
      # zero-padded), NOT a real MIGPUMA area code -- confirmed directly:
      # every 2012 row with MIGPUMA10=="00000" is ALSO a non-mover by the
      # existing MIGSP-based test (0 of 447,616 rows disagree), and
      # "00000" isn't a real code in the PUMA->MIGPUMA composition file.
      # Left un-zeroed, this silently inflated the apparent "moved to a
      # different MIGPUMA" rate to ~93% of ALL respondents in a Phase B
      # flow-margin test -- caught by a sanity check on the resulting
      # migration rate (12-13% vs. an expected ~4%), not assumed clean.
      n_migpuma_zero <- sum(rows & dt[[migpuma_col]] == "00000", na.rm = TRUE)
      cat(sprintf("  '%s' '00000' non-mover sentinel: %d rows reset to NA (same fix class as MIGSP==0)\n", migpuma_col, n_migpuma_zero))
      dt[rows & get(migpuma_col) == "00000", (migpuma_col) := NA_character_]
    }
  }
  invisible(NULL)
}
derive_puma_migpuma(pums_1yr, PUMA_YEARS_2010, "PUMA10", "MIGPUMA10")
derive_puma_migpuma(pums_1yr, PUMA_YEARS_2020, "PUMA20", "MIGPUMA20")

cat("Per-year sanity check (mover %, n) -- a codebook drift in one specific\n")
cat("year should show up as a visible anomaly here, not be averaged away:\n")
print(pums_1yr[, .(n = .N, mover_pct = round(100 * mean(moved_out_of_state), 2)), by = survey_year][order(survey_year)])

if ("MIGPUMA10" %in% names(pums_1yr)) {
  cat("\nPer-year MIGPUMA10 coverage (2010-vintage window only) -- a vintage-specific\n")
  cat("sentinel-value spike would show up as a visible dip here, not be averaged away:\n")
  print(pums_1yr[survey_year %in% PUMA_YEARS_2010, .(n = .N, migpuma10_pct = round(100 * mean(!is.na(MIGPUMA10)), 2)), by = survey_year][order(survey_year)])
}
if ("MIGPUMA20" %in% names(pums_1yr)) {
  cat("\nPer-year MIGPUMA20 coverage (2020-vintage window only):\n")
  print(pums_1yr[survey_year %in% PUMA_YEARS_2020, .(n = .N, migpuma20_pct = round(100 * mean(!is.na(MIGPUMA20)), 2)), by = survey_year][order(survey_year)])
}

stopifnot(!2020 %in% unique(pums_1yr$survey_year))

saveRDS(pums_1yr, file.path(data_dir, "intermediate/pums_1yr_filt.rds"))
log_step(paste("saved pums_1yr_filt.rds:", nrow(pums_1yr), "rows,", uniqueN(pums_1yr$survey_year), "years"))

rm(pums_1yr, raw_files)
gc()

log_step("SECTION 2 done.")

## ===========================================================================
## SECTION 3: supplemental ACS 1-year pulls (race/sex 2015, occupation)
## ===========================================================================
log_step("SECTION 3: supplemental ACS 1-year pulls")

VINTAGE_WINDOWS <- list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023)
CALIB_YEARS <- sort(unlist(VINTAGE_WINDOWS, use.names = FALSE))
STATE_LIST <- c(state.abb, "DC")

## ---- Generic single-year, 51-state(+DC) PUMS puller with retry -- backs
## all three supplements below. `variables` is passed straight to
## get_pums(); get_pums() always returns SERIALNO/SPORDER/PWGTP by default
## alongside whatever's requested. ----
pull_one_state_year <- function(st, year, variables, max_attempts = 6) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch({
      d <- get_pums(variables = variables, state = st, survey = "acs1", year = year, show_call = FALSE)
      setDT(d)
      d
    }, error = function(e) {
      cat(sprintf("  %s %d attempt %d/%d FAILED: %s\n", st, year, attempt, max_attempts, conditionMessage(e)))
      NULL
    })
    if (!is.null(result)) return(result)
    if (attempt < max_attempts) Sys.sleep(10 * attempt)
  }
  stop(sprintf("get_pums() failed for state=%s year=%d after %d attempts", st, year, max_attempts))
}

pull_acs_year <- function(year, variables) {
  log_step(sprintf("Pulling ACS 1yr PUMS %s for %d (%d states)", paste(variables, collapse = "/"), year, length(STATE_LIST)))
  parts <- vector("list", length(STATE_LIST))
  for (i in seq_along(STATE_LIST)) {
    st <- STATE_LIST[i]
    cat(sprintf("  [%d/%d] %s...", i, length(STATE_LIST), st)); flush(stdout())
    t0 <- Sys.time()
    pull <- pull_one_state_year(st, year, variables)
    cat(sprintf(" %d rows (%.0fs)\n", nrow(pull), as.numeric(Sys.time() - t0, units = "secs"))); flush(stdout())
    parts[[i]] <- pull
  }
  d <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  cat(sprintf("Total: %d rows across %d states (expect 51+DC=52)\n", nrow(d), uniqueN(d$ST)))
  d
}

## ---- Supplement 1: RAC2P/SEX/HISP, 2015 only -> pums_1yr_race2015.rds ----
race2015_path <- file.path(data_dir, "intermediate/pums_1yr_race2015.rds")
if (!file.exists(race2015_path)) {
  d <- pull_acs_year(2015, c("ST", "RAC2P", "SEX", "HISP", "SERIALNO", "SPORDER"))

  # [VERIFIED LIVE, original build] 2015's RAC2P is NOT zero-padded
  # ("1","15","2", not "01","15","02") but uses the same underlying numeric
  # scheme Section 1's derivation already handles -- this is 2015-specific,
  # not claimed to generalize to other years.
  d[, rac2p_int := as.integer(RAC2P)]
  d[, hisp_int := as.integer(HISP)]
  d[, race := fcase(
    hisp_int != 1L, "hispanic",
    rac2p_int == 1L, "white",
    rac2p_int == 2L, "black",
    rac2p_int >= 3L & rac2p_int <= 37L, "native",
    rac2p_int >= 38L & rac2p_int <= 66L, "asian",
    default = "multiple"
  )]
  d[, sex := fifelse(as.integer(SEX) == 1L, "male", "female")]

  cat("Race distribution (sanity check, no single category ~100%):\n")
  print(d[, .N, by = race][, share := N / sum(N)][order(-share)])
  cat("Sex distribution:\n")
  print(d[, .N, by = sex])

  race2015 <- d[, .(SERIALNO, SPORDER, race, sex)]
  saveRDS(race2015, race2015_path)
  log_step(paste("saved pums_1yr_race2015.rds:", nrow(race2015), "rows"))
} else {
  cat("pums_1yr_race2015.rds already cached -- skipping Census API loop\n")
}

## ---- Supplement 2: OCCP, 2015 only -> pums_1yr_occp2015.rds ----
occp2015_path <- file.path(data_dir, "intermediate/pums_1yr_occp2015.rds")
if (!file.exists(occp2015_path)) {
  d <- pull_acs_year(2015, c("SERIALNO", "SPORDER", "OCCP"))
  occp2015 <- d[, .(SERIALNO, SPORDER, OCCP)]
  saveRDS(occp2015, occp2015_path)
  log_step(paste("saved pums_1yr_occp2015.rds:", nrow(occp2015), "rows"))
} else {
  cat("pums_1yr_occp2015.rds already cached -- skipping Census API loop\n")
}

## ---- Supplement 3: OCCP, all CALIB_YEARS -> pums_1yr_occp_allyears.rds.
## Reuses Supplement 2's own 2015 pull instead of a redundant API call;
## per-year checkpointing so an interrupted run never re-touches an
## already-pulled year. ----
allyears_path <- file.path(data_dir, "intermediate/pums_1yr_occp_allyears.rds")
occp_raw_dir <- file.path(data_dir, "intermediate/pums_1yr_occp_raw")
dir.create(occp_raw_dir, showWarnings = FALSE, recursive = TRUE)

for (y in CALIB_YEARS) {
  raw_path_y <- file.path(occp_raw_dir, sprintf("pums_1yr_occp_raw_%d.rds", y))
  if (file.exists(raw_path_y)) {
    cat(sprintf("Year %d already pulled -- skipping\n", y))
    next
  }
  if (y == 2015 && file.exists(occp2015_path)) {
    log_step("Year 2015 -- reusing Supplement 2's existing pull instead of a redundant API call")
    d2015 <- readRDS(occp2015_path); setDT(d2015)
    d2015[, survey_year := 2015L]
    saveRDS(d2015, raw_path_y)
    next
  }
  year_dt <- pull_acs_year(y, c("SERIALNO", "SPORDER", "OCCP"))
  year_dt <- year_dt[, .(SERIALNO, SPORDER, OCCP)]
  year_dt[, survey_year := y]
  saveRDS(year_dt, raw_path_y)
}

log_step("Combining per-year OCCP raw checkpoints")
occp_raw_files <- list.files(occp_raw_dir, pattern = "^pums_1yr_occp_raw_\\d{4}\\.rds$", full.names = TRUE)
stopifnot(length(occp_raw_files) == length(CALIB_YEARS))
occp_all <- rbindlist(lapply(occp_raw_files, readRDS), use.names = TRUE, fill = TRUE)
occp_all[, `:=`(SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER))]

cat("Per-year row counts:\n")
print(occp_all[, .N, by = survey_year][order(survey_year)])

saveRDS(occp_all, allyears_path)
log_step(sprintf("Wrote pums_1yr_occp_allyears.rds: %d rows, %d years", nrow(occp_all), uniqueN(occp_all$survey_year)))

log_step("SECTION 3 done. memo1_02_acs_pulls.R done.")
