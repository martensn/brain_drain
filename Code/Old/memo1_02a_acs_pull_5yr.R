## =============================================================================
## memo1_02a_acs_pull_5yr.R
##
## ACS 5-year PUMS pull + population-cell totals for Memo 1's reweighting of
## Column 2 and its ACS-benchmark comparison column. Extracted from
## Code/acs_reweight.R's Sections 0/3/4/5 (that script's PUMS-pull/recode/
## cell-building logic, reused essentially verbatim -- see that file for the
## original, JOLE-referee-response context it was written for) into a
## standalone script that has zero dependency on Column 1/2 or demographics,
## so it's cached once rather than re-hit on every iteration of
## Code/memo1_05_reweight_column2.R's merge/raking logic.
##
## This is a REAL, first-time execution as of 2026-08-10 -- no
## pums_cells.rds/pums_acs5_filt.rds exist on disk yet, so running this
## script means genuine live Census API calls (51 states x 80 replicate
## weights each via get_pums()), not a cache hit. Expect real runtime.
##
## Run this before Code/memo1_05_reweight_column2.R. No source() between files, per
## this repo's convention -- re-run this script only if PUMS_YEAR changes.
## =============================================================================


## -----------------------------------------------------------------------
## SECTION 0: config, libraries, parameters (acs_reweight.R Section 0, as-is)
## -----------------------------------------------------------------------

library(tidycensus)
library(data.table)
library(dotenv)
library(here)

set.seed(49)

load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

tidycensus::census_api_key(Sys.getenv("CENSUS_KEY"))

# ACS 5-year vintage (end year of the 5-year window) -- keep in sync with
# Code/memo1_01c_covariates.R's pums_year (used there for the LI-side age
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
# Code/memo1_01c_covariates.R's exactly (both sides must use identical breaks
# for the age-bucket raking margin to mean anything) -- the actual under-65
# filter is applied to pums_filt right after it's loaded, below.
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
# Code/demographics.R L23-40 / Code/memo1_01c_covariates.R -- keeps the ACS
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
# (built in Section 4 below) is also FIPS. state_region_crosswalk is keyed
# by 2-letter abbreviation, so a FIPS -> abbreviation crosswalk is needed to
# attach census_region on the ACS side.
data(fips_codes, package = "tidycensus")
fips_to_abb <- unique(fips_codes[, c("state", "state_code")])
setDT(fips_to_abb)
setnames(fips_to_abb, c("state", "state_code"), c("state_abbr", "state_fips"))


## -----------------------------------------------------------------------
## SECTION 1: pull ACS 5-year PUMS, one state at a time (acs_reweight.R
## Section 3, as-is)
## -----------------------------------------------------------------------
## Looping and filtering per-state instead of one national call, since the
## national file with replicate weights is large (order 15-20M rows) --
## bounding peak memory to one state's raw pull at a time.

# Checkpointed separately from Section 2's derived columns (race/sex/
# origin_state/age_bucket/etc.) -- 2026-08-10's first run found two real
# bugs in that derivation logic (see Section 2's notes) that had nothing to
# do with the raw pull itself. Re-deriving from a cached raw pull avoids
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
    # Code/memo1_03a_puma_cbsa_crosswalk.R's 2020-vintage tract/PUMA crosswalk)
    # is needed to assign each respondent a metro-tier via
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


## -----------------------------------------------------------------------
## SECTION 2: race recode + origin-state construction (acs_reweight.R
## Section 4, as-is)
## -----------------------------------------------------------------------

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


## -----------------------------------------------------------------------
## SECTION 3: characteristics-table covariates (new -- census_region,
## graduate-degree attainment)
## -----------------------------------------------------------------------

pums_filt <- merge(pums_filt, fips_to_abb, by.x = "origin_state", by.y = "state_fips", all.x = TRUE)
pums_filt <- merge(pums_filt, state_region_crosswalk, by = "state_abbr", all.x = TRUE)
cat(sprintf("census_region match rate: %.1f%%\n", 100 * mean(!is.na(pums_filt$census_region))))

# SCHL: 21 = Bachelor's, 22 = Master's, 23 = Professional degree,
# 24 = Doctorate (all rows already SCHL > 20 per the BA+ filter above).
# [FIXED 2026-08-10] integer 0/1, not logical -- matches the 0/1 convention
# every Revelio-side binary covariate uses (Code/memo1_01c_covariates.R).
pums_filt[, grad_degree := as.integer(as.integer(SCHL) >= 22)]

# No ACS PUMS analog exists for transfer status (no question asks whether a
# respondent transferred between undergraduate institutions) -- deliberately
# not derived here. Code/memo1_05_reweight_column2.R's final characteristics table
# should leave that cell blank for the ACS column and say so, consistent
# with this memo's established pattern of documenting asymmetries (e.g.
# Column 1's missing HS-side geography) rather than papering over them.


## -----------------------------------------------------------------------
## SECTION 4: PUMS population-cell totals (acs_reweight.R Section 5, as-is)
## -----------------------------------------------------------------------

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


## -----------------------------------------------------------------------
## SECTION 5: benchmark covariate summary (new -- for a quick standalone
## look; Code/memo1_05_reweight_column2.R's final characteristics table recomputes
## these shares directly from pums_acs5_filt.rds rather than depending on
## this file, so this is a convenience artifact, not a load-bearing one)
## -----------------------------------------------------------------------

pums_benchmark_summary <- list(
  race   = pums_filt[, .(share = sum(PWGTP)), by = race][, share := share / sum(share)][order(-share)],
  sex    = pums_filt[, .(share = sum(PWGTP)), by = sex][, share := share / sum(share)][order(-share)],
  region = pums_filt[, .(share = sum(PWGTP)), by = census_region][, share := share / sum(share)][order(-share)],
  age_bucket   = pums_filt[, .(share = sum(PWGTP)), by = age_bucket][, share := share / sum(share)][order(age_bucket)],
  grad_degree  = pums_filt[, .(share = sum(PWGTP)), by = grad_degree][, share := share / sum(share)][order(-share)]
)
saveRDS(pums_benchmark_summary, file.path(data_dir, "intermediate/pums_benchmark_summary.rds"))
print(pums_benchmark_summary)

cat("acs_pull.R done.\n")
