# nativity_profile_creation.R
#
# [NEW 2026-08-21] Standalone descriptive analysis, NOT part of Memo 1's
# reweighting pipeline (no MEMO1_WEIGHTING.md write-up requested or
# produced) -- Nicholas's question: does the decision to create a LinkedIn
# profile (and, conditional on that, to disclose a high school) vary
# systematically with how "native" a labor market is, i.e. what share of
# its own college-educated full-time workforce was born in-state? His
# hypothesis: LI-heavy industries cluster in places with lots of migrants
# (domestic + international), so profile creation should be lower where
# nativity is higher.
#
# [REVISED 2026-08-21, same day] First pass used STATE as the labor-market
# unit (51 states x 3 cohorts = 153 cells) -- Nicholas correctly pointed
# out a "labor market" should be finer than that, and every other part of
# this project already defines labor markets via CBSA (metro tier, flow
# calibration, etc.), not state. Unit of observation is now (CBSA, birth
# cohort) -- 442 CBSAs x 3 cohorts, far more dots. Nativity ITSELF still
# has to be measured at the state level: ACS's place-of-birth variable
# (POBP) has no finer geography than state/country, so "born in the state
# they currently reside in" is the finest available proxy for "native to
# this labor market" -- a real, worth-stating limitation, not a design
# choice. It's computed at the PERSON level (each respondent's own ST vs.
# POBP) before aggregating to CBSA, so multi-state CBSAs (e.g. NYC spans
# NY/NJ/CT) are handled correctly -- a person in the NJ part of that metro
# is compared to NJ's birth state, not NY's.
#
# Three birth cohorts, matching Memo 1's own framing (MEMO1_WEIGHTING.md
# SS6.1) plus a new "everyone older" bucket:
#   pre-1980   : birth in [PUMS_YEAR - 65, 1979]  (under-65-in-2022, same
#                age ceiling used throughout Memo 1)
#   1980s      : birth in [1980, 1989]
#   1990s      : birth in [1990, 1999]
#
# X-axis (both plots): nativity share -- among ACS's BA+, employed,
# FULL-TIME (WKHP >= 35, new relative to Memo 1's pull, which never
# needed an hours-worked filter), under-65 population of that cohort
# CURRENTLY RESIDING in a CBSA, the share who were born in the state
# containing their PUMA of residence.
#
# Y-axis, plot 1 (profile creation): Column 1 headcount / ACS population
# estimate, for the same (CBSA, cohort) cell -- what fraction of the TRUE
# labor market has a Revelio-captured profile at all. A real coverage-rate
# computation, different in kind from anything in Memo 1 (which only ever
# reweights Column 2 internally; it never asks what share of the true
# population Column 1 itself represents).
#
# Y-axis, plot 2 (HS disclosure): Column 2 headcount / Column 1 headcount,
# same cell -- purely Revelio-internal, no ACS denominator. Column 2 is a
# proper subset of Column 1 (MEMO1_WEIGHTING.md SS2), so this is exactly
# the share of college-identified profiles that also list a high school.
#
# Both Revelio counts come from a FIXED-YEAR (2022) cross-section --
# matching PUMS_YEAR below -- not "whichever year a person was last
# observed," so numerator and denominator describe roughly the same
# real-world snapshot. Same t-slice-then-filter pattern already used in
# Code/memo1_07d_cohort_demo_table.R's extract_fixed_year(), copied here
# rather than re-derived. Revelio's own CBSA assignment (cbsa_code_t) is
# used directly -- no PUMA crosswalk needed on that side, same as every
# other Memo 1 script that touches Revelio's geography.
#
# [VERIFIED LIVE before trusting it, not assumed -- this project's
# standing discipline after repeatedly getting burned by silently-wrong
# ACS codebook assumptions] POBP (place of birth, ACS5 2022 recode) uses
# the SAME state-FIPS numbering as ST (1=Alabama ... 56=Wyoming, verified
# via tidycensus::pums_variables), not a different/wider scheme -- but ST
# and POBP still get compared as INTEGERS, not raw strings, following the
# exact same discipline as this project's MIGSP-vs-ST padding-drift fix
# (HANDOFF.md 2026-08-10). WKHP (usual hours worked/week) is numeric with
# "bb" (blank) for non-workers -- an as.integer(WKHP) >= 35 filter
# naturally drops those rows (NA fails the comparison).
#
# PUMA-to-CBSA crosswalk: built fresh here from
# Data/intermediate/tract_puma_cbsa_pop_2020.rds -- the same tract-level
# population table Code/memo1_03a_puma_cbsa_crosswalk.R uses, but
# aggregated to a raw state_puma -> cbsa_code population-share table
# WITHOUT collapsing to a metro-tier category (this project's existing
# puma_cbsa_tier_crosswalk_*.rds files all discard individual CBSA
# identity in favor of a tier label, which is useless here -- we want
# each actual metro as its own dot, not folded into "Top 10").
#
# Reliability floor: cells with fewer than 30 ACS respondents contributing
# (effective, fractional-CBSA-split count) are dropped before plotting --
# same SMALL_CELL_THRESHOLD=30 convention memo1_02a/reweight_column2.R
# already use to flag thin PUMS cells, applied here as an outright drop
# since this is a purely descriptive scatter, not a reweighting margin.

library(tidycensus)
library(data.table)
library(ggplot2)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
tidycensus::census_api_key(Sys.getenv("CENSUS_KEY"))
source(here::here("Code/memo1_00_metro_tier_definitions.R"))  # STATE_REGION_CROSSWALK, state_fips_to_region()

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

PUMS_YEAR <- 2022   # matches memo1_02a's Stage-1 ACS 5yr vintage (2018-2022 pooled)
MAX_AGE <- 65       # matches Memo 1's under-65 restriction throughout
FT_HOURS_MIN <- 35  # standard BLS full-time threshold; new relative to Memo 1 (no prior script filtered on hours)
T_MAX <- 20
MIN_CELL_N <- 30    # reliability floor, see header note

fips_lookup <- state_fips_to_region()  # state_fips (2-digit string) <-> state_abbr <-> census_region
fips_to_abbr <- setNames(fips_lookup$state_abbr, as.integer(fips_lookup$state_fips))

## =========================================================================
## PART 1: ACS pull -- adds PUMA20 (needed for the CBSA crosswalk) relative
## to the state-level first pass; new cache path since the variable set
## changed.
## =========================================================================
acs_raw_path <- file.path(data_dir, "intermediate/pums_nativity_fulltime_cbsa_raw.rds")

if (!file.exists(acs_raw_path)) {
  log_step("Pulling ACS 5yr PUMS (POBP/WKHP/ST/PUMA20/AGEP/SCHL/ESR/PWGTP), 51 states + DC")
  state_list <- c(state.abb, "DC")
  parts <- vector("list", length(state_list))
  for (i in seq_along(state_list)) {
    st <- state_list[i]
    cat(sprintf("Pulling PUMS for %s (%d/%d)...\n", st, i, length(state_list)))
    pull <- get_pums(
      variables   = c("ST", "PUMA20", "POBP", "AGEP", "SCHL", "ESR", "WKHP"),
      state       = st,
      survey      = "acs5",
      year        = PUMS_YEAR,
      rep_weights = NULL,  # point estimates only -- no variance/SE needed for a scatter plot
      show_call   = FALSE
    )
    setDT(pull)
    pull[, esr := as.integer(ESR)]
    pull[, wkhp_int := suppressWarnings(as.integer(WKHP))]
    pull <- pull[esr %in% c(1, 2, 3) & SCHL > 20 & PWGTP < 10000 & !is.na(wkhp_int) & wkhp_int >= FT_HOURS_MIN]
    parts[[i]] <- pull
  }
  acs_raw <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  rm(parts)
  cat(sprintf("ACS filtered sample (BA+, employed, full-time, PWGTP<10000): %d rows across %d states\n",
              nrow(acs_raw), uniqueN(acs_raw$ST)))
  saveRDS(acs_raw, acs_raw_path)
} else {
  log_step("ACS raw pull already cached -- skipping Census API loop")
  acs_raw <- readRDS(acs_raw_path)
}

acs_raw[, `:=`(st_int = as.integer(ST), pobp_int = suppressWarnings(as.integer(POBP)), agep_int = as.integer(AGEP))]
acs_raw[, birth_approx := PUMS_YEAR - agep_int]
acs_raw <- acs_raw[birth_approx >= (PUMS_YEAR - MAX_AGE)]  # under-65, same ceiling as Memo 1

acs_raw[, cohort := fcase(
  birth_approx < 1980,                          "Pre-1980",
  birth_approx >= 1980 & birth_approx <= 1989,   "1980s",
  birth_approx >= 1990 & birth_approx <= 1999,   "1990s",
  default = NA_character_
)]
acs_cohort <- acs_raw[!is.na(cohort)]

# native = born in the SAME state as current residence, compared as
# integers -- pobp_int > 56 (territory/foreign-born) never matches a
# valid st_int (1-56), so it correctly falls into "not native" without a
# separate exclusion. Computed PERSON-level, before any CBSA splitting.
acs_cohort[, native := as.integer(pobp_int == st_int)]
acs_cohort[, state_puma := paste0(sprintf("%02d", st_int), PUMA20)]

## =========================================================================
## PART 2: raw PUMA -> CBSA population-share crosswalk (no tier collapse)
## =========================================================================
log_step("Building raw PUMA -> CBSA crosswalk from tract_puma_cbsa_pop_2020.rds")
tract_pop <- readRDS(file.path(data_dir, "intermediate/tract_puma_cbsa_pop_2020.rds")); setDT(tract_pop)
puma_cbsa <- tract_pop[, .(cbsa_pop = sum(tract_pop)), by = .(state_puma, cbsa_code)]
puma_cbsa[, cbsa_share := cbsa_pop / sum(cbsa_pop), by = state_puma]
cat(sprintf("Raw PUMA->CBSA crosswalk: %d (PUMA x CBSA) rows, %d distinct CBSAs, %d distinct PUMAs\n",
            nrow(puma_cbsa), uniqueN(puma_cbsa$cbsa_code), uniqueN(puma_cbsa$state_puma)))

# CBSA's "primary state" (plurality of population) -- used only to attach
# a Census-region color to CBSAs that don't cross state lines cleanly.
cbsa_primary_state <- tract_pop[, .(pop = sum(tract_pop)), by = .(cbsa_code, STATEFP)]
cbsa_primary_state <- cbsa_primary_state[cbsa_primary_state[, .I[which.max(pop)], by = cbsa_code]$V1]
cbsa_primary_state[, state_abbr := fips_to_abbr[as.character(as.integer(STATEFP))]]
region_for_state_abbr <- setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
cbsa_primary_state[, region := unname(region_for_state_abbr[state_abbr])]

## ---- Fractionally assign each ACS respondent to CBSA(s), keeping the
## person-level `native` flag fixed (it doesn't depend on which fraction
## of their PUMA belongs to which CBSA) ----
acs_cbsa_long <- merge(
  acs_cohort[, .(state_puma, cohort, PWGTP, native)],
  puma_cbsa[, .(state_puma, cbsa_code, cbsa_share)],
  by = "state_puma", all.x = TRUE, allow.cartesian = TRUE
)
acs_cbsa_long <- acs_cbsa_long[!is.na(cbsa_code)]  # drop respondents whose PUMA has no CBSA overlap at all (non-metro) -- "labor market" here means an actual metro

acs_cbsa_cohort <- acs_cbsa_long[, .(
  pop_n    = sum(PWGTP * cbsa_share),
  native_n = sum(PWGTP * cbsa_share * native),
  eff_n    = sum(cbsa_share)   # effective respondent count, for the reliability floor
), by = .(cbsa_code, cohort)]
acs_cbsa_cohort[, nativity_share := native_n / pop_n]

cat("\nACS CBSA x cohort cells (pre-reliability-floor):", nrow(acs_cbsa_cohort), "\n")
cat("Nativity share range:", round(range(acs_cbsa_cohort$nativity_share), 3), "\n")

## =========================================================================
## PART 3: Revelio Column 1 / Column 2 counts, FIXED_YEAR = 2022, by CBSA
## =========================================================================
log_step("Loading Column 1/2 for the fixed-year cross-section")
col1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds")); setDT(col1)
li   <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds")); setDT(li)

resolve_col_end <- function(dt, is_factor_like) {
  if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
}
col_end_col1 <- resolve_col_end(col1, is_factor_like = FALSE)
col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

assign_cohort <- function(birth_vec) {
  fcase(
    birth_vec < 1980,                        "Pre-1980",
    birth_vec >= 1980 & birth_vec <= 1989,    "1980s",
    birth_vec >= 1990 & birth_vec <= 1999,    "1990s",
    default = NA_character_
  )
}

# Same t-slice-to-a-fixed-calendar-year extraction as
# Code/memo1_07d_cohort_demo_table.R's extract_fixed_year() -- one row per
# person who has an observation landing exactly on FIXED_YEAR, current
# CBSA from Revelio's own cbsa_code_t at that t (no crosswalk needed).
# Carries user_id along -- needed below to enforce Column 2 subset-of-
# Column 1 by INTERSECTION, not by assumption (see note below).
extract_cbsa_at_fixed_year <- function(dt, col_end_vec, birth_vec, fixed_year, t_max = T_MAX) {
  parts <- vector("list", t_max + 1)
  for (t in 0:t_max) {
    code_col <- paste0("cbsa_code_", t)
    if (!code_col %in% names(dt)) next
    cy <- col_end_vec + t
    idx <- which(cy == fixed_year)
    if (length(idx) == 0) next
    parts[[t + 1]] <- data.table(user_id = dt$user_id[idx], cbsa_code = dt[[code_col]][idx], birth = birth_vec[idx])
  }
  rbindlist(parts)
}

log_step("Extracting 2022 cross-section: Column 1")
slice1 <- extract_cbsa_at_fixed_year(col1, col_end_col1, as.integer(col1$birth), PUMS_YEAR)
slice1[, cbsa_code := as.character(cbsa_code)]  # Revelio stores this as integer; the crosswalk-derived cbsa_code (from tract_puma_cbsa_pop_2020.rds) is character -- coerce before merging, same FIPS-type-mismatch class this project has hit before
slice1 <- slice1[!is.na(cbsa_code) & cbsa_code != "" & !is.na(birth)]
slice1[, cohort := assign_cohort(birth)]
slice1 <- slice1[!is.na(cohort)]
col1_n <- slice1[, .(col1_n = .N), by = .(cbsa_code, cohort)]

log_step("Extracting 2022 cross-section: Column 2")
slice2 <- extract_cbsa_at_fixed_year(li, col_end_col2, as.integer(as.character(li$birth)), PUMS_YEAR)
slice2[, cbsa_code := as.character(cbsa_code)]  # same coercion as slice1, see note above
slice2 <- slice2[!is.na(cbsa_code) & cbsa_code != "" & !is.na(birth)]
slice2[, cohort := assign_cohort(birth)]
slice2 <- slice2[!is.na(cohort)]

# [FOUND 2026-08-21, caught by a real HS-disclosure ratio exceeding 1.0 --
# not assumed clean] MEMO1_WEIGHTING.md SS2 describes Column 2 as a strict
# subset of Column 1, but the CURRENT cached files aren't perfectly nested
# that way. Two layers to this, both real:
#   1. ~95% of Column 2's user_ids are found in column1_covariates.rds at
#      all (checked directly).
#   2. [FOUND on a second pass -- a same-user_id-%in%-check alone still
#      left hs_disclosure_rate as high as 1.62, so (1) wasn't the whole
#      story] Even for a user present in BOTH files, their 2022 CBSA
#      assignment can differ slightly between column1_covariates.rds and
#      column2_reweighted.rds (independently-built files, not guaranteed
#      to agree row-for-row on every derived column for a shared person).
#      Filtering slice2 by user_id membership but keeping SLICE2's OWN
#      cbsa_code let a person get counted in col2_n under a cell where
#      col1_n didn't count them at all.
# Fixed properly this time: col2_n is now a literal subset count of
# slice1's OWN rows (slice1's cbsa_code/cohort, not slice2's) -- take
# Column 1's cross-section and count how many of THOSE rows belong to a
# user who also shows up anywhere in Column 2. This makes
# col2_n[cell] <= col1_n[cell] true by construction, not by hoping the two
# files agree.
col2_user_ids <- unique(slice2$user_id)
cat(sprintf("Distinct Column 2 users in the 2022 cross-section: %d; of Column 1's %d rows, %d belong to one of them\n",
            length(col2_user_ids), nrow(slice1), sum(slice1$user_id %in% col2_user_ids)))
col2_n <- slice1[user_id %in% col2_user_ids, .(col2_n = .N), by = .(cbsa_code, cohort)]

cat("\nColumn 1 2022 cross-section, n =", nrow(slice1), " | Column 2 2022 cross-section, n =", nrow(slice2), "\n")

rm(col1, li); gc()

## =========================================================================
## PART 4: merge, apply reliability floor, compute rates, attach region
## =========================================================================
d <- merge(acs_cbsa_cohort[, .(cbsa_code, cohort, pop_n, eff_n, nativity_share)], col1_n, by = c("cbsa_code", "cohort"), all.x = TRUE)
d <- merge(d, col2_n, by = c("cbsa_code", "cohort"), all.x = TRUE)
d[is.na(col1_n), col1_n := 0L]
d[is.na(col2_n), col2_n := 0L]
stopifnot(all(d$col2_n <= d$col1_n))  # col2_n is now a literal subset count of col1_n's own rows -- this must hold exactly, not approximately

n_before_floor <- nrow(d)
d <- d[eff_n >= MIN_CELL_N]
cat(sprintf("\nReliability floor (ACS effective n >= %d): kept %d of %d cells\n", MIN_CELL_N, nrow(d), n_before_floor))

d[, profile_creation_rate := col1_n / pop_n]
d[, hs_disclosure_rate := fifelse(col1_n > 0, col2_n / col1_n, NA_real_)]
d <- merge(d, cbsa_primary_state[, .(cbsa_code, state_abbr, region)], by = "cbsa_code", all.x = TRUE)
d[, cohort := factor(cohort, levels = c("Pre-1980", "1980s", "1990s"))]

cat("\nFinal CBSA x cohort cells:", nrow(d), "\n")
cat("Cells with zero Column 1 presence (excluded from HS-disclosure rate, kept in profile-creation rate):",
    sum(d$col1_n == 0), "of", nrow(d), "\n")
cat("Cells with no resolved region (should be 0):", sum(is.na(d$region)), "\n")
cat("Profile-creation rate range:", round(range(d$profile_creation_rate), 4), "\n")
cat("HS-disclosure rate range:", round(range(d$hs_disclosure_rate, na.rm = TRUE), 4), "\n")
cat("\nCells per cohort:\n")
print(d[, .N, by = cohort])

fwrite(d, file.path(data_dir, "results/nativity_profile_creation_cbsa_cohort.csv"))
log_step("Wrote nativity_profile_creation_cbsa_cohort.csv")
