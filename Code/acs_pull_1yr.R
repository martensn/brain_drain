## =============================================================================
## acs_pull_1yr.R
##
## ACS 1-year PUMS pull across many calendar years, for the redesigned
## calendar-year migration-rate chart (Code/memo1_migration_profile.R) and
## metro-tier-share chart (Code/memo1_metro_tiers.R). A sibling to
## Code/acs_pull.R, NOT a parameterization of it -- acs_pull.R's whole
## design (one static 5-year vintage, one checkpoint, feeds
## Code/reweight_column2.R's raking cells) stays untouched. This script
## duplicates (does not source()) acs_pull.R's MIGSP-vs-ST mover-flag logic;
## same codebook concept, but 1-year files need real per-year handling (see
## below), so it isn't a literal copy-paste.
##
## Only pulls what these two charts actually consume: MIGSP/ST/SCHL/AGEP/
## ESR/PWGTP every year, plus PUMA for 2012-2021 (the metro-tier chart's
## 2010-PUMA-vintage window -- see Code/memo1_puma_cbsa_crosswalk.R). Race/
## sex/region are deliberately NOT pulled here -- neither chart uses them,
## and a 2026-08-11 live-data check (not assumed) found RAC2P's own coding
## scheme drifts across vintages even within 2019-2023 (68/67/65/64
## categories in successive years, and 2023 unexpectedly reverts to an old
## 4-digit code scheme) -- pulling race would import a real, separate
## verification burden this pull doesn't need to take on.
##
## [Real codebook-drift findings, confirmed 2026-08-11 by trial-pulling DC
## across 2008/2012/2015/2019/2021/2022/2023 -- NOT assumed from a static
## reference table, since tidycensus::pums_variables' bundled metadata only
## covers 2019-2023 for survey="acs1" and doesn't reach the older years this
## pull needs:]
##   1. SCHL/MIGSP are NOT zero-padded before ~2018 ("9" not "09"; "1" not
##      "001"), but ARE zero-padded from ~2019 on. acs_pull.R's existing
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
##   6. [CAUGHT BY memo1_metro_tiers.R's match-rate check, NOT ASSUMED] ST
##      itself is inconsistently zero-padded, same bug class as PUMA/SCHL/
##      MIGSP (header note #1/#2) but on the ONE column this script hadn't
##      defensively re-padded. Confirmed directly against pums_1yr_filt.rds:
##      years 2012-2016 return single-digit FIPS states (AL/AK/AZ/AR/CA/CO/
##      CT = codes 1/2/4/5/6/8/9) as "1" not "01"; 2017+ zero-pads. This
##      alone explained memo1_metro_tiers.R's state_puma join match rate
##      landing at 89.4% instead of ~100% (those 7 states, weighted by
##      population -- California alone is a huge share -- account for
##      almost exactly the missing ~10.6%). Fixed below with the same
##      integer-round-trip pattern already used for PUMA10.
##   5. [CAUGHT BY A SMOKE TEST, NOT ASSUMED -- see below] MIGSP's "not a
##      mover" sentinel differs by vintage. acs_pull.R's own comment
##      ("MIGSP is populated for every row, not just movers") is correct
##      for its one 2018-2022-vintage pull, where non-movers get MIGSP set
##      to their OWN current state (migsp_int == st_int). A 2-state/2-year
##      smoke test of THIS script (DC+RI, 2012 vs 2019) caught that this is
##      NOT true for the 2012 file: there, non-movers get MIGSP=0 (not
##      their own state), and treating 0 as a valid "moved from state 0"
##      origin inflated the 2012 mover rate to 89% (vs. a sane ~7% once
##      excluded) -- confirmed directly against the raw pull, not assumed.
##      Fixed below by requiring migsp_int >= 1, which is harmless for
##      vintages where 0 never appears.
##
## Checkpointing: one raw file per YEAR (not one monolithic checkpoint) --
## this is a ~16x-cost pull relative to acs_pull.R's single vintage (16
## years x 51 states), so an interrupted run or a later range extension
## must never re-touch already-pulled years. Same per-unit-checkpoint
## philosophy as Code/memo1_column1_construct.R's per-chunk checkpoints
## (which survived a mid-run reboot).
##
## Run before Code/memo1_migration_profile.R and Code/memo1_metro_tiers.R.
## Standalone script, no source() between files.
## =============================================================================

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
# for real on the first full-scale run: one state's download stopped
# advancing entirely, CPU time and memory both flat for 20+ minutes with no
# error ever raised, so the retry logic in pull_one_state() below never
# triggered because nothing failed) hangs forever instead of erroring.
# A finite timeout converts a silent hang into a catchable "Timeout was
# reached" error that the retry wrapper CAN act on.
httr::set_config(httr::timeout(120))

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

# 2020 excluded: COVID-era data-collection problems make the 2020 ACS 1-year
# file "experimental," not directly comparable to other years (Census's own
# guidance). 2005-2007 excluded by default: SCHL==21="Bachelor's" and this
# pull's other codebook assumptions are only confirmed stable from 2008 on
# (see the live-data findings above) -- extending back further would need
# the same trial-pull verification repeated for those years first.
YEARS <- c(2008:2019, 2021:2023)

# PUMA requested only for the metro-tier chart's fixed-vintage window (see
# Code/memo1_puma_cbsa_crosswalk.R's PUMA_VINTAGE=2010 -- ACS 1-year survey
# years 2012-2021 used 2010-vintage PUMA boundaries). Requesting it outside
# this window is pointless (no crosswalk vintage covers it).
PUMA_YEARS <- 2012:2021

MAX_AGE <- 65  # matches acs_pull.R's under-65 trim, for a comparable population

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

## -----------------------------------------------------------------------
## SECTION 1: per-year pull, checkpointed one file per year
## -----------------------------------------------------------------------

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
  variables <- c(state_var, "MIGSP", "SCHL", "AGEP", "ESR", if (include_puma) "PUMA")

  log_step(sprintf("Pulling ACS 1-year PUMS for %d (%d states, PUMA %s)",
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
    # comparing, never rely on string comparison. Filter matches
    # acs_pull.R's thresholds: employed, BA+, drop the PWGTP<10000 outlier
    # artifact.
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

## -----------------------------------------------------------------------
## SECTION 2: combine + derive (cheap, rebuilt every run from raw
## checkpoints -- no separate checkpoint, matching acs_pull.R's own
## established precedent)
## -----------------------------------------------------------------------

log_step("Combining per-year raw checkpoints")
raw_files <- list.files(raw_dir, pattern = "^pums_1yr_raw_\\d{4}\\.rds$", full.names = TRUE)
stopifnot(length(raw_files) == length(YEARS))
pums_1yr <- rbindlist(lapply(raw_files, readRDS), use.names = TRUE, fill = TRUE)

# [FIXED 2026-08-11, see header note #6] ST itself is inconsistently
# zero-padded across vintages (2012-2016 return single-digit FIPS states
# as "1" not "01") -- pad to 2 digits via the same integer-round-trip
# pattern used for PUMA10, so downstream string joins (state_puma in
# Code/memo1_metro_tiers.R) are safe regardless of which year's raw
# formatting produced a given row. Harmless for already-padded years.
pums_1yr[, ST := sprintf("%02d", as.integer(ST))]

n_before_age_trim <- nrow(pums_1yr)
pums_1yr[, age := as.integer(AGEP)]
pums_1yr <- pums_1yr[age < MAX_AGE]
cat(sprintf("Under-%d age trim: %d of %d rows kept (%.1f%%)\n",
            MAX_AGE, nrow(pums_1yr), n_before_age_trim, 100 * nrow(pums_1yr) / n_before_age_trim))

# MIGSP-vs-ST mover flag -- identical concept to acs_pull.R's (MIGSP is
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

# PUMA -> zero-padded 5-digit state_puma join key (2012-2021 rows only;
# other years have PUMA == NA by construction, since it was never
# requested for them). [FIXED 2026-08-11] explicit integer round-trip
# handles the padding inconsistency documented above (header note #2)
# regardless of which raw format a given year's API response used.
if ("PUMA" %in% names(pums_1yr)) {
  pums_1yr[!is.na(PUMA), PUMA10 := sprintf("%05d", as.integer(PUMA))]
}

cat("Per-year sanity check (mover %, n) -- a codebook drift in one specific\n")
cat("year should show up as a visible anomaly here, not be averaged away:\n")
print(pums_1yr[, .(n = .N, mover_pct = round(100 * mean(moved_out_of_state), 2)), by = survey_year][order(survey_year)])

stopifnot(!2020 %in% unique(pums_1yr$survey_year))

saveRDS(pums_1yr, file.path(data_dir, "intermediate/pums_1yr_filt.rds"))
log_step(paste("saved pums_1yr_filt.rds:", nrow(pums_1yr), "rows,", uniqueN(pums_1yr$survey_year), "years"))

cat("acs_pull_1yr.R done.\n")
