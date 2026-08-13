# memo1_02b_acs_pull_1yr.R
## acs_pull_1yr.R
##
## ACS 1-year PUMS pull across many calendar years, for the redesigned
## calendar-year migration-rate chart (Code/memo1_05a_migration_profile.R) and
## metro-tier-share chart (Code/memo1_05b_metro_tiers.R). A sibling to
## Code/memo1_02a_acs_pull_5yr.R, NOT a parameterization of it -- acs_pull.R's whole
## design (one static 5-year vintage, one checkpoint, feeds
## Code/memo1_04_reweight_column2.R's raking cells) stays untouched. This script
## duplicates (does not source()) acs_pull.R's MIGSP-vs-ST mover-flag logic;
## same codebook concept, but 1-year files need real per-year handling (see
## below), so it isn't a literal copy-paste.
##
## Only pulls what these two charts actually consume: MIGSP/ST/SCHL/AGEP/
## ESR/PWGTP every year, plus PUMA for 2012-2021 (the metro-tier chart's
## 2010-PUMA-vintage window -- see Code/memo1_03a_puma_cbsa_crosswalk.R). Race/
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
##   7. [ADDED 2026-08-12, Phase B -- migration-behavior plan] MIGPUMA now
##      pulled alongside PUMA for the same PUMA_YEARS window (Phase B of
##      D:\Users\martensn\.claude\plans\nope-i-had-something-logical-aurora.md
##      needs each ACS respondent's migration-PUMA of residence one year
##      ago, matched via Code/memo1_03b_migpuma_cbsa_crosswalk.R). Smoke-tested
##      first (RI, 2012/2019/2021), not assumed clean: (a) same padding
##      drift as every other geography variable here -- unpadded
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
## this is a ~16x-cost pull relative to acs_pull.R's single vintage (16
## years x 51 states), so an interrupted run or a later range extension
## must never re-touch already-pulled years. Same per-unit-checkpoint
## philosophy as Code/memo1_01a_column1_construct.R's per-chunk checkpoints
## (which survived a mid-run reboot).
##
## Run before Code/memo1_05a_migration_profile.R and Code/memo1_05b_metro_tiers.R.
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

# PUMA requested only for windows where this repo has a matching crosswalk
# vintage (see Code/memo1_03a_puma_cbsa_crosswalk.R). 2010-vintage PUMA
# boundaries covered ACS 1-year survey years 2012-2021; 2020-vintage took
# over starting with the 2022 file. [EXTENDED 2026-08-13, per Nicholas's
# request to widen the flow-calibration window] PUMA_YEARS_2020 added --
# confirmed live that the 1-year file's raw PUMA/MIGPUMA variable names
# don't change with vintage (still plain "PUMA"/"MIGPUMA", not "PUMA20"),
# only their MEANING does -- so the two windows are derived into
# separately-named columns below (PUMA10/MIGPUMA10 vs PUMA20/MIGPUMA20)
# rather than one column silently holding two different vintages of code.
# Pre-2012 (2000-vintage) deliberately excluded -- see memo1_puma_cbsa_
# crosswalk.R's header for why.
PUMA_YEARS_2010 <- 2012:2021
PUMA_YEARS_2020 <- 2022:2023
PUMA_YEARS <- c(PUMA_YEARS_2010, PUMA_YEARS_2020)

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
# Code/memo1_05b_metro_tiers.R) are safe regardless of which year's raw
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

cat("acs_pull_1yr.R done.\n")
