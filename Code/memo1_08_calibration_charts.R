# memo1_08_calibration_charts.R
#
# [CONSOLIDATED 2026-08-24, per Nicholas's request to reduce script count]
# Merges four separate files into one, run top-to-bottom: memo1_08a
# (migration-rate profile), memo1_08b (metro-tier-share profile),
# memo1_08c (Phase A/B calibration lines), memo1_08d (scheme comparison
# across rank5/rank3/rank3_region). All four build calendar-year chart-data
# CSVs from Column 2's already-built weights -- none of them run any new
# reweighting of its own. No logic changed from any original script.
# `resolve_col_end()` was identical (same core conversion, same 1900-2100
# sanity-check wrapper) in Sections 1 and 2 -- kept as ONE shared top-level
# copy. The CBSA tier lookup (`cbsa_pop`/`code_to_tier`, a fixed-2022
# rank5-only ranking) was also byte-identical between Sections 2 and 3 --
# built ONCE, reused by both. Section 4 uses a genuinely different,
# scheme-parameterized tier mechanism (memo1_00_metro_tier_definitions.R)
# and keeps its own simpler local `resolve_col_end()` (no sanity check) as
# a closure inside `run_scheme()`, exactly as originally written -- not
# collapsed into the shared one, since it's a different scope with
# different (simpler) behavior, not a true duplicate.
#
# ===========================================================================
# SECTION 1 (was memo1_08a_migration_profile.R, was memo1_05a_...):
# ===========================================================================
# [REDESIGNED 2026-08-11 at Nicholas's request] x-axis changed from years-
# since-graduation (lifecycle) to CALENDAR YEAR (secular trend) -- the
# question is no longer "how does migration probability evolve over a
# graduate's career" but "has migration probability itself changed over
# calendar time." Two comparisons, same as before: (1) this section's
# year-over-year interstate migration rate, now by calendar year, and
# (2) metro-tier share by calendar year (Section 2 below).
#
# Revelio: cbsa_state_0..50 is indexed by years-since-grad (t); a given t
# slice's observations occurred in calendar_year = col_end + t (confirmed
# against 04_li_ed_pos.Rmd/memo1_column1_construct.R -- the wide panel is
# dcast()'d from yrs_graduated = interval - col_end, so a row's t=k value
# really was observed in calendar year col_end+k). The per-row mover
# computation (cbsa_state_t != cbsa_state_(t-1)) is UNCHANGED from the old
# years-since-grad version -- only the grouping variable changes.
#
# ACS: the old age-22 synthetic-cohort proxy against the pooled 5-year file
# is replaced entirely by a direct per-calendar-year measurement from
# memo1_02_acs_pulls.R Section 2's ACS 1-year PUMS pull (one point per
# year, no age profile needed -- MIGSP-vs-ST already IS a 1-year mover
# flag). No age filtering on the ACS side: this line is now "annual
# interstate-mover rate among employed BA+ under-65 adults," matching
# Revelio's side pooling across all grad cohorts within a calendar year --
# both are now secular-trend measures of the same underlying population
# concept, not a lifecycle profile matched to a synthetic cohort.
#
# ===========================================================================
# SECTION 2 (was memo1_08b_metro_tiers.R, was memo1_05b_...):
# ===========================================================================
# [REDESIGNED 2026-08-11 at Nicholas's request] x-axis changed from years-
# since-graduation (lifecycle) to CALENDAR YEAR (secular trend) -- has the
# population's spatial distribution shifted toward superstar metros over
# the sample period, not how does an individual's own location evolve over
# a career. Same four sources (Column 1, Column 2 unweighted, Column 2
# reweighted, ACS benchmark), same tier scheme (Top 10/Top 11-50/
# Top 51-100/Other metro/Non-metro CBSAs, ranked by a single fixed 2022
# population snapshot).
#
# Revelio: same calendar_year = col_end + t re-bucketing as Section 1 (see
# that section's header for the full rationale and the col_end-type
# gotcha). Revelio's line is NOT bounded to any particular window -- its
# CBSA assignment comes from this repo's own unified_cbsa.csv, a single
# fixed scheme, not PUMA-vintage-dependent.
#
# ACS: previously a synthetic-cohort age proxy against the pooled 5-year
# file's ~22%-valid PUMA20 subset. Now uses memo1_02_acs_pulls.R Section
# 2's ACS 1-year PUMS, restricted to survey years 2012-2021 -- the window
# where ACS 1-year files used 2010-vintage PUMA boundaries -- joined
# against memo1_03_geo_crosswalks.R Section 1's PUMA_VINTAGE=2010
# crosswalk (puma_cbsa_tier_crosswalk_2010.rds). This is a REAL, CONFIRMED
# asymmetry, not a bug: the ACS benchmark line in THIS chart only covers
# 2012-2021, while Revelio's lines cover its full calendar range -- worth
# a sentence in the memo.
#
# ===========================================================================
# SECTION 3 (was memo1_08c_calibration_lines.R, was memo1_06a_...):
# ===========================================================================
# [NEW 2026-08-12] Produces real, savable calendar-year output for BOTH
# Phase A (unconditional tier-share calibration) and Phase B (origin-
# destination flow calibration, corrected version) -- migration rate AND
# metro-tier share, both years, not just a single-summary console print.
# Feeds the chart artifact so Nicholas can see the actual lines, per his
# explicit request, rather than summary statistics. Both phases share the
# same Stage 1 base weight (w_full_joint) and the same restriction to
# CALIB_YEARS (2012-2021, where a real ACS calendar-year target exists) --
# see D:\Users\martensn\.claude\plans\nope-i-had-something-logical-aurora.md
# for the full design rationale and HANDOFF.md for the debugging history
# (including the corrected Phase B result -- the origin-destination flow
# calibration turned out NOT to beat the static baseline once a real
# state-prefix bug was fixed, contrary to an initial buggy-code result).
#
# ===========================================================================
# SECTION 4 (was memo1_08d_scheme_comparison.R, was memo1_06b_...):
# ===========================================================================
# [NEW 2026-08-12] Generalizes Section 2's ACS/Revelio tier-share
# computation and Section 3's Phase A/Phase B calibration across any
# memo1_00_metro_tier_definitions.R scheme, so "does Phase B's result hold
# under a different metro-tier classification" is a real, answerable
# question rather than a one-off. Does not touch the production
# memo1_05_reweight_column2.R output or Sections 1-3's own committed
# output (which stay on the "rank5" scheme, the one the published
# chart/memo currently use).
#
# [EXTENDED 2026-08-21, per Nicholas's request] Window widened from
# 2012-2021 to 2012-2023 (2020 excluded), matching the extension already
# applied to the two birth-cohort cuts (memo1_09_cohort_variants.R Section 3).
# Vintage-aware: 2012-2021 uses 2010-vintage PUMA/MIGPUMA, 2022-2023 uses
# 2020-vintage -- these can't be mixed within one vintage. The REVELIO side
# needs no vintage handling at all (fixed-2022 CBSA ranking, no PUMA
# dependency).

library(data.table)
library(tidycensus)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
tidycensus::census_api_key(Sys.getenv("CENSUS_KEY"))

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

T_MAX <- 20  # years post-grad; shared by all four sections

# [col_end type gotcha, confirmed 2026-08-11 against live code] Shared by
# Sections 1-3 -- Section 4 keeps its own simpler local version (no sanity
# check), see header note above for why. column1_covariates.rds's col_end
# is numeric (as.integer(col_end) is correct, memo1_covariates.R:295);
# column2_covariates.rds/column2_reweighted.rds's col_end is a
# factor/character requiring as.integer(as.character(col_end))
# (memo1_covariates.R:421) -- calling as.integer() directly on the Column 2
# factor silently returns level codes (small integers, NOT years), which
# would put every Column 2 point at a bogus "calendar year" in the 1-50
# range. Resolve explicitly per table and assert the result looks like a
# real year range before trusting it.
resolve_col_end <- function(dt, is_factor_like) {
  raw <- if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
  rng <- range(raw, na.rm = TRUE)
  if (rng[1] < 1900 || rng[2] > 2100) {
    stop(sprintf("col_end resolved to an implausible range [%d, %d] -- likely a factor-level-code bug, not real years", rng[1], rng[2]))
  }
  cat(sprintf("  col_end range: %d-%d (sane)\n", rng[1], rng[2]))
  raw
}

# Fixed-2022 CBSA -> tier lookup, shared by Sections 2 and 3 (byte-identical
# in both original scripts). Section 4 uses a different, scheme-
# parameterized mechanism (memo1_00_metro_tier_definitions.R) and does not
# use this.
CBSA_RANK_YEAR <- 2022
cbsa_pop <- get_estimates(geography = "cbsa", product = "population", year = CBSA_RANK_YEAR, vintage = CBSA_RANK_YEAR)
setDT(cbsa_pop)
cbsa_pop <- cbsa_pop[variable == "POPESTIMATE", .(cbsa_code = as.character(GEOID), cbsa_pop = value)]
setorder(cbsa_pop, -cbsa_pop)
cbsa_pop[, cbsa_rank := seq_len(.N)]
cbsa_pop[, metro_tier := fcase(
  cbsa_rank <= 10, "Top 10",
  cbsa_rank <= 50, "Top 11-50",
  cbsa_rank <= 100, "Top 51-100",
  default = "Other metro"
)]
log_step(paste("CBSA tier lookup built:", nrow(cbsa_pop), "CBSAs"))

code_to_tier <- function(codes) {
  tier <- cbsa_pop$metro_tier[match(codes, cbsa_pop$cbsa_code)]
  tier[is.na(tier) & grepl("999$", codes)] <- "Non-metro"
  tier
}

## ===========================================================================
## SECTION 1: Migration-rate-by-calendar-year profile
## ===========================================================================
log_step("SECTION 1: migration-rate-by-calendar-year profile")

## ---- Revelio -- mover rate by calendar year ----
## rate(calendar_year) = share with cbsa_state_t != cbsa_state_(t-1), among
## rows with both non-missing, pooled across EVERY (person, t) pair whose
## col_end+t equals that calendar_year -- i.e. every grad cohort contributes
## to whichever calendar years its own t=1..T_MAX span actually falls in.
## Processed one t-slice at a time and immediately collapsed to per-
## calendar-year partial sums before moving to the next t, rather than
## materializing a full person x t long table (Column 1 alone is ~34M rows
## x 20 t's -- flattening that naively would be ~680M rows).

# Returns the full aggregate (calendar_year, sum_w, sum_wmoved, n), not just
# the trimmed (source, calendar_year, rate, n) public schema -- the grand
# totals of sum_w/sum_wmoved are needed by the invariant check below (rate*n
# alone can't reconstruct a weighted total once weight_col is non-NULL,
# since n is an unweighted count).
revelio_rate_by_calendar_year_full <- function(dt, weight_col, t_max, col_end_numeric) {
  partials <- vector("list", t_max)
  for (t in 1:t_max) {
    cur_col  <- paste0("cbsa_state_", t)
    prev_col <- paste0("cbsa_state_", t - 1)
    if (!all(c(cur_col, prev_col) %in% names(dt))) next
    # [carried over unchanged, FIXED 2026-08-10] weighted.mean()'s na.rm
    # only drops NAs in the value, not the weight -- w_full_joint is NA for
    # ~2.9% of users whose LI cell had no matching PUMS cell, and any NA
    # weight silently poisons the whole aggregate, not just that row.
    weight_valid <- if (is.null(weight_col)) TRUE else !is.na(dt[[weight_col]])
    valid <- !is.na(dt[[cur_col]]) & !is.na(dt[[prev_col]]) & weight_valid & !is.na(col_end_numeric)
    if (sum(valid) == 0) next
    moved <- as.numeric(dt[[cur_col]][valid] != dt[[prev_col]][valid])
    w  <- if (is.null(weight_col)) rep(1, sum(valid)) else dt[[weight_col]][valid]
    cy <- col_end_numeric[valid] + t
    partials[[t]] <- data.table(calendar_year = cy, w = w, wmoved = w * moved)[
      , .(sum_w = sum(w), sum_wmoved = sum(wmoved), n = .N), by = calendar_year]
  }
  rbindlist(partials)[, .(sum_w = sum(sum_w), sum_wmoved = sum(sum_wmoved), n = sum(n)), by = calendar_year]
}
revelio_rate_by_calendar_year <- function(dt, weight_col, t_max, source_label, col_end_numeric) {
  agg <- revelio_rate_by_calendar_year_full(dt, weight_col, t_max, col_end_numeric)
  agg[, .(source = source_label, calendar_year, rate = sum_wmoved / sum_w, n)]
}

log_step("Loading Column 1/2 covariate tables")
column1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds"))
setDT(column1)
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds"))
setDT(li)

log_step("Resolving col_end to real integer years (per-table, see type-gotcha note above)")
cat("Column 1 col_end:\n")
col_end_col1 <- resolve_col_end(column1, is_factor_like = FALSE)
cat("Column 2 col_end:\n")
col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

log_step("Computing Revelio migration-rate-by-calendar-year profiles")
profile_col1      <- revelio_rate_by_calendar_year(column1, NULL, T_MAX, "Column 1 (college-only)", col_end_col1)
profile_col2_unwt <- revelio_rate_by_calendar_year(li, "w_unweighted", T_MAX, "Column 2 (HS+college, unweighted)", col_end_col2)
profile_col2_rewt <- revelio_rate_by_calendar_year(li, "w_full_joint", T_MAX, "Column 2 (reweighted to ACS)", col_end_col2)

## ---- ACS -- direct per-calendar-year mover rate (ACS 1-year PUMS) ----
log_step("Loading ACS 1-year PUMS pull")
pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds"))
setDT(pums_1yr)
stopifnot(!2020 %in% unique(pums_1yr$survey_year))  # COVID-experimental year must have been excluded upstream

acs_rate_by_calendar_year <- function(dt) {
  years <- sort(unique(dt$survey_year))
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    y <- years[i]
    d <- dt[survey_year == y]
    out[[i]] <- data.table(source = "ACS PUMS benchmark", calendar_year = y,
                            rate = if (nrow(d)) weighted.mean(d$moved_out_of_state, d$PWGTP) else NA_real_,
                            n = nrow(d))
  }
  rbindlist(out)
}
profile_acs <- acs_rate_by_calendar_year(pums_1yr)

migration_profile <- rbindlist(list(profile_col1, profile_col2_unwt, profile_col2_rewt, profile_acs))

log_step("Sample sizes by calendar_year (check where the lines get thin):")
print(dcast(migration_profile, calendar_year ~ source, value.var = "n"))

## ---- Verification: invariant check ----
# The underlying per-row mover computation is byte-for-byte unchanged from
# the old years-since-grad version, only regrouped -- each source's total
# weighted mover count must be identical whether grouped by calendar_year
# (this section) or by the original years-since-grad t (recomputed here for
# comparison, not loaded from an old output file, so this check is
# self-contained and doesn't depend on stale files on disk).
revelio_rate_by_t <- function(dt, weight_col, t_max, source_label) {
  out <- vector("list", t_max)
  for (t in 1:t_max) {
    cur_col <- paste0("cbsa_state_", t); prev_col <- paste0("cbsa_state_", t - 1)
    if (!all(c(cur_col, prev_col) %in% names(dt))) next
    weight_valid <- if (is.null(weight_col)) TRUE else !is.na(dt[[weight_col]])
    valid <- !is.na(dt[[cur_col]]) & !is.na(dt[[prev_col]]) & weight_valid
    if (sum(valid) == 0) next
    moved <- as.numeric(dt[[cur_col]][valid] != dt[[prev_col]][valid])
    w <- if (is.null(weight_col)) rep(1, sum(valid)) else dt[[weight_col]][valid]
    out[[t]] <- data.table(sum_w = sum(w), sum_wmoved = sum(w * moved))
  }
  agg <- rbindlist(out)
  c(sum_w = sum(agg$sum_w), sum_wmoved = sum(agg$sum_wmoved))
}
for (chk in list(
  list(dt = column1, wc = NULL, cy = col_end_col1, lbl = "Column 1"),
  list(dt = li, wc = "w_unweighted", cy = col_end_col2, lbl = "Column 2 unweighted"),
  list(dt = li, wc = "w_full_joint", cy = col_end_col2, lbl = "Column 2 reweighted")
)) {
  by_t <- revelio_rate_by_t(chk$dt, chk$wc, T_MAX, chk$lbl)
  by_cy_full <- revelio_rate_by_calendar_year_full(chk$dt, chk$wc, T_MAX, chk$cy)
  sum_wmoved_cy <- sum(by_cy_full$sum_wmoved)
  ok <- isTRUE(all.equal(unname(by_t["sum_wmoved"]), sum_wmoved_cy, tolerance = 1e-6))
  cat(sprintf("[invariant check] %s: sum_wmoved by-t = %.2f, by-calendar-year = %.2f -- %s\n",
              chk$lbl, by_t["sum_wmoved"], sum_wmoved_cy, if (ok) "MATCH" else "MISMATCH -- investigate"))
  if (!ok) stop(sprintf("Invariant check failed for %s: regrouping by calendar_year changed the total weighted mover count", chk$lbl))
}

cat(sprintf("2020 present in ACS calendar_year column: %s (expect FALSE)\n",
            2020 %in% profile_acs$calendar_year))

fwrite(migration_profile, file.path(data_dir, "results/memo1_migration_rate_by_calendar_year.csv"))
saveRDS(migration_profile, file.path(data_dir, "results/memo1_migration_rate_by_calendar_year.rds"))
log_step("SECTION 1 done: saved memo1_migration_rate_by_calendar_year.csv/.rds")

## ===========================================================================
## SECTION 2: Metro-tier-share-by-calendar-year profile
## ===========================================================================
log_step("SECTION 2: metro-tier-share-by-calendar-year profile")

## ---- Revelio -- share by tier, by calendar year ----
## Same t-slice-then-collapse-immediately pattern as Section 1's
## revelio_rate_by_calendar_year() -- never materializes a full person x t
## long table.
ACS_PUMA_WINDOW <- 2012:2021  # matches memo1_03_geo_crosswalks.R Section 1's PUMA_VINTAGE=2010 coverage

revelio_tier_share_by_calendar_year <- function(dt, weight_col, t_max, source_label, col_end_numeric) {
  partials <- vector("list", t_max + 1)
  for (t in 0:t_max) {
    col <- paste0("cbsa_code_", t)
    if (!col %in% names(dt)) next
    tier <- code_to_tier(dt[[col]])
    # [carried over unchanged, FIXED 2026-08-10] exclude NA weight
    # explicitly, not just NA tier -- sum(w) for a group is NA if any
    # contributing row has an NA weight.
    weight_valid <- if (is.null(weight_col)) TRUE else !is.na(dt[[weight_col]])
    valid <- !is.na(tier) & weight_valid & !is.na(col_end_numeric)
    if (sum(valid) == 0) next
    w  <- if (is.null(weight_col)) rep(1, sum(valid)) else dt[[weight_col]][valid]
    cy <- col_end_numeric[valid] + t
    partials[[t + 1]] <- data.table(calendar_year = cy, tier = tier[valid], w = w)[
      , .(w = sum(w), n = .N), by = .(calendar_year, tier)]
  }
  agg <- rbindlist(partials)[, .(w = sum(w), n = sum(n)), by = .(calendar_year, tier)]
  agg[, share := w / sum(w), by = calendar_year]
  agg[, .(source = source_label, calendar_year, tier, share, n)]
}

log_step("Loading Column 1/2 covariate tables")
column1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds"))
setDT(column1)
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds"))
setDT(li)

log_step("Resolving col_end to real integer years (per-table)")
cat("Column 1 col_end:\n")
col_end_col1 <- resolve_col_end(column1, is_factor_like = FALSE)
cat("Column 2 col_end:\n")
col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

log_step("Computing Revelio metro-tier-share-by-calendar-year profiles")
tier_col1      <- revelio_tier_share_by_calendar_year(column1, NULL, T_MAX, "Column 1 (college-only)", col_end_col1)
tier_col2_unwt <- revelio_tier_share_by_calendar_year(li, "w_unweighted", T_MAX, "Column 2 (HS+college, unweighted)", col_end_col2)
tier_col2_rewt <- revelio_tier_share_by_calendar_year(li, "w_full_joint", T_MAX, "Column 2 (reweighted to ACS)", col_end_col2)

rm(column1, li)
gc()

## ---- ACS -- share by tier, by calendar year (2012-2021 only) ----
log_step("Loading ACS 1-year PUMS pull + 2010-vintage PUMA-CBSA-tier crosswalk")
pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds"))
setDT(pums_1yr)
stopifnot("PUMA10" %in% names(pums_1yr))
puma_xwalk <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2010.rds"))

pums_window <- pums_1yr[survey_year %in% ACS_PUMA_WINDOW & !is.na(PUMA10)]
pums_window[, state_puma := paste0(ST, PUMA10)]
cat(sprintf("ACS rows in the %d-%d PUMA window: %d\n", min(ACS_PUMA_WINDOW), max(ACS_PUMA_WINDOW), nrow(pums_window)))
cat(sprintf("ACS state_puma format check (should look like '1100100' etc.): %s\n",
            paste(head(unique(pums_window$state_puma), 3), collapse = ", ")))
# [First-class check] every row in this window carries ONE genuine PUMA
# vintage (unlike the old pooled-5-year file's ~78% "-0009" placeholder
# problem) -- match rate should be close to 100%. A low rate here means
# something is still pointed at the wrong vintage.
match_rate <- 100 * mean(pums_window$state_puma %in% puma_xwalk$state_puma)
cat(sprintf("state_puma match rate onto the 2010-vintage crosswalk: %.1f%% (expect close to 100%%)\n", match_rate))

# Fractional expansion: each respondent contributes PWGTP * tier_share to
# every tier their PUMA overlaps (same "soft" pattern used elsewhere in
# this pipeline).
pums_long <- merge(pums_window[, .(state_puma, survey_year, PWGTP)], puma_xwalk,
                    by = "state_puma", all.x = TRUE, allow.cartesian = TRUE)
pums_long[, w := PWGTP * share]

acs_tier_share_by_calendar_year <- function(dt, n_lookup, years) {
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    y <- years[i]
    d <- dt[survey_year == y & !is.na(metro_tier)]
    if (nrow(d) == 0) next
    tab <- d[, .(w = sum(w, na.rm = TRUE)), by = metro_tier]
    tab[, share := w / sum(w)]
    # n = actual respondent count at this year (from the un-expanded table),
    # not nrow(d) -- d fractionally expands split-PUMA respondents.
    out[[i]] <- data.table(source = "ACS PUMS benchmark", calendar_year = y, tier = tab$metro_tier,
                            share = tab$share, n = sum(n_lookup$survey_year == y))
  }
  rbindlist(out)
}
tier_acs <- acs_tier_share_by_calendar_year(pums_long, pums_window, ACS_PUMA_WINDOW)

metro_tier_profile <- rbindlist(list(tier_col1, tier_col2_unwt, tier_col2_rewt, tier_acs), fill = TRUE)

## ---- Verification ----
log_step("Sample sizes by calendar_year and source:")
print(unique(metro_tier_profile[, .(source, calendar_year, n)])[order(source, calendar_year)])

share_sums <- metro_tier_profile[, .(total_share = sum(share)), by = .(source, calendar_year)]
n_bad <- nrow(share_sums[abs(total_share - 1) > 0.01])
cat(sprintf("(source, calendar_year) groups where tier shares don't sum to ~1: %d of %d (expect 0)\n",
            n_bad, nrow(share_sums)))

fwrite(metro_tier_profile, file.path(data_dir, "results/memo1_metro_tier_by_calendar_year.csv"))
saveRDS(metro_tier_profile, file.path(data_dir, "results/memo1_metro_tier_by_calendar_year.rds"))
log_step("SECTION 2 done: saved memo1_metro_tier_by_calendar_year.csv/.rds")

## ===========================================================================
## SECTION 3: Phase A/B calibration lines (rank5 scheme)
## ===========================================================================
log_step("SECTION 3: Phase A/B calibration lines")

# [FIXED 2026-08-12, caught by a visible chart-rendering artifact] 2020 is
# excluded from ACS's own tier/migration data everywhere else in this
# project (COVID-experimental year) -- naively including it in CALIB_YEARS
# meant the ratio-merge below fell back to its is.na(ratio)->1 default for
# 2020 specifically (no ACS row to calibrate against that year), producing
# an uncalibrated point sandwiched between two calibrated ones and a
# visible kink/spike in the chart line. The migration-rate/tier-share GAP
# numbers already reported were unaffected (those compare via inner merges
# against ACS truth data, which has no 2020 row either way) -- this was
# purely a chart-output bug, not a computation-result bug.
CALIB_YEARS <- setdiff(2012:2021, 2020)
RATIO_CAP_LO <- 0.05
RATIO_CAP_HI <- 20

log_step("Loading column2_reweighted.rds")
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds"))
setDT(li)
col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

existing_tier <- fread(file.path(data_dir, "results/memo1_metro_tier_by_calendar_year.csv"))
existing_mig  <- fread(file.path(data_dir, "results/memo1_migration_rate_by_calendar_year.csv"))

## ---- PHASE A: unconditional destination-tier calibration by year ----
log_step("Building Phase A ratios")
revelio_static <- existing_tier[source == "Column 2 (reweighted to ACS)" & calendar_year %in% CALIB_YEARS,
                                 .(calendar_year, tier, revelio_share = share)]
acs_actual_tier <- existing_tier[source == "ACS PUMS benchmark" & calendar_year %in% CALIB_YEARS,
                                  .(calendar_year, tier, acs_share = share)]
ratio_a <- merge(revelio_static, acs_actual_tier, by = c("calendar_year", "tier"))
ratio_a[, ratio := pmin(pmax(acs_share / revelio_share, RATIO_CAP_LO), RATIO_CAP_HI)]

log_step("Computing Phase A calibrated tier share + migration rate")
tier_a_partials <- vector("list", T_MAX + 1)
for (t in 0:T_MAX) {
  col <- paste0("cbsa_code_", t)
  if (!col %in% names(li)) next
  tier <- code_to_tier(li[[col]])
  cy <- col_end_col2 + t
  valid <- !is.na(tier) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
  if (sum(valid) == 0) next
  d <- data.table(calendar_year = cy[valid], tier = tier[valid], w_base = li$w_full_joint[valid])
  d <- merge(d, ratio_a[, .(calendar_year, tier, ratio)], by = c("calendar_year", "tier"), all.x = TRUE)
  d[, ratio := fifelse(is.na(ratio), 1, ratio)]
  tier_a_partials[[t + 1]] <- d[, .(w = sum(w_base * ratio)), by = .(calendar_year, tier)]
}
tier_a <- rbindlist(tier_a_partials)[, .(w = sum(w)), by = .(calendar_year, tier)]
tier_a[, share := w / sum(w), by = calendar_year]
tier_a_out <- tier_a[, .(source = "Column 2 (Phase A: tier-calibrated)", calendar_year, tier, share, n = NA_integer_)]

mig_a_partials <- vector("list", T_MAX)
for (t in 1:T_MAX) {
  cur_col <- paste0("cbsa_state_", t); prev_col <- paste0("cbsa_state_", t - 1); tier_col <- paste0("cbsa_code_", t)
  if (!all(c(cur_col, prev_col, tier_col) %in% names(li))) next
  cy <- col_end_col2 + t
  valid <- !is.na(li[[cur_col]]) & !is.na(li[[prev_col]]) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
  if (sum(valid) == 0) next
  moved <- as.numeric(li[[cur_col]][valid] != li[[prev_col]][valid])
  tier <- code_to_tier(li[[tier_col]][valid])
  d <- data.table(calendar_year = cy[valid], tier, w_base = li$w_full_joint[valid], moved)
  d <- merge(d, ratio_a[, .(calendar_year, tier, ratio)], by = c("calendar_year", "tier"), all.x = TRUE)
  d[, ratio := fifelse(is.na(ratio), 1, ratio)]
  mig_a_partials[[t]] <- d[, .(sum_w = sum(w_base * ratio), sum_wmoved = sum(w_base * ratio * moved), n = .N), by = calendar_year]
}
mig_a <- rbindlist(mig_a_partials)[, .(sum_w = sum(sum_w), sum_wmoved = sum(sum_wmoved), n = sum(n)), by = calendar_year]
mig_a_out <- mig_a[, .(source = "Column 2 (Phase A: tier-calibrated)", calendar_year, rate = sum_wmoved / sum_w, n)]

## ---- PHASE B: origin-destination flow calibration by year (CORRECTED --
## MIGSP-derived origin state, not current state, as MIGPUMA's prefix) ----
log_step("Loading ACS 1-year PUMS + both tier crosswalks for Phase B")
pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds"))
setDT(pums_1yr)
puma_tier <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2010.rds"))
setDT(puma_tier)
migpuma_tier <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2010.rds"))
setDT(migpuma_tier)

acs_window <- pums_1yr[survey_year %in% CALIB_YEARS & !is.na(PUMA10)]
acs_window[, state_puma := paste0(ST, PUMA10)]
acs_window[, migsp_int := suppressWarnings(as.integer(MIGSP))]
# [CORRECTED 2026-08-12, see HANDOFF.md] origin state prefix must come
# from MIGSP (state 1 year ago), not ST (current state) -- confirmed
# directly (100.0% vs 35.7% crosswalk match rate for interstate movers).
acs_window[, state_migpuma := fifelse(
  !is.na(MIGPUMA10) & !is.na(migsp_int) & migsp_int >= 1L & migsp_int <= 56L,
  paste0(sprintf("%02d", migsp_int), MIGPUMA10), NA_character_
)]

dest_long <- merge(
  acs_window[, .(row_id = .I, survey_year, state_puma, state_migpuma, PWGTP)],
  puma_tier[, .(state_puma, dest_tier = metro_tier, dest_share = share)],
  by = "state_puma", all.x = TRUE, allow.cartesian = TRUE
)
movers <- dest_long[!is.na(state_migpuma)]
origin_movers <- merge(
  movers[, .(row_id, survey_year, state_migpuma, dest_tier, dest_share, PWGTP)],
  migpuma_tier[, .(state_migpuma, origin_tier = metro_tier, origin_share = share)],
  by = "state_migpuma", all.x = TRUE, allow.cartesian = TRUE
)
origin_movers <- origin_movers[!is.na(origin_tier)]
non_movers <- dest_long[is.na(state_migpuma) & !is.na(dest_tier)]
non_movers[, `:=`(origin_tier = dest_tier, origin_share = dest_share)]
acs_flows <- rbindlist(list(
  origin_movers[, .(row_id, survey_year, dest_tier, dest_share, origin_tier, origin_share, PWGTP)],
  non_movers[, .(row_id, survey_year, dest_tier, dest_share, origin_tier, origin_share, PWGTP)]
))
acs_flows[, w := PWGTP * dest_share * origin_share]
acs_margin_b <- acs_flows[, .(w = sum(w)), by = .(survey_year, origin_tier, dest_tier)]
acs_margin_b[, acs_share := w / sum(w), by = survey_year]
setnames(acs_margin_b, "survey_year", "calendar_year")
cat(sprintf("Phase B ACS-side margin match: %.2f%% of mover rows resolved a tier\n",
            100 * uniqueN(origin_movers$row_id) / uniqueN(movers$row_id)))

log_step("Computing Revelio origin x destination x year shares (Phase B)")
rev_partials <- vector("list", T_MAX)
for (t in 1:T_MAX) {
  cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
  if (!all(c(cur_col, prev_col) %in% names(li))) next
  dest_tier <- code_to_tier(li[[cur_col]]); origin_tier <- code_to_tier(li[[prev_col]])
  cy <- col_end_col2 + t
  valid <- !is.na(dest_tier) & !is.na(origin_tier) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
  if (sum(valid) == 0) next
  rev_partials[[t]] <- data.table(
    calendar_year = cy[valid], origin_tier = origin_tier[valid], dest_tier = dest_tier[valid], w = li$w_full_joint[valid]
  )[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
}
revelio_margin_b <- rbindlist(rev_partials)[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
revelio_margin_b[, revelio_share := w / sum(w), by = calendar_year]

ratio_b <- merge(revelio_margin_b[, .(calendar_year, origin_tier, dest_tier, revelio_share)],
                  acs_margin_b[, .(calendar_year, origin_tier, dest_tier, acs_share)],
                  by = c("calendar_year", "origin_tier", "dest_tier"))
ratio_b[, ratio := pmin(pmax(acs_share / revelio_share, RATIO_CAP_LO), RATIO_CAP_HI)]

log_step("Computing Phase B calibrated tier share + migration rate")
tier_b_partials <- vector("list", T_MAX + 1)
for (t in 0:T_MAX) {
  cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
  if (!cur_col %in% names(li)) next
  dest_tier <- code_to_tier(li[[cur_col]])
  origin_tier <- if (prev_col %in% names(li)) code_to_tier(li[[prev_col]]) else rep(NA_character_, nrow(li))
  cy <- col_end_col2 + t
  valid <- !is.na(dest_tier) & !is.na(origin_tier) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
  if (sum(valid) == 0) next
  d <- data.table(calendar_year = cy[valid], origin_tier = origin_tier[valid], dest_tier = dest_tier[valid], w_base = li$w_full_joint[valid])
  d <- merge(d, ratio_b[, .(calendar_year, origin_tier, dest_tier, ratio)], by = c("calendar_year", "origin_tier", "dest_tier"), all.x = TRUE)
  d[, ratio := fifelse(is.na(ratio), 1, ratio)]
  tier_b_partials[[t + 1]] <- d[, .(w = sum(w_base * ratio)), by = .(calendar_year, dest_tier)]
}
tier_b <- rbindlist(tier_b_partials)[, .(w = sum(w)), by = .(calendar_year, tier = dest_tier)]
tier_b[, share := w / sum(w), by = calendar_year]
tier_b_out <- tier_b[, .(source = "Column 2 (Phase B: flow-calibrated)", calendar_year, tier, share, n = NA_integer_)]

mig_b_partials <- vector("list", T_MAX)
for (t in 1:T_MAX) {
  cur_col <- paste0("cbsa_state_", t); prev_col <- paste0("cbsa_state_", t - 1)
  tier_cur_col <- paste0("cbsa_code_", t); tier_prev_col <- paste0("cbsa_code_", t - 1)
  if (!all(c(cur_col, prev_col, tier_cur_col, tier_prev_col) %in% names(li))) next
  cy <- col_end_col2 + t
  valid <- !is.na(li[[cur_col]]) & !is.na(li[[prev_col]]) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
  if (sum(valid) == 0) next
  moved <- as.numeric(li[[cur_col]][valid] != li[[prev_col]][valid])
  dest_tier <- code_to_tier(li[[tier_cur_col]][valid]); origin_tier <- code_to_tier(li[[tier_prev_col]][valid])
  d <- data.table(calendar_year = cy[valid], origin_tier, dest_tier, w_base = li$w_full_joint[valid], moved)
  d <- merge(d, ratio_b[, .(calendar_year, origin_tier, dest_tier, ratio)], by = c("calendar_year", "origin_tier", "dest_tier"), all.x = TRUE)
  d[, ratio := fifelse(is.na(ratio), 1, ratio)]
  mig_b_partials[[t]] <- d[, .(sum_w = sum(w_base * ratio), sum_wmoved = sum(w_base * ratio * moved), n = .N), by = calendar_year]
}
mig_b <- rbindlist(mig_b_partials)[, .(sum_w = sum(sum_w), sum_wmoved = sum(sum_wmoved), n = sum(n)), by = calendar_year]
mig_b_out <- mig_b[, .(source = "Column 2 (Phase B: flow-calibrated)", calendar_year, rate = sum_wmoved / sum_w, n)]

## ---- Save, both phases, both metrics ----
tier_calibrated <- rbindlist(list(tier_a_out, tier_b_out))
mig_calibrated <- rbindlist(list(mig_a_out, mig_b_out))

fwrite(tier_calibrated, file.path(data_dir, "results/memo1_metro_tier_by_calendar_year_calibrated.csv"))
fwrite(mig_calibrated, file.path(data_dir, "results/memo1_migration_rate_by_calendar_year_calibrated.csv"))

cat("\n=== Metro tier share comparison, mean abs gap vs ACS, by source ===\n")
acs_t <- existing_tier[source == "ACS PUMS benchmark", .(calendar_year, tier, acs_share = share)]
for (src in c("Column 2 (Phase A: tier-calibrated)", "Column 2 (Phase B: flow-calibrated)")) {
  m <- merge(tier_calibrated[source == src, .(calendar_year, tier, share)], acs_t, by = c("calendar_year", "tier"))
  cat(sprintf("%s: %.5f\n", src, mean(abs(m$share - m$acs_share))))
}
cat("(Column 2 static reweighted, for reference, was 0.02206 per the 2026-08-11 HANDOFF entry)\n")

cat("\n=== Migration rate comparison, mean abs gap vs ACS, by source ===\n")
acs_m <- existing_mig[source == "ACS PUMS benchmark" & calendar_year %in% CALIB_YEARS, .(calendar_year, acs_rate = rate)]
for (src in c("Column 2 (Phase A: tier-calibrated)", "Column 2 (Phase B: flow-calibrated)")) {
  m <- merge(mig_calibrated[source == src, .(calendar_year, rate)], acs_m, by = "calendar_year")
  cat(sprintf("%s: %.5f\n", src, mean(abs(m$rate - m$acs_rate))))
}
cat("(Column 2 static reweighted, for reference, was 0.01214 -- see HANDOFF.md)\n")

log_step("SECTION 3 done.")

## ===========================================================================
## SECTION 4: Scheme comparison (rank5 / rank3 / rank3_region)
## ===========================================================================
log_step("SECTION 4: scheme comparison across rank5/rank3/rank3_region")
source(here::here("Code/memo1_00_metro_tier_definitions.R"))

VINTAGE_WINDOWS <- list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023)  # 2020 excluded everywhere in this project (COVID-experimental)
S4_CALIB_YEARS <- unlist(VINTAGE_WINDOWS, use.names = FALSE)

# ---- ACS-side flow data for ONE PUMA vintage window -- copied from
# memo1_09_cohort_variants.R Section 3 (see that file for the original
# header note on why this needs to be vintage-parameterized). Returns
# list(dest_long, flow) ready to rbind across vintage windows.
build_acs_flow_data <- function(pums_1yr, years, puma_col, migpuma_col, puma_tier, migpuma_tier) {
  acs_window <- pums_1yr[survey_year %in% years & !is.na(get(puma_col))]
  acs_window[, state_puma := paste0(ST, get(puma_col))]
  dest_long <- merge(
    acs_window[, .(row_id, survey_year, state_puma, PWGTP)],
    puma_tier[, .(state_puma, dest_tier = metro_tier, dest_share = share)],
    by = "state_puma", all.x = TRUE, allow.cartesian = TRUE
  )

  acs_window[, migsp_int := suppressWarnings(as.integer(MIGSP))]
  acs_window[, state_migpuma := fifelse(
    !is.na(get(migpuma_col)) & !is.na(migsp_int) & migsp_int >= 1L & migsp_int <= 56L,
    paste0(sprintf("%02d", migsp_int), get(migpuma_col)), NA_character_
  )]
  dest_long_mig <- merge(dest_long[!is.na(dest_tier)], acs_window[, .(row_id, survey_year, state_migpuma)],
                          by = c("row_id", "survey_year"))
  movers_full <- dest_long_mig[!is.na(state_migpuma)]
  origin_movers <- merge(
    movers_full[, .(row_id, calendar_year = survey_year, state_migpuma, dest_tier, dest_share, PWGTP)],
    migpuma_tier[, .(state_migpuma, origin_tier = metro_tier, origin_share = share)],
    by = "state_migpuma", all.x = TRUE, allow.cartesian = TRUE
  )
  origin_movers <- origin_movers[!is.na(origin_tier)]
  nm <- dest_long_mig[is.na(state_migpuma)]
  nm[, `:=`(origin_tier = dest_tier, origin_share = dest_share, calendar_year = survey_year)]
  flow <- rbindlist(list(
    origin_movers[, .(calendar_year, origin_tier, dest_tier, w = PWGTP * dest_share * origin_share)],
    nm[, .(calendar_year, origin_tier, dest_tier, w = PWGTP * dest_share * origin_share)]
  ))
  list(dest_long = dest_long[, .(calendar_year = survey_year, tier = dest_tier, w = PWGTP * dest_share)], flow = flow)
}

run_scheme <- function(scheme_name) {
  cat(sprintf("\n\n========== SCHEME: %s ==========\n", scheme_name))
  cat(sprintf("%s\n", METRO_TIER_SCHEMES[[scheme_name]]$label))

  puma_tier_2010 <- readRDS(file.path(data_dir, sprintf("intermediate/puma_cbsa_tier_crosswalk_2010_%s.rds", scheme_name))); setDT(puma_tier_2010)
  migpuma_tier_2010 <- readRDS(file.path(data_dir, sprintf("intermediate/migpuma_cbsa_tier_crosswalk_2010_%s.rds", scheme_name))); setDT(migpuma_tier_2010)
  puma_tier_2020 <- readRDS(file.path(data_dir, sprintf("intermediate/puma_cbsa_tier_crosswalk_2020_%s.rds", scheme_name))); setDT(puma_tier_2020)
  migpuma_tier_2020 <- readRDS(file.path(data_dir, sprintf("intermediate/migpuma_cbsa_tier_crosswalk_2020_%s.rds", scheme_name))); setDT(migpuma_tier_2020)

  lookup <- build_cbsa_tier_lookup(scheme_name)
  code_to_tier_cbsa <- code_to_tier_for_scheme(lookup)  # for Revelio's real CBSA codes
  region_lookup <- if (lookup$scheme$uses_region) state_fips_to_region() else NULL

  ## ---- ACS-side: tier share + origin-destination flow, by calendar year,
  ## combined across both PUMA vintage windows (see build_acs_flow_data()
  ## and this section's header for why the Revelio side needs no equivalent
  ## split) ----
  pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds"))
  setDT(pums_1yr)
  pums_1yr[, row_id := .I]  # global, stable across both vintage-window subsets below
  acs_2010 <- build_acs_flow_data(pums_1yr, VINTAGE_WINDOWS[["2010"]], "PUMA10", "MIGPUMA10", puma_tier_2010, migpuma_tier_2010)
  acs_2020 <- build_acs_flow_data(pums_1yr, VINTAGE_WINDOWS[["2020"]], "PUMA20", "MIGPUMA20", puma_tier_2020, migpuma_tier_2020)
  cat(sprintf("  ACS 2010-vintage window: %d flow rows; 2020-vintage window: %d flow rows\n",
              nrow(acs_2010$flow), nrow(acs_2020$flow)))

  acs_tier <- rbindlist(list(acs_2010$dest_long, acs_2020$dest_long))[, .(w = sum(w)), by = .(calendar_year, tier)]
  acs_tier[, acs_share := w / sum(w), by = calendar_year]

  acs_margin_b <- rbindlist(list(acs_2010$flow, acs_2020$flow))[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
  acs_margin_b[, acs_share := w / sum(w), by = calendar_year]

  ## ---- Revelio-side: load li, resolve col_end (local simpler version, no
  ## sanity-check wrapper -- kept exactly as originally written, not
  ## collapsed into the shared top-level resolve_col_end(), since this one
  ## is a scoped closure with genuinely different, simpler behavior) ----
  li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds"))
  setDT(li)
  resolve_col_end_local <- function(dt, is_factor_like) {
    raw <- if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
    raw
  }
  col_end_col2 <- resolve_col_end_local(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

  # [CHECKED before writing, not assumed] li's cbsa_state_t columns store
  # 2-letter POSTAL abbreviations ("MA", "IL"), not FIPS codes -- confirmed
  # directly against a live sample. Index STATE_REGION_CROSSWALK (also
  # abbr-keyed) directly; state_fips_to_region()'s FIPS-keyed lookup is
  # only needed on the ACS/crosswalk-build side, which works from STATEFP.
  region_for_state <- if (lookup$scheme$uses_region) {
    setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
  } else NULL

  # Shared helper, used everywhere a tier gets computed from Revelio's own
  # columns -- takes the CBSA code vector and the MATCHING state-abbr
  # vector for THAT SAME year (critical for Phase B's origin tier, which
  # must use cbsa_state_{t-1}, not cbsa_state_t -- a person's origin
  # region is where they WERE, not where they ended up. Passing the wrong
  # state vector would silently mislabel every mover's origin region).
  tier_from_code <- function(code_vals, state_vals) {
    if (lookup$scheme$uses_region) {
      code_to_tier_cbsa(code_vals, region = unname(region_for_state[state_vals]))
    } else {
      code_to_tier_cbsa(code_vals)
    }
  }

  ## ---- Revelio static-reweighted tier share by calendar year ----
  static_partials <- vector("list", T_MAX + 1)
  for (t in 0:T_MAX) {
    code_col <- paste0("cbsa_code_", t); state_col <- paste0("cbsa_state_", t)
    if (!code_col %in% names(li)) next
    cy <- col_end_col2 + t
    valid <- !is.na(li[[code_col]]) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% S4_CALIB_YEARS
    if (sum(valid) == 0) next
    tier <- tier_from_code(li[[code_col]][valid], li[[state_col]][valid])
    static_partials[[t + 1]] <- data.table(calendar_year = cy[valid], tier, w = li$w_full_joint[valid])[!is.na(tier), .(w = sum(w)), by = .(calendar_year, tier)]
  }
  static_tier <- rbindlist(static_partials)[, .(w = sum(w)), by = .(calendar_year, tier)]
  static_tier[, static_share := w / sum(w), by = calendar_year]

  gap_static <- merge(static_tier[, .(calendar_year, tier, static_share)], acs_tier[, .(calendar_year, tier, acs_share)],
                       by = c("calendar_year", "tier"))
  cat(sprintf("Static-reweighted tier share, mean abs gap vs ACS: %.5f\n", mean(abs(gap_static$static_share - gap_static$acs_share))))

  ## ---- Phase A: unconditional tier calibration ----
  ratio_a <- merge(static_tier[, .(calendar_year, tier, revelio_share = static_share)],
                    acs_tier[, .(calendar_year, tier, acs_share)], by = c("calendar_year", "tier"))
  ratio_a[, ratio := pmin(pmax(acs_share / revelio_share, RATIO_CAP_LO), RATIO_CAP_HI)]
  n_capped_a <- ratio_a[ratio != acs_share / revelio_share, .N]

  # [FIXED before trusting any output -- caught by a real number mismatch
  # against Section 3's already-reported rank5 numbers for the exact same
  # scheme] Two loops, matching Section 3's structure exactly rather than
  # reusing one `valid` mask for both -- the migration-rate `valid` is
  # based SOLELY on the state columns (cbsa_state_t/t-1), never requiring
  # cbsa_code_t (tier) to be non-NA; a missing tier there falls through to
  # the ratio-defaults-to-1 merge behavior rather than being excluded. An
  # earlier version of this loop shared one `valid` mask between the
  # tier-share and migration computations, inheriting the tier-share
  # loop's extra !is.na(cbsa_code_t) requirement into the migration count
  # -- silently excluding rows the original script kept, which shifted the
  # reported gap numbers (e.g. rank5 Phase A migration gap 0.01269 vs. the
  # already-reported 0.01184). No `[!is.na(tier)]` post-filter either, for
  # the same reason -- the original never filters NA tier out, it lets the
  # ratio-merge handle it.
  tier_partials_a <- vector("list", T_MAX + 1)
  for (t in 0:T_MAX) {
    code_col <- paste0("cbsa_code_", t); state_col <- paste0("cbsa_state_", t)
    if (!code_col %in% names(li)) next
    cy <- col_end_col2 + t
    valid <- !is.na(li[[code_col]]) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% S4_CALIB_YEARS
    if (sum(valid) == 0) next
    tier <- tier_from_code(li[[code_col]][valid], li[[state_col]][valid])
    d <- data.table(calendar_year = cy[valid], tier, w_base = li$w_full_joint[valid])
    d <- merge(d, ratio_a[, .(calendar_year, tier, ratio)], by = c("calendar_year", "tier"), all.x = TRUE)
    d[, ratio := fifelse(is.na(ratio), 1, ratio)]
    tier_partials_a[[t + 1]] <- d[, .(w = sum(w_base * ratio)), by = .(calendar_year, tier)]
  }
  tier_a <- rbindlist(tier_partials_a)[, .(w = sum(w)), by = .(calendar_year, tier)]
  tier_a[, share := w / sum(w), by = calendar_year]
  gap_a <- merge(tier_a[, .(calendar_year, tier, share)], acs_tier[, .(calendar_year, tier, acs_share)], by = c("calendar_year", "tier"))
  cat(sprintf("Phase A tier share, mean abs gap vs ACS: %.5f (%d cells capped)\n", mean(abs(gap_a$share - gap_a$acs_share)), n_capped_a))

  # Separate migration loop, `valid` based ONLY on the state columns --
  # matching Section 3's mig_a_partials loop exactly (see the note above
  # the tier-share loop for why this has to be independent).
  mig_partials_a <- vector("list", T_MAX)
  for (t in 1:T_MAX) {
    cur_col <- paste0("cbsa_state_", t); prev_col <- paste0("cbsa_state_", t - 1); tier_col <- paste0("cbsa_code_", t)
    if (!all(c(cur_col, prev_col, tier_col) %in% names(li))) next
    cy <- col_end_col2 + t
    valid <- !is.na(li[[cur_col]]) & !is.na(li[[prev_col]]) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% S4_CALIB_YEARS
    if (sum(valid) == 0) next
    moved <- as.numeric(li[[cur_col]][valid] != li[[prev_col]][valid])
    tier <- tier_from_code(li[[tier_col]][valid], li[[cur_col]][valid])  # cur_col here IS the current-year state column
    d <- data.table(calendar_year = cy[valid], tier, w_base = li$w_full_joint[valid], moved)
    d <- merge(d, ratio_a[, .(calendar_year, tier, ratio)], by = c("calendar_year", "tier"), all.x = TRUE)
    d[, ratio := fifelse(is.na(ratio), 1, ratio)]
    mig_partials_a[[t]] <- d[, .(sum_w = sum(w_base * ratio), sum_wmoved = sum(w_base * ratio * moved)), by = calendar_year]
  }
  mig_a <- rbindlist(mig_partials_a)[, .(sum_w = sum(sum_w), sum_wmoved = sum(sum_wmoved)), by = calendar_year]
  mig_a[, rate := sum_wmoved / sum_w]
  existing_mig_s4 <- fread(file.path(data_dir, "results/memo1_migration_rate_by_calendar_year.csv"))
  acs_mig <- existing_mig_s4[source == "ACS PUMS benchmark" & calendar_year %in% S4_CALIB_YEARS, .(calendar_year, acs_rate = rate)]
  static_mig <- existing_mig_s4[source == "Column 2 (reweighted to ACS)" & calendar_year %in% S4_CALIB_YEARS, .(calendar_year, static_rate = rate)]
  gap_mig_a <- merge(mig_a[, .(calendar_year, rate)], acs_mig, by = "calendar_year")
  gap_mig_static <- merge(static_mig, acs_mig, by = "calendar_year")
  cat(sprintf("Static-reweighted migration rate, mean abs gap vs ACS: %.5f\n", mean(abs(gap_mig_static$static_rate - gap_mig_static$acs_rate))))
  cat(sprintf("Phase A migration rate, mean abs gap vs ACS: %.5f\n", mean(abs(gap_mig_a$rate - gap_mig_a$acs_rate))))

  ## ---- Phase B: origin-destination flow calibration. ACS side
  ## (acs_margin_b) already built above, combined across both vintage
  ## windows -- only the Revelio side is computed here. For region-crossed
  ## schemes this is a real sparsity test, not just a mechanical extension --
  ## 12x12=144 cells vs. a mover subsample that's already only ~15% of any
  ## year's ACS sample. Run it anyway (Nicholas's explicit call: a sparse
  ## result here is itself informative -- evidence the simpler univariate
  ## region signal is the more reliable one to build on).
  rev_partials <- vector("list", T_MAX)
  for (t in 1:T_MAX) {
    cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
    cur_state_col <- paste0("cbsa_state_", t); prev_state_col <- paste0("cbsa_state_", t - 1)
    if (!all(c(cur_col, prev_col) %in% names(li))) next
    # dest_tier's region comes from THIS year's state; origin_tier's from
    # LAST year's -- using the wrong one would silently swap a mover's
    # origin and destination regions.
    dest_tier <- tier_from_code(li[[cur_col]], li[[cur_state_col]])
    origin_tier <- tier_from_code(li[[prev_col]], li[[prev_state_col]])
    cy <- col_end_col2 + t
    valid <- !is.na(dest_tier) & !is.na(origin_tier) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% S4_CALIB_YEARS
    if (sum(valid) == 0) next
    rev_partials[[t]] <- data.table(calendar_year = cy[valid], origin_tier = origin_tier[valid], dest_tier = dest_tier[valid], w = li$w_full_joint[valid])[
      , .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
  }
  revelio_margin_b <- rbindlist(rev_partials)[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
  revelio_margin_b[, revelio_share := w / sum(w), by = calendar_year]
  ratio_b <- merge(revelio_margin_b[, .(calendar_year, origin_tier, dest_tier, revelio_share)],
                    acs_margin_b[, .(calendar_year, origin_tier, dest_tier, acs_share)],
                    by = c("calendar_year", "origin_tier", "dest_tier"))
  ratio_b[, ratio_raw := acs_share / revelio_share]
  ratio_b[, ratio := pmin(pmax(ratio_raw, RATIO_CAP_LO), RATIO_CAP_HI)]
  n_tiers <- uniqueN(c(revelio_margin_b$origin_tier, revelio_margin_b$dest_tier))
  n_possible_cells <- n_tiers * n_tiers * uniqueN(ratio_b$calendar_year)
  n_capped_b <- ratio_b[ratio != ratio_raw, .N]
  cat(sprintf("Phase B ratio table: %d of %d possible (year x origin x dest) cells have BOTH Revelio and ACS coverage (%.1f%%), %d capped\n",
              nrow(ratio_b), n_possible_cells, 100 * nrow(ratio_b) / n_possible_cells, n_capped_b))
  if (n_capped_b > 0) {
    cat(sprintf("  Capped ratio range before clamping: [%.2f, %.2f]\n", min(ratio_b$ratio_raw), max(ratio_b$ratio_raw)))
  }

  # Two independent loops, same reasoning as Phase A above -- the original
  # mig_b_partials loop's `valid` is based solely on the state columns,
  # never inheriting a tier-non-NA requirement.
  tier_b_partials <- vector("list", T_MAX + 1)
  for (t in 0:T_MAX) {
    cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
    cur_state_col <- paste0("cbsa_state_", t); prev_state_col <- paste0("cbsa_state_", t - 1)
    if (!cur_col %in% names(li)) next
    dest_tier <- tier_from_code(li[[cur_col]], li[[cur_state_col]])
    origin_tier <- if (prev_col %in% names(li)) tier_from_code(li[[prev_col]], li[[prev_state_col]]) else rep(NA_character_, nrow(li))
    cy <- col_end_col2 + t
    valid <- !is.na(dest_tier) & !is.na(origin_tier) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% S4_CALIB_YEARS
    if (sum(valid) == 0) next
    d <- data.table(calendar_year = cy[valid], origin_tier = origin_tier[valid], dest_tier = dest_tier[valid], w_base = li$w_full_joint[valid])
    d <- merge(d, ratio_b[, .(calendar_year, origin_tier, dest_tier, ratio)], by = c("calendar_year", "origin_tier", "dest_tier"), all.x = TRUE)
    d[, ratio := fifelse(is.na(ratio), 1, ratio)]
    tier_b_partials[[t + 1]] <- d[, .(w = sum(w_base * ratio)), by = .(calendar_year, dest_tier)]
  }

  mig_b_partials <- vector("list", T_MAX)
  for (t in 1:T_MAX) {
    cur_col <- paste0("cbsa_state_", t); prev_col <- paste0("cbsa_state_", t - 1)
    tier_cur_col <- paste0("cbsa_code_", t); tier_prev_col <- paste0("cbsa_code_", t - 1)
    if (!all(c(cur_col, prev_col, tier_cur_col, tier_prev_col) %in% names(li))) next
    cy <- col_end_col2 + t
    valid <- !is.na(li[[cur_col]]) & !is.na(li[[prev_col]]) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% S4_CALIB_YEARS
    if (sum(valid) == 0) next
    moved <- as.numeric(li[[cur_col]][valid] != li[[prev_col]][valid])
    dest_tier <- tier_from_code(li[[tier_cur_col]][valid], li[[cur_col]][valid])
    origin_tier <- tier_from_code(li[[tier_prev_col]][valid], li[[prev_col]][valid])
    d <- data.table(calendar_year = cy[valid], origin_tier, dest_tier, w_base = li$w_full_joint[valid], moved)
    d <- merge(d, ratio_b[, .(calendar_year, origin_tier, dest_tier, ratio)], by = c("calendar_year", "origin_tier", "dest_tier"), all.x = TRUE)
    d[, ratio := fifelse(is.na(ratio), 1, ratio)]
    mig_b_partials[[t]] <- d[, .(sum_w = sum(w_base * ratio), sum_wmoved = sum(w_base * ratio * moved)), by = calendar_year]
  }
  tier_b <- rbindlist(tier_b_partials)[, .(w = sum(w)), by = .(calendar_year, tier = dest_tier)]
  tier_b[, share := w / sum(w), by = calendar_year]
  gap_b <- merge(tier_b[, .(calendar_year, tier, share)], acs_tier[, .(calendar_year, tier, acs_share)], by = c("calendar_year", "tier"))
  cat(sprintf("Phase B tier share, mean abs gap vs ACS: %.5f\n", mean(abs(gap_b$share - gap_b$acs_share))))

  mig_b <- rbindlist(mig_b_partials)[, .(sum_w = sum(sum_w), sum_wmoved = sum(sum_wmoved)), by = calendar_year]
  mig_b[, rate := sum_wmoved / sum_w]
  gap_mig_b <- merge(mig_b[, .(calendar_year, rate)], acs_mig, by = "calendar_year")
  cat(sprintf("Phase B migration rate, mean abs gap vs ACS: %.5f\n", mean(abs(gap_mig_b$rate - gap_mig_b$acs_rate))))

  # [NEW 2026-08-12] Return the underlying tables, not just console prints,
  # so a driver can export chart-ready CSVs.
  gap_summary <- data.table(
    scheme = scheme_name,
    metric = c("tier", "tier", "tier", "migration", "migration", "migration"),
    phase  = c("static", "phase_a", "phase_b", "static", "phase_a", "phase_b"),
    gap    = c(
      mean(abs(gap_static$static_share - gap_static$acs_share)),
      mean(abs(gap_a$share - gap_a$acs_share)),
      mean(abs(gap_b$share - gap_b$acs_share)),
      mean(abs(gap_mig_static$static_rate - gap_mig_static$acs_rate)),
      mean(abs(gap_mig_a$rate - gap_mig_a$acs_rate)),
      mean(abs(gap_mig_b$rate - gap_mig_b$acs_rate))
    )
  )

  mig_lines <- rbindlist(list(
    acs_mig[, .(source = "ACS PUMS benchmark", calendar_year, rate = acs_rate, n = NA_integer_)],
    static_mig[, .(source = "Column 2 (reweighted to ACS)", calendar_year, rate = static_rate, n = NA_integer_)],
    mig_a[, .(source = "Column 2 (Phase A: tier-calibrated)", calendar_year, rate, n = NA_integer_)],
    mig_b[, .(source = "Column 2 (Phase B: flow-calibrated)", calendar_year, rate, n = NA_integer_)]
  ))
  mig_lines[, scheme := scheme_name]

  list(gap_summary = gap_summary, mig_lines = mig_lines)
}

results <- lapply(c("rank5", "rank3", "rank3_region"), run_scheme)
names(results) <- c("rank5", "rank3", "rank3_region")

## ---- Export: scheme-comparison gap summary (all 3 schemes) ----
gap_summary_all <- rbindlist(lapply(results, `[[`, "gap_summary"))
fwrite(gap_summary_all, file.path(data_dir, "results/memo1_scheme_gap_summary.csv"))
cat("\nWrote memo1_scheme_gap_summary.csv:\n")
print(gap_summary_all)

## ---- Export: rank3_region migration-rate calendar-year lines (chart-ready,
## same schema as memo1_migration_rate_by_calendar_year_calibrated.csv) ----
mig_lines_rank3_region <- results[["rank3_region"]]$mig_lines[, .(source, calendar_year, rate, n)]
fwrite(mig_lines_rank3_region, file.path(data_dir, "results/memo1_migration_rate_by_calendar_year_rank3_region.csv"))
cat("\nWrote memo1_migration_rate_by_calendar_year_rank3_region.csv\n")

log_step("memo1_08_calibration_charts.R done.")
