# memo1_phase_b_flow_calibration_test.R
#
# [NEW 2026-08-12] Phase B of the calendar-year migration-behavior plan
# (D:\Users\martensn\.claude\plans\nope-i-had-something-logical-aurora.md).
# Standalone TEST script -- does not touch reweight_column2.R,
# memo1_metro_tiers.R, memo1_migration_profile.R, or Phase A's test script,
# or any of their committed output.
#
# Extends Phase A's year-sliced calibration (unconditional destination-tier
# share by year) to a genuine ORIGIN x DESTINATION flow calibration --
# using each person's own rolling prior-year location, not their fixed
# hometown, matched against ACS's MIGPUMA (year-ago residence) -> current
# PUMA transition for that same year. This is the fuller "ACS flows"
# realization Nicholas originally asked about.
#
# ACS-side non-mover imputation, confirmed NECESSARY not hypothetical (see
# HANDOFF.md 2026-08-12 entry): MIGSP/MIGPUMA's ACS universe is "persons
# who lived in a different house 1 year ago" -- NIU/blank for the ~85% who
# didn't move at all (any distance), which is the CORRECT value, not
# missing data. For those rows, origin_tier is imputed as their own
# CURRENT PUMA's tier (origin = destination), the plan's stated convention
# for non-movers -- not a dropped/NA row.
#
# Revelio-side origin is genuinely rolling (not fixed hs_state): each
# person's own location in year Y-1, from cbsa_code_{t-1} in their
# trajectory -- already directly available, no imputation needed, since
# Revelio (unlike ACS) has full year-by-year coverage, not just a
# 1-year-ago snapshot.
#
# Both sides use FRACTIONAL tier assignment (a single PUMA/MIGPUMA can
# straddle >1 metro tier, same as memo1_metro_tiers.R's existing pattern)
# -- expanded via allow.cartesian merges on both origin and destination,
# not just one dimension.

library(data.table)
library(tidycensus)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
tidycensus::census_api_key(Sys.getenv("CENSUS_KEY"))

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

CALIB_YEARS <- 2012:2021
RATIO_CAP_LO <- 0.05
RATIO_CAP_HI <- 20
T_MAX <- 20

## -----------------------------------------------------------------------
## SECTION 0: CBSA -> tier lookup, identical vintage to every other script
## -----------------------------------------------------------------------

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
code_to_tier <- function(codes) {
  tier <- cbsa_pop$metro_tier[match(codes, cbsa_pop$cbsa_code)]
  tier[is.na(tier) & grepl("999$", codes)] <- "Non-metro"
  tier
}
log_step(paste("CBSA tier lookup built:", nrow(cbsa_pop), "CBSAs"))

## -----------------------------------------------------------------------
## SECTION 1: ACS-side origin x destination x year margin
## -----------------------------------------------------------------------

log_step("Loading ACS 1-year PUMS (with MIGPUMA) and both PUMA/MIGPUMA tier crosswalks")
pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds"))
setDT(pums_1yr)
stopifnot(all(c("MIGPUMA10", "PUMA10", "ST", "PWGTP", "survey_year") %in% names(pums_1yr)))

puma_tier <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2010.rds"))
setDT(puma_tier)
migpuma_tier <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2010.rds"))
setDT(migpuma_tier)

acs_window <- pums_1yr[survey_year %in% CALIB_YEARS & !is.na(PUMA10)]
acs_window[, state_puma := paste0(ST, PUMA10)]
# [FIXED 2026-08-12, found via an 18.2% MIGPUMA-crosswalk match-rate
# investigation] MIGPUMA is a sub-area code that only makes sense combined
# with the ORIGIN state, not the respondent's CURRENT state -- for
# interstate movers these differ. Using ST (current state) as the prefix,
# as an earlier version of this script did, got the state right for
# same-state movers (ST==MIGSP there) but wrong for interstate movers
# (only 35.7% matched the crosswalk, vs. 100.0% once corrected -- checked
# directly before writing this fix, not assumed). MIGSP is exactly "state
# of residence 1 year ago," i.e. the correct origin-state prefix for
# MIGPUMA, already available on every row (same column the existing
# moved_out_of_state flag uses).
acs_window[, migsp_int := suppressWarnings(as.integer(MIGSP))]
acs_window[, state_migpuma := fifelse(
  !is.na(MIGPUMA10) & !is.na(migsp_int) & migsp_int >= 1L & migsp_int <= 56L,
  paste0(sprintf("%02d", migsp_int), MIGPUMA10), NA_character_
)]
cat(sprintf("ACS rows in window: %d (of which %d, %.1f%%, have a real MIGPUMA -- i.e. moved at all, any distance)\n",
            nrow(acs_window), sum(!is.na(acs_window$state_migpuma)),
            100 * mean(!is.na(acs_window$state_migpuma))))

# Destination: fractional tier expansion (a respondent's PUMA can straddle
# >1 tier), same pattern memo1_metro_tiers.R already uses.
dest_long <- merge(
  acs_window[, .(row_id = .I, survey_year, state_puma, state_migpuma, PWGTP)],
  puma_tier[, .(state_puma, dest_tier = metro_tier, dest_share = share)],
  by = "state_puma", all.x = TRUE, allow.cartesian = TRUE
)
cat(sprintf("Destination tier match rate: %.1f%%\n", 100 * mean(!is.na(dest_long$dest_tier))))

# Origin: for movers (real state_migpuma), fractional tier via the MIGPUMA
# crosswalk. For non-movers (state_migpuma NA), origin = destination
# (imputed, confirmed necessary -- see header note), carried through at
# THIS row's own dest_tier/dest_share values, not re-derived separately.
movers <- dest_long[!is.na(state_migpuma)]
origin_movers <- merge(
  movers[, .(row_id, survey_year, state_migpuma, dest_tier, dest_share, PWGTP)],
  migpuma_tier[, .(state_migpuma, origin_tier = metro_tier, origin_share = share)],
  by = "state_migpuma", all.x = TRUE, allow.cartesian = TRUE
)
n_migpuma_unmatched <- origin_movers[, uniqueN(row_id[is.na(origin_tier)])]
cat(sprintf("Movers with a MIGPUMA value but no match in the MIGPUMA tier crosswalk: %d of %d mover rows (%.1f%%)\n",
            n_migpuma_unmatched, uniqueN(movers$row_id), 100 * n_migpuma_unmatched / uniqueN(movers$row_id)))
origin_movers <- origin_movers[!is.na(origin_tier)]

non_movers <- dest_long[is.na(state_migpuma) & !is.na(dest_tier)]
non_movers[, `:=`(origin_tier = dest_tier, origin_share = dest_share)]
non_movers <- non_movers[, .(row_id, survey_year, dest_tier, dest_share, origin_tier, origin_share, PWGTP)]

acs_flows <- rbindlist(list(
  origin_movers[, .(row_id, survey_year, dest_tier, dest_share, origin_tier, origin_share, PWGTP)],
  non_movers
))
acs_flows[, w := PWGTP * dest_share * origin_share]
acs_margin <- acs_flows[, .(w = sum(w)), by = .(survey_year, origin_tier, dest_tier)]
acs_margin[, acs_share := w / sum(w), by = survey_year]
cat(sprintf("\nACS origin x destination x year margin: %d cells across %d years\n",
            nrow(acs_margin), uniqueN(acs_margin$survey_year)))
cat("Sample (2017, all origin x dest combos):\n")
print(acs_margin[survey_year == 2017][order(-acs_share)])

## -----------------------------------------------------------------------
## SECTION 2: Revelio-side origin x destination x year, using each
## person's OWN rolling prior-year location (cbsa_code_{t-1}), not a fixed
## hometown -- the actual point of Phase B.
## -----------------------------------------------------------------------

log_step("Loading column2_reweighted.rds")
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds"))
setDT(li)

resolve_col_end <- function(dt, is_factor_like) {
  raw <- if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
  rng <- range(raw, na.rm = TRUE)
  if (rng[1] < 1900 || rng[2] > 2100) stop("col_end resolved to an implausible range")
  raw
}
col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

log_step("Computing Revelio origin x destination x year shares (rolling prior-year origin)")
rev_partials <- vector("list", T_MAX)
for (t in 1:T_MAX) {
  cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
  if (!all(c(cur_col, prev_col) %in% names(li))) next
  dest_tier <- code_to_tier(li[[cur_col]])
  origin_tier <- code_to_tier(li[[prev_col]])
  cy <- col_end_col2 + t
  weight_valid <- !is.na(li$w_full_joint)
  valid <- !is.na(dest_tier) & !is.na(origin_tier) & weight_valid & !is.na(cy) & cy %in% CALIB_YEARS
  if (sum(valid) == 0) next
  rev_partials[[t]] <- data.table(
    survey_year = cy[valid], origin_tier = origin_tier[valid], dest_tier = dest_tier[valid], w = li$w_full_joint[valid]
  )[, .(w = sum(w)), by = .(survey_year, origin_tier, dest_tier)]
}
revelio_margin <- rbindlist(rev_partials)[, .(w = sum(w)), by = .(survey_year, origin_tier, dest_tier)]
revelio_margin[, revelio_share := w / sum(w), by = survey_year]
cat(sprintf("Revelio origin x destination x year margin: %d cells across %d years\n",
            nrow(revelio_margin), uniqueN(revelio_margin$survey_year)))

## -----------------------------------------------------------------------
## SECTION 3: calibration ratio, capped, applied
## -----------------------------------------------------------------------

log_step("Building calibration ratios")
ratio_tab <- merge(
  revelio_margin[, .(survey_year, origin_tier, dest_tier, revelio_share)],
  acs_margin[, .(survey_year, origin_tier, dest_tier, acs_share)],
  by = c("survey_year", "origin_tier", "dest_tier")
)
ratio_tab[, ratio_raw := acs_share / revelio_share]
ratio_tab[, ratio := pmin(pmax(ratio_raw, RATIO_CAP_LO), RATIO_CAP_HI)]
n_capped <- ratio_tab[ratio_raw != ratio, .N]
cat(sprintf("Ratio table: %d (year x origin x dest) cells, %d capped at [%.2f, %d] (%.1f%%)\n",
            nrow(ratio_tab), n_capped, RATIO_CAP_LO, RATIO_CAP_HI, 100 * n_capped / nrow(ratio_tab)))
cat("Ratio distribution:\n")
print(summary(ratio_tab$ratio_raw))

## -----------------------------------------------------------------------
## SECTION 4: apply to migration rate -- the actual test, same as Phase A
## -----------------------------------------------------------------------

log_step("Computing flow-calibrated migration rate")
setnames(ratio_tab, "survey_year", "calendar_year")
mig_partials <- vector("list", T_MAX)
for (t in 1:T_MAX) {
  cur_col <- paste0("cbsa_state_", t); prev_col <- paste0("cbsa_state_", t - 1)
  tier_cur_col <- paste0("cbsa_code_", t); tier_prev_col <- paste0("cbsa_code_", t - 1)
  if (!all(c(cur_col, prev_col, tier_cur_col, tier_prev_col) %in% names(li))) next
  weight_valid <- !is.na(li$w_full_joint)
  cy <- col_end_col2 + t
  valid <- !is.na(li[[cur_col]]) & !is.na(li[[prev_col]]) & weight_valid & !is.na(cy) & cy %in% CALIB_YEARS
  if (sum(valid) == 0) next
  moved <- as.numeric(li[[cur_col]][valid] != li[[prev_col]][valid])
  dest_tier <- code_to_tier(li[[tier_cur_col]][valid])
  origin_tier <- code_to_tier(li[[tier_prev_col]][valid])
  dt_t <- data.table(calendar_year = cy[valid], origin_tier, dest_tier, w_base = li$w_full_joint[valid], moved = moved)
  dt_t <- merge(dt_t, ratio_tab[, .(calendar_year, origin_tier, dest_tier, ratio)],
                by = c("calendar_year", "origin_tier", "dest_tier"), all.x = TRUE)
  dt_t[, ratio := fifelse(is.na(ratio), 1, ratio)]
  dt_t[, w_calibrated := w_base * ratio]
  mig_partials[[t]] <- dt_t[, .(sum_w = sum(w_calibrated), sum_wmoved = sum(w_calibrated * moved), n = .N), by = calendar_year]
}
mig_calibrated <- rbindlist(mig_partials)[, .(sum_w = sum(sum_w), sum_wmoved = sum(sum_wmoved), n = sum(n)), by = calendar_year]
mig_calibrated[, rate := sum_wmoved / sum_w]

existing_mig <- fread(file.path(data_dir, "results/memo1_migration_rate_by_calendar_year.csv"))
static_line <- existing_mig[source == "Column 2 (reweighted to ACS)" & calendar_year %in% CALIB_YEARS, .(calendar_year, static_rate = rate)]
acs_line <- existing_mig[source == "ACS PUMS benchmark" & calendar_year %in% CALIB_YEARS, .(calendar_year, acs_rate = rate)]

comparison <- merge(mig_calibrated[, .(calendar_year, flowcal_rate = rate)], static_line, by = "calendar_year")
comparison <- merge(comparison, acs_line, by = "calendar_year")
comparison[, gap_static := abs(static_rate - acs_rate)]
comparison[, gap_flowcal := abs(flowcal_rate - acs_rate)]
setorder(comparison, calendar_year)
cat("\n=== Migration rate, 2012-2021: static weight vs. Phase B flow-calibrated weight vs. ACS ===\n")
print(comparison)
cat(sprintf("\nMean absolute gap vs ACS -- static weight: %.5f\n", mean(comparison$gap_static)))
cat(sprintf("Mean absolute gap vs ACS -- Phase B flow-calibrated weight: %.5f\n", mean(comparison$gap_flowcal)))
cat("\n(For reference, Phase A's unconditional tier calibration got 0.01184 -- see HANDOFF.md 2026-08-12 entry)\n")

log_step("memo1_phase_b_flow_calibration_test.R done.")
