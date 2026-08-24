# memo1_08c_calibration_lines.R (was memo1_06a_calibration_lines.R)
#
# [NEW 2026-08-12] Produces real, savable calendar-year output for BOTH
# Phase A (unconditional tier-share calibration) and Phase B (origin-
# destination flow calibration, corrected version) -- migration rate AND
# metro-tier share, both years, not just a single-summary console print
# the way memo1_phase_a_year_calibration_test.R / memo1_phase_b_flow_
# calibration_test.R did. Feeds the chart artifact so Nicholas can see the
# actual lines, per his explicit request, rather than summary statistics.
#
# Standalone -- does not touch reweight_column2.R, memo1_metro_tiers.R, or
# memo1_migration_profile.R's own committed output files (writes new,
# separate _calibrated files).
#
# Both phases share the same Stage 1 base weight (w_full_joint) and the
# same restriction to CALIB_YEARS (2012-2021, where a real ACS calendar-
# year target exists) -- see D:\Users\martensn\.claude\plans\
# nope-i-had-something-logical-aurora.md for the full design rationale and
# HANDOFF.md for the debugging history (including the corrected Phase B
# result -- the origin-destination flow calibration turned out NOT to beat
# the static baseline once a real state-prefix bug was fixed, contrary to
# an initial buggy-code result).

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
T_MAX <- 20

## -----------------------------------------------------------------------
## SECTION 0: CBSA -> tier lookup (same vintage as every other script)
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
## SECTION 1: load li, resolve col_end (shared by both phases)
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

existing_tier <- fread(file.path(data_dir, "results/memo1_metro_tier_by_calendar_year.csv"))
existing_mig  <- fread(file.path(data_dir, "results/memo1_migration_rate_by_calendar_year.csv"))

## =========================================================================
## PHASE A: unconditional destination-tier calibration by year
## =========================================================================

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

## =========================================================================
## PHASE B: origin-destination flow calibration by year (CORRECTED --
## MIGSP-derived origin state, not current state, as MIGPUMA's prefix)
## =========================================================================

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

## =========================================================================
## SECTION 3: save, both phases, both metrics
## =========================================================================

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

log_step("memo1_calibration_lines.R done.")
