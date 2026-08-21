# memo1_06b_scheme_comparison.R
#
# [NEW 2026-08-12] Generalizes memo1_metro_tiers.R's ACS/Revelio tier-share
# computation and memo1_calibration_lines.R's Phase A/Phase B calibration
# across any Code/memo1_00_metro_tier_definitions.R scheme, so "does Phase B's
# result hold under a different metro-tier classification" is a real,
# answerable question rather than a one-off. Standalone test script --
# does not touch reweight_column2.R or the production memo1_metro_tiers.R/
# memo1_migration_profile.R output (which stay on the "rank5" scheme, the
# one the published chart/memo currently use).
#
# Requires the scheme's crosswalks already built, BOTH PUMA vintages:
#   Data/intermediate/puma_cbsa_tier_crosswalk_2010_<scheme>.rds / _2020_<scheme>.rds
#   Data/intermediate/migpuma_cbsa_tier_crosswalk_2010_<scheme>.rds / _2020_<scheme>.rds
# (Code/memo1_03a_puma_cbsa_crosswalk.R / memo1_migpuma_cbsa_crosswalk.R,
# both scheme- and vintage-parameterized.)
#
# [EXTENDED 2026-08-21, per Nicholas's request] Window widened from
# 2012-2021 to 2012-2023 (2020 excluded), matching the extension already
# applied to the two birth-cohort cuts on 2026-08-13
# (memo1_07c_cohort_scheme_comparison.R) -- this script was the one place
# that extension never reached, flagged explicitly in MEMO1_WEIGHTING.md
# SS6.4 until now. Same vintage-aware approach as memo1_07c: 2012-2021 uses
# 2010-vintage PUMA/MIGPUMA (PUMA10/MIGPUMA10), 2022-2023 uses 2020-vintage
# (PUMA20/MIGPUMA20) -- these can't be mixed within one vintage, see
# memo1_02b_acs_pull_1yr.R / memo1_03a/03b's headers. The REVELIO side
# needs no vintage handling at all (fixed-2022 CBSA ranking, no PUMA
# dependency) -- confirmed by inspection, not changed here.
# build_acs_flow_data() is copied near-verbatim from memo1_07c (same
# already-verified logic, not re-derived) so it can run once per vintage
# window and get combined, rather than duplicating the block.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
source(here::here("Code/memo1_00_metro_tier_definitions.R"))

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

VINTAGE_WINDOWS <- list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023)  # 2020 excluded everywhere in this project (COVID-experimental)
CALIB_YEARS <- unlist(VINTAGE_WINDOWS, use.names = FALSE)
T_MAX <- 20
RATIO_CAP_LO <- 0.05
RATIO_CAP_HI <- 20

# ---- ACS-side flow data for ONE PUMA vintage window -- copied from
# memo1_07c_cohort_scheme_comparison.R (see that file for the original
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
  ## and this file's header for why the Revelio side needs no equivalent
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

  ## ---- Revelio-side: load li, resolve col_end ----
  li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds"))
  setDT(li)
  resolve_col_end <- function(dt, is_factor_like) {
    raw <- if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
    raw
  }
  col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

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
    valid <- !is.na(li[[code_col]]) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
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
  # against memo1_calibration_lines.R's already-reported rank5 numbers for
  # the exact same scheme] Two loops, matching the ORIGINAL script's
  # structure exactly rather than reusing one `valid` mask for both --
  # the original's migration-rate `valid` is based SOLELY on the state
  # columns (cbsa_state_t/t-1), never requiring cbsa_code_t (tier) to be
  # non-NA; a missing tier there falls through to the ratio-defaults-to-1
  # merge behavior rather than being excluded. An earlier version of this
  # loop shared one `valid` mask between the tier-share and migration
  # computations, inheriting the tier-share loop's extra
  # !is.na(cbsa_code_t) requirement into the migration count -- silently
  # excluding rows the original script kept, which shifted the reported
  # gap numbers (e.g. rank5 Phase A migration gap 0.01269 vs. the
  # already-reported 0.01184). No `[!is.na(tier)]` post-filter either, for
  # the same reason -- the original never filters NA tier out, it lets the
  # ratio-merge handle it.
  tier_partials_a <- vector("list", T_MAX + 1)
  for (t in 0:T_MAX) {
    code_col <- paste0("cbsa_code_", t); state_col <- paste0("cbsa_state_", t)
    if (!code_col %in% names(li)) next
    cy <- col_end_col2 + t
    valid <- !is.na(li[[code_col]]) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
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
  # matching memo1_calibration_lines.R's mig_a_partials loop exactly (see
  # the note above the tier-share loop for why this has to be independent).
  mig_partials_a <- vector("list", T_MAX)
  for (t in 1:T_MAX) {
    cur_col <- paste0("cbsa_state_", t); prev_col <- paste0("cbsa_state_", t - 1); tier_col <- paste0("cbsa_code_", t)
    if (!all(c(cur_col, prev_col, tier_col) %in% names(li))) next
    cy <- col_end_col2 + t
    valid <- !is.na(li[[cur_col]]) & !is.na(li[[prev_col]]) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
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
  existing_mig <- fread(file.path(data_dir, "results/memo1_migration_rate_by_calendar_year.csv"))
  acs_mig <- existing_mig[source == "ACS PUMS benchmark" & calendar_year %in% CALIB_YEARS, .(calendar_year, acs_rate = rate)]
  static_mig <- existing_mig[source == "Column 2 (reweighted to ACS)" & calendar_year %in% CALIB_YEARS, .(calendar_year, static_rate = rate)]
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
    valid <- !is.na(dest_tier) & !is.na(origin_tier) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
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
    valid <- !is.na(dest_tier) & !is.na(origin_tier) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
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
    valid <- !is.na(li[[cur_col]]) & !is.na(li[[prev_col]]) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
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
  # so a driver can export chart-ready CSVs (Nicholas asked to see the
  # rank3_region result folded into the actual published chart, not just
  # reported as summary statistics -- same standard applied to Phase A/B
  # when they were first built).
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
cat("\n\nmemo1_scheme_comparison.R done.\n")

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
