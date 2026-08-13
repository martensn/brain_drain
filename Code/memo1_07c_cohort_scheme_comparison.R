# memo1_07c_cohort_scheme_comparison.R
#
# [NEW 2026-08-12] Generalizes Code/memo1_06b_scheme_comparison.R's run_scheme()
# (already-verified logic -- see that file's own header for the two real
# bugs found and fixed there, both still fixed here since this reuses the
# function body essentially unchanged) across the two birth-cohort cuts
# built by Code/memo1_07a_cohort_inputs.R, per Nicholas's request. Adds a
# `cohort_name` parameter plus cohort-suffixed input/output paths; also
# now returns the Phase A/B TIER-share lines (tier_a/tier_b), not just the
# migration-rate lines, since the cohort charts need both (chart 2's
# small multiples + chart 3's gap-comparison bars), where the original
# script only needed the latter (chart 3's precursor + chart 1's dashed
# lines).
#
# [EXTENDED 2026-08-13, per Nicholas's request to widen the flow-
# calibration window] The ACS-side origin/destination flow computation is
# now vintage-aware: 2012-2021 uses 2010-vintage PUMA/MIGPUMA (PUMA10/
# MIGPUMA10 + the *_2010_<scheme> crosswalks), 2022-2023 uses 2020-vintage
# (PUMA20/MIGPUMA20 + the *_2020_<scheme> crosswalks) -- see
# Code/memo1_02b_acs_pull_1yr.R and Code/memo1_03a/03b's headers for why
# these can't be mixed within one vintage. The REVELIO side needs no
# vintage handling at all: it already assigns tiers from Revelio's own
# fixed-2022 CBSA ranking (Code/memo1_00_metro_tier_definitions.R), which
# has no PUMA dependency, so it already covered 2022-2023 mechanically as
# soon as CALIB_YEARS was widened -- confirmed by inspection, not changed
# here. `build_acs_flow_data()` factors out what used to be inline ACS-side
# logic so it can run once per vintage window and get combined, rather
# than duplicating the block.
#
# Standalone -- does not touch memo1_06b_scheme_comparison.R or its outputs.
# Requires Code/memo1_07a_cohort_inputs.R and Code/memo1_07b_cohort_
# calendar_charts.R to have already run (needs column2_reweighted_<cohort>.rds,
# pums_1yr_filt_<cohort>.rds, and memo1_migration_rate_by_calendar_year_
# <cohort>.csv as the ACS/static migration-rate benchmark).

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

VINTAGE_WINDOWS <- list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023)
CALIB_YEARS <- unlist(VINTAGE_WINDOWS, use.names = FALSE)
T_MAX <- 20
RATIO_CAP_LO <- 0.05
RATIO_CAP_HI <- 20

# ---- ACS-side flow data for ONE PUMA vintage window -----------------------
# Same logic as the original single-vintage inline code, parameterized by
# which columns/crosswalk to use. Returns a list(dest_long, flow) where
# `flow` already carries origin_tier/dest_tier/calendar_year/w, ready to
# rbind across vintage windows.
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

run_scheme <- function(scheme_name, cohort_name, column2_path, pums1yr_path, mig_benchmark_path) {
  cat(sprintf("\n\n========== COHORT: %s / SCHEME: %s ==========\n", cohort_name, scheme_name))
  cat(sprintf("%s\n", METRO_TIER_SCHEMES[[scheme_name]]$label))

  # [EXTENDED 2026-08-13] Both PUMA vintages' crosswalks, loaded
  # unconditionally -- CALIB_YEARS now always spans both windows.
  puma_tier_2010 <- readRDS(file.path(data_dir, sprintf("intermediate/puma_cbsa_tier_crosswalk_2010_%s.rds", scheme_name))); setDT(puma_tier_2010)
  migpuma_tier_2010 <- readRDS(file.path(data_dir, sprintf("intermediate/migpuma_cbsa_tier_crosswalk_2010_%s.rds", scheme_name))); setDT(migpuma_tier_2010)
  puma_tier_2020 <- readRDS(file.path(data_dir, sprintf("intermediate/puma_cbsa_tier_crosswalk_2020_%s.rds", scheme_name))); setDT(puma_tier_2020)
  migpuma_tier_2020 <- readRDS(file.path(data_dir, sprintf("intermediate/migpuma_cbsa_tier_crosswalk_2020_%s.rds", scheme_name))); setDT(migpuma_tier_2020)

  lookup <- build_cbsa_tier_lookup(scheme_name)
  code_to_tier_cbsa <- code_to_tier_for_scheme(lookup)
  region_lookup <- if (lookup$scheme$uses_region) state_fips_to_region() else NULL

  ## ---- ACS-side: tier share + origin-destination flow, by calendar year,
  ## combined across both PUMA vintage windows (see build_acs_flow_data()
  ## and this file's header for why the Revelio side needs no equivalent
  ## split) ----
  pums_1yr <- readRDS(pums1yr_path)
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
  li <- readRDS(column2_path)
  setDT(li)
  resolve_col_end <- function(dt, is_factor_like) {
    if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
  }
  col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

  region_for_state <- if (lookup$scheme$uses_region) {
    setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
  } else NULL

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

  mig_partials_a <- vector("list", T_MAX)
  for (t in 1:T_MAX) {
    cur_col <- paste0("cbsa_state_", t); prev_col <- paste0("cbsa_state_", t - 1); tier_col <- paste0("cbsa_code_", t)
    if (!all(c(cur_col, prev_col, tier_col) %in% names(li))) next
    cy <- col_end_col2 + t
    valid <- !is.na(li[[cur_col]]) & !is.na(li[[prev_col]]) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
    if (sum(valid) == 0) next
    moved <- as.numeric(li[[cur_col]][valid] != li[[prev_col]][valid])
    tier <- tier_from_code(li[[tier_col]][valid], li[[cur_col]][valid])
    d <- data.table(calendar_year = cy[valid], tier, w_base = li$w_full_joint[valid], moved)
    d <- merge(d, ratio_a[, .(calendar_year, tier, ratio)], by = c("calendar_year", "tier"), all.x = TRUE)
    d[, ratio := fifelse(is.na(ratio), 1, ratio)]
    mig_partials_a[[t]] <- d[, .(sum_w = sum(w_base * ratio), sum_wmoved = sum(w_base * ratio * moved)), by = calendar_year]
  }
  mig_a <- rbindlist(mig_partials_a)[, .(sum_w = sum(sum_w), sum_wmoved = sum(sum_wmoved)), by = calendar_year]
  mig_a[, rate := sum_wmoved / sum_w]
  existing_mig <- fread(mig_benchmark_path)
  acs_mig <- existing_mig[source == "ACS PUMS benchmark" & calendar_year %in% CALIB_YEARS, .(calendar_year, acs_rate = rate)]
  static_mig <- existing_mig[source == "Column 2 (reweighted to ACS)" & calendar_year %in% CALIB_YEARS, .(calendar_year, static_rate = rate)]
  gap_mig_a <- merge(mig_a[, .(calendar_year, rate)], acs_mig, by = "calendar_year")
  gap_mig_static <- merge(static_mig, acs_mig, by = "calendar_year")
  cat(sprintf("Static-reweighted migration rate, mean abs gap vs ACS: %.5f\n", mean(abs(gap_mig_static$static_rate - gap_mig_static$acs_rate))))
  cat(sprintf("Phase A migration rate, mean abs gap vs ACS: %.5f\n", mean(abs(gap_mig_a$rate - gap_mig_a$acs_rate))))

  ## ---- Phase B: origin-destination flow calibration. ACS side
  ## (acs_margin_b) already built above, combined across both vintage
  ## windows -- only the Revelio side is computed here. ----
  rev_partials <- vector("list", T_MAX)
  for (t in 1:T_MAX) {
    cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
    cur_state_col <- paste0("cbsa_state_", t); prev_state_col <- paste0("cbsa_state_", t - 1)
    if (!all(c(cur_col, prev_col) %in% names(li))) next
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

  gap_summary <- data.table(
    cohort = cohort_name,
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
    mig_a[, .(source = "Column 2 (Phase A: tier-calibrated)", calendar_year, rate, n = NA_integer_)],
    mig_b[, .(source = "Column 2 (Phase B: flow-calibrated)", calendar_year, rate, n = NA_integer_)]
  ))

  tier_lines <- rbindlist(list(
    tier_a[, .(source = "Column 2 (Phase A: tier-calibrated)", calendar_year, tier, share, n = NA_integer_)],
    tier_b[, .(source = "Column 2 (Phase B: flow-calibrated)", calendar_year, tier, share, n = NA_integer_)]
  ))

  # Phase B cell-coverage % -- named risk from the plan: cohort restriction
  # compounds Phase B's existing sparsity exposure under region-crossed
  # schemes. Returned so the driver can flag a cohort/scheme combo that
  # comes back visibly sparser than the full-sample run (100% coverage,
  # 0 capped, was the full-sample rank3_region result).
  coverage <- list(n_cells = nrow(ratio_b), n_possible = n_possible_cells,
                    pct = 100 * nrow(ratio_b) / n_possible_cells, n_capped = n_capped_b)

  # [NEW 2026-08-13] Return the ratio tables themselves too, not just what
  # they were used to build -- Nicholas's demographic-crosstab request needs
  # to apply these SAME (year, tier) / (year, origin_tier, dest_tier) ratios
  # to a fixed-calendar-year cross-section, and re-deriving them a third
  # time (after memo1_scheme_comparison.R -> memo1_cohort_scheme_comparison.R)
  # would just be another chance to reintroduce a bug already caught and
  # fixed twice in this exact logic. ratio_a/ratio_b are cheap byproducts
  # already sitting in memory at this point.
  list(gap_summary = gap_summary, mig_lines = mig_lines, tier_lines = tier_lines, coverage = coverage,
       ratio_a = ratio_a[, .(calendar_year, tier, ratio)],
       ratio_b = ratio_b[, .(calendar_year, origin_tier, dest_tier, ratio)])
}

## =========================================================================
## Driver: 2 cohorts x 3 schemes
## =========================================================================

COHORTS <- c("born_1980s", "born_1990s")
SCHEMES <- c("rank5", "rank3", "rank3_region")

all_results <- list()
coverage_report <- vector("list", length(COHORTS) * length(SCHEMES))
ci <- 0
for (cohort_name in COHORTS) {
  column2_path <- file.path(data_dir, sprintf("intermediate/column2_reweighted_%s.rds", cohort_name))
  pums1yr_path <- file.path(data_dir, sprintf("intermediate/pums_1yr_filt_%s.rds", cohort_name))
  mig_benchmark_path <- file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_%s.csv", cohort_name))
  stopifnot(file.exists(column2_path), file.exists(pums1yr_path), file.exists(mig_benchmark_path))

  for (scheme_name in SCHEMES) {
    res <- run_scheme(scheme_name, cohort_name, column2_path, pums1yr_path, mig_benchmark_path)
    all_results[[paste(cohort_name, scheme_name, sep = "__")]] <- res
    ci <- ci + 1
    coverage_report[[ci]] <- data.table(cohort = cohort_name, scheme = scheme_name,
                                         n_cells = res$coverage$n_cells, n_possible = res$coverage$n_possible,
                                         pct_coverage = res$coverage$pct, n_capped = res$coverage$n_capped)
  }
}
cat("\n\nmemo1_cohort_scheme_comparison.R done.\n")

cat("\n\n=== Phase B cell-coverage report (sparsity check per cohort x scheme) ===\n")
print(rbindlist(coverage_report))

## ---- Export per cohort ----
for (cohort_name in COHORTS) {
  keys <- paste(cohort_name, SCHEMES, sep = "__")
  cohort_results <- all_results[keys]
  names(cohort_results) <- SCHEMES

  gap_summary_cohort <- rbindlist(lapply(cohort_results, `[[`, "gap_summary"))
  fwrite(gap_summary_cohort, file.path(data_dir, sprintf("results/memo1_scheme_gap_summary_%s.csv", cohort_name)))

  mig_calibrated <- cohort_results[["rank5"]]$mig_lines
  fwrite(mig_calibrated, file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_calibrated_%s.csv", cohort_name)))

  tier_calibrated <- cohort_results[["rank5"]]$tier_lines
  fwrite(tier_calibrated, file.path(data_dir, sprintf("results/memo1_metro_tier_by_calendar_year_calibrated_%s.csv", cohort_name)))

  mig_lines_rank3_region <- cohort_results[["rank3_region"]]$mig_lines
  fwrite(mig_lines_rank3_region, file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_rank3_region_%s.csv", cohort_name)))

  # [NEW 2026-08-13] Phase A/B ratio tables, per scheme -- needed by
  # Code/memo1_07d_cohort_demo_table.R to apply the same calibration to a
  # fixed-calendar-year demographic cross-section.
  for (scheme_name in SCHEMES) {
    saveRDS(list(ratio_a = cohort_results[[scheme_name]]$ratio_a, ratio_b = cohort_results[[scheme_name]]$ratio_b),
            file.path(data_dir, sprintf("intermediate/phase_ratios_%s_%s.rds", cohort_name, scheme_name)))
  }

  cat(sprintf("\nWrote all export files for %s\n", cohort_name))
}
