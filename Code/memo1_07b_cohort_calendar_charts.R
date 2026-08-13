# memo1_07b_cohort_calendar_charts.R
#
# [NEW 2026-08-12] Generalizes memo1_migration_profile.R + memo1_metro_
# tiers.R (the un-calibrated calendar-year lines: Column 1, Column 2
# unweighted/reweighted, ACS benchmark) across the two birth-cohort cuts
# built by Code/memo1_07a_cohort_inputs.R (born_1980s, born_1990s), per
# Nicholas's request for standalone cohort cuts of the whole memo. Same
# exact logic as those two production scripts -- copied, not sourced, per
# this project's standalone-script convention -- just looped over cohort-
# suffixed input/output paths instead of the full-sample files. Does NOT
# touch memo1_migration_profile.R/memo1_metro_tiers.R or their production
# outputs.
#
# Uses Code/memo1_00_metro_tier_definitions.R's "rank5" scheme for CBSA-code ->
# tier assignment (same Top 10/11-50/51-100/Other metro/Non-metro cutoffs
# memo1_metro_tiers.R hardcodes, just from the one centralized place) and
# the matching scheme-suffixed crosswalk
# (puma_cbsa_tier_crosswalk_2010_rank5.rds) rather than duplicating a
# fourth copy of the CBSA-tier lookup.
#
# [EXTENDED 2026-08-13] The metro-tier ACS benchmark now combines both
# PUMA vintage windows (2012-2021 via PUMA10 + the 2010-vintage crosswalk,
# 2022-2023 via PUMA20 + the 2020-vintage crosswalk), matching
# Code/memo1_07c_cohort_scheme_comparison.R's extension. The migration-
# rate side needed no change -- it never depended on PUMA at all, so it
# already covered 2022-2023 mechanically once the underlying ACS pull did.
#
# Run after Code/memo1_07a_cohort_inputs.R.
#
# Omits memo1_migration_profile.R's Section-3 by-t-vs-by-calendar-year
# invariant check: that check verifies the regrouping FUNCTION is correct,
# not anything cohort-specific, and the function here is copied verbatim
# (unchanged) from the already-verified original -- re-checking it per
# cohort would be redundant, not a coverage gap.

library(data.table)
library(tidycensus)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
tidycensus::census_api_key(Sys.getenv("CENSUS_KEY"))
source(here::here("Code/memo1_00_metro_tier_definitions.R"))

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

T_MAX <- 20
VINTAGE_WINDOWS <- list(`2010` = 2012:2021, `2020` = 2022:2023)
COHORTS <- c("born_1980s", "born_1990s")

## ---- rank5 CBSA tier lookup, built once, shared across both cohorts ----
lookup <- build_cbsa_tier_lookup("rank5")
code_to_tier_cbsa <- code_to_tier_for_scheme(lookup)
code_to_tier <- function(codes) code_to_tier_cbsa(codes)  # rank5 ignores `region`
log_step(paste("CBSA tier lookup built (rank5):", nrow(lookup$cbsa_pop), "CBSAs"))

puma_xwalk_2010 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2010_rank5.rds")); setDT(puma_xwalk_2010)
puma_xwalk_2020 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2020_rank5.rds")); setDT(puma_xwalk_2020)

resolve_col_end <- function(dt, is_factor_like) {
  raw <- if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
  rng <- range(raw, na.rm = TRUE)
  if (rng[1] < 1900 || rng[2] > 2100) {
    stop(sprintf("col_end resolved to an implausible range [%d, %d] -- likely a factor-level-code bug, not real years", rng[1], rng[2]))
  }
  cat(sprintf("  col_end range: %d-%d (sane)\n", rng[1], rng[2]))
  raw
}

revelio_rate_by_calendar_year_full <- function(dt, weight_col, t_max, col_end_numeric) {
  partials <- vector("list", t_max)
  for (t in 1:t_max) {
    cur_col  <- paste0("cbsa_state_", t)
    prev_col <- paste0("cbsa_state_", t - 1)
    if (!all(c(cur_col, prev_col) %in% names(dt))) next
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

revelio_tier_share_by_calendar_year <- function(dt, weight_col, t_max, source_label, col_end_numeric) {
  partials <- vector("list", t_max + 1)
  for (t in 0:t_max) {
    col <- paste0("cbsa_code_", t)
    if (!col %in% names(dt)) next
    tier <- code_to_tier(dt[[col]])
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

acs_tier_share_by_calendar_year <- function(dt, n_lookup, years) {
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    y <- years[i]
    d <- dt[survey_year == y & !is.na(metro_tier)]
    if (nrow(d) == 0) next
    tab <- d[, .(w = sum(w, na.rm = TRUE)), by = metro_tier]
    tab[, share := w / sum(w)]
    out[[i]] <- data.table(source = "ACS PUMS benchmark", calendar_year = y, tier = tab$metro_tier,
                            share = tab$share, n = sum(n_lookup$survey_year == y))
  }
  rbindlist(out)
}

## =========================================================================
## Per-cohort build
## =========================================================================

for (cohort_name in COHORTS) {
  cat(sprintf("\n\n========== COHORT: %s ==========\n", cohort_name))

  log_step("Loading cohort-restricted Column 1/2 tables")
  column1 <- readRDS(file.path(data_dir, sprintf("intermediate/column1_covariates_%s.rds", cohort_name)))
  setDT(column1)
  li <- readRDS(file.path(data_dir, sprintf("intermediate/column2_reweighted_%s.rds", cohort_name)))
  setDT(li)

  cat("Column 1 col_end:\n")
  col_end_col1 <- resolve_col_end(column1, is_factor_like = FALSE)
  cat("Column 2 col_end:\n")
  col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

  ## ---- Migration rate ----
  log_step("Computing Revelio migration-rate-by-calendar-year profiles")
  profile_col1      <- revelio_rate_by_calendar_year(column1, NULL, T_MAX, "Column 1 (college-only)", col_end_col1)
  profile_col2_unwt <- revelio_rate_by_calendar_year(li, "w_unweighted", T_MAX, "Column 2 (HS+college, unweighted)", col_end_col2)
  profile_col2_rewt <- revelio_rate_by_calendar_year(li, "w_full_joint", T_MAX, "Column 2 (reweighted to ACS)", col_end_col2)

  pums_1yr <- readRDS(file.path(data_dir, sprintf("intermediate/pums_1yr_filt_%s.rds", cohort_name)))
  setDT(pums_1yr)
  stopifnot(!2020 %in% unique(pums_1yr$survey_year))
  profile_acs <- acs_rate_by_calendar_year(pums_1yr)

  migration_profile <- rbindlist(list(profile_col1, profile_col2_unwt, profile_col2_rewt, profile_acs))
  cat(sprintf("2020 present in ACS calendar_year column: %s (expect FALSE)\n", 2020 %in% profile_acs$calendar_year))

  fwrite(migration_profile, file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_%s.csv", cohort_name)))
  saveRDS(migration_profile, file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_%s.rds", cohort_name)))
  log_step(sprintf("saved memo1_migration_rate_by_calendar_year_%s.csv/.rds", cohort_name))

  ## ---- Metro tier share ----
  log_step("Computing Revelio metro-tier-share-by-calendar-year profiles")
  tier_col1      <- revelio_tier_share_by_calendar_year(column1, NULL, T_MAX, "Column 1 (college-only)", col_end_col1)
  tier_col2_unwt <- revelio_tier_share_by_calendar_year(li, "w_unweighted", T_MAX, "Column 2 (HS+college, unweighted)", col_end_col2)
  tier_col2_rewt <- revelio_tier_share_by_calendar_year(li, "w_full_joint", T_MAX, "Column 2 (reweighted to ACS)", col_end_col2)

  stopifnot("PUMA10" %in% names(pums_1yr), "PUMA20" %in% names(pums_1yr))
  build_tier_acs <- function(puma_col, years, puma_xwalk) {
    pums_window <- pums_1yr[survey_year %in% years & !is.na(get(puma_col))]
    pums_window[, state_puma := paste0(ST, get(puma_col))]
    match_rate <- 100 * mean(pums_window$state_puma %in% puma_xwalk$state_puma)
    cat(sprintf("ACS rows in the %d-%d PUMA window (%s): %d, state_puma match rate onto crosswalk: %.1f%% (expect close to 100%%)\n",
                min(years), max(years), puma_col, nrow(pums_window), match_rate))
    pums_long <- merge(pums_window[, .(state_puma, survey_year, PWGTP)], puma_xwalk[, .(state_puma, metro_tier, share)],
                        by = "state_puma", all.x = TRUE, allow.cartesian = TRUE)
    pums_long[, w := PWGTP * share]
    list(tier_acs = acs_tier_share_by_calendar_year(pums_long, pums_window, years), pums_window = pums_window)
  }
  acs_2010 <- build_tier_acs("PUMA10", VINTAGE_WINDOWS[["2010"]], puma_xwalk_2010)
  acs_2020 <- build_tier_acs("PUMA20", VINTAGE_WINDOWS[["2020"]], puma_xwalk_2020)
  tier_acs <- rbindlist(list(acs_2010$tier_acs, acs_2020$tier_acs))
  pums_window <- rbindlist(list(acs_2010$pums_window, acs_2020$pums_window), fill = TRUE)  # kept only for the rm() below

  metro_tier_profile <- rbindlist(list(tier_col1, tier_col2_unwt, tier_col2_rewt, tier_acs), fill = TRUE)

  share_sums <- metro_tier_profile[, .(total_share = sum(share)), by = .(source, calendar_year)]
  n_bad <- nrow(share_sums[abs(total_share - 1) > 0.01])
  cat(sprintf("(source, calendar_year) groups where tier shares don't sum to ~1: %d of %d (expect 0)\n",
              n_bad, nrow(share_sums)))

  fwrite(metro_tier_profile, file.path(data_dir, sprintf("results/memo1_metro_tier_by_calendar_year_%s.csv", cohort_name)))
  saveRDS(metro_tier_profile, file.path(data_dir, sprintf("results/memo1_metro_tier_by_calendar_year_%s.rds", cohort_name)))
  log_step(sprintf("saved memo1_metro_tier_by_calendar_year_%s.csv/.rds", cohort_name))

  rm(column1, li, pums_1yr, pums_window)
  gc()
}

log_step("memo1_cohort_calendar_charts.R done.")
