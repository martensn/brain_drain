# memo1_09_cohort_variants.R
#
# [CONSOLIDATED 2026-08-24, per Nicholas's request to reduce script count]
# Merges what were five separate files (memo1_09a_cohort_inputs.R,
# memo1_09b_cohort_calendar_charts.R, memo1_09c_cohort_scheme_comparison.R,
# memo1_09d_cohort_demo_table.R, memo1_09e_reweight_column2_occupation_cohort.R)
# into one, run top-to-bottom in this order -- each later section depends on
# an earlier one's output (Section 2 needs Section 1's cohort-restricted
# inputs; Section 3 needs Sections 1+2; Section 4 needs Sections 1+3;
# Section 5 needs Section 1) and none was ever run standalone. No logic
# changed from any original script, except two function-name collisions
# that would otherwise silently shadow each other now that all five live in
# one script's global environment -- flagged explicitly at each site below
# (Section 5's own `build_acs_flow_data`/`resolve_col_end` renamed with an
# `_s5` suffix; Sections 1-4 keep their original names unchanged).
#
# Together these five sections rebuild Column 2's Stage-1 weight AND
# multiple downstream chart/comparison/table/Stage-2 outputs separately for
# each birth cohort (born_1980s / born_1990s) -- per Nicholas's request for
# two cuts of the whole analysis excluding older adults he'd exclude from
# any Revelio-based analysis given how selected coverage gets at older ages.
#
# SECTION 1 (was memo1_09a_cohort_inputs.R): builds birth-cohort-restricted
# versions of the base inputs used throughout Memo 1. Reuses manual_ipf()
# (now centralized in Code/memo1_ipf.R) since a birth-cohort-restricted
# Revelio sample needs to be raked against a birth-cohort-restricted ACS
# margin, not the full-population one -- filtering column2_reweighted.rds
# after the fact would rake against the wrong reference population. Full
# design rationale in
# D:\Users\martensn\.claude\plans\nope-i-had-something-logical-aurora.md.
#
# Birth year is exact on the Revelio side (a `birth` column already present
# on column1_covariates.rds/column2_covariates.rds). ACS gives age, not
# birth year -- pums_acs5_filt.rds's Stage-1 margin uses a single fixed
# PUMS_YEAR=2022 reference, so the cohort-equivalent filter there is
# `2022 - AGEP`; pums_1yr_filt.rds spans multiple calendar years, so its
# filter is row-varying: `survey_year - AGEP`. Both are 1-year-precision
# approximations, unlike Revelio's exact `birth` -- a real, worth-noting
# asymmetry.
#
# SECTION 2 (was memo1_09b_cohort_calendar_charts.R): generalizes the
# un-calibrated calendar-year lines (Column 1, Column 2 unweighted/
# reweighted, ACS benchmark) across the two birth-cohort cuts built by
# Section 1. Uses Code/memo1_00_metro_tier_definitions.R's "rank5" scheme.
# The metro-tier ACS benchmark combines both PUMA vintage windows
# (2012-2021 via PUMA10 + the 2010-vintage crosswalk, 2022-2023 via PUMA20
# + the 2020-vintage crosswalk).
#
# SECTION 3 (was memo1_09c_cohort_scheme_comparison.R): generalizes the
# scheme-comparison run_scheme() logic across the two birth-cohort cuts.
# Also returns the Phase A/B TIER-share lines (tier_a/tier_b), not just the
# migration-rate lines, since the cohort charts need both. ACS-side
# origin/destination flow computation is vintage-aware (2012-2021 uses
# 2010-vintage PUMA/MIGPUMA, 2022-2023 uses 2020-vintage) -- the REVELIO
# side needs no vintage handling at all, since it assigns tiers from
# Revelio's own fixed-2022 CBSA ranking, which has no PUMA dependency.
#
# SECTION 4 (was memo1_09d_cohort_demo_table.R): a demographic (race/sex/
# region) cross-tab comparing every weighting scheme shown in the cohort
# charts, at one fixed calendar year (2015), for both birth cohorts. Race
# under col2r/phasea/phaseb needs the TRUE post-raking race split (Stage-1
# raking uses race as a raking key, so pre-IPF race_frac shifts after
# raking) -- Section 4 re-runs the exact same melt + manual_ipf() to
# recover it, using the fresh run only for RELATIVE race proportions per
# person (the ABSOLUTE per-person total still comes from the already-
# saved, already-capped w_full_joint). ACS race/sex for 2015 comes from the
# supplementary pull built in Section 3 of memo1_02_acs_pulls.R (was
# memo1_02c_acs_pull_1yr_race2015.R), joined on via SERIALNO+SPORDER.
#
# SECTION 5 (was memo1_09e_reweight_column2_occupation_cohort.R):
# cohort-restricted version of memo1_07_reweight_column2_occupation.R --
# the full-sample w2_occ weight can't be used for the born_1980s/
# born_1990s migration-rate charts since it was calibrated against the
# FULL ACS population, not either birth cohort's. Migration rate here is
# the SAME metric Section 2/memo1_08_calibration_charts.R uses -- share
# with cbsa_state_t != cbsa_state_(t-1), i.e. crossed a STATE line -- not
# the metro-TIER-crossing concept the rest of this file's panels track.
# Output: a separate CSV per cohort
# (memo1_migration_rate_by_calendar_year_geo_occ_born_{cohort}.csv) that
# does NOT touch the CSVs Sections 2/3 write, so nothing already published
# is corrupted by this section.

library(data.table)
library(tidycensus)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
tidycensus::census_api_key(Sys.getenv("CENSUS_KEY"))
source(here::here("Code/memo1_00_metro_tier_definitions.R"))
source(here::here("Code/memo1_ipf.R"))

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

# Shared constants, identical across the sections that used them
# individually before consolidation.
PUMS_YEAR <- 2022  # keep in sync with Code/memo1_02_acs_pulls.R Section 1's PUMS_YEAR
RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob",
                     "native_prob", "multiple_prob", "hispanic_prob")
T_MAX <- 20
RATIO_CAP_LO <- 0.05
RATIO_CAP_HI <- 20

## ===========================================================================
## SECTION 1: cohort-restricted base inputs (column1_covariates_<cohort>.rds,
## column2_reweighted_<cohort>.rds, pums_1yr_filt_<cohort>.rds)
## ===========================================================================
log_step("SECTION 1: cohort-restricted base inputs")

COHORTS <- list(
  born_1980s = c(1980, 1989),
  born_1990s = c(1990, 1999)
)

log_step("Loading column1_covariates.rds / column2_covariates.rds")
column1_covariates <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds"))
setDT(column1_covariates)
stopifnot(all(c("birth") %in% names(column1_covariates)))

li_all <- readRDS(file.path(data_dir, "intermediate/column2_covariates.rds"))
setDT(li_all)
stopifnot(all(c("user_id", "hs_state", "age_bucket", "birth", RACE_PROB_COLS, "m_prob", "f_prob",
                "moved_last_year_state") %in% names(li_all)))
if (is.character(li_all$transfer)) {
  stopifnot(all(unique(li_all$transfer) %in% c("Non-Transfer", "Transfer")))
  li_all[, transfer := as.integer(transfer == "Transfer")]
}
li_all[, `:=`(
  has_associate = as.integer(has_associate),
  has_master    = as.integer(has_master),
  has_mba       = as.integer(has_mba),
  has_doctor    = as.integer(has_doctor)
)]

log_step("Loading pums_acs5_filt.rds / pums_1yr_filt.rds")
pums_acs5_all <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds"))
setDT(pums_acs5_all)
stopifnot(all(c("origin_state", "moved_out_of_state", "PWGTP", "AGEP", "age_bucket", "race", "sex") %in% names(pums_acs5_all)))
pums_acs5_all[, birth_approx := PUMS_YEAR - as.integer(AGEP)]

pums_1yr_all <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds"))
setDT(pums_1yr_all)
stopifnot(all(c("survey_year", "AGEP", "moved_out_of_state", "PWGTP") %in% names(pums_1yr_all)))
pums_1yr_all[, birth_approx := survey_year - as.integer(AGEP)]

data(fips_codes, package = "tidycensus")
abb_to_fips <- unique(fips_codes[, c("state", "state_code")])
setDT(abb_to_fips)
setnames(abb_to_fips, c("state", "state_code"), c("state_abbr", "state_fips"))

## ---- Early sanity check: does the ACS migration benchmark actually shift
## up for younger cohorts, before investing in the full IPF/Phase-A/B
## build? Pooled across all available years (2008-2023, minus 2020) -- a
## quick gate, not the final per-calendar-year chart number. ----
full_acs_mig_rate <- pums_1yr_all[, weighted.mean(moved_out_of_state, PWGTP)]
cat(sprintf("\nFull-sample ACS 1yr migration rate (all birth years, pooled, reference): %.4f\n\n", full_acs_mig_rate))

## ---- Per-cohort build ----
for (cohort_name in names(COHORTS)) {
  rng <- COHORTS[[cohort_name]]
  cat(sprintf("\n\n========== COHORT: %s (born %d-%d) ==========\n", cohort_name, rng[1], rng[2]))

  ## Column 1 (no reweighting -- used unweighted throughout)
  col1_cohort <- column1_covariates[birth >= rng[1] & birth <= rng[2]]
  cat(sprintf("Column 1: %d of %d rows (%.1f%%)\n",
              nrow(col1_cohort), nrow(column1_covariates), 100 * nrow(col1_cohort) / nrow(column1_covariates)))
  saveRDS(col1_cohort, file.path(data_dir, sprintf("intermediate/column1_covariates_%s.rds", cohort_name)))

  ## ACS 1yr benchmark (calendar-year charts + Phase A/B calibration)
  pums_1yr_cohort <- pums_1yr_all[birth_approx >= rng[1] & birth_approx <= rng[2]]
  cat(sprintf("ACS 1yr PUMS: %d of %d rows (%.1f%%)\n",
              nrow(pums_1yr_cohort), nrow(pums_1yr_all), 100 * nrow(pums_1yr_cohort) / nrow(pums_1yr_all)))
  cohort_acs_mig_rate <- pums_1yr_cohort[, weighted.mean(moved_out_of_state, PWGTP)]
  cat(sprintf("Cohort ACS 1yr migration rate (pooled): %.4f vs full-sample %.4f (%+.1f%% relative)\n",
              cohort_acs_mig_rate, full_acs_mig_rate, 100 * (cohort_acs_mig_rate / full_acs_mig_rate - 1)))
  saveRDS(pums_1yr_cohort, file.path(data_dir, sprintf("intermediate/pums_1yr_filt_%s.rds", cohort_name)))

  ## Column 2 Stage-1 IPF, restricted to this cohort on BOTH sides
  li <- li_all[birth >= rng[1] & birth <= rng[2]]
  cat(sprintf("Column 2 (pre-IPF): %d of %d rows (%.1f%%)\n",
              nrow(li), nrow(li_all), 100 * nrow(li) / nrow(li_all)))

  pums_filt <- pums_acs5_all[birth_approx >= rng[1] & birth_approx <= rng[2]]
  cat(sprintf("ACS 5yr PUMS margin population: %d of %d rows (%.1f%%)\n",
              nrow(pums_filt), nrow(pums_acs5_all), 100 * nrow(pums_filt) / nrow(pums_acs5_all)))

  li <- merge(li, abb_to_fips, by.x = "hs_state", by.y = "state_abbr", all.x = TRUE)
  setnames(li, "state_fips", "origin_state")
  li[, w_unweighted := 1]

  li_complete <- li[!is.na(origin_state) & !is.na(age_bucket) & !is.na(white_prob)]
  cat(sprintf("li_complete (non-missing origin_state/age_bucket/race_prob): %d of %d\n", nrow(li_complete), nrow(li)))

  li_long <- melt(
    li_complete,
    id.vars = setdiff(names(li_complete), RACE_PROB_COLS),
    measure.vars = RACE_PROB_COLS,
    variable.name = "race_prob_col",
    value.name = "race_frac"
  )
  race_col_to_label <- setNames(RACE_LABELS, RACE_PROB_COLS)
  li_long[, race := race_col_to_label[as.character(race_prob_col)]]
  li_long[, sex := fifelse(m_prob >= f_prob, "male", "female")]
  li_long[, w_base := w_unweighted * race_frac]

  n_before_mover_na_drop <- nrow(li_long)
  li_long <- li_long[!is.na(moved_last_year_state)]
  cat(sprintf("Dropped for NA moved_last_year_state: %d of %d melted rows (%.1f%%)\n",
              n_before_mover_na_drop - nrow(li_long), n_before_mover_na_drop,
              100 * (n_before_mover_na_drop - nrow(li_long)) / n_before_mover_na_drop))

  cell_state_age_race_sex <- pums_filt[, .(pop = sum(PWGTP)), by = .(origin_state, age_bucket, race, sex)]
  margin_demo <- copy(cell_state_age_race_sex)
  setnames(margin_demo, "pop", "Freq")
  for (col in c("origin_state", "age_bucket", "race", "sex")) margin_demo[[col]] <- as.character(margin_demo[[col]])
  li_long[, `:=`(origin_state = as.character(origin_state), age_bucket = as.character(age_bucket),
                  race = as.character(race), sex = as.character(sex),
                  moved_last_year_state = as.character(moved_last_year_state))]

  margin_mover <- pums_filt[, .(Freq = sum(PWGTP)), by = .(origin_state, moved_out_of_state)]
  setnames(margin_mover, "moved_out_of_state", "moved_last_year_state")
  margin_mover[, `:=`(origin_state = as.character(origin_state), moved_last_year_state = as.character(moved_last_year_state))]

  demo_key_cols <- c("origin_state", "age_bucket", "race", "sex")
  n_before_cell_match <- nrow(li_long)
  li_long <- li_long[margin_demo[, ..demo_key_cols], on = demo_key_cols, nomatch = 0]
  cat(sprintf("Dropped for no matching ACS demo cell: %d of %d melted rows (%.1f%%)\n",
              n_before_cell_match - nrow(li_long), n_before_cell_match,
              100 * (n_before_cell_match - nrow(li_long)) / n_before_cell_match))
  li_long_keys <- unique(li_long[, ..demo_key_cols])
  n_before_margin_restrict <- nrow(margin_demo)
  margin_demo <- margin_demo[li_long_keys, on = demo_key_cols, nomatch = 0]
  cat(sprintf("Restricted margin_demo to cells with sample coverage: %d of %d ACS cells kept (%.1f%%)\n",
              nrow(margin_demo), n_before_margin_restrict, 100 * nrow(margin_demo) / n_before_margin_restrict))

  mover_key_cols <- c("origin_state", "moved_last_year_state")
  li_long <- li_long[margin_mover[, ..mover_key_cols], on = mover_key_cols, nomatch = 0]
  margin_mover <- margin_mover[unique(li_long[, ..mover_key_cols]), on = mover_key_cols, nomatch = 0]

  margins_spec <- list(
    list(keys = demo_key_cols, pop = margin_demo),
    list(keys = mover_key_cols, pop = margin_mover)
  )

  ## Small-subsample validation before the full-scale run, same discipline
  ## reweight_column2.R uses.
  set.seed(20260812)
  li_long_test <- li_long[sample.int(.N, min(100000, .N))]
  margin_demo_test <- margin_demo[unique(li_long_test[, ..demo_key_cols]), on = demo_key_cols, nomatch = 0]
  margin_mover_test <- margin_mover[unique(li_long_test[, ..mover_key_cols]), on = mover_key_cols, nomatch = 0]
  li_long_test <- li_long_test[margin_demo_test[, ..demo_key_cols], on = demo_key_cols, nomatch = 0]
  li_long_test <- li_long_test[margin_mover_test[, ..mover_key_cols], on = mover_key_cols, nomatch = 0]
  w_test <- manual_ipf(li_long_test, "w_base",
                        list(list(keys = demo_key_cols, pop = margin_demo_test),
                             list(keys = mover_key_cols, pop = margin_mover_test)),
                        verbose = FALSE)
  cat(sprintf("Subsample IPF test (n=%d melted rows): weight range [%.3f, %.3f], any non-finite: %s\n",
              length(w_test), min(w_test), max(w_test), any(!is.finite(w_test))))
  if (any(!is.finite(w_test))) stop(sprintf("[%s] Subsample IPF test produced non-finite weights -- investigate before running at full scale.", cohort_name))

  cat(sprintf("Running full-scale manual_ipf() for %s (%d melted rows, %d demo cells, %d mover cells)\n",
              cohort_name, nrow(li_long), nrow(margin_demo), nrow(margin_mover)))
  li_long[, w_raked := manual_ipf(li_long, "w_base", margins_spec, verbose = TRUE)]

  w_full_collapsed <- li_long[, .(w_full_joint_uncapped = sum(w_raked, na.rm = TRUE)), by = user_id]
  li <- merge(li, w_full_collapsed, by = "user_id", all.x = TRUE)

  med_w <- median(li$w_full_joint_uncapped, na.rm = TRUE)
  cap_hi <- med_w * 20
  cap_lo <- med_w * 0.05
  n_capped_hi <- sum(li$w_full_joint_uncapped > cap_hi, na.rm = TRUE)
  n_capped_lo <- sum(li$w_full_joint_uncapped < cap_lo, na.rm = TRUE)
  li[, w_full_joint := pmin(pmax(w_full_joint_uncapped, cap_lo), cap_hi)]
  cat(sprintf("Final weight cap [%.3f, %.3f] (0.05x-20x median %.3f): capped %d rows high, %d rows low, of %d (%.1f%% total capped)\n",
              cap_lo, cap_hi, med_w, n_capped_hi, n_capped_lo, nrow(li), 100 * (n_capped_hi + n_capped_lo) / nrow(li)))

  saveRDS(li, file.path(data_dir, sprintf("intermediate/column2_reweighted_%s.rds", cohort_name)))
  cat(sprintf("saved column2_reweighted_%s.rds: %d rows\n", cohort_name, nrow(li)))
}

log_step("SECTION 1 done.")

## ===========================================================================
## SECTION 2: un-calibrated calendar-year lines per cohort (migration rate +
## metro-tier share)
## ===========================================================================
log_step("SECTION 2: cohort calendar-year charts")

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

## ---- Per-cohort build ----
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

  ## Migration rate
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

  ## Metro tier share
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

log_step("SECTION 2 done.")

## ===========================================================================
## SECTION 3: scheme comparison (rank5 / rank3 / rank3_region) per cohort --
## Phase A (tier calibration) and Phase B (origin-destination flow
## calibration) vs. the static-reweighted baseline
## ===========================================================================
log_step("SECTION 3: cohort scheme comparison")

CALIB_YEARS <- unlist(list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023), use.names = FALSE)

## ---- ACS-side flow data for ONE PUMA vintage window -----------------------
## Same logic as the original single-vintage inline code, parameterized by
## which columns/crosswalk to use. Returns a list(dest_long, flow) where
## `flow` already carries origin_tier/dest_tier/calendar_year/w, ready to
## rbind across vintage windows.
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

  ## Both PUMA vintages' crosswalks, loaded unconditionally -- CALIB_YEARS
  ## always spans both windows.
  puma_tier_2010 <- readRDS(file.path(data_dir, sprintf("intermediate/puma_cbsa_tier_crosswalk_2010_%s.rds", scheme_name))); setDT(puma_tier_2010)
  migpuma_tier_2010 <- readRDS(file.path(data_dir, sprintf("intermediate/migpuma_cbsa_tier_crosswalk_2010_%s.rds", scheme_name))); setDT(migpuma_tier_2010)
  puma_tier_2020 <- readRDS(file.path(data_dir, sprintf("intermediate/puma_cbsa_tier_crosswalk_2020_%s.rds", scheme_name))); setDT(puma_tier_2020)
  migpuma_tier_2020 <- readRDS(file.path(data_dir, sprintf("intermediate/migpuma_cbsa_tier_crosswalk_2020_%s.rds", scheme_name))); setDT(migpuma_tier_2020)

  lookup <- build_cbsa_tier_lookup(scheme_name)
  code_to_tier_cbsa <- code_to_tier_for_scheme(lookup)
  region_lookup <- if (lookup$scheme$uses_region) state_fips_to_region() else NULL

  ## ---- ACS-side: tier share + origin-destination flow, by calendar year,
  ## combined across both PUMA vintage windows ----
  pums_1yr <- readRDS(pums1yr_path)
  setDT(pums_1yr)
  pums_1yr[, row_id := .I]  # global, stable across both vintage-window subsets below
  acs_2010 <- build_acs_flow_data(pums_1yr, list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023)[["2010"]], "PUMA10", "MIGPUMA10", puma_tier_2010, migpuma_tier_2010)
  acs_2020 <- build_acs_flow_data(pums_1yr, list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023)[["2020"]], "PUMA20", "MIGPUMA20", puma_tier_2020, migpuma_tier_2020)
  cat(sprintf("  ACS 2010-vintage window: %d flow rows; 2020-vintage window: %d flow rows\n",
              nrow(acs_2010$flow), nrow(acs_2020$flow)))

  acs_tier <- rbindlist(list(acs_2010$dest_long, acs_2020$dest_long))[, .(w = sum(w)), by = .(calendar_year, tier)]
  acs_tier[, acs_share := w / sum(w), by = calendar_year]

  acs_margin_b <- rbindlist(list(acs_2010$flow, acs_2020$flow))[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
  acs_margin_b[, acs_share := w / sum(w), by = calendar_year]

  ## ---- Revelio-side: load li, resolve col_end ----
  li <- readRDS(column2_path)
  setDT(li)
  resolve_col_end_run_scheme <- function(dt, is_factor_like) {
    if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
  }
  col_end_col2 <- resolve_col_end_run_scheme(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

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

  # ratio_a/ratio_b returned too, not just what they were used to build --
  # Section 4's demographic-crosstab needs to apply these SAME (year,
  # tier)/(year, origin_tier, dest_tier) ratios to a fixed-calendar-year
  # cross-section, and re-deriving them a third time would just be another
  # chance to reintroduce a bug already caught and fixed twice in this
  # exact logic. Cheap byproducts already sitting in memory at this point.
  list(gap_summary = gap_summary, mig_lines = mig_lines, tier_lines = tier_lines, coverage = coverage,
       ratio_a = ratio_a[, .(calendar_year, tier, ratio)],
       ratio_b = ratio_b[, .(calendar_year, origin_tier, dest_tier, ratio)])
}

## ---- Driver: 2 cohorts x 3 schemes ----
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
cat("\n\nSECTION 3 comparison runs done.\n")

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

  # Phase A/B ratio tables, per scheme -- needed by Section 4 to apply the
  # same calibration to a fixed-calendar-year demographic cross-section.
  for (scheme_name in SCHEMES) {
    saveRDS(list(ratio_a = cohort_results[[scheme_name]]$ratio_a, ratio_b = cohort_results[[scheme_name]]$ratio_b),
            file.path(data_dir, sprintf("intermediate/phase_ratios_%s_%s.rds", cohort_name, scheme_name)))
  }

  cat(sprintf("\nWrote all export files for %s\n", cohort_name))
}

log_step("SECTION 3 done.")

## ===========================================================================
## SECTION 4: demographic (race/sex/region) cross-tab per cohort, fixed
## calendar year 2015, across every weighting scheme in the cohort charts
## ===========================================================================
log_step("SECTION 4: cohort demographic cross-tab")

FIXED_YEAR <- 2015
COHORTS_RNG <- list(born_1980s = c(1980, 1989), born_1990s = c(1990, 1999))
SCHEMES_FOR_TABLE <- c("rank5", "rank3_region")  # matches the lines actually plotted in chart 1

region_for_state_abbr <- setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
region_fips_lookup <- state_fips_to_region()
region_for_fips <- setNames(region_fips_lookup$census_region, region_fips_lookup$state_fips)

lookups <- setNames(lapply(SCHEMES_FOR_TABLE, build_cbsa_tier_lookup), SCHEMES_FOR_TABLE)
code_to_tier_fns <- setNames(lapply(lookups, code_to_tier_for_scheme), SCHEMES_FOR_TABLE)
tier_from_code_for <- function(scheme, code_vals, state_vals) {
  lookup <- lookups[[scheme]]
  if (lookup$scheme$uses_region) {
    code_to_tier_fns[[scheme]](code_vals, region = unname(region_for_state_abbr[state_vals]))
  } else {
    code_to_tier_fns[[scheme]](code_vals)
  }
}

log_step("Loading ACS 2015 race/sex supplement")
acs_race2015 <- readRDS(file.path(data_dir, "intermediate/pums_1yr_race2015.rds"))
setDT(acs_race2015)
acs_race2015[, `:=`(SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER))]

## ---- Per-cohort t-slice extractor: for each row of `dt`, find the ONE t
## in [0, T_MAX] where col_end+t == FIXED_YEAR (at most one match per
## person, since t is a strictly increasing function of calendar year for
## fixed col_end), and return that row's current AND prior-year
## cbsa_code/state. ----
extract_fixed_year <- function(dt, col_end_vec, t_max = T_MAX) {
  parts <- vector("list", t_max + 1)
  for (t in 0:t_max) {
    code_col <- paste0("cbsa_code_", t); state_col <- paste0("cbsa_state_", t)
    if (!code_col %in% names(dt)) next
    cy <- col_end_vec + t
    idx <- which(cy == FIXED_YEAR)
    if (length(idx) == 0) next
    prev_code_col <- paste0("cbsa_code_", t - 1); prev_state_col <- paste0("cbsa_state_", t - 1)
    has_prev <- t >= 1 && prev_code_col %in% names(dt)
    parts[[t + 1]] <- data.table(
      row_idx = idx,
      cbsa_code = dt[[code_col]][idx], cbsa_state = dt[[state_col]][idx],
      cbsa_code_prev = if (has_prev) dt[[prev_code_col]][idx] else NA_character_,
      cbsa_state_prev = if (has_prev) dt[[prev_state_col]][idx] else NA_character_
    )
  }
  rbindlist(parts)
}

all_out <- vector("list", length(COHORTS_RNG))
names(all_out) <- names(COHORTS_RNG)

for (cohort_name in names(COHORTS_RNG)) {
  rng <- COHORTS_RNG[[cohort_name]]
  cat(sprintf("\n\n========== COHORT: %s ==========\n", cohort_name))

  log_step("Loading cohort files")
  li <- readRDS(file.path(data_dir, sprintf("intermediate/column2_reweighted_%s.rds", cohort_name)))
  setDT(li)
  col1 <- readRDS(file.path(data_dir, sprintf("intermediate/column1_covariates_%s.rds", cohort_name)))
  setDT(col1)
  pums_1yr <- readRDS(file.path(data_dir, sprintf("intermediate/pums_1yr_filt_%s.rds", cohort_name)))
  setDT(pums_1yr)

  ## Re-derive true post-IPF race shares (see Section 4's header point 1)
  log_step("Re-deriving race-cell-level post-IPF weights")
  pums_acs5_all_s4 <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds"))
  setDT(pums_acs5_all_s4)
  pums_acs5_all_s4[, birth_approx := PUMS_YEAR - as.integer(AGEP)]
  pums_filt <- pums_acs5_all_s4[birth_approx >= rng[1] & birth_approx <= rng[2]]

  li_complete <- li[!is.na(origin_state) & !is.na(age_bucket) & !is.na(white_prob)]
  li_long <- melt(
    li_complete, id.vars = setdiff(names(li_complete), RACE_PROB_COLS),
    measure.vars = RACE_PROB_COLS, variable.name = "race_prob_col", value.name = "race_frac"
  )
  race_col_to_label <- setNames(RACE_LABELS, RACE_PROB_COLS)
  li_long[, race := race_col_to_label[as.character(race_prob_col)]]
  li_long[, sex := fifelse(m_prob >= f_prob, "male", "female")]
  li_long[, w_base := w_unweighted * race_frac]
  li_long <- li_long[!is.na(moved_last_year_state)]

  cell_state_age_race_sex <- pums_filt[, .(pop = sum(PWGTP)), by = .(origin_state, age_bucket, race, sex)]
  margin_demo <- copy(cell_state_age_race_sex)
  setnames(margin_demo, "pop", "Freq")
  for (col in c("origin_state", "age_bucket", "race", "sex")) margin_demo[[col]] <- as.character(margin_demo[[col]])
  li_long[, `:=`(origin_state = as.character(origin_state), age_bucket = as.character(age_bucket),
                  race = as.character(race), sex = as.character(sex),
                  moved_last_year_state = as.character(moved_last_year_state))]

  margin_mover <- pums_filt[, .(Freq = sum(PWGTP)), by = .(origin_state, moved_out_of_state)]
  setnames(margin_mover, "moved_out_of_state", "moved_last_year_state")
  margin_mover[, `:=`(origin_state = as.character(origin_state), moved_last_year_state = as.character(moved_last_year_state))]

  demo_key_cols <- c("origin_state", "age_bucket", "race", "sex")
  li_long <- li_long[margin_demo[, ..demo_key_cols], on = demo_key_cols, nomatch = 0]
  li_long_keys <- unique(li_long[, ..demo_key_cols])
  margin_demo <- margin_demo[li_long_keys, on = demo_key_cols, nomatch = 0]

  mover_key_cols <- c("origin_state", "moved_last_year_state")
  li_long <- li_long[margin_mover[, ..mover_key_cols], on = mover_key_cols, nomatch = 0]
  margin_mover <- margin_mover[unique(li_long[, ..mover_key_cols]), on = mover_key_cols, nomatch = 0]

  margins_spec <- list(list(keys = demo_key_cols, pop = margin_demo), list(keys = mover_key_cols, pop = margin_mover))
  li_long[, w_raked := manual_ipf(li_long, "w_base", margins_spec, verbose = TRUE)]

  race_share <- li_long[, .(w_raked = sum(w_raked)), by = .(user_id, race)]
  race_share[, race_share := w_raked / sum(w_raked), by = user_id]
  race_share_wide <- dcast(race_share, user_id ~ race, value.var = "race_share", fill = 0)
  stopifnot(all(RACE_LABELS %in% names(race_share_wide)))

  li <- merge(li, race_share_wide, by = "user_id", all.x = TRUE)
  li[, sex_hard := fifelse(m_prob >= f_prob, "male", "female")]
  rm(li_long, li_complete); gc()

  ## Phase A/B ratio tables
  ratios <- setNames(lapply(SCHEMES_FOR_TABLE, function(s)
    readRDS(file.path(data_dir, sprintf("intermediate/phase_ratios_%s_%s.rds", cohort_name, s)))), SCHEMES_FOR_TABLE)

  ## col_end
  col_end_col2 <- as.integer(as.character(li$col_end))
  col_end_col1 <- as.integer(col1$col_end)

  ## FIXED_YEAR cross-section extraction
  log_step(sprintf("Extracting calendar year %d cross-section", FIXED_YEAR))
  slice2 <- extract_fixed_year(li, col_end_col2)
  slice2[, `:=`(
    user_id = li$user_id[row_idx], w_unweighted = li$w_unweighted[row_idx], w_full_joint = li$w_full_joint[row_idx],
    sex = li$sex_hard[row_idx], region = unname(region_for_state_abbr[cbsa_state])
  )]
  for (r in RACE_LABELS) slice2[[r]] <- li[[r]][slice2$row_idx]
  for (scheme in SCHEMES_FOR_TABLE) {
    slice2[[paste0("tier_", scheme)]] <- tier_from_code_for(scheme, slice2$cbsa_code, slice2$cbsa_state)
    slice2[[paste0("origin_tier_", scheme)]] <- tier_from_code_for(scheme, slice2$cbsa_code_prev, slice2$cbsa_state_prev)
  }

  slice1 <- extract_fixed_year(col1, col_end_col1)
  slice1[, `:=`(
    sex = fifelse(col1$m_prob[row_idx] >= col1$f_prob[row_idx], "male", "female"),
    region = unname(region_for_state_abbr[cbsa_state])
  )]
  for (rp in RACE_PROB_COLS) {
    lbl <- race_col_to_label[[rp]]
    slice1[[lbl]] <- col1[[rp]][slice1$row_idx]
  }

  acs_year <- pums_1yr[survey_year == FIXED_YEAR]
  acs_year[, `:=`(SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER))]
  acs_year <- merge(acs_year, acs_race2015, by = c("SERIALNO", "SPORDER"), all.x = TRUE)
  acs_year[, region := unname(region_for_fips[ST])]
  cat(sprintf("ACS %d race/sex match rate onto the supplementary pull: %.1f%%\n",
              FIXED_YEAR, 100 * mean(!is.na(acs_year$race))))

  ## Weighted share helper
  weighted_share <- function(weight, category) {
    d <- data.table(weight = weight, category = category)
    d <- d[!is.na(category) & !is.na(weight) & weight > 0]
    agg <- d[, .(w = sum(weight)), by = category]
    agg[, share := w / sum(w)]
    agg[order(-share)]
  }
  weighted_share_race <- function(weight, race_share_cols) {
    # race_share_cols: named list of per-row fractional shares (sums to 1 across categories per row)
    tot <- sapply(RACE_LABELS, function(r) sum(weight * race_share_cols[[r]], na.rm = TRUE))
    tot <- tot[is.finite(tot)]
    data.table(category = names(tot), share = tot / sum(tot))[order(-share)]
  }

  rows <- list()

  add_source <- function(source_key, race_dt, sex_dt, region_dt) {
    race_dt[, `:=`(source = source_key, category_type = "race")]
    sex_dt[, `:=`(source = source_key, category_type = "sex")]
    region_dt[, `:=`(source = source_key, category_type = "region")]
    rows[[length(rows) + 1]] <<- rbindlist(list(race_dt[, .(source, category_type, category, share)],
                                                  sex_dt[, .(source, category_type, category, share)],
                                                  region_dt[, .(source, category_type, category, share)]))
  }

  ## col1 (unweighted, race_prob used directly -- Column 1 was never raked)
  add_source("col1",
             weighted_share_race(rep(1, nrow(slice1)), setNames(lapply(RACE_LABELS, function(r) slice1[[r]]), RACE_LABELS)),
             weighted_share(rep(1, nrow(slice1)), slice1$sex),
             weighted_share(rep(1, nrow(slice1)), slice1$region))

  ## col2u (unweighted, race_prob used directly)
  add_source("col2u",
             weighted_share_race(rep(1, nrow(slice2)), setNames(lapply(RACE_LABELS, function(r) li[[r]][slice2$row_idx]), RACE_LABELS)),
             weighted_share(rep(1, nrow(slice2)), slice2$sex),
             weighted_share(rep(1, nrow(slice2)), slice2$region))

  ## col2r (static reweighted, TRUE post-IPF race split)
  add_source("col2r",
             weighted_share_race(slice2$w_full_joint, setNames(lapply(RACE_LABELS, function(r) slice2[[r]]), RACE_LABELS)),
             weighted_share(slice2$w_full_joint, slice2$sex),
             weighted_share(slice2$w_full_joint, slice2$region))

  ## Phase A/B, per scheme
  for (scheme in SCHEMES_FOR_TABLE) {
    ra <- ratios[[scheme]]$ratio_a[calendar_year == FIXED_YEAR]
    rb <- ratios[[scheme]]$ratio_b[calendar_year == FIXED_YEAR]

    tier_col <- paste0("tier_", scheme); origin_tier_col <- paste0("origin_tier_", scheme)
    m_a <- merge(data.table(tier = slice2[[tier_col]]), ra[, .(tier, ratio)], by = "tier", all.x = TRUE, sort = FALSE)
    ratio_a_vec <- fifelse(is.na(m_a$ratio), 1, m_a$ratio)
    w_phasea <- slice2$w_full_joint * ratio_a_vec

    m_b <- merge(data.table(origin_tier = slice2[[origin_tier_col]], dest_tier = slice2[[tier_col]]),
                 rb[, .(origin_tier, dest_tier, ratio)], by = c("origin_tier", "dest_tier"), all.x = TRUE, sort = FALSE)
    ratio_b_vec <- fifelse(is.na(m_b$ratio), 1, m_b$ratio)
    # Phase B undefined for rows with no prior-year tier (t=0) -- exclude, matching every other Phase B loop in this project
    w_phaseb <- fifelse(is.na(slice2[[origin_tier_col]]), NA_real_, slice2$w_full_joint * ratio_b_vec)

    a_key <- if (scheme == "rank5") "phasea" else paste0("phasea_", scheme)
    b_key <- if (scheme == "rank5") "phaseb" else paste0("phaseb_", scheme)
    if (scheme == "rank3_region") { a_key <- "phasea_region"; b_key <- "phaseb_region" }

    add_source(a_key,
               weighted_share_race(w_phasea, setNames(lapply(RACE_LABELS, function(r) slice2[[r]]), RACE_LABELS)),
               weighted_share(w_phasea, slice2$sex),
               weighted_share(w_phasea, slice2$region))
    add_source(b_key,
               weighted_share_race(w_phaseb, setNames(lapply(RACE_LABELS, function(r) slice2[[r]]), RACE_LABELS)),
               weighted_share(w_phaseb, slice2$sex),
               weighted_share(w_phaseb, slice2$region))
  }

  ## ACS benchmark
  add_source("acs",
             weighted_share(acs_year$PWGTP, acs_year$race),
             weighted_share(acs_year$PWGTP, acs_year$sex),
             weighted_share(acs_year$PWGTP, acs_year$region))

  out <- rbindlist(rows)
  out[, cohort := cohort_name]
  all_out[[cohort_name]] <- out

  cat(sprintf("\n%s cross-section sizes: Column 1 n=%d, Column 2 n=%d (of which %d have a defined Phase B origin), ACS n=%d\n",
              cohort_name, nrow(slice1), nrow(slice2), sum(!is.na(slice2$origin_tier_rank5)), nrow(acs_year)))

  fwrite(out, file.path(data_dir, sprintf("results/memo1_demo_crosstab_%s.csv", cohort_name)))
  cat(sprintf("\n=== %s demographic cross-tab, %d (wide) ===\n", cohort_name, FIXED_YEAR))
  print(dcast(out, category_type + category ~ source, value.var = "share"))
}

log_step("SECTION 4 done.")

## ===========================================================================
## SECTION 5: cohort-restricted geography+occupation Stage-2 weight
## (w2_occ analog) and its migration-rate line
## ===========================================================================
log_step("SECTION 5: cohort geo+occupation reweight")

LBL_NEW_S5 <- "BA + HS on LI (reweighted, geo+occupation)"

SOC_MAJOR_GROUPS <- c(
  "11" = "Management", "13" = "Business and Financial Operations", "15" = "Computer and Mathematical",
  "17" = "Architecture and Engineering", "19" = "Life, Physical, and Social Science",
  "21" = "Community and Social Service", "23" = "Legal", "25" = "Educational Instruction and Library",
  "27" = "Arts, Design, Entertainment, Sports, and Media", "29" = "Healthcare Practitioners and Technical",
  "31" = "Healthcare Support", "33" = "Protective Service", "35" = "Food Preparation and Serving",
  "37" = "Building and Grounds Cleaning and Maintenance", "39" = "Personal Care and Service",
  "41" = "Sales and Related", "43" = "Office and Administrative Support",
  "45" = "Farming, Fishing, and Forestry", "47" = "Construction and Extraction",
  "49" = "Installation, Maintenance, and Repair", "51" = "Production",
  "53" = "Transportation and Material Moving", "55" = "Military Specific"
)
soc_prefix_to_major <- function(soc_short) unname(SOC_MAJOR_GROUPS[substr(soc_short, 1, 2)])
occ_vintage_for_year <- function(y) fifelse(y <= 2017, "2010", "2018")

# NOTE: this section's own build_acs_flow_data()/resolve_col_end() are
# deliberately named with an `_s5` suffix, NOT reusing Section 3's
# identically-named functions above -- their bodies genuinely differ
# (Section 3's build_acs_flow_data() returns list(dest_long, flow); this
# one returns just the flow rbindlist; Section 3's resolve_col_end() takes
# an is_factor_like flag and range-validates, this one auto-detects with no
# validation) and reusing the same name would let the LATER definition
# (this one, since it runs after Section 3 in file order) silently shadow
# the earlier one in this script's shared global environment.
build_acs_flow_data_s5 <- function(pums_1yr, years, puma_col, migpuma_col, puma_tier, migpuma_tier) {
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
  rbindlist(list(
    origin_movers[, .(calendar_year, origin_tier, dest_tier, w = PWGTP * dest_share * origin_share)],
    nm[, .(calendar_year, origin_tier, dest_tier, w = PWGTP * dest_share * origin_share)]
  ))
}

region_for_state_abbr_s5 <- setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
lookup_rank3_region_s5 <- build_cbsa_tier_lookup("rank3_region")
code_to_tier_region_s5 <- code_to_tier_for_scheme(lookup_rank3_region_s5)
tier_from_code_region_s5 <- function(code_vals, state_vals) code_to_tier_region_s5(code_vals, region = unname(region_for_state_abbr_s5[state_vals]))

resolve_col_end_s5 <- function(dt) {
  if (is.factor(dt$col_end) || is.character(dt$col_end)) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
}

## ---- Shared (not per-cohort) loads: occupation supplement + crosswalks ----
log_step("Loading shared OCCP supplement + crosswalks")
occp_all <- readRDS(file.path(data_dir, "intermediate/pums_1yr_occp_allyears.rds")); setDT(occp_all)
occp_all[, occp_padded := sprintf("%04d", suppressWarnings(as.integer(OCCP)))]
xwalk_2010 <- readRDS(file.path(data_dir, "raw/bls/census_occp_2010_to_soc_major_group.rds")); setDT(xwalk_2010)
xwalk_2018 <- readRDS(file.path(data_dir, "raw/bls/census_occp_2018_to_soc_major_group.rds")); setDT(xwalk_2018)
occp_to_major_2010 <- setNames(unname(SOC_MAJOR_GROUPS[xwalk_2010$major_prefix]), xwalk_2010$census_2010)
occp_to_major_2018 <- setNames(unname(SOC_MAJOR_GROUPS[xwalk_2018$major_prefix]), xwalk_2018$census_2018)

puma_tier_region_2010 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2010_rank3_region.rds")); setDT(puma_tier_region_2010)
puma_tier_region_2020 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2020_rank3_region.rds")); setDT(puma_tier_region_2020)
migpuma_tier_region_2010 <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2010_rank3_region.rds")); setDT(migpuma_tier_region_2010)
migpuma_tier_region_2020 <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2020_rank3_region.rds")); setDT(migpuma_tier_region_2020)

## ---- Per-cohort calibration ----
run_cohort_s5 <- function(cohort_name) {
  log_step(sprintf("=== Cohort %s ===", cohort_name))
  li <- readRDS(file.path(data_dir, sprintf("intermediate/column2_reweighted_%s.rds", cohort_name))); setDT(li)
  pums_1yr <- readRDS(file.path(data_dir, sprintf("intermediate/pums_1yr_filt_%s.rds", cohort_name))); setDT(pums_1yr)
  col_end_col2 <- resolve_col_end_s5(li)
  pums_1yr[, row_id := .I]
  pums_1yr <- pums_1yr[survey_year %in% CALIB_YEARS]

  ## ACS geography flow margin
  flow_2010 <- build_acs_flow_data_s5(pums_1yr, setdiff(2012:2021, 2020), "PUMA10", "MIGPUMA10", puma_tier_region_2010, migpuma_tier_region_2010)
  flow_2020 <- build_acs_flow_data_s5(pums_1yr, 2022:2023, "PUMA20", "MIGPUMA20", puma_tier_region_2020, migpuma_tier_region_2020)
  acs_geo <- rbindlist(list(flow_2010, flow_2020))[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
  acs_geo[, acs_share := w / sum(w), by = calendar_year]

  ## ACS occupation margin, cohort-restricted via the join itself
  acs_occ_src <- merge(pums_1yr[, .(row_id, survey_year, SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER), PWGTP)],
                        occp_all[, .(survey_year, SERIALNO, SPORDER, occp_padded)],
                        by = c("survey_year", "SERIALNO", "SPORDER"), all.x = TRUE)
  acs_occ_src[, vintage := occ_vintage_for_year(survey_year)]
  acs_occ_src[vintage == "2010", major_group := unname(occp_to_major_2010[occp_padded])]
  acs_occ_src[vintage == "2018", major_group := unname(occp_to_major_2018[occp_padded])]
  cat(sprintf("  %s: ACS OCCP match rate %.1f%%, resolve rate %.1f%%\n", cohort_name,
              100 * mean(!is.na(acs_occ_src$occp_padded)), 100 * mean(!is.na(acs_occ_src$major_group[!is.na(acs_occ_src$occp_padded)]))))
  acs_occ <- acs_occ_src[!is.na(major_group), .(w = sum(PWGTP)), by = .(calendar_year = survey_year, major_group)]
  acs_occ[, acs_share := w / sum(w), by = calendar_year]

  ## Revelio person-year panel: geo tier + occupation + RAW state (for the
  ## state-crossing migration-rate metric afterward)
  rev_partials <- vector("list", T_MAX)
  for (t in 1:T_MAX) {
    cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
    cur_state_col <- paste0("cbsa_state_", t); prev_state_col <- paste0("cbsa_state_", t - 1)
    soc_col <- paste0("soc_code_", t)
    if (!all(c(cur_col, prev_col, soc_col) %in% names(li))) next
    dest_tier <- tier_from_code_region_s5(li[[cur_col]], li[[cur_state_col]])
    origin_tier <- tier_from_code_region_s5(li[[prev_col]], li[[prev_state_col]])
    major_group <- soc_prefix_to_major(substr(li[[soc_col]], 1, 7))
    cy <- col_end_col2 + t
    valid <- !is.na(dest_tier) & !is.na(origin_tier) & !is.na(major_group) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
    if (sum(valid) == 0) next
    rev_partials[[t]] <- data.table(
      user_id = li$user_id[valid], calendar_year = cy[valid],
      origin_tier = origin_tier[valid], dest_tier = dest_tier[valid],
      major_group = major_group[valid], w_full_joint = li$w_full_joint[valid],
      cbsa_state_cur = li[[cur_state_col]][valid], cbsa_state_prev = li[[prev_state_col]][valid]
    )
  }
  rev_panel <- rbindlist(rev_partials)
  cat(sprintf("  %s: Revelio panel %d rows, %d users, %d years\n", cohort_name, nrow(rev_panel), uniqueN(rev_panel$user_id), uniqueN(rev_panel$calendar_year)))

  ## per-calendar-year 2-margin IPF
  out_parts <- vector("list", length(CALIB_YEARS))
  for (i in seq_along(CALIB_YEARS)) {
    y <- CALIB_YEARS[i]
    rows_y <- rev_panel[calendar_year == y]
    if (nrow(rows_y) == 0) next
    total_w_y <- sum(rows_y$w_full_joint, na.rm = TRUE)
    geo_y <- acs_geo[calendar_year == y, .(origin_tier, dest_tier, Freq = acs_share * total_w_y)]
    occ_y <- acs_occ[calendar_year == y, .(major_group, Freq = acs_share * total_w_y)]
    if (nrow(geo_y) == 0 || nrow(occ_y) == 0) { cat(sprintf("  %s year %d: no ACS margin coverage -- skipping\n", cohort_name, y)); next }
    w2_occ <- manual_ipf(dt = rows_y, w_col = "w_full_joint",
                          margins = list(list(keys = c("origin_tier", "dest_tier"), pop = geo_y),
                                          list(keys = "major_group", pop = occ_y)))
    out_parts[[i]] <- data.table(user_id = rows_y$user_id, calendar_year = y, w2_occ = w2_occ)
  }
  w2_occ_panel <- rbindlist(out_parts)

  ## migration rate (state-crossing) under w2_occ
  mig_panel <- merge(rev_panel[, .(user_id, calendar_year, cbsa_state_cur, cbsa_state_prev)], w2_occ_panel, by = c("user_id", "calendar_year"))
  mig_panel <- mig_panel[!is.na(cbsa_state_cur) & !is.na(cbsa_state_prev)]
  mig_panel[, moved := as.numeric(cbsa_state_cur != cbsa_state_prev)]
  rate_dt <- mig_panel[, .(rate = weighted.mean(moved, w2_occ), n = .N), by = calendar_year]
  rate_dt[, source := LBL_NEW_S5]
  setorder(rate_dt, calendar_year)

  saveRDS(w2_occ_panel, file.path(data_dir, sprintf("results/memo1_w2_occupation_calibrated_by_year_%s.rds", cohort_name)))
  out_path <- file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_geo_occ_%s.csv", cohort_name))
  fwrite(rate_dt[, .(source, calendar_year, rate)], out_path)
  log_step(sprintf("Wrote %s (%d rows)", out_path, nrow(rate_dt)))
  print(rate_dt)
  invisible(rate_dt)
}

run_cohort_s5("born_1980s")
run_cohort_s5("born_1990s")

log_step("memo1_09_cohort_variants.R done.")
