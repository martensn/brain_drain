# memo1_10a_full_sample_extras.R (was memo1_08_full_sample_extras.R)
#
# [NEW 2026-08-21] Per Nicholas's request: two full-sample additions to
# MEMO1_WEIGHTING.md, mirroring what the born-cohort artifacts already show
# but stripped to the four lines that matter (per his explicit instruction,
# same as memo1_06a's "four lines that matter" framing already used in
# MEMO1_WEIGHTING.md's SS6.2):
#
#   1. Metro-tier share by calendar year, under the memo's own 3-tier
#      (size-only: Top 10 / Top 11-50 / Everything else) definition --
#      SS5.1 defines "metro tier" as exactly this 3-way size split before
#      region is crossed in for the flow-calibration TARGET, so this is
#      the natural, already-defined tier vocabulary for a share-over-time
#      chart (not the old 5-tier scheme the cohort artifacts used, and not
#      the 12-cell size x region scheme, which has no shared axis to plot
#      shares on).
#   2. A demographic (race/sex/region) cross-tab at calendar year 2015,
#      mirroring memo1_09d_cohort_demo_table.R but for the FULL sample and
#      only the four kept lines.
#
# Both use exactly ONE "reweighted" series: w_full_joint (Stage 1) x the
# rank3_region (size x region) Phase B flow-calibration ratio -- confirmed
# via memo1_scheme_gap_summary.csv and MEMO1_WEIGHTING.md SS5.1/SS8 as the
# settled, kept scheme (best gap on both migration rate AND tier share).
# For the tier-share chart, the region-crossed Phase B weight is computed
# at its native 12-cell (3 size x 4 region) resolution -- that's the actual
# production calibration -- then COLLAPSED to the 3 size-only categories by
# stripping the " (Region)" suffix and re-summing, purely for display; the
# underlying weight is untouched.
#
# [EXTENDED 2026-08-21, per Nicholas's request] The scope limit noted here
# originally ("full-sample scheme comparison never extended to 2022-2023")
# is now resolved -- Code/memo1_08d_scheme_comparison.R was made
# vintage-aware the same day, matching memo1_09c_cohort_scheme_comparison.R's
# approach exactly (2012-2021 on 2010-vintage PUMA/MIGPUMA, 2022-2023 on
# 2020-vintage). This script's own Part A (the only part with any ACS-side
# PUMA dependency -- Part B's FIXED_YEAR=2015 sits entirely inside the
# 2010-vintage window either way, so its numbers are unaffected by this
# extension) is updated to match, via the same build_acs_flow_data() helper
# copied from memo1_06b/07c.
#
# Requires: column2_reweighted.rds, column1_covariates.rds,
# pums_acs5_filt.rds, pums_1yr_filt.rds, pums_1yr_race2015.rds, and the
# rank3 / rank3_region crosswalks already built (memo1_03a/03b), BOTH
# PUMA vintages (2010 and 2020).

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

# ---- ACS-side flow data for ONE PUMA vintage window -- copied from
# memo1_08d_scheme_comparison.R / memo1_07c (same already-verified logic).
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
FIXED_YEAR <- 2015
RATIO_CAP_LO <- 0.05
RATIO_CAP_HI <- 20
RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob", "native_prob", "multiple_prob", "hispanic_prob")

# Final labels, applied directly at export (per Nicholas's instruction --
# no separate relabeling pass needed downstream).
LBL_COL1  <- "BA Only"
LBL_COL2U <- "BA + HS on LI"
LBL_COL2R <- "BA + HS on LI (reweighted)"
LBL_ACS   <- "ACS PUMS"

strip_region <- function(tier) sub(" \\(.*\\)$", "", tier)

region_for_state_abbr <- setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
region_fips_lookup <- state_fips_to_region()
region_for_fips <- setNames(region_fips_lookup$census_region, region_fips_lookup$state_fips)

lookup_rank3        <- build_cbsa_tier_lookup("rank3")
lookup_rank3_region  <- build_cbsa_tier_lookup("rank3_region")
code_to_tier_rank3  <- code_to_tier_for_scheme(lookup_rank3)
code_to_tier_region <- code_to_tier_for_scheme(lookup_rank3_region)

tier_from_code_region <- function(code_vals, state_vals) {
  code_to_tier_region(code_vals, region = unname(region_for_state_abbr[state_vals]))
}

resolve_col_end <- function(dt, is_factor_like) {
  raw <- if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
  raw
}

## =========================================================================
## manual_ipf() -- [2026-08-23] centralized to Code/memo1_ipf.R, was an
## independent copy here (and in 5+ other memo1_* scripts).
## =========================================================================
source(here::here("Code/memo1_ipf.R"))

## =========================================================================
## LOAD
## =========================================================================
log_step("Loading Column 1/2, ACS 5yr/1yr, race-2015 supplement")
col1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds")); setDT(col1)
li   <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds")); setDT(li)
pums_acs5 <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds")); setDT(pums_acs5)
pums_1yr  <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds"));  setDT(pums_1yr)
acs_race2015 <- readRDS(file.path(data_dir, "intermediate/pums_1yr_race2015.rds")); setDT(acs_race2015)
acs_race2015[, `:=`(SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER))]

col_end_col1 <- resolve_col_end(col1, is_factor_like = FALSE)
col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

## =========================================================================
## PART A: metro-tier share by calendar year, 3-tier (size-only), 4 lines
## =========================================================================
log_step("PART A: metro-tier share (3-tier, size-only)")

## ---- Column 1 / Column 2 unweighted, under rank3 (size-only) ----
revelio_tier_share <- function(dt, weight_col, col_end_numeric, source_label) {
  partials <- vector("list", T_MAX + 1)
  for (t in 0:T_MAX) {
    col <- paste0("cbsa_code_", t)
    if (!col %in% names(dt)) next
    tier <- code_to_tier_rank3(dt[[col]])
    w <- if (is.null(weight_col)) rep(1, nrow(dt)) else dt[[weight_col]]
    valid <- !is.na(tier) & !is.na(w) & !is.na(col_end_numeric)
    if (sum(valid) == 0) next
    cy <- col_end_numeric[valid] + t
    partials[[t + 1]] <- data.table(calendar_year = cy, tier = tier[valid], w = w[valid])[
      , .(w = sum(w)), by = .(calendar_year, tier)]
  }
  agg <- rbindlist(partials)[, .(w = sum(w)), by = .(calendar_year, tier)]
  agg[, share := w / sum(w), by = calendar_year]
  agg[, .(source = source_label, calendar_year, tier, share)]
}
tier_col1  <- revelio_tier_share(col1, NULL, col_end_col1, LBL_COL1)
tier_col2u <- revelio_tier_share(li, "w_unweighted", col_end_col2, LBL_COL2U)

## ---- ACS, under rank3 (size-only), both PUMA vintage windows ----
pums_1yr[, row_id := .I]  # global, stable across both vintage-window subsets below
puma_tier_rank3_2010 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2010_rank3.rds")); setDT(puma_tier_rank3_2010)
puma_tier_rank3_2020 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2020_rank3.rds")); setDT(puma_tier_rank3_2020)
migpuma_tier_rank3_2010 <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2010_rank3.rds")); setDT(migpuma_tier_rank3_2010)
migpuma_tier_rank3_2020 <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2020_rank3.rds")); setDT(migpuma_tier_rank3_2020)
rank3_2010 <- build_acs_flow_data(pums_1yr, VINTAGE_WINDOWS[["2010"]], "PUMA10", "MIGPUMA10", puma_tier_rank3_2010, migpuma_tier_rank3_2010)
rank3_2020 <- build_acs_flow_data(pums_1yr, VINTAGE_WINDOWS[["2020"]], "PUMA20", "MIGPUMA20", puma_tier_rank3_2020, migpuma_tier_rank3_2020)
acs_tier <- rbindlist(list(rank3_2010$dest_long, rank3_2020$dest_long))[, .(w = sum(w)), by = .(calendar_year, tier)]
acs_tier[, share := w / sum(w), by = calendar_year]
tier_acs <- acs_tier[, .(source = LBL_ACS, calendar_year, tier, share)]

## ---- "Reweighted" line: rank3_region Phase B, collapsed to size-only ----
log_step("PART A: rank3_region Phase B flow calibration (production scheme)")
puma_tier_region_2010 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2010_rank3_region.rds")); setDT(puma_tier_region_2010)
puma_tier_region_2020 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2020_rank3_region.rds")); setDT(puma_tier_region_2020)
migpuma_tier_region_2010 <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2010_rank3_region.rds")); setDT(migpuma_tier_region_2010)
migpuma_tier_region_2020 <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2020_rank3_region.rds")); setDT(migpuma_tier_region_2020)
region_2010 <- build_acs_flow_data(pums_1yr, VINTAGE_WINDOWS[["2010"]], "PUMA10", "MIGPUMA10", puma_tier_region_2010, migpuma_tier_region_2010)
region_2020 <- build_acs_flow_data(pums_1yr, VINTAGE_WINDOWS[["2020"]], "PUMA20", "MIGPUMA20", puma_tier_region_2020, migpuma_tier_region_2020)
acs_margin_b <- rbindlist(list(region_2010$flow, region_2020$flow))[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
acs_margin_b[, acs_share := w / sum(w), by = calendar_year]

rev_partials <- vector("list", T_MAX)
for (t in 1:T_MAX) {
  cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
  cur_state_col <- paste0("cbsa_state_", t); prev_state_col <- paste0("cbsa_state_", t - 1)
  if (!all(c(cur_col, prev_col) %in% names(li))) next
  dest_tier <- tier_from_code_region(li[[cur_col]], li[[cur_state_col]])
  origin_tier <- tier_from_code_region(li[[prev_col]], li[[prev_state_col]])
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
cat(sprintf("Phase B (rank3_region, full sample) ratio table: %d of %d possible cells covered (%.1f%%), %d capped\n",
            nrow(ratio_b), n_possible_cells, 100 * nrow(ratio_b) / n_possible_cells, n_capped_b))

tier_b_partials <- vector("list", T_MAX + 1)
for (t in 0:T_MAX) {
  cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
  cur_state_col <- paste0("cbsa_state_", t); prev_state_col <- paste0("cbsa_state_", t - 1)
  if (!cur_col %in% names(li)) next
  dest_tier <- tier_from_code_region(li[[cur_col]], li[[cur_state_col]])
  origin_tier <- if (prev_col %in% names(li)) tier_from_code_region(li[[prev_col]], li[[prev_state_col]]) else rep(NA_character_, nrow(li))
  cy <- col_end_col2 + t
  valid <- !is.na(dest_tier) & !is.na(origin_tier) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
  if (sum(valid) == 0) next
  d <- data.table(calendar_year = cy[valid], origin_tier = origin_tier[valid], dest_tier = dest_tier[valid], w_base = li$w_full_joint[valid])
  d <- merge(d, ratio_b[, .(calendar_year, origin_tier, dest_tier, ratio)], by = c("calendar_year", "origin_tier", "dest_tier"), all.x = TRUE)
  d[, ratio := fifelse(is.na(ratio), 1, ratio)]
  tier_b_partials[[t + 1]] <- d[, .(w = sum(w_base * ratio)), by = .(calendar_year, dest_tier)]
}
tier_b <- rbindlist(tier_b_partials)[, .(w = sum(w)), by = .(calendar_year, dest_tier)]
tier_b[, tier := strip_region(dest_tier)]
tier_b <- tier_b[, .(w = sum(w)), by = .(calendar_year, tier)]
tier_b[, share := w / sum(w), by = calendar_year]
tier_col2r <- tier_b[, .(source = LBL_COL2R, calendar_year, tier, share)]

## ---- Verify against the size-only ACS/static gap (sanity check vs. rank3's own numbers) ----
gap_check <- merge(tier_col2r[, .(calendar_year, tier, share)], acs_tier[, .(calendar_year, tier, acs_share = share)],
                    by = c("calendar_year", "tier"))
cat(sprintf("Sanity check -- collapsed rank3_region Phase B tier share, mean abs gap vs ACS (size-only): %.5f\n",
            mean(abs(gap_check$share - gap_check$acs_share))))

metro_tier_full <- rbindlist(list(tier_col1, tier_col2u, tier_col2r, tier_acs), fill = TRUE)
share_sums <- metro_tier_full[, .(total = sum(share)), by = .(source, calendar_year)]
n_bad <- nrow(share_sums[abs(total - 1) > 0.01])
cat(sprintf("(source, calendar_year) groups where tier shares don't sum to ~1: %d of %d (expect 0)\n", n_bad, nrow(share_sums)))

fwrite(metro_tier_full, file.path(data_dir, "results/memo1_metro_tier_by_calendar_year_full_simplified.csv"))
log_step("Wrote memo1_metro_tier_by_calendar_year_full_simplified.csv")

## =========================================================================
## PART B: demographic (race/sex/region) cross-tab, calendar year 2015
## =========================================================================
log_step("PART B: demographic cross-tab, full sample, 2015")

## ---- Re-derive TRUE post-Stage1 race shares (see memo1_07d's header pt.1) ----
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

cell_state_age_race_sex <- pums_acs5[, .(pop = sum(PWGTP)), by = .(origin_state, age_bucket, race, sex)]
margin_demo <- copy(cell_state_age_race_sex)
setnames(margin_demo, "pop", "Freq")
for (col in c("origin_state", "age_bucket", "race", "sex")) margin_demo[[col]] <- as.character(margin_demo[[col]])
li_long[, `:=`(origin_state = as.character(origin_state), age_bucket = as.character(age_bucket),
                race = as.character(race), sex = as.character(sex),
                moved_last_year_state = as.character(moved_last_year_state))]

margin_mover <- pums_acs5[, .(Freq = sum(PWGTP)), by = .(origin_state, moved_out_of_state)]
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
log_step("Running manual_ipf() on full-sample race melt (this is the slow step)")
li_long[, w_raked := manual_ipf(li_long, "w_base", margins_spec, verbose = TRUE)]

race_share <- li_long[, .(w_raked = sum(w_raked)), by = .(user_id, race)]
race_share[, race_share := w_raked / sum(w_raked), by = user_id]
race_share_wide <- dcast(race_share, user_id ~ race, value.var = "race_share", fill = 0)
stopifnot(all(RACE_LABELS %in% names(race_share_wide)))

li <- merge(li, race_share_wide, by = "user_id", all.x = TRUE)
li[, sex_hard := fifelse(m_prob >= f_prob, "male", "female")]
rm(li_long, li_complete); gc()

## ---- FIXED_YEAR cross-section extraction (same t-slice pattern as memo1_07d) ----
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

slice2 <- extract_fixed_year(li, col_end_col2)
slice2[, `:=`(
  user_id = li$user_id[row_idx], w_unweighted = li$w_unweighted[row_idx], w_full_joint = li$w_full_joint[row_idx],
  sex = li$sex_hard[row_idx], region = unname(region_for_state_abbr[cbsa_state])
)]
for (r in RACE_LABELS) slice2[[r]] <- li[[r]][slice2$row_idx]
slice2[, `:=`(
  tier_region = tier_from_code_region(cbsa_code, cbsa_state),
  origin_tier_region = tier_from_code_region(cbsa_code_prev, cbsa_state_prev)
)]

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

weighted_share <- function(weight, category) {
  d <- data.table(weight = weight, category = category)
  d <- d[!is.na(category) & !is.na(weight) & weight > 0]
  agg <- d[, .(w = sum(weight)), by = category]
  agg[, share := w / sum(w)]
  agg[order(-share)]
}
weighted_share_race <- function(weight, race_share_cols) {
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

add_source(LBL_COL1,
           weighted_share_race(rep(1, nrow(slice1)), setNames(lapply(RACE_LABELS, function(r) slice1[[r]]), RACE_LABELS)),
           weighted_share(rep(1, nrow(slice1)), slice1$sex),
           weighted_share(rep(1, nrow(slice1)), slice1$region))

add_source(LBL_COL2U,
           weighted_share_race(rep(1, nrow(slice2)), setNames(lapply(RACE_LABELS, function(r) li[[r]][slice2$row_idx]), RACE_LABELS)),
           weighted_share(rep(1, nrow(slice2)), slice2$sex),
           weighted_share(rep(1, nrow(slice2)), slice2$region))

## Phase B (rank3_region), the single "reweighted" line
rb_2015 <- ratio_b[calendar_year == FIXED_YEAR]
m_b <- merge(data.table(origin_tier = slice2$origin_tier_region, dest_tier = slice2$tier_region),
             rb_2015[, .(origin_tier, dest_tier, ratio)], by = c("origin_tier", "dest_tier"), all.x = TRUE, sort = FALSE)
ratio_b_vec <- fifelse(is.na(m_b$ratio), 1, m_b$ratio)
w_phaseb <- fifelse(is.na(slice2$origin_tier_region), NA_real_, slice2$w_full_joint * ratio_b_vec)

add_source(LBL_COL2R,
           weighted_share_race(w_phaseb, setNames(lapply(RACE_LABELS, function(r) slice2[[r]]), RACE_LABELS)),
           weighted_share(w_phaseb, slice2$sex),
           weighted_share(w_phaseb, slice2$region))

add_source(LBL_ACS,
           weighted_share(acs_year$PWGTP, acs_year$race),
           weighted_share(acs_year$PWGTP, acs_year$sex),
           weighted_share(acs_year$PWGTP, acs_year$region))

out <- rbindlist(rows)
cat(sprintf("\nCross-section sizes: Column 1 n=%d, Column 2 n=%d (of which %d have a defined Phase B origin), ACS n=%d\n",
            nrow(slice1), nrow(slice2), sum(!is.na(slice2$origin_tier_region)), nrow(acs_year)))

fwrite(out, file.path(data_dir, "results/memo1_demo_crosstab_full_simplified.csv"))
log_step("Wrote memo1_demo_crosstab_full_simplified.csv")

cat(sprintf("\n=== Full-sample demographic cross-tab, %d (wide) ===\n", FIXED_YEAR))
print(dcast(out, category_type + category ~ source, value.var = "share"))

log_step("memo1_10a_full_sample_extras.R done.")
