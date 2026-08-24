# memo1_10_full_sample_extras.R
#
# [CONSOLIDATED 2026-08-24, per Nicholas's request to reduce script count]
# Merges what were five separate files into one, run top-to-bottom in this
# order (Sections 3 and 4 have a REAL sequential dependency: Section 3
# appends a draft "### 6.5" section to MEMO1_WEIGHTING.md, and Section 4
# finds and entirely replaces that same section with a corrected version --
# this only works if Section 3 has already run). No logic changed from any
# original script, except one genuine duplication removed: Section 3's own
# independent copy of manual_ipf() (never centralized to Code/memo1_ipf.R
# during the 2026-08-23 cleanup pass -- missed because at the time it was
# slated for archiving, then kept instead once memo1_10d turned out to
# depend on its output) now sources the shared version like every other
# section here.
#
# SECTION 1 (was memo1_10a_full_sample_extras.R, was memo1_08_full_sample_extras.R):
# Two full-sample additions to MEMO1_WEIGHTING.md, mirroring what the
# born-cohort artifacts already show but stripped to the four lines that
# matter (per Nicholas's explicit instruction): (A) metro-tier share by
# calendar year under the memo's own 3-tier (size-only) definition, (B) a
# demographic (race/sex/region) cross-tab at calendar year 2015. Both use
# exactly ONE "reweighted" series: w_full_joint (Stage 1) x the
# rank3_region (size x region) Phase B flow-calibration ratio -- confirmed
# via memo1_scheme_gap_summary.csv and MEMO1_WEIGHTING.md SS5.1/SS8 as the
# settled, kept scheme. For the tier-share chart, the region-crossed Phase
# B weight is computed at its native 12-cell (3 size x 4 region)
# resolution -- the actual production calibration -- then COLLAPSED to the
# 3 size-only categories by stripping the " (Region)" suffix and
# re-summing, purely for display; the underlying weight is untouched.
#
# SECTION 2 (was memo1_10b_occupation_calibration_verify.R, was
# memo1_09_occupation_calibration_verify.R): Verification pass for
# Section (production script) memo1_07_reweight_column2_occupation.R's
# w2_occ weight (2-margin geography x occupation IPF, per calendar year).
# Two checks: (1) does the occupation gap against ACS shrink under w2_occ,
# across ALL calibration years, relative to w_full_joint (Stage 1 only)
# and the existing geography-only w2 (single-shot ratio, no occupation
# input)? (2) does adding the occupation margin measurably degrade the
# existing geography calibration, relative to the geography-only w2? The
# existing geography-only w2 is recomputed here with the SAME single-shot
# ratio formula memo1_04_occupation.R Section 2 already uses (not sourced,
# to avoid re-running its full output), applied to the SAME row set as
# w2_occ so the two weights are compared apples-to-apples.
#
# SECTION 3 (was memo1_10c_occupation_memo_table_draft.R, was
# memo1_09_occupation_memo_table.R): Produces ONE new column -- "BA + HS
# on LI (reweighted, geo+occupation)", the w2_occ weight -- for the SAME
# two tables already published in MEMO1_WEIGHTING.md SS6.4 (metro-tier gap
# summary, and the race/sex/region/occupation composition cross-tab at
# calendar year 2015), then appends both as a new SS6.5 with table results
# only. The existing SS6.4 numbers are NOT touched or recomputed. Race
# needs the SAME "true post-Stage1 share" reconstruction Section 1 Part B
# already does (Stage 1's IPF used race as an actual raking KEY on a
# melted table, so only a melt+manual_ipf() re-run recovers each person's
# TRUE post-raking race split).
#
# SECTION 4 (was memo1_10d_occupation_memo_table.R, was
# memo1_09_occupation_memo_table_v2.R; REPLACES Section 3's draft): the
# first draft only had the new weight's column, with no ACS benchmark or
# unweighted baselines to compare against. This rebuilds SS6.5 as a
# genuinely self-contained 4-column table (BA Only / BA + HS on LI / BA +
# HS on LI (reweighted, geo+occupation) / ACS PUMS), reusing SS6.4's OWN
# already-published numbers for the first two and the ACS column (read
# back from the exact CSVs SS6.4 itself was built from) alongside the new
# weight's own numbers already saved by Section 3.
#
# SECTION 5 (was memo1_10e_memo_plots.R, was memo1_09_memo_plots.R):
# Rebuilds MEMO1_WEIGHTING.md's SS6.2 four-line (now five-line) migration-
# rate figure using Code/scripts/09_plots.R's design conventions
# (transparent background, bottom legend, horizontal-only gridlines,
# Segoe UI). 5th line "BA + HS on LI (reweighted, geo+occupation)" reads
# from a third, separate CSV rather than merging into either
# already-published one.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
source(here::here("Code/memo1_00_metro_tier_definitions.R"))
source(here::here("Code/memo1_ipf.R"))

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob", "native_prob", "multiple_prob", "hispanic_prob")
race_col_to_label <- setNames(RACE_LABELS, RACE_PROB_COLS)

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
strip_region <- function(tier) sub(" \\(.*\\)$", "", tier)

## ===========================================================================
## SECTION 1: full-sample metro-tier share + demographic cross-tab
## ===========================================================================
log_step("SECTION 1: full-sample metro-tier share (Part A) + demographic cross-tab (Part B)")

VINTAGE_WINDOWS <- list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023)
CALIB_YEARS <- unlist(VINTAGE_WINDOWS, use.names = FALSE)
T_MAX <- 20

# ---- ACS-side flow data for ONE PUMA vintage window -- copied from
# memo1_08_calibration_charts.R Section 4 / memo1_09c (same already-verified logic).
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

# Final labels, applied directly at export (per Nicholas's instruction --
# no separate relabeling pass needed downstream).
LBL_COL1  <- "BA Only"
LBL_COL2U <- "BA + HS on LI"
LBL_COL2R <- "BA + HS on LI (reweighted)"
LBL_ACS   <- "ACS PUMS"

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

log_step("Loading Column 1/2, ACS 5yr/1yr, race-2015 supplement")
col1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds")); setDT(col1)
li   <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds")); setDT(li)
pums_acs5 <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds")); setDT(pums_acs5)
pums_1yr  <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds"));  setDT(pums_1yr)
acs_race2015 <- readRDS(file.path(data_dir, "intermediate/pums_1yr_race2015.rds")); setDT(acs_race2015)
acs_race2015[, `:=`(SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER))]

col_end_col1 <- resolve_col_end(col1, is_factor_like = FALSE)
col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

## ---- Part A: metro-tier share by calendar year, 3-tier (size-only), 4 lines ----
log_step("Part A: metro-tier share (3-tier, size-only)")

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

log_step("Part A: rank3_region Phase B flow calibration (production scheme)")
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

## ---- Part B: demographic (race/sex/region) cross-tab, calendar year 2015 ----
log_step("Part B: demographic cross-tab, full sample, 2015")

li_complete <- li[!is.na(origin_state) & !is.na(age_bucket) & !is.na(white_prob)]
li_long <- melt(
  li_complete, id.vars = setdiff(names(li_complete), RACE_PROB_COLS),
  measure.vars = RACE_PROB_COLS, variable.name = "race_prob_col", value.name = "race_frac"
)
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

demo_crosstab_out <- rbindlist(rows)
cat(sprintf("\nCross-section sizes: Column 1 n=%d, Column 2 n=%d (of which %d have a defined Phase B origin), ACS n=%d\n",
            nrow(slice1), nrow(slice2), sum(!is.na(slice2$origin_tier_region)), nrow(acs_year)))

fwrite(demo_crosstab_out, file.path(data_dir, "results/memo1_demo_crosstab_full_simplified.csv"))
log_step("Wrote memo1_demo_crosstab_full_simplified.csv")

cat(sprintf("\n=== Full-sample demographic cross-tab, %d (wide) ===\n", FIXED_YEAR))
print(dcast(demo_crosstab_out, category_type + category ~ source, value.var = "share"))

log_step("SECTION 1 done.")
rm(col1, tier_b_partials); gc()

## ===========================================================================
## SECTION 2: verification of w2_occ vs. Stage 1 / geography-only w2
## ===========================================================================
log_step("SECTION 2: w2_occ verification")

log_step("Loading Section 07's saved intermediates")
acs_geo <- readRDS(file.path(data_dir, "intermediate/acs_geo_margin_by_year.rds"))
acs_occ <- readRDS(file.path(data_dir, "intermediate/acs_occ_margin_by_year.rds"))
rev_panel <- readRDS(file.path(data_dir, "intermediate/revelio_geo_occ_person_year_panel.rds")); setDT(rev_panel)
w2_occ_panel <- readRDS(file.path(data_dir, "results/memo1_w2_occupation_calibrated_by_year.rds")); setDT(w2_occ_panel)

## ---- Existing (geography-only) Stage 2 weight, single-shot ratio, computed
## over the SAME rev_panel row set as w2_occ for a fair comparison. ----
log_step("Computing the existing geography-only w2 over the same row set")
revelio_geo <- rev_panel[, .(w = sum(w_full_joint)), by = .(calendar_year, origin_tier, dest_tier)]
revelio_geo[, revelio_share := w / sum(w), by = calendar_year]
ratio_geo <- merge(revelio_geo[, .(calendar_year, origin_tier, dest_tier, revelio_share)],
                    acs_geo[, .(calendar_year, origin_tier, dest_tier, acs_share)],
                    by = c("calendar_year", "origin_tier", "dest_tier"))
ratio_geo[, ratio := pmin(pmax(acs_share / revelio_share, RATIO_CAP_LO), RATIO_CAP_HI)]

rev_panel <- merge(rev_panel, ratio_geo[, .(calendar_year, origin_tier, dest_tier, ratio)],
                    by = c("calendar_year", "origin_tier", "dest_tier"), all.x = TRUE)
rev_panel[, ratio := fifelse(is.na(ratio), 1, ratio)]
rev_panel[, w2_geo_only := w_full_joint * ratio]
rev_panel[, ratio := NULL]

setkey(rev_panel, user_id, calendar_year)
w2_occ_panel_k <- copy(w2_occ_panel); setkey(w2_occ_panel_k, user_id, calendar_year)
rev_panel <- merge(rev_panel, w2_occ_panel_k, by = c("user_id", "calendar_year"), all.x = TRUE)
cat(sprintf("Rows with a w2_occ value: %d of %d (%.1f%%)\n", sum(!is.na(rev_panel$w2_occ)), nrow(rev_panel),
            100 * mean(!is.na(rev_panel$w2_occ))))

weighted_gap <- function(dt, weight_col, key_col, target) {
  d <- dt[!is.na(get(weight_col)) & !is.na(get(key_col))]
  agg <- d[, .(w = sum(get(weight_col))), by = c("calendar_year", key_col)]
  agg[, share := w / sum(w), by = calendar_year]
  setnames(agg, key_col, "cell")
  m <- merge(agg, target, by.x = c("calendar_year", "cell"), by.y = c("calendar_year", names(target)[2]),
             all.x = TRUE)
  setnames(m, "acs_share", "target_share")
  m[, gap := abs(share - target_share)]
  m
}

## ---- CHECK 1: occupation gap vs ACS, by calendar year, three weights ----
log_step("CHECK 1: occupation gap vs ACS")
occ_target <- acs_occ[, .(calendar_year, major_group, acs_share)]
gap_occ_unweighted <- weighted_gap(rev_panel[, .(calendar_year, major_group, w = 1)], "w", "major_group", occ_target)[, .(calendar_year, gap, source = "Unweighted")]
gap_occ_stage1 <- weighted_gap(rev_panel, "w_full_joint", "major_group", occ_target)[, .(calendar_year, gap, source = "Stage 1 only (w_full_joint)")]
gap_occ_geo <- weighted_gap(rev_panel, "w2_geo_only", "major_group", occ_target)[, .(calendar_year, gap, source = "Geography-only w2 (existing)")]
gap_occ_new <- weighted_gap(rev_panel, "w2_occ", "major_group", occ_target)[, .(calendar_year, gap, source = "Geography+Occupation w2_occ (new)")]

occ_gap_by_year <- rbindlist(list(gap_occ_unweighted, gap_occ_stage1, gap_occ_geo, gap_occ_new))[
  , .(mean_abs_gap = mean(gap, na.rm = TRUE)), by = .(calendar_year, source)]
occ_gap_summary <- occ_gap_by_year[, .(mean_abs_gap = mean(mean_abs_gap)), by = source][order(mean_abs_gap)]
cat("\nOccupation composition gap vs ACS, mean |share - ACS share| across major groups, averaged over all calibration years:\n")
print(occ_gap_summary)
cat("\nBy calendar year:\n")
print(dcast(occ_gap_by_year, calendar_year ~ source, value.var = "mean_abs_gap"))

## ---- CHECK 2: geography (tier x tier) gap vs ACS, geography-only w2 vs
## w2_occ -- confirms adding the occupation margin doesn't materially
## degrade the existing geography calibration. ----
log_step("CHECK 2: geography gap vs ACS -- geography-only w2 vs w2_occ")
rev_panel[, tier_pair := paste(origin_tier, dest_tier, sep = " -> ")]
geo_target <- acs_geo[, .(calendar_year, tier_pair = paste(origin_tier, dest_tier, sep = " -> "), acs_share)]
gap_geo_geo <- weighted_gap(rev_panel, "w2_geo_only", "tier_pair", geo_target)[, .(calendar_year, gap, source = "Geography-only w2 (existing)")]
gap_geo_new <- weighted_gap(rev_panel, "w2_occ", "tier_pair", geo_target)[, .(calendar_year, gap, source = "Geography+Occupation w2_occ (new)")]

geo_gap_by_year <- rbindlist(list(gap_geo_geo, gap_geo_new))[, .(mean_abs_gap = mean(gap, na.rm = TRUE)), by = .(calendar_year, source)]
geo_gap_summary <- geo_gap_by_year[, .(mean_abs_gap = mean(mean_abs_gap)), by = source][order(mean_abs_gap)]
cat("\nGeography (origin_tier -> dest_tier) composition gap vs ACS, averaged over all calibration years:\n")
print(geo_gap_summary)
cat("\nBy calendar year:\n")
print(dcast(geo_gap_by_year, calendar_year ~ source, value.var = "mean_abs_gap"))

fwrite(occ_gap_by_year, file.path(data_dir, "results/memo1_w2_occupation_verify_occ_gap.csv"))
fwrite(geo_gap_by_year, file.path(data_dir, "results/memo1_w2_occupation_verify_geo_gap.csv"))
log_step("Wrote memo1_w2_occupation_verify_occ_gap.csv and memo1_w2_occupation_verify_geo_gap.csv")
log_step("SECTION 2 done.")

## ===========================================================================
## SECTION 3: SS6.5 draft -- adds the geo+occupation column to SS6.4's tables
## ===========================================================================
log_step("SECTION 3: SS6.5 draft (geo+occupation column)")

LBL_NEW <- "BA + HS on LI (reweighted, geo+occupation)"
region_of_tier <- function(tier) sub("^.*\\((.*)\\)$", "\\1", tier)

log_step("Loading Column 2 (fresh -- Section 1 modified `li` in place)")
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds")); setDT(li)

## ---- PART 1: re-derive TRUE post-Stage1 race shares -- same mechanism as
## Section 1 Part B, on the freshly-reloaded li. ----
log_step("PART 1: re-deriving true post-Stage1 race shares (melt + manual_ipf, the slow step)")
li_complete <- li[!is.na(origin_state) & !is.na(age_bucket) & !is.na(white_prob)]
li_long <- melt(
  li_complete, id.vars = setdiff(names(li_complete), RACE_PROB_COLS),
  measure.vars = RACE_PROB_COLS, variable.name = "race_prob_col", value.name = "race_frac"
)
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
li_long[, w_raked := manual_ipf(li_long, "w_base", margins_spec, verbose = TRUE)]

race_share <- li_long[, .(w_raked = sum(w_raked)), by = .(user_id, race)]
race_share[, race_share := w_raked / sum(w_raked), by = user_id]
race_share_wide <- dcast(race_share, user_id ~ race, value.var = "race_share", fill = 0)
stopifnot(all(RACE_LABELS %in% names(race_share_wide)))
rm(li_long, li_complete); gc()

li_slim <- li[, .(user_id, m_prob, f_prob)]
li_slim[, sex_hard := fifelse(m_prob >= f_prob, "male", "female")]
li_slim <- merge(li_slim, race_share_wide, by = "user_id", all.x = TRUE)

## ---- PART 2: calendar-year-2015 slice under w2_occ, with race/sex/region/
## occupation all attached. ----
log_step("PART 2: 2015 cross-section under w2_occ")
slice_2015 <- merge(w2_occ_panel[calendar_year == FIXED_YEAR],
                     rev_panel[calendar_year == FIXED_YEAR, .(user_id, calendar_year, dest_tier, major_group)],
                     by = c("user_id", "calendar_year"))
slice_2015 <- merge(slice_2015, li_slim, by = "user_id", all.x = TRUE)
slice_2015[, region := region_of_tier(dest_tier)]
cat(sprintf("2015 cross-section under w2_occ: n=%d\n", nrow(slice_2015)))

race_row <- weighted_share_race(slice_2015$w2_occ, setNames(lapply(RACE_LABELS, function(r) slice_2015[[r]]), RACE_LABELS))
sex_row <- weighted_share(slice_2015$w2_occ, slice_2015$sex_hard)
region_row <- weighted_share(slice_2015$w2_occ, slice_2015$region)
occ_row <- weighted_share(slice_2015$w2_occ, slice_2015$major_group)

demo_table <- rbindlist(list(
  race_row[, .(category_type = "race", category, share)],
  sex_row[, .(category_type = "sex", category, share)],
  region_row[, .(category_type = "region", category, share)],
  occ_row[, .(category_type = "occupation", category, share)]
))
demo_table[, source := LBL_NEW]
fwrite(demo_table, file.path(data_dir, "results/memo1_demo_crosstab_geo_occ_2015.csv"))

## ---- PART 3: metro-tier (3-size) mean absolute gap vs. ACS, 2012-2023,
## under w2_occ -- same metric as SS6.4's first table, reusing the
## already-saved ACS PUMS line from Section 1's own output rather than
## rebuilding the ACS side from scratch. ----
log_step("PART 3: metro-tier gap vs ACS, 2012-2023, under w2_occ")
existing_tier <- fread(file.path(data_dir, "results/memo1_metro_tier_by_calendar_year_full_simplified.csv"))
acs_tier_target <- existing_tier[source == "ACS PUMS", .(calendar_year, tier, acs_share = share)]

panel_w2occ <- merge(rev_panel[, .(user_id, calendar_year, dest_tier)], w2_occ_panel, by = c("user_id", "calendar_year"))
panel_w2occ[, tier := strip_region(dest_tier)]
tier_w2occ <- panel_w2occ[, .(w = sum(w2_occ)), by = .(calendar_year, tier)]
tier_w2occ[, share := w / sum(w), by = calendar_year]

gap_check <- merge(tier_w2occ[, .(calendar_year, tier, share)], acs_tier_target, by = c("calendar_year", "tier"))
mean_gap <- mean(abs(gap_check$share - gap_check$acs_share))
cat(sprintf("Mean abs metro-tier gap vs ACS, 2012-2023, w2_occ: %.4f (%.2f pp)\n", mean_gap, 100 * mean_gap))

## ---- PART 4: append DRAFT SS6.5 to MEMO1_WEIGHTING.md, table results only
## (Section 4 below replaces this in full once it runs). ----
log_step("PART 4: appending draft SS6.5 to MEMO1_WEIGHTING.md")
memo_path <- file.path(directory, "MEMO1_WEIGHTING.md")

fmt_pct <- function(x) sprintf("%.1f%%", 100 * x)
row_lookup <- function(dt, cat) if (cat %in% dt$category) fmt_pct(dt[category == cat, share]) else "~0.0%"

race_order <- c("White" = "white", "Black" = "black", "Hispanic" = "hispanic", "Asian" = "asian",
                 "Multiple" = "multiple", "Native" = "native")
sex_order <- c("Male" = "male", "Female" = "female")
region_order <- c("Northeast" = "Northeast", "Midwest" = "Midwest", "South" = "South", "West" = "West")
occ_rows_in_memo_order <- c(
  "Management", "Educational Instruction and Library", "Healthcare Practitioners and Technical",
  "Business and Financial Operations", "Sales and Related", "Office and Administrative Support",
  "Computer and Mathematical", "Arts, Design, Entertainment, Sports, and Media", "Community and Social Service",
  "Architecture and Engineering", "Legal", "Life, Physical, and Social Science", "Personal Care and Service",
  "Protective Service", "Food Preparation and Serving", "Production", "Transportation and Material Moving",
  "Healthcare Support", "Construction and Extraction", "Installation, Maintenance, and Repair",
  "Building and Grounds Cleaning and Maintenance", "Farming, Fishing, and Forestry", "Military Specific"
)

lines <- c(
  "",
  "### 6.5 Full-sample check, with occupation added as a Stage 2 margin",
  "",
  sprintf("Table results only, appended %s -- adds `%s` (Code/memo1_07_reweight_column2_occupation.R's",
          format(Sys.Date(), "%Y-%m-%d"), LBL_NEW),
  "w2_occ: geography and destination-occupation raked jointly per calendar year via a 2-margin IPF) as a fifth",
  "series alongside SS6.4's existing four. SS6.4's own numbers are unchanged.",
  "",
  "**Mean absolute gap vs. ACS, metro-tier share, 2012-2023 (2020 excluded), percentage points:**",
  "",
  "| BA Only | BA + HS on LI | BA + HS on LI (reweighted) | BA + HS on LI (reweighted, geo+occupation) |",
  "|---|---|---|---|",
  sprintf("| 3.80 | 4.04 | 0.12 | %.2f |", 100 * mean_gap),
  "",
  sprintf("**Demographic and occupational composition, calendar year %d** (fifth column added to SS6.4's table; race/sex/occupation are outside this weight's calibration target the same way they were for SS6.4's \"reweighted\" column -- see SS6.4's own note; occupation is now IN-sample for this weight, geography remains partly in-sample under the kept scheme):", FIXED_YEAR),
  "",
  "| Category | BA + HS on LI (reweighted, geo+occupation) |",
  "|---|---|"
)

for (lbl in names(race_order)) lines <- c(lines, sprintf("| %s | %s |", lbl, row_lookup(race_row, race_order[[lbl]])))
for (lbl in names(sex_order)) lines <- c(lines, sprintf("| %s | %s |", lbl, row_lookup(sex_row, sex_order[[lbl]])))
for (lbl in names(region_order)) lines <- c(lines, sprintf("| %s | %s |", lbl, row_lookup(region_row, region_order[[lbl]])))
for (lbl in occ_rows_in_memo_order) lines <- c(lines, sprintf("| %s | %s |", lbl, row_lookup(occ_row, lbl)))

lines <- c(lines, "")
writeLines(lines, con = file(memo_path, open = "a"))
log_step(sprintf("Appended draft SS6.5 to %s", memo_path))
log_step("SECTION 3 done.")

## ===========================================================================
## SECTION 4: rebuild SS6.5 as a self-contained 4-column table (replaces
## Section 3's draft). Depends on Section 3 having just appended a
## "### 6.5" section to MEMO1_WEIGHTING.md, and on Section 3's
## memo1_demo_crosstab_geo_occ_2015.csv output.
## ===========================================================================
log_step("SECTION 4: rebuilding SS6.5 as a self-contained 4-column table")

LBL_COL1  <- "BA Only"
LBL_COL2U <- "BA + HS on LI"
LBL_ACS   <- "ACS PUMS"

log_step("Loading source CSVs")
demo_full <- fread(file.path(data_dir, "results/memo1_demo_crosstab_full_simplified.csv"))
occ_full  <- fread(file.path(data_dir, "results/memo1_occupation_crosstab_full_simplified.csv"))
setnames(occ_full, "major_group", "category")
occ_full[, category_type := "occupation"]
occ_full <- occ_full[, .(source, category_type, category, share)]

new_col <- fread(file.path(data_dir, "results/memo1_demo_crosstab_geo_occ_2015.csv"))
new_col <- new_col[, .(source, category_type, category, share)]

cap_map <- c(white = "White", black = "Black", hispanic = "Hispanic", asian = "Asian",
             multiple = "Multiple", native = "Native", male = "Male", female = "Female")
demo_full[category_type %in% c("race", "sex"), category := unname(cap_map[category])]
new_col[category_type %in% c("race", "sex"), category := unname(cap_map[category])]

all_rows <- rbindlist(list(demo_full, occ_full, new_col), use.names = TRUE)
all_rows <- all_rows[source %in% c(LBL_COL1, LBL_COL2U, LBL_NEW, LBL_ACS)]

wide <- dcast(all_rows, category_type + category ~ source, value.var = "share")
setcolorder(wide, c("category_type", "category", LBL_COL1, LBL_COL2U, LBL_NEW, LBL_ACS))

race_order2 <- c("White", "Black", "Hispanic", "Asian", "Multiple", "Native")
sex_order2 <- c("Male", "Female")
region_order2 <- c("Northeast", "Midwest", "South", "West")
row_order <- c(race_order2, sex_order2, region_order2, occ_rows_in_memo_order)
wide[, category := factor(category, levels = row_order)]
setorder(wide, category)

fmt_pct2 <- function(x) {
  ifelse(is.na(x), "~0.0%",
         ifelse(x < 0.0005, "~0.0%", sprintf("%.1f%%", 100 * x)))
}

log_step("Rebuilding SS6.5 in MEMO1_WEIGHTING.md")
existing_lines <- readLines(memo_path)
cut_idx <- which(grepl("^### 6\\.5", existing_lines))
stopifnot(length(cut_idx) == 1)
kept_lines <- existing_lines[seq_len(cut_idx - 1)]
while (length(kept_lines) > 0 && kept_lines[length(kept_lines)] == "") kept_lines <- kept_lines[-length(kept_lines)]

header_lines <- c(
  "",
  "### 6.5 Full-sample check, with occupation added as a Stage 2 margin",
  "",
  sprintf("Table results only, appended %s -- corrected from an earlier draft that showed only the new",
          format(Sys.Date(), "%Y-%m-%d")),
  "weight's own numbers with nothing to compare them against. This reproduces SS6.4's own table structure --",
  "same four columns (BA Only / BA + HS on LI / [a reweighted column] / ACS PUMS), same race/sex/region/",
  "occupation rows -- swapping in `w2_occ` (Code/memo1_07_reweight_column2_occupation.R: geography and",
  "destination occupation raked jointly per calendar year via a 2-margin IPF) as the reweighted column,",
  "in place of SS6.4's geography-only one. BA Only, BA + HS on LI, and ACS PUMS values are read back",
  "unchanged from the exact CSVs SS6.4 itself was built from, not re-derived.",
  "",
  sprintf("**Requested but not yet built: industry.** The Revelio position data this pipeline reads from"),
  "(`Data/intermediate/pos_parquet_pilot`) only carries `onet_code` (occupation) -- no NAICS/industry field",
  "at all, confirmed by reading its Arrow schema directly rather than assumed. Occupation and industry are",
  "different axes (what job vs. what sector), and there's no reliable crosswalk from one to the other --",
  "most SOC codes span many industries. If there's a different Revelio source/table with an industry field,",
  "point me to it and I'll build this properly; otherwise this row-group stays out rather than being faked",
  "via an approximate occupation-to-industry mapping.",
  "",
  "**Mean absolute gap vs. ACS, metro-tier share, 2012-2023 (2020 excluded), percentage points:**",
  "",
  sprintf("| %s | %s | %s |", LBL_COL1, LBL_COL2U, LBL_NEW),
  "|---|---|---|",
  "| 3.80 | 4.04 | 0.11 |",
  "",
  sprintf("**Demographic and occupational composition, calendar year 2015:**"),
  "",
  sprintf("| Category | %s | %s | %s | %s |", LBL_COL1, LBL_COL2U, LBL_NEW, LBL_ACS),
  "|---|---|---|---|---|"
)

row_lines <- wide[, sprintf("| %s | %s | %s | %s | %s |", category,
                             fmt_pct2(get(LBL_COL1)), fmt_pct2(get(LBL_COL2U)), fmt_pct2(get(LBL_NEW)), fmt_pct2(get(LBL_ACS)))]

writeLines(c(kept_lines, header_lines, row_lines, ""), con = memo_path)
log_step(sprintf("Rewrote %s with corrected SS6.5 (%d rows)", memo_path, length(row_lines)))
log_step("SECTION 4 done.")

## ===========================================================================
## SECTION 5: rebuild the SS6.2 five-line migration-rate figure
## ===========================================================================
log_step("SECTION 5: rebuilding the SS6.2 migration-rate figure")
library(ggplot2)

FONT <- "Segoe UI"
inst_group_colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
LBL_COL2R <- "BA + HS on LI (reweighted)"
LBL_COL2R_OCC <- "BA + HS on LI (reweighted, geo+occupation)"
# 5th color added (not a re-generated 5-color hue_pal(), which would shift
# all four existing colors) -- the existing four stay pixel-identical to
# every previously-published version of this figure.
series_order  <- c(LBL_COL1, LBL_COL2U, LBL_COL2R, LBL_COL2R_OCC, LBL_ACS)
series_colors <- setNames(c(inst_group_colors[1:3], "#4C72B0", inst_group_colors[4]), series_order)

relabel <- c(
  "Column 1 (college-only)" = LBL_COL1,
  "Column 2 (HS+college, unweighted)" = LBL_COL2U,
  "Column 2 (Phase B: flow-calibrated)" = LBL_COL2R,
  "ACS PUMS benchmark" = LBL_ACS
)

load_cohort <- function(cohort_name, cohort_label) {
  base <- fread(file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_%s.csv", cohort_name)))
  region <- fread(file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_rank3_region_%s.csv", cohort_name)))
  geo_occ <- fread(file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_geo_occ_%s.csv", cohort_name)))
  d <- rbind(
    base[source %in% c("Column 1 (college-only)", "Column 2 (HS+college, unweighted)", "ACS PUMS benchmark"),
         .(source, calendar_year, rate)],
    region[source == "Column 2 (Phase B: flow-calibrated)", .(source, calendar_year, rate)],
    geo_occ[, .(source = LBL_COL2R_OCC, calendar_year, rate)]
  )
  d[, source := fifelse(source %in% names(relabel), relabel[source], source)]
  d[, cohort := cohort_label]
  # Pre-2000 Revelio observations are a data-coverage artifact (a handful
  # of implausibly-early panel entries), documented and excluded the same
  # way in the interactive chart artifacts' footer notes -- not a real
  # finding about early-panel mobility.
  d[calendar_year >= 2000]
}

plot_data <- rbind(
  load_cohort("born_1980s", "Born 1980-1989"),
  load_cohort("born_1990s", "Born 1990-1999")
)
plot_data[, source := factor(source, levels = series_order)]
plot_data[, cohort := factor(cohort, levels = c("Born 1980-1989", "Born 1990-1999"))]

p <- ggplot(plot_data, aes(x = calendar_year, y = rate, color = source)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.3) +
  facet_wrap(~cohort) +
  scale_color_manual(values = series_colors, name = NULL) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Chance of moving across a state line", title = NULL) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        text = element_text(size = 10, family = FONT),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linewidth = 0.3),
        legend.position = "bottom")

out_path <- file.path(data_dir, "results/memo1_simplified_migration_rate_by_cohort.png")
ggsave(filename = out_path, plot = p, width = 6.5, height = 3.75, units = "in", dpi = 600, bg = "transparent")
cat(sprintf("Wrote %s\n", out_path))
log_step("SECTION 5 done.")

log_step("memo1_10_full_sample_extras.R done.")
