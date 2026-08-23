# college_origin_destination_counts.R
#
# [NEW 2026-08-21, REVISED same day per Nicholas's follow-up] Standalone
# descriptive output, NOT part of Memo 1's own write-up. For each college
# (col_unitid x col_opeid), counts of students by ORIGIN labor market
# (home CBSA, hs_cbsa_code -- time-invariant, same every year for a given
# person) and DESTINATION labor market (current CBSA, split out by
# CALENDAR YEAR of observation -- every year, not a single snapshot, per
# Nicholas's explicit follow-up), further split by race and sex. NOT split
# by graduation year (col_end) -- explicitly dropped per his first
# follow-up ("I don't expect a college's migration destinations to change
# all that much over time -- I'd rather look at differences across
# sub-groups"). Calendar year of OBSERVATION and graduation-year COHORT
# are different axes; only the latter was dropped.
#
# Population: Column 2 only -- origin (hs_cbsa_code) requires the
# HS-college position-tracking construction that distinguishes Column 2
# from Column 1 (MEMO1_WEIGHTING.md SS2); Column 1 has no origin geography
# at all.
#
# Weights, per Nicholas's explicit request ("include our newly-developed
# time-varying and time-invariant weights"):
#   w1 = w_full_joint, Stage 1 (time-invariant -- MEMO1_WEIGHTING.md SS4).
#        Constant for a person across every calendar year they appear in.
#   w2 = w_full_joint x rank3_region Phase B ratio AT THAT CALENDAR YEAR
#        (time-varying -- SS5). Genuinely different by year, using each
#        year's own ratio_b[y, origin_tier, dest_tier], where origin_tier
#        comes from the person's OWN prior-year CBSA (cbsa_code_{t-1} --
#        a materially different "origin" concept from the table's own
#        origin_cbsa=hs_cbsa_code; both are reported, not conflated). w2
#        is only defined within Phase B's own calibration window
#        (2012-2023, 2020 excluded) AND only for years where the person
#        has a defined prior-year location -- NA (excluded from the w2
#        sum, not imputed) otherwise, exactly Phase B's own convention
#        everywhere else in this project.
#
# ratio_b is built across the FULL 2012-2023 window, vintage-aware (2010
# PUMA/MIGPUMA for 2012-2021, 2020-vintage for 2022-2023) -- the same
# build_acs_flow_data() logic already proven in
# Code/memo1_06b_scheme_comparison.R (extended to this window 2026-08-21),
# reused here rather than re-derived.
#
# Race is carried FRACTIONALLY (MEMO1_WEIGHTING.md SS4.1) -- each person
# contributes their own race probability to every race category's n and
# both weight sums, rather than a hard-classified draw. To avoid ever
# materializing a 6x-larger melted table across what can be tens of
# millions of person-year rows, the six race columns are aggregated
# SEPARATELY (one groupby per race, label added, then combined) instead of
# via melt() + one big groupby -- same result, far less peak memory.
# Sex is hard-classified (m_prob >= f_prob), same convention as elsewhere.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
source(here::here("Code/memo1_00_metro_tier_definitions.R"))

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

RATIO_CAP_LO <- 0.05
RATIO_CAP_HI <- 20
VINTAGE_WINDOWS <- list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023)
CALIB_YEARS <- unlist(VINTAGE_WINDOWS, use.names = FALSE)
T_MAX <- 50
RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob", "native_prob", "multiple_prob", "hispanic_prob")
race_col_to_label <- setNames(RACE_LABELS, RACE_PROB_COLS)

log_step("Loading Column 2")
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds")); setDT(li)
col_end <- as.integer(if (is.factor(li$col_end) || is.character(li$col_end)) as.character(li$col_end) else li$col_end)
# (deliberately NOT stored back onto li via li[, col_end_i := col_end] --
# li already has its own col_end column (factor-typed); data.table's NSE
# would silently resolve the bare name to THAT column instead of this
# local integer vector -- the same bug class already caught and fixed in
# phase_b_flow_loss_analysis.R earlier today.)

## =========================================================================
## rank3_region Phase B ratio table, FULL 2012-2023 window, vintage-aware
## -- copied from memo1_06b_scheme_comparison.R's already-extended,
## already-verified build_acs_flow_data() logic, not re-derived.
## =========================================================================
log_step("Building rank3_region Phase B ratio table, 2012-2023 (vintage-aware)")
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

lookup_region <- build_cbsa_tier_lookup("rank3_region")
code_to_tier_region <- code_to_tier_for_scheme(lookup_region)
region_for_state_abbr <- setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
tier_from_code_region <- function(code_vals, state_vals) code_to_tier_region(code_vals, region = unname(region_for_state_abbr[state_vals]))

pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds")); setDT(pums_1yr)
pums_1yr[, row_id := .I]
puma_tier_2010 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2010_rank3_region.rds")); setDT(puma_tier_2010)
puma_tier_2020 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2020_rank3_region.rds")); setDT(puma_tier_2020)
migpuma_tier_2010 <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2010_rank3_region.rds")); setDT(migpuma_tier_2010)
migpuma_tier_2020 <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2020_rank3_region.rds")); setDT(migpuma_tier_2020)

acs_2010 <- build_acs_flow_data(pums_1yr, VINTAGE_WINDOWS[["2010"]], "PUMA10", "MIGPUMA10", puma_tier_2010, migpuma_tier_2010)
acs_2020 <- build_acs_flow_data(pums_1yr, VINTAGE_WINDOWS[["2020"]], "PUMA20", "MIGPUMA20", puma_tier_2020, migpuma_tier_2020)
acs_margin_b <- rbindlist(list(acs_2010$flow, acs_2020$flow))[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
acs_margin_b[, acs_share := w / sum(w), by = calendar_year]

rev_partials <- vector("list", T_MAX)
for (t in 1:T_MAX) {
  cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
  cur_state_col <- paste0("cbsa_state_", t); prev_state_col <- paste0("cbsa_state_", t - 1)
  if (!all(c(cur_col, prev_col) %in% names(li))) next
  d_tier <- tier_from_code_region(li[[cur_col]], li[[cur_state_col]])
  o_tier <- tier_from_code_region(li[[prev_col]], li[[prev_state_col]])
  cy <- col_end + t
  valid <- !is.na(d_tier) & !is.na(o_tier) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
  if (sum(valid) == 0) next
  rev_partials[[t]] <- data.table(calendar_year = cy[valid], origin_tier = o_tier[valid], dest_tier = d_tier[valid], w = li$w_full_joint[valid])[
    , .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
}
revelio_margin_b <- rbindlist(rev_partials)[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
revelio_margin_b[, revelio_share := w / sum(w), by = calendar_year]
ratio_b <- merge(revelio_margin_b[, .(calendar_year, origin_tier, dest_tier, revelio_share)],
                  acs_margin_b[, .(calendar_year, origin_tier, dest_tier, acs_share)],
                  by = c("calendar_year", "origin_tier", "dest_tier"))
ratio_b[, ratio := pmin(pmax(acs_share / revelio_share, RATIO_CAP_LO), RATIO_CAP_HI)]
setkey(ratio_b, calendar_year, origin_tier, dest_tier)
cat(sprintf("ratio_b: %d (year x origin x dest) cells across %d calendar years\n", nrow(ratio_b), uniqueN(ratio_b$calendar_year)))

## =========================================================================
## Person-year accumulation: one t-loop, every t where this person has a
## defined destination CBSA that year. w2 computed inline per t using that
## year's own ratio_b slice.
## =========================================================================
log_step("Accumulating person-year rows across all t (0..50)")
base_cols <- data.table(
  col_unitid = li$col_unitid, col_opeid = li$col_opeid,
  origin_cbsa = li$hs_cbsa_code, w_full_joint = li$w_full_joint,
  sex = fifelse(li$m_prob >= li$f_prob, "male", "female")
)
for (rp in RACE_PROB_COLS) base_cols[[rp]] <- li[[rp]]

py_parts <- vector("list", T_MAX + 1)
for (t in 0:T_MAX) {
  dest_col <- paste0("cbsa_code_", t)
  if (!dest_col %in% names(li)) next
  cy <- col_end + t
  dest_cbsa_t <- li[[dest_col]]
  valid <- !is.na(dest_cbsa_t) & dest_cbsa_t != "" & !is.na(cy) & !is.na(li$col_unitid)
  if (sum(valid) == 0) next
  idx <- which(valid)

  # Phase B origin tier for THIS t comes from the person's own prior-year
  # (t-1) CBSA -- a different "origin" concept from origin_cbsa=hs_cbsa_code.
  prev_code_col <- paste0("cbsa_code_", t - 1); prev_state_col <- paste0("cbsa_state_", t - 1)
  cur_state_col <- paste0("cbsa_state_", t)
  has_prev <- t >= 1 && prev_code_col %in% names(li)
  dest_tier_t <- tier_from_code_region(dest_cbsa_t[idx], li[[cur_state_col]][idx])
  origin_tier_t <- if (has_prev) tier_from_code_region(li[[prev_code_col]][idx], li[[prev_state_col]][idx]) else rep(NA_character_, length(idx))

  yr <- cy[idx]
  ratio_lookup <- data.table(calendar_year = yr, origin_tier = origin_tier_t, dest_tier = dest_tier_t)
  m <- ratio_b[ratio_lookup, on = c("calendar_year", "origin_tier", "dest_tier"), .(ratio = x.ratio)]
  ratio_vec <- fifelse(is.na(m$ratio), 1, m$ratio)
  w2_t <- fifelse(is.na(origin_tier_t), NA_real_, base_cols$w_full_joint[idx] * ratio_vec)

  py_parts[[t + 1]] <- cbind(base_cols[idx], data.table(destination_cbsa = dest_cbsa_t[idx], calendar_year = yr, w2 = w2_t))
  if (t %% 10 == 0) log_step(sprintf("  t=%d done (%d valid rows this t)", t, length(idx)))
}
person_year <- rbindlist(py_parts)
rm(py_parts, li); gc()

n_before <- nrow(person_year)
person_year <- person_year[!is.na(origin_cbsa) & origin_cbsa != ""]
cat(sprintf("Person-year rows with defined college+origin+destination: %d of %d (%.1f%%)\n",
            nrow(person_year), n_before, 100 * nrow(person_year) / n_before))
cat(sprintf("...of which also have a defined w2: %d (%.1f%%)\n",
            sum(!is.na(person_year$w2)), 100 * mean(!is.na(person_year$w2))))
cat(sprintf("Calendar year range: %d - %d\n", min(person_year$calendar_year), max(person_year$calendar_year)))

## =========================================================================
## Aggregate PER RACE COLUMN separately (avoids a 6x melt on tens of
## millions of rows), then combine.
## =========================================================================
log_step("Aggregating by college x origin x destination x calendar_year x race x sex")
agg_parts <- vector("list", length(RACE_PROB_COLS))
for (i in seq_along(RACE_PROB_COLS)) {
  rp <- RACE_PROB_COLS[i]
  agg_parts[[i]] <- person_year[, .(
    n = sum(get(rp), na.rm = TRUE),
    w1_sum = sum(w_full_joint * get(rp), na.rm = TRUE),
    w2_sum = sum(w2 * get(rp), na.rm = TRUE),
    n_valid_w2 = sum(get(rp) * !is.na(w2), na.rm = TRUE)
  ), by = .(col_unitid, col_opeid, origin_cbsa, destination_cbsa, calendar_year, sex)]
  agg_parts[[i]][, race := race_col_to_label[[rp]]]
  log_step(sprintf("  race=%s done", race_col_to_label[[rp]]))
}
out <- rbindlist(agg_parts)
setcolorder(out, c("col_unitid", "col_opeid", "origin_cbsa", "destination_cbsa", "calendar_year", "race", "sex", "n", "w1_sum", "w2_sum", "n_valid_w2"))

fwrite(out, file.path(data_dir, "results/college_origin_destination_counts_by_year.csv"))
log_step("Wrote college_origin_destination_counts_by_year.csv")

cat(sprintf("\nTotal rows (college x origin x destination x year x race x sex cells): %d\n", nrow(out)))
cat(sprintf("Distinct colleges (col_unitid): %d\n", uniqueN(out$col_unitid)))
cat(sprintf("Distinct origin CBSAs: %d | distinct destination CBSAs: %d | distinct calendar years: %d\n",
            uniqueN(out$origin_cbsa), uniqueN(out$destination_cbsa), uniqueN(out$calendar_year)))
cat("\nSample rows (top 10 by n):\n")
print(head(out[order(-n)], 10))
