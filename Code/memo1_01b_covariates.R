# memo1_01b_covariates.R
#
# [CONSOLIDATED 2026-08-24, per Nicholas's request to reduce script count]
# Merges what were two files (memo1_01b_column1_demo.R,
# memo1_01c_covariates.R) into one -- both are comparatively small/cheap
# steps that always run back-to-back after memo1_01a_column1_construct.R.
# `memo1_01a_column1_construct.R` itself is deliberately NOT folded in here:
# at 1000+ lines it is by far the largest, most heavily checkpointed script
# in this pipeline (survived a mid-run machine reboot via per-chunk
# checkpointing, multiple hard-won OOM fixes documented inline, a ~9-hour
# worst-case rebuild) -- merging it into a bigger file would add real risk
# for no benefit, the same reason Nicholas's own core `00`-`09` pipeline
# scripts are left untouched by this whole cleanup. No logic changed from
# either original script below.
#
# SECTION 1 (was memo1_01b_column1_demo.R): Column 1 analogue of
# Code/demographics.R's `both_demo.rds` build (L66-87): demographic
# (race/sex) probabilities for Column 1's population
# (Data/intermediate/column1_population.rds, 34,467,516 users), which
# demographics.R never covered -- it was built filtered to both_final.rds's
# user_id universe (5.0M) for the main pipeline's HS-matched sample only.
#
# Adaptation is a straight substitution of the filter population
# (both_final.rds's user_id -> column1_population.rds's user_id); the
# *_prob columns themselves are native to the raw Revelio CSV, not derived,
# so no other logic changes. Deliberately skips demographics.R's
# draw_category()-based sex_draw/race_draw stochastic sampling (L76-86) --
# Code/acs_reweight.R's existing pattern already consumes the fractional
# *_prob columns directly, never the draws, and re-running apply()-based
# per-row sampling over 34.4M rows (~7x demographics.R's original scale)
# buys nothing this memo needs.
#
# SECTION 2 (was memo1_01c_covariates.R): Assembles the covariate-complete
# versions of Column 1 and Column 2 for Memo 1's comparison table:
# race/sex (fractional probabilities), age (birth year), geography (state
# + Census region), transfer status, and graduate-degree attainment, for
# both columns on a common schema wherever the underlying data supports
# it. Run after Section 1 (Column 1's demographics) and
# memo1_01a_column1_construct.R (Column 1's population + degree
# enrichment) have both completed. Reads column2_regression_refresh.rds
# directly for Column 2 -- no dependency on that file being persisted by
# any run_pipeline*.R script (see HANDOFF.md's open item on that gap; not
# this script's job to fix).
#
# Standalone from the core pipeline, not part of run_pipeline.R, per this
# repo's no-source()-between-files convention (except college_lookup.R,
# the same one-time exception memo1_01a_column1_construct.R already
# makes). All Section 2 joins use merge() (explicit by.x/by.y, all.x=TRUE),
# not data.table bracket-chaining, to keep column provenance unambiguous.
#
# table.express note: if table.express has been attached earlier in this
# session (it registers select.data.table/mutate.data.table S3 methods that
# silently corrupt plain dplyr chains on data.tables -- a repeat offender in
# this codebase), it doesn't matter here, since every operation below is
# either merge() or data.table [i, j] syntax, never a bare dplyr verb.

library(data.table)
library(arrow)
library(dplyr)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

# resolve_college(): utility function only (no pipeline-stage side effects),
# same exception to the no-source() convention memo1_01a_column1_construct.R
# already makes. Used in Section 2 to attach col_cbsa_code to Column 1,
# which memo1_01a_column1_construct.R's narrower column1_col_match schema
# never carried through (unlike Column 2, which already has col_cbsa_code
# natively via 05_merge.Rmd).
source(file.path(here::here(), "Code", "college_lookup.R"))

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

## ===========================================================================
## SECTION 1: Column 1 demographics (race/sex probabilities)
## ===========================================================================
log_step("SECTION 1: Column 1 demographics")

column1_demo_path <- file.path(data_dir, "intermediate/column1_demo.rds")

if (!file.exists(column1_demo_path)) {

log_step("memo1_column1_demo starting")

column1_population <- readRDS(file.path(data_dir, "intermediate/column1_population.rds"))
setDT(column1_population)
keep_users <- unique(column1_population[, .(user_id)])
log_step(paste("column1_population loaded:", nrow(keep_users), "unique users"))

# Same raw file demographics.R reads (L70) -- arrow::open_dataset() streams
# the scan rather than loading the whole CSV, which matters here since this
# filter is against ~7x more user_ids than demographics.R's original run.
users <- open_dataset(file.path(data_dir, "intermediate/uwdbhdlkbnzhaqxd.csv"), format = "csv")

column1_demo <- users %>% filter(user_id %in% keep_users$user_id) %>% collect()
setDT(column1_demo)
log_step(paste("filtered + collected:", nrow(column1_demo), "rows"))

stopifnot(all(c("white_prob", "black_prob", "api_prob", "hispanic_prob",
                "native_prob", "multiple_prob", "m_prob", "f_prob") %in% names(column1_demo)))

cat(sprintf("Demographic-probability coverage on Column 1: %.1f%% (%d of %d users matched)\n",
            100 * nrow(column1_demo) / nrow(keep_users), nrow(column1_demo), nrow(keep_users)))
# Compare against Column 2's ~96.5% match rate onto both_demo.rds (see
# velvet-churning-galaxy.md's Step 2 plan) -- don't assume this lands at the
# same rate; measure it.

saveRDS(column1_demo, column1_demo_path)
log_step("saved column1_demo.rds")
rm(column1_population, keep_users, users, column1_demo)
gc()

} else {
  log_step("column1_demo.rds already exists -- skipping Section 1")
}

log_step("SECTION 1 done.")

## ===========================================================================
## SECTION 2: covariate-complete Column 1 and Column 2 tables
## ===========================================================================
log_step("SECTION 2: covariates for Column 1 and Column 2")

# Census-Bureau-consistent region/division labels, copied verbatim from
# Code/demographics.R L23-52 -- NOT base R's state.region, which uses
# different category labels (e.g. "North Central" instead of "Midwest") and
# would silently fail to line up with the census_region derived from ACS's
# own state codes in Code/memo1_02_acs_pulls.R.
state_region_crosswalk <- data.table(
  state_abbr = c(
    "CT", "ME", "MA", "NH", "RI", "VT",
    "NJ", "NY", "PA",
    "IL", "IN", "MI", "OH", "WI",
    "IA", "KS", "MN", "MO", "NE", "ND", "SD",
    "DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV",
    "AL", "KY", "MS", "TN",
    "AR", "LA", "OK", "TX",
    "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
    "AK", "CA", "HI", "OR", "WA"
  ),
  census_region = c(
    rep("Northeast", 9),
    rep("Midwest", 12),
    rep("South", 17),
    rep("West", 13)
  )
)

RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob", "native_prob", "multiple_prob", "hispanic_prob")

# [NEW 2026-08-10] Migration/graduation-timing variables, at Nicholas's
# request: share who stay in the college labor market, share who leave and
# return, mean duration away, an ACS-comparable "moved in the past year"
# analog, age-at-graduation shares, and associate's-degree share (Revelio
# only -- ACS's BA+ filter excludes associate's-only respondents entirely,
# so there's no ACS analog to report there).
#
# MIGRATION_WINDOW=10 matches this repo's own established convention
# (Code/06_finalize_data.Rmd's `migration_ends <- c(5, 10)`, and the
# already-existing `ever_leave_cbsa_col_10`/`yagan_cbsa_col_10` on Column 2)
# -- both columns use the same window so the two are comparable.
MIGRATION_WINDOW <- 10

# count_years_away(): for each row, counts how many of the `window` years
# post-graduation (0..window) had an observed CBSA that differs from
# ref_cbsa_col, treating no-data years as "not away" (excluded, not
# counted) rather than assumed-away. This is a plain headcount of years
# spent elsewhere -- deliberately NOT the same concept as this repo's
# existing yr_return_col_cbsa (06_finalize_data.Rmd), which finds the FIRST
# year a person's location EVER matched the reference and so reports 0 for
# anyone observed at the college metro in year 0, regardless of whether
# they left later -- that conflates day-one non-movers with genuine
# return-migrants. yrs_away avoids that by just counting mismatch-years
# directly.
count_years_away <- function(dt, ref_cbsa_col, window) {
  cbsa_cols <- paste0("cbsa_code_", 0:window)
  stopifnot(all(cbsa_cols %in% names(dt)))
  mismatch_mat <- do.call(cbind, lapply(cbsa_cols, function(cc) {
    x <- dt[[cc]] != dt[[ref_cbsa_col]]
    fifelse(is.na(x), 0L, as.integer(x))
  }))
  rowSums(mismatch_mat)
}

# build_cbsa_ever_leave_yagan(): builds ever_leave_cbsa_col_{window} and
# yagan_cbsa_col_{window} in place, matching Code/05_merge.Rmd's/
# Code/06_finalize_data.Rmd's own naming and Reduce(`|`, lapply(.SD, ...))
# construction exactly (mirrored, not reinvented) -- only needed for
# Column 1, which never had these built; Column 2 already has both natively.
build_cbsa_ever_leave_yagan <- function(dt, ref_cbsa_col, window) {
  cbsa_cols <- paste0("cbsa_code_", 0:window)
  stopifnot(all(cbsa_cols %in% names(dt)))
  mismatch_list <- lapply(cbsa_cols, function(cc) dt[[cc]] != dt[[ref_cbsa_col]])
  ever_mismatch <- Reduce(`|`, mismatch_list)
  dt[, (paste0("ever_leave_cbsa_col_", window)) := fifelse(is.na(ever_mismatch), 0L, as.integer(ever_mismatch))]
  dt[, (paste0("yagan_cbsa_col_", window)) := as.integer(get(paste0("cbsa_code_", window)) == get(ref_cbsa_col))]
  invisible(dt)
}

# college_metro_status(): a 3-category partition built from ever_leave/
# yagan_return -- "Stayed" (never observed outside the college metro within
# the window), "Left, Returned" (left at some point but back in the college
# metro by year `window`, i.e. Yagan (2014)'s own start/end convention,
# already used elsewhere in this pipeline), "Left, Never Returned" (left
# and not back by year `window`). NA when there's no position data in the
# window at all.
college_metro_status <- function(ever_leave, yagan_return) {
  fcase(
    ever_leave == 0L, "Stayed",
    ever_leave == 1L & yagan_return == 1L, "Left, Returned",
    ever_leave == 1L & yagan_return == 0L, "Left, Never Returned",
    default = NA_character_
  )
}

# last_year_state_move(): Revelio's structural analog of ACS's MIGSP-vs-ST
# "moved in the past year" flag. Revelio's position panel is indexed by
# years-since-graduation, not a fixed survey date, so there's no single
# "one year ago" reference point the way ACS has -- the closest analog is
# each person's own most recent PAIR of consecutive observed years: did
# their state change between those two years? Requires the last two years
# to be genuinely adjacent (no gap in the panel); left NA otherwise rather
# than guessed at (report the NA rate -- this is a real limitation, not a
# free comparison).
last_year_state_move <- function(dt, state_col_prefix = "cbsa_state_", max_idx = 50) {
  cols <- paste0(state_col_prefix, 0:max_idx)
  stopifnot(all(cols %in% names(dt)))
  mat <- as.matrix(dt[, ..cols])
  has_data <- !is.na(mat)
  n <- nrow(mat)

  last_idx <- max.col(has_data * col(has_data), ties.method = "last")
  last_idx[rowSums(has_data) == 0L] <- NA_integer_
  prev_idx <- last_idx - 1L

  row_ids <- seq_len(n)
  ok_rows <- !is.na(last_idx) & last_idx >= 2L
  prev_has_data <- rep(FALSE, n)
  prev_has_data[ok_rows] <- has_data[cbind(row_ids[ok_rows], prev_idx[ok_rows])]

  moved <- rep(NA_integer_, n)
  cmp_rows <- ok_rows & prev_has_data
  moved[cmp_rows] <- as.integer(
    mat[cbind(row_ids[cmp_rows], last_idx[cmp_rows])] !=
    mat[cbind(row_ids[cmp_rows], prev_idx[cmp_rows])]
  )
  moved
}

# ==========================================================================
# Column 1
# ==========================================================================
column1_covariates_path <- file.path(data_dir, "intermediate/column1_covariates.rds")

if (!file.exists(column1_covariates_path)) {

log_step("Column 1 covariates starting")

column1_population <- readRDS(file.path(data_dir, "intermediate/column1_population.rds"))
setDT(column1_population)
stopifnot(all(c("user_id", "col_state", "birth", "col_unitid", "col_opeid") %in% names(column1_population)))
log_step(paste("column1_population loaded:", nrow(column1_population), "users"))

column1_demo <- readRDS(file.path(data_dir, "intermediate/column1_demo.rds"))
setDT(column1_demo)
stopifnot(all(c("user_id", RACE_PROB_COLS, "m_prob", "f_prob") %in% names(column1_demo)))

column1_degree_enrichment <- readRDS(file.path(data_dir, "intermediate/column1_degree_enrichment.rds"))
setDT(column1_degree_enrichment)

colleges <- readRDS(file.path(data_dir, "intermediate/colleges.rds"))
setDT(colleges)

column1_covariates <- merge(
  column1_population,
  column1_demo[, c("user_id", RACE_PROB_COLS, "m_prob", "f_prob"), with = FALSE],
  by = "user_id", all.x = TRUE
)
cat(sprintf("Demographic match rate on Column 1: %.1f%%\n",
            100 * mean(!is.na(column1_covariates$white_prob))))

column1_covariates <- merge(
  column1_covariates,
  column1_degree_enrichment[, .(user_id, has_transfer, has_associate, has_master,
                                 has_mba, has_doctor, transfer, any_grad)],
  by = "user_id", all.x = TRUE
)
cat(sprintf("Degree-enrichment match rate on Column 1: %.1f%%\n",
            100 * mean(!is.na(column1_covariates$transfer))))
# Expect ~100% -- column1_degree_enrichment.rds is keyed off the exact same
# input_col population Column 1 itself descends from (see
# memo1_01a_column1_construct.R's Section 1). Investigate if this comes in low.

# [FIXED 2026-08-10] standardize every binary covariate to integer 0/1 --
# has_associate/has_master/has_mba/has_doctor come in as logical
# (!is.na(...)) from column1_degree_enrichment.rds; `transfer` is already
# 0/1 here, listed for consistency with Column 2's identical block below.
column1_covariates[, `:=`(
  has_transfer  = as.integer(has_transfer),
  has_associate = as.integer(has_associate),
  has_master    = as.integer(has_master),
  has_mba       = as.integer(has_mba),
  has_doctor    = as.integer(has_doctor)
)]

# col_state already exists on column1_population (set at Section 2's
# birth-year-waterfall step, column1_merge[, col_state := state_abbr]) --
# the colleges.rds join here is only to attach census_region, not to
# recover col_state itself (Column 1 already has it natively, unlike what
# earlier planning notes assumed -- confirmed directly against the live
# column1_population.rds schema before writing this script).
column1_covariates <- merge(
  column1_covariates,
  state_region_crosswalk,
  by.x = "col_state", by.y = "state_abbr", all.x = TRUE
)
setnames(column1_covariates, "census_region", "col_region")
cat(sprintf("col_region match rate on Column 1: %.1f%%\n",
            100 * mean(!is.na(column1_covariates$col_region))))

# Age bucket, matching Code/memo1_02_acs_pulls.R's PUMS_YEAR-as-observation-year
# convention (see that script's Section 1 for the same caveat: this is an
# approximation, not a verified snapshot date). [CHANGED 2026-08-10]
# max_age=65 matches Code/memo1_02_acs_pulls.R's MAX_AGE trim exactly (per-column
# note there for the rationale) -- under-65 rows are dropped entirely, not
# just left with an NA age_bucket, so every row of this table reflects the
# trimmed population, not just the raking margin.
age_breaks <- seq(20, 65, by = 5)
max_age <- 65
pums_year <- 2022  # keep in sync with Code/memo1_02_acs_pulls.R's PUMS_YEAR
column1_covariates[, age_approx := pums_year - as.integer(birth)]
n_before_age_trim <- nrow(column1_covariates)
column1_covariates <- column1_covariates[age_approx >= 20 & age_approx < max_age]
cat(sprintf("Under-%d age trim on Column 1: %d of %d rows kept (%.1f%%)\n",
            max_age, nrow(column1_covariates), n_before_age_trim, 100 * nrow(column1_covariates) / n_before_age_trim))
column1_covariates[, age_bucket := cut(age_approx, breaks = age_breaks, right = FALSE, include.lowest = TRUE)]

# ---- Migration variables (new) -------------------------------------------

# col_cbsa_code: Column 1 never carries this natively (unlike Column 2,
# built at Code/05_merge.Rmd L73-93) -- resolve_college()'s default
# select_cols already includes col_fips, so this is the same two-step
# recipe Column 2 uses: era-correct county FIPS via resolve_college(), then
# a merge onto the county->CBSA crosswalk.
colleges_lookup <- readRDS(file.path(data_dir, "intermediate/colleges.rds"))
setDT(colleges_lookup)
column1_covariates <- resolve_college(column1_covariates, "col_unitid", "col_end", colleges_lookup,
                                       select_cols = "col_fips")
unified_cbsa <- fread(file.path(data_dir, "raw/census_geo/unified_cbsa.csv"))
# colleges.rds's col_fips is character; unified_cbsa.csv's GeoFIPS gets
# auto-parsed as integer by fread (both represent the same county FIPS
# code, leading-zero formatting aside) -- coerce to match.
column1_covariates[, col_fips := as.integer(col_fips)]
column1_covariates <- merge(column1_covariates, unified_cbsa, by.x = "col_fips", by.y = "GeoFIPS", all.x = TRUE)
setnames(column1_covariates, "cbsa_code", "col_cbsa_code")
cat(sprintf("col_cbsa_code match rate on Column 1: %.1f%%\n",
            100 * mean(!is.na(column1_covariates$col_cbsa_code))))
rm(colleges_lookup, unified_cbsa)

build_cbsa_ever_leave_yagan(column1_covariates, "col_cbsa_code", MIGRATION_WINDOW)
column1_covariates[, college_metro_status := college_metro_status(
  get(paste0("ever_leave_cbsa_col_", MIGRATION_WINDOW)), get(paste0("yagan_cbsa_col_", MIGRATION_WINDOW))
)]
column1_covariates[, yrs_away_col := count_years_away(column1_covariates, "col_cbsa_code", MIGRATION_WINDOW)]
cat("Column 1 college_metro_status distribution:\n")
print(column1_covariates[, .N, by = college_metro_status])

column1_covariates[, moved_last_year_state := last_year_state_move(column1_covariates)]
cat(sprintf("moved_last_year_state coverage on Column 1: %.1f%% (rest NA -- last two observed years not adjacent)\n",
            100 * mean(!is.na(column1_covariates$moved_last_year_state))))

column1_covariates[, age_at_grad := as.integer(col_end) - as.integer(birth)]
column1_covariates[, `:=`(
  grad_after_22 = as.integer(age_at_grad > 22),
  grad_after_26 = as.integer(age_at_grad > 26),
  grad_after_30 = as.integer(age_at_grad > 30)
)]

saveRDS(column1_covariates, column1_covariates_path)
log_step(paste("saved column1_covariates.rds:", nrow(column1_covariates), "rows"))

rm(column1_population, column1_demo, column1_degree_enrichment, column1_covariates, colleges)
gc()

} else {
  log_step("column1_covariates.rds already exists -- skipping")
}

# ==========================================================================
# Column 2
# ==========================================================================
column2_covariates_path <- file.path(data_dir, "intermediate/column2_covariates.rds")

if (!file.exists(column2_covariates_path)) {

log_step("Column 2 covariates starting")

column2 <- readRDS(file.path(data_dir, "intermediate/column2_regression_refresh.rds"))
setDT(column2)
stopifnot(all(c("user_id", "hs_state", "col_state", "birth", "transfer",
                 "col_cbsa_code", paste0(c("ever_leave_cbsa_col_", "yagan_cbsa_col_"), MIGRATION_WINDOW)) %in% names(column2)))
log_step(paste("column2_regression_refresh loaded:", nrow(column2), "users"))

# [FIXED 2026-08-10] `transfer` arrives here as a character factor label
# ("Non-Transfer"/"Transfer"), not the 0/1 integer Column 1's own `transfer`
# uses -- coerce to match, standardizing every binary covariate in this
# table to integer 0/1 (see the matching Column 1 block above).
stopifnot(all(unique(column2$transfer) %in% c("Non-Transfer", "Transfer")))
column2[, transfer := as.integer(transfer == "Transfer")]

both_demo <- readRDS(file.path(data_dir, "intermediate/both_demo.rds"))
setDT(both_demo)
stopifnot(all(c("user_id", RACE_PROB_COLS, "m_prob", "f_prob") %in% names(both_demo)))

both_final <- readRDS(file.path(data_dir, "intermediate/both_final.rds"))
setDT(both_final)
degree_cols <- unlist(lapply(
  c("associate", "master", "mba", "doctor"),
  function(p) paste0(p, c("_school", "_degree", "_end", "_opeid", "_unitid"))
))
stopifnot(all(c("user_id", degree_cols) %in% names(both_final)))

column2_covariates <- merge(
  column2,
  both_demo[, c("user_id", RACE_PROB_COLS, "m_prob", "f_prob"), with = FALSE],
  by = "user_id", all.x = TRUE
)
cat(sprintf("Demographic match rate on Column 2: %.1f%%\n",
            100 * mean(!is.na(column2_covariates$white_prob))))
# Expect ~96.5% (both_demo.rds's 2026-07-22 build vs. both_final.rds's
# 2026-08-09 rebuild -- see velvet-churning-galaxy.md's Step 2 plan; the
# user_id universe itself is unchanged, Step 0/1 were pure column additions).

cat(sprintf("Degree-detail (both_final.rds) match rate on Column 2: %.1f%%\n",
            100 * mean(column2_covariates$user_id %in% both_final$user_id)))
# Expect ~100% -- both_final.rds is column2_regression_refresh.rds's own
# direct ancestor in the same 2026-08-09 pipeline run.
column2_covariates <- merge(
  column2_covariates,
  both_final[, c("user_id", degree_cols), with = FALSE],
  by = "user_id", all.x = TRUE
)

column2_covariates[, `:=`(
  has_associate = as.integer(!is.na(associate_school)),
  has_master    = as.integer(!is.na(master_school)),
  has_mba       = as.integer(!is.na(mba_school)),
  has_doctor    = as.integer(!is.na(doctor_school))
)]
# `transfer` is already present natively on Column 2 (04_li_ed_pos.Rmd
# L167-171, coerced to 0/1 above); `any_grad` reuses demographics.R L92's
# exact definition, for consistency with Column 1's own derivation above.
column2_covariates[, any_grad := fifelse(has_master == 1L | has_mba == 1L | has_doctor == 1L, 1L, 0L)]

column2_covariates <- merge(
  column2_covariates, state_region_crosswalk,
  by.x = "hs_state", by.y = "state_abbr", all.x = TRUE
)
setnames(column2_covariates, "census_region", "hs_region")
column2_covariates <- merge(
  column2_covariates, state_region_crosswalk,
  by.x = "col_state", by.y = "state_abbr", all.x = TRUE
)
setnames(column2_covariates, "census_region", "col_region")
cat(sprintf("hs_region / col_region match rate on Column 2: %.1f%% / %.1f%%\n",
            100 * mean(!is.na(column2_covariates$hs_region)),
            100 * mean(!is.na(column2_covariates$col_region))))

# [CHANGED 2026-08-10] see Column 1's identical note above -- max_age=65
# matches Code/memo1_02_acs_pulls.R's MAX_AGE trim; rows dropped entirely, not just
# NA-bucketed.
age_breaks <- seq(20, 65, by = 5)
max_age <- 65
pums_year <- 2022  # keep in sync with Code/memo1_02_acs_pulls.R's PUMS_YEAR
column2_covariates[, age_approx := pums_year - as.integer(birth)]
n_before_age_trim <- nrow(column2_covariates)
column2_covariates <- column2_covariates[age_approx >= 20 & age_approx < max_age]
cat(sprintf("Under-%d age trim on Column 2: %d of %d rows kept (%.1f%%)\n",
            max_age, nrow(column2_covariates), n_before_age_trim, 100 * nrow(column2_covariates) / n_before_age_trim))
column2_covariates[, age_bucket := cut(age_approx, breaks = age_breaks, right = FALSE, include.lowest = TRUE)]

# ---- Migration variables (new) -------------------------------------------
# ever_leave_cbsa_col_10/yagan_cbsa_col_10 already exist natively on Column 2
# (Code/06_finalize_data.Rmd) -- only yrs_away/college_metro_status/
# moved_last_year_state/age-at-grad need building, mirroring Column 1 exactly.

column2_covariates[, college_metro_status := college_metro_status(
  get(paste0("ever_leave_cbsa_col_", MIGRATION_WINDOW)), get(paste0("yagan_cbsa_col_", MIGRATION_WINDOW))
)]
column2_covariates[, yrs_away_col := count_years_away(column2_covariates, "col_cbsa_code", MIGRATION_WINDOW)]
cat("Column 2 college_metro_status distribution:\n")
print(column2_covariates[, .N, by = college_metro_status])

column2_covariates[, moved_last_year_state := last_year_state_move(column2_covariates)]
cat(sprintf("moved_last_year_state coverage on Column 2: %.1f%% (rest NA -- last two observed years not adjacent)\n",
            100 * mean(!is.na(column2_covariates$moved_last_year_state))))

column2_covariates[, age_at_grad := as.integer(as.character(col_end)) - as.integer(birth)]
column2_covariates[, `:=`(
  grad_after_22 = as.integer(age_at_grad > 22),
  grad_after_26 = as.integer(age_at_grad > 26),
  grad_after_30 = as.integer(age_at_grad > 30)
)]

saveRDS(column2_covariates, column2_covariates_path)
log_step(paste("saved column2_covariates.rds:", nrow(column2_covariates), "rows"))

rm(column2, both_demo, both_final, column2_covariates)
gc()

} else {
  log_step("column2_covariates.rds already exists -- skipping")
}

log_step("SECTION 2 done. memo1_01b_covariates.R done.")
