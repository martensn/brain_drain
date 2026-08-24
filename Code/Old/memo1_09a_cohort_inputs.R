# memo1_09a_cohort_inputs.R (was memo1_07a_cohort_inputs.R)
#
# [NEW 2026-08-12] Builds birth-cohort-restricted versions of the base
# inputs used throughout Memo 1, per Nicholas's request for two cuts of
# the whole analysis (born 1980-1989, born 1990-1999) instead of the full
# sample, which includes older adults he'd exclude from any Revelio-based
# analysis given how selected coverage gets at older ages.
#
# Standalone -- does not touch reweight_column2.R, memo1_covariates.R, or
# any of their production outputs. Reuses reweight_column2.R's manual_ipf()
# verbatim (copied, not sourced, per this project's convention) since a
# birth-cohort-restricted Revelio sample needs to be raked against a
# birth-cohort-restricted ACS margin, not the full-population one --
# filtering column2_reweighted.rds after the fact would rake against the
# wrong reference population. Full design rationale in
# D:\Users\martensn\.claude\plans\nope-i-had-something-logical-aurora.md.
#
# Birth year is exact on the Revelio side (a `birth` column already present
# on column1_covariates.rds/column2_covariates.rds, confirmed to survive
# unchanged from memo1_covariates.R -- that script has no cross-person
# aggregation before `birth` is attached, so filtering its cached output is
# equivalent to filtering the raw inputs first). ACS gives age, not birth
# year -- pums_acs5_filt.rds's Stage-1 margin uses a single fixed
# PUMS_YEAR=2022 reference (acs_pull.R), so the cohort-equivalent filter
# there is `2022 - AGEP`; pums_1yr_filt.rds spans multiple calendar years,
# so its filter is row-varying: `survey_year - AGEP`. Both are
# 1-year-precision approximations, unlike Revelio's exact `birth` -- a
# real, worth-noting asymmetry.
#
# Only rebuilds what Phase A/B calibration + the calendar-year charts need
# (w_full_joint, the ACS 1yr benchmark file) -- NOT reweight_column2.R's
# Part 3 characteristics table, out of scope per the approved plan.

library(data.table)
library(dotenv)
library(here)
library(tidycensus)

load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

PUMS_YEAR <- 2022  # keep in sync with Code/memo1_02a_acs_pull_5yr.R's PUMS_YEAR
RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob",
                     "native_prob", "multiple_prob", "hispanic_prob")

COHORTS <- list(
  born_1980s = c(1980, 1989),
  born_1990s = c(1990, 1999)
)

# ---- manual_ipf(): [2026-08-23] centralized to Code/memo1_ipf.R, was an
# independent copy here.
source(here::here("Code/memo1_ipf.R"))

## =========================================================================
## Load everything once (shared across both cohorts)
## =========================================================================

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

## =========================================================================
## Early sanity check: does the ACS migration benchmark actually shift up
## for younger cohorts, before investing in the full IPF/Phase-A/B build?
## Pooled across all available years (2008-2023, minus 2020) -- a quick
## gate, not the final per-calendar-year chart number.
## =========================================================================

full_acs_mig_rate <- pums_1yr_all[, weighted.mean(moved_out_of_state, PWGTP)]
cat(sprintf("\nFull-sample ACS 1yr migration rate (all birth years, pooled, reference): %.4f\n\n", full_acs_mig_rate))

## =========================================================================
## Per-cohort build
## =========================================================================

for (cohort_name in names(COHORTS)) {
  rng <- COHORTS[[cohort_name]]
  cat(sprintf("\n\n========== COHORT: %s (born %d-%d) ==========\n", cohort_name, rng[1], rng[2]))

  ## ---- Column 1 (no reweighting -- used unweighted throughout) ----
  col1_cohort <- column1_covariates[birth >= rng[1] & birth <= rng[2]]
  cat(sprintf("Column 1: %d of %d rows (%.1f%%)\n",
              nrow(col1_cohort), nrow(column1_covariates), 100 * nrow(col1_cohort) / nrow(column1_covariates)))
  saveRDS(col1_cohort, file.path(data_dir, sprintf("intermediate/column1_covariates_%s.rds", cohort_name)))

  ## ---- ACS 1yr benchmark (calendar-year charts + Phase A/B calibration) ----
  pums_1yr_cohort <- pums_1yr_all[birth_approx >= rng[1] & birth_approx <= rng[2]]
  cat(sprintf("ACS 1yr PUMS: %d of %d rows (%.1f%%)\n",
              nrow(pums_1yr_cohort), nrow(pums_1yr_all), 100 * nrow(pums_1yr_cohort) / nrow(pums_1yr_all)))
  cohort_acs_mig_rate <- pums_1yr_cohort[, weighted.mean(moved_out_of_state, PWGTP)]
  cat(sprintf("Cohort ACS 1yr migration rate (pooled): %.4f vs full-sample %.4f (%+.1f%% relative)\n",
              cohort_acs_mig_rate, full_acs_mig_rate, 100 * (cohort_acs_mig_rate / full_acs_mig_rate - 1)))
  saveRDS(pums_1yr_cohort, file.path(data_dir, sprintf("intermediate/pums_1yr_filt_%s.rds", cohort_name)))

  ## ---- Column 2 Stage-1 IPF, restricted to this cohort on BOTH sides ----
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

log_step("memo1_cohort_inputs.R done.")
