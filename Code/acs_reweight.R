## =============================================================================
## acs_reweight.R (formerly Code/new/07_acs_reweight.R -- Code/new/ retired
## 2026-07-29, Phase C)
## State-level ACS PUMS reweighting of the LI sample (Item 2, referee 2's
## "try reweighting to ACS" ask). Floor scope only: state-level cells.
## County-level stretch goal is sketched but NOT implemented here (see the
## note at the bottom) because the join key needed to recover college-side
## county geography isn't available in any currently-persisted object -- see
## "STRETCH GOAL" section.
##
## Run this in the same long R session as Code/demographics.R (no source()
## anywhere in this repo, per convention) -- specifically AFTER
## Code/demographics.R (for both_demo.rds / the *_prob columns) has been run
## at least once so that file exists on disk.
##
## Table B (Section 9) additionally requires `regression` to already exist in
## memory, i.e. this script's Section 9 must be run in the same session AFTER
## Code/06_finalize_data.Rmd has executed through its measure_return/
## measure_leave loop. same_state_hs_10 / ever_leave_state_hs_10 are NOT
## persisted anywhere on disk -- they only exist transiently inside that Rmd's
## session. Sections 0-8 (everything through diagnostics table A) have no such
## dependency and can be run and evaluated on their own.
## =============================================================================


## -----------------------------------------------------------------------
## SECTION 0: config, libraries, parameters
## -----------------------------------------------------------------------

library(tidycensus)
library(tidyverse)
library(data.table)
library(survey)
library(dotenv)
library(here)

set.seed(49)

load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")

# Explicit key call -- Code/Old/06_census.R (archived)'s get_pums() call relies on some
# ambient cached key instead of reading .env directly; don't copy that.
# CENSUS_KEY is the case used in .env/.env.example (Code/intercensal.R reads
# a lowercase "census_key" for a *different* API wrapper -- that's a separate,
# pre-existing bug, left alone here).
tidycensus::census_api_key(Sys.getenv("CENSUS_KEY"))

# ---- Parameters you should sanity-check / adjust before running ----------

# ACS 5-year vintage (end year of the 5-year window). Pick the most recent
# vintage tidycensus actually has available at run time -- 2022 (2018-2022)
# is used here as a placeholder; confirm with
#   tidycensus::load_variables(2022, "acs5", cache = TRUE)
# or by just letting get_pums() below error if the year isn't live yet.
PUMS_YEAR <- 2022

# 5-year age buckets over the working-age range. Not sourced from any
# existing convention in this codebase (cohort.R works in single birth-years,
# not buckets) -- this is a new default, easy to change in one place.
AGE_BREAKS <- seq(20, 70, by = 5)

# Below this unweighted cell N, a PUMS cell is flagged as thin in the
# diagnostics printout (does not drop the cell, just flags it -- aggressive
# raking on thin cells is exactly what the Kish effective-N column in table A
# is there to catch).
SMALL_CELL_THRESHOLD <- 30

# 6-category race scheme, reused verbatim from Code/Old/06_census.R (archived) lines
# 145-151, and the *_prob column names from Code/demographics.R.
RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob",
                     "native_prob", "multiple_prob", "hispanic_prob")


## -----------------------------------------------------------------------
## SECTION 1: load the LI analytical sample
## -----------------------------------------------------------------------
## Code/regression_data.csv already carries user_id, hs_state, col_state,
## hs_fips, birth, and the un-horizoned same_state / in_state_col /
## yr_return_state columns -- this is a persisted snapshot of `regression`
## from an earlier point in Code/06_finalize_data.Rmd, taken before that
## script's measure_return()/measure_leave() loop adds the _hs_10 /
## ever_leave_* columns. It does NOT have unitid/opeid/hs_id, so it can't be
## rejoined to schools.rds/colleges.rds for county-level geography (see
## STRETCH GOAL note at the bottom) -- but state-level geography is already
## sitting right here, which is the floor's actual requirement.

li <- fread(file.path("Code", "regression_data.csv"))
# NOTE: this file lives directly under Code/, not Data/intermediate -- an
# existing inconsistency with this repo's raw/intermediate/results
# convention, not something this script tries to fix.

stopifnot(all(c("user_id", "hs_state", "col_state", "birth",
                 "same_state", "in_state_col") %in% names(li)))

cat(sprintf("Loaded LI sample: %d rows, %d unique user_id\n",
            nrow(li), uniqueN(li$user_id)))

# hs_state / col_state here are 2-letter postal abbreviations (confirm this
# assumption -- if they're already FIPS, the crosswalk below is a no-op you
# should remove). PUMS's ST/MIGSP are numeric-string FIPS codes, so we need a
# state abbreviation -> FIPS crosswalk to make the two sides joinable.
data(fips_codes, package = "tidycensus")
abb_to_fips <- unique(fips_codes[, c("state", "state_code")])
setDT(abb_to_fips)
setnames(abb_to_fips, c("state", "state_code"), c("state_abbr", "state_fips"))

li[abb_to_fips, on = c(hs_state = "state_abbr"), hs_state_fips := i.state_fips]
li[abb_to_fips, on = c(col_state = "state_abbr"), col_state_fips := i.state_fips]

cat(sprintf("hs_state -> FIPS match rate: %.1f%%\n",
            100 * mean(!is.na(li$hs_state_fips))))
cat(sprintf("col_state -> FIPS match rate: %.1f%%\n",
            100 * mean(!is.na(li$col_state_fips))))
# If either match rate is well under 100%, hs_state/col_state may not be
# plain 2-letter abbreviations -- inspect unique(li$hs_state) before
# proceeding rather than silently dropping unmatched rows downstream.


## -----------------------------------------------------------------------
## SECTION 2: attach LI-side demographic probabilities (fractional, not
## draw_category()'s stochastic draw) and birth-year-based age bucket
## -----------------------------------------------------------------------

demo <- readRDS(file.path(data_dir, "intermediate", "both_demo.rds"))
setDT(demo)
stopifnot(all(c("user_id", RACE_PROB_COLS, "m_prob", "f_prob") %in% names(demo)))

li <- demo[, c("user_id", RACE_PROB_COLS, "m_prob", "f_prob"), with = FALSE][li, on = "user_id"]

cat(sprintf("Demographic-probability match rate onto LI sample: %.1f%%\n",
            100 * mean(!is.na(li$white_prob))))
# Rows with no match keep NA *_prob columns rather than being dropped --
# they'll fall out of the race/sex-stratified rungs of the ladder but still
# count in the state-only rung.

# Age bucket: PUMS_YEAR is used as a stand-in "as-of" year for computing age
# from birth year, since the LI sample doesn't carry its own observation
# date here. This is an approximation, not a verified snapshot date -- if
# `birth` in regression_data.csv is graduation-cohort-adjusted rather than a
# literal birth year, this bucket will be off; spot-check a few rows against
# col_end (graduation year) - birth before trusting it.
li[, age_approx := PUMS_YEAR - as.integer(birth)]
li[, age_bucket := cut(age_approx, breaks = AGE_BREAKS, right = FALSE, include.lowest = TRUE)]

cat("LI age_bucket distribution:\n")
print(li[, .N, by = age_bucket][order(age_bucket)])


## -----------------------------------------------------------------------
## SECTION 3: pull ACS 5-year PUMS, one state at a time
## -----------------------------------------------------------------------
## Looping and filtering per-state instead of one national call, since the
## national file with replicate weights is large (order 15-20M rows) and
## this codebase already has at least one script commented "Runs with 50 GB
## on cluster" -- bounding peak memory to one state's raw pull at a time.

state_list <- c(state.abb, "DC")
pums_parts <- vector("list", length(state_list))

for (i in seq_along(state_list)) {
  st <- state_list[i]
  cat(sprintf("Pulling PUMS for %s (%d/%d)...\n", st, i, length(state_list)))

  pull <- get_pums(
    variables   = c("ST", "MIGSP", "MIG", "SCHL", "AGEP", "SEX", "RAC2P", "HISP", "ESR"),
    state       = st,
    survey      = "acs5",
    year        = PUMS_YEAR,
    rep_weights = "person",   # fetches PWGTP1-80; PWGTP comes back by default
    show_call   = FALSE
  )
  setDT(pull)

  # Filter immediately, matching Code/Old/06_census.R (archived)'s existing thresholds:
  # esr %in% c(1,2,3) (currently employed -- resolved decision, see plan),
  # SCHL > 20 (BA+), PWGTP < 10000 (drops a known PUMS weight-outlier
  # artifact, copied from 06_census.R for consistency).
  pull[, esr := as.integer(ESR)]
  pull <- pull[esr %in% c(1, 2, 3) & SCHL > 20 & PWGTP < 10000]

  pums_parts[[i]] <- pull
}

pums_filt <- rbindlist(pums_parts, use.names = TRUE, fill = TRUE)
rm(pums_parts)

cat(sprintf("PUMS filtered sample: %d rows across %d states (expect 51)\n",
            nrow(pums_filt), uniqueN(pums_filt$ST)))
cat("MIG distribution (mover-type recode, for reference only -- classification\n")
cat("below does NOT rely on MIG's exact codes, see Section 4):\n")
print(pums_filt[, .N, by = MIG])


## -----------------------------------------------------------------------
## SECTION 4: race recode (reused verbatim) + origin-state construction
## -----------------------------------------------------------------------

# Verbatim from Code/Old/06_census.R (archived) lines 145-151: HISP != "01" always maps
# to "hispanic" regardless of RAC2P; only HISP == "01" (not Hispanic) rows
# get split into white/black/asian/native/multiple by RAC2P.
pums_filt[, race := fcase(
  RAC2P == "1000" & HISP == "01", "white",
  RAC2P == "3000" & HISP == "01", "black",
  RAC2P %in% as.character(c(4000:4025, 7500:7508)) & HISP == "01", "asian",
  RAC2P %in% as.character(5000:5025) & HISP == "01", "native",
  RAC2P %in% c("8000", "9000") & HISP == "01", "multiple",
  default = "hispanic"
)]

pums_filt[, sex := fifelse(SEX == "1", "male", "female")]

# Origin-state: deliberately NOT keyed off MIG's exact category codes (the
# design note this script started from described MIG as "1=same house,
# 2=moved within state, 3=moved from different state, 4=from abroad", but
# that doesn't match the actual PUMS MIG codebook, which only distinguishes
# same-house / moved-domestically / moved-from-abroad -- it does not itself
# encode within- vs across-state). Within- vs across-state is recovered
# instead by directly comparing MIGSP (state 1 year ago, populated only for
# movers) against ST (current state):
#   - MIGSP populated and != ST  -> genuine interstate mover, origin = MIGSP
#   - otherwise (non-mover, intra-state mover, or MIGSP missing/NA)
#     -> origin = ST (current state), since by definition they didn't cross
#        a state line in the past year
pums_filt[, origin_state := fifelse(!is.na(MIGSP) & MIGSP != ST, MIGSP, ST)]

pums_filt[, age := as.integer(AGEP)]
pums_filt[, age_bucket := cut(age, breaks = AGE_BREAKS, right = FALSE, include.lowest = TRUE)]

# Out-of-state-mover flag, for the ACS benchmark row of diagnostics table A.
pums_filt[, moved_out_of_state := fifelse(!is.na(MIGSP) & MIGSP != ST, 1L, 0L)]

cat(sprintf("Interstate movers in PUMS sample: %.2f%% (sanity-check this is a\n",
            100 * mean(pums_filt$moved_out_of_state)))
cat("small share, not e.g. 50%+, which would indicate the MIGSP/ST logic above is wrong)\n")


## -----------------------------------------------------------------------
## SECTION 5: PUMS population-cell totals -- the weighting-scheme ladder
## -----------------------------------------------------------------------

cell_state <- pums_filt[, .(pop = sum(PWGTP)), by = .(origin_state)]

cell_state_age <- pums_filt[, .(pop = sum(PWGTP)), by = .(origin_state, age_bucket)]

cell_state_age_race_sex <- pums_filt[, .(pop = sum(PWGTP)), by = .(origin_state, age_bucket, race, sex)]

pums_cells <- list(
  state             = cell_state,
  state_age         = cell_state_age,
  state_age_racesex = cell_state_age_race_sex
)
saveRDS(pums_cells, file.path(data_dir, "intermediate", "pums_cells.rds"))
saveRDS(pums_filt, file.path(data_dir, "intermediate", "pums_acs5_filt.rds"))

flag_thin <- cell_state_age_race_sex[pop < SMALL_CELL_THRESHOLD]
cat(sprintf("%d of %d state x age x race x sex cells are thin (unweighted... actually\n",
            nrow(flag_thin), nrow(cell_state_age_race_sex)))
cat("pop-weighted; re-check with an unweighted N if this looks too permissive):\n")
print(head(flag_thin[order(pop)], 10))


## -----------------------------------------------------------------------
## SECTION 6: build weights on the LI side -- the same ladder
## -----------------------------------------------------------------------
## origin_state on the LI side = hs_state_fips (home state, the direct
## analogue of PUMS's reconstructed origin_state). Race is fractional, so
## rungs 4-5 use the row-expansion trick below rather than a hard category.

li[, origin_state := hs_state_fips]

# ---- Rung 1: unweighted ----
li[, w_unweighted := 1]

# ---- Rung 2: origin-state only (simple ratio weight) ----
li_state_n <- li[, .(n_li = .N), by = origin_state]
state_ratio <- cell_state[li_state_n, on = "origin_state"][, ratio := pop / n_li]
li[state_ratio, on = "origin_state", w_state := i.ratio]

# ---- Rung 3: + age bucket (survey::rake on two hard margins) ----
li_design <- svydesign(ids = ~1, weights = ~w_unweighted, data = li[!is.na(origin_state) & !is.na(age_bucket)])

pop_margin_state <- cell_state[, .(origin_state, Freq = pop)]
pop_margin_age   <- cell_state_age[, .(pop = sum(pop)), by = age_bucket][, .(age_bucket, Freq = pop)]

li_raked_age <- rake(
  li_design,
  sample.margins     = list(~origin_state, ~age_bucket),
  population.margins = list(pop_margin_state, pop_margin_age)
)
li[!is.na(origin_state) & !is.na(age_bucket), w_state_age := weights(li_raked_age)]

# ---- Rungs 4-5: + race, sex, via fractional row-expansion ----
# Each person contributes RACE_PROB_COLS as soft/fractional mass across the
# 6 race categories, rather than a single stochastic draw_category() pick
# (which would inject pure noise into the raking targets -- deliberate
# departure from Code/demographics.R's pattern, per the plan).
li_long <- melt(
  li[!is.na(origin_state) & !is.na(age_bucket) & !is.na(white_prob)],
  id.vars = setdiff(names(li), RACE_PROB_COLS),
  measure.vars = RACE_PROB_COLS,
  variable.name = "race_prob_col",
  value.name = "race_frac"
)
race_col_to_label <- setNames(RACE_LABELS, RACE_PROB_COLS)
li_long[, race := race_col_to_label[as.character(race_prob_col)]]
li_long[, sex := fifelse(m_prob >= f_prob, "male", "female")]
# NOTE: sex is taken as the higher-probability category rather than
# fractionally split -- m_prob/f_prob are typically near-degenerate (close
# to 0/1) so this loses little; flag if that assumption doesn't hold in
# practice (check summary(pmin(li$m_prob, li$f_prob))).
li_long[, w_base := w_unweighted * race_frac]

li_design_long <- svydesign(ids = ~1, weights = ~w_base, data = li_long)

pop_margin_racesex <- cell_state_age_race_sex[, .(pop = sum(pop)), by = .(race, sex)][, .(race, sex, Freq = pop)]

li_raked_racesex <- rake(
  li_design_long,
  sample.margins     = list(~origin_state, ~age_bucket, ~race, ~sex),
  population.margins = list(pop_margin_state, pop_margin_age,
                             pop_margin_racesex[, .(Freq = sum(Freq)), by = race],
                             pop_margin_racesex[, .(Freq = sum(Freq)), by = sex])
)
li_long[, w_state_age_racesex := weights(li_raked_racesex)]

# Collapse the fractional expansion back to one row per user_id by summing
# the expanded weight across that person's race pseudo-rows.
w_collapsed <- li_long[, .(w_state_age_racesex = sum(w_state_age_racesex)), by = user_id]
li[w_collapsed, on = "user_id", w_state_age_racesex := i.w_state_age_racesex]

# ---- Rung 5 ("full joint cell"): direct ratio to the joint PUMS cell ----
# Uses the same fractional expansion, but ratio-weights straight to the full
# state x age x race x sex joint cell rather than raking on marginals.
li_long_n <- li_long[, .(n_li = sum(race_frac)), by = .(origin_state, age_bucket, race, sex)]
joint_ratio <- cell_state_age_race_sex[li_long_n, on = c("origin_state", "age_bucket", "race", "sex")][, ratio := pop / n_li]
li_long[joint_ratio, on = c("origin_state", "age_bucket", "race", "sex"), w_full_joint := w_base * i.ratio]
w_full_collapsed <- li_long[, .(w_full_joint = sum(w_full_joint)), by = user_id]
li[w_full_collapsed, on = "user_id", w_full_joint := i.w_full_joint]

# ---- Sanity checks on the resulting weights ----
weight_cols <- c("w_unweighted", "w_state", "w_state_age", "w_state_age_racesex", "w_full_joint")
for (wc in weight_cols) {
  rng <- range(li[[wc]], na.rm = TRUE)
  cat(sprintf("%-22s range: [%.3f, %.3f]  (flag if outside ~0.05x-20x)\n", wc, rng[1], rng[2]))
}

saveRDS(li[, c("user_id", weight_cols), with = FALSE], file.path(data_dir, "intermediate", "li_weights.rds"))
saveRDS(li, file.path(data_dir, "intermediate", "ed_wide_geo.rds"))
# NOTE: despite the filename (kept for continuity with the plan doc's naming),
# this is `li` (Code/regression_data.csv + demographics), not a rebuild of
# d_sample_construction.R's ed_wide -- see Section 1's note.


## -----------------------------------------------------------------------
## SECTION 7: Kish effective sample size helper
## -----------------------------------------------------------------------

kish_n <- function(w) {
  w <- w[!is.na(w)]
  (sum(w))^2 / sum(w^2)
}

# Simple weighted-variance helper (population-style denominator) -- avoids
# pulling in Hmisc as a new dependency on top of survey/mipfp, which are
# already flagged as needing a fresh-install check once the R environment
# triage lands. Approximate, consistent with everything else on the LI side
# of these tables being approximate (LI has no replicate weights of its own).
wtd_var <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  x <- x[keep]; w <- w[keep]
  mu <- weighted.mean(x, w)
  sum(w * (x - mu)^2) / sum(w)
}

# weighted.mean()'s na.rm only drops NAs in x, not in w -- an NA weight
# (e.g. a race_prob from an unmatched demographics join) would silently
# propagate to NA for the whole statistic otherwise. Filter both explicitly.
wmean_safe <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  if (!any(keep)) return(NA_real_)
  weighted.mean(x[keep], w[keep])
}


## -----------------------------------------------------------------------
## SECTION 8: diagnostics table A -- validation against the ACS benchmark
## -----------------------------------------------------------------------
## LI-side proxy: 1 - in_state_col ("left home state for college"). This is
## NOT a 1-year retrospective flag the way PUMS's MIGSP-based measure is --
## it's a static, one-time transition -- so this table validates that
## reweighting moves LI's composition toward the ACS benchmark, it is not a
## claim that the two rates are measuring literally the same event. Say so
## in whatever write-up quotes this table.

li[, moved_proxy := 1 - in_state_col]

acs_benchmark_rate <- pums_filt[, weighted.mean(moved_out_of_state, PWGTP)]
# Design-based CI for the benchmark using the 80 replicate weights.
pums_rep_design <- svrepdesign(
  variables = pums_filt[, .(moved_out_of_state)],
  weights   = pums_filt$PWGTP,
  repweights = pums_filt[, paste0("PWGTP", 1:80), with = FALSE],
  type = "JK1", scale = 4/80, rscales = rep(1, 80), mse = TRUE
)
acs_benchmark_ci <- confint(svymean(~moved_out_of_state, pums_rep_design))

build_table_a_row <- function(w_col, label) {
  d <- li[!is.na(get(w_col)) & !is.na(moved_proxy)]
  w <- d[[w_col]]
  m <- weighted.mean(d$moved_proxy, w)
  se <- sqrt(wtd_var(d$moved_proxy, w) / kish_n(w))  # approximate; LI has no replicate weights of its own -- not on identical footing with the ACS CI, note this in any caption
  data.table(
    scheme = label,
    overall_rate = m,
    ci_lo = m - 1.96 * se,
    ci_hi = m + 1.96 * se,
    n_unweighted = nrow(d),
    n_eff_kish = kish_n(w),
    # Race/sex breakdown: race_prob is used as an ADDITIONAL weight
    # multiplier (fractional membership), not multiplied into the outcome --
    # weighted.mean(moved_proxy, w) vs. weighted.mean(moved_proxy, w * prob)
    # are not the same quantity, and only the latter is a race-conditional
    # rate.
    white    = wmean_safe(d$moved_proxy, w * d$white_prob),
    black    = wmean_safe(d$moved_proxy, w * d$black_prob),
    asian    = wmean_safe(d$moved_proxy, w * d$api_prob),
    native   = wmean_safe(d$moved_proxy, w * d$native_prob),
    multiple = wmean_safe(d$moved_proxy, w * d$multiple_prob),
    hispanic = wmean_safe(d$moved_proxy, w * d$hispanic_prob),
    male     = wmean_safe(d$moved_proxy, w * d$m_prob),
    female   = wmean_safe(d$moved_proxy, w * d$f_prob)
  )
}

table_a <- rbindlist(list(
  build_table_a_row("w_unweighted", "Unweighted"),
  build_table_a_row("w_state", "Origin-state only"),
  build_table_a_row("w_state_age", "+ age bucket"),
  build_table_a_row("w_state_age_racesex", "+ race, sex"),
  build_table_a_row("w_full_joint", "Full joint cell"),
  data.table(scheme = "ACS PUMS benchmark", overall_rate = acs_benchmark_rate,
             ci_lo = acs_benchmark_ci[1], ci_hi = acs_benchmark_ci[2],
             n_unweighted = nrow(pums_filt), n_eff_kish = NA_real_,
             white = NA, black = NA, asian = NA, native = NA, multiple = NA,
             hispanic = NA, male = NA, female = NA)
), fill = TRUE)

print(table_a)
fwrite(table_a, file.path(data_dir, "results", "reweight_diagnostics_A_validation.csv"))
saveRDS(table_a, file.path(data_dir, "results", "reweight_diagnostics_A_validation.rds"))

# ---- Mechanical gate check, per the plan: (A) must gate (B) ----
unweighted_gap <- abs(table_a[scheme == "Unweighted", overall_rate] - acs_benchmark_rate)
full_joint_gap <- abs(table_a[scheme == "Full joint cell", overall_rate] - acs_benchmark_rate)
gate_pass <- full_joint_gap < unweighted_gap
cat(sprintf("\nGATE CHECK: full-joint gap to ACS benchmark = %.4f, unweighted gap = %.4f -> %s\n",
            full_joint_gap, unweighted_gap,
            if (gate_pass) "PASS, reweighting is moving toward the benchmark -- ok to build table B"
            else "FAIL, reweighting is NOT moving toward the benchmark -- stop and debug before table B"))


## -----------------------------------------------------------------------
## SECTION 9: diagnostics table B -- the paper's actual outcome (STOP if
## table A's gate check above failed)
## -----------------------------------------------------------------------
## Requires `regression` to already exist in memory from a same-session run
## of Code/06_finalize_data.Rmd through its measure_return()/measure_leave()
## loop (these columns are never persisted to disk). If you haven't run that
## Rmd in this session, this section will stop with an informative error
## rather than silently computing nothing or the wrong thing.

if (!gate_pass) {
  stop("Table A gate check failed above -- do not proceed to table B until that's resolved.")
}

if (!exists("regression")) {
  stop(paste(
    "`regression` not found in this session.",
    "Run Code/06_finalize_data.Rmd through its measure_return()/measure_leave()",
    "loop (migration_ends, ~line 203-236) first, in this same R session --",
    "same_state_hs_10 / ever_leave_state_hs_10 are not persisted to disk anywhere."
  ))
}

expected_cols <- c("user_id", "same_state_hs_10", "ever_leave_state_hs_10")
missing_cols <- setdiff(expected_cols, names(regression))
if (length(missing_cols) > 0) {
  stop(paste0(
    "`regression` is missing expected column(s): ", paste(missing_cols, collapse = ", "),
    ". Available columns:\n", paste(names(regression), collapse = ", ")
  ))
}

reg_dt <- as.data.table(regression)
li_w <- readRDS(file.path(data_dir, "intermediate", "li_weights.rds"))
cat(sprintf("user_id match rate, li_weights -> regression: %.1f%%\n",
            100 * mean(reg_dt$user_id %in% li_w$user_id)))
# li_weights.rds comes from Code/regression_data.csv (an earlier snapshot of
# `regression`), while `regression` here is a fresh in-session build off the
# current microdata.csv -- these are not guaranteed to be the exact same
# user_id universe. A low match rate here means table B would be computed on
# a silently different sample than table A; investigate before trusting it.

reg_dt <- li_w[reg_dt, on = "user_id"]

build_table_b_row <- function(w_col, label) {
  d <- reg_dt[ever_leave_state_hs_10 == 1 & !is.na(get(w_col))]
  w <- d[[w_col]]
  m <- weighted.mean(d$same_state_hs_10, w)
  se <- sqrt(wtd_var(d$same_state_hs_10, w) / kish_n(w))
  data.table(scheme = label, rate_10yr_return = m,
             ci_lo = m - 1.96 * se, ci_hi = m + 1.96 * se,
             n_unweighted = nrow(d), n_eff_kish = kish_n(w))
}

table_b <- rbindlist(list(
  build_table_b_row("w_unweighted", "Unweighted"),
  build_table_b_row("w_state", "Origin-state only"),
  build_table_b_row("w_state_age", "+ age bucket"),
  build_table_b_row("w_state_age_racesex", "+ race, sex"),
  build_table_b_row("w_full_joint", "Full joint cell")
))

print(table_b)
fwrite(table_b, file.path(data_dir, "results", "reweight_diagnostics_B_outcome.csv"))
saveRDS(table_b, file.path(data_dir, "results", "reweight_diagnostics_B_outcome.rds"))


## -----------------------------------------------------------------------
## STRETCH GOAL (not implemented): county-level disaggregation
## -----------------------------------------------------------------------
## Blocked on a missing join key, discovered while writing this script:
## Code/regression_data.csv has hs_fips (county) but no col_fips, and no
## unitid/opeid/hs_id to rejoin against colleges.rds/schools.rds for the
## missing college-side county. Recovering col_fips requires tracing back
## through whatever join in Code/05_merge.Rmd (or earlier) first attached
## hs_state/hs_fips/col_state to this sample, to find where unitid/opeid is
## still present. Not attempted here -- do this only if the state-level
## floor above finishes with runway to spare, per the plan's own hard
## cut-line on the stretch goal.
