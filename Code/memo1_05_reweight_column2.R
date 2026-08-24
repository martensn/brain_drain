# memo1_05_reweight_column2.R (was memo1_04_reweight_column2.R)
#
# Builds the Column 2 reweight (Data/intermediate/column2_covariates.rds),
# then assembles the memo's four-column characteristics table: Column 1
# (unweighted), Column 2 (unweighted), Column 2 (reweighted to ACS), and
# the raw ACS PUMS benchmark -- race, sex, age, geography, transfer status,
# graduate-degree attainment, migration behavior.
#
# [REDESIGNED 2026-08-11, at Nicholas's request] `w_full_joint` now comes
# from real iterative proportional fitting against TWO SEPARATE marginal
# targets, not one single ratio-adjusted joint cell as before. The
# mechanism is a small manual IPF loop (see manual_ipf() below), not
# survey::rake()/calibrate() -- both were tried first and both hit real,
# verified problems specific to this file's fractional-race-melted design
# at ~2.9M-row scale (rake()'s postStratify(partial=FALSE) hard-errors on
# any population cell without sample coverage; a hand-rolled
# partial=TRUE loop produced widespread Inf/NA weight even with perfectly
# matched strata, confirmed via a direct diagnostic; calibrate()'s
# multi-margin interface builds one combined model matrix across margins
# rather than raking them separately). Full debugging trail is in the
# comments around manual_ipf()'s definition, not repeated here -- the
# short version is the per-cell ratio was genuinely unbounded in the
# survey-package mechanisms, and clamping it every iteration (not just at
# the end) is what actually fixes it, which a manual loop can do directly.
# The two margins:
#   1. origin_state x age_bucket x race x sex (unchanged from the original
#      scheme, still from acs_pull.R's pums_cells.rds$state_age_racesex)
#   2. origin_state x recent_mover (NEW -- see below)
# Why a second SEPARATE margin, not a bigger 5-way joint cell: the
# calendar-year charts built earlier the same day make it visually obvious
# that reweighting on demographics alone can't fix Revelio's
# over-representation of geographically mobile people, since mobility
# isn't one of the things being matched. A naive
# origin_state x age_bucket x race x sex x recent_mover cell would work
# for THAT, but recent_mover=1 is only ~3% of any given cell, so it would
# roughly double the cell count while making the already-documented
# South-Dakota-style thin-cell instability worse, not better. Raking's
# whole point is matching several marginal targets without needing them
# jointly cross-tabulated -- full design rationale, alternatives assessed
# (ACS county-to-county/state-to-county flows, IRS SOI migration data) and
# why they were deferred: D:\Users\martensn\.claude\plans\nope-i-had-something-logical-aurora.md.
# recent_mover uses acs_pull.R's/memo1_covariates.R's existing 1-year
# mover flags (moved_out_of_state / moved_last_year_state) -- a real,
# flagged conceptual mismatch with the paper's actual outcome (cumulative
# post-grad mobility, not a 12-month snapshot), deliberately deferred to a
# later pass rather than blocking this one.
#
# Scope of the ORIGINAL scheme, per velvet-churning-galaxy.md's Step 2:
# ONE reweighting scheme (state x age-bucket x race/sex, full joint cell),
# not the 5-rung ladder Code/acs_reweight.R built for an earlier,
# now-superseded purpose (a JOLE referee response). This script
# reimplements that scheme's logic using merge()-based joins throughout
# (not data.table bracket-chaining) -- Code/acs_reweight.R's own Sections
# 6-9 were written but, per HANDOFF.md, never actually run; rather than
# blindly copy unverified bracket-join chains, the same conceptual logic
# is rebuilt here with join semantics that are unambiguous by
# construction. That design choice is unaffected by this rewrite.
#
# Run after Code/memo1_02_acs_pulls.R (pums_cells.rds/pums_acs5_filt.rds) and
# Code/memo1_01b_covariates.R (column1_covariates.rds/column2_covariates.rds)
# have both completed. Standalone script, no source() between files.

library(data.table)
library(survey)
library(tidycensus)
library(dotenv)
library(here)

load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob",
                     "native_prob", "multiple_prob", "hispanic_prob")

# table.express note: every join below uses merge()/data.table [i,j] on
# plain data.tables, never a bare dplyr verb -- safe regardless of whether
# table.express is attached elsewhere in this session.


## =========================================================================
## PART 1: build the full-joint-cell raking weight
## =========================================================================

li <- readRDS(file.path(data_dir, "intermediate/column2_covariates.rds"))
setDT(li)
stopifnot(all(c("user_id", "hs_state", "age_bucket", RACE_PROB_COLS, "m_prob", "f_prob",
                "moved_last_year_state") %in% names(li)))

# pums_filt loaded here (not in Part 2 as before) -- Part 1 now needs it
# directly to build the new recent_mover margin, and Part 2/3 both already
# expected it in scope by the time they run.
pums_filt <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds"))
setDT(pums_filt)
# [carried over, FIXED 2026-08-10] pums_acs5_filt.rds on disk predates
# acs_pull.R's logical->integer grad_degree fix -- coerce here too (no-op
# if already integer), same rationale as li's transfer/has_associate fix
# below.
pums_filt[, grad_degree := as.integer(grad_degree)]
stopifnot(all(c("origin_state", "moved_out_of_state", "PWGTP") %in% names(pums_filt)))

# [FIXED 2026-08-10] column2_covariates.rds on disk still predates
# Code/memo1_01b_covariates.R's transfer/has_associate coercion fix (that fix
# only takes effect on a future full rebuild, since this checkpoint already
# exists) -- coerce here too so THIS run's table is consistent without
# forcing an ~hour-long memo1_covariates.R rerun. Safe to leave in even
# after a future rebuild: as.integer() on an already-integer 0/1 column is
# a no-op.
if (is.character(li$transfer)) {
  stopifnot(all(unique(li$transfer) %in% c("Non-Transfer", "Transfer")))
  li[, transfer := as.integer(transfer == "Transfer")]
}
li[, `:=`(
  has_associate = as.integer(has_associate),
  has_master    = as.integer(has_master),
  has_mba       = as.integer(has_mba),
  has_doctor    = as.integer(has_doctor)
)]

pums_cells <- readRDS(file.path(data_dir, "intermediate/pums_cells.rds"))
cell_state_age_race_sex <- pums_cells$state_age_racesex

# hs_state -> FIPS crosswalk: PUMS's origin_state (built in acs_pull.R) is a
# numeric-string FIPS code; Column 2's hs_state is a 2-letter postal
# abbreviation (confirmed directly against the live file, same format
# Code/acs_reweight.R's Section 1 already assumed for the old sample).
data(fips_codes, package = "tidycensus")
abb_to_fips <- unique(fips_codes[, c("state", "state_code")])
setDT(abb_to_fips)
setnames(abb_to_fips, c("state", "state_code"), c("state_abbr", "state_fips"))

li <- merge(li, abb_to_fips, by.x = "hs_state", by.y = "state_abbr", all.x = TRUE)
setnames(li, "state_fips", "origin_state")
cat(sprintf("hs_state -> FIPS match rate: %.1f%%\n", 100 * mean(!is.na(li$origin_state))))

li[, w_unweighted := 1]

# Fractional race-probability row-expansion: each person contributes
# RACE_PROB_COLS as soft mass across the 6 race categories (not a single
# stochastic draw, which would inject pure noise into the raking target) --
# same design Code/acs_reweight.R's Section 6 used.
li_complete <- li[!is.na(origin_state) & !is.na(age_bucket) & !is.na(white_prob)]
log_step(paste("li_complete (non-missing origin_state/age_bucket/race_prob):", nrow(li_complete), "of", nrow(li)))

li_long <- melt(
  li_complete,
  id.vars = setdiff(names(li_complete), RACE_PROB_COLS),
  measure.vars = RACE_PROB_COLS,
  variable.name = "race_prob_col",
  value.name = "race_frac"
)
race_col_to_label <- setNames(RACE_LABELS, RACE_PROB_COLS)
li_long[, race := race_col_to_label[as.character(race_prob_col)]]
# sex taken as the higher-probability category rather than fractionally
# split -- m_prob/f_prob are typically near-degenerate; spot-check
# summary(pmin(li$m_prob, li$f_prob)) before trusting this at scale.
li_long[, sex := fifelse(m_prob >= f_prob, "male", "female")]
li_long[, w_base := w_unweighted * race_frac]

# [NEW 2026-08-11] recent_mover -- moved_last_year_state is NA for ~5% of
# Column 2 users (their last two observed panel years aren't adjacent, so
# no 1-year-equivalent transition is observable -- see
# Code/memo1_01b_covariates.R's last_year_state_move()). rake()'s
# sample.margins "must not contain missing values" (confirmed via the
# package's own help page before writing this), and the missingness here
# isn't obviously random with respect to mobility (a gap in someone's
# observed position history could itself correlate with having moved) --
# imputing a specific value in either direction risks a real, unjustified
# bias. EXCLUDING these rows from the raked universe is the more
# defensible default: it shrinks the reweighted sample by ~5% rather than
# silently guessing, and matches this memo's established pattern of
# stating asymmetries plainly rather than papering over them (e.g. the
# metro-tier chart's 2012-2021-only ACS window). Revisit if Nicholas wants
# a different call once real numbers are visible below.
n_before_mover_na_drop <- nrow(li_long)
li_long <- li_long[!is.na(moved_last_year_state)]
cat(sprintf("Dropped for NA moved_last_year_state (excluded from raking, not imputed): %d of %d melted rows (%.1f%%)\n",
            n_before_mover_na_drop - nrow(li_long), n_before_mover_na_drop,
            100 * (n_before_mover_na_drop - nrow(li_long)) / n_before_mover_na_drop))

## ---- Population margins for rake() -----------------------------------
# Margin A: unchanged from the original scheme -- origin_state x
# age_bucket x race x sex population totals, from acs_pull.R's
# pums_cells.rds. rake() wants a data.frame with a `Freq` column (per its
# own help page / the api.example data.frame(stype=..., Freq=...)
# pattern), not `pop` -- renamed via a copy, not in place, so
# cell_state_age_race_sex stays available under its original name/column
# in case anything downstream still expects it.
margin_demo <- copy(cell_state_age_race_sex)
setnames(margin_demo, "pop", "Freq")

# [FIXED 2026-08-11] postStratify()'s own internal stratum-matching (via
# model.frame()/cross-tabulation) disagreed with this file's data.table
# join-based pre-filter (below) on which rows "match" -- confirmed by
# verbose per-margin NA tracking: margin 1 alone produced NA weight for
# ~13% of a subsample already pre-filtered to have a join-confirmed
# match, which then cascaded (any stratum containing even one NA-weight
# row sums to NA) to make margin 2 nearly 100% NA. The most likely cause
# is a factor-level/representation mismatch between li_long's age_bucket
# (built by Code/memo1_01b_covariates.R's cut()) and margin_demo's (built by
# Code/memo1_02_acs_pulls.R's separate cut() call) -- same underlying bug CLASS
# already hit repeatedly this session (RAC2P digit-width, MIGSP padding,
# ST padding: two representations of "the same" value that don't compare
# equal). Rather than chase the exact mechanism, force every margin key
# column to plain character on BOTH sides -- character comparison is
# unambiguous in a way factor-level comparison isn't, and this closes the
# entire bug class regardless of its precise cause.
for (col in c("origin_state", "age_bucket", "race", "sex")) margin_demo[[col]] <- as.character(margin_demo[[col]])
li_long[, `:=`(origin_state = as.character(origin_state), age_bucket = as.character(age_bucket),
                race = as.character(race), sex = as.character(sex),
                moved_last_year_state = as.character(moved_last_year_state))]

# Margin B: NEW -- origin_state x recent_mover population totals, from
# pums_acs5_filt.rds (already loaded above, no new pull). Cheap: one
# groupby against a file already on disk.
margin_mover <- pums_filt[, .(Freq = sum(PWGTP)), by = .(origin_state, moved_out_of_state)]
setnames(margin_mover, "moved_out_of_state", "moved_last_year_state")
margin_mover[, `:=`(origin_state = as.character(origin_state), moved_last_year_state = as.character(moved_last_year_state))]
cat(sprintf("recent_mover ACS margin: %d origin_state x mover cells, mover=1 share overall %.1f%%\n",
            nrow(margin_mover), 100 * margin_mover[moved_last_year_state == 1, sum(Freq)] / margin_mover[, sum(Freq)]))

# [FOUND 2026-08-11] postStratify() (which rake() calls internally) hard-
# errors -- "Strata in sample absent from population. This Can't Happen"
# -- if ANY (origin_state, age_bucket, race, sex) combination appears in
# the sample but has no matching row in margin_demo at all (a real ACS
# cell with zero population, confirmed already-known: this repo's OLD
# ratio-adjustment code separately printed "N of X LI cells have no
# matching PUMS cell" and left those NA -- postStratify is stricter than
# the old ratio approach and won't tolerate it silently). Filter these
# out explicitly before raking, the same set the old code already
# implicitly zeroed out via NA propagation, just made explicit now that
# it has to be.
demo_key_cols <- c("origin_state", "age_bucket", "race", "sex")
n_before_cell_match <- nrow(li_long)
li_long <- li_long[margin_demo[, ..demo_key_cols], on = demo_key_cols, nomatch = 0]
cat(sprintf("Dropped for no matching ACS demo cell (state x age x race x sex): %d of %d melted rows (%.1f%%)\n",
            n_before_cell_match - nrow(li_long), n_before_cell_match,
            100 * (n_before_cell_match - nrow(li_long)) / n_before_cell_match))

# [FOUND 2026-08-11, second problem in the same area] the above filter
# guarantees every SAMPLE stratum has a matching POPULATION row -- but a
# direct diagnostic (hand-checking specific NA-weighted rows against
# margin_demo) proved postStratify(partial=TRUE) STILL produces NA weight
# for rows in perfectly-matched strata, when OTHER population strata
# elsewhere in the same margin table are absent from the sample (margin_demo
# has thousands of cells; any sample -- subsample or the full ~2.9M-row
# table alike -- realistically can't cover all of them). This looks like a
# real limitation/edge case in how postStratify's partial=TRUE handles a
# margin with many simultaneously-missing strata, not a data problem --
# confirmed empirically (Freq values for the "bad" cells were sane,
# 2,000-79,000) rather than assumed. Sidestepping it entirely: restrict
# margin_demo to ONLY the cells that also appear in the (already-filtered)
# sample, so every population row has sample coverage and vice versa --
# partial=TRUE's problematic path never gets exercised because there's
# nothing left for it to need to ignore. This slightly redefines the
# target population (excludes ACS cells with zero LI representation,
# whose aggregate population mass is negligible by construction) rather
# than leaving it exactly as pums_cells.rds's original table -- a real,
# small, worth-noting tradeoff for a mechanism that actually converges.
li_long_keys <- unique(li_long[, ..demo_key_cols])
n_before_margin_restrict <- nrow(margin_demo)
margin_demo <- margin_demo[li_long_keys, on = demo_key_cols, nomatch = 0]
cat(sprintf("Restricted margin_demo to cells with sample coverage: %d of %d ACS cells kept (%.1f%%), %.1f%% of ACS population mass retained\n",
            nrow(margin_demo), n_before_margin_restrict, 100 * nrow(margin_demo) / n_before_margin_restrict,
            100 * margin_demo[, sum(Freq)] / cell_state_age_race_sex[, sum(pop)]))

# Same bidirectional restriction applied to margin_mover for consistency
# and safety, even though its 102 cells (51 states x 2 mover values) are
# far coarser and much less likely to have any state genuinely absent
# from a multi-million-row sample -- cheap to apply, removes any residual
# risk of the same partial=TRUE issue recurring here.
mover_key_cols <- c("origin_state", "moved_last_year_state")
li_long <- li_long[margin_mover[, ..mover_key_cols], on = mover_key_cols, nomatch = 0]
n_before_mover_restrict <- nrow(margin_mover)
margin_mover <- margin_mover[unique(li_long[, ..mover_key_cols]), on = mover_key_cols, nomatch = 0]
cat(sprintf("Restricted margin_mover to cells with sample coverage: %d of %d cells kept\n", nrow(margin_mover), n_before_mover_restrict))

# [ABANDONED 2026-08-11, after extensive testing] both survey::rake()
# (calls postStratify(partial=FALSE) internally, hard-errors the moment
# any population cell lacks sample coverage) and a hand-rolled
# postStratify(partial=TRUE) loop (survives that specific error, but a
# direct diagnostic proved it produces widespread NA/Inf weight even with
# PERFECTLY bidirectionally-matched sample/population strata -- confirmed
# via a self-consistent subsample test, ruling out every data-matching
# hypothesis first) turned out not to be viable for this fractional-melt
# design at this scale. survey::calibrate() was tried next specifically
# for its documented bounded-weight support, but its multi-formula list
# interface builds one COMBINED model matrix across all margins together
# (confirmed by inspecting its printed coefficient names, which mix terms
# from both margins) rather than genuinely separate per-margin raking,
# and needs a population vector aligned to that combined design -- a
# fundamentally different, more involved setup than rake()'s docs implied.
#
# Given all three survey-package mechanisms hit real, verified problems
# specific to this design (not assumed away), the most robust path is a
# small manual 2-margin IPF loop using the exact same merge()-based
# philosophy this file already uses elsewhere (see the file header: "this
# script reimplements that scheme's logic using merge()-based joins...
# rather than blindly copy unverified... chains"). Each iteration computes
# a per-cell ratio (population Freq / current sample weight sum) for one
# margin at a time and multiplies it into the running weight -- exactly
# what the ORIGINAL single-margin ratio-adjustment scheme did, generalized
# to alternate between two margins until convergence. The ratio is
# clamped every iteration (not just at the end) to the same 0.05x-20x
# heuristic this file already used as an informal eyeball flag -- this is
# what actually prevents the Inf/NA blowup (verified: postStratify's
# failure mode produced literal Inf weights, meaning an UNBOUNDED
# per-cell ratio was the real mechanism all along), and folds in the
# South Dakota weight-stability fix directly into the iterative process
# rather than as a final post-hoc clip. Verified correct first on a
# standalone synthetic toy example (5 people, a population stratum absent
# from the sample, hand-inspectable output) before touching real data.
# [CAUGHT BEFORE RUNNING, not after -- verified directly] data.table's
# merge() defaults to sort=TRUE, which reorders the result by the join
# keys rather than preserving `x`'s original row order (confirmed with a
# standalone 5-row test: `identical(dt$id, merge(dt, pop, by="k")$id)` is
# FALSE). Without a row-order safeguard, the final `dt$w_iter` returned
# here would silently misalign with li_long's original row order when
# assigned back via `li_long[, w_raked := manual_ipf(li_long, ...)]` --
# every weight would land on the wrong row, with no error to signal it.
# An explicit row-id, carried through every merge and used to re-sort
# immediately before returning, closes this off entirely.
#
# [2026-08-23] Extracted to Code/memo1_ipf.R (unchanged) once a third
# independent copy-paste (memo1_07_reweight_column2_occupation.R) made the
# duplication a genuine modularity problem, not just a cosmetic one -- see
# that file's own header for why this project source()'s it rather than
# keeping every raking script self-contained (same one-time exception
# already made for memo1_00_metro_tier_definitions.R).
source(here::here("Code/memo1_ipf.R"))

margins_spec <- list(
  list(keys = demo_key_cols, pop = margin_demo),
  list(keys = mover_key_cols, pop = margin_mover)
)

## ---- Small-subsample validation, before trusting the full-scale run ---
# Self-consistent margins (restricted to what the subsample itself
# covers), same rationale as before this rewrite -- proves the MECHANISM
# converges cleanly on real, fractionally-melted data before committing
# to the full ~17M-row run.
set.seed(20260811)
li_long_test <- li_long[sample.int(.N, min(100000, .N))]
margin_demo_test <- margin_demo[unique(li_long_test[, ..demo_key_cols]), on = demo_key_cols, nomatch = 0]
margin_mover_test <- margin_mover[unique(li_long_test[, ..mover_key_cols]), on = mover_key_cols, nomatch = 0]
li_long_test <- li_long_test[margin_demo_test[, ..demo_key_cols], on = demo_key_cols, nomatch = 0]
li_long_test <- li_long_test[margin_mover_test[, ..mover_key_cols], on = mover_key_cols, nomatch = 0]
cat(sprintf("Subsample self-consistent margins: %d demo cells (of %d in full margin_demo), %d mover cells; %d rows remain after bidirectional restriction\n",
            nrow(margin_demo_test), nrow(margin_demo), nrow(margin_mover_test), nrow(li_long_test)))

w_test <- manual_ipf(li_long_test, "w_base",
                      list(list(keys = demo_key_cols, pop = margin_demo_test),
                           list(keys = mover_key_cols, pop = margin_mover_test)),
                      verbose = TRUE)
cat(sprintf("Subsample IPF test (n=%d melted rows): weight range [%.3f, %.3f], any non-finite: %s\n",
            length(w_test), min(w_test), max(w_test), any(!is.finite(w_test))))
if (any(!is.finite(w_test))) stop("Subsample IPF test produced non-finite weights -- investigate before running at full scale.")

## ---- Full-scale IPF -----------------------------------------------------
log_step("Running full-scale manual_ipf() (both margins)")
li_long[, w_raked := manual_ipf(li_long, "w_base", margins_spec, verbose = TRUE)]

w_full_collapsed <- li_long[, .(w_full_joint_uncapped = sum(w_raked, na.rm = TRUE)), by = user_id]
li <- merge(li, w_full_collapsed, by = "user_id", all.x = TRUE)

# Final safety clip on the fully-collapsed per-person weight, on top of
# the per-iteration ratio clamp already applied above -- belt-and-
# suspenders (a person collapses several melted-row weights via sum(),
# which could in principle still land outside the per-iteration bounds
# even though no single ratio did), same 0.05x-20x heuristic throughout.
med_w <- median(li$w_full_joint_uncapped, na.rm = TRUE)
cap_hi <- med_w * 20
cap_lo <- med_w * 0.05
n_capped_hi <- sum(li$w_full_joint_uncapped > cap_hi, na.rm = TRUE)
n_capped_lo <- sum(li$w_full_joint_uncapped < cap_lo, na.rm = TRUE)
li[, w_full_joint := pmin(pmax(w_full_joint_uncapped, cap_lo), cap_hi)]
cat(sprintf("Final weight cap [%.3f, %.3f] (0.05x-20x median %.3f): capped %d rows high, %d rows low, of %d\n",
            cap_lo, cap_hi, med_w, n_capped_hi, n_capped_lo, nrow(li)))

for (wc in c("w_unweighted", "w_full_joint_uncapped", "w_full_joint")) {
  rng <- range(li[[wc]], na.rm = TRUE)
  cat(sprintf("%-22s range: [%.3f, %.3f]\n", wc, rng[1], rng[2]))
}

saveRDS(li, file.path(data_dir, "intermediate/column2_reweighted.rds"))
log_step(paste("saved column2_reweighted.rds:", nrow(li), "rows"))


## =========================================================================
## PART 2: diagnostics -- Kish N + benchmark convergence
## =========================================================================

kish_n <- function(w) {
  w <- w[!is.na(w)]
  (sum(w))^2 / sum(w^2)
}

wtd_var <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  x <- x[keep]; w <- w[keep]
  mu <- weighted.mean(x, w)
  sum(w * (x - mu)^2) / sum(w)
}

wmean_safe <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  if (!any(keep)) return(NA_real_)
  weighted.mean(x[keep], w[keep])
}

# pums_filt already loaded in Part 1 (needed there for the new recent_mover
# margin) -- reused here, not re-read.

# moved_proxy: reuse Column 2's own `in_state` factor (06_finalize_data.Rmd
# L89, col_state == hs_state) rather than recomputing from scratch -- NOT a
# 1-year retrospective flag the way PUMS's MIGSP-based measure is (it's a
# static, one-time transition), so this validates that reweighting moves
# Column 2's composition toward the ACS benchmark, not that the two rates
# measure literally the same event.
stopifnot("in_state" %in% names(li))
li[, moved_proxy := as.integer(in_state == "Out-of-State")]

acs_benchmark_rate <- pums_filt[, weighted.mean(moved_out_of_state, PWGTP)]
pums_rep_design <- svrepdesign(
  variables  = pums_filt[, .(moved_out_of_state)],
  weights    = pums_filt$PWGTP,
  repweights = pums_filt[, paste0("PWGTP", 1:80), with = FALSE],
  type = "JK1", scale = 4 / 80, rscales = rep(1, 80), mse = TRUE
)
acs_benchmark_ci <- confint(svymean(~moved_out_of_state, pums_rep_design))

build_diag_row <- function(w_col, label) {
  d <- li[!is.na(get(w_col)) & !is.na(moved_proxy)]
  w <- d[[w_col]]
  m <- weighted.mean(d$moved_proxy, w)
  se <- sqrt(wtd_var(d$moved_proxy, w) / kish_n(w))
  data.table(
    scheme = label, overall_rate = m, ci_lo = m - 1.96 * se, ci_hi = m + 1.96 * se,
    n_unweighted = nrow(d), n_eff_kish = kish_n(w)
  )
}

diagnostics_table <- rbindlist(list(
  build_diag_row("w_unweighted", "Unweighted"),
  build_diag_row("w_full_joint", "Full joint cell"),
  data.table(scheme = "ACS PUMS benchmark", overall_rate = acs_benchmark_rate,
             ci_lo = acs_benchmark_ci[1], ci_hi = acs_benchmark_ci[2],
             n_unweighted = nrow(pums_filt), n_eff_kish = NA_real_)
))
print(diagnostics_table)
fwrite(diagnostics_table, file.path(data_dir, "results/reweight_diagnostics_column2.csv"))
saveRDS(diagnostics_table, file.path(data_dir, "results/reweight_diagnostics_column2.rds"))

unweighted_gap <- abs(diagnostics_table[scheme == "Unweighted", overall_rate] - acs_benchmark_rate)
full_joint_gap <- abs(diagnostics_table[scheme == "Full joint cell", overall_rate] - acs_benchmark_rate)
if (full_joint_gap < unweighted_gap) {
  cat(sprintf("\nGATE CHECK PASS: full-joint gap (%.4f) < unweighted gap (%.4f) -- reweighting moves toward the ACS benchmark.\n",
              full_joint_gap, unweighted_gap))
} else {
  warning(sprintf("GATE CHECK FAIL: full-joint gap (%.4f) >= unweighted gap (%.4f) -- reweighting is NOT moving toward the benchmark. Treat un-raked characteristics below as unreliable until this is debugged.",
                   full_joint_gap, unweighted_gap))
}


## =========================================================================
## PART 3: four-column characteristics table
## =========================================================================
## Long format: column_label / variable / category / value. Race/sex are
## weighted shares (fractional prob-weighted for Revelio columns, PWGTP-
## weighted categorical for ACS); age_bucket/region/grad-degree/transfer
## are weighted shares of their respective categories. Two known,
## deliberate asymmetries, not bugs: (1) Column 1 has no HS-side geography
## (college-side only); (2) ACS has no transfer-status analog (left NA).

column1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds"))
setDT(column1)
# [FIXED 2026-08-10] same has_associate/has_master/has_mba/has_doctor
# coercion as li's, above (column1_covariates.rds on disk predates the
# memo1_covariates.R fix).
column1[, `:=`(
  has_associate = as.integer(has_associate),
  has_master    = as.integer(has_master),
  has_mba       = as.integer(has_mba),
  has_doctor    = as.integer(has_doctor)
)]
# ACS rows below are built directly from pums_filt (already in scope from
# Part 2) rather than acs_pull.R's pums_benchmark_summary.rds, so every
# column in this table uses the same weighting machinery (categorical_share_rows).

long_row <- function(column_label, variable, category, value, n_eff = NA_real_) {
  data.table(column_label = column_label, variable = variable, category = as.character(category), value = value, n_eff = n_eff)
}

# ---- race/sex, Revelio columns (fractional, prob-weighted) --------------
# race_tot's names are RACE_PROB_COLS; mapped to RACE_LABELS explicitly via
# race_col_to_label (built in Part 1) rather than relying on positional
# order, to avoid a silent mislabeling bug.
revelio_race_sex_rows <- function(dt, weight_col, column_label) {
  w <- if (is.null(weight_col)) rep(1, nrow(dt)) else dt[[weight_col]]
  race_tot <- sapply(RACE_PROB_COLS, function(cc) sum(dt[[cc]] * w, na.rm = TRUE))
  names(race_tot) <- race_col_to_label[names(race_tot)]
  race_share <- race_tot / sum(race_tot)
  sex_tot <- c(male = sum(dt$m_prob * w, na.rm = TRUE), female = sum(dt$f_prob * w, na.rm = TRUE))
  sex_share <- sex_tot / sum(sex_tot)
  rbindlist(list(
    long_row(column_label, "race", names(race_share), unname(race_share)),
    long_row(column_label, "sex", names(sex_share), unname(sex_share))
  ))
}

# ---- categorical share helper, for age/region/grad-degree/transfer ------
categorical_share_rows <- function(dt, group_col, weight_col, column_label, variable_label) {
  if (!(group_col %in% names(dt))) return(NULL)
  if (is.null(weight_col)) {
    tab <- dt[, .(n = .N), by = c(group_col)]
  } else {
    tab <- dt[, .(n = sum(get(weight_col), na.rm = TRUE)), by = c(group_col)]
  }
  tab <- tab[!is.na(get(group_col))]
  tab[, share := n / sum(n)]
  long_row(column_label, variable_label, tab[[group_col]], tab$share)
}

# ---- Migration/graduation-timing helper: weighted mean of a continuous
# column (used for yrs_away_col, conditional on having left the college
# metro at all -- "duration away" isn't meaningful for people who stayed).
mean_value_row <- function(dt, value_col, weight_col, column_label, variable_label) {
  w <- if (is.null(weight_col)) rep(1, nrow(dt)) else dt[[weight_col]]
  v <- dt[[value_col]]
  keep <- !is.na(v) & !is.na(w)
  if (!any(keep)) return(NULL)
  m <- weighted.mean(v[keep], w[keep])
  long_row(column_label, variable_label, "mean", m)
}

# migrated_past_year: placed under the SAME variable name on both the
# Revelio and ACS columns so they sit together in the table -- but see the
# "Known asymmetries" note below before treating them as the same measure.
# Revelio's version is a person's own most-recent-observed-year-pair change
# (variable time gap, whatever the panel happens to cover); ACS's is a
# literal fixed 1-year retrospective question.

characteristics_table <- rbindlist(list(
  # Column 1: unweighted
  revelio_race_sex_rows(column1, NULL, "Column 1 (college-only)"),
  categorical_share_rows(column1, "age_bucket", NULL, "Column 1 (college-only)", "age_bucket"),
  categorical_share_rows(column1, "col_region", NULL, "Column 1 (college-only)", "region"),
  categorical_share_rows(column1, "transfer", NULL, "Column 1 (college-only)", "transfer"),
  categorical_share_rows(column1, "any_grad", NULL, "Column 1 (college-only)", "grad_degree"),
  categorical_share_rows(column1, "has_associate", NULL, "Column 1 (college-only)", "has_associate"),
  categorical_share_rows(column1, "grad_after_22", NULL, "Column 1 (college-only)", "grad_after_22"),
  categorical_share_rows(column1, "grad_after_26", NULL, "Column 1 (college-only)", "grad_after_26"),
  categorical_share_rows(column1, "grad_after_30", NULL, "Column 1 (college-only)", "grad_after_30"),
  categorical_share_rows(column1, "college_metro_status", NULL, "Column 1 (college-only)", "college_metro_status"),
  mean_value_row(column1[college_metro_status != "Stayed"], "yrs_away_col", NULL,
                 "Column 1 (college-only)", "yrs_away_col_mean_if_left"),
  categorical_share_rows(column1, "moved_last_year_state", NULL, "Column 1 (college-only)", "migrated_past_year"),

  # Column 2: unweighted
  revelio_race_sex_rows(li, "w_unweighted", "Column 2 (HS+college, unweighted)"),
  categorical_share_rows(li, "age_bucket", "w_unweighted", "Column 2 (HS+college, unweighted)", "age_bucket"),
  categorical_share_rows(li, "hs_region", "w_unweighted", "Column 2 (HS+college, unweighted)", "hs_region"),
  categorical_share_rows(li, "col_region", "w_unweighted", "Column 2 (HS+college, unweighted)", "col_region"),
  categorical_share_rows(li, "transfer", "w_unweighted", "Column 2 (HS+college, unweighted)", "transfer"),
  categorical_share_rows(li, "any_grad", "w_unweighted", "Column 2 (HS+college, unweighted)", "grad_degree"),
  categorical_share_rows(li, "has_associate", "w_unweighted", "Column 2 (HS+college, unweighted)", "has_associate"),
  categorical_share_rows(li, "grad_after_22", "w_unweighted", "Column 2 (HS+college, unweighted)", "grad_after_22"),
  categorical_share_rows(li, "grad_after_26", "w_unweighted", "Column 2 (HS+college, unweighted)", "grad_after_26"),
  categorical_share_rows(li, "grad_after_30", "w_unweighted", "Column 2 (HS+college, unweighted)", "grad_after_30"),
  categorical_share_rows(li, "college_metro_status", "w_unweighted", "Column 2 (HS+college, unweighted)", "college_metro_status"),
  mean_value_row(li[college_metro_status != "Stayed"], "yrs_away_col", "w_unweighted",
                 "Column 2 (HS+college, unweighted)", "yrs_away_col_mean_if_left"),
  categorical_share_rows(li, "moved_last_year_state", "w_unweighted", "Column 2 (HS+college, unweighted)", "migrated_past_year"),

  # Column 2: reweighted to ACS
  revelio_race_sex_rows(li, "w_full_joint", "Column 2 (reweighted to ACS)"),
  categorical_share_rows(li, "age_bucket", "w_full_joint", "Column 2 (reweighted to ACS)", "age_bucket"),
  categorical_share_rows(li, "hs_region", "w_full_joint", "Column 2 (reweighted to ACS)", "hs_region"),
  categorical_share_rows(li, "col_region", "w_full_joint", "Column 2 (reweighted to ACS)", "col_region"),
  categorical_share_rows(li, "transfer", "w_full_joint", "Column 2 (reweighted to ACS)", "transfer"),
  categorical_share_rows(li, "any_grad", "w_full_joint", "Column 2 (reweighted to ACS)", "grad_degree"),
  categorical_share_rows(li, "has_associate", "w_full_joint", "Column 2 (reweighted to ACS)", "has_associate"),
  categorical_share_rows(li, "grad_after_22", "w_full_joint", "Column 2 (reweighted to ACS)", "grad_after_22"),
  categorical_share_rows(li, "grad_after_26", "w_full_joint", "Column 2 (reweighted to ACS)", "grad_after_26"),
  categorical_share_rows(li, "grad_after_30", "w_full_joint", "Column 2 (reweighted to ACS)", "grad_after_30"),
  categorical_share_rows(li, "college_metro_status", "w_full_joint", "Column 2 (reweighted to ACS)", "college_metro_status"),
  mean_value_row(li[college_metro_status != "Stayed"], "yrs_away_col", "w_full_joint",
                 "Column 2 (reweighted to ACS)", "yrs_away_col_mean_if_left"),
  categorical_share_rows(li, "moved_last_year_state", "w_full_joint", "Column 2 (reweighted to ACS)", "migrated_past_year"),

  # ACS PUMS benchmark -- categorical (not fractional), PWGTP-weighted;
  # no transfer-status/associate's-degree/college-metro/grad-age analog
  # (left absent, not zero-filled -- ACS respondents aren't tied to a
  # college, and its BA+ filter excludes associate's-only respondents).
  categorical_share_rows(pums_filt, "race", "PWGTP", "ACS PUMS benchmark", "race"),
  categorical_share_rows(pums_filt, "sex", "PWGTP", "ACS PUMS benchmark", "sex"),
  categorical_share_rows(pums_filt, "age_bucket", "PWGTP", "ACS PUMS benchmark", "age_bucket"),
  categorical_share_rows(pums_filt, "census_region", "PWGTP", "ACS PUMS benchmark", "region"),
  categorical_share_rows(pums_filt, "grad_degree", "PWGTP", "ACS PUMS benchmark", "grad_degree"),
  categorical_share_rows(pums_filt, "moved_out_of_state", "PWGTP", "ACS PUMS benchmark", "migrated_past_year")
), fill = TRUE)

print(characteristics_table)

# [CHANGED 2026-08-10] wide is now the primary output -- one real column per
# column_label (Column 1 / Column 2 unweighted / Column 2 reweighted / ACS
# benchmark), so values sit side by side for direct comparison instead of
# needing a pivot to actually read. The long form is kept too, under
# _long.csv/.rds, for any downstream code that wants to reshape it further.
column_order <- c("Column 1 (college-only)", "Column 2 (HS+college, unweighted)",
                   "Column 2 (reweighted to ACS)", "ACS PUMS benchmark")
characteristics_table_wide <- dcast(characteristics_table, variable + category ~ column_label, value.var = "value")
setcolorder(characteristics_table_wide, c("variable", "category", intersect(column_order, names(characteristics_table_wide))))

print(characteristics_table_wide)
fwrite(characteristics_table_wide, file.path(data_dir, "results/memo1_characteristics_table.csv"))
saveRDS(characteristics_table_wide, file.path(data_dir, "results/memo1_characteristics_table.rds"))
fwrite(characteristics_table, file.path(data_dir, "results/memo1_characteristics_table_long.csv"))
saveRDS(characteristics_table, file.path(data_dir, "results/memo1_characteristics_table_long.rds"))

cat("\nKnown asymmetries in this table (state plainly in any write-up, not fix):\n")
cat("  - Column 1 has no HS-side geography (hs_region) -- college-side (col_region) only.\n")
cat("  - ACS PUMS has no transfer-status/associate's-degree/college-metro/grad-age analog.\n")
cat("  - Column 2's `moved_proxy`/table above is a static one-time transition; ACS's moved_out_of_state\n")
cat("    is 1-year retrospective -- not the same event, only used to validate reweighting direction.\n")
cat("  - `migrated_past_year`: ACS's version is a literal fixed 1-year retrospective question;\n")
cat("    Revelio's is each person's own most-recent-observed-year-pair change, which could span more\n")
cat("    or less than a year depending on gaps in their position history -- structurally comparable,\n")
cat("    not the identical measure. Check moved_last_year_state's coverage rate (printed above by\n")
cat("    Code/memo1_01b_covariates.R) before leaning on this row -- non-adjacent last-observed pairs are NA.\n")
cat("  - `yrs_away_col_mean_if_left` is a headcount of mismatch-years within the first 10 years\n")
cat("    post-grad (deliberately NOT this repo's existing yr_return_col_cbsa, which finds the\n")
cat("    first-ever match year and conflates day-one non-movers with genuine returners -- see\n")
cat("    HANDOFF.md's Decisions).\n")

cat("\nreweight_column2.R done.\n")
