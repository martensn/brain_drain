# memo1_05a_migration_profile.R
#
# [REDESIGNED 2026-08-11 at Nicholas's request] x-axis changed from years-
# since-graduation (lifecycle) to CALENDAR YEAR (secular trend) -- the
# question is no longer "how does migration probability evolve over a
# graduate's career" but "has migration probability itself changed over
# calendar time." Two comparisons, same as before: (1) this script's
# year-over-year interstate migration rate, now by calendar year, and
# (2) metro-tier share by calendar year (Code/memo1_05b_metro_tiers.R).
#
# Revelio: cbsa_state_0..50 is indexed by years-since-grad (t); a given t
# slice's observations occurred in calendar_year = col_end + t (confirmed
# against 04_li_ed_pos.Rmd/memo1_column1_construct.R -- the wide panel is
# dcast()'d from yrs_graduated = interval - col_end, so a row's t=k value
# really was observed in calendar year col_end+k). The per-row mover
# computation (cbsa_state_t != cbsa_state_(t-1)) is UNCHANGED from the old
# years-since-grad version -- only the grouping variable changes.
#
# ACS: the old age-22 synthetic-cohort proxy against the pooled 5-year file
# is replaced entirely by a direct per-calendar-year measurement from
# Code/memo1_02b_acs_pull_1yr.R's ACS 1-year PUMS pull (one point per year, no age
# profile needed -- MIGSP-vs-ST already IS a 1-year mover flag). No age
# filtering on the ACS side: this line is now "annual interstate-mover rate
# among employed BA+ under-65 adults," matching Revelio's side pooling
# across all grad cohorts within a calendar year -- both are now secular-
# trend measures of the same underlying population concept, not a lifecycle
# profile matched to a synthetic cohort.
#
# Run after Code/memo1_04_reweight_column2.R (column2_reweighted.rds, w_full_joint),
# Code/memo1_01c_covariates.R (column1_covariates.rds), and Code/memo1_02b_acs_pull_1yr.R
# (pums_1yr_filt.rds). Standalone script, no source() between files.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

T_MAX <- 20  # years post-grad; unchanged from the years-since-grad version

## -----------------------------------------------------------------------
## SECTION 1: Revelio -- mover rate by calendar year
## -----------------------------------------------------------------------
## rate(calendar_year) = share with cbsa_state_t != cbsa_state_(t-1), among
## rows with both non-missing, pooled across EVERY (person, t) pair whose
## col_end+t equals that calendar_year -- i.e. every grad cohort contributes
## to whichever calendar years its own t=1..T_MAX span actually falls in.
## Processed one t-slice at a time and immediately collapsed to per-
## calendar-year partial sums before moving to the next t, rather than
## materializing a full person x t long table (Column 1 alone is ~34M rows
## x 20 t's -- flattening that naively would be ~680M rows).

# [col_end type gotcha, confirmed 2026-08-11 against live code]
# column1_covariates.rds's col_end is numeric (as.integer(col_end) is
# correct, memo1_covariates.R:295); column2_covariates.rds/
# column2_reweighted.rds's col_end is a factor/character requiring
# as.integer(as.character(col_end)) (memo1_covariates.R:421) -- calling
# as.integer() directly on the Column 2 factor silently returns level codes
# (small integers, NOT years), which would put every Column 2 point at a
# bogus "calendar year" in the 1-50 range. Resolve explicitly per table and
# assert the result looks like a real year range before trusting it.
resolve_col_end <- function(dt, is_factor_like) {
  raw <- if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
  rng <- range(raw, na.rm = TRUE)
  if (rng[1] < 1900 || rng[2] > 2100) {
    stop(sprintf("col_end resolved to an implausible range [%d, %d] -- likely a factor-level-code bug, not real years", rng[1], rng[2]))
  }
  cat(sprintf("  col_end range: %d-%d (sane)\n", rng[1], rng[2]))
  raw
}

# Returns the full aggregate (calendar_year, sum_w, sum_wmoved, n), not just
# the trimmed (source, calendar_year, rate, n) public schema -- the grand
# totals of sum_w/sum_wmoved are needed by the invariant check in Section 3
# below (rate*n alone can't reconstruct a weighted total once weight_col is
# non-NULL, since n is an unweighted count).
revelio_rate_by_calendar_year_full <- function(dt, weight_col, t_max, col_end_numeric) {
  partials <- vector("list", t_max)
  for (t in 1:t_max) {
    cur_col  <- paste0("cbsa_state_", t)
    prev_col <- paste0("cbsa_state_", t - 1)
    if (!all(c(cur_col, prev_col) %in% names(dt))) next
    # [carried over unchanged, FIXED 2026-08-10] weighted.mean()'s na.rm
    # only drops NAs in the value, not the weight -- w_full_joint is NA for
    # ~2.9% of users whose LI cell had no matching PUMS cell, and any NA
    # weight silently poisons the whole aggregate, not just that row.
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

log_step("Loading Column 1/2 covariate tables")
column1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds"))
setDT(column1)
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds"))
setDT(li)

log_step("Resolving col_end to real integer years (per-table, see type-gotcha note above)")
cat("Column 1 col_end:\n")
col_end_col1 <- resolve_col_end(column1, is_factor_like = FALSE)
cat("Column 2 col_end:\n")
col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

log_step("Computing Revelio migration-rate-by-calendar-year profiles")
profile_col1      <- revelio_rate_by_calendar_year(column1, NULL, T_MAX, "Column 1 (college-only)", col_end_col1)
profile_col2_unwt <- revelio_rate_by_calendar_year(li, "w_unweighted", T_MAX, "Column 2 (HS+college, unweighted)", col_end_col2)
profile_col2_rewt <- revelio_rate_by_calendar_year(li, "w_full_joint", T_MAX, "Column 2 (reweighted to ACS)", col_end_col2)

## -----------------------------------------------------------------------
## SECTION 2: ACS -- direct per-calendar-year mover rate (ACS 1-year PUMS)
## -----------------------------------------------------------------------

log_step("Loading ACS 1-year PUMS pull")
pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds"))
setDT(pums_1yr)
stopifnot(!2020 %in% unique(pums_1yr$survey_year))  # COVID-experimental year must have been excluded upstream

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
profile_acs <- acs_rate_by_calendar_year(pums_1yr)

migration_profile <- rbindlist(list(profile_col1, profile_col2_unwt, profile_col2_rewt, profile_acs))

log_step("Sample sizes by calendar_year (check where the lines get thin):")
print(dcast(migration_profile, calendar_year ~ source, value.var = "n"))

## -----------------------------------------------------------------------
## SECTION 3: verification checks
## -----------------------------------------------------------------------

# Invariant check: the underlying per-row mover computation is byte-for-
# byte unchanged from the old years-since-grad version, only regrouped --
# each source's total weighted mover count must be identical whether
# grouped by calendar_year (this script) or by the original years-since-
# grad t (recomputed here for comparison, not loaded from the old output
# file, so this check is self-contained and doesn't depend on stale files
# on disk).
revelio_rate_by_t <- function(dt, weight_col, t_max, source_label) {
  out <- vector("list", t_max)
  for (t in 1:t_max) {
    cur_col <- paste0("cbsa_state_", t); prev_col <- paste0("cbsa_state_", t - 1)
    if (!all(c(cur_col, prev_col) %in% names(dt))) next
    weight_valid <- if (is.null(weight_col)) TRUE else !is.na(dt[[weight_col]])
    valid <- !is.na(dt[[cur_col]]) & !is.na(dt[[prev_col]]) & weight_valid
    if (sum(valid) == 0) next
    moved <- as.numeric(dt[[cur_col]][valid] != dt[[prev_col]][valid])
    w <- if (is.null(weight_col)) rep(1, sum(valid)) else dt[[weight_col]][valid]
    out[[t]] <- data.table(sum_w = sum(w), sum_wmoved = sum(w * moved))
  }
  agg <- rbindlist(out)
  c(sum_w = sum(agg$sum_w), sum_wmoved = sum(agg$sum_wmoved))
}
for (chk in list(
  list(dt = column1, wc = NULL, cy = col_end_col1, lbl = "Column 1"),
  list(dt = li, wc = "w_unweighted", cy = col_end_col2, lbl = "Column 2 unweighted"),
  list(dt = li, wc = "w_full_joint", cy = col_end_col2, lbl = "Column 2 reweighted")
)) {
  by_t <- revelio_rate_by_t(chk$dt, chk$wc, T_MAX, chk$lbl)
  by_cy_full <- revelio_rate_by_calendar_year_full(chk$dt, chk$wc, T_MAX, chk$cy)
  sum_wmoved_cy <- sum(by_cy_full$sum_wmoved)
  ok <- isTRUE(all.equal(unname(by_t["sum_wmoved"]), sum_wmoved_cy, tolerance = 1e-6))
  cat(sprintf("[invariant check] %s: sum_wmoved by-t = %.2f, by-calendar-year = %.2f -- %s\n",
              chk$lbl, by_t["sum_wmoved"], sum_wmoved_cy, if (ok) "MATCH" else "MISMATCH -- investigate"))
  if (!ok) stop(sprintf("Invariant check failed for %s: regrouping by calendar_year changed the total weighted mover count", chk$lbl))
}

cat(sprintf("2020 present in ACS calendar_year column: %s (expect FALSE)\n",
            2020 %in% profile_acs$calendar_year))

fwrite(migration_profile, file.path(data_dir, "results/memo1_migration_rate_by_calendar_year.csv"))
saveRDS(migration_profile, file.path(data_dir, "results/memo1_migration_rate_by_calendar_year.rds"))
log_step("saved memo1_migration_rate_by_calendar_year.csv/.rds")
