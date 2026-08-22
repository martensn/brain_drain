# omission_missingness_analysis.R
#
# [NEW 2026-08-21] Standalone descriptive analysis, NOT part of Memo 1.
# Nicholas's questions, in order: (1) how is cbsa_code_t actually built --
# does a missing year ever get silently carried forward from an earlier
# observation, or is NA a genuine gap? (2) how many users have "Swiss
# cheese" work histories (a gap on the profile that's later followed by
# more reported history, not just a single trailing dropout), how many
# have zero gaps at all, all the way through to the present? (3) does
# early-career work history get suppressed more than later years, and does
# that differ between all college grads (Column 1) and HS-disclosers
# (Column 2)? (4) a by-graduation-year table (1990+) summarizing all of
# the above, to inform how the eventual analysis sample should be built.
#
# [ANSWERED, (1)] Checked directly against the construction code
# (Code/memo1_01a_column1_construct.R's Section 2, "restriction 12
# vehicle: dcast into per-year wide columns" -- the SAME dcast() logic
# Column 2's build also uses via 04_li_ed_pos.Rmd/05_merge.Rmd, confirmed
# via those scripts' identical min_post_grad=0/max_post_grad=50 window
# bounds). The reshape is `dcast(std_pos_chunk, user_id ~ yrs_graduated,
# value.var=c("cbsa_code","cbsa_state"), fun.aggregate=mode_pick)` with NO
# `fill=` argument -- data.table's dcast defaults an absent (user_id,
# yrs_graduated) combination to NA, not to the previous year's value.
# `std_pos_chunk` itself only contains a row for a (user, year) pair when a
# real position (from `foverlaps()`, an interval join against each
# position's actual start/end dates) genuinely spans that year. So: NA in
# cbsa_code_t is a real, tracked gap -- no carry-forward anywhere in this
# pipeline. This rules out "stale imputed location" as the explanation for
# the profile_creation_rate anomaly found earlier the same day -- that
# question is still open, just not explained by this mechanism.
#
# Reference for the plot style: Outputs/omission_distribution.png /
# Code/Old/08_tables.Rmd's "om plot" chunk -- ONE series (an old sample,
# pre-dating this project's current Column 1/Column 2 construction), x =
# years elapsed since graduation (as of a fixed present-day reference
# year), y = quantiles of each person's total non-missing "years of work
# history," capped at years elapsed, with a y=x "theoretical maximum"
# reference line. Reproduced here with the SAME exact definition (for
# comparability) but TWO series -- Column 1 (all college grads) vs
# Column 2 (HS disclosers) -- and PRESENT_YEAR updated to 2025.
#
# Run-based classification (new, not in the old omission script): for each
# user, walk their cbsa_code_t sequence from t=0 through
# years_elapsed = PRESENT_YEAR - col_end (only years that have actually
# happened), count contiguous "present" runs. This distinguishes:
#   Complete            -- present every year, 0..years_elapsed
#   Late start, no gaps -- absent at the start (early-history suppression),
#                          then one unbroken run through the present
#   Trailing dropout     -- present from t=0, then stops and never returns
#   Late start + dropout -- one run somewhere in the middle (both patterns)
#   Swiss cheese          -- 2+ separate runs (a gap that's later followed
#                            by more reported history) -- what Nicholas is
#                            calling "swiss cheese"
#   Never observed        -- zero runs (flagged, expected to be near-zero
#                            since Column 1/2 membership itself requires
#                            at least one qualifying position)

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

PRESENT_YEAR <- 2025
T_MAX <- 50  # matches min_post_grad=0/max_post_grad=50 in the construction pipeline
TABLE_MIN_GRAD_YEAR <- 1990
WINDOW_YEARS <- 2015:2025

## =========================================================================
## Per-user run-based scan: one t-loop (0..T_MAX) per dataset, tracking
## running state across all users at once (same t-loop-over-columns
## pattern used everywhere else in this project, just with more state
## carried between iterations than a typical Memo 1 script needs).
## =========================================================================
scan_missingness <- function(dt, label) {
  n <- nrow(dt)
  col_end <- as.integer(if (is.factor(dt$col_end) || is.character(dt$col_end)) as.character(dt$col_end) else dt$col_end)
  years_elapsed <- PRESENT_YEAR - col_end  # -1 or negative for future grad years -- handled by valid<- below

  n_present     <- integer(n)
  n_runs        <- integer(n)
  was_present   <- logical(n)   # state from the previous iteration
  first_t       <- rep(NA_integer_, n)
  last_t        <- rep(NA_integer_, n)

  avail_cols <- paste0("cbsa_code_", 0:T_MAX)
  avail_cols <- avail_cols[avail_cols %in% names(dt)]

  for (col in avail_cols) {
    t <- as.integer(sub("cbsa_code_", "", col))
    valid <- !is.na(years_elapsed) & years_elapsed >= t  # this t has actually happened for this person
    if (!any(valid)) next
    is_present <- valid & !is.na(dt[[col]])

    n_present[is_present] <- n_present[is_present] + 1L
    rising_edge <- valid & is_present & !was_present
    n_runs[rising_edge] <- n_runs[rising_edge] + 1L

    newly_first <- is_present & is.na(first_t)
    first_t[newly_first] <- t
    last_t[is_present] <- t  # overwritten every time this user is present -- ends up as their true last-observed t

    was_present[valid] <- is_present[valid]  # only update state for rows where this t was in-range
  }

  n_possible <- pmax(years_elapsed + 1L, 0L)
  n_missing  <- n_possible - n_present

  out <- data.table(
    user_id = dt$user_id, col_end, years_elapsed, n_possible, n_present, n_missing,
    n_runs, first_t, last_t
  )
  out[, pattern := fcase(
    n_runs == 0, "Never observed",
    n_runs == 1 & first_t == 0 & n_missing == 0, "Complete",
    n_runs == 1 & first_t == 0 & n_missing > 0,  "Trailing dropout",
    n_runs == 1 & first_t > 0  & last_t == years_elapsed, "Late start, no further gaps",
    n_runs == 1 & first_t > 0  & last_t <  years_elapsed, "Late start + dropout",
    n_runs >= 2, "Swiss cheese",
    default = NA_character_
  )]
  out[, series := label]
  cat(sprintf("\n%s pattern breakdown (n=%d):\n", label, nrow(out)))
  print(out[, .N, by = pattern][order(-N)])
  out
}

log_step("Loading Column 1 (used for both the run-scan and the by-grad-year table -- loaded once, not twice)")
col1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds")); setDT(col1)
res1 <- scan_missingness(col1, "All college grads (Column 1)")

log_step("Loading Column 2")
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds")); setDT(li)
res2 <- scan_missingness(li, "HS disclosers (Column 2)")

fwrite(res1, file.path(data_dir, "results/omission_user_patterns_col1.csv"))
fwrite(res2, file.path(data_dir, "results/omission_user_patterns_col2.csv"))
log_step("Wrote per-user pattern files")

## =========================================================================
## Two-series omission_distribution-style plot data: quantiles of
## (n_missing-adjusted) years-included, by years-elapsed-since-grad --
## SAME definition as the old omission_distribution.png (capped at years
## elapsed, grouped by col_end, x-axis = PRESENT_YEAR - col_end).
## =========================================================================
build_omission_curve <- function(res, label) {
  d <- res[years_elapsed >= 1 & years_elapsed <= 45]  # matches the old plot's roughly-40-year range; drops brand-new grads (0 years elapsed, a degenerate single-point x=0)
  d[, yrs_inc_capped := pmin(n_present, years_elapsed)]
  d[, .(
    yrs_inc_25 = quantile(yrs_inc_capped, 0.25),
    yrs_inc_50 = quantile(yrs_inc_capped, 0.50),
    yrs_inc_75 = quantile(yrs_inc_capped, 0.75),
    n_users    = .N
  ), by = .(years_elapsed)][, series := label][]
}
curve1 <- build_omission_curve(res1, "All college grads (Column 1)")
curve2 <- build_omission_curve(res2, "HS disclosers (Column 2)")
omission_curve <- rbindlist(list(curve1, curve2))
fwrite(omission_curve, file.path(data_dir, "results/omission_distribution_two_series.csv"))
log_step("Wrote omission_distribution_two_series.csv")

## =========================================================================
## Early-history suppression: share of each series whose panel does NOT
## start at t=0 (i.e., first_t > 0 -- they don't show up until some years
## after graduation), plus the distribution of how many years are
## suppressed at the start, for those who have any history at all.
## =========================================================================
early_suppression <- function(res, label) {
  d <- res[pattern != "Never observed" & years_elapsed >= 1]
  d[, .(
    n_users = .N,
    share_late_start = mean(first_t > 0),
    median_years_suppressed_if_late = if (sum(first_t > 0) > 0) median(first_t[first_t > 0]) else NA_real_,
    p75_years_suppressed_if_late    = if (sum(first_t > 0) > 0) quantile(first_t[first_t > 0], 0.75) else NA_real_
  ), by = .()][, series := label][]
}
supp1 <- early_suppression(res1, "All college grads (Column 1)")
supp2 <- early_suppression(res2, "HS disclosers (Column 2)")
early_suppression_summary <- rbindlist(list(supp1, supp2))
cat("\n=== Early work-history suppression ===\n")
print(early_suppression_summary)
fwrite(early_suppression_summary, file.path(data_dir, "results/omission_early_suppression_summary.csv"))

## =========================================================================
## By-graduation-year table (1990+), each series separately: N with gaps,
## N with no gaps, quantiles of years missing, average share reporting
## something in 2015-2025 (of the applicable years for that cohort), share
## reporting specifically in 2025.
## =========================================================================
build_grad_year_table <- function(dt, res, label) {
  col_end <- res$col_end
  cohorts <- sort(unique(col_end[col_end >= TABLE_MIN_GRAD_YEAR & col_end <= PRESENT_YEAR]))

  parts <- vector("list", length(cohorts))
  for (i in seq_along(cohorts)) {
    G <- cohorts[i]
    idx <- which(col_end == G)
    sub <- res[idx]

    applicable_years <- WINDOW_YEARS[WINDOW_YEARS >= G]
    if (length(applicable_years) > 0) {
      share_by_year <- sapply(applicable_years, function(Y) {
        t <- Y - G
        col <- paste0("cbsa_code_", t)
        if (!col %in% names(dt)) return(NA_real_)
        mean(!is.na(dt[[col]][idx]))
      })
      avg_share_2015_2025 <- mean(share_by_year, na.rm = TRUE)
    } else {
      avg_share_2015_2025 <- NA_real_
    }

    t_2025 <- PRESENT_YEAR - G
    col_2025 <- paste0("cbsa_code_", t_2025)
    share_2025 <- if (t_2025 >= 0 && col_2025 %in% names(dt)) mean(!is.na(dt[[col_2025]][idx])) else NA_real_

    parts[[i]] <- data.table(
      grad_year = G,
      n_users = nrow(sub),
      n_with_gaps = sum(sub$n_missing > 0),
      n_no_gaps = sum(sub$n_missing == 0),
      years_missing_p25 = quantile(sub$n_missing, 0.25),
      years_missing_p50 = quantile(sub$n_missing, 0.50),
      years_missing_p75 = quantile(sub$n_missing, 0.75),
      avg_share_reporting_2015_2025 = avg_share_2015_2025,
      share_reporting_2025 = share_2025
    )
  }
  out <- rbindlist(parts)
  out[, series := label]
  out
}

log_step("Building by-grad-year table: Column 1")
tab1 <- build_grad_year_table(col1, res1, "All college grads (Column 1)")
rm(col1); gc()

log_step("Building by-grad-year table: Column 2")
tab2 <- build_grad_year_table(li, res2, "HS disclosers (Column 2)")
rm(li); gc()

grad_year_table <- rbindlist(list(tab1, tab2))
fwrite(grad_year_table, file.path(data_dir, "results/omission_by_grad_year_table.csv"))
log_step("Wrote omission_by_grad_year_table.csv")

cat("\n=== By-grad-year table (Column 1), first/last few rows ===\n")
print(tab1[c(1:5, (.N-4):.N)])
cat("\n=== By-grad-year table (Column 2), first/last few rows ===\n")
print(tab2[c(1:5, (.N-4):.N)])

log_step("omission_missingness_analysis.R done.")
