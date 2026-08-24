# memo1_12a_origin_destination_hsdiscloser.R (was college_origin_destination_counts.R)
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
#   w2 = w2_occ, the PRODUCTION Stage 2 weight (time-varying), read
#        directly from Code/memo1_07_reweight_column2_occupation.R's own
#        output (Data/results/memo1_w2_occupation_calibrated_by_year.rds)
#        and merged on by (user_id, calendar_year) -- NOT recomputed here.
#
#        [REVISED 2026-08-23, was: a from-scratch rank3_region-only
#        Phase B ratio, computed independently in this script.] That
#        geography-only design is now superseded in production:
#        memo1_07_reweight_column2_occupation.R alternates geography
#        (rank3_region flow) AND destination-occupation together in one
#        per-calendar-year IPF, and memo1_11a_final_plots.R/final_tables.R
#        already made w2_occ the sole "(reweighted)" weight everywhere
#        else in the memo -- this script had been left one generation
#        behind. Reusing the production table (rather than re-deriving
#        it a second time) also removes ~75 lines of duplicated
#        ACS-flow-margin logic from this file.
#
#        One real, inherited limitation worth stating plainly: w2_occ is
#        only computed for t=1..20 post-grad (memo1_07's own T_MAX), so
#        rows in this table beyond t=20 will show an increasingly
#        NA-heavy w2_sum/n_valid_w2 -- not a new gap introduced here, just
#        carried through from the upstream production weight as-is.
#        w2 remains defined only within memo1_07's own calibration window
#        (2012-2023, 2020 excluded) and only where the person has a
#        defined prior-year location, per that script's own convention.
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
library(bit64)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

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
## Production Stage 2 weight (w2_occ), read directly rather than
## re-derived -- see header note above.
## =========================================================================
log_step("Loading production w2_occ weight table")
w2_occ_panel <- readRDS(file.path(data_dir, "results/memo1_w2_occupation_calibrated_by_year.rds")); setDT(w2_occ_panel)
# user_id is integer64 (bit64) on both sides, but a bare merge() on an
# integer64 key errors ("i.user_id is type double") the moment bit64 isn't
# attached in this exact session -- same representation-mismatch bug class
# already documented elsewhere in this project (RAC2P/MIGSP/ST padding).
# Force to character on both sides, same fix philosophy: unambiguous
# regardless of which package attachments happen to be in scope.
w2_occ_panel[, user_id := as.character(user_id)]
cat(sprintf("w2_occ panel: %d rows, %d distinct users, calendar years %d-%d\n",
            nrow(w2_occ_panel), uniqueN(w2_occ_panel$user_id), min(w2_occ_panel$calendar_year), max(w2_occ_panel$calendar_year)))

## =========================================================================
## Person-year accumulation: one t-loop, every t where this person has a
## defined destination CBSA that year. w2 looked up per t from the
## production w2_occ panel loaded above.
## =========================================================================
log_step("Accumulating person-year rows across all t (0..50)")
base_cols <- data.table(
  col_unitid = li$col_unitid, col_opeid = li$col_opeid,
  origin_cbsa = li$hs_cbsa_code, user_id = as.character(li$user_id), w_full_joint = li$w_full_joint,
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

  # w2 for THIS t is looked up from the production w2_occ panel by
  # (user_id, calendar_year) -- NOT recomputed. An explicit .lookid tracks
  # row order through the merge (data.table::merge() does not reliably
  # preserve input row order -- same fix already established elsewhere in
  # this project, e.g. reweight_column2.R's manual_ipf()).
  yr <- cy[idx]
  lookup_t <- data.table(.lookid = seq_along(idx), user_id = base_cols$user_id[idx], calendar_year = yr)
  m <- merge(lookup_t, w2_occ_panel, by = c("user_id", "calendar_year"), all.x = TRUE)
  setorder(m, .lookid)
  w2_t <- m$w2_occ

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
