# Resolves a (unitid, reference_year) pair to the correct colleges.rds row.
#
# colleges.rds has 3,903 rows but only 3,510 distinct unitid values: 354
# unitids span more than one row because the institution was renamed and/or
# re-issued a new opeid over time (e.g. unitid 101365 is "HERZING COLLEGE"
# (opeid 01019300) through 2009, then "Herzing University-Birmingham" (opeid
# 00962107) from 2010 on -- same physical campus, tracked as two IPEDS-year
# eras). Joining on unitid alone silently picks an arbitrary era's
# name/opeid/geography; matching on unitid + the person's own reference year
# against each row's [start_year, end_year] window picks the era-correct one.
# See yes-let-s-resolve-this-misty-kahan.md's 2026-07-28 finding/correction.
#
# Used by Code/01d_col_hs_construct.R (ba_school/ba_state/ba_opeid
# backfill), Code/04_li_ed_pos.Rmd (inst_zip/col_state), and Code/05_merge.Rmd
# (col_cbsas) -- kept in one place so the three usages can't drift apart.

library(data.table)

#' @param dt data.table to resolve against; not modified in place (copied).
#' @param unitid_col name of dt's unitid column (character or numeric).
#' @param year_col name of dt's reference-year column (numeric year, may
#'   contain NA -- rows with NA year fall back to the single/most-recent row).
#' @param colleges colleges.rds, or an equivalent table with unitid,
#'   start_year, end_year plus whatever columns should be pulled in.
#' @param select_cols columns from `colleges` to bring back (besides unitid).
#' @return dt (in original row order) with `select_cols` appended, each row
#'   resolved to its era-correct colleges.rds match (or NA if unitid isn't in
#'   colleges at all).
resolve_college <- function(dt, unitid_col, year_col, colleges,
                             select_cols = c("inst_name", "state_abbr", "opeid", "col_fips", "zip")) {
  stopifnot(is.data.table(dt), is.data.table(colleges))

  cols <- unique(colleges[, c("unitid", "start_year", "end_year", select_cols), with = FALSE])
  cols[, unitid := as.integer(unitid)]
  cols <- cols[!is.na(unitid)]

  # Fallback row per unitid when no [start_year,end_year] window covers the
  # target year (or the year is NA): the most recent era on file.
  setorder(cols, unitid, -end_year)
  fallback <- cols[, .SD[1], by = unitid]
  setnames(fallback, "unitid", ".._unitid")

  work <- data.table(
    .._row_id = seq_len(nrow(dt)),
    .._unitid = suppressWarnings(as.integer(dt[[unitid_col]])),
    .._year   = suppressWarnings(as.numeric(dt[[year_col]]))
  )

  # Exact era match: unitid matches AND the reference year falls inside
  # [start_year, end_year].
  eligible <- work[!is.na(.._unitid) & !is.na(.._year)]
  exact <- eligible[cols, on = .(.._unitid = unitid, .._year >= start_year, .._year <= end_year),
                     nomatch = NULL, mult = "first"]
  # [CHANGED 2026-07-29 -- Phase A.3 diff] mult="first" bounds matches per row
  # of `cols` (colleges, the join's i-argument here), not per input row --
  # it does NOT guard against overlapping windows the way the comment above
  # used to claim. Two colleges.rds unitids (197674: [2000,2013] and
  # [2011,2012]; 178989: [2008,2013] and [2000,2012]) have windows that
  # actually overlap, not just sit in sequential eras -- a person whose
  # reference year falls in the overlap matched both rows, producing an
  # exact duplicate row downstream (caught via a duplicate-user_id check on
  # microdata.csv, 2 people affected). Dedup by .._row_id, keeping the first
  # -- cols is sorted by -end_year above, so this deterministically prefers
  # the most recent overlapping era.
  exact <- exact[!duplicated(.._row_id)]
  exact <- exact[, c(".._row_id", select_cols), with = FALSE]

  need_fallback <- work[!.._row_id %in% exact$.._row_id]
  fb <- need_fallback[fallback, on = ".._unitid", nomatch = NULL]
  fb <- fb[, c(".._row_id", select_cols), with = FALSE]

  out <- rbindlist(list(exact, fb), use.names = TRUE)

  result <- copy(dt)
  result[, .._row_id := seq_len(.N)]
  result <- out[result, on = ".._row_id"]
  result[, .._row_id := NULL]
  result[]
}
