# memo1_13_origin_destination_fullsample.R (was memo1_column1_origin_destination.R,
# then briefly memo1_13a_origin_destination_fullsample.R paired with a
# memo1_13b_..._export_box.R companion -- that companion turned out to
# duplicate logic already inline here (it existed only as a standalone
# recovery path while debugging a type-mismatch bug in the export step, so
# a ~3.5-hour build didn't need repeating just to fix it) and was archived
# 2026-08-24 once the bug was fixed in both places; this file alone is now
# the complete build+export script)
#
# [NEW 2026-08-23] Full-sample (Column 1) analog of
# Code/memo1_12_origin_destination_hsdiscloser.R -- built for robustness/larger-N
# checks, per Nicholas's explicit request. Same aggregation grain and
# column schema as the HS-discloser (Column 2) version -- college x
# origin_cbsa x destination_cbsa x calendar_year x race x sex -- so the two
# files are structurally interchangeable, EXCEPT origin_cbsa is always NA
# here: Column 1 has no hs_cbsa_code (no high-school data at all), so
# "origin labor market" cannot be characterized for this population, per
# Nicholas's own explicit acknowledgment when requesting this file. Kept as
# a real (NA-valued) column rather than dropped, at his direction, so the
# two tables share one schema.
#
# Weights: w1 = Column 1's own Stage 1 weight (w_full_joint, from
# memo1_06_column1_reweight.R Section 1); w2 = Column 1's own Stage 2
# geography+occupation weight (w2_occ, from
# memo1_06_column1_reweight.R Section 2, keyed (user_id, calendar_year)) --
# Column 1's own two-stage reweighting pipeline, NOT a reuse of Column 2's
# weights (a different, unweighted-until-now population). Same inherited
# w2 limitation as the Column 2 script: only defined for t=1..20 post-grad
# and only within the 2012-2023 (2020 excluded) calibration window.
#
# Output: FULL, unfiltered table saved LOCALLY ONLY (not Box -- this is a
# genuinely large file, larger than Column 2's own 5.67GB full CSV given
# Column 1's ~10x row count). A calendar_year==2023-filtered copy (with
# college_name joined, matching the existing Box convention for the
# Column 2 file) is written to Box for cross-machine access.

library(data.table)
library(bit64)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
box_dir   <- "D:/Users/martensn/Box/Claude-Settings/Plans/BRAIN_DRAIN/data"

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

T_MAX <- 50
RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob", "native_prob", "multiple_prob", "hispanic_prob")
race_col_to_label <- setNames(RACE_LABELS, RACE_PROB_COLS)

log_step("Loading Column 1 (reweighted)")
col1 <- readRDS(file.path(data_dir, "intermediate/column1_reweighted.rds")); setDT(col1)
col_end <- as.integer(if (is.factor(col1$col_end) || is.character(col1$col_end)) as.character(col1$col_end) else col1$col_end)

log_step("Loading Column 1's Stage 2 (w2_occ) weight table")
w2_occ_panel <- readRDS(file.path(data_dir, "results/memo1_column1_w2_occupation_calibrated_by_year.rds")); setDT(w2_occ_panel)
w2_occ_panel[, user_id := as.character(user_id)]
cat(sprintf("w2_occ panel: %d rows, %d distinct users, calendar years %d-%d\n",
            nrow(w2_occ_panel), uniqueN(w2_occ_panel$user_id), min(w2_occ_panel$calendar_year), max(w2_occ_panel$calendar_year)))

## =========================================================================
## Person-year accumulation -- same structure as
## Code/memo1_12_origin_destination_hsdiscloser.R, origin_cbsa hardcoded NA.
## =========================================================================
log_step("Accumulating person-year rows across all t (0..50)")
base_cols <- data.table(
  col_unitid = col1$col_unitid, col_opeid = col1$col_opeid,
  origin_cbsa = NA_integer_, user_id = as.character(col1$user_id), w_full_joint = col1$w_full_joint,
  sex = fifelse(col1$m_prob >= col1$f_prob, "male", "female")
)
for (rp in RACE_PROB_COLS) base_cols[[rp]] <- col1[[rp]]

py_parts <- vector("list", T_MAX + 1)
for (t in 0:T_MAX) {
  dest_col <- paste0("cbsa_code_", t)
  if (!dest_col %in% names(col1)) next
  cy <- col_end + t
  dest_cbsa_t <- col1[[dest_col]]
  valid <- !is.na(dest_cbsa_t) & dest_cbsa_t != "" & !is.na(cy) & !is.na(col1$col_unitid)
  if (sum(valid) == 0) next
  idx <- which(valid)

  yr <- cy[idx]
  lookup_t <- data.table(.lookid = seq_along(idx), user_id = base_cols$user_id[idx], calendar_year = yr)
  m <- merge(lookup_t, w2_occ_panel, by = c("user_id", "calendar_year"), all.x = TRUE)
  setorder(m, .lookid)
  w2_t <- m$w2_occ

  py_parts[[t + 1]] <- cbind(base_cols[idx], data.table(destination_cbsa = dest_cbsa_t[idx], calendar_year = yr, w2 = w2_t))
  if (t %% 10 == 0) log_step(sprintf("  t=%d done (%d valid rows this t)", t, length(idx)))
}
person_year <- rbindlist(py_parts)
rm(py_parts, col1); gc()

cat(sprintf("Person-year rows (college+destination defined; origin_cbsa always NA): %d\n", nrow(person_year)))
cat(sprintf("...of which have a defined w2: %d (%.1f%%)\n",
            sum(!is.na(person_year$w2)), 100 * mean(!is.na(person_year$w2))))
cat(sprintf("Calendar year range: %d - %d\n", min(person_year$calendar_year), max(person_year$calendar_year)))

## =========================================================================
## Aggregate PER RACE COLUMN separately, same as the Column 2 script.
## =========================================================================
log_step("Aggregating by college x origin(NA) x destination x calendar_year x race x sex")
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
rm(person_year, agg_parts); gc()

log_step("Writing full local table")
fwrite(out, file.path(data_dir, "results/college_origin_destination_counts_by_year_fullsample.csv"))
saveRDS(out, file.path(data_dir, "results/college_origin_destination_counts_by_year_fullsample.rds"))
log_step("Wrote college_origin_destination_counts_by_year_fullsample.csv/.rds (local only)")

cat(sprintf("\nTotal rows (college x origin x destination x year x race x sex cells): %d\n", nrow(out)))
cat(sprintf("Distinct colleges (col_unitid): %d\n", uniqueN(out$col_unitid)))
cat(sprintf("Distinct destination CBSAs: %d | distinct calendar years: %d\n",
            uniqueN(out$destination_cbsa), uniqueN(out$calendar_year)))

## =========================================================================
## Box export: calendar_year == 2023 only (per Nicholas's explicit
## instruction -- the full table is too big for Box), college_name joined,
## matching the existing Box convention for the Column 2 file.
## =========================================================================
if (dir.exists(box_dir)) {
  log_step("Filtering to calendar_year == 2023 and joining college_name for Box export")
  windowed <- out[calendar_year == 2023]
  inst <- fread(file.path(data_dir, "intermediate/institutional_characteristics.csv"), select = c("unitid", "inst_name"))
  # [FIXED 2026-08-23, caught on the first real run] Column 1's col_unitid
  # is character (all-numeric strings, e.g. "178396") -- unlike Column 2's,
  # which is integer -- while institutional_characteristics.csv's unitid
  # is integer (fread auto-parse). Coerce to a common type before merging,
  # same representation-mismatch fix pattern used throughout this project.
  windowed[, col_unitid := as.integer(col_unitid)]
  named <- merge(windowed, inst, by.x = "col_unitid", by.y = "unitid", all.x = TRUE)
  setnames(named, "inst_name", "college_name")
  setcolorder(named, c("col_unitid", "college_name", setdiff(names(named), c("col_unitid", "college_name"))))
  setkey(named, col_unitid)
  cat(sprintf("2023-filtered rows: %d | college_name match rate: %.1f%%\n",
              nrow(named), 100 * mean(!is.na(named$college_name))))
  saveRDS(named, file.path(box_dir, "college_origin_destination_counts_by_year_fullsample_2023.rds"))
  log_step("Wrote college_origin_destination_counts_by_year_fullsample_2023.rds to Box")
} else {
  warning("Box folder not found -- skipped Box export. Check Box Drive is signed in and synced on this machine.")
}

log_step("memo1_13_origin_destination_fullsample.R done.")
