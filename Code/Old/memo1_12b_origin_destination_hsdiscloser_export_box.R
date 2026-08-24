# memo1_12b_origin_destination_hsdiscloser_export_box.R (was college_origin_destination_export_box.R)
#
# [NEW 2026-08-23] Reads Code/memo1_12a_origin_destination_hsdiscloser.R's
# freshly-written output (Data/results/college_origin_destination_counts_by_year.csv,
# the full unfiltered table -- 2026-08-23: rebuilt to use the production
# w2_occ weight, see that script's own header) and produces the two
# lighter, Box-syncable exports Nicholas actually works with day to day,
# reproducing the existing convention already on Box (both files there as
# of Aug 21-22 were built ad hoc, not from a saved script -- this
# formalizes that step so it's reproducible going forward):
#
#   1. college_origin_destination_counts_by_year_2012_2023.rds -- filtered
#      to calendar_year 2012-2023 (Phase B's own calibration window, where
#      w2 is actually defined), unsorted, no institution names.
#   2. ..._2012_2023_named.rds -- same rows, plus college_name (joined
#      from Data/intermediate/institutional_characteristics.csv by
#      col_unitid == unitid), and keyed/sorted by col_unitid. Sorting
#      before saveRDS is deliberate, not cosmetic: grouping identical
#      college_name strings together lets R's serializer compress far
#      better -- confirmed against the existing stale Box files, where
#      the named+sorted version (475MB) is smaller on disk than the
#      unsorted plain version (897MB) despite carrying one MORE column.
#
# Both written to Box\Claude-Settings\Plans\BRAIN_DRAIN\data\ for
# cross-machine access, per this project's established convention
# (MEMO1_WEIGHTING.md and its HTML export live in the same Box folder).

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
box_dir   <- "D:/Users/martensn/Box/Claude-Settings/Plans/BRAIN_DRAIN/data"

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

stopifnot("Box folder not found -- check Box Drive is signed in and synced on this machine" = dir.exists(box_dir))

log_step("Reading full college_origin_destination_counts_by_year.csv")
full <- fread(file.path(data_dir, "results/college_origin_destination_counts_by_year.csv"))
cat(sprintf("Full table: %d rows, calendar years %d-%d\n", nrow(full), min(full$calendar_year), max(full$calendar_year)))

log_step("Filtering to calendar_year 2012-2023")
windowed <- full[calendar_year %in% 2012:2023]
rm(full); gc()
cat(sprintf("Windowed table: %d rows\n", nrow(windowed)))

saveRDS(windowed, file.path(box_dir, "college_origin_destination_counts_by_year_2012_2023.rds"))
log_step("Wrote college_origin_destination_counts_by_year_2012_2023.rds")

log_step("Joining college_name and sorting by col_unitid")
inst <- fread(file.path(data_dir, "intermediate/institutional_characteristics.csv"), select = c("unitid", "inst_name"))
named <- merge(windowed, inst, by.x = "col_unitid", by.y = "unitid", all.x = TRUE)
setnames(named, "inst_name", "college_name")
setcolorder(named, c("col_unitid", "college_name", setdiff(names(named), c("col_unitid", "college_name"))))
setkey(named, col_unitid)
cat(sprintf("Match rate onto institutional_characteristics.csv: %.1f%%\n", 100 * mean(!is.na(named$college_name))))

saveRDS(named, file.path(box_dir, "college_origin_destination_counts_by_year_2012_2023_named.rds"))
log_step("Wrote college_origin_destination_counts_by_year_2012_2023_named.rds")
