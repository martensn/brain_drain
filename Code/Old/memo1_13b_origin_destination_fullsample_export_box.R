# memo1_13b_origin_destination_fullsample_export_box.R (was memo1_column1_export_box.R)
#
# [NEW 2026-08-23] Companion to memo1_13a_origin_destination_fullsample.R, mirroring
# Code/memo1_12b_origin_destination_hsdiscloser_export_box.R's role for the Column 2 file.
# Reads the already-saved full local table (produced once, at real
# multi-hour cost -- this script re-reads it rather than recomputing) and
# writes the calendar_year==2023-filtered, college_name-joined Box export.
# Exists as a standalone re-run path for exactly this step (the export
# logic now also lives inline in memo1_13a_origin_destination_fullsample.R itself,
# fixed there too) so a bug caught only in the export step doesn't require
# re-running the full ~3.5-hour build to fix.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
box_dir   <- "D:/Users/martensn/Box/Claude-Settings/Plans/BRAIN_DRAIN/data"

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

stopifnot("Box folder not found -- check Box Drive is signed in and synced on this machine" = dir.exists(box_dir))

log_step("Reading full local college_origin_destination_counts_by_year_fullsample.rds")
out <- readRDS(file.path(data_dir, "results/college_origin_destination_counts_by_year_fullsample.rds")); setDT(out)
cat(sprintf("Full table: %d rows, calendar years %d-%d\n", nrow(out), min(out$calendar_year), max(out$calendar_year)))

log_step("Filtering to calendar_year == 2023 and joining college_name")
windowed <- out[calendar_year == 2023]
inst <- fread(file.path(data_dir, "intermediate/institutional_characteristics.csv"), select = c("unitid", "inst_name"))
# col_unitid is character in Column 1's data (all-numeric strings) vs.
# institutional_characteristics.csv's integer unitid -- coerce to match.
windowed[, col_unitid := as.integer(col_unitid)]
named <- merge(windowed, inst, by.x = "col_unitid", by.y = "unitid", all.x = TRUE)
setnames(named, "inst_name", "college_name")
setcolorder(named, c("col_unitid", "college_name", setdiff(names(named), c("col_unitid", "college_name"))))
setkey(named, col_unitid)
cat(sprintf("2023-filtered rows: %d | college_name match rate: %.1f%%\n",
            nrow(named), 100 * mean(!is.na(named$college_name))))

saveRDS(named, file.path(box_dir, "college_origin_destination_counts_by_year_fullsample_2023.rds"))
log_step("Wrote college_origin_destination_counts_by_year_fullsample_2023.rds to Box")
