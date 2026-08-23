# memo1_10_metro_tier_plot_add_occ_line.R
#
# [NEW 2026-08-23] Adds the w2_occ (geography+occupation) line to the
# existing full-sample metro-tier-share-by-calendar-year data, APPENDING
# to memo1_metro_tier_by_calendar_year_full_simplified.csv rather than
# recomputing or overwriting its existing four rows-per-cell (BA Only,
# BA + HS on LI, BA + HS on LI (reweighted), ACS PUMS) -- per Nicholas's
# instruction to keep all old underlying data intact. Run
# memo1_10_metro_tier_plot.R afterward to regenerate the PNG with all five
# lines (that script is separately extended to add the 5th series/color).

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

LBL_NEW <- "BA + HS on LI (reweighted, geo+occupation)"
strip_region <- function(tier) sub(" \\(.*\\)$", "", tier)

log_step("Loading existing CSV, w2_occ panel, and revelio geo+occ panel")
csv_path <- file.path(data_dir, "results/memo1_metro_tier_by_calendar_year_full_simplified.csv")
existing_tier <- fread(csv_path)

w2_occ_panel <- readRDS(file.path(data_dir, "results/memo1_w2_occupation_calibrated_by_year.rds")); setDT(w2_occ_panel)
rev_panel <- readRDS(file.path(data_dir, "intermediate/revelio_geo_occ_person_year_panel.rds")); setDT(rev_panel)

panel_w2occ <- merge(rev_panel[, .(user_id, calendar_year, dest_tier)], w2_occ_panel, by = c("user_id", "calendar_year"))
panel_w2occ[, tier := strip_region(dest_tier)]
tier_w2occ <- panel_w2occ[, .(w = sum(w2_occ)), by = .(calendar_year, tier)]
tier_w2occ[, share := w / sum(w), by = calendar_year]
new_rows <- tier_w2occ[, .(source = LBL_NEW, calendar_year, tier, share)]

cat(sprintf("New rows: %d (calendar years %d-%d)\n", nrow(new_rows), min(new_rows$calendar_year), max(new_rows$calendar_year)))

# Idempotent: drop any prior run's rows for this source before appending,
# so re-running this script doesn't duplicate rows.
existing_tier <- existing_tier[source != LBL_NEW]
out <- rbindlist(list(existing_tier, new_rows), use.names = TRUE)
fwrite(out, csv_path)
log_step(sprintf("Wrote %s (%d total rows, %d sources)", csv_path, nrow(out), uniqueN(out$source)))
