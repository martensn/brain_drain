# puma_boundary_crossing_analysis.R
#
# [NEW 2026-08-21] Standalone descriptive check, NOT part of Memo 1's own
# write-up. Nicholas's question: of the PUMAs (and MIGPUMAs) used in
# Stage 2's flow calibration (Code/memo1_06b_scheme_comparison.R's
# rank3_region scheme), how many straddle a metro-tier x Census-region
# boundary within their own state? PUMAs never cross state lines, so this
# can only happen when a single PUMA's territory is split across two (or
# more) tier x region cells -- e.g. a PUMA that's partly inside a Top-10
# metro and partly "Everything else" in the same state/region.
#
# This is exactly what Code/memo1_03a_puma_cbsa_crosswalk.R /
# memo1_03b_migpuma_cbsa_crosswalk.R's fractional `share` column already
# encodes: a PUMA entirely inside one cell has ONE row (share=1); a
# crossing PUMA has 2+ rows (shares summing to 1 across cells). No new
# geography work needed here, just tabulating what's already built.
#
# Population weighting note: memo1_03b's own header confirms the MIGPUMA
# crosswalk's population weights are ACS's own PWGTP for the BA+/employed/
# under-65 filtered analysis population (the exact Stage-2 sample), NOT
# general Decennial/Census population -- so "the ACS [side]" and "the
# [analysis] sample" are the SAME underlying population throughout this
# project (there's only ever one filtered ACS population, used both as
# the sample and the benchmark). This script recomputes that same
# PWGTP-weighted population directly from pums_1yr_filt.rds, restricted to
# Stage 2's actual CALIB_YEARS window (2012-2023, 2020 excluded), rather
# than re-deriving it from memo1_03b's own internal (unsaved) totals.
#
# Revelio's own side has no analog to report here -- it never touches
# PUMA/MIGPUMA geography at all (cbsa_code_t is Revelio's own CBSA
# assignment, unrelated to Census PUMA boundaries), so "PUMA crossing" is
# purely an ACS-side phenomenon.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

CALIB_YEARS <- setdiff(2012:2023, 2020)
VINTAGE_2010_YEARS <- setdiff(2012:2021, 2020)
VINTAGE_2020_YEARS <- 2022:2023

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

## ---- crossing flags from the already-built fractional crosswalks ----
flag_crossing <- function(path, key_col) {
  x <- readRDS(path); setDT(x)
  n_rows <- x[, .N, by = key_col]
  crossing_keys <- n_rows[N > 1][[key_col]]
  list(crossing_keys = crossing_keys, n_total = nrow(n_rows), n_crossing = length(crossing_keys))
}

int_dir <- file.path(data_dir, "intermediate")
puma_2010   <- flag_crossing(file.path(int_dir, "puma_cbsa_tier_crosswalk_2010_rank3_region.rds"), "state_puma")
puma_2020   <- flag_crossing(file.path(int_dir, "puma_cbsa_tier_crosswalk_2020_rank3_region.rds"), "state_puma")
migpuma_2010 <- flag_crossing(file.path(int_dir, "migpuma_cbsa_tier_crosswalk_2010_rank3_region.rds"), "state_migpuma")
migpuma_2020 <- flag_crossing(file.path(int_dir, "migpuma_cbsa_tier_crosswalk_2020_rank3_region.rds"), "state_migpuma")

cat(sprintf("PUMA (destination), 2010-vintage:   %d of %d cross a tier x region boundary (%.1f%%)\n",
            puma_2010$n_crossing, puma_2010$n_total, 100 * puma_2010$n_crossing / puma_2010$n_total))
cat(sprintf("PUMA (destination), 2020-vintage:   %d of %d cross (%.1f%%)\n",
            puma_2020$n_crossing, puma_2020$n_total, 100 * puma_2020$n_crossing / puma_2020$n_total))
cat(sprintf("MIGPUMA (origin), 2010-vintage:     %d of %d cross (%.1f%%)\n",
            migpuma_2010$n_crossing, migpuma_2010$n_total, 100 * migpuma_2010$n_crossing / migpuma_2010$n_total))
cat(sprintf("MIGPUMA (origin), 2020-vintage:     %d of %d cross (%.1f%%)\n",
            migpuma_2020$n_crossing, migpuma_2020$n_total, 100 * migpuma_2020$n_crossing / migpuma_2020$n_total))

## ---- population share, from the actual Stage-2 ACS analysis sample ----
log_step("Loading pums_1yr_filt.rds")
pums_1yr <- readRDS(file.path(int_dir, "pums_1yr_filt.rds")); setDT(pums_1yr)

## -- Destination side (PUMA10/PUMA20), by vintage window --
pop_share_puma <- function(years, puma_col, crossing_keys, label) {
  d <- pums_1yr[survey_year %in% years & !is.na(get(puma_col))]
  d[, state_puma := paste0(ST, get(puma_col))]
  tot <- sum(d$PWGTP)
  crossing_pop <- sum(d[state_puma %in% crossing_keys]$PWGTP)
  cat(sprintf("%s: %.2f%% of the Stage-2 ACS analysis sample's weighted population lives in a crossing PUMA (n=%d respondents)\n",
              label, 100 * crossing_pop / tot, nrow(d)))
  invisible(data.table(side = "destination", vintage = label, pop_share_crossing = crossing_pop / tot, n_resp = nrow(d)))
}
r1 <- pop_share_puma(VINTAGE_2010_YEARS, "PUMA10", puma_2010$crossing_keys, "PUMA, 2010-vintage (2012-2021, 2020 excl.)")
r2 <- pop_share_puma(VINTAGE_2020_YEARS, "PUMA20", puma_2020$crossing_keys, "PUMA, 2020-vintage (2022-2023)")

## -- Origin side (MIGPUMA10/MIGPUMA20), movers only -- same
## state_migpuma construction as memo1_06b/07c's build_acs_flow_data() --
## copied, not re-derived. ----
pop_share_migpuma <- function(years, migpuma_col, crossing_keys, label) {
  d <- pums_1yr[survey_year %in% years & !is.na(get(migpuma_col))]
  d[, migsp_int := suppressWarnings(as.integer(MIGSP))]
  d[, state_migpuma := fifelse(
    !is.na(migsp_int) & migsp_int >= 1L & migsp_int <= 56L,
    paste0(sprintf("%02d", migsp_int), get(migpuma_col)), NA_character_
  )]
  movers <- d[!is.na(state_migpuma)]
  tot <- sum(movers$PWGTP)
  crossing_pop <- sum(movers[state_migpuma %in% crossing_keys]$PWGTP)
  cat(sprintf("%s: %.2f%% of the Stage-2 ACS mover subsample's weighted population has an origin MIGPUMA that crosses (n=%d movers)\n",
              label, 100 * crossing_pop / tot, nrow(movers)))
  invisible(data.table(side = "origin", vintage = label, pop_share_crossing = crossing_pop / tot, n_resp = nrow(movers)))
}
r3 <- pop_share_migpuma(VINTAGE_2010_YEARS, "MIGPUMA10", migpuma_2010$crossing_keys, "MIGPUMA, 2010-vintage (2012-2021, 2020 excl.)")
r4 <- pop_share_migpuma(VINTAGE_2020_YEARS, "MIGPUMA20", migpuma_2020$crossing_keys, "MIGPUMA, 2020-vintage (2022-2023)")

results <- rbindlist(list(r1, r2, r3, r4))
fwrite(results, file.path(data_dir, "results/puma_boundary_crossing_summary.csv"))
log_step("Wrote puma_boundary_crossing_summary.csv")
