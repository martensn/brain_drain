# memo1_phase_a_year_calibration_test.R
#
# [NEW 2026-08-12] Phase A of the calendar-year-varying migration-behavior
# plan (D:\Users\martensn\.claude\plans\nope-i-had-something-logical-aurora.md).
# Standalone TEST script -- does not touch reweight_column2.R,
# memo1_metro_tiers.R, or memo1_migration_profile.R, or any of their
# committed output.
#
# Why this exists: two prior attempts at a mobility-related raking margin
# (the PUMS recent-mover flag, then an IRS-SOI-derived destination-tier
# margin) each collapsed every person to ONE static, per-person weight --
# throwing away Revelio's actual advantage, a full year-by-year location
# trajectory. Nicholas's explicit redirect: use the whole history, not just
# where someone lives at present.
#
# Mechanism (two-stage, NOT a bigger joint IPF -- see the plan file for why
# a naive person-year-by-race melt is intractable at this scale): Stage 1 is
# UNCHANGED -- the existing static w_full_joint from reweight_column2.R.
# Stage 2 (this script) is a YEAR-SLICED CALIBRATION layered on top: for
# each calendar year 2012-2021 (the window where ACS's own metro-tier line
# already exists), compute ratio[year, tier] = ACS_share / Revelio_share
# using tables ALREADY on disk (memo1_metro_tier_by_calendar_year.csv --
# no new pull needed), then rescale each person's contribution AT THAT
# SPECIFIC YEAR by the ratio matching their OWN observed tier that year.
# A person observed across 10 years gets 10 separately-corrected
# contributions -- the trajectory is used directly, not compressed to one
# snapshot the way both prior attempts did.
#
# IMPORTANT FRAMING, stated here and repeated in HANDOFF.md: calibrating
# Revelio's tier share to match ACS's tier share MECHANICALLY closes the
# metro-tier gap by construction -- that isn't a finding, it's what
# calibration does. The real test this script runs is whether re-deriving
# the MIGRATION-RATE line (which the calibration ratios don't directly
# target -- they're built from tier shares, not moves) under this new
# year-varying weight moves it relative to the existing static-weight line.
#
# Restricted to calendar years 2012-2021 throughout -- the window where a
# real ACS tier-share target actually exists (memo1_metro_tiers.R's own
# ACS line is limited to the same window, same PUMA-vintage reason).
# Outside that window there is no calibration target, so this line simply
# isn't computed there (not silently defaulted to ratio=1).

library(data.table)
library(tidycensus)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
tidycensus::census_api_key(Sys.getenv("CENSUS_KEY"))

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

CALIB_YEARS <- 2012:2021
RATIO_CAP_LO <- 0.05
RATIO_CAP_HI <- 20

## -----------------------------------------------------------------------
## SECTION 0: CBSA -> tier lookup, identical to memo1_metro_tiers.R's
## Section 0 (same CBSA_RANK_YEAR=2022 vintage) -- destination_tier here
## must mean exactly the same thing it means in the file the calibration
## ratios are built from.
## -----------------------------------------------------------------------

CBSA_RANK_YEAR <- 2022
cbsa_pop <- get_estimates(geography = "cbsa", product = "population", year = CBSA_RANK_YEAR, vintage = CBSA_RANK_YEAR)
setDT(cbsa_pop)
cbsa_pop <- cbsa_pop[variable == "POPESTIMATE", .(cbsa_code = as.character(GEOID), cbsa_pop = value)]
setorder(cbsa_pop, -cbsa_pop)
cbsa_pop[, cbsa_rank := seq_len(.N)]
cbsa_pop[, metro_tier := fcase(
  cbsa_rank <= 10, "Top 10",
  cbsa_rank <= 50, "Top 11-50",
  cbsa_rank <= 100, "Top 51-100",
  default = "Other metro"
)]
log_step(paste("CBSA tier lookup built:", nrow(cbsa_pop), "CBSAs"))

code_to_tier <- function(codes) {
  tier <- cbsa_pop$metro_tier[match(codes, cbsa_pop$cbsa_code)]
  tier[is.na(tier) & grepl("999$", codes)] <- "Non-metro"
  tier
}

## -----------------------------------------------------------------------
## SECTION 1: build ratio[year, tier] from the ALREADY-COMPUTED calendar-
## year metro-tier file -- no new pull.
## -----------------------------------------------------------------------

log_step("Building year x tier calibration ratios from existing memo1_metro_tier_by_calendar_year.csv")
existing_tier <- fread(file.path(data_dir, "results/memo1_metro_tier_by_calendar_year.csv"))

revelio_static <- existing_tier[source == "Column 2 (reweighted to ACS)" & calendar_year %in% CALIB_YEARS,
                                 .(calendar_year, tier, revelio_share = share)]
acs_actual <- existing_tier[source == "ACS PUMS benchmark" & calendar_year %in% CALIB_YEARS,
                             .(calendar_year, tier, acs_share = share)]
ratio_tab <- merge(revelio_static, acs_actual, by = c("calendar_year", "tier"))
ratio_tab[, ratio_raw := acs_share / revelio_share]
ratio_tab[, ratio := pmin(pmax(ratio_raw, RATIO_CAP_LO), RATIO_CAP_HI)]
n_capped <- ratio_tab[ratio_raw != ratio, .N]
cat(sprintf("Ratio table: %d (year x tier) cells, %d capped at [%.2f, %d]\n",
            nrow(ratio_tab), n_capped, RATIO_CAP_LO, RATIO_CAP_HI))
print(ratio_tab[order(calendar_year, tier), .(calendar_year, tier, revelio_share, acs_share, ratio_raw, ratio)])

## -----------------------------------------------------------------------
## SECTION 2: load li, resolve col_end (same gotcha as the other two
## calendar-year scripts)
## -----------------------------------------------------------------------

log_step("Loading column2_reweighted.rds")
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds"))
setDT(li)

resolve_col_end <- function(dt, is_factor_like) {
  raw <- if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
  rng <- range(raw, na.rm = TRUE)
  if (rng[1] < 1900 || rng[2] > 2100) stop("col_end resolved to an implausible range")
  raw
}
col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

T_MAX <- 20

## -----------------------------------------------------------------------
## SECTION 3: year-calibrated metro-tier share (sanity check -- should
## land almost exactly on ACS by construction; this is the "does the
## mechanism work" check, not the real test)
## -----------------------------------------------------------------------

log_step("Computing year-calibrated tier share (sanity check)")
tier_partials <- vector("list", T_MAX + 1)
for (t in 0:T_MAX) {
  col <- paste0("cbsa_code_", t)
  if (!col %in% names(li)) next
  tier <- code_to_tier(li[[col]])
  cy <- col_end_col2 + t
  weight_valid <- !is.na(li$w_full_joint)
  valid <- !is.na(tier) & weight_valid & !is.na(cy) & cy %in% CALIB_YEARS
  if (sum(valid) == 0) next
  dt_t <- data.table(calendar_year = cy[valid], tier = tier[valid], w_base = li$w_full_joint[valid])
  dt_t <- merge(dt_t, ratio_tab[, .(calendar_year, tier, ratio)], by = c("calendar_year", "tier"), all.x = TRUE)
  dt_t[, ratio := fifelse(is.na(ratio), 1, ratio)]
  dt_t[, w_calibrated := w_base * ratio]
  tier_partials[[t + 1]] <- dt_t[, .(w = sum(w_calibrated)), by = .(calendar_year, tier)]
}
tier_calibrated <- rbindlist(tier_partials)[, .(w = sum(w)), by = .(calendar_year, tier)]
tier_calibrated[, share := w / sum(w), by = calendar_year]

sanity <- merge(tier_calibrated[, .(calendar_year, tier, calibrated_share = share)],
                 acs_actual, by = c("calendar_year", "tier"))
cat(sprintf("\nSanity check -- year-calibrated tier share vs ACS, mean abs gap (should be ~0): %.6f\n",
            mean(abs(sanity$calibrated_share - sanity$acs_share))))

## -----------------------------------------------------------------------
## SECTION 4: year-calibrated MIGRATION RATE -- the actual test. Ratios
## were built from TIER shares, not moves, so this checks whether the
## calibration mechanism moves a benchmark it doesn't directly target.
## -----------------------------------------------------------------------

log_step("Computing year-calibrated migration rate (the real test)")
mig_partials <- vector("list", T_MAX)
for (t in 1:T_MAX) {
  cur_col <- paste0("cbsa_state_", t); prev_col <- paste0("cbsa_state_", t - 1)
  tier_col <- paste0("cbsa_code_", t)
  if (!all(c(cur_col, prev_col, tier_col) %in% names(li))) next
  weight_valid <- !is.na(li$w_full_joint)
  cy <- col_end_col2 + t
  valid <- !is.na(li[[cur_col]]) & !is.na(li[[prev_col]]) & weight_valid & !is.na(cy) & cy %in% CALIB_YEARS
  if (sum(valid) == 0) next
  moved <- as.numeric(li[[cur_col]][valid] != li[[prev_col]][valid])
  tier <- code_to_tier(li[[tier_col]][valid])
  dt_t <- data.table(calendar_year = cy[valid], tier = tier, w_base = li$w_full_joint[valid], moved = moved)
  dt_t <- merge(dt_t, ratio_tab[, .(calendar_year, tier, ratio)], by = c("calendar_year", "tier"), all.x = TRUE)
  dt_t[, ratio := fifelse(is.na(ratio), 1, ratio)]
  dt_t[, w_calibrated := w_base * ratio]
  mig_partials[[t]] <- dt_t[, .(sum_w = sum(w_calibrated), sum_wmoved = sum(w_calibrated * moved), n = .N), by = calendar_year]
}
mig_calibrated <- rbindlist(mig_partials)[, .(sum_w = sum(sum_w), sum_wmoved = sum(sum_wmoved), n = sum(n)), by = calendar_year]
mig_calibrated[, rate := sum_wmoved / sum_w]

## Compare against the EXISTING static-weight line and ACS, all restricted
## to the same 2012-2021 window.
existing_mig <- fread(file.path(data_dir, "results/memo1_migration_rate_by_calendar_year.csv"))
static_line <- existing_mig[source == "Column 2 (reweighted to ACS)" & calendar_year %in% CALIB_YEARS, .(calendar_year, static_rate = rate)]
acs_line <- existing_mig[source == "ACS PUMS benchmark" & calendar_year %in% CALIB_YEARS, .(calendar_year, acs_rate = rate)]

comparison <- merge(mig_calibrated[, .(calendar_year, calibrated_rate = rate)], static_line, by = "calendar_year")
comparison <- merge(comparison, acs_line, by = "calendar_year")
comparison[, gap_static := abs(static_rate - acs_rate)]
comparison[, gap_calibrated := abs(calibrated_rate - acs_rate)]
setorder(comparison, calendar_year)
cat("\n=== Migration rate, 2012-2021: static weight vs. year-calibrated weight vs. ACS ===\n")
print(comparison)
cat(sprintf("\nMean absolute gap vs ACS -- static weight: %.5f\n", mean(comparison$gap_static)))
cat(sprintf("Mean absolute gap vs ACS -- year-calibrated weight: %.5f\n", mean(comparison$gap_calibrated)))

## -----------------------------------------------------------------------
## SECTION 5: spot-check a handful of long-panel people by hand
## -----------------------------------------------------------------------

log_step("Spot-checking individual long-panel people")
# [FIXED, same bug class hit earlier this session] li$user_id is
# integer64 (from data.table's fread/readRDS on a large id column) --
# comparing it against a plain double via == or %in% throws "Incompatible
# join types" under bmerge. Sidestep entirely with row-index sampling
# instead of value-based filtering on user_id.
n_years_observed <- sapply(0:T_MAX, function(t) {
  col <- paste0("cbsa_code_", t)
  if (!col %in% names(li)) return(rep(FALSE, nrow(li)))
  !is.na(li[[col]])
})
n_years <- rowSums(n_years_observed)
long_panel_rows <- which(n_years >= 10)
long_panel_rows <- long_panel_rows[sample.int(length(long_panel_rows), min(3, length(long_panel_rows)))]

for (ridx in long_panel_rows) {
  row <- li[ridx]
  cat(sprintf("\n--- user_id %s (w_full_joint = %.3f) ---\n", row$user_id, row$w_full_joint))
  ce <- col_end_col2[ridx]
  for (t in 0:T_MAX) {
    col <- paste0("cbsa_code_", t)
    if (!col %in% names(li) || is.na(row[[col]])) next
    cy <- ce + t
    if (!cy %in% CALIB_YEARS) next
    this_tier <- code_to_tier(row[[col]])[1]
    if (is.na(this_tier)) next
    r <- ratio_tab[calendar_year == cy & tier == this_tier, ratio]
    r <- if (length(r) == 0) 1 else r
    cat(sprintf("  year %d: tier=%s, ratio=%.3f, calibrated contribution=%.3f\n",
                cy, this_tier, r, row$w_full_joint * r))
  }
}

log_step("memo1_phase_a_year_calibration_test.R done.")
