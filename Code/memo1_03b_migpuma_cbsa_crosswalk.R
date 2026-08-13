# memo1_03b_migpuma_cbsa_crosswalk.R
#
# Builds a MIGPUMA -> metro-tier crosswalk so ACS respondents' 1-year-ago
# residence (MIGPUMA, coarser than PUMA) can be assigned a tier, the same
# way memo1_puma_cbsa_crosswalk.R does for current-residence PUMA.
#
# Key simplification, confirmed live (not assumed) before writing any code:
# a 2010 Migration PUMA is a UNION of one or more whole 2010 PUMAs (IPUMS's
# own documentation: "Each Migration PUMA corresponds exactly to one or
# more PUMAs"). That means NO new tract-level relationship file is needed --
# this just aggregates memo1_puma_cbsa_crosswalk.R's scheme-specific
# puma_cbsa_tier_crosswalk up to the MIGPUMA level, via the PUMA->MIGPUMA
# composition file.
#
# [RESTRUCTURED 2026-08-12, per Nicholas's request] Scheme-parameterized,
# matching memo1_puma_cbsa_crosswalk.R's restructuring -- the PUMA->MIGPUMA
# composition table and the ACS PWGTP population weights are BOTH
# scheme-independent (already on disk, no new pull), so building a new
# scheme's MIGPUMA crosswalk is cheap regardless of which
# Code/memo1_00_metro_tier_definitions.R scheme the underlying PUMA crosswalk used.
#
# Source: https://usa.ipums.org/usa/resources/volii/puma_migpuma1_pwpuma00_2010.xls
# (downloaded with explicit permission 2026-08-12, ~229KB, a public
# geographic reference table -- see this script's original 2026-08-12
# header / HANDOFF.md for the full provenance, including the WebFetch
# URL-fabrication incident caught before downloading, and the 3-digit
# state-code padding fix).
#
# Population weighting for aggregating multiple PUMAs into one MIGPUMA:
# ACS 1-year PUMS's own PWGTP summed by PUMA (already on disk in
# pums_1yr_filt.rds), restricted to that vintage's own survey-year window --
# the exact BA+/employed/under-65 filtered population this margin
# represents, not general Decennial population.
#
# [EXTENDED 2026-08-13] Looped over both PUMA vintages this repo has a
# usable relationship file for (2010 and 2020 -- see memo1_puma_cbsa_
# crosswalk.R's header for why 2000/pre-2012 is deliberately excluded).
# 2020-vintage composition file source:
# https://usa.ipums.org/usa/resources/volii/puma_migpuma1_pwpuma00_2020.xls
# (downloaded 2026-08-13, same sheet name and 4-column layout as the 2010
# file, confirmed live before writing code against it).

library(data.table)
library(readxl)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

SCHEME_NAMES <- c("rank5", "rank3", "rank3_region")
VINTAGE_WINDOWS <- list(`2010` = 2012:2021, `2020` = 2022:2023)

for (PUMA_VINTAGE in names(VINTAGE_WINDOWS)) {
cat(sprintf("\n\n========== PUMA_VINTAGE: %s ==========\n", PUMA_VINTAGE))
PUMA_WINDOW <- VINTAGE_WINDOWS[[PUMA_VINTAGE]]

## -----------------------------------------------------------------------
## SECTION 1 (scheme-independent): PUMA -> MIGPUMA composition
## -----------------------------------------------------------------------

log_step("Loading PUMA->MIGPUMA composition file")
puma_migpuma_raw <- read_excel(
  file.path(data_dir, sprintf("raw/census_geo/puma_migpuma1_pwpuma00_%s.xls", PUMA_VINTAGE)),
  sheet = "PUMA_POWPUMA_MIGPUMA", skip = 1
)
setDT(puma_migpuma_raw)
setnames(puma_migpuma_raw, c("ST", "PUMA", "migplac_state_raw", "migpuma_raw"))
puma_migpuma <- puma_migpuma_raw[, .(
  state_puma = sprintf("%02d%s", as.integer(ST), PUMA),
  state_migpuma = sprintf("%02d%s", as.integer(migplac_state_raw), migpuma_raw)
)]
cat(sprintf("Distinct state_puma: %d, distinct state_migpuma: %d\n",
            uniqueN(puma_migpuma$state_puma), uniqueN(puma_migpuma$state_migpuma)))

## -----------------------------------------------------------------------
## SECTION 2 (scheme-independent): PUMA population weight, from ACS PWGTP
## -----------------------------------------------------------------------

# [FIXED 2026-08-13, caught by a suspicious "0 distinct PUMAs" output --
# not assumed clean] The PUMA *column name* is vintage-scoped (PUMA10 for
# the 2010 window, PUMA20 for the 2020 window -- see memo1_02b's header),
# but this line was still hardcoded to PUMA10 after the PUMA_WINDOW/
# PUMA_VINTAGE loop was added, so the 2020-vintage pass silently computed
# population weights for zero PUMAs (querying a column that's NA for every
# 2022-2023 row) and produced an empty crosswalk. Column name now derived
# from PUMA_VINTAGE, matching every other vintage-aware reference in this
# script and in memo1_07b/memo1_07c.
puma_col <- paste0("PUMA", substr(PUMA_VINTAGE, 3, 4))
pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds"))
setDT(pums_1yr)
stopifnot(all(c("ST", "PWGTP", "survey_year", puma_col) %in% names(pums_1yr)))
puma_pop <- pums_1yr[survey_year %in% PUMA_WINDOW & !is.na(get(puma_col)),
                      .(pop = sum(PWGTP)), by = .(state_puma = paste0(ST, get(puma_col)))]
cat(sprintf("PUMA population weights (from ACS PWGTP, %d-%d, %s): %d distinct PUMAs\n",
            min(PUMA_WINDOW), max(PUMA_WINDOW), puma_col, nrow(puma_pop)))

## -----------------------------------------------------------------------
## SECTION 3 (per scheme, cheap): join and aggregate PUMA-level tier
## shares up to MIGPUMA, population-weighted -- no new pull per scheme.
## -----------------------------------------------------------------------

for (scheme_name in SCHEME_NAMES) {
  out_path <- file.path(data_dir, sprintf("intermediate/migpuma_cbsa_tier_crosswalk_%s_%s.rds", PUMA_VINTAGE, scheme_name))
  puma_tier_path <- file.path(data_dir, sprintf("intermediate/puma_cbsa_tier_crosswalk_%s_%s.rds", PUMA_VINTAGE, scheme_name))
  if (file.exists(out_path)) {
    log_step(paste(basename(out_path), "already exists -- skipping"))
    next
  }
  if (!file.exists(puma_tier_path)) {
    cat(sprintf("SKIPPING %s -- %s not built yet (run memo1_puma_cbsa_crosswalk.R first)\n", scheme_name, basename(puma_tier_path)))
    next
  }

  log_step(paste("Building MIGPUMA crosswalk for scheme:", scheme_name))
  puma_tier <- readRDS(puma_tier_path)
  setDT(puma_tier)

  joined <- merge(puma_migpuma, puma_tier, by = "state_puma", allow.cartesian = TRUE)
  n_puma_no_tier_match <- uniqueN(puma_migpuma$state_puma) - uniqueN(joined$state_puma)
  cat(sprintf("  Distinct PUMAs in composition file with NO match in %s: %d of %d\n",
              basename(puma_tier_path), n_puma_no_tier_match, uniqueN(puma_migpuma$state_puma)))

  joined <- merge(joined, puma_pop, by = "state_puma", all.x = TRUE)
  joined[is.na(pop), pop := 0]  # a PUMA never sampled by ACS in this window drops out of the weighted average

  joined[, weighted_share := share * pop]
  migpuma_pop_totals <- unique(joined[, .(state_puma, state_migpuma, pop)])[, .(pop_total = sum(pop)), by = state_migpuma]
  migpuma_tier_pop <- joined[, .(w = sum(weighted_share)), by = .(state_migpuma, metro_tier)]
  migpuma_cbsa_tier_crosswalk <- merge(migpuma_tier_pop, migpuma_pop_totals, by = "state_migpuma")
  migpuma_cbsa_tier_crosswalk[, share := fifelse(pop_total > 0, w / pop_total, NA_real_)]
  migpuma_cbsa_tier_crosswalk <- migpuma_cbsa_tier_crosswalk[!is.na(share), .(state_migpuma, metro_tier, share)]

  n_zero_pop_migpuma <- uniqueN(migpuma_pop_totals[pop_total == 0]$state_migpuma)
  share_check <- migpuma_cbsa_tier_crosswalk[, .(total_share = sum(share)), by = state_migpuma]
  n_bad_share <- share_check[abs(total_share - 1) > 0.01, .N]
  cat(sprintf("  %s: %d rows, %d distinct MIGPUMAs (%d zero-population, dropped), %d bad-share MIGPUMAs (expect 0)\n",
              scheme_name, nrow(migpuma_cbsa_tier_crosswalk), uniqueN(migpuma_cbsa_tier_crosswalk$state_migpuma),
              n_zero_pop_migpuma, n_bad_share))

  saveRDS(migpuma_cbsa_tier_crosswalk, out_path)
  log_step(paste("saved", basename(out_path)))
}
} # end PUMA_VINTAGE loop
