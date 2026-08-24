# memo1_03_geo_crosswalks.R
#
# [CONSOLIDATED 2026-08-24, per Nicholas's request to reduce script count]
# Merges what were two files (memo1_03a_puma_cbsa_crosswalk.R,
# memo1_03b_migpuma_cbsa_crosswalk.R) into one -- Section 2 directly
# depends on Section 1's output files (puma_cbsa_tier_crosswalk_*.rds) and
# always runs after it, so merging changes nothing about execution order,
# only file count. No logic changed from either original script.
#
# SECTION 1 (was memo1_03a_puma_cbsa_crosswalk.R): builds a
# population-weighted PUMA -> metro-tier crosswalk, so ACS PUMS
# respondents (who only carry PUMA, not CBSA) can be assigned a
# (fractional) tier for the metro-tier-share-over-time chart, comparable
# to Revelio's real CBSA codes.
#
# Approach (at Nicholas's direction): PUMAs of a given vintage are built
# from whole Census tracts of that SAME vintage (no split tracts), so the
# Census Bureau's own "Tract to PUMA Relationship File" for that vintage
# gives an EXACT tract<->PUMA mapping -- no GIS/shapefile overlay needed for
# that step. Tract GEOIDs encode county FIPS directly, and county->CBSA is
# already solved in this repo (Data/raw/census_geo/unified_cbsa.csv). The
# only real approximation is tract-level population weighting for PUMAs
# whose tracts span more than one CBSA tier (or metro/non-metro) --
# weighted by each tract's Census population (same vintage as the PUMA/
# tract geography), not by area, per Nicholas's instruction.
#
# [RESTRUCTURED 2026-08-12, per Nicholas's request to make trying
# alternative metro-tier classifications cheap] Split in two: Section 1.1
# caches the raw tract-level (tract_geoid, state_puma, STATEFP, cbsa_code,
# tract_pop) table ONCE per PUMA_VINTAGE, independent of any tier scheme;
# Section 1.2 applies a named scheme from
# Code/memo1_00_metro_tier_definitions.R (source()'d, a deliberate
# exception to this project's usual standalone-script convention -- see
# that file's header) to the cached table and aggregates -- cheap, no new
# pull, safe to re-run per scheme.
#
# SECTION 2 (was memo1_03b_migpuma_cbsa_crosswalk.R): builds a MIGPUMA ->
# metro-tier crosswalk so ACS respondents' 1-year-ago residence (MIGPUMA,
# coarser than PUMA) can be assigned a tier, the same way Section 1 does
# for current-residence PUMA.
#
# Key simplification, confirmed live (not assumed) before writing any code:
# a 2010 Migration PUMA is a UNION of one or more whole 2010 PUMAs (IPUMS's
# own documentation: "Each Migration PUMA corresponds exactly to one or
# more PUMAs"). That means NO new tract-level relationship file is needed --
# this just aggregates Section 1's scheme-specific puma_cbsa_tier_crosswalk
# up to the MIGPUMA level, via the PUMA->MIGPUMA composition file.

library(data.table)
library(tidycensus)
library(readxl)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
tidycensus::census_api_key(Sys.getenv("CENSUS_KEY"))
source(here::here("Code/memo1_00_metro_tier_definitions.R"))

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

## ===========================================================================
## SECTION 1: PUMA -> metro-tier crosswalk
## ===========================================================================
log_step("SECTION 1: PUMA -> metro-tier crosswalk")

# ---- Vintage + scheme switches --------------------------------------------
# [EXTENDED 2026-08-13, per Nicholas's request to extend flow calibration
# past its original 2012-2021 window] Looped over BOTH vintages this repo
# has a usable tract-to-PUMA relationship file for, matching the existing
# SCHEME_NAMES loop's already-idempotent pattern (each vintage's cache is
# skipped if it already exists, so re-running this script is always safe).
# 2020-vintage covers ACS 1-year survey years 2022-2023 (2020-vintage PUMA
# boundaries went into effect with the 2022 file). Pre-2012 (2000-vintage)
# is NOT included here -- investigated and deliberately deferred: Census
# never published a modern tract-to-PUMA relationship file for the 2000
# vintage (confirmed by checking Census's own file index), and IPUMS's
# 2000-vintage composition file is a genuinely different, hierarchical
# county/place/tract format requiring new parsing logic, not a drop-in
# vintage swap the way 2020 is. See the memo's Data & Methods appendix for
# the full writeup of what was checked.
#
# 2020 source: https://www2.census.gov/geo/docs/maps-data/data/rel2020/2020_Census_Tract_to_2020_PUMA.txt
#   (downloaded to Data/raw/census_geo/2020_Census_Tract_to_2020_PUMA.txt)
# 2010 source: https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt
#   (downloaded to Data/raw/census_geo/2010_Census_Tract_to_2010_PUMA.txt;
#   same column schema as the 2020 file -- STATEFP,COUNTYFP,TRACTCE,PUMA5CE)
PUMA_VINTAGES <- c(2010, 2020)
SCHEME_NAMES <- c("rank5", "rank3", "rank3_region")  # build crosswalks for all of these

CBSA_RANK_YEAR <- 2022  # deliberately NOT tied to PUMA_VINTAGE (see header)

vintage_cfg_for <- function(puma_vintage) {
  if (puma_vintage == 2010) {
    list(
      rel_file    = "raw/census_geo/2010_Census_Tract_to_2010_PUMA.txt",
      decennial_year = 2010,
      decennial_var  = "P001001",
      decennial_sumfile = "sf1"
    )
  } else if (puma_vintage == 2020) {
    list(
      rel_file    = "raw/census_geo/2020_Census_Tract_to_2020_PUMA.txt",
      decennial_year = 2020,
      decennial_var  = "P1_001N",
      decennial_sumfile = "pl"
    )
  } else {
    stop("PUMA vintage must be 2010 or 2020")
  }
}

for (PUMA_VINTAGE in PUMA_VINTAGES) {
cat(sprintf("\n\n========== PUMA_VINTAGE: %d ==========\n", PUMA_VINTAGE))
vintage_cfg <- vintage_cfg_for(PUMA_VINTAGE)
tract_cache_path <- file.path(data_dir, sprintf("intermediate/tract_puma_cbsa_pop_%d.rds", PUMA_VINTAGE))

## ---- SECTION 1.1 (scheme-independent, cached ONCE): tract -> PUMA ->
## CBSA -> population. No tier assignment here at all. ----

if (!file.exists(tract_cache_path)) {

  ## tract -> PUMA (exact, from the Census relationship file)
  tract_puma <- fread(file.path(data_dir, vintage_cfg$rel_file), colClasses = "character")
  tract_puma[, `:=`(
    county_fips = paste0(STATEFP, COUNTYFP),
    tract_geoid = paste0(STATEFP, COUNTYFP, TRACTCE),
    state_puma  = paste0(STATEFP, PUMA5CE)  # PUMA5CE alone repeats across states; ACS PUMS's own PUMA is ST+PUMA
  )]
  log_step(paste("tract_puma loaded (vintage", PUMA_VINTAGE, "):", nrow(tract_puma), "tracts,", uniqueN(tract_puma$state_puma), "PUMAs"))

  ## county -> CBSA (already-solved crosswalk, this repo's own)
  unified_cbsa <- fread(file.path(data_dir, "raw/census_geo/unified_cbsa.csv"), colClasses = c(GeoFIPS = "character", cbsa_code = "character"))
  tract_puma <- merge(tract_puma, unified_cbsa, by.x = "county_fips", by.y = "GeoFIPS", all.x = TRUE)
  tract_puma[, cbsa_code := as.character(cbsa_code)]

  ## tract population (for weighting split PUMAs), same-vintage Decennial
  ## as PUMA_VINTAGE (2010 SF1 P001001 / 2020 PL P1_001N) -- THE EXPENSIVE
  ## STEP, now cached independent of any tier scheme.
  data(fips_codes, package = "tidycensus")
  setDT(fips_codes)
  valid_state_fips <- unique(fips_codes[state %in% c(state.abb, "DC")]$state_code)
  state_fips_list <- sort(intersect(unique(tract_puma$STATEFP), valid_state_fips))
  cat(sprintf("Restricting to %d of %d STATEFP values present in tract_puma (50 states + DC)\n",
              length(state_fips_list), uniqueN(tract_puma$STATEFP)))
  tract_pop_parts <- vector("list", length(state_fips_list))
  for (i in seq_along(state_fips_list)) {
    st <- state_fips_list[i]
    cat(sprintf("Pulling %d Decennial tract population for state FIPS %s (%d/%d)...\n",
                vintage_cfg$decennial_year, st, i, length(state_fips_list)))
    d <- get_decennial(geography = "tract", variables = vintage_cfg$decennial_var,
                        year = vintage_cfg$decennial_year, sumfile = vintage_cfg$decennial_sumfile, state = st)
    setDT(d)
    tract_pop_parts[[i]] <- d[, .(tract_geoid = GEOID, tract_pop = value)]
  }
  tract_pop <- rbindlist(tract_pop_parts)
  log_step(paste("tract_pop loaded:", nrow(tract_pop), "tracts"))

  tract_puma <- merge(tract_puma, tract_pop, by = "tract_geoid", all.x = TRUE)
  cat(sprintf("Tracts with a population match: %.1f%%\n", 100 * mean(!is.na(tract_puma$tract_pop))))
  tract_puma[is.na(tract_pop), tract_pop := 0]  # a handful of zero-population tracts (e.g. water-only) are fine as 0 weight

  tract_cache <- tract_puma[, .(tract_geoid, state_puma, STATEFP, cbsa_code, tract_pop)]
  saveRDS(tract_cache, tract_cache_path)
  log_step(paste("saved", basename(tract_cache_path), ":", nrow(tract_cache), "tracts (scheme-independent, reusable)"))

} else {
  log_step(paste(basename(tract_cache_path), "already exists -- skipping the expensive tract-population pull"))
}

tract_cache <- readRDS(tract_cache_path)

## ---- SECTION 1.2 (per scheme, cheap): assign tiers + aggregate to PUMA
## level. No Census pull here -- reuses tract_cache and one cheap
## CBSA-population API call per scheme (for the rank lookup, not
## tract-level data). ----

region_lookup <- state_fips_to_region()  # state_fips -> census_region, cheap, reused by any region-crossed scheme

for (scheme_name in SCHEME_NAMES) {
  crosswalk_path <- file.path(data_dir, sprintf("intermediate/puma_cbsa_tier_crosswalk_%d_%s.rds", PUMA_VINTAGE, scheme_name))
  if (file.exists(crosswalk_path)) {
    log_step(paste(basename(crosswalk_path), "already exists -- skipping"))
    next
  }

  log_step(paste("Building crosswalk for scheme:", scheme_name))
  lookup <- build_cbsa_tier_lookup(scheme_name, rank_year = CBSA_RANK_YEAR)
  code_to_tier <- code_to_tier_for_scheme(lookup)

  tp <- copy(tract_cache)
  if (lookup$scheme$uses_region) {
    tp <- merge(tp, region_lookup[, .(STATEFP = state_fips, census_region)], by = "STATEFP", all.x = TRUE)
    n_no_region <- sum(is.na(tp$census_region))
    if (n_no_region > 0) cat(sprintf("  %d tracts (territories outside the 50 states+DC) have no census_region -- dropped\n", n_no_region))
    tp <- tp[!is.na(census_region)]
    tp[, metro_tier := code_to_tier(cbsa_code, region = census_region)]
  } else {
    tp[, metro_tier := code_to_tier(cbsa_code)]
  }
  n_unexplained <- tp[!grepl("999$", cbsa_code) & is.na(metro_tier), .N]
  if (n_unexplained > 0) cat(sprintf("  %d tracts with a real cbsa_code but no tier match (unexplained) -- dropped\n", n_unexplained))
  tp <- tp[!is.na(metro_tier)]

  puma_tier_pop <- tp[, .(pop = sum(tract_pop)), by = .(state_puma, metro_tier)]
  puma_totals <- puma_tier_pop[, .(total_pop = sum(pop)), by = state_puma]
  puma_cbsa_tier_crosswalk <- merge(puma_tier_pop, puma_totals, by = "state_puma")
  puma_cbsa_tier_crosswalk[, share := fifelse(total_pop > 0, pop / total_pop, 0)]
  puma_cbsa_tier_crosswalk <- puma_cbsa_tier_crosswalk[, .(state_puma, metro_tier, share)]

  cat(sprintf("  %s: %d rows, %d distinct PUMAs, %d distinct tiers\n",
              scheme_name, nrow(puma_cbsa_tier_crosswalk), uniqueN(puma_cbsa_tier_crosswalk$state_puma),
              uniqueN(puma_cbsa_tier_crosswalk$metro_tier)))
  cat(sprintf("  PUMAs where population splits across >1 tier: %d of %d\n",
              uniqueN(puma_cbsa_tier_crosswalk[, .N, by = state_puma][N > 1]$state_puma),
              uniqueN(puma_cbsa_tier_crosswalk$state_puma)))

  saveRDS(puma_cbsa_tier_crosswalk, crosswalk_path)
  log_step(paste("saved", basename(crosswalk_path), ":", nrow(puma_cbsa_tier_crosswalk), "rows"))
}
} # end PUMA_VINTAGE loop

log_step("SECTION 1 done.")

## ===========================================================================
## SECTION 2: MIGPUMA -> metro-tier crosswalk
## ===========================================================================
log_step("SECTION 2: MIGPUMA -> metro-tier crosswalk")

# Source: https://usa.ipums.org/usa/resources/volii/puma_migpuma1_pwpuma00_2010.xls
# (downloaded with explicit permission 2026-08-12, ~229KB, a public
# geographic reference table -- see this file's original 2026-08-12
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
# usable relationship file for (2010 and 2020 -- see Section 1's header
# for why 2000/pre-2012 is deliberately excluded). 2020-vintage
# composition file source:
# https://usa.ipums.org/usa/resources/volii/puma_migpuma1_pwpuma00_2020.xls
# (downloaded 2026-08-13, same sheet name and 4-column layout as the 2010
# file, confirmed live before writing code against it).

MIGPUMA_SCHEME_NAMES <- c("rank5", "rank3", "rank3_region")
MIGPUMA_VINTAGE_WINDOWS <- list(`2010` = 2012:2021, `2020` = 2022:2023)

for (PUMA_VINTAGE in names(MIGPUMA_VINTAGE_WINDOWS)) {
cat(sprintf("\n\n========== PUMA_VINTAGE: %s ==========\n", PUMA_VINTAGE))
PUMA_WINDOW <- MIGPUMA_VINTAGE_WINDOWS[[PUMA_VINTAGE]]

## ---- SECTION 2.1 (scheme-independent): PUMA -> MIGPUMA composition ----

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

## ---- SECTION 2.2 (scheme-independent): PUMA population weight, from
## ACS PWGTP ----

# [FIXED 2026-08-13, caught by a suspicious "0 distinct PUMAs" output --
# not assumed clean] The PUMA *column name* is vintage-scoped (PUMA10 for
# the 2010 window, PUMA20 for the 2020 window -- see Section 2 of
# memo1_02_acs_pulls.R), but this line was still hardcoded to PUMA10 after
# the PUMA_WINDOW/PUMA_VINTAGE loop was added, so the 2020-vintage pass
# silently computed population weights for zero PUMAs (querying a column
# that's NA for every 2022-2023 row) and produced an empty crosswalk.
# Column name now derived from PUMA_VINTAGE, matching every other
# vintage-aware reference in this file and in memo1_09b/memo1_09c.
puma_col <- paste0("PUMA", substr(PUMA_VINTAGE, 3, 4))
pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds"))
setDT(pums_1yr)
stopifnot(all(c("ST", "PWGTP", "survey_year", puma_col) %in% names(pums_1yr)))
puma_pop <- pums_1yr[survey_year %in% PUMA_WINDOW & !is.na(get(puma_col)),
                      .(pop = sum(PWGTP)), by = .(state_puma = paste0(ST, get(puma_col)))]
cat(sprintf("PUMA population weights (from ACS PWGTP, %d-%d, %s): %d distinct PUMAs\n",
            min(PUMA_WINDOW), max(PUMA_WINDOW), puma_col, nrow(puma_pop)))
rm(pums_1yr)

## ---- SECTION 2.3 (per scheme, cheap): join and aggregate PUMA-level
## tier shares up to MIGPUMA, population-weighted -- no new pull per
## scheme. ----

for (scheme_name in MIGPUMA_SCHEME_NAMES) {
  out_path <- file.path(data_dir, sprintf("intermediate/migpuma_cbsa_tier_crosswalk_%s_%s.rds", PUMA_VINTAGE, scheme_name))
  puma_tier_path <- file.path(data_dir, sprintf("intermediate/puma_cbsa_tier_crosswalk_%s_%s.rds", PUMA_VINTAGE, scheme_name))
  if (file.exists(out_path)) {
    log_step(paste(basename(out_path), "already exists -- skipping"))
    next
  }
  if (!file.exists(puma_tier_path)) {
    cat(sprintf("SKIPPING %s -- %s not built yet (run Section 1 of this file first)\n", scheme_name, basename(puma_tier_path)))
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

log_step("SECTION 2 done. memo1_03_geo_crosswalks.R done.")
