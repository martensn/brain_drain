# memo1_03a_puma_cbsa_crosswalk.R
#
# Builds a population-weighted PUMA -> metro-tier crosswalk, so ACS PUMS
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
# alternative metro-tier classifications cheap] Previously this script
# pulled tract-level Decennial population AND assigned tiers AND
# aggregated to PUMA level in one pass, with only the FINAL tier-level
# output cached -- meaning every new classification scheme required
# re-pulling ~50 states of Decennial tract population from the Census API
# again, the expensive step. Now split in two: SECTION 4 caches the raw
# tract-level (tract_geoid, state_puma, STATEFP, cbsa_code, tract_pop)
# table ONCE per PUMA_VINTAGE, independent of any tier scheme; SECTION 5
# applies a named scheme from Code/memo1_00_metro_tier_definitions.R (source()'d,
# a deliberate exception to this project's usual standalone-script
# convention -- see that file's header) to the cached table and
# aggregates -- cheap, no new pull, safe to re-run per scheme.
#
# [CHANGED 2026-08-11] Parameterized by PUMA_VINTAGE at Nicholas's request,
# to support a calendar-year migration/metro-tier chart redesign that needs
# ONE fixed PUMA vintage held constant across many ACS 1-year survey years
# (PUMA boundaries get redrawn each decennial, so mixing vintages within one
# chart would make "moved into a Top-10 metro" partly an artifact of
# redistricting rather than a real geographic change). PUMA_VINTAGE=2010
# covers ACS 1-year survey years 2012-2021 (the longest single-vintage
# window available); PUMA_VINTAGE=2020 (the original, still supported) was
# built first for the pooled 2018-2022 5-year file's 2022-only PUMA20 slice.
#
# 2020 source: https://www2.census.gov/geo/docs/maps-data/data/rel2020/2020_Census_Tract_to_2020_PUMA.txt
#   (downloaded to Data/raw/census_geo/2020_Census_Tract_to_2020_PUMA.txt)
# 2010 source: https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt
#   (downloaded to Data/raw/census_geo/2010_Census_Tract_to_2010_PUMA.txt;
#   same column schema as the 2020 file -- STATEFP,COUNTYFP,TRACTCE,PUMA5CE)

library(data.table)
library(tidycensus)
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

## -----------------------------------------------------------------------
## SECTION 4 (scheme-independent, cached ONCE): tract -> PUMA -> CBSA ->
## population. No tier assignment here at all.
## -----------------------------------------------------------------------

if (!file.exists(tract_cache_path)) {

  ## SECTION 1: tract -> PUMA (exact, from the Census relationship file)
  tract_puma <- fread(file.path(data_dir, vintage_cfg$rel_file), colClasses = "character")
  tract_puma[, `:=`(
    county_fips = paste0(STATEFP, COUNTYFP),
    tract_geoid = paste0(STATEFP, COUNTYFP, TRACTCE),
    state_puma  = paste0(STATEFP, PUMA5CE)  # PUMA5CE alone repeats across states; ACS PUMS's own PUMA is ST+PUMA
  )]
  log_step(paste("tract_puma loaded (vintage", PUMA_VINTAGE, "):", nrow(tract_puma), "tracts,", uniqueN(tract_puma$state_puma), "PUMAs"))

  ## SECTION 2: county -> CBSA (already-solved crosswalk, this repo's own)
  unified_cbsa <- fread(file.path(data_dir, "raw/census_geo/unified_cbsa.csv"), colClasses = c(GeoFIPS = "character", cbsa_code = "character"))
  tract_puma <- merge(tract_puma, unified_cbsa, by.x = "county_fips", by.y = "GeoFIPS", all.x = TRUE)
  tract_puma[, cbsa_code := as.character(cbsa_code)]

  ## SECTION 3: tract population (for weighting split PUMAs), same-vintage
  ## Decennial as PUMA_VINTAGE (2010 SF1 P001001 / 2020 PL P1_001N) --
  ## THE EXPENSIVE STEP, now cached independent of any tier scheme.
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

## -----------------------------------------------------------------------
## SECTION 5 (per scheme, cheap): assign tiers + aggregate to PUMA level.
## No Census pull here -- reuses tract_cache and one cheap CBSA-population
## API call per scheme (for the rank lookup, not tract-level data).
## -----------------------------------------------------------------------

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
