# memo1_puma_cbsa_crosswalk.R
#
# Builds a population-weighted PUMA -> CBSA-population-tier crosswalk, so
# ACS PUMS respondents (who only carry PUMA, not CBSA) can be assigned a
# (fractional) top-10/top-50/top-100/other-metro/non-metro tier for the
# metro-tier-share-over-time chart, comparable to Revelio's real CBSA codes.
#
# Approach (at Nicholas's direction): 2020 PUMAs are built from whole 2020
# Census tracts (no split tracts), so the Census Bureau's own "2020 Census
# Tract to 2020 PUMA Relationship File" gives an EXACT tract<->PUMA mapping
# -- no GIS/shapefile overlay needed for that step. Tract GEOIDs encode
# county FIPS directly, and county->CBSA is already solved in this repo
# (Data/raw/census_geo/unified_cbsa.csv). The only real approximation is
# tract-level population weighting for PUMAs whose tracts span more than
# one CBSA tier (or metro/non-metro) -- weighted by each tract's 2020
# Census population, not by area, per Nicholas's instruction.
#
# Source: https://www2.census.gov/geo/docs/maps-data/data/rel2020/2020_Census_Tract_to_2020_PUMA.txt
# (downloaded to Data/raw/census_geo/2020_Census_Tract_to_2020_PUMA.txt)

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

crosswalk_path <- file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk.rds")

if (!file.exists(crosswalk_path)) {

## -----------------------------------------------------------------------
## SECTION 1: tract -> PUMA (exact, from the Census relationship file)
## -----------------------------------------------------------------------

tract_puma <- fread(file.path(data_dir, "raw/census_geo/2020_Census_Tract_to_2020_PUMA.txt"), colClasses = "character")
tract_puma[, `:=`(
  county_fips = paste0(STATEFP, COUNTYFP),
  tract_geoid = paste0(STATEFP, COUNTYFP, TRACTCE),
  state_puma  = paste0(STATEFP, PUMA5CE)  # PUMA5CE alone repeats across states; ACS PUMS's own PUMA is ST+PUMA
)]
log_step(paste("tract_puma loaded:", nrow(tract_puma), "tracts,", uniqueN(tract_puma$state_puma), "PUMAs"))

## -----------------------------------------------------------------------
## SECTION 2: county -> CBSA (already-solved crosswalk, this repo's own)
## -----------------------------------------------------------------------

unified_cbsa <- fread(file.path(data_dir, "raw/census_geo/unified_cbsa.csv"), colClasses = c(GeoFIPS = "character", cbsa_code = "character"))
tract_puma <- merge(tract_puma, unified_cbsa, by.x = "county_fips", by.y = "GeoFIPS", all.x = TRUE)
# unified_cbsa.csv's cbsa_code is NOT "NA for non-metro" -- non-metro
# counties get a synthetic pseudo-code (Code/00_crosswalks.Rmd L145-151:
# paste0(state_code, "999")), so a real CBSA population-estimate join
# legitimately fails to match these (by design, not a bug) -- detect and
# label them explicitly as "Non-metro" rather than leaving them as an
# unexplained join failure.
tract_puma[, is_non_metro_pseudo := grepl("999$", cbsa_code)]
cat(sprintf("Tracts genuinely outside any real CBSA (non-metro pseudo-code): %.1f%%\n",
            100 * mean(tract_puma$is_non_metro_pseudo, na.rm = TRUE)))

## -----------------------------------------------------------------------
## SECTION 3: CBSA population ranking -> tier (top10/top50/top100/other)
## -----------------------------------------------------------------------

cbsa_pop <- get_estimates(geography = "cbsa", product = "population", year = 2022, vintage = 2022)
setDT(cbsa_pop)
cbsa_pop <- cbsa_pop[variable == "POPESTIMATE", .(cbsa_code = as.character(GEOID), cbsa_pop = value)]
tract_puma[, cbsa_code := as.character(cbsa_code)]
setorder(cbsa_pop, -cbsa_pop)
cbsa_pop[, cbsa_rank := seq_len(.N)]
cbsa_pop[, metro_tier := fcase(
  cbsa_rank <= 10, "Top 10",
  cbsa_rank <= 50, "Top 11-50",
  cbsa_rank <= 100, "Top 51-100",
  default = "Other metro"
)]
log_step(paste("CBSA population ranking built:", nrow(cbsa_pop), "CBSAs"))

tract_puma <- merge(tract_puma, cbsa_pop[, .(cbsa_code, metro_tier)], by = "cbsa_code", all.x = TRUE)
tract_puma[is_non_metro_pseudo == TRUE, metro_tier := "Non-metro"]
# Any remaining NA metro_tier here is a genuine, unexplained gap (a real
# CBSA code that didn't match cbsa_pop's population-estimate CBSA set,
# e.g. a vintage/delineation mismatch) -- flag it plainly rather than
# silently dropping or reclassifying it.
n_unexplained <- sum(is.na(tract_puma$metro_tier))
cat(sprintf("Tracts with unexplained NA metro_tier (real cbsa_code, no population-estimate match): %d (%.2f%%)\n",
            n_unexplained, 100 * n_unexplained / nrow(tract_puma)))
if (n_unexplained > 0) {
  cat("Unmatched cbsa_codes:\n")
  print(unique(tract_puma[is.na(metro_tier)]$cbsa_code))
}

## -----------------------------------------------------------------------
## SECTION 4: tract population (for weighting split PUMAs), 2020 Decennial
## -----------------------------------------------------------------------

# Restrict to 50 states + DC, matching Code/acs_pull.R's own scope --
# tract_puma's STATEFP also includes territories (PR, Guam, etc.), which
# aren't part of get_pums()'s state list here and would 404 against
# get_decennial() the same way (confirmed: FIPS 66 = Guam failed this way
# on first run).
data(fips_codes, package = "tidycensus")
setDT(fips_codes)
valid_state_fips <- unique(fips_codes[state %in% c(state.abb, "DC")]$state_code)
state_fips_list <- sort(intersect(unique(tract_puma$STATEFP), valid_state_fips))
cat(sprintf("Restricting to %d of %d STATEFP values present in tract_puma (50 states + DC)\n",
            length(state_fips_list), uniqueN(tract_puma$STATEFP)))
tract_pop_parts <- vector("list", length(state_fips_list))
for (i in seq_along(state_fips_list)) {
  st <- state_fips_list[i]
  cat(sprintf("Pulling 2020 tract population for state FIPS %s (%d/%d)...\n", st, i, length(state_fips_list)))
  d <- get_decennial(geography = "tract", variables = "P1_001N", year = 2020, state = st)
  setDT(d)
  tract_pop_parts[[i]] <- d[, .(tract_geoid = GEOID, tract_pop = value)]
}
tract_pop <- rbindlist(tract_pop_parts)
log_step(paste("tract_pop loaded:", nrow(tract_pop), "tracts"))

tract_puma <- merge(tract_puma, tract_pop, by = "tract_geoid", all.x = TRUE)
cat(sprintf("Tracts with a population match: %.1f%%\n", 100 * mean(!is.na(tract_puma$tract_pop))))
tract_puma[is.na(tract_pop), tract_pop := 0]  # a handful of zero-population tracts (e.g. water-only) are fine as 0 weight

## -----------------------------------------------------------------------
## SECTION 5: aggregate to PUMA x metro_tier population shares
## -----------------------------------------------------------------------

puma_tier_pop <- tract_puma[, .(pop = sum(tract_pop)), by = .(state_puma, metro_tier)]
puma_totals <- puma_tier_pop[, .(total_pop = sum(pop)), by = state_puma]
puma_cbsa_tier_crosswalk <- merge(puma_tier_pop, puma_totals, by = "state_puma")
puma_cbsa_tier_crosswalk[, share := fifelse(total_pop > 0, pop / total_pop, 0)]
puma_cbsa_tier_crosswalk <- puma_cbsa_tier_crosswalk[, .(state_puma, metro_tier, share)]

cat(sprintf("PUMAs where population splits across >1 metro_tier: %d of %d\n",
            uniqueN(puma_cbsa_tier_crosswalk[, .N, by = state_puma][N > 1]$state_puma),
            uniqueN(puma_cbsa_tier_crosswalk$state_puma)))
cat("Sample:\n")
print(puma_cbsa_tier_crosswalk[state_puma %in% sample(unique(state_puma), 5)][order(state_puma)])

saveRDS(puma_cbsa_tier_crosswalk, crosswalk_path)
log_step(paste("saved puma_cbsa_tier_crosswalk.rds:", nrow(puma_cbsa_tier_crosswalk), "rows"))

} else {
  log_step("puma_cbsa_tier_crosswalk.rds already exists -- skipping")
}
