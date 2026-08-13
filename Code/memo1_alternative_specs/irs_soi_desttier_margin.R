# memo1_irs_soi_desttier_margin.R
#
# [NEW 2026-08-11/12] Builds an origin_state x destination_metro_tier
# population margin from IRS SOI's county-to-county outflow file (2020-2021
# vintage, filing year "2021"), for use as a REPLACEMENT for
# reweight_column2.R's origin_state x recent_mover margin -- a direct
# old-vs-new comparison earlier the same day (see HANDOFF.md) showed that
# margin added ~nothing beyond demographic-only reweighting on either the
# migration-rate or metro-tier calendar-year benchmarks. Nicholas's read:
# a binary "moved y/n" margin is redundant with age/race (already in the
# demographic margin and already correlated with mobility), so the more
# promising direction is DESTINATION geography, which nothing tried so far
# captures at all -- directly targeted at the metro-tier chart, the one
# benchmark that showed no improvement whatsoever.
#
# Source: https://www.irs.gov/pub/irs-soi/countyoutflow2021.csv (downloaded
# with explicit permission 2026-08-11, ~4.4MB, aggregate county-to-county
# return counts, no individual-level data) -- single national CSV, not
# per-state files. Single year only, per Nicholas's explicit "start cheap,
# see if it helps before committing further" framing -- if this margin
# earns its keep, a multi-year pull is a natural follow-up, not built here.
#
# Record layout (from the 2020-2021 Migration Data Users Guide,
# https://www.irs.gov/pub/irs-soi/2021inpublicmigdoc.pdf, fetched live
# 2026-08-11 rather than assumed -- confirms flow files carry NO age/AGI
# breakdown at all, correcting an earlier planning-doc assumption; only the
# separate Gross Migration file has age, and only at state level with no
# destination detail):
#   y1_statefips, y1_countyfips  -- origin state/county (year 1)
#   y2_statefips, y2_countyfips  -- destination state/county (year 2)
#   y2_state, y2_countyname, n1 (returns), n2 (individuals), agi
# Special y2_statefips codes to EXCLUDE (not real destinations): 57
# (foreign), 58/59 (Other Flows aggregates -- county-level detail
# suppressed below IRS's 10-return disclosure threshold, no real
# destination county to map to a tier), 96/97/98 (state-level "Total
# Migration" summary rows -- would double-count if kept alongside the
# individual county rows they summarize).
# Non-migrants (y1==y2) ARE kept: a person who stayed still has a
# "destination tier" (their own county's tier) and needs to be represented
# in the margin the same way movers are, since this margin's role
# (replacing the OLD recent_mover margin) is an unconditional joint
# population distribution -- same role margin_demo already plays -- not a
# conditional "where do movers go" distribution.

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

## -----------------------------------------------------------------------
## SECTION 0: CBSA -> tier lookup -- IDENTICAL logic/vintage to
## memo1_metro_tiers.R's Section 0 (CBSA_RANK_YEAR=2022, same 4-tier +
## non-metro scheme), so a county's tier here means exactly the same thing
## it means in the metro-tier chart being compared against. Not cached to
## disk anywhere in this repo (memo1_metro_tiers.R/memo1_puma_cbsa_crosswalk.R
## both rebuild it fresh each run too) -- cheap live API call, same
## established pattern, not a new one.
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
## SECTION 1: load IRS SOI county-to-county outflow file + county->CBSA
## crosswalk (unified_cbsa.csv -- county-based, so no PUMA-vintage risk at
## all, unlike the ACS geography work)
## -----------------------------------------------------------------------

log_step("Loading IRS SOI county outflow file")
irs <- fread(file.path(data_dir, "raw/irs_soi/countyoutflow2021.csv"))
cat(sprintf("Raw rows: %d\n", nrow(irs)))

unified_cbsa <- fread(file.path(data_dir, "raw/census_geo/unified_cbsa.csv"), colClasses = "character")
stopifnot(all(c("cbsa_code", "GeoFIPS") %in% names(unified_cbsa)))

## -----------------------------------------------------------------------
## SECTION 2: filter to genuine county-level rows (movers + non-migrants),
## excluding summary/aggregate/foreign records
## -----------------------------------------------------------------------

n_raw <- nrow(irs)
irs_real <- irs[y1_countyfips != 0 & y2_statefips %in% 1:56 & y2_countyfips != 0]
cat(sprintf("Kept %d of %d rows after excluding state-total/foreign/other-flows summary records (%.1f%%)\n",
            nrow(irs_real), n_raw, 100 * nrow(irs_real) / n_raw))
cat(sprintf("  Of those, %d are non-migrant rows (y1==y2), %d are genuine mover rows\n",
            irs_real[y1_statefips == y2_statefips & y1_countyfips == y2_countyfips, .N],
            irs_real[!(y1_statefips == y2_statefips & y1_countyfips == y2_countyfips), .N]))

## -----------------------------------------------------------------------
## SECTION 3: map destination county -> CBSA -> tier
## -----------------------------------------------------------------------

irs_real[, dest_county_fips := sprintf("%02d%03d", y2_statefips, y2_countyfips)]
irs_real <- merge(irs_real, unified_cbsa, by.x = "dest_county_fips", by.y = "GeoFIPS", all.x = TRUE)
cat(sprintf("Destination county -> CBSA crosswalk match rate: %.1f%% (%d of %d rows unmatched)\n",
            100 * mean(!is.na(irs_real$cbsa_code)), sum(is.na(irs_real$cbsa_code)), nrow(irs_real)))

irs_real[, destination_tier := code_to_tier(cbsa_code)]
n_before_tier_drop <- nrow(irs_real)
irs_real <- irs_real[!is.na(destination_tier)]
cat(sprintf("Dropped %d rows (%.1f%%) with no resolvable destination tier (crosswalk miss and not a recognizable non-metro '999' code)\n",
            n_before_tier_drop - nrow(irs_real), 100 * (n_before_tier_drop - nrow(irs_real)) / n_before_tier_drop))

## -----------------------------------------------------------------------
## SECTION 4: aggregate to origin_state x destination_tier -- origin kept
## at STATE level (not CBSA) to match li_long's existing origin_state
## raking key and keep the cell count tractable (50 states x 5 tiers = 250
## cells, comparable sparsity to the old 51 x 2 mover margin, not the
## county-level granularity IRS actually provides on the origin side)
## -----------------------------------------------------------------------

irs_real[, origin_state := sprintf("%02d", y1_statefips)]
margin_desttier <- irs_real[, .(Freq = sum(n1)), by = .(origin_state, destination_tier)]
setorder(margin_desttier, origin_state, destination_tier)

cat(sprintf("\norigin_state x destination_tier margin: %d cells, %d total returns represented\n",
            nrow(margin_desttier), margin_desttier[, sum(Freq)]))
cat("Overall destination_tier distribution (share of all returns, unconditional on origin):\n")
print(margin_desttier[, .(Freq = sum(Freq)), by = destination_tier][, share := Freq / sum(Freq)][order(-share)])

out_path <- file.path(data_dir, "intermediate/irs_soi_desttier_margin_2021.rds")
saveRDS(list(margin = margin_desttier, cbsa_pop = cbsa_pop), out_path)
log_step(paste("saved", out_path))
