# memo1_metro_tiers.R
#
# [REDESIGNED 2026-08-11 at Nicholas's request] x-axis changed from years-
# since-graduation (lifecycle) to CALENDAR YEAR (secular trend) -- has the
# population's spatial distribution shifted toward superstar metros over
# the sample period, not how does an individual's own location evolve over
# a career. Same four sources (Column 1, Column 2 unweighted, Column 2
# reweighted, ACS benchmark), same tier scheme (Top 10/Top 11-50/
# Top 51-100/Other metro/Non-metro CBSAs, ranked by a single fixed 2022
# population snapshot -- unchanged, see Section 0).
#
# Revelio: same calendar_year = col_end + t re-bucketing as
# Code/memo1_migration_profile.R (see that script's header for the full
# rationale and the col_end-type gotcha). Revelio's line is NOT bounded to
# any particular window -- its CBSA assignment comes from this repo's own
# unified_cbsa.csv, a single fixed scheme, not PUMA-vintage-dependent.
#
# ACS: previously a synthetic-cohort age proxy against the pooled 5-year
# file's ~22%-valid PUMA20 subset. Now uses Code/acs_pull_1yr.R's ACS
# 1-year PUMS, restricted to survey years 2012-2021 -- the window where
# ACS 1-year files used 2010-vintage PUMA boundaries -- joined against
# Code/memo1_puma_cbsa_crosswalk.R's PUMA_VINTAGE=2010 crosswalk
# (puma_cbsa_tier_crosswalk_2010.rds). This is a REAL, CONFIRMED asymmetry,
# not a bug: the ACS benchmark line in THIS chart only covers 2012-2021,
# while Revelio's lines cover its full calendar range -- worth a sentence
# in the memo. Every row within that 2012-2021 window carries one genuine
# PUMA vintage (unlike the old pooled-5-year file's ~78% "-0009" placeholder
# problem), so the match rate onto the crosswalk should be close to 100%,
# not ~22% -- checked explicitly below.
#
# Run after: Code/acs_pull_1yr.R (pums_1yr_filt.rds, PUMA10 populated for
# 2012-2021), Code/memo1_puma_cbsa_crosswalk.R (PUMA_VINTAGE=2010),
# Code/memo1_covariates.R (column1/2), Code/reweight_column2.R
# (column2_reweighted.rds).

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

T_MAX <- 20
ACS_PUMA_WINDOW <- 2012:2021  # matches Code/memo1_puma_cbsa_crosswalk.R's PUMA_VINTAGE=2010 coverage

## -----------------------------------------------------------------------
## SECTION 0: CBSA code -> tier (unchanged from the years-since-grad
## version -- a single fixed 2022 population snapshot, independent of
## PUMA_VINTAGE; see Code/memo1_puma_cbsa_crosswalk.R's header note on why
## tier THRESHOLDS and PUMA geography-matching vary independently)
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
## SECTION 1: Revelio -- share by tier, by calendar year
## -----------------------------------------------------------------------
## Same t-slice-then-collapse-immediately pattern as
## Code/memo1_migration_profile.R's revelio_rate_by_calendar_year() -- never
## materializes a full person x t long table.

resolve_col_end <- function(dt, is_factor_like) {
  raw <- if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
  rng <- range(raw, na.rm = TRUE)
  if (rng[1] < 1900 || rng[2] > 2100) {
    stop(sprintf("col_end resolved to an implausible range [%d, %d] -- likely a factor-level-code bug, not real years", rng[1], rng[2]))
  }
  cat(sprintf("  col_end range: %d-%d (sane)\n", rng[1], rng[2]))
  raw
}

revelio_tier_share_by_calendar_year <- function(dt, weight_col, t_max, source_label, col_end_numeric) {
  partials <- vector("list", t_max + 1)
  for (t in 0:t_max) {
    col <- paste0("cbsa_code_", t)
    if (!col %in% names(dt)) next
    tier <- code_to_tier(dt[[col]])
    # [carried over unchanged, FIXED 2026-08-10] exclude NA weight
    # explicitly, not just NA tier -- sum(w) for a group is NA if any
    # contributing row has an NA weight.
    weight_valid <- if (is.null(weight_col)) TRUE else !is.na(dt[[weight_col]])
    valid <- !is.na(tier) & weight_valid & !is.na(col_end_numeric)
    if (sum(valid) == 0) next
    w  <- if (is.null(weight_col)) rep(1, sum(valid)) else dt[[weight_col]][valid]
    cy <- col_end_numeric[valid] + t
    partials[[t + 1]] <- data.table(calendar_year = cy, tier = tier[valid], w = w)[
      , .(w = sum(w), n = .N), by = .(calendar_year, tier)]
  }
  agg <- rbindlist(partials)[, .(w = sum(w), n = sum(n)), by = .(calendar_year, tier)]
  agg[, share := w / sum(w), by = calendar_year]
  agg[, .(source = source_label, calendar_year, tier, share, n)]
}

log_step("Loading Column 1/2 covariate tables")
column1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds"))
setDT(column1)
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds"))
setDT(li)

log_step("Resolving col_end to real integer years (per-table, see memo1_migration_profile.R's header note)")
cat("Column 1 col_end:\n")
col_end_col1 <- resolve_col_end(column1, is_factor_like = FALSE)
cat("Column 2 col_end:\n")
col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

log_step("Computing Revelio metro-tier-share-by-calendar-year profiles")
tier_col1      <- revelio_tier_share_by_calendar_year(column1, NULL, T_MAX, "Column 1 (college-only)", col_end_col1)
tier_col2_unwt <- revelio_tier_share_by_calendar_year(li, "w_unweighted", T_MAX, "Column 2 (HS+college, unweighted)", col_end_col2)
tier_col2_rewt <- revelio_tier_share_by_calendar_year(li, "w_full_joint", T_MAX, "Column 2 (reweighted to ACS)", col_end_col2)

rm(column1, li)
gc()

## -----------------------------------------------------------------------
## SECTION 2: ACS -- share by tier, by calendar year (2012-2021 only)
## -----------------------------------------------------------------------

log_step("Loading ACS 1-year PUMS pull + 2010-vintage PUMA-CBSA-tier crosswalk")
pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds"))
setDT(pums_1yr)
stopifnot("PUMA10" %in% names(pums_1yr))
puma_xwalk <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2010.rds"))

pums_window <- pums_1yr[survey_year %in% ACS_PUMA_WINDOW & !is.na(PUMA10)]
pums_window[, state_puma := paste0(ST, PUMA10)]
cat(sprintf("ACS rows in the %d-%d PUMA window: %d\n", min(ACS_PUMA_WINDOW), max(ACS_PUMA_WINDOW), nrow(pums_window)))
cat(sprintf("ACS state_puma format check (should look like '1100100' etc.): %s\n",
            paste(head(unique(pums_window$state_puma), 3), collapse = ", ")))
# [First-class check] every row in this window carries ONE genuine PUMA
# vintage (unlike the old pooled-5-year file's ~78% "-0009" placeholder
# problem) -- match rate should be close to 100%. A low rate here means
# something is still pointed at the wrong vintage.
match_rate <- 100 * mean(pums_window$state_puma %in% puma_xwalk$state_puma)
cat(sprintf("state_puma match rate onto the 2010-vintage crosswalk: %.1f%% (expect close to 100%%)\n", match_rate))

# Fractional expansion: each respondent contributes PWGTP * tier_share to
# every tier their PUMA overlaps (same "soft" pattern used elsewhere in
# this pipeline).
pums_long <- merge(pums_window[, .(state_puma, survey_year, PWGTP)], puma_xwalk,
                    by = "state_puma", all.x = TRUE, allow.cartesian = TRUE)
pums_long[, w := PWGTP * share]

acs_tier_share_by_calendar_year <- function(dt, n_lookup, years) {
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    y <- years[i]
    d <- dt[survey_year == y & !is.na(metro_tier)]
    if (nrow(d) == 0) next
    tab <- d[, .(w = sum(w, na.rm = TRUE)), by = metro_tier]
    tab[, share := w / sum(w)]
    # n = actual respondent count at this year (from the un-expanded table),
    # not nrow(d) -- d fractionally expands split-PUMA respondents.
    out[[i]] <- data.table(source = "ACS PUMS benchmark", calendar_year = y, tier = tab$metro_tier,
                            share = tab$share, n = sum(n_lookup$survey_year == y))
  }
  rbindlist(out)
}
tier_acs <- acs_tier_share_by_calendar_year(pums_long, pums_window, ACS_PUMA_WINDOW)

metro_tier_profile <- rbindlist(list(tier_col1, tier_col2_unwt, tier_col2_rewt, tier_acs), fill = TRUE)

## -----------------------------------------------------------------------
## SECTION 3: verification checks
## -----------------------------------------------------------------------

log_step("Sample sizes by calendar_year and source:")
print(unique(metro_tier_profile[, .(source, calendar_year, n)])[order(source, calendar_year)])

share_sums <- metro_tier_profile[, .(total_share = sum(share)), by = .(source, calendar_year)]
n_bad <- nrow(share_sums[abs(total_share - 1) > 0.01])
cat(sprintf("(source, calendar_year) groups where tier shares don't sum to ~1: %d of %d (expect 0)\n",
            n_bad, nrow(share_sums)))

fwrite(metro_tier_profile, file.path(data_dir, "results/memo1_metro_tier_by_calendar_year.csv"))
saveRDS(metro_tier_profile, file.path(data_dir, "results/memo1_metro_tier_by_calendar_year.rds"))
log_step("saved memo1_metro_tier_by_calendar_year.csv/.rds")
