# memo1_metro_tiers.R
#
# Share living in the top-10/top-11-50/top-51-100/other-metro/non-metro
# CBSAs, by years-since-graduation -- Revelio (real CBSA codes, from the
# cbsa_code_0..50 position panel) vs. ACS (PUMA -> CBSA-tier, via the
# population-weighted crosswalk built by Code/memo1_puma_cbsa_crosswalk.R).
# Same "years since grad" framework as Code/memo1_migration_profile.R (ACS
# side uses age-22 as a synthetic-cohort proxy -- see that script's header
# for the caveat).
#
# Run after: Code/acs_pull.R (must include PUMA in its variable list --
# re-pulled 2026-08-10 specifically for this), Code/memo1_puma_cbsa_crosswalk.R,
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

## -----------------------------------------------------------------------
## SECTION 0: CBSA code -> tier (real codes only; rebuilt fresh here since
## it's cheap and Code/memo1_puma_cbsa_crosswalk.R didn't persist it
## standalone -- only the already-aggregated PUMA-level crosswalk)
## -----------------------------------------------------------------------

cbsa_pop <- get_estimates(geography = "cbsa", product = "population", year = 2022, vintage = 2022)
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

# Revelio's own cbsa_code_N panel uses the same unified_cbsa.csv convention
# as the PUMA crosswalk (Code/00_crosswalks.Rmd) -- non-metro positions get
# a "state+999" pseudo-code, same detection logic.
code_to_tier <- function(codes) {
  tier <- cbsa_pop$metro_tier[match(codes, cbsa_pop$cbsa_code)]
  tier[is.na(tier) & grepl("999$", codes)] <- "Non-metro"
  tier
}

## -----------------------------------------------------------------------
## SECTION 1: Revelio -- share by tier, by years-since-grad
## -----------------------------------------------------------------------

revelio_tier_share_by_year <- function(dt, weight_col, t_max, source_label) {
  out <- vector("list", t_max + 1)
  for (t in 0:t_max) {
    col <- paste0("cbsa_code_", t)
    if (!col %in% names(dt)) { out[[t + 1]] <- NULL; next }
    codes <- dt[[col]]
    tier <- code_to_tier(codes)
    # [FIXED 2026-08-10] same NA-weight gotcha as
    # Code/memo1_migration_profile.R's revelio_rate_by_year() -- sum(w) for
    # a group is NA if any contributing row has an NA weight (w_full_joint
    # is NA for ~2.9% of users), so a valid tier row can still poison its
    # group's total. Exclude NA weight explicitly, not just NA tier.
    weight_valid <- if (is.null(weight_col)) TRUE else !is.na(dt[[weight_col]])
    valid <- !is.na(tier) & weight_valid
    n <- sum(valid)
    if (n == 0) { out[[t + 1]] <- NULL; next }
    w <- if (is.null(weight_col)) rep(1, n) else dt[[weight_col]][valid]
    tab <- data.table(tier = tier[valid], w = w)[, .(w = sum(w)), by = tier]
    tab[, share := w / sum(w)]
    out[[t + 1]] <- data.table(source = source_label, years_since_grad = t, tier = tab$tier, share = tab$share, n = n)
  }
  rbindlist(out)
}

log_step("Loading Column 1/2 covariate tables")
column1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds"))
setDT(column1)
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds"))
setDT(li)

log_step("Computing Revelio metro-tier-share-by-year profiles")
tier_col1      <- revelio_tier_share_by_year(column1, NULL, T_MAX, "Column 1 (college-only)")
tier_col2_unwt <- revelio_tier_share_by_year(li, "w_unweighted", T_MAX, "Column 2 (HS+college, unweighted)")
tier_col2_rewt <- revelio_tier_share_by_year(li, "w_full_joint", T_MAX, "Column 2 (reweighted to ACS)")

rm(column1, li)
gc()

## -----------------------------------------------------------------------
## SECTION 2: ACS -- share by tier, by single-year age (-> years-since-grad)
## -----------------------------------------------------------------------

log_step("Loading ACS PUMS pull + PUMA-CBSA-tier crosswalk")
pums_filt <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds"))
setDT(pums_filt)
stopifnot("PUMA20" %in% names(pums_filt))
puma_xwalk <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk.rds"))

# [FOUND 2026-08-10] PUMA20 is only genuinely populated for the ~1-of-5
# pooled 5-year survey years that actually used 2020-vintage PUMA
# boundaries (2022 itself) -- the other ~4/5 (surveyed 2018-2021, still on
# 2010-vintage boundaries) get a "-0009" placeholder (confirmed: 77.9% of
# rows), which correctly fails to match any real PUMA in the crosswalk.
# This is a real constraint of the pooled ACS 5-year file, not a bug --
# restrict explicitly to the valid subset rather than let it silently
# shrink the effective N. Extending to PUMA10 (a second crosswalk vintage)
# would recover the other ~4/5 but isn't built here -- flagged as a future
# option if this subset's sample size proves too thin.
pums_filt[, state_puma := paste0(ST, PUMA20)]
n_before_vintage_filt <- nrow(pums_filt)
pums_filt <- pums_filt[PUMA20 != "-0009"]
cat(sprintf("Restricting to rows with a genuine 2020-vintage PUMA20 (not the '-0009' placeholder): %d of %d (%.1f%%)\n",
            nrow(pums_filt), n_before_vintage_filt, 100 * nrow(pums_filt) / n_before_vintage_filt))
cat(sprintf("ACS state_puma format check (should look like '0100100' etc.): %s\n",
            paste(head(unique(pums_filt$state_puma), 3), collapse = ", ")))
cat(sprintf("state_puma match rate onto crosswalk (within the valid-vintage subset): %.1f%%\n",
            100 * mean(pums_filt$state_puma %in% puma_xwalk$state_puma)))

# Fractional expansion: each respondent contributes PWGTP * tier_share to
# every tier their PUMA overlaps (same "soft" pattern already used for
# race probabilities elsewhere in this pipeline -- Code/reweight_column2.R).
pums_long <- merge(pums_filt[, .(state_puma, age, PWGTP)], puma_xwalk, by = "state_puma", all.x = TRUE, allow.cartesian = TRUE)
pums_long[, w := PWGTP * share]

acs_tier_share_by_age <- function(dt, n_lookup, min_age, max_age) {
  ages <- min_age:max_age
  out <- vector("list", length(ages))
  for (i in seq_along(ages)) {
    a <- ages[i]
    d <- dt[age == a & !is.na(metro_tier)]
    if (nrow(d) == 0) { out[[i]] <- NULL; next }
    tab <- d[, .(w = sum(w, na.rm = TRUE)), by = metro_tier]
    tab[, share := w / sum(w)]
    # n = actual respondent count at this age (from the un-expanded,
    # one-row-per-person table), not nrow(d) -- d is the fractional
    # tier-expansion, which double(+)-counts anyone whose PUMA splits
    # across tiers.
    out[[i]] <- data.table(source = "ACS PUMS benchmark", years_since_grad = a - 22, tier = tab$metro_tier,
                            share = tab$share, n = sum(n_lookup$age == a))
  }
  rbindlist(out)
}
tier_acs <- acs_tier_share_by_age(pums_long, pums_filt, min_age = 23, max_age = 22 + T_MAX)

metro_tier_profile <- rbindlist(list(tier_col1, tier_col2_unwt, tier_col2_rewt, tier_acs), fill = TRUE)

log_step("Sample sizes by years_since_grad and source:")
print(unique(metro_tier_profile[, .(source, years_since_grad, n)])[order(source, years_since_grad)])

fwrite(metro_tier_profile, file.path(data_dir, "results/memo1_metro_tier_by_year.csv"))
saveRDS(metro_tier_profile, file.path(data_dir, "results/memo1_metro_tier_by_year.rds"))
log_step("saved memo1_metro_tier_by_year.csv/.rds")
