# reweight_column2_desttier_test.R
#
# [NEW 2026-08-11/12] Standalone TEST script -- does NOT touch
# reweight_column2.R or its committed output (column2_reweighted.rds).
# Nicholas's instruction after the old-vs-new comparison showed the
# origin_state x recent_mover margin added ~nothing beyond demographic-only
# reweighting: drop that margin, replace it with one derived from IRS SOI
# CBSA-to-CBSA (county-to-county, aggregated to metro tier) flows, and
# check whether it does any better -- starting with a single year (2021)
# before committing to a multi-year build. This script is that first check.
# If it shows real improvement, the natural next step is folding this into
# reweight_column2.R proper (replacing margin_mover) and pulling more years;
# not done here, per "start cheap, see if it helps" framing.
#
# Margin: origin_state x destination_metro_tier, built by
# Code/memo1_alternative_specs/irs_soi_desttier_margin.R from IRS SOI's countyoutflow2021.csv
# (single national file, 2020-2021 vintage, downloaded with explicit
# permission -- see that script's header for full provenance/caveats,
# including the state-level-origin/tractability tradeoff and the
# non-migrant-inclusion rationale).
#
# Sample-side destination_tier: each li row's OWN most-recently-observed
# CBSA (fcoalesce over cbsa_code_50..cbsa_code_0, i.e. latest non-NA
# position first), mapped to a tier using the IDENTICAL CBSA population
# ranking (CBSA_RANK_YEAR=2022) saved alongside the margin -- sample and
# population sides must agree on what a "tier" means, or the raking target
# is silently wrong.
#
# Everything else (li load, hs_state->FIPS origin_state, race-fractional
# melt, margin_demo, manual_ipf mechanism) is copied verbatim from
# reweight_column2.R -- see that script for the full debugging history
# behind manual_ipf() (why rake()/calibrate() were abandoned). Not
# refactored into a shared file; both scripts are standalone by the
# existing convention.

library(data.table)
library(tidycensus)
library(dotenv)
library(here)

load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob",
                     "native_prob", "multiple_prob", "hispanic_prob")

## =========================================================================
## PART 1: build the demographics + destination-tier raking weight
## =========================================================================

li <- readRDS(file.path(data_dir, "intermediate/column2_covariates.rds"))
setDT(li)
stopifnot(all(c("user_id", "hs_state", "age_bucket", RACE_PROB_COLS, "m_prob", "f_prob",
                "col_end") %in% names(li)))

if (is.character(li$transfer)) {
  li[, transfer := as.integer(transfer == "Transfer")]
}

pums_cells <- readRDS(file.path(data_dir, "intermediate/pums_cells.rds"))
cell_state_age_race_sex <- pums_cells$state_age_racesex

data(fips_codes, package = "tidycensus")
abb_to_fips <- unique(fips_codes[, c("state", "state_code")])
setDT(abb_to_fips)
setnames(abb_to_fips, c("state", "state_code"), c("state_abbr", "state_fips"))

li <- merge(li, abb_to_fips, by.x = "hs_state", by.y = "state_abbr", all.x = TRUE)
setnames(li, "state_fips", "origin_state")
cat(sprintf("hs_state -> FIPS match rate: %.1f%%\n", 100 * mean(!is.na(li$origin_state))))

li[, w_unweighted := 1]

## ---- destination_tier: each person's most-recently-observed CBSA -------
## fcoalesce(cbsa_code_50, ..., cbsa_code_0) takes the first non-NA value in
## argument order, i.e. the LATEST observed position if available, falling
## back to progressively earlier years -- the person-level analog of
## reweight_column2.R's moved_last_year_state ("last observed year pair"),
## but for location rather than a move flag.
irs_margin_obj <- readRDS(file.path(data_dir, "intermediate/irs_soi_desttier_margin_2021.rds"))
margin_desttier_pop <- irs_margin_obj$margin
cbsa_pop <- irs_margin_obj$cbsa_pop

code_to_tier <- function(codes) {
  tier <- cbsa_pop$metro_tier[match(codes, cbsa_pop$cbsa_code)]
  tier[is.na(tier) & grepl("999$", codes)] <- "Non-metro"
  tier
}

cbsa_code_cols <- paste0("cbsa_code_", 50:0)
stopifnot(all(cbsa_code_cols %in% names(li)))
# [FIXED, same bug class seen repeatedly this session] some cbsa_code_<t>
# columns (far-out t, few careers span that long) are entirely NA and get
# read/stored as logical rather than character/integer -- fcoalesce()
# requires uniform type across its arguments. Coerce every column to
# character explicitly before coalescing (character is also what
# code_to_tier()'s match() against cbsa_pop$cbsa_code expects).
cbsa_code_chr <- lapply(li[, ..cbsa_code_cols], as.character)
last_cbsa <- do.call(fcoalesce, cbsa_code_chr)
li[, destination_tier := code_to_tier(last_cbsa)]
cat(sprintf("destination_tier resolved for %.1f%% of rows (%d of %d)\n",
            100 * mean(!is.na(li$destination_tier)), sum(!is.na(li$destination_tier)), nrow(li)))

## ---- fractional race-probability row-expansion (unchanged) -------------
li_complete <- li[!is.na(origin_state) & !is.na(age_bucket) & !is.na(white_prob) & !is.na(destination_tier)]
log_step(paste("li_complete (non-missing origin_state/age_bucket/race_prob/destination_tier):", nrow(li_complete), "of", nrow(li)))

li_long <- melt(
  li_complete,
  id.vars = setdiff(names(li_complete), RACE_PROB_COLS),
  measure.vars = RACE_PROB_COLS,
  variable.name = "race_prob_col",
  value.name = "race_frac"
)
race_col_to_label <- setNames(RACE_LABELS, RACE_PROB_COLS)
li_long[, race := race_col_to_label[as.character(race_prob_col)]]
li_long[, sex := fifelse(m_prob >= f_prob, "male", "female")]
li_long[, w_base := w_unweighted * race_frac]

## ---- margins -------------------------------------------------------------
margin_demo <- copy(cell_state_age_race_sex)
setnames(margin_demo, "pop", "Freq")
for (col in c("origin_state", "age_bucket", "race", "sex")) margin_demo[[col]] <- as.character(margin_demo[[col]])
li_long[, `:=`(origin_state = as.character(origin_state), age_bucket = as.character(age_bucket),
                race = as.character(race), sex = as.character(sex),
                destination_tier = as.character(destination_tier))]
margin_desttier_pop[, `:=`(origin_state = as.character(origin_state), destination_tier = as.character(destination_tier))]

demo_key_cols <- c("origin_state", "age_bucket", "race", "sex")
n_before_cell_match <- nrow(li_long)
li_long <- li_long[margin_demo[, ..demo_key_cols], on = demo_key_cols, nomatch = 0]
cat(sprintf("Dropped for no matching ACS demo cell: %d of %d melted rows (%.1f%%)\n",
            n_before_cell_match - nrow(li_long), n_before_cell_match,
            100 * (n_before_cell_match - nrow(li_long)) / n_before_cell_match))

li_long_keys <- unique(li_long[, ..demo_key_cols])
n_before_margin_restrict <- nrow(margin_demo)
margin_demo <- margin_demo[li_long_keys, on = demo_key_cols, nomatch = 0]
cat(sprintf("Restricted margin_demo to cells with sample coverage: %d of %d ACS cells kept (%.1f%%)\n",
            nrow(margin_demo), n_before_margin_restrict, 100 * nrow(margin_demo) / n_before_margin_restrict))

desttier_key_cols <- c("origin_state", "destination_tier")
n_before_dt_match <- nrow(li_long)
li_long <- li_long[margin_desttier_pop[, ..desttier_key_cols], on = desttier_key_cols, nomatch = 0]
cat(sprintf("Dropped for no matching IRS desttier cell: %d of %d melted rows (%.1f%%)\n",
            n_before_dt_match - nrow(li_long), n_before_dt_match,
            100 * (n_before_dt_match - nrow(li_long)) / n_before_dt_match))
n_before_dt_restrict <- nrow(margin_desttier_pop)
margin_desttier_pop <- margin_desttier_pop[unique(li_long[, ..desttier_key_cols]), on = desttier_key_cols, nomatch = 0]
cat(sprintf("Restricted margin_desttier to cells with sample coverage: %d of %d cells kept\n",
            nrow(margin_desttier_pop), n_before_dt_restrict))

## ---- manual_ipf (verbatim from reweight_column2.R) ----------------------
manual_ipf <- function(dt, w_col, margins, maxit = 50, epsilon = 1, cap_lo = 0.05, cap_hi = 20, verbose = FALSE) {
  dt <- copy(dt)
  dt[, .rowid := .I]
  dt[, w_iter := get(w_col)]
  old_w <- dt$w_iter
  iter <- 0; converged <- FALSE
  while (iter < maxit) {
    for (m in margins) {
      keys <- m$keys; pop <- m$pop
      cell_sum <- dt[, .(sample_sum = sum(w_iter)), by = keys]
      r <- merge(cell_sum, pop, by = keys, all.x = TRUE)
      r[, ratio := fifelse(!is.na(Freq) & sample_sum > 0, Freq / sample_sum, 1)]
      r[, ratio := pmin(pmax(ratio, cap_lo), cap_hi)]
      dt <- merge(dt, r[, c(keys, "ratio"), with = FALSE], by = keys, all.x = TRUE)
      dt[, ratio := fifelse(is.na(ratio), 1, ratio)]
      dt[, w_iter := w_iter * ratio]
      dt[, ratio := NULL]
    }
    setorder(dt, .rowid)
    delta <- max(abs(dt$w_iter - old_w))
    if (verbose) cat(sprintf("  [manual_ipf] iter=%d delta=%.4f any_nonfinite=%s\n", iter, delta, any(!is.finite(dt$w_iter))))
    if (is.finite(delta) && delta < epsilon) { converged <- TRUE; break }
    old_w <- dt$w_iter
    iter <- iter + 1
  }
  if (!converged) warning(sprintf("manual_ipf did not converge after %d iterations (delta=%.4f, epsilon=%d)", iter, delta, epsilon))
  cat(sprintf("manual_ipf: %s after %d iteration(s) (delta=%.4f), any non-finite: %s\n",
              if (converged) "converged" else "DID NOT CONVERGE", iter, delta, any(!is.finite(dt$w_iter))))
  setorder(dt, .rowid)
  dt$w_iter
}

margins_spec <- list(
  list(keys = demo_key_cols, pop = margin_demo),
  list(keys = desttier_key_cols, pop = margin_desttier_pop)
)

## ---- small-subsample validation -----------------------------------------
set.seed(20260812)
li_long_test <- li_long[sample.int(.N, min(100000, .N))]
margin_demo_test <- margin_demo[unique(li_long_test[, ..demo_key_cols]), on = demo_key_cols, nomatch = 0]
margin_dt_test <- margin_desttier_pop[unique(li_long_test[, ..desttier_key_cols]), on = desttier_key_cols, nomatch = 0]
li_long_test <- li_long_test[margin_demo_test[, ..demo_key_cols], on = demo_key_cols, nomatch = 0]
li_long_test <- li_long_test[margin_dt_test[, ..desttier_key_cols], on = desttier_key_cols, nomatch = 0]
cat(sprintf("Subsample self-consistent margins: %d demo cells, %d desttier cells; %d rows remain\n",
            nrow(margin_demo_test), nrow(margin_dt_test), nrow(li_long_test)))

w_test <- manual_ipf(li_long_test, "w_base",
                      list(list(keys = demo_key_cols, pop = margin_demo_test),
                           list(keys = desttier_key_cols, pop = margin_dt_test)),
                      verbose = TRUE)
cat(sprintf("Subsample IPF test (n=%d melted rows): weight range [%.3f, %.3f], any non-finite: %s\n",
            length(w_test), min(w_test), max(w_test), any(!is.finite(w_test))))
if (any(!is.finite(w_test))) stop("Subsample IPF test produced non-finite weights -- investigate before running at full scale.")

## ---- full-scale IPF -------------------------------------------------------
log_step("Running full-scale manual_ipf() (demo + desttier margins)")
li_long[, w_raked := manual_ipf(li_long, "w_base", margins_spec, verbose = TRUE)]

w_full_collapsed <- li_long[, .(w_desttier_uncapped = sum(w_raked, na.rm = TRUE)), by = user_id]
li <- merge(li, w_full_collapsed, by = "user_id", all.x = TRUE)

med_w <- median(li$w_desttier_uncapped, na.rm = TRUE)
cap_hi <- med_w * 20
cap_lo <- med_w * 0.05
n_capped_hi <- sum(li$w_desttier_uncapped > cap_hi, na.rm = TRUE)
n_capped_lo <- sum(li$w_desttier_uncapped < cap_lo, na.rm = TRUE)
li[, w_desttier := pmin(pmax(w_desttier_uncapped, cap_lo), cap_hi)]
cat(sprintf("Final weight cap [%.3f, %.3f] (0.05x-20x median %.3f): capped %d rows high, %d rows low, of %d\n",
            cap_lo, cap_hi, med_w, n_capped_hi, n_capped_lo, nrow(li)))

saveRDS(li, file.path(data_dir, "intermediate/column2_reweighted_desttier_test.rds"))
log_step("saved column2_reweighted_desttier_test.rds")

## =========================================================================
## PART 2: cheap single-year (2021) comparison against existing benchmarks
## =========================================================================
## Reuses Data/results/memo1_metro_tier_by_calendar_year.csv's ALREADY-
## COMPUTED 2021 rows for ACS, unweighted Column 2, and the old
## demographics+mover Column 2 line -- no need to re-pull ACS 1-year PUMS
## (the expensive ~15min step) just to check a single year. Only Revelio's
## NEW weight needs computing fresh here, using the exact same
## t-slice-then-collapse pattern memo1_metro_tiers.R uses.

log_step("Computing 2021 tier share under the new demographics+desttier weight")
resolve_col_end <- function(dt, is_factor_like) {
  raw <- if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
  rng <- range(raw, na.rm = TRUE)
  if (rng[1] < 1900 || rng[2] > 2100) stop("col_end resolved to an implausible range")
  raw
}
col_end_numeric <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

T_MAX <- 20
TARGET_YEAR <- 2021
tier_partials <- vector("list", T_MAX + 1)
for (t in 0:T_MAX) {
  col <- paste0("cbsa_code_", t)
  if (!col %in% names(li)) next
  tier <- code_to_tier(li[[col]])
  weight_valid <- !is.na(li$w_desttier)
  valid <- !is.na(tier) & weight_valid & !is.na(col_end_numeric) & (col_end_numeric + t == TARGET_YEAR)
  if (sum(valid) == 0) next
  tier_partials[[t + 1]] <- data.table(tier = tier[valid], w = li$w_desttier[valid])[, .(w = sum(w)), by = tier]
}
tier_2021_new <- rbindlist(tier_partials)[, .(w = sum(w)), by = tier]
tier_2021_new[, share := w / sum(w)]
setorder(tier_2021_new, -share)
cat("\n=== Column 2, NEW (demographics + IRS desttier) weight, 2021 tier share ===\n")
print(tier_2021_new)

cat("\n=== Comparison benchmarks already on disk (memo1_metro_tier_by_calendar_year.csv, calendar_year==2021) ===\n")
existing <- fread(file.path(data_dir, "results/memo1_metro_tier_by_calendar_year.csv"))
print(existing[calendar_year == 2021][order(source, tier)])

# Mean absolute gap vs ACS, restricted to 2021, for a direct old-vs-new-vs-
# unweighted comparison on the SAME single year this new margin was built
# from -- deliberately not claiming this generalizes to other years yet.
acs_2021 <- existing[source == "ACS PUMS benchmark" & calendar_year == 2021, .(tier, acs_share = share)]
for (src in c("Column 2 (HS+college, unweighted)", "Column 2 (reweighted to ACS)")) {
  s <- existing[source == src & calendar_year == 2021, .(tier, share)]
  m <- merge(s, acs_2021, by = "tier")
  cat(sprintf("\n%s, 2021 mean abs gap vs ACS: %.5f\n", src, mean(abs(m$share - m$acs_share))))
}
m_new <- merge(tier_2021_new[, .(tier, share)], acs_2021, by = "tier")
cat(sprintf("\nColumn 2 (demographics + IRS desttier), 2021 mean abs gap vs ACS: %.5f\n", mean(abs(m_new$share - m_new$acs_share))))

log_step("reweight_column2_desttier_test.R done.")
