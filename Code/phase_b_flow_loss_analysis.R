# phase_b_flow_loss_analysis.R
#
# [NEW 2026-08-21] Standalone descriptive analysis, NOT part of Memo 1's
# own write-up. Nicholas's question: how many of Phase B's potential
# origin->destination flow calculations are lost to gaps in individual
# work histories, for post-1980-birth Column 2 (HS-disclosing college
# grads)? "Potential" = if every grad reported an uninterrupted work
# history from graduation through the present, how many (person x
# calendar-year) flow transitions COULD Phase B use, within its actual
# calibration window (CALIB_YEARS = 2012-2023, 2020 excluded)? "Realized"
# = how many of those actually have BOTH a defined origin tier (t-1) AND
# destination tier (t), the exact same `valid` condition
# memo1_06b_scheme_comparison.R's rev_partials loop uses -- reproduced
# here exactly, not approximated, so the loss numbers are accurate to
# what Phase B itself actually loses (tier-resolution failures included,
# not just raw cbsa_code_t missingness).
#
# Two loss measures, both requested:
#   1. Percent of potential PERSON-YEAR FLOWS missing (unweighted count) --
#      note a single missing calendar year can break up to TWO flows (the
#      one into it and the one out of it), so this is not the same number
#      as the omission analysis's plain "years missing" from earlier today.
#   2. Percent of potential Stage-1-weighted p-hat MASS missing -- since
#      w_full_joint is person-level and time-invariant, "potential mass"
#      for a cohort is sum(w_full_joint) x (number of eligible transition
#      years for that cohort); "realized mass" only counts w_full_joint
#      for person-years where the flow is actually defined. If missingness
#      were unrelated to weight magnitude, measures 1 and 2 would be
#      numerically close -- checked directly below, not assumed.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
source(here::here("Code/memo1_00_metro_tier_definitions.R"))

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

CALIB_YEARS <- setdiff(2012:2023, 2020)  # Phase B's actual full-sample window (memo1_06b, extended 2026-08-21)
T_MAX <- 50
MIN_BIRTH_YEAR <- 1980

log_step("Loading Column 2 (Phase B's own analysis sample)")
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds")); setDT(li)

col_end_int <- as.integer(if (is.factor(li$col_end) || is.character(li$col_end)) as.character(li$col_end) else li$col_end)
birth_int <- as.integer(as.character(li$birth))
li[, `:=`(col_end_i = ..col_end_int, birth_i = ..birth_int)]  # ..prefix: pull from the calling scope, not li's own (factor) col_end/birth columns of the same name -- data.table's NSE would otherwise silently resolve the bare names to li's own columns
li <- li[birth_i >= MIN_BIRTH_YEAR & !is.na(col_end_i)]
cat(sprintf("Post-%d-birth Column 2 sample: %d users\n", MIN_BIRTH_YEAR, nrow(li)))

## ---- rank3_region tier resolver -- IDENTICAL to memo1_06b's, not
## re-derived, so "realized" here means exactly what Phase B itself would
## count as a valid flow. ----
lookup <- build_cbsa_tier_lookup("rank3_region")
code_to_tier_cbsa <- code_to_tier_for_scheme(lookup)
region_for_state <- setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
tier_from_code <- function(code_vals, state_vals) code_to_tier_cbsa(code_vals, region = unname(region_for_state[state_vals]))

## ---- per-cohort (grad year) potential vs. realized flows ----
cohorts <- sort(unique(li$col_end_i[li$col_end_i >= MIN_BIRTH_YEAR + 15 & li$col_end_i <= max(CALIB_YEARS)]))
# (a floor of birth+15 just guards against a nonsensical grad year -- not a real restriction, everyone here already has a real col_end)

parts <- vector("list", length(cohorts))
for (i in seq_along(cohorts)) {
  G <- cohorts[i]
  idx <- which(li$col_end_i == G)
  n_grads <- length(idx)
  eligible_years <- CALIB_YEARS[CALIB_YEARS > G]
  n_eligible <- length(eligible_years)
  if (n_eligible == 0 || n_grads == 0) {
    parts[[i]] <- data.table(grad_year = G, n_grads = n_grads, n_eligible_years = n_eligible,
                              potential_flows = 0, realized_flows = 0,
                              potential_mass = 0, realized_mass = 0)
    next
  }

  w_sub <- li$w_full_joint[idx]
  realized_flows <- 0L
  realized_mass <- 0

  for (y in eligible_years) {
    t <- y - G
    cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
    cur_state_col <- paste0("cbsa_state_", t); prev_state_col <- paste0("cbsa_state_", t - 1)
    if (!all(c(cur_col, prev_col, cur_state_col, prev_state_col) %in% names(li))) next  # beyond T_MAX, no data possible -- still "potential" (counted above), just never realized

    dest_tier <- tier_from_code(li[[cur_col]][idx], li[[cur_state_col]][idx])
    origin_tier <- tier_from_code(li[[prev_col]][idx], li[[prev_state_col]][idx])
    valid <- !is.na(dest_tier) & !is.na(origin_tier) & !is.na(w_sub)

    realized_flows <- realized_flows + sum(valid)
    realized_mass <- realized_mass + sum(w_sub[valid])
  }

  parts[[i]] <- data.table(
    grad_year = G, n_grads = n_grads, n_eligible_years = n_eligible,
    potential_flows = n_grads * n_eligible,
    realized_flows = realized_flows,
    potential_mass = sum(w_sub, na.rm = TRUE) * n_eligible,  # na.rm: ~2.9% of Column 2 users have w_full_joint==NA (no matching PUMS cell, per HANDOFF.md) -- they can never contribute mass to Phase B, so they shouldn't inflate the potential baseline either
    realized_mass = realized_mass
  )
  if (i %% 5 == 0) log_step(sprintf("  processed grad_year=%d (%d/%d cohorts)", G, i, length(cohorts)))
}

tab <- rbindlist(parts)
tab[, missing_flows := potential_flows - realized_flows]
tab[, pct_flows_missing := 100 * missing_flows / potential_flows]
tab[, missing_mass := potential_mass - realized_mass]
tab[, pct_mass_missing := 100 * missing_mass / potential_mass]

fwrite(tab, file.path(data_dir, "results/phase_b_flow_loss_by_grad_year.csv"))

cat("\n=== By grad year ===\n")
print(tab[, .(grad_year, n_grads, n_eligible_years, potential_flows, realized_flows,
               pct_flows_missing = round(pct_flows_missing, 1), pct_mass_missing = round(pct_mass_missing, 1))])

overall <- tab[, .(
  n_grads = sum(n_grads), potential_flows = sum(potential_flows), realized_flows = sum(realized_flows),
  potential_mass = sum(potential_mass), realized_mass = sum(realized_mass)
)]
overall[, `:=`(pct_flows_missing = 100 * (potential_flows - realized_flows) / potential_flows,
               pct_mass_missing = 100 * (potential_mass - realized_mass) / potential_mass)]
cat("\n=== Overall (post-1980-birth, pooled across grad years) ===\n")
print(overall)
fwrite(overall, file.path(data_dir, "results/phase_b_flow_loss_overall.csv"))

log_step("phase_b_flow_loss_analysis.R done.")
