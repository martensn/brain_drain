# memo1_07d_cohort_demo_table.R
#
# [NEW 2026-08-13] Per Nicholas's request: a demographic (race / sex /
# region) cross-tab comparing every weighting scheme shown in the cohort
# charts, at one fixed calendar year (2015), for both birth cohorts. Two
# purposes he named directly: (1) race and sex are NOT part of Phase A/B's
# calibration target (that's tier/flow, not demographics), so checking
# whether they still track ACS under Phase A/B is a genuine out-of-sample
# validity check, unlike tier share which is calibrated by construction;
# (2) this doubles as the demographic-composition input for the paper's
# Table 1.
#
# Columns = exactly the 8 lines already plotted in chart 1 of each cohort
# artifact: col1 (Column 1, unweighted), col2u (Column 2, unweighted),
# col2r (Column 2, static-reweighted), phasea/phaseb (rank5 scheme),
# phasea_region/phaseb_region (rank3_region scheme), acs (ACS PUMS
# benchmark). rank3's own Phase A/B are NOT included -- they were never a
# plotted line either.
#
# Two real complications, both handled explicitly rather than
# approximated:
#
# 1. RACE under col2r/phasea/phaseb needs the TRUE post-raking race split,
#    not a naive re-application of pre-IPF race_frac to the already-
#    collapsed w_full_joint. Race is part of the Stage-1 raking key
#    (state x age_bucket x race x sex) -- a person's white_prob=0.7/
#    black_prob=0.3 pre-IPF split can shift after raking, since their
#    white-cell and black-cell ratios differ (ACS's population composition
#    differs by race within the same state/age/sex stratum). But
#    column2_reweighted_<cohort>.rds only stores the person-COLLAPSED
#    w_full_joint (summed across the race melt during
#    Code/memo1_07a_cohort_inputs.R) -- the race-cell-level detail needed to
#    get this right doesn't survive to disk. This script re-runs the exact
#    same melt + manual_ipf() (copied verbatim, not sourced, per this
#    project's convention) to recover it, but uses the fresh run only for
#    RELATIVE race proportions per person -- the ABSOLUTE per-person total
#    still comes from the already-saved, already-capped w_full_joint, so a
#    float/ordering difference between this run and the original can't
#    introduce a magnitude discrepancy, only (at most) a proportion one
#    (and the same margins/inputs/deterministic algorithm should reproduce
#    identical proportions regardless).
#
# 2. Code/memo1_02b_acs_pull_1yr.R deliberately never pulled ACS race/sex (its own
#    header explains why: RAC2P's coding scheme drifts across ACS 1-year
#    vintages, a real verification burden that chart didn't need). A
#    small, separate, single-year supplementary pull
#    (Code/memo1_02c_acs_pull_1yr_race2015.R) fills this in for 2015 only, verified
#    live against that specific vintage before trusting it (see that
#    script's header) -- joined back on here via SERIALNO+SPORDER.
#
# Run after Code/memo1_07a_cohort_inputs.R, Code/memo1_cohort_scheme_
# comparison.R (needs its phase_ratios_<cohort>_<scheme>.rds exports), and
# Code/memo1_02c_acs_pull_1yr_race2015.R.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
source(here::here("Code/memo1_00_metro_tier_definitions.R"))

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

FIXED_YEAR <- 2015
T_MAX <- 20
PUMS_YEAR <- 2022  # keep in sync with Code/memo1_02a_acs_pull_5yr.R
RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob", "native_prob", "multiple_prob", "hispanic_prob")
RATIO_CAP_LO <- 0.05
RATIO_CAP_HI <- 20
COHORTS <- list(born_1980s = c(1980, 1989), born_1990s = c(1990, 1999))
SCHEMES_FOR_TABLE <- c("rank5", "rank3_region")  # matches the lines actually plotted in chart 1

region_for_state_abbr <- setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
region_fips_lookup <- state_fips_to_region()
region_for_fips <- setNames(region_fips_lookup$census_region, region_fips_lookup$state_fips)

lookups <- setNames(lapply(SCHEMES_FOR_TABLE, build_cbsa_tier_lookup), SCHEMES_FOR_TABLE)
code_to_tier_fns <- setNames(lapply(lookups, code_to_tier_for_scheme), SCHEMES_FOR_TABLE)
tier_from_code_for <- function(scheme, code_vals, state_vals) {
  lookup <- lookups[[scheme]]
  if (lookup$scheme$uses_region) {
    code_to_tier_fns[[scheme]](code_vals, region = unname(region_for_state_abbr[state_vals]))
  } else {
    code_to_tier_fns[[scheme]](code_vals)
  }
}

# ---- manual_ipf(): copied verbatim from Code/memo1_04_reweight_column2.R /
# Code/memo1_07a_cohort_inputs.R, same reason as those files: reproducing the
# exact deterministic algorithm on the exact same inputs is what makes the
# race-share re-derivation above trustworthy.
manual_ipf <- function(dt, w_col, margins, maxit = 10, epsilon = 1, cap_lo = 0.05, cap_hi = 20, verbose = FALSE) {
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
    if (verbose) cat(sprintf("  [manual_ipf] iter=%d delta=%.4f\n", iter, delta))
    if (is.finite(delta) && delta < epsilon) { converged <- TRUE; break }
    old_w <- dt$w_iter
    iter <- iter + 1
  }
  cat(sprintf("manual_ipf: %s after %d iteration(s)\n", if (converged) "converged" else "DID NOT CONVERGE", iter))
  setorder(dt, .rowid)
  dt$w_iter
}

log_step("Loading ACS 2015 race/sex supplement")
acs_race2015 <- readRDS(file.path(data_dir, "intermediate/pums_1yr_race2015.rds"))
setDT(acs_race2015)
acs_race2015[, `:=`(SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER))]

## =========================================================================
## Per-cohort t-slice extractor: for each row of `dt`, find the ONE t in
## [0, T_MAX] where col_end+t == FIXED_YEAR (at most one match per person,
## since t is a strictly increasing function of calendar year for fixed
## col_end), and return that row's current AND prior-year cbsa_code/state.
## Same t-loop-then-filter pattern used throughout this project's other
## calendar-year code (memo1_migration_profile.R etc.) -- reused here for
## an identical reason: a person's applicable t varies row-by-row, so a
## single column selection can't do this, but slicing by t and keeping
## only the rows landing on FIXED_YEAR can.
## =========================================================================
extract_fixed_year <- function(dt, col_end_vec, t_max = T_MAX) {
  parts <- vector("list", t_max + 1)
  for (t in 0:t_max) {
    code_col <- paste0("cbsa_code_", t); state_col <- paste0("cbsa_state_", t)
    if (!code_col %in% names(dt)) next
    cy <- col_end_vec + t
    idx <- which(cy == FIXED_YEAR)
    if (length(idx) == 0) next
    prev_code_col <- paste0("cbsa_code_", t - 1); prev_state_col <- paste0("cbsa_state_", t - 1)
    has_prev <- t >= 1 && prev_code_col %in% names(dt)
    parts[[t + 1]] <- data.table(
      row_idx = idx,
      cbsa_code = dt[[code_col]][idx], cbsa_state = dt[[state_col]][idx],
      cbsa_code_prev = if (has_prev) dt[[prev_code_col]][idx] else NA_character_,
      cbsa_state_prev = if (has_prev) dt[[prev_state_col]][idx] else NA_character_
    )
  }
  rbindlist(parts)
}

all_out <- vector("list", length(COHORTS))
names(all_out) <- names(COHORTS)

for (cohort_name in names(COHORTS)) {
  rng <- COHORTS[[cohort_name]]
  cat(sprintf("\n\n========== COHORT: %s ==========\n", cohort_name))

  log_step("Loading cohort files")
  li <- readRDS(file.path(data_dir, sprintf("intermediate/column2_reweighted_%s.rds", cohort_name)))
  setDT(li)
  col1 <- readRDS(file.path(data_dir, sprintf("intermediate/column1_covariates_%s.rds", cohort_name)))
  setDT(col1)
  pums_1yr <- readRDS(file.path(data_dir, sprintf("intermediate/pums_1yr_filt_%s.rds", cohort_name)))
  setDT(pums_1yr)

  ## ---- Re-derive true post-IPF race shares (see header point 1) ----
  log_step("Re-deriving race-cell-level post-IPF weights")
  pums_acs5_all <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds"))
  setDT(pums_acs5_all)
  pums_acs5_all[, birth_approx := PUMS_YEAR - as.integer(AGEP)]
  pums_filt <- pums_acs5_all[birth_approx >= rng[1] & birth_approx <= rng[2]]

  li_complete <- li[!is.na(origin_state) & !is.na(age_bucket) & !is.na(white_prob)]
  li_long <- melt(
    li_complete, id.vars = setdiff(names(li_complete), RACE_PROB_COLS),
    measure.vars = RACE_PROB_COLS, variable.name = "race_prob_col", value.name = "race_frac"
  )
  race_col_to_label <- setNames(RACE_LABELS, RACE_PROB_COLS)
  li_long[, race := race_col_to_label[as.character(race_prob_col)]]
  li_long[, sex := fifelse(m_prob >= f_prob, "male", "female")]
  li_long[, w_base := w_unweighted * race_frac]
  li_long <- li_long[!is.na(moved_last_year_state)]

  cell_state_age_race_sex <- pums_filt[, .(pop = sum(PWGTP)), by = .(origin_state, age_bucket, race, sex)]
  margin_demo <- copy(cell_state_age_race_sex)
  setnames(margin_demo, "pop", "Freq")
  for (col in c("origin_state", "age_bucket", "race", "sex")) margin_demo[[col]] <- as.character(margin_demo[[col]])
  li_long[, `:=`(origin_state = as.character(origin_state), age_bucket = as.character(age_bucket),
                  race = as.character(race), sex = as.character(sex),
                  moved_last_year_state = as.character(moved_last_year_state))]

  margin_mover <- pums_filt[, .(Freq = sum(PWGTP)), by = .(origin_state, moved_out_of_state)]
  setnames(margin_mover, "moved_out_of_state", "moved_last_year_state")
  margin_mover[, `:=`(origin_state = as.character(origin_state), moved_last_year_state = as.character(moved_last_year_state))]

  demo_key_cols <- c("origin_state", "age_bucket", "race", "sex")
  li_long <- li_long[margin_demo[, ..demo_key_cols], on = demo_key_cols, nomatch = 0]
  li_long_keys <- unique(li_long[, ..demo_key_cols])
  margin_demo <- margin_demo[li_long_keys, on = demo_key_cols, nomatch = 0]

  mover_key_cols <- c("origin_state", "moved_last_year_state")
  li_long <- li_long[margin_mover[, ..mover_key_cols], on = mover_key_cols, nomatch = 0]
  margin_mover <- margin_mover[unique(li_long[, ..mover_key_cols]), on = mover_key_cols, nomatch = 0]

  margins_spec <- list(list(keys = demo_key_cols, pop = margin_demo), list(keys = mover_key_cols, pop = margin_mover))
  li_long[, w_raked := manual_ipf(li_long, "w_base", margins_spec, verbose = TRUE)]

  race_share <- li_long[, .(w_raked = sum(w_raked)), by = .(user_id, race)]
  race_share[, race_share := w_raked / sum(w_raked), by = user_id]
  race_share_wide <- dcast(race_share, user_id ~ race, value.var = "race_share", fill = 0)
  stopifnot(all(RACE_LABELS %in% names(race_share_wide)))

  li <- merge(li, race_share_wide, by = "user_id", all.x = TRUE)
  li[, sex_hard := fifelse(m_prob >= f_prob, "male", "female")]
  rm(li_long, li_complete); gc()

  ## ---- Phase A/B ratio tables ----
  ratios <- setNames(lapply(SCHEMES_FOR_TABLE, function(s)
    readRDS(file.path(data_dir, sprintf("intermediate/phase_ratios_%s_%s.rds", cohort_name, s)))), SCHEMES_FOR_TABLE)

  ## ---- col_end ----
  col_end_col2 <- as.integer(as.character(li$col_end))
  col_end_col1 <- as.integer(col1$col_end)

  ## ---- FIXED_YEAR cross-section extraction ----
  log_step(sprintf("Extracting calendar year %d cross-section", FIXED_YEAR))
  slice2 <- extract_fixed_year(li, col_end_col2)
  slice2[, `:=`(
    user_id = li$user_id[row_idx], w_unweighted = li$w_unweighted[row_idx], w_full_joint = li$w_full_joint[row_idx],
    sex = li$sex_hard[row_idx], region = unname(region_for_state_abbr[cbsa_state])
  )]
  for (r in RACE_LABELS) slice2[[r]] <- li[[r]][slice2$row_idx]
  for (scheme in SCHEMES_FOR_TABLE) {
    slice2[[paste0("tier_", scheme)]] <- tier_from_code_for(scheme, slice2$cbsa_code, slice2$cbsa_state)
    slice2[[paste0("origin_tier_", scheme)]] <- tier_from_code_for(scheme, slice2$cbsa_code_prev, slice2$cbsa_state_prev)
  }

  slice1 <- extract_fixed_year(col1, col_end_col1)
  slice1[, `:=`(
    sex = fifelse(col1$m_prob[row_idx] >= col1$f_prob[row_idx], "male", "female"),
    region = unname(region_for_state_abbr[cbsa_state])
  )]
  for (rp in RACE_PROB_COLS) {
    lbl <- race_col_to_label[[rp]]
    slice1[[lbl]] <- col1[[rp]][slice1$row_idx]
  }

  acs_year <- pums_1yr[survey_year == FIXED_YEAR]
  acs_year[, `:=`(SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER))]
  acs_year <- merge(acs_year, acs_race2015, by = c("SERIALNO", "SPORDER"), all.x = TRUE)
  acs_year[, region := unname(region_for_fips[ST])]
  cat(sprintf("ACS %d race/sex match rate onto the supplementary pull: %.1f%%\n",
              FIXED_YEAR, 100 * mean(!is.na(acs_year$race))))

  ## ---- Weighted share helper ----
  weighted_share <- function(weight, category) {
    d <- data.table(weight = weight, category = category)
    d <- d[!is.na(category) & !is.na(weight) & weight > 0]
    agg <- d[, .(w = sum(weight)), by = category]
    agg[, share := w / sum(w)]
    agg[order(-share)]
  }
  weighted_share_race <- function(weight, race_share_cols) {
    # race_share_cols: named list of per-row fractional shares (sums to 1 across categories per row)
    tot <- sapply(RACE_LABELS, function(r) sum(weight * race_share_cols[[r]], na.rm = TRUE))
    tot <- tot[is.finite(tot)]
    data.table(category = names(tot), share = tot / sum(tot))[order(-share)]
  }

  rows <- list()

  add_source <- function(source_key, race_dt, sex_dt, region_dt) {
    race_dt[, `:=`(source = source_key, category_type = "race")]
    sex_dt[, `:=`(source = source_key, category_type = "sex")]
    region_dt[, `:=`(source = source_key, category_type = "region")]
    rows[[length(rows) + 1]] <<- rbindlist(list(race_dt[, .(source, category_type, category, share)],
                                                  sex_dt[, .(source, category_type, category, share)],
                                                  region_dt[, .(source, category_type, category, share)]))
  }

  ## col1 (unweighted, race_prob used directly -- Column 1 was never raked)
  add_source("col1",
             weighted_share_race(rep(1, nrow(slice1)), setNames(lapply(RACE_LABELS, function(r) slice1[[r]]), RACE_LABELS)),
             weighted_share(rep(1, nrow(slice1)), slice1$sex),
             weighted_share(rep(1, nrow(slice1)), slice1$region))

  ## col2u (unweighted, race_prob used directly)
  add_source("col2u",
             weighted_share_race(rep(1, nrow(slice2)), setNames(lapply(RACE_LABELS, function(r) li[[r]][slice2$row_idx]), RACE_LABELS)),
             weighted_share(rep(1, nrow(slice2)), slice2$sex),
             weighted_share(rep(1, nrow(slice2)), slice2$region))

  ## col2r (static reweighted, TRUE post-IPF race split)
  add_source("col2r",
             weighted_share_race(slice2$w_full_joint, setNames(lapply(RACE_LABELS, function(r) slice2[[r]]), RACE_LABELS)),
             weighted_share(slice2$w_full_joint, slice2$sex),
             weighted_share(slice2$w_full_joint, slice2$region))

  ## Phase A/B, per scheme
  for (scheme in SCHEMES_FOR_TABLE) {
    ra <- ratios[[scheme]]$ratio_a[calendar_year == FIXED_YEAR]
    rb <- ratios[[scheme]]$ratio_b[calendar_year == FIXED_YEAR]

    tier_col <- paste0("tier_", scheme); origin_tier_col <- paste0("origin_tier_", scheme)
    m_a <- merge(data.table(tier = slice2[[tier_col]]), ra[, .(tier, ratio)], by = "tier", all.x = TRUE, sort = FALSE)
    ratio_a_vec <- fifelse(is.na(m_a$ratio), 1, m_a$ratio)
    w_phasea <- slice2$w_full_joint * ratio_a_vec

    m_b <- merge(data.table(origin_tier = slice2[[origin_tier_col]], dest_tier = slice2[[tier_col]]),
                 rb[, .(origin_tier, dest_tier, ratio)], by = c("origin_tier", "dest_tier"), all.x = TRUE, sort = FALSE)
    ratio_b_vec <- fifelse(is.na(m_b$ratio), 1, m_b$ratio)
    # Phase B undefined for rows with no prior-year tier (t=0) -- exclude, matching every other Phase B loop in this project
    w_phaseb <- fifelse(is.na(slice2[[origin_tier_col]]), NA_real_, slice2$w_full_joint * ratio_b_vec)

    a_key <- if (scheme == "rank5") "phasea" else paste0("phasea_", scheme)
    b_key <- if (scheme == "rank5") "phaseb" else paste0("phaseb_", scheme)
    if (scheme == "rank3_region") { a_key <- "phasea_region"; b_key <- "phaseb_region" }

    add_source(a_key,
               weighted_share_race(w_phasea, setNames(lapply(RACE_LABELS, function(r) slice2[[r]]), RACE_LABELS)),
               weighted_share(w_phasea, slice2$sex),
               weighted_share(w_phasea, slice2$region))
    add_source(b_key,
               weighted_share_race(w_phaseb, setNames(lapply(RACE_LABELS, function(r) slice2[[r]]), RACE_LABELS)),
               weighted_share(w_phaseb, slice2$sex),
               weighted_share(w_phaseb, slice2$region))
  }

  ## ACS benchmark
  add_source("acs",
             weighted_share(acs_year$PWGTP, acs_year$race),
             weighted_share(acs_year$PWGTP, acs_year$sex),
             weighted_share(acs_year$PWGTP, acs_year$region))

  out <- rbindlist(rows)
  out[, cohort := cohort_name]
  all_out[[cohort_name]] <- out

  cat(sprintf("\n%s cross-section sizes: Column 1 n=%d, Column 2 n=%d (of which %d have a defined Phase B origin), ACS n=%d\n",
              cohort_name, nrow(slice1), nrow(slice2), sum(!is.na(slice2$origin_tier_rank5)), nrow(acs_year)))

  fwrite(out, file.path(data_dir, sprintf("results/memo1_demo_crosstab_%s.csv", cohort_name)))
  cat(sprintf("\n=== %s demographic cross-tab, %d (wide) ===\n", cohort_name, FIXED_YEAR))
  print(dcast(out, category_type + category ~ source, value.var = "share"))
}

log_step("memo1_cohort_demo_table.R done.")
