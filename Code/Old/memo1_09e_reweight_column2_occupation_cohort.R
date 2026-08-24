# memo1_09e_reweight_column2_occupation_cohort.R (was memo1_09_reweight_column2_occupation_cohort.R)
#
# [NEW 2026-08-23] Cohort-restricted version of
# Code/memo1_07_reweight_column2_occupation.R -- the full-sample w2_occ
# weight can't be used for the born_1980s/born_1990s migration-rate charts
# (Code/memo1_10e_memo_plots.R) since it was calibrated against the FULL
# ACS population, not either birth cohort's. Reuses the already-built
# cohort inputs (column2_reweighted_born_{cohort}.rds,
# pums_1yr_filt_born_{cohort}.rds -- both already birth-restricted and,
# for the ACS side, age-restricted per calendar year, same convention as
# every other cohort script this project has built) and the SAME
# occupation supplement/crosswalks memo1_09 already pulled (OCCP doesn't
# need re-pulling per cohort -- it's keyed by SERIALNO/SPORDER/survey_year,
# and merging it onto the cohort-filtered ACS rows automatically
# cohort-restricts it too).
#
# Migration rate here is the SAME metric memo1_08a_migration_profile.R
# uses -- share with cbsa_state_t != cbsa_state_(t-1), i.e. crossed a
# STATE line -- not the metro-TIER-crossing concept memo1_09's own panel
# otherwise tracks. So this script's Revelio panel carries raw
# cbsa_state_t/cbsa_state_(t-1) alongside the tier labels, specifically to
# support this metric.
#
# Real, worth-flagging scope note: w2_occ requires BOTH geography AND
# occupation resolved for a person-year to be calibrated at all (same
# restriction as the full-sample version) -- so the new line's migration
# rate is computed over a narrower row set than the existing "Phase B:
# flow-calibrated" line (which only requires geography). Same restriction
# already accepted for the full-sample metro-tier chart's new line.
#
# Output: appends a NEW, separate CSV per cohort
# (memo1_migration_rate_by_calendar_year_geo_occ_born_{cohort}.csv) --
# does NOT touch the two existing CSVs memo1_10e_memo_plots.R already
# reads, so nothing already published can be corrupted by this script.
# memo1_10e_memo_plots.R is separately extended to also read this third file.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
source(here::here("Code/memo1_00_metro_tier_definitions.R"))

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

VINTAGE_WINDOWS <- list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023)
CALIB_YEARS <- sort(unlist(VINTAGE_WINDOWS, use.names = FALSE))
T_MAX <- 20
LBL_NEW <- "BA + HS on LI (reweighted, geo+occupation)"

SOC_MAJOR_GROUPS <- c(
  "11" = "Management", "13" = "Business and Financial Operations", "15" = "Computer and Mathematical",
  "17" = "Architecture and Engineering", "19" = "Life, Physical, and Social Science",
  "21" = "Community and Social Service", "23" = "Legal", "25" = "Educational Instruction and Library",
  "27" = "Arts, Design, Entertainment, Sports, and Media", "29" = "Healthcare Practitioners and Technical",
  "31" = "Healthcare Support", "33" = "Protective Service", "35" = "Food Preparation and Serving",
  "37" = "Building and Grounds Cleaning and Maintenance", "39" = "Personal Care and Service",
  "41" = "Sales and Related", "43" = "Office and Administrative Support",
  "45" = "Farming, Fishing, and Forestry", "47" = "Construction and Extraction",
  "49" = "Installation, Maintenance, and Repair", "51" = "Production",
  "53" = "Transportation and Material Moving", "55" = "Military Specific"
)
soc_prefix_to_major <- function(soc_short) unname(SOC_MAJOR_GROUPS[substr(soc_short, 1, 2)])
occ_vintage_for_year <- function(y) fifelse(y <= 2017, "2010", "2018")

## manual_ipf() -- [2026-08-23] centralized to Code/memo1_ipf.R.
source(here::here("Code/memo1_ipf.R"))

build_acs_flow_data <- function(pums_1yr, years, puma_col, migpuma_col, puma_tier, migpuma_tier) {
  acs_window <- pums_1yr[survey_year %in% years & !is.na(get(puma_col))]
  acs_window[, state_puma := paste0(ST, get(puma_col))]
  dest_long <- merge(
    acs_window[, .(row_id, survey_year, state_puma, PWGTP)],
    puma_tier[, .(state_puma, dest_tier = metro_tier, dest_share = share)],
    by = "state_puma", all.x = TRUE, allow.cartesian = TRUE
  )
  acs_window[, migsp_int := suppressWarnings(as.integer(MIGSP))]
  acs_window[, state_migpuma := fifelse(
    !is.na(get(migpuma_col)) & !is.na(migsp_int) & migsp_int >= 1L & migsp_int <= 56L,
    paste0(sprintf("%02d", migsp_int), get(migpuma_col)), NA_character_
  )]
  dest_long_mig <- merge(dest_long[!is.na(dest_tier)], acs_window[, .(row_id, survey_year, state_migpuma)],
                          by = c("row_id", "survey_year"))
  movers_full <- dest_long_mig[!is.na(state_migpuma)]
  origin_movers <- merge(
    movers_full[, .(row_id, calendar_year = survey_year, state_migpuma, dest_tier, dest_share, PWGTP)],
    migpuma_tier[, .(state_migpuma, origin_tier = metro_tier, origin_share = share)],
    by = "state_migpuma", all.x = TRUE, allow.cartesian = TRUE
  )
  origin_movers <- origin_movers[!is.na(origin_tier)]
  nm <- dest_long_mig[is.na(state_migpuma)]
  nm[, `:=`(origin_tier = dest_tier, origin_share = dest_share, calendar_year = survey_year)]
  rbindlist(list(
    origin_movers[, .(calendar_year, origin_tier, dest_tier, w = PWGTP * dest_share * origin_share)],
    nm[, .(calendar_year, origin_tier, dest_tier, w = PWGTP * dest_share * origin_share)]
  ))
}

region_for_state_abbr <- setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
lookup_rank3_region <- build_cbsa_tier_lookup("rank3_region")
code_to_tier_region <- code_to_tier_for_scheme(lookup_rank3_region)
tier_from_code_region <- function(code_vals, state_vals) code_to_tier_region(code_vals, region = unname(region_for_state_abbr[state_vals]))

resolve_col_end <- function(dt) {
  if (is.factor(dt$col_end) || is.character(dt$col_end)) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
}

## =========================================================================
## Shared (not per-cohort) loads: occupation supplement + crosswalks
## =========================================================================
log_step("Loading shared OCCP supplement + crosswalks")
occp_all <- readRDS(file.path(data_dir, "intermediate/pums_1yr_occp_allyears.rds")); setDT(occp_all)
occp_all[, occp_padded := sprintf("%04d", suppressWarnings(as.integer(OCCP)))]
xwalk_2010 <- readRDS(file.path(data_dir, "raw/bls/census_occp_2010_to_soc_major_group.rds")); setDT(xwalk_2010)
xwalk_2018 <- readRDS(file.path(data_dir, "raw/bls/census_occp_2018_to_soc_major_group.rds")); setDT(xwalk_2018)
occp_to_major_2010 <- setNames(unname(SOC_MAJOR_GROUPS[xwalk_2010$major_prefix]), xwalk_2010$census_2010)
occp_to_major_2018 <- setNames(unname(SOC_MAJOR_GROUPS[xwalk_2018$major_prefix]), xwalk_2018$census_2018)

puma_tier_region_2010 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2010_rank3_region.rds")); setDT(puma_tier_region_2010)
puma_tier_region_2020 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2020_rank3_region.rds")); setDT(puma_tier_region_2020)
migpuma_tier_region_2010 <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2010_rank3_region.rds")); setDT(migpuma_tier_region_2010)
migpuma_tier_region_2020 <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2020_rank3_region.rds")); setDT(migpuma_tier_region_2020)

## =========================================================================
## Per-cohort calibration
## =========================================================================
run_cohort <- function(cohort_name) {
  log_step(sprintf("=== Cohort %s ===", cohort_name))
  li <- readRDS(file.path(data_dir, sprintf("intermediate/column2_reweighted_%s.rds", cohort_name))); setDT(li)
  pums_1yr <- readRDS(file.path(data_dir, sprintf("intermediate/pums_1yr_filt_%s.rds", cohort_name))); setDT(pums_1yr)
  col_end_col2 <- resolve_col_end(li)
  pums_1yr[, row_id := .I]
  pums_1yr <- pums_1yr[survey_year %in% CALIB_YEARS]

  ## ---- ACS geography flow margin ----
  flow_2010 <- build_acs_flow_data(pums_1yr, VINTAGE_WINDOWS[["2010"]], "PUMA10", "MIGPUMA10", puma_tier_region_2010, migpuma_tier_region_2010)
  flow_2020 <- build_acs_flow_data(pums_1yr, VINTAGE_WINDOWS[["2020"]], "PUMA20", "MIGPUMA20", puma_tier_region_2020, migpuma_tier_region_2020)
  acs_geo <- rbindlist(list(flow_2010, flow_2020))[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
  acs_geo[, acs_share := w / sum(w), by = calendar_year]

  ## ---- ACS occupation margin, cohort-restricted via the join itself ----
  acs_occ_src <- merge(pums_1yr[, .(row_id, survey_year, SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER), PWGTP)],
                        occp_all[, .(survey_year, SERIALNO, SPORDER, occp_padded)],
                        by = c("survey_year", "SERIALNO", "SPORDER"), all.x = TRUE)
  acs_occ_src[, vintage := occ_vintage_for_year(survey_year)]
  acs_occ_src[vintage == "2010", major_group := unname(occp_to_major_2010[occp_padded])]
  acs_occ_src[vintage == "2018", major_group := unname(occp_to_major_2018[occp_padded])]
  cat(sprintf("  %s: ACS OCCP match rate %.1f%%, resolve rate %.1f%%\n", cohort_name,
              100 * mean(!is.na(acs_occ_src$occp_padded)), 100 * mean(!is.na(acs_occ_src$major_group[!is.na(acs_occ_src$occp_padded)]))))
  acs_occ <- acs_occ_src[!is.na(major_group), .(w = sum(PWGTP)), by = .(calendar_year = survey_year, major_group)]
  acs_occ[, acs_share := w / sum(w), by = calendar_year]

  ## ---- Revelio person-year panel: geo tier + occupation + RAW state
  ## (for the state-crossing migration-rate metric afterward) ----
  rev_partials <- vector("list", T_MAX)
  for (t in 1:T_MAX) {
    cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
    cur_state_col <- paste0("cbsa_state_", t); prev_state_col <- paste0("cbsa_state_", t - 1)
    soc_col <- paste0("soc_code_", t)
    if (!all(c(cur_col, prev_col, soc_col) %in% names(li))) next
    dest_tier <- tier_from_code_region(li[[cur_col]], li[[cur_state_col]])
    origin_tier <- tier_from_code_region(li[[prev_col]], li[[prev_state_col]])
    major_group <- soc_prefix_to_major(substr(li[[soc_col]], 1, 7))
    cy <- col_end_col2 + t
    valid <- !is.na(dest_tier) & !is.na(origin_tier) & !is.na(major_group) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
    if (sum(valid) == 0) next
    rev_partials[[t]] <- data.table(
      user_id = li$user_id[valid], calendar_year = cy[valid],
      origin_tier = origin_tier[valid], dest_tier = dest_tier[valid],
      major_group = major_group[valid], w_full_joint = li$w_full_joint[valid],
      cbsa_state_cur = li[[cur_state_col]][valid], cbsa_state_prev = li[[prev_state_col]][valid]
    )
  }
  rev_panel <- rbindlist(rev_partials)
  cat(sprintf("  %s: Revelio panel %d rows, %d users, %d years\n", cohort_name, nrow(rev_panel), uniqueN(rev_panel$user_id), uniqueN(rev_panel$calendar_year)))

  ## ---- per-calendar-year 2-margin IPF ----
  out_parts <- vector("list", length(CALIB_YEARS))
  for (i in seq_along(CALIB_YEARS)) {
    y <- CALIB_YEARS[i]
    rows_y <- rev_panel[calendar_year == y]
    if (nrow(rows_y) == 0) next
    total_w_y <- sum(rows_y$w_full_joint, na.rm = TRUE)
    geo_y <- acs_geo[calendar_year == y, .(origin_tier, dest_tier, Freq = acs_share * total_w_y)]
    occ_y <- acs_occ[calendar_year == y, .(major_group, Freq = acs_share * total_w_y)]
    if (nrow(geo_y) == 0 || nrow(occ_y) == 0) { cat(sprintf("  %s year %d: no ACS margin coverage -- skipping\n", cohort_name, y)); next }
    w2_occ <- manual_ipf(dt = rows_y, w_col = "w_full_joint",
                          margins = list(list(keys = c("origin_tier", "dest_tier"), pop = geo_y),
                                          list(keys = "major_group", pop = occ_y)))
    out_parts[[i]] <- data.table(user_id = rows_y$user_id, calendar_year = y, w2_occ = w2_occ)
  }
  w2_occ_panel <- rbindlist(out_parts)

  ## ---- migration rate (state-crossing) under w2_occ ----
  mig_panel <- merge(rev_panel[, .(user_id, calendar_year, cbsa_state_cur, cbsa_state_prev)], w2_occ_panel, by = c("user_id", "calendar_year"))
  mig_panel <- mig_panel[!is.na(cbsa_state_cur) & !is.na(cbsa_state_prev)]
  mig_panel[, moved := as.numeric(cbsa_state_cur != cbsa_state_prev)]
  rate_dt <- mig_panel[, .(rate = weighted.mean(moved, w2_occ), n = .N), by = calendar_year]
  rate_dt[, source := LBL_NEW]
  setorder(rate_dt, calendar_year)

  saveRDS(w2_occ_panel, file.path(data_dir, sprintf("results/memo1_w2_occupation_calibrated_by_year_%s.rds", cohort_name)))
  out_path <- file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_geo_occ_%s.csv", cohort_name))
  fwrite(rate_dt[, .(source, calendar_year, rate)], out_path)
  log_step(sprintf("Wrote %s (%d rows)", out_path, nrow(rate_dt)))
  print(rate_dt)
  invisible(rate_dt)
}

run_cohort("born_1980s")
run_cohort("born_1990s")
log_step("Done.")
