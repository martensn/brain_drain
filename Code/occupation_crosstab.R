# occupation_crosstab.R
#
# [NEW 2026-08-21] Adds an occupation row-group to the existing full-sample
# demographic cross-tab (Code/memo1_08_full_sample_extras.R Part B /
# MEMO1_WEIGHTING.md SS6.4), per Nicholas's request. Occupation, like
# race/sex, is outside Phase A/B's calibration target (that's metro tier
# and origin-destination flow, not occupation), so this is a genuine
# additional out-of-sample check, at the SAME calendar year (2015) and
# with the SAME four kept lines (BA Only / BA + HS on LI / BA + HS on LI
# (reweighted) / ACS PUMS) as the existing table.
#
# Occupation crosswalk: Revelio stores occupation as O*NET-SOC codes
# ("19-4061.00") in soc_code_t -- already a real SOC code, so its own
# major-group prefix (first 2 digits) is used DIRECTLY, no crosswalk
# lookup needed on that side at all.
#
# ACS's OCCP needs a real crosswalk, and [FOUND 2026-08-21, caught by a
# suspiciously low ~72% match rate against the Census Bureau's 2018
# Occupation Code List, not assumed fine] the reason is a vintage
# mismatch: ACS 2015's OCCP variable uses the PRE-2018 (2010-vintage)
# Census occupation classification, not the 2018 one Census's own
# "Summary of 2018 Changes" sheet lists on its own (that sheet's 571 rows
# are 2018-vintage codes only -- codes like "0430"/"5700"/"3600" that are
# real, common 2010-vintage occupations simply don't appear there because
# Census merged/renamed them for 2018). Fixed by using the SAME downloaded
# workbook's "2010 to 2018 Crosswalk" sheet instead, which carries BOTH
# vintages side by side -- built into
# Data/raw/bls/census_occp_2010_to_soc_major_group.rds (540 distinct 2010
# Census codes, each resolving to exactly one SOC major-group prefix,
# verified no cross-row disagreement before trusting it).
#
# Both sides collapse to the 23 standard SOC major groups (the SOC code's
# own first 2 digits) for a presentable table -- the raw code lists have
# 500+ detailed categories, too granular to show alongside race/sex/region.
#
# ACS-side OCCP comes from a new single-year supplement
# (Code/memo1_02d_acs_pull_1yr_occp2015.R), mirroring
# Code/memo1_02c_acs_pull_1yr_race2015.R's pattern exactly (the main 1yr
# pull never carried occupation, same reasoning as that script's own
# header: not needed until now).
#
# The reweighted line's Phase B ratio table only needs calendar_year=2015,
# which sits entirely inside the 2010-vintage PUMA window (2012-2021) --
# no need for memo1_06b's dual-vintage complexity here, single-vintage
# crosswalks suffice.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
source(here::here("Code/memo1_00_metro_tier_definitions.R"))

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

FIXED_YEAR <- 2015
T_MAX <- 20
RATIO_CAP_LO <- 0.05
RATIO_CAP_HI <- 20

LBL_COL1  <- "BA Only"
LBL_COL2U <- "BA + HS on LI"
LBL_COL2R <- "BA + HS on LI (reweighted)"
LBL_ACS   <- "ACS PUMS"

## =========================================================================
## PART 0: occupation crosswalk -- ACS OCCP (2010-vintage, see header) ->
## SOC major-group prefix. Revelio's own soc_code_t needs no crosswalk at
## all -- it's already a real SOC code, first-2-digits taken directly.
## =========================================================================
log_step("Building Census OCCP (2010-vintage) -> SOC major-group crosswalk")
occ_xwalk_2010 <- readRDS(file.path(data_dir, "raw/bls/census_occp_2010_to_soc_major_group.rds"))
setDT(occ_xwalk_2010)

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
occ_xwalk_2010[, major_group := unname(SOC_MAJOR_GROUPS[major_prefix])]
stopifnot(all(!is.na(occ_xwalk_2010$major_group)))  # every 2-digit prefix in the real crosswalk should resolve to a known major group

occp_to_major <- setNames(occ_xwalk_2010$major_group, occ_xwalk_2010$census_2010)
soc_prefix_to_major <- function(soc_short) unname(SOC_MAJOR_GROUPS[substr(soc_short, 1, 2)])

## =========================================================================
## PART 1: ACS side -- 2015 OCCP supplement joined to pums_1yr_filt.rds
## =========================================================================
log_step("Loading ACS 1yr PUMS + OCCP 2015 supplement")
pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds")); setDT(pums_1yr)
occp2015 <- readRDS(file.path(data_dir, "intermediate/pums_1yr_occp2015.rds")); setDT(occp2015)
occp2015[, `:=`(SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER))]
occp2015[, occp_padded := sprintf("%04d", suppressWarnings(as.integer(OCCP)))]

acs_year <- pums_1yr[survey_year == FIXED_YEAR]
acs_year[, `:=`(SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER))]
acs_year <- merge(acs_year, occp2015[, .(SERIALNO, SPORDER, occp_padded)], by = c("SERIALNO", "SPORDER"), all.x = TRUE)
acs_year[, major_group := unname(occp_to_major[occp_padded])]
cat(sprintf("ACS %d OCCP match rate onto the supplementary pull: %.1f%%\n", FIXED_YEAR, 100 * mean(!is.na(acs_year$occp_padded))))
cat(sprintf("ACS %d OCCP -> major-group resolve rate (of matched): %.1f%%\n", FIXED_YEAR,
            100 * mean(!is.na(acs_year$major_group[!is.na(acs_year$occp_padded)]))))

## =========================================================================
## PART 2: Revelio side -- Column 1/2 at the FIXED_YEAR cross-section
## =========================================================================
log_step("Loading Column 1/2")
col1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds")); setDT(col1)
li   <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds")); setDT(li)

resolve_col_end <- function(dt, is_factor_like) if (is_factor_like) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
col_end_col1 <- resolve_col_end(col1, is_factor_like = FALSE)
col_end_col2 <- resolve_col_end(li, is_factor_like = is.factor(li$col_end) || is.character(li$col_end))

extract_soc_at_fixed_year <- function(dt, col_end_vec, fixed_year, t_max = T_MAX) {
  parts <- vector("list", t_max + 1)
  for (t in 0:t_max) {
    soc_col <- paste0("soc_code_", t); code_col <- paste0("cbsa_code_", t); state_col <- paste0("cbsa_state_", t)
    if (!soc_col %in% names(dt)) next
    cy <- col_end_vec + t
    idx <- which(cy == fixed_year)
    if (length(idx) == 0) next
    prev_code_col <- paste0("cbsa_code_", t - 1); prev_state_col <- paste0("cbsa_state_", t - 1)
    has_prev <- t >= 1 && prev_code_col %in% names(dt)
    parts[[t + 1]] <- data.table(
      row_idx = idx, soc_code = dt[[soc_col]][idx],
      cbsa_code = dt[[code_col]][idx], cbsa_state = dt[[state_col]][idx],
      cbsa_code_prev = if (has_prev) dt[[prev_code_col]][idx] else NA_character_,
      cbsa_state_prev = if (has_prev) dt[[prev_state_col]][idx] else NA_character_
    )
  }
  rbindlist(parts)
}

slice1 <- extract_soc_at_fixed_year(col1, col_end_col1, FIXED_YEAR)
slice1[, soc_short := substr(soc_code, 1, 7)]
slice1[, major_group := soc_prefix_to_major(soc_short)]

slice2 <- extract_soc_at_fixed_year(li, col_end_col2, FIXED_YEAR)
slice2[, `:=`(user_id = li$user_id[row_idx], w_unweighted = li$w_unweighted[row_idx], w_full_joint = li$w_full_joint[row_idx])]
slice2[, soc_short := substr(soc_code, 1, 7)]
slice2[, major_group := soc_prefix_to_major(soc_short)]

cat(sprintf("Column 1 %d cross-section n=%d, SOC->major-group resolve rate: %.1f%%\n",
            FIXED_YEAR, nrow(slice1), 100 * mean(!is.na(slice1$major_group))))
cat(sprintf("Column 2 %d cross-section n=%d, SOC->major-group resolve rate: %.1f%%\n",
            FIXED_YEAR, nrow(slice2), 100 * mean(!is.na(slice2$major_group))))

## =========================================================================
## PART 3: rank3_region Phase B ratio table, calendar_year=2015 only --
## single-vintage (2010) suffices, same construction as memo1_06b/07d but
## without the dual-vintage machinery this year doesn't need.
## =========================================================================
log_step("Building rank3_region Phase B ratio table (2010-vintage, covers 2015)")
CALIB_YEARS_2010 <- setdiff(2012:2021, 2020)
lookup_region <- build_cbsa_tier_lookup("rank3_region")
code_to_tier_region <- code_to_tier_for_scheme(lookup_region)
region_for_state_abbr <- setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
tier_from_code_region <- function(code_vals, state_vals) code_to_tier_region(code_vals, region = unname(region_for_state_abbr[state_vals]))

puma_tier_region <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2010_rank3_region.rds")); setDT(puma_tier_region)
migpuma_tier_region <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2010_rank3_region.rds")); setDT(migpuma_tier_region)

acs_window <- pums_1yr[survey_year %in% CALIB_YEARS_2010 & !is.na(PUMA10)]
acs_window[, state_puma := paste0(ST, PUMA10)]
dest_long_region <- merge(acs_window[, .(row_id = .I, survey_year, state_puma, PWGTP)],
                           puma_tier_region[, .(state_puma, dest_tier = metro_tier, dest_share = share)],
                           by = "state_puma", all.x = TRUE, allow.cartesian = TRUE)
acs_window[, migsp_int := suppressWarnings(as.integer(MIGSP))]
acs_window[, state_migpuma := fifelse(!is.na(MIGPUMA10) & !is.na(migsp_int) & migsp_int >= 1L & migsp_int <= 56L,
                                       paste0(sprintf("%02d", migsp_int), MIGPUMA10), NA_character_)]
dest_long_mig <- merge(dest_long_region[!is.na(dest_tier)], acs_window[, .(row_id = .I, survey_year, state_migpuma)],
                        by = c("row_id", "survey_year"))
movers_full <- dest_long_mig[!is.na(state_migpuma)]
origin_movers <- merge(movers_full[, .(row_id, calendar_year = survey_year, state_migpuma, dest_tier, dest_share, PWGTP)],
                        migpuma_tier_region[, .(state_migpuma, origin_tier = metro_tier, origin_share = share)],
                        by = "state_migpuma", all.x = TRUE, allow.cartesian = TRUE)
origin_movers <- origin_movers[!is.na(origin_tier)]
nm <- dest_long_mig[is.na(state_migpuma)]
nm[, `:=`(origin_tier = dest_tier, origin_share = dest_share, calendar_year = survey_year)]
acs_margin_b <- rbindlist(list(
  origin_movers[, .(calendar_year, origin_tier, dest_tier, w = PWGTP * dest_share * origin_share)],
  nm[, .(calendar_year, origin_tier, dest_tier, w = PWGTP * dest_share * origin_share)]
))[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
acs_margin_b[, acs_share := w / sum(w), by = calendar_year]

rev_partials <- vector("list", T_MAX)
for (t in 1:T_MAX) {
  cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
  cur_state_col <- paste0("cbsa_state_", t); prev_state_col <- paste0("cbsa_state_", t - 1)
  if (!all(c(cur_col, prev_col) %in% names(li))) next
  dest_tier <- tier_from_code_region(li[[cur_col]], li[[cur_state_col]])
  origin_tier <- tier_from_code_region(li[[prev_col]], li[[prev_state_col]])
  cy <- col_end_col2 + t
  valid <- !is.na(dest_tier) & !is.na(origin_tier) & !is.na(li$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS_2010
  if (sum(valid) == 0) next
  rev_partials[[t]] <- data.table(calendar_year = cy[valid], origin_tier = origin_tier[valid], dest_tier = dest_tier[valid], w = li$w_full_joint[valid])[
    , .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
}
revelio_margin_b <- rbindlist(rev_partials)[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
revelio_margin_b[, revelio_share := w / sum(w), by = calendar_year]
ratio_b <- merge(revelio_margin_b[, .(calendar_year, origin_tier, dest_tier, revelio_share)],
                  acs_margin_b[, .(calendar_year, origin_tier, dest_tier, acs_share)],
                  by = c("calendar_year", "origin_tier", "dest_tier"))
ratio_b[, ratio := pmin(pmax(acs_share / revelio_share, RATIO_CAP_LO), RATIO_CAP_HI)]
rb_2015 <- ratio_b[calendar_year == FIXED_YEAR]

slice2[, `:=`(tier_region = tier_from_code_region(cbsa_code, cbsa_state),
              origin_tier_region = tier_from_code_region(cbsa_code_prev, cbsa_state_prev))]
m_b <- merge(data.table(origin_tier = slice2$origin_tier_region, dest_tier = slice2$tier_region),
             rb_2015[, .(origin_tier, dest_tier, ratio)], by = c("origin_tier", "dest_tier"), all.x = TRUE, sort = FALSE)
ratio_b_vec <- fifelse(is.na(m_b$ratio), 1, m_b$ratio)
slice2[, w_phaseb := fifelse(is.na(origin_tier_region), NA_real_, w_full_joint * ratio_b_vec)]

## =========================================================================
## PART 4: weighted occupation shares, all four series
## =========================================================================
weighted_share <- function(weight, category) {
  d <- data.table(weight = weight, category = category)
  d <- d[!is.na(category) & !is.na(weight) & weight > 0]
  agg <- d[, .(w = sum(weight)), by = category]
  agg[, share := w / sum(w)][order(-share)]
}

rows <- list()
add_source <- function(source_key, dt) { dt[, source := source_key]; rows[[length(rows) + 1]] <<- dt }

add_source(LBL_COL1,  weighted_share(rep(1, nrow(slice1)), slice1$major_group))
add_source(LBL_COL2U, weighted_share(rep(1, nrow(slice2)), slice2$major_group))
add_source(LBL_COL2R, weighted_share(slice2$w_phaseb, slice2$major_group))
add_source(LBL_ACS,   weighted_share(acs_year$PWGTP, acs_year$major_group))

out <- rbindlist(rows)
setnames(out, "category", "major_group")
fwrite(out, file.path(data_dir, "results/memo1_occupation_crosstab_full_simplified.csv"))
log_step("Wrote memo1_occupation_crosstab_full_simplified.csv")

cat(sprintf("\n=== Occupation cross-tab, %d (wide) ===\n", FIXED_YEAR))
print(dcast(out, major_group ~ source, value.var = "share"))
