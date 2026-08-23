# memo1_09_reweight_column2_occupation.R
#
# [NEW 2026-08-23] Adds occupation as a genuine per-calendar-year Stage 2
# calibration margin, not just an out-of-sample diagnostic
# (occupation_crosstab.R stays untouched, as a fixed-2015 check). Per
# Nicholas's explicit design choices:
#   - Margin type: MARGINAL destination-occupation share by year (23 SOC
#     major groups), not an origin->destination occupation TRANSITION --
#     an occupation transition design (529 possible cells/year) would be
#     far sparser than the 12-cell rank3_region geography design, which
#     already showed real sparsity in some cohort x scheme cuts.
#   - Combination with geography: an ACTUAL alternating 2-margin IPF,
#     reusing manual_ipf() (copied verbatim from memo1_04/memo1_08, same
#     as every other file that needs it) unmodified -- run once PER
#     CALENDAR YEAR over that year's Revelio person-year rows. This is a
#     real, deliberate departure from the existing geography-only Stage 2
#     scripts (memo1_08/memo1_06b/occupation_crosstab.R), which use a
#     single-shot ratio and explicitly are NOT IPF -- that design is left
#     untouched; this script is a new, additional weight, not a
#     replacement.
#
# KEY ADAPTATION, worth documenting since it's not obvious from
# manual_ipf()'s own code: Stage 1's IPF (memo1_04) matches margins$pop$Freq
# against literal ACS POPULATION COUNTS, because w_base there already sits
# on a scale comparable to true ACS population. w_full_joint (this script's
# starting weight) has no such absolute-level meaning -- it's a
# COMPOSITIONAL weight, only ever calibrated to ACS SHARES (see the
# existing Stage 2 scripts' `ratio := acs_share / revelio_share`, not a
# Freq/sample_sum ratio). So each margin's `Freq` here is constructed as
# `acs_share_cell(year) * total_w_full_joint(year)` -- i.e. the ACS
# COMPOSITION target rescaled onto Revelio's own total mass for that year.
# On manual_ipf()'s first internal iteration (before any ratio has been
# applied yet), this reduces EXACTLY to the existing single-margin ratio
# formula (Freq/sample_sum = acs_share*total_w / (revelio_share*total_w) =
# acs_share/revelio_share) -- so this is a genuine generalization of the
# existing Stage 2 design to two alternating margins, not an unrelated new
# mechanism.
#
# Requires: column2_reweighted.rds, pums_1yr_filt.rds,
# pums_1yr_occp_allyears.rds (Code/memo1_02e), both occupation crosswalks
# (Code/build_occupation_crosswalk.R), and the rank3_region PUMA/MIGPUMA
# tier crosswalks (memo1_03a/03b), both PUMA vintages.

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
RATIO_CAP_LO <- 0.05
RATIO_CAP_HI <- 20

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

## =========================================================================
## manual_ipf() -- copied verbatim from Code/memo1_04_reweight_column2.R /
## Code/memo1_08_full_sample_extras.R, same reason as those files.
## =========================================================================
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

# ---- ACS-side geography flow data for ONE PUMA vintage window -- copied
# from memo1_06b_scheme_comparison.R / memo1_08 (same already-verified logic).
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
  flow <- rbindlist(list(
    origin_movers[, .(calendar_year, origin_tier, dest_tier, w = PWGTP * dest_share * origin_share)],
    nm[, .(calendar_year, origin_tier, dest_tier, w = PWGTP * dest_share * origin_share)]
  ))
  flow
}

region_for_state_abbr <- setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
lookup_rank3_region <- build_cbsa_tier_lookup("rank3_region")
code_to_tier_region <- code_to_tier_for_scheme(lookup_rank3_region)
tier_from_code_region <- function(code_vals, state_vals) code_to_tier_region(code_vals, region = unname(region_for_state_abbr[state_vals]))

resolve_col_end <- function(dt) {
  if (is.factor(dt$col_end) || is.character(dt$col_end)) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
}

## =========================================================================
## LOAD
## =========================================================================
log_step("Loading Column 2, ACS 1yr, OCCP all-years supplement, occupation crosswalks")
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds")); setDT(li)
pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds")); setDT(pums_1yr)
occp_all <- readRDS(file.path(data_dir, "intermediate/pums_1yr_occp_allyears.rds")); setDT(occp_all)
xwalk_2010 <- readRDS(file.path(data_dir, "raw/bls/census_occp_2010_to_soc_major_group.rds")); setDT(xwalk_2010)
xwalk_2018 <- readRDS(file.path(data_dir, "raw/bls/census_occp_2018_to_soc_major_group.rds")); setDT(xwalk_2018)

occp_to_major_2010 <- setNames(unname(SOC_MAJOR_GROUPS[xwalk_2010$major_prefix]), xwalk_2010$census_2010)
occp_to_major_2018 <- setNames(unname(SOC_MAJOR_GROUPS[xwalk_2018$major_prefix]), xwalk_2018$census_2018)

col_end_col2 <- resolve_col_end(li)
pums_1yr[, row_id := .I]
pums_1yr <- pums_1yr[survey_year %in% CALIB_YEARS]

## =========================================================================
## PART 1: ACS geography flow margin (rank3_region, both PUMA vintages) --
## identical construction to memo1_08/occupation_crosstab.R.
## =========================================================================
log_step("PART 1: ACS geography flow margin (rank3_region)")
puma_tier_region_2010 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2010_rank3_region.rds")); setDT(puma_tier_region_2010)
puma_tier_region_2020 <- readRDS(file.path(data_dir, "intermediate/puma_cbsa_tier_crosswalk_2020_rank3_region.rds")); setDT(puma_tier_region_2020)
migpuma_tier_region_2010 <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2010_rank3_region.rds")); setDT(migpuma_tier_region_2010)
migpuma_tier_region_2020 <- readRDS(file.path(data_dir, "intermediate/migpuma_cbsa_tier_crosswalk_2020_rank3_region.rds")); setDT(migpuma_tier_region_2020)

flow_2010 <- build_acs_flow_data(pums_1yr, VINTAGE_WINDOWS[["2010"]], "PUMA10", "MIGPUMA10", puma_tier_region_2010, migpuma_tier_region_2010)
flow_2020 <- build_acs_flow_data(pums_1yr, VINTAGE_WINDOWS[["2020"]], "PUMA20", "MIGPUMA20", puma_tier_region_2020, migpuma_tier_region_2020)
acs_geo <- rbindlist(list(flow_2010, flow_2020))[, .(w = sum(w)), by = .(calendar_year, origin_tier, dest_tier)]
acs_geo[, acs_share := w / sum(w), by = calendar_year]

## =========================================================================
## PART 2: ACS occupation margin, all calendar years, vintage-aware
## crosswalk (<=2017 -> 2010-vintage, >=2018 -> 2018-vintage).
## =========================================================================
log_step("PART 2: ACS occupation margin (destination share, all years)")
occp_all[, occp_padded := sprintf("%04d", suppressWarnings(as.integer(OCCP)))]
acs_occ_src <- merge(pums_1yr[, .(row_id, survey_year, SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER), PWGTP)],
                      occp_all[, .(survey_year, SERIALNO, SPORDER, occp_padded)],
                      by = c("survey_year", "SERIALNO", "SPORDER"), all.x = TRUE)
cat(sprintf("ACS OCCP match rate onto pums_1yr_filt.rds (all calib years): %.1f%%\n", 100 * mean(!is.na(acs_occ_src$occp_padded))))

acs_occ_src[, vintage := occ_vintage_for_year(survey_year)]
acs_occ_src[vintage == "2010", major_group := unname(occp_to_major_2010[occp_padded])]
acs_occ_src[vintage == "2018", major_group := unname(occp_to_major_2018[occp_padded])]
cat(sprintf("ACS OCCP -> major-group resolve rate (of matched): %.1f%%\n",
            100 * mean(!is.na(acs_occ_src$major_group[!is.na(acs_occ_src$occp_padded)]))))
cat("Per-year resolve rate (a vintage-crosswalk mismatch would show up as a visible dip, not be averaged away):\n")
print(acs_occ_src[, .(match_rate = round(100 * mean(!is.na(occp_padded)), 1),
                       resolve_rate = round(100 * mean(!is.na(major_group[!is.na(occp_padded)])), 1)), by = survey_year][order(survey_year)])

acs_occ <- acs_occ_src[!is.na(major_group), .(w = sum(PWGTP)), by = .(calendar_year = survey_year, major_group)]
acs_occ[, acs_share := w / sum(w), by = calendar_year]

# Saved alongside the final weight table so a downstream verification pass
# (comparing w2_occ's occupation/geography gaps vs. the existing
# geography-only w2) doesn't need to rebuild these ACS margins from scratch.
saveRDS(acs_geo, file.path(data_dir, "intermediate/acs_geo_margin_by_year.rds"))
saveRDS(acs_occ, file.path(data_dir, "intermediate/acs_occ_margin_by_year.rds"))

## =========================================================================
## PART 3: Revelio-side per-(person, calendar year) panel -- geography +
## destination occupation together on the same row, so both margins can be
## raked over the SAME unit set.
## =========================================================================
log_step("PART 3: Revelio person-year panel (geo tier + destination occupation)")
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
    major_group = major_group[valid], w_full_joint = li$w_full_joint[valid]
  )
}
rev_panel <- rbindlist(rev_partials)
cat(sprintf("Revelio person-year panel: %d rows, %d distinct users, %d calendar years\n",
            nrow(rev_panel), uniqueN(rev_panel$user_id), uniqueN(rev_panel$calendar_year)))
saveRDS(rev_panel, file.path(data_dir, "intermediate/revelio_geo_occ_person_year_panel.rds"))

## =========================================================================
## PART 4: per-calendar-year alternating 2-margin IPF (geography + occupation)
## =========================================================================
log_step("PART 4: per-year 2-margin manual_ipf() -- geography x occupation")
out_parts <- vector("list", length(CALIB_YEARS))
diag_parts <- vector("list", length(CALIB_YEARS))

for (i in seq_along(CALIB_YEARS)) {
  y <- CALIB_YEARS[i]
  rows_y <- rev_panel[calendar_year == y]
  if (nrow(rows_y) == 0) { cat(sprintf("Year %d: no Revelio rows -- skipping\n", y)); next }
  total_w_y <- sum(rows_y$w_full_joint, na.rm = TRUE)

  geo_y <- acs_geo[calendar_year == y, .(origin_tier, dest_tier, Freq = acs_share * total_w_y)]
  occ_y <- acs_occ[calendar_year == y, .(major_group, Freq = acs_share * total_w_y)]

  n_geo_possible <- uniqueN(c(geo_y$origin_tier, geo_y$dest_tier))^2
  n_geo_rev_cells <- rows_y[, uniqueN(paste(origin_tier, dest_tier))]
  n_occ_rev_cells <- uniqueN(rows_y$major_group)
  cat(sprintf("Year %d: n=%d person-years | geo margin cells: ACS %d / Revelio %d observed (of %d possible) | occ margin cells: ACS %d / Revelio %d observed (of 23 possible)\n",
              y, nrow(rows_y), nrow(geo_y), n_geo_rev_cells, n_geo_possible, nrow(occ_y), n_occ_rev_cells))

  w2_occ <- manual_ipf(
    dt = rows_y, w_col = "w_full_joint",
    margins = list(list(keys = c("origin_tier", "dest_tier"), pop = geo_y),
                    list(keys = "major_group", pop = occ_y)),
    verbose = FALSE
  )
  out_parts[[i]] <- data.table(user_id = rows_y$user_id, calendar_year = y, w2_occ = w2_occ)
  diag_parts[[i]] <- data.table(calendar_year = y, n = nrow(rows_y),
                                 n_geo_cells_acs = nrow(geo_y), n_geo_cells_revelio = n_geo_rev_cells,
                                 n_occ_cells_acs = nrow(occ_y), n_occ_cells_revelio = n_occ_rev_cells,
                                 w2_occ_min = min(w2_occ), w2_occ_max = max(w2_occ))
}

w2_occ_panel <- rbindlist(out_parts)
diag_table <- rbindlist(diag_parts)

cat("\nPer-year diagnostics:\n")
print(diag_table)

saveRDS(w2_occ_panel, file.path(data_dir, "results/memo1_w2_occupation_calibrated_by_year.rds"))
fwrite(diag_table, file.path(data_dir, "results/memo1_w2_occupation_diagnostics_by_year.csv"))
log_step(sprintf("Wrote memo1_w2_occupation_calibrated_by_year.rds (%d rows) and diagnostics CSV", nrow(w2_occ_panel)))
