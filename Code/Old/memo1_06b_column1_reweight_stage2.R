# memo1_06b_column1_reweight_stage2.R
#
# [NEW 2026-08-23] Column 1 analog of memo1_07_reweight_column2_occupation.R
# -- per-calendar-year alternating 2-margin IPF (geography x destination
# occupation), producing Column 1's own w2_occ-equivalent. Run after
# memo1_06a_column1_reweight_stage1.R (needs column1_reweighted.rds's
# w_full_joint as the pre-calibration base weight).
#
# The ACS-side margins (acs_geo, acs_occ) are POPULATION margins -- they
# describe ACS respondents, not anything about Column 1 vs. Column 2 -- so
# they are IDENTICAL to Column 2's and reused directly from disk
# (Data/intermediate/acs_geo_margin_by_year.rds / acs_occ_margin_by_year.rds,
# saved by memo1_07_reweight_column2_occupation.R specifically so a
# downstream script wouldn't need to rebuild them). Only the Revelio/sample
# side is built fresh, from Column 1's own cbsa_code_t/soc_code_t series.
#
# NOTE on "origin": exactly as in the Column 2 production script, the
# geography margin's "origin_tier" comes from each person's own PRIOR-YEAR
# CBSA (cbsa_code_{t-1}) -- i.e. a MOBILITY concept, not a hometown concept.
# This does NOT depend on hs_cbsa_code/high-school data at all, so Column 1
# (which has no HS data) can compute this margin exactly the same way
# Column 2 does -- the only thing Column 1 structurally cannot report is
# origin_cbsa (home/HS labor market) in the FINAL destination table
# (handled in memo1_13a_origin_destination_fullsample.R), not this weight itself.
#
# T_MAX=20, matching memo1_09's own choice, for direct comparability
# between the two columns' weights (not a new limitation introduced here).
#
# This is the expensive step (~10x Column 2's row count feeding into a
# per-year person-year panel across up to 20 post-grad years) -- budget
# real wall-clock time.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
source(here::here("Code/memo1_00_metro_tier_definitions.R"))
source(here::here("Code/memo1_ipf.R"))

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

VINTAGE_WINDOWS <- list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023)
CALIB_YEARS <- sort(unlist(VINTAGE_WINDOWS, use.names = FALSE))
T_MAX <- 20

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
log_step("Loading Column 1 (reweighted), pre-built ACS geo+occ margins")
col1 <- readRDS(file.path(data_dir, "intermediate/column1_reweighted.rds")); setDT(col1)
stopifnot("w_full_joint" %in% names(col1))
acs_geo <- readRDS(file.path(data_dir, "intermediate/acs_geo_margin_by_year.rds")); setDT(acs_geo)
acs_occ <- readRDS(file.path(data_dir, "intermediate/acs_occ_margin_by_year.rds")); setDT(acs_occ)

col_end_col1 <- resolve_col_end(col1)

## =========================================================================
## Revelio-side person-year panel -- geography (origin/dest tier from own
## prior-year CBSA) + destination occupation, mirroring memo1_09's PART 3
## exactly, using col1 instead of li.
## =========================================================================
log_step("Building Revelio person-year panel (geo tier + destination occupation) for Column 1")
rev_partials <- vector("list", T_MAX)
for (t in 1:T_MAX) {
  cur_col <- paste0("cbsa_code_", t); prev_col <- paste0("cbsa_code_", t - 1)
  cur_state_col <- paste0("cbsa_state_", t); prev_state_col <- paste0("cbsa_state_", t - 1)
  soc_col <- paste0("soc_code_", t)
  if (!all(c(cur_col, prev_col, soc_col) %in% names(col1))) next
  dest_tier <- tier_from_code_region(col1[[cur_col]], col1[[cur_state_col]])
  origin_tier <- tier_from_code_region(col1[[prev_col]], col1[[prev_state_col]])
  major_group <- soc_prefix_to_major(substr(col1[[soc_col]], 1, 7))
  cy <- col_end_col1 + t
  valid <- !is.na(dest_tier) & !is.na(origin_tier) & !is.na(major_group) & !is.na(col1$w_full_joint) & !is.na(cy) & cy %in% CALIB_YEARS
  if (sum(valid) == 0) next
  rev_partials[[t]] <- data.table(
    user_id = col1$user_id[valid], calendar_year = cy[valid],
    origin_tier = origin_tier[valid], dest_tier = dest_tier[valid],
    major_group = major_group[valid], w_full_joint = col1$w_full_joint[valid]
  )
  if (t %% 5 == 0) log_step(sprintf("  t=%d done (%d valid rows)", t, sum(valid)))
}
rev_panel <- rbindlist(rev_partials)
rm(rev_partials); gc()
cat(sprintf("Revelio person-year panel (Column 1): %d rows, %d distinct users, %d calendar years\n",
            nrow(rev_panel), uniqueN(rev_panel$user_id), uniqueN(rev_panel$calendar_year)))
saveRDS(rev_panel, file.path(data_dir, "intermediate/column1_revelio_geo_occ_person_year_panel.rds"))

## =========================================================================
## Per-calendar-year alternating 2-margin IPF, mirroring memo1_09 PART 4.
## =========================================================================
log_step("Per-year 2-margin manual_ipf() -- geography x occupation, Column 1")
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

w2_occ_panel_col1 <- rbindlist(out_parts)
diag_table <- rbindlist(diag_parts)

cat("\nPer-year diagnostics (Column 1):\n")
print(diag_table)

# Weight-cap-rate check by state, same diagnostic already established for
# Column 2 (South Dakota thin-cell issue) -- worth re-checking here since
# Column 1's ~10x scale could plausibly behave differently.
cap_rate_by_state <- merge(w2_occ_panel_col1, col1[, .(user_id, col_state)], by = "user_id")
cap_hi_val <- max(w2_occ_panel_col1$w2_occ, na.rm = TRUE)
cat("\nTop 5 states by w2_occ high-cap rate (sanity check vs. Column 2's documented SD thin-cell issue):\n")
print(cap_rate_by_state[, .(cap_rate = mean(w2_occ >= 0.999 * max(w2_occ), na.rm = TRUE), n = .N), by = col_state][order(-cap_rate)][1:5])

saveRDS(w2_occ_panel_col1, file.path(data_dir, "results/memo1_column1_w2_occupation_calibrated_by_year.rds"))
fwrite(diag_table, file.path(data_dir, "results/memo1_column1_w2_occupation_diagnostics_by_year.csv"))
log_step(sprintf("Wrote memo1_column1_w2_occupation_calibrated_by_year.rds (%d rows) and diagnostics CSV", nrow(w2_occ_panel_col1)))
