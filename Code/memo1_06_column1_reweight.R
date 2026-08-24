# memo1_06_column1_reweight.R
#
# [CONSOLIDATED 2026-08-24, per Nicholas's request to reduce script count]
# Merges what were two separate files (memo1_06a_column1_reweight_stage1.R,
# memo1_06b_column1_reweight_stage2.R) into one, run top-to-bottom -- they
# were always meant to run sequentially (Section 2 needs Section 1's
# output) and neither is ever run standalone. No logic changed from either
# original script.
#
# SECTION 1: Column 1's Stage 1 demographic IPF (w_full_joint). Column 1
# (the full college-graduate sample, no high-school-disclosure requirement)
# had never had any weighting applied -- confirmed directly against
# column1_covariates.rds/memo1_01b_covariates.R before writing this: no
# w_full_joint or any IPF output existed anywhere in Column 1's
# construction. Mirrors memo1_05_reweight_column2.R's Part 1 mechanism
# exactly (same two margins, same manual_ipf(), same 0.05x-20x cap) -- see
# that script's header for the full design rationale (why manual IPF, not
# survey::rake()/calibrate()), not repeated here.
#
# ONE necessary substitution, not a copy error: Column 2's Stage 1 rakes on
# origin_state = hs_state (home/high-school state). Column 1 has no HS data
# at all, so no hs_state exists. The only geography Column 1 natively
# carries is col_state (college state, set during Column 1's own
# birth-year-waterfall construction) -- substituted here as the state-margin
# join key. This is a real, worth-stating modeling choice: "college state"
# and "home state" are different concepts (a person's home state is not
# generally their college's state), so Column 1's demographic reweighting
# targets a state-distribution defined differently than Column 2's. The ACS
# population margin itself (pums_cells.rds$state_age_racesex) is UNCHANGED
# -- reused as-is, no new pull -- since it's simply "ACS respondents by
# state x age x race x sex," agnostic to which LI-side state concept gets
# matched against it.
#
# Known, inherited limitation (not new here): Column 1's age is the cruder
# birth2 = col_start - 18 proxy (see memo1_01a_column1_construct.R), not
# Column 2's HS-anchored birth1 -- already-documented systematic skew
# toward non-traditional-timing students (HANDOFF.md Decisions).
#
# SECTION 2: Column 1 analog of memo1_07_reweight_column2_occupation.R --
# per-calendar-year alternating 2-margin IPF (geography x destination
# occupation), producing Column 1's own w2_occ-equivalent.
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
# (handled in memo1_13_origin_destination_fullsample.R), not this weight
# itself.
#
# T_MAX=20 in Section 2, matching memo1_07's own choice, for direct
# comparability between the two columns' weights (not a new limitation
# introduced here).
#
# Both sections are genuinely expensive (Column 1 is ~10x Column 2's row
# count) -- budget real wall-clock time for a full run of this file.

library(data.table)
library(tidycensus)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
source(here::here("Code/memo1_00_metro_tier_definitions.R"))
source(here::here("Code/memo1_ipf.R"))

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob", "native_prob", "multiple_prob", "hispanic_prob")
race_col_to_label <- setNames(RACE_LABELS, RACE_PROB_COLS)

## ===========================================================================
## SECTION 1: Stage 1 demographic IPF (w_full_joint)
## ===========================================================================
log_step("SECTION 1: Column 1 Stage 1 demographic IPF")
log_step("Loading Column 1 covariates and ACS margins (reused, not re-pulled)")
col1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds")); setDT(col1)
stopifnot(all(c("user_id", "col_state", "age_bucket", RACE_PROB_COLS, "m_prob", "f_prob",
                "moved_last_year_state") %in% names(col1)))

pums_filt <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds")); setDT(pums_filt)
pums_filt[, grad_degree := as.integer(grad_degree)]
stopifnot(all(c("origin_state", "moved_out_of_state", "PWGTP") %in% names(pums_filt)))

pums_cells <- readRDS(file.path(data_dir, "intermediate/pums_cells.rds"))
cell_state_age_race_sex <- pums_cells$state_age_racesex

## ---- col_state -> FIPS (substituting col_state for hs_state -- see header note above) ----
data(fips_codes, package = "tidycensus")
abb_to_fips <- unique(fips_codes[, c("state", "state_code")])
setDT(abb_to_fips)
setnames(abb_to_fips, c("state", "state_code"), c("state_abbr", "state_fips"))

col1 <- merge(col1, abb_to_fips, by.x = "col_state", by.y = "state_abbr", all.x = TRUE)
setnames(col1, "state_fips", "origin_state")
cat(sprintf("col_state -> FIPS match rate: %.1f%%\n", 100 * mean(!is.na(col1$origin_state))))

col1[, w_unweighted := 1]

## [FIXED 2026-08-23, after a real OOM on the first attempt] column1_covariates.rds
## carries Column 1's full per-year position series (cbsa_code_0..50 /
## cbsa_state_0..50 / soc_code_0..50 -- ~150 columns, see
## memo1_01a_column1_construct.R Section 4), none of which the raking
## itself needs. Melting ALL of col1's columns 6x (the way memo1_05 safely
## does at Column 2's ~12x-smaller scale) and churning that through
## manual_ipf()'s per-iteration merges tried to allocate far more memory
## than necessary and failed ("cannot allocate vector of size 1.5 Gb" on
## the first real run, despite ~962GB of system RAM free -- this is
## R-process heap fragmentation from repeated large-table copies, not a
## true system memory shortage). Fix: melt only the columns the raking
## actually uses, rake on that slim table, then merge the resulting
## w_full_joint back onto the FULL col1 (position series intact) at the
## very end -- same "collapse then merge back" shape memo1_05 already uses
## for the per-person weight, just applied one step earlier so the
## position-series columns never enter the melt at all.
raking_cols <- c("user_id", "origin_state", "age_bucket", RACE_PROB_COLS, "m_prob", "f_prob",
                  "moved_last_year_state", "w_unweighted")
col1_slim <- col1[, ..raking_cols]
col1_complete <- col1_slim[!is.na(origin_state) & !is.na(age_bucket) & !is.na(white_prob)]
log_step(paste("col1_complete (non-missing origin_state/age_bucket/race_prob):", nrow(col1_complete), "of", nrow(col1)))
rm(col1_slim)

col1_long <- melt(
  col1_complete,
  id.vars = setdiff(names(col1_complete), RACE_PROB_COLS),
  measure.vars = RACE_PROB_COLS,
  variable.name = "race_prob_col",
  value.name = "race_frac"
)
rm(col1_complete); gc()
col1_long[, race := race_col_to_label[as.character(race_prob_col)]]
col1_long[, sex := fifelse(m_prob >= f_prob, "male", "female")]
col1_long[, w_base := w_unweighted * race_frac]

n_before_mover_na_drop <- nrow(col1_long)
col1_long <- col1_long[!is.na(moved_last_year_state)]
cat(sprintf("Dropped for NA moved_last_year_state (excluded from raking, not imputed): %d of %d melted rows (%.1f%%)\n",
            n_before_mover_na_drop - nrow(col1_long), n_before_mover_na_drop,
            100 * (n_before_mover_na_drop - nrow(col1_long)) / n_before_mover_na_drop))

## ---- Margins (ACS side identical to memo1_05 -- reused, not re-derived) ----
margin_demo <- copy(cell_state_age_race_sex)
setnames(margin_demo, "pop", "Freq")

# Same factor/character representation-mismatch fix as memo1_05's own note
# (age_bucket built by two independent cut() calls) -- force character on
# both sides.
for (col in c("origin_state", "age_bucket", "race", "sex")) margin_demo[[col]] <- as.character(margin_demo[[col]])
col1_long[, `:=`(origin_state = as.character(origin_state), age_bucket = as.character(age_bucket),
                  race = as.character(race), sex = as.character(sex),
                  moved_last_year_state = as.character(moved_last_year_state))]

margin_mover <- pums_filt[, .(Freq = sum(PWGTP)), by = .(origin_state, moved_out_of_state)]
setnames(margin_mover, "moved_out_of_state", "moved_last_year_state")
margin_mover[, `:=`(origin_state = as.character(origin_state), moved_last_year_state = as.character(moved_last_year_state))]

demo_key_cols <- c("origin_state", "age_bucket", "race", "sex")
n_before_cell_match <- nrow(col1_long)
col1_long <- col1_long[margin_demo[, ..demo_key_cols], on = demo_key_cols, nomatch = 0]
cat(sprintf("Dropped for no matching ACS demo cell (state x age x race x sex): %d of %d melted rows (%.1f%%)\n",
            n_before_cell_match - nrow(col1_long), n_before_cell_match,
            100 * (n_before_cell_match - nrow(col1_long)) / n_before_cell_match))

col1_long_keys <- unique(col1_long[, ..demo_key_cols])
n_before_margin_restrict <- nrow(margin_demo)
margin_demo <- margin_demo[col1_long_keys, on = demo_key_cols, nomatch = 0]
cat(sprintf("Restricted margin_demo to cells with sample coverage: %d of %d ACS cells kept (%.1f%%), %.1f%% of ACS population mass retained\n",
            nrow(margin_demo), n_before_margin_restrict, 100 * nrow(margin_demo) / n_before_margin_restrict,
            100 * margin_demo[, sum(Freq)] / cell_state_age_race_sex[, sum(pop)]))

mover_key_cols <- c("origin_state", "moved_last_year_state")
col1_long <- col1_long[margin_mover[, ..mover_key_cols], on = mover_key_cols, nomatch = 0]
n_before_mover_restrict <- nrow(margin_mover)
margin_mover <- margin_mover[unique(col1_long[, ..mover_key_cols]), on = mover_key_cols, nomatch = 0]
cat(sprintf("Restricted margin_mover to cells with sample coverage: %d of %d cells kept\n", nrow(margin_mover), n_before_mover_restrict))

margins_spec <- list(
  list(keys = demo_key_cols, pop = margin_demo),
  list(keys = mover_key_cols, pop = margin_mover)
)

## ---- Small-subsample validation, before trusting the full-scale run ----
set.seed(20260823)
col1_long_test <- col1_long[sample.int(.N, min(100000, .N))]
margin_demo_test <- margin_demo[unique(col1_long_test[, ..demo_key_cols]), on = demo_key_cols, nomatch = 0]
margin_mover_test <- margin_mover[unique(col1_long_test[, ..mover_key_cols]), on = mover_key_cols, nomatch = 0]
col1_long_test <- col1_long_test[margin_demo_test[, ..demo_key_cols], on = demo_key_cols, nomatch = 0]
col1_long_test <- col1_long_test[margin_mover_test[, ..mover_key_cols], on = mover_key_cols, nomatch = 0]
cat(sprintf("Subsample self-consistent margins: %d demo cells (of %d in full margin_demo), %d mover cells; %d rows remain\n",
            nrow(margin_demo_test), nrow(margin_demo), nrow(margin_mover_test), nrow(col1_long_test)))

w_test <- manual_ipf(col1_long_test, "w_base",
                      list(list(keys = demo_key_cols, pop = margin_demo_test),
                           list(keys = mover_key_cols, pop = margin_mover_test)),
                      verbose = TRUE)
cat(sprintf("Subsample IPF test (n=%d melted rows): weight range [%.3f, %.3f], any non-finite: %s\n",
            length(w_test), min(w_test), max(w_test), any(!is.finite(w_test))))
if (any(!is.finite(w_test))) stop("Subsample IPF test produced non-finite weights -- investigate before running at full scale.")

## ---- Full-scale IPF (Section 1's expensive step) ----
log_step("Running full-scale manual_ipf() (both margins) on Column 1 -- ~10x Column 2's scale")
col1_long[, w_raked := manual_ipf(col1_long, "w_base", margins_spec, verbose = TRUE)]

w_full_collapsed <- col1_long[, .(w_full_joint_uncapped = sum(w_raked, na.rm = TRUE)), by = user_id]
col1 <- merge(col1, w_full_collapsed, by = "user_id", all.x = TRUE)
rm(col1_long, w_full_collapsed); gc()

med_w <- median(col1$w_full_joint_uncapped, na.rm = TRUE)
cap_hi <- med_w * 20
cap_lo <- med_w * 0.05
n_capped_hi <- sum(col1$w_full_joint_uncapped > cap_hi, na.rm = TRUE)
n_capped_lo <- sum(col1$w_full_joint_uncapped < cap_lo, na.rm = TRUE)
col1[, w_full_joint := pmin(pmax(w_full_joint_uncapped, cap_lo), cap_hi)]
cat(sprintf("Final weight cap [%.3f, %.3f] (0.05x-20x median %.3f): capped %d rows high, %d rows low, of %d\n",
            cap_lo, cap_hi, med_w, n_capped_hi, n_capped_lo, nrow(col1)))

for (wc in c("w_unweighted", "w_full_joint_uncapped", "w_full_joint")) {
  rng <- range(col1[[wc]], na.rm = TRUE)
  cat(sprintf("%-22s range: [%.3f, %.3f]\n", wc, rng[1], rng[2]))
}

saveRDS(col1, file.path(data_dir, "intermediate/column1_reweighted.rds"))
log_step(paste("saved column1_reweighted.rds:", nrow(col1), "rows"))

## ---- Section 1 diagnostics -- Kish N only. NOTE: unlike Column 2's gate
## check (which compares against `moved_proxy`, a static hs-state-vs-
## college-state transition), Column 1 has no analogous held-out geographic
## variable -- it has no home/HS state at all, so there is no independent
## "did this person relocate from home" measure to check reweighting
## against. Reporting Kish N and the raked margins' own convergence
## (necessarily near-exact, since they ARE the raking targets) rather than
## fabricating a held-out check that doesn't exist for this population.
kish_n <- function(w) { w <- w[!is.na(w)]; (sum(w))^2 / sum(w^2) }
cat(sprintf("\nKish effective N: unweighted %d, w_full_joint %.0f (%.1f%% of unweighted)\n",
            nrow(col1), kish_n(col1$w_full_joint), 100 * kish_n(col1$w_full_joint) / nrow(col1)))

log_step("SECTION 1 done.")

## ===========================================================================
## SECTION 2: Stage 2 geography x occupation flow calibration (w2_occ)
## ===========================================================================
log_step("SECTION 2: Column 1 Stage 2 geography x occupation IPF")

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

log_step("Loading pre-built ACS geo+occ margins")
acs_geo <- readRDS(file.path(data_dir, "intermediate/acs_geo_margin_by_year.rds")); setDT(acs_geo)
acs_occ <- readRDS(file.path(data_dir, "intermediate/acs_occ_margin_by_year.rds")); setDT(acs_occ)

col_end_col1 <- resolve_col_end(col1)

## ---- Revelio-side person-year panel -- geography (origin/dest tier from
## own prior-year CBSA) + destination occupation, mirroring memo1_07's
## PART 3 exactly, using col1 instead of li. ----
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
# Keep just (user_id, col_state) for the cap-rate-by-state diagnostic below,
# then drop the full ~150-column col1 before the per-year IPF loop --
# same memory discipline as Section 1's slim-table fix, applied here too
# since col1 is no longer needed in full past this point.
col1_states <- col1[, .(user_id, col_state)]
rm(col1); gc()
cat(sprintf("Revelio person-year panel (Column 1): %d rows, %d distinct users, %d calendar years\n",
            nrow(rev_panel), uniqueN(rev_panel$user_id), uniqueN(rev_panel$calendar_year)))
saveRDS(rev_panel, file.path(data_dir, "intermediate/column1_revelio_geo_occ_person_year_panel.rds"))

## ---- Per-calendar-year alternating 2-margin IPF, mirroring memo1_07 PART 4. ----
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
cap_rate_by_state <- merge(w2_occ_panel_col1, col1_states, by = "user_id")
cat("\nTop 5 states by w2_occ high-cap rate (sanity check vs. Column 2's documented SD thin-cell issue):\n")
print(cap_rate_by_state[, .(cap_rate = mean(w2_occ >= 0.999 * max(w2_occ), na.rm = TRUE), n = .N), by = col_state][order(-cap_rate)][1:5])

saveRDS(w2_occ_panel_col1, file.path(data_dir, "results/memo1_column1_w2_occupation_calibrated_by_year.rds"))
fwrite(diag_table, file.path(data_dir, "results/memo1_column1_w2_occupation_diagnostics_by_year.csv"))
log_step(sprintf("Wrote memo1_column1_w2_occupation_calibrated_by_year.rds (%d rows) and diagnostics CSV", nrow(w2_occ_panel_col1)))

log_step("memo1_06_column1_reweight.R done.")
