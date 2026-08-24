# memo1_10c_occupation_memo_table_draft.R (was memo1_09_occupation_memo_table.R -- kept, not
# archived, because memo1_10d_occupation_memo_table.R reads this script's own
# output CSV, memo1_demo_crosstab_geo_occ_2015.csv)
#
# [NEW 2026-08-23] Produces ONE new column -- "BA + HS on LI (reweighted,
# geo+occupation)", the w2_occ weight from
# Code/memo1_07_reweight_column2_occupation.R -- for the SAME two tables
# already published in MEMO1_WEIGHTING.md SS6.4 (metro-tier gap summary,
# and the race/sex/region/occupation composition cross-tab at calendar
# year 2015), then appends both as a new SS6.5 with only the table
# results (no interpretive prose -- Nicholas drafts that himself locally).
# The existing SS6.4 numbers are NOT touched or recomputed.
#
# Race needs the SAME "true post-Stage1 share" reconstruction
# memo1_10a_full_sample_extras.R Part B already does (copied verbatim,
# same reason as every other file that needs it) -- Stage 1's IPF used
# race as an actual raking KEY on a melted (person x race) table, so only
# a melt+manual_ipf() re-run recovers each person's TRUE post-raking race
# split; the on-disk race_prob columns were never touched by that raking
# directly. Stage 2 (w2_occ) does NOT re-touch race at all -- it's a
# scalar multiplier on top of the Stage-1 per-person weight -- so once the
# refined per-user race shares are recovered, computing race shares under
# w2_occ is a plain weighted_share_race() call, same mechanism the
# existing "reweighted" column already uses for w_phaseb.
#
# Metro-tier gap and occupation/region/sex shares need no such
# reconstruction -- occupation, sex, and metro tier are read directly off
# the person-year panel memo1_09 already built and saved
# (revelio_geo_occ_person_year_panel.rds), and region is recovered from
# that panel's own dest_tier label (its "(Region)" suffix), avoiding a
# second geography derivation.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
source(here::here("Code/memo1_00_metro_tier_definitions.R"))

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

FIXED_YEAR <- 2015
CALIB_YEARS <- sort(unlist(list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023), use.names = FALSE))
RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob", "native_prob", "multiple_prob", "hispanic_prob")
LBL_NEW <- "BA + HS on LI (reweighted, geo+occupation)"

strip_region <- function(tier) sub(" \\(.*\\)$", "", tier)
region_of_tier <- function(tier) sub("^.*\\((.*)\\)$", "\\1", tier)

## =========================================================================
## manual_ipf() -- copied verbatim, same reason as every other file.
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

## =========================================================================
## LOAD
## =========================================================================
log_step("Loading Column 2, ACS 5yr margins, w2_occ panel")
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds")); setDT(li)
pums_acs5 <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds")); setDT(pums_acs5)
w2_occ_panel <- readRDS(file.path(data_dir, "results/memo1_w2_occupation_calibrated_by_year.rds")); setDT(w2_occ_panel)
rev_panel <- readRDS(file.path(data_dir, "intermediate/revelio_geo_occ_person_year_panel.rds")); setDT(rev_panel)

## =========================================================================
## PART 1: re-derive TRUE post-Stage1 race shares -- copied verbatim from
## memo1_10a_full_sample_extras.R Part B.
## =========================================================================
log_step("PART 1: re-deriving true post-Stage1 race shares (melt + manual_ipf, the slow step)")
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

cell_state_age_race_sex <- pums_acs5[, .(pop = sum(PWGTP)), by = .(origin_state, age_bucket, race, sex)]
margin_demo <- copy(cell_state_age_race_sex)
setnames(margin_demo, "pop", "Freq")
for (col in c("origin_state", "age_bucket", "race", "sex")) margin_demo[[col]] <- as.character(margin_demo[[col]])
li_long[, `:=`(origin_state = as.character(origin_state), age_bucket = as.character(age_bucket),
                race = as.character(race), sex = as.character(sex),
                moved_last_year_state = as.character(moved_last_year_state))]

margin_mover <- pums_acs5[, .(Freq = sum(PWGTP)), by = .(origin_state, moved_out_of_state)]
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
rm(li_long, li_complete); gc()

li_slim <- li[, .(user_id, m_prob, f_prob)]
li_slim[, sex_hard := fifelse(m_prob >= f_prob, "male", "female")]
li_slim <- merge(li_slim, race_share_wide, by = "user_id", all.x = TRUE)

## =========================================================================
## PART 2: calendar-year-2015 slice under w2_occ, with race/sex/region/
## occupation all attached.
## =========================================================================
log_step("PART 2: 2015 cross-section under w2_occ")
slice_2015 <- merge(w2_occ_panel[calendar_year == FIXED_YEAR],
                     rev_panel[calendar_year == FIXED_YEAR, .(user_id, calendar_year, dest_tier, major_group)],
                     by = c("user_id", "calendar_year"))
slice_2015 <- merge(slice_2015, li_slim, by = "user_id", all.x = TRUE)
slice_2015[, region := region_of_tier(dest_tier)]
cat(sprintf("2015 cross-section under w2_occ: n=%d\n", nrow(slice_2015)))

weighted_share <- function(weight, category) {
  d <- data.table(weight = weight, category = category)
  d <- d[!is.na(category) & !is.na(weight) & weight > 0]
  agg <- d[, .(w = sum(weight)), by = category]
  agg[, share := w / sum(w)]
  agg[order(-share)]
}
weighted_share_race <- function(weight, race_share_cols) {
  tot <- sapply(RACE_LABELS, function(r) sum(weight * race_share_cols[[r]], na.rm = TRUE))
  tot <- tot[is.finite(tot)]
  data.table(category = names(tot), share = tot / sum(tot))[order(-share)]
}

race_row <- weighted_share_race(slice_2015$w2_occ, setNames(lapply(RACE_LABELS, function(r) slice_2015[[r]]), RACE_LABELS))
sex_row <- weighted_share(slice_2015$w2_occ, slice_2015$sex_hard)
region_row <- weighted_share(slice_2015$w2_occ, slice_2015$region)
occ_row <- weighted_share(slice_2015$w2_occ, slice_2015$major_group)

demo_table <- rbindlist(list(
  race_row[, .(category_type = "race", category, share)],
  sex_row[, .(category_type = "sex", category, share)],
  region_row[, .(category_type = "region", category, share)],
  occ_row[, .(category_type = "occupation", category, share)]
))
demo_table[, source := LBL_NEW]
fwrite(demo_table, file.path(data_dir, "results/memo1_demo_crosstab_geo_occ_2015.csv"))

## =========================================================================
## PART 3: metro-tier (3-size) mean absolute gap vs. ACS, 2012-2023,
## under w2_occ -- same metric as SS6.4's first table, reusing the
## already-saved ACS PUMS line from memo1_metro_tier_by_calendar_year_
## full_simplified.csv rather than rebuilding the ACS side from scratch.
## =========================================================================
log_step("PART 3: metro-tier gap vs ACS, 2012-2023, under w2_occ")
existing_tier <- fread(file.path(data_dir, "results/memo1_metro_tier_by_calendar_year_full_simplified.csv"))
acs_tier_target <- existing_tier[source == "ACS PUMS", .(calendar_year, tier, acs_share = share)]

panel_w2occ <- merge(rev_panel[, .(user_id, calendar_year, dest_tier)], w2_occ_panel, by = c("user_id", "calendar_year"))
panel_w2occ[, tier := strip_region(dest_tier)]
tier_w2occ <- panel_w2occ[, .(w = sum(w2_occ)), by = .(calendar_year, tier)]
tier_w2occ[, share := w / sum(w), by = calendar_year]

gap_check <- merge(tier_w2occ[, .(calendar_year, tier, share)], acs_tier_target, by = c("calendar_year", "tier"))
mean_gap <- mean(abs(gap_check$share - gap_check$acs_share))
cat(sprintf("Mean abs metro-tier gap vs ACS, 2012-2023, w2_occ: %.4f (%.2f pp)\n", mean_gap, 100 * mean_gap))

## =========================================================================
## PART 4: append to MEMO1_WEIGHTING.md, table results only
## =========================================================================
log_step("PART 4: appending SS6.5 to MEMO1_WEIGHTING.md")
memo_path <- file.path(directory, "MEMO1_WEIGHTING.md")

fmt_pct <- function(x) sprintf("%.1f%%", 100 * x)
row_lookup <- function(dt, cat) if (cat %in% dt$category) fmt_pct(dt[category == cat, share]) else "~0.0%"

race_order <- c("White" = "white", "Black" = "black", "Hispanic" = "hispanic", "Asian" = "asian",
                 "Multiple" = "multiple", "Native" = "native")
sex_order <- c("Male" = "male", "Female" = "female")
region_order <- c("Northeast" = "Northeast", "Midwest" = "Midwest", "South" = "South", "West" = "West")
occ_order_labels <- existing_tier[0]  # placeholder, replaced below by reading the existing memo table's own occupation row order
occ_rows_in_memo_order <- c(
  "Management", "Educational Instruction and Library", "Healthcare Practitioners and Technical",
  "Business and Financial Operations", "Sales and Related", "Office and Administrative Support",
  "Computer and Mathematical", "Arts, Design, Entertainment, Sports, and Media", "Community and Social Service",
  "Architecture and Engineering", "Legal", "Life, Physical, and Social Science", "Personal Care and Service",
  "Protective Service", "Food Preparation and Serving", "Production", "Transportation and Material Moving",
  "Healthcare Support", "Construction and Extraction", "Installation, Maintenance, and Repair",
  "Building and Grounds Cleaning and Maintenance", "Farming, Fishing, and Forestry", "Military Specific"
)

lines <- c(
  "",
  "### 6.5 Full-sample check, with occupation added as a Stage 2 margin",
  "",
  sprintf("Table results only, appended %s -- adds `%s` (Code/memo1_07_reweight_column2_occupation.R's",
          format(Sys.Date(), "%Y-%m-%d"), LBL_NEW),
  "w2_occ: geography and destination-occupation raked jointly per calendar year via a 2-margin IPF) as a fifth",
  "series alongside SS6.4's existing four. SS6.4's own numbers are unchanged.",
  "",
  "**Mean absolute gap vs. ACS, metro-tier share, 2012-2023 (2020 excluded), percentage points:**",
  "",
  "| BA Only | BA + HS on LI | BA + HS on LI (reweighted) | BA + HS on LI (reweighted, geo+occupation) |",
  "|---|---|---|---|",
  sprintf("| 3.80 | 4.04 | 0.12 | %.2f |", 100 * mean_gap),
  "",
  sprintf("**Demographic and occupational composition, calendar year %d** (fifth column added to SS6.4's table; race/sex/occupation are outside this weight's calibration target the same way they were for SS6.4's \"reweighted\" column -- see SS6.4's own note; occupation is now IN-sample for this weight, geography remains partly in-sample under the kept scheme):", FIXED_YEAR),
  "",
  "| Category | BA + HS on LI (reweighted, geo+occupation) |",
  "|---|---|"
)

for (lbl in names(race_order)) lines <- c(lines, sprintf("| %s | %s |", lbl, row_lookup(race_row, race_order[[lbl]])))
for (lbl in names(sex_order)) lines <- c(lines, sprintf("| %s | %s |", lbl, row_lookup(sex_row, sex_order[[lbl]])))
for (lbl in names(region_order)) lines <- c(lines, sprintf("| %s | %s |", lbl, row_lookup(region_row, region_order[[lbl]])))
for (lbl in occ_rows_in_memo_order) lines <- c(lines, sprintf("| %s | %s |", lbl, row_lookup(occ_row, lbl)))

lines <- c(lines, "")
writeLines(lines, con = file(memo_path, open = "a"))
log_step(sprintf("Appended SS6.5 to %s", memo_path))
cat("\nDone.\n")
