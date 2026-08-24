# memo1_11c_stage1_only_lines.R
#
# [NEW 2026-08-23] Computes the one weighting-scheme line that's never
# existed as an output anywhere: Stage 1 alone (w_full_joint, the
# time-invariant demographic+mobility weight), with NO Stage 2 flow
# calibration applied at all. Needed for the 6-line "full comparison"
# versions of both plots and the memo table, per Nicholas's request:
# 2 raw means (BA Only / BA + HS on LI) + 3 weighting-scheme stages
# (Stage 1: demographic only / Stage 2: migration only, i.e. the existing
# geography-only w2 / Stage 2: migration+occupation, the final w2_occ) +
# ACS PUMS.
#
# Canonical labels adopted here for the 6-line deliverables, used
# consistently downstream:
#   "BA + HS on LI (Stage 1: demographic)"          <- w_full_joint alone
#   "BA + HS on LI (Stage 2: migration)"             <- existing geography-only w2
#   "BA + HS on LI (Stage 2: migration+occupation)"  <- w2_occ (today's new weight)
#
# Three outputs, each APPENDED (idempotent, replace-by-source-key) to an
# existing CSV or written as a new standalone one -- never overwriting
# unrelated rows:
#   1. Full-sample metro-tier (rank3, size-only) share by calendar year ->
#      appended to memo1_metro_tier_by_calendar_year_full_simplified.csv.
#   2. Full-sample demographic/occupation composition, calendar year 2015
#      -> new memo1_demo_crosstab_stage1_only_2015.csv (race share
#      reconstruction -- the same melt+manual_ipf() re-derivation
#      memo1_08/memo1_10c_occupation_memo_table_draft.R already needed, SAVED
#      here to intermediate/race_share_wide_full_sample.rds so it isn't
#      recomputed a third time by the final table-assembly script).
#   3. Cohort-restricted (born_1980s/1990s) state-crossing migration rate
#      by calendar year, using each cohort's OWN w_full_joint (already a
#      cohort-specific Stage-1 IPF result, not a filtered slice of the
#      full-sample one) -> new memo1_migration_rate_by_calendar_year_
#      stage1_only_born_{cohort}.csv per cohort.

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
LBL_STAGE1 <- "BA + HS on LI (Stage 1: demographic)"
RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob", "native_prob", "multiple_prob", "hispanic_prob")

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

lookup_rank3 <- build_cbsa_tier_lookup("rank3")
code_to_tier_rank3 <- code_to_tier_for_scheme(lookup_rank3)
region_for_state_abbr <- setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)

resolve_col_end <- function(dt) {
  if (is.factor(dt$col_end) || is.character(dt$col_end)) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
}

# manual_ipf(): [2026-08-23] centralized to Code/memo1_ipf.R.
source(here::here("Code/memo1_ipf.R"))

## =========================================================================
## PART 1 + 2 (full sample): load once, reused for both
## =========================================================================
log_step("Loading Column 2 (full sample), ACS 5yr margins")
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds")); setDT(li)
pums_acs5 <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds")); setDT(pums_acs5)
col_end_col2 <- resolve_col_end(li)

## ---- PART 1: metro-tier (rank3) share by calendar year, w_full_joint ----
log_step("PART 1: full-sample metro-tier share under w_full_joint")
partials <- vector("list", T_MAX + 1)
for (t in 0:T_MAX) {
  col <- paste0("cbsa_code_", t)
  if (!col %in% names(li)) next
  tier <- code_to_tier_rank3(li[[col]])
  valid <- !is.na(tier) & !is.na(li$w_full_joint) & !is.na(col_end_col2)
  if (sum(valid) == 0) next
  cy <- col_end_col2[valid] + t
  partials[[t + 1]] <- data.table(calendar_year = cy, tier = tier[valid], w = li$w_full_joint[valid])[
    , .(w = sum(w)), by = .(calendar_year, tier)]
}
tier_stage1 <- rbindlist(partials)[, .(w = sum(w)), by = .(calendar_year, tier)]
tier_stage1[, share := w / sum(w), by = calendar_year]
tier_stage1_out <- tier_stage1[, .(source = LBL_STAGE1, calendar_year, tier, share)]

csv_path <- file.path(data_dir, "results/memo1_metro_tier_by_calendar_year_full_simplified.csv")
existing_tier <- fread(csv_path)
existing_tier <- existing_tier[source != LBL_STAGE1]
fwrite(rbindlist(list(existing_tier, tier_stage1_out), use.names = TRUE), csv_path)
log_step(sprintf("Appended '%s' to %s", LBL_STAGE1, csv_path))

## ---- PART 2: demographic/occupation table, calendar year 2015, w_full_joint ----
log_step("PART 2: re-deriving true post-Stage1 race shares (melt + manual_ipf)")
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
saveRDS(race_share_wide, file.path(data_dir, "intermediate/race_share_wide_full_sample.rds"))
log_step("Saved race_share_wide_full_sample.rds for downstream reuse")
rm(li_long, li_complete); gc()

li_slim <- li[, .(user_id, m_prob, f_prob, w_full_joint)]
li_slim[, sex_hard := fifelse(m_prob >= f_prob, "male", "female")]
li_slim <- merge(li_slim, race_share_wide, by = "user_id", all.x = TRUE)

## FIXED_YEAR cross-section, w_full_joint (Stage 1 only -- no Phase B ratio)
extract_fixed_year <- function(dt, col_end_vec, t_max = T_MAX) {
  parts <- vector("list", t_max + 1)
  for (t in 0:t_max) {
    code_col <- paste0("cbsa_code_", t); state_col <- paste0("cbsa_state_", t); soc_col <- paste0("soc_code_", t)
    if (!code_col %in% names(dt)) next
    cy <- col_end_vec + t
    idx <- which(cy == FIXED_YEAR)
    if (length(idx) == 0) next
    parts[[t + 1]] <- data.table(row_idx = idx, cbsa_state = dt[[state_col]][idx],
                                  soc_code = if (soc_col %in% names(dt)) dt[[soc_col]][idx] else NA_character_)
  }
  rbindlist(parts)
}
slice_stage1 <- extract_fixed_year(li, col_end_col2)
slice_stage1[, `:=`(
  user_id = li$user_id[row_idx], w_full_joint = li$w_full_joint[row_idx],
  sex = fifelse(li$m_prob[row_idx] >= li$f_prob[row_idx], "male", "female"),
  region = unname(region_for_state_abbr[cbsa_state])
)]
for (r in RACE_LABELS) slice_stage1[[r]] <- race_share_wide[[r]][match(slice_stage1$user_id, race_share_wide$user_id)]
slice_stage1[, soc_short := substr(soc_code, 1, 7)]
slice_stage1[, major_group := soc_prefix_to_major(soc_short)]
cat(sprintf("2015 cross-section under w_full_joint: n=%d\n", nrow(slice_stage1)))

weighted_share <- function(weight, category) {
  d <- data.table(weight = weight, category = category)
  d <- d[!is.na(category) & !is.na(weight) & weight > 0]
  agg <- d[, .(w = sum(weight)), by = category]
  agg[, share := w / sum(w)][order(-share)]
}
weighted_share_race <- function(weight, race_share_cols) {
  tot <- sapply(RACE_LABELS, function(r) sum(weight * race_share_cols[[r]], na.rm = TRUE))
  tot <- tot[is.finite(tot)]
  data.table(category = names(tot), share = tot / sum(tot))[order(-share)]
}

race_row <- weighted_share_race(slice_stage1$w_full_joint, setNames(lapply(RACE_LABELS, function(r) slice_stage1[[r]]), RACE_LABELS))
sex_row <- weighted_share(slice_stage1$w_full_joint, slice_stage1$sex)
region_row <- weighted_share(slice_stage1$w_full_joint, slice_stage1$region)
occ_row <- weighted_share(slice_stage1$w_full_joint, slice_stage1$major_group)

demo_table_stage1 <- rbindlist(list(
  race_row[, .(category_type = "race", category, share)],
  sex_row[, .(category_type = "sex", category, share)],
  region_row[, .(category_type = "region", category, share)],
  occ_row[, .(category_type = "occupation", category, share)]
))
demo_table_stage1[, source := LBL_STAGE1]
fwrite(demo_table_stage1, file.path(data_dir, "results/memo1_demo_crosstab_stage1_only_2015.csv"))
log_step("Wrote memo1_demo_crosstab_stage1_only_2015.csv")

## =========================================================================
## PART 3: cohort-restricted state-crossing migration rate under each
## cohort's OWN w_full_joint (Stage 1 only, no Stage 2).
## =========================================================================
run_cohort_migration <- function(cohort_name) {
  log_step(sprintf("PART 3: cohort %s, migration rate under w_full_joint", cohort_name))
  li_c <- readRDS(file.path(data_dir, sprintf("intermediate/column2_reweighted_%s.rds", cohort_name))); setDT(li_c)
  col_end_c <- resolve_col_end(li_c)
  partials_c <- vector("list", T_MAX)
  for (t in 1:T_MAX) {
    cur_col <- paste0("cbsa_state_", t); prev_col <- paste0("cbsa_state_", t - 1)
    if (!all(c(cur_col, prev_col) %in% names(li_c))) next
    valid <- !is.na(li_c[[cur_col]]) & !is.na(li_c[[prev_col]]) & !is.na(li_c$w_full_joint) & !is.na(col_end_c)
    if (sum(valid) == 0) next
    moved <- as.numeric(li_c[[cur_col]][valid] != li_c[[prev_col]][valid])
    cy <- col_end_c[valid] + t
    partials_c[[t]] <- data.table(calendar_year = cy, w = li_c$w_full_joint[valid], wmoved = li_c$w_full_joint[valid] * moved)[
      , .(sum_w = sum(w), sum_wmoved = sum(wmoved)), by = calendar_year]
  }
  agg_c <- rbindlist(partials_c)[, .(sum_w = sum(sum_w), sum_wmoved = sum(sum_wmoved)), by = calendar_year]
  agg_c[, `:=`(rate = sum_wmoved / sum_w, source = LBL_STAGE1)]
  setorder(agg_c, calendar_year)
  out_path <- file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_stage1_only_%s.csv", cohort_name))
  fwrite(agg_c[, .(source, calendar_year, rate)], out_path)
  log_step(sprintf("Wrote %s (%d rows)", out_path, nrow(agg_c)))
}
run_cohort_migration("born_1980s")
run_cohort_migration("born_1990s")

log_step("memo1_11c_stage1_only_lines.R done.")
