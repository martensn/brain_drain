# memo1_11_final_outputs.R
#
# [CONSOLIDATED 2026-08-24, per Nicholas's request to reduce script count]
# Merges three files into one: memo1_11c_stage1_only_lines.R,
# memo1_11a_final_plots.R, memo1_11b_final_tables.R. No logic changed from
# any original script -- shared LBL_*/RAW_* label constants and FIXED_YEAR
# (byte-identical across the originals) are now defined once at the top.
#
# ONE real correction made by this consolidation, not just a rename: the
# original alphabetical numbering (11a/11b/11c) did NOT match true
# execution order. memo1_11a_final_plots.R and memo1_11b_final_tables.R
# both READ output files that memo1_11c_stage1_only_lines.R WRITES
# (memo1_metro_tier_by_calendar_year_full_simplified.csv's Stage-1 row,
# memo1_migration_rate_by_calendar_year_stage1_only_<cohort>.csv,
# memo1_demo_crosstab_stage1_only_2015.csv) -- so 11c had to run FIRST,
# despite sorting last alphabetically. Running the three as separate
# scripts in the wrong order would silently read stale/missing Stage-1
# data. Sections below are ordered by TRUE dependency (old 11c, then 11a,
# then 11b), which a single top-to-bottom script enforces by construction
# -- this ordering bug can no longer happen now that they're one file.
#
# ===========================================================================
# SECTION 1 (was memo1_11c_stage1_only_lines.R):
# ===========================================================================
# Computes the one weighting-scheme line that's never existed as an output
# anywhere: Stage 1 alone (w_full_joint, the time-invariant
# demographic+mobility weight), with NO Stage 2 flow calibration applied at
# all. Needed for the 6-line "full comparison" versions of both plots and
# the memo table, per Nicholas's request: 2 raw means (BA Only / BA + HS on
# LI) + 3 weighting-scheme stages (Stage 1: demographic only / Stage 2:
# migration only, i.e. the existing geography-only w2 / Stage 2:
# migration+occupation, the final w2_occ) + ACS PUMS.
#
# Three outputs, each APPENDED (idempotent, replace-by-source-key) to an
# existing CSV or written as a new standalone one -- never overwriting
# unrelated rows:
#   1. Full-sample metro-tier (rank3, size-only) share by calendar year ->
#      appended to memo1_metro_tier_by_calendar_year_full_simplified.csv.
#   2. Full-sample demographic/occupation composition, calendar year 2015
#      -> new memo1_demo_crosstab_stage1_only_2015.csv (race share
#      reconstruction -- the same melt+manual_ipf() re-derivation
#      memo1_10_full_sample_and_occupation_tables.R already needed, SAVED
#      here to intermediate/race_share_wide_full_sample.rds so it isn't
#      recomputed a third time by Section 3 below).
#   3. Cohort-restricted (born_1980s/1990s) state-crossing migration rate
#      by calendar year, using each cohort's OWN w_full_joint (already a
#      cohort-specific Stage-1 IPF result, not a filtered slice of the
#      full-sample one) -> new memo1_migration_rate_by_calendar_year_
#      stage1_only_born_{cohort}.csv per cohort.
#
# ===========================================================================
# SECTION 2 (was memo1_11a_final_plots.R):
# ===========================================================================
# Builds the two FINAL deliverable versions of both calibration-comparison
# figures, replacing the single ambiguous "5-line" transitional versions
# built earlier the same day:
#
#   4-line: BA Only / BA + HS on LI / BA + HS on LI (reweighted) / ACS PUMS
#     -- the old geography-only Stage 2 weight is DROPPED entirely; the
#     final geo+occupation weight (w2_occ) takes over the single
#     "reweighted" slot and label, unqualified.
#
#   6-line: BA Only / BA + HS on LI / BA + HS on LI (Stage 1: demographic)
#     / BA + HS on LI (Stage 2: migration) / BA + HS on LI (Stage 2:
#     migration+occupation) / ACS PUMS -- the full progression through all
#     three weighting stages, per Nicholas's explicit spec.
#
# Both versions read the SAME underlying CSVs already on disk (the base
# 4-source files, the rank3_region/geo_occ/stage1_only source-tagged
# additions) -- no data is recomputed here, only re-labeled and
# re-filtered per version.
#
# ===========================================================================
# SECTION 3 (was memo1_11b_final_tables.R):
# ===========================================================================
# Builds the two FINAL table versions for MEMO1_WEIGHTING.md, replacing the
# ad hoc single-column SS6.5 draft entirely: a 4-column table (BA Only /
# BA + HS on LI / BA + HS on LI (reweighted) / ACS PUMS -- final
# geo+occupation weight only, the old geography-only weight dropped) and a
# 6-column table (adds the two intermediate weighting stages: Stage 1
# demographic-only, Stage 2 migration-only). Both cover the same metro-tier
# gap summary and race/sex/region/occupation composition at calendar year
# 2015 that SS6.4/SS6.5 already used.
#
# [LESSON FROM 2026-08-23, applied here] Every prior memo-editing script
# used `Sys.getenv("BRAIN_DRAIN_ROOT")` (P:/BRAIN_DRAIN) for BOTH data
# outputs AND MEMO1_WEIGHTING.md itself -- correct for the former, WRONG
# for the latter: the memo is git-tracked at
# D:/Users/martensn/BRAIN_DRAIN/MEMO1_WEIGHTING.md, a different file
# entirely (confirmed via differing inodes) from whatever sat at
# P:/BRAIN_DRAIN/MEMO1_WEIGHTING.md, which is NOT the canonical copy and
# should not be written to as if it were. This section uses `directory`
# (BRAIN_DRAIN_ROOT/P:) only for `data_dir`, and a SEPARATE, explicit
# `git_repo_dir` for the memo path.

library(data.table)
library(ggplot2)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

# Shared label constants -- byte-identical across all three original
# scripts, kept as one copy.
FIXED_YEAR <- 2015
LBL_COL1  <- "BA Only"
LBL_COL2U <- "BA + HS on LI"
LBL_ACS   <- "ACS PUMS"
LBL_RW_4  <- "BA + HS on LI (reweighted)"
LBL_STAGE1 <- "BA + HS on LI (Stage 1: demographic)"
LBL_STAGE2_MIG <- "BA + HS on LI (Stage 2: migration)"
LBL_STAGE2_OCC <- "BA + HS on LI (Stage 2: migration+occupation)"
# Raw source labels as they exist in the underlying CSVs, before relabeling.
RAW_OLD_GEO_ONLY <- "BA + HS on LI (reweighted)"
RAW_NEW_GEO_OCC  <- "BA + HS on LI (reweighted, geo+occupation)"
RAW_STAGE1       <- LBL_STAGE1  # already written with this exact label

## ===========================================================================
## SECTION 1: Stage-1-only comparison lines
## ===========================================================================
log_step("SECTION 1: Stage-1-only comparison lines")
source(here::here("Code/memo1_00_metro_tier_definitions.R"))
# manual_ipf(): centralized to Code/memo1_ipf.R.
source(here::here("Code/memo1_ipf.R"))

T_MAX <- 20
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

# Local simple resolve_col_end (no sanity-check wrapper) -- kept exactly as
# originally written, distinct from any other section's version.
resolve_col_end_s1 <- function(dt) {
  if (is.factor(dt$col_end) || is.character(dt$col_end)) as.integer(as.character(dt$col_end)) else as.integer(dt$col_end)
}

## ---- PART 1 + 2 (full sample): load once, reused for both ----
log_step("Loading Column 2 (full sample), ACS 5yr margins")
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds")); setDT(li)
pums_acs5 <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds")); setDT(pums_acs5)
col_end_col2 <- resolve_col_end_s1(li)

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

## ---- PART 3: cohort-restricted state-crossing migration rate under each
## cohort's OWN w_full_joint (Stage 1 only, no Stage 2). ----
run_cohort_migration <- function(cohort_name) {
  log_step(sprintf("PART 3: cohort %s, migration rate under w_full_joint", cohort_name))
  li_c <- readRDS(file.path(data_dir, sprintf("intermediate/column2_reweighted_%s.rds", cohort_name))); setDT(li_c)
  col_end_c <- resolve_col_end_s1(li_c)
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

rm(li, li_slim, pums_acs5, slice_stage1); gc()
log_step("SECTION 1 done.")

## ===========================================================================
## SECTION 2: final comparison figures (4-line and 6-line)
## ===========================================================================
log_step("SECTION 2: final comparison figures")

FONT <- "Segoe UI"
inst_group_colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")

series_4 <- c(LBL_COL1, LBL_COL2U, LBL_RW_4, LBL_ACS)
colors_4 <- setNames(inst_group_colors, series_4)

series_6 <- c(LBL_COL1, LBL_COL2U, LBL_STAGE1, LBL_STAGE2_MIG, LBL_STAGE2_OCC, LBL_ACS)
# 6-color qualitative set: keep the original 4 colors for BA Only/BA+HS on
# LI/ACS/one reweighted slot where possible, add 2 more distinguishable
# hues for the extra two weighting stages -- chosen to be visually
# separable from the existing 4, not a regenerated hue_pal(6) (which would
# shift every existing color).
colors_6 <- c(
  setNames(inst_group_colors[1], LBL_COL1),
  setNames(inst_group_colors[2], LBL_COL2U),
  setNames("#E8A33D", LBL_STAGE1),
  setNames(inst_group_colors[3], LBL_STAGE2_MIG),
  setNames("#4C72B0", LBL_STAGE2_OCC),
  setNames(inst_group_colors[4], LBL_ACS)
)

theme_memo <- function(legend_rows = 2) list(
  guides(color = guide_legend(nrow = legend_rows, byrow = TRUE)),
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        text = element_text(size = 10, family = FONT),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linewidth = 0.3),
        legend.position = "bottom")
)

## ---- FIGURE 1: full-sample metro-tier share by calendar year ----
tier_order <- c("Top 10", "Top 11-50", "Everything else")
d_tier <- fread(file.path(data_dir, "results/memo1_metro_tier_by_calendar_year_full_simplified.csv"))

build_tier_plot <- function(mode) {
  if (mode == "4line") {
    d <- rbind(
      d_tier[source %in% c(LBL_COL1, LBL_COL2U, LBL_ACS)],
      d_tier[source == RAW_NEW_GEO_OCC][, source := LBL_RW_4]
    )
    series_order <- series_4; series_colors <- colors_4; legend_rows <- 1
  } else {
    d <- rbind(
      d_tier[source %in% c(LBL_COL1, LBL_COL2U, LBL_ACS, RAW_STAGE1)],
      d_tier[source == RAW_OLD_GEO_ONLY][, source := LBL_STAGE2_MIG],
      d_tier[source == RAW_NEW_GEO_OCC][, source := LBL_STAGE2_OCC]
    )
    series_order <- series_6; series_colors <- colors_6; legend_rows <- 3
  }
  d[, source := factor(source, levels = series_order)]
  d[, tier := factor(tier, levels = tier_order)]
  d <- d[calendar_year >= 2000]

  p <- ggplot(d, aes(x = calendar_year, y = share, color = source)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.1) +
    facet_wrap(~tier, nrow = 1) +
    scale_color_manual(values = series_colors, name = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = NULL, y = "Share of population", title = NULL) +
    theme_memo(legend_rows) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  out_path <- file.path(data_dir, sprintf("results/memo1_full_sample_metro_tier_share_%s.png", mode))
  ggsave(filename = out_path, plot = p, width = 6.5, height = if (mode == "4line") 3.4 else 3.9, units = "in", dpi = 600, bg = "white")
  cat(sprintf("Wrote %s\n", out_path))
}
build_tier_plot("4line")
build_tier_plot("6line")

## ---- FIGURE 2: migration rate by calendar year, born_1980s / born_1990s ----
relabel_base <- c(
  "Column 1 (college-only)" = LBL_COL1,
  "Column 2 (HS+college, unweighted)" = LBL_COL2U,
  "ACS PUMS benchmark" = LBL_ACS
)

load_cohort_all_sources <- function(cohort_name, cohort_label) {
  base <- fread(file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_%s.csv", cohort_name)))
  region <- fread(file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_rank3_region_%s.csv", cohort_name)))
  geo_occ <- fread(file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_geo_occ_%s.csv", cohort_name)))
  stage1 <- fread(file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_stage1_only_%s.csv", cohort_name)))
  d <- rbind(
    base[source %in% names(relabel_base), .(source, calendar_year, rate)],
    region[source == "Column 2 (Phase B: flow-calibrated)", .(source = RAW_OLD_GEO_ONLY, calendar_year, rate)],
    geo_occ[, .(source = RAW_NEW_GEO_OCC, calendar_year, rate)],
    stage1[, .(source, calendar_year, rate)]
  )
  d[, source := fifelse(source %in% names(relabel_base), relabel_base[source], source)]
  d[, cohort := cohort_label]
  d[calendar_year >= 2000]
}
mig_all <- rbind(
  load_cohort_all_sources("born_1980s", "Born 1980–1989"),
  load_cohort_all_sources("born_1990s", "Born 1990–1999")
)

build_migration_plot <- function(mode) {
  if (mode == "4line") {
    d <- rbind(
      mig_all[source %in% c(LBL_COL1, LBL_COL2U, LBL_ACS)],
      mig_all[source == RAW_NEW_GEO_OCC][, source := LBL_RW_4]
    )
    series_order <- series_4; series_colors <- colors_4; legend_rows <- 1
  } else {
    d <- rbind(
      mig_all[source %in% c(LBL_COL1, LBL_COL2U, LBL_ACS, RAW_STAGE1)],
      mig_all[source == RAW_OLD_GEO_ONLY][, source := LBL_STAGE2_MIG],
      mig_all[source == RAW_NEW_GEO_OCC][, source := LBL_STAGE2_OCC]
    )
    series_order <- series_6; series_colors <- colors_6; legend_rows <- 3
  }
  d[, source := factor(source, levels = series_order)]
  d[, cohort := factor(cohort, levels = c("Born 1980–1989", "Born 1990–1999"))]

  p <- ggplot(d, aes(x = calendar_year, y = rate, color = source)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.3) +
    facet_wrap(~cohort) +
    scale_color_manual(values = series_colors, name = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = NULL, y = "Chance of moving across a state line", title = NULL) +
    theme_memo(legend_rows)

  out_path <- file.path(data_dir, sprintf("results/memo1_simplified_migration_rate_by_cohort_%s.png", mode))
  ggsave(filename = out_path, plot = p, width = 6.5, height = if (mode == "4line") 3.75 else 4.4, units = "in", dpi = 600, bg = "white")
  cat(sprintf("Wrote %s\n", out_path))
}
build_migration_plot("4line")
build_migration_plot("6line")

log_step("SECTION 2 done.")

## ===========================================================================
## SECTION 3: final markdown tables (4-column and 6-column)
## ===========================================================================
log_step("SECTION 3: final markdown tables")

git_repo_dir <- "D:/Users/martensn/BRAIN_DRAIN"
memo_path <- file.path(git_repo_dir, "MEMO1_WEIGHTING.md")
stopifnot(file.exists(memo_path))

cap_map <- c(white = "White", black = "Black", hispanic = "Hispanic", asian = "Asian",
             multiple = "Multiple", native = "Native", male = "Male", female = "Female")

race_order <- c("White", "Black", "Hispanic", "Asian", "Multiple", "Native")
sex_order <- c("Male", "Female")
region_order <- c("Northeast", "Midwest", "South", "West")
occ_order <- c(
  "Management", "Educational Instruction and Library", "Healthcare Practitioners and Technical",
  "Business and Financial Operations", "Sales and Related", "Office and Administrative Support",
  "Computer and Mathematical", "Arts, Design, Entertainment, Sports, and Media", "Community and Social Service",
  "Architecture and Engineering", "Legal", "Life, Physical, and Social Science", "Personal Care and Service",
  "Protective Service", "Food Preparation and Serving", "Production", "Transportation and Material Moving",
  "Healthcare Support", "Construction and Extraction", "Installation, Maintenance, and Repair",
  "Building and Grounds Cleaning and Maintenance", "Farming, Fishing, and Forestry", "Military Specific"
)
row_order <- c(race_order, sex_order, region_order, occ_order)

## ---- PART 1: metro-tier gap vs ACS, 2012-2023 -- all 5 candidate sources ----
log_step("PART 1: metro-tier gap vs ACS, all sources")
d_tier_s3 <- fread(file.path(data_dir, "results/memo1_metro_tier_by_calendar_year_full_simplified.csv"))
acs_tier_target <- d_tier_s3[source == LBL_ACS, .(calendar_year, tier, acs_share = share)]

gap_for_source <- function(src) {
  d <- merge(d_tier_s3[source == src, .(calendar_year, tier, share)], acs_tier_target, by = c("calendar_year", "tier"))
  mean(abs(d$share - d$acs_share))
}
gap_stage1 <- gap_for_source(LBL_STAGE1)
gap_stage2_mig <- gap_for_source(RAW_OLD_GEO_ONLY)
gap_stage2_occ <- gap_for_source(RAW_NEW_GEO_OCC)
cat(sprintf("Gaps: BA Only=3.80 | BA+HS on LI=4.04 | Stage1=%.2f | Stage2-mig=%.2f | Stage2-occ=%.2f (pp)\n",
            100 * gap_stage1, 100 * gap_stage2_mig, 100 * gap_stage2_occ))

## ---- PART 2: demographic/occupation composition, calendar year 2015 --
## all sources, from the already-saved CSVs (no recomputation). ----
log_step("PART 2: assembling demographic/occupation table, all sources")
demo_full <- fread(file.path(data_dir, "results/memo1_demo_crosstab_full_simplified.csv"))
occ_full  <- fread(file.path(data_dir, "results/memo1_occupation_crosstab_full_simplified.csv"))
setnames(occ_full, "major_group", "category")
occ_full[, category_type := "occupation"]
occ_full <- occ_full[, .(source, category_type, category, share)]

geo_occ_col <- fread(file.path(data_dir, "results/memo1_demo_crosstab_geo_occ_2015.csv"))[, .(source, category_type, category, share)]
stage1_col <- fread(file.path(data_dir, "results/memo1_demo_crosstab_stage1_only_2015.csv"))[, .(source, category_type, category, share)]

all_rows <- rbindlist(list(demo_full, occ_full, geo_occ_col, stage1_col), use.names = TRUE)
all_rows[category_type %in% c("race", "sex"), category := unname(cap_map[category])]

wide <- dcast(all_rows, category_type + category ~ source, value.var = "share")
setnames(wide, RAW_OLD_GEO_ONLY, "STAGE2_MIG_RAW", skip_absent = TRUE)
setnames(wide, RAW_NEW_GEO_OCC, "STAGE2_OCC_RAW", skip_absent = TRUE)
wide[, category := factor(category, levels = row_order)]
setorder(wide, category)

fmt_pct <- function(x) {
  ifelse(is.na(x), "~0.0%", ifelse(x < 0.0005, "~0.0%", sprintf("%.1f%%", 100 * x)))
}

## ---- PART 3: assemble markdown for both table versions ----
log_step("PART 3: writing markdown")

build_4col_lines <- function() {
  c(
    "",
    "### 6.5 Final specification",
    "",
    sprintf("Table results only, rebuilt %s. The final specification's \"reweighted\" line is `w2_occ`", format(Sys.Date(), "%Y-%m-%d")),
    "(Code/memo1_07_reweight_column2_occupation.R: geography and destination occupation raked jointly per",
    "calendar year via a 2-margin IPF). The intermediate geography-only weighting scheme used earlier in this",
    "project is dropped from this table entirely -- see SS6.6 for the full stage-by-stage comparison including it.",
    "",
    "**Mean absolute gap vs. ACS, metro-tier share, 2012-2023 (2020 excluded), percentage points:**",
    "",
    sprintf("| %s | %s | %s |", LBL_COL1, LBL_COL2U, LBL_RW_4),
    "|---|---|---|",
    sprintf("| 3.80 | 4.04 | %.2f |", 100 * gap_stage2_occ),
    "",
    sprintf("**Demographic and occupational composition, calendar year %d:**", FIXED_YEAR),
    "",
    sprintf("| Category | %s | %s | %s | %s |", LBL_COL1, LBL_COL2U, LBL_RW_4, LBL_ACS),
    "|---|---|---|---|---|"
  )
}
row_lines_4col <- wide[, sprintf("| %s | %s | %s | %s | %s |", category,
                                  fmt_pct(get(LBL_COL1)), fmt_pct(get(LBL_COL2U)), fmt_pct(STAGE2_OCC_RAW), fmt_pct(get(LBL_ACS)))]

build_6col_lines <- function() {
  c(
    "",
    "### 6.6 Full weighting-stage comparison",
    "",
    sprintf("Table results only, added %s. All three weighting stages side by side: Stage 1 alone", format(Sys.Date(), "%Y-%m-%d")),
    "(`w_full_joint`, time-invariant demographic+mobility raking, no calendar-year information at all --",
    "identical value in every year by construction, since nothing here varies by calendar year), Stage 2",
    "migration-only (the geography-flow single-shot ratio this project used before today), and Stage 2",
    "migration+occupation (`w2_occ`, today's final specification, also shown alone in SS6.5).",
    "",
    "**Mean absolute gap vs. ACS, metro-tier share, 2012-2023 (2020 excluded), percentage points:**",
    "",
    sprintf("| %s | %s | %s | %s | %s |", LBL_COL1, LBL_COL2U, LBL_STAGE1, LBL_STAGE2_MIG, LBL_STAGE2_OCC),
    "|---|---|---|---|---|",
    sprintf("| 3.80 | 4.04 | %.2f | %.2f | %.2f |", 100 * gap_stage1, 100 * gap_stage2_mig, 100 * gap_stage2_occ),
    "",
    sprintf("**Demographic and occupational composition, calendar year %d:**", FIXED_YEAR),
    "",
    sprintf("| Category | %s | %s | %s | %s | %s | %s |", LBL_COL1, LBL_COL2U, LBL_STAGE1, LBL_STAGE2_MIG, LBL_STAGE2_OCC, LBL_ACS),
    "|---|---|---|---|---|---|---|"
  )
}
row_lines_6col <- wide[, sprintf("| %s | %s | %s | %s | %s | %s | %s |", category,
                                  fmt_pct(get(LBL_COL1)), fmt_pct(get(LBL_COL2U)), fmt_pct(get(LBL_STAGE1)),
                                  fmt_pct(STAGE2_MIG_RAW), fmt_pct(STAGE2_OCC_RAW), fmt_pct(get(LBL_ACS)))]

## ---- PART 4: insert into the GIT-TRACKED memo, between SS6.4 and SS7 --
## if "### 6.5" already exists (a prior run of this script), replace from
## there through end of SS6.6 (i.e. up to "## 7."); otherwise insert fresh
## before "## 7." SS6.4 and everything from SS7 onward is preserved
## byte-for-byte either way. ----
existing_lines <- readLines(memo_path)
sec7_idx <- which(grepl("^## 7\\.", existing_lines))
stopifnot(length(sec7_idx) == 1)
cut_idx <- which(grepl("^### 6\\.5", existing_lines))
stopifnot(length(cut_idx) <= 1)

before_lines <- existing_lines[seq_len((if (length(cut_idx) == 1) cut_idx else sec7_idx) - 1)]
while (length(before_lines) > 0 && before_lines[length(before_lines)] == "") before_lines <- before_lines[-length(before_lines)]
after_lines <- existing_lines[sec7_idx:length(existing_lines)]

final_lines <- c(before_lines, build_4col_lines(), row_lines_4col, build_6col_lines(), row_lines_6col, "", after_lines)
writeLines(final_lines, con = memo_path)
log_step(sprintf("Wrote %s (SS6.5 4-col + SS6.6 6-col inserted before SS7), %d total lines", memo_path, length(final_lines)))

log_step("memo1_11_final_outputs.R done.")
