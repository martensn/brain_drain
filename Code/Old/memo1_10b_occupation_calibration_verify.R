# memo1_10b_occupation_calibration_verify.R (was memo1_09_occupation_calibration_verify.R)
#
# [NEW 2026-08-23] Verification pass for Code/memo1_07_reweight_column2_occupation.R's
# new w2_occ weight (2-margin geography x occupation IPF, per calendar
# year). Two checks, per the plan's verification section:
#   1. Does the occupation gap against ACS actually shrink under w2_occ,
#      across ALL calibration years -- not just the single 2015 snapshot
#      memo1_04b_occupation_crosstab.R checks -- relative to w_full_joint (Stage 1
#      only, no occupation or geography adjustment) and to the existing
#      geography-only w2 (single-shot ratio, no occupation input at all)?
#   2. Does adding the occupation margin measurably degrade the existing
#      geography calibration, relative to the geography-only w2?
#
# Reuses memo1_09's own saved intermediates (acs_geo_margin_by_year.rds,
# acs_occ_margin_by_year.rds, revelio_geo_occ_person_year_panel.rds,
# memo1_w2_occupation_calibrated_by_year.rds) rather than rebuilding them.
# The existing geography-only w2 is recomputed here with the SAME
# single-shot ratio formula memo1_08/memo1_04b_occupation_crosstab.R already use
# (not sourced from those scripts, to avoid re-running their full output --
# same "copy the small piece verbatim" convention used throughout this
# project), applied to the SAME row set as w2_occ (rev_panel) so the two
# weights are compared on an apples-to-apples row set.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

RATIO_CAP_LO <- 0.05
RATIO_CAP_HI <- 20

log_step("Loading memo1_09's saved intermediates")
acs_geo <- readRDS(file.path(data_dir, "intermediate/acs_geo_margin_by_year.rds"))
acs_occ <- readRDS(file.path(data_dir, "intermediate/acs_occ_margin_by_year.rds"))
rev_panel <- readRDS(file.path(data_dir, "intermediate/revelio_geo_occ_person_year_panel.rds")); setDT(rev_panel)
w2_occ_panel <- readRDS(file.path(data_dir, "results/memo1_w2_occupation_calibrated_by_year.rds")); setDT(w2_occ_panel)

## =========================================================================
## Existing (geography-only) Stage 2 weight, single-shot ratio, computed
## over the SAME rev_panel row set as w2_occ for a fair comparison.
## =========================================================================
log_step("Computing the existing geography-only w2 over the same row set")
revelio_geo <- rev_panel[, .(w = sum(w_full_joint)), by = .(calendar_year, origin_tier, dest_tier)]
revelio_geo[, revelio_share := w / sum(w), by = calendar_year]
ratio_geo <- merge(revelio_geo[, .(calendar_year, origin_tier, dest_tier, revelio_share)],
                    acs_geo[, .(calendar_year, origin_tier, dest_tier, acs_share)],
                    by = c("calendar_year", "origin_tier", "dest_tier"))
ratio_geo[, ratio := pmin(pmax(acs_share / revelio_share, RATIO_CAP_LO), RATIO_CAP_HI)]

rev_panel <- merge(rev_panel, ratio_geo[, .(calendar_year, origin_tier, dest_tier, ratio)],
                    by = c("calendar_year", "origin_tier", "dest_tier"), all.x = TRUE)
rev_panel[, ratio := fifelse(is.na(ratio), 1, ratio)]
rev_panel[, w2_geo_only := w_full_joint * ratio]
rev_panel[, ratio := NULL]

setkey(rev_panel, user_id, calendar_year)
w2_occ_panel_k <- copy(w2_occ_panel); setkey(w2_occ_panel_k, user_id, calendar_year)
rev_panel <- merge(rev_panel, w2_occ_panel_k, by = c("user_id", "calendar_year"), all.x = TRUE)
cat(sprintf("Rows with a w2_occ value: %d of %d (%.1f%%)\n", sum(!is.na(rev_panel$w2_occ)), nrow(rev_panel),
            100 * mean(!is.na(rev_panel$w2_occ))))

weighted_gap <- function(dt, weight_col, key_col, target) {
  d <- dt[!is.na(get(weight_col)) & !is.na(get(key_col))]
  agg <- d[, .(w = sum(get(weight_col))), by = c("calendar_year", key_col)]
  agg[, share := w / sum(w), by = calendar_year]
  setnames(agg, key_col, "cell")
  m <- merge(agg, target, by.x = c("calendar_year", "cell"), by.y = c("calendar_year", names(target)[2]),
             all.x = TRUE)
  setnames(m, "acs_share", "target_share")
  m[, gap := abs(share - target_share)]
  m
}

## =========================================================================
## CHECK 1: occupation gap vs ACS, by calendar year, three weights
## =========================================================================
log_step("CHECK 1: occupation gap vs ACS")
occ_target <- acs_occ[, .(calendar_year, major_group, acs_share)]
gap_occ_unweighted <- weighted_gap(rev_panel[, .(calendar_year, major_group, w = 1)], "w", "major_group", occ_target)[, .(calendar_year, gap, source = "Unweighted")]
gap_occ_stage1 <- weighted_gap(rev_panel, "w_full_joint", "major_group", occ_target)[, .(calendar_year, gap, source = "Stage 1 only (w_full_joint)")]
gap_occ_geo <- weighted_gap(rev_panel, "w2_geo_only", "major_group", occ_target)[, .(calendar_year, gap, source = "Geography-only w2 (existing)")]
gap_occ_new <- weighted_gap(rev_panel, "w2_occ", "major_group", occ_target)[, .(calendar_year, gap, source = "Geography+Occupation w2_occ (new)")]

occ_gap_by_year <- rbindlist(list(gap_occ_unweighted, gap_occ_stage1, gap_occ_geo, gap_occ_new))[
  , .(mean_abs_gap = mean(gap, na.rm = TRUE)), by = .(calendar_year, source)]
occ_gap_summary <- occ_gap_by_year[, .(mean_abs_gap = mean(mean_abs_gap)), by = source][order(mean_abs_gap)]
cat("\nOccupation composition gap vs ACS, mean |share - ACS share| across major groups, averaged over all calibration years:\n")
print(occ_gap_summary)
cat("\nBy calendar year:\n")
print(dcast(occ_gap_by_year, calendar_year ~ source, value.var = "mean_abs_gap"))

## =========================================================================
## CHECK 2: geography (tier x tier) gap vs ACS, geography-only w2 vs w2_occ
## -- confirms adding the occupation margin doesn't materially degrade the
## existing geography calibration.
## =========================================================================
log_step("CHECK 2: geography gap vs ACS -- geography-only w2 vs w2_occ")
rev_panel[, tier_pair := paste(origin_tier, dest_tier, sep = " -> ")]
geo_target <- acs_geo[, .(calendar_year, tier_pair = paste(origin_tier, dest_tier, sep = " -> "), acs_share)]
gap_geo_geo <- weighted_gap(rev_panel, "w2_geo_only", "tier_pair", geo_target)[, .(calendar_year, gap, source = "Geography-only w2 (existing)")]
gap_geo_new <- weighted_gap(rev_panel, "w2_occ", "tier_pair", geo_target)[, .(calendar_year, gap, source = "Geography+Occupation w2_occ (new)")]

geo_gap_by_year <- rbindlist(list(gap_geo_geo, gap_geo_new))[, .(mean_abs_gap = mean(gap, na.rm = TRUE)), by = .(calendar_year, source)]
geo_gap_summary <- geo_gap_by_year[, .(mean_abs_gap = mean(mean_abs_gap)), by = source][order(mean_abs_gap)]
cat("\nGeography (origin_tier -> dest_tier) composition gap vs ACS, averaged over all calibration years:\n")
print(geo_gap_summary)
cat("\nBy calendar year:\n")
print(dcast(geo_gap_by_year, calendar_year ~ source, value.var = "mean_abs_gap"))

fwrite(occ_gap_by_year, file.path(data_dir, "results/memo1_w2_occupation_verify_occ_gap.csv"))
fwrite(geo_gap_by_year, file.path(data_dir, "results/memo1_w2_occupation_verify_geo_gap.csv"))
log_step("Wrote memo1_w2_occupation_verify_occ_gap.csv and memo1_w2_occupation_verify_geo_gap.csv")
