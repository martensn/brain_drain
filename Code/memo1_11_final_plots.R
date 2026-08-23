# memo1_11_final_plots.R
#
# [NEW 2026-08-23] Builds the two FINAL deliverable versions of both
# calibration-comparison figures, replacing the single ambiguous
# "5-line" transitional versions built earlier today:
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
# re-filtered per version. Old ambiguous single-version PNGs
# (memo1_full_sample_metro_tier_share.png,
# memo1_simplified_migration_rate_by_cohort.png, with no line-count
# suffix) are superseded by these four explicitly-named outputs.

library(data.table)
library(ggplot2)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

FONT <- "Segoe UI"
inst_group_colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")

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

## =========================================================================
## FIGURE 1: full-sample metro-tier share by calendar year
## =========================================================================
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

## =========================================================================
## FIGURE 2: migration rate by calendar year, born_1980s / born_1990s
## =========================================================================
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

cat("memo1_11_final_plots.R done.\n")
