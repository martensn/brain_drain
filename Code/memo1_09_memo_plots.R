# memo1_09_memo_plots.R
#
# [NEW 2026-08-21] Rebuilds MEMO1_WEIGHTING.md's SS6.2 four-line migration-rate
# figure (memo1_simplified_migration_rate_by_cohort.png) using Code/scripts/
# 09_plots.R's actual design conventions -- transparent background, bottom
# legend, horizontal-only major gridlines, centered title -- which the
# original version of this figure predated and never picked up (a known,
# previously-flagged inconsistency). Font swapped from "LM Roman 10"
# (never registered on this Windows machine, silently fell back to serif)
# to "Segoe UI" -- a real system font here, and the same "clean modern
# sans" family the interactive chart artifacts already use as their web
# font stack, per Nicholas's request for something cleaner/more modern for
# markdown/webpage use. Labels also updated to Nicholas's requested
# wording: "BA Only" / "BA + HS on LI" / "BA + HS on LI (reweighted)" /
# "ACS PUMS" (was "Column 1 (college-only)" / "Column 2 (HS+college,
# unweighted)" / "Column 2 (Phase B: flow-calibrated)" / "ACS PUMS
# benchmark").
#
# Colors: Code/scripts/09_plots.R's own inst_group_colors -- a 4-color
# qualitative palette already used elsewhere in this project for exactly
# this shape of comparison (4 categorical series) -- reused here rather
# than inventing a new palette, matching Nicholas's "keep using my design"
# instruction.
#
# Data: memo1_migration_rate_by_calendar_year_born_{cohort}.csv (Column 1/
# Column 2 unweighted/ACS) + memo1_migration_rate_by_calendar_year_
# rank3_region_born_{cohort}.csv (Phase B: flow-calibrated, the settled
# rank3_region scheme -- confirmed via memo1_scheme_gap_summary.csv as the
# kept scheme, same one already named "flow-calibrated" throughout
# MEMO1_WEIGHTING.md SS5-SS6).

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
LBL_COL2R <- "BA + HS on LI (reweighted)"
LBL_ACS   <- "ACS PUMS"
series_colors <- setNames(inst_group_colors, c(LBL_COL1, LBL_COL2U, LBL_COL2R, LBL_ACS))
series_order  <- c(LBL_COL1, LBL_COL2U, LBL_COL2R, LBL_ACS)

relabel <- c(
  "Column 1 (college-only)" = LBL_COL1,
  "Column 2 (HS+college, unweighted)" = LBL_COL2U,
  "Column 2 (Phase B: flow-calibrated)" = LBL_COL2R,
  "ACS PUMS benchmark" = LBL_ACS
)

load_cohort <- function(cohort_name, cohort_label) {
  base <- fread(file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_%s.csv", cohort_name)))
  region <- fread(file.path(data_dir, sprintf("results/memo1_migration_rate_by_calendar_year_rank3_region_%s.csv", cohort_name)))
  d <- rbind(
    base[source %in% c("Column 1 (college-only)", "Column 2 (HS+college, unweighted)", "ACS PUMS benchmark"),
         .(source, calendar_year, rate)],
    region[source == "Column 2 (Phase B: flow-calibrated)", .(source, calendar_year, rate)]
  )
  d[, source := relabel[source]]
  d[, cohort := cohort_label]
  # Pre-2000 Revelio observations are a data-coverage artifact (a handful
  # of implausibly-early panel entries), documented and excluded the same
  # way in the interactive chart artifacts' footer notes -- not a real
  # finding about early-panel mobility.
  d[calendar_year >= 2000]
}

plot_data <- rbind(
  load_cohort("born_1980s", "Born 1980–1989"),
  load_cohort("born_1990s", "Born 1990–1999")
)
plot_data[, source := factor(source, levels = series_order)]
plot_data[, cohort := factor(cohort, levels = c("Born 1980–1989", "Born 1990–1999"))]

p <- ggplot(plot_data, aes(x = calendar_year, y = rate, color = source)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.3) +
  facet_wrap(~cohort) +
  scale_color_manual(values = series_colors, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Chance of moving across a state line", title = NULL) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        text = element_text(size = 10, family = FONT),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linewidth = 0.3),
        legend.position = "bottom")

out_path <- file.path(data_dir, "results/memo1_simplified_migration_rate_by_cohort.png")
ggsave(filename = out_path, plot = p, width = 6.5, height = 3.75, units = "in", dpi = 600, bg = "transparent")
cat(sprintf("Wrote %s\n", out_path))
