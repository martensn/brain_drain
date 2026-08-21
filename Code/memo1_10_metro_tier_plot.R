# memo1_10_metro_tier_plot.R
#
# [NEW 2026-08-21] Full-sample metro-tier share figure for MEMO1_WEIGHTING.md
# -- companion to memo1_09_memo_plots.R's migration-rate figure. Same four
# kept lines (BA Only / BA + HS on LI / BA + HS on LI (reweighted) / ACS
# PUMS), same 09_plots.R styling (transparent background, bottom legend,
# horizontal-only gridlines, Segoe UI), same inst_group_colors palette.
#
# Tier categories are the memo's OWN SS5.1 definition -- 3 size tiers
# (Top 10 / Top 11-50 / Everything else) -- not the 5-tier scheme the
# interactive cohort artifacts used. The "reweighted" line is the settled
# rank3_region (size x region) Phase B flow calibration, computed at its
# native 12-cell resolution and collapsed to these 3 size categories for
# display (see memo1_08_full_sample_extras.R Part A for the calibration
# itself -- this script only plots its output,
# memo1_metro_tier_by_calendar_year_full_simplified.csv).

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
series_order  <- c(LBL_COL1, LBL_COL2U, LBL_COL2R, LBL_ACS)
series_colors <- setNames(inst_group_colors, series_order)
tier_order <- c("Top 10", "Top 11-50", "Everything else")

d <- fread(file.path(data_dir, "results/memo1_metro_tier_by_calendar_year_full_simplified.csv"))
d[, source := factor(source, levels = series_order)]
d[, tier := factor(tier, levels = tier_order)]
# Same pre-2000 data-coverage exclusion as the migration-rate figure --
# only touches the two unweighted Revelio lines, which are the only ones
# with pre-2012 coverage at all.
d <- d[calendar_year >= 2000]

p <- ggplot(d, aes(x = calendar_year, y = share, color = source)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.1) +
  facet_wrap(~tier, nrow = 1) +
  scale_color_manual(values = series_colors, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Share of population", title = NULL) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        text = element_text(size = 10, family = FONT),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linewidth = 0.3),
        legend.position = "bottom")

out_path <- file.path(data_dir, "results/memo1_full_sample_metro_tier_share.png")
ggsave(filename = out_path, plot = p, width = 6.5, height = 3.4, units = "in", dpi = 600, bg = "transparent")
cat(sprintf("Wrote %s\n", out_path))
