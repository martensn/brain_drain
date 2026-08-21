# memo1_11_alt_specs_plot.R
#
# [NEW 2026-08-21, REPLACES a scratch-only script from the same week that
# no longer exists] Rebuilds MEMO1_WEIGHTING.md SS8's summary figure
# (memo1_alt_specs_summary.png), in Code/scripts/09_plots.R's actual
# conventions (Segoe UI, transparent background, bottom legend,
# horizontal-only gridlines -- the original build predated that
# convention request and used a serif fallback).
#
# Checked before rebuilding, per Nicholas's explicit ask ("make sure clean
# replication ... wouldn't be a giant pain") -- both panels' source data
# are genuinely still on disk / re-derivable without re-running anything
# expensive:
#
#   Panel A (IRS SOI destination-tier margin test, SS8 row 1): the three
#   numbers (unweighted 0.0287, kept demo+mover scheme 0.0234, demo+IRS
#   desttier 0.0263 -- single year, 2021) are hardcoded below because
#   that's genuinely all that exists: reweight_column2_desttier_test.R
#   PRINTS this comparison but never saved it to a CSV (documented in
#   MEMO1_WEIGHTING.md SS8's own table). The exact numbers are transcribed
#   verbatim from HANDOFF.md's 2026-08-11/12 entry, not re-derived from
#   memory. If an exact re-run is ever wanted: the raw IRS SOI file is
#   still cached (Data/raw/irs_soi/countyoutflow2021.csv, 4.4MB), the
#   margin/test-weight intermediate .rds files are still on disk
#   (Data/intermediate/irs_soi_desttier_margin_2021.rds,
#   column2_reweighted_desttier_test.rds), and both scripts
#   (Code/memo1_alternative_specs/irs_soi_desttier_margin.R,
#   reweight_column2_desttier_test.R) are self-contained, well-headered,
#   and don't touch production -- rerunning end-to-end would NOT require
#   re-pulling anything from Census or IRS.
#
#   Panel B (metro-tier scheme granularity, SS8 row 4): fully saved,
#   Data/results/memo1_scheme_gap_summary.csv, written by the production
#   Code/memo1_06b_scheme_comparison.R. No re-run needed at all.

library(data.table)
library(ggplot2)
library(gridExtra)
library(grid)
library(ragg)  # base grDevices::png() doesn't see Segoe UI (GDI font db) --
                # same silent-fallback trap "LM Roman 10" hit earlier; ragg's
                # agg_png() uses systemfonts, same as ggsave()'s default device.
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

FONT <- "Segoe UI"
diverging_colors <- c('#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837')

shared_theme <- theme(
  panel.background = element_rect(fill = "transparent", color = NA),
  plot.background  = element_rect(fill = "transparent", color = NA),
  legend.background = element_rect(fill = "transparent", color = NA),
  legend.key = element_rect(fill = "transparent", color = NA),
  text = element_text(size = 10, family = FONT),
  plot.title = element_text(hjust = 0.5, size = 11),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "grey", linewidth = 0.3),
  legend.position = "bottom"
)

## ---- Panel A: IRS SOI destination-tier margin test (2021 only) ----
## Numbers transcribed verbatim from HANDOFF.md's 2026-08-11/12 entry --
## see header above for why these aren't re-derived from a saved CSV.
irs_test <- data.table(
  spec = factor(c("Unweighted", "Demo. + mover\n(kept)", "Demo. + IRS\ndest-tier"),
                levels = c("Unweighted", "Demo. + mover\n(kept)", "Demo. + IRS\ndest-tier")),
  gap = c(0.0287, 0.0234, 0.0263),
  status = c("baseline", "better", "worse")
)
status_colors <- c(baseline = "#9e9e9e", better = diverging_colors[9], worse = diverging_colors[1])

pA <- ggplot(irs_test, aes(x = spec, y = gap, fill = status)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(gap, accuracy = 0.1)), vjust = -0.5, family = FONT, size = 3.2) +
  scale_fill_manual(values = status_colors, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Metro-tier gap vs. ACS (2021)",
       title = "IRS SOI destination-tier margin, single-year test")

## ---- Panel B: metro-tier scheme granularity (full sample, migration gap) ----
scheme_gap <- fread(file.path(data_dir, "results/memo1_scheme_gap_summary.csv"))
scheme_mig <- scheme_gap[metric == "migration"]
scheme_mig[, scheme := factor(scheme, levels = c("rank5", "rank3", "rank3_region"),
                               labels = c("5-tier", "3-tier", "3-tier×region\n(kept)"))]
scheme_mig[, phase := factor(phase, levels = c("static", "phase_a", "phase_b"),
                              labels = c("Static", "Phase A", "Phase B"))]

pB <- ggplot(scheme_mig, aes(x = scheme, y = gap, fill = phase)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  scale_fill_manual(values = c(Static = diverging_colors[5], `Phase A` = diverging_colors[3], `Phase B` = diverging_colors[9]),
                     name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "Migration-rate gap vs. ACS",
       title = "Metro-tier classification granularity (full sample)")

## Device opened BEFORE any grob construction, not just before drawing --
## grid's text-metric calculations (geom_text label widths) query font
## metrics off whatever device is active AT CONSTRUCTION TIME, not draw
## time. arrangeGrob() alone (built with no device open) silently falls
## back to a generic PostScript font table; grid.arrange() inside an
## already-open ragg device avoids that same-class silent-fallback trap.
out_path <- file.path(data_dir, "results/memo1_alt_specs_summary.png")
agg_png(out_path, width = 8.5, height = 4.2, units = "in", res = 600, background = "transparent")
grid.arrange(pA + shared_theme, pB + shared_theme, ncol = 2, widths = c(1, 1.15))
dev.off()
cat(sprintf("Wrote %s\n", out_path))
