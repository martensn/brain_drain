# nativity_profile_creation_plots.R
#
# [NEW 2026-08-21] Two 3-panel scatter figures from
# Code/nativity_profile_creation.R's output
# (Data/results/nativity_profile_creation_state_cohort.csv), styled to
# resemble Code/09_plots.Rmd's orig_benchmark.png -- size-weighted points,
# color+shape by a 4-category grouping (Census region here, in place of
# orig_benchmark's institutional group), transparent background, bottom
# legend -- but with a real fitted regression line drawn per panel (not
# just a y=x reference line, since nativity share and profile-creation/
# HS-disclosure rate are different concepts, not two measures of the same
# thing) plus its equation/R² annotated, per Nicholas's explicit request.
# Font is Segoe UI, matching every other plot rebuilt this session (not
# "LM Roman 10", which orig_benchmark.png itself used and which silently
# falls back to serif on this machine).
#
# Each fitted line is a population-weighted OLS (weight = pop_n, the ACS
# cell population -- same convention as 09_plots.Rmd's own weighted lm()
# calls, weight = total_deg_awarded there), fit SEPARATELY per cohort
# panel, matching the facet structure.

library(data.table)
library(ggplot2)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

FONT <- "Segoe UI"
inst_group_colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")  # Code/scripts/09_plots.R's own 4-color qualitative palette
region_order <- c("Northeast", "Midwest", "South", "West")
region_colors <- setNames(inst_group_colors, region_order)
region_shapes <- setNames(c(15, 17, 16, 18), region_order)

d <- fread(file.path(data_dir, "results/nativity_profile_creation_cbsa_cohort.csv"))
d[, cohort := factor(cohort, levels = c("Pre-1980", "1980s", "1990s"))]
d[, region := factor(region, levels = region_order)]

shared_theme <- theme(
  panel.background = element_rect(fill = "transparent", color = NA),
  plot.background  = element_rect(fill = "transparent", color = NA),
  legend.background = element_rect(fill = "transparent", color = NA),
  legend.key = element_rect(fill = "transparent", color = NA),
  strip.background = element_blank(),
  strip.text = element_text(face = "bold"),
  text = element_text(size = 10, family = FONT),
  plot.title = element_text(hjust = 0.5),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "grey", linewidth = 0.25),
  axis.line.x = element_line(color = "black"),
  legend.position = "bottom"
)

# Per-cohort weighted OLS -> equation/R² label table, positioned at each
# panel's own top-left corner via -Inf/Inf (resolves independently per
# facet, so this works even though plot 1 and plot 2's y-ranges differ a
# lot from each other).
fit_labels <- function(dt, xvar, yvar) {
  dt[, {
    m <- lm(get(yvar) ~ get(xvar), weights = pop_n)
    s <- summary(m)
    b0 <- round(coef(m)[1], 3); b1 <- round(coef(m)[2], 3)
    r2 <- round(s$r.squared, 3); se <- round(s$coefficients[2, 2], 3)
    .(label = sprintf("y = %s + %sx\nR² = %s   SE = %s", b0, b1, r2, se))
  }, by = cohort]
}

make_plot <- function(yvar, ylab, title) {
  labs_dt <- fit_labels(copy(d), "nativity_share", yvar)
  ggplot(d, aes(x = nativity_share, y = .data[[yvar]])) +
    geom_point(aes(size = pop_n, color = region, shape = region), alpha = 0.55, show.legend = c(size = FALSE)) +
    geom_smooth(aes(weight = pop_n), method = "lm", formula = y ~ x, se = FALSE,
                color = "black", linetype = "dashed", linewidth = 0.6) +
    facet_wrap(~cohort) +
    scale_shape_manual(values = region_shapes, name = NULL) +
    scale_color_manual(values = region_colors, name = NULL) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_text(data = labs_dt, aes(x = -Inf, y = Inf, label = label), inherit.aes = FALSE,
              hjust = -0.05, vjust = 1.3, size = 2.9, family = FONT, lineheight = 0.9) +
    labs(x = "Share of the metro's college-educated FT workforce born in-state (ACS)", y = ylab, title = title) +
    shared_theme
}

# [HELD BACK 2026-08-21] profile_creation_rate (Column 1 / ACS population)
# has an unresolved structural problem -- median cell is 173% of the true
# ACS full-time population, 59% of cells exceed 100%, not explainable by
# small-cell noise alone. Leading hypothesis: Column 1's cbsa_code_t for a
# given year may not always reflect a genuinely-observed 2022 position
# (could be carrying forward a stale/imputed location), or Column 1 simply
# isn't restricted to full-time workers the way the ACS denominator is.
# Per Nicholas's explicit instruction, only hs_disclosure_rate is built
# for now -- that one is internally consistent (Column 2 is a verified
# subset of Column 1 by construction, hs_disclosure_rate in [0, 0.333])
# and doesn't depend on the ACS population estimate at all.
#
# p1 <- make_plot("profile_creation_rate", "Column 1 profiles / true labor market (ACS)",
#                  "Does profile creation vary with labor-market nativity?")
# out1 <- file.path(data_dir, "results/nativity_profile_creation_rate.png")
# ggsave(out1, p1, width = 9.5, height = 3.6, units = "in", dpi = 600, bg = "transparent")
# cat(sprintf("Wrote %s\n", out1))

p2 <- make_plot("hs_disclosure_rate", "HS disclosure rate (Column 2 / Column 1)",
                 "Does HS disclosure vary with labor-market nativity?")
out2 <- file.path(data_dir, "results/nativity_hs_disclosure_rate.png")
ggsave(out2, p2, width = 9.5, height = 3.9, units = "in", dpi = 600, bg = "transparent")
cat(sprintf("Wrote %s\n", out2))
