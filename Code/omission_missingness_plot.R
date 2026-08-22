# omission_missingness_plot.R
#
# [NEW 2026-08-21] Two-series version of Outputs/omission_distribution.png
# (Code/Old/08_tables.Rmd's "om plot" chunk) -- All college grads (Column
# 1) vs HS disclosers (Column 2), from Code/omission_missingness_analysis.R's
# omission_distribution_two_series.csv. Same visual language as the
# original (median line + IQR ribbon per series, y=x theoretical-maximum
# reference), Segoe UI / transparent-background / bottom-legend styling
# matching every other plot rebuilt this session (the original used "LM
# Roman 10", never registered on this machine).

library(data.table)
library(ggplot2)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

FONT <- "Segoe UI"
series_colors <- c("All college grads (Column 1)" = "#40004b", "HS disclosers (Column 2)" = "#1b7837")

d <- fread(file.path(data_dir, "results/omission_distribution_two_series.csv"))
d[, series := factor(series, levels = names(series_colors))]

p <- ggplot(d, aes(x = years_elapsed, color = series, fill = series)) +
  geom_ribbon(aes(ymin = yrs_inc_25, ymax = yrs_inc_75), color = NA, alpha = 0.25) +
  geom_line(aes(y = yrs_inc_50), linewidth = 0.9) +
  geom_point(aes(y = yrs_inc_50), size = 1.3) +
  geom_abline(slope = 1, intercept = 0, color = "#2c7fb8", linetype = "solid", linewidth = 0.6) +
  annotate("text", x = max(d$years_elapsed) * 0.42, y = max(d$years_elapsed) * 0.42 + 1.5,
           label = "Theoretical maximum", color = "#2c7fb8", family = FONT, size = 3.2, angle = 33, hjust = 0) +
  scale_color_manual(values = series_colors, name = NULL) +
  scale_fill_manual(values = series_colors, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.005, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)), limits = c(0, NA)) +
  labs(x = "Years since college graduation", y = "Years of work history on LI profile",
       title = "Omission of work history on LI, by years since graduation") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        text = element_text(size = 10, family = FONT),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linewidth = 0.25),
        axis.line.x = element_line(color = "black"),
        legend.position = "bottom")

out <- file.path(data_dir, "results/omission_distribution_two_series.png")
ggsave(out, p, width = 8, height = 5.2, units = "in", dpi = 600, bg = "transparent")
cat(sprintf("Wrote %s\n", out))
