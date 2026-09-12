# theme_web.R
#
# [NEW 2026-09-11] Reusable ggplot2 "website" theme + save helper for
# embedding BRAIN_DRAIN figures on martensn.github.io -- a dark-mode
# personal site (background #282b33, foreground #fffdda, per that repo's
# styles.css :root tokens; https://github.com/martensn/martensn.github.io).
#
# WHY THIS EXISTS: every figure-producing script in this project builds its
# own theme() call assuming a LIGHT page background. Three genuinely
# different variants exist across the scripts feeding
# https://martensn.github.io/trade-in-training.html:
#   1. memo1_11_final_outputs.R's theme_memo() -- solid white panel/plot/
#      legend background, default (black) text.
#   2. Old/nativity_profile_creation_plots.R's `shared_theme` and
#      Old/omission_missingness_plot.R's inline theme() -- transparent
#      panel/plot/legend background, but STILL default black text and an
#      explicit `axis.line.x = element_line(color = "black")`.
#   3. Old/metro_tier_map.R's map theme (built on theme_void()) -- solid
#      white background, explicit dark (#1a1a1a) text/title/legend text.
#
# None of the three render acceptably on the site's dark background as-is:
# variant 1 and 3 show up as a jarring white box; variant 2's "transparent"
# background just makes the still-black text invisible against the page.
#
# HOW TO USE: build your plot exactly as before -- same data layers,
# scales, geoms, and whichever of the three light-mode themes above you'd
# normally use -- into an object `p`, then export the web version with:
#
#   source(here::here("Code/theme_web.R"))
#   ggsave_web("Data/results/some_figure.png", p, width = 6.5, height = 3.9)
#
# This writes a SECOND file (inserting "_web" before ".png") rather than
# replacing the original -- the existing light-mode PNG this project
# already produces is untouched, so nothing that currently reads it (the
# memo, other charts) breaks.
#
# WHY theme_web() sets so many elements explicitly, not just the generic
# `text`/`axis.line`: ggplot2's `plot + theme_A + theme_B` composes
# element-by-element, and inheritance between a general element (e.g.
# `axis.line`) and a more specific one (`axis.line.x`) is resolved at
# RENDER time, not at composition time. If an earlier layer (theme_A) set
# a specific element like `axis.line.x` or `plot.title` explicitly, a
# later layer (theme_B, this file) that only sets the general parent
# (`axis.line`) will NOT override it -- the specific child still wins.
# Since metro_tier_map.R sets `plot.title`/`legend.text`/`axis.line.x`
# explicitly (not just the generic `text`), theme_web() sets every one of
# those same specific elements too, guaranteeing a full override
# regardless of which of the three source templates built the plot.
#
# theme_web() deliberately does NOT touch scale_color_manual()/
# scale_fill_manual() data colors, or deliberate cartographic ink in
# metro_tier_map.R (white state borders, pink #d1476b region borders and
# labels) -- those are substantive content, not chrome, and are left as
# each script's own author chose them.

library(ggplot2)

WEB_BG   <- "#282b33"                                       # --bg in styles.css
WEB_FG   <- "#fffdda"                                       # --text
WEB_GRID <- grDevices::adjustcolor(WEB_FG, alpha.f = 0.18)  # subtle light gridlines on dark
WEB_AXIS <- grDevices::adjustcolor(WEB_FG, alpha.f = 0.55)  # axis line/ticks -- a bit more visible than gridlines
WEB_FONT <- "Arial"  # [FIXED 2026-09-12, was "Segoe UI" -- a plausible-looking
                      # guess that was never actually checked against the site's
                      # real font stack] styles.css sets font-family: "Helvetica
                      # Neue", Helvetica, Arial, sans-serif. Neither Helvetica
                      # Neue nor plain Helvetica are real fonts on Windows (this
                      # machine's "Helvetica Neue for SAS" is a different,
                      # SAS-bundled font, confirmed via systemfonts::system_fonts()
                      # -- not a match), so any Windows/Chrome visitor's browser
                      # already renders the site itself in Arial, its next
                      # fallback -- confirmed actually installed here too. Using
                      # Arial directly is therefore not an approximation, it's
                      # the same font the site's own text already falls back to.
                      # Pass font=NULL to theme_web() to leave whatever font the
                      # original theme already set instead.

#' Web-ready ggplot2 theme for embedding on the dark martensn.github.io site.
#'
#' Add this LAST, after whichever of this project's existing light-mode
#' themes built the plot (theme_memo(), the nativity/omission ad hoc
#' theme(), or metro_tier_map.R's theme_void()-based map theme) --
#' ggplot2 layers compose left-to-right, so tacking this onto the end of
#' an existing plot pipeline overrides every chrome element regardless of
#' what came before, with no need to know or reverse-engineer which
#' template was originally used.
#'
#' @param legend_rows Passed through to guide_legend(nrow=...), matching
#'   theme_memo()'s own signature so existing plot-build code needs no
#'   other change when swapping themes.
#' @param font Font family; defaults to WEB_FONT (Segoe UI). Pass NULL to
#'   leave whatever font the original theme already set (harmless if you
#'   just want the color/background fix without touching typography).
#' @param transparent If TRUE (default), panel/plot/legend backgrounds are
#'   fully transparent -- the page's own dark background shows through,
#'   robust to any future palette tweak on the site. If FALSE, backgrounds
#'   are filled solid with WEB_BG instead (use this only if the PNG needs
#'   to look correct somewhere the background isn't guaranteed to be this
#'   exact dark color, e.g. pasted into a different document).
theme_web <- function(legend_rows = 2, font = WEB_FONT, transparent = TRUE) {
  bg_fill <- if (transparent) "transparent" else WEB_BG

  list(
    guides(color = guide_legend(nrow = legend_rows, byrow = TRUE)),
    theme(
      panel.background   = element_rect(fill = bg_fill, color = NA),
      plot.background    = element_rect(fill = bg_fill, color = NA),
      legend.background  = element_rect(fill = bg_fill, color = NA),
      legend.key         = element_rect(fill = bg_fill, color = NA),
      strip.background   = element_blank(),

      # Every text element any of this project's three light-mode
      # templates sets explicitly, set here too -- see header note on why
      # the generic `text` element alone isn't sufficient.
      text          = element_text(color = WEB_FG, family = font),
      plot.title    = element_text(color = WEB_FG, family = font, hjust = 0.5),
      plot.subtitle = element_text(color = WEB_FG, family = font),
      axis.title    = element_text(color = WEB_FG, family = font),
      axis.text     = element_text(color = WEB_FG, family = font),
      legend.text   = element_text(color = WEB_FG, family = font),
      legend.title  = element_text(color = WEB_FG, family = font),
      strip.text    = element_text(color = WEB_FG, family = font, face = "bold"),

      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = WEB_GRID, linewidth = 0.3),

      axis.line   = element_line(color = WEB_AXIS),
      axis.line.x = element_line(color = WEB_AXIS),
      axis.line.y = element_line(color = WEB_AXIS),
      axis.ticks  = element_line(color = WEB_AXIS),

      legend.position = "bottom"
    )
  )
}

#' Save a ggplot as a website-ready PNG for the dark martensn.github.io
#' site. Thin wrapper around ggsave() that forces the device background
#' to match theme_web()'s own default (transparent, unless transparent =
#' FALSE), so the exported PNG carries no baked-in canvas color at all.
#'
#' @param filename Output path for the ORIGINAL (light-mode) figure --
#'   e.g. the same `out_path` an existing script already passes to its
#'   own ggsave() call. If it doesn't already end in "_web.png", "_web"
#'   is inserted before the extension, so a script that already calls
#'   ggsave() for the light-mode PNG can add one line for the web
#'   version without picking a new filename convention.
#' @param plot The ggplot object, with theme_web() already applied (or
#'   apply it here via the `plot + theme_web(...)` pattern before
#'   passing it in -- either works, since ggplot2 layers are additive).
ggsave_web <- function(filename, plot, width, height, units = "in", dpi = 600, transparent = TRUE, ...) {
  if (!grepl("_web\\.png$", filename)) {
    filename <- sub("(\\.png)$", "_web\\1", filename)
  }
  ggsave(filename = filename, plot = plot, width = width, height = height,
         units = units, dpi = dpi, bg = if (transparent) "transparent" else WEB_BG, ...)
  cat(sprintf("Wrote %s\n", filename))
  invisible(filename)
}
