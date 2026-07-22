# Converts the root numbered .Rmd pipeline (00_crosswalks.Rmd .. 10_roi.Rmd)
# into plain .R scripts under Code/scripts/, so the pipeline can be run
# non-interactively via `Rscript Code/scripts/run_pipeline.R`.
#
# knitr::purl() already comments out eval=FALSE chunks entirely (confirmed
# empirically -- it's not just an eval-at-knit-time thing, purl respects it
# too), so dormant/exploratory chunks come through as inert comments with no
# extra handling needed here.
#
# Re-run this script any time the source .Rmd files change.

library(knitr)

# knitr::purl() (like knitting) errors on duplicate chunk labels within a
# single file -- and duplicate labels turn out to be widespread across this
# codebase (e.g. "groupings" appears twice in 02_col_chars.Rmd, "sumstat"
# three times in 09_plots.Rmd). Pre-existing in the source, not something
# this conversion should silently paper over by renaming in the original
# .Rmd -- instead, rename duplicates only in the temp copy used for purl.
dedupe_chunk_labels <- function(lines) {
  header_idx <- grep("^```\\{r", lines)
  seen <- list()
  for (i in header_idx) {
    ln <- lines[i]
    m <- regexpr("^```\\{r\\s*", ln)
    prefix <- substr(ln, 1, attr(m, "match.length"))
    rest <- substr(ln, attr(m, "match.length") + 1, nchar(ln))
    lbl <- trimws(strsplit(rest, ",", fixed = TRUE)[[1]][1])
    lbl <- sub("\\}\\s*$", "", lbl)  # handles a label-only, no-options chunk like ```{r label}
    if (!nchar(lbl)) next  # unlabeled chunk -- knitr auto-names these, no collision risk
    seen[[lbl]] <- (seen[[lbl]] %||% 0) + 1
    if (seen[[lbl]] > 1) {
      new_lbl <- paste0(lbl, "_dup", seen[[lbl]])
      rest_new <- paste0(new_lbl, substr(rest, nchar(lbl) + 1, nchar(rest)))
      lines[i] <- paste0(prefix, rest_new)
    }
  }
  lines
}
`%||%` <- function(a, b) if (is.null(a)) b else a

rmd_files <- c(
  "Code/00_crosswalks.Rmd",
  "Code/01_shocks.Rmd",
  "Code/02_col_chars.Rmd",
  "Code/02_hs_chars.Rmd",
  "Code/03_li_ed.Rmd",
  "Code/04_li_ed_pos.Rmd",
  "Code/05_merge.Rmd",
  "Code/06_finalize_data.Rmd",
  "Code/07_regressions.Rmd",
  "Code/08_data_generation.Rmd",
  "Code/09_plots.Rmd",
  "Code/10_roi.Rmd"
)

dir.create("Code/scripts", showWarnings = FALSE)

for (rmd in rmd_files) {
  lines <- readLines(rmd, warn = FALSE)
  lines <- dedupe_chunk_labels(lines)

  tmp <- tempfile(fileext = ".Rmd")
  writeLines(lines, tmp)
  out <- file.path("Code", "scripts", sub("\\.Rmd$", ".R", basename(rmd)))
  knitr::purl(tmp, output = out, documentation = 1, quiet = TRUE)
  unlink(tmp)
  cat("Converted:", rmd, "->", out, "\n")
}
