# Non-interactive driver for the root numbered pipeline (converted from the
# 00_crosswalks.Rmd .. 10_roi.Rmd sequence by convert_rmd_to_r.R).
#
# Run from the repo root:
#   Rscript Code/scripts/run_pipeline.R [last_stage]
#
# last_stage is optional -- one of the names below, default runs everything.
# Example: Rscript Code/scripts/run_pipeline.R 05_merge
#
# This is a `source()`-chain, not independent processes: the original .Rmd
# design has several scripts reading bare in-session objects from earlier
# ones with no file-based fallback (see CODEBASE_AUDIT.md). Running each
# stage as its own separate Rscript call would break on those; sourcing them
# in order within one process reproduces the same continuous session the
# interactive workflow already relies on.

stages <- c(
  "00_crosswalks", "01_shocks", "02_col_chars", "02_hs_chars",
  "03_li_ed", "04_li_ed_pos", "05_merge", "06_finalize_data",
  "07_regressions", "08_data_generation", "09_plots", "10_roi"
)

args <- commandArgs(trailingOnly = TRUE)
last_stage <- if (length(args) >= 1) args[1] else stages[length(stages)]
if (!last_stage %in% stages) {
  stop("Unknown stage '", last_stage, "'. Valid: ", paste(stages, collapse = ", "))
}
run_stages <- stages[seq_len(which(stages == last_stage))]

for (s in run_stages) {
  path <- file.path("Code", "scripts", paste0(s, ".R"))
  cat(sprintf("\n=== [%s] Running %s ===\n", format(Sys.time()), s))
  source(path, echo = FALSE)
  cat(sprintf("=== [%s] Completed %s ===\n", format(Sys.time()), s))
}
