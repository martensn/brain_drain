# Non-interactive driver for the root numbered pipeline. Two kinds of stages:
# .Rmd-derived (converted to Code/scripts/<stage>.R by convert_rmd_to_r.R) and
# plain hand-written .R stages that live directly under Code/ with no .Rmd
# source (the crosswalk-construction sub-sequence below, folded in from
# Code/new/ in Phase C, 2026-07-29). 10_roi.Rmd was archived to Code/Old/
# (2026-07-29) -- confirmed genuinely broken (reads a pre-reorg path that no
# longer exists), not just fragile.
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
  "00_crosswalks", "01_shocks",
  "01a_alias_generation", "01b_embed_new", "01c_crosswalk_val", "01d_col_hs_construct",
  "02_col_chars", "02_hs_chars",
  "03_li_ed", "04_li_ed_pos", "05_merge", "06_finalize_data",
  "07_regressions", "08_data_generation", "09_plots"
)

# Stages with no .Rmd source -- live directly under Code/, not Code/scripts/.
plain_r_stages <- c("01a_alias_generation", "01b_embed_new", "01c_crosswalk_val", "01d_col_hs_construct")

args <- commandArgs(trailingOnly = TRUE)
last_stage <- if (length(args) >= 1) args[1] else stages[length(stages)]
if (!last_stage %in% stages) {
  stop("Unknown stage '", last_stage, "'. Valid: ", paste(stages, collapse = ", "))
}
run_stages <- stages[seq_len(which(stages == last_stage))]

for (s in run_stages) {
  path <- if (s %in% plain_r_stages) {
    file.path("Code", paste0(s, ".R"))
  } else {
    file.path("Code", "scripts", paste0(s, ".R"))
  }
  cat(sprintf("\n=== [%s] Running %s ===\n", format(Sys.time()), s))
  source(path, echo = FALSE)
  cat(sprintf("=== [%s] Completed %s ===\n", format(Sys.time()), s))
}
