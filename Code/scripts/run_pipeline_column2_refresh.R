# One-off driver: regenerate Column 2 (06_finalize_data's `regression`
# table) to reflect Step 0's col_start/hs_start/col_field fix (commit
# f885f9a), without re-running 01a_alias_generation/01b_embed_new/
# 01c_crosswalk_val -- expensive one-time live geocoding/embedding calls,
# unaffected by that fix, whose file outputs (col_rsid_crosswalk.rds,
# colleges.rds, schools.rds, etc.) already exist on disk.
#
# Dependency chain confirmed by reading every stage's actual inputs
# (2026-08-09): 00_crosswalks and 01_shocks are NOT safely skippable
# despite looking file-output-only -- almost everything they build
# (county_full, cbsa_li_crosswalk, institution_crosswalk, cbsa_shock_ptile)
# is a bare in-session object with no file fallback, consumed downstream by
# 02_col_chars/02_hs_chars/04_li_ed_pos/05_merge. Matches
# run_pipeline.R's own source()-chain, single-continuous-session pattern --
# just a shorter stage list.
#
# Run from the repo root:
#   Rscript Code/scripts/run_pipeline_column2_refresh.R

stages <- c(
  "00_crosswalks", "01_shocks",
  "01d_col_hs_construct",
  "02_col_chars", "02_hs_chars",
  "03_li_ed", "04_li_ed_pos", "05_merge", "06_finalize_data"
)

plain_r_stages <- c("01d_col_hs_construct")

for (s in stages) {
  path <- if (s %in% plain_r_stages) {
    file.path("Code", paste0(s, ".R"))
  } else {
    file.path("Code", "scripts", paste0(s, ".R"))
  }
  cat(sprintf("\n=== [%s] Running %s ===\n", format(Sys.time()), s))
  source(path, echo = FALSE)
  cat(sprintf("=== [%s] Completed %s ===\n", format(Sys.time()), s))
}
