Legacy scripts, not updated in the 2026 Data/ reorg -- paths in here are stale
(they still reference the old numbered `Data/01`-`07` layout and dead machine
paths). Retained for reference only, not guaranteed to run.

`b_embedding_test.R`, `c_col_hs_classification.R`, `01_col_hs_crosswalk.R`,
`02_embed.R` (2026-07-25): different situation -- paths are current, not
stale. Confirmed superseded during Phase 1 crosswalk consolidation:
`Code/new/02_embed_new.R` is the script that actually produced the submitted
sample's crosswalk (and now, post-consolidation, drives the full pipeline);
these four were either a parallel/abandoned matching approach
(`c_col_hs_classification.R`, `b_embedding_test.R`) or had their only
load-bearing logic relocated into `00_alias_generation.R`
(`01_col_hs_crosswalk.R`, `02_embed.R`).

`d_sample_construction.R` (2026-07-29): orphaned, not superseded by a
specific replacement. Was meant to feed the reweighting work in
`Code/new/07_acs_reweight.R` (`col_ct`/`imp_ct` institution coverage
ratios), but confirmed via full-repo search that nothing currently reads any
of its outputs (`ed_wide.rds`, `ed_wide_sample.rds`, `col_ct.rds`/`.csv`,
`raw_unmatched_strings.rds`/`.csv`) -- `07_acs_reweight.R` itself explicitly
notes it does *not* rebuild `d_sample_construction.R`'s `ed_wide`. If the
reweighting item resumes, this is the starting point, but its inputs
(`col_strings.rds`/`hs_strings.rds`, unresolved-match string tables) predate
the Phase 1 crosswalk consolidation and should be re-checked against
`both_final.rds`/`colleges.rds` rather than assumed current.

`a_hs_col_construct.R`, `e_scrape.R` (2026-07-29): **genuine landmines, not
just stale.** `a_hs_col_construct.R` is an old, independent draft of the
same institution/alias construction `Code/new/00_alias_generation.R` now
does -- it writes to the exact same paths (`colleges.rds`, `schools.rds`,
`hs_alias.rds`, `col_alias.rds`) that the entire Phase 1 crosswalk
consolidation (and everything built on `resolve_college()` since) now
depends on. If ever run, it would silently overwrite the verified,
era-aware `colleges.rds` with a different, pre-Phase-1 construction. Same
letter-script era as the already-archived `b_embedding_test.R`/
`c_col_hs_classification.R`, just not caught in that pass. `e_scrape.R` only
feeds `a_hs_col_construct.R` (via `be_sch.csv`, itself broken -- writes
`be_sch.csv` but `a_hs_col_construct.R` reads `be_sch_full.csv`, no script
bridges the rename) and has no other consumer, so it's archived alongside
it.

`demo.R`, `03_dedup_col.R` (2026-07-29): duplicates of each other and of the
same dead lineage as `a_hs_col_construct.R` above -- `03_dedup_col.R`'s
entire 14 lines are a subset of `demo.R`'s tail, and `03_dedup_col.R`'s own
comment says it "supersedes `new/01_col_hs_crosswalk.R`" (already archived
above). **`demo.R` is also a landmine**: it writes to
`Data/intermediate/unmatched_col.rds`, the exact path the live
`Code/new/02_embed_new.R` writes to and `Code/new/03_crosswalk_val.R` reads
from -- running it by accident would silently corrupt a real production
intermediate. Neither file is referenced anywhere in the codebase.

`06_census.R` (2026-07-29): unfinished draft (undefined object reference,
broken cache check per `CODEBASE_AUDIT.md`). Confirmed not a live
dependency of anything -- `Code/new/07_acs_reweight.R`'s comments show its
race/sex category-scheme logic was already copied inline from this file's
source, not `source()`d from it. Archived rather than finished since nothing
currently needs it completed.

`10_roi.Rmd` (2026-07-29): confirmed genuinely broken on a fresh run, not
just fragile -- reads `results/regression_data__06.csv`, a pre-2026-reorg
path that doesn't exist under the current `Data/intermediate/` layout. Also
depends on a 197MB `Code/06_workspace.RData` snapshot (present on disk but
git-ignored, and would silently overwrite whatever the current session
built if it does exist) and separately rebuilds its own
`institutional_characteristics` from a `year=2021`-only IPEDS pull, already
noted in `CODEBASE_AUDIT.md` as drifted from `02_col_chars.Rmd`'s real
version. Three independent problems; archived rather than fixed. Revisit
deliberately if the ROI analysis is specifically needed later -- not
replicated anywhere else in the active pipeline.

`09b_plots_old.Rmd` (2026-07-29): superseded by its own `_old` suffix and
naming (`13_9b_plots_old.Rmd` in some older notes -- `09b_plots_old.Rmd` is
the actual current filename). No live consumer.

## Memo 1-era archive (merged in from a separate `Archive/` folder, 2026-08-24)

The Memo 1 cleanup pass (see `../README_memo1.md`) produced its own batch of
superseded/dead files, briefly kept in a second `Code/Archive/` folder before
being merged here, since one archive location is clearer than two. Verified
via output-file grep (not just `source()`) before archiving any of these --
see `README_memo1.md`'s own "`Archive/`" section for the full per-file
rationale, not repeated here. Summary:
- `memo1_10_metro_tier_plot.R` / `_add_occ_line.R` -- superseded by
  `memo1_11a_final_plots.R`.
- `memo1_02c_acs_pull_1yr_race2015.R` / `_occp2015.R` / `_occp_allyears.R` --
  superseded by the consolidated `memo1_02c_acs_pull_1yr_supplements.R`.
- `metro_tier_map.R`, `acs_cbsa_population_benchmark.R`,
  `nativity_profile_creation.R` + `_plots.R`, `omission_missingness_analysis.R`
  + `_plot.R`, `phase_b_flow_loss_analysis.R`, `puma_boundary_crossing_analysis.R` --
  standalone diagnostics, never part of Memo 1's own write-up, no downstream
  consumer.
- `cohort.R`, `kappa.R`, `measure_return.R`, `e_detect_mismatches.R`,
  `yagan.R` -- pre-Memo-1 (JOLE-era) analysis scripts, no saved output at all.
- `acs_reweight.R` + `regression_data.csv` -- the JOLE-referee-response-era
  5-rung reweighting ladder; its `pums_cells.rds`/`pums_acs5_filt.rds` writes
  are shadowed by `memo1_02a_acs_pull_5yr.R`'s fresher build of the same
  filenames.

Unlike the rest of this folder, these files are current-vintage (2026-08,
not 2023-2025) and their paths are NOT stale -- they were archived for being
superseded or dead-ended, not for being outdated in the "old `Data/`
layout" sense that applies to everything above this section.
