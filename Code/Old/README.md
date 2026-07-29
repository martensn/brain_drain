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
