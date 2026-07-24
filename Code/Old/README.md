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
