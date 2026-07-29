# Handoff — BRAIN_DRAIN

## Status
**Phases A, B, and C are all closed.** Phase A: Phase 1 (crosswalk), Phase 2
(parquet position rewiring), Phase A.3b (institution-lookup fixes), and Phase
A.3's before/after diff are landed and verified. `microdata.csv` =
3,603,589 rows, zero duplicate `user_id`s (pre-Phase-1 baseline: 3,097,275,
+16.3%, inside tolerance per safeguard 5). `d_sample_construction.R` retired
to `Code/Old/`. Tagged `phase1-crosswalk-consolidated`, merged to `master`.

**Phase B**: all 5 catalogued bugs resolved (4 fixed, 1 — `09_plots.Rmd`'s
`specification` toggle — confirmed intentional, not a bug). Verifying the
fixes surfaced a much bigger, previously-uncatalogued bug:
`institutional_characteristics.csv` had duplicate rows for 95% of its
institutions (up to 20 rows/unitid), from an uncollapsed multi-year IPEDS
pull plus an uncollapsed Chetty mobility/super_opeid merge in
`02_col_chars.Rmd`. **This predates this summer's work and may have silently
affected the originally-submitted paper's figures/tables** — `09_plots.Rmd`'s
`dplyr::left_join()` calls have no Cartesian-join guard and would have
absorbed the duplication with no error. Fixed by sourcing institution
directory data from `colleges.rds` (via `resolve_college()`) instead of a
fresh IPEDS pull, and properly collapsing the mobility fan-out. Verified:
exactly one row per `unitid` (3,510 = 3,510).
**Deliberately deferred, not forgotten**: re-running `09_plots.Rmd`/
`10_roi.Rmd` against the fix and diffing against the submitted figures.

**Phase C**: `Code/new/` no longer exists as a folder. The four live
crosswalk scripts folded into the root pipeline as a `01a-01d` sub-sequence
(`Code/01a_alias_generation.R` .. `Code/01d_col_hs_construct.R`, between
`01_shocks` and `02_col_chars`/`02_hs_chars` — sourced directly from `Code/`
by `run_pipeline.R`, not `Code/scripts/`, since they have no `.Rmd` source).
Investigating each ambiguous item (not just static reads) surfaced two
genuine landmines beyond what the audit flagged: `demo.R` wrote to the same
path the *live* `02_embed_new.R`/`03_crosswalk_val.R` use
(`unmatched_col.rds`), and `a_hs_col_construct.R` wrote to the same paths
the live `01a_alias_generation.R` owns (`colleges.rds`, `schools.rds`,
`hs_alias.rds`, `col_alias.rds`) — both archived along with their only
feeders (`03_dedup_col.R`, `e_scrape.R`). Also archived: `06_census.R`
(confirmed not a live dependency — `acs_reweight.R`'s logic was already
copied inline), `10_roi.Rmd` (confirmed genuinely broken, not just fragile —
reads a pre-reorg path that no longer exists), `09b_plots_old.Rmd`. The two
remaining non-pipeline `Code/new/` scripts relocated:
`Code/demographics.R`, `Code/acs_reweight.R` (prefix dropped, standalone
scripts like `kappa.R`).

**Not yet done**: a full pipeline re-verification run. No functional code
changed inside the four moved crosswalk scripts (confirmed no internal
`Code/new/` self-references existed), so the move itself is low-risk, but
an end-to-end rerun is the honest confirmation — held off given cost (the
moved scripts are the multi-hour embedding/matching stage). Naturally folds
into Phase F rather than needing its own expensive run right now.

Working tree clean, all committed and pushed to `origin/master`.

## Next steps
- [ ] **Phase D** (active now): cross-cutting hygiene — `CENSUS_KEY` vs
      `census_key` case mismatches (3 locations), move the OpenAI key and
      Census key into `.env`, move `RETICULATE_PYTHON`'s hardcoded path into
      `.env`, have scripts create their own output directories
- [ ] Phase E: `renv` pinning
- [ ] Phase F: final full pipeline re-verification (also covers Phase C's
      deferred rerun), fresh `HANDOFF.md`, unblock Group 1 in
      `purrfect-mapping-acorn.md`
- [ ] **(Deferred, revisit deliberately)** Re-run `09_plots.Rmd`/`10_roi.Rmd`
      against the fixed `institutional_characteristics.csv` and compare
      against the originally-submitted figures

## Key files
- `D:\Users\martensn\.claude\plans\yes-let-s-resolve-this-misty-kahan.md` —
  live master plan, current through Phase C's close
- `Code/01a_alias_generation.R` .. `Code/01d_col_hs_construct.R` — the
  crosswalk-construction sub-sequence, folded in from `Code/new/`
- `Code/college_lookup.R` — shared era-aware institution-resolution helper,
  used by `01d_col_hs_construct.R`, `04_li_ed_pos.Rmd`, `05_merge.Rmd`, and
  now `02_col_chars.Rmd`
- `Code/Old/README.md` — up to date with every 2026-07-29 archival decision
  and why
- `Data/intermediate/microdata.csv` — regenerated, verified duplicate-free
- `Data/intermediate/institutional_characteristics.csv` — regenerated,
  verified one row per `unitid`

## Decisions
- `unitid` alone isn't a unique institution key — lookups key on `unitid` +
  reference year via `resolve_college()`
- Tolerance-fork (safeguard 5): +16.3% is inside tolerance, treated as a
  like-for-like update, not a fork trigger
- `09_plots.Rmd`'s `specification` toggle is intentional design, not a bug
- Institution directory data in `02_col_chars.Rmd` now sourced from
  `colleges.rds` (most-recent-era fallback) rather than a fresh IPEDS pull;
  mobility/super_opeid rows collapsed by summing counts and weighted-
  averaging rates
- Phase C's crosswalk scripts numbered as `01a-01d` (minimal-diff sub-
  sequence) rather than a full pipeline renumber
- `demographics.R`/`acs_reweight.R` kept unnumbered (standalone, manually-
  run scripts, not automated-pipeline stages) rather than folded into the
  numbered sequence
- `06_census.R`, `10_roi.Rmd`, `demo.R`/`03_dedup_col.R`,
  `e_scrape.R`/`a_hs_col_construct.R`, `09b_plots_old.Rmd`: all archived to
  `Code/Old/` rather than fixed — each confirmed either genuinely broken,
  superseded, or actively dangerous (silent overwrite of a live production
  file) before archiving, not assumed
