# Handoff — BRAIN_DRAIN

## Status
**Phase A is fully closed.** Phase 1 (crosswalk), Phase 2 (parquet position
rewiring), Phase A.3b (institution-lookup fixes), and Phase A.3's before/after
diff (waterfall + current-sample plausibility checks — a true row-level
pre-Phase-1 baseline was never preserved, so this was a deliberate scope
choice) are all landed and verified. `microdata.csv` = 3,603,589 rows, zero
duplicate `user_id`s, no stray columns (pre-Phase-1 baseline: 3,097,275,
+16.3%). The tolerance-fork decision (safeguard 5): **inside tolerance,
proceed** — the full +16.3% traces to specific verified causes (vintage fix,
institution-lookup backfill, 4 bug fixes found while producing the diff), no
plausibility red flags. `d_sample_construction.R` confirmed orphaned (no
active consumers anywhere in the repo) and retired to `Code/Old/`. Tagged
`phase1-crosswalk-consolidated`; `phase1-crosswalk-consolidation` merged into
`master` (fast-forward, clean — `master` was a strict ancestor). `master` is
now 7 commits ahead of `origin/master`, not yet pushed. Working tree clean.

## Next steps
- [ ] Push `master` to `origin` (holding off — confirm you want the merge
      published before doing so)
- [ ] Phase B: fix the 5 submission-relevant bugs in `CODEBASE_AUDIT.md` §0
      (`06_finalize_data.Rmd:241-242` HS/college cohort-horizon mismatch;
      `08_data_generation.Rmd:488`/`kappa.R:16` `x*x` typo; `05_merge.Rmd:560`
      `soc_merge.csv` written from the wrong object; `09_plots.Rmd:41`
      `specification` reset — confirm which one produced the submitted
      figures before fixing; `Code/measure_return.R` mislabeled/inert)
- [ ] Phase C: retire `Code/new/` as a folder structure
- [ ] Phase D: cross-cutting hygiene (`CENSUS_KEY` case mismatches, move
      secrets to `.env`, output-dir creation)
- [ ] Phase E: `renv` pinning
- [ ] Phase F: final re-verification, fresh `HANDOFF.md`, unblock Group 1
      in `purrfect-mapping-acorn.md`

## Key files
- `D:\Users\martensn\.claude\plans\yes-let-s-resolve-this-misty-kahan.md` —
  live master plan, current through Phase A's close (tolerance decision +
  `d_sample_construction.R` resolution)
- `Code/college_lookup.R` — shared era-aware institution-resolution helper
- `Code/Old/d_sample_construction.R` — retired, see `Code/Old/README.md` for
  why and what it used to feed
- `Data/intermediate/microdata.csv` — regenerated, verified duplicate-free

## Decisions
- `unitid` alone isn't a unique institution key — lookups key on `unitid` +
  reference year via `resolve_college()`
- Chose waterfall + plausibility checks over reconstructing a true row-level
  baseline (would mean re-running superseded code against preserved inputs)
- Chugach/Copper River Alaska GeoFIPS mismatch (`00_crosswalks.Rmd:88`)
  noted, left unfixed — confirmed zero rows affected
- Tolerance-fork (safeguard 5): +16.3% is inside tolerance, treated as a
  like-for-like update, not a fork trigger
- `d_sample_construction.R`: retired to `Code/Old/`, not deleted or kept in
  place — zero active consumers confirmed by repo-wide search
