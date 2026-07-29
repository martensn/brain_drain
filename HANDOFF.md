# Handoff — BRAIN_DRAIN

## Status
**Phase A and Phase B are both closed.** Phase A: Phase 1 (crosswalk), Phase 2
(parquet position rewiring), Phase A.3b (institution-lookup fixes), and Phase
A.3's before/after diff are landed and verified. `microdata.csv` =
3,603,589 rows, zero duplicate `user_id`s (pre-Phase-1 baseline: 3,097,275,
+16.3%, inside tolerance per safeguard 5). `d_sample_construction.R` retired
to `Code/Old/`. Tagged `phase1-crosswalk-consolidated`, merged to `master`,
pushed to `origin`.

**Phase B**: all 5 catalogued bugs resolved (4 fixed, 1 — `09_plots.Rmd`'s
`specification` toggle — confirmed intentional, not a bug, per your
correction). Verifying the fixes surfaced a much bigger, previously-
uncatalogued bug: `institutional_characteristics.csv` had duplicate rows for
95% of its institutions (up to 20 rows/unitid) from an uncollapsed
multi-year IPEDS pull plus an uncollapsed Chetty mobility/super_opeid merge,
both in `02_col_chars.Rmd`. **This predates this summer's work and may have
silently affected the originally-submitted paper's figures/tables** —
`09_plots.Rmd`'s `dplyr::left_join()` calls have no Cartesian-join guard and
would have absorbed the duplication with no error, unlike the
`data.table::merge()` in `06_finalize_data.Rmd` that's what actually
surfaced this. Fixed by sourcing institution directory data from the
already-era-aware `colleges.rds` (via `resolve_college()`) instead of a
fresh IPEDS pull, and properly collapsing the mobility fan-out. A second,
dependent bug (`06_finalize_data.Rmd` redundantly re-merging `region`,
colliding into `region.x`/`region.y`) was only reachable once this fix let
the join complete, and is also fixed. Verified: `institutional_characteristics.csv`
now has exactly one row per `unitid` (3,510 = 3,510), and a full
`06_finalize_data.R` rerun completes cleanly end to end.

**Not yet done**: `09_plots.Rmd` and `10_roi.Rmd` haven't been re-run against
the fixed `institutional_characteristics.csv` — they're the scripts whose
figures/tables could actually change. Working tree clean, all committed and
pushed to `origin/master`.

## Next steps
- [ ] Re-run `09_plots.Rmd` (and `10_roi.Rmd`, separately — it rebuilds its
      own `institutional_characteristics` from a different, already-flagged
      year=2021-only pull) against the fixed data, and compare any figures
      that use institution-level covariates (`inst_group`, mobility
      measures, degree mix, `region`, acceptance rate) against the
      originally-submitted versions
- [ ] Phase C: retire `Code/new/` as a folder structure
- [ ] Phase D: cross-cutting hygiene (`CENSUS_KEY` case mismatches, move
      secrets to `.env`, output-dir creation)
- [ ] Phase E: `renv` pinning
- [ ] Phase F: final re-verification, fresh `HANDOFF.md`, unblock Group 1
      in `purrfect-mapping-acorn.md`

## Key files
- `D:\Users\martensn\.claude\plans\yes-let-s-resolve-this-misty-kahan.md` —
  live master plan, current through Phase B's close and the
  `institutional_characteristics.csv` fix
- `Code/college_lookup.R` — shared era-aware institution-resolution helper,
  now also used by `02_col_chars.Rmd`
- `Code/Old/d_sample_construction.R` — retired, see `Code/Old/README.md` for
  why and what it used to feed
- `Data/intermediate/microdata.csv` — regenerated, verified duplicate-free
- `Data/intermediate/institutional_characteristics.csv` — regenerated,
  verified one row per `unitid`
- `Data/intermediate/api_cache/raw_institutions.rds` — now dead/unused (the
  code path that read it was replaced); left on disk, deletion was blocked
  by a permission classifier and isn't essential

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
- `09_plots.Rmd`'s `specification` toggle is intentional design, not a bug —
  dropped from Phase B scope, `CODEBASE_AUDIT.md` corrected
- Institution directory data in `02_col_chars.Rmd` now sourced from
  `colleges.rds` (most-recent-era fallback) rather than a fresh IPEDS pull;
  mobility/super_opeid rows collapsed by summing counts and weighted-
  averaging rates
