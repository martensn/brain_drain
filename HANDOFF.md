# Handoff — BRAIN_DRAIN

## Status
Phase 1 (crosswalk), Phase 2 (parquet position rewiring), and Phase A.3b
(institution-lookup fixes) are landed and verified. Phase A.3's before/after
diff is done — waterfall + current-sample plausibility checks (a true
row-level pre-Phase-1 baseline was never preserved, so this was a deliberate
scope choice). Producing the diff surfaced and fixed 4 more bugs: a duplicate
row in `unified_cbsa`, two missing `row.names=FALSE` writes (stray `V1.x`/
`V1.y` columns), a missing `all.x=TRUE` on `05_merge.Rmd`'s `hs_cnty` merge,
and a fan-out bug in `college_lookup.R`'s `resolve_college()` for overlapping
IPEDS-year eras. `microdata.csv` = 3,603,589 rows, zero duplicate `user_id`s,
no stray columns (pre-Phase-1 baseline: 3,097,275, +16.3%). Committed as
`c6543b5`. Working tree clean.

## Next steps
- [x] Apply the tolerance-fork decision (Phase A safeguard 5): **inside
      tolerance, proceed** — the full +16.3% traces to specific verified
      causes (vintage fix, institution-lookup backfill, 4 bug fixes), no
      plausibility red flags
- [ ] Phase A.4: tag `phase1-crosswalk-consolidated`, merge
      `phase1-crosswalk-consolidation` → `master`, resolve
      `d_sample_construction.R`'s fate
- [ ] Phase B: fix the 5 submission-relevant bugs in `CODEBASE_AUDIT.md` §0

## Key files
- `D:\Users\martensn\.claude\plans\yes-let-s-resolve-this-misty-kahan.md` —
  live master plan, current through Phase A.3's diff
- `Code/college_lookup.R` — shared era-aware institution-resolution helper
- `Data/intermediate/microdata.csv` — regenerated, verified duplicate-free

## Decisions
- `unitid` alone isn't a unique institution key — lookups key on `unitid` +
  reference year via `resolve_college()`
- Chose waterfall + plausibility checks over reconstructing a true row-level
  baseline (would mean re-running superseded code against preserved inputs)
- Chugach/Copper River Alaska GeoFIPS mismatch (`00_crosswalks.Rmd:88`)
  noted, left unfixed — confirmed zero rows affected
