# Handoff — BRAIN_DRAIN

## Status
Phase 1 (embedding-based HS/college crosswalk) and Phase 2 (25-year parquet
position-data rewiring) are landed and verified. Phase A.3b (two newly-found
bugs — single-year-2021 IPEDS pulls silently dropping 247 real institutions;
`04_col_hs_construct.R` leaving `ba_school` NA despite a valid resolved
`ba_unitid`) are fixed and confirmed end-to-end. Full pipeline reruns clean:
`microdata.csv` = 3,593,431 rows (pre-Phase-1 baseline was 3,097,275, +15.9%).
Working tree has uncommitted changes; no commits made yet this session.

## Next steps
- [ ] Commit this session's changes (`Code/college_lookup.R`, the
      `04_col_hs_construct.R` backfill, `04_li_ed_pos.Rmd`/`05_merge.Rmd`
      IPEDS-pull fixes, regenerated `Code/scripts/*.R`) plus the still-open
      7 files from Phase A.1
- [ ] Phase A.3: produce the formal before/after diff (3,097,275 →
      3,593,431) and apply the tolerance-fork decision
- [ ] Phase A.4: tag `phase1-crosswalk-consolidated`, merge
      `phase1-crosswalk-consolidation` → `master`, resolve
      `d_sample_construction.R`'s fate
- [ ] Phase B: fix the 5 submission-relevant bugs in `CODEBASE_AUDIT.md` §0

## Key files
- `D:\Users\martensn\.claude\plans\yes-let-s-resolve-this-misty-kahan.md` —
  live master plan, current through Phase A.3b
- `Code/college_lookup.R` — new shared era-aware institution-resolution helper
- `Code/new/04_col_hs_construct.R`, `Code/04_li_ed_pos.Rmd`,
  `Code/05_merge.Rmd` — this session's fixes
- `Data/intermediate/both_final.rds`, `Data/intermediate/microdata.csv` —
  regenerated outputs

## Decisions
- `unitid` alone is not a unique institution key (354 span multiple
  opeid-eras) — `col_match`'s `(unitid, opeid)` join stays as-is; new lookups
  key on `unitid` + reference year via `resolve_college()`
- Widening `00_alias_generation.R`'s 2000-2013 institution row-universe is
  deferred as its own task (touches the already-verified crosswalk)
