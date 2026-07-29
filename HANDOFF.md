# Handoff — BRAIN_DRAIN

## Status
**Phases A through E are all closed. Only Phase F (final re-verification) remains.**

**Phase A**: Phase 1 (crosswalk) + Phase 2 (parquet position rewiring) landed and verified. `microdata.csv` = 3,603,589 rows, zero duplicate `user_id`s (+16.3% vs. pre-Phase-1 baseline, inside tolerance). Tagged `phase1-crosswalk-consolidated`, merged to `master`.

**Phase B**: all 5 catalogued bugs resolved. Verifying them surfaced a much bigger, previously-uncatalogued bug: `institutional_characteristics.csv` had duplicate rows for 95% of its institutions, from a long-standing (pre-this-summer) issue in `02_col_chars.Rmd`. **May have silently affected the originally-submitted paper's figures/tables.** Fixed via `colleges.rds`/`resolve_college()`. **Deliberately deferred, not forgotten**: re-running `09_plots.Rmd`/`10_roi.Rmd` against the fix and diffing against the submitted figures.

**Phase C**: `Code/new/` no longer exists. Four crosswalk scripts folded into the root pipeline as `Code/01a_alias_generation.R` .. `Code/01d_col_hs_construct.R`. Investigating rather than assuming surfaced two more landmines (`demo.R`, `a_hs_col_construct.R` silently writing to paths the *live* scripts now own) — archived along with three other confirmed-dead/broken files.

**Phase D**: cross-cutting hygiene — `CENSUS_KEY` case-mismatch fix, `RETICULATE_PYTHON` moved into `.env` (plus a load-ordering fix), `dir.create()` added for the four scripts that assumed `Data/<specification>/`/`Outputs/<specification>/` already existed. The OpenAI-key audit item turned out already resolved pre-session.

**Phase E**: environment pinned with `renv`. Curated 47-package manifest (renv's auto-detected 44 + `estimatr`/`progressr`/`zipcodeR`, per your explicit call to include packages referenced only via commented-out `#library(x)` lines — empirically confirmed this codebase has zero *actual* eval=FALSE-chunk-only packages, so that was the real gap, not the eval=FALSE case itself). `blsAPI` excluded — confirmed CRAN removed it entirely, and its only reference is one dead comment. 202 total packages pinned in `renv.lock`, R 4.6.0 recorded. Verified: `06_finalize_data.R` runs cleanly under the renv-activated session. Bonus: fixed a genuine syntax error in `Code/kappa.R` that `renv::dependencies()`'s parse step surfaced.

**Not yet done, carried into Phase F**:
1. A full pipeline re-verification run (covers Phase C's script move and Phase D's changes together — neither changed core logic and both were verified narrowly, but an honest end-to-end confirmation is still outstanding).
2. Re-running `09_plots.Rmd`/`10_roi.Rmd` against the fixed `institutional_characteristics.csv` and comparing to the originally-submitted figures (Phase B's deferred item).

Working tree clean, all committed and pushed to `origin/master`.

## Next steps
- [ ] **Phase F** (active now): full pipeline re-verification end-to-end, covering everything above; fresh `HANDOFF.md` once done; unblock Group 1 in `purrfect-mapping-acorn.md` (item 1 skeleton pass, SCI pilot, item 4, reweighting, advisor outreach — all currently gated on this resolution being fully merged/closed)
- [ ] Within Phase F: the `09_plots.Rmd`/`10_roi.Rmd` diff against submitted figures — the one item with real research-integrity stakes (did the institutional_characteristics bug reach anything actually submitted to JOLE)

## Key files
- `D:\Users\martensn\.claude\plans\yes-let-s-resolve-this-misty-kahan.md` — live master plan, current through Phase E's close
- `Code/01a_alias_generation.R` .. `Code/01d_col_hs_construct.R` — the crosswalk-construction sub-sequence, folded in from `Code/new/`
- `Code/college_lookup.R` — shared era-aware institution-resolution helper
- `Code/Old/README.md` — up to date with every 2026-07-29 archival decision and why
- `renv.lock` / `renv/` — pinned environment, R 4.6.0, 202 packages
- `.env` / `.env.example` — now also holds `RETICULATE_PYTHON`
- `Data/intermediate/microdata.csv` — regenerated, verified duplicate-free
- `Data/intermediate/institutional_characteristics.csv` — regenerated, verified one row per `unitid`

## Decisions
- `unitid` alone isn't a unique institution key — lookups key on `unitid` + reference year via `resolve_college()`
- Tolerance-fork (safeguard 5): +16.3% is inside tolerance
- `09_plots.Rmd`'s `specification` toggle is intentional design, not a bug
- Institution directory data in `02_col_chars.Rmd` now sourced from `colleges.rds` rather than a fresh IPEDS pull
- Phase C's crosswalk scripts numbered as `01a-01d` (minimal-diff sub-sequence) rather than a full pipeline renumber
- `06_census.R`, `10_roi.Rmd`, `demo.R`/`03_dedup_col.R`, `e_scrape.R`/`a_hs_col_construct.R`, `09b_plots_old.Rmd`: archived to `Code/Old/` — each confirmed genuinely broken, superseded, or actively dangerous before archiving
- `CODEBASE_AUDIT.md`'s "3 locations" for the `CENSUS_KEY` case mismatch was likely a double-count; only 2 real live sites found
- `renv`'s manifest deliberately excludes `Code/Old/`'s dependencies and `blsAPI` (CRAN-removed, dead reference only) — `renv::status()` will flag these as "inconsistent" going forward, by design, not a bug
