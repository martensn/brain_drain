# Handoff — BRAIN_DRAIN

## Status
**The full codebase-consolidation resolution plan (Phases A-F) is closed.** `master` reflects all of it, pushed to `origin`. Group 1 in the research plan (`purrfect-mapping-acorn.md`: item 1 skeleton pass, SCI pilot, item 4, reweighting, advisor outreach) is now unblocked.

**Phase A**: Phase 1 (crosswalk) + Phase 2 (position rewiring) landed and verified. `microdata.csv` = 3,603,589 rows (+16.3% vs. pre-Phase-1 baseline, inside tolerance).

**Phase B**: 5 catalogued bugs resolved. Verifying them surfaced a much bigger, pre-existing bug: `institutional_characteristics.csv` had duplicate rows for 95% of institutions. **May have silently affected the originally-submitted (JOLE-rejected) paper's figures/tables** — deliberately not chased further (moot given the rejection, per your call).

**Phase C**: `Code/new/` retired. Four crosswalk scripts folded into the root pipeline as `Code/01a_alias_generation.R` .. `Code/01d_col_hs_construct.R`. Two more landmines found and archived along the way (`demo.R`, `a_hs_col_construct.R` silently writing to paths the live scripts now own).

**Phase D**: cross-cutting hygiene — `CENSUS_KEY` case fix, `RETICULATE_PYTHON` moved into `.env`, output-directory creation added.

**Phase E**: environment pinned with `renv` — 47-package curated manifest, 202 total incl. transitive deps, R 4.6.0. `blsAPI` excluded (CRAN removed it).

**Phase F**: scoped re-verification (`06_finalize_data.R → 07_regressions.R → 08_data_generation.R → 09_plots.R`, run together for the first time this session) surfaced **four more real bugs**:
- `02_col_chars.Rmd`: Phase B's `colleges.rds` fix silently zeroed out the entire "RPU" institution category (an encoding mismatch — old API's 1/2 land_grant coding vs. `colleges.rds`'s 0/1). Corrupted a real analysis variable, not just a crash. Verified fix against known land-grant flagships.
- `08_data_generation.Rmd`: inherited a `geos` variable narrowed by `07_regressions.Rmd`, so state-level output files were never generated. Reset explicitly.
- `09_plots.Rmd`: three stale reads assumed a stray CSV column two source files no longer have (fixed); a phantom `num_bins=25` value nothing upstream produces (corrected); `raw_lm`'s construction commented out with live downstream usage (restored).

06/07/08 now run cleanly end-to-end. `09_plots.Rmd` verified clean through these fixes (isolated via a temporary, uncommitted `specification` override, not a real code change). **Deliberately stopped** at a second instance of the same bug pattern (`col_grad_micro`, a live 51-state Census PUMS pull) — per your call, `09_plots.Rmd` isn't worth fully debugging right now given the rejection and expected rewrite. Logged as a known open issue.

Working tree clean, all committed and pushed to `origin/master`.

## Known open issues (not fixed, deliberately)
- `09_plots.Rmd`: `col_grad_micro` (~line 2570) and likely more instances of the same "commented-out expensive step, live downstream usage" pattern in the file's remaining ~800 lines. Fix when this file gets its expected rewrite, not before.
- `09_plots.Rmd`/`10_roi.Rmd` were never re-run against the fixed `institutional_characteristics.csv` to check whether the Phase B duplicate-row bug reached the submitted paper's actual figures — deprioritized as moot (paper already rejected).
- `Data/pre_2000/` (and other non-`great_recession` specification directories) lack complete state-level source data — a data-population gap distinct from any code bug; would need its own `06→07→08` run under that specification if ever needed.

## Key files
- `D:\Users\martensn\.claude\plans\yes-let-s-resolve-this-misty-kahan.md` — the (now closed) resolution plan, full detail on every phase
- `Code/01a_alias_generation.R` .. `Code/01d_col_hs_construct.R` — the crosswalk-construction sub-sequence
- `Code/college_lookup.R` — shared era-aware institution-resolution helper
- `Code/Old/README.md` — every 2026-07-29 archival decision and why
- `renv.lock` / `renv/` — pinned environment, R 4.6.0, 202 packages
- `.env` / `.env.example` — holds `RETICULATE_PYTHON` now too
- `Data/intermediate/microdata.csv` — verified duplicate-free
- `Data/intermediate/institutional_characteristics.csv` — verified duplicate-free, RPU classification corrected

## Decisions
- `unitid` alone isn't a unique institution key — lookups key on `unitid` + reference year via `resolve_college()`
- `09_plots.Rmd`'s `specification` toggle is intentional design, not a bug
- Institution directory data in `02_col_chars.Rmd` sourced from `colleges.rds` rather than a fresh IPEDS pull; `inst_group`'s `land_grant` thresholds corrected to match its 0/1 encoding
- `06_census.R`, `10_roi.Rmd`, `demo.R`/`03_dedup_col.R`, `e_scrape.R`/`a_hs_col_construct.R`, `09b_plots_old.Rmd`: archived to `Code/Old/` — each confirmed genuinely broken, superseded, or actively dangerous before archiving
- `renv`'s manifest deliberately excludes `Code/Old/`'s dependencies and `blsAPI` (CRAN-removed)
- `09_plots.Rmd`/`10_roi.Rmd` submitted-figure diff and full `col_grad_micro`-pattern debugging both explicitly deprioritized (paper rejected, figure code expected to be rewritten) — not silently dropped, logged above
