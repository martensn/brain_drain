# Handoff — BRAIN_DRAIN

## Status
**Phases A, B, C, and D are all closed.**

**Phase A**: Phase 1 (crosswalk) + Phase 2 (parquet position rewiring) landed and verified. `microdata.csv` = 3,603,589 rows, zero duplicate `user_id`s (pre-Phase-1 baseline 3,097,275, +16.3%, inside tolerance per safeguard 5). Tagged `phase1-crosswalk-consolidated`, merged to `master`.

**Phase B**: all 5 catalogued bugs resolved. Verifying them surfaced a much bigger, previously-uncatalogued bug: `institutional_characteristics.csv` had duplicate rows for 95% of its institutions (up to 20x), from a long-standing (pre-this-summer) uncollapsed multi-year IPEDS pull + uncollapsed Chetty mobility merge in `02_col_chars.Rmd`. **May have silently affected the originally-submitted paper's figures/tables** (`09_plots.Rmd`'s `dplyr::left_join()` has no Cartesian-join guard). Fixed via `colleges.rds`/`resolve_college()` instead of a fresh IPEDS pull. Verified: one row per `unitid`. **Deliberately deferred, not forgotten**: re-running `09_plots.Rmd`/`10_roi.Rmd` against the fix and diffing against the submitted figures.

**Phase C**: `Code/new/` no longer exists. Four crosswalk scripts folded into the root pipeline as `Code/01a_alias_generation.R` .. `Code/01d_col_hs_construct.R`. Investigating rather than assuming surfaced two more landmines: `demo.R` and `a_hs_col_construct.R` were old drafts silently writing to paths the *live* scripts now own (`unmatched_col.rds`; `colleges.rds`/etc.) — archived along with feeders and three other confirmed-dead/broken files.

**Phase D**: cross-cutting hygiene. Fixed the `CENSUS_KEY`/`census_key` case mismatch (2 live sites — both re-called `Sys.getenv()` with the wrong case instead of using the already-correct local variable a few lines above). Confirmed the OpenAI-key-into-`.env` item was already done pre-session (stale audit finding). Moved `RETICULATE_PYTHON` out of a hardcoded path in `01b_embed_new.R` into `.env`, and fixed load ordering (`.env` used to load *after* the hardcoded value was already in use) — verified `reticulate` still binds correctly. Added `dir.create()` for `Data/<specification>`/`Outputs/<specification>` at the top of the four scripts that write dozens of files into them without ever creating the directory.

**Not yet done**: a full pipeline re-verification run (covers Phase C's move and Phase D's changes together — neither changed core logic, both verified narrowly already, but an honest end-to-end confirmation is still outstanding). Held off given cost (the crosswalk scripts are the multi-hour embedding/matching stage). This is Phase F's job.

Working tree clean, all committed and pushed to `origin/master`.

## Next steps
- [ ] **Phase E** (active now): `renv` pinning — `renv::init()` + `renv::snapshot()` against a curated package list, once the `eval=FALSE`/commented-code inclusion question is decided
- [ ] Phase F: final full pipeline re-verification (covers Phases C and D's changes plus the original Phase 1/2 verification), fresh `HANDOFF.md`, unblock Group 1 in `purrfect-mapping-acorn.md`
- [ ] **(Deferred, revisit deliberately)** Re-run `09_plots.Rmd`/`10_roi.Rmd` against the fixed `institutional_characteristics.csv` and compare against the originally-submitted figures

## Key files
- `D:\Users\martensn\.claude\plans\yes-let-s-resolve-this-misty-kahan.md` — live master plan, current through Phase D's close
- `Code/01a_alias_generation.R` .. `Code/01d_col_hs_construct.R` — the crosswalk-construction sub-sequence, folded in from `Code/new/`
- `Code/college_lookup.R` — shared era-aware institution-resolution helper
- `Code/Old/README.md` — up to date with every 2026-07-29 archival decision and why
- `.env` / `.env.example` — now also holds `RETICULATE_PYTHON`
- `Data/intermediate/microdata.csv` — regenerated, verified duplicate-free
- `Data/intermediate/institutional_characteristics.csv` — regenerated, verified one row per `unitid`

## Decisions
- `unitid` alone isn't a unique institution key — lookups key on `unitid` + reference year via `resolve_college()`
- Tolerance-fork (safeguard 5): +16.3% is inside tolerance, treated as a like-for-like update, not a fork trigger
- `09_plots.Rmd`'s `specification` toggle is intentional design, not a bug
- Institution directory data in `02_col_chars.Rmd` now sourced from `colleges.rds` rather than a fresh IPEDS pull
- Phase C's crosswalk scripts numbered as `01a-01d` (minimal-diff sub-sequence) rather than a full pipeline renumber
- `06_census.R`, `10_roi.Rmd`, `demo.R`/`03_dedup_col.R`, `e_scrape.R`/`a_hs_col_construct.R`, `09b_plots_old.Rmd`: archived to `Code/Old/` rather than fixed — each confirmed either genuinely broken, superseded, or actively dangerous before archiving, not assumed
- `CODEBASE_AUDIT.md`'s "3 locations" for the `CENSUS_KEY` case mismatch was likely a double-count (source `.Rmd` + auto-generated `.R` copy); only 2 real live sites found after a thorough sweep
- The OpenAI-key-into-`.env` audit item was already resolved before this session (stale finding, not verified against current code)
