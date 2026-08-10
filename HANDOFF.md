# Handoff — BRAIN_DRAIN

## Status
Codebase-consolidation resolution plan (Phases A-F) closed 2026-07-29, pushed to `origin`. Research plan pivoted 2026-08-07 from resubmission to a memo-based wind-down before PhD coursework starts (`purrfect-mapping-acorn.md`) — 2-3 short memos, no resubmission goal, advisor outreach not tracked here. Memo 1 (reweighting the LI sample vs. ACS, isolating the HS-listing sample restriction's effect): Step 0 (real `hs_start`/`col_start`/`col_field`, plus an unrelated `table.express`/`county_fips` bug) done 2026-08-07 (`f885f9a`, `c6e51be`). **Step 1 (define both populations) done 2026-08-10**: Column 1 built (`Data/intermediate/column1_population.rds`, 34,467,516 users, no HS requirement) via new standalone `Code/memo1_column1_construct.R`; Column 2 regenerated end-to-end to reflect Step 0's fix (`Data/intermediate/column2_regression_refresh.rds`, 3,092,892 users). Column 1 is an **11.14x** expansion over Column 2 — the headline number for the memo. Superset check 95.66% clean; the remainder is explained, not a bug (see Decisions). All local commits, including this session's, still **not pushed** to `origin`, by design.

## Next steps
- [ ] "Section 2" for both Column 1 and Column 2: merge in demographics (`Code/05_demographics.R`'s predicted race/sex — not wired into either column, or into `run_pipeline.R` at all) and richer education history (associate/transfer/graduate-degree detail — exists in `both_final.rds` for Column 2 already but gets dropped before `regression`; doesn't exist at all yet for Column 1, needs `01d` L702-857's enrichment logic adapted the same way the BA reconciliation was). Enables comparing the samples on race, sex, geography, transfer status, graduate-degree attainment. Detail in `velvet-churning-galaxy.md`'s Step 1 section
- [ ] Extend `Code/acs_reweight.R`: ACS PUMS pull + reweighting (race/sex/age/region/top-10-MSA/major/migration rows) — Memo 1 Step 2
- [ ] Build geography helpers: `hs_region` (via `state.region`), hardcoded top-10 CBSA codes — Step 4
- [ ] Operationalize migration-behavior row(s), assemble the 4-column table, write the memo — Step 4/5
- [ ] `06_finalize_data.R`/`.Rmd` never persists `regression` to disk (found this session) — the July 1 `regression_data__06.csv` was a manual export, not a `run_pipeline.R` artifact. This session's fresh copy (`column2_regression_refresh.rds`) is a one-off; add a real `saveRDS`/`fwrite` to the pipeline before relying on this again
- [ ] Push local commits to `origin` when ready

## Key files
- `D:\Users\martensn\.claude\plans\velvet-churning-galaxy.md` — granular Memo 1 execution plan, step-by-step
- `D:\Users\martensn\.claude\plans\purrfect-mapping-acorn.md` — higher-level research plan
- `D:\Users\martensn\.claude\plans\wild-imagining-kurzweil.md` — Step 1 execution plan (Column 1 restriction map, verification criteria)
- `Code/memo1_column1_construct.R` — Column 1 build: standalone, 4 checkpointed sections, not wired into `run_pipeline.R`. Checkpoints in `Data/intermediate/column1_*.rds`
- `Code/scripts/run_pipeline_column2_refresh.R` — regenerates Column 2 from `01d` onward without re-running `01a`/`01b`/`01c`'s expensive one-time geocoding/embedding calls
- `Code/01d_col_hs_construct.R`, `Code/03_li_ed.Rmd` — Step 0's fixes
- `Code/acs_reweight.R` — starting point for the ACS pull/reweighting

## Decisions
- Column 2 ("actual sample") = `06_finalize_data.Rmd`'s broad `regression` table, not the tighter `regression_cbsa_10`/`state_10`
- `table.express`, once attached earlier in a session, masks `select()`/`mutate()` for data.tables via S3 dispatch — hand data.tables off to `as_tibble()` before plain dplyr chains. Found and fixed two more live instances this session (`05_merge.Rmd`'s `hs_zip`, which feeds `dist`, a hard filter in `06_finalize_data.Rmd`; and `06_finalize_data.Rmd`'s own `inst_group` merge) beyond the original `02_col_chars.Rmd`/`county_fips` case — same bug class, same `as_tibble()` fix.
- Column 1 structurally can't use HS-anchored birth-year imputation (`birth1 = hs_end - 18`, `04_li_ed_pos.Rmd` line 245) — only the cruder `birth2 = col_start - 18`, which assumes immediate HS-to-college enrollment. This systematically undercounts non-traditional-timing students (gap year, delayed enrollment, career-changers) relative to Column 2, and fully explains the superset gap (134,195 of Column 2's 3,092,892 users, 86.4% directly attributable, concentrated among "Non-Traditional" students) — worth a sentence in the memo, not something to fix (fixing it would require HS data, defeating Column 1's purpose).
- Column 1's population (50.45M pre-filter) is ~10x larger than initially assumed, since dropping the HS join removes the pipeline's main narrowing step — full pipeline re-runs from `01d` onward take ~6 hrs at Column 2's scale (skip `01a`/`01b`/`01c`, unaffected by Step 0); Column 1's build is far more expensive still (Section 2's position-matching alone: chunked `user_id %% 50` processing, ~35 chunks/9hrs, survived a mid-run machine reboot via per-chunk checkpointing)
- Remember to re-run `Code/scripts/convert_rmd_to_r.R` after editing a root `.Rmd` — `run_pipeline.R` executes the converted copies, not the `.Rmd`s
