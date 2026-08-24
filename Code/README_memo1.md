# Memo 1: reweighting pipeline

Scripts for Memo 1 (Revelio-vs-ACS migration/metro-concentration reweighting,
for both the HS-discloser sample and, as of 2026-08-23, the full college-
graduate sample) are prefixed `memo1_` and numbered in run order, following
this repo's existing `00_`/`01a_` convention (see the root `README.md`).
This is a sub-pipeline within the larger project: it starts from
`column1_population.rds` and `column2_regression_refresh.rds` (outputs of
the main, separately-numbered `00`-`09` pipeline) and does not touch or
depend on anything past that point — confirmed directly: `run_pipeline.R`
contains zero references to any `memo1_*` script.

Full narrative history — every bug found, every dead end, every number
reported — is in the root `HANDOFF.md`, organized by date (current only
through 2026-08-21; treat later dates' history as living in git log +
this file). This file is just the map: what runs, in what order, producing
what. The companion methodology write-up, `MEMO1_WEIGHTING.md` (repo
root), explains *why* the pipeline is built this way, with the actual math.

**2026-08-24: consolidated from ~35 scripts down to 14 numbered stages
(+2 shared utilities), per Nicholas's explicit request** — his own early
pipeline (`00`-`09` above) covers a much larger scope in about the same
number of files, and this sub-pipeline had accumulated many small,
single-purpose scripts over three weeks of iterative work. Each numbered
stage below that used to be several files is now one file with internal
`## SECTION N: ...` headers, one per original script, run top-to-bottom in
the same order they always ran in — no logic changed in the process (only
duplicated boilerplate — repeated `library()` calls, repeated
`data_dir`/`log_step` setup — was collapsed to one copy per file). Every
old constituent script is preserved, not deleted, in `Old/` (see that
folder's own section below).

**No `source()` between files, same as the rest of this repo** — each
script is standalone and re-runs its own dependency checks (skips
already-built checkpoints where cheap to do so). Two deliberate exceptions,
both `source()`'d by every script downstream that needs them:
- `memo1_00_metro_tier_definitions.R` — classifies a CBSA into a metro
  tier under any of several schemes (`rank5` / `rank3` / `rank3_region`).
  **To try a different MSA/metro-tier grouping**: add a new entry to this
  file's `METRO_TIER_SCHEMES` registry and pass its name to
  `build_cbsa_tier_lookup()` — every downstream script takes the scheme
  name as an argument, so nothing else needs to change.
- `memo1_ipf.R` — the iterative-proportional-fitting raking loop
  (`manual_ipf()`) that produces every weight in this pipeline, Stage 1 and
  Stage 2 alike, for both Column 1 and Column 2. Centralized 2026-08-23
  after the same function body had been independently copy-pasted into 9
  different scripts. **To add a future Stage 2 margin** (e.g. industry):
  build an ACS-side population margin table keyed the same way as the
  existing geography/occupation margins, build a matching Revelio-side
  person-year panel column, and add one more `list(keys=..., pop=...)`
  entry to the `margins=` argument of the `manual_ipf()` call — see
  `memo1_07_reweight_column2_occupation.R`'s PART 4 for the worked
  2-margin example this pattern already follows.

Two production Stage 2 weights exist, one per population: `w2_occ`
(Column 2 / HS-discloser, `Data/results/memo1_w2_occupation_calibrated_by_year.rds`)
and its Column 1 analog (`Data/results/memo1_column1_w2_occupation_calibrated_by_year.rds`).
Both are geography-and-occupation-calibrated jointly per calendar year; both
are keyed `(user_id, calendar_year)`; both are the "reweighted" line
everywhere they're plotted or tabulated.

## Run order

`memo1_01a` was deliberately kept standalone rather than folded into the
`01` merge — it's the single largest, most heavily checkpointed script in
the pipeline (per-chunk checkpointing that survived a mid-run reboot, up to
~9 hours worst-case), the same reasoning this cleanup already applies to
the untouched core `00`-`09` pipeline: don't add risk to something that
expensive for a purely organizational benefit.

| # | Script | Produces | Depends on |
|---|--------|----------|------------|
| 00 | `memo1_00_metro_tier_definitions.R` | *(shared definitions, `source()`'d — not run standalone)* | — |
| 01a | `memo1_01a_column1_construct.R` | `column1_population.rds` and intermediate checkpoints | main pipeline's `01d_col_hs_construct.R` / `06_finalize_data.Rmd` |
| 01b | `memo1_01b_covariates.R` (Section 1: Column 1 demo; Section 2: covariates for both columns) | `column1_demo.rds`, `column1_covariates.rds`, `column2_covariates.rds` | 01a, `column2_regression_refresh.rds`, and (via its own output file, not `source()`) `demographics.R`'s `both_demo.rds` — see "Older, unnumbered dependencies" below |
| 02 | `memo1_02_acs_pulls.R` (Section 1: 5yr pull; Section 2: 1yr multi-year pull; Section 3: 1yr supplements) | `pums_cells.rds`, `pums_acs5_filt.rds`, `pums_1yr_filt.rds`, `pums_1yr_race2015.rds`, `pums_1yr_occp2015.rds`, `pums_1yr_occp_allyears.rds` | — (live Census API pulls) |
| 03 | `memo1_03_geo_crosswalks.R` (Section 1: PUMA; Section 2: MIGPUMA) | `puma_cbsa_tier_crosswalk_<vintage>_<scheme>.rds`, `migpuma_cbsa_tier_crosswalk_<vintage>_<scheme>.rds` | 00, 02 (+ live Census tract/decennial pulls) |
| 04 | `memo1_04_occupation.R` (Section 1: crosswalk build; Section 2: 2015 crosstab diagnostic) | `census_occp_2010_to_soc_major_group.rds`, `census_occp_2018_to_soc_major_group.rds`, `memo1_occupation_crosstab_full_simplified.csv` | 01b, 02, 10 (Section 2 feeds 10's Section 4) |
| 05 | `memo1_05_reweight_column2.R` | `column2_reweighted.rds` (`w_full_joint`, Column 2's Stage-1 demographic weight) + `memo1_characteristics_table.csv` | 01b, 02 |
| 06 | `memo1_06_column1_reweight.R` (Section 1: Stage 1; Section 2: Stage 2) | `column1_reweighted.rds`, `memo1_column1_w2_occupation_calibrated_by_year.rds` — **Column 1's own two-stage weight, new 2026-08-23; Column 1 had zero weighting before this** | 01b, 02, 03, 04 |
| 07 | `memo1_07_reweight_column2_occupation.R` | `memo1_w2_occupation_calibrated_by_year.rds` — **Column 2's production Stage-2 weight** | 05, 02, 03, 04 |
| 08 | `memo1_08_calibration_charts.R` (Section 1: migration profile; Section 2: metro tiers; Section 3: calibration lines; Section 4: scheme comparison) | `memo1_migration_rate_by_calendar_year.csv`, `memo1_metro_tier_by_calendar_year.csv`, `..._calibrated.csv`, `memo1_scheme_gap_summary.csv` | 01b, 02, 03, 05 |
| 09 | `memo1_09_cohort_variants.R` (Sections 1-5: inputs, calendar charts, scheme comparison, demo table, occupation reweight — all per birth cohort) | Cohort-restricted (`born_1980s`/`born_1990s`) versions of most of 05's/07's/08's outputs — Stage-1 weight **rebuilt from scratch per cohort**, not a filtered slice | 00, 01b, 02, 03 |
| 10 | `memo1_10_full_sample_extras.R` (Section 1: full-sample tier/demo; Section 2: occupation calibration verify; Section 3: occupation crosstab table draft; Section 4: final occupation memo table; Section 5: memo chart-data exports) | Full-sample (not cohort-restricted) metro-tier share + demo cross-tab, `MEMO1_WEIGHTING.md` SS6.5 | 00, 02, 04, 05, 07, 09. **Real sequential dependency**: Section 4 reads Section 3's own output CSV — they must stay in that order |
| 11 | `memo1_11_final_outputs.R` (Section 1: Stage-1-only comparison line; Section 2: final plots; Section 3: final tables) | Final 4-line/6-line comparison figures and tables for the memo | 05, 07, 08. **Real dependency, not just alphabetical**: Section 1 (originally the "c" script) must run before Sections 2-3 — both read files it writes; the earlier alphabetical numbering had this backwards, fixed by construction now that it's one script |
| 12 | `memo1_12_origin_destination_hsdiscloser.R` (Section 1: build; Section 2: Box export) | `college_origin_destination_counts_by_year.csv` — college × origin(HS) × destination × year × race × sex, Column 2 only; Box copies `..._2012_2023.rds` / `..._named.rds` | 05, 07 |
| 13 | `memo1_13_origin_destination_fullsample.R` | `college_origin_destination_counts_by_year_fullsample.{csv,rds}` — same grain, Column 1, `origin_cbsa` always NA (no HS data); Box copy `..._fullsample_2023.rds` (2023 only — the full file is too large for Box) | 06 |

Output **data** filenames (in `Data/intermediate/` and `Data/results/`) are
stable and do NOT follow the script numbering above — e.g.
`college_origin_destination_counts_by_year.csv` is still named that,
regardless of which script builds it. Only the `.R` script files were
renumbered/consolidated; nothing downstream needed to change.

## Older, unnumbered dependencies

Three scripts predate the Memo 1 numbering entirely and were never folded
into it, but their **output files** (not `source()`) are still real,
load-bearing inputs — confirmed directly by grepping for what reads their
output filenames, not assumed from their age:
- `college_lookup.R` — `resolve_college()`, used by 01b and by the main
  `00`-`09` pipeline's `02_col_chars`/`04_li_ed_pos`/`05_merge`.
- `demographics.R` — produces `both_demo.rds`, read by 01b for Column 2's
  race/sex probabilities.
- `intercensal.R` — produces `intercensal.csv`, read by the main pipeline's
  `09_plots.Rmd`.

None of these were touched or renumbered — the first is genuinely shared
with the untouchable core pipeline, and moving any of the three would
break a real dependency for no benefit.

## `Old/`

One consolidated archive for everything superseded, exploratory, or
dead-ended — both the pre-2026 (2023-2025) JOLE-era dump this folder
originally held, and the Memo 1-era files retired during the 2026-08-23/24
cleanup (merged in from a short-lived separate `Archive/` folder once it
was clear one archive location is clearer than two). `Old/README.md` has
the full per-file rationale for both eras, verified via grep for
`source()`/output-file consumers before anything was moved — nothing here
was archived on the strength of "looks old" alone.

## `memo1_alternative_specs/`

Every weighting/calibration approach that was tried, diagnosed, and set
aside — kept runnable, not deleted, so the "why not X" answer in
`MEMO1_WEIGHTING.md`'s alternative-specifications section is backed by
actual code rather than just prose:

- `irs_soi_desttier_margin.R` — a destination-tier margin built from IRS
  SOI county-to-county migration flows instead of ACS; underperformed
  because SOI covers all tax filers, not just college graduates.
- `reweight_column2_desttier_test.R` — the reweighting test harness that
  used the above margin.
- `phase_a_year_calibration_test.R` — unconditional (non-origin-crossed)
  destination-tier calibration by calendar year; the first test of
  year-varying calibration, superseded by the origin-destination flow
  version once that proved out.
- `phase_b_flow_calibration_test.R` — the first working version of the
  origin-destination flow calibration, on the original 5-tier scheme,
  before the size×region scheme and the multi-cohort/multi-vintage
  extensions.

## Naming note

Some of these scripts still say "Phase A" / "Phase B" internally (their
own code and comments predate the current naming) — `MEMO1_WEIGHTING.md`
uses plain language throughout ("tier-share calibration", "flow
calibration") and does not use that terminology. Read Phase A as the
tier-share-only calibration and Phase B as the origin-destination flow
calibration if you see it in a script or in `HANDOFF.md`.
