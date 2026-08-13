# Memo 1: reweighting pipeline

Scripts for Memo 1 (Revelio-vs-ACS migration/metro-concentration reweighting)
are prefixed `memo1_` and numbered in run order, following this repo's
existing `00_`/`01a_` convention (see the root `README.md`). This is a
sub-pipeline within the larger project: it starts from `column1_population.rds`
and `column2_regression_refresh.rds` (outputs of the main, separately-numbered
`00`-`09` pipeline) and does not touch or depend on anything past that point.

Full narrative history — every bug found, every dead end, every number
reported — is in the root `HANDOFF.md`, organized by date. This file is
just the map: what runs, in what order, producing what. The companion
methodology write-up, `MEMO1_WEIGHTING.md` (repo root), explains *why* the
pipeline is built this way, with the actual math.

**No `source()` between files, same as the rest of this repo** — each
script is standalone and re-runs its own dependencies check (skips
already-built checkpoints where cheap to do so). The one deliberate
exception is `memo1_00_metro_tier_definitions.R`, `source()`'d by every
script downstream of it that needs to classify a metro area — flagged in
its own header as an intentional exception, for exactly one reason: testing
several metro-area classification schemes side by side only stays cheap if
the classification logic lives in one place.

## Run order

| # | Script | Produces | Depends on |
|---|--------|----------|------------|
| 00 | `memo1_00_metro_tier_definitions.R` | *(shared definitions, `source()`'d — not run standalone)* | — |
| 01a | `memo1_01a_column1_construct.R` | `column1_population.rds` and intermediate checkpoints | main pipeline's `01d_col_hs_construct.R` / `06_finalize_data.Rmd` |
| 01b | `memo1_01b_column1_demo.R` | `column1_demo.rds` (race/sex probabilities for Column 1) | 01a |
| 01c | `memo1_01c_covariates.R` | `column1_covariates.rds`, `column2_covariates.rds` | 01a, 01b, `column2_regression_refresh.rds` |
| 02a | `memo1_02a_acs_pull_5yr.R` | `pums_cells.rds`, `pums_acs5_filt.rds` (Stage-1 raking margins, one fixed 2022 vintage) | — (live Census API pull) |
| 02b | `memo1_02b_acs_pull_1yr.R` | `pums_1yr_filt.rds` (ACS 1-year files, 2008-2023 minus 2020, PUMA/MIGPUMA for 2012-2021 + 2022-2023) | — (live Census API pull) |
| 02c | `memo1_02c_acs_pull_1yr_race2015.R` | `pums_1yr_race2015.rds` (supplementary race/sex for 2015 only) | — (live Census API pull) |
| 03a | `memo1_03a_puma_cbsa_crosswalk.R` | `puma_cbsa_tier_crosswalk_<vintage>_<scheme>.rds` | 00 (+ live Census tract/decennial pulls) |
| 03b | `memo1_03b_migpuma_cbsa_crosswalk.R` | `migpuma_cbsa_tier_crosswalk_<vintage>_<scheme>.rds` | 03a, 02b |
| 04 | `memo1_04_reweight_column2.R` | `column2_reweighted.rds` (`w_full_joint`, Stage-1 demographic weight) — **full sample, all ages** | 01c, 02a |
| 05a | `memo1_05a_migration_profile.R` | `memo1_migration_rate_by_calendar_year.csv` | 01c, 02b, 04 |
| 05b | `memo1_05b_metro_tiers.R` | `memo1_metro_tier_by_calendar_year.csv` | 01c, 02b, 03a, 04 |
| 06a | `memo1_06a_calibration_lines.R` | `..._calibrated.csv` (flow-calibrated lines, 5-tier scheme only) | 04, 05a, 05b |
| 06b | `memo1_06b_scheme_comparison.R` | `memo1_scheme_gap_summary.csv` + scheme-specific calibrated lines (5-tier / 3-tier / size×region) | 03a, 03b, 04, 05a |
| 07a | `memo1_07a_cohort_inputs.R` | `column2_reweighted_<cohort>.rds`, `column1_covariates_<cohort>.rds`, `pums_1yr_filt_<cohort>.rds` — Stage-1 weight **rebuilt from scratch per birth cohort** | 01c, 02a, 02b |
| 07b | `memo1_07b_cohort_calendar_charts.R` | `memo1_migration_rate_by_calendar_year_<cohort>.csv`, `memo1_metro_tier_by_calendar_year_<cohort>.csv` | 00, 03a, 07a |
| 07c | `memo1_07c_cohort_scheme_comparison.R` | Per-cohort `memo1_scheme_gap_summary_<cohort>.csv`, calibrated lines, `phase_ratios_<cohort>_<scheme>.rds` | 00, 03a, 03b, 07a, 07b |
| 07d | `memo1_07d_cohort_demo_table.R` | `memo1_demo_crosstab_<cohort>.csv` (race/sex/region by weighting scheme, fixed year) | 00, 02a, 02c, 07a, 07c |

Chart assembly (turning the CSVs above into the published HTML artifacts) is
scratch tooling, not part of this numbered pipeline — see `HANDOFF.md`'s
2026-08-12/13 entries for how those were built.

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
