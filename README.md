# BRAIN_DRAIN

Labor migration / "brain drain" of college graduates -- college and high-school
characteristics, migration patterns, and wage returns.

## Layout

`Code/` (this repo) lives locally; `Data/` and `Outputs/` live on the shared
`P:\BRAIN_DRAIN` drive (too large -- ~1TB -- to track in git or keep alongside
the code). Every script finds them via `BRAIN_DRAIN_ROOT` in `.env`.

## First-time setup on a new machine

1. Copy `.env.example` to `.env` and fill in:
   - `OPENAI_API_KEY`, `CENSUS_KEY` -- your own keys
   - `BRAIN_DRAIN_ROOT` -- the path to wherever `Data/`/`Outputs/` live on
     *this* machine (forward slashes, e.g. `P:/BRAIN_DRAIN`)
2. Install `dotenv` and `here` if not already present: `install.packages(c("dotenv","here"))`
3. Open `BRAIN_DRAIN.Rproj` in RStudio (or otherwise `cd` into this repo root
   before running scripts) so `here::here()` can find `.env`.

Every in-scope script starts with:
```r
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
```
A few scripts (`03_dedup_col.R`, `cohort.R`, `e_scrape.R`, `intercensal.R`)
don't run this themselves -- they rely on another script having run first in
the same R session (there's no `source()` anywhere in this codebase; it's
meant to be run as one long session, numbered scripts roughly in order).

## Data/ taxonomy

```
Data/
  raw/
    revelio/              Revelio Labs resume/education/employment panels
    ipeds/                IPEDS survey exports + master crosswalks
    urban_institute/       Urban Institute Education Data Portal
    nces/                  NCES ELSI exports, CCD school files, Digest tables
    census_acs/            Census ACS detailed tables
    census_geo/            Census/OMB geography crosswalks (CBSA, county, zip)
    cbp/                   Census County Business Patterns (+ EFSY panel)
    chetty_oi/             Opportunity Insights / Chetty mobility data
    bls/                   BLS unemployment, SOC crosswalk, occupational prestige
    rankings_membership/   Barron's rankings, AAU membership
    other/                 Grawe projections, and a user-created public
                           flagship-institution list (pf.xlsx)
  intermediate/            Pipeline-built, still read as input by a later stage
                           (entity-resolution/dedup family, merged panels, etc.)
  results/                 Pipeline-built, terminal -- feeds only Outputs/
  pooled/ pre_2000/ post_2000/ great_recession/
                           Subsample-specific pipeline re-runs (unchanged
                           structure -- some shared filenames across these are
                           byte-identical by design, not duplicates)
```

Reorganized 2026-07 from a numbered pipeline-stage scheme (`Data/01`-`07`)
that didn't map 1:1 to script numbers. Full migration provenance (file-by-file
classification, path rewrite log, verification reports) is in `migration/`.

## Code/Old/

Legacy scripts, not touched by the 2026 reorg -- paths in there are stale.
Retained for reference only, not guaranteed to run.

## Known issues

- `Data/intermediate/hs_alias_resp.rds` is truncated (pre-existing, confirmed
  not caused by the reorg -- needs to be regenerated or restored from backup).
