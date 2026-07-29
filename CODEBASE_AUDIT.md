# Codebase Audit — Dependencies, Bugs, and Environment Standardization

*Compiled 2026-07-22. Read-only audit (no code changed as part of this pass, except where noted). Covers three coexisting lineages: the root numbered pipeline (`00_crosswalks.Rmd`–`10_roi.Rmd`, the JOLE-submission pipeline), the letter-prefixed/standalone scripts (`a`–`e`, `cohort.R`, `yagan.R`, `intercensal.R`, `kappa.R`, `measure_return.R`, `demo.R`, `03_dedup_col.R`), and `Code/new/` (`00`–`07`).*

---

## 0. Read this part first — findings that may bear on submitted/near-submission numbers

Most of what follows is code hygiene. These six are different in kind — they're either currently live in the pipeline behind the JOLE submission, or directly affect a file that feeds a submission figure/table.

1. **`06_finalize_data.Rmd:241-242` — the two "10-year" cohort-return files are not on the same horizon.**
   ```r
   cohort_return_hs  <- regression[ever_leave_cbsa_hs_10  == 1, ...]   # 10-year-bounded
   cohort_return_col <- regression[ever_leave_cbsa_col    == 1, ...]   # NOT bounded — full career
   ```
   `cohort_return_hs_10.csv` and `cohort_return_col_10.csv` are both named as 10-year measures, but the college-side one filters on `ever_leave_cbsa_col` (no `_10` suffix — the unrestricted, full-career version from `05_merge.Rmd`), not `ever_leave_cbsa_col_10`. **`cohort.R` — which you've described as producing a plot used in the journal submission — reads exactly these two files.** Worth checking directly against whatever's in the submitted draft: if the HS and college panels of that figure are supposed to be on the same time horizon, one of them currently isn't.

2. **The same `x * x` typo appears twice, in two different files, computing `return_to_both`:**
   - `08_data_generation.Rmd:488`: `regression[, return_to_both := same_cbsa_col_10 * same_cbsa_col_10]`
   - `kappa.R:16`: `regression[, return_to_both := same_cbsa_col_10 * same_cbsa_col_10]`

   Both almost certainly intend `same_cbsa_hs_10 * same_cbsa_col_10` (returned to *both* HS and college CBSA — a joint indicator). As written, `return_to_both` is just a no-op copy of `same_cbsa_col_10` in both places. In `08_data_generation.Rmd` this feeds `problems` (line 489), which doesn't appear to be exported downstream from what's visible — likely a dead diagnostic rather than a paper number, but worth a direct check. The duplication across two files is itself informative: it suggests one was copy-pasted from the other with the bug intact.

3. **`05_merge.Rmd:560` — `soc_merge.csv` contains the wrong table.**
   ```r
   write.csv(soc_ba, file.path(data_dir,"intermediate/soc_merge.csv"), ...)   # writes soc_ba, not soc_merge
   ```
   The file literally named `soc_merge.csv` holds `soc_ba`'s full column set, not the narrower `soc_merge` object built two lines later. If anything downstream reads `soc_merge.csv` expecting the slim schema, it's silently getting the wide one instead. Worth a repo-wide check for readers of this specific filename.

4. **`01_shocks.Rmd:160` — `shifts.csv` is written before `shifts` is computed in that run.**
   ```r
   write.csv(shifts, ...)      # line 160
   ...
   shifts <- cbp_nat_chg %>% ...   # line 164, one block later
   ```
   On a genuinely fresh run this throws `object 'shifts' not found`. If it "works" today, it's because a stale `shifts` from an earlier interactive run is still sitting in the session — meaning `intermediate/shifts.csv` may reflect an older computation, not the one that just ran. Worth confirming the currently-saved `shifts.csv` actually corresponds to the latest `cbp_nat_chg` logic.

5. ~~**`09_plots.Rmd:41` resets `specification` to `"pre_2000"`, while `06_finalize_data.Rmd`/`07_regressions.Rmd` both use `"great_recession"`.**~~ **[CONFIRMED NOT A BUG — 2026-07-29, author's correction]** Intentional by design: `specification` is meant to be a configurable toggle so the same plotting code can be re-run under multiple cohort windows (`pre_2000`/`great_recession`/`post_2000`/`pooled`) without a separate script per specification. `06_finalize_data.Rmd`/`07_regressions.Rmd` using a different default is expected, not a mismatch to reconcile. Dropped from Phase B scope in the resolution plan.

6. **`Code/measure_return.R` is mislabeled — it's `measure_leave`'s logic under `measure_return`'s name, inverted.** Diffed byte-for-byte against `06_finalize_data.Rmd`'s live `measure_return`/`measure_leave` pair: this standalone file's body matches `measure_leave`, not `measure_return`. It's also missing its own assignment (`measure_return <- function(...)`  never actually happens — the file is just an anonymous function literal) and has no `measure_leave` companion. **Because nothing in this codebase uses `source()`, this file currently has zero effect on the live pipeline** — `06_finalize_data.Rmd` defines and uses its own correct, self-contained versions. No danger today, but it's a landmine: if it's ever copied into an interactive session under the assumption that it's an authoritative definition of `measure_return`, every result built on it would be silently inverted. Recommend deleting it or fixing the label + adding the missing companion.

Also, from this session's own work: **`Code/d_sample_construction.R` has two live bugs right now**, one pre-existing and one introduced while fixing the `hs_id` drop:
- Line ~35: `select(...,string_method)` — but the producing script (`c_col_hs_classification.R`) names this column `method`, not `string_method`. This will error (`column 'string_method' doesn't exist`) the moment it runs. Three lines later, the equivalent HS-side select correctly does `string_method=method` (a rename) — the college-side select is missing the same rename.
- Line ~120: `ed_wide_sample = ed_wide[sample(N,100000)]` — almost certainly means `.N` (data.table's row-count pronoun, used correctly elsewhere in this file family), not bare `N`. No object named `N` exists in scope; this will error unless some unrelated global `N` happens to be lying around, in which case it silently samples the wrong row count.

These two are worth fixing before you next run that script — happy to do it now if you want, just say the word (see the very end of this document for a fix menu).

---

## 1. True dependency graphs (the numbering does not reflect real run order)

A recurring, cross-cutting finding: **because nothing in this codebase calls `source()`, every script assumes specific objects already exist in the R session from having run earlier scripts — and in several places, the *actual* required order contradicts what the filename numbering/lettering implies.**

### 1a. Root pipeline (`00`–`10`)

Numeric order holds reasonably well through `00→08`. One break:
- **`10_roi.Rmd` is effectively disconnected** from a fresh run: it `load()`s a git-ignored, 197MB `Code/06_workspace.RData` snapshot that doesn't exist on a fresh clone (and if it does exist locally, silently overwrites whatever `00`-`09` just built in-session); it reads `results/regression_data__06.csv`, a pre-July-2026-reorg filename nothing in `00`-`09` produces; and it rebuilds `institutional_characteristics` with `year=2021` only, vs. `02_col_chars.Rmd`'s `year=2000:2013` panel — two different objects with the same name and purpose, already drifted.
- `13_9b_plots_old.Rmd` and `Code/03_dedup_col.R` exist alongside this numbering but are explicitly legacy/out-of-lineage (the former by its own `_old` suffix, the latter by a header comment naming a `Code/new/` dependency) — flagging their presence in case they're meant to be deleted rather than confused for part of the active sequence.

Two "resume from cache" patterns exist (`05_merge.Rmd`'s `if(!exists("completed"))` check reading `raw_microdata__05.csv`; `06_finalize_data.Rmd`'s fallback to `microdata_sample__06.csv`) but both are **incomplete** — `05_merge.Rmd` still bare-references `all_hs` (from `02_hs_chars.Rmd`) and `cbsa_shock_ptile` (from `01_shocks.Rmd`) with no file-based fallback, so "resuming" from the microdata cache alone doesn't actually work end-to-end.

Full per-script input/output/dependency detail (packages, every read/write, every bare in-session reference, and file:line-cited bugs not already listed in §0) is in the appendix — §5a.

### 1b. Letter-prefixed / standalone scripts

The letter order (`a→e`) implies `a,b,c,d` run before `e`. The actual data dependencies say otherwise:
- **`c_col_hs_classification.R` must run before `b_embedding_test.R`** — `b` reads `hs_col_classification.csv`, which only `c` writes. The opposite of what the naming suggests.
- **`03_dedup_col.R` and `demo.R` both depend on `Code/new/02_embed_new.R`** having already run (for `col_embed.rds`), i.e. a dependency reaching into the *other* lineage.
- `e_scrape.R` writes `be_sch.csv`; `a_hs_col_construct.R` reads `be_sch_full.csv` — different names, no in-scope script performs the rename/transform between them. Either a manual step is missing or this link is currently broken.
- `intermediate/input_col.rds` is read by three scripts (`Code/new/01_col_hs_crosswalk.R`, `02_embed_new.R`, `03_crosswalk_val.R`) and produced by **none** of them (the one `saveRDS` that would create it is commented out). This is the single largest undocumented external input in the whole codebase.
- **`Code/regression_data.csv`'s own producing script no longer exists anywhere in the repo** (root, letter scripts, or `Code/new/`) — this is the file `07_acs_reweight.R` (this session's new script) reads as its LI-side analytical sample. It's a real, working snapshot, but its provenance is currently unreproducible from source.
- `b_embedding_test.R` is currently broken as written — it references an undefined object `tp` (line ~269, almost certainly meant to be `correct`) and calls `top_embed()` with an argument (`alias_matrix`) that function's own signature doesn't accept while omitting the required `index` argument. The back half of that script cannot run past those lines.
- `Code/Old/` (the pre-reorg legacy tree) was given a lighter, inventory-only pass per scope — package list and one-line purpose per file are in the appendix, no bug-hunting.

Full detail: §5b.

### 1c. `Code/new/`

- **Real order is `00 → 02_embed_new.R → 01_col_hs_crosswalk.R → 02_embed.R → 03 → 04 → 05 → 06/07`**, not `00→01→02→...`. `01` depends on `02_embed_new.R`'s output (`col_strings.rds`/`hs_strings.rds`); `02_embed.R` in turn depends on `01`'s output, and *also* produces `state_fips.rds`, which `04_col_hs_construct.R` needs — meaning `04` has a hidden dependency specifically on the older `02_embed.R`, not on `02_embed_new.R` despite the "new" naming suggesting the latter supersedes the former.
- **`02_embed.R` and `02_embed_new.R` both write `unmatched_col.rds`** from two structurally different pipelines — whichever runs later silently overwrites the other's output, with no versioning or collision check.
- **`06_census.R` is an unfinished draft, not a working script past its first ~150 lines** — it references `raw_grad_rates`, which is never defined in the file (will error `object not found`); its cache-check at line 19 uses `exists(file.path(...))` instead of `file.exists(...)`, so the guarded block essentially never runs; and it has the already-known redundant `esr`/`ESR` filter. None of its computed objects are ever persisted to disk, and nothing else in the repo reads them.
- **`05_demographics.R`'s dead line 74** (confirmed, matches the bug already fixed live in this session): `users_ = users[user_id %in% both__$user_id]` is leftover data.table syntax against what is by that point an Arrow `Dataset` object — dead/broken, though harmless since the correct dplyr-based line above it already produces the right result.
- Both `02_embed.R`/`02_embed_new.R` hardcode `Sys.setenv(RETICULATE_PYTHON = "/usr/local/bin/python3")` — a POSIX path that does not exist on this Windows machine. This is a hard portability blocker for anything touching the embedding/FAISS matching pipeline.
- `07_acs_reweight.R` (this session's script) already documents its own caveats thoroughly in-file — light pass only, no new issues found beyond what it already says about itself.

Full detail: §5c.

---

## 2. Package-usage inventory (for `renv`/pinning)

**No package manifest exists anywhere in the repo** — no `renv.lock`, `packrat/`, `DESCRIPTION`, or `requirements*` file. `.Rprofile` only auto-loads `.env` (paths/secrets, not package versions). This is the actual gap to fill.

| Package | Where used | Standardization note |
|---|---|---|
| `tidyverse` (+ dplyr/tidyr/stringr/purrr/readr/ggplot2) | Nearly every script, all three lineages | Frequently loaded both individually *and* via `tidyverse` — harmless but worth cleaning up |
| `data.table` | Nearly every script | Version-sensitive — this is the exact package whose version drift caused the `IDate` corruption bug diagnosed this session |
| `dotenv`, `here` | Every script in the root pipeline and `Code/new/`; most of the letter scripts | The `.env`-loading convention itself is sound; case-mismatch bugs (below) are the actual risk |
| `readxl` | 9 of 11 root scripts, most letter scripts | **`09_plots.Rmd` calls `read_excel()` without ever loading `readxl`** — only works because an earlier script in the same session left it attached. Breaks if `09` is ever run standalone |
| `table.express` | 6 root scripts explicitly; 2 more (`04_li_ed_pos.Rmd`, `09_plots.Rmd`) use it via `::` with no `library()` call at all; also loaded-but-never-called in 5 of the letter/standalone scripts (`a`–`e_detect_mismatches`) | **A naive dependency scanner undercounts this package** — it won't see the `::`-only usages. Also a strong candidate to drop entirely from the 5 files that load it without using it |
| `educationdata` | 5 root scripts, `00_alias_generation.R`, `04_col_hs_construct.R` | Live Urban Institute API — version/endpoint-sensitive |
| `tidycensus`, `censusapi` | `intercensal.R`, `06_census.R`, `07_acs_reweight.R`, dormant chunks in `02_col_chars.Rmd`/`09_plots.Rmd` | **Two independent Census-API credential mechanisms in play** (see §3) |
| `reticulate`, `openai`, `matrixStats`, `RANN` | `02_embed.R`, `02_embed_new.R`, `b_embedding_test.R`, `c_col_hs_classification.R` | The single biggest portability/credential risk in the whole codebase — hardcoded POSIX Python path, ambient (non-`.env`) OpenAI key, and `matrixStats`/`RANN` both loaded but apparently unused everywhere they appear |
| `survey` | `07_acs_reweight.R` only | New this session |
| `mipfp` | Referenced in a comment only (`07_acs_reweight.R`), not actually used yet | Needed only if the county-level stretch goal is picked back up |
| `sf`, `tigris`, `tidygeocoder` | `00_alias_generation.R`/`a_hs_col_construct.R`, `06_census.R`, `09_plots.Rmd` | Live geocoding — network + rate-limit dependent |
| `fixest`, `texreg`, `xtable`, `stargazer` | Regression/table scripts (`06`-`10`, `kappa.R`) | `stargazer` called in `kappa.R` without `library(stargazer)` — same "ambient attachment" fragility as `readxl` in `09` |
| `rvest`, `stringdist` | `e_scrape.R` | `stringdist` loaded, never used |
| `arrow` | `05_demographics.R` only, currently | The one existing toehold for the I/O-modernization goal |
| Vestigial/likely-dead across various files | `DescTools` (01_shocks), `ggrepel`/`extrafont`/`vars`-naming-collision (09_plots), `RANN`/`matrixStats` (embedding scripts), `stringdist` (e_scrape), `table.express` (5 files that load-but-don't-call it) | Good candidates to drop before pinning, rather than pinning dead weight |

**Two design questions the standardization pass should answer explicitly, since they materially change what "the package list" even is:**
1. Should the manifest include packages only exercised inside `eval=FALSE` R Markdown chunks or commented-out `library()` calls (`estimatr`, `progressr`, dormant `blsAPI`/`tidycensus`/`jsonlite` usage)? Two defensible answers, different manifests.
2. Should packages used only via `::` (never `library()`'d) count as a hard dependency of that specific file? (They should for `renv::snapshot()`'s purposes regardless, but it affects how you audit "does this script declare what it needs.")

---

## 3. Cross-cutting fragility patterns (beyond the specific bugs above)

- **Env-var case mismatch, `CENSUS_KEY` vs `census_key`**, appears in **three** places, not just the one already known: `intercensal.R:64` (live), `09_plots.Rmd:2119` (dormant, `eval=FALSE` chunk), and is correctly avoided in `07_acs_reweight.R`. `intercensal.R` additionally has a dead, correctly-cased `census_key` variable (line 9) that's defined and never used — the actual API call re-derives the wrong-cased value independently instead. Note: `censusapi::getCensus()` would have fallen back to the correctly-cased `CENSUS_KEY` automatically if the `key=` argument were simply omitted — the explicit lowercase override actively defeats a working fallback.
- **Ambient, non-`.env` credentials**: OpenAI key (embedding scripts) and, in `06_census.R`, the Census key — both rely on whatever's cached in the R/Python session rather than being read from `.env` the way `CENSUS_KEY`/`BRAIN_DRAIN_ROOT` are everywhere else. `07_acs_reweight.R` is the only script that does this correctly today.
- **No script anywhere creates its output directories** — every `write.csv`/`saveRDS`/`ggsave` assumes `Data/<specification>/` and `Outputs/<specification>/` already exist. Fresh setup, or a new `specification` value, fails at first write with a bare "cannot open file," not a helpful error.
- **Derived files land in `raw/`, and intermediate files land in `results/`**, against the README's own stated taxonomy (`unified_cbsa.csv`, `raw_microdata__05.csv` written into `raw/`; `10_roi.Rmd` treats a `results/` file as an input). Worth resolving before an arrow/parquet migration that might treat `raw/` as immutable.
- **Silent out-of-bounds / silent type coercion risk pattern**, seen twice: `05_merge.Rmd`'s `cumulative_weights[1]`...`[11]` hardcoded to an assumed 11-row table (R returns `NA`, not an error, if that assumption ever breaks); and the general `IDate`-storage-corruption class of bug already diagnosed live this session in `both_final.rds` — worth checking whether *other* `.rds` files built before the R 4.6.0 reinstall carry the same latent issue (this audit did not attempt to check every `.rds` on disk — see Uncertain, below).
- **Hardcoded absolute paths outside `.env`**: `/usr/local/bin/python3` (embedding scripts, breaks outright on Windows); assorted `E:/`, Dropbox, and Stata paths in `Code/Old/` (legacy, lower priority since that tree is already marked stale).

---

## 4. Recommendation

Given this supports a journal submission and needs to be reproducible by you (and potentially a referee or RA) months from now, on a machine that isn't this one:

1. **Pin with `renv`**, not `groundhog` or a hand-written manifest. `renv::init()` + `renv::snapshot()` against a curated package list (start from §2's table, minus the flagged dead imports) gives you a lockfile that travels with the repo and reproduces exact versions — directly addresses the `IDate`/data.table-version drift that just bit you. `groundhog` is a reasonable alternative if you want date-based CRAN snapshots instead of a lockfile, but `renv` is the more common choice for a single-repo, single-collaborator research project and integrates better with RStudio project files, which you're already using.
2. **Decide the `eval=FALSE`/commented-out-code question from §2 before snapshotting** — otherwise the lockfile either drags in packages (`estimatr`, `blsAPI`, `progressr`) you may not actually need, or omits ones you'd need the moment you re-enable a dormant chunk.
3. **Fix the `RETICULATE_PYTHON` hardcoding before pinning anything Python-adjacent** — move it into `.env` (e.g. `RETICULATE_PYTHON=...`) so it resolves per-machine, the same way `BRAIN_DRAIN_ROOT` already does.
4. **This audit, plus a resolved package list, is what "confident in the current configuration" should mean** — I'd treat the actual `renv::snapshot()` step, and any `.env` additions it implies, as the next concrete action, separate from (and prior to) the arrow/`04_li_ed_pos` modernization work.

---

## What I verified vs. what remains uncertain

**Verified by reading code** (not by executing anything — no R is available in this environment): every package/file dependency and bug cited above was found via direct file reads and repo-wide grep for the referenced object/file names, cross-checked against actual producing/consuming scripts where a chain could be traced.

**Uncertain, would need an actual run to confirm:**
- Whether `01_shocts.Rmd`'s stale-`shifts` scenario (§0.4) has actually ever produced a wrong `shifts.csv` in practice, or whether the script has always happened to be run in an order that avoids it.
- Whether `08_data_generation.Rmd`'s `return_to_both` bug (§0.2) actually reaches any submitted table, or dead-ends at the unexported `problems` diagnostic.
- Whether other pre-2026-reorg `.rds` files carry the same `IDate` double-storage corruption as `both_final.rds` — this audit did not enumerate and check every `.rds` on disk.
- The exact current install state of R 4.6.0 vs. 4.5.2 (this audit is a static code read, not a live environment probe).

**Key assumptions made:**
- Treated `Code/Old/` and `09b_plots_old.Rmd` as explicitly out-of-lineage based on their own naming/README, not as candidates for the same depth of review as the active pipeline.
- Treated "the codebase" as everything under `Code/` plus `Code/new/`; did not re-audit `Code/notes/` (prose docs) or binary data files themselves.
- Where a script's true dependency contradicts its filename-implied order, I reported the dependency as found via grep/read, not by actually executing the sequence to confirm run-time behavior.

---

*Appendix (§5a/5b/5c: full per-script input/output/dependency/bug detail, and the `Code/Old/` inventory) intentionally omitted from this file to keep it scannable — available on request if you want the complete per-script writeups the three research passes produced, rather than this synthesized top-level version.*
