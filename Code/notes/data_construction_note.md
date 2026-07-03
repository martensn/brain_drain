# Education Data Construction Notes

This note documents the first five scripts in the education matching pipeline as they exist in the current codebase. It is written as a practical reference for future use and is intended to preserve the logic, intermediate objects, and main assumptions behind data construction.

## Overview

The pipeline builds a matched dataset linking user-reported educational histories to U.S. high schools and colleges. The broad workflow is:

1. Construct cleaned reference universes of U.S. high schools and degree-granting colleges, along with broad alias dictionaries.
2. Use direct string rules to match user-reported school names to those reference universes.
3. Use embeddings to recover additional matches among still-unmatched names.
4. Consolidate and validate the crosswalks, resolving some duplicate college mappings and flagging ambiguous high-school mappings for later disambiguation.
5. Construct a user-level dataset containing matched college and high-school records, and probabilistically resolve ambiguous high-school names using college-specific state-of-residence patterns from IPEDS.

The current scripts are:

- `00_alias_generation.R`
- `01_col_hs_crosswalk.R`
- `02_embed.R`
- `03_crosswalk_val.R`
- `04_col_hs_construct.R`

---

## Script 00: `00_alias_generation.R`

### Purpose

This script builds the underlying reference datasets for U.S. high schools and colleges and generates alias dictionaries used later for direct string matching and embedding-based recovery.

### Main inputs

The script draws from several sources:

- NCES CCD directory and grade-12 enrollment data for public high schools.
- NCES private school files for private high schools, including a separate address file.
- IPEDS directory data for colleges and universities.
- IPEDS completions data for bachelor's degrees awarded.
- IPEDS fall enrollment by state of residence.
- Geographic support files from `tigris`, including ZCTAs and counties.
- A subcollege/subunit file (`be_sch_full.csv`) used to generate college subunit aliases.

### High-school construction

The script builds a national high-school universe covering public and private schools, then harmonizes them into a single `schools` file.

For public schools, it:

- pulls CCD directory and enrollment records,
- uses grade-12 enrollment to summarize high-school size over time,
- collapses multiple annual records to one school-level record,
- keeps ordinary public and vocational high schools,
- stores identifiers, agency identifiers, geographic information, and counts such as `avg_grad_class` and `n_grads`.

For private schools, it:

- reads raw NCES CSV extracts,
- reshapes year-specific columns from wide to long form,
- constructs school-level graduating-class measures using biannual data,
- derives weighted average graduating class size,
- geocodes schools using the Census geocoder in batches,
- fills remaining geography using ZIP centroids and then county centroids.

The resulting public and private data are combined, non-state jurisdictions are removed, and school names are modestly standardized to reduce false distinct names. Examples include expanding abbreviations such as `HS` to `HIGH SCHOOL`, `ACAD` to `ACADEMY`, and `CTR` to `CENTER`.

The final `schools` object includes school identifiers, names, agency identifiers, location, county FIPS, school control type, start and end years, graduating-class summaries, and geocoding method.

### College construction

The script also builds a national college universe of institutions awarding bachelor's degrees during the study window.

It:

- pulls IPEDS directory data for institutions with undergraduate offerings and bachelor's or higher degree authority,
- pulls completions data restricted to bachelor's awards,
- pulls even-year fall residence enrollment data,
- collapses institution-level geography to one record per `unitid`-`opeid` pair,
- geocodes missing counties using the Census geocoder and ZIP centroids,
- builds degree-award totals over the study period,
- filters out nonproductive or non-baccalaureate branches,
- constructs parent-system fields such as `parent_institution`,
- defines `systems`, a system-level table with each branch's share of system degree production.

The final `colleges` file is a harmonized institution-level reference table keyed by `unitid` and `opeid`, containing institution names, aliases, system relationships, degree totals, geographic fields, and parent-system assignments.

### Alias generation

The script then generates aliases for both colleges and high schools.

For colleges, aliases come from several sources:

- direct institution names,
- parent institution names,
- cleaned aliases already present in IPEDS,
- derived variants that remove common prefixes and suffixes,
- acronym-style aliases,
- system aliases,
- system-branch abbreviations,
- subunit aliases derived from subcollege names.

The resulting college alias table tracks whether an alias is potentially system-level and links aliases to `unitid`, `opeid`, `parent_institution`, and `inst_name`.

For high schools, aliases are more name-centered. The script derives:

- the full school name,
- a stripped base name,
- an aggressively stripped base,
- variants appending standardized `high`,
- city-prefixed variants,
- `Saint`/`St.` expansions.

It then computes alias counts and alias weights based on school size (`n_grads`).

### Saved outputs

The script saves the core reference objects used by the rest of the pipeline:

- `schools.rds`
- `colleges.rds`
- `hs_alias.rds`
- `col_alias.rds`
- CSV versions of the alias tables

In practice, this script defines the reference universe to which all downstream user-reported names are matched.

### Important design choices

- The high-school universe is restricted to U.S. states and excludes territories and outlying areas.
- Private-school geography is partially imputed when exact geocoding fails.
- College alias generation is intentionally expansive, especially for systems and subunits.
- High-school aliases are weighted by school size, which later helps prioritize more plausible matches.

---

## Script 01: `01_col_hs_crosswalk.R`

### Purpose

This script builds the initial name-to-institution crosswalks from the user-reported education file using direct string rules only. It separately matches colleges and high schools and saves both matched and unmatched cases for later recovery.

### Main input

The main user-level input is the education extract:

- `02/2026.04.09_education.csv`

The script reads at least the following fields:

- `university_name`
- `rsid`
- `degree`
- `university_country`
- `university_location`

It also reads the alias/reference objects built in Script 00.

### String normalization and classification

Before matching, the script normalizes names using a `normalize_ed()` function that:

- lowercases strings,
- strips punctuation to spaces,
- collapses multiple spaces.

It then classifies strings into broad categories using a rule-based classifier:

- `college`
- `high_school`
- `foreign`
- `not_school`
- `other`

This classifier uses positive regex cues, negative evidence, foreign-language indicators, and simple encoding checks. The goal is to avoid matching obvious non-school strings and to separate likely colleges from likely high schools before joining to alias tables.

### College matching

For colleges, the script:

1. keeps U.S.-based entries with degree in `Bachelor` or blank,
2. excludes strings that appear to be high schools,
3. classifies the cleaned string as a likely college,
4. tries three matching passes:
   - exact match on `raw_string`,
   - exact match on alias in the college string table,
   - exact match on alias in the broader college alias table.

Matched college rows are stored in `matched_col.rds`.

Unmatched college rows are stored in `unmatched_col.rds` for embedding-based recovery.

### High-school matching

For high schools, the script:

1. keeps U.S.-based entries with degree in `High School` or blank,
2. excludes rows already classified as likely college cases,
3. classifies the cleaned string as a likely high school,
4. applies three analogous matching passes:
   - exact match on `raw_string`,
   - exact match on alias in the high-school string table,
   - exact match on alias in the broader high-school alias table.

Matched high-school rows are stored in `matched_hs.rds`.

Unmatched high-school rows are stored in `unmatched_hs.rds` for embedding-based recovery.

### Saved outputs

- `matched_col.rds`
- `unmatched_col.rds`
- `matched_hs.rds`
- `unmatched_hs.rds`

### Important design choices

- Direct string matching is attempted before embeddings.
- The script works at the `rsid`-name level rather than immediately collapsing to users.
- Blank degree values are allowed into both the college and high-school candidate pools, so long as the string classifier supports the interpretation.
- The script is intentionally conservative in the direct stage; unresolved cases are deferred to embeddings rather than forced.

---

## Script 02: `02_embed.R`

### Purpose

This script uses embeddings to recover additional college and high-school matches among names left unmatched by Script 01.

### Main inputs

- `unmatched_col.rds`
- `unmatched_hs.rds`
- `col_alias.rds`
- `hs_alias.rds`

It also creates and saves `state_fips.rds`, which is later reused in Script 04.

### Embedding workflow

The script:

1. embeds the unmatched raw input strings,
2. embeds the alias dictionaries,
3. builds a FAISS index over alias embeddings,
4. searches each raw string against that index,
5. retrieves the top candidate aliases and cosine similarities,
6. keeps the top match if the similarity exceeds a threshold.

Thresholds in the current script are:

- `col_threshold = 0.83`
- `hs_threshold = 0.91`

These are asymmetric, reflecting a stricter threshold for high schools.

### College embedding output

For colleges, the script:

- retrieves top alias neighbors,
- identifies whether the recovered alias is still ambiguous over multiple `unitid`-`opeid` pairs,
- retains the top-scoring alias above threshold,
- links the recovered alias back to the unmatched input rows,
- labels the match method as `Embedding`.

The output is saved as `col_embed.rds`.

### High-school embedding output

The same process is repeated for high schools:

- retrieve top alias neighbors,
- determine whether the match is ambiguous over multiple `hs_id`s,
- keep the top-scoring alias above threshold,
- link the result back to the original unmatched rows,
- label the match method as `Embedding`.

The output is saved as `hs_embed.rds`.

### Saved outputs

- `state_fips.rds`
- `col_embed.rds`
- `hs_embed.rds`

### Important design choices

- Embeddings are only used after deterministic string rules fail.
- Alias embeddings are built once per run and searched with FAISS for speed.
- The script keeps only the best match above threshold, not a posterior over candidates.
- Ambiguity is preserved as a flag rather than forcibly resolved at this stage.

---

## Script 03: `03_crosswalk_val.R`

### Purpose

This script consolidates direct and embedding matches, resolves some duplicate mappings, and produces the crosswalks that are used downstream to merge user-level records into matched college and high-school datasets.

### College-side consolidation

The script combines:

- direct college matches from `matched_col.rds`
- embedding college matches from `col_embed.rds`

It then joins these to the college reference file to inspect cases where a single `rsid` maps to multiple `unitid`-`opeid` pairs.

At present, the script resolves duplicate college mappings by choosing the branch with the largest `deg_awarded` within each duplicated `rsid`. This is a pragmatic de-duplication rule rather than a final substantive model.

The de-duplicated college crosswalk is saved as:

- `col_rsid_crosswalk.rds`

### High-school-side consolidation

For high schools, the script combines:

- direct high-school matches from `matched_hs.rds`
- embedding high-school matches from `hs_embed.rds`

It then identifies `rsid`s that map to more than one `hs_id`.

Unlike the college side, these ambiguous high-school cases are **not** collapsed immediately. Instead, the script:

- stores the duplicated `rsid`-to-school candidate set in `rsid_id_dup_crosswalk`,
- builds `hs_rsid_crosswalk.rds` such that ambiguous rows retain the `rsid` and ambiguity flag but have the actual school identifiers blanked out.

This is deliberate: high-school ambiguity is deferred until Script 04, where it can be resolved conditional on the matched college.

### Coverage diagnostics

The script also computes a rough coverage exercise for colleges by identifying unmatched institutions and estimating how much degree-award mass would remain uncovered with and without manual review of the largest missing cases.

This is best interpreted as a diagnostic rather than a core data-construction step.

### Saved outputs

- `col_rsid_crosswalk.rds`
- `hs_rsid_crosswalk.rds`

and, in memory, a duplicated high-school candidate object used later:

- `rsid_id_dup_crosswalk`

### Important design choices

- College duplicates are resolved immediately using branch size.
- High-school duplicates are postponed because the same ambiguous name may plausibly refer to different schools depending on the college attended.
- The high-school crosswalk therefore contains a mixture of deterministic matches and unresolved ambiguous placeholders.

---

## Script 04: `04_col_hs_construct.R`

### Purpose

This script constructs the user-level matched education dataset. It merges user records to the validated college and high-school crosswalks, creates one college record and one high-school record per user, then probabilistically resolves ambiguous high-school names using IPEDS residence patterns.

### Main inputs

- `02/2026.04.09_education.csv`
- `col_rsid_crosswalk.rds`
- `hs_rsid_crosswalk.rds`
- `schools.rds`
- `colleges.rds`
- `state_fips.rds`
- `colleges_ipeds_fall-res.csv`
- `rsid_id_dup_crosswalk` from the validation stage

### User-level college and high-school extraction

The script reads a large slice of the raw education file and converts `enddate` to `end_year` for speed. It then constructs two user-level panels:

#### `input_hs`

For each user, it:

- joins reported school strings to `hs_rsid_crosswalk`,
- keeps the latest high-school observation per `user_id` based on `end_year`,
- stores the matched high-school string, `rsid`, end year, `hs_id` if already resolved, ambiguity flag, and match type.

#### `input_col`

For each user, it:

- excludes degrees in `Associate`, `Doctor`, `Master`, and `MBA`,
- joins reported school strings to `col_rsid_crosswalk`,
- keeps the latest college observation per `user_id`,
- stores the matched college string, `rsid`, degree, `opeid`, `unitid`, system indicator, ambiguity flag, match type, and college state.

These two panels are then inner-joined on `user_id` to produce `both`, a user-level file containing one college-high-school pair per user.

### Ambiguous high-school resolution

The remaining task is to resolve users whose high-school names are still ambiguous.

The current implementation does this probabilistically.

#### Candidate high-school table

`prep_hs_candidates()` converts the duplicated `rsid`-to-high-school candidate file into a candidate table keyed by:

- normalized high-school string,
- `hs_id`,
- state FIPS.

Each candidate also carries:

- `hs_agency_id`
- `n_grads`

#### Unit-by-state probability table

`prep_unit_state_probs()` computes a residence distribution for each college `unitid` using IPEDS fall enrollment by state of residence.

For units with observed residence data, it uses the empirical state shares directly.

For units missing such data, it imputes a distribution using:

- a weight `alpha = 0.7` on the institution's own state,
- the remaining weight on the national residence distribution.

This produces a table of `unitid × state_fips` probabilities.

#### Probabilistic matching rule

For each ambiguous user, the script:

1. expands the reported high-school string to all candidate `hs_id`s with that name,
2. merges in the college's state-of-residence probabilities,
3. computes a candidate score equal to:

   `score = state_prob * n_grads`

4. normalizes these scores within user,
5. draws one candidate high school probabilistically according to those normalized weights.

This yields a sampled `hs_id` and an associated draw probability.

Users whose high school was already resolved in the previous stage bypass this procedure and are labeled deterministic matches.

### Final matched dataset

The script combines probabilistic and deterministic cases into `both_`, which contains:

- the user-level college-high-school pair,
- the resolved `hs_id`,
- an indicator for whether the high-school match was deterministic or probabilistic,
- the probability associated with the probabilistic draw.

`both_` is saved as:

- `both_.rds`

### Calibration / evaluation objects

The latter part of the script evaluates whether the probabilistic matching produces plausible aggregate geography.

It:

- computes college-year weights from the observed sample,
- reweights IPEDS in-state shares to align with the cohort composition of the matched sample,
- compares sample-derived in-state shares to IPEDS-based in-state shares,
- runs a simple weighted regression as a calibration check.

This section is primarily diagnostic and helps assess whether the probabilistic high-school assignment reproduces plausible state-of-origin patterns at the college level.

### Saved outputs

- `both_.rds`

and various in-memory diagnostic objects such as:

- `input_hs`
- `input_col`
- `both`
- `hs_candidates`
- `unit_state`
- `matched_hs`
- `comparison`

### Important design choices

- User-level college and high-school records are reduced to the latest observed entry per user.
- College matching is treated as fixed before high-school disambiguation.
- High-school disambiguation uses college-specific geography rather than name-only frequency.
- The current version uses probabilistic sampling, not deterministic top-score assignment.

---

## Key data products across the first five scripts

### Core reference files

- `schools.rds`: harmonized U.S. high-school universe
- `colleges.rds`: harmonized U.S. college universe
- `hs_alias.rds`: high-school alias dictionary
- `col_alias.rds`: college alias dictionary
- `state_fips.rds`: state abbreviation to FIPS lookup

### Match-stage files

- `matched_col.rds`: direct string college matches
- `unmatched_col.rds`: unresolved college names after direct rules
- `matched_hs.rds`: direct string high-school matches
- `unmatched_hs.rds`: unresolved high-school names after direct rules
- `col_embed.rds`: embedding-recovered college matches
- `hs_embed.rds`: embedding-recovered high-school matches

### Validated crosswalks

- `col_rsid_crosswalk.rds`: de-duplicated college crosswalk from reported string to `unitid`-`opeid`
- `hs_rsid_crosswalk.rds`: high-school crosswalk with ambiguous cases left unresolved

### Final constructed user-level dataset

- `both_.rds`: final user-level matched college-high-school file with deterministic/probabilistic high-school match information

---

## Current identifying variables and match logic

### College identifiers

The primary college identifiers are:

- `unitid`
- `opeid`

In the final pipeline, colleges are matched first and treated as known when resolving ambiguous high schools.

### High-school identifiers

The primary high-school identifier is:

- `hs_id`

Ambiguous high-school names are not resolved solely by name. Instead, the final decision uses:

- the user's reported high-school string,
- the set of candidate `hs_id`s sharing that name,
- each candidate school's size (`n_grads`),
- the matched college's state-of-residence distribution from IPEDS.

---

## Main assumptions and caveats to remember

1. **This is a U.S.-focused matching pipeline.**
   The direct crosswalk stage filters toward U.S. institutions and excludes foreign or obviously non-school strings.

2. **College matching is stronger than high-school matching.**
   Colleges are resolved earlier and more aggressively; ambiguous colleges are collapsed using branch degree counts, while ambiguous high schools are deferred to a conditional model.

3. **High-school geography is partly imputed.**
   Some schools use ZIP centroids or county centroids rather than exact coordinates or county identifiers.

4. **Latest observed school per user is used.**
   The user-level construction step keeps only the latest matched college and latest matched high school by `end_year`.

5. **The current high-school disambiguation is stochastic.**
   Re-running Script 04 can change some assignments unless the random seed is fixed.

6. **The probabilistic high-school model is intentionally simple.**
   The current score is proportional to `state_prob * n_grads`. It does not yet use richer features such as distance, city matching, agency information, or cohort-specific residence distributions.

7. **Some intermediate objects matter even if not saved in the final file.**
   In particular, `rsid_id_dup_crosswalk` is important because it preserves the candidate set needed to resolve ambiguous high-school names.

---

## Suggested future additions for a formal data appendix

If this eventually becomes a true data appendix, the following would be worth adding:

- exact sample restrictions for the raw education file,
- counts at each stage of the pipeline,
- match rates before and after embeddings,
- coverage by degree-award mass and by institution count,
- examples of aliases and ambiguous-name cases,
- validation evidence for the probabilistic high-school matcher,
- discussion of how sensitive results are to thresholds and to the probabilistic draw,
- reproducibility notes on seeds, API use, and cached geography files.

---

## Minimal run order

The intended order of the first five scripts is:

1. `00_alias_generation.R`
2. `01_col_hs_crosswalk.R`
3. `02_embed.R`
4. `03_crosswalk_val.R`
5. `04_col_hs_construct.R`

This order matters because each stage consumes saved objects from the previous one.

---

## Bottom line

The current pipeline creates a user-level matched education dataset by combining deterministic string rules, embedding-based recovery, explicit de-duplication of college matches, and college-conditioned probabilistic resolution of ambiguous high-school names. The resulting final object, `both_.rds`, is the first fully constructed user-level dataset in the sequence and is the main downstream analysis file produced by these scripts.
