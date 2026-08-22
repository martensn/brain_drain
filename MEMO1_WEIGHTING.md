# Memo 1: Weighting the Revelio Sample to ACS

**Status: draft.** This is a first pass, written to be complete enough that
someone with the relevant technical background — including a version of me
returning to this after a break — can read it and reconstruct exactly what
was done and why, without needing to read the code. Numbers throughout are
final as of this draft; prose is not. Code references point into
`Code/README_memo1.md`'s numbered pipeline.

## 1. The problem

Revelio Labs' resume-derived panel gives us a large sample of college
graduates with observed year-by-year location, but it is not a random
sample of college graduates. It is built from whoever chose to maintain a
public LinkedIn-style profile, which correlates with things we care about
directly — most importantly, geographic mobility itself. A raw, unweighted
tabulation of "how often do college graduates move across state lines" or
"what share of college graduates live in a Top-10 metro" from this sample
will not match a real national benchmark, and the gap is large enough to
distort any migration analysis built on top of it.

This memo documents how we construct a person-year weight that corrects
for this, so that Revelio-based statistics on migration and metro-area
concentration track the American Community Survey (ACS) — the standard
nationally-representative benchmark for this population — as closely as
the data allow. Everything here concerns the *weighting*, not the
underlying sample construction (matching resumes to institutions, defining
a graduation cohort, etc.), which is documented elsewhere.

## 2. The two samples

Two versions of the Revelio sample appear throughout this memo and the
rest of the project:

- **Column 1**: every Revelio user matched to a four-year college, with no
  requirement that we also observe their high school. The larger, less
  restrictive sample.
- **Column 2**: the subset of Column 1 for whom we *also* observe a
  matched high school. This is the paper's primary analysis sample —
  everything that depends on origin (home-state) geography needs it — and
  the only one we reweight. Column 1 is shown throughout as an unweighted
  reference line, never itself corrected.

Both are restricted the same way before any weighting happens: bachelor's
degree or higher, employed, under 65. The ACS benchmark applies the
identical restriction (`SCHL` &gt; 20, `ESR` &isin; {1,2,3}, age &lt; 65) so
that every comparison in this memo is population-to-population, not
population-to-subpopulation.

## 3. Setup and notation

Index people by $i$ and calendar years by $y$. For person $i$ in year $y$,
let:

- $g_i$ = race (fractional — see §4.1), $s_i$ = sex, $a_{i,y}$ = age bucket,
  $o_i$ = origin state (state of the high school Column 2 requires)
- $\ell_{i,y}$ = person $i$'s state of residence in year $y$ (from
  Revelio's year-by-year panel)
- $\tau(\ell_{i,y})$ = the **metro tier** of that location — a
  classification of $\ell_{i,y}$'s metro area, defined in §5.1

Every Revelio person-year observation eventually carries a weight
$w_{i,y}$. Construction happens in two stages, described in §4 and §5.

## 4. Stage 1: a demographic and mobility weight (constant across years)

The first stage answers: *within Column 2, is a given person's
demographic profile and recent mobility over- or under-represented,
relative to what the same slice of ACS looks like?* This produces one
weight per person, $w_i^{(1)}$, constant across all of that person's
observed years — it does not yet use any information about *when* a
person moved, only *whether* their overall profile looks like ACS's.

### 4.1 Race is fractional, not categorical

Revelio does not observe race directly; it infers race and sex
probabilities per person from name/photo classifiers, so each person
carries six numbers $p_{i,\text{white}}, p_{i,\text{black}},
p_{i,\text{asian}}, p_{i,\text{native}}, p_{i,\text{multiple}},
p_{i,\text{hispanic}}$ summing to 1, rather than a single category. We
carry this fractionally rather than drawing a single race per person: a
stochastic draw would inject sampling noise into the raking target for no
benefit, since raking already handles a fractional/soft assignment
correctly (each person contributes $p_{i,g}$ of a unit to every race
category $g$, not one full unit to one category). Sex probabilities are
in practice close to degenerate (0 or 1), so sex is used as a hard
category (whichever of $p_{i,\text{male}}, p_{i,\text{female}}$ is
larger) — nothing of substance would change by treating it fractionally
too, and it keeps that margin's cell count from doubling for no reason.

### 4.2 Raking against two ACS margins

We match Column 2 to ACS on two separate marginal targets, simultaneously:

1. **Demographics**: origin state &times; age bucket &times; race &times; sex
   (a 4-way cell).
2. **Recent mobility**: origin state &times; whether the person moved
   across a state line in the last year (a 2-way cell, from ACS's own
   1-year mover flag).

Both targets come from ACS 5-year PUMS microdata (`memo1_02a`, a single
fixed vintage), restricted to the same BA+/employed/under-65 population.
We rake to *two separate* margins rather than one combined 5-way cell
(state &times; age &times; race &times; sex &times; mover) because the
mover flag is rare (&asymp;3% of any cell) — folding it into the joint cell
would roughly double the number of cells without adding real information,
worsening the thin-cell instability described next.

**Mechanism.** This is standard iterative proportional fitting (raking),
implemented as a small manual loop rather than a survey-package function,
for a documented reason: `survey::rake()`/`postStratify()`/`calibrate()`
were tried first and each hit a real, verified failure mode at this
sample's scale (millions of rows, fractionally melted by race) —
full diagnostic trail in `memo1_04`'s header comments, not repeated here.
The failure mode in every case traced back to the same root cause: an
**unbounded per-cell ratio**. The fix is to clamp the ratio *every
iteration*, not just at the end, which a hand-rolled loop can do directly
and a black-box `rake()` call cannot.

Formally: initialize $w_i^{(0)} = 1$ for every (person &times; race-category)
melted row. At each iteration, alternate between the two margins. For
margin $m$ with cells indexed by $k$, compute

$$r_k = \operatorname{clip}\!\left(\frac{N_k^{\text{ACS}}}{\sum_{i \in k} w_i^{(t)}},\ 0.05,\ 20\right)$$

and update $w_i^{(t+1)} = w_i^{(t)} \cdot r_{k(i)}$ for every $i$ in cell
$k$. Iterate (alternating margins) until the largest single-row weight
change falls below 1, or 10 iterations, whichever comes first. The
0.05&ndash;20&times; clamp is applied to *every* per-cell ratio at *every*
iteration — this, not the convergence criterion, is what keeps weights
from blowing up on thin cells (a real, observed problem: without a cap,
some state &times; race &times; sex cells with very few Column 2
observations relative to their ACS population received explosive weight).

After convergence, each person's melted (person &times; race-category) rows
are summed back to a single person-level weight, then given one final
safety clamp at 0.05&ndash;20&times; the sample median (belt-and-suspenders:
summing several already-capped per-iteration weights can in principle
still land outside that band, even though no single ratio did). Call the
result $w_i^{(1)}$ — this is Column 2's Stage-1 weight, and it is what
"Column 2, reweighted" means everywhere in this memo unless stated
otherwise. Column 1 never receives this treatment; it is always shown
unweighted, for reference.

## 5. Stage 2: calibrating each year's migration flows to ACS

Stage 1 fixes one static profile per person. But the actual claim this
memo cares about is about *migration* — the year-by-year chance of moving
and the metro areas people move between — and demographic reweighting
alone does not target that at all. A person's age, race, sex, and
one-year mover status don't fully capture whether Revelio over- or
under-samples people who, say, moved from a mid-size Southern city to a
Northeastern metro in exactly 2015. Stage 2 targets that directly, using
each person's **own year-by-year location history**, not a single
snapshot.

### 5.1 Metro tiers

CBSAs (Core-Based Statistical Areas — the Census's metro-area definition)
are ranked once, by a single fixed 2022 population snapshot, into three
size tiers:

- **Top 10** — the ten largest metro areas
- **Top 11&ndash;50** — the next forty
- **Everything else** — every other metro or non-metro area

This ranking never changes across the sample period — "Top 10" means the
same ten metros in 2000 and in 2023 — so any change in a tier's
population share is a real compositional shift, not an artifact of metros
re-ranking. Non-metro counties get their own pseudo-CBSA code and fall
into "everything else."

Each size tier is then **crossed with Census region** (Northeast,
Midwest, South, West), giving 12 cells total: e.g. "Top 10 (Northeast)"
and "Top 10 (South)" are different cells. This crossing turned out to
matter a great deal (§6) — a size-only classification cannot distinguish
someone moving between two Top-10 metros in different regions from
someone staying in the same one, and a lot of the geographic
mismatch between Revelio and ACS turned out to be regional, not purely
about metro size.

*(Two coarser classifications — size alone, 5 tiers; size alone,
3 tiers — were also built and compared; see the alternative-specifications
section. The size&times;region scheme is the one used everywhere in this
main section because it is the one that actually worked.)*

### 5.2 The calibration target: an origin&rarr;destination flow, not a snapshot

For a given calendar year $y$, define the **flow** of a person as the
pair (their metro tier one year earlier, their metro tier in year $y$):
$\big(\tau(\ell_{i,y-1}),\ \tau(\ell_{i,y})\big)$. ACS respondents report
exactly this: their PUMA of residence one year ago (`MIGPUMA`) and their
current PUMA, both mapped to the same tier&times;region scheme via a
population-weighted geographic crosswalk (Census tract population,
official tract-to-PUMA relationship files — see `memo1_03a`/`memo1_03b`).
This gives a genuine, year-specific origin&rarr;destination transition
matrix from ACS, not a static geography.

For each calendar year $y$ and each (origin tier $o$, destination tier
$d$) pair, define the calibration ratio

$$r_{y,o,d} = \operatorname{clip}\!\left(\frac{\hat{p}^{\text{ACS}}_{y,o,d}}{\hat{p}^{\text{Revelio}}_{y,o,d}},\ 0.05,\ 20\right)$$

where $\hat{p}^{\text{ACS}}_{y,o,d}$ is ACS's population share in that
flow cell that year, and $\hat{p}^{\text{Revelio}}_{y,o,d}$ is Column 2's
Stage-1-weighted share in the same cell, computed by pooling every
person-year observation from every graduation cohort whose panel touches
year $y$ (not one cohort's trajectory at a time). The final,
calendar-year-varying weight for person $i$'s year-$y$ observation is

$$w_{i,y}^{(2)} = w_i^{(1)} \cdot r_{y,\ \tau(\ell_{i,y-1}),\ \tau(\ell_{i,y})}$$

A person with no observed year-$(y-1)$ location (their first year in the
panel) has no defined origin tier and is excluded from this specific
calculation for that year — they still contribute their Stage-1 weight to
every other chart in this memo, just not to the flow-calibrated line.

### 5.3 What years this covers, and why

Census redraws PUMA boundaries roughly every decade, and mixing
boundary vintages within one crosswalk would make "moved into a Top-10
metro" partly an artifact of redistricting rather than a real geographic
change. The calibration is therefore built separately per PUMA vintage
and only covers years where a vintage-consistent crosswalk exists:

- **2012&ndash;2021**: 2010-vintage PUMA boundaries
- **2022&ndash;2023**: 2020-vintage PUMA boundaries

(2020 itself is excluded throughout this memo as a COVID-era experimental
ACS file, per Census's own guidance — not a boundary-vintage issue.)

Extending backward past 2012 (2000-vintage PUMA boundaries) was
investigated and set aside: Census never published a modern tract-to-PUMA
relationship file for the 2000 vintage, and the closest available
substitute (IPUMS's 2000-vintage composition file) uses a fundamentally
different, hierarchical county/place/tract format that would need new,
separately-validated parsing logic rather than a drop-in swap of the
existing crosswalk-building step. Full detail on what was checked is in
`HANDOFF.md`'s 2026-08-13 entry.

## 6. Results

*(Numbers below reflect the final, verified build. Both birth cohorts —
see §6.1 for why the memo is organized this way — get an identical
treatment.)*

### 6.1 Why birth cohorts, not the full sample

Every calibration above can be run on the full Column 2 sample, but the
full sample includes people well into their 40s and 50s, where Revelio's
LinkedIn-derived coverage is more selected and less representative to
start with — not the population this project's analysis will actually
use. This memo's main results are therefore built on two birth-year
cohorts, each with **both stages rebuilt from scratch** (not filtered
post-hoc from the full sample's weights, which would rake a restricted
sample against the wrong, unrestricted ACS population):

- **Born 1980&ndash;1989** &mdash; roughly the 2002&ndash;2011 graduation cohorts
- **Born 1990&ndash;1999** &mdash; roughly the 2012&ndash;2021 graduation cohorts, closest
  to the population this project will actually draw its final sample from

Birth year is exact on the Revelio side; on the ACS side it is
approximated as survey year minus reported age (accurate to within about
a year, since ACS reports age rather than birth year directly).

### 6.2 The four lines that matter

The rest of the project's charts carry several diagnostic lines (an
intermediate demographic-only line before mobility calibration, several
metro-tier classifications tested against each other, a tier-only
calibration that matches ACS by construction and isn't a real test). This
section keeps only the four that answer the actual question — does the
final, production weight track ACS:

1. **BA Only** — the least restrictive sample (Column 1), no correction
2. **BA + HS on LI** — the analysis sample (Column 2), no correction
3. **BA + HS on LI (reweighted)** — $w_{i,y}^{(2)}$ from §5, the weight
   this memo argues for
4. **ACS PUMS** — the target

![Chance of moving across a state line, born 1980&ndash;1989 vs. born 1990&ndash;1999](Data/results/memo1_simplified_migration_rate_by_cohort.png)

**Mean absolute gap vs. ACS, 2012&ndash;2023 (2020 excluded), percentage points:**

*Migration rate*

| Cohort | BA Only | BA + HS on LI | BA + HS on LI (reweighted) |
|---|---|---|---|
| Born 1980&ndash;1989 | 0.78 | 0.09 | 0.51 |
| Born 1990&ndash;1999 | 2.30 | 1.02 | 0.74 |

*Metro-tier share*

| Cohort | BA Only | BA + HS on LI | BA + HS on LI (reweighted) |
|---|---|---|---|
| Born 1980&ndash;1989 | 1.95 | 1.84 | 0.03 |
| Born 1990&ndash;1999 | 1.75 | 2.90 | 0.03 |

Full interactive versions of both charts — every diagnostic line this
project built, not just the four above, plus a demographic (race/sex/
region) cross-tab at a fixed calendar year and a 3-way metro-tier-scheme
comparison — remain published separately:

- Born 1980&ndash;1989: `https://claude.ai/code/artifact/73a974c9-cc11-41e9-af9a-3a257e486e08`
- Born 1990&ndash;1999: `https://claude.ai/code/artifact/6af4431c-ab36-44bc-a56c-bc54aadebc6c`

### 6.3 Headline finding

**Metro-tier calibration works, unconditionally.** For both cohorts,
flow calibration cuts the tier-share gap to ACS by well over 90% relative
to the unweighted line — from roughly 1.8&ndash;2.9 points down to about
0.03 points, regardless of whether the uncalibrated starting point was
already close (1980s) or not (1990s). This holds up under every
metro-tier classification tested (§5.1, §8) and is the more robust of the
two results in this memo.

**Migration-rate calibration is real but cohort-dependent.** For the
born-1990s cohort — the one closest to this project's actual target
population — flow calibration cuts the migration-rate gap by roughly a
quarter (1.02 &rarr; 0.74 points) relative to the unweighted line, in the
same direction as the tier result. For the born-1980s cohort, it does
not: the unweighted line is already very close to ACS on migration rate
specifically (0.09 points), and every calibration attempted — flow or
otherwise, under any metro-tier scheme — makes it slightly worse rather
than better. This isn't a failure of the method so much as a ceiling
effect: there is very little unweighted gap left for calibration to
close in that cohort, on this one measure.

**Read together**, the two results say different things about the same
mechanism: origin&rarr;destination flow calibration reliably fixes *where*
people are concentrated (metro tier), and it helps *how often* people
move only when there was a real gap to close in the first place. Both
cohorts' results are reported here as found, including the case where
calibration doesn't help — the asymmetry is the finding, not something
to average away.

### 6.4 Full-sample check

The birth-cohort restriction in §6.1 is a deliberate choice about which
population this project cares about, not evidence that the method only
works on a restricted sample. This section runs the same four-line
comparison — BA Only, BA + HS on LI, BA + HS on LI (reweighted), ACS PUMS
— on the **full Column 2 sample**, all ages, as a check that the
calibration itself isn't somehow an artifact of the cohort restriction.

![Metro-tier share by calendar year, full sample](Data/results/memo1_full_sample_metro_tier_share.png)

**Mean absolute gap vs. ACS, 2012&ndash;2023 (2020 excluded), percentage points:**

| BA Only | BA + HS on LI | BA + HS on LI (reweighted) |
|---|---|---|
| 3.80 | 4.04 | 0.12 |

Same story as both birth cohorts: the unweighted lines sit several points
off ACS with no consistent direction, and flow calibration closes nearly
all of it — a ~97% reduction in the full sample, in the same range as the
90%+ reductions seen in both cohorts (§6.3). The full sample's unweighted
gap (4.0 points) is larger than either cohort's, consistent with §6.1's
premise that the older ages folded into the full sample are less
representative to begin with. *(Extended to the full 2012&ndash;2023 window
2026-08-21 — this section originally covered only 2012&ndash;2021, one
PUMA vintage short of the two birth-cohort cuts; `memo1_06b_scheme_
comparison.R` was made vintage-aware the same way `memo1_07c` already was,
closing that gap. The qualitative story is unchanged by the extension —
the kept scheme still wins on both metrics — see §8 row 4 for the updated
full-sample numbers.)*

**Demographic and occupational composition, calendar year 2015** (race,
sex, and occupation are all outside the calibration target — see §7's
note on this being the genuinely out-of-sample check; region is partly
in-sample under the kept scheme):

| Category | BA Only | BA + HS on LI | BA + HS on LI (reweighted) | ACS PUMS |
|---|---|---|---|---|
| White | 72.4% | 73.1% | 67.3% | 71.2% |
| Black | 11.5% | 9.4% | 8.7% | 8.3% |
| Hispanic | 7.8% | 7.6% | 9.5% | 8.2% |
| Asian | 5.7% | 6.1% | 10.7% | 9.9% |
| Multiple | 2.2% | 3.6% | 3.5% | 2.0% |
| Native | 0.5% | 0.2% | 0.2% | 0.3% |
| Male | 50.1% | 51.6% | 48.3% | 48.7% |
| Female | 49.9% | 48.4% | 51.7% | 51.3% |
| Northeast | 20.9% | 24.0% | 21.5% | 21.5% |
| Midwest | 21.6% | 21.8% | 20.8% | 20.9% |
| South | 34.2% | 31.9% | 34.0% | 34.2% |
| West | 23.4% | 22.2% | 23.6% | 23.4% |
| Management | 20.6% | 19.5% | 22.1% | 17.0% |
| Educational Instruction and Library | 6.9% | 5.5% | 5.6% | 13.5% |
| Healthcare Practitioners and Technical | 5.7% | 3.6% | 4.2% | 10.0% |
| Business and Financial Operations | 10.2% | 10.5% | 10.4% | 9.6% |
| Sales and Related | 9.0% | 11.3% | 11.4% | 8.6% |
| Office and Administrative Support | 6.3% | 7.9% | 7.8% | 8.4% |
| Computer and Mathematical | 8.9% | 7.1% | 7.3% | 6.0% |
| Arts, Design, Entertainment, Sports, and Media | 8.3% | 10.6% | 9.4% | 3.6% |
| Community and Social Service | 3.6% | 3.0% | 3.1% | 3.6% |
| Architecture and Engineering | 4.7% | 3.5% | 3.2% | 3.6% |
| Legal | 2.7% | 2.8% | 3.4% | 2.6% |
| Life, Physical, and Social Science | 4.1% | 3.2% | 2.6% | 2.1% |
| Personal Care and Service | 2.2% | 2.8% | 2.3% | 1.7% |
| Protective Service | 1.0% | 1.2% | 1.1% | 1.6% |
| Food Preparation and Serving | 1.3% | 2.1% | 1.3% | 1.6% |
| Production | 0.7% | 0.9% | 0.7% | 1.5% |
| Transportation and Material Moving | 0.9% | 1.1% | 1.0% | 1.5% |
| Healthcare Support | 0.8% | 0.9% | 0.8% | 0.9% |
| Construction and Extraction | 0.4% | 0.5% | 0.5% | 0.9% |
| Installation, Maintenance, and Repair | 0.7% | 0.8% | 0.8% | 0.7% |
| Building and Grounds Cleaning and Maintenance | 0.1% | 0.2% | 0.1% | 0.7% |
| Farming, Fishing, and Forestry | 0.1% | 0.1% | 0.1% | 0.1% |
| Military Specific | 0.9% | 0.8% | 0.8% | ~0.0% |

Same pattern as both cohorts' cross-tabs (published artifacts, §6.2): the
reweighted line doesn't uniformly track ACS on race — it overshoots on
Asian (10.7% vs. ACS's 9.9%, itself already an overshoot from the
unweighted 6.1%) and undershoots White (67.3% vs. 71.2%). Region tells a
cleaner story: unweighted BA + HS on LI is off by up to 2.6 points
(Northeast, 24.0% vs. ACS's 21.5%), and reweighting brings every region
within 0.2 points of ACS — expected, since region is partly baked into
the kept scheme's own tier definition, so this is closer to an
internal-consistency check than a fully independent one (§7 makes the
same point for the cohort cross-tabs). Race and sex are the honest
out-of-sample check here, and they show the same "real but imperfect"
pattern as §6.3 already describes for migration rate: the mechanism does
something, not everything.

**Occupation is the sharpest selection story in this table.** Education
and healthcare are the two largest gaps by far — ACS shows Educational
Instruction and Library and Healthcare Practitioners as the 2nd- and
3rd-largest occupation groups (13.5%, 10.0%), while Revelio shows both at
roughly a third of that, unweighted or reweighted alike. Arts, Design,
Entertainment, Sports, and Media runs the other direction, about 3x
over-represented (3.6% ACS vs. 9-11% across every Revelio series).
Reweighting doesn't correct this — occupation was never a calibration
target — and in at least one case moves the wrong way: Management goes
from 19.5% (unweighted) to 22.1% (reweighted), *more* over-represented,
not less. One entry is likely a data artifact rather than a real finding:
Military Specific is ~0% of the ACS benchmark (as expected for a BA+/
employed/civilian-heavy population) but ~0.8-0.9% across all three
Revelio series — a 100x+ gap on a category this small more plausibly
reflects an occupation-classification quirk (a job-title match landing in
the SOC "55-" range in error) than tens of thousands of genuine
military-employed LinkedIn users, and shouldn't be read as a substantive
result. *(Occupation crosswalk: Revelio's own SOC code, collapsed to its
2018 SOC major group; ACS's OCCP resolved via the Census Bureau's
official 2010-vintage Census-occupation-to-SOC crosswalk, since ACS 2015
predates the 2018 occupation-code revision — confirmed live rather than
assumed, after an initial ~72% match rate against the 2018-only code list
flagged the vintage mismatch. Resolution rates: 99.6% on the ACS side,
88-92% on Revelio's, since a person's job title occasionally fails to
resolve to any SOC code even when their location that year is known —
unresolved rows are excluded from the shares above, not imputed.)*

## 7. Interpretation and limitations

- The flow calibration corrects the *cross-sectional composition* of
  Column 2's metro-tier and migration patterns to match ACS, year by
  year. It does not correct anything about the underlying
  high-school-to-college relationship, or any bias not captured by
  location and demographics.
- Coverage is 2012&ndash;2023 (minus 2020). Years outside that window use
  only the Stage-1 weight — there is currently no flow-calibrated
  correction available before 2012.
- Demographic reweighting (Stage 1) does not perfectly reproduce ACS's
  race composition even at the single reference year it targets in
  aggregate; the cross-tab in §6.2 shows this directly rather than
  asserting a clean match.
- Region enters the tier definition, so the region dimension of the
  flow calibration's success is not a fully independent validity check —
  race and sex, which are outside the calibration target entirely, are
  the genuinely out-of-sample evidence that the method is doing something
  real rather than fitting itself by construction.

## 8. Alternative specifications

*(Nicholas: your section — a catalog of what else was tried and why it
was set aside. The data below is everything that's retrievable without
re-running anything; two rows genuinely only exist as console output from
a one-off test script, noted explicitly. Full detail behind every number
is in `HANDOFF.md`'s 2026-08-11/12 dated entries.)*

![Alternative specifications summary](Data/results/memo1_alt_specs_summary.png)

| # | Alternative | Result | Where the number lives |
|---|---|---|---|
| 1 | IRS SOI destination-tier margin, in place of ACS (`memo1_alternative_specs/irs_soi_desttier_margin.R` + `reweight_column2_desttier_test.R`) | Worse than the kept demographics+mover margin: metro-tier gap (2021 only) 0.0234 &rarr; 0.0263. Likely cause: IRS SOI's flow file covers all tax filers, not just college graduates, so its destination-tier distribution is more dispersed than ACS's college-grad-specific one. | **Console output only** — transcribed from `HANDOFF.md`; not saved to disk (the script writes the constructed margin, `Data/intermediate/irs_soi_desttier_margin_2021.rds`, but not the comparison stat). |
| 2 | Demographic-only Stage 1 (no recent-mover margin) vs. demographics+mover | Land within noise of each other on both migration-rate and metro-tier checks — the mover margin's marginal contribution before Stage 2 existed was never large. | Qualitative finding only, `HANDOFF.md` 2026-08-11. No saved comparison table — Stage 1's mover margin was folded into production before this was checked precisely. |
| 3 | Unconditional (non-origin-crossed) tier-share calibration by year, i.e. Phase A alone (`phase_a_year_calibration_test.R`) | Matches ACS's tier distribution by construction (~0 gap) — not a real test of anything except that the mechanism works. Its migration-rate side effect is what's actually informative: see row 4. | Superseded by the production `memo1_06b_scheme_comparison.R` run — use that table, not this script's own printed numbers (an early, since-fixed version of the pipeline). |
| 4 | Metro-tier classification granularity: 5-tier vs. 3-tier (both size-only) vs. 3-tier&times;region (kept) | Full-sample migration-rate gap, 2012&ndash;2023 (2020 excluded): only the region-crossed scheme's Phase B beats its own static baseline (1.09% &rarr; 0.86%, ~21% reduction); 5-tier makes it *worse* (1.09% &rarr; 1.20%) and 3-tier is a marginal improvement (1.09% &rarr; 1.07%). Tier-share gap improves monotonically with granularity regardless (5-tier 2.23%&rarr;0.57% &middot; 3-tier 3.71%&rarr;0.12% &middot; region 0.98%&rarr;0.03%). This is the result that decided the kept scheme. | **Saved**: `Data/results/memo1_scheme_gap_summary.csv` (full sample) and the `_born_1980s`/`_born_1990s` suffixed versions (§6.2), all written by `memo1_06b_scheme_comparison.R`'s equivalents. Genuinely easy to reload — no rerun needed. |
| 5 | Extending flow calibration to pre-2012 (2000-vintage PUMA boundaries) | Investigated, not built: no public 2000-vintage tract-to-PUMA relationship file exists; the closest substitute uses an incompatible hierarchical format. | §5.3 above and `HANDOFF.md`'s 2026-08-13 entry. Feasibility finding only, no numeric result to retrieve. |

## Appendix: data provenance

| Input | Source | Vintage | Script |
|---|---|---|---|
| Stage-1 demographic/mover margins | ACS 5-year PUMS | 2022 (pooled 2018&ndash;2022) | `memo1_02a` |
| Calendar-year migration rate, metro tier | ACS 1-year PUMS | 2008&ndash;2023, minus 2020 | `memo1_02b` |
| Race/sex for the demographic cross-tab | ACS 1-year PUMS supplement | 2015 only | `memo1_02c` |
| PUMA&rarr;metro-tier crosswalk | Census tract population + tract-to-PUMA relationship files | 2010, 2020 | `memo1_03a` |
| MIGPUMA&rarr;metro-tier crosswalk | IPUMS PUMA&rarr;MigPUMA composition tables | 2010, 2020 | `memo1_03b` |
| CBSA population ranking (tier definitions) | Census population estimates | 2022 (fixed, all years) | `memo1_00` |
