# memo1_11b_final_tables.R
#
# [NEW 2026-08-23] Builds the two FINAL table versions for
# MEMO1_WEIGHTING.md, replacing the ad hoc single-column SS6.5 draft
# entirely: a 4-column table (BA Only / BA + HS on LI / BA + HS on LI
# (reweighted) / ACS PUMS -- final geo+occupation weight only, the old
# geography-only weight dropped) and a 6-column table (adds the two
# intermediate weighting stages: Stage 1 demographic-only, Stage 2
# migration-only). Both cover the same metro-tier gap summary and
# race/sex/region/occupation composition at calendar year 2015 that
# SS6.4/SS6.5 already used.
#
# [LESSON FROM TODAY'S EARLIER BUG, applied here] Every prior memo-editing
# script used `Sys.getenv("BRAIN_DRAIN_ROOT")` (P:/BRAIN_DRAIN) for BOTH
# data outputs AND MEMO1_WEIGHTING.md itself -- correct for the former,
# WRONG for the latter: the memo is git-tracked at
# D:/Users/martensn/BRAIN_DRAIN/MEMO1_WEIGHTING.md, a different file
# entirely (confirmed via differing inodes) from whatever sat at
# P:/BRAIN_DRAIN/MEMO1_WEIGHTING.md, which is NOT the canonical copy and
# should not be written to as if it were. This script uses `directory`
# (BRAIN_DRAIN_ROOT/P:) only for `data_dir`, and a SEPARATE, explicit
# `git_repo_dir` for the memo path.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
git_repo_dir <- "D:/Users/martensn/BRAIN_DRAIN"
memo_path <- file.path(git_repo_dir, "MEMO1_WEIGHTING.md")
stopifnot(file.exists(memo_path))

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

FIXED_YEAR <- 2015
LBL_COL1  <- "BA Only"
LBL_COL2U <- "BA + HS on LI"
LBL_ACS   <- "ACS PUMS"
LBL_RW_4  <- "BA + HS on LI (reweighted)"
LBL_STAGE1 <- "BA + HS on LI (Stage 1: demographic)"
LBL_STAGE2_MIG <- "BA + HS on LI (Stage 2: migration)"
LBL_STAGE2_OCC <- "BA + HS on LI (Stage 2: migration+occupation)"

RAW_OLD_GEO_ONLY <- "BA + HS on LI (reweighted)"
RAW_NEW_GEO_OCC  <- "BA + HS on LI (reweighted, geo+occupation)"

cap_map <- c(white = "White", black = "Black", hispanic = "Hispanic", asian = "Asian",
             multiple = "Multiple", native = "Native", male = "Male", female = "Female")

race_order <- c("White", "Black", "Hispanic", "Asian", "Multiple", "Native")
sex_order <- c("Male", "Female")
region_order <- c("Northeast", "Midwest", "South", "West")
occ_order <- c(
  "Management", "Educational Instruction and Library", "Healthcare Practitioners and Technical",
  "Business and Financial Operations", "Sales and Related", "Office and Administrative Support",
  "Computer and Mathematical", "Arts, Design, Entertainment, Sports, and Media", "Community and Social Service",
  "Architecture and Engineering", "Legal", "Life, Physical, and Social Science", "Personal Care and Service",
  "Protective Service", "Food Preparation and Serving", "Production", "Transportation and Material Moving",
  "Healthcare Support", "Construction and Extraction", "Installation, Maintenance, and Repair",
  "Building and Grounds Cleaning and Maintenance", "Farming, Fishing, and Forestry", "Military Specific"
)
row_order <- c(race_order, sex_order, region_order, occ_order)

## =========================================================================
## PART 1: metro-tier gap vs ACS, 2012-2023 -- all 5 candidate sources
## =========================================================================
log_step("PART 1: metro-tier gap vs ACS, all sources")
d_tier <- fread(file.path(data_dir, "results/memo1_metro_tier_by_calendar_year_full_simplified.csv"))
acs_tier_target <- d_tier[source == LBL_ACS, .(calendar_year, tier, acs_share = share)]

gap_for_source <- function(src) {
  d <- merge(d_tier[source == src, .(calendar_year, tier, share)], acs_tier_target, by = c("calendar_year", "tier"))
  mean(abs(d$share - d$acs_share))
}
gap_stage1 <- gap_for_source(LBL_STAGE1)
gap_stage2_mig <- gap_for_source(RAW_OLD_GEO_ONLY)
gap_stage2_occ <- gap_for_source(RAW_NEW_GEO_OCC)
cat(sprintf("Gaps: BA Only=3.80 | BA+HS on LI=4.04 | Stage1=%.2f | Stage2-mig=%.2f | Stage2-occ=%.2f (pp)\n",
            100 * gap_stage1, 100 * gap_stage2_mig, 100 * gap_stage2_occ))

## =========================================================================
## PART 2: demographic/occupation composition, calendar year 2015 -- all
## sources, from the already-saved CSVs (no recomputation).
## =========================================================================
log_step("PART 2: assembling demographic/occupation table, all sources")
demo_full <- fread(file.path(data_dir, "results/memo1_demo_crosstab_full_simplified.csv"))
occ_full  <- fread(file.path(data_dir, "results/memo1_occupation_crosstab_full_simplified.csv"))
setnames(occ_full, "major_group", "category")
occ_full[, category_type := "occupation"]
occ_full <- occ_full[, .(source, category_type, category, share)]

geo_occ_col <- fread(file.path(data_dir, "results/memo1_demo_crosstab_geo_occ_2015.csv"))[, .(source, category_type, category, share)]
stage1_col <- fread(file.path(data_dir, "results/memo1_demo_crosstab_stage1_only_2015.csv"))[, .(source, category_type, category, share)]

all_rows <- rbindlist(list(demo_full, occ_full, geo_occ_col, stage1_col), use.names = TRUE)
all_rows[category_type %in% c("race", "sex"), category := unname(cap_map[category])]

wide <- dcast(all_rows, category_type + category ~ source, value.var = "share")
setnames(wide, RAW_OLD_GEO_ONLY, "STAGE2_MIG_RAW", skip_absent = TRUE)
setnames(wide, RAW_NEW_GEO_OCC, "STAGE2_OCC_RAW", skip_absent = TRUE)
wide[, category := factor(category, levels = row_order)]
setorder(wide, category)

fmt_pct <- function(x) {
  ifelse(is.na(x), "~0.0%", ifelse(x < 0.0005, "~0.0%", sprintf("%.1f%%", 100 * x)))
}

## =========================================================================
## PART 3: assemble markdown for both table versions
## =========================================================================
log_step("PART 3: writing markdown")

build_4col_lines <- function() {
  c(
    "",
    "### 6.5 Final specification",
    "",
    sprintf("Table results only, rebuilt %s. The final specification's \"reweighted\" line is `w2_occ`", format(Sys.Date(), "%Y-%m-%d")),
    "(Code/memo1_07_reweight_column2_occupation.R: geography and destination occupation raked jointly per",
    "calendar year via a 2-margin IPF). The intermediate geography-only weighting scheme used earlier in this",
    "project is dropped from this table entirely -- see SS6.6 for the full stage-by-stage comparison including it.",
    "",
    "**Mean absolute gap vs. ACS, metro-tier share, 2012-2023 (2020 excluded), percentage points:**",
    "",
    sprintf("| %s | %s | %s |", LBL_COL1, LBL_COL2U, LBL_RW_4),
    "|---|---|---|",
    sprintf("| 3.80 | 4.04 | %.2f |", 100 * gap_stage2_occ),
    "",
    sprintf("**Demographic and occupational composition, calendar year %d:**", FIXED_YEAR),
    "",
    sprintf("| Category | %s | %s | %s | %s |", LBL_COL1, LBL_COL2U, LBL_RW_4, LBL_ACS),
    "|---|---|---|---|---|"
  )
}
row_lines_4col <- wide[, sprintf("| %s | %s | %s | %s | %s |", category,
                                  fmt_pct(get(LBL_COL1)), fmt_pct(get(LBL_COL2U)), fmt_pct(STAGE2_OCC_RAW), fmt_pct(get(LBL_ACS)))]

build_6col_lines <- function() {
  c(
    "",
    "### 6.6 Full weighting-stage comparison",
    "",
    sprintf("Table results only, added %s. All three weighting stages side by side: Stage 1 alone", format(Sys.Date(), "%Y-%m-%d")),
    "(`w_full_joint`, time-invariant demographic+mobility raking, no calendar-year information at all --",
    "identical value in every year by construction, since nothing here varies by calendar year), Stage 2",
    "migration-only (the geography-flow single-shot ratio this project used before today), and Stage 2",
    "migration+occupation (`w2_occ`, today's final specification, also shown alone in SS6.5).",
    "",
    "**Mean absolute gap vs. ACS, metro-tier share, 2012-2023 (2020 excluded), percentage points:**",
    "",
    sprintf("| %s | %s | %s | %s | %s |", LBL_COL1, LBL_COL2U, LBL_STAGE1, LBL_STAGE2_MIG, LBL_STAGE2_OCC),
    "|---|---|---|---|---|",
    sprintf("| 3.80 | 4.04 | %.2f | %.2f | %.2f |", 100 * gap_stage1, 100 * gap_stage2_mig, 100 * gap_stage2_occ),
    "",
    sprintf("**Demographic and occupational composition, calendar year %d:**", FIXED_YEAR),
    "",
    sprintf("| Category | %s | %s | %s | %s | %s | %s |", LBL_COL1, LBL_COL2U, LBL_STAGE1, LBL_STAGE2_MIG, LBL_STAGE2_OCC, LBL_ACS),
    "|---|---|---|---|---|---|---|"
  )
}
row_lines_6col <- wide[, sprintf("| %s | %s | %s | %s | %s | %s | %s |", category,
                                  fmt_pct(get(LBL_COL1)), fmt_pct(get(LBL_COL2U)), fmt_pct(get(LBL_STAGE1)),
                                  fmt_pct(STAGE2_MIG_RAW), fmt_pct(STAGE2_OCC_RAW), fmt_pct(get(LBL_ACS)))]

## =========================================================================
## PART 4: insert into the GIT-TRACKED memo, between SS6.4 and SS7 -- if
## "### 6.5" already exists (a prior run of this script), replace from
## there through end of SS6.6 (i.e. up to "## 7."); otherwise insert fresh
## before "## 7." SS6.4 and everything from SS7 onward is preserved
## byte-for-byte either way.
## =========================================================================
existing_lines <- readLines(memo_path)
sec7_idx <- which(grepl("^## 7\\.", existing_lines))
stopifnot(length(sec7_idx) == 1)
cut_idx <- which(grepl("^### 6\\.5", existing_lines))
stopifnot(length(cut_idx) <= 1)

before_lines <- existing_lines[seq_len((if (length(cut_idx) == 1) cut_idx else sec7_idx) - 1)]
while (length(before_lines) > 0 && before_lines[length(before_lines)] == "") before_lines <- before_lines[-length(before_lines)]
after_lines <- existing_lines[sec7_idx:length(existing_lines)]

final_lines <- c(before_lines, build_4col_lines(), row_lines_4col, build_6col_lines(), row_lines_6col, "", after_lines)
writeLines(final_lines, con = memo_path)
log_step(sprintf("Wrote %s (SS6.5 4-col + SS6.6 6-col inserted before SS7), %d total lines", memo_path, length(final_lines)))
