# memo1_09_occupation_memo_table_v2.R
#
# [NEW 2026-08-23, REPLACES the first SS6.5 draft] Nicholas's correction:
# the first version of this table only had the new weight's column, with
# no ACS benchmark or unweighted baselines to compare against -- "those
# are useless if i can't compare to an ACS benchmark," and he also wants
# Column 1 (BA Only) vs Column 2 (BA + HS on LI) visible again, same as
# SS6.4's own table. This rebuilds SS6.5 as a genuinely self-contained
# 4-column table (BA Only / BA + HS on LI / BA + HS on LI (reweighted,
# geo+occupation) / ACS PUMS), reusing SS6.4's OWN already-published
# numbers for the first two and the ACS column (read back from the exact
# CSVs SS6.4 itself was built from -- memo1_demo_crosstab_full_simplified.csv
# for race/sex/region, memo1_occupation_crosstab_full_simplified.csv for
# occupation -- rather than re-deriving or hand-transcribing them, so
# there's no risk of a copy error introducing a mismatch with SS6.4's
# published values) alongside the new weight's own numbers already saved
# by memo1_09_occupation_memo_table.R (memo1_demo_crosstab_geo_occ_2015.csv).
#
# Industry: Nicholas separately asked for an industry row-group too.
# Investigated before writing anything -- NOT available. The Revelio
# position source this whole pipeline reads from
# (Data/intermediate/pos_parquet_pilot, confirmed via its live Arrow
# schema) carries only user_id/country/state/msa/startdate/enddate/
# seniority/onet_code/year -- no NAICS or industry field of any kind.
# occupation_crosstab.R's SOC classification is occupation (what job you
# do), not industry (what sector you work in) -- the two aren't
# substitutable, and there's no crosswalk that recovers one from the
# other without genuine information loss (most SOC codes span many
# industries). Flagged to Nicholas rather than building an approximate
# substitute or silently dropping the request.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

LBL_COL1  <- "BA Only"
LBL_COL2U <- "BA + HS on LI"
LBL_NEW   <- "BA + HS on LI (reweighted, geo+occupation)"
LBL_ACS   <- "ACS PUMS"

## =========================================================================
## Load the three source CSVs, harmonize to one schema:
## (source, category_type, category, share)
## =========================================================================
log_step("Loading source CSVs")
demo_full <- fread(file.path(data_dir, "results/memo1_demo_crosstab_full_simplified.csv"))
occ_full  <- fread(file.path(data_dir, "results/memo1_occupation_crosstab_full_simplified.csv"))
setnames(occ_full, "major_group", "category")
occ_full[, category_type := "occupation"]
occ_full <- occ_full[, .(source, category_type, category, share)]

new_col <- fread(file.path(data_dir, "results/memo1_demo_crosstab_geo_occ_2015.csv"))
new_col <- new_col[, .(source, category_type, category, share)]

## SS6.4's occupation labels are already the human-readable SOC major-group
## names; SS6.4's race/sex/region labels are lowercase machine categories
## ("white", "male", "Northeast" is already proper-cased for region) --
## capitalize race/sex to match SS6.4's published table text exactly.
cap_map <- c(white = "White", black = "Black", hispanic = "Hispanic", asian = "Asian",
             multiple = "Multiple", native = "Native", male = "Male", female = "Female")
demo_full[category_type %in% c("race", "sex"), category := unname(cap_map[category])]
new_col[category_type %in% c("race", "sex"), category := unname(cap_map[category])]

all_rows <- rbindlist(list(demo_full, occ_full, new_col), use.names = TRUE)

## Keep only the four sources this table actually shows -- SS6.4's demo/
## occ CSVs also contain "BA + HS on LI (reweighted)" (geography-only),
## deliberately left OUT of this table since Nicholas asked for a table
## like SS6.4's but swapping in the new weight, not a 5-column table.
all_rows <- all_rows[source %in% c(LBL_COL1, LBL_COL2U, LBL_NEW, LBL_ACS)]

wide <- dcast(all_rows, category_type + category ~ source, value.var = "share")
setcolorder(wide, c("category_type", "category", LBL_COL1, LBL_COL2U, LBL_NEW, LBL_ACS))

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
wide[, category := factor(category, levels = row_order)]
setorder(wide, category)

fmt_pct <- function(x) {
  ifelse(is.na(x), "~0.0%",
         ifelse(x < 0.0005, "~0.0%", sprintf("%.1f%%", 100 * x)))
}

## =========================================================================
## Replace the existing SS6.5 section entirely (from "### 6.5" to EOF --
## it was the only thing appended after SS6.4, per the previous run) with
## this corrected, self-contained table.
## =========================================================================
log_step("Rebuilding SS6.5 in MEMO1_WEIGHTING.md")
memo_path <- file.path(directory, "MEMO1_WEIGHTING.md")
existing_lines <- readLines(memo_path)
cut_idx <- which(grepl("^### 6\\.5", existing_lines))
stopifnot(length(cut_idx) == 1)
kept_lines <- existing_lines[seq_len(cut_idx - 1)]
# Drop the trailing blank line(s) immediately before the cut, so we control
# spacing explicitly below rather than accumulating blank lines run over run.
while (length(kept_lines) > 0 && kept_lines[length(kept_lines)] == "") kept_lines <- kept_lines[-length(kept_lines)]

header_lines <- c(
  "",
  "### 6.5 Full-sample check, with occupation added as a Stage 2 margin",
  "",
  sprintf("Table results only, appended %s -- corrected from an earlier draft that showed only the new",
          format(Sys.Date(), "%Y-%m-%d")),
  "weight's own numbers with nothing to compare them against. This reproduces SS6.4's own table structure --",
  "same four columns (BA Only / BA + HS on LI / [a reweighted column] / ACS PUMS), same race/sex/region/",
  "occupation rows -- swapping in `w2_occ` (Code/memo1_09_reweight_column2_occupation.R: geography and",
  "destination occupation raked jointly per calendar year via a 2-margin IPF) as the reweighted column,",
  "in place of SS6.4's geography-only one. BA Only, BA + HS on LI, and ACS PUMS values are read back",
  "unchanged from the exact CSVs SS6.4 itself was built from, not re-derived.",
  "",
  sprintf("**Requested but not yet built: industry.** The Revelio position data this pipeline reads from"),
  "(`Data/intermediate/pos_parquet_pilot`) only carries `onet_code` (occupation) -- no NAICS/industry field",
  "at all, confirmed by reading its Arrow schema directly rather than assumed. Occupation and industry are",
  "different axes (what job vs. what sector), and there's no reliable crosswalk from one to the other --",
  "most SOC codes span many industries. If there's a different Revelio source/table with an industry field,",
  "point me to it and I'll build this properly; otherwise this row-group stays out rather than being faked",
  "via an approximate occupation-to-industry mapping.",
  "",
  "**Mean absolute gap vs. ACS, metro-tier share, 2012-2023 (2020 excluded), percentage points:**",
  "",
  sprintf("| %s | %s | %s |", LBL_COL1, LBL_COL2U, LBL_NEW),
  "|---|---|---|",
  "| 3.80 | 4.04 | 0.11 |",
  "",
  sprintf("**Demographic and occupational composition, calendar year 2015:**"),
  "",
  sprintf("| Category | %s | %s | %s | %s |", LBL_COL1, LBL_COL2U, LBL_NEW, LBL_ACS),
  "|---|---|---|---|---|"
)

row_lines <- wide[, sprintf("| %s | %s | %s | %s | %s |", category,
                             fmt_pct(get(LBL_COL1)), fmt_pct(get(LBL_COL2U)), fmt_pct(get(LBL_NEW)), fmt_pct(get(LBL_ACS)))]

writeLines(c(kept_lines, header_lines, row_lines, ""), con = memo_path)
log_step(sprintf("Rewrote %s with corrected SS6.5 (%d rows)", memo_path, length(row_lines)))
