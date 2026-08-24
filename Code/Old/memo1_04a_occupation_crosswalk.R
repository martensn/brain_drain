# memo1_04a_occupation_crosswalk.R (was build_occupation_crosswalk.R)
#
# [NEW 2026-08-23] Builds both vintages of the ACS OCCP -> SOC major-group
# crosswalk from the already-downloaded Census workbook
# (Data/raw/bls/2018-occupation-code-list-and-crosswalk.xlsx), as a real,
# committed, reproducible script -- the existing
# census_occp_2010_to_soc_major_group.rds (used by memo1_04b_occupation_crosstab.R)
# was built ad hoc in a prior interactive session with no script behind
# it. This script regenerates that file identically (verified byte-for-byte
# against the existing 538-row RDS before this script existed) and adds a
# NEW census_occp_2018_to_soc_major_group.rds for the post-2017 OCCP
# vintage, needed because Code/memo1_07_reweight_column2_occupation.R
# calibrates on multiple calendar years, not just 2015.
#
# Vintage cutover: VERIFIED LIVE (not assumed) via tidycensus's bundled
# pums_variables -- ACS 1yr OCCP carries 480 distinct values in 2017 and
# 531 in 2018, a clean break exactly at 2018. So calendar years <=2017 use
# the 2010-vintage crosswalk, years >=2018 use the 2018-vintage one.
#
# Both crosswalk sheets ("2010 to 2018 Crosswalk " and "2018 Census Occ
# Code List") only need the SOC major-group PREFIX (first 2 digits), not
# the full detailed SOC code, so residual/"Other ..." aggregate rows whose
# SOC code carries a placeholder suffix (e.g. "13-20XX", "15-124X") are
# kept -- they're real, valid OCCP codes that appear in survey responses,
# and their major-group prefix is exactly as informative as a fully
# resolved leaf code's. The extraction rule is therefore: keep rows with a
# single (non-range) 4-digit Census code AND a SOC field starting with two
# digits + a dash, which cleanly excludes category-header rows (whose
# Census code is itself a range like "0010-3550") without excluding
# legitimate aggregate codes. Verified this produces exactly the 538-row,
# 2-special-code-excluded (9830/9920 -> "none") result the ad hoc 2010
# build already had, before trusting the same logic on the 2018 sheet.

library(readxl)
library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

xlsx_path <- file.path(data_dir, "raw/bls/2018-occupation-code-list-and-crosswalk.xlsx")

extract_leaf_rows <- function(dt, code_col, soc_col) {
  rows <- dt[grepl("^[0-9]{4}$", get(code_col)) & grepl("^[0-9]{2}-", get(soc_col))]
  rows[, major_prefix := substr(get(soc_col), 1, 2)]
  dup <- rows[, .N, by = code_col][N > 1]
  if (nrow(dup) > 0) stop(sprintf("%d duplicated %s values -- extraction rule is not unique, investigate before trusting", nrow(dup), code_col))
  rows
}

## =========================================================================
## 2010-vintage: "2010 to 2018 Crosswalk " sheet, its own 2010 SOC/Census
## columns (header rows found at skip=3 by inspection).
## =========================================================================
log_step("Building 2010-vintage OCCP -> SOC major-group crosswalk")
raw_2010 <- read_excel(xlsx_path, sheet = "2010 to 2018 Crosswalk ", col_names = FALSE, skip = 3)
setDT(raw_2010)
setnames(raw_2010, c("soc2010", "census2010", "title2010", "soc2018", "census2018", "title2018"))

xwalk_2010 <- extract_leaf_rows(raw_2010, "census2010", "soc2010")
n_special_2010 <- raw_2010[grepl("^[0-9]{4}$", census2010) & !grepl("^[0-9]{2}-", soc2010), .N]
cat(sprintf("2010-vintage: %d codes resolved to a SOC major-group prefix, %d special non-occupation code(s) excluded (soc2010=\"none\")\n",
            nrow(xwalk_2010), n_special_2010))

out_2010 <- xwalk_2010[, .(census_2010 = census2010, major_prefix)]
setorder(out_2010, census_2010)
saveRDS(out_2010, file.path(data_dir, "raw/bls/census_occp_2010_to_soc_major_group.rds"))
log_step(sprintf("Wrote census_occp_2010_to_soc_major_group.rds (%d rows)", nrow(out_2010)))

## =========================================================================
## 2018-vintage: "2018 Census Occ Code List" sheet -- a nested outline
## (category header rows, then leaf/aggregate rows), 4 raw columns.
## =========================================================================
log_step("Building 2018-vintage OCCP -> SOC major-group crosswalk")
raw_2018 <- read_excel(xlsx_path, sheet = "2018 Census Occ Code List", col_names = FALSE)
setDT(raw_2018)
setnames(raw_2018, c("cat1", "title2018", "census2018", "soc2018"))

xwalk_2018 <- extract_leaf_rows(raw_2018, "census2018", "soc2018")
n_special_2018 <- raw_2018[grepl("^[0-9]{4}$", census2018) & !grepl("^[0-9]{2}-", soc2018), .N]
cat(sprintf("2018-vintage: %d codes resolved to a SOC major-group prefix, %d special non-occupation code(s) excluded (soc2018=\"none\")\n",
            nrow(xwalk_2018), n_special_2018))

out_2018 <- xwalk_2018[, .(census_2018 = census2018, major_prefix)]
setorder(out_2018, census_2018)
saveRDS(out_2018, file.path(data_dir, "raw/bls/census_occp_2018_to_soc_major_group.rds"))
log_step(sprintf("Wrote census_occp_2018_to_soc_major_group.rds (%d rows)", nrow(out_2018)))

## ---- sanity: both vintages resolve to exactly the 23 known SOC major
## groups, no orphan prefixes ----
SOC_MAJOR_GROUPS <- c(
  "11" = "Management", "13" = "Business and Financial Operations", "15" = "Computer and Mathematical",
  "17" = "Architecture and Engineering", "19" = "Life, Physical, and Social Science",
  "21" = "Community and Social Service", "23" = "Legal", "25" = "Educational Instruction and Library",
  "27" = "Arts, Design, Entertainment, Sports, and Media", "29" = "Healthcare Practitioners and Technical",
  "31" = "Healthcare Support", "33" = "Protective Service", "35" = "Food Preparation and Serving",
  "37" = "Building and Grounds Cleaning and Maintenance", "39" = "Personal Care and Service",
  "41" = "Sales and Related", "43" = "Office and Administrative Support",
  "45" = "Farming, Fishing, and Forestry", "47" = "Construction and Extraction",
  "49" = "Installation, Maintenance, and Repair", "51" = "Production",
  "53" = "Transportation and Material Moving", "55" = "Military Specific"
)
stopifnot(all(out_2010$major_prefix %in% names(SOC_MAJOR_GROUPS)))
stopifnot(all(out_2018$major_prefix %in% names(SOC_MAJOR_GROUPS)))
cat("Both crosswalks resolve entirely within the 23 known SOC major groups -- OK\n")
