# acs_cbsa_population_benchmark.R
#
# [NEW 2026-08-22] Standalone descriptive output, NOT part of Memo 1's own
# write-up. Nicholas's request: total ACS-weighted (PWGTP) count of college
# graduates satisfying Memo 1's standard sample requirements (BA+,
# employed, under 65 -- Code/memo1_02a/02b's own restriction, already
# baked into pums_1yr_filt.rds -- no new pull needed), by destination CBSA
# and calendar year. Purpose, per his own framing: a true CBSA-level
# population benchmark to compare against the college-level w2 table
# (Code/college_origin_destination_counts.R) summed up to CBSA, to gauge
# how much CBSA-level error remains that an institution-blind flow
# calibration wouldn't catch -- Phase B's own calibration target is the
# coarser 3-tier x region cell (12 cells), not individual CBSA identity,
# so real CBSA-level slack is possible even where the tier-level
# calibration looks clean.
#
# PUMA -> CBSA: same raw, population-weighted (not tier-collapsed)
# crosswalk construction as Code/nativity_profile_creation.R, built fresh
# here for BOTH PUMA vintages (2010 for survey years 2012-2021, 2020 for
# 2022-2023) from the same tract-level population tables, not re-derived
# differently. No race/sex split -- just total population per CBSA per
# year, per the request ("total college graduates... per CBSA").

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

log_step("Loading pums_1yr_filt.rds (already BA+/employed/under-65 -- Memo 1's standard sample requirements)")
pums_1yr <- readRDS(file.path(data_dir, "intermediate/pums_1yr_filt.rds")); setDT(pums_1yr)
cat("Year range available:", paste(range(pums_1yr$survey_year), collapse = "-"), "\n")
cat("Years present:", paste(sort(unique(pums_1yr$survey_year)), collapse = ", "), "\n")

## ---- raw PUMA -> CBSA population-share crosswalks, both vintages ----
build_puma_cbsa_xwalk <- function(tract_path) {
  tp <- readRDS(tract_path); setDT(tp)
  x <- tp[, .(cbsa_pop = sum(tract_pop)), by = .(state_puma, cbsa_code)]
  x[, cbsa_share := cbsa_pop / sum(cbsa_pop), by = state_puma]
  x
}
log_step("Building PUMA->CBSA crosswalks (2010 and 2020 vintage)")
puma_cbsa_2010 <- build_puma_cbsa_xwalk(file.path(data_dir, "intermediate/tract_puma_cbsa_pop_2010.rds"))
puma_cbsa_2020 <- build_puma_cbsa_xwalk(file.path(data_dir, "intermediate/tract_puma_cbsa_pop_2020.rds"))
cat(sprintf("2010-vintage: %d distinct CBSAs | 2020-vintage: %d distinct CBSAs\n",
            uniqueN(puma_cbsa_2010$cbsa_code), uniqueN(puma_cbsa_2020$cbsa_code)))

## ---- fractionally assign each respondent-year to CBSA(s), by vintage ----
assign_cbsa <- function(pums, years, puma_col, xwalk) {
  d <- pums[survey_year %in% years & !is.na(get(puma_col))]
  d[, state_puma := paste0(ST, get(puma_col))]
  merge(d[, .(survey_year, state_puma, PWGTP)], xwalk[, .(state_puma, cbsa_code, cbsa_share)],
        by = "state_puma", all.x = TRUE, allow.cartesian = TRUE)
}

years_2010vintage <- sort(unique(pums_1yr$survey_year))[sort(unique(pums_1yr$survey_year)) <= 2021]
years_2020vintage <- sort(unique(pums_1yr$survey_year))[sort(unique(pums_1yr$survey_year)) >= 2022]
cat(sprintf("Using 2010-vintage crosswalk for: %s\n", paste(years_2010vintage, collapse = ", ")))
cat(sprintf("Using 2020-vintage crosswalk for: %s\n", paste(years_2020vintage, collapse = ", ")))

log_step("Assigning CBSA (fractional) for each vintage window")
long_2010 <- assign_cbsa(pums_1yr, years_2010vintage, "PUMA10", puma_cbsa_2010)
long_2020 <- assign_cbsa(pums_1yr, years_2020vintage, "PUMA20", puma_cbsa_2020)
long_all <- rbindlist(list(long_2010, long_2020), use.names = TRUE)
long_all <- long_all[!is.na(cbsa_code)]

## ---- aggregate: total ACS-weighted population per CBSA per year ----
log_step("Aggregating by destination CBSA x calendar year")
out <- long_all[, .(acs_weighted_n = sum(PWGTP * cbsa_share)), by = .(destination_cbsa = cbsa_code, calendar_year = survey_year)]
setorder(out, destination_cbsa, calendar_year)

fwrite(out, file.path(data_dir, "results/acs_cbsa_population_benchmark.csv"))
log_step("Wrote acs_cbsa_population_benchmark.csv")

cat(sprintf("\nTotal rows (CBSA x year): %d\n", nrow(out)))
cat(sprintf("Distinct CBSAs: %d | distinct years: %d\n", uniqueN(out$destination_cbsa), uniqueN(out$calendar_year)))
cat("\nTop 10 CBSA-year cells by ACS-weighted population:\n")
print(head(out[order(-acs_weighted_n)], 10))

## ---- pooled total per CBSA, across all years -- handy for a quick
## Revelio-vs-ACS comparison without dealing with year alignment first ----
pooled <- out[, .(acs_weighted_n_avg_per_year = mean(acs_weighted_n), n_years = .N), by = destination_cbsa]
setorder(pooled, -acs_weighted_n_avg_per_year)
fwrite(pooled, file.path(data_dir, "results/acs_cbsa_population_benchmark_pooled.csv"))
cat("\nTop 10 CBSAs by average annual ACS-weighted population:\n")
print(head(pooled, 10))
