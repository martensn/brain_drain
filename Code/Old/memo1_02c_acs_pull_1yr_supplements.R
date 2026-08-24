# memo1_02c_acs_pull_1yr_supplements.R
#
# [CONSOLIDATED 2026-08-23, per Nicholas's harmonization request] Replaces
# three near-duplicate small ACS 1-year pull scripts written incrementally
# over the course of Memo 1 (each copy-pasting the prior one's
# state-loop/retry pattern with a different variable list and year range):
#   - memo1_02c_acs_pull_1yr_race2015.R  -> RAC2P/SEX/HISP, 2015 only
#   - memo1_02d_acs_pull_1yr_occp2015.R  -> OCCP, 2015 only
#   - memo1_02e_acs_pull_1yr_occp_allyears.R -> OCCP, all CALIB_YEARS
# One parameterized puller (pull_acs_years()) now backs all three -- adding
# a future supplemental variable/year range is a call to that function, not
# a fourth copy-pasted script. Output filenames are UNCHANGED (multiple
# downstream scripts read them by name): pums_1yr_race2015.rds,
# pums_1yr_occp2015.rds, pums_1yr_occp_allyears.rds.
#
# Preserves each original script's real behavior, not just its shape:
#   - Race/sex derivation (RAC2P/HISP -> race, SEX -> sex) is 2015-specific
#     and NOT part of the generic puller -- 2015's RAC2P is unpadded but
#     uses the same numeric scheme memo1_02a's derivation already handles
#     (verified live against a DC trial pull, per the original script);
#     this is NOT claimed to generalize to other years.
#   - The OCCP all-years pull reuses the 2015 OCCP pull's own output
#     instead of re-hitting the Census API for a year already pulled.
#   - Per-year checkpointing for the multi-year OCCP pull (one raw file per
#     year) so an interrupted run never re-touches an already-pulled year.

library(tidycensus)
library(httr)
library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
tidycensus::census_api_key(Sys.getenv("CENSUS_KEY"))
httr::set_config(httr::timeout(120))

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

VINTAGE_WINDOWS <- list(`2010` = setdiff(2012:2021, 2020), `2020` = 2022:2023)
CALIB_YEARS <- sort(unlist(VINTAGE_WINDOWS, use.names = FALSE))
STATE_LIST <- c(state.abb, "DC")

## =========================================================================
## Generic single-year, 51-state(+DC) PUMS puller with retry -- backs all
## three supplements below. `variables` is passed straight to get_pums();
## get_pums() always returns SERIALNO/SPORDER/PWGTP by default alongside
## whatever's requested.
## =========================================================================
pull_one_state_year <- function(st, year, variables, max_attempts = 6) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch({
      d <- get_pums(variables = variables, state = st, survey = "acs1", year = year, show_call = FALSE)
      setDT(d)
      d
    }, error = function(e) {
      cat(sprintf("  %s %d attempt %d/%d FAILED: %s\n", st, year, attempt, max_attempts, conditionMessage(e)))
      NULL
    })
    if (!is.null(result)) return(result)
    if (attempt < max_attempts) Sys.sleep(10 * attempt)
  }
  stop(sprintf("get_pums() failed for state=%s year=%d after %d attempts", st, year, max_attempts))
}

pull_acs_year <- function(year, variables) {
  log_step(sprintf("Pulling ACS 1yr PUMS %s for %d (%d states)", paste(variables, collapse = "/"), year, length(STATE_LIST)))
  parts <- vector("list", length(STATE_LIST))
  for (i in seq_along(STATE_LIST)) {
    st <- STATE_LIST[i]
    cat(sprintf("  [%d/%d] %s...", i, length(STATE_LIST), st)); flush(stdout())
    t0 <- Sys.time()
    pull <- pull_one_state_year(st, year, variables)
    cat(sprintf(" %d rows (%.0fs)\n", nrow(pull), as.numeric(Sys.time() - t0, units = "secs"))); flush(stdout())
    parts[[i]] <- pull
  }
  d <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  cat(sprintf("Total: %d rows across %d states (expect 51+DC=52)\n", nrow(d), uniqueN(d$ST)))
  d
}

## =========================================================================
## Supplement 1: RAC2P/SEX/HISP, 2015 only -> pums_1yr_race2015.rds
## (was memo1_02c_acs_pull_1yr_race2015.R)
## =========================================================================
race2015_path <- file.path(data_dir, "intermediate/pums_1yr_race2015.rds")
if (!file.exists(race2015_path)) {
  d <- pull_acs_year(2015, c("ST", "RAC2P", "SEX", "HISP", "SERIALNO", "SPORDER"))

  # [VERIFIED LIVE, original build] 2015's RAC2P is NOT zero-padded
  # ("1","15","2", not "01","15","02") but uses the same underlying numeric
  # scheme memo1_02a_acs_pull_5yr.R's derivation already handles -- this is
  # 2015-specific, not claimed to generalize to other years.
  d[, rac2p_int := as.integer(RAC2P)]
  d[, hisp_int := as.integer(HISP)]
  d[, race := fcase(
    hisp_int != 1L, "hispanic",
    rac2p_int == 1L, "white",
    rac2p_int == 2L, "black",
    rac2p_int >= 3L & rac2p_int <= 37L, "native",
    rac2p_int >= 38L & rac2p_int <= 66L, "asian",
    default = "multiple"
  )]
  d[, sex := fifelse(as.integer(SEX) == 1L, "male", "female")]

  cat("Race distribution (sanity check, no single category ~100%):\n")
  print(d[, .N, by = race][, share := N / sum(N)][order(-share)])
  cat("Sex distribution:\n")
  print(d[, .N, by = sex])

  race2015 <- d[, .(SERIALNO, SPORDER, race, sex)]
  saveRDS(race2015, race2015_path)
  log_step(paste("saved pums_1yr_race2015.rds:", nrow(race2015), "rows"))
} else {
  cat("pums_1yr_race2015.rds already cached -- skipping Census API loop\n")
}

## =========================================================================
## Supplement 2: OCCP, 2015 only -> pums_1yr_occp2015.rds
## (was memo1_02d_acs_pull_1yr_occp2015.R)
## =========================================================================
occp2015_path <- file.path(data_dir, "intermediate/pums_1yr_occp2015.rds")
if (!file.exists(occp2015_path)) {
  d <- pull_acs_year(2015, c("SERIALNO", "SPORDER", "OCCP"))
  occp2015 <- d[, .(SERIALNO, SPORDER, OCCP)]
  saveRDS(occp2015, occp2015_path)
  log_step(paste("saved pums_1yr_occp2015.rds:", nrow(occp2015), "rows"))
} else {
  cat("pums_1yr_occp2015.rds already cached -- skipping Census API loop\n")
}

## =========================================================================
## Supplement 3: OCCP, all CALIB_YEARS -> pums_1yr_occp_allyears.rds
## (was memo1_02e_acs_pull_1yr_occp_allyears.R). Reuses Supplement 2's own
## 2015 pull instead of a redundant API call; per-year checkpointing so an
## interrupted run never re-touches an already-pulled year.
## =========================================================================
allyears_path <- file.path(data_dir, "intermediate/pums_1yr_occp_allyears.rds")
raw_dir <- file.path(data_dir, "intermediate/pums_1yr_occp_raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

for (y in CALIB_YEARS) {
  raw_path_y <- file.path(raw_dir, sprintf("pums_1yr_occp_raw_%d.rds", y))
  if (file.exists(raw_path_y)) {
    cat(sprintf("Year %d already pulled -- skipping\n", y))
    next
  }
  if (y == 2015 && file.exists(occp2015_path)) {
    log_step("Year 2015 -- reusing Supplement 2's existing pull instead of a redundant API call")
    d2015 <- readRDS(occp2015_path); setDT(d2015)
    d2015[, survey_year := 2015L]
    saveRDS(d2015, raw_path_y)
    next
  }
  year_dt <- pull_acs_year(y, c("SERIALNO", "SPORDER", "OCCP"))
  year_dt <- year_dt[, .(SERIALNO, SPORDER, OCCP)]
  year_dt[, survey_year := y]
  saveRDS(year_dt, raw_path_y)
}

log_step("Combining per-year OCCP raw checkpoints")
raw_files <- list.files(raw_dir, pattern = "^pums_1yr_occp_raw_\\d{4}\\.rds$", full.names = TRUE)
stopifnot(length(raw_files) == length(CALIB_YEARS))
occp_all <- rbindlist(lapply(raw_files, readRDS), use.names = TRUE, fill = TRUE)
occp_all[, `:=`(SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER))]

cat("Per-year row counts:\n")
print(occp_all[, .N, by = survey_year][order(survey_year)])

saveRDS(occp_all, allyears_path)
log_step(sprintf("Wrote pums_1yr_occp_allyears.rds: %d rows, %d years", nrow(occp_all), uniqueN(occp_all$survey_year)))
