# memo1_02e_acs_pull_1yr_occp_allyears.R
#
# [NEW 2026-08-23] Generalizes memo1_02d_acs_pull_1yr_occp2015.R's
# lightweight supplement pattern (SERIALNO, SPORDER, OCCP only -- NOT a
# full re-pull of memo1_02b's heavier multi-variable file) across every
# calendar year Stage 2's flow calibration actually uses, so occupation
# can become a real per-year calibration margin
# (Code/memo1_09_reweight_column2_occupation.R) instead of the single
# fixed-2015 diagnostic snapshot occupation_crosstab.R used.
#
# CALIB_YEARS mirrors memo1_08_full_sample_extras.R's VINTAGE_WINDOWS
# exactly (2012-2019 + 2021 on 2010-vintage PUMA, 2022-2023 on 2020-vintage)
# -- 2020 excluded for the same COVID-era reason memo1_02b excludes it.
# 2015 is NOT re-pulled from the Census API -- memo1_02d already pulled it
# (pums_1yr_occp2015.rds); reused here as that year's checkpoint instead of
# spending a redundant 51-state API pull.
#
# Per-year checkpointing (one raw file per year, same philosophy as
# memo1_02b) so an interrupted run never re-touches an already-pulled year.
#
# get_pums() always returns SERIALNO/SPORDER/PWGTP by default alongside any
# requested `variables`, even though they aren't listed explicitly here --
# confirmed by memo1_02d's identical pattern already relying on this.
# Output stays a SEPARATE file (not merged onto pums_1yr_filt.rds), keyed
# by (survey_year, SERIALNO, SPORDER), matching how the 2015 supplement was
# kept separate.

library(tidycensus)
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

state_list <- c(state.abb, "DC")
raw_dir <- file.path(data_dir, "intermediate/pums_1yr_occp_raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

pull_one_state <- function(st, year, max_attempts = 6) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch({
      d <- get_pums(variables = c("SERIALNO", "SPORDER", "OCCP"), state = st, survey = "acs1", year = year, show_call = FALSE)
      setDT(d)
      d[, .(SERIALNO, SPORDER, OCCP)]
    }, error = function(e) {
      cat(sprintf("  %s %d attempt %d/%d FAILED: %s\n", st, year, attempt, max_attempts, conditionMessage(e)))
      NULL
    })
    if (!is.null(result)) return(result)
    if (attempt < max_attempts) Sys.sleep(10 * attempt)
  }
  stop(sprintf("get_pums() failed for state=%s year=%d after %d attempts", st, year, max_attempts))
}

existing_2015 <- file.path(data_dir, "intermediate/pums_1yr_occp2015.rds")
for (y in CALIB_YEARS) {
  raw_path_y <- file.path(raw_dir, sprintf("pums_1yr_occp_raw_%d.rds", y))
  if (file.exists(raw_path_y)) {
    cat(sprintf("Year %d already pulled -- skipping\n", y))
    next
  }
  if (y == 2015 && file.exists(existing_2015)) {
    log_step("Year 2015 -- reusing memo1_02d's existing pull instead of a redundant API call")
    d2015 <- readRDS(existing_2015); setDT(d2015)
    d2015[, survey_year := 2015L]
    saveRDS(d2015, raw_path_y)
    next
  }

  log_step(sprintf("Pulling ACS 1yr OCCP for %d (%d states)", y, length(state_list)))
  year_parts <- vector("list", length(state_list))
  for (i in seq_along(state_list)) {
    st <- state_list[i]
    cat(sprintf("  [%d/%d] %s...", i, length(state_list), st)); flush(stdout())
    t0 <- Sys.time()
    pull <- pull_one_state(st, y)
    cat(sprintf(" %d rows (%.0fs)\n", nrow(pull), as.numeric(Sys.time() - t0, units = "secs"))); flush(stdout())
    year_parts[[i]] <- pull
  }
  year_dt <- rbindlist(year_parts, use.names = TRUE, fill = TRUE)
  year_dt[, survey_year := y]
  cat(sprintf("  %d: %d rows across %d states (expect 51)\n", y, nrow(year_dt), length(state_list)))
  saveRDS(year_dt, raw_path_y)
}

log_step("Combining per-year raw checkpoints")
raw_files <- list.files(raw_dir, pattern = "^pums_1yr_occp_raw_\\d{4}\\.rds$", full.names = TRUE)
stopifnot(length(raw_files) == length(CALIB_YEARS))
occp_all <- rbindlist(lapply(raw_files, readRDS), use.names = TRUE, fill = TRUE)
occp_all[, `:=`(SERIALNO = as.character(SERIALNO), SPORDER = as.character(SPORDER))]

cat("Per-year row counts:\n")
print(occp_all[, .N, by = survey_year][order(survey_year)])

saveRDS(occp_all, file.path(data_dir, "intermediate/pums_1yr_occp_allyears.rds"))
log_step(sprintf("Wrote pums_1yr_occp_allyears.rds: %d rows, %d years", nrow(occp_all), uniqueN(occp_all$survey_year)))
