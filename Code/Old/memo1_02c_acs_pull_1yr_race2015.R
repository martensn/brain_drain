# memo1_02c_acs_pull_1yr_race2015.R
#
# [NEW 2026-08-13] Code/memo1_02b_acs_pull_1yr.R deliberately did NOT pull race/sex --
# its own header documents why (RAC2P's coding scheme drifts across ACS
# vintages, a real verification burden that chart didn't need to take on).
# Nicholas now wants a demographic (race/sex/region) cross-tab comparing
# weighting schemes at a fixed calendar year (2015), which DOES need ACS's
# race/sex composition for that one year -- so this is a small, targeted
# supplementary pull: RAC2P/SEX/HISP for survey year 2015 only, all 51
# states + DC, joined back onto the already-pulled pums_1yr_filt_<cohort>.rds
# via SERIALNO+SPORDER (ACS's own person-level unique identifiers, present
# on that file already via get_pums()'s default columns).
#
# [VERIFIED LIVE before writing the real pull, not assumed] A DC trial pull
# for 2015 confirmed: RAC2P is NOT zero-padded that year ("1","15","2", not
# "01","15","02") but uses the SAME underlying numeric scheme
# Code/memo1_02a_acs_pull_5yr.R's derivation already handles (values observed: 1,2,15-59,
# 60-68 -- consistent with 01=White/02=Black/03-37=AIAN/38-66=Asian-NHPI/
# 67=SOR/68=Two+). Reusing acs_pull.R's exact thresholds after
# as.integer() coercion (never string comparison, same discipline as every
# other geography/codebook variable in this project) is therefore valid for
# this one year -- this script does NOT claim that scheme is stable across
# OTHER years, only 2015, which is all it pulls.
#
# Standalone, no source() between files. Run once; produces
# Data/intermediate/pums_1yr_race2015.rds.

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

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

YEAR <- 2015
state_list <- c(state.abb, "DC")

pull_one_state <- function(st, year, variables, max_attempts = 6) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch({
      d <- get_pums(variables = variables, state = st, survey = "acs1", year = year, show_call = FALSE)
      setDT(d)
      d
    }, error = function(e) {
      cat(sprintf("  %s attempt %d/%d FAILED: %s\n", st, attempt, max_attempts, conditionMessage(e)))
      NULL
    })
    if (!is.null(result)) return(result)
    if (attempt < max_attempts) Sys.sleep(10 * attempt)
  }
  stop(sprintf("get_pums() failed for state=%s year=%d after %d attempts", st, year, max_attempts))
}

log_step(sprintf("Pulling ACS 1-year PUMS RAC2P/SEX/HISP for %d (%d states)", YEAR, length(state_list)))
parts <- vector("list", length(state_list))
for (i in seq_along(state_list)) {
  st <- state_list[i]
  cat(sprintf("  [%d/%d] %s...", i, length(state_list), st)); flush(stdout())
  t0 <- Sys.time()
  pull <- pull_one_state(st, YEAR, c("ST", "RAC2P", "SEX", "HISP", "SERIALNO", "SPORDER"))
  cat(sprintf(" %d rows (%.0fs)\n", nrow(pull), as.numeric(Sys.time() - t0, units = "secs"))); flush(stdout())
  parts[[i]] <- pull
}
d <- rbindlist(parts, use.names = TRUE, fill = TRUE)
cat(sprintf("Total: %d rows across %d states (expect 51)\n", nrow(d), uniqueN(d$ST)))

# Same derivation as Code/memo1_02a_acs_pull_5yr.R (RAC2P/HISP -> race, SEX -> sex),
# as.integer() throughout -- never string comparison, since this year's
# RAC2P isn't zero-padded (confirmed live above).
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

out <- d[, .(SERIALNO, SPORDER, race, sex)]
saveRDS(out, file.path(data_dir, "intermediate/pums_1yr_race2015.rds"))
log_step(paste("saved pums_1yr_race2015.rds:", nrow(out), "rows"))
