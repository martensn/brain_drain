# memo1_02d_acs_pull_1yr_occp2015.R
#
# [NEW 2026-08-21] Small supplementary ACS 1-year pull, exactly mirroring
# Code/memo1_02c_acs_pull_1yr_race2015.R's pattern (that script's own
# header explains why the MAIN 1yr pull never carries race; the same
# reasoning applies here -- occupation was simply never needed until now)
# -- OCCP + PWGTP for 2015 only, joined back onto pums_1yr_filt.rds via
# SERIALNO+SPORDER. Used to add an occupation row-group to the existing
# race/sex/region demographic cross-tab (Code/memo1_08_full_sample_extras.R
# Part B / MEMO1_WEIGHTING.md SS6.4), per Nicholas's request -- occupation
# is, like race/sex, outside Phase A/B's calibration target, so it's a
# genuine additional out-of-sample check.

library(tidycensus)
library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
tidycensus::census_api_key(Sys.getenv("CENSUS_KEY"))

FIXED_YEAR <- 2015

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

out_path <- file.path(data_dir, "intermediate/pums_1yr_occp2015.rds")
if (!file.exists(out_path)) {
  log_step(sprintf("Pulling ACS 1yr PUMS OCCP, %d only, 51 states + DC", FIXED_YEAR))
  state_list <- c(state.abb, "DC")
  parts <- vector("list", length(state_list))
  for (i in seq_along(state_list)) {
    st <- state_list[i]
    cat(sprintf("Pulling PUMS for %s (%d/%d)...\n", st, i, length(state_list)))
    pull <- get_pums(
      variables   = c("SERIALNO", "SPORDER", "OCCP"),
      state       = st,
      survey      = "acs1",
      year        = FIXED_YEAR,
      rep_weights = NULL,
      show_call   = FALSE
    )
    setDT(pull)
    parts[[i]] <- pull[, .(SERIALNO, SPORDER, OCCP)]
  }
  occp2015 <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  cat(sprintf("Pulled %d rows across %d states\n", nrow(occp2015), length(state_list)))
  saveRDS(occp2015, out_path)
} else {
  cat("pums_1yr_occp2015.rds already cached -- skipping Census API loop\n")
}
