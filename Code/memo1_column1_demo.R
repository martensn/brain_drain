# memo1_column1_demo.R
#
# Column 1 analogue of Code/demographics.R's `both_demo.rds` build (L66-87):
# demographic (race/sex) probabilities for Column 1's population
# (Data/intermediate/column1_population.rds, 34,467,516 users), which
# demographics.R never covered -- it was built filtered to both_final.rds's
# user_id universe (5.0M) for the main pipeline's HS-matched sample only.
#
# Adaptation is a straight substitution of the filter population
# (both_final.rds's user_id -> column1_population.rds's user_id); the
# *_prob columns themselves are native to the raw Revelio CSV, not derived,
# so no other logic changes. Deliberately skips demographics.R's
# draw_category()-based sex_draw/race_draw stochastic sampling (L76-86) --
# Code/acs_reweight.R's existing pattern already consumes the fractional
# *_prob columns directly, never the draws, and re-running apply()-based
# per-row sampling over 34.4M rows (~7x demographics.R's original scale)
# buys nothing this memo needs.
#
# Standalone script, not part of run_pipeline.R, per this repo's
# no-source()-between-files convention.

library(data.table)
library(arrow)
library(dplyr)

library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

column1_demo_path <- file.path(data_dir, "intermediate/column1_demo.rds")

if (!file.exists(column1_demo_path)) {

log_step("memo1_column1_demo starting")

column1_population <- readRDS(file.path(data_dir, "intermediate/column1_population.rds"))
setDT(column1_population)
keep_users <- unique(column1_population[, .(user_id)])
log_step(paste("column1_population loaded:", nrow(keep_users), "unique users"))

# Same raw file demographics.R reads (L70) -- arrow::open_dataset() streams
# the scan rather than loading the whole CSV, which matters here since this
# filter is against ~7x more user_ids than demographics.R's original run.
users <- open_dataset(file.path(data_dir, "intermediate/uwdbhdlkbnzhaqxd.csv"), format = "csv")

column1_demo <- users %>% filter(user_id %in% keep_users$user_id) %>% collect()
setDT(column1_demo)
log_step(paste("filtered + collected:", nrow(column1_demo), "rows"))

stopifnot(all(c("white_prob", "black_prob", "api_prob", "hispanic_prob",
                "native_prob", "multiple_prob", "m_prob", "f_prob") %in% names(column1_demo)))

cat(sprintf("Demographic-probability coverage on Column 1: %.1f%% (%d of %d users matched)\n",
            100 * nrow(column1_demo) / nrow(keep_users), nrow(column1_demo), nrow(keep_users)))
# Compare against Column 2's ~96.5% match rate onto both_demo.rds (see
# velvet-churning-galaxy.md's Step 2 plan) -- don't assume this lands at the
# same rate; measure it.

saveRDS(column1_demo, column1_demo_path)
log_step("saved column1_demo.rds")

} else {
  log_step("column1_demo.rds already exists -- skipping")
}
