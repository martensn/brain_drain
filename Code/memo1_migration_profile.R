# memo1_migration_profile.R
#
# Two "over time" comparisons of Revelio vs. ACS, at Nicholas's request:
# (1) year-over-year migration rate by years-since-graduation, and
# (2) share living in the top-10/50/100/all metros by years-since-graduation
#     (metro-tier piece: see Code/memo1_metro_tiers.R -- ACS side needs a
#     PUMA->CBSA crosswalk not yet in this pipeline, tracked separately).
#
# Revelio's position panel (cbsa_state_0..50, cbsa_code_0..50) is indexed by
# years since graduation already -- no new geography needed. ACS is
# cross-sectional (no per-person panel), so its "years since grad" is a
# synthetic-cohort proxy: age - 22 (nominal graduation age), using the
# standard pooled-cross-section approach for building age/cohort profiles
# out of repeated cross-sections. This is an approximation (real graduation
# age varies -- see Column 1/2's own grad_after_22/26/30 shares, built in
# Code/memo1_covariates.R, for how much it varies in the Revelio data
# itself), not a per-person measurement.
#
# Run after Code/reweight_column2.R (needs column2_reweighted.rds for the
# w_full_joint weight) and Code/memo1_covariates.R (column1_covariates.rds).
# Standalone script, no source() between files.

library(data.table)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

T_MAX <- 20  # years post-grad; truncated further below wherever n gets thin

# ---- Revelio: year-over-year mover rate at each years-since-grad checkpoint
# rate(t) = share with cbsa_state_t != cbsa_state_(t-1), among rows with
# BOTH non-missing -- pooled across the whole population's own year-t/year-
# (t-1) pair, not just each person's single most-recent pair (unlike
# Code/memo1_covariates.R's moved_last_year_state, which is one point per
# person; this is a full profile across the career).
revelio_rate_by_year <- function(dt, weight_col, t_max, source_label) {
  out <- vector("list", t_max)
  for (t in 1:t_max) {
    cur_col  <- paste0("cbsa_state_", t)
    prev_col <- paste0("cbsa_state_", t - 1)
    if (!all(c(cur_col, prev_col) %in% names(dt))) {
      out[[t]] <- data.table(source = source_label, years_since_grad = t, rate = NA_real_, n = 0L)
      next
    }
    # [FIXED 2026-08-10] weighted.mean()'s na.rm only drops NAs in the
    # value, not the weight (same gotcha already documented in
    # Code/reweight_column2.R's wmean_safe()) -- w_full_joint is NA for the
    # ~2.9% of users whose LI cell had no matching PUMS cell, and any NA
    # weight silently poisons the whole weighted.mean() to NA, not just
    # that row. `valid` must also exclude NA weight.
    weight_valid <- if (is.null(weight_col)) TRUE else !is.na(dt[[weight_col]])
    valid <- !is.na(dt[[cur_col]]) & !is.na(dt[[prev_col]]) & weight_valid
    n <- sum(valid)
    if (n == 0) {
      out[[t]] <- data.table(source = source_label, years_since_grad = t, rate = NA_real_, n = 0L)
      next
    }
    moved <- as.numeric(dt[[cur_col]][valid] != dt[[prev_col]][valid])
    w <- if (is.null(weight_col)) rep(1, n) else dt[[weight_col]][valid]
    out[[t]] <- data.table(source = source_label, years_since_grad = t,
                            rate = weighted.mean(moved, w), n = n)
  }
  rbindlist(out)
}

log_step("Loading Column 1/2 covariate tables")
column1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds"))
setDT(column1)
li <- readRDS(file.path(data_dir, "intermediate/column2_reweighted.rds"))
setDT(li)

log_step("Computing Revelio migration-rate-by-year profiles")
profile_col1        <- revelio_rate_by_year(column1, NULL, T_MAX, "Column 1 (college-only)")
profile_col2_unwt   <- revelio_rate_by_year(li, "w_unweighted", T_MAX, "Column 2 (HS+college, unweighted)")
profile_col2_rewt   <- revelio_rate_by_year(li, "w_full_joint", T_MAX, "Column 2 (reweighted to ACS)")

# ---- ACS: mover rate by single-year age, relabeled to years-since-grad
log_step("Loading ACS PUMS pull")
pums_filt <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds"))
setDT(pums_filt)

acs_rate_by_age <- function(dt, min_age, max_age) {
  ages <- min_age:max_age
  out <- vector("list", length(ages))
  for (i in seq_along(ages)) {
    a <- ages[i]
    d <- dt[age == a]
    n <- nrow(d)
    if (n == 0) {
      out[[i]] <- data.table(source = "ACS PUMS benchmark", years_since_grad = a - 22, rate = NA_real_, n = 0L)
      next
    }
    out[[i]] <- data.table(source = "ACS PUMS benchmark", years_since_grad = a - 22,
                            rate = weighted.mean(d$moved_out_of_state, d$PWGTP), n = n)
  }
  rbindlist(out)
}
profile_acs <- acs_rate_by_age(pums_filt, min_age = 23, max_age = 22 + T_MAX)

migration_profile <- rbindlist(list(profile_col1, profile_col2_unwt, profile_col2_rewt, profile_acs))

log_step("Sample sizes by years_since_grad (check where the lines get thin):")
print(dcast(migration_profile, years_since_grad ~ source, value.var = "n"))

fwrite(migration_profile, file.path(data_dir, "results/memo1_migration_rate_by_year.csv"))
saveRDS(migration_profile, file.path(data_dir, "results/memo1_migration_rate_by_year.rds"))
log_step("saved memo1_migration_rate_by_year.csv/.rds")
