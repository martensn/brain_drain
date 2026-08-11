# reweight_column2.R
#
# Builds the single full-joint-cell ACS raking weight for Column 2
# (Data/intermediate/column2_covariates.rds), then assembles the memo's
# four-column characteristics table: Column 1 (unweighted), Column 2
# (unweighted), Column 2 (reweighted to ACS), and the raw ACS PUMS
# benchmark -- race, sex, age, geography, transfer status, graduate-degree
# attainment.
#
# Scope, per velvet-churning-galaxy.md's Step 2: ONE reweighting scheme
# (state x age-bucket x race/sex, full joint cell), not the 5-rung ladder
# Code/acs_reweight.R built for an earlier, now-superseded purpose (a JOLE
# referee response). This script reimplements that scheme's logic using
# merge()-based joins throughout (not data.table bracket-chaining) --
# Code/acs_reweight.R's own Sections 6-9 were written but, per HANDOFF.md,
# never actually run; rather than blindly copy unverified bracket-join
# chains, the same conceptual logic is rebuilt here with join semantics
# that are unambiguous by construction.
#
# Run after Code/acs_pull.R (pums_cells.rds/pums_acs5_filt.rds) and
# Code/memo1_covariates.R (column1_covariates.rds/column2_covariates.rds)
# have both completed. Standalone script, no source() between files.

library(data.table)
library(survey)
library(tidycensus)
library(dotenv)
library(here)

load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}

RACE_LABELS <- c("white", "black", "asian", "native", "multiple", "hispanic")
RACE_PROB_COLS <- c("white_prob", "black_prob", "api_prob",
                     "native_prob", "multiple_prob", "hispanic_prob")

# table.express note: every join below uses merge()/data.table [i,j] on
# plain data.tables, never a bare dplyr verb -- safe regardless of whether
# table.express is attached elsewhere in this session.


## =========================================================================
## PART 1: build the full-joint-cell raking weight
## =========================================================================

li <- readRDS(file.path(data_dir, "intermediate/column2_covariates.rds"))
setDT(li)
stopifnot(all(c("user_id", "hs_state", "age_bucket", RACE_PROB_COLS, "m_prob", "f_prob") %in% names(li)))

# [FIXED 2026-08-10] column2_covariates.rds on disk still predates
# Code/memo1_covariates.R's transfer/has_associate coercion fix (that fix
# only takes effect on a future full rebuild, since this checkpoint already
# exists) -- coerce here too so THIS run's table is consistent without
# forcing an ~hour-long memo1_covariates.R rerun. Safe to leave in even
# after a future rebuild: as.integer() on an already-integer 0/1 column is
# a no-op.
if (is.character(li$transfer)) {
  stopifnot(all(unique(li$transfer) %in% c("Non-Transfer", "Transfer")))
  li[, transfer := as.integer(transfer == "Transfer")]
}
li[, `:=`(
  has_associate = as.integer(has_associate),
  has_master    = as.integer(has_master),
  has_mba       = as.integer(has_mba),
  has_doctor    = as.integer(has_doctor)
)]

pums_cells <- readRDS(file.path(data_dir, "intermediate/pums_cells.rds"))
cell_state_age_race_sex <- pums_cells$state_age_racesex

# hs_state -> FIPS crosswalk: PUMS's origin_state (built in acs_pull.R) is a
# numeric-string FIPS code; Column 2's hs_state is a 2-letter postal
# abbreviation (confirmed directly against the live file, same format
# Code/acs_reweight.R's Section 1 already assumed for the old sample).
data(fips_codes, package = "tidycensus")
abb_to_fips <- unique(fips_codes[, c("state", "state_code")])
setDT(abb_to_fips)
setnames(abb_to_fips, c("state", "state_code"), c("state_abbr", "state_fips"))

li <- merge(li, abb_to_fips, by.x = "hs_state", by.y = "state_abbr", all.x = TRUE)
setnames(li, "state_fips", "origin_state")
cat(sprintf("hs_state -> FIPS match rate: %.1f%%\n", 100 * mean(!is.na(li$origin_state))))

li[, w_unweighted := 1]

# Fractional race-probability row-expansion: each person contributes
# RACE_PROB_COLS as soft mass across the 6 race categories (not a single
# stochastic draw, which would inject pure noise into the raking target) --
# same design Code/acs_reweight.R's Section 6 used.
li_complete <- li[!is.na(origin_state) & !is.na(age_bucket) & !is.na(white_prob)]
log_step(paste("li_complete (non-missing origin_state/age_bucket/race_prob):", nrow(li_complete), "of", nrow(li)))

li_long <- melt(
  li_complete,
  id.vars = setdiff(names(li_complete), RACE_PROB_COLS),
  measure.vars = RACE_PROB_COLS,
  variable.name = "race_prob_col",
  value.name = "race_frac"
)
race_col_to_label <- setNames(RACE_LABELS, RACE_PROB_COLS)
li_long[, race := race_col_to_label[as.character(race_prob_col)]]
# sex taken as the higher-probability category rather than fractionally
# split -- m_prob/f_prob are typically near-degenerate; spot-check
# summary(pmin(li$m_prob, li$f_prob)) before trusting this at scale.
li_long[, sex := fifelse(m_prob >= f_prob, "male", "female")]
li_long[, w_base := w_unweighted * race_frac]

li_long_n <- li_long[, .(n_li = sum(race_frac)), by = .(origin_state, age_bucket, race, sex)]

joint_ratio <- merge(li_long_n, cell_state_age_race_sex,
                      by = c("origin_state", "age_bucket", "race", "sex"), all.x = TRUE)
joint_ratio[, ratio := pop / n_li]
cat(sprintf("%d of %d LI cells have no matching PUMS cell (ratio will be NA there):\n",
            sum(is.na(joint_ratio$ratio)), nrow(joint_ratio)))

li_long <- merge(li_long, joint_ratio[, .(origin_state, age_bucket, race, sex, ratio)],
                  by = c("origin_state", "age_bucket", "race", "sex"), all.x = TRUE)
li_long[, w_full_joint := w_base * ratio]

w_full_collapsed <- li_long[, .(w_full_joint = sum(w_full_joint, na.rm = TRUE)), by = user_id]
li <- merge(li, w_full_collapsed, by = "user_id", all.x = TRUE)

for (wc in c("w_unweighted", "w_full_joint")) {
  rng <- range(li[[wc]], na.rm = TRUE)
  cat(sprintf("%-15s range: [%.3f, %.3f]  (flag if outside ~0.05x-20x)\n", wc, rng[1], rng[2]))
}

saveRDS(li, file.path(data_dir, "intermediate/column2_reweighted.rds"))
log_step(paste("saved column2_reweighted.rds:", nrow(li), "rows"))


## =========================================================================
## PART 2: diagnostics -- Kish N + benchmark convergence
## =========================================================================

kish_n <- function(w) {
  w <- w[!is.na(w)]
  (sum(w))^2 / sum(w^2)
}

wtd_var <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  x <- x[keep]; w <- w[keep]
  mu <- weighted.mean(x, w)
  sum(w * (x - mu)^2) / sum(w)
}

wmean_safe <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  if (!any(keep)) return(NA_real_)
  weighted.mean(x[keep], w[keep])
}

pums_filt <- readRDS(file.path(data_dir, "intermediate/pums_acs5_filt.rds"))
# [FIXED 2026-08-10] pums_acs5_filt.rds on disk predates acs_pull.R's
# logical->integer grad_degree fix -- coerce here too (no-op if already
# integer), same rationale as li's transfer/has_associate fix above.
pums_filt[, grad_degree := as.integer(grad_degree)]

# moved_proxy: reuse Column 2's own `in_state` factor (06_finalize_data.Rmd
# L89, col_state == hs_state) rather than recomputing from scratch -- NOT a
# 1-year retrospective flag the way PUMS's MIGSP-based measure is (it's a
# static, one-time transition), so this validates that reweighting moves
# Column 2's composition toward the ACS benchmark, not that the two rates
# measure literally the same event.
stopifnot("in_state" %in% names(li))
li[, moved_proxy := as.integer(in_state == "Out-of-State")]

acs_benchmark_rate <- pums_filt[, weighted.mean(moved_out_of_state, PWGTP)]
pums_rep_design <- svrepdesign(
  variables  = pums_filt[, .(moved_out_of_state)],
  weights    = pums_filt$PWGTP,
  repweights = pums_filt[, paste0("PWGTP", 1:80), with = FALSE],
  type = "JK1", scale = 4 / 80, rscales = rep(1, 80), mse = TRUE
)
acs_benchmark_ci <- confint(svymean(~moved_out_of_state, pums_rep_design))

build_diag_row <- function(w_col, label) {
  d <- li[!is.na(get(w_col)) & !is.na(moved_proxy)]
  w <- d[[w_col]]
  m <- weighted.mean(d$moved_proxy, w)
  se <- sqrt(wtd_var(d$moved_proxy, w) / kish_n(w))
  data.table(
    scheme = label, overall_rate = m, ci_lo = m - 1.96 * se, ci_hi = m + 1.96 * se,
    n_unweighted = nrow(d), n_eff_kish = kish_n(w)
  )
}

diagnostics_table <- rbindlist(list(
  build_diag_row("w_unweighted", "Unweighted"),
  build_diag_row("w_full_joint", "Full joint cell"),
  data.table(scheme = "ACS PUMS benchmark", overall_rate = acs_benchmark_rate,
             ci_lo = acs_benchmark_ci[1], ci_hi = acs_benchmark_ci[2],
             n_unweighted = nrow(pums_filt), n_eff_kish = NA_real_)
))
print(diagnostics_table)
fwrite(diagnostics_table, file.path(data_dir, "results/reweight_diagnostics_column2.csv"))
saveRDS(diagnostics_table, file.path(data_dir, "results/reweight_diagnostics_column2.rds"))

unweighted_gap <- abs(diagnostics_table[scheme == "Unweighted", overall_rate] - acs_benchmark_rate)
full_joint_gap <- abs(diagnostics_table[scheme == "Full joint cell", overall_rate] - acs_benchmark_rate)
if (full_joint_gap < unweighted_gap) {
  cat(sprintf("\nGATE CHECK PASS: full-joint gap (%.4f) < unweighted gap (%.4f) -- reweighting moves toward the ACS benchmark.\n",
              full_joint_gap, unweighted_gap))
} else {
  warning(sprintf("GATE CHECK FAIL: full-joint gap (%.4f) >= unweighted gap (%.4f) -- reweighting is NOT moving toward the benchmark. Treat un-raked characteristics below as unreliable until this is debugged.",
                   full_joint_gap, unweighted_gap))
}


## =========================================================================
## PART 3: four-column characteristics table
## =========================================================================
## Long format: column_label / variable / category / value. Race/sex are
## weighted shares (fractional prob-weighted for Revelio columns, PWGTP-
## weighted categorical for ACS); age_bucket/region/grad-degree/transfer
## are weighted shares of their respective categories. Two known,
## deliberate asymmetries, not bugs: (1) Column 1 has no HS-side geography
## (college-side only); (2) ACS has no transfer-status analog (left NA).

column1 <- readRDS(file.path(data_dir, "intermediate/column1_covariates.rds"))
setDT(column1)
# [FIXED 2026-08-10] same has_associate/has_master/has_mba/has_doctor
# coercion as li's, above (column1_covariates.rds on disk predates the
# memo1_covariates.R fix).
column1[, `:=`(
  has_associate = as.integer(has_associate),
  has_master    = as.integer(has_master),
  has_mba       = as.integer(has_mba),
  has_doctor    = as.integer(has_doctor)
)]
# ACS rows below are built directly from pums_filt (already in scope from
# Part 2) rather than acs_pull.R's pums_benchmark_summary.rds, so every
# column in this table uses the same weighting machinery (categorical_share_rows).

long_row <- function(column_label, variable, category, value, n_eff = NA_real_) {
  data.table(column_label = column_label, variable = variable, category = as.character(category), value = value, n_eff = n_eff)
}

# ---- race/sex, Revelio columns (fractional, prob-weighted) --------------
# race_tot's names are RACE_PROB_COLS; mapped to RACE_LABELS explicitly via
# race_col_to_label (built in Part 1) rather than relying on positional
# order, to avoid a silent mislabeling bug.
revelio_race_sex_rows <- function(dt, weight_col, column_label) {
  w <- if (is.null(weight_col)) rep(1, nrow(dt)) else dt[[weight_col]]
  race_tot <- sapply(RACE_PROB_COLS, function(cc) sum(dt[[cc]] * w, na.rm = TRUE))
  names(race_tot) <- race_col_to_label[names(race_tot)]
  race_share <- race_tot / sum(race_tot)
  sex_tot <- c(male = sum(dt$m_prob * w, na.rm = TRUE), female = sum(dt$f_prob * w, na.rm = TRUE))
  sex_share <- sex_tot / sum(sex_tot)
  rbindlist(list(
    long_row(column_label, "race", names(race_share), unname(race_share)),
    long_row(column_label, "sex", names(sex_share), unname(sex_share))
  ))
}

# ---- categorical share helper, for age/region/grad-degree/transfer ------
categorical_share_rows <- function(dt, group_col, weight_col, column_label, variable_label) {
  if (!(group_col %in% names(dt))) return(NULL)
  if (is.null(weight_col)) {
    tab <- dt[, .(n = .N), by = c(group_col)]
  } else {
    tab <- dt[, .(n = sum(get(weight_col), na.rm = TRUE)), by = c(group_col)]
  }
  tab <- tab[!is.na(get(group_col))]
  tab[, share := n / sum(n)]
  long_row(column_label, variable_label, tab[[group_col]], tab$share)
}

# ---- Migration/graduation-timing helper: weighted mean of a continuous
# column (used for yrs_away_col, conditional on having left the college
# metro at all -- "duration away" isn't meaningful for people who stayed).
mean_value_row <- function(dt, value_col, weight_col, column_label, variable_label) {
  w <- if (is.null(weight_col)) rep(1, nrow(dt)) else dt[[weight_col]]
  v <- dt[[value_col]]
  keep <- !is.na(v) & !is.na(w)
  if (!any(keep)) return(NULL)
  m <- weighted.mean(v[keep], w[keep])
  long_row(column_label, variable_label, "mean", m)
}

# migrated_past_year: placed under the SAME variable name on both the
# Revelio and ACS columns so they sit together in the table -- but see the
# "Known asymmetries" note below before treating them as the same measure.
# Revelio's version is a person's own most-recent-observed-year-pair change
# (variable time gap, whatever the panel happens to cover); ACS's is a
# literal fixed 1-year retrospective question.

characteristics_table <- rbindlist(list(
  # Column 1: unweighted
  revelio_race_sex_rows(column1, NULL, "Column 1 (college-only)"),
  categorical_share_rows(column1, "age_bucket", NULL, "Column 1 (college-only)", "age_bucket"),
  categorical_share_rows(column1, "col_region", NULL, "Column 1 (college-only)", "region"),
  categorical_share_rows(column1, "transfer", NULL, "Column 1 (college-only)", "transfer"),
  categorical_share_rows(column1, "any_grad", NULL, "Column 1 (college-only)", "grad_degree"),
  categorical_share_rows(column1, "has_associate", NULL, "Column 1 (college-only)", "has_associate"),
  categorical_share_rows(column1, "grad_after_22", NULL, "Column 1 (college-only)", "grad_after_22"),
  categorical_share_rows(column1, "grad_after_26", NULL, "Column 1 (college-only)", "grad_after_26"),
  categorical_share_rows(column1, "grad_after_30", NULL, "Column 1 (college-only)", "grad_after_30"),
  categorical_share_rows(column1, "college_metro_status", NULL, "Column 1 (college-only)", "college_metro_status"),
  mean_value_row(column1[college_metro_status != "Stayed"], "yrs_away_col", NULL,
                 "Column 1 (college-only)", "yrs_away_col_mean_if_left"),
  categorical_share_rows(column1, "moved_last_year_state", NULL, "Column 1 (college-only)", "migrated_past_year"),

  # Column 2: unweighted
  revelio_race_sex_rows(li, "w_unweighted", "Column 2 (HS+college, unweighted)"),
  categorical_share_rows(li, "age_bucket", "w_unweighted", "Column 2 (HS+college, unweighted)", "age_bucket"),
  categorical_share_rows(li, "hs_region", "w_unweighted", "Column 2 (HS+college, unweighted)", "hs_region"),
  categorical_share_rows(li, "col_region", "w_unweighted", "Column 2 (HS+college, unweighted)", "col_region"),
  categorical_share_rows(li, "transfer", "w_unweighted", "Column 2 (HS+college, unweighted)", "transfer"),
  categorical_share_rows(li, "any_grad", "w_unweighted", "Column 2 (HS+college, unweighted)", "grad_degree"),
  categorical_share_rows(li, "has_associate", "w_unweighted", "Column 2 (HS+college, unweighted)", "has_associate"),
  categorical_share_rows(li, "grad_after_22", "w_unweighted", "Column 2 (HS+college, unweighted)", "grad_after_22"),
  categorical_share_rows(li, "grad_after_26", "w_unweighted", "Column 2 (HS+college, unweighted)", "grad_after_26"),
  categorical_share_rows(li, "grad_after_30", "w_unweighted", "Column 2 (HS+college, unweighted)", "grad_after_30"),
  categorical_share_rows(li, "college_metro_status", "w_unweighted", "Column 2 (HS+college, unweighted)", "college_metro_status"),
  mean_value_row(li[college_metro_status != "Stayed"], "yrs_away_col", "w_unweighted",
                 "Column 2 (HS+college, unweighted)", "yrs_away_col_mean_if_left"),
  categorical_share_rows(li, "moved_last_year_state", "w_unweighted", "Column 2 (HS+college, unweighted)", "migrated_past_year"),

  # Column 2: reweighted to ACS
  revelio_race_sex_rows(li, "w_full_joint", "Column 2 (reweighted to ACS)"),
  categorical_share_rows(li, "age_bucket", "w_full_joint", "Column 2 (reweighted to ACS)", "age_bucket"),
  categorical_share_rows(li, "hs_region", "w_full_joint", "Column 2 (reweighted to ACS)", "hs_region"),
  categorical_share_rows(li, "col_region", "w_full_joint", "Column 2 (reweighted to ACS)", "col_region"),
  categorical_share_rows(li, "transfer", "w_full_joint", "Column 2 (reweighted to ACS)", "transfer"),
  categorical_share_rows(li, "any_grad", "w_full_joint", "Column 2 (reweighted to ACS)", "grad_degree"),
  categorical_share_rows(li, "has_associate", "w_full_joint", "Column 2 (reweighted to ACS)", "has_associate"),
  categorical_share_rows(li, "grad_after_22", "w_full_joint", "Column 2 (reweighted to ACS)", "grad_after_22"),
  categorical_share_rows(li, "grad_after_26", "w_full_joint", "Column 2 (reweighted to ACS)", "grad_after_26"),
  categorical_share_rows(li, "grad_after_30", "w_full_joint", "Column 2 (reweighted to ACS)", "grad_after_30"),
  categorical_share_rows(li, "college_metro_status", "w_full_joint", "Column 2 (reweighted to ACS)", "college_metro_status"),
  mean_value_row(li[college_metro_status != "Stayed"], "yrs_away_col", "w_full_joint",
                 "Column 2 (reweighted to ACS)", "yrs_away_col_mean_if_left"),
  categorical_share_rows(li, "moved_last_year_state", "w_full_joint", "Column 2 (reweighted to ACS)", "migrated_past_year"),

  # ACS PUMS benchmark -- categorical (not fractional), PWGTP-weighted;
  # no transfer-status/associate's-degree/college-metro/grad-age analog
  # (left absent, not zero-filled -- ACS respondents aren't tied to a
  # college, and its BA+ filter excludes associate's-only respondents).
  categorical_share_rows(pums_filt, "race", "PWGTP", "ACS PUMS benchmark", "race"),
  categorical_share_rows(pums_filt, "sex", "PWGTP", "ACS PUMS benchmark", "sex"),
  categorical_share_rows(pums_filt, "age_bucket", "PWGTP", "ACS PUMS benchmark", "age_bucket"),
  categorical_share_rows(pums_filt, "census_region", "PWGTP", "ACS PUMS benchmark", "region"),
  categorical_share_rows(pums_filt, "grad_degree", "PWGTP", "ACS PUMS benchmark", "grad_degree"),
  categorical_share_rows(pums_filt, "moved_out_of_state", "PWGTP", "ACS PUMS benchmark", "migrated_past_year")
), fill = TRUE)

print(characteristics_table)

# [CHANGED 2026-08-10] wide is now the primary output -- one real column per
# column_label (Column 1 / Column 2 unweighted / Column 2 reweighted / ACS
# benchmark), so values sit side by side for direct comparison instead of
# needing a pivot to actually read. The long form is kept too, under
# _long.csv/.rds, for any downstream code that wants to reshape it further.
column_order <- c("Column 1 (college-only)", "Column 2 (HS+college, unweighted)",
                   "Column 2 (reweighted to ACS)", "ACS PUMS benchmark")
characteristics_table_wide <- dcast(characteristics_table, variable + category ~ column_label, value.var = "value")
setcolorder(characteristics_table_wide, c("variable", "category", intersect(column_order, names(characteristics_table_wide))))

print(characteristics_table_wide)
fwrite(characteristics_table_wide, file.path(data_dir, "results/memo1_characteristics_table.csv"))
saveRDS(characteristics_table_wide, file.path(data_dir, "results/memo1_characteristics_table.rds"))
fwrite(characteristics_table, file.path(data_dir, "results/memo1_characteristics_table_long.csv"))
saveRDS(characteristics_table, file.path(data_dir, "results/memo1_characteristics_table_long.rds"))

cat("\nKnown asymmetries in this table (state plainly in any write-up, not fix):\n")
cat("  - Column 1 has no HS-side geography (hs_region) -- college-side (col_region) only.\n")
cat("  - ACS PUMS has no transfer-status/associate's-degree/college-metro/grad-age analog.\n")
cat("  - Column 2's `moved_proxy`/table above is a static one-time transition; ACS's moved_out_of_state\n")
cat("    is 1-year retrospective -- not the same event, only used to validate reweighting direction.\n")
cat("  - `migrated_past_year`: ACS's version is a literal fixed 1-year retrospective question;\n")
cat("    Revelio's is each person's own most-recent-observed-year-pair change, which could span more\n")
cat("    or less than a year depending on gaps in their position history -- structurally comparable,\n")
cat("    not the identical measure. Check moved_last_year_state's coverage rate (printed above by\n")
cat("    Code/memo1_covariates.R) before leaning on this row -- non-adjacent last-observed pairs are NA.\n")
cat("  - `yrs_away_col_mean_if_left` is a headcount of mismatch-years within the first 10 years\n")
cat("    post-grad (deliberately NOT this repo's existing yr_return_col_cbsa, which finds the\n")
cat("    first-ever match year and conflates day-one non-movers with genuine returners -- see\n")
cat("    HANDOFF.md's Decisions).\n")

cat("\nreweight_column2.R done.\n")
