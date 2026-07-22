# Runs with 50 GB on cluster
library(data.table)
library(arrow)
library(tidyr)
library(dplyr)

set.seed(49)

library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
data_dir = file.path(directory,"Data")

sex_cols  <- c("m_prob", "f_prob")
race_cols <- c("white_prob", "black_prob", "api_prob", "hispanic_prob", "native_prob","multiple_prob")
sex_labels <- c("male", "female")
race_labels <- c("white", "black", "asian", "hispanic", "native", "multiple")


state_region_crosswalk <- data.table(
  state_abbr = c(
    "CT", "ME", "MA", "NH", "RI", "VT",
    "NJ", "NY", "PA",
    "IL", "IN", "MI", "OH", "WI",
    "IA", "KS", "MN", "MO", "NE", "ND", "SD",
    "DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV",
    "AL", "KY", "MS", "TN",
    "AR", "LA", "OK", "TX",
    "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
    "AK", "CA", "HI", "OR", "WA"
  ),
  census_region = c(
    rep("Northeast", 9),
    rep("Midwest", 12),
    rep("South", 17),
    rep("West", 13)
  ),
  census_division = c(
    rep("New England", 6),
    rep("Middle Atlantic", 3),
    rep("East North Central", 5),
    rep("West North Central", 7),
    rep("South Atlantic", 9),
    rep("East South Central", 4),
    rep("West South Central", 4),
    rep("Mountain", 8),
    rep("Pacific", 5)
  )
)


draw_category <- function(probs, labels) {
  if (all(is.na(probs))) return(NA_character_)
  
  probs[is.na(probs)] <- 0
  
  if (sum(probs) <= 0) return(NA_character_)
  
  probs <- probs / sum(probs)
  
  sample(labels, size = 1, prob = probs)
}

both__ = readRDS(file.path(data_dir,"intermediate/both_final.rds"))
users = fread(file.path(data_dir,"intermediate/uwdbhdlkbnzhaqxd.csv"),nrows=10000)

users = open_dataset(file.path(data_dir,"intermediate/uwdbhdlkbnzhaqxd.csv"), format = "csv")

users_ = users %>% filter(user_id %in% both__$user_id) %>% collect()
setDT(users_)
#users_ = users[user_id %in% both__$user_id]

users_[
  ,
  sex_draw := apply(.SD, 1, draw_category, labels = sex_labels),
  .SDcols = sex_cols
]

users_[
  ,
  race_draw := apply(.SD, 1, draw_category, labels = race_labels),
  .SDcols = race_cols
]
saveRDS(users_,file.path(data_dir,"intermediate/both_demo.rds"))

final = merge(both__,users_[,.(user_id,sex_draw,race_draw)])
setDT(final)
final[,black_indicator := fifelse(race_draw == "black",1,0)]
final[,any_grad := fifelse(!is.na(master_school) | !is.na(mba_school) | !is.na(doctor_school),1,0)]

# ------------------------------------------------------------
# Helper: convert one-row wide summaries to long format
# ------------------------------------------------------------

wide_summary_to_long <- function(x, section) {
  x <- as.data.table(x)
  melt(
    x,
    measure.vars = names(x),
    variable.name = "variable",
    value.name = "value"
  )[
    , `:=`(
      section = section,
      category = NA_character_,
      stat = variable
    )
  ][
    , .(section, variable, category, stat, value)
  ]
}

# ------------------------------------------------------------
# 1. Counts
# ------------------------------------------------------------

counts <- data.table(
  n_users = final[, uniqueN(user_id)],
  n_hs    = final[, uniqueN(hs_id)],
  n_col   = final[, uniqueN(.SD), .SDcols = c("ba_unitid", "ba_opeid")]
)

counts_long <- wide_summary_to_long(counts, "counts")

# ------------------------------------------------------------
# 2. Race shares
# ------------------------------------------------------------

race_long <- final[
  , .(value = .N / nrow(final)),
  by = .(category = race_draw)
][
  , `:=`(
    section = "race",
    variable = "race_draw",
    stat = "prob"
  )
][
  , .(section, variable, category, stat, value)
]

# ------------------------------------------------------------
# 3. Sex shares
# ------------------------------------------------------------

sex_long <- final[
  , .(value = .N / nrow(final)),
  by = .(category = sex_draw)
][
  , `:=`(
    section = "sex",
    variable = "sex_draw",
    stat = "prob"
  )
][
  , .(section, variable, category, stat, value)
]

# ------------------------------------------------------------
# 4. BA / grad / transfer shares
# ------------------------------------------------------------

ba_transfer <- final[
  , .(
    prob_any_grad  = mean(any_grad == 1, na.rm = TRUE),
    prob_associate = mean(!is.na(associate_school)),
    prob_transfer  = mean(!is.na(ba_transfer_school)),
    prob_master    = mean(!is.na(master_school)),
    prob_mba       = mean(!is.na(mba_school)),
    prob_doctor    = mean(!is.na(doctor_school))
  )
]

ba_transfer_long <- wide_summary_to_long(ba_transfer, "ba_transfer")

# ------------------------------------------------------------
# 5. HS counts: distribution of users per high school
# ------------------------------------------------------------

hs_counts <- final[
  , .N, by = hs_id
][
  , .(
    p01 = quantile(N, 0.01, na.rm = TRUE),
    p10 = quantile(N, 0.10, na.rm = TRUE),
    p25 = quantile(N, 0.25, na.rm = TRUE),
    p50 = quantile(N, 0.50, na.rm = TRUE),
    p75 = quantile(N, 0.75, na.rm = TRUE),
    p90 = quantile(N, 0.90, na.rm = TRUE),
    p99 = quantile(N, 0.99, na.rm = TRUE)
  )
]

hs_counts_long <- wide_summary_to_long(hs_counts, "hs_counts")

# ------------------------------------------------------------
# 6. College counts: distribution of users per BA institution
# ------------------------------------------------------------

col_counts <- final[
  , .N, by = .(ba_unitid, ba_opeid)
][
  , .(
    p01 = quantile(N, 0.01, na.rm = TRUE),
    p10 = quantile(N, 0.10, na.rm = TRUE),
    p25 = quantile(N, 0.25, na.rm = TRUE),
    p50 = quantile(N, 0.50, na.rm = TRUE),
    p75 = quantile(N, 0.75, na.rm = TRUE),
    p90 = quantile(N, 0.90, na.rm = TRUE),
    p99 = quantile(N, 0.99, na.rm = TRUE)
  )
]

col_counts_long <- wide_summary_to_long(col_counts, "col_counts")

# ------------------------------------------------------------
# 7. Graduation year quantiles
# ------------------------------------------------------------

final[, ba_year := as.integer(format(ba_end, "%Y"))]

grad_counts <- final[
  , .(
    p01 = quantile(ba_year, 0.01, na.rm = TRUE),
    p10 = quantile(ba_year, 0.10, na.rm = TRUE),
    p25 = quantile(ba_year, 0.25, na.rm = TRUE),
    p50 = quantile(ba_year, 0.50, na.rm = TRUE),
    p75 = quantile(ba_year, 0.75, na.rm = TRUE),
    p90 = quantile(ba_year, 0.90, na.rm = TRUE),
    p99 = quantile(ba_year, 0.99, na.rm = TRUE)
  )
]

grad_counts_long <- wide_summary_to_long(grad_counts, "grad_counts")

# ------------------------------------------------------------
# 8. Census region shares
# ------------------------------------------------------------

regions <- merge(
  final,
  state_region_crosswalk,
  by.x = "ba_state",
  by.y = "state_abbr",
  all.x = TRUE
)

regions_long <- regions[
  , .(value = .N / nrow(final)),
  by = .(category = census_region)
][
  , `:=`(
    section = "regions",
    variable = "census_region",
    stat = "prob"
  )
][
  , .(section, variable, category, stat, value)
]

# Optional: Census division too
divisions_long <- regions[
  , .(value = .N / nrow(final)),
  by = .(category = census_division)
][
  , `:=`(
    section = "divisions",
    variable = "census_division",
    stat = "prob"
  )
][
  , .(section, variable, category, stat, value)
]

# ------------------------------------------------------------
# 9. Final exportable summary table
# ------------------------------------------------------------

sumstat <- rbindlist(
  list(
    counts_long,
    race_long,
    sex_long,
    ba_transfer_long,
    grad_counts_long,
    hs_counts_long,
    col_counts_long,
    regions_long,
    divisions_long
  ),
  use.names = TRUE,
  fill = TRUE
)

# Inspect
sumstat[]

# Export
fwrite(sumstat, file.path(data_dir,"results/summary_demographics.csv"))
saveRDS(sumstat, file.path(data_dir,"results/summary_demographics.rds"))