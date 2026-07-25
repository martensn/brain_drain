library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(educationdata)

library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
data_dir = file.path(directory,"Data")

#------------------------------------------------------------
# 1. Precompute high-school candidate tables
#------------------------------------------------------------
prep_hs_candidates <- function(schools, state_fips, verbose = TRUE) {
  if (verbose) cat("prep_hs_candidates: start\n")
  
  schools <- as.data.table(copy(schools))
  state_fips <- as.data.table(copy(state_fips))
  
  if (verbose) {
    cat("  input school rows:", nrow(schools), "\n")
    cat("  unique hs_name:", uniqueN(schools$university_name), "\n")
  }
  
  schools <- merge(
    schools,
    unique(state_fips[, .(state_abbr, state_fips)]),
    by = "state_abbr",
    all.x = TRUE,
    sort = FALSE
  )
  
  if (verbose) {
    cat("  after state merge rows:", nrow(schools), "\n")
    cat("  missing state_fips:", schools[, sum(is.na(state_fips))], "\n")
  }
  
  schools <- schools[
    !is.na(university_name) &
      !is.na(hs_id) &
      !is.na(state_fips) &
      !is.na(n_grads) &
      n_grads > 0
  ]
  
  if (verbose) {
    cat("  usable school rows:", nrow(schools), "\n")
  }
  
  out <- schools[
    ,
    .(
      hs_agency_id = first(na.omit(hs_agency_id)),
      n_grads = sum(n_grads, na.rm = TRUE)
    ),
    by = .(hs_string=university_name, hs_id, state_fips)
  ]
  
  if (verbose) {
    cat("  candidate school rows:", nrow(out), "\n")
    cat("  unique hs_string:", uniqueN(out$hs_string), "\n")
    cat("prep_hs_candidates: done\n")
  }
  
  setkey(out, hs_string)
  out
}

prep_unit_state_probs <- function(instate, colleges, state_fips, alpha = 0.7, verbose = TRUE) {
  if (verbose) cat("prep_unit_state_probs: start\n")
  
  instate <- as.data.table(copy(instate))
  colleges <- as.data.table(copy(colleges))
  state_fips <- as.data.table(copy(state_fips))
  
  colleges <- unique(colleges[, .(unitid, opeid, col_state)])
  
  if (verbose) {
    cat("  unique colleges:", nrow(colleges), "\n")
    cat("  unique unitids:", uniqueN(colleges$unitid), "\n")
  }
  
  state_map <- unique(state_fips[, .(state_abbr, state_fips)])
  setnames(state_map, c("state_abbr", "state_fips"), c("col_state", "col_fips"))
  
  colleges <- merge(
    colleges,
    state_map,
    by = "col_state",
    all.x = TRUE,
    sort = FALSE
  )
  
  if (verbose) {
    cat("  colleges after col_state->col_fips merge:", nrow(colleges), "\n")
    cat("  missing col_fips:", colleges[, sum(is.na(col_fips))], "\n")
  }
  
  unit_state_obs <- instate[
    !is.na(unitid) &
      !is.na(state_of_residence) &
      state_of_residence != 58 &
      !is.na(enrollment_fall) &
      enrollment_fall > 0,
    .(n = sum(enrollment_fall, na.rm = TRUE)),
    by = .(unitid, state_fips = state_of_residence)
  ]
  
  unit_state_obs[, state_prob := n / sum(n), by = unitid]
  unit_state_obs[, imputed_flow := 0L]
  
  if (verbose) {
    cat("  observed unit-state rows:", nrow(unit_state_obs), "\n")
    cat("  units with observed flows:", uniqueN(unit_state_obs$unitid), "\n")
  }
  
  observed_units <- unique(unit_state_obs$unitid)
  missing_units <- setdiff(unique(colleges$unitid), observed_units)
  
  if (verbose) {
    cat("  units missing flows:", length(missing_units), "\n")
  }
  
  national_state <- instate[
    !is.na(state_of_residence) &
      state_of_residence != 58 &
      !is.na(enrollment_fall) &
      enrollment_fall > 0,
    .(n = sum(enrollment_fall, na.rm = TRUE)),
    by = .(state_fips = state_of_residence)
  ]
  national_state[, p_nat := n / sum(n)]
  
  if (verbose) {
    cat("  national state rows:", nrow(national_state), "\n")
  }
  
  if (length(missing_units) > 0L) {
    if (verbose) cat("  imputing missing unit-state flows\n")
    
    missing_dt <- colleges[unitid %in% missing_units]
    missing_dt[, cross_key := 1L]
    national_state[, cross_key := 1L]
    
    unit_state_imp <- merge(
      missing_dt,
      national_state,
      by = "cross_key",
      allow.cartesian = TRUE,
      sort = FALSE
    )
    
    unit_state_imp[
      ,
      raw_p := alpha * (state_fips == col_fips) + (1 - alpha) * p_nat
    ]
    unit_state_imp[, state_prob := raw_p / sum(raw_p), by = unitid]
    unit_state_imp <- unit_state_imp[, .(unitid, state_fips, state_prob, imputed_flow = 1L)]
    
    if (verbose) {
      cat("  imputed unit-state rows:", nrow(unit_state_imp), "\n")
    }
  } else {
    unit_state_imp <- unit_state_obs[0, .(unitid, state_fips, state_prob, imputed_flow)]
  }
  
  out <- rbindlist(
    list(
      unit_state_obs[, .(unitid, state_fips, state_prob, imputed_flow)],
      unit_state_imp
    ),
    use.names = TRUE
  )
  
  if (verbose) {
    cat("  final unit-state rows:", nrow(out), "\n")
    cat("prep_unit_state_probs: done\n")
  }
  
  setkey(out, unitid, state_fips)
  out
}

match_ambiguous_hs <- function(users, hs_candidates, unit_state, verbose = TRUE, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (verbose) cat("match_ambiguous_hs_probabilistic: start\n")
  
  users <- as.data.table(copy(users))
  hs_candidates <- as.data.table(copy(hs_candidates))
  unit_state <- as.data.table(copy(unit_state))
  
  req <- c("user_id", "hs_string", "unitid")
  miss <- setdiff(req, names(users))
  if (length(miss) > 0) stop("Missing required user columns: ", paste(miss, collapse = ", "))
  
  users[, row_id := .I]
  
  if (verbose) {
    cat("  input users:", nrow(users), "\n")
    cat("  unique hs_string:", uniqueN(users$hs_string), "\n")
    cat("  unique unitid:", uniqueN(users$unitid), "\n")
  }
  
  cand <- merge(
    users,
    hs_candidates,
    by = "hs_string",
    all.x = TRUE,
    allow.cartesian = TRUE,
    sort = FALSE
  )
  
  if (verbose) {
    cat("  after joining to candidate schools:", nrow(cand), "\n")
    cat("  users with any candidate school:", uniqueN(cand[!is.na(hs_id)]$row_id), "\n")
  }
  
  cand <- merge(
    cand,
    unit_state,
    by = c("unitid", "state_fips"),
    all.x = TRUE,
    sort = FALSE
  )
  
  cand[
    ,
    score := fifelse(
      !is.na(state_prob) & !is.na(n_grads),
      state_prob * n_grads,
      NA_real_
    )
  ]
  
  cand <- cand[!is.na(score) & score > 0]
  
  if (verbose) {
    cat("  rows with positive score:", nrow(cand), "\n")
    cat("  users with any scored candidate:", uniqueN(cand$row_id), "\n")
  }
  
  if (nrow(cand) == 0L) {
    out <- copy(users)
    out[
      ,
      `:=`(
        hs_id = NA_integer_,
        hs_agency_id = NA_integer_,
        matched_state_fips = NA_integer_,
        hs_n_grads = NA_real_,
        best_state_prob = NA_real_,
        score = NA_real_,
        draw_prob = NA_real_,
        imputed_flow = NA_integer_,
        hs_match_status = "no_feasible_match"
      )
    ]
    out[, row_id := NULL]
    return(out[])
  }
  
  cand[
    ,
    draw_prob := score / sum(score),
    by = row_id
  ]
  
  if (verbose) {
    cat("  drawing one candidate per user\n")
  }
  
  chosen <- cand[
    ,
    .SD[sample.int(.N, 1L, prob = draw_prob)],
    by = row_id
  ]
  
  out <- merge(
    users,
    chosen[, .(
      row_id,
      hs_id,
      hs_agency_id,
      matched_state_fips = state_fips,
      hs_n_grads = n_grads,
      best_state_prob = state_prob,
      score,
      draw_prob,
      imputed_flow
    )],
    by = "row_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  out[
    ,
    hs_match_status := fcase(
      is.na(hs_string), "missing_hs_string",
      is.na(unitid), "missing_unitid",
      is.na(score), "no_feasible_match",
      default = "matched"
    )
  ]
  
  if (verbose) {
    cat("  final matched:", out[, sum(hs_match_status == "matched")], "\n")
    cat("  final unmatched:", out[, sum(hs_match_status != "matched")], "\n")
    cat("match_ambiguous_hs_probabilistic: done\n")
  }
  
  out[, row_id := NULL]
  out[]
}

# Helper: earliest row by user within one or more level values
pick_earliest_level <- function(dt, levels, prefix) {
  tmp <- copy(dt)[level %chin% levels & !is.na(enddate)]
  if (nrow(tmp) == 0L) {
    return(data.table(user_id = integer()))
  }
  
  setorder(tmp, user_id, enddate)
  
  out <- tmp[
    ,
    .SD[1],
    by = user_id
  ][
    ,
    setNames(
      .(
        user_id,
        university_name,
        degree,
        enddate,
        extra_opeid,
        extra_unitid
      ),
      c(
        "user_id",
        paste0(prefix, "_school"),
        paste0(prefix, "_degree"),
        paste0(prefix, "_end"),
        paste0(prefix, "_opeid"),
        paste0(prefix, "_unitid")
      )
    )
  ]
  
  out
}

input_ <- fread(file.path(data_dir,"raw/revelio/2026.04.09_education.csv"), 
                select=c("user_id","university_name","enddate","rsid","degree"))
setDT(input_)

col_rsid_crosswalk <- readRDS(file.path(data_dir,"intermediate/col_rsid_crosswalk.rds"))
cc_rsid_crosswalk <- readRDS(file.path(data_dir,"intermediate/cc_rsid_crosswalk.rds"))
hs_rsid_crosswalk <- readRDS(file.path(data_dir,"intermediate/hs_rsid_crosswalk.rds"))
setDT(col_rsid_crosswalk)
setDT(cc_rsid_crosswalk)
setDT(hs_rsid_crosswalk)

# Subset to users with both high school and colleges
input_hs <- input_[
  hs_rsid_crosswalk,
  on = .(university_name, rsid),
  nomatch = 0
][
  , .SD[which.max(enddate)], by = user_id
][
  , .(
    user_id,
    hs_string = university_name,
    hs_rsid = rsid,
    hs_end = enddate,
    hs_id,
    hs_ambiguous_name = ambiguous_name,
    hs_match_type = match_type
  )
]

input_col <- input_[
  !degree %in% c("Associate", "Doctor", "Master", "MBA")
][
  col_rsid_crosswalk,
  on = .(university_name, rsid),
  nomatch = 0
][
  , .SD[which.max(enddate)], by = user_id
][
  , .(
    user_id,
    col_string = university_name,
    col_rsid = rsid,
    col_end = enddate,
    degree,
    col_opeid = opeid,
    col_unitid = unitid,
    col_system_indicator = system_indicator,
    col_ambiguous_name = ambiguous_name,
    col_match_type = string_method,
    col_state = state_abbr
  )
]
both_ = input_col[input_hs, on = .(user_id), nomatch = 0]
saveRDS(both_,file.path(data_dir,"intermediate/both_orig.rds"))
saveRDS(input_col,file.path(data_dir,"intermediate/input_col_m.rds"))

# Convert enddate to year to improve computation
#both_[,hs_en := as.integer(substr(enddate,1,4))]
both_ = readRDS(file.path(data_dir,"intermediate/both_orig.rds"))
input_col = readRDS(file.path(data_dir,"intermediate/input_col_m.rds"))

keep_users <- unique(both_[, .(user_id)])

extra_ed <- input_[
  keep_users,
  on = "user_id",
  nomatch = 0
][
  !both_,
  on = .(user_id, rsid=col_rsid,degree)
][
  !both_,
  on = .(user_id, rsid=hs_rsid)
]

col_transfer <- col_rsid_crosswalk[
  extra_ed,
  on = c("rsid","university_name"),
  nomatch = 0
]

cc_transfer <- cc_rsid_crosswalk[
  extra_ed,
  on = c("rsid","university_name"),
  nomatch = 0
]

# Deduplicate extra college rows
col_transfer_dedup <- col_transfer[
  col_rsid_crosswalk[, .(rsid, extra_opeid = opeid, extra_unitid = unitid)],
  on = "rsid",
  nomatch = 0
][
  , degree_clean := fifelse(is.na(degree), "", trimws(degree))
][
  , degree_rank := fcase(
    grepl("bachelor", tolower(degree_clean)), 1L,
    degree_clean != "", 2L,
    default = 3L
  )
][
  order(user_id, enddate, extra_opeid, extra_unitid, degree_rank)
][
  , .SD[1], by = .(user_id, enddate, extra_opeid, extra_unitid)
][
  , c("degree_clean", "degree_rank") := NULL
]

# Join extra rows to built HS/college data
colts <- col_transfer_dedup[, .(
  user_id, university_name, degree, enddate, state_abbr, extra_opeid, extra_unitid
)][
  both_,
  on = "user_id",
  nomatch = 0
][
  , .(
    user_id,
    university_name,
    degree,
    enddate,
    state_abbr,
    extra_opeid,
    extra_unitid,
    #hs_string,
    hs_end,
    #hs_id,
    col_string,
    col_opeid,
    col_unitid,
    col_end,
    col_degree = i.degree,
    col_state
  )
][
  , `:=`(
    degree_ba = as.integer(grepl("bachelor", tolower(fifelse(is.na(degree), "", degree)))),
    col_degree_ba = as.integer(grepl("bachelor", tolower(fifelse(is.na(col_degree), "", col_degree)))),
    yrs_post_hs = year(enddate) - year(hs_end),
    yrs_post_col = year(enddate) - year(col_end),
    #yrs_post_hs = as.integer(substr(enddate,1,4)) - as.integer(substr(hs_end,1,4)),
    #yrs_post_col = as.integer(substr(enddate,1,4)) - as.integer(substr(col_end,1,4)),
    col_mislabel = fifelse((year(enddate) - year(col_end)) < 0 & (year(enddate) - year(hs_end)) %in% 4:6, 1L, 0L)
  )
][col_degree %in% c("Bachelor","") & degree %in% c("Bachelor","")]

# Earliest LI record in the 4-6 year post-HS window
earliest_li <- colts[
  yrs_post_hs %in% 4:6,
  .SD[which.min(enddate)],
  by = user_id
][
  , .(
    user_id,
    li_earliest_university_name = university_name,
    li_earliest_degree = degree,
    li_earliest_enddate = enddate,
    li_earliest_opeid = extra_opeid,
    li_earliest_unitid = extra_unitid,
    li_earliest_state = state_abbr
  )
]

colts[earliest_li, on = "user_id", `:=`(
  li_earliest_university_name = i.li_earliest_university_name,
  li_earliest_degree = i.li_earliest_degree,
  li_earliest_enddate = i.li_earliest_enddate,
  li_earliest_opeid = i.li_earliest_opeid,
  li_earliest_unitid = i.li_earliest_unitid,
  li_earliest_state = i.li_earliest_state
)]

# Decide which BA source wins
colts[, chosen_source := fcase(
  col_mislabel == 1L & degree_ba == 1L & col_degree_ba == 0L, "extra_ba",
  col_mislabel == 1L & degree_ba == 0L & col_degree_ba == 1L, "og_ba",
  col_mislabel == 1L & !is.na(li_earliest_university_name),   "earliest",
  default = "default"
)]

# Collapse to one corrected BA record per user
ba_corrected <- colts[, .SD[1], by = user_id]

ba_corrected[
  , `:=`(
    ba_school = fcase(
      chosen_source == "extra_ba", as.character(university_name),
      chosen_source == "og_ba", as.character(col_string),
      chosen_source == "earliest", as.character(li_earliest_university_name),
      chosen_source == "default" & !is.na(li_earliest_university_name), as.character(li_earliest_university_name),
      default = NA_character_
    ),
    ba_degree = fcase(
      chosen_source == "extra_ba", as.character(degree),
      chosen_source == "og_ba", as.character(col_degree),
      chosen_source == "earliest", as.character(li_earliest_degree),
      chosen_source == "default" & !is.na(li_earliest_degree), as.character(li_earliest_degree),
      default = NA_character_
    ),
    ba_state = fcase(
      chosen_source == "extra_ba", as.character(state_abbr),
      chosen_source == "og_ba", as.character(col_state),
      chosen_source == "earliest", as.character(li_earliest_state),
      chosen_source == "default" & !is.na(li_earliest_state), as.character(li_earliest_state),
      default = NA_character_
    ),
    ba_end = fcase(
      chosen_source == "extra_ba", enddate,
      chosen_source == "og_ba", col_end,
      chosen_source == "earliest", li_earliest_enddate,
      chosen_source == "default" & !is.na(li_earliest_enddate), li_earliest_enddate,
      rep(TRUE, .N), as.IDate(col_end)
    ),
    ba_opeid = fcase(
      chosen_source == "extra_ba", as.character(extra_opeid),
      chosen_source == "og_ba", as.character(col_opeid),
      chosen_source == "earliest", as.character(li_earliest_opeid),
      chosen_source == "default" & !is.na(li_earliest_opeid), as.character(li_earliest_opeid),
      default = NA_character_
    ),
    
    ba_unitid = fcase(
      chosen_source == "extra_ba", as.character(extra_unitid),
      chosen_source == "og_ba", as.character(col_unitid),
      chosen_source == "earliest", as.character(li_earliest_unitid),
      chosen_source == "default" & !is.na(li_earliest_unitid), as.character(li_earliest_unitid),
      rep(TRUE, .N), as.character(col_unitid)
    )
  )
]

ba_corrected[
  , ba_corrected_flag := fcase(
    chosen_source == "extra_ba", 1L,
    chosen_source == "og_ba", 0L,
    chosen_source == "earliest" & ba_unitid == as.character(col_unitid), 0L,
    chosen_source == "earliest" & ba_unitid != as.character(col_unitid), 1L,
    chosen_source == "default" & ba_unitid == as.character(col_unitid), 0L,
    chosen_source == "default" & ba_unitid != as.character(col_unitid), 1L,
    default = NA_integer_
  )
]

ba_corrected = ba_corrected[
  , .(
    user_id,
    ba_school,
    ba_degree,
    ba_state,
    ba_end,
    ba_opeid,
    ba_unitid,
    chosen_source,
    ba_corrected_flag
  )
]

# Create updated college file
ba <- rbindlist(
  list(
    both_[!ba_corrected, on = "user_id"][
      ,
      .(
        user_id,
        ba_school = col_string,
        ba_degree = degree,
        ba_end = col_end,
        ba_opeid = col_opeid,
        ba_unitid = col_unitid,
        ba_state = col_state,
        chosen_source = "default",
        ba_corrected_flag = 0L
      )
    ],
    ba_corrected
  ),
  use.names = TRUE,
  fill = TRUE
)

# Existing 4-year extra matches
col_transfer_dedup <- col_transfer[
  col_rsid_crosswalk[, .(rsid, extra_opeid = opeid, extra_unitid = unitid)],
  on = "rsid",
  nomatch = 0
][
  , degree_clean := fifelse(is.na(degree), "", trimws(degree))
][
  , degree_rank := fcase(
    grepl("bachelor", tolower(degree_clean)), 1L,
    degree_clean != "", 2L,
    default = 3L
  )
][
  order(user_id, enddate, extra_opeid, extra_unitid, degree_rank)
][
  , .SD[1], by = .(user_id, enddate, extra_opeid, extra_unitid)
][
  , source := "col"
][
  , c("degree_clean", "degree_rank") := NULL
]

# New CC-based extra matches
cc_transfer_dedup <- cc_transfer[
  cc_rsid_crosswalk[, .(rsid, extra_opeid = opeid, extra_unitid = unitid)],
  on = "rsid",
  nomatch = 0
][
  , degree_clean := fifelse(is.na(degree), "", trimws(degree))
][
  , degree_rank := fcase(
    degree_clean == "Associate", 1L,
    degree_clean != "", 2L,
    default = 3L
  )
][
  order(user_id, enddate, extra_opeid, extra_unitid, degree_rank)
][
  , .SD[1], by = .(user_id, enddate, extra_opeid, extra_unitid)
][
  , source := "cc"
][
  , c("degree_clean", "degree_rank") := NULL
]

extra_ed_long <- rbindlist(
  list(
    col_transfer_dedup[, .(user_id, university_name, degree, enddate, extra_opeid, extra_unitid, source)],
    cc_transfer_dedup[, .(user_id, university_name, degree, enddate, extra_opeid, extra_unitid, source)]
  ),
  use.names = TRUE,
  fill = TRUE
)

extra_levels <- extra_ed_long[
  both_[, .(user_id, hs_end)],
  on = "user_id",
  nomatch = 0
][
  ba[, .(user_id, ba_end, ba_opeid, ba_unitid, ba_school)],
  on = "user_id",
  nomatch = 0
][
  , ba_duplicate := fifelse(
    extra_opeid == ba_opeid & extra_unitid == ba_unitid & degree == "Bachelor",
    1L, 0L
  )
][
  ba_duplicate == 0L
][
  ,
  level := fcase(
    degree == "Associate" & enddate <  ba_end, "associate",
    degree == "Associate" & enddate >= ba_end, "post_ba_associate",
    degree == "Bachelor"  & enddate <  ba_end, "ba_transfer",
    degree == "Bachelor"  & enddate >= ba_end, "second_ba",
    degree == "Doctor"    & enddate <  ba_end, "pre_ba_doctor",
    degree == "Doctor"    & enddate >= ba_end, "doctor",
    degree == "Master"    & enddate <  ba_end, "pre_ba_master",
    degree == "Master"    & enddate >= ba_end, "master",
    degree == "MBA"       & enddate <  ba_end, "pre_ba_mba",
    degree == "MBA"       & enddate >= ba_end, "mba",
    source == "col" & degree == "" & enddate <  ba_end, "ba_transfer_imp",
    source == "col" & degree == "" & enddate >= ba_end, "master_imp",
    source == "cc"  & degree == "", "associate_imp",
    degree == "Associate" & is.na(enddate), "associate",
    degree == "Bachelor"  & is.na(enddate), "ba_transfer",
    degree == "Doctor"    & is.na(enddate), "doctor",
    degree == "Master"    & is.na(enddate), "master",
    degree == "MBA"       & is.na(enddate), "mba",    
    default = "ba_transfer_imp"
  )
]

# High school table
hs_dt <- unique(
  both_[,
        .(
          user_id,
          hs_string,
          hs_degree = "High School",
          hs_end = hs_end,
          hs_id = hs_id,
          hs_ambiguous_name
        )
  ],
  by = "user_id"
)

ba_transfer_dt <- pick_earliest_level(
  extra_levels,
  levels = c("ba_transfer","ba_transfer_imp"),
  prefix = "ba_transfer"
)

associate_dt <- pick_earliest_level(
  extra_levels,
  levels = c("associate", "associate_imp"),
  prefix = "associate"
)

master_dt <- pick_earliest_level(
  extra_levels,
  levels = c("master", "master_imp"),
  prefix = "master"
)

mba_dt <- pick_earliest_level(
  extra_levels,
  levels = c("mba"),
  prefix = "mba"
)

doctor_dt <- pick_earliest_level(
  extra_levels,
  levels = c("doctor"),
  prefix = "doctor"
)

both <- Reduce(
  function(x, y) y[x, on = "user_id"],
  list(
    hs_dt,
    ba,
    ba_transfer_dt,
    associate_dt,
    master_dt,
    mba_dt,
    doctor_dt
    # , post_ba_associate_dt
    # , second_ba_dt
    # , ba_transfer_dt
  )
)

# Keep only users with required HS + BA information

setcolorder(
  both,
  c(
    "user_id",
    "hs_string", "hs_degree", "hs_end", "hs_id", "hs_ambiguous_name",
    "ba_school", "ba_degree", "ba_end", "ba_opeid", "ba_unitid","ba_state",
    "ba_transfer_school", "ba_transfer_degree", "ba_transfer_end", "ba_transfer_opeid", "ba_transfer_unitid",
    "chosen_source", "ba_corrected_flag",
    "associate_school", "associate_degree", "associate_end", "associate_opeid", "associate_unitid",
    "master_school", "master_degree", "master_end", "master_opeid", "master_unitid",
    "mba_school", "mba_degree", "mba_end", "mba_opeid", "mba_unitid",
    "doctor_school", "doctor_degree", "doctor_end", "doctor_opeid", "doctor_unitid"
  )
)


#sample = both[grepl("(University of (Michigan|Wisconsin|Southern California)|New York University)",col_string,ignore.case=TRUE) & hs_ambiguous_name == 1]
sample = both[hs_ambiguous_name == 1]
setDT(sample)

schools = readRDS(file.path(data_dir,"intermediate/schools.rds"))
colleges = readRDS(file.path(data_dir,"intermediate/colleges.rds"))

# Create state fips crosswalk
state_fips = readRDS(file.path(data_dir,"intermediate/state_fips.rds"))

# Pull degrees awarded data
# Only pull even years because reporting isn't mandatory in odd years
raw_instate = fread(file.path(data_dir,"raw/ipeds/colleges_ipeds_fall-res.csv")) %>%
  filter(type_of_freshman == 99, fips %in% c(1:56), state_of_residence %in% c(1:58), year %in% c(1994,1996,1998,2000,2002,2004,2006,2008,2010))
instate = raw_instate %>% filter(unitid %in% colleges$unitid) 

rsid_id_dup_crosswalk = readRDS(file.path(data_dir,"intermediate/rsid_id_dup_crosswalk.rds"))
hs_candidates <- prep_hs_candidates(
  schools = rsid_id_dup_crosswalk,
  state_fips = state_fips,
  verbose = TRUE
)

#alpha_grid <- c(0.01,0,1,0,0.3,0.5,0.7,0.9,0.99)

alpha_results <- data.frame(
  alpha = numeric(),
  r_squared = numeric(),
  intercept = numeric(),
  slope = numeric(),
  stringsAsFactors = FALSE
)

a=0.5

#for(a in alpha_grid)
#{
  unit_state <- prep_unit_state_probs(
    instate = instate,
    colleges = unique(sample[, .(
      unitid = ba_unitid,
      opeid = ba_opeid,
      col_state = ba_state
    )]),
    state_fips = state_fips,
    alpha = a,
    verbose = TRUE
  )
  matched_hs <- match_ambiguous_hs(
    users = sample[, .(
      user_id,
      hs_string,
      unitid = ba_unitid
    )],
    hs_candidates = hs_candidates,
    unit_state = unit_state,
    verbose = TRUE
  )
  
  # Note that we might want remove the small number of observations
  # with an apparent data entry error: the user names a high school which only appears
  # in states that didn't send students to their reported college
  #hs_prob = matched_hs %>% 
    #select(user_id,hs_id,unitid,draw_prob) %>% 
    #filter(!is.na(hs_id)) %>%
    #mutate(hs_id_match = "Probabilistic") %>%
    #rename(col_unitid = unitid, hs_id_prob=draw_prob) %>%
    #left_join(both %>% select(-c("hs_id")), by = c("user_id","col_unitid"),relationship="one-to-one")
  
  matched_hs_dt <- copy(as.data.table(matched_hs))
  setnames(matched_hs_dt, "unitid", "col_unitid")
  setnames(matched_hs_dt, "draw_prob", "hs_id_prob")
  matched_hs_dt[, hs_id_match := "Probabilistic"]
  
  both_no_hs <- copy(both)
  both_no_hs[, hs_id := NULL]
  
  hs_prob <- merge(
    both_no_hs,
    matched_hs_dt,
    by.x = c("user_id", "ba_unitid", "hs_string"),
    by.y = c("user_id", "col_unitid", "hs_string"),
    all = FALSE,
    sort = FALSE
  )
  hs_det = both %>%
    filter(!user_id %in% hs_prob$user_id) %>%
    mutate(hs_id_match = "Deterministic", hs_id_prob=1)
  
  hs_prob <- hs_prob[, .SD, .SDcols = names(hs_det)]
  # Create finalized high school match with two column describing match
  # Deterministic or probabilistic factor+ a column with probability (geq 1 and non-missing)
  both__ = rbind(hs_prob,hs_det) %>% as_tibble()
  
  # [CHANGED 2026-07-25 -- Phase 1] both__ (saved below as both_final.rds) is
  # already fully built at this point. Everything from here through the end
  # of this block is diagnostic-only calibration (comparing probabilistic vs.
  # deterministic HS match quality against IPEDS in-state shares) -- not
  # required for the actual output. Wrapped in tryCatch so a bug in this
  # calibration code (there's at least one confirmed one: line ~959 below
  # referenced a `unitid` column that doesn't exist in that piped object,
  # beyond the unitid/opeid character-vs-integer mismatches already fixed)
  # can't block saving both_final.rds.
  tryCatch({

  # Construct weights so the IPEDS shares aren't skewed toward older cohorts
  # who attended colleges when they were less geographically diverse
  col_grad_wt = both__ %>%
    mutate(ba_end = as.integer(year(ba_end))) %>%
    filter(!is.na(ba_end)) %>%
    #filter(col_end %in% c(1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020)) %>%
    group_by(ba_unitid,ba_end) %>%
    summarize(n = n()) %>%
    mutate(col_shr = n/sum(n))
  
  # Construct in-state shares, reweighted from IPEDS based off the year distribution
  # for each college
  col_instate = fread(file.path(data_dir,"raw/ipeds/colleges_ipeds_fall-res.csv")) %>%
    filter(state_of_residence %in% c(1:57),type_of_freshman == 99, fips %in% c(1:56), state_of_residence %in% c(1:58)) %>%
    as_tibble() %>%
    mutate(enroll_in = if_else(fips == state_of_residence,enrollment_fall,0),
           enroll_out = if_else(fips != state_of_residence,enrollment_fall,0),
           ) %>%
    group_by(unitid,year) %>%
    summarize(enroll_in = sum(enroll_in), enroll_out = sum(enroll_out)) %>%
    # [FIXED 2026-07-25 -- Phase 1] unitid here is integer (raw IPEDS CSV);
    # ba_unitid in col_grad_wt is character (both__ casts it that way) --
    # cast explicitly, same fix as the join in 03_li_ed.Rmd.
    ungroup() %>%
    mutate(unitid = as.character(unitid)) %>%
    inner_join(as_tibble(col_grad_wt), by = c("unitid"="ba_unitid","year"="ba_end")) %>%
    mutate(ipeds_instate_shr = enroll_in/(enroll_in+enroll_out)) %>%
    group_by(unitid) %>%
    summarize(ipeds_instate_shr = weighted.mean(ipeds_instate_shr,w=col_shr))
  
  # Comparing probabilistic matches to deterministic
  # How do the in-state shares compare
  comparison = both__ %>%
    rbind(both__ %>% mutate(hs_id_match = "sample_instate_shr")) %>%
    filter(ba_unitid %in% hs_prob$ba_unitid) %>%
    left_join(schools %>% select(hs_id,hs_state=state_abbr) %>% distinct(), by = "hs_id") %>%
    mutate(in_state = if_else(hs_state == ba_state,1,0)) %>%
    group_by(ba_unitid,ba_opeid,hs_id_match) %>%
    summarize(in_state = mean(in_state,na.rm=TRUE)) %>%
              #hs_end = mean(hs_end,na.rm=TRUE)) %>%
    # [FIXED 2026-07-25 -- Phase 1] Same unitid/opeid character-vs-integer
    # mismatch as the col_instate join above -- ba_unitid/ba_opeid are
    # character, colleges$unitid/opeid are integer.
    left_join(colleges %>% mutate(unitid = as.character(unitid), opeid = as.character(opeid)) %>%
                select(unitid,opeid,inst_name), by = c("ba_unitid"="unitid","ba_opeid"="opeid")) %>%
    pivot_wider(id_cols=c(ba_unitid,ba_opeid,inst_name),
                names_from = hs_id_match,
                values_from = in_state) %>%
    left_join(colleges %>% mutate(unitid = as.character(unitid), opeid = as.character(opeid)) %>%
                select(unitid,opeid,inst_control), by = c("ba_unitid"="unitid","ba_opeid"="opeid")) %>%
    left_join(col_instate %>% select(unitid,ipeds_instate_shr), by = c("ba_unitid"="unitid")) %>%
    left_join(
      both %>% as_tibble() %>% count(ba_unitid, ba_opeid, name = "n") %>% mutate(unitid = as.character(unitid)),
      by = c("ba_unitid", "ba_opeid")
    )
  
  # Weighted regression of predicting the IPEDS share from the in-state share
  model = lm(ipeds_instate_shr ~ sample_instate_shr + factor(inst_control), data=comparison, w=n)
  alpha_results <- rbind(
    alpha_results,
    data.frame(
      alpha = a,
      r_squared = summary(model)$r.squared,
      intercept = unname(coef(model)[1]),
      slope = unname(coef(model)["sample_instate_shr"]),
      stringsAsFactors = FALSE
    )
  )
#}

  }, error = function(e) {
    cat("Diagnostic calibration block failed (not fatal, both_final.rds is unaffected):\n")
    cat(" ", conditionMessage(e), "\n")
  })

saveRDS(both__,file.path(data_dir,"intermediate/both_final.rds"))

