# memo1_01a_column1_construct.R
#
# Builds "Column 1" for Memo 1 (see D:\Users\martensn\.claude\plans\
# velvet-churning-galaxy.md, Step 1, and the execution plan at
# wild-imagining-kurzweil.md): the population of Revelio college grads with
# NO high-school-match requirement, benchmarked against Column 2
# (06_finalize_data.Rmd's `regression` table, which requires a matched HS
# record via the inner join at 01d_col_hs_construct.R L416). Standalone
# script, not part of run_pipeline.R.
#
# Each section below adapts one pipeline stage, reanchored on the
# HS-independent population (input_col's users) instead of `both_`/
# `both_final.rds` (= input_col INNER JOINED to input_hs). Restrictions
# replicated vs. skipped are enumerated in wild-imagining-kurzweil.md's
# restriction map; real adaptations (not just "delete the HS bits") are
# flagged inline where they occur.
#
# Section 1 (this file, part A): input_col + BA-record reconciliation,
# adapting 01d_col_hs_construct.R L360-857 with `both_` -> `input_col`.
# Two places where the original logic depended on HS timing:
#   - col_mislabel originally used `yrs_post_hs %in% 4:6` (a "roughly 4-6
#     years after high school" check) as one of two AND'd conditions for
#     flagging a possibly-mislabeled BA record. That condition needs hs_end
#     and is dropped; the other half (an extra record showing an earlier
#     completion than the originally-picked record) is kept alone.
#   - earliest_li, a fallback BA source defined as "the earliest LI record
#     in the 4-6-year post-HS window," is HS-dependent by definition and is
#     dropped entirely -- chosen_source can only be extra_ba/og_ba/default
#     here, never "earliest". This changes which record gets used for a
#     small number of ambiguous multi-BA-record users; it does not change
#     row counts (every input_col user still gets exactly one ba_* record,
#     via the "default" fallback if nothing else applies).
# Associate/master/mba/doctor "extra degree" enrichment and the transfer
# dummy (01d L702-857, 03_li_ed.Rmd's `transfer` vector), initially skipped
# as "not needed," were added back into Section 1 below on 2026-08-10 --
# the memo's comparison table needs transfer status and graduate-degree
# attainment for Column 1 too, not just Column 2. Output:
# Data/intermediate/column1_degree_enrichment.rds, a standalone table
# joined onto column1_population.rds downstream (not folded into
# column1_col_match's own narrow schema, to avoid touching Sections 2-4's
# already-built, expensive checkpoints).

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")

source(file.path(here::here(), "Code", "college_lookup.R"))

# Progress logging -- this build processes a substantially larger
# population than 01d_col_hs_construct.R normally does (no HS join to
# narrow it down first), so intermediate visibility matters for a
# standalone run. flush() forces output to appear immediately even when
# stdout is redirected/piped rather than buffered until exit.
log_step <- function(msg) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n")
  flush(stdout())
}
# Resumability: Section 1's output doesn't depend on anything Section 2
# touches, and it's expensive enough (~20 min at this population's scale)
# that re-running it every time Section 2 needs a retry (e.g. after the
# arrow-scan OOM fix below) would waste real time. Skip straight to loading
# the checkpoint if it's already there.
column1_col_match_path <- file.path(data_dir, "intermediate/column1_col_match.rds")
column1_degree_enrichment_path <- file.path(data_dir, "intermediate/column1_degree_enrichment.rds")

if (!file.exists(column1_col_match_path) || !file.exists(column1_degree_enrichment_path)) {

log_step("Section 1 starting")

# ---- input_col: identical to 01d_col_hs_construct.R L360-415, except the
# which.max(enddate)-per-user dedup uses .I instead of .SD -- .SD[] rebuilds
# a full sub-data.table per group, which is fine for both_'s (smaller,
# HS-narrowed) population but scales badly here; .I[] just returns row
# indices, same result, far less overhead at this population size. Same
# swap applied to col_transfer_dedup and ba_corrected's by-group picks
# below -- all three are pure "pick one row per group" operations with no
# ordering dependency the .SD form was providing.

input_ <- fread(file.path(data_dir, "raw/revelio/2026.04.09_education.csv"),
                 select = c("user_id", "university_name", "enddate", "rsid",
                            "degree", "startdate", "field"))
setDT(input_)
log_step(paste("read raw education file:", nrow(input_), "rows"))

col_rsid_crosswalk <- readRDS(file.path(data_dir, "intermediate/col_rsid_crosswalk.rds"))
cc_rsid_crosswalk  <- readRDS(file.path(data_dir, "intermediate/cc_rsid_crosswalk.rds"))
setDT(col_rsid_crosswalk)
setDT(cc_rsid_crosswalk)

input_col <- input_[
  !degree %in% c("Associate", "Doctor", "Master", "MBA")
][
  col_rsid_crosswalk,
  on = .(university_name, rsid),
  nomatch = 0
]
log_step(paste("input_ filtered + joined to col_rsid_crosswalk:", nrow(input_col), "rows, before per-user dedup"))
input_col <- input_col[input_col[, .I[which.max(enddate)], by = user_id]$V1][
  , .(
    user_id,
    col_string = university_name,
    col_rsid = rsid,
    col_end = enddate,
    col_start = startdate,
    degree,
    col_field = field,
    col_opeid = opeid,
    col_unitid = unitid,
    col_system_indicator = system_indicator,
    col_ambiguous_name = ambiguous_name,
    col_match_type = string_method,
    col_state = state_abbr
  )
]
log_step(paste("input_col deduped to one row/user:", nrow(input_col), "users"))

saveRDS(input_col, file.path(data_dir, "intermediate/column1_input_col.rds"))

# ---- BA-record reconciliation: adapts 01d L427-700, both_ -> input_col --

keep_users <- unique(input_col[, .(user_id)])

# [Column 1 adaptation] the original excluded rows matching EITHER the
# chosen college record (rsid=col_rsid) OR the chosen HS record
# (rsid=hs_rsid) -- there is no HS record here, so only the college-side
# exclusion applies.
extra_ed <- input_[
  keep_users,
  on = "user_id",
  nomatch = 0
][
  !input_col,
  on = .(user_id, rsid = col_rsid, degree)
]
log_step(paste("extra_ed (candidate non-chosen education records):", nrow(extra_ed), "rows"))

col_transfer <- col_rsid_crosswalk[
  extra_ed,
  on = c("rsid", "university_name"),
  nomatch = 0
]
log_step(paste("col_transfer:", nrow(col_transfer), "rows"))

cc_transfer <- cc_rsid_crosswalk[
  extra_ed,
  on = c("rsid", "university_name"),
  nomatch = 0
]
log_step(paste("cc_transfer:", nrow(cc_transfer), "rows"))

# Deduplicate extra college rows (identical to 01d L452-470, except the
# final .SD[1]-by-group pick after ordering is replaced with setorder() +
# unique(by=) -- same "keep first row per group after this priority sort"
# result, but unique() on an already-sorted table is a fast C-level dedup
# instead of one .SD alloc per group).
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
]
log_step(paste("col_transfer_dedup pre-dedup:", nrow(col_transfer_dedup), "rows"))
setorder(col_transfer_dedup, user_id, enddate, extra_opeid, extra_unitid, degree_rank)
col_transfer_dedup <- unique(col_transfer_dedup, by = c("user_id", "enddate", "extra_opeid", "extra_unitid"))
col_transfer_dedup[, c("degree_clean", "degree_rank") := NULL]
log_step(paste("col_transfer_dedup deduped:", nrow(col_transfer_dedup), "rows"))

# [Column 1 adaptation] joins to input_col instead of both_; drops
# hs_end/hs_start (not available) and yrs_post_hs (see header note)
colts <- col_transfer_dedup[, .(
  user_id, university_name, degree, enddate, startdate, field, state_abbr, extra_opeid, extra_unitid
)][
  input_col,
  on = "user_id",
  nomatch = 0
][
  , .(
    user_id,
    university_name,
    degree,
    enddate,
    startdate,
    field,
    state_abbr,
    extra_opeid,
    extra_unitid,
    col_string,
    col_opeid,
    col_unitid,
    col_end,
    col_start,
    col_field,
    col_degree = i.degree,
    col_state
  )
][
  , `:=`(
    degree_ba = as.integer(grepl("bachelor", tolower(fifelse(is.na(degree), "", degree)))),
    col_degree_ba = as.integer(grepl("bachelor", tolower(fifelse(is.na(col_degree), "", col_degree)))),
    yrs_post_col = year(enddate) - year(col_end),
    col_mislabel = fifelse((year(enddate) - year(col_end)) < 0, 1L, 0L)
  )
][col_degree %in% c("Bachelor", "") & degree %in% c("Bachelor", "")]
log_step(paste("colts:", nrow(colts), "rows"))

# [Column 1 adaptation] no earliest_li fallback (HS-dependent by
# definition) -- chosen_source is extra_ba / og_ba / default only.
colts[, chosen_source := fcase(
  col_mislabel == 1L & degree_ba == 1L & col_degree_ba == 0L, "extra_ba",
  col_mislabel == 1L & degree_ba == 0L & col_degree_ba == 1L, "og_ba",
  default = "default"
)]

# Collapse to one corrected BA record per user (.I instead of .SD -- see
# header note; no ordering dependency, same as the original .SD[1] form)
ba_corrected <- colts[colts[, .I[1], by = user_id]$V1]
log_step(paste("ba_corrected:", nrow(ba_corrected), "users"))

ba_corrected[
  , `:=`(
    ba_school = fcase(
      chosen_source == "extra_ba", as.character(university_name),
      chosen_source == "og_ba", as.character(col_string),
      default = NA_character_
    ),
    ba_degree = fcase(
      chosen_source == "extra_ba", as.character(degree),
      chosen_source == "og_ba", as.character(col_degree),
      default = NA_character_
    ),
    ba_state = fcase(
      chosen_source == "extra_ba", as.character(state_abbr),
      chosen_source == "og_ba", as.character(col_state),
      default = NA_character_
    ),
    ba_end = fcase(
      chosen_source == "extra_ba", enddate,
      chosen_source == "og_ba", col_end,
      rep(TRUE, .N), as.IDate(col_end)
    ),
    ba_start = fcase(
      chosen_source == "extra_ba", startdate,
      chosen_source == "og_ba", col_start,
      rep(TRUE, .N), as.IDate(col_start)
    ),
    ba_field = fcase(
      chosen_source == "extra_ba", as.character(field),
      chosen_source == "og_ba", as.character(col_field),
      default = NA_character_
    ),
    ba_opeid = fcase(
      chosen_source == "extra_ba", as.character(extra_opeid),
      chosen_source == "og_ba", as.character(col_opeid),
      default = NA_character_
    ),
    ba_unitid = fcase(
      chosen_source == "extra_ba", as.character(extra_unitid),
      chosen_source == "og_ba", as.character(col_unitid),
      rep(TRUE, .N), as.character(col_unitid)
    )
  )
]

ba_corrected[
  , ba_corrected_flag := fcase(
    chosen_source == "extra_ba", 1L,
    chosen_source == "og_ba", 0L,
    chosen_source == "default" & ba_unitid == as.character(col_unitid), 0L,
    chosen_source == "default" & ba_unitid != as.character(col_unitid), 1L,
    default = NA_integer_
  )
]

ba_corrected <- ba_corrected[
  , .(
    user_id,
    ba_school,
    ba_degree,
    ba_state,
    ba_end,
    ba_start,
    ba_opeid,
    ba_unitid,
    ba_field,
    chosen_source,
    ba_corrected_flag
  )
]

# Users with no "extra" record at all keep their original input_col pick.
ba <- rbindlist(
  list(
    input_col[!ba_corrected, on = "user_id"][
      ,
      .(
        user_id,
        ba_school = col_string,
        ba_degree = degree,
        ba_end = col_end,
        ba_start = col_start,
        ba_opeid = col_opeid,
        ba_unitid = col_unitid,
        ba_state = col_state,
        ba_field = col_field,
        chosen_source = "default",
        ba_corrected_flag = 0L
      )
    ],
    ba_corrected
  ),
  use.names = TRUE,
  fill = TRUE
)
log_step(paste("ba (pre-backfill):", nrow(ba), "users"))

# [Phase A.3b fix, carried over from 01d_col_hs_construct.R] chosen_source
# == "default" can resolve a valid ba_unitid while leaving ba_school/
# ba_state/ba_opeid NA (a data-lineage quirk in the extra/transfer-record
# dedup above) -- backfill from colleges.rds using the person's own
# graduation year, same as the main pipeline.
colleges_lookup <- readRDS(file.path(data_dir, "intermediate/colleges.rds"))
setDT(colleges_lookup)

need_backfill <- ba[is.na(ba_school) & !is.na(ba_unitid)]
if (nrow(need_backfill) > 0) {
  need_backfill[, backfill_year := suppressWarnings(as.integer(format(ba_end, "%Y")))]
  resolved <- resolve_college(need_backfill, "ba_unitid", "backfill_year", colleges_lookup,
                               select_cols = c("inst_name", "state_abbr", "opeid"))
  ba[resolved, on = "user_id",
     `:=`(ba_school = fifelse(is.na(ba_school), i.inst_name, ba_school),
          ba_state  = fifelse(is.na(ba_state),  i.state_abbr, ba_state),
          ba_opeid  = fifelse(is.na(ba_opeid),  i.opeid, ba_opeid))]
  rm(resolved)
}
rm(need_backfill, colleges_lookup)
log_step(paste("ba backfilled:", sum(is.na(ba$ba_school)), "still NA ba_school"))

saveRDS(ba, file.path(data_dir, "intermediate/column1_ba.rds"))
log_step("saved column1_ba.rds")

# ---- Degree enrichment (associate/master/mba/doctor) + transfer dummy: ----
# adapts 01d_col_hs_construct.R L702-857, both_ -> input_col. Reuses
# col_transfer_dedup/cc_transfer, already built above (both already
# reanchored on input_col's population, same as the BA reconciliation
# itself) -- 01d builds these once and uses them for both the BA
# reconciliation AND this enrichment; this file previously only used them
# for the former and discarded them afterward (see this section's header
# note -- that's the gap being closed here).

# New CC-based extra matches (01d L726-746, not previously built in this
# file -- cc_transfer was computed above for the BA-reconciliation pass but
# never deduped/used, since that pass only needs the college side).
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
]
setorder(cc_transfer_dedup, user_id, enddate, extra_opeid, extra_unitid, degree_rank)
cc_transfer_dedup <- unique(cc_transfer_dedup, by = c("user_id", "enddate", "extra_opeid", "extra_unitid"))
cc_transfer_dedup[, c("degree_clean", "degree_rank") := NULL]
cc_transfer_dedup[, source := "cc"]
log_step(paste("cc_transfer_dedup deduped:", nrow(cc_transfer_dedup), "rows"))

# col_transfer_dedup (built above for the BA reconciliation) never got a
# `source` column, since that path doesn't need one -- 01d L720 tags it
# "col" for exactly this section's use.
col_transfer_dedup[, source := "col"]

extra_ed_long <- rbindlist(
  list(
    col_transfer_dedup[, .(user_id, university_name, degree, enddate, extra_opeid, extra_unitid, source)],
    cc_transfer_dedup[, .(user_id, university_name, degree, enddate, extra_opeid, extra_unitid, source)]
  ),
  use.names = TRUE,
  fill = TRUE
)
log_step(paste("extra_ed_long:", nrow(extra_ed_long), "rows"))

# [Column 1 adaptation] existence-filter only -- 01d's both_[,.(user_id,
# hs_end)] join exists purely to restrict extra_ed_long to the population
# (hs_end itself is never referenced downstream in this block); input_col
# is Column 1's direct population analogue, so this is a like-for-like
# substitution, not a redesign.
extra_levels <- extra_ed_long[
  input_col[, .(user_id)],
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
log_step(paste("extra_levels classified:", nrow(extra_levels), "rows"))

# pick_earliest_level(): copied verbatim from 01d_col_hs_construct.R
# L323-358 -- population-agnostic, operates only on columns extra_levels
# already carries (already reanchored on input_col above).
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

ba_transfer_dt <- pick_earliest_level(extra_levels, levels = c("ba_transfer", "ba_transfer_imp"), prefix = "ba_transfer")
associate_dt   <- pick_earliest_level(extra_levels, levels = c("associate", "associate_imp"), prefix = "associate")
master_dt      <- pick_earliest_level(extra_levels, levels = c("master", "master_imp"), prefix = "master")
mba_dt         <- pick_earliest_level(extra_levels, levels = c("mba"), prefix = "mba")
doctor_dt      <- pick_earliest_level(extra_levels, levels = c("doctor"), prefix = "doctor")

column1_degree_enrichment <- Reduce(
  function(x, y) y[x, on = "user_id"],
  list(
    input_col[, .(user_id)],
    ba_transfer_dt,
    associate_dt,
    master_dt,
    mba_dt,
    doctor_dt
  )
)

column1_degree_enrichment[, `:=`(
  has_transfer  = !is.na(ba_transfer_school),
  has_associate = !is.na(associate_school),
  has_master    = !is.na(master_school),
  has_mba       = !is.na(mba_school),
  has_doctor    = !is.na(doctor_school)
)]
# `transfer`/`any_grad` definitions reused verbatim from, respectively,
# 03_li_ed.Rmd L140-142 and demographics.R L92, for cross-script
# consistency with Column 2's own definitions of the same concepts.
column1_degree_enrichment[, `:=`(
  transfer = as.integer(has_transfer),
  any_grad = fifelse(has_master | has_mba | has_doctor, 1L, 0L)
)]

log_step(paste(
  "column1_degree_enrichment:", nrow(column1_degree_enrichment), "users.",
  "has_transfer:", sum(column1_degree_enrichment$has_transfer),
  " any_grad:", sum(column1_degree_enrichment$any_grad == 1)
))

saveRDS(column1_degree_enrichment, file.path(data_dir, "intermediate/column1_degree_enrichment.rds"))
log_step("saved column1_degree_enrichment.rds")

# ---- col_match-equivalent: mirrors 03_li_ed.Rmd L100-117 ----------------
# (renames ba_* -> col_* the same way the real pipeline does for Column 2;
# no hs_match/both/col_users distinction needed since there is no HS side)
#
# [Column 1 performance fix] 03_li_ed.Rmd derives col_start/col_end via
# as.numeric(format(as.Date(x), "%Y")) -- fine at both_final's ~5.24M-row
# scale, but format.Date() over Column 1's ~50M rows (x2, for start and
# end) is the actual bottleneck here, confirmed by killing a run that sat
# on this exact step for 15+ min with CPU still climbing while every
# other step took seconds to a few minutes. data.table::year() extracts
# the same calendar-year integer directly in C, no string round-trip --
# identical output, no format-parsing cost. Also uses a data.table join
# instead of dplyr's left_join (same reasoning as the .I[]-swaps above).

colleges <- readRDS(file.path(data_dir, "intermediate/colleges.rds"))
setDT(colleges)

column1_col_match <- merge(
  ba,
  colleges[, .(unitid = as.character(unitid), opeid = as.character(opeid), system_opeid)],
  by.x = c("ba_unitid", "ba_opeid"),
  by.y = c("unitid", "opeid"),
  all.x = TRUE,
  sort = FALSE
)
log_step(paste("column1_col_match joined to colleges:", nrow(column1_col_match), "rows"))

column1_col_match <- column1_col_match[
  , .(
    user_id,
    col_start = as.numeric(year(ba_start)),
    col_end = as.numeric(year(ba_end)),
    col_name = ba_school,
    col_major = ba_degree,
    col_field = ba_field,
    col_unitid = ba_unitid,
    col_opeid = ba_opeid,
    col_super_opeid = system_opeid
  )
]

saveRDS(column1_col_match, column1_col_match_path)

# col_field's raw source column uses "" for missing, not NA (confirmed via
# both_final.rds too: 43.8% coverage excluding "", vs. a misleadingly high
# ~92% if empty string is counted as populated) -- exclude both.
cat("Section 1 done. input_col:", nrow(input_col), "users. column1_col_match:",
    nrow(column1_col_match), "users. col_field coverage:",
    round(100 * mean(!is.na(column1_col_match$col_field) & column1_col_match$col_field != ""), 1), "%\n")

# Free the large Section 1 intermediates that Section 2 no longer needs --
# column1_col_match (50.45M users) is about to be joined against a position
# dataset an order of magnitude bigger than the real pipeline's equivalent
# step, so headroom matters.
rm(input_, input_col, keep_users, extra_ed, col_transfer, cc_transfer,
   col_transfer_dedup, cc_transfer_dedup, colts, ba_corrected, ba,
   col_rsid_crosswalk, cc_rsid_crosswalk, colleges, extra_ed_long,
   extra_levels, ba_transfer_dt, associate_dt, master_dt, mba_dt, doctor_dt,
   column1_degree_enrichment, pick_earliest_level)
gc()

} else {
  log_step("Section 1 checkpoints (column1_col_match.rds, column1_degree_enrichment.rds) already exist -- skipping Section 1")
  column1_col_match <- readRDS(column1_col_match_path)
  log_step(paste("loaded column1_col_match:", nrow(column1_col_match), "users"))
}

# ==========================================================================
# Section 2 -- position matching, adapts 04_li_ed_pos.Rmd
# ==========================================================================
# column1_col_match stands in for `both` (03_li_ed.Rmd: both <-
# both_final %>% distinct(user_id), i.e. just a user_id list) at every join
# below -- there's no separate hs_match/col_match/both split needed since
# there's no HS side. `full_position` (col_users[pos,...] in the original)
# is built there but never referenced again before being rm()'d -- confirmed
# by reading the whole file end to end -- so it's skipped here entirely.
# The Transfer Student Dummy chunk (04 L167-171) is also skipped -- it
# depends on the associate/master/mba/doctor "extra degree" tables Section 1
# deliberately didn't build (not part of the restriction map or any planned
# table row, see Section 1's header note).

library(arrow)
library(bit64)     # user_id is integer64 in the position parquet dataset
library(lubridate)  # ymd/years/days/year() for the interval construction
library(stringi)    # stri_replace_all_fixed for cbsa_core string cleaning
library(table.express)

# Resumability, same rationale as Section 1: this section reruns a full
# arrow scan + per-chunk foverlaps/dcast pass over Column 1's much larger
# population (~50M vs both_'s narrower set), on the order of many hours.
# Skip straight to the checkpoint if it's already there.
column1_positions_path <- file.path(data_dir, "intermediate/column1_positions.rds")

if (!file.exists(column1_positions_path)) {

log_step("Section 2 starting: position matching")

work_history_start <- 1975
work_history_end   <- 2025
max_post_grad <- 50
min_post_grad <- 0

# ---- arrow scan: restriction 5 (US-country filter) + semi-join to Column
# 1's user set (10x the real pipeline's ~5.24M) --
#
# [Column 1 fix, discovered by running it] A single collect() across the
# whole dataset (matching the real pipeline's approach, fine at ~5.24M
# users) OOM'd here: "Out of memory: realloc of size 1048640 failed" inside
# arrow's compute layer, even with ~500GB free system RAM -- this is an
# Arrow-internal allocator failure materializing a huge result set in one
# shot, not literal system exhaustion. pos_parquet_pilot is already
# Hive-partitioned by year (P:\BRAIN_DRAIN\Data\intermediate\
# pos_parquet_pilot\year=2000 .. year=2025, 2024 absent), so scan+collect
# one partition at a time instead -- each chunk is a small fraction of the
# size that crashed, and checkpointing per year means a crash partway
# through loses at most one year's worth of work, not the whole scan.
pos_dir <- file.path(data_dir, "intermediate/pos_parquet_pilot")
pos_years <- sort(as.integer(gsub("year=", "", basename(list.dirs(pos_dir, recursive = FALSE)))))
log_step(paste("position dataset partitions:", paste(pos_years, collapse = ",")))

pos_checkpoint_dir <- file.path(data_dir, "intermediate/column1_pos_by_year")
dir.create(pos_checkpoint_dir, showWarnings = FALSE)

# [Column 1 fix, round 2 -- discovered by running it] The combined position
# table (963,395,619 rows across 45,527,742 users, the largest table built
# anywhere in this build) had no checkpoint of its own -- a later failure
# (or, as actually happened, an unlogged step running far longer than
# expected with no visibility) meant losing ~2.5 hours of work with no way
# to resume short of redoing the whole per-year scan. This checkpoint, plus
# explicit logging around the as.IDate() conversions (the likely actual
# bottleneck: character-to-date parsing over 963M rows x2, apparently
# single-threaded given CPU tracked ~1:1 with wall-clock time), fixes both
# problems at once.
pos_combined_path <- file.path(pos_checkpoint_dir, "combined.rds")

if (!file.exists(pos_combined_path)) {

  relevant_ids <- arrow_table(user_id = unique(column1_col_match$user_id))

  for (yr in pos_years) {
    chunk_path <- file.path(pos_checkpoint_dir, paste0("year_", yr, ".rds"))
    if (file.exists(chunk_path)) {
      log_step(paste("year", yr, "-- checkpoint exists, skipping"))
      next
    }
    chunk <- open_dataset(pos_dir) %>%
      filter(year == yr, country == "United States") %>%
      select(user_id, country, state, msa, startdate, enddate, seniority, onet_code) %>%
      semi_join(relevant_ids, by = "user_id") %>%
      collect() %>%
      setDT()
    saveRDS(chunk, chunk_path)
    log_step(paste("year", yr, "collected:", nrow(chunk), "rows"))
    rm(chunk)
    gc()
  }
  rm(relevant_ids)

  pos <- rbindlist(lapply(pos_years, function(yr) readRDS(file.path(pos_checkpoint_dir, paste0("year_", yr, ".rds")))))
  log_step(paste("pos combined across all years:", nrow(pos), "rows,", uniqueN(pos$user_id), "users"))

  # [Column 1 fix, round 3 -- discovered by running it] as.IDate()'s default
  # auto-format-detection took 225.7 sec for a 31.5M-row sample (measured
  # directly) -- extrapolates to ~2 hrs for this 963M-row table, confirmed
  # in practice (a run left this step running 18+ min with no end in
  # sight). An explicit format= (the raw strings are plain "YYYY-MM-DD",
  # confirmed by sampling) roughly halves that (116.7 sec on the same
  # sample, byte-identical output, verified via identical() before
  # trusting it). Still slow, but no longer a multi-hour black box.
  pos[, startdate := as.IDate(startdate, format = "%Y-%m-%d")]
  log_step("startdate converted to IDate")
  pos[, enddate := as.IDate(enddate, format = "%Y-%m-%d")]
  log_step("enddate converted to IDate")

  saveRDS(pos, pos_combined_path)
  log_step("saved column1_pos_by_year/combined.rds")

  # Now that the combined+converted table is safely checkpointed, the 25
  # per-year raw chunks are redundant -- free the disk space.
  file.remove(list.files(pos_checkpoint_dir, pattern = "^year_.*\\.rds$", full.names = TRUE))

} else {
  log_step("pos checkpoint (combined.rds) already exists -- loading it")
  pos <- readRDS(pos_combined_path)
  log_step(paste("loaded pos:", nrow(pos), "rows,", uniqueN(pos$user_id), "users"))
}

# `both[pos, on="user_id", nomatch=NULL]` in the original is a no-op restriction
# (the arrow semi-join above already restricts pos to exactly this user set) --
# `position` is just `pos` under the original's name, matching what `position`
# means in the rest of 04_li_ed_pos.Rmd's logic below.
position <- pos
log_step("position assigned")

# ---- restriction 6: drop users whose college name never resolved --------
column1_merge <- column1_col_match[!is.na(col_name)]
column1_merge[, col_length := col_end - col_start]
log_step(paste("column1_merge (col_name resolved):", nrow(column1_merge), "users"))

# ---- restriction 7: earliest qualifying position per user ---------------
# (04 "Extract Earliest Job" -- work_earliest is derived from `position`,
# not merge_col_hs/column1_merge)
work_earliest <- position[position[, .I[which.min(startdate)], by = user_id]$V1]
work_earliest <- work_earliest[, .(user_id, startdate)]
work_earliest[, user_id := as.integer64(user_id)]

column1_merge <- column1_merge[work_earliest, on = "user_id"]
setnames(column1_merge, "startdate", "work_start")
log_step(paste("column1_merge + work_earliest (restriction 7):", nrow(column1_merge), "users"))

# col_state: needed for Section 3's territory filter (col_match doesn't
# carry it -- the real pipeline only resolves it this late too, via
# resolve_college() on colleges.rds keyed to each user's own col_end era).
colleges_lookup <- readRDS(file.path(data_dir, "intermediate/colleges.rds"))
setDT(colleges_lookup)
column1_merge <- resolve_college(column1_merge, "col_unitid", "col_end", colleges_lookup,
                                  select_cols = c("state_abbr"))
column1_merge[, col_state := state_abbr]
column1_merge[, state_abbr := NULL]
rm(colleges_lookup)
log_step("col_state resolved")

# ---- birth-year waterfall (04 "Calculating Birth Year") ------------------
# [Column 1 adaptation] the original's first-priority source is hs_end
# (birth1); there's no HS side here, so birth1 is always 0 and the
# waterfall starts from birth2 (col_start) -- which Step 0 made real, so
# this branch is alive for Column 1 in a way it wasn't before that fix.
column1_merge[, col_start := as.numeric(col_start)]
column1_merge[, col_end := as.numeric(col_end)]

column1_merge[, `:=`(
  birth2 = fifelse(!is.na(col_start), col_start - 18, 0),
  birth3 = fifelse(!is.na(col_end), col_end - 22, 0),
  birth4 = fifelse(!is.na(work_start), year(work_start) - 22, 0)
)]
column1_merge[, birth := fifelse(birth2 > 0, birth2,
                          fifelse(birth3 > 0, birth3,
                          fifelse(birth4 > 0, birth4, 0)))]
column1_merge[, col_end := fifelse(!is.na(col_end), col_end, birth + 22)]
log_step("birth-year waterfall computed")

# ---- restrictions 8-11: CBSA/MSA merge + chronology + post-grad window --
# (04 "Checking for Position in nth Year" through "Standardizing time
# intervals" -- reused verbatim, cbsa_li_crosswalk/unified_cbsa rebuilt via
# the same static, non-user-data-dependent script 00_crosswalks.Rmd/.R
# normally leaves in memory for a continuous pipeline session)
source(file.path(here::here(), "Code", "scripts", "00_crosswalks.R"))
log_step("cbsa_li_crosswalk rebuilt via 00_crosswalks.R")

position[, user_id := as.integer64(user_id)]

state_name_to_abb <- setNames(c(state.abb, "DC"), c(state.name, "Washington, D.C."))

string_cleaning <- fread(file.path(data_dir, "intermediate/msa_string_cleaning.csv"))
patterns <- string_cleaning$Lookup
replacements <- string_cleaning$Rename
rm(string_cleaning)

start <- ymd(rep(work_history_start:work_history_end), truncated = 2L)
end <- start + years(1) - days(1)
interval <- year(start)
work_hist <- data.table(interval = interval, start = as.IDate(start), end = as.IDate(end))
setkey(work_hist, start, end)

# [Column 1 fix, round 4 -- discovered by running it] `column1_merge[position,
# on="user_id", allow.cartesian=TRUE]` (position = 963,395,619 rows) crashed
# with "Error: cannot allocate vector of size 7.2 Gb" -- twice, once after a
# 3.5hr run and again immediately in a fresh process, ruling out fragmentation
# as the cause. The size is telling: R character vectors cost 8 bytes/element
# just for the pointer array regardless of string content, and 963,395,619 x
# 8 bytes = ~7.2GB almost exactly -- broadcasting column1_merge's several
# character columns (col_name, col_major, etc.) onto every position row in
# one shot needs several such vectors simultaneously. Same fix as the arrow
# scan: chunk it, this time by user_id %% N instead of by year (position has
# no year column post-combine). Each chunk runs the full birth_position ->
# CBSA-merge -> chronology-filter -> foverlaps pipeline (restrictions 8-11)
# and checkpoints separately, so a crash partway through only costs one
# chunk's worth of (re-)work, not the whole thing.
#
# [Column 1 fix, round 3, continued] the original round-trips startdate/
# enddate through character and back to IDate, solely so the enddate-missing
# fallback ("2026-03-01", the position dataset's snapshot date) can be
# assigned as a string. Both columns are already IDate coming out of
# `position` -- fifelse() works directly on IDate and preserves the class,
# so the whole round-trip (and the second as.IDate() pass the original
# needed to undo it) is skipped. startdate has no substitution logic in the
# original at all -- its round-trip was a pure no-op.

# [Column 1 fix, round 5 -- discovered by running it] chunk 0 still hit
# "cannot allocate vector of size 6.6 Gb" even after cleanly clearing the
# join and CBSA merge (confirmed via per-stage logging -- no fan-out
# anywhere, chunk_id distribution perfectly even). The failure sizes
# reported (7.2Gb unchunked, 6.6Gb for a 10x-smaller chunk) don't scale
# with MY data size, which pointed elsewhere: this is a shared cluster
# machine, and Get-Process at the moment of the crash showed a different
# user's python process alone using 290GB, another at 46GB, plus multiple
# Stata/RStudio sessions -- ~430GB from other jobs, fluctuating in real
# time. The OOM is resource contention, not a sizing bug in this script.
# Shrinking chunks further (10 -> 50) buys more safety margin against
# whatever headroom other users' jobs leave at any given moment; explicit
# gc() between stages below encourages returning memory promptly rather
# than holding it until each chunk's iteration ends.
# [Column 1 fix, round 6 -- discovered by running it] Chunks 0-1 both
# completed cleanly but produced ~153.6M rows each (an ~8.5x fan-out from
# foverlaps: multi-year positions match multiple yearly intervals -- the
# same thing the real pipeline does, just at scale here). Extrapolated
# across 50 chunks that's ~7.7 BILLION rows for a combined long-format
# std_pos -- both the rbindlist combine and the dcast afterward would almost
# certainly repeat the same allocation failure, and finding that out would
# cost the ~9 hours needed to run all 50 chunks first. Since chunking is by
# user_id and dcast's grouping key is also user_id, chunks are fully
# independent for this purpose: dcast (and the rest of the pipeline through
# `completed`) can run PER CHUNK -- on ~150M rows instead of ~7.7B -- and
# the final per-chunk `completed` tables (one row per user, ~1M users/chunk)
# just get stacked at the end. This also means chunk_0.rds/chunk_1.rds
# (the long std_pos already computed and checkpointed by the prior run) are
# reused as-is below, not recomputed.
mode_pick <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

n_chunks <- 50L
position[, chunk_id := as.integer(as.double(user_id) %% n_chunks)]
column1_merge[, chunk_id := as.integer(as.double(user_id) %% n_chunks)]
log_step("chunk_id distribution:")
print(position[, .N, by = chunk_id][order(chunk_id)])
flush(stdout())

std_pos_chunks_dir <- file.path(data_dir, "intermediate/column1_std_pos_chunks")
dir.create(std_pos_chunks_dir, showWarnings = FALSE)
completed_chunks_dir <- file.path(data_dir, "intermediate/column1_completed_chunks")
dir.create(completed_chunks_dir, showWarnings = FALSE)

for (cid in 0:(n_chunks - 1L)) {
  completed_chunk_path <- file.path(completed_chunks_dir, paste0("chunk_", cid, ".rds"))
  if (file.exists(completed_chunk_path)) {
    log_step(paste("completed chunk", cid, "-- checkpoint exists, skipping"))
    next
  }

  std_pos_chunk_path <- file.path(std_pos_chunks_dir, paste0("chunk_", cid, ".rds"))
  if (file.exists(std_pos_chunk_path)) {
    log_step(paste("chunk", cid, "-- std_pos checkpoint exists, loading it (reused from before the dcast restructure)"))
    std_pos_chunk <- readRDS(std_pos_chunk_path)
  } else {
    pos_chunk <- position[chunk_id == cid]
    log_step(paste("chunk", cid, "-- pos_chunk:", nrow(pos_chunk), "rows"))

    bp_chunk <- column1_merge[pos_chunk, on = "user_id", allow.cartesian = TRUE] %>%
      setnames(c("state"), c("cbsa_state")) %>%
      table.express::mutate(cbsa_state = ifelse(cbsa_state %in% names(state_name_to_abb),
                                                 state_name_to_abb[cbsa_state], cbsa_state)) %>%
      table.express::mutate(cbsa_core = str_replace(msa, "-.*", "")) %>%
      table.express::mutate(cbsa_core = str_replace_all(cbsa_core, " [A-Z]{2} MSA$| MSA$", "")) %>%
      table.express::mutate(cbsa_core = str_replace_all(cbsa_core, " [A-Z]{2}$", ""))
    bp_chunk[, enddate := fifelse(is.na(enddate), as.IDate("2026-03-01"), enddate)]
    bp_chunk[, cbsa_core := stri_replace_all_fixed(cbsa_core, patterns, replacements, vectorize_all = FALSE)]
    log_step(paste("chunk", cid, "-- bp_chunk pre-CBSA-merge:", nrow(bp_chunk), "rows"))

    # restriction 8: msa != "empty" (drop unmapped-MSA rows)
    bp_chunk <- merge(bp_chunk, cbsa_li_crosswalk, by = c("cbsa_core", "cbsa_state"), all.x = TRUE) %>%
      table.express::filter(!is.na(col_name) & msa != "empty")
    log_step(paste("chunk", cid, "-- bp_chunk post-CBSA-merge:", nrow(bp_chunk), "rows"))
    gc()

    # restrictions 9-10: startdate non-missing, startdate <= enddate
    bp_chunk <- bp_chunk %>%
      table.express::filter(is.na(startdate) == FALSE) %>%
      table.express::filter((startdate <= enddate) == TRUE)
    log_step(paste("chunk", cid, "-- bp_chunk post-chronology:", nrow(bp_chunk), "rows"))
    gc()

    # restriction 11: 0-50-yr post-grad window
    std_pos_chunk <- foverlaps(bp_chunk, work_hist,
                                by.x = c("startdate", "enddate"), by.y = c("start", "end"),
                                type = "any") %>%
      table.express::mutate(yrs_graduated = interval - col_end) %>%
      table.express::filter(min_post_grad <= yrs_graduated & yrs_graduated <= max_post_grad)
    log_step(paste("chunk", cid, "-- std_pos_chunk post-foverlaps:", nrow(std_pos_chunk), "rows"))
    rm(pos_chunk, bp_chunk)
    gc()
  }

  # ---- restriction 12 vehicle: dcast into per-year wide columns, THIS
  # CHUNK ONLY (the same wide cbsa_code_0..50/cbsa_state_0..50/soc_code_0..50
  # columns Step 4's migration-behavior row will need for Column 1 too)
  std_pos_geo_chunk <- dcast(std_pos_chunk, user_id ~ yrs_graduated,
                              value.var = c("cbsa_code", "cbsa_state"),
                              fun.aggregate = mode_pick)
  std_pos_soc_chunk <- dcast(std_pos_chunk, user_id ~ yrs_graduated,
                              value.var = c("onet_code"),
                              fun.aggregate = mode_pick)
  log_step(paste("chunk", cid, "-- dcast:", nrow(std_pos_geo_chunk), "users"))
  rm(std_pos_chunk)
  gc()

  cm_chunk <- column1_merge[chunk_id == cid]
  almost_completed_chunk <- cm_chunk[std_pos_geo_chunk, on = "user_id", all = TRUE]
  almost_completed_chunk <- almost_completed_chunk[std_pos_soc_chunk, on = "user_id", all = TRUE]
  rm(cm_chunk, std_pos_geo_chunk, std_pos_soc_chunk)

  # restriction 6, reapplied (matches the original's defensive re-check
  # after the all=TRUE outer joins above, which can reintroduce
  # col_name==NA rows)
  completed_chunk <- almost_completed_chunk[!is.na(col_name)]
  rm(almost_completed_chunk)

  # restriction 12: dedup to earliest col_end per user
  completed_chunk <- completed_chunk[completed_chunk[, .I[which.min(col_end)], by = "user_id"]$V1]

  saveRDS(completed_chunk, completed_chunk_path)
  log_step(paste("completed chunk", cid, ":", nrow(completed_chunk), "users"))
  rm(completed_chunk)
  gc()

  # Once a chunk's final completed_chunk_N.rds is safely saved, its
  # intermediate std_pos_chunk_N.rds (long-format, ~150M rows) is no
  # longer needed.
  if (file.exists(std_pos_chunk_path)) file.remove(std_pos_chunk_path)
}

rm(work_earliest, position, pos, column1_merge)
gc()

completed <- rbindlist(lapply(0:(n_chunks - 1L), function(cid) {
  readRDS(file.path(completed_chunks_dir, paste0("chunk_", cid, ".rds")))
}))
log_step(paste("completed combined across chunks:", nrow(completed), "users"))

setnames(completed, old = as.character(c(0:50)), new = paste0("soc_code_", 0:50))

saveRDS(completed, file.path(data_dir, "intermediate/column1_positions.rds"))
log_step(paste("Section 2 done. column1_positions:", nrow(completed), "users. saved column1_positions.rds"))

file.remove(list.files(completed_chunks_dir, pattern = "^chunk_.*\\.rds$", full.names = TRUE))

} else {
  log_step("Section 2 checkpoint (column1_positions.rds) already exists -- skipping Section 2")
}

# ---- Section 3: birth/cohort/territory filter, adapting 05_merge.Rmd
# L316-343. Only the col_state half of the territory filter applies here --
# the hs_state check is dropped outright (Column 1 has no hs_state; there is
# nothing to filter). Bounds are unchanged from the original: col_end in
# (1981, 2026), i.e. 1982-2025, and birth in (1929, 2003), i.e. 1930-2002.
column1_microdata_path <- file.path(data_dir, "intermediate/column1_microdata.rds")

if (!file.exists(column1_microdata_path)) {

log_step("Section 3 starting")

if (!exists("completed")) {
  completed <- readRDS(file.path(data_dir, "intermediate/column1_positions.rds"))
}

column1_microdata <- completed %>%
  table.express::filter(col_end > 1981 & col_end < 2026 & birth > 1929 & birth < 2003) %>%
  table.express::filter(!col_state %in% c("GU", "PR", "AS", "VI"))

saveRDS(column1_microdata, column1_microdata_path)
log_step(paste("Section 3 done. column1_microdata:", nrow(column1_microdata), "users. saved column1_microdata.rds"))

} else {
  log_step("Section 3 checkpoint (column1_microdata.rds) already exists -- skipping Section 3")
}

# ---- Section 4: institution-group filter, adapting 06_finalize_data.Rmd
# L104-119. Only the inst_group merge + filter (restriction 15) -- the
# surrounding covariate block (non_traditional, transfer, in_state,
# binary_dist, soc_2/soc_3, etc.) is `regression`'s own construction and is
# explicitly out of scope here (deferred to Memo 1 Step 4, per the plan).
# Column1_population's schema is column1_microdata's schema plus inst_group,
# minus the users the filter drops -- not the full `regression` covariate
# set. by.x/by.y types intentionally left as-is (col_unitid character,
# unitid integer) to match the original's untyped merge() call verbatim.
column1_population_path <- file.path(data_dir, "intermediate/column1_population.rds")

if (!file.exists(column1_population_path)) {

log_step("Section 4 starting")

if (!exists("column1_microdata")) {
  column1_microdata <- readRDS(column1_microdata_path)
}

inst_group <- fread(file.path(data_dir, "intermediate/institutional_characteristics.csv")) %>%
  select(unitid, inst_group)
# col_unitid is character throughout this script (Section 1's ba_unitid is
# built via as.character(col_unitid)); unitid here comes in as integer from
# fread. The original 06_finalize_data.Rmd merge() call is untyped and
# apparently relies on whatever implicit coercion its data.table version
# allowed -- this one raises "Incompatible join types" instead, so coerce
# explicitly rather than relying on it.
inst_group[, unitid := as.character(unitid)]

column1_population <- merge(column1_microdata, inst_group,
                             by.x = "col_unitid", by.y = "unitid", all.x = TRUE)

# restriction 15: drop missing inst_group / Private, For-Profit
column1_population <- column1_population[!is.na(inst_group) & inst_group != "Private, For-Profit"]

saveRDS(column1_population, column1_population_path)
log_step(paste("Section 4 done. column1_population:", nrow(column1_population), "users. saved column1_population.rds"))

} else {
  log_step("Section 4 checkpoint (column1_population.rds) already exists -- skipping Section 4")
}
