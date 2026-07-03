library(tidyr)
library(tidyverse)
library(table.express)
library(data.table)

set.seed(49)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
edufilename1 = "raw_educ_STATES_AK-MD.csv"
edufilename1edit = "STATES_AK-MD.csv"
edufilename2 = "raw_educ_STATES_MI-WY.csv"
edutfilename2edit = "STATES_MI-WY.csv"


ed_li1 = read_delim(file.path(data_dir,"raw/revelio/raw_educ_STATES_AK-MD.csv"))
ed_li2 = read_delim(file.path(data_dir,"raw/revelio/raw_educ_STATES_MI-WY.csv"))

ed_li = rbind(ed_li1,ed_li2) %>% select(-c(world_rank)) %>% as.data.table()
rm(ed_li2,ed_li1)
gc()
# Remove redundant intermediate objects to save memory
# Filter out associates degrees
#ed_li = ed_li[degree != "Associate"]
ed_li = ed_li[university_country %in% c("United States","\\N")]
setDT(ed_li)

ed_li_col = ed_li[,.N, by = .(university_name,university_country)]

col_strings = readRDS(file.path(data_dir,"intermediate/col_strings.rds")) %>%
  select(unitid,opeid,parent_institution,university_name,ambiguous_name,system_indicator,method) %>%
  mutate(hs_id = NA, 
         ed_type = "college",
         unitid = if_else(ambiguous_name == 0,unitid,NA),
         opeid = if_else(ambiguous_name == 0,opeid,NA)) %>%
  distinct()

hs_strings = readRDS(file.path(data_dir,"intermediate/hs_strings.rds")) %>%
  select(hs_id,university_name,ambiguous_name,method) %>%
  mutate(unitid = NA, 
         opeid = NA, 
         parent_institution = NA, 
         system_indicator = NA, 
         ed_type = "high_school",
         hs_id = if_else(ambiguous_name == 0,hs_id,NA)) %>%
  distinct()
ed_strings = rbind(col_strings,hs_strings) 
setDT(ed_strings)

raw_strings = ed_li[,.N,by = .(university_name)]
string_comp = ed_strings[raw_strings, on = .(university_name)]
string_ = string_comp[is.na(method) & N > 5][order(-N)]
fwrite(string_,file.path(data_dir,"intermediate/raw_unmatched_strings.csv"))
saveRDS(string_,file.path(data_dir,"intermediate/raw_unmatched_strings.rds"))

# Merge full set of cleaned strings on the education file
ed_id = ed_strings[ed_li[degree %in% c("Bachelor","empty","High School")], on = "university_name", nomatch=0]
# Identify users with both high school and college on their profiles
both = ed_id[
  , .(has_hs = any(ed_type == "high_school", na.rm=TRUE),
      has_col = any(ed_type == "college", na.rm=TRUE)),
  by = user_id
][
  has_hs & has_col,
  user_id
]

users_with_both = length(both)
ed_id = ed_id[user_id %in% both]

ed_id[
  , educ_startdate := as.IDate(educ_startdate)
][
  , educ_enddate := as.IDate(educ_enddate)
]
# Identify the latest high school and college experience
# Eventually this won't be missing for anyone but I need to dig up exactly
# where in the pipeline I figure that out
ed_latest = ed_id[
  order(user_id, ed_type, educ_enddate, educ_startdate),
  .SD[.N],
  by = .(user_id, ed_type)
]
# Split by education type
hs_dt = ed_latest[ed_type == "high_school"]
col_dt = ed_latest[ed_type == "college"]

# Rename files 
setnames(
  hs_dt,
  c("university_name","ambiguous_name", "method",
    "educ_startdate", "educ_enddate"),
  c("hs_name", "hs_ambiguous_name", "hs_method",
    "hs_startdate", "hs_enddate")
)
setnames(
  col_dt,
  c("university_name","ambiguous_name", "method",
    "educ_startdate", "educ_enddate"),
  c("col_name", "col_ambiguous_name", "col_method",
    "col_startdate", "col_enddate")
)

hs_keep = hs_dt[, .(user_id, hs_name, hs_ambiguous_name, hs_method, hs_startdate, hs_enddate)]
col_keep = col_dt[, .(user_id, unitid, opeid, parent_institution, 
                      system_indicator, col_name, col_ambiguous_name,
                      col_method, col_startdate, col_enddate)]



schools = readRDS(file.path(data_dir,"intermediate/schools.rds"))
colleges = readRDS(file.path(data_dir,"intermediate/colleges.rds"))
setDT(colleges)
setDT(schools)
ed_wide = hs_keep[col_keep, on = "user_id"]
ed_wide_sample = ed_wide[sample(N,100000)]
saveRDS(ed_wide,file.path(data_dir,"intermediate/ed_wide.rds"))
saveRDS(ed_wide_sample,file.path(directory,"Data/ed_wide_sample.rds"))

col_ct = ed_wide[,.N, by = .(unitid,opeid,parent_institution)][order(-N)]
col_ct = colleges[col_ct, on = c("unitid","opeid","parent_institution")]

unallocated = col_ct[is.na(parent_university), .(parent_institution,N)]
setnames(unallocated, c("N"), c("system_n"))
imp_ct = unallocated[col_ct, on = c("parent_institution")]
imp_ct = imp_ct[!is.na(parent_university)]
imp_ct[,sys_deg_awarded := sum(deg_awarded, na.rm=TRUE), by = .(parent_institution)]
imp_ct[,imp_n := fifelse(is.na(system_n),N,((deg_awarded/sys_deg_awarded)*system_n) + N)]
imp_ct[,coverage := imp_n / deg_awarded]

saveRDS(imp_ct,file.path(data_dir,"intermediate/col_ct.rds"))
fwrite(imp_ct,file.path(data_dir,"intermediate/col_ct.csv"))