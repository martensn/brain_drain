library(tidyr)
library(tidyverse)
library(table.express)
library(data.table)

set.seed(49)
#directory = "/nfs/turbo/lsa-areynoso"
directory = "/Volumes/lsa-areynoso"
edufilename1 = "raw_educ_STATES_AK-MD.csv"
edufilename1edit = "STATES_AK-MD.csv"
edufilename2 = "raw_educ_STATES_MI-WY.csv"
edutfilename2edit = "STATES_MI-WY.csv"


ed_li1 = read_delim(file.path(directory,"Data/02",edufilename1))
ed_li2 = read_delim(file.path(directory,"Data/02",edufilename2))

ed_li = rbind(ed_li1,ed_li2) %>% select(-c(world_rank)) %>% as.data.table()
rm(ed_li2,ed_li1)
gc()
# Remove redundant intermediate objects to save memory
# Filter out associates degrees
#ed_li = ed_li[degree != "Associate"]
ed_li = ed_li[university_country %in% c("United States","\\N")]
setDT(ed_li)

ed_li_col = ed_li[,.N, by = .(university_name,university_country)]


# IS UC Blue Ash college actually UCLA
# Also there are university of minnesota morris entries
# same with Darla Moore School of Business
mismatched_names = c("College of Oceaneering","College Station High School",
                     "University of Forestry","University Tun Hussein Onn","University Tun Hussein Onn ",
                     "University of Coventry","Fachhochschule Dortmund",
                     "College Montmorency","University of Cape Town South Africa",
                     "College of Tourism","Sciences Po Lille","University of Kanpur",
                     "University of North Sumatra","Heinrich Heine UniversitÃ¤t DÃ¼sseldorf",
                     "University of Dhaka Bangladesh")
mn_df = data.frame(hs_id = NA,
                   university_name = mismatched_names,
                   ambiguous_name = NA,
                   method = NA,
                   unitid = NA,
                   opeid = NA,
                   parent_institution = NA,
                   system_indicator = NA,
                   ed_type = "college")

ed_m = ed_li[university_name %in% mismatched_names]
ed_mhs = ed_li[user_id %in% ed_m$user_id]

col_strings = readRDS(file.path(directory,"Data","col_strings.rds")) %>%
  select(unitid,opeid,parent_institution,university_name,ambiguous_name,system_indicator,method) %>%
  mutate(hs_id = NA, 
         ed_type = "college",
         unitid = if_else(ambiguous_name == 0,unitid,NA),
         opeid = if_else(ambiguous_name == 0,opeid,NA)) %>%
  distinct() %>%
  rbind(mn_df)

hs_strings = readRDS(file.path(directory,"Data","hs_strings.rds")) %>%
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

raw_strings = ed_mhs[degree %in% c("Bachelor","empty","High School"),.N,by = .(university_name)]
string_comp = ed_strings[raw_strings, on = .(university_name)]
string_comp[,ed_type := fifelse(university_name %in% mismatched_names,"college",ed_type)]
string_comp[,method := fifelse(university_name %in% mismatched_names,"mismatched",method)]

string_ = string_comp[is.na(method) & N > 5][order(-N)]

# Merge full set of cleaned strings on the education file
ed_id = ed_strings[ed_mhs[degree %in% c("Bachelor","empty","High School")], on = "university_name", nomatch=0]
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

hs_keep = hs_dt[, .(user_id, hs_name, hs_id, hs_ambiguous_name, hs_method, hs_startdate, hs_enddate)]
col_keep = col_dt[, .(user_id, unitid, opeid, parent_institution, 
                      system_indicator, col_name, col_ambiguous_name,
                      col_method, col_startdate, col_enddate)]



schools = readRDS(file.path(directory,"Data/schools.rds")) %>%
  left_join(unified_cbsa_name, by=c("hs_fips"="GeoFIPS")) 
colleges = readRDS(file.path(directory,"Data/colleges.rds"))
setDT(colleges)
setDT(schools)

schl = schools[, .(hs_id,state_abbr,cbsa_code,cbsa_name)]
ed_wide = schl[hs_keep[col_keep, on = "user_id"], on = "hs_id"]
ed_wide_m = ed_wide[col_name %in% mismatched_names & !is.na(hs_id)]

state_mhs = ed_wide_m[,.N, by = .(col_name,cbsa_name)]
state_mhs[,shr := N/sum(N), by = .(col_name)]

state_mhs = state_mhs 
