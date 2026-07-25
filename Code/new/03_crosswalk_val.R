library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
data_dir = file.path(directory,"Data")

colleges <- readRDS(file.path(data_dir,"intermediate/colleges.rds")) %>%
  # Remove territorial colleges and universities
  filter(!state_abbr %in% c("AS","GU","MP","PR","VI"))

matched_col <- readRDS(file.path(data_dir,"intermediate/col_strings.rds"))
unmatched_col <- readRDS(file.path(data_dir,"intermediate/unmatched_col.rds"))

# Manual merges for colleges with name changes, academies, institutes, etc
# First passes looks for the largest colleges without an rsid
col_wo_rsid <- read_csv(file.path(data_dir,"intermediate/col_wo_rsid.csv")) %>%
  dplyr::filter(!is.na(rsid)) %>%
  left_join(readRDS(file.path(data_dir,"intermediate/input_col.rds")) %>% 
              filter(degree %in% c("","Bachelor")) %>% 
              group_by(rsid,university_name) %>% 
              summarize(N = sum(N)), 
            by = "rsid", 
            relationship = "many-to-one") %>%
  mutate(system_indicator = if_else(is.na(system_name),0,1),
         ambiguous_name = 0,
         string_method = "Manual: col_wo_rsid") %>%
  select(university_name,rsid,opeid,unitid,system_indicator,ambiguous_name,N,string_method)
# Next pass looks for largest rsids without colleges, plus correcting outliers 
# based on degrees awarded
rsid_wo_col <- read_csv(file.path(data_dir,"intermediate/rsid_wo_col.csv")) %>%
  filter(!is.na(unitid)) %>%
  select(-c(university_name,N)) %>%
  left_join(readRDS(file.path(data_dir,"intermediate/input_col.rds")) %>%
              filter(degree %in% c("","Bachelor")) %>%
              group_by(rsid,university_name) %>%
              summarize(N = sum(N)), 
            by = "rsid", 
            relationship = "many-to-one") %>%
  left_join(colleges %>% select(c(unitid,opeid,system_name)), by = c("unitid","opeid")) %>%
  mutate(system_indicator = if_else(is.na(system_name),0,1),
         ambiguous_name = 0) %>%
  select(university_name,rsid,opeid,unitid,system_indicator,ambiguous_name,N,string_method=match_type)

matches_col = rbind(matched_col %>% select(university_name,rsid,opeid,unitid,system_indicator,ambiguous_name,N,string_method),col_wo_rsid) %>%
  anti_join(rsid_wo_col %>% filter(string_method == "Manual correction: rsid_wo_col"), by = c("rsid")) %>%
  rbind(rsid_wo_col) %>%
  left_join(colleges %>% select(unitid,opeid,inst_name,system_name,system_flag,inst_control,deg_awarded,city,state_abbr,starts_with("parent")),
            by = c("unitid","opeid"),
            relationship="many-to-one")
  

# I need to review instances of rsids matching to multiple unitid-opeid pairs
dup_rsid = matches_col %>% group_by(rsid) %>% summarize(n = n()) %>% filter(n > 1)
dupes = matches_col %>% filter(rsid %in% dup_rsid$rsid)
#write.csv(dupes,file.path(data_dir,"intermediate/dupe_match_rsid.csv"),row.names = FALSE)
#dupes_with_unique_rsid = matched %>% inner_join(dupes %>% ungroup() %>% select(unitid,opeid), by = c("unitid","opeid")) %>% group_by(rsid) %>% summarize(n = n()) %>% filter(n == 1)

# For now, just de-dup by allocating ambiguous names to largest branch of university
dedup = dupes %>% 
  group_by(rsid) %>% 
  slice_max(n=1, order_by=deg_awarded, with_ties=FALSE)

# Save crosswalk with a single unitid-opeid pair for each rsid
col_rsid_crosswalk = matches_col %>% 
  anti_join(dupes, by = c("rsid","unitid","opeid")) %>% 
  rbind(dedup) %>%
  distinct() 
saveRDS(col_rsid_crosswalk, file.path(data_dir,"intermediate/col_rsid_crosswalk.rds"))

# Match diagnostics
col_rsid_diagnostics = col_rsid_crosswalk %>% 
  group_by(opeid,unitid,inst_name) %>% 
  summarize(N = sum(N), deg_awarded = first(deg_awarded)) %>%
  mutate(coverage = N/deg_awarded) %>%
  filter(deg_awarded > 0) #%>%
  #filter(coverage < 1 | coverage > 8)
  #filter(deg_awarded > 2500 & deg_awarded < 5000)
  #filter(deg_awarded < 2500 & deg_awarded > 500)

cc <- readRDS(file.path(data_dir,"intermediate/cc.rds")) %>%
  # Remove territorial colleges and universities
  filter(!state_abbr %in% c("AS","GU","MP","PR","VI"))
matched_cc <- readRDS(file.path(data_dir,"intermediate/cc_strings.rds"))
unmatched_cc <- readRDS(file.path(data_dir,"intermediate/unmatched_cc.rds"))

matches_cc = matched_cc %>%
  left_join(cc %>% select(unitid,opeid,inst_control,deg_awarded,city,state_abbr,parent_university),
            by = c("unitid","opeid"),
            relationship="many-to-one")


# I need to review instances of rsids matching to multiple unitid-opeid pairs
cc_dup_rsid = matches_cc %>% group_by(rsid) %>% summarize(n = n()) %>% filter(n > 1)
cc_dupes = matches_cc %>% filter(rsid %in% cc_dup_rsid$rsid)
#write.csv(dupes,file.path(data_dir,"intermediate/dupe_match_rsid.csv"),row.names = FALSE)
#dupes_with_unique_rsid = matched %>% inner_join(dupes %>% ungroup() %>% select(unitid,opeid), by = c("unitid","opeid")) %>% group_by(rsid) %>% summarize(n = n()) %>% filter(n == 1)

# For now, just de-dup by allocating ambiguous names to largest branch of university
cc_dedup = cc_dupes %>% 
  group_by(rsid) %>% 
  slice_max(n=1, order_by=deg_awarded, with_ties=FALSE)

# Save crosswalk with a single unitid-opeid pair for each rsid
cc_rsid_crosswalk = matches_cc %>% 
  anti_join(cc_dupes, by = c("rsid","unitid","opeid")) %>% 
  rbind(cc_dedup) %>%
  distinct() #%>%
#filter(!unitid %in% sub_ba$unitid)
saveRDS(cc_rsid_crosswalk, file.path(data_dir,"intermediate/cc_rsid_crosswalk.rds"))

# Match diagnostics
cc_rsid_diagnostics = cc_rsid_crosswalk %>% 
  group_by(opeid,unitid,inst_name) %>% 
  summarize(N = sum(N), deg_awarded = first(deg_awarded)) %>%
  mutate(coverage = N/deg_awarded) %>%
  filter(deg_awarded > 0)

# Institutions with a negligible number of observations from irregular strings
cc_under_represented = cc_rsid_diagnostics %>%
  filter(coverage < 0.1) %>%
  select(opeid,unitid,inst_name,deg_awarded)

# Identify the largest opeid-unitid pairs
cc_scragglers = cc %>%
  # Remove territorial colleges and universities
  filter(!state_abbr %in% c("AS","GU","MP","PR","VI")) %>%
  # Remove for-profits
  filter(inst_control != 3) %>%
  anti_join(cc_rsid_crosswalk, by = c("unitid","opeid")) %>%
  select(opeid,unitid,inst_name,deg_awarded) %>%
  rbind(cc_under_represented)


# Repeat the process for high schools
schools <- readRDS(file.path(data_dir,"intermediate/schools.rds")) %>%
  # Remove territorial colleges and universities
  filter(!state_abbr %in% c("AS","GU","MP","PR","VI"))

hs_alias <- readRDS(file.path(data_dir,"intermediate/hs_alias.rds"))

# [CHANGED 2026-07-25 -- Phase 1: fix HS/college asymmetry] Read hs_strings.rds
# (02_embed_new.R's classify+string+alias+embed output) instead of 01's
# matched_hs.rds/unmatched_hs.rds, mirroring how the college section above
# already reads col_strings.rds. This brings HS matching up to the same
# embedding-recovery treatment colleges already get. Renamed to match_type to
# match what 04_col_hs_construct.R expects on the HS side (it reads
# string_method on the college side -- an existing asymmetry in 04 itself,
# not something to change here).
matched_hs <- readRDS(file.path(data_dir,"intermediate/hs_strings.rds")) %>%
  rename(match_type = string_method)

# matched_hs is one row per rsid, with ambiguous_name flagging whether the
# matched alias is shared by multiple hs_ids -- unlike the old matched_hs.rds,
# it only carries the one hs_id the embedding/exact-match step happened to
# attach, not every candidate. Rebuild the full candidate set for ambiguous
# names by rejoining to hs_alias, which does have every hs_id sharing a given
# alias; keep the main crosswalk one-row-per-rsid, blanking hs_id/school
# metadata for ambiguous cases until 04 resolves them (same as before).
rsid_id_dup_crosswalk = matched_hs %>%
  filter(ambiguous_name == 1) %>%
  select(-hs_id) %>%
  inner_join(hs_alias %>% select(hs_id,alias), by = c("clean_name" = "alias")) %>%
  left_join(schools %>%
              select(hs_id,hs_name,hs_agency_id,hs_agency_name,n_grads,state_abbr),
            by = c("hs_id"))
saveRDS(rsid_id_dup_crosswalk,file.path(data_dir,"intermediate/rsid_id_dup_crosswalk.rds"))

hs_rsid_crosswalk = matched_hs %>%
  mutate(hs_id = if_else(ambiguous_name == 1,NA,hs_id)) %>%
  left_join(schools %>%
              select(hs_id,hs_name,hs_agency_id,hs_agency_name,n_grads,state_abbr),
            by = c("hs_id")) %>%
  ungroup() %>%
  distinct()

# Save crosswalk with a single unitid-opeid pair for each rsid
saveRDS(hs_rsid_crosswalk, file.path(data_dir,"intermediate/hs_rsid_crosswalk.rds"))
