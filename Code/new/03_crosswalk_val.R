library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

directory = "/nfs/turbo/lsa-areynoso"
data_dir = file.path(directory,"Data")

colleges <- readRDS(file.path(directory,"Data","colleges.rds")) %>%
  # Remove territorial colleges and universities
  filter(!state_abbr %in% c("AS","GU","MP","PR","VI"))

matched_col <- readRDS(file.path(directory,"Data","col_strings.rds"))
unmatched_col <- readRDS(file.path(directory,"Data","unmatched_col.rds"))

# Manual merges for colleges with name changes, academies, institutes, etc
# First passes looks for the largest colleges without an rsid
col_wo_rsid <- read_csv(file.path(directory,"Data","col_wo_rsid.csv")) %>%
  dplyr::filter(!is.na(rsid)) %>%
  left_join(readRDS(file.path(directory,"Data","input_col.rds")) %>% 
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
rsid_wo_col <- read_csv(file.path(directory,"Data","rsid_wo_col.csv")) %>%
  filter(!is.na(unitid)) %>%
  select(-c(university_name,N)) %>%
  left_join(readRDS(file.path(directory,"Data","input_col.rds")) %>%
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
#write.csv(dupes,file.path(data_dir,"dupe_match_rsid.csv"),row.names = FALSE)
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
saveRDS(col_rsid_crosswalk, file.path(data_dir,"col_rsid_crosswalk.rds"))

# Match diagnostics
col_rsid_diagnostics = col_rsid_crosswalk %>% 
  group_by(opeid,unitid,inst_name) %>% 
  summarize(N = sum(N), deg_awarded = first(deg_awarded)) %>%
  mutate(coverage = N/deg_awarded) %>%
  filter(deg_awarded > 0) #%>%
  #filter(coverage < 1 | coverage > 8)
  #filter(deg_awarded > 2500 & deg_awarded < 5000)
  #filter(deg_awarded < 2500 & deg_awarded > 500)

cc <- readRDS(file.path(directory,"Data","cc.rds")) %>%
  # Remove territorial colleges and universities
  filter(!state_abbr %in% c("AS","GU","MP","PR","VI"))
matched_cc <- readRDS(file.path(directory,"Data","cc_strings.rds"))
unmatched_cc <- readRDS(file.path(directory,"Data","unmatched_cc.rds"))

matches_cc = matched_cc %>%
  left_join(cc %>% select(unitid,opeid,inst_control,deg_awarded,city,state_abbr,parent_university),
            by = c("unitid","opeid"),
            relationship="many-to-one")


# I need to review instances of rsids matching to multiple unitid-opeid pairs
cc_dup_rsid = matches_cc %>% group_by(rsid) %>% summarize(n = n()) %>% filter(n > 1)
cc_dupes = matches_cc %>% filter(rsid %in% cc_dup_rsid$rsid)
#write.csv(dupes,file.path(data_dir,"dupe_match_rsid.csv"),row.names = FALSE)
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
saveRDS(cc_rsid_crosswalk, file.path(directory,"Data","cc_rsid_crosswalk.rds"))

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
schools <- readRDS(file.path(directory,"Data","schools.rds")) %>%
  # Remove territorial colleges and universities
  filter(!state_abbr %in% c("AS","GU","MP","PR","VI"))

matched_hs <- readRDS(file.path(directory,"Data","matched_hs.rds"))
unmatched_hs <- readRDS(file.path(directory,"Data","unmatched_hs.rds"))

# Identify instances of strings matching to multiple names
dup_rsid_hs = matched_hs %>%
  distinct() %>%
  group_by(rsid) %>% 
  summarize(n = n()) %>% 
  filter(n > 1)

# Create a de-duplicated set of strings
# We won't be able to de-duplicate until we can pick the right ambiguously-named
# high school based on college location
matches_hs = matched_hs %>%
  left_join(schools %>% 
              select(hs_id,hs_name,hs_agency_id,hs_agency_name,n_grads,state_abbr),
            by = c("hs_id"))
rsid_id_dup_crosswalk = matches_hs %>%
  filter(rsid %in% dup_rsid_hs$rsid)
saveRDS(rsid_id_dup_crosswalk,file.path(data_dir,"rsid_id_dup_crosswalk.rds"))

hs_rsid_crosswalk = matches_hs %>%
  mutate(ambiguous_name = if_else(rsid %in% dup_rsid_hs$rsid,1,0),
         hs_id = if_else(ambiguous_name == 1,NA,hs_id),
         hs_agency_id = if_else(ambiguous_name == 1,NA,hs_agency_id),
         hs_agency_name = if_else(ambiguous_name == 1,NA,hs_agency_name),
         hs_name = if_else(ambiguous_name == 1,NA,hs_name),
         n_grads = if_else(ambiguous_name == 1,NA,n_grads),
         state_abbr = if_else(ambiguous_name ==1,NA,state_abbr)) %>%
  ungroup() %>%
  distinct()

# Save crosswalk with a single unitid-opeid pair for each rsid
saveRDS(hs_rsid_crosswalk, file.path(data_dir,"hs_rsid_crosswalk.rds"))
