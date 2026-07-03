# Author  : Nick Martens
# Date    : 24 March 2023
# Purpose : Matching degrees in LinkedIn to CBSAs

library(readxl)
library(tidyverse)

# Delcare %nin% so we can filter observations
`%nin%` = Negate(`%in%`)

filepath = "E:/Nick/Stata"
edufilename = "education_0000_part_00.csv"

# With no cleaning 1,118,613 matches
# 7,469,386
ed_li = read_csv(paste0(filepath,"/",edufilename)) %>%
  mutate(school = str_to_lower(school),
         school = str_trim(school),
         alt_name = str_replace_all(school, "-"," "),
         alt_name = str_replace_all(alt_name, "—"," "),
         alt_name = str_replace_all(alt_name, ","," "),
         alt_name = str_replace_all(alt_name, "main campus",""),
#        alt_name = str_replace_all(alt_name,"^the ",""),
         alt_name = str_trim(alt_name)) %>%
  mutate(clean_name = alt_name) %>%
  # Filter out 135,449 associates 
  filter(degree != "Associate")

# Split into high school, unknown, and college
li_hs = ed_li %>%
  filter(degree == "High School") %>%
  # Remove any observations that match to a postsecondary institution
  left_join(all_col, by = "alt_name") %>%
  distinct(user_id,NAME, .keep_all = TRUE)

# Reclassifies observations Revelio misidentified
li_hs_missclassified = li_hs %>%
  filter(is.na(clean_name.y)==FALSE) %>%
  mutate(degree.x = "Bachelor")

# High schools correctly classified by Revelio
li_hs_correct = li_hs %>%
  filter(is.na(clean_name.y)==TRUE) %>%
  select(-c(NAME:clean_name.y)) %>%
  left_join(all_hs, by = "alt_name") %>%
  filter(is.na(clean_name)==FALSE) %>%
  distinct(user_id,NAME, .keep_all = TRUE) %>%
  # Rename to columns to appease to rbind gods
  rename(degree.y = degree,
         clean_name.y = clean_name)
  
  
# Users with unknown degrees
# Currently matches based on first institution listed
# Eventually I will hone the matching based on workhistory
li_unknown = ed_li %>%
  filter(degree == "empty") %>%
  left_join(all_col, by = "alt_name") %>%
  distinct(user_id,NAME, .keep_all = TRUE)
li_unknown_col = li_unknown %>%
  filter(is.na(clean_name.y)==FALSE)
li_unknown_unmatched = li_unknown %>%
  filter(is.na(clean_name.y)==TRUE) %>%
  select(-c(NAME:clean_name.y)) %>%
  left_join(all_hs, by = "alt_name") %>%
  distinct(user_id,NAME, .keep_all = TRUE)
li_unknown_hs = li_unknown_unmatched %>%
  filter(is.na(clean_name)==FALSE) %>%
  # Rename to columns to appease to rbind gods
  rename(degree.y = degree,
         clean_name.y = clean_name)
  

# College groups bachelor's and master's degrees together
li_col = ed_li %>%
  filter(degree %nin% c("empty","High School")) %>%
  # Remove any observations that match to a postsecondary institution
  left_join(all_hs, by = "alt_name") %>%
  distinct(user_id,NAME, .keep_all = TRUE)

# Reclassifies observations Revelio misidentified
# Marks any graduates of the New School as high schoolers unless corrected
li_col_missclassified = li_col %>%
  filter(is.na(clean_name.y)==FALSE) %>%
  filter(school != "the new school") %>%
  mutate(degree.x = "High School")

# High schools correctly classified by Revelio
li_col_correct = li_col %>%
  filter(is.na(clean_name.y)==TRUE | school == "the new school") %>%
  select(-c(NAME:clean_name.y)) %>%
  left_join(all_col, by = "alt_name") %>%
  filter(is.na(clean_name)==FALSE) %>%
  distinct(user_id,NAME, .keep_all = TRUE) %>%
  mutate(degree.x = if_else(is.na(degree.x)==TRUE,degree,degree.x)) %>%
  # For now I will filter out all graduate degrees
  filter(degree.x == "Bachelor") %>%
  # Rename to columns to appease to rbind gods
  rename(degree.y = degree,
         clean_name.y = clean_name)

# Create unified list of users with bachelors degrees
col = rbind(li_col_correct, li_hs_missclassified, li_unknown_col) %>%
  group_by(user_id)
# Create unified list of users with high schools listed
hs = rbind(li_hs_correct, li_col_missclassified, li_unknown_hs) %>%
  group_by(user_id)

col_users = col %>%
  group_keys()

hs_users = hs %>%
  group_keys()

both = intersect(col_users,hs_users)

li_colhs_users = rbind(li_col,li_hs) %>%
  group_by(user_id) %>%
  group_keys()
  
li_unknown_users = li_unknown %>%
  group_by(user_id) %>%
  group_keys()

both2 = intersect(li_colhs_users,li_unknown_users)
  
# This identifies unmatched universities
# Use it to continue piecemeal string cleaning in 2023.03.20_school_import
li_col_unmatched = li_col %>%
  filter(is.na(clean_name.y)==TRUE) %>%
  group_by(university_name) %>%
  summarize(n = n()) %>%
  arrange(n)
