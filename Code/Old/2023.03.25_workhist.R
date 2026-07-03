library(data.table)
library(tidyverse)

# Need to pull college graduation year from 
filepath = "E:/Nick/Stata"
posfilename0 = "position_0000_part_00.csv"

pos = fread(paste0(filepath,"/",posfilename0))
pos_keys = pos %>%
  group_by(user_id) %>%
  group_keys()

pos_keys = pos %>%
  distinct()
pos_keys_samp = sample_n(pos_keys,30000)
keys0 = intersect(pos_keys_samp,pos0_keys)
keys1 = intersect(pos_keys_samp,pos1_keys)

position0_sample = pos0 %>%
  filter(user_id %in% keys0$user_id)
position1_sample = pos1 %>%
  filter(user_id %in% keys1$user_id)

position_sample = rbind(position0_sample, position1_sample) %>%
  filter(country == "United States")

# For now we will just filter out 'empty'
# It appears CBSA could be imputed for some of them based on the LI raw geography
empty = position_sample %>%
  filter(state == "empty")

# We need to calculate graduation year 
both3 = col_users %>%
  filter(user_id %in% hs_users$user_id)

col_1 = col %>%
  filter(user_id %in% both3$user_id)
hs_1 = hs %>%
  filter(user_id %in% both3$user_id)

merge_col_hs = col_1 %>%
  left_join(hs_1, by = "user_id", multiple = "all") %>%
  select(c(user_id, university_name.x, startdate.x, enddate.x,
           degree_raw.x, STATE.x, cbsa_code.x, school.y, enddate.y, cbsa_code.y))

pos0$user_id = as.numeric(pos0$user_id)
merge_position0 = pos0 %>%
  filter(user_id %in% col_users$user_id)
merge_position1 = pos1 %>%
  filter(user_id %in% col_users$user_id)
merge_position = rbind(merge_position0,merge_position1)