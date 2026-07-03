library(data.table)
library(tidyverse)

# Need to pull college graduation year from 
filepath = "E:/Nick/Stata"
posfilename0 = "cleaned_pos_educ_STATES.csv"

pos = fread(paste0(filepath,"/",posfilename0))
pos_keys = pos %>%
  group_by(user_id) %>%
  group_keys() %>%
  distinct()

position = pos %>%
  filter(user_id %in% both$user_id) %>%
  filter(country == "United States") %>%
  # For now we will just filter out 'empty'
  # It appears CBSA could be imputed for some of them based on the LI raw geography
  filter(state != "empty")

# We need to calculate graduation year 
both_col = col %>%
  filter(user_id %in% both$user_id)
both_hs = hs %>%
  filter(user_id %in% both$user_id)

merge_col_hs = both_col %>%
  left_join(both_hs, by = "user_id", multiple = "all") %>%
  select(c(user_id, university_name.x, educ_startdate.x, educ_enddate.x,
           field.x, STATE.x, cbsa_code.x, university_name.y, educ_enddate.y, cbsa_code.y))

merge_col_hs_keys = merge_col_hs %>%
  group_by(user_id) %>%
  group_keys()