# Author  : Nick Martens
# Date    : 02 April 2023
# Purpose : Calculating the return rate of graduates to their home state and MSA

cbsa_state1 = cbsa_state %>% rename(cbsa_1 = cbsa_code, state_1 = STATE)
cbsa_state2 = cbsa_state %>% rename(cbsa_2 = cbsa_code, state_2 = STATE)
cbsa_state3 = cbsa_state %>% rename(cbsa_3 = cbsa_code, state_3 = STATE)
cbsa_state4 = cbsa_state %>% rename(cbsa_4 = cbsa_code, state_4 = STATE)
cbsa_state5 = cbsa_state %>% rename(cbsa_5 = cbsa_code, state_5 = STATE)
cbsa_state6 = cbsa_state %>% rename(cbsa_6 = cbsa_code, state_6 = STATE)
cbsa_state7 = cbsa_state %>% rename(cbsa_7 = cbsa_code, state_7 = STATE)
cbsa_state8 = cbsa_state %>% rename(cbsa_8 = cbsa_code, state_8 = STATE)
cbsa_state9 = cbsa_state %>% rename(cbsa_9 = cbsa_code, state_9 = STATE)
cbsa_state10 = cbsa_state %>% rename(cbsa_10 = cbsa_code, state_10 = STATE)

# Calculate return rates for graduates of public universities
public_binary1 = completed_tiered %>%
  filter(CONTROL == 1) %>%
  # Determine if individual resided in same CBSA as their high school
  mutate(same_cbsa = ifelse((str_detect(cbsa_1,hs_cbsa)==TRUE | 
                               str_detect(cbsa_2,hs_cbsa)==TRUE |
                               str_detect(cbsa_3,hs_cbsa)==TRUE | 
                               str_detect(cbsa_4,hs_cbsa)==TRUE |
                               str_detect(cbsa_5,hs_cbsa)==TRUE | 
                               str_detect(cbsa_6,hs_cbsa)==TRUE |
                               str_detect(cbsa_7,hs_cbsa)==TRUE | 
                               str_detect(cbsa_8,hs_cbsa)==TRUE |
                               str_detect(cbsa_9,hs_cbsa)==TRUE | 
                               str_detect(cbsa_10,hs_cbsa)==TRUE),1,0),
         in_state_col = ifelse(col_state == STATE,1,0)
         )

# Determine if individual (possibly) resides in same state as CBSA
# Since work location isn't precisely known, it is possible to graduate from
# high in school in NJ but work in NY
pu_ss1 = public_binary1 %>%
  left_join(cbsa_state1, by = "cbsa_1") %>%
  mutate(same_state = ifelse(STATE == state_1,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 1) %>%
  mutate(yr_return = col_end + yr_postgrad)
pu_ss2 = public_binary1 %>%
  left_join(cbsa_state2, by = "cbsa_2") %>%
  mutate(same_state = ifelse(STATE == state_2,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 2) %>%
  mutate(yr_return = col_end + yr_postgrad)
pu_ss3 = public_binary1 %>%
  left_join(cbsa_state3, by = "cbsa_3") %>%
  mutate(same_state = ifelse(STATE == state_3,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 3) %>%
  mutate(yr_return = col_end + yr_postgrad)
pu_ss4 = public_binary1 %>%
  left_join(cbsa_state4, by = "cbsa_4") %>%
  mutate(same_state = ifelse(STATE == state_4,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 4)
pu_ss5 = public_binary1 %>%
  left_join(cbsa_state5, by = "cbsa_5") %>%
  mutate(same_state = ifelse(STATE == state_5,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 5) %>%
  mutate(yr_return = col_end + yr_postgrad)
pu_ss6 = public_binary1 %>%
  left_join(cbsa_state6, by = "cbsa_6") %>%
  mutate(same_state = ifelse(STATE == state_6,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 6) %>%
  mutate(yr_return = col_end + yr_postgrad)
pu_ss7 = public_binary1 %>%
  left_join(cbsa_state7, by = "cbsa_7") %>%
  mutate(same_state = ifelse(STATE == state_7,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 7) %>%
  mutate(yr_return = col_end + yr_postgrad)
pu_ss8 = public_binary1 %>%
  left_join(cbsa_state8, by = "cbsa_8") %>%
  mutate(same_state = ifelse(STATE == state_8,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 8) %>%
  mutate(yr_return = col_end + yr_postgrad)
pu_ss9 = public_binary1 %>%
  left_join(cbsa_state9, by = "cbsa_9") %>%
  mutate(same_state = ifelse(STATE == state_9,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 9) %>%
  mutate(yr_return = col_end + yr_postgrad)
pu_ss10 = public_binary1 %>%
  left_join(cbsa_state10, by = "cbsa_10") %>%
  mutate(same_state = ifelse(STATE == state_10,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 10) %>%
  mutate(yr_return = col_end + yr_postgrad)

# Creates list of people who live in the same state
pu_same_state = rbind(pu_ss1,pu_ss2,pu_ss3,pu_ss4,pu_ss5,
                   pu_ss6,pu_ss7,pu_ss8,pu_ss9,pu_ss10) %>%
  group_by(user_id) %>%
  mutate(yr_return = min(yr_postgrad)) %>%
  select(-c(state_1,yr_postgrad,state_2:state_10)) %>%
  distinct()

# Creates list of people who never lived in the same state
pu_diff_state = public_binary1 %>%
  filter(user_id %nin% pu_same_state$user_id) %>%
  # Creates null binary variable for same_state and yr_return since they never return
  mutate(same_state = 0,
         yr_return = 0)

# Calculate share of graduates who live in same CBSA or state
# and whether their high school and college were in the same state
public_binary = rbind(pu_same_state,pu_diff_state) %>%
  filter(is.na(barrons)==FALSE) %>%
  rename(hs_state = STATE) %>%
  relocate(c(hs_state,hs_cbsa), .before = "col_start") %>%
  relocate(barrons, .before = "hs_name") %>%
  select(-c(cbsa_1:cbsa_10,clean_name:LANDGRNT,cbsa_code.y:degree)) %>%
  mutate(same_cbsa = ifelse(is.na(same_cbsa)==TRUE,0,1)) %>%
  group_by(hs_state,barrons) %>%
  summarize(n = n(),
            same_state = sum(same_state),
            same_cbsa = sum(same_cbsa),
            in_state_col = sum(in_state_col))

# Calculate return rates for graduates of public universities
private_binary1 = completed_tiered %>%
  rename(hs_cbsa = cbsa_code.x) %>%
  filter(CONTROL != 1) %>%
  # Determine if individual resided in same CBSA as their high school
  mutate(same_cbsa = ifelse((str_detect(cbsa_1,hs_cbsa)==TRUE | 
                               str_detect(cbsa_2,hs_cbsa)==TRUE |
                               str_detect(cbsa_3,hs_cbsa)==TRUE | 
                               str_detect(cbsa_4,hs_cbsa)==TRUE |
                               str_detect(cbsa_5,hs_cbsa)==TRUE | 
                               str_detect(cbsa_6,hs_cbsa)==TRUE |
                               str_detect(cbsa_7,hs_cbsa)==TRUE | 
                               str_detect(cbsa_8,hs_cbsa)==TRUE |
                               str_detect(cbsa_9,hs_cbsa)==TRUE | 
                               str_detect(cbsa_10,hs_cbsa)==TRUE),1,0),
         in_state_col = ifelse(col_state == STATE,1,0)
  )

# Determine if individual (possibly) resides in same state as CBSA
# Since work location isn't precisely known, it is possible to graduate from
# high in school in NJ but work in NY
pr_ss1 = private_binary1 %>%
  left_join(cbsa_state1, by = "cbsa_1") %>%
  mutate(same_state = ifelse(STATE == state_1,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 1) %>%
  mutate(yr_return = col_end + yr_postgrad)
pr_ss2 = private_binary1 %>%
  left_join(cbsa_state2, by = "cbsa_2") %>%
  mutate(same_state = ifelse(STATE == state_2,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 2) %>%
  mutate(yr_return = col_end + yr_postgrad)
pr_ss3 = private_binary1 %>%
  left_join(cbsa_state3, by = "cbsa_3") %>%
  mutate(same_state = ifelse(STATE == state_3,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 3) %>%
  mutate(yr_return = col_end + yr_postgrad)
pr_ss4 = private_binary1 %>%
  left_join(cbsa_state4, by = "cbsa_4") %>%
  mutate(same_state = ifelse(STATE == state_4,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 4)
pr_ss5 = private_binary1 %>%
  left_join(cbsa_state5, by = "cbsa_5") %>%
  mutate(same_state = ifelse(STATE == state_5,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 5) %>%
  mutate(yr_return = col_end + yr_postgrad)
pr_ss6 = private_binary1 %>%
  left_join(cbsa_state6, by = "cbsa_6") %>%
  mutate(same_state = ifelse(STATE == state_6,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 6) %>%
  mutate(yr_return = col_end + yr_postgrad)
pr_ss7 = private_binary1 %>%
  left_join(cbsa_state7, by = "cbsa_7") %>%
  mutate(same_state = ifelse(STATE == state_7,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 7) %>%
  mutate(yr_return = col_end + yr_postgrad)
pr_ss8 = private_binary1 %>%
  left_join(cbsa_state8, by = "cbsa_8") %>%
  mutate(same_state = ifelse(STATE == state_8,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 8) %>%
  mutate(yr_return = col_end + yr_postgrad)
pr_ss9 = private_binary1 %>%
  left_join(cbsa_state9, by = "cbsa_9") %>%
  mutate(same_state = ifelse(STATE == state_9,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 9) %>%
  mutate(yr_return = col_end + yr_postgrad)
pr_ss10 = private_binary1 %>%
  left_join(cbsa_state10, by = "cbsa_10") %>%
  mutate(same_state = ifelse(STATE == state_10,1,0)) %>%
  filter(same_state == 1) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  mutate(yr_postgrad = 10) %>%
  mutate(yr_return = col_end + yr_postgrad)

# Creates list of people who live in the same state
pr_same_state = rbind(pr_ss1,pr_ss2,pr_ss3,pr_ss4,pr_ss5,
                      pr_ss6,pr_ss7,pr_ss8,pr_ss9,pr_ss10) %>%
  group_by(user_id) %>%
  mutate(yr_return = min(yr_postgrad)) %>%
  select(-c(state_1,yr_postgrad,state_2:state_10)) %>%
  distinct()

# Creates list of people who never lived in the same state
pr_diff_state = private_binary1 %>%
  filter(user_id %nin% pr_same_state$user_id) %>%
  # Creates null binary variable for same_state and yr_return since they never return
  mutate(same_state = 0,
         yr_return = 0)

# Calculate share of graduates who live in same CBSA or state
# and whether their high school and college were in the same state
private_binary = rbind(pr_same_state,pr_diff_state) %>%
  filter(is.na(barrons)==FALSE) %>%
  rename(hs_state = STATE) %>%
  relocate(c(hs_state,hs_cbsa), .before = "col_start") %>%
  relocate(barrons, .before = "hs_name") %>%
  select(-c(cbsa_1:cbsa_10,clean_name:LANDGRNT,cbsa_code.y:degree)) %>%
  mutate(same_cbsa = ifelse(is.na(same_cbsa)==TRUE,0,1)) %>%
  group_by(hs_state,barrons) %>%
  summarize(n = n(),
            same_state = sum(same_state),
            same_cbsa = sum(same_cbsa),
            in_state_col = sum(in_state_col))

all_binary = public_binary %>%
  full_join(private_binary, by = (c("hs_state","barrons"))) %>%
  # Remove US territories with incomplete data
  filter(hs_state %nin% c("AS","GU","VI","NA")) %>%
  mutate(same_state.x = ifelse(is.na(same_state.x)==TRUE,0,same_state.x),
         same_state.y = ifelse(is.na(same_state.y)==TRUE,0,same_state.y),
         same_cbsa.x = ifelse(is.na(same_cbsa.x)==TRUE,0,same_cbsa.x),
         same_cbsa.y = ifelse(is.na(same_cbsa.y)==TRUE,0,same_cbsa.y),
         in_state_col.x = ifelse(is.na(in_state_col.x)==TRUE,0,in_state_col.x),
         in_state_col.y = ifelse(is.na(in_state_col.y)==TRUE,0,in_state_col.y),
         n.x = ifelse(is.na(n.x)==TRUE,0,n.x),
         n.y = ifelse(is.na(n.y)==TRUE,0,n.y)) %>%
  mutate(n = n.x + n.y,
         same_state = same_state.x + same_state.y,
         same_cbsa = same_cbsa.x + same_cbsa.y,
         in_state_col = in_state_col.x + in_state_col.x) %>%
  rename(pu_same_state = same_state.x,
         pu_same_cbsa = same_cbsa.x,
         pu_in_state_col = in_state_col.x,
         pu_n = n.x,
         pr_same_state = same_state.y,
         pr_same_cbsa = same_cbsa.y,
         pr_in_state_col = in_state_col.y,
         pr_n = n.y)
  

write.csv(all_binary,paste0(filepath,"all_binary.csv"))

totals_state = all_binary %>%
  group_by(hs_state) %>%
  summarize(n = sum(n))
