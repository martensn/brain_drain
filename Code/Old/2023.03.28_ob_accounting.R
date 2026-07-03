# Total number of users with educations listed
ed_li %>% group_by(user_id) %>% group_keys()

# Total number of users with positions listed
pos = rbind(pos0,pos1) %>% group_by(user_id) 
pos_keys = pos %>% group_keys()

# Education with no experience
ed_only = ed_li %>% filter(user_id %nin% pos_keys$user_id)
pos_only = pos_keys %>% filter(user_id %in% ed_li$user_id)