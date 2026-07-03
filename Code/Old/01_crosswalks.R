


# Create expanded county object for crosswalking positions to CBSA codes
county_expanded = county %>%
  mutate(full_name = paste0(county_name,", ",state_name)) %>%
  select(c(state_code,state_name,GeoFIPS,full_name))

# Unifying state abbreviation and MSA codes to automate construction of non-metro labels
big_chungus = county_expanded %>%
  left_join(unified_cbsa_name, by = "GeoFIPS") %>%
  mutate(cbsa_name = ifelse(is.na(cbsa_name)==TRUE,
                            paste(state_name,"NONMETROPOLITAN AREA"),
                            cbsa_name)) %>%
  mutate(cbsa_name =str_replace_all(cbsa_name, ",","")) %>%
  rename("cbsa_state"=state_name) %>%
  mutate(cbsa_code = ifelse(is.na(cbsa_code)==TRUE,paste0(state_code,"999"),cbsa_code))

name_match = big_chungus %>%
  select(c(cbsa_code,cbsa_name,cbsa_state)) %>%
  distinct() %>%
  mutate(cbsa_city = str_extract(cbsa_name,'(^.{1,}-(?=([A-Z][a-z].*)))|(^.{1,} (?=([A-Z][A-Z].*)))'),
         cbsa_city = str_replace_all(cbsa_city, '-'," ")) %>%
  #separate(cbsa_name, into = c(NA,"cbsa_state"), sep = (' (?=([A-Z][A-Z].*))')) %>%
  as.data.table()