# Author  : Nick Martens
# Date    : 31 Match 2023
# Purpose : Identical code, just with micropolitan areas filtered out
#         : Also creates two new objects to ease geocoding of positions:
#         : unified_cbsa_name + county_expanded

library(readxl)
library(tidyverse)

filepath = "E:/Nick/Stata/"
pubfilename = "Public.csv"
privfilename = "Private.csv"
postfilename = "ipeds_hd2021.csv"
cbsafilename = "msa_2020.csv"
countyfilename = "counties.xlsx"
chettyfilename = "chetty_college.xlsx"
barronsfilename = "barrons_mod.xlsx"

# Declare %nin% so we can filter observations
`%nin%` = Negate(`%in%`)

# Read in CBSA conversion file
cbsa = read_csv(paste0(filepath,cbsafilename))

# Pad FIPS county code
cbsa$`FIPS County Code` = str_pad(cbsa$`FIPS County Code`, width = 3, side = "left", pad = "0")
cbsa$`FIPS State Code` = str_pad(cbsa$`FIPS State Code`, width = 2, side = "left", pad = "0")
cbsa$`CBSA Code` = as.character(cbsa$`CBSA Code`)

# Paste state code 
cbsa_fips = cbsa %>%
  mutate(GeoFIPS = paste0(`FIPS State Code`,`FIPS County Code`)) %>%
  select(c(`CBSA Code`,`GeoFIPS`)) %>%
  rename(cbsa_code = `CBSA Code`)

# Read in county list
county = read_excel(paste0(filepath,countyfilename))

# Pad FIPS codes with zeroes so R interpets them correctly
#county$`FIPS County Code` = str_pad(county$`FIPS County Code`, width = 3, side = "left", pad = "0")
#county$`FIPS State Code` = str_pad(county$`FIPS State Code`, width = 2, side = "left", pad = "0")
#county$`GeoFIPS` = str_pad(county$`GeoFIPS`, width = 5, side = "left", pad = "0")

# Filter counties to only include counties outside of CBSAs
# Create psuedo-CBSA code for non-CBSA counties by adding "999" to the end
cbsa_na = county %>%
  mutate(noncbsa_code = paste0(county$`Official Code State`,"999")) %>%
  filter(`Official Code County` %nin% cbsa_fips$GeoFIPS) %>%
  # filter(is.na(cbsa_code)==TRUE) %>%
  mutate(cbsa_code = noncbsa_code) %>%
  select(c(`Official Code County`,noncbsa_code)) %>%
  rename(GeoFIPS = `Official Code County`,
         cbsa_code = noncbsa_code)

# Join counties with CBSA codes and psuedo-coded non-CBSA counties
unified_cbsa = rbind(cbsa_fips,cbsa_na)

cbsa_code_name = cbsa[c(1,4)] %>%
  rename("cbsa_code" = `CBSA Code`,
         "cbsa_name" = `CBSA Title`) %>%
  distinct()
unified_cbsa_name = unified_cbsa %>%
  left_join(cbsa_code_name, by = "cbsa_code")

# Create column combining county name and state abbreviation
county_full = county %>%
  mutate(full_name = paste0(`Name with legal/statistical area description`,", ",`Official Name State`)) %>%
  select(c(`Official Code County`,full_name)) %>%
  rename(GeoFIPS = `Official Code County`)

# Create expanded county object for crosswalking positions to CBSA codes
county_expanded = county %>%
  mutate(full_name = paste0(`Name with legal/statistical area description`,", ",`Official Name State`)) %>%
  select(c(`Official Code State`:`Official Code County`,full_name)) %>%
  rename(GeoFIPS = `Official Code County`,
         state_abb = `Official Code State`, 
         state_code = `Official Code State`)

# Import csv files
public = read_delim(paste0(filepath,pubfilename))
private = read_delim(paste0(filepath,privfilename))
postsec = read_delim(paste0(filepath,postfilename))

# For some reason public school data includes weird geographic information
public_hs = public %>%
  # Filter to only include institutions offering 12th grade
  filter(G12 > 0) %>%
  # Remove extra columns
  select(c(SCH_NAME:LZIP,NMCNTY)) %>%
  select(-LSTREET2) %>%
  mutate(full_name = paste0(NMCNTY,", ",LSTATE)) %>%
  # Match to county FIPS code
  left_join(y = county_full, by = "full_name") %>%
  # Match to CBSA codes using county-CBSA crosswalk
  left_join(y = unified_cbsa, by = "GeoFIPS") %>%
  # Remove unnecessary columns
  select(-full_name) %>%
  # Rename columns to ensure harmonious merge with private school data
  rename(NAME = SCH_NAME,
         STREET = LSTREET1,
         CITY = LCITY,
         STATE = LSTATE,
         ZIP = LZIP)

private_hs = private %>% 
  # Removing columns to ensure harmonious merge with K12 data
  select(c(NAME:NMCNTY)) %>%
  select(-c(STFIP)) %>%
  # Rename columns to ensure harmonious merge with K12 data
  rename(GeoFIPS = CNTY) %>%
  # Match to CBSA codes using county-CBSA crosswalk
  left_join(y = unified_cbsa, by = "GeoFIPS")

# Combine public and private high schools and add column for degree type
all_hs = rbind(public_hs, private_hs) %>%
  mutate(degree = "High School",
         NAME = str_to_lower(NAME),
         NAME = str_trim(NAME),
         alt_name = str_replace_all(NAME, "-"," "),
         alt_name = str_replace_all(alt_name, "—"," "),
         alt_name = str_replace_all(alt_name, ","," "),
         alt_name = str_replace_all(alt_name, "main campus",""),
         #        alt_name = str_replace_all(alt_name,"^the ",""),
         alt_name = str_trim(alt_name)) %>%
  mutate(clean_name = alt_name)

all_col_unrated = postsec %>%
  # Remove institutions that don't offer bachelor's degrees
  filter(HLOFFER > 4) %>%
  # Removing columns to ensure harmonious merge with K12 data
  select(c(UNITID:FIPS,CONTROL,C21UGPRF,CARNEGIE,HBCU,TRIBAL,LANDGRNT,CBSA,CBSATYPE)) %>%
  # Rename columns to ensure harmonious merge with K12 data
#  rename(GeoFIPS = CNTY) %>%
  # Match to CBSA codes using county-CBSA crosswalk
  mutate(FIPS = str_replace_all(FIPS," ","0"),
         CBSA = ifelse(CBSATYPE != 1,paste0(FIPS,999),CBSA),
         degree = "Bachelor's",
         alt_name = str_to_lower(INSTNM),
         alt_name = str_trim(alt_name),
         alt_name = str_replace_all(alt_name, "-"," "),
         alt_name = str_replace_all(alt_name, "—"," "),
         alt_name = str_replace_all(alt_name, ","," "),
         alt_name = str_replace_all(alt_name, "main campus",""),
         alt_name = str_replace_all(alt_name," at "," "),
         #  Manual recodes for large universities
         #  We can do more cleaning later
#         alt_name = str_replace_all(alt_name, "the pennsylvania state","penn state"),
#         alt_name = str_replace_all(alt_name, "at raleigh",""),
#         alt_name = str_replace_all(alt_name, "fort collins",""),
#         alt_name = str_replace_all(alt_name, "campus immersion",""),
#         alt_name = str_replace_all(alt_name, "and agricultural & mechanical college",""),
#         alt_name = str_replace_all(alt_name, "university oxford",""),
         alt_name = str_replace_all(alt_name, "the university of","university of"),
#         alt_name = str_replace_all(alt_name, "university carbondale","university"),
         alt_name = str_replace_all(alt_name, "a&m","agricultural and mechanical"),
         alt_name = str_replace_all(alt_name, "a & m","agricultural and mechanical"),
         alt_name = str_trim(alt_name),
         alt_name = str_squish(alt_name)) %>%
  rename(cbsa_code = CBSA,
         col_state = STABBR)

# Merge selectivity rating with geographic data so there's only big merge with LI
# First use the data from Chetty (2019) found at
# https://opportunityinsights.org/wp-content/uploads/2018/04/mrc_table10.csv
chetty = read_xlsx(paste0(filepath,chettyfilename)) %>%
  select(name,barrons) %>%
  rename(col_name = name) %>%
  mutate(alt_name = str_to_lower(col_name),
         alt_name = str_trim(alt_name),
         alt_name = str_replace_all(alt_name, "-"," "),
         alt_name = str_replace_all(alt_name, "—"," "),
         alt_name = str_replace_all(alt_name, ","," "),
         alt_name = str_replace_all(alt_name, "main campus",""),
         alt_name = str_replace_all(alt_name, " at "," "),
         alt_name = str_replace_all(alt_name, "the university of","university of"),
         alt_name = str_replace_all(alt_name, "a&m","agricultural and mechanical"),
         alt_name = str_replace_all(alt_name, "a & m","agricultural and mechanical"),
         alt_name = str_trim(alt_name),
         alt_name = str_squish(alt_name))

# Since their data collapses multiple branches of some universities into a 
# single entry, I supplement with Barron's 2001 data available in the UMich library
barrons = read_xlsx(paste0(filepath,barronsfilename)) %>%
  rename(col_name = name,
         col_state = state) %>%
  mutate(alt_name = str_to_lower(col_name),
         alt_name = str_trim(alt_name),
         alt_name = str_replace_all(alt_name, "-"," "),
         alt_name = str_replace_all(alt_name, "—"," "),
         alt_name = str_replace_all(alt_name, ","," "),
         alt_name = str_replace_all(alt_name, "main campus",""),
         alt_name = str_replace_all(alt_name, " at "," "),
         alt_name = str_replace_all(alt_name, "the university of","university of"),
         alt_name = str_replace_all(alt_name, "a&m","agricultural and mechanical"),
         alt_name = str_replace_all(alt_name, "a & m","agricultural and mechanical"),
         alt_name = str_trim(alt_name),
         alt_name = str_squish(alt_name))


# Begin by matching Chetty to all_col
# Chetty includes 913 2-year schools that shouldn't show up in our data
# The remaining 1,550 should show up
tiered_chetty = all_col_unrated %>%
  left_join(chetty, by = "alt_name") %>%
  filter(is.na(col_name)==FALSE) %>%
  # To appease the rbind gods
  select(-c(col_name))

# Create crosswalk with 2001 Barron's ratings
tiered_barrons = all_col_unrated %>%
  left_join(chetty, by = "alt_name") %>%
  filter(is.na(col_name)==TRUE) %>%
  select(-c(col_name:barrons)) %>%
  left_join(barrons, by = c("alt_name","col_state")) %>%
  select(-col_name) %>%
  filter(is.na(barrons)==FALSE)

barrons_chetty = rbind(tiered_chetty,tiered_barrons) %>%
  select(c(alt_name,cbsa_code,barrons)) %>%
  distinct()
#  rename(col_name = NAME)

all_col = all_col_unrated %>%
  left_join(barrons_chetty, by = c("alt_name","cbsa_code"))

completed_neither = all_col_unrated %>%
  left_join(chetty, by = "alt_name") %>%
  filter(is.na(col_name)==TRUE) %>%
  select(-c(col_name:barrons)) %>%
  left_join(barrons, by = c("alt_name","col_state")) %>%
  select(-col_name) %>%
  filter(is.na(barrons)==TRUE)