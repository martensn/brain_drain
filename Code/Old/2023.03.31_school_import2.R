# Author  : Nick Martens
# Date    : 31 Match 2023
# Purpose : Identical code, just with micropolitan areas filtered out
#         : Also creates two new objects to ease geocoding of positions:
#         : unified_cbsa_name + county_expanded

library(readxl)
library(tidyverse)

filepath = "E:/Nick/Stata"
pubfilename = "Public.csv"
privfilename = "Private.csv"
postfilename = "Postsecondary.csv"
cbsafilename = "msa_2020.csv"
countyfilename = "counties.xlsx"
ch

# Delcare %nin% so we can filter observations
`%nin%` = Negate(`%in%`)

# Read in CBSA conversion file
cbsa = read_csv(paste0(filepath,"/",cbsafilename))

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
county = read_excel(paste0(filepath,"/",countyfilename))

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
public = read_delim(paste0(filepath,"/",pubfilename))
private = read_delim(paste0(filepath,"/",privfilename))
postsec = read_delim(paste0(filepath,"/",postfilename))

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

all_col = postsec %>%
  # Removing columns to ensure harmonious merge with K12 data
  select(c(NAME:NMCNTY)) %>%
  select(-c(STFIP)) %>%
  # Rename columns to ensure harmonious merge with K12 data
  rename(GeoFIPS = CNTY) %>%
  # Match to CBSA codes using county-CBSA crosswalk
  left_join(y = unified_cbsa, by = "GeoFIPS") %>%
  mutate(degree = "Bachelor's",
         alt_name = str_to_lower(NAME),
         alt_name = str_trim(alt_name),
         alt_name = str_replace_all(alt_name, "-"," "),
         alt_name = str_replace_all(alt_name, "—"," "),
         alt_name = str_replace_all(alt_name, ","," "),
         alt_name = str_replace_all(alt_name, "main campus",""),
         alt_name = str_replace_all(alt_name," at "," "),
         #  Manual recodes for large universities
         #  We can do more cleaning later
         alt_name = str_replace_all(alt_name, "the pennsylvania state","penn state"),
         alt_name = str_replace_all(alt_name, "at raleigh",""),
         alt_name = str_replace_all(alt_name, "fort collins",""),
         alt_name = str_replace_all(alt_name, "campus immersion",""),
         alt_name = str_replace_all(alt_name, "and agricultural & mechanical college",""),
         alt_name = str_replace_all(alt_name, "university oxford",""),
         alt_name = str_replace_all(alt_name, "the university of","university of"),
         alt_name = str_replace_all(alt_name, "university carbondale","university"),
         alt_name = str_replace_all(alt_name, "a&m","agricultural and mechanical"),
         alt_name = str_replace_all(alt_name, "a & m","agricultural and mechanical"),
         alt_name = str_trim(alt_name),
         alt_name = str_squish(alt_name)) %>%
  mutate(clean_name = alt_name)