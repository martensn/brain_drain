## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(readxl)
library(tidyverse)
library(data.table)


## ----directory, include=FALSE-------------------------------------------------
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")

pubfilename = "Public.csv"
privfilename = "Private.csv"



## ----mode, include=FALSE------------------------------------------------------
Mode <- function(x, na.rm = TRUE)
{
  if(na.rm)
  {
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x,ux)))])
}


## ----negate, include=FALSE----------------------------------------------------
`%nin%` = Negate(`%in%`)


## ----import, include=FALSE----------------------------------------------------

# Import lists of (public + private) high schools and colleges
public = read_delim(file.path(data_dir,"raw/nces/Public.csv"))
private = read_delim(file.path(data_dir,"raw/nces/Private.csv"))



## ----hs clean, include=FALSE--------------------------------------------------
# For some reason public school data includes weird geographic information
public_hs = public %>%
  # Filter to only include institutions offering 12th grade
  filter(G12 > 0) %>%
  # Remove extra columns
  select(c(SCH_NAME:LZIP,NMCNTY,NCESSCH,LATCOD,LONCOD)) %>%
  select(-c(LSTREET2,LSTREET1)) %>%
  # Doña Ana County, New Mexico didn't render correctly so it needs to be corrected
  mutate(NMCNTY = ifelse(NMCNTY=="DoÃ±a Ana County","Doña Ana County",NMCNTY)) %>%
  mutate(full_name = paste0(NMCNTY,", ",LSTATE)) %>%
  # Match to county FIPS code
  left_join(y = county_full, by = "full_name") %>%
  # Match to CBSA codes using county-CBSA crosswalk
  left_join(y = unified_cbsa, by = "GeoFIPS") %>%
  # Remove unnecessary columns
  select(-full_name) %>%
  # Rename columns to ensure harmonious merge with private school data
  rename(NAME = SCH_NAME,
         CITY = LCITY,
         STATE = LSTATE,
         ZIP = LZIP,
         LAT = LATCOD,
         LON = LONCOD,
         hs_unitid = NCESSCH)


private_hs = private %>% 
  # Removing columns to ensure harmonious merge with K12 data
  select(c(NAME:NMCNTY,PPIN,LAT,LON)) %>%
  select(-c(STFIP,STREET)) %>%
  # Rename columns to ensure harmonious merge with K12 data
  rename(GeoFIPS = CNTY,
         hs_unitid = PPIN) %>%
  #  Recode Oglala Lakota County (formerly Shannon), 
  #  South Dakota (FIPS 46102) to 46113
  mutate(GeoFIPS = ifelse(GeoFIPS=="46102","46113",GeoFIPS)) %>%
  # Match to CBSA codes using county-CBSA crosswalk
  left_join(y = unified_cbsa, by = "GeoFIPS")

leading_zero_zip = c("ME","NH","VT","MA","RI","CT","NJ")


replacements <- c("junior" = "", "senior" = "", "middle" = "", "jrsr" = "","the" = "",
                  "high" = "", "hs" = "", "school" = "", "h s" = "", "city" = "")

# Combine public and private high schools and add column for degree type
all_hs = rbind(public_hs, private_hs) %>%
  mutate(degree = "High School",
         NAME = str_to_lower(NAME),
         NAME = str_trim(NAME),
         alt_name = str_replace_all(NAME, "[[:punct:]]"," "),
         alt_name = str_replace_all(alt_name, "main campus",""),
         #        alt_name = str_replace_all(alt_name,"^the ",""),
         alt_name = str_trim(alt_name)) %>%
  mutate(stripped_name1 = str_replace_all(alt_name, replacements)) %>%
  mutate(stripped_name2 = str_replace_all(stripped_name1, "\\s{2,}", " ")) %>%
  mutate(clean_name = alt_name) %>%
  # Some New England + NJ ZIP codes omit the leading one, which systematically messes
  # up distance calculations. If ZIP lacks leading zero, missing one added.
  mutate(ZIP = 
           ifelse(STATE %in% leading_zero_zip & str_starts(ZIP, "0", negate=TRUE),
                  str_sub(str_pad(ZIP,6,side="left",pad="0"),1,5),
                  ZIP) ) %>%
  # Fix typos in ZIP codes (zip code doesn't actually exist)
  # I expect to encounter more errors like this
  # Arkansas High School in Texarkana appears to have mistyped ZIP code
  mutate(ZIP = case_when(ZIP == "70488" ~ "70448",
                         # Lakeshore High Schol in St. Tammany Parish, LA
                         ZIP == "71875" ~ "71854",
                         #  Community High in Beaverton, OR
                         ZIP == "97003" ~ "97006",
                         TRUE ~ ZIP)) %>%
  filter(is.na(cbsa_code)==FALSE) %>%
  as.data.table()


