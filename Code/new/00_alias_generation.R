library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(readxl)
library(stringi)
library(tidyverse)
library(table.express)
library(data.table)
library(tidygeocoder)
library(educationdata)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

set.seed(49)
#directory = "/nfs/turbo/lsa-areynoso"
directory = "/Volumes/lsa-areynoso"

#public_school_filename = "ELSI_csv_export_6390435784786712152626.csv"
private_school_filename = "ELSI_csv_export_639044261135902865330.csv"
private_school_address_filename = "ELSI_csv_export_6390455368533882136542.csv"

study_start_year = 2000
study_end_year = 2013

first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    if (is.numeric(x)) NA_real_ else NA_character_
  } else {
    x[[1]]
  }
}


longest_string <- function(x) {
  x <- x[!is.na(x)]
  x <- trimws(x)
  
  if (length(x) == 0) {
    NA_character_
  } else {
    x[which.max(nchar(x))]
  }
}

make_acronym <- function(x) {
  words <- str_split(x, "\\s+")[[1]]
  
  # drop stopwords
  words <- words[!words %in% c("of", "in", "the", "at", "main", "campus")]
  
  if (length(words) <= 1) return(NA_character_)
  
  paste0(str_sub(words, 1, 1), collapse = "")
}


derive_aliases <- function(x) {
  x <- str_to_lower(x)
  x <- str_squish(x)
  
  x_nodash <- str_replace_all(x, "-", " ")
  
  base <- c(
    x,
    x_nodash,
    str_remove(x, "^the "),
    str_remove(x_nodash, "^the "),
    str_remove(x, "^university of "),
    str_remove(x_nodash, "^university of "),
    str_remove(x, "^college of "),
    str_remove(x_nodash, "^college of "),
    str_remove(x, " university$"),
    str_remove(x_nodash, " university$"),
    str_remove(x, " college$"),
    str_remove(x_nodash, " college$")
  ) %>%
    str_squish() %>%
    unique()
  
  # ---- NEW: generate acronyms ----
  acronyms <- base[
    str_detect(base, "(state university$|university$)")
  ] %>%
    map_chr(make_acronym) %>%
    discard(is.na)
  
  unique(c(base, acronyms))
}

wmean <- function(x, w) {
  if (all(is.na(x)) || all(is.na(w))) return(NA_real_)
  weighted.mean(x, w, na.rm = TRUE)
}


clean_strings <- function(x) {
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

# Create different variations of the college name
generate_subunit_aliases <- function(subunit, university, allow_standalone = TRUE) {
  
  base_forms <- c(
    paste(subunit, university),                 # 2
    paste(subunit, "at", university),           # 3
    paste(university, subunit)                  # 5
  )
  
  if (allow_standalone) {
    c(subunit, base_forms)                      # 1 + others
  } else {
    base_forms
  }
}

# Remove the first, middle, and honorific from college names
derive_short_subunit <- function(x) {
  sapply(x, function(single_x) {
    x_clean <- single_x %>%
      str_replace_all(",", "") %>%
      str_squish()
    words <- str_split(x_clean, "\\s+")[[1]]
    words_lower <- str_to_lower(words)
    idx <- which(words_lower %in% c("school", "college"))
    if (length(idx) == 0) return(NA_character_)
    idx <- idx[1]
    if (idx >= 3) {
      suffixes <- c("jr", "jr.", "sr", "sr.", "ii", "iii", "iv", "s.j.")
      before <- words[1:(idx - 1)]
      before_lower <- words_lower[1:(idx - 1)]
      keep_before <- before[!before_lower %in% suffixes]
      if (length(keep_before) == 0) return(NA_character_)
      surname <- tail(keep_before, 1)
      after <- words[idx:length(words)]
      return(str_squish(paste(c(surname, after), collapse = " ")))
    }
    NA_character_
  })
}

# Create state fips crosswalk
state_fips = fips_codes %>% 
  select(state,state_code) %>% 
  distinct() %>%
  mutate(state_fips = as.numeric(state_code)) %>%
  select(state,state_fips) %>%
  rename(state_abbr = state)

# These steps are slow
zcta <- zctas(cb = TRUE, year = 2010)      # ZCTA polygons
county <- counties(cb = TRUE, year = 2010)
zcta_centroids <- zcta %>%
  st_centroid() %>%
  st_join(county, left = FALSE) 
county_centroids <- county %>%
  st_centroid()

# If geocoder cannot find an address, use centroid of ZCTA
zcta_coords <- st_coordinates(zcta_centroids)
zcta_shrunk = zcta_centroids %>%
  mutate(county_fips_zcta = paste0(STATEFP,COUNTYFP),
         zcta_longitude = zcta_coords[, "X"],
         zcta_latitude = zcta_coords[, "Y"]) %>%
  select(ZCTA5,county_fips_zcta,zcta_latitude,zcta_longitude) %>%
  rename(zip = ZCTA5) %>%
  st_drop_geometry()
# IF ZCTA fails, use county centroid
county_coords <- st_coordinates(county_centroids)
county_shrunk = county_centroids %>%
  mutate(county_fips = paste0(STATEFP,COUNTYFP),
         county_longitude = county_coords[, "X"],
         county_latitude = county_coords[, "Y"]) %>%
  select(county_fips,county_latitude,county_longitude) %>%
  st_drop_geometry()

# Create file of all public high school graduating classes since 1986
raw_public_dir = get_education_data(level = "schools",
                                    source = "ccd",
                                    topic = "directory",
                                    filters = list(year = c((study_start_year-7):(study_end_year-3)),
                                                   school_level = c(3,6,7))
)
raw_public_enroll = get_education_data(level = "schools",
                                       source = "ccd",
                                       topic = "enrollment",
                                       filters = list(year = c((study_start_year-7):(study_end_year-3)),
                                                      grade = 12,
                                                      race = 99,
                                                      sex = 99)
)


# Summarize public school enrollment
pub_enr = raw_public_enroll %>%
  filter(enrollment > 0 & !is.na(enrollment)) %>%
  select(year,ncessch,enrollment,leaid) %>%
  distinct() %>%
  group_by(ncessch) %>%
  summarize(hs_start = min(year),
            hs_end = max(year),
            avg_grad_class = round(mean(enrollment),0),
            n_grads = sum(enrollment,na.rm=TRUE))

# Institutional characteristics of high schools
public = raw_public_dir %>%
  mutate(latitude = if_else(latitude %in% c(-3,-2,-1),NA,latitude),
         longitude = if_else(longitude %in% c(-3,-2,-1),NA,longitude),
         street_location = if_else(street_location %in% c(-3,-2,-1),NA,street_location),
         city_location = if_else(city_location %in% c(-3,-2,-1),NA,city_location),
         state_location = if_else(state_location %in% c(-3,-2,-1),NA,state_location),
         zip_location = if_else(zip_location %in% c(-3,-2,-1),NA,zip_location),
         #fips = if_else(fips %in% c(-3,-2,-1),NA,str_pad(fips,width=2,side="left",pad="0")),
         hs_fips = case_when(county_code %in% c(-3,-2,-1) ~ NA,
                             !is.na(county_code) ~ str_pad(county_code,width=5,side="left",pad="0"),
                             TRUE ~ NA),
         virtual = if_else(virtual %in% c(-3,-2,-1),NA,virtual),
  ) %>%
  group_by(ncessch,leaid) %>%
  summarize(
    hs_name     = longest_string(school_name),
    lea_name    = longest_string(lea_name),
    latitude    = first_non_missing(latitude),
    longitude   = first_non_missing(longitude),
    hs_fips     = first_non_missing(hs_fips),
    address     = first_non_missing(street_location),
    city        = first_non_missing(city_location),
    state_abbr  = first_non_missing(state_location),
    zip         = first_non_missing(zip_location),
    school_type = max(school_type)
  ) %>%
  # Remove special education and alternative schools
  filter(school_type %in% c(1,3)) %>%
  mutate(control = if_else(school_type == 1,"public high school","public vocational school"),
         method = if_else(is.na(latitude) | is.na(hs_fips),NA,"NCES")) %>%
  left_join(pub_enr, by = "ncessch") %>%
  rename(hs_id = ncessch, hs_agency_id = leaid, hs_agency_name = lea_name) %>%
  select(hs_id,hs_name,hs_agency_id,hs_agency_name,latitude,longitude,hs_fips,control,
         hs_start,hs_end,avg_grad_class,n_grads,method,address,city,state_abbr,zip)


# Private school data isn't available via API
raw_private = fread(file.path(directory,"Data",private_school_filename),
                    skip=2, colClasses = "character")
raw_private_address = fread(file.path(directory,"Data",private_school_address_filename),
                            skip=2, colClasses = "character")

# Clean private school address data
private_geo <- raw_private_address %>%
  as_tibble() %>%
  pivot_longer(
    cols = matches("\\[Private School\\] \\d{4}-\\d{2}$"),
    names_to = c("variable", "acad_year"),
    names_pattern = "(.*) \\[Private School\\] (\\d{4}-\\d{2})",
    values_to = "value"
  ) %>%
  mutate(
    variable = case_when(
      str_detect(variable, "Physical Address") ~ "address",
      str_detect(variable, "City") ~ "city",
      str_detect(variable, "ZIP") ~ "zip",
      TRUE ~ variable
    ),
    hs_start = as.integer(substr(acad_year, 1, 4)),
    hs_end   = as.integer(substr(acad_year, 6, 7)),
    hs_end   = if_else(hs_end < 50, hs_end + 2000, hs_end + 1900),
    value = na_if(str_trim(value), "†")) %>%
  rename(
    hs_name  = `Private School Name`,
    hs_state = `State Abbr [Private School] Latest available year`,
    hs_id    = `School ID - NCES Assigned [Private School] Latest available year`
  ) %>%
  pivot_wider(
    names_from  = variable,
    values_from = value,
    values_fn   = first_non_missing
  ) %>%
  group_by(hs_id) %>%
  arrange(hs_start) %>%
  summarize(
    address  = first_non_missing(address),
    city     = first_non_missing(city),
    state_abbr = first_non_missing(hs_state),
    zip      = first_non_missing(zip),
    .groups  = "drop"
  )


# Process private school data, which NCES collects bi-annually
priv = raw_private %>%
  as_tibble() %>%
  pivot_longer(
    cols = matches("\\[Private School\\] \\d{4}-\\d{2}$"),
    names_to = c("variable", "acad_year"),
    names_pattern = "(.*) \\[Private School\\] (\\d{4}-\\d{2})",
    values_to = "value"
  ) %>%
  mutate(
    variable = case_when(
      str_detect(variable, "County Code") ~ "hs_fips",
      str_detect(variable, "Grade 12 Students") ~ "graduating_class",
      TRUE ~ variable
    )
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  filter(str_detect(hs_fips,"^\\d+$"),str_detect(graduating_class,"^\\d+$")) %>%
  mutate(graduating_class = as.numeric(graduating_class),
         ay_start = as.integer(substr(acad_year,1,4)),
         ay_end = as.integer(substr(acad_year,6,7)),
         ay_end = if_else(ay_end < 50, ay_end + 2000, ay_end + 1900),
         hs_agency_id = NA,
         hs_agency_name = NA,
         control = "private") %>%
  rename(hs_name       = `Private School Name`,
         hs_state      = `State Name [Private School] Latest available year`,
         hs_id         = `School ID - NCES Assigned [Private School] Latest available year`) %>%
  filter(ay_start %in% c(1993:study_end_year-4)) %>%
  mutate(
    years_covered = ay_end - ay_start + 1,
    weight = graduating_class * years_covered
  ) %>%
  group_by(hs_id, hs_name, control) %>%
  summarize(hs_start = min(ay_start),
            hs_end = max(ay_end),
            avg_grad_class = wmean(graduating_class, weight),
            n_grads = sum(weight, na.rm = TRUE),
            hs_fips = first(na.omit(hs_fips))) %>%
  ungroup() %>%
  mutate(.row_id = dplyr::row_number()) %>%
  left_join(private_geo, by = "hs_id")

# Batching to avoid Census geocoder's limits
batch_size <- 500
batches <- priv %>%
  mutate(batch = ceiling(.row_id / batch_size)) %>%
  group_split(batch)
# Since private schools lack latitude and longitude, all need to be geocoded 
private <- map_dfr(
  batches,
  ~ .x %>%
    geocode(
      street = address,
      city = city,
      state = state_abbr,
      postalcode = zip,
      method = "census",
      full_results = FALSE
    ) %>%
    mutate(method = "Census geocoder"),
  .id = "batch_id") %>%
  arrange(.row_id) %>%
  rename(latitude = lat, longitude = long) %>%
  select(-c(batch_id,batch,.row_id))

# Remove outlying areas and territories
collapsed = rbind(public,private) %>%
  filter(state_abbr %in% state.abb)

# Private schools geocoded via ZIP
geo_zip = collapsed %>%
  filter(is.na(longitude) | is.na(hs_fips)) %>%
  mutate(zip = str_sub(zip,1,5)) %>%
  left_join(zcta_shrunk, by = "zip") %>%
  mutate(hs_fips_imp_flag = if_else(is.na(hs_fips),1,0),
         hs_fips = if_else(is.na(hs_fips),county_fips_zcta,hs_fips)) %>%
  select(-c(latitude,longitude,county_fips_zcta)) %>%
  rename(latitude=zcta_latitude, longitude=zcta_longitude) %>%
  mutate(method = if_else(is.na(latitude),"Unable to geocode","ZCTA centroid"))

# Private schools geocoded via county
geo_county = geo_zip %>%
  filter(is.na(longitude)) %>%
  mutate(zip = str_sub(zip,1,5)) %>%
  left_join(county_shrunk, by = c("hs_fips"="county_fips")) %>%
  select(-c(latitude,longitude)) %>%
  rename(latitude=county_latitude, longitude=county_longitude) %>%
  mutate(method = if_else(is.na(latitude),"Unable to geocode","County centroid"))

# Create single, geocoded file
imputed = geo_zip %>% filter(!hs_id %in% geo_county$hs_id) %>% rbind(geo_county)
schools = collapsed %>% 
  mutate(hs_fips_imp_flag = 0) %>% 
  filter(!hs_id %in% imputed$hs_id) %>% 
  rbind(imputed) %>%
  # Modest standardization of names to improve counts of "redundant names
  mutate(
    hs_name_clean = str_squish(str_to_upper(hs_name)),
    hs_name_clean = str_replace(hs_name_clean, "\\bH\\.?\\s*S\\.?$", "HIGH SCHOOL"),
    hs_name_clean = str_replace(hs_name_clean, "\\bHIGH$", "HIGH SCHOOL"),
    hs_name_clean = str_replace_all(hs_name_clean, "\\bSCH\\b", "SCHOOL"),
    hs_name_clean = str_replace_all(hs_name_clean, "\\bACAD\\b", "ACADEMY"),
    hs_name_clean = str_replace_all(hs_name_clean, "\\bJSHS\\b", "JUNIOR SENIOR HIGH SCHOOL"),
    hs_name_clean = str_replace_all(hs_name_clean, "\\bSHS\\b", "SENIOR HIGH SCHOOL"),
    hs_name_clean = str_replace_all(hs_name_clean, "\\bHS\\b", "HIGH SCHOOL"),
    hs_name_clean = str_replace_all(hs_name_clean, "\\bCTR\\b", "CENTER")
  ) %>%
  mutate(hs_name = hs_name_clean) %>%
  select(-hs_name_clean)

# Names share by multiples schools
ambiguous = schools %>% 
  group_by(hs_name) %>%
  summarize(n = n(),
            n_grads = sum(n_grads)) %>%
  filter(n > 1)


# Next, create a list of colleges granting bachelor's degrees between 2000 and 2013
# Pull directory of basic information about institutions
# Filtering community colleges and other non-baccalaureate institutions
#raw_institutions = get_education_data(level = "college-university",
#                                      source = "ipeds",
#                                      topic = "directory",
#                                      filters = list(year = study_start_year:study_end_year,
#                                                     #  Highest degree offered must be bachelors or graduate degree
#                                                     #offering_highest_degree = 10:31,
#                                                     #  With undergraduate degrees available too
#                                                     offering_undergrad = 1)) 
raw_institutions = fread(file.path(data_dir,"colleges_ipeds_directory.csv"))


# Pull degrees awarded data
raw_deg_award = get_education_data(level = "college-university",
                                   source = "ipeds",
                                   topic = "completions-cip-2",
                                   filters = list(award_level = c(4,7),
                                                  year = c(study_start_year:study_end_year),
                                                  #fips = 26,
                                                  race = 99,
                                                  sex = 99,
                                                  majornum = 1))
saveRDS(raw_deg_award,file.path(data_dir,"raw_deg_award"))

# Pull degrees awarded data
# Only pull even years because reporting isn't mandatory in odd years
raw_instate = get_education_data(level = "college-university",
                                 source = "ipeds",
                                 topic = "fall-enrollment",
                                 filters = list(year = c(1994,1996,1998,2000,2002,2004,2006,2008,2010),
                                                type_of_freshman = 99,
                                                fips = c(1:56),
                                                state_of_residence = c(1:58)),
                                 subtopic = list("residence")) 

# Import CIP-2 award data
deg_award_2000 = fread(file.path(data_dir,"colleges_ipeds_completions-2digcip_2000.csv"))
deg_award_2010 = fread(file.path(data_dir,"colleges_ipeds_completions-2digcip_2010.csv"))
deg_award_2020 = fread(file.path(data_dir,"colleges_ipeds_completions-2digcip_2020.csv"))

# Create wide dataset of the share of awards that were less than a BA
deg_award = rbind(deg_award_2000,deg_award_2010,deg_award_2020) %>%
  table.express::filter(race == 99, sex == 99, cipcode %in% c(99,990000)) %>%
  as_tibble() %>%
  mutate(degree = case_when(award_level == 4 ~ "associate",
                            award_level %in% c(1,2,3,5,6,8,30:33) ~ "sub_ba",
                            award_level == 7 ~ "bachelors",
                            TRUE ~ "graduate_degree")
  ) %>%
  group_by(unitid,year,degree) %>%
  summarize(awards = sum(awards,na.rm=TRUE)) %>%
  pivot_wider(id_cols = c(unitid,year),
              names_from = degree,
              values_from = awards,
              values_fill = 0) %>%
  mutate(lt_ba = case_when((associate + sub_ba + bachelors + graduate_degree) > 0 ~ (associate+sub_ba)/(associate+sub_ba+bachelors+graduate_degree),
                           TRUE ~ 1)) %>%
  select(unitid,year,lt_ba) %>%
  pivot_wider(id_cols = unitid,
              names_from = year,
              names_glue = "lt_ba_{year}",
              values_from = lt_ba,
              values_fill = 0)

# Remove any institutions in which 80 percent of degrees awarded were sub-BA
# in 2000, 2010, and 2020
sub_ba = deg_award %>%
  filter(lt_ba_2000 > 0.75 & lt_ba_2010 > 0.75 & lt_ba_2020 > 0.75) %>%
  left_join(colleges %>% 
              select(unitid,inst_name,inst_control,state_abbr,deg_awarded),
            by = "unitid")
# Collapse raw file so that each OPEID-unitid combo has a single address
geo <- raw_institutions %>%
  as_tibble() %>%
  filter(year >= 2000) %>%
  #filter(year %in% c(study_start_year:study_end_year)) %>%
  #filter(unitid == 196130) %>%
  #filter(offering_highest_level %in% c(3,5,7,9)) %>%
  group_by(unitid,opeid) %>%
  summarize(address    = first_non_missing(address),
            city    = first_non_missing(city),
            state_abbr = first_non_missing(state_abbr),
            latitude    = first_non_missing(latitude),
            longitude   = first_non_missing(longitude),
            county_fips = first_non_missing(county_fips),
            zip = first_non_missing(zip),
            offering_highest_level = max(offering_highest_level, na.rm=TRUE), 
            offering_highest_degree = max(offering_highest_degree, na.rm=TRUE)) %>%
  mutate(
    col_fips = str_pad(county_fips, width = 5, side = "left", pad = "0"),
    zip = str_sub(zip, 1, 5)
  ) %>%
  #group_by(unitid, opeid) %>%
  #mutate(
  #  latitude    = first_non_missing(latitude),
  #  longitude   = first_non_missing(longitude),
  #  col_fips    = first_non_missing(county_fips),
  #  address     = first_non_missing(address),
  #  city        = first_non_missing(city),
  #  state_abbr  = first_non_missing(state_abbr),
  #  zip         = first_non_missing(zip),
  #  offering_highest_level = first_non_missing(offering_highest_level),
  #  offering_highest_degree = first_non_missing(offering_highest_degree)
  #) %>%
  ungroup() %>%
  mutate(
    geocode_method  = if_else(!is.na(col_fips), "IPEDS", NA_character_),
    address = str_squish(str_to_title(address)),
    city    = str_to_title(city)
  ) %>%
  select(-county_fips) %>%
  distinct()

# this is the manifest for tracking alternative
missing_geo = geo %>% filter(is.na(col_fips))

# If no FIPS available, try Census geocoder first  
geocoded = missing_geo %>%
  filter(is.na(col_fips)) %>%
  distinct(unitid, opeid, address, city, state_abbr, zip, offering_highest_level, offering_highest_degree) %>%
  geocode(
    street = address,
    city = city,
    state = state_abbr,
    postalcode = zip,
    method = "census",
    api_options = list(census_return_type = 'geographies'),
    full_results = TRUE
  ) %>%
  mutate(col_fips = if_else(!is.na(state_fips),paste0(state_fips,county_fips),NA),
         geocode_method = "Census geocoder") %>%
  rename(latitude = lat, longitude = long) %>%
  select(unitid,opeid,address,city,state_abbr,zip,latitude,longitude,zip,col_fips,geocode_method,offering_highest_level,offering_highest_degree) %>%
  filter(!is.na(col_fips))


# Colleges geocoded via ZIP
zipcoded = missing_geo %>%
  mutate(zip = str_sub(zip,1,5)) %>%
  left_join(zcta_shrunk, by = "zip") %>%
  select(unitid,opeid,county_fips_zcta,zcta_latitude,zcta_longitude,offering_highest_level,offering_highest_degree)

# Harmonize the missing data, so we have lat/long/fips for as many institutions as possible
mg = missing_geo %>%
  select(unitid,opeid,address,city,state_abbr,zip) %>%
  left_join(geocoded %>% select(unitid,opeid,latitude,longitude,col_fips,geocode_method), by = c("unitid","opeid"), relationship="one-to-one") %>%
  left_join(zipcoded, by = c("unitid","opeid"), relationship="one-to-one") %>%
  mutate(geocode_method = case_when(!is.na(col_fips) ~ geocode_method,
                            !is.na(county_fips_zcta) ~ "ZCTA centroid",
                            TRUE ~ "Unable to identify county"),
         col_fips = case_when(is.na(col_fips) & !is.na(county_fips_zcta) ~ county_fips_zcta,
                              TRUE ~ col_fips),
         latitude = case_when(is.na(latitude) & !is.na(zcta_latitude) ~ zcta_latitude,
                              TRUE ~ latitude),
         longitude = case_when(is.na(longitude) & !is.na(zcta_longitude) ~ zcta_longitude,
                               TRUE ~ longitude)) %>%
  select(unitid,opeid,address,city,state_abbr,latitude,longitude,zip,col_fips,geocode_method,offering_highest_level,offering_highest_degree)

# Create single harmonized dataset of institutional geographic identifiers
col_geos = geo %>%
  anti_join(mg, by = c("unitid","opeid")) %>%
  rbind(mg) 

raw_deg_award = readRDS(file.path(data_dir,"raw_deg_award"))
awards_ba = raw_deg_award %>%
  filter(award_level == 7) %>%
  filter(cipcode == 99) %>%
  select(unitid,year,awards)
awards_as = raw_deg_award %>%
  filter(award_level == 4) %>%
  filter(cipcode == 99) %>%
  select(unitid,year,awards)

parent_overrides <- tribble(
  ~pattern,                         ~parent,
  "MIAMI UNIVERSITY.*",             "MIAMI UNIVERSITY",
  "UNIVERSITY OF WISCONSIN.*",      "UNIVERSITY OF WISCONSIN",
  "UNIVERSITY OF TEXAS.*",          "UNIVERSITY OF TEXAS",
  "PENNSYLVANIA STATE.*","PENNSYLVANIA STATE UNIVERSITY",
  "LOUISIANA STATE UNIV.*","LOUISIANA STATE UNIVERSITY",
  "CORNELL UNIV.*","CORNELL UNIVERSITY",
  "UNIVERSITY OF ALASKA.*","UNIVERSITY OF ALASKA",
  "UNIVERSITY OF ARKANSAS.*","UNIVERSITY OF ARKANSAS",
  "UNIVERSITY OF COLORADO.*","UNIVERSITY OF COLORADO",
  "UNIVERSITY OF KANSAS*","UNIVERSITY OF KANSAS",
  "UNIVERSITY OF MISSISSIPPI.*","UNIVERSITY OF MISSISSIPPI",
  "UNIVERSITY OF NEBRASKA.*","UNIVERSITY OF NEBRASKA",
  "UNIVERSITY OF OKLAHOMA.*","UNIVERSITY OF OKLAHOMA",
  "UNIVERSITY OF SOUTH FLORIDA.*","UNIVERSITY OF SOUTH FLORIDA",
  "UNIVERSITY OF TENNESSEE.*","UNIVERSITY OF TENNESSEE",
  "WEST VIRGINIA UNIVERSITY.*","WEST VIRGINIA UNIVERSITY",
  "ANTIOCH UNIVERSITY.*","ANTIOCH UNIVERSITY",
  "High-Tech Institute.*","High-Tech Institute",
  "TROY STATE UNIVERSITY.*","TROY STATE UNIVERSITY",
  "SUNY.*","STATE UNIVERSITY OF NEW YORK",
  "CUNY.*","CITY UNIVERSITY OF NEW YORK",
  "KENT STATE UNIVERSITY.*","KENT STATE UNIVERSITY",
  #"CARSON-NEWMAN COLLEGE.*","CARSON-NEWMAN COLLEGE",
  "BAKER COLLEGE.*","BAKER COLLEGE",
  #"TRI-STATE UNIVERSITY.*","TRI-STATE UNIVERSITY",
  #"TRI-STATE BIBLE COLLEGE*","TRI-STATE BIBLE COLLEGE",
  "ARIZONA STATE UNIVERSITY.*","ARIZONA STATE UNIVERSITY",
  "INDIANA UNIVERSITY-PURDUE UNIVERSITY.*","INDIANA UNIVERSITY-PURDUE UNIVERSITY",
  #"MID-AMERICA COLLEGE OF FUNERAL SERVICE.*","MID-AMERICA COLLEGE OF FUNERAL SERVICE",
  #"MID-CONTINENT COLLEGE.*","MID-CONTINENT COLLEGE",
  "TEXAS A & M.*","TEXAS A & M UNIVERSITY",
  #"WINSTON-SALEM STATE UNIVERSITY.*","WINSTON-SALEM STATE UNIVERSITY",
  "MONTANA TECH.*","THE UNIVERSITY OF MONTANA",
  "EMBRY RIDDLE AERONAUTICAL.*","EMBRY RIDDLE AERONAUTICAL UNIVERSITY",
  "MARYMOUNT COLLEGE OF FORDHAM UNIVERSITY.*","FORDHAM UNIVERSITY"
)

# Create integrated institutional characteristics file
# Consisting of degree-granting institutions awarding bachelor's degrees
colleges <- raw_institutions %>%
  as_tibble() %>%
  filter(offering_highest_degree %in% 10:31) %>%
  filter(year %in% c(study_start_year:study_end_year)) %>%
  mutate(land_grant = coalesce(as.integer(land_grant), 0L),
         inst_system_flag = coalesce(as.integer(inst_system_flag), 0L),
         inst_system_name = inst_system_name %>%
           str_trim() %>%
           na_if("") %>%
           na_if("-1") %>%
           na_if("-2")) %>%
  group_by(unitid) %>%
  mutate(
    inst_system_name = {
      x <- na.omit(inst_system_name)
      if (length(x) == 0) NA_character_ else first(x)
    }
  ) %>%
  ungroup() %>%
  left_join(awards_ba, by = c("unitid","year")) %>%
  # only one system per unitid
  group_by(unitid,opeid) %>%
  summarize(inst_name = first(inst_name),
            aliases = paste(
              sort(unique(na.omit(clean_strings(inst_alias)))),
              collapse = " | "),
            start_year = min(year),
            end_year = max(year),
            currently_active = min(currently_active_ipeds),
            system_name = first(inst_system_name),
            system_flag = max(inst_system_flag),
            land_grant = max(land_grant),
            inst_control = max(inst_control),
            deg_awarded = sum(awards, na.rm=TRUE),
            inst_control = first(inst_control)
  ) %>%
  left_join(col_geos, by = c("opeid","unitid")) %>%
  mutate(system_opeid = str_sub(opeid,1,6),
         branch_opeid = str_sub(opeid,7,8)) %>%
  # Remove public universities with no enrollment
  # Mostly system offices and non-degree-granting branches
  filter(!(inst_control == 1 & deg_awarded == 0)) %>%
  # Remove branches of private colleges with no degrees awarded
  # We will keep main branches with no degrees, since they could just be
  # unwilling to be eligible for title IX
  filter(!(inst_control %in% c(2,3) & deg_awarded == 0 & branch_opeid %in% str_pad(c(1:99),"left","0",width=2))) %>%
  #filter(branch_opeid %in% str_pad(c(1:99),"left","0",width=2) | !is.na(system_name)) %>%
  mutate(
    inst_name_clean = inst_name |>
      str_to_upper() |>
      #str_replace_all("[[:punct:]]", " ") |>
      str_squish() |>
      str_replace("^([^\\s-]+)-", "\\1__HYPHEN__"),
    parent_university = inst_name_clean |>
      str_replace(
        "\\s*(MAIN CAMPUS|REGIONAL CAMPUS|CAMPUS|BRANCH|EXTENSION|\\bAT\\b\\s+.+|\\bIN\\b\\s+.+|-\\s+.*)$",
        ""
      ) |>
      str_replace("\\s*-\\s*.*$", "") |>
      str_squish() |>
      str_replace("__HYPHEN__", "-")
  ) %>%
  rowwise() %>%
  mutate(
    parent_university = {
      hit <- parent_overrides %>%
        filter(str_detect(inst_name_clean, pattern))
      if (nrow(hit) > 0) hit$parent[1] else parent_university
    }
  ) %>%
  ungroup() %>%
  mutate(parent_institution = case_when(inst_control == 3 & !is.na(system_name) ~ system_name,
                                        TRUE ~ parent_university)) %>%
  # Remove institutions which primairly award associate's degrees
  anti_join(sub_ba %>% select(unitid), by = c("unitid"))
  

# Construct list of community colleges
cc <- raw_institutions %>%
  as_tibble() %>%
  filter(offering_highest_degree %in% 40:41) %>%
  filter(year %in% c(study_start_year:study_end_year)) %>%
  # Add in institutions which formally grant bachelor's degrees
  # though primarily issue associate's degrees
  rbind(raw_institutions %>% filter(unitid %in% sub_ba$unitid)) %>%
  filter(year >= study_start_year) %>%
  mutate(inst_system_flag = coalesce(as.integer(inst_system_flag), 0L),
         inst_system_name = inst_system_name %>%
           str_trim() %>%
           na_if("") %>%
           na_if("-1") %>%
           na_if("-2")) %>%
  group_by(unitid) %>%
  mutate(
    inst_system_name = {
      x <- na.omit(inst_system_name)
      if (length(x) == 0) NA_character_ else first(x)
    }
  ) %>%
  ungroup() %>%
  left_join(awards_as, by = c("unitid","year")) %>%
  # only one system per unitid
  group_by(unitid,opeid) %>%
  summarize(inst_name = first(inst_name),
            aliases = paste(
              sort(unique(na.omit(clean_strings(inst_alias)))),
              collapse = " | "),
            start_year = min(year),
            end_year = max(year),
            currently_active = min(currently_active_ipeds,na.rm=TRUE),
            system_name = first(inst_system_name),
            system_flag = max(inst_system_flag),
            land_grant = max(land_grant),
            inst_control = max(inst_control),
            deg_awarded = sum(awards, na.rm=TRUE),
            inst_control = first(inst_control)
  ) %>%
  anti_join(colleges, by = c("opeid","unitid")) %>%
  left_join(col_geos, by = c("opeid","unitid")) %>%
  mutate(system_opeid = str_sub(opeid,1,6),
         branch_opeid = str_sub(opeid,7,8)) %>%
  # Remove public universities with no enrollment
  # Mostly system offices and non-degree-granting branches
  filter(!(inst_control == 1 & deg_awarded == 0)) %>%
  # Remove branches of private colleges with no degrees awarded
  # We will keep main branches with no degrees, since they could just be
  # unwilling to be eligible for title IX
  filter(!(inst_control %in% c(2,3) & deg_awarded == 0 & branch_opeid %in% str_pad(c(1:99),"left","0",width=2))) %>%
  #filter(branch_opeid %in% str_pad(c(1:99),"left","0",width=2) | !is.na(system_name)) %>%
  mutate(
    inst_name_clean = inst_name |>
      str_to_upper() |>
      #str_replace_all("[[:punct:]]", " ") |>
      str_squish() |>
      str_replace("^([^\\s-]+)-", "\\1__HYPHEN__"),
    parent_university = inst_name_clean |>
      str_replace(
        "\\s*(MAIN CAMPUS|REGIONAL CAMPUS|CAMPUS|BRANCH|EXTENSION|\\bAT\\b\\s+.+|\\bIN\\b\\s+.+|-\\s+.*)$",
        ""
      ) |>
      str_replace("\\s*-\\s*.*$", "") |>
      str_squish() |>
      str_replace("__HYPHEN__", "-")
  ) %>%
  rowwise() %>%
  mutate(
    parent_university = {
      hit <- parent_overrides %>%
        filter(str_detect(inst_name_clean, pattern))
      if (nrow(hit) > 0) hit$parent[1] else parent_university
    }
  ) %>%
  ungroup() %>%
  mutate(parent_institution = case_when(inst_control == 3 & !is.na(system_name) ~ system_name,
                                        TRUE ~ parent_university))


system_abbrev_rules <- tribble(
  ~parent_institution,                    ~sys_abbrev,                               ~patterns,
  
  # ---- Big public systems ----
  "UNIVERSITY OF WISCONSIN",               c("uw"),                                   c("-", " at "),
  "CALIFORNIA STATE UNIVERSITY",           c("csu", "cal state"),                     c("-", " at "),
  "UNIVERSITY OF CALIFORNIA",              c("uc"),                                   c("-", " at "),
  "STATE UNIVERSITY OF NEW YORK",          c("suny"),                                 c("-", " at "),
  "CITY UNIVERSITY OF NEW YORK",           c("cuny"),                                 c("-", " at "),
  "UNIVERSITY OF TEXAS",                   c("ut"),                                   c("-", " at ", " of the "),
  "UNIVERSITY OF NORTH CAROLINA",          c("unc"),                                  c("-", " at "),
  "UNIVERSITY OF ALASKA",                  c("ua"),                                   c("-", " at "),
  "UNIVERSITY OF HAWAII",                  c("uh"),                                   c("-", " at "),
  "UNIVERSITY OF ILLINOIS",                c("illinois"),                             c("-", " at "),
  "UNIVERSITY OF MASSACHUSETTS",           c("umass"),                                c("-", " at "),
  "UNIVERSITY OF MICHIGAN",                c("um","umich"),                           c("-", " at "),
  "UNIVERSITY OF MINNESOTA",               c("umn"),                                  c("-", " at "),
  "UNIVERSITY OF MISSOURI",                c("mu", "mizzou"),                         c("-", " at "),
  "UNIVERSITY OF NEBRASKA",                c("unl"),                                  c("-", " at "),
  "UNIVERSITY OF WASHINGTON",              c("uw"),                                   c("-", " at "),
  "OHIO STATE UNIVERSITY",                 c("osu", "ohio state"),                    c("-", " at "),
  "PENNSYLVANIA STATE UNIVERSITY",        c("psu", "penn state"),                     c("-", " at "),
  "OKLAHOMA STATE UNIVERSITY",            c("okstate", "osu"),                       c("-", " at "),
  "MONTANA STATE UNIVERSITY",             c("msu", "montana state"),                 c("-", " at "),
  "WEST VIRGINIA UNIVERSITY",             c("wvu"),                                  c("-", " at "),
  "LOUISIANA STATE UNIVERSITY",           c("lsu"),                                  c("-", " at "),
  "UNIVERSITY OF ALABAMA",                c("ua", "alabama", "bama"),                c("-", " at "),
  "UNIVERSITY OF ARKANSAS",               c("uark"),                                 c("-", " at "),
  "UNIVERSITY OF SOUTH CAROLINA",         c("usc"),                                  c("-", " at "),
  "UNIVERSITY OF TENNESSEE",              c("ut", "tennessee"),                      c("-", " at "),
  "UNIVERSITY OF OKLAHOMA",               c("ou"),                                   c("-", " at "),
  "UNIVERSITY OF CINCINNATI",             c("uc"),                                   c("-", " at "),
  "UNIVERSITY OF HOUSTON",                c("uh"),                                   c("-", " at "),
  
  # ---- Multi-campus / special systems ----
  "INDIANA UNIVERSITY",                    c("iu"),                                   c("-", " at "),
  "RUTGERS UNIVERSITY",                    c("rutgers"),                              c("-", " at "),
  "LONG ISLAND UNIVERSITY",                c("liu"),                                  c("-", " at "),
  "TEXAS A & M UNIVERSITY",                c("tamu", "texas a&m"),                    c("-", " at "),
  "UNIVERSITY OF MAINE",                   c("umaine"),                               c("-", " at "),
  "UNIVERSITY OF PITTSBURGH",               c("pitt"),                                c("-", " at "),
  "UNIVERSITY OF MARYLAND",                c("umd"),                                  c("-", " at "),
  "UNIVERSITY OF SOUTH FLORIDA",            c("usf"),                                 c("-", " at "),
  "UNIVERSITY OF NEBRASKA",                c("unl"),                                  c("-", " at "),
  
  # ---- Private systems / networks ----
  "BRIGHAM YOUNG UNIVERSITY",              c("byu"),                                  c("-", " at "),
)


extract_branch <- function(inst_name, parent, patterns) {
  x <- str_to_lower(inst_name)
  parent <- str_to_lower(parent)
  
  # remove system name prefix if present
  x <- str_remove(x, paste0("^", parent, "\\s*"))
  
  for (p in patterns) {
    if (str_detect(x, fixed(p))) {
      return(str_trim(str_split(x, fixed(p))[[1]][2]))
    }
  }
  
  NA_character_
}

derive_system_branch_aliases <- function(inst_name, parent, sys_abbrev, patterns) {
  branch <- extract_branch(inst_name, parent, patterns)
  if (is.na(branch) || branch == "") return(character(0))
  
  # ensure branch is clean (no leading separators)
  branch <- str_squish(str_remove(branch, "^[-–—,:]+\\s*"))
  
  expand.grid(
    abbrev = sys_abbrev,
    sep    = c(" ", "-"),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      alias = str_c(abbrev, sep, branch)
    ) %>%
    pull(alias) %>%
    unique()
}

# Create systems-level file
systems = colleges %>% 
  filter(inst_control != 3) %>%
  group_by(parent_institution) %>%
  # Given just the system name, what's the probability of someone graduating from a branch
  mutate(shr_deg_awarded = if_else(deg_awarded>0,deg_awarded/sum(deg_awarded),1)) %>%
  rename(col_state = state_abbr) %>%
  select(unitid,opeid,start_year,end_year,inst_name,parent_institution,col_state,shr_deg_awarded) %>%
  ungroup()

# Derive standardized list of college aliases
derived_aliases <- colleges %>%
  transmute(
    unitid,
    opeid,
    parent_institution,
    inst_name,
    alias = map(inst_name, derive_aliases)
  ) %>%
  unnest(alias)

# Derived standardized list of system-level name aliases
derived_system_aliases <- systems %>%
  filter(shr_deg_awarded < 1) %>%
  transmute(
    parent_institution,
    alias = map(parent_institution, derive_aliases)
  ) %>%
  unnest(alias)

# Derive standardized abbreviated names of college branches
system_branch_aliases <- colleges %>%
  select(unitid, opeid, inst_name, parent_institution) %>%
  inner_join(system_abbrev_rules, by = "parent_institution") %>%
  mutate(
    alias = pmap(
      list(inst_name, parent_institution, sys_abbrev, patterns),
      derive_system_branch_aliases
    )
  ) %>%
  unnest(alias) %>%
  select(-c(sys_abbrev,patterns)) %>%
  mutate(system_indicator = 1L)

# Sub-institution unit names
raw_subunit <- read_csv(file.path(directory,"Data/be_sch_full.csv")) 

# Count unique 
subunit_counts <- raw_subunit %>% 
  dplyr::count(subcollege_name, name = "n_units")

# Create set of aliases associated with each college
# Includes last-name only version of college if name includes donor's full name
subunit_aliases_full <- raw_subunit %>%
  left_join(subunit_counts, by = "subcollege_name") %>%
  mutate(
    is_generic = n_units > 1,
    donor_short = derive_short_subunit(subcollege_name)
  ) %>%
  rowwise() %>%
  mutate(
    alias = list({
      # Official name aliases
      official_aliases <- generate_subunit_aliases(
        subcollege_name,
        parent_university,
        allow_standalone = !is_generic
      )
      # Donor-stripped aliases (if different + not NA)
      donor_aliases <- if (!is.na(donor_short) &&
                           donor_short != subcollege_name) {
        generate_subunit_aliases(
          donor_short,
          parent_university,
          allow_standalone = !is_generic
        )
      } else {
        character(0)
      }
      unique(c(official_aliases, donor_aliases))
    })
  ) %>%
  unnest(alias) %>%
  mutate(alias = str_squish(alias)) %>%
  distinct(unitid, opeid, alias, .keep_all = TRUE) %>%
  ungroup() 

# Create condensed versin for inclusion in the embedding file
subunit_aliases = subunit_aliases_full %>%
  select(alias,unitid,opeid) %>%
  left_join(colleges %>% select(unitid,opeid,inst_name,parent_institution), by = c("unitid","opeid")) %>%
  mutate(system_indicator = 0)

# Derive standardized list of college aliases
cc_derived_aliases <- cc %>%
  transmute(
    unitid,
    opeid,
    parent_institution,
    inst_name,
    alias = map(inst_name, derive_aliases)
  ) %>%
  unnest(alias)

# In some cases I will manually add common abbreviations
parent_overrides <- tribble(
  ~alias,    ~parent_institution,
  "u of m", "UNIVERSITY OF MICHIGAN",
  "u of m", "UNIVERSITY OF MINNESOTA",
  "u of m", "UNIVERSITY OF MAINE",
  "u of m", "UNIVERSITY OF MASSACHUSETTS",
  "u of m", "UNIVERSITY OF MIAMI",
  "u of m", "UNIVERSITY OF MISSISSIPPI",
  "u of m", "UNIVERSITY OF MISSOURI",
  "u of m", "UNIVERSITY OF MONTANA",
  "cal state", "CALIFORNIA STATE UNIVERSITY",
  "app state", "APPALACHIAN STATE UNIVERSITY",
  "university of wa", "UNIVERSITY OF WASHINGTON",
  "university of ct", "UNIVERSITY OF CONNECTICUT",
  "university of notre dame", "UNIVERSITY OF NOTRE DAME",
  "high point university", "HIGH POINT UNIVERSITY",
  "missouri state university", "SOUTHWEST MISSOURI STATE UNIVERSITY",
  "utah valley university", "UTAH VALLEY STATE COLLEGE",
  "university of central missouri", "CENTRAL MISSOURI STATE UNIVERSITY",
  "loyola university maryland", "LOYOLA COLLEGE",
  "bryant university", "BRYANT COLLEGE",
  "robert morris university", "ROBERT MORRIS COLLEGE",
  "elon university", "ELON COLLEGE",
  "oklahoma wesleyan university", "BARTLESVILLE WESLEYAN COLLEGE",
  "bethel university", "BETHEL COLLEGE",
  "daytona state college", "DAYTONA BEACH COMMUNITY COLLEGE",
  "florida state college at jacksonville","FLORIDA COMMUNITY COLLEGE",
  "florida state college jacksonville","FLORIDA COMMUNITY COLLEGE",
  "the citadel","CITADEL MILITARY COLLEGE OF SOUTH CAROLINA",
  "university of mary washington", "MARY WASHINGTON COLLEGE",
  "washburn university", "WASHBURN UNIVERSITY OF TOPEKA",
  "clayton state university", "CLAYTON COLLEGE AND STATE UNIVERSITY",
  "lipscomb university", "DAVID LIPSCOMB UNIVERSITY",
  "baker university", "BAKER UNIVERSITY SCH OF PROF AND GRAD STUDIES",
  "baker university", "BAKER UNIVERSITY SCHOOL OF NURSING",
  "school of the art institute of chicago","SCHOOL OF ART INSTITUTE OF CHICAGO",
  "Southern University and Agricultural and Mechanical College at Baton Rouge","SOUTHERN UNIVERSITY AND A & M COLLEGE",
  "Fashion Institute of Design and Merchandising","FASHION INSTITUTE OF TECHNOLOGY",
  "Longwood University", "LONGWOOD COLLEGE",
  "College of Saint Benedict and Saint John's University", "COLLEGE OF SAINT BENEDICT",
  "Marian University", "MARIAN COLLEGE",
  "Augsburg University", "AUGSBURG COLLEGE",
  "Dixie State University","DIXIE STATE COLLEGE OF UTAH",
  "Keiser College", "KEISER UNIVERSITY",
  "Lakeland University", "LAKELAND COLLEGE",
  "St. Catherine University", "COLLEGE OF SAINT CATHERINE",
  "University of Lynchburg", "LYNCHBURG COLLEGE",
  "Chamberlain University", "CHAMBERLAIN COLLEGE OF NURSING"
)


# Link each system alias to all branches of a university system
system_aliases_expanded <- colleges %>%
  select(unitid, opeid, parent_institution, inst_name) %>%
  distinct() %>%
  left_join(bind_rows(derived_system_aliases, parent_overrides), by = "parent_institution") %>%
  distinct(unitid,opeid,alias, .keep_all=TRUE) %>%
  filter(!is.na(alias)) %>%
  mutate( system_indicator = 1L)

# Transform aliases in IPEDS into data
aliases_long <- bind_rows(
  colleges %>% transmute(unitid, opeid, parent_institution, inst_name, alias = inst_name),
  colleges %>% transmute(unitid, opeid, parent_institution, inst_name, alias = parent_institution),
  colleges %>% transmute(unitid, opeid, parent_institution, inst_name, alias = aliases) %>% separate_rows(alias, sep = "\\s*[,;|\\\\]\\s*"),
  derived_aliases) %>%
  mutate(alias = str_to_lower(alias),
         #alias = str_replace_all(alias, "[^a-z0-9 ]", ""),
         alias = str_squish(alias)) %>%
  filter(!is.na(alias), alias != "") %>%
  bind_rows(system_aliases_expanded) %>%
  bind_rows(system_branch_aliases) %>%
  bind_rows(subunit_aliases) %>%
  distinct(unitid, alias, .keep_all = TRUE)

# Identify aliases shared by multiple institutions (MSU)
alias_counts <- aliases_long %>%
  group_by(alias) %>%
  summarize(
    n_units = n_distinct(unitid),
    n_systems = n_distinct(parent_institution),
    .groups = "drop"
  ) %>%
  mutate(system_indicator_emp = n_units > 1) 

# Create integrated list of aliases for each university
alias <- aliases_long %>%
  left_join(alias_counts, by = "alias") %>%
  mutate(system_indicator = case_when(
    system_indicator == 1L ~ 1L,
    TRUE ~ system_indicator_emp)) %>%
  select(unitid,opeid,alias,system_indicator,parent_institution,inst_name)

# Link each system alias to all branches of a university system
cc_system_aliases_expanded <- cc %>%
  select(unitid, opeid, parent_institution, inst_name) %>%
  distinct() %>%
  left_join(bind_rows(parent_overrides), by = "parent_institution") %>%
  distinct(unitid,opeid,alias, .keep_all=TRUE) %>%
  filter(!is.na(alias)) %>%
  mutate(system_indicator = 1L)

# Transform aliases in IPEDS into data
cc_aliases_long <- bind_rows(
  cc %>% transmute(unitid, opeid, parent_institution, inst_name, alias = inst_name),
  cc %>% transmute(unitid, opeid, parent_institution, inst_name, alias = parent_institution),
  cc %>% transmute(unitid, opeid, parent_institution, inst_name, alias = aliases) %>% separate_rows(alias, sep = "\\s*[,;|\\\\]\\s*"),
  cc_derived_aliases) %>%
  mutate(alias = str_to_lower(alias),
         #alias = str_replace_all(alias, "[^a-z0-9 ]", ""),
         alias = str_squish(alias)) %>%
  filter(!is.na(alias), alias != "") %>%
  bind_rows(cc_system_aliases_expanded) %>%
  distinct(unitid, alias, .keep_all = TRUE)

# Identify aliases shared by multiple institutions (MSU)
cc_alias_counts <- cc_aliases_long %>%
  group_by(alias) %>%
  summarize(
    n_units = n_distinct(unitid),
    n_systems = n_distinct(parent_institution),
    .groups = "drop"
  ) %>%
  mutate(system_indicator_emp = n_units > 1) 

# In some states, both four-year and community colleges operate out of the same system
# For example, in Arkansas the University of Arkansas runs both
# In these instances, the four-year institutions get precedent
col_parents = as_tibble(alias) %>% 
  left_join(colleges %>% select(unitid,opeid,inst_control), by = c("unitid","opeid")) %>%
  filter(inst_control != 3) %>%
  mutate(parent_alias = str_to_lower(parent_institution))

# Create integrated list of aliases for each university
cc_alias <- cc_aliases_long %>%
  anti_join(col_parents, by = c("alias"="parent_alias")) %>%
  left_join(cc_alias_counts, by = "alias") %>%
  mutate(system_indicator = case_when(
    system_indicator == 1L ~ 1L,
    TRUE ~ system_indicator_emp)) %>%
  select(unitid,opeid,alias,system_indicator,parent_institution,inst_name)

# Output used for inspection
as = systems %>%
  filter(shr_deg_awarded < 1) %>%
  left_join(alias, by = c("unitid","opeid","inst_name","parent_institution"))

# When an alias appears among multiple systems, we need a way to choose which 
# university system to use for setup
alias_weights = alias %>%
  left_join(colleges %>% select(unitid,opeid,deg_awarded), by = c("unitid","opeid")) %>%
  group_by(alias,parent_institution) %>%
  summarize(alias_w = sum(deg_awarded)) %>%
  # Normalize weights so it represents the system's share of degrees awarded
  # among all institutions with the same alias
  mutate(alias_w = alias_w/sum(alias_w))



expand_saint <- function(x) {
  if (!str_detect(x, "\\b(st\\.|st|saint)\\b")) return(x)
  
  c(
    str_replace_all(x, "\\b(st\\.|st|saint)\\b", "saint"),
    str_replace_all(x, "\\b(st\\.|st|saint)\\b", "st"),
    str_replace_all(x, "\\b(st\\.|st|saint)\\b", "st.")
  ) %>%
    str_squish() %>%
    unique()
}

hs_terms <- paste0(
  "(",
  "high school|secondary school|",
  "senior high|junior senior high|",
  "jr/sr|jr\\.\\s*sr\\.|jr-sr|jr\\s*&\\s*sr|",
  "junior/senior|junior-senior|",
  "academy|prep|preparatory school|preparatory|",
  "community|comm|",
  "charter school|charter|",
  "school",
  ")"
)

# end-anchored (for standard base)
hs_suffix_rx <- paste0("\\s+", hs_terms, "$")

# anywhere (for aggressive stripping)
hs_any_rx <- paste0("\\b", hs_terms, "\\b")

add_city_prefix <- function(base, city) {
  if (is.na(city) || city == "" || is.na(base) || base == "")
    return(NA_character_)
  
  str_squish(paste(str_to_lower(city), base))
}


derive_hs_aliases <- function(x, city = NA_character_) {
  
  x <- str_to_lower(x)
  x <- str_squish(x)
  
  # ---- full name ----
  full <- x
  
  # ---- standard base (remove trailing suffix) ----
  base <- str_remove(x, hs_suffix_rx) %>%
    str_squish()
  
  # ---- aggressive base (remove any HS term anywhere) ----
  base_aggressive <- str_remove_all(x, hs_any_rx) %>%
    str_squish()
  
  # ---- standardized "high" ----
  high <- if (!is.na(base) && base != "" && base != x) {
    str_squish(paste(base, "high"))
  } else {
    NA_character_
  }
  
  # ---- city + base ----
  city_base <- add_city_prefix(base, city)
  
  # collect core aliases
  aliases <- c(full, base, base_aggressive, high, city_base)
  aliases <- aliases[!is.na(aliases) & aliases != ""]
  
  # ---- expand Saint variants ----
  aliases <- map(aliases, expand_saint) %>%
    unlist()
  
  unique(aliases)
}


derived_hs_aliases <- schools %>%
  transmute(
    hs_id,
    hs_name,
    alias = map2(hs_name, city, derive_hs_aliases)
  ) %>%
  unnest(alias)

hs_aliases_long <- derived_hs_aliases %>%
  mutate(
    alias = str_to_lower(alias),
    alias = str_squish(alias)
  ) %>%
  filter(!is.na(alias), alias != "") %>%
  distinct(hs_id, alias, .keep_all = TRUE)

hs_alias_counts <- hs_aliases_long %>%
  group_by(alias) %>%
  summarize(
    n_schools = n_distinct(hs_id),
    .groups = "drop"
  )

hs_alias <- hs_aliases_long %>%
  left_join(hs_alias_counts, by = "alias") %>%
  left_join(
    schools %>% select(hs_id, n_grads),
    by = "hs_id"
  ) %>%
  group_by(alias) %>%
  mutate(
    total_alumni = sum(n_grads, na.rm = TRUE),
    alias_weight = n_grads / total_alumni
  ) %>%
  ungroup() %>%
  select(hs_id, alias, alias_weight, hs_name)

saveRDS(hs_alias,file.path(directory,"Data/hs_alias.rds"))
saveRDS(alias,file.path(directory,"Data/col_alias.rds"))
saveRDS(cc_alias,file.path(directory,"Data/cc_alias.rds"))
write.csv(hs_alias,file.path(directory,"Data/hs_alias.csv"),row.names=FALSE)
write.csv(alias,file.path(directory,"Data/col_alias.csv"),row.names=FALSE)
write.csv(cc_alias,file.path(directory,"Data/cc_alias.csv"),row.names=FALSE)
saveRDS(schools,file.path(directory,"Data/schools.rds"))
saveRDS(colleges,file.path(directory,"Data/colleges.rds"))
saveRDS(cc,file.path(directory,"Data/cc.rds"))
