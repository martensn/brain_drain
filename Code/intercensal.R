library(tidyverse)
library(tidycensus)
library(censusapi)
library(data.table)

census_key <- Sys.getenv("CENSUS_KEY")

# Check to see that the expected key is output in your R console
get_api_key()

# Obtain CBSA-level employment counts
cbsa_shock <- read_csv(file.path(directory,"Data/04","cbsa_shock.csv")) %>%
  dplyr::select(year,cbsa_code,cbsa_actual) %>%
  filter(year %in% c(2000:2019))

# https://bookdown.org/dereksonderegger/444/api-data-queries.html 
# I inferred age groups based on the 2010-19 groups
CensusFactorLevels <- function(name, vintage, variable){
  file <- str_c('https://api.census.gov/data/',vintage,'/',name,
                '/variables/',variable,'.json')
  print(file)
  Meta <- jsonlite::read_json(file) %>% 
    .[['values']] %>% .[['item']] %>% 
    unlist() %>% tibble::enframe() 
  colnames(Meta) <- c(variable, str_c(variable,'_DESC'))
  return(Meta)
}
p <- CensusFactorLevels('pep/charagegroups', 2018, 'AGEGROUP') 

# Get county-level intercensal population estimates 2000-2009
#old <- getCensus(
#  name = "pep/int_charagegroups",
#  vintage = 2000,
#  vars = c("DATE_","STATE","COUNTY","AGEGROUP","POP"),
#  region = "county:*",
#  AGEGROUP = 6,
  #region = "county:001",
#  regionin = "state:26"
#) 
# Removal decennial census estimates, which serve as baselines
#filter(!DATE_ %in% c(1,12)) %>%
#  mutate(year = as.numeric(DATE_) + 1998,
#         GEOID = paste0(STATE,COUNTY)) %>%
#  rename(value = POP) %>%
#  dplyr::select(year,GEOID,value)

# Obtain county-level estimates of 25-64 population by year
get_agegroup <- function(states, agegroups = 5:18) {
#  get_agegroup <- function(states, agegroups = 28) {
    
  results <- list()  # Store results here
  
  for (state in states) {
    for (age in agegroups) {
      cat("Fetching data for state:", state, "age group:", age, "\n")
      
      state_code <- paste0("state:",state)
      # API call for one state and one age group at a time
      temp <- getCensus(
        name = "pep/int_charagegroups",
        key = Sys.getenv("census_key"),
        vintage = 2000,
        vars = c("DATE_","STATE","COUNTY","AGEGROUP","POP"),
        region = "county:*",
        AGEGROUP = age,
        #region = "county:001",
        regionin = state_code
      ) 
      
      # Store the data if it's not NULL
      if (!is.null(temp)) {
        results <- append(results, list(temp))
      }
      
      #Sys.sleep(1)  # Add delay to avoid API rate limits
    }
  }
  
  # Combine all results into a single data frame
  final_data <- bind_rows(results)
  return(final_data)
}


# Example usage: Fetch data for Michigan (26) and Ohio (39)
fips <- str_pad(c(1,2,4:6,9:13,15:42,44:51,53:56),2,"left",pad="0")
raw_age_00 <- get_agegroup(fips)

age_00 <- raw_age_00 %>%
  # Removal decennial census estimates, which serve as baselines
  filter(!DATE_ %in% c(1,12)) %>%
  mutate(year = as.numeric(DATE_) + 1998,
         GEOID = paste0(STATE,COUNTY)) %>%
  group_by(year,GEOID) %>%
  summarize(value = sum(POP, na.rm=TRUE)) %>%
  dplyr::select(year,GEOID,value)

# Get county-level intercensal population estimates 2010-2019
raw_age_10 <- get_estimates(
  geography = "county",
  product = "characteristics",
  breakdown = "AGEGROUP",
  vintage = 2019,
  time_series = TRUE,
  #state = "MI",
  #county = "Kalamazoo",
  output = "tidy") 

# Clean 2010-19 intercensal estimates
age_10 <- raw_age_10 %>%
  filter(!DATE %in% c(1,12)) %>%
  filter(AGEGROUP %in% 5:18) %>%
  mutate(year = 2008 + DATE) %>%
  group_by(year,GEOID) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  dplyr::select(year,GEOID,value)

raw_laus <- read_csv(file.path(directory,"Data/06/bls_laus_data.csv"))
# Run first four chunks of 06_regressions.Rmd to get the regression object

laus = raw_laus %>%
  mutate(across(starts_with("Annual "),as.double)) %>%
  filter(Dataset == "6") %>%
  pivot_longer(cols = starts_with("Annual"),
               names_prefix = "Annual ",
               names_to = "year",
               values_to = "labor_force") %>%
  mutate(GeoFIPS = str_pad(`s ID`,side="left",pad="0",width=5)) %>%
  select("GeoFIPS","year","labor_force") %>%
  left_join(unified_cbsa,by="GeoFIPS") %>%
  mutate(year = as.integer(year)) %>%
  group_by(cbsa_code,year) %>%
  summarize(labor_force = sum(labor_force,na.rm=TRUE)) %>%
  filter(year != 2024)

# Combine intercensal estimates together, creating CBSA x year panel
intercensal <- rbind(age_00,age_10) %>%
  left_join(unified_cbsa, by=c("GEOID"="GeoFIPS")) %>%
  group_by(cbsa_code,year) %>%
  filter(year %in% c(2000:2017)) %>%
  summarize(pop = sum(value,na.rm=TRUE)) %>%
  left_join(cbsa_shock, by = c("cbsa_code","year")) %>%
  filter(!is.na(cbsa_actual)) %>%
  left_join(laus, by = c("cbsa_code","year")) %>%
  mutate(emp_rate = labor_force / pop)

write.csv(intercensal,file.path(directory,"Data/06/intercensal.csv"),row.names = FALSE)




