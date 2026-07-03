library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggmap)

starttime = Sys.time()

Mode <- function(x, na.rm = TRUE)
{
  if(na.rm)
  {
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x,ux)))])
}

register_google(key = "AIzaSyDky02qwEUreJO9ZKFOYA2xR_0BWOA784w")
#has_google_key()
#google_key()
#has_google_client()
#has_google_signature()

#	Read in files
filepath = "E:/Nick/Stata"
user_edfilename = "education_cleaned.csv"
user_ed = read_delim(paste0(filepath,"/",user_edfilename),
                     col_names=TRUE,
                     show_col_types = FALSE,
                     delim=",") 
user_ed_us = user_ed %>%
  filter(v13 == "United States")

geo("100 Main St New York, NY",
    full_results = TRUE,
    method = "census", api_options = list(census_return_type = geographies.Counties)
)

geo("100 Main St New York, NY",
    full_results = FALSE,
    method = "census", api_options = list(census_return_type = "Counties")
)


library(dplyr)
library(purrr)
library(data.table)
library(opencage)

opencage_key <- "ee7616c6453a450b820ec42aa43e35d1"
  
  map(sample(user_ed$v14, 5), function(x) opencage_forward(x, key = opencage_key)) %>%
  map('results') %>%
  data.table::rbindlist(., fill = TRUE)

user_ed01 = user_ed