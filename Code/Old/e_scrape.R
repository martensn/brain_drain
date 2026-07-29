library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(stringdist)

col_alias <- readRDS(file.path(data_dir,"intermediate/col_alias.rds"))

us_states <- tolower(c(
  "Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut",
  "Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa",
  "Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan",
  "Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
  "New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio",
  "Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota",
  "Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia",
  "Wisconsin","Wyoming"
))


# helper: clean school names
clean_text <- function(x) {
  x |>
    str_replace_all("\\[.*?\\]", "") |>  # remove [1], [2]
    str_squish()
}

# scrape a Wikipedia list page
scrape_business_table <- function(url) {
  message("Scraping business schools")
  pg <- read_html(url)
  rows <- pg %>% html_elements("table.wikitable tbody tr")
  map_dfr(rows, function(r) {
    cells <- r %>% html_elements("td")
    if (length(cells) < 2) return(NULL)
    texts <- cells %>%
      html_text2() %>%
      clean_text() %>%
      tolower()
    
    # drop empty cells
    texts <- texts[texts != ""]
    # drop state column if present
    if (texts[1] %in% us_states) {
      texts <- texts[-1]
    }
    if (length(texts) < 2) return(NULL)
    tibble(
      subcollege_name   = texts[1],
      parent_university = texts[2],
      school_type       = "business",
      source            = "wikipedia"
    )
  }) %>%
    distinct()
}

scrape_engineering_table <- function(url) {
  message("Scraping engineering schools")
  pg <- read_html(url)
  rows <- pg %>% html_elements("table.wikitable tbody tr")
  map_dfr(rows, function(r) {
    cells <- r %>% html_elements("td")
    if (length(cells) < 2) return(NULL)
    texts <- cells %>%
      html_text2() %>%
      clean_text() %>%
      tolower()
    
    # drop empty cells
    texts <- texts[texts != ""]
    # drop state column if present
    if (texts[1] %in% us_states) {
      texts <- texts[-1]
    }
    if (length(texts) < 2) return(NULL)
    tibble(
      subcollege_name   = texts[3],
      parent_university = texts[2],
      school_type       = "engineering",
      source            = "wikipedia"
    )
  }) %>%
    distinct()
}



# -----------------------------
# scrape both lists
# -----------------------------
business_url <- "https://en.wikipedia.org/wiki/List_of_business_schools_in_the_United_States"
engineering_url <- "https://en.wikipedia.org/wiki/List_of_engineering_schools#United_States"

business_schools <- scrape_business_table(business_url)
engineering_schools <- scrape_engineering_table(engineering_url)[267:640,] %>%
  filter(!is.na(subcollege_name)) %>%
  filter(subcollege_name != "yes")

be_sch = rbind(business_schools,engineering_schools) %>%
  left_join(col_alias %>%
              filter(system_indicator == 0) %>%
              select(unitid,opeid,alias), 
            by = c("parent_university"="alias"))
fwrite(be_sch,file.path(data_dir,"intermediate/be_sch.csv"))