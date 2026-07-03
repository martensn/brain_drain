# Runs with memory set to 75 GB

library(data.table)
library(dplyr)
library(readr)
library(tidyr)

library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
data_dir = file.path(directory,"Data")
file_dir = file.path(data_dir,"raw/revelio/2026.04.09_education.csv")


# normalize once
normalize_ed <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  gsub("\\s+", " ", trimws(x))
}

# strong signals
college_rx <- "\\b(university|univeristy|univ|u|college|colleges|institute of technology|polytechnic|(wharton|new) school|engineering|education|graduate|public health|management|mines|music|law|medicine|medical|business|pharmacy|economics|nursing|seminary)\\b"
hs_rx <- "\\b(high school|highschool|secondary|prep|preparatory|academy|school|high|h s|h. s.|h.s.|hs|schools|charter|senior|sr)\\b"

# negative evidence
non_school_rx <- "\\b(army|navy|air force|marines|company|corp|course|courses|coursework|society|class|seminar|services|consulting|training|sales|hospital|program|medical center|coach|coaching|test|tax|executive|acting|photography|bank|association|holistic|bartending|mixology|cosmetology|accounting|driving|tractor trailer|board|computer|extension|licensed|license|certificate|certification|certified|real estate|realtor|realtors|refrigeration|hair|nails|hypnotherapy|hypnotism|hypnotist|yoga|massage|professional|interior design|hairstyling|beauty|fire|police)\\b"

# weak signals (use only if undecided)
college_weak_rx <- "\\b(state|tech|poly|inst)\\b"
hs_weak_rx <- "\\b(district|isd|usd|county|regional|career center|technical center|career (and|&) technical|vocational|votech|vo-tech|catholic|collegiate|grammar)\\b"
foreign_rx <- paste0(
  "(?i)", # case-insensitive
  "(",
  # Romance languages
  "universit[aàáâãäåæ]|universidad|universidade|università|universitat|universitaire|universite|universitatea|universita|universiteti|uniwersytet|universitu00e0|accademia|nationale",
  "école|ecole|escuela|lycée|lycee|lyc|ecol|liceo|lyceum|colegio|colégio|istituto|instituto|seminario|scuola|catholique|escola|facultad|faculdades|faculdade|estudio|conservatorio|centro|comercio",
  # Germanic / Nordic
  "hochschule|fachhochschule|universität|gymnasium|gymnasiet|gimnasio|skole|skool|skola|seminarium|schule|collegium|Universitu00e4t|hogeschool",
  # Slavic / Eastern Europe
  "škola|skola|gimnazija|licej|akademija|universitet|universitetas|szkol",
  # Asian / transliterations
  "daxue|zhongxue|gaozhong|xueyuan|",
  # Generic
  "akademi|akademie|academia|tecnico|tecnológica|technologiko|tecnologico",
  "conservatoire|abroad|foreign|federazione",
  ")"
)

has_non_latin_script <- function(x) {
  # Detect CJK ranges explicitly (safe, fast)
  grepl(
    "[\u4E00-\u9FFF\u3040-\u30FF\uAC00-\uD7AF]",
    x
  )
}

has_encoding_issue <- function(x) {
  # Any UTF-8 lead byte rendered as Latin-1 OR raw CJK blocks
  grepl(
    paste0(
      "(",
      "[ÃÄÅÆÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ]", "|",  # UTF-8 lead bytes
      "[â€˜â€™â€œâ€�â€“â€”]", "|",          # smart punctuation
      "[ä¸åæçèéêëìíîïñòóôõöøùúûüýþ]", "|", # common misdecoded bytes
      "[\\p{Han}\\p{Hiragana}\\p{Katakana}\\p{Hangul}]", # real CJK
      ")"
    ),
    x,
    perl = TRUE
  )
}

has_any_letter <- function(x) {
  grepl("\\p{L}", x, perl = TRUE)
}
is_not_school <- function(x) {
  # NA or empty or whitespace-only
  if (is.na(x) || grepl("^\\s*$", x)) return(TRUE)
  
  # No letters at all → only numbers / symbols / whitespace
  !has_any_letter(x)
}

has_mojibake_strong <- function(x) {
  grepl(
    paste0(
      "(",
      "Ã[\\x80-\\xBF]|",   # Ã© Ã§ Ã¯ Ã‰ etc
      "Å[\\x80-\\xBF]|",   # Å¡ Å¾ etc
      "Ä[\\x80-\\xBF]|",   # Äƒ etc
      "â€[\\x80-\\xBF]|",  # â€‹ â€™ â€“ etc
      "â„¢|â€œ|â€�|â€“|â€”",
      ")"
    ),
    x,
    perl = TRUE
  )
}


normalize_high_school <- function(x) {
  # Work on lowercase copy
  x <- str_squish(tolower(x))
  
  # Replace variants with canonical "high school"
  x <- gsub(
    "\\b(h\\s*\\.?\\s*s\\.?|high\\s*school|highschool|high(?=\\s|$))\\b",
    "high school",
    x,
    perl = TRUE
  )
  x
}


classify_ed_string <- function(x_raw) {
  
  x <- normalize_ed(x_raw)
  
  # ---- hard rejects ----
  if (is.na(x) || x == "") return("not_school")
  if (is_not_school(x))   return("not_school")
  if (grepl(non_school_rx, x)) return("not_school")
  
  # ---- foreign: non-latin scripts ----
  if (has_non_latin_script(x)) return("foreign")
  # ---- foreign: mojibake ----
  if (has_mojibake_strong(x)) return("foreign")
  # ---- foreign: education vocabulary ----
  if (grepl(foreign_rx, x)) return("foreign")
  
  # ---- college / HS (domestic only) ----
  if (grepl(college_rx, x)) return("college")
  if (grepl(hs_rx, x))      return("high_school")
  
  # ---- weak cues ----
  if (grepl(college_weak_rx, x)) return("college")
  if (grepl(hs_weak_rx, x))      return("high_school")
  
  "other"
}

# Construct crosswalk from rsid to colleges, use the embeddings as a supplement
input <- fread(file_dir, select=c("university_name","rsid","degree","university_country","university_location"))
gc()

input_col = input[!is.na(rsid),.N, by = .(university_name,university_country,rsid,degree,university_location)]
input_col[,clean_name := tolower(university_name)]
#saveRDS(input_col,file.path(data_dir,"intermediate/input_col.rds"))

input_col = readRDS(file.path(data_dir,"intermediate/input_col.rds"))

col_alias <- readRDS(file.path(data_dir,"intermediate/col_alias.rds"))
hs_alias <- readRDS(file.path(data_dir,"intermediate/hs_alias.rds"))
cc_alias <- readRDS(file.path(data_dir,"intermediate/cc_alias.rds"))
hs_alias_strings = unique(hs_alias[, c("alias"), with = FALSE])
col_alias_strings = unique(col_alias[, c("alias"), with = FALSE])
cc_alias_strings = unique(cc_alias[, c("alias"), with = FALSE])


col_ <- readRDS(file.path(data_dir,"intermediate/col_strings.rds")) %>%
  select(raw_string,alias,unitid,opeid,system_indicator,ed_type,ambiguous_name,method) %>%
  distinct()
hs_ <- readRDS(file.path(data_dir,"intermediate/hs_strings.rds")) %>%
  select(raw_string,alias,hs_id,ed_type,ambiguous_name,method) %>%
  distinct()
#raw_ind <- fread(file_dir, select=c("user_id","rsid","degree"))
# Subset to unambiguous names at first
col_unambig = col_ %>% filter(ambiguous_name == 0) %>% distinct()
hs_unambig = hs_ %>% filter(ambiguous_name == 0) %>% distinct()
# Why not just use both datasets, rbind, then remove duplicates
# Just take the most common one


# Avoid accidental matches on high school
col_p0 = input_col %>% 
  filter(university_country == "United States", degree %in% c("Bachelor","")) %>% 
  anti_join(hs_, by = c("clean_name"="raw_string")) %>%
  anti_join(hs_, by = c("clean_name"="alias")) %>%
  anti_join(hs_alias, by = c("clean_name"="alias"))
col_p0[, ed_type := vapply(clean_name, classify_ed_string, character(1))]
# Remove any obviously non-collegiate names for post-secondary institutions
col_p1 = col_p0 %>%
  filter(ed_type == "college") %>%
  as_tibble()
# Attempt merges based on a few different potential merge keys
col_p2 = col_p1 %>%
  inner_join(col_, by = c("clean_name"="raw_string","ed_type"), relationship = "many-to-many") %>% 
  group_by(university_name,rsid,unitid,opeid,system_indicator,ambiguous_name) %>%
  summarize(N = sum(N))%>%
  mutate(match_type = "String col_$raw_string (1)")
col_p3 = col_p1 %>%
  filter(!rsid %in% col_p2$rsid) %>%
  inner_join(col_ %>% select(-raw_string) %>% distinct(), by = c("clean_name"="alias","ed_type"), relationship = "many-to-many") %>% 
  group_by(university_name,rsid,unitid,opeid,system_indicator,ambiguous_name) %>%
  summarize(N = sum(N)) %>%
  mutate(match_type = "String col_$alias (2)")
col_p4 = col_p1 %>%
  filter(!rsid %in% col_p2$rsid) %>%
  filter(!rsid %in% col_p3$rsid) %>%
  inner_join(col_alias, by = c("clean_name"="alias")) %>% 
  group_by(university_name,rsid,unitid,opeid,system_indicator) %>%
  summarize(N = sum(N)) %>%
  mutate(ambiguous_name = 0,
         match_type = "String col_alias$alias (3)")

# Repeat the process for high schools
hs_p0 = input_col %>% 
  filter(university_country == "United States", degree %in% c("High School","")) %>% 
  filter(!rsid %in% col_p1$rsid)
hs_p0[, ed_type := vapply(clean_name, classify_ed_string, character(1))]
# Remove any obviously non-collegiate names for post-secondary institutions
hs_p1 = hs_p0 %>%
  filter(ed_type == "high_school") %>%
  as_tibble()
# Attempt merges based on a few different potential merge keys
hs_p2 = hs_p1 %>%
  inner_join(hs_, by = c("clean_name"="raw_string","ed_type"), relationship = "many-to-many") %>% 
  group_by(university_name,rsid,hs_id,ambiguous_name) %>%
  summarize(N = sum(N))%>%
  mutate(match_type = "String col_$raw_string (1)")
hs_p3 = hs_p1 %>%
  filter(!rsid %in% hs_p2$rsid) %>%
  inner_join(hs_ %>% select(-raw_string) %>% distinct(), by = c("clean_name"="alias","ed_type"), relationship = "many-to-many") %>% 
  group_by(university_name,rsid,hs_id,ambiguous_name) %>%
  summarize(N = sum(N)) %>%
  mutate(match_type = "String col_$alias (2)")
hs_p4 = hs_p1 %>%
  filter(!rsid %in% hs_p2$rsid) %>%
  filter(!rsid %in% hs_p3$rsid) %>%
  inner_join(hs_alias, by = c("clean_name"="alias")) %>% 
  group_by(university_name,rsid,hs_id) %>%
  summarize(N = sum(N)) %>%
  mutate(ambiguous_name = 0,
         match_type = "String col_alias$alias (3)")

# Save matched column names
matched_col = rbind(col_p2,col_p3,col_p4)
saveRDS(matched_col,file.path(data_dir,"intermediate/matched_col.rds"))

# Save unmatched college names, which I attempt to match with embeddings in step 2
unmatched_col = col_p1 %>%
  filter(!rsid %in% col_p2$rsid) %>%
  filter(!rsid %in% col_p3$rsid) %>%
  filter(!rsid %in% col_p4$rsid)
saveRDS(unmatched_col,file.path(data_dir,"intermediate/unmatched_col.rds"))

# Save matched column names
matched_hs = rbind(hs_p2,hs_p3,hs_p4)
saveRDS(matched_hs,file.path(data_dir,"intermediate/matched_hs.rds"))

# Save unmatched high school names, which I attempt to match with embeddings in step 2
unmatched_hs = hs_p1 %>%
  filter(!rsid %in% hs_p2$rsid) %>%
  filter(!rsid %in% hs_p3$rsid) %>%
  filter(!rsid %in% hs_p4$rsid)
saveRDS(unmatched_hs,file.path(data_dir,"intermediate/unmatched_hs.rds"))
