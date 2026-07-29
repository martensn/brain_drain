## ----directory, include=FALSE-------------------------------------------------

library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
msastringfilename = "msa_string_cleaning.csv"
zipcodefilename = "zipcodes.csv"

source(file.path(here::here(), "Code", "college_lookup.R"))

# Variables user can control
# Time range for work history
work_history_start = 1975
# [CHANGED 2026-07-26 -- Phase 2 arrow rewiring] extended 2023 -> 2025 now that
# the position dataset covers position-years through 2025 (see "pos import"
# chunk below); previously matched the old cleaned_pos_educ_*.csv vintage's
# July-2023 pull cutoff.
work_history_end = 2025
# Number of years after college graduation to include in data
max_post_grad = 50
min_post_grad = 0



## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(lubridate)
#library(zipcodeR)
library(geodist)
library(stringi)
library(data.table)
# user_id is integer64 throughout this stage (both_final.rds, the position
# parquet dataset); without bit64 loaded, printing/comparing integer64 values
# silently produces garbage instead of an error -- see the project's Phase 2
# investigation notes for a concrete false-alarm this caused.
library(bit64)


## ----pos import, include=FALSE------------------------------------------------
library(arrow)

relevant_ids = arrow_table(user_id = unique(both$user_id))

pos = open_dataset(file.path(data_dir, "intermediate/pos_parquet_pilot")) %>%
  filter(country == "United States") %>%
  select(user_id, country, state, msa, startdate, enddate, seniority, onet_code) %>%
  semi_join(relevant_ids, by = "user_id") %>%
  collect() %>%
  setDT()

rm(relevant_ids)

# [CHANGED 2026-07-26 -- Phase 2 arrow rewiring] the ingestion schema declares
# startdate/enddate as plain strings (arrow has no reliable auto-date-detection
# the way fread() does, which is what the old cleaned_pos_educ_*.csv pipeline
# silently relied on for this same conversion). Missing dates arrive as "" (an
# empty string), not a true NA -- as.IDate("") correctly resolves that to NA,
# which the "return binary" chunk below depends on for its still-employed
# fallback logic. Without this conversion, which.min(startdate) inside the
# work_earliest by-group operation below silently coerces the character column
# to numeric, produces NA for every row, and drops every group -- discovered
# via a full pipeline run that failed several stages later with an opaque
# "columns not found" error once the resulting empty frame reached a dcast().
pos[, startdate := as.IDate(startdate)]
pos[, enddate := as.IDate(enddate)]



## ----pos trim, include=FALSE--------------------------------------------------

# Match position file to users with high school and college degrees
# (country filter now applied during the arrow scan in the "pos import" chunk above)
position = both[pos, on = "user_id", nomatch = NULL] %>%
  table.express::mutate(user_id = as.character(user_id))

full_position = col_users[pos, on = "user_id", nomatch = NULL, allow.cartesian = TRUE] %>%
  table.express::mutate(user_id = as.character(user_id))


# Remove redundant intermediate objects to save memory
rm(pos)
gc()

# Used to make figures; counts ONET codes
# [CHANGED 2026-07-26 -- Phase 2 arrow rewiring] onet_title isn't in the new
# position schema. It was already dropped before this summary's final grouping
# (by socp), so grouping by onet_code alone here produces identical shr_li values.
pos_summary <- position[, .(n = .N), by = .(onet_code)] %>%
  as.data.frame() %>%
  mutate(shr_li = n/sum(n)) %>%
  mutate(socp = sub("\\..*$", "",onet_code)) %>%
  select(-c("onet_code","n")) %>%
  group_by(socp) %>%
  summarize(shr_li = sum(shr_li))

write.csv(pos_summary,file.path(data_dir,"intermediate/soc_li_distribution.csv"), row.names = FALSE)



## ----filter, include=FALSE----------------------------------------------------
merge_col_hs = col_match[hs_match, on = "user_id", nomatch = NULL] %>%
  table.express::filter(is.na(col_name)==FALSE) %>%
  table.express::mutate(col_length = col_end - col_start)

rm(col_match,hs_match,hs_users)
gc()


## ----merge hs col, include=FALSE----------------------------------------------
merge_col_hs = merge_col_hs %>%
  table.express::mutate(transfer = as.integer(user_id %in% transfer)) %>%
  table.express::filter(is.na(col_name)==FALSE)


## ----earliest job, include=FALSE----------------------------------------------

# Transform position file into wide data
# To start, we only need to know the earliest latest start dates
# For users with no graduation date we can use it to estimate graduation year
#work_latest = position[position[, .I[which.max(enddate)], by=user_id]$V1]
#work_latest = work_latest[,c("user_id","enddate")] 
work_earliest = position[position[, .I[which.min(startdate)], by=user_id]$V1]
# [CHANGED 2026-07-26 -- Phase 2 arrow rewiring] as.integer() truncates to
# 32-bit (max ~2.15B); the new position dataset's user_ids run up to ~2.35B
# (exactly the previously-unreachable high-ID users this phase was meant to
# recover), so this silently NA'd every such user and then failed to join
# against merge_col_hs's integer64 user_id below, zeroing out the whole
# downstream chain. bit64::as.integer64() preserves the full range and
# matches merge_col_hs's type.
work_earliest = work_earliest[,c("user_id","startdate")] %>%
  table.express::mutate(user_id = as.integer64(user_id))

merge_col_hs = merge_col_hs[work_earliest, on = "user_id"] %>%
  setnames(c("startdate"),
           c("work_start"))

# [CHANGED 2026-07-28 -- Phase A.3b fix] Was a single-year-2021 IPEDS API
# pull (get_education_data(..., filters=list(year=2021))), which silently
# dropped 247 real, large, currently-active institutions (Penn State Main
# Campus, University of Phoenix-Arizona, Kennesaw State, etc.) not present
# in that one snapshot -- likely IPEDS unitid consolidation/splits across
# years, costing ~109K rows downstream. colleges.rds (built in
# 00_alias_generation.R) already covers these institutions via a
# comprehensive 2000-2024 pull; resolve_college() (Code/college_lookup.R)
# picks the era-correct row for the ~10% of unitids that were renamed/
# re-OPEID'd over time, using each user's own col_end as the reference year.
# See yes-let-s-resolve-this-misty-kahan.md's 2026-07-28 finding.
colleges_lookup <- readRDS(file.path(data_dir, "intermediate/colleges.rds"))
setDT(colleges_lookup)

merge_col_hs <- resolve_college(merge_col_hs, "col_unitid", "col_end", colleges_lookup,
                                 select_cols = c("zip")) %>%
  table.express::mutate(col_zip = str_extract(zip, "^.{5}"))
rm(colleges_lookup)


#work_extent = work_earliest[work_latest, on = "user_id"]

#rm(work_latest,work_earliest)


## ----birth year, include=FALSE------------------------------------------------
# Convert graduation and start years from character to numeric
merge_col_hs$col_start = as.numeric(merge_col_hs$col_start)
merge_col_hs$col_end = as.numeric(merge_col_hs$col_end)
merge_col_hs$hs_end = as.numeric(merge_col_hs$hs_end)

# Calculating birth date based on high school graduation if available, 
# then college start date, then college graduation if the first two aren't available
# finally work start date if the other three are unavailable
merge_col_hs = merge_col_hs %>%
  table.express::mutate(birth1 = ifelse(is.na(hs_end)==FALSE, hs_end - 18,0),
                        birth2 = ifelse(is.na(col_start)==FALSE, col_start - 18,0),
                        birth3 = ifelse(is.na(col_end)==FALSE, col_end - 22,0),
                        # Use work history if no dates provided 
                        birth4 = ifelse(is.na(work_start)==FALSE,
                                        year(work_start)-22,0)) %>%
  table.express::mutate(birth = ifelse(birth1 > 0, birth1,
                                       ifelse(birth2 > 0, birth2,
                                              ifelse(birth3 > 0, birth3,
                                                     ifelse(birth4 > 0, birth4,0))))) %>%
  # Use estimated birth years to fill in missing graduation or start dates
  table.express::mutate(col_end = ifelse(is.na(col_end)==FALSE, col_end, birth + 22))

#merge_col_hs[,c("birth1","birth2","birth3","birth4") := NULL]


## ----return binary, include=FALSE---------------------------------------------

# [CHANGED 2026-07-26 -- Phase 2 arrow rewiring] see the work_earliest note
# above -- same 32-bit truncation bug, same fix.
position[, user_id := as.integer64(user_id)]

# [CHANGED 2026-07-26 -- Phase 2 arrow rewiring] the raw position dataset's
# state field is a full name ("California"), unlike the old cleaned_pos_educ_*.csv
# vintage which arrived pre-abbreviated -- cbsa_li_crosswalk's cbsa_state is
# always a 2-letter abbreviation ("CA"), so without this conversion the merge
# below on c("cbsa_core","cbsa_state") never matches on the state half of the
# key, silently NA-ing out cbsa_code for every single row (discovered when the
# final melt(na.rm=TRUE) came back with zero rows despite everything upstream
# looking healthy). Only the 50 states + DC appear in this field (confirmed by
# inspecting the ingested data directly); unmapped/empty values pass through
# unchanged and simply won't match the crosswalk, same as before this fix.
state_name_to_abb = setNames(c(state.abb, "DC"), c(state.name, "Washington, D.C."))

# Merge education and position files
birth_position = merge_col_hs[position, on = "user_id", allow.cartesian = TRUE] %>%
  setnames(c("state"),c("cbsa_state")) %>%
  table.express::mutate(cbsa_state = ifelse(cbsa_state %in% names(state_name_to_abb),
                                             state_name_to_abb[cbsa_state], cbsa_state)) %>%
  table.express::mutate(startdate = as.character(startdate),
                        enddate = as.character(enddate)) %>%
  # [CHANGED 2026-07-26 -- Phase 2 arrow rewiring] still-employed spells arrive
  # from the raw data with enddate = "" (empty string), not a true NA -- an
  # early check for is.na() at the raw-string level found zero NAs and wrongly
  # suggested this branch was dead code (see the "pos import" chunk's note);
  # the as.IDate() conversion added there resolves "" to a true NA, which this
  # check now correctly catches (~30M of ~93M rows). Fallback date updated from
  # the old pipeline's "when Ian finished pulling data in July 2023" to this
  # vintage's actual snapshot date (2026-03-01, uniform across every year
  # partition of the new position dataset).
  table.express::mutate(enddate = ifelse(is.na(enddate)==TRUE,"2026-03-01",enddate)) %>%
  table.express::mutate(cbsa_core = str_replace(msa,"-.*","")) %>%
  table.express::mutate(cbsa_core = str_replace_all(cbsa_core," [A-Z]{2} MSA$| MSA$","")) %>%
  table.express::mutate(cbsa_core = str_replace_all(cbsa_core," [A-Z]{2}$", ""))

msa = table(birth_position$msa)
cbsa_core = table(birth_position$cbsa_core)

  #table.express::mutate(cbsa_city = str_extract(cbsa_clean,'(^.{1,}-(?=([A-Z][a-z].*)))|(^.{1,} (?=([A-Z][A-Z].*)))')) %>%
  #table.express::mutate(cbsa_city = str_replace_all(cbsa_city, '-'," "))

# Import list of string corrections to ease merge with CBSA file
string_cleaning = fread(file.path(data_dir,"intermediate/msa_string_cleaning.csv"))
patterns = string_cleaning$Lookup
replacements = string_cleaning$Rename

# Make replacements on birth_position
birth_position = birth_position[, cbsa_core := stri_replace_all_fixed(cbsa_core, patterns, replacements, vectorize_all=FALSE)]

#unique_in_both <- unique(birth_position$user_id[birth_position$user_id %in% both$user_id])
#in_both_count <- length(unique_in_both)
#na_in_both_count <- sum(birth_position$user_id %i% both & !is.na(birth_position$col_name))

li_core = unique(birth_position$cbsa_core)

rm(string_cleaning,patterns,replacements)
gc()

#birth_position_sample = birth_position[sample(.N,10000)]
birth_position = merge(birth_position,cbsa_li_crosswalk, by = c("cbsa_core","cbsa_state"), all.x = TRUE)  %>%
  table.express::filter(!is.na(col_name) & msa != "empty")
gc()

no_work_location = length(unique(birth_position$user_id))

non_chronological_work = birth_position %>% 
  # Convert start and end dates to data.table's date format
  mutate(startdate = as.IDate(startdate)) %>%
  # Remove observations with no startdate
  filter(is.na(startdate)==FALSE)

no_work_date = non_chronological_work %>%
  # Filter out jobs where end date precedes start_date
  mutate(weird = ifelse((startdate <= enddate)==TRUE,0,1)) %>%
  #mutate(num_cbsa = as.numeric(cbsa_code)) %>%
  filter(weird == 0)

non_chronological_work = length(unique(non_chronological_work$user_id))
no_work_date = length(unique(no_work_date$user_id))
gc()

birth_position = birth_position %>% 
  #filter(col_name == "Jackson College") %>%
  # Convert start and end dates to data.table's date format
  table.express::mutate(startdate = as.IDate(startdate),
         enddate = as.IDate(enddate)) %>%
  # Remove observations with no startdate
  table.express::filter(is.na(startdate)==FALSE) %>%
  # Filter out jobs where end date precedes start_date
  #table.express::mutate(weird = ifelse(,0,1)) %>%
  #mutate(num_cbsa = as.numeric(cbsa_code)) %>%
  table.express::filter((startdate <= enddate)==TRUE)

startdate_before_enddate = length(unique(birth_position$user_id))
# Since the next steps are among the most memory-intensive in the entire codebase
# Remove some particularly large objects that aren't necessary
rm(still_hs,work_earliest,full_position,ed_li_sample,position)
gc()

# Create list of non-overlapping intervals representing year between the start
# and end defined by the user
start = ymd(rep(work_history_start:work_history_end),truncated=2L)
end = start + years(1) - days(1)
interval = year(start)
work_hist = data.table(interval = interval,
                       start = as.IDate(start),
                       end = as.IDate(end))

# Standardizing time intervals on LI profiles as years since college graduation
# Reference list (just intervals from Jan 1 to Dec 31 of every year)
setkey(work_hist, start, end)
# Detecting overlaps
std_pos = foverlaps(birth_position,
                work_hist,
                by.x=c("startdate","enddate"),
                by.y=c("start","end"),
                type = "any") %>%
  # Converting overlaps into years since graduation
  table.express::mutate(yrs_graduated = interval - col_end) %>%
  # Removing jobs prior to college graduation
  # Approximately 15 percent of users only list jobs prior to their college graduation
  table.express::filter(min_post_grad <= yrs_graduated  & yrs_graduated <= max_post_grad)

#rm(work_history_start,work_history_end,interval,work_hist,start,end)
gc()

# If multiple jobs listed, selects the most common CBSA for employment
std_pos_geo = dcast(std_pos, user_id ~ yrs_graduated, 
             value.var = c("cbsa_code","cbsa_state"),
             fun.aggregate = function(x, na.rm = TRUE)
               {
               if(na.rm)
                 {
                 x = x[!is.na(x)]
                 }
               ux <- unique(x)
               return(ux[which.max(tabulate(match(x,ux)))])
               })
std_pos_soc = dcast(std_pos, user_id ~ yrs_graduated, 
             value.var = c("onet_code"),
             fun.aggregate = function(x, na.rm = TRUE)
               {
               if(na.rm)
                 {
                 x = x[!is.na(x)]
                 }
               ux <- unique(x)
               return(ux[which.max(tabulate(match(x,ux)))])
               })


almost_completed = merge_col_hs[std_pos_geo, on = "user_id",all=TRUE]
almost_completed = almost_completed[std_pos_soc, on = "user_id", all = TRUE]

# Calculate number of merges for observation accounting
no_work_hist = length(unique(almost_completed$user_id))

# [CHANGED 2026-07-28 -- Phase A.3b fix] see the "earliest job" chunk's note
# above (inst_zip) -- same fix, same reasoning: colleges.rds instead of a
# single-year-2021 IPEDS pull, resolved to the era-correct row via each
# user's own col_end.
colleges_lookup <- readRDS(file.path(data_dir, "intermediate/colleges.rds"))
setDT(colleges_lookup)

completed = almost_completed %>%
  # Filter out remaining incomplete observations (3% of mass)
  table.express::filter(is.na(col_name)==FALSE)

completed <- resolve_college(completed, "col_unitid", "col_end", colleges_lookup,
                              select_cols = c("state_abbr"))
completed[, col_state := state_abbr]
completed[, state_abbr := NULL]
rm(colleges_lookup)

# Filter out users with multiple entries
completed <- completed[, .SD[which.min(col_end)], by = "user_id"]

college_dne = length(unique(completed$user_id))

# Should not be necessary if properly filtered
#extra_cols = c(paste("cbsa_code",c(-577:-1,51:123),sep = "_"),paste("cbsa_state",c(-577:-1,51:123),sep = "_"),c(-577:-1,51:123),"NA")
#new_names = c("user_id","col_unitid",names(almost_completed)[3:24],paste0("cbsa_code_",0:50),paste0("cbsa_state_",0:50),paste0("soc_code_",0:50),"col_state")

setnames(completed, old = as.character(c(0:50)), new = paste0("soc_code_",0:50))

rm(std_pos_geo,std_pos,birth_position)
gc()


## ----numerator, include=FALSE-------------------------------------------------

cols = c("user_id","col_end","birth","hs_state","col_state",paste0("cbsa_code_",min_post_grad:max_post_grad))
# [CHANGED 2026-07-27 -- Phase 2 arrow rewiring] col_end < 2023 predates
# work_history_end's extension to 2025 (see the "directory" chunk) -- left
# unchanged, this cutoff silently discarded ~880K of the ~895K rows the new,
# wider position-year coverage newly made visible, right back out again.
# Extended to 2026 to match, per user decision.
completed_melt = completed[, ..cols] %>%
    table.express::filter(col_end > 1981 & col_end < 2026 & birth > 1929 & birth > 1929 & birth < 2003) %>%
  table.express::filter(!hs_state %in% c("GU","PR","AS","VI")) %>%
  table.express::filter(!col_state %in% c("GU","PR","AS","VI")) %>%
  table.express::mutate_sd(c("col_end","birth","hs_state","col_state"),NULL) %>%
  melt(id=c("user_id"), na.rm=TRUE)
  
numerator = completed_melt[, .(count = .N), by = c("value","variable")]

write.csv(numerator,file.path(data_dir,"intermediate/numerator.csv"), row.names = FALSE)
# [CHANGED 2026-07-29 -- Phase A.3 diff] row.names=FALSE added -- this write
# was missing it (the only other omission besides unified_cbsa.csv, already
# fixed in 00_crosswalks.Rmd), producing an unnamed leading index column that
# fread reads back as "V1". 05_merge.Rmd reads this file as raw_microdata and
# merges it against unified_cbsa twice; with both sides carrying a stray "V1"
# before this fix, the merges suffixed them V1.x/V1.y, which is exactly how
# they ended up polluting the final microdata.csv.
write.csv(completed,file.path(data_dir,"raw/revelio/raw_microdata__05.csv"), row.names = FALSE)


rm(position,all_col_unrated,almost_completed,barrons,barrons_chetty,big_chungus,both,
   cbsa_fips,cbsa_na,cbsa_code_name,chetty,chetty_raw,col_users,completed_neither,
   ed_li,full_position,merge_col_hs,not_hs,position,postsec,private,private_hs,
   public,public_hs,raw_county,tiered_barrons,tiered_chetty,transfer,unified_cbsa_name,
   unoriginal_hs,work_earliest)
gc()

