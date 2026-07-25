## ----directory, include=FALSE-------------------------------------------------
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
edufilename1 = "raw_educ_STATES_AK-MD.csv"
edufilename1edit = "STATES_AK-MD.csv"
edufilename2 = "raw_educ_STATES_MI-WY.csv"
edutfilename2edit = "STATES_MI-WY.csv"
misfitscorrection = "misfits_correction.csv"


## ----setup, include=FALSE-----------------------------------------------------
#knitr::opts_chunk$set(echo = TRUE)
library(readxl)
library(stringi)
library(tidyverse)
library(table.express)
library(data.table)


## ----negate, include=FALSE----------------------------------------------------
`%nin%` = Negate(`%in%`)


## ----ed match, echo=FALSE-----------------------------------------------------
both_final <- readRDS(file.path(data_dir,"intermediate/both_final.rds"))
schools <- readRDS(file.path(data_dir,"intermediate/schools.rds"))
colleges <- readRDS(file.path(data_dir,"intermediate/colleges.rds"))
unified_cbsa <- fread(file.path(data_dir,"raw/census_geo/unified_cbsa.csv"),
                      colClasses = c(GeoFIPS = "character"))

hs_match = both_final %>%
  mutate(hs_id = as.character(hs_id)) %>%
  left_join(schools %>% mutate(hs_id = as.character(hs_id)) %>%
              select(hs_id,hs_fips,state_abbr), by = "hs_id") %>%
  left_join(unified_cbsa %>% select(GeoFIPS,cbsa_code), by = c("hs_fips" = "GeoFIPS")) %>%
  transmute(user_id,
            hs_start = NA_real_,
            # 04_li_ed_pos.Rmd's birth-year step expects a plain year number
            # (the old code extracted this from a "YYYY-MM-DD" string via
            # str_sub); hs_end/ba_end here are Date/IDate objects, so extract
            # the year explicitly rather than letting as.numeric() downstream
            # silently return a day-count instead.
            hs_end = as.numeric(format(as.Date(hs_end), "%Y")),
            hs_name = hs_string,
            hs_unitid = hs_id,
            hs_state = state_abbr,
            hs_cnty = hs_fips,
            hs_cbsa = cbsa_code)

col_match = both_final %>%
  # ba_unitid/ba_opeid are cast to character in 04_col_hs_construct.R;
  # colleges.rds's unitid/opeid come straight from the raw IPEDS pull and
  # are numeric -- cast explicitly rather than relying on left_join's
  # implicit coercion, since a silent type mismatch here would just drop
  # every row with no error.
  left_join(colleges %>% mutate(unitid = as.character(unitid), opeid = as.character(opeid)) %>%
              select(unitid,opeid,col_fips,system_opeid),
            by = c("ba_unitid" = "unitid", "ba_opeid" = "opeid")) %>%
  transmute(user_id,
            col_start = NA_real_,
            col_end = as.numeric(format(as.Date(ba_end), "%Y")),
            col_name = ba_school,
            col_major = ba_degree,
            col_unitid = ba_unitid,
            col_opeid = ba_opeid,
            col_super_opeid = system_opeid)

# 04_col_hs_construct.R only keeps users with both a resolved HS and BA
# college match, so every row already qualifies -- no separate intersect()
# needed.
both = both_final %>% distinct(user_id)
col_users = col_match %>% distinct(user_id)
hs_users = hs_match %>% distinct(user_id)

# 04_li_ed_pos.Rmd joins into these with data.table's `x[i, on=]` syntax
# (both[pos,...], col_users[pos,...], col_match[hs_match,...]);
# both_final.rds's tibble-ness (04_col_hs_construct.R's as_tibble()) carries
# through left_join()/transmute()/distinct(), so without this they're plain
# tibbles and `[.tbl_df` gets dispatched instead of `[.data.table` -- the
# join then errors trying to use the join table as a bare row index.
setDT(both)
setDT(col_users)
setDT(col_match)
setDT(hs_match)

# Users with more than one BA-level record already carry that in
# ba_transfer_* fields on the same row (04_col_hs_construct.R), rather than
# needing a separate multi-row count.
transfer = both_final %>%
  filter(!is.na(ba_transfer_school)) %>%
  distinct(user_id) %>%
  pull(user_id)

rm(both_final)
gc()


## ----obs----------------------------------------------------------------------

col_users_length = length(unique(col_users$user_id))
both_hs_col_length = length(unique(both$user_id))


