## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(educationdata)
#library(blsAPI)
library(jsonlite)
library(readxl)
library(data.table)
library(tidyverse)

options(digits = 1)
options(scipen = 999)


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


## ----directory, include=FALSE-------------------------------------------------

library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")

aaufilename = "AAU.xlsx"
mobilityfilename = "mrc_table2.xlsx"

# Choose a base year (e.g., 2020) for CPI
cpi_base_year <- 2020

#load(file.path(directory,"Code/06_workspace.RData"))


## ----eval = FALSE-------------------------------------------------------------
# 
# # Import microdata file if not loaded already
# #if(!exists("microdata"))
# #{
#   # Import microdata file
#  # microdata = fread(file.path(data_dir,"results/regression_data__06.csv"),
# #                  colClasses = c("hs_zip" = "character",
# #                                 "col_zip" = "character",
# #                                 "hs_cnty" = "character",
# #                                 "col_cnty" = "character",
# #                                 "col_cbsa_code" = "character",
# #                                 "hs_cbsa_code"= "character" ))
# #}
# 


## ----pull---------------------------------------------------------------------

# Disk cache for the slow/live get_education_data() pulls below. The
# existing raw_deg_award guard (if(!exists("raw_deg_award"))) only helps
# within a single long-lived interactive session -- it's always a cache
# miss in a fresh Rscript run, which is now how this script can be invoked
# (Code/scripts/run_pipeline.R). Caching to disk instead means a re-run
# (e.g. after fixing an unrelated bug downstream, or retrying past a
# transient API error like Urban Institute's "Query page not found") skips
# straight past these pulls instead of re-fetching everything from scratch.
# Delete the relevant .rds under intermediate/api_cache/ to force a refresh.
cached_pull <- function(cache_name, fetch_expr) {
  cache_path <- file.path(data_dir, "intermediate", "api_cache", paste0(cache_name, ".rds"))
  if (file.exists(cache_path)) {
    return(readRDS(cache_path))
  }
  result <- fetch_expr
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, cache_path)
  result
}

# List of members of the American Association of Universities
aau = read_excel(file.path(data_dir,"raw/rankings_membership/AAU__06.xlsx"),
                 col_types = "text") %>%
  mutate(unitid = as.integer(unitid),
         aau = as.integer(aau)) %>%
  select(c(unitid,aau)) %>%
  as.data.table()

# Pull directory of basic information about institutions
raw_institutions = cached_pull("raw_institutions", {
  get_education_data(level = "college-university",
                                   source = "ipeds",
                                   topic = "directory",
                                   filters = list(year = 2000:2013)
                   ) %>%
  filter(inst_category == 2) %>%
  select(c(unitid,state_abbr,inst_name,inst_control,land_grant,county_fips))
})

# Pull acceptance rate data
raw_acceptance = cached_pull("raw_acceptance", {
  get_education_data(level = "college-university",
                                   source = "ipeds",
                                   topic = "admissions-enrollment",
                                   filters = list(sex = 99)
                   ) %>%
  #select(c(unitid,year,fips,number_applied,number_admitted)) %>%
  filter(!is.na(number_applied)) %>%
  mutate(acceptance_rate = case_when(is.na(number_admitted > number_applied)==TRUE ~ 1,
                                     number_applied == 0 ~ 1,
                                     TRUE ~ number_admitted/number_applied)) %>%
  # Remove cohort x institution combinations where no students enroll
  filter(number_enrolled_total != 0)
})

# Pull degrees awarded data (this is the slow one, and where the live API
# error surfaced -- cached so a retry doesn't re-pull years that already
# succeeded)
raw_deg_award = cached_pull("raw_deg_award", {
  get_education_data(level = "college-university",
                                   source = "ipeds",
                                   topic = "completions-cip-2",
                                   filters = list(award_level = 7,
                                                  year = c(2000:2021),
                                                  #fips = 26,
                                                  race = 99,
                                                  sex = 99,
                                                  majornum = 1)
                                  )
})



# Import Table 2:  Baseline Cross-Sectional Estimates by College
# https://opportunityinsights.org/wp-content/uploads/2018/04/Codebook-MRC-Table-2.pdf
raw_mobility = read_excel(file.path(data_dir,"raw/chetty_oi/mrc_table2.xlsx"))

# Import appropriations data
# https://nces.ed.gov/ipeds/survey-components/2#glossary
raw_finance = cached_pull("raw_finance", {
  get_education_data(level = "college-university",
                   source = "ipeds",
                   topic = "finance",
                   filters = list(year = c(2002:2017)),
                   add_labels = TRUE)
})

# Estimate cohort's share of undergraduate student body in given year
raw_grad_rates = cached_pull("raw_grad_rates", {
  get_education_data(level = "college-university",
                   source = "ipeds",
                   topic = "grad-rates-200pct",
                   #filters = list(year = 2017),
                   add_labels = TRUE) %>%
  filter(institution_level == "Four or more years") %>%
  mutate(still_enrolled_200pct = if_else(is.na(still_enrolled_200pct)==TRUE,0,still_enrolled_200pct),
         non_completers = cohort_rev - still_enrolled_200pct - completers_200pct,
         non_completion_rate = non_completers/cohort_rev) %>%
  # Distributes costs evenly across between enrollment and graduation
  mutate(yr_1_to_4_complete = (completion_rate_100pct/4)+((completion_rate_150pct-completion_rate_100pct)/6)+((completion_rate_200pct-completion_rate_150pct)/8),
         yr_4_to_6_complete = ((completion_rate_150pct-completion_rate_100pct)/6)+((completion_rate_200pct-completion_rate_150pct)/8),
         yr_6_to_8_complete = (completion_rate_200pct-completion_rate_150pct)/8)
})

# Pull degrees awarded data
# Only pull even years because reporting isn't mandatory in odd years
raw_instate = cached_pull("raw_instate", {
  get_education_data(level = "college-university",
                                   source = "ipeds",
                                   topic = "fall-enrollment",
                                   filters = list(year = c(2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020),
                                                  type_of_freshman = 99,
                                                  fips = c(1:56),
                                                  state_of_residence = c(1:58)),
                                 subtopic = list("residence")
                                   ) %>%
  filter(state_of_residence == fips | state_of_residence == 58)
})


## ----cpi, eval = FALSE--------------------------------------------------------
# 
# # Request CPI data for All Urban Consumers (CPI-U) for the U.S.
# payload1 <- list(
#   'seriesid' = 'CUUR0000SA0',  # CPI-U, All items
#   'startyear' = '2000',  # Specify the start year
#   'endyear' = '2009',
#   'annualaverage' = 'true' # Specify the end year
# )
# payload2 <- list(
#   'seriesid' = 'CUUR0000SA0',  # CPI-U, All items
#   'startyear' = '2010',  # Specify the start year
#   'endyear' = '2019',
#   'annualaverage' = 'true' # Specify the end year
# )
# payload3 <- list(
#   'seriesid' = 'CUUR0000SA0',  # CPI-U, All items
#   'startyear' = '2020',  # Specify the start year
#   'endyear' = '2024',
#   'annualaverage' = 'true' # Specify the end year
# )
# 
# response1 <- blsAPI(payload1, api_version = 2)
# deflator1 <- fromJSON(response1)$Results$series$data
# response2 <- blsAPI(payload2, api_version = 2)
# deflator2 <- fromJSON(response2)$Results$series$data
# response3 <- blsAPI(payload3, api_version = 2)
# deflator3 <- fromJSON(response3)$Results$series$data
# # Combine the deflator data from all responses
# deflator_combined <- bind_rows(deflator1, deflator2, deflator3)
# 
# # Convert to a data frame and filter for annual averages
# cpi_annual <- deflator_combined %>%
#   as.data.frame() %>%
#   filter(periodName == "Annual") %>%  # Keep only annual data
#   select(year, value) %>%  # Select relevant columns
#   arrange(desc(year))  %>%
#   mutate(across(everything(), as.numeric))
#   # Order by year
# 
# # Extract the CPI value for the base year
# base_cpi <- cpi_annual %>% filter(year == cpi_base_year)
# 
# # Create the deflator by dividing each year's CPI by the base year CPI
# cpi_annual <- cpi_annual %>%
#   mutate(base_cpi = case_when(year == cpi_base_year ~ value,
#                               TRUE ~ base_cpi$value)) %>%
#   mutate(deflator = base_cpi / value) %>%
#   select(c(year,deflator))


## ----groupings----------------------------------------------------------------

# Calculate average acceptance rate as a mean of acceptance rates between 2001-2021
# Weighted by number of enrolled students in each class
acceptance_grouped = raw_acceptance %>%
  group_by(unitid) %>%
  summarize(avg_acceptance = 100*weighted.mean(acceptance_rate, na.rm = TRUE, w = number_enrolled_total),
            total_enrolled = sum(number_enrolled_total))

institutional_characteristics = raw_institutions %>%
  mutate(GeoFIPS = as.character(str_pad(county_fips,5,side="left","0"))) %>%
  left_join(unified_cbsa, by = "GeoFIPS") %>%
  select(-c(GeoFIPS,county_fips)) %>%
  left_join(acceptance_grouped, by = "unitid") %>%
  mutate(avg_acceptance = if_else(is.na(avg_acceptance),99,avg_acceptance)) %>%
  left_join(aau, by = "unitid") %>%
  mutate(aau = if_else(is.na(aau),0,aau),
         inst_group = case_when(inst_control == 1 & (land_grant == 1 | aau == 1) ~ "PF",
                                inst_control == 1 & land_grant == 2 & aau == 0 ~ "RPU",
                                inst_control == 2 ~ "Private, Non-Profit",
                                inst_control == 3 ~ "Private, For-Profit"))


# Filter for the RPU group and calculate cumulative enrollment
#rpu_group <- institutional_characteristics %>%
#  filter(inst_group == "RPU") %>%
#  arrange(avg_acceptance,total_enrolled) %>%
#  mutate(cum_enrolled = cumsum(total_enrolled))

# Find the total number of degrees awarded for the RPU group
#total_degrees_rpu <- sum(rpu_group$total_enrolled, na.rm = TRUE)

# Create two bins based on cumulative enrollment for the RPU group
#rpu_group <- rpu_group %>%
#  mutate(bin = cut(cum_enrolled, 
#                   breaks = c(0, total_degrees_rpu / 2, total_degrees_rpu),
#                   labels = c("RPU, More Selective", "RPU, Less Selective"),
#                   include.lowest = TRUE))

# Combine the binned RPU group back with the original data
#institutional_characteristics <- institutional_characteristics %>%
#  left_join(rpu_group %>% select(unitid, bin), by = "unitid") %>%
#  mutate(inst_group = case_when(inst_group == "RPU" & !is.na(bin) ~ bin,
#                                inst_group == "RPU" & is.na(bin) ~ "RPU, Less Selective",
#                                TRUE ~ inst_group
#                                )) %>%
#  select(-bin)


## ----groupings_dup2-----------------------------------------------------------
deg_award_total = raw_deg_award %>%
  filter(cipcode == 990000 | cipcode == 99) %>%
  #rename(total = awards) %>%
  group_by(unitid) %>%
  summarize(total_deg_awarded = sum(awards))
  #select(c(unitid,total)) 

# List of major names here:
# https://educationdata.urban.org/documentation/colleges.html#ipeds-awards-by-2-digit-cip-code 
deg_award_detail = raw_deg_award %>%
  filter(cipcode != 99) %>%
  filter(cipcode != 990000) %>%
  mutate(category = case_when(
    cipcode %in% c(950000, -1, -2, -3) ~ "unavailable",
    cipcode %in% c(10000, 20000, 30000) ~ "agriculture",
    cipcode %in% c(40000, 100000, 110000, 140000, 150000) ~ "engineering",
    cipcode %in% c(50000, 230000, 240000, 250000, 380000, 500000, 540000) ~ "humanities",
    cipcode %in% c(130000, 160000, 190000, 200000) ~ "education",
    cipcode %in% c(260000, 270000, 400000, 410000) ~ "physical_sciences",
    cipcode %in% c(90000, 420000, 440000, 450000) ~ "social_sciences",
    cipcode %in% c(120000, 290000, 310000, 430000) ~ "services",
    cipcode %in% c(460000, 470000, 480000, 490000) ~ "other", # trades
    cipcode %in% c(510000,512001) ~ "healthcare",
    cipcode %in% c(80000,520000) ~ "business",
    cipcode %in% c(220000, 220101) ~ "other",
    cipcode %in% c(390000, 390602, 390603, 390605) ~ "other", # religion
    cipcode == 300000 ~ "other", #  interdisciplinary
    TRUE ~ "other"
  )) %>%
  group_by(unitid,category) %>%
  summarize(field = sum(awards)) %>%
  pivot_wider(names_from = category, values_from = field, values_fill = list(field=0)) %>%
  left_join(deg_award_total, by = "unitid") 

# Merge in IPEDS data on degrees awarded
institutional_characteristics = deg_award_detail %>%
  #group_by(unitid) %>%
  #summarize(deg_awarded = sum(total)) %>%
  right_join(institutional_characteristics, by = "unitid")
  


## ----institution, eval = FALSE------------------------------------------------
# 
# deg_award = raw_deg_award %>%
#   #filter(year <= 2018 & year >= 2010) %>%
#   filter(cipcode == 99) %>%
#   group_by(unitid) %>%
#   summarize(total_deg_awarded = sum(awards)) %>%
#   right_join(institutional_characteristics %>% select(c(unitid,state_abbr,inst_name,inst_group)), by = "unitid")
# 
# # Count the occurrences of each combination of col_unitid and recruited
# recruitment <- microdata[, .N, by = .(col_unitid, recruited)] %>%
#   as_tibble() %>%
#   pivot_wider(names_from = "recruited", values_from = "N", values_fill = list(n=0)) %>%
#   mutate(total = Recruited + Not_Recruited + Retained + Not_Retained) %>%
#   mutate(shr_recruited = Recruited/total,
#          shr_not_recruited = Not_Recruited/total,
#          shr_retained = Retained/total,
#          shr_not_retained = Not_Retained/total,
#          shr_instate_orig = Retained/(Retained+Recruited)
#          ) %>%
#   left_join(deg_award,
#             by = c("col_unitid"="unitid")) %>%
#   filter(!is.na(total_deg_awarded))
# 
# recruitment_grouped = recruitment %>%
#   group_by(inst_group) %>%
#   summarise(total_deg_awarded_sum = sum(total_deg_awarded, na.rm = TRUE),
#             across(starts_with("shr_"),
#                    ~ weighted.mean(.x, total_deg_awarded, na.rm = TRUE),
#                    .names = "{col}"))
# 
# 


## ----chetty-------------------------------------------------------------------

unitid_super_opeid_crosswalk = institution_crosswalk %>% 
  select(unitid,super_opeid) %>%
  mutate(unitid = as.integer(unitid)) %>%
  filter(super_opeid > 0) %>% 
  distinct()

# Count degrees awarded by super_opeid
# OI could not aggregate every superope_id into a single unitid
# We will weight super_opeid characteristics propprtionate to the degrees awarded
# by each unitid in the super_opeid
deg_award_super_opeid = deg_award_total %>% 
  left_join(unitid_super_opeid_crosswalk, by = "unitid") %>%
  group_by(super_opeid) %>%
  summarize(total_deg_awarded_opeid = sum(total_deg_awarded))

# Calculate weights for each unitid in super_opeid
unit_super_opeid_weights = unitid_super_opeid_crosswalk %>%
  left_join(deg_award_total, by = "unitid") %>%
  left_join(deg_award_super_opeid, by = "super_opeid") %>%
  filter(!is.na(total_deg_awarded) & !is.na(total_deg_awarded_opeid)) %>%
  mutate(unitid_shr = total_deg_awarded/total_deg_awarded_opeid) %>%
  select(unitid,super_opeid,unitid_shr)

# Extract necessary columns and calculate institution-level measures of income distribution
mobility = raw_mobility %>%
  # Merge on a cleaned version of the crosswalk
  left_join(unit_super_opeid_weights, by = "super_opeid") %>%
  # Remove missing data
  filter(!is.na(unitid) & !is.na(count) & !is.na(unitid_shr)) %>%
  # Reweight mobility estimates to the unitid level 
  mutate(mobility_n = count * unitid_shr,
         par_bottom60 = (par_q1 + par_q2 + par_q3) * mobility_n,
         par_top20 = par_q4 * mobility_n,
         par_top01 = par_top1pc * mobility_n,
         k_bottom60 = (k_q1 + k_q2 + k_q3) * mobility_n,
         k_top20 = k_q4 * mobility_n,
         k_top01 = k_top1pc * mobility_n,
         ) %>%
  rename(mobility_rate = mr_kq5_pq1) %>%
  select(c("super_opeid","unitid","mobility_n","par_mean","par_bottom60","par_top20","par_top01",
           "mobility_rate","k_mean","k_bottom60","k_top20","k_top01"))

institutional_characteristics <- institutional_characteristics %>%
  left_join(mobility, by = "unitid")

rm(unitid_super_opeid_crosswalk,deg_award_super_opeid)


## ----finance, eval = FALSE----------------------------------------------------
# 
# finance_recode = raw_finance %>%
#   left_join(cpi_annual, by = "year") %>%
#   # Adjust prices for changes in purchasing power
#   # I excluded grants based on IPEPS documentation
#   # While grants might be either operating or non-operating, generally they entail
#   # exchange of some service. By contrast, appropriations fund day-to-day operations
#   # of an institution
#   # https://nces.ed.gov/ipeds/survey-components/2#glossary
#   mutate(rev_appropriations_state = if_else(is.na(rev_appropriations_state)==TRUE,0,rev_appropriations_state * deflator),
#          rev_tuition_fees =  if_else(is.na(rev_tuition_fees_net)==TRUE,0,rev_tuition_fees_net * deflator),
#          rev_total_current = if_else(is.na(rev_total_current)==TRUE,0,rev_total_current * deflator)) %>%
#   select(c(year,unitid,rev_appropriations_state,rev_tuition_fees,rev_total_current)) %>%
#   # Cot analysis requires a yearly panel
#   rename(year_in_school = year)
# 
# # I aggregate yearly data for the summary statistics
# finance = finance_recode %>%
#   group_by(unitid) %>%
#   summarize(rev_appropriations_state = sum(rev_appropriations_state),
#             rev_tuition_fees = sum(rev_tuition_fees),
#             rev_total_current = sum(rev_total_current)) %>%
#   mutate(state_approp_shr = rev_appropriations_state/rev_total_current,
#          tuition_shr = rev_tuition_fees/rev_total_current,
#          state_approp_tuition_shr = state_approp_shr + tuition_shr)
# 
# institutional_characteristics <- institutional_characteristics %>%
#   left_join(finance, by = "unitid")
# 


## ----gradrate-----------------------------------------------------------------

grad_rates = raw_grad_rates %>% 
  # Instead of taking weighted average of rates, I take cohort size and completions
  select(c("unitid",starts_with("completers"),starts_with("cohort_adj"),"unitid")) %>%
  group_by(unitid) %>%
  summarize(across(everything(), sum))

institutional_characteristics <- institutional_characteristics %>%
  left_join(grad_rates, by = "unitid")
  


## -----------------------------------------------------------------------------

# Create a crosswalk between state abbreviations and Census regions
state_region_crosswalk <- data.frame(
  state = state.abb,  # State abbreviations
  region = state.region  # Census regions
) %>%
  rbind(c("DC","South")) %>% as.data.table()

institutional_characteristics <- institutional_characteristics %>%
  left_join(state_region_crosswalk, by = c("state_abbr"="state"))


## ----gradrate_dup2------------------------------------------------------------

write.csv(institutional_characteristics, file.path(data_dir,"intermediate/institutional_characteristics.csv"))
  

