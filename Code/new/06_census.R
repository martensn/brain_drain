library(tidycensus)
library(sf)
library(tidyverse)
library(data.table)


set.seed(49)
#directory = "/nfs/turbo/lsa-areynoso"
directory = "/Volumes/lsa-areynoso"
data_dir = file.path(directory,"Data")



# Query API for degrees awared data
if(exists(file.path(data_dir,"ipeds_deg_awarded.rds")))
{
  raw_deg_award_1 = get_education_data(level = "college-university",
                                       source = "ipeds",
                                       topic = "completions-cip-6",
                                       filters = list(award_level = 7,
                                                      year = c(1983:1999),
                                                      race = 99,
                                                      sex = 99)
  )
  raw_deg_award_2 = get_education_data(level = "college-university",
                                       source = "ipeds",
                                       topic = "completions-cip-6",
                                       filters = list(award_level = 7,
                                                      year = c(2000:2022),
                                                      #fips = 26,
                                                      race = 99,
                                                      sex = 99,
                                                      majornum = 1)
  )
  
  raw_deg_award = rbind(raw_deg_award_1,raw_deg_award_2) %>%
    filter(race == 99, sex == 99, cipcode_6digit == 99)
  saveRDS(raw_deg_award,file.path(data_dir,"ipeds_deg_awarded.rds"))
}

# Import residential file
res = fread(file.path(data_dir,"colleges_ipeds_fall-res.csv"))


# Pull 2024 1-year ACS microdata 
col_grad_micro = get_pums(variables = c("ESR","AGEP","SCHL","OCCP","RAC2P","SEX","HISP"),
                          state = "all",
                          survey = "acs1",
                          year = 2023,
                          show_call = TRUE)

# Filter ACS data to only include college graduates who work
col_grad_filt = col_grad_micro %>%
  mutate(esr = as.integer(ESR)) %>%
  filter(esr %in% c(1,2,3)) %>%
  filter(PWGTP < 10000) %>%
  # Filter to college graduates
  filter(SCHL > 20) %>%
  # Filter to people in labor force
  filter(!ESR %in% c(0,6)) %>%
  mutate(age = as.numeric(AGEP),
         cohort = 2023 - age) %>%
  dplyr::select(-c(AGEP,age)) %>%
  filter(cohort > 1929 & cohort < 2003)

# Identify the censored upper bound of age cohorts
retired = col_grad_micro %>%
  mutate(esr = as.integer(ESR),
         age = as.numeric(AGEP),
         cohort = 2023 - age) %>%
  dplyr::select(-c(AGEP,age)) %>%
  mutate(emp = case_when(esr == 6 | is.na(esr) ~ "not_emp",
                         esr %in% c(1:3) ~ "emp",
                         esr %in% c(4,5) ~ "military")) %>%
  filter(SCHL > 20) %>%
  group_by(cohort,emp) %>%
  summarize(n = sum(PWGTP)) %>%
  ungroup(cohort) %>%
  group_by(emp) %>%
  mutate(shr = n/sum(n)) %>%
  arrange(emp,cohort) %>%
  mutate(cum_shr = cumsum(shr))

# P(employed) by birth cohort conditional on college graduate
lf = retired %>%
  ungroup() %>%
  group_by(cohort) %>%
  mutate(shr = n/sum(n),
         year = cohort + 22) %>%
  filter(emp == "emp", cohort > 1960) %>%
  select(birth_cohort=cohort,year,n,shr)


# IPEDS award data begins with the class of 1983
# Thus, we create the diagnostic to report how much of the college educated
# labor force is removed from the sample
# When building the estimate of the labor force by BA institution,
# we can use the weights here to discount older cohorts more likely to be retired
da = raw_deg_award %>%
  left_join(lf, by = "year") %>%
  mutate(adj_count = shr * awards_6digit,
         entry_cohort = birth_cohort + 18) %>%
  select(unitid,birth_cohort,entry_cohort,awards_6digit,adj_count) 
 
# For the simplest possible model, let's assume everyone finished college in four years
# Thus, the relationship between college graduation cohort and birth cohort is obvious and intuititve
# For now, ignore racial and sex differences in labor force participation 
# (mostly because I don't want to pull all the data right now)

# With these simplifications, we can get predicted number of working alumni per college cohort
# To make progress, let's imagine the origin state shares are fixed by institution
# Then you can construct estimated counts of every college graduate in the US by their origin state
orig = res %>% 
  filter(type_of_freshman == 99) %>%
  filter(!state_of_residence %in% c(58,89,99,98)) %>%
  group_by(unitid) %>%
  mutate(min_year = min(year)) %>%
  filter(year == min_year) %>%
  ungroup() %>%
  group_by(unitid) %>%
  mutate(total_enroll = sum(enrollment_fall)) %>%
  inner_join(da, by = c("unitid","year"="entry_cohort")) %>%
  mutate(shr_state = enrollment_fall/total_enroll,
         adj_state_count = shr_state * adj_count) 
  
# Collapse to a unitid x state dataset without time variation
orig_col = orig %>%
  group_by(unitid,fips,state_of_residence) %>%
  summarize(adj_state_count = sum(adj_state_count,na.rm=TRUE))

# Useful additions to this model:
# 1. Completion rates that vary across cohort
# 2. In-state shares that vary across cohorts
# 3. Completion rates that vary across race and sex
# 4. Labor force participation shares that vary by race and sex
# 5. Graduation rates vary across institutions
# 6. Graduation rates vary across cohorts within institutions

# Construct state-level panel of full-time college graduates by race and sex
# How many Native American college graduates worked in Michigan
cgp = col_grad_filt %>% 
  mutate(race = case_when(RAC2P == "1000" & HISP == "01" ~ "white",
                          RAC2P == "3000" & HISP == "01" ~ "black",
                          RAC2P %in% c(4000:4025,7500:7508) & HISP == "01" ~ "asian",
                          RAC2P %in% c(5000:5025) & HISP == "01" ~ "native",
                          RAC2P %in% c(8000,9000) & HISP == "01" ~ "multiple",
                          TRUE ~ "hispanic")) %>%
  group_by(race,cohort,STATE) %>%
  summarize(n = sum(PWGTP, na.rm=TRUE)) %>%
  ungroup(race,cohort) %>%
  group_by(STATE) %>%
  mutate(shr = n/sum(n))

# Calculate total number of college graduates in labor market by age
deg_award_2000 = fread(file.path(data_dir,"colleges_ipeds_completions-2digcip_2000.csv"))
deg_award_2010 = fread(file.path(data_dir,"colleges_ipeds_completions-2digcip_2010.csv"))
deg_award_2020 = fread(file.path(data_dir,"colleges_ipeds_completions-2digcip_2020.csv"))
origin = fread(file.path(data_dir,"colleges_ipeds_fall-res.csv"))
grad_rates_100 = fread(file.path(data_dir,"colleges_ipeds_grad-rates.csv"))

# Ultimately, we want to know the number degrees awarded by state of origin
origin %>% filter(unitid == 170976, type_of_freshman == 99)

grad_rates_100 %>% filter(unitid == 170976, subcohort == 2, race == 99, sex == 99, year == 2014)




grad_rates_cohort = raw_grad_rates %>%
  # Assume non-completers quit at a linear rate over eight years
  mutate(yr_1 = yr_1_to_4_complete + non_completion_rate,
         yr_2 = yr_1_to_4_complete + (non_completion_rate * 0.875),
         yr_3 = yr_1_to_4_complete + (non_completion_rate * 0.75),
         yr_4 = yr_1_to_4_complete + (non_completion_rate * 0.625),
         yr_5 = yr_4_to_6_complete + (non_completion_rate * 0.5),
         yr_6 = yr_4_to_6_complete + (non_completion_rate * 0.375),
         yr_7 = yr_6_to_8_complete + (non_completion_rate * 0.25),
         yr_8 = yr_6_to_8_complete + (non_completion_rate * 0.125),
         sum = yr_1 + yr_2 + yr_3 + yr_4 + yr_5 + yr_6 + yr_7 + yr_8) %>%
  select(-c(ends_with("complete"))) %>%
  select(c(unitid,year,cohort_year,cohort_rev,fips,starts_with("yr"))) %>%
  pivot_longer(
    cols = starts_with("yr_"),      # Select columns that start with "yr_"
    names_to = "year_in_school",    # Name of the new column that will contain the original column names
    names_prefix = "yr_",           # Remove the "yr_" prefix from the column names
    values_to = "shr"               # Name of the new column that will contain the values
  ) %>%
  mutate(year_in_school = as.numeric(year_in_school)+cohort_year-1,
         shr_hc_adj = shr * cohort_rev)

denominators = grad_rates_cohort %>%
  group_by(year_in_school,unitid) %>%
  summarize(annual_shr = sum(shr_hc_adj))

cohort_enrollments = grad_rates_cohort %>%
  left_join(denominators, by = c("year_in_school","unitid")) %>%
  mutate(norm_shr = shr_hc_adj/annual_shr) %>%
  select(-c(year,shr_hc_adj,annual_shr,shr)) %>%
  filter(year_in_school %in% c(2002:2015))

cohort_non_completes = raw_grad_rates %>%
  # Assume non-completers quit at a linear rate over eight years
  mutate(yr_1 = non_completion_rate,
         yr_2 = non_completion_rate * 0.875,
         yr_3 = non_completion_rate * 0.75,
         yr_4 = non_completion_rate * 0.625,
         yr_5 = non_completion_rate * 0.5,
         yr_6 = non_completion_rate * 0.375,
         yr_7 = non_completion_rate * 0.25,
         yr_8 = non_completion_rate * 0.125,
         sum = yr_1 + yr_2 + yr_3 + yr_4 + yr_5 + yr_6 + yr_7 + yr_8) %>%
  select(-c(ends_with("complete"))) %>%
  select(c(unitid,year,cohort_year,cohort_rev,fips,starts_with("yr"))) %>%
  pivot_longer(
    cols = starts_with("yr_"),      # Select columns that start with "yr_"
    names_to = "year_in_school",    # Name of the new column that will contain the original column names
    names_prefix = "yr_",           # Remove the "yr_" prefix from the column names
    values_to = "shr"               # Name of the new column that will contain the values
  ) %>%
  mutate(year_in_school = as.numeric(year_in_school)+cohort_year-1,
         shr_non_complete_adj = shr * cohort_rev) %>%
  left_join(denominators, by = c("year_in_school","unitid")) %>%
  mutate(non_complete_shr = shr_non_complete_adj / annual_shr) %>%
  select(c(unitid,cohort_year,year_in_school,non_complete_shr)) %>%
  filter(year_in_school %in% c(2002:2015))

unified_cohort = cohort_enrollments %>%
  left_join(cohort_non_completes, by = c("unitid","cohort_year","year_in_school"))