## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(texreg)
# Fastest package for adding cluster-robust standard errors:
# https://www.r-bloggers.com/2021/05/clustered-standard-errors-with-r/#google_vignette 
#library(estimatr)
library(fixest)
library(tidyverse)
library(readxl)
library(table.express)
library(data.table)
#library(progressr)

options(digits = 1)
#handlers(global = TRUE) #  For progress bars


## ----directory, include=FALSE-------------------------------------------------
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
specification = "great_recession"
# [FIXED -- 2026-07-29, Phase D] Every write below assumes this exists;
# create it rather than erroring on first write with a bare "cannot open
# file" (CODEBASE_AUDIT.md sec 3).
dir.create(file.path(data_dir, specification), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, specification), recursive = TRUE, showWarnings = FALSE)

# Automate filtering
cohort_list <- list(post_2000 = c(2000:2013),
                pre_2000 = c(1982:1999),
                pooled = c(1982:2013),
                great_recession = c(2007:2009))
cohorts = cohort_list[specification]

geos = c("cbsa","state")
#geos = c("state")
avgs = c(7,5,3,1)
migration_ends <- c(5, 10)
#avgs = c(7,5)
#norm = c("","_ptile")
norm = c("","_ptile")
stage = c("_shock","_growth")
shock_lbl = c("1-Year Shock",
              "3-Year Average",
              "5-Year Average",
              "7-Year Average")
# Number of bins is flexible
num_bins = c(5,10)
universes = c("stay","leave_hs","leave_col")


## ----import, include=FALSE----------------------------------------------------
if(file.exists(file.path(data_dir,"intermediate/microdata.csv")))
{
  microdata <- fread(file.path(data_dir,"intermediate/microdata.csv"))
} else {
  microdata <- fread(file.path(data_dir,"intermediate/microdata_sample__06.csv"))
}
microdata <- microdata[,V1 := NULL]


## ----import_dup2, include=FALSE-----------------------------------------------
regression <- microdata[, !duplicated(names(microdata)), with = FALSE] %>%
  # Create variable for non-traditional students
  table.express::mutate(non_traditional = ifelse(col_end - birth > 24,
                                                 "Non-Traditional",
                                                 "Traditional"),
                        # Recode variable for transfer students to ease interpretation
                        transfer = ifelse(transfer == "0",
                                          "Non-Transfer",
                                          "Transfer"),
                        in_state = ifelse(col_state == hs_state, "In-State","Out-of-State"),
                        col_end = as.factor(col_end),
                        #in_state = ifelse(hs_state == col_state,"In-State","Out-of-State"),
                        binary_dist = case_when(dist %in% c("[0,50]","(50,100]","(100,150]","(150,200]","(200,250]") ~ "Near",
                                               TRUE ~ "Far"
                                               ),
                        col_in_not_hs_cbsa = ifelse(hs_cbsa_code == col_cbsa_code,0,1),
                        soc_2 = substr(soc_code_mode, start = 1, stop = 2),
                        soc_3 = substr(soc_code_mode, start = 1, stop = 4)) %>%
  table.express::filter(is.na(dist)==FALSE) %>%
  # For now just filter out College Station
  table.express::filter(hs_name != "College Station High School")
dist_bins = levels(regression$dist)[-1]


# Select only the 'unitid' and 'inst_group' columns from institutional_characteristics
inst_group <- fread(file.path(data_dir,"intermediate/institutional_characteristics.csv")) %>%
  select(unitid,inst_group)#institutional_characteristics[,c("unitid", "inst_group")]

# Merge the two data.tables on 'unitid'
regression <- merge(regression, inst_group, by.x = "col_unitid",by.y = "unitid",all.x = TRUE)
regression[, inst_group := factor(inst_group)]
regression[, in_state := factor(in_state)]
regression[, inst_group := relevel(inst_group, ref = "PF")]
regression[, in_state := relevel(regression$in_state, ref = "Out-of-State")]
regression[, instate := ifelse(hs_state==col_state,1,0)]
regression[, hs_col_diff_cbsa := ifelse(hs_cbsa==col_cbsa_code,"Same","Moved for College")]


# Filter institutions without a unitid (mostly community college)
regression <- regression[!is.na(inst_group) & inst_group != "Private, For-Profit"]

# Create in-state x inst_group column
regression[,inst_group_in_state := case_when(inst_group == "RPU" & in_state == "In-State" ~ "RPU, In-State",
                                                  inst_group == "PF" & in_state == "In-State" ~ "PF, In-State",
                                                  inst_group == "Private, Non-Profit" & in_state == "In-State" ~ "Private, In-State",
                                                  inst_group == "RPU" & in_state == "Out-of-State" ~ "RPU, Out-of-State",
                                                  inst_group == "PF" & in_state == "Out-of-State" ~ "PF, Out-of-State",
                                                  inst_group == "Private, Non-Profit" & in_state == "Out-of-State" ~ "Private, Out-of-State")]

# Create return bin columns
regression[,return_within_5 := case_when(yr_return_hs_cbsa %in% c(1:5) | yr_return_col_cbsa %in% c(1:5)  ~ "Return Within 5 Years",
                                              yr_return_hs_cbsa %in% c(6:10) | yr_return_col_cbsa %in% c(6:10) ~ "Return in 5-10 Years",
                                              yr_return_hs_cbsa > 10 | yr_return_col_cbsa > 10 ~ "Return After 10 Years",
                                              is.na(yr_return_hs_cbsa) & is.na(yr_return_col_cbsa) ~ "Never Returned")]
# Create labor market distress columns
# Create bottom 25% indicator
regression[, hs_shock_quartile := fifelse(
  hs_shock_1 <= quantile(hs_shock_1, 0.25, na.rm = TRUE), "Bottom LD Quartile",
  fifelse(hs_shock_1 >= quantile(hs_shock_1, 0.75, na.rm = TRUE), "Top LD Quartile", "Middle LD Quartiles")
), by = col_end]

#differences_unfilt <- read_csv(file.path(data_dir,"intermediate/local_differences_unfilt.csv")) %>%
#  mutate(cbsa_code = str_pad(hs_cbsa,width=5,side="left",pad="0")) %>%
#  select(-c("hs_cbsa","...1")) %>%
#  left_join(cbsa_li_crosswalk %>% select(c("cbsa_code","cbsa_core")) %>% distinct(), by = "cbsa_code") %>%
#  mutate(difference_cbsa_alt = shr_returners_cbsa - pred_shr_returners_cbsa) %>%
#  filter(difference_cbsa_alt > 0)



regression[, col_end_numeric := as.numeric(as.character(col_end))]

# Do people who finish college after age 24 have different mobility patterns than
# those who finish before age 24. Probably use, profile of institutions varies
na_count = regression[, lapply(.SD, function(x) sum(is.na(x)))]  

missing_inst_group = regression[is.na(inst_group), .N, by = .(col_unitid,col_name)]
#all_binary_final = read.csv(file.path(data_dir,"results/regression_data__top.csv"))
#actual_cohort = read.csv(file.path(data_dir,"results/actual_cohort.csv"))



## ----clean dep var------------------------------------------------------------

measure_return <- function(data, reference_col, output_col, column_prefix, max) {
  data[, (output_col) := {
    equality <- lapply(.SD, `==`, get(reference_col))  # Check if columns match reference_col
    matches <- Reduce(`|`, equality)  # Combine with OR to check if any match exists
    inequality <- lapply(.SD, `!=`, get(reference_col))  # Check if columns don't match reference_col
    no_matches <- Reduce(`|`, inequality)  # Combine with OR to check if any mismatch exists
    has_data <- rowSums(!is.na(.SD)) > 0  # Check if there's valid data in any column
    
    # Apply case_when logic
    case_when(
      is.na(matches) & is.na(no_matches) ~ NA,
      matches == TRUE & is.na(no_matches) ~ 1,
      is.na(matches) & no_matches == TRUE ~ 0,
      matches == TRUE & no_matches == FALSE ~ 1,
      matches == FALSE & no_matches == TRUE ~ 0,
      matches == TRUE & no_matches == TRUE ~ 1
    )
  }, .SDcols = paste0(column_prefix, 0:max)]
}

measure_leave <- function(data, reference_col, output_col, column_prefix, max) {
  data[, (output_col) := {
    equality <- lapply(.SD, `==`, get(reference_col))  # Check if columns match reference_col
    matches <- Reduce(`|`, equality)  # Combine with OR to check if any match exists
    inequality <- lapply(.SD, `!=`, get(reference_col))  # Check if columns don't match reference_col
    no_matches <- Reduce(`|`, inequality)  # Combine with OR to check if any mismatch exists
    has_data <- rowSums(!is.na(.SD)) > 0  # Check if there's valid data in any column
    
    # Apply case_when logic
    case_when(
      is.na(matches) & is.na(no_matches) ~ NA,
      matches == TRUE & is.na(no_matches) ~ 0,
      is.na(matches) & no_matches == TRUE ~ 1,
      matches == TRUE & no_matches == FALSE ~ 0,
      matches == FALSE & no_matches == TRUE ~ 1,
      matches == TRUE & no_matches == TRUE ~ 1
    )
  }, .SDcols = paste0(column_prefix, 0:max)]
}



for (max in migration_ends) {  # Define the lookaround periods dynamically
  
  # Apply the function for different versions
  measure_return(regression, "hs_cbsa_code", paste0("same_cbsa_hs_", max), "cbsa_code_", max)  
  measure_return(regression, "col_cbsa_code", paste0("same_cbsa_col_", max), "cbsa_code_", max)  
  measure_return(regression, "hs_state", paste0("same_state_hs_", max), "cbsa_state_", max)  
  measure_return(regression, "col_state", paste0("same_state_col_", max), "cbsa_state_", max)  
  
  # Make dependent variables mutually exclusive
  regression[, paste0("return_hs_col_cbsa_", max) := get(paste0("same_cbsa_hs_", max)) * get(paste0("same_cbsa_col_", max))]
  regression[, (paste0("same_cbsa_hs_", max)) := case_when(
    get(paste0("return_hs_col_cbsa_", max)) == 1 & yr_return_hs_cbsa > yr_return_col_cbsa ~ 1,
    get(paste0("return_hs_col_cbsa_", max)) == 1 & yr_return_hs_cbsa < yr_return_col_cbsa ~ 0,
    TRUE ~ get(paste0("same_cbsa_hs_", max))
  )]
  regression[, (paste0("same_cbsa_col_", max)) := case_when(
    get(paste0("return_hs_col_cbsa_", max)) == 1 & yr_return_hs_cbsa > yr_return_col_cbsa ~ 0,
    get(paste0("return_hs_col_cbsa_", max)) == 1 & yr_return_hs_cbsa < yr_return_col_cbsa ~ 1,
    TRUE ~ get(paste0("same_cbsa_col_", max))
  )]
  
  # Create return like Yagan (2014): residence at beginning and end of period
  regression[, (paste0("yagan_cbsa_col_", max)) := fifelse(get(paste0("cbsa_code_", max))==get(paste0("col_cbsa_code")),1,0)]
  regression[, (paste0("yagan_cbsa_hs_", max)) := fifelse(get(paste0("cbsa_code_", max))==get(paste0("hs_cbsa")),1,0)]
  regression[, (paste0("yagan_state_col_", max)) := fifelse(get(paste0("cbsa_state_", max))==get(paste0("col_state")),1,0)]
  regression[, (paste0("yagan_state_hs_", max)) := fifelse(get(paste0("cbsa_state_", max))==get(paste0("hs_state")),1,0)]

  
  # Apply the function for different versions
  measure_leave(regression, "hs_cbsa_code", paste0("ever_leave_cbsa_hs_", max), "cbsa_code_", max)  
  measure_leave(regression, "col_cbsa_code", paste0("ever_leave_cbsa_col_", max), "cbsa_code_", max)  
  measure_leave(regression, "hs_state", paste0("ever_leave_state_hs_", max), "cbsa_state_", max)  
  measure_leave(regression, "col_state", paste0("ever_leave_state_col_", max), "cbsa_state_", max)  
}
max <- migration_ends[1]


# These generate data used to show the linearity of the dependent variable
cohort_return_hs <- regression[ever_leave_cbsa_hs_10 == 1, mean(same_cbsa_hs_10 == 1, na.rm = TRUE), by = .(col_end)]
cohort_return_col <- regression[ever_leave_cbsa_col_10 == 1, mean(same_cbsa_col_10 == 1, na.rm = TRUE), by = .(col_end)]
write.csv(cohort_return_hs,file.path(data_dir,"intermediate/cohort_return_hs_10.csv"),row.names = FALSE)
write.csv(cohort_return_col,file.path(data_dir,"intermediate/cohort_return_col_10.csv"),row.names = FALSE)

# Measure levels of return for first figure 
leaver_levels_state <- regression[col_end %in% cohorts[[specification]] & ever_leave_state_hs_10 == 1,.N,by=.(same_state_hs_10,same_state_col_10,inst_group,hs_col_diff_cbsa,inst_group)]
write.csv(leaver_levels_state,file=file.path(directory,"Data",specification,"leaver_levels_state.csv"),row.names = FALSE)
leaver_levels_cbsa <- regression[col_end %in% cohorts[[specification]] & ever_leave_cbsa_hs_10 == 1,.N,by=.(same_cbsa_hs_10,same_cbsa_col_10,inst_group,hs_col_diff_cbsa,inst_group)]
write.csv(leaver_levels_cbsa,file=file.path(directory,"Data",specification,"leaver_levels_cbsa.csv"), row.names = FALSE)

for (max in migration_ends) {
  # Dynamically define relevant columns
  complete_cols <- c(paste0("ever_leave_cbsa_hs_", max), paste0("ever_leave_cbsa_col_", max), 
                     paste0("ever_leave_state_hs_", max), paste0("ever_leave_state_col_", max),
                     "inst_group", "in_state", "non_traditional", "transfer",
                     "col_major", "soc_3", "col_end", "hs_state", "prestige_bin")

  # Create relevant universes
  assign(paste0("regression_cbsa_", max), regression %>% 
           filter(get(paste0("ever_leave_cbsa_hs_", max)) == 1) %>% 
           mutate(hs_cbsa_code = factor(hs_cbsa_code)) %>%
           filter(col_end %in% cohorts[[specification]]) %>%
           .[complete.cases(.[, ..complete_cols])])

  assign(paste0("regression_state_", max), regression %>% 
           filter(get(paste0("ever_leave_state_hs_", max)) == 1) %>% 
           mutate(hs_cbsa_code = factor(hs_cbsa_code)) %>%
           filter(col_end %in% cohorts[[specification]]) %>%
           .[complete.cases(.[, ..complete_cols])])
  
  # Create out-of-sample universes for measuring selection
  assign(paste0("outsample_cbsa_", max), regression %>% 
           filter(get(paste0("ever_leave_cbsa_hs_", max)) == 1) %>% 
           mutate(hs_cbsa_code = factor(hs_cbsa_code)) %>%
           filter(col_end %in% cohort_list[["pre_2000"]]) %>%
           .[complete.cases(.[, ..complete_cols])])

  assign(paste0("outsample_state_", max), regression %>% 
           filter(get(paste0("ever_leave_state_hs_", max)) == 1) %>% 
           mutate(hs_cbsa_code = factor(hs_cbsa_code)) %>%
           filter(col_end %in% cohort_list[["pre_2000"]]) %>%
           .[complete.cases(.[, ..complete_cols])])

}





## ----sumstat------------------------------------------------------------------

## [FIXED -- 2026-07-29] institutional_characteristics.csv already carries a
## `region` column (02_col_chars.Rmd builds it the same way, state.abb/
## state.region, but also maps DC -> South). This block used to redundantly
## rebuild a less-complete version and re-merge it, colliding into
## region.x/region.y and breaking every downstream `by=.(region)` grouping
## below -- unreachable until institutional_characteristics.csv's duplicate-
## unitid bug (fixed the same session) let this merge actually complete.
institutional_characteristics <- fread(file.path(data_dir,"intermediate/institutional_characteristics.csv"))

cohort_x_grad <- as.data.frame(as.table(prop.table(table(regression$birth,regression$col_end), margin = 2)))
names(cohort_x_grad) <- c("birth","col_end","shr")
write.csv(cohort_x_grad,file.path(data_dir,"intermediate/cohort_x_grad.csv"),row.names = FALSE)

#  Compute summary statistics with no restrictions on college graduation year
sample_means = merge(regression, institutional_characteristics, by.x="col_unitid", by.y="unitid") %>%
  table.express::select(mobility_rate,par_mean,k_mean,avg_acceptance)
sample_chetty <- sample_means[,lapply(.SD, mean,na.rm=TRUE)]
# Distribution by institutional group
sample_inst_group <- regression[,.N,by= .(inst_group)] %>%
  table.express::mutate(shr = (N/sum(N))*100) %>%
  table.express::arrange(inst_group) 
# Distribution of birth years
sample_quantiles <- quantile(regression$birth, probs=c(0.1,0.25,0.5,0.75,0.9))
# Regional composition
sample_regions = merge(regression, institutional_characteristics, by.x="col_unitid", by.y="unitid")
sample_regions <- sample_regions[,.N,by=.(region)] %>%
  table.express::mutate(shr_region = (N/sum(N))*100) %>%
  table.express::arrange(region) %>%
  filter(!is.na(region))
# Share majoring in engineering or business
shr_eng <- regression[,mean(col_major=="Engineering")] * 100
shr_bus <- regression[,mean(col_major=="Business")] * 100

sample_values <- c(sample_inst_group$shr[1],0,sample_inst_group$shr[2],sample_inst_group$shr[3],shr_bus,shr_eng,sample_chetty$mobility_rate*100,sample_chetty$par_mean,sample_chetty$k_mean,sample_chetty$avg_acceptance,sample_quantiles,sample_regions$shr_region,nrow(regression))
saveRDS(sample_values,file.path(directory,"Data",specification,"sample_values.rds"))

# Compute summary statistics, restricting to main analysis sample (2000-2013)
restrict_means = merge(regression_cbsa_10, institutional_characteristics, by.x="col_unitid", by.y="unitid") %>%
  table.express::select(mobility_rate,par_mean,k_mean,avg_acceptance)
restrict_chetty <- restrict_means[,lapply(.SD, mean,na.rm=TRUE)]
# Distribution by institutional group
restrict_inst_group <- regression_cbsa_10[,.N,by= .(inst_group)] %>%
  table.express::mutate(shr = (N/sum(N))*100) %>%
  table.express::arrange(inst_group) 
# Distribution of birth years
restrict_quantiles <- quantile(regression_cbsa_10$birth, probs=c(0.1,0.25,0.5,0.75,0.9))
# Regional composition
restrict_regions = merge(regression_cbsa_10, institutional_characteristics, by.x="col_unitid", by.y="unitid")
restrict_regions <- restrict_regions[,.N,by=.(region)] %>%
  table.express::mutate(shr_region = (N/sum(N))*100) %>%
  table.express::arrange(region) 
# Share majoring in engineering or business
shr_eng <- regression_cbsa_10[,mean(col_major=="Engineering")] * 100
shr_bus <- regression_cbsa_10[,mean(col_major=="Business")] * 100

restrict_values <- c(restrict_inst_group$shr[1],0,restrict_inst_group$shr[2],restrict_inst_group$shr[3],shr_bus,shr_eng,restrict_chetty$mobility_rate*100,restrict_chetty$par_mean,restrict_chetty$k_mean,restrict_chetty$avg_acceptance,restrict_quantiles,restrict_regions$shr_region,nrow(regression_cbsa_10))
saveRDS(restrict_values,file.path(directory,"Data",specification,"restrict_values.rds"))

# Creating summary statistics for out-of-sample
outsample_means = merge(outsample_cbsa_10, institutional_characteristics, by.x="col_unitid", by.y="unitid") %>%
  table.express::select(mobility_rate,par_mean,k_mean,avg_acceptance)
outsample_chetty <- outsample_means[,lapply(.SD, mean,na.rm=TRUE)]
# Distribution by institutional group
outsample_inst_group <- outsample_cbsa_10[,.N,by= .(inst_group)] %>%
  table.express::mutate(shr = (N/sum(N))*100) %>%
  table.express::arrange(inst_group) 
# Distribution of birth years
outsample_quantiles <- quantile(outsample_cbsa_10$birth, probs=c(0.1,0.25,0.5,0.75,0.9))
# Regional composition
outsample_regions = merge(outsample_cbsa_10, institutional_characteristics, by.x="col_unitid", by.y="unitid")
outsample_regions <- outsample_regions[,.N,by=.(region)] %>%
  table.express::mutate(shr_region = (N/sum(N))*100) %>%
  table.express::arrange(region) %>%
  table.express::filter(!is.na(region))
# Share majoring in engineering or business
shr_eng <- regression_cbsa_10[,mean(col_major=="Engineering")] * 100
shr_bus <- regression_cbsa_10[,mean(col_major=="Business")] * 100

outsample_values <- c(outsample_inst_group$shr[1],0,outsample_inst_group$shr[2],outsample_inst_group$shr[3],shr_bus,shr_eng,outsample_chetty$mobility_rate*100,outsample_chetty$par_mean,outsample_chetty$k_mean,outsample_chetty$avg_acceptance,outsample_quantiles,outsample_regions$shr_region,nrow(regression_cbsa_10))
saveRDS(outsample_values,file.path(directory,"Data",specification,"outsample_values.rds"))

