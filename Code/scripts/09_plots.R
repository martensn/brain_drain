## ----setup, include=FALSE-----------------------------------------------------

knitr::opts_chunk$set(echo = TRUE)
library(xtable)
library(data.table)
library(extrafont)
library(educationdata)
library(ggrepel)
library(cartogram)
library(spatstat)
library(vars)
library(tidycensus)
library(spdep)
library(sf)
library(texreg)
library(scales)
library(stargazer)
library(ggplot2)
library(tidyverse)
library(dplyr)

census_key <- Sys.getenv("CENSUS_KEY")

# Only needs to run once
#font_import(pattern = "lmroman*")
#loadfonts()

library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
specification = "pre_2000"
# Automate filtering
cohort_list <- list(post_2000 = c(2000:2013),
                pre_2000 = c(1982:1999),
                pooled = c(1982:2013),
                great_recession = c(2007:2009))
cohorts = cohort_list[specification]

# Height, width, units, and background for maps
hei = 3.75
wid = 6.5
dpi = 600
bkgrnd = 'transparent'
unit = "in"
geos = c("cbsa","state")

diverging_colors = c('#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837')
prestige_bin_colors <- c("#7fc97f","#beaed4","#fdc086","#ffff99")
inst_group_colors <- c("#F8766D","#7CAE00","#00BFC4","#C77CFF")
monotonic_colors = c('#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837') 
return_colors = c()

options(scipen = 999)
# Ensures stargazer tables have three digits instead of a zero after the first two
options(digits=3)
`%nin%` = Negate(`%in%`)


## ----eval=FALSE---------------------------------------------------------------
# 
# observation_accounting = data.table(Reason = c("All BA Completers in US",
#                       "Include HS",
#                       "Work history has geodata",
#                       "Work history has end date after start",
#                       "Work history has start or end date",
#                       paste0("Work history between 1975 and 2023"),
#                       "College appears in IPEDS",
#                       "Graduated college [1982,2021] and born [1930,2002]",
#                       "Educational institutions in 50 states or DC",
#                       "Final analytic sample"),
#            Sample = c(col_users_length,
#                       both_hs_col_length,
#                       no_work_location,
#                       non_chronological_work,
#                       no_work_date,
#                       no_work_hist,
#                       college_dne,
#                       birth_outliers,
#                       territory_hs_col,
#                       "Add later"))
#                       #nobs(models[[1]])))
# 
# #tli.table <- xtable(observation_accounting[1:10, ])
# #print(tli.table, include.rownames = FALSE, booktabs = TRUE)
# 
# stargazer(observation_accounting[1:10,],
#           summary = FALSE,
#           rownames = FALSE,
#           digits = 1,
#           #style = "qje",
#           align = TRUE,
#           title = "Effect of data cleaning on sample size",
#           float = TRUE,
#           out = file.path(directory,"Outputs",specification,"observation-accounting.tex"),
#           #label = "table:observation",
#           digit.separator = ","
#           )
#           #caption = "Most LI users omit their high school, eliminating more than 90 percent of users. Imposing the requirement of at least one position with a location, start date, and end date removes users with minimal information on their profiles. Availability of County Business Patterns data resulted in the restrictions on college graduation. County-level estimates of labor demand shocks are only available until 2021. Most of the observations removed from the sample were people who graduated after 2021. The limits on estimated year of birth remove obvious outliers that likely stem from data entry errors.",
# 
# 


## ----occp---------------------------------------------------------------------

li_soc = read_csv(file.path(data_dir,"intermediate/soc_li_distribution.csv"),col_select = -`...1`) %>%
  mutate(socp = substr(soc_code_mode,1,7)) %>%
  group_by(socp) %>%
  summarize(shr_li = sum(shr_li))

# Import
soc_ba = read_csv(file.path(data_dir,"intermediate/soc_ba.csv"),col_select = -`...1`) %>%
  left_join(li_soc, by = "socp")

# Filter to 2-digit occupational groups  
soc_2 = soc_ba %>%
  group_by(soc_2_title,prestige_bin) %>%
  summarize(shr_li = sum(shr_li, na.rm = TRUE),
            shr_bls = sum(shr_bls, na.rm=TRUE),
            col_grad = sum(col_grad, na.rm=TRUE))

# Fit the linear model
soc_model <- lm(shr_bls ~ shr_li, weight = col_grad, data = soc_2)

soc_model_sum <- summary(soc_model)

soc_r_sq <- round(soc_model_sum$r.squared, 3)

soc_coefficients <- soc_model_sum$coefficients

# Extract coefficients
soc_intercept <- round(soc_coefficients[1, 1], 3)
soc_slope <- round(soc_coefficients[2, 1], 3)
soc_std_err <- round(soc_coefficients[2, 2], 3)

# Create the equation as a string
soc_equation <- paste0("Best Fit: y = ",soc_intercept, " + ",soc_slope, "x")
soc_r_squared <- paste0("R² = ", soc_r_sq)
soc_std_error <- paste0("Std. Error = ",soc_std_err)

ggplot(soc_2, aes(y = shr_bls, x = shr_li, color = prestige_bin, shape = prestige_bin, size = col_grad)) + 
  geom_point(alpha = 0.6, show.legend = c(size = FALSE)) +
  scale_fill_manual(values = prestige_bin_colors) +
  scale_color_manual(values = prestige_bin_colors) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +  # y = x line
  scale_x_continuous(expand = c(0.001,0),
                     limits = c(0,0.101),
                     labels = percent_format()) + 
  scale_y_continuous(limits = c(0,0.10),
                     expand = c(0,0),
                     labels = percent_format()) + 
  xlab("Occupational Group Share of Labor Force (Analytical Sample)") +
  ylab("BLS Benchmark") +
  labs(title = "Occupational Composition of College-Educated Labor Force") +
  # Adjust legend title width or text wrapping
  guides(color = guide_legend(title = "Prestige\nBin"),
         shape = guide_legend(title = "Prestige\nBin")) + # Split the title into two lines
  #       size = guide_legend(title = "College\nGraduates")) +  # Adjust other legends similarly if needed
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color="black")) +
  # Add the best fit line equation, R^2, and standard error below the legend
  annotate("text", x = 0.0005, y = 0.095, label = soc_equation, size = 4, color = "black", hjust = 0, vjust = 1, family = "LM Roman 10") +
  annotate("text", x = 0.0005, y = 0.085, label = soc_r_squared, size = 4, color = "black", hjust = 0, vjust = 1, family = "LM Roman 10") +
  annotate("text", x = 0.0005, y = 0.090, label = soc_std_error, size = 4, color = "black", hjust = 0, vjust = 1, family = "LM Roman 10")
  
  ggsave(file = paste(directory,"/Outputs/",specification,"/occupational_composition.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi) 



## ----prestige-bins------------------------------------------------------------

largest_occp = soc_ba %>%
  distinct(soc_title,socp, .keep_all = TRUE) %>%
  group_by(prestige_bin) %>%
  slice_max(col_grad,n=5) %>%
  mutate(shr_li = ifelse(is.na(shr_li),0,shr_li * 100),
         shr_bls = ifelse(is.na(shr_bls),0,shr_bls * 100),
         col_grad = round(col_grad/1000,1)) %>%
  dplyr::select(c(soc_title,socp,col_grad,prestige_bin,shr_li,shr_bls)) %>%
  ungroup()

names(largest_occp) <- c("Occupation Title","SOC","Graduates","prestige_bin","LI Share","BLS Share")

occp_h <- xtable(largest_occp %>% 
         filter(prestige_bin == "High") %>%
         dplyr::select(-prestige_bin),
       digits = c(0,0,0,1,2,2))
print(occp_h,file=file.path(directory,"Outputs",specification,"largest_occp_high.tex"),include.rownames = FALSE)
occp_m <- xtable(largest_occp %>% 
         filter(prestige_bin == "Middle") %>%
         dplyr::select(-prestige_bin),
       digits = c(0,0,0,1,2,2))
print(occp_m,file=file.path(directory,"Outputs",specification,"largest_occp_middle.tex"),include.rownames = FALSE)
occp_m <- xtable(largest_occp %>% 
         filter(prestige_bin == "Low") %>%
         dplyr::select(-prestige_bin),
       digits = c(0,0,0,1,2,2))
print(occp_m,file=file.path(directory,"Outputs",specification,"largest_occp_low.tex"),include.rownames = FALSE)



## ----occp-inst-group----------------------------------------------------------

occp_inst_group_counts <- fread(file.path(directory,"Data",specification,"occp_inst_group.csv"))
occp_inst_group_counts[, prestige_bin := factor(prestige_bin, levels = c("High","Middle","Low"),ordered=TRUE)]
occp_inst_group_counts[, inst_group_total := sum(N), by = .(inst_group,col_end_bin)]
occp_inst_group_counts[, shr_inst_group := N/sum(N), by = .(inst_group,col_end_bin)]
occp_inst_group_counts[, shr_total := N/sum(N)]
#occp_inst_group_counts[, col_end_bin := factor(col_end_bin, levels = c("0-5","6-10","11-15"),ordered=TRUE)]

oi <- occp_inst_group_counts[,.(shr_total = sum(shr_total)), by = .(prestige_bin,inst_group)]

ggplot(occp_inst_group_counts, aes(x = col_end_bin, y = shr_inst_group, fill = prestige_bin)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = prestige_bin_colors) +
  scale_color_manual(values = prestige_bin_colors) +
  facet_wrap(~ inst_group, scales = "free_x") +
  labs(title ="Occupational Prestige by Institutional Grouping",
       fill = "Prestige Bin") +
  labs(x = "Years of College Graduation", y = "Share in Prestige Bin") +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0,1.01),
                     expand = c(0.001,0)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text         = element_text(size=10, family="LM Roman 10"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(color="black"))
ggsave(file = paste(directory,"/Outputs/",specification,"/occp_cross_section.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)



## ----return-------------------------------------------------------------------

raw_return <- fread(file.path(data_dir,"intermediate/return_occp.csv"))

# Obtain weights and institutional groupings from institutional_characteristics 
inst_weights <- read_csv(file.path(data_dir,"intermediate/institutional_characteristics.csv")) %>%
  table.express::select(inst_group,unitid,total_deg_awarded) %>%
  # Remove universities that don't report degrees awarded and for-profits
  table.express::filter(!is.na(total_deg_awarded) & inst_group != "Private, For-Profit")

# Compute cumulative return by institutional group, weighted by institution-level returns
# LI data reweighted to represent the institutional composition of post-2000 college grads
# Perform the merge and calculate weighted means
inst_return <- merge(raw_return, inst_weights, by.x = "col_unitid", by.y = "unitid")[
  , .(
    shr_ever_return_cbsa = weighted.mean(shr_ever_return_cbsa, w = total_deg_awarded, na.rm = TRUE),
    shr_return_cbsa = weighted.mean(shr_return_cbsa, w = total_deg_awarded, na.rm = TRUE),
    shr_ever_return_state = weighted.mean(shr_ever_return_state, w = total_deg_awarded, na.rm = TRUE),
    shr_return_state = weighted.mean(shr_return_state, w = total_deg_awarded, na.rm = TRUE)
  ),
  by = .(years, inst_group,prestige_bin)
] %>%
  mutate(prestige_bin = factor(prestige_bin, ordered=TRUE,labels=c("High","Middle","Low")))




## ----return_dup2--------------------------------------------------------------
# Reshape with melt to put `shr_ever_return` and `shr_return` values into the same column
inst_return_long <- melt(
  inst_return,
  id.vars = c("years", "inst_group"), 
  measure.vars = patterns("shr_ever_return_", "shr_return_"), 
  variable.name = "type", 
  value.name = c("shr_ever_return", "shr_return")
)

# Replace numeric `type` values (1, 2) with "CBSA" and "State"
inst_return_long[, type := fifelse(type == 1, "Labor Market", "State")]

# Plot returns by institutional grouping
ggplot(inst_return_long, aes(x = years, y = shr_ever_return)) + 
  geom_point(aes(color = inst_group,shape = type)) +
  geom_line(aes(color = inst_group,linetype = type)) +
  scale_color_manual(values = inst_group_colors) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.1,20.1)) + 
  scale_y_continuous(limits = c(0,0.61),
                     expand = c(0,0),
                     labels = percent_format()) + 
  xlab("Years Since College Graduation") +
  ylab("Share Ever Returning") +
  labs(title = "Share of College Graduates Ever Returning to High School Geography") +
  # Adjust legend title width or text wrapping
  guides(color = guide_legend(title = "Institutional\nGroup"),
         linetype = guide_legend(title = ""),
         shape = guide_legend(title = "")) +  # Adjust other legends similarly if needed
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color="black"))
ggsave(file = paste(directory,"/Outputs/",specification,"/ever_return.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi) 

# Plot returns by institutional grouping
ggplot(inst_return_long, aes(x = years, y = shr_return)) + 
  geom_point(aes(color = inst_group,shape = type)) +
  geom_line(aes(color = inst_group,linetype = type)) +
  scale_color_manual(values = inst_group_colors) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.1,20.1)) + 
  scale_y_continuous(limits = c(0,0.8),
                     expand = c(0,0),
                     labels = percent_format()) + 
  xlab("Years Since College Graduation") +
  ylab("Share in Geography") +
  labs(title = "Share of College Graduates in High School Geography") +
  # Adjust legend title width or text wrapping
  guides(color = guide_legend(title = "Institutional\nGroup"),
         linetype = guide_legend(title = ""),
         shape = guide_legend(title = "")) +  # Adjust other legends similarly if needed
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color="black"))
ggsave(file = paste(directory,"/Outputs/",specification,"/return.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi) 



## ----return_dup3--------------------------------------------------------------

# Reshape with melt to put `shr_ever_return` and `shr_return` values into the same column
inst_return_long_occp <- melt(
  inst_return,
  id.vars = c("years", "inst_group","prestige_bin"), 
  measure.vars = patterns("shr_ever_return_", "shr_return_"), 
  variable.name = "type", 
  value.name = c("shr_ever_return", "shr_return")
)

# Replace numeric `type` values (1, 2) with "CBSA" and "State"
inst_return_long_occp[, type := fifelse(type == 1, "Labor Market", "State")]

# Plot returns by institutional grouping
ggplot(inst_return_long_occp, aes(x = years, y = shr_ever_return)) + 
  geom_point(aes(color = inst_group, shape = inst_group), alpha = 0.9) +
  geom_line(aes(color = inst_group)) +
  scale_color_manual(values = inst_group_colors) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.1,10.1)) + 
  scale_y_continuous(limits = c(0,0.81),
                     expand = c(0,0),
                     labels = percent_format()) + 
  facet_wrap(type ~ prestige_bin, scales = "free_x") +
  xlab("Years Since College Graduation") +
  ylab("Share Ever Returning") +
  labs(title = "Return to High School Geography, by Occupational Prestige") +
  # Adjust legend title width or text wrapping
  guides(color = guide_legend(title = "Institutional\nGroup"),
         shape = guide_legend(title = "Institutional\nGroup")) +  # Adjust other legends similarly if needed
  theme(strip.background = element_blank(), 
        strip.placement = "outside",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color = "black"),
        strip.text = element_text(margin = margin(b = 2, t = 2)),
        # Adding extra space between facets
        panel.spacing = unit(1, "lines"),
        # Adjust vertical space between facet rows
        panel.spacing.y = unit(0.1, "lines")
        )
ggsave(file = paste(directory,"/Outputs/",specification,"/ever_return_occp.png",sep = ""),
         width = wid, height = 2*hei, units = unit, bg = bkgrnd, dpi = dpi) 




## ----return by year-----------------------------------------------------------

return_by_year <- fread(file.path(data_dir,"intermediate/return_by_year.csv")) %>%
  table.express::filter(group %in% c("unc","30")) %>%
  table.express::mutate(level = ifelse(level == "hs","High School","College"),
                        geo = ifelse(geo == "cbsa","Labor Market","State"),
                        group = ifelse(group == "30","30+ Years","Any")
                        ) %>%
  # Calculate cumulative sum of 'shr'
  dplyr::group_by(level, geo, group) %>%
  dplyr::mutate(cum_shr = cumsum(shr)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(level == "High School")

# Plot returns by institutional grouping
ggplot(return_by_year, aes(x = year, y = cum_shr)) + 
  #geom_bar(stat = "identity", fill = "#0072B2") +
  geom_line(aes(color = group, linetype = group)) +
  geom_point(aes(color = group, shape = group)) +
  scale_color_manual(values = c("orange","#16D4BB")) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0, 20, by = 5), 
                     limits = c(-0.2,20.2)) + 
  scale_y_continuous(limits = c(0,1.01),
                     expand = c(0,0),
                     labels = percent_format()) + 
  facet_wrap(level ~ geo, scales = "free_x") +
  xlab("Years Since College Graduation") +
  ylab("Cumulative Share of Returners") +
  labs(title = "Return Migration, by Years Since College Graduation") +
  # Adjust legend title width or text wrapping
  guides(color = guide_legend(title = "Work\nExperience"),
         linetype = guide_legend(title = "Work\nExperience"),
         shape = guide_legend(title = "Work\nExperience")) +  # Adjust other legends similarly if needed
  theme(strip.background = element_blank(), 
        strip.placement = "outside",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color = "black"),
        strip.text = element_text(margin = margin(b = 2, t = 2)),
        # Adding extra space between facets
        panel.spacing = unit(1, "lines"),
        # Adjust vertical space between facet rows
        panel.spacing.y = unit(0.1, "lines")
        )
ggsave(file = paste(directory,"/Outputs/",specification,"/return_by_year.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi) 



## ----prime age----------------------------------------------------------------

net_cbsa <- read_csv(file.path(directory,"Data",specification,"net_cbsa.csv"),col_select = -`...1`) %>%
  mutate(cbsa_code = str_pad(cbsa,width=5,side="left",pad="0")) %>%
  #select(-c("origin","destination","total_destination","net")) %>%
  #pivot_wider(names_from = "inst_group",
  #            values_from = "net_norm") %>%
    left_join(cbsa_li_crosswalk %>% dplyr::select(c("cbsa_code","cbsa_core")) %>% distinct(), by = "cbsa_code") %>%
  group_by(cbsa_code,cbsa_core) %>%
  summarize(total_origin = first(total_origin),
            total_destination = first(total_destination)) %>%
  mutate(replacement_rate = total_destination/total_origin)

net_state <- read_csv(file.path(directory,"Data",specification,"net_state.csv"),col_select = -`...1`) %>%
  #select(-c("origin","destination","total_destination","net")) %>%
  filter(state != "empty") %>% 
  # Add state FIPS codes to simplify spatial merges later
  left_join(cbsa %>% 
              separate_wider_delim(`CBSA Title`,delim = ", ", names = c("cbsa_city","state_abbr")) %>%
              rename(state_code = `FIPS State Code`) %>%
              dplyr::select(state_code,state_abbr) %>%
              filter(nchar(state_abbr)==2) %>%
              distinct(), by = c("state"="state_abbr")) %>%
  # Coerce DC FIPS code
  mutate(state_code = ifelse(is.na(state_code)==TRUE,11,state_code)) %>%
  group_by(state,state_code) %>%
  summarize(total_origin = first(total_origin),
            total_destination = first(total_destination)) %>%
  mutate(replacement_rate = total_destination/total_origin)


# with columns 'replacement_rate' and 'total_origin'

eb_shrink_fixed <- function(data) {
  # Make a copy of the data to avoid modifying the original
  data_clean <- data
  
  # Identify problematic rows with zero total_origin or infinite replacement_rate
  problem_rows <- which(data_clean$total_origin == 0 | is.infinite(data_clean$replacement_rate))
  
  if(length(problem_rows) > 0) {
    # Temporarily remove problematic rows for calculation purposes
    temp_data <- data_clean[-problem_rows, ]
  } else {
    temp_data <- data_clean
  }
  
  # Extract the values from clean data
  rates <- temp_data$replacement_rate
  sizes <- temp_data$total_origin
  
  # Calculate the overall weighted mean
  weighted_mean <- sum(rates * sizes) / sum(sizes)
  
  # Estimate the prior variance using method of moments
  variance_obs <- sum(sizes * (rates - weighted_mean)^2) / sum(sizes)
  sampling_var <- 1 / sizes
  prior_var <- max(0.00001, variance_obs - mean(sampling_var))
  
  # Calculate shrinkage weights
  weights <- prior_var / (prior_var + sampling_var)
  
  # Calculate shrinkage estimates
  shrinkage_estimates <- weighted_mean + weights * (rates - weighted_mean)
  
  # Add results to clean data
  temp_data$shrinkage_weight <- weights
  temp_data$shrunk_rate <- shrinkage_estimates
  
  # Now handle the problematic rows
  if(length(problem_rows) > 0) {
    # For rows with zero total_origin, use the overall mean as the shrunk value
    data_clean$shrinkage_weight <- NA
    data_clean$shrunk_rate <- NA
    
    # Fill in the values for non-problematic rows
    data_clean$shrinkage_weight[-problem_rows] <- weights
    data_clean$shrunk_rate[-problem_rows] <- shrinkage_estimates
    
    # For problematic rows, use the overall mean
    data_clean$shrinkage_weight[problem_rows] <- 0  # Weight of 0 means "all prior"
    data_clean$shrunk_rate[problem_rows] <- weighted_mean
    
  } else {
    data_clean$shrinkage_weight <- weights
    data_clean$shrunk_rate <- shrinkage_estimates
  }
  
  # Print diagnostics
  cat("Diagnostics:\n")
  cat("Weighted mean:", weighted_mean, "\n")
  cat("Observed variance:", variance_obs, "\n")
  cat("Prior variance:", prior_var, "\n")
  cat("Range of weights:", range(weights), "\n")
  
  return(data_clean)
}

# Apply the updated function
net_cbsa_shrunk <- eb_shrink_fixed(net_cbsa)
net_state_shrunk <- eb_shrink_fixed(net_state)

net_cbsa_map = county_laea %>% 
  dplyr::left_join(unified_cbsa, by = c("GEOID"="GeoFIPS")) %>%
  group_by(cbsa_code) %>%
  dplyr::summarize(geometry = st_union(geometry)) %>%
  dplyr::left_join(net_cbsa_shrunk, by = "cbsa_code") %>%
  # Remove NAs since Dorling function doesn't accept NA weights
  filter(is.na(total_origin)==FALSE) %>%
  mutate(geo = "Labor Market") %>%
  dplyr::select(-c("cbsa_code","cbsa_core"))


net_state_map = county_laea %>% 
  dplyr::left_join(unified_cbsa, by = c("GEOID"="GeoFIPS")) %>%
  dplyr::mutate(state_code = substr(GEOID,start=1,stop=2)) %>%
  group_by(state_code) %>%
  dplyr::summarize(geometry = st_union(geometry)) %>%
  dplyr::left_join(net_state_shrunk, by = "state_code") %>%
  # Remove NAs since Dorling function doesn't accept NA weights
  filter(is.na(total_origin)==FALSE) %>%
  mutate(geo = "State") %>%
  dplyr::select(-c("state","state_code"))

#net_cbsa_state = rbind(net_state_map,net_cbsa_map) %>%
#  mutate(mode = "choropleth")

# We actually need duplicates of the maps because the Dorling transformation
# deletes the shapefile and replaces it with a circle
net_cbsa_state =  rbind(net_state_map,net_cbsa_map) %>%
  mutate(mode = "dorling") %>%
  rbind(rbind(net_state_map,net_cbsa_map) %>%
  mutate(mode = "choropleth"))

# Create dorling cartograms
# Coordinates created by code form circles rather than contiguous shapes
# Requires shapefile underneath
dorling_cbsa <- cartogram_dorling(x = net_cbsa_map,
                                    weight = "total_origin",
                                    k = 1, itermax = 100)
dorling_state <- cartogram_dorling(x = net_state_map,
                                    weight = "total_origin",
                                    k = 1, itermax = 100)

#net_cbsa_dorling = rbind(dorling_rpu,dorling_pf,dorling_private)
net_cbsa_state_dorling = rbind(dorling_cbsa,dorling_state) %>%
  mutate(mode = "dorling")

net_cbsa_state_full = rbind(net_cbsa_state_dorling,rbind(net_state_map,net_cbsa_map) %>%
  mutate(mode = "choropleth"))

ggplot() +
  geom_sf(data = net_cbsa_state, fill=NA, color="gray", size=0.1) +
  #geom_sf(data = net_cbsa_state, aes(fill=shrunk_rate),color="gray", size = 0.1) + 
  geom_sf(data = net_cbsa_state_full, aes(fill=shrunk_rate),color="gray", size = 0.1) + 
  scale_fill_gradientn(
    name = "Dest. Grads\nPer Orig.\nGrad  ",
    colors = diverging_colors,
    values = scales::rescale(c(0, 0.5, 1, 1.5, 2, 3.5)), # Define where colors are focused
    limits = c(0, 2.75),  # Set the visible range of the color scale
    oob = scales::squish  # Squish values outside the limits
  ) +
  labs(title = "Net Importers and Exporters of College Graduates") +
  facet_wrap(
    mode ~ geo,  # Include both inst_group and geo for mapping
    ncol = 2)+#,              # Two maps side by side per row
    #labeller = labeller(geo = element_blank(),
   #                     mode = element_blank())  # Remove labels for geo
  ##) + 
  theme_void() + 
  theme(text = element_text(size=12, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),               # Clean strip background
    panel.spacing = unit(1, "lines"),
        strip.text = element_blank(), 
    strip.text.y = element_blank()) +
  guides(
    fill = guide_colorbar(
      barwidth = 20,  # Adjust width of the color bar (in lines or points)
      barheight = 0.5 # Adjust height of the color bar (in lines or points)
    )
  )
ggsave(file = paste(directory,"/Outputs/",specification,"/net_cbsa_state.png",sep = ""),
         width = 1.6*wid, height = 2*hei, units = unit, bg = bkgrnd, dpi = dpi)




## ----return-levels------------------------------------------------------------
#return_levels_state <- read_csv(file.path(directory,"Data",specification,"return_levels_state.csv")) %>%
#  mutate(migration = case_when(ever_leave_state_hs_10 == 0 ~ "Never Left",
#                               ever_leave_state_hs_10 == 1 & same_state_hs == 0 ~ "Left and Never Returned",
#                               ever_leave_state_hs_10 == 1 & same_state_hs == 1 ~ "Left and Returned")) %>%
#  group_by(migration,inst_group) %>%
#  summarize(N = sum(N)) %>%
#  group_by(inst_group) %>%
#  mutate(shr_inst_group = N/sum(N),
#         geo = "State")
return_levels_cbsa <- read_csv(file.path(directory,"Data",specification,"return_levels_cbsa.csv")) %>%
  mutate(migration = case_when(ever_leave_cbsa_hs_10 == 0 ~ "Never Left",
                               ever_leave_cbsa_hs_10 == 1 & same_cbsa_hs_10 == 0 ~ "Left and Never Returned",
                               ever_leave_cbsa_hs_10 == 1 & same_cbsa_hs_10 == 1 ~ "Left and Returned")) %>%
  group_by(migration,inst_group) %>%
  summarize(N = sum(N)) %>%
  group_by(inst_group) %>%
  mutate(shr_inst_group = N/sum(N),
         geo = "Labor Market") %>%
  filter(!is.na(migration))

#return_levels <- rbind(return_levels_state,return_levels_cbsa)
return_levels <- return_levels_cbsa

# Compute return levels by institutional grouping
leaver_levels_ig <- return_levels %>%
  filter(migration != "Never Left") %>%
  group_by(geo,migration,inst_group) %>%
  summarize(N = sum(N)) %>%
  ungroup() %>%
  group_by(geo,inst_group) %>%
  mutate(shr = round((N/sum(N)) * 100,1)) %>%
  dplyr::select(-N) %>%
  pivot_wider(names_from = inst_group,
               values_from = shr) %>%
  filter(migration == "Left and Returned") %>%
  dplyr::select(-migration)

# Compute total return levels and merge institutional grouping results
raw_ll <- read_csv(file.path(directory,"Data",specification,"leaver_levels_cbsa.csv")) %>%
  filter(!is.na(same_cbsa_hs_10)) %>%
  mutate(outcome = case_when(same_cbsa_hs_10 == 0 & same_cbsa_col_10 == 0 ~ "No Return",
                             same_cbsa_hs_10 == 1 & same_cbsa_col_10 == 0 ~ "HS Return",
                             same_cbsa_hs_10 == 0 & same_cbsa_col_10 == 1 ~ "College Return",
                             same_cbsa_hs_10 == 1 & same_cbsa_col_10 == 1 ~ "HS Return"
                             )) %>%
  
  group_by(hs_col_diff_cbsa,outcome,inst_group) %>%
  summarize(N = sum(N))

# Compute rates of return by institutional group and universe
ll_i <- raw_ll %>%
  ungroup() %>%
  group_by(hs_col_diff_cbsa,inst_group) %>%
  summarize(inst_group_total = sum(N)) %>%
  group_by(hs_col_diff_cbsa) %>%
  mutate(universe_total = sum(inst_group_total)) %>%
  right_join(raw_ll, by = c("hs_col_diff_cbsa","inst_group")) %>%
  mutate(shr = N/inst_group_total) %>%
  dplyr::select(-c(inst_group_total,N)) %>%
  pivot_wider(names_from = inst_group,
               values_from = shr)

# Compute overall return by universe, to add a column to ll_i
leaver_levels = raw_ll %>%
  group_by(hs_col_diff_cbsa,outcome) %>%
  summarize(universe_portion = sum(N)) %>%
  right_join(ll_i, by = c("hs_col_diff_cbsa","outcome")) %>%
  mutate(Overall = universe_portion/universe_total) %>%
  filter(outcome != "No Return") %>%
  dplyr::select(-c(universe_portion,universe_total)) %>%
  mutate(Channel = case_when(hs_col_diff_cbsa == "Moved for College" & outcome == "College Return" ~ "Return to college labor market",
                             hs_col_diff_cbsa == "Moved for College" & outcome == "HS Return" ~ "Return to college labor market",
                             TRUE ~ "Return to high school and college labor market")) %>%
  ungroup() %>%
  dplyr::select(c(Channel,Overall,PF,`Private, Non-Profit`,RPU))

# Export table
ll <- xtable(leaver_levels,
       digits = c(0,0,2,2,2,2))
print(ll,file=file.path(directory,"Outputs",specification,"leaver_levels.tex"),include.rownames = FALSE)


migration_colors = c("Left and Never Returned" = '#d9f0d3',
                  "Left and Returned" = '#5aae61',
                  "Never Left" = '#e7d4e8')


# Create plot of both state and local labor market level returns 
ggplot(return_levels, aes(x = shr_inst_group, y = reorder(inst_group, shr_inst_group), fill = migration)) +
  geom_col(alpha = 0.8) +
  labs(x = "Percent of Graduates", y= NULL) +
  #facet_wrap(~ geo,ncol=1) +
  scale_fill_manual(values = migration_colors) +  # Apply custom colors
  scale_x_continuous(labels = percent_format(),
                     limits = c(0,1.01),
                     expand = c(0.001,0)) +
  labs(title = "Migration to High School Geography, by Institutional Group") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text         = element_text(size=10, family="LM Roman 10"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave(file = paste(directory,"/Outputs/",specification,"/return_levels.png",sep = ""),
         width = wid, height = 1.5*hei, units = unit, bg = bkgrnd, dpi = dpi)




## -----------------------------------------------------------------------------
# Create crosswalk between state codes and FIPS counterparts using built-in tidycensus feature
cw <- fips_codes %>% dplyr::select(state,state_code) %>% distinct() %>% as.data.frame()

# Import data and crosswalk state-level data for subsequent mergge
local_ties_cbsa <- read_csv(file.path(directory,"Data",specification,"local_ties_cbsa.csv"),col_select = -`...1`) %>%
  mutate(local_returners = ((return_col_only + return_hs_only + return_hs_col)/(return_col_only + return_hs_only + return_hs_col+no_ties)))
local_ties_state <- read_csv(file.path(directory,"Data",specification,"local_ties_state.csv"),col_select = -`...1`) %>%
  mutate(local_returners = ((return_col_only + return_hs_only + return_hs_col)/(return_col_only + return_hs_only + return_hs_col+no_ties))) %>%
  left_join(cw, by = c("state_mode"="state_code")) %>%
  rename(state_code = state_mode)

# Merge with shapefile
data(county_laea)
local_ties_cbsa_map = county_laea %>% 
  dplyr::left_join(unified_cbsa, by = c("GEOID"="GeoFIPS")) %>%
  group_by(cbsa_code) %>%
  dplyr::summarize(geometry = st_union(geometry)) %>%
  dplyr::left_join(local_ties_cbsa, by = "cbsa_code")
local_ties_state_map = county_laea %>% 
  dplyr::left_join(unified_cbsa, by = c("GEOID"="GeoFIPS")) %>%
  dplyr::mutate(state_code = substr(GEOID,start=1,stop=2)) %>%
  group_by(state_code) %>%
  dplyr::summarize(geometry = st_union(geometry)) %>%
  dplyr::left_join(local_ties_state, by = "state_code")

# Plot Map
ggplot(local_ties_cbsa_map, aes(fill = local_returners)) +
  geom_sf(show.legend = TRUE) +
  scale_fill_gradientn(name = "Returners\nAttending\nLocal\nHigh School\nor College",
                       labels = scales::percent,
                       colors = monotonic_colors,
                       #values = c(-1,-0.1,-0.05,-0.025,0,0.025,0.05,0.1,1),
                       #low = "#40004b",
                       #high = "#1b7837",
                       limits=c(0,1),
    ) +
  labs(title = "Where Returners Have Local Ties") +
  theme_void() + 
  theme(text = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5))
ggsave(file = paste(directory,"/Outputs/",specification,"/local_returners.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)




## ----heterogeneity------------------------------------------------------------

extremes <- read_csv(file.path(directory,"Data",specification,"bin_cbsa_5.csv"),col_select = -`...1`) %>%
  filter(hs_shock_7_bin %in% c(1,5) & shr_cbsa > 0.4) %>%
  mutate(cbsa_code = str_pad(hs_cbsa,width=5,side="left",pad="0")) %>%
  dplyr::select(cbsa_code,hs_shock_7_bin)

# Wrangle data to map the labor markets with the most distressed labor markets
data(county_laea)
extremes_map = county_laea %>% 
  dplyr::left_join(unified_cbsa, by = c("GEOID"="GeoFIPS")) %>%
  group_by(cbsa_code) %>%
  dplyr::summarize(geometry = st_union(geometry)) %>%
  dplyr::left_join(extremes %>%
                     mutate(bin_label = case_when(hs_shock_7_bin == 1 ~ "Most Distressed",
                               hs_shock_7_bin == 5 ~ "Least Distressed")) %>%
                     dplyr::select(-c(hs_shock_7_bin))
                     , by = "cbsa_code") %>%
  dplyr::mutate(bin_label = ifelse(is.na(bin_label),"In Between",bin_label))
# Plot labor market distress for appendix map
ggplot(extremes_map, aes(fill = bin_label)) +
  geom_sf(show.legend = TRUE) +
  scale_fill_manual(name = "Labor Market\nDistress",
                       values = c(
      "Most Distressed" = "#D87093",  # Soft magenta
      "Least Distressed" = "#66CDAA", # Light teal
      "In Between" = "#D3D3E3"    
    )
  ) + 
  labs(title = "Most and Least Distessed Labor Markets") +
  theme_void() + 
  theme(text = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5))
ggsave(file = paste(directory,"/Outputs/",specification,"/extremes_map.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)

# Collapse local ties data for plotting
local_ties_extreme <- local_ties_cbsa %>% 
  dplyr::select(-c(local_returners)) %>%
  left_join(extremes, by = "cbsa_code") %>%
  group_by(hs_shock_7_bin) %>%
  #filter(cbsa_code %in% distressed$hs_cbsa) %>%
  summarize(stay_hs_col = weighted.mean(stay_hs_col,w=total),
            stay_hs_only = weighted.mean(stay_hs_only,w=total),
            return_hs_col = weighted.mean(return_hs_col,w=total),
            return_hs_only = weighted.mean(return_hs_only,w=total),
            return_col_only = weighted.mean(return_col_only,w=total),
            no_ties = weighted.mean(no_ties,w=total)) %>%
  mutate(stayers = stay_hs_col + stay_hs_only,
         return_hs = return_hs_col + return_hs_only) %>%
  dplyr::select(-c(stay_hs_col,stay_hs_only,return_hs_col,return_hs_only)) %>%
  mutate(bin_label = case_when(hs_shock_7_bin == 1 ~ "Most Distressed",
                               hs_shock_7_bin == 5 ~ "Least Distressed",
                               is.na(hs_shock_7_bin) ~ "In Between"),
         bin_label = factor(bin_label,
                            ordered=TRUE)) %>%
  dplyr::select(-hs_shock_7_bin) %>%
  pivot_longer(!bin_label,
               names_to = "ties",
               values_to = "shr") %>%
  mutate(ties = case_when(ties == "no_ties" ~ "No Educational Ties",
                          ties == "return_hs" ~ "Left After HS, Returned",
                          ties == "return_col_only" ~ "Left After College, Returned",
                          ties == "stayers" ~ "Never Left"),
         ties = factor(ties,ordered=TRUE,levels=c("No Educational Ties","Left After HS, Returned","Left After College, Returned","Never Left"))
         )

migration_colors = c("Never Left" = '#e7d4e8',
                     "Left After HS, Returned" = '#1b7837',
                     "Left After College, Returned" = '#5aae61',
                     "No Educational Ties" = '#75DBDF') 

# Plot the distribution of local ties
ggplot(local_ties_extreme, aes(x = shr, y = reorder(bin_label, shr), fill = ties)) +
  geom_col(alpha = 0.8) +
  labs(x = "Percent of Resident College Graduates", y= NULL) +
  #facet_wrap(~ geo,ncol=1) +
  scale_fill_manual(values = migration_colors) +  # Apply custom colors
  scale_x_continuous(labels = percent_format(),
                     limits = c(0,1.01),
                     expand = c(0.001,0)) +
  labs(title = "Labor Market Distress and the Local Ties of Residents") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text         = element_text(size=10, family="LM Roman 10"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave(file = paste(directory,"/Outputs/",specification,"/local_ties.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)



## -----------------------------------------------------------------------------

# Pull degrees awarded data
# Only pull even years because reporting isn't mandatory in odd years
if(!exists("raw_instate"))
{
  raw_instate = get_education_data(level = "college-university",
                                   source = "ipeds",
                                   topic = "fall-enrollment",
                                   filters = list(year = c(2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020),
                                                  type_of_freshman = 99,
                                                  fips = c(1:56),
                                                  state_of_residence = c(1:58)),
                                 subtopic = list("residence")
                                   ) %>%
  filter(state_of_residence == fips | state_of_residence == 58)
}

#microdata[, instate := ifelse(col_state == hs_state, 1, 0)]
instate_li = read_csv(file.path(directory,"Data",specification,"instate_li.csv"),col_select = -`...1`)
  
# Calculcate in-state share, weighted by cohort size
instate = raw_instate %>%
  mutate(in_state = case_when(fips == state_of_residence ~ "instate",
                              TRUE ~ "total")) %>%
  dplyr::select(-c(state_of_residence,type_of_freshman)) %>%
  pivot_wider(names_from = "in_state", values_from = "enrollment_fall") %>%
  group_by(unitid) %>%
  summarize(instate = sum(instate),
            total_us = sum(total)) %>%
  mutate(shr_instate_ipeds = instate/total_us) %>%
  dplyr::select(-c(instate,total_us)) %>%
  left_join(instate_li, by = c("unitid"="col_unitid"))
#microdata_sample <- microdata[sample(.N, size = 0.01 * .N)]

# Define a function to calculate the mode, excluding NA values
#calculate_mode <- function(x) {
#  x <- na.omit(x)  # Remove NA values
#  if (length(x) == 0) return(NA)  # Return NA if all values were NA
#  ux <- unique(x)
#  ux[which.max(tabulate(match(x, ux)))]
#}

# Select columns that start with 'col_a'
#cbsa_cols <- microdata[, .SD, .SDcols = patterns("^cbsa_code_")]
#state_cols <- microdata[, .SD, .SDcols = patterns("^cbsa_state_")]

# Calculate the mode across these columns row-wise
#microdata[, mode_cbsa := apply(cbsa_cols, 1, calculate_mode)]
#microdata[, mode_state := apply(state_cols, 1, calculate_mode)]

# Count destinations of graduates by state and CBSA
destination_cbsa <- fread(file.path(directory,"Data",specification,"destination_cbsa.csv"),drop="V1")
destination_state <- fread(file.path(directory,"Data",specification,"destination_state.csv"),drop="V1")

#microdata[col_end >= 2010 & col_end <= 2018, .N, by = .(col_unitid, cbsa_mode)]
#destination_state <- microdata[col_end >= 2010 & col_end <= 2018, .N, by = .(col_unitid, state_mode)]

# Calculate total graduates per school so we can estimate shares
#destination_cbsa[, total_li_hs := sum(N), by = col_unitid]
#destination_state[, total_li_hs := sum(N), by = col_unitid]

# Calculate the share (proportion) of each geography,
# with each institution summing to 1
#destination_cbsa[, shr_cbsa := N / total_li_hs]
#destination_state[, shr_state := N / total_li_hs]

gc()
# Optionally, remove the `total` column if you no longer need it
#destination_cbsa[, total := NULL]
#destination_state[, total := NULL]



## -----------------------------------------------------------------------------
institutional_characteristics = read_csv(file.path(data_dir,"intermediate/institutional_characteristics.csv"))

# raw_deg_award is an RDS file (no .rds extension) written by new/00_alias_generation.R
raw_deg_award = readRDS(file.path(data_dir,"intermediate/raw_deg_award"))

# Calculate degrees awarded between 2010 and 2018
gotg_degree = raw_deg_award %>%
  filter(year <= 2018 & year >= 2010) %>%
  filter(cipcode == 99) %>%
  group_by(unitid) %>%
  summarize(total_deg_awarded = sum(awards)) %>%
  right_join(institutional_characteristics %>% dplyr::select(unitid,inst_group), by = "unitid")

# share_state includes unlocated graduates
# share_state_renorm excludes unlocated graduates
# share_adj realllocates CBSAs that cross state boundaries
gotg = fread(file.path(data_dir,"intermediate/CollegeMarketShares_state.csv"),drop="V1")

# Select the maximum value of share_state_adj by unitid, keeping geo_stabbr and share_state_adj
gotg_max <- gotg[gotg[, .I[which.max(share_state_renorm_adj)], by = unitid]$V1]


gotg_merged <- merge(gotg_max, destination_state, by.x = c("unitid", "geo_stabbr"), by.y = c("col_unitid","state_mode"), all.x = TRUE) %>%
  table.express::mutate(shr_li_hs = li_total_college / alum_uscovered,
                        shr_state = destination/li_total_college) %>%
  merge(gotg_degree, by = "unitid") %>%
  merge(instate, by = "unitid")

# Fit the linear model
dest_model <- lm(shr_state ~ share_state_adj, weight = total_deg_awarded, data = gotg_merged)
orig_model <- lm(shr_instate_li ~ shr_instate_ipeds, weight = total_deg_awarded, data = gotg_merged)

dest_model_sum <- summary(dest_model)
orig_model_sum <- summary(orig_model)

dest_r_sq <- round(dest_model_sum$r.squared, 3)
orig_r_sq <- round(orig_model_sum$r.squared, 3)

dest_coefficients <- dest_model_sum$coefficients
orig_coefficients <- orig_model_sum$coefficients

# Extract coefficients
dest_intercept <- round(dest_coefficients[1, 1], 3)
dest_slope <- round(dest_coefficients[2, 1], 3)
dest_std_err <- round(dest_coefficients[2, 2], 3)
orig_intercept <- round(orig_coefficients[1, 1], 3)
orig_slope <-  round(orig_coefficients[2, 1], 3)
orig_std_err <- round(orig_coefficients[2, 2], 3)

# Create the equation as a string
dest_equation <- paste0("Best Fit: y = ",dest_intercept, " + ",dest_slope, "x")
dest_r_squared <- paste0("R² = ", dest_r_sq)
dest_std_error <- paste0("Std. Error = ",dest_std_err)
orig_equation <- paste0("Best Fit: y = ",orig_intercept, " + ",orig_slope, "x")
orig_r_squared <- paste0("R² = ", orig_r_sq)
orig_std_error <- paste0("Std. Error = ",orig_std_err)


## -----------------------------------------------------------------------------

ggplot(gotg_merged, aes(x = share_state_adj, y = shr_state)) + 
  geom_point(aes(size = total_deg_awarded, color = inst_group, shape = inst_group),
             alpha = 0.4, show.legend = c(size = FALSE)) +
  scale_shape_manual(values = c(15,17,16,18)) +
  scale_color_manual(values = inst_group_colors) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +  # y = x line
  scale_x_continuous(expand = c(0.001,0),
                     limits = c(0,1.02),
                     labels = percent_format()) + 
  scale_y_continuous(limits = c(0,1.02),
                     expand = c(0,0),
                     labels = percent_format()) + 
  xlab("Share in Top Destination State (Analytical Sample)") +
  ylab("Grads on the Go Benchmark") +
  labs(title = "Share of Graduates in Top Destination State") +
  # Adjust legend title width or text wrapping
  guides(color = guide_legend(title = "Institutional\nGroup"),  # Split the title into two lines
         shape = guide_legend(title = "Institutional\nGroup")) +  # Adjust other legends similarly if needed
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color="black")) +
  # Add the best fit line equation, R^2, and standard error below the legend
  annotate("text", x = 0.005, y = 0.95, label = dest_equation, size = 4, color = "black", hjust = 0, vjust = 1, family = "LM Roman 10") +
  annotate("text", x = 0.005, y = 0.85, label = dest_r_squared, size = 4, color = "black", hjust = 0, vjust = 1, family = "LM Roman 10") +
  annotate("text", x = 0.005, y = 0.90, label = dest_std_error, size = 4, color = "black", hjust = 0, vjust = 1, family = "LM Roman 10")
  
  ggsave(file = paste(directory,"/Outputs/",specification,"/dest_benchmark.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)     


## -----------------------------------------------------------------------------

ggplot(gotg_merged, aes(x = shr_instate_li, y = shr_instate_ipeds)) + 
  geom_point(aes(size = total_deg_awarded, color = inst_group, shape = inst_group),
             alpha = 0.4, show.legend = c(size = FALSE)) +
  scale_shape_manual(values = c(15,17,16,18)) +
  scale_color_manual(values = inst_group_colors) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +  # y = x line
  scale_x_continuous(expand = c(0.001,0),
                     limits = c(-0.01,1.02),
                     labels = percent_format()) + 
  scale_y_continuous(limits = c(0,1.01),
                     expand = c(0,0),
                     labels = percent_format()) + 
  xlab("Share In-State (Analytical Sample)") +
  ylab("IPEDS Benchmark") +
  labs(title = "In-State Student Share") +
  # Adjust legend title width or text wrapping
  guides(color = guide_legend(title = "Institutional\nGroup"),  # Split the title into two lines
         shape = guide_legend(title = "Institutional\nGroup")) +  # Adjust other legends similarly if needed
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color="black"))+
  # Add the best fit line equation, R^2, and standard error below the legend
  annotate("text", x = 0.005, y = 0.95, label = orig_equation, size = 4, color = "black", hjust = 0, vjust = 1, family = "LM Roman 10") +
  annotate("text", x = 0.005, y = 0.85, label = orig_r_squared, size = 4, color = "black", hjust = 0, vjust = 1, family = "LM Roman 10") +
  annotate("text", x = 0.005, y = 0.90, label = orig_std_error, size = 4, color = "black", hjust = 0, vjust = 1, family = "LM Roman 10")
  
  ggsave(file = paste(directory,"/Outputs/",specification,"/orig_benchmark.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)     


## ----ic-----------------------------------------------------------------------

li_inst = read_csv(file.path(directory,"Data",specification,"li_inst.csv"))

##  Compute share of degrees awarded by institution
ipeds_inst = institutional_characteristics %>%
  filter(inst_group != "Private, For-Profit") %>%
  dplyr::select(total_deg_awarded,inst_name,inst_group,unitid) %>%
  mutate(ipeds_shr = total_deg_awarded/sum(total_deg_awarded,na.rm=TRUE))

inst <- merge(ipeds_inst,li_inst, by.x = "unitid", by.y = "col_unitid", all.x = TRUE)
# Fit the linear model
inst_model <- lm(ipeds_shr ~ li_shr, weight = total_deg_awarded, data = inst)
inst_model_sum <- summary(inst_model)
inst_r_sq <- round(inst_model_sum$r.squared, 3)
inst_coefficients <- inst_model_sum$coefficients

# Extract coefficients
inst_intercept <- round(inst_coefficients[1, 1], 3)
inst_slope <- round(inst_coefficients[2, 1], 3)
inst_std_err <- round(inst_coefficients[2, 2], 3)

# Create the equation as a string
inst_equation <- paste0("Best Fit: y = ",inst_intercept, " + ",inst_slope, "x")
inst_r_squared <- paste0("R² = ", inst_r_sq)
inst_std_error <- paste0("Std. Error = ",inst_std_err)

# Plot share of degrees awarded versus representation in LI sample
ggplot(inst, aes(x = li_shr, y = ipeds_shr)) + 
  geom_point(aes(size = total_deg_awarded, color = inst_group, shape = inst_group),
             alpha = 0.4, show.legend = c(size = FALSE)) +
  scale_shape_manual(values = c(15,17,16,18)) +
  scale_color_manual(values = inst_group_colors) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +  # y = x line
  scale_x_continuous(expand = c(0.001,0),
                     limits = c(-0.00008,0.0077),
                     labels = percent_format()) + 
  scale_y_continuous(limits = c(0,0.0075),
                     expand = c(0,0),
                     labels = percent_format()) + 
  xlab("Share of Degrees (Analytical Sample)") +
  ylab("IPEDS Benchmark") +
  labs(title = "Share of Bachelor's Degrees Awarded Nationwide Since 2000") +
  # Adjust legend title width or text wrapping
  guides(color = guide_legend(title = "Institutional\nGroup"),  # Split the title into two lines
         shape = guide_legend(title = "Institutional\nGroup")) +  # Adjust other legends similarly if needed
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color="black"))+
  # Add the best fit line equation, R^2, and standard error below the legend
  annotate("text", x = 0.000005, y = 0.0070, label = inst_equation, size = 4, color = "black", hjust = 0, vjust = 1, family = "LM Roman 10") +
  annotate("text", x = 0.000005, y = 0.00665, label = inst_r_squared, size = 4, color = "black", hjust = 0, vjust = 1, family = "LM Roman 10") +
  annotate("text", x = 0.000005, y = 0.0063, label = inst_std_error, size = 4, color = "black", hjust = 0, vjust = 1, family = "LM Roman 10")
  
  ggsave(file = paste(directory,"/Outputs/",specification,"/inst_distribution.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi) 


## ----shocks-------------------------------------------------------------------
# Select relevant data (least parsimonious model and standard shock)
coef_data <- fread(file.path(directory,"Data",specification,"coef_data_semi.csv")) %>%
  filter(model == 4,migration_end==10,dep_var=="same",hs_var=="hs_shock_1") %>%
  pivot_longer(c(hs_coef,col_coef), names_to = "coef_geo", values_to="coef") %>%
  pivot_longer(c(hs_conf_high,col_conf_high), names_to = "conf_high_geo", values_to="conf_high") %>%
  pivot_longer(c(hs_conf_low,col_conf_low), names_to = "conf_low_geo", values_to="conf_low") %>%
  # Filter out OLS estimates
  filter(grepl("shock",hs_var)) %>%
  filter((coef_geo=="hs_coef" & conf_high_geo=="hs_conf_high" & conf_low_geo=="hs_conf_low")|(coef_geo=="col_coef" & conf_high_geo=="col_conf_high" & conf_low_geo=="col_conf_low")) %>%

  mutate(shock = case_when(coef_geo == "hs_coef" & universe %in% c("leave_hs","leave_col") ~ "High School",
                           coef_geo == "col_coef" & universe %in% c("leave_hs","leave_col") ~ "College",
                           TRUE ~ "Both"),
         universe = factor(case_when(universe == "stay" ~ "High School + College LM",
                              universe == "leave_hs" ~ "High School Only LM",
                              universe == "leave_col" ~ "College Only LM"),
                           levels=c("High School + College LM","High School Only LM","College Only LM")
         )) %>%
    dplyr::select(-c(coef_geo,conf_high_geo,conf_low_geo,hs_var,col_var)) %>%
  distinct()

ggplot(coef_data, aes(y = universe, x = coef, color=shock,shape=shock)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 6) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high), position = position_dodge(width = 0.5), width = 0.2) +
  geom_line(position = position_dodge(width = 0.5)) +
  #facet_wrap(
  #  vars(universe),  # Include both inst_group and geo for mapping
  #  ncol = 1
  #) +
  labs(
    title = "How Do Labor Demand Shocks Affect Return to Different Geographies",
    x = "Effect of 1 Percent LD Shock on Percentage Point Change in Return to Geography",
    y = "Return Geography"
  ) +
  guides(color = guide_legend(title = "LD Shock\nLocation"),  # Split the title into two lines
        shape = guide_legend(title = "LD Shock\nLocation")) + 
  theme(#panel.background = element_rect(fill='transparent'),
        panel.background = element_blank(),
        #plot.background = element_rect(fill='transparent', color=NA),
        plot.background = element_blank(),
        text         = element_text(size=10, family="LM Roman 10",color="black"),
        axis.text.x = element_text(family="LM Roman 10",color="black"),
        axis.text.y = element_text(family="LM Roman 10",color="black"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.y = element_blank(),
        axis.line.y = element_line(color="black"),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA))
ggsave(filename = file.path(directory,"Outputs",specification,"ld_coef.png"), width = 8, height = 6)
  



## ----differences--------------------------------------------------------------

differences <- read_csv(file.path(directory,"Data",specification,"local_differences.csv")) %>%
  mutate(cbsa_code = str_pad(hs_cbsa,width=5,side="left",pad="0")) %>%
  dplyr::select(-c("hs_cbsa","...1")) %>%
  left_join(cbsa_li_crosswalk %>% dplyr::select(c("cbsa_code","cbsa_core")) %>% distinct(), by = "cbsa_code") %>%
  mutate(difference_cbsa_alt = shr_returners_cbsa - pred_shr_returners_cbsa)

# Import population weights
lt_cbsa <- read_csv(file.path(directory,"Data",specification,"local_ties_cbsa.csv"),col_select = -`...1`) %>%
  mutate(returners = (total *(return_col_only + return_hs_only + return_hs_col + no_ties))) %>%
  dplyr::select(c(cbsa_code,returners))

# Merge into shapefile
data(county_laea)
diff_map = county_laea %>% 
  dplyr::left_join(unified_cbsa, by = c("GEOID"="GeoFIPS")) %>%
  group_by(cbsa_code) %>%
  dplyr::summarize(geometry = st_union(geometry)) %>%
  dplyr::left_join(differences, by = "cbsa_code") %>%
  dplyr::left_join(lt_cbsa, by = "cbsa_code") %>%
  #dplyr::filter(!is.na(returners)) %>%
  dplyr::select(c(cbsa_code,geometry,difference_cbsa_alt,returners))

eb_shrink_fixed <- function(data) {
  # Make a copy of the data to avoid modifying the original
  data_clean <- data
  
  # Identify problematic rows with zero total_origin or infinite replacement_rate
  problem_rows <- which(data_clean$returners == 0 | is.infinite(data_clean$difference_cbsa_alt) | is.na(data_clean$difference_cbsa_alt) | is.na(data_clean$returners))
  
  if(length(problem_rows) > 0) {
    # Temporarily remove problematic rows for calculation purposes
    temp_data <- data_clean[-problem_rows, ]
  } else {
    temp_data <- data_clean
  }
  
  # Extract the values from clean data
  rates <- temp_data$difference_cbsa_alt
  sizes <- temp_data$returners
  
  # Calculate the overall weighted mean
  weighted_mean <- sum(rates * sizes) / sum(sizes)
  
  # Estimate the prior variance using method of moments
  variance_obs <- sum(sizes * (rates - weighted_mean)^2) / sum(sizes)
  sampling_var <- 1 / sizes
  prior_var <- max(0.00001, variance_obs - mean(sampling_var))
  
  # Calculate shrinkage weights
  weights <- prior_var / (prior_var + sampling_var)
  
  # Calculate shrinkage estimates
  shrinkage_estimates <- weighted_mean + weights * (rates - weighted_mean)
  
  # Add results to clean data
  temp_data$shrinkage_weight <- weights
  temp_data$shrunk_rate <- shrinkage_estimates
  
  # Now handle the problematic rows
  if(length(problem_rows) > 0) {
    # For rows with zero total_origin, use the overall mean as the shrunk value
    data_clean$shrinkage_weight <- NA
    data_clean$shrunk_rate <- NA
    
    # Fill in the values for non-problematic rows
    data_clean$shrinkage_weight[-problem_rows] <- weights
    data_clean$shrunk_rate[-problem_rows] <- shrinkage_estimates
    
    # For problematic rows, use the overall mean
    data_clean$shrinkage_weight[problem_rows] <- 0  # Weight of 0 means "all prior"
    data_clean$shrunk_rate[problem_rows] <- weighted_mean
    
  } else {
    data_clean$shrinkage_weight <- weights
    data_clean$shrunk_rate <- shrinkage_estimates
  }
  
  # Print diagnostics
  cat("Diagnostics:\n")
  cat("Weighted mean:", weighted_mean, "\n")
  cat("Observed variance:", variance_obs, "\n")
  cat("Prior variance:", prior_var, "\n")
  cat("Range of weights:", range(weights), "\n")
  
  return(data_clean)
}

# Apply the updated function
diff_map_shrunk <- eb_shrink_fixed(diff_map)




ggplot(diff_map_shrunk, aes(fill = shrunk_rate)) +
  geom_sf(show.legend = TRUE) +
  #facet_wrap(~ geo, ncol = 1) + # Facet for each level of inst_group
  scale_fill_gradientn(name = "Residual on\nLM Return",
                       labels = scales::percent,
                       colors = diverging_colors,
                       #oob = scales::squish,
                       #values = c(-1,-0.1,-0.05,-0.025,0,0.025,0.05,0.1,1),
                       #low = "#40004b",
                       #high = "#1b7837",
                       limits=c(-0.16,0.16),
    ) +
  labs(title = "Where College Graduates Return Despite Labor Demand?") +
  theme_void() + 
  theme(text = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5))
ggsave(file = paste0(directory,"/Outputs/",specification,"/amenities.png"),
                 width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi) 


## ----inst-group---------------------------------------------------------------

num_bins = c(25,10,5)
leaver_colors = c("Never Returned" = '#d9f0d3',
                  "Returned" = '#5aae61')

# Import occupational prestige crosswalk created in step 8
occp_prestige <- fread(file.path(data_dir,"intermediate/occupational_prestige.csv")) %>%
  as.data.frame() %>%
  distinct(socp_raw, .keep_all = TRUE) %>%
  dplyr::select(socp_raw,prestige_bin)

for(b in num_bins)
{
  for(h in geos) 
  {
    returner_data <- fread(file.path(directory,"Data",specification,paste0("returners_", h,"_",b,".csv"))) %>%
      table.express::filter(!is.na(hs_shock_7_bin)) %>%
      merge(occp_prestige, by.x = "soc_code_mode", by.y = "socp_raw") %>%
      table.express::mutate(prestige_bin = factor(prestige_bin, 
                                                  labels=c("High","Middle","Low"), 
                                                  ordered=TRUE))
    
    returner_inst <- returner_data[,.(returners = sum(returners),
                     total = sum(total)),
                  by = .(hs_shock_7_bin,inst_group)]
    # Calculate total returners within each hs_shock_7_bin
    returner_inst[, total_returners_bin := sum(returners), by = hs_shock_7_bin]
    # Calculate share of returners for each inst_group within hs_shock_7_bin
    returner_inst[, shr_returners := returners / total_returners_bin]
    
    returner_occp <- returner_data[,.(returners = sum(returners),
                     total = sum(total)),
                  by = .(hs_shock_7_bin,prestige_bin)]
    # Calculate total returners within each hs_shock_7_bin
    returner_occp[, total_returners_bin := sum(returners), by = hs_shock_7_bin]
    # Calculate share of returners for each inst_group within hs_shock_7_bin
    returner_occp[, shr_returners := returners / total_returners_bin]


    # Define reusable elements
    bar_theme <- theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      panel.background = element_rect(fill = 'transparent'),
      plot.background = element_rect(fill = 'transparent', color = NA),
      text = element_text(size = 10, family = "LM Roman 10"),
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = 'transparent', color = NA),
      legend.box.background = element_rect(fill = 'transparent', color = NA),
      legend.position = "bottom",
      panel.grid.major.y = element_line(color = "grey", linewidth = 0.25),
      panel.grid.major.x = element_blank(),
      axis.line.x = element_line(color = "black"))
    
    bar_guides <- guides(
      color = guide_legend(title = "Institutional\nGroup"),
      fill = guide_legend(title = "Institutional\nGroup"))
    
    ggplot(returner_inst, aes(x = hs_shock_7_bin, y = shr_returners, color = inst_group, 
                                 fill=inst_group)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = inst_group_colors) +
      scale_color_manual(values = inst_group_colors) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Returners by Institutional Group") +
      xlab("Quantile of HS Labor Demand Shock") +
      ylab("Share of Returners") +
      bar_theme +
      bar_guides
    ggsave(file = paste0(directory,"/Outputs/",specification,"/return_inst_group_",h,"_",b,".png"),
                 width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi) 
    
    ggplot(returner_occp, aes(x = hs_shock_7_bin, y = shr_returners, color = prestige_bin, 
                                 fill=prestige_bin)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = prestige_bin_colors) +
      scale_color_manual(values = prestige_bin_colors) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Returners by Occupational Prestige Bin") +
      xlab("Quantile of HS Labor Demand Shock") +
      ylab("Share of Returners") +
      bar_theme +
      bar_guides
    ggsave(file = paste0(directory,"/Outputs/",specification,"/return_occp_",h,"_",b,".png"),
                 width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi) 
    
    returner_data <- returner_data[,.(returners = sum(returners),
                    total = sum(total)), by = .(hs_shock_7_bin)]
    returner_data[,shr_returner := returners/total]
    returner_data[,shr_not_returner := (1-shr_returner)] 
    returner = returner_data %>%
      as_tibble() %>%
      pivot_longer(c(shr_returner,shr_not_returner),
                   names_to = "migration",
                   values_to = "shr") %>%
      mutate(returner_status = ifelse(migration=="shr_returner","Returned","Never Returned"))
    ggplot(returner, aes(x = hs_shock_7_bin, y = shr, color = returner_status, 
                                 fill=returner_status)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = leaver_colors) +
      scale_color_manual(values = leaver_colors) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Returners as Share of Leavers by HS Labor Demand Shock") +
      xlab("Quantile of HS Labor Demand Shock") +
      ylab("Share of Leavers") +
      bar_theme +
      bar_guides
    ggsave(file = paste0(directory,"/Outputs/",specification,"/return_",h,"_",b,".png"),
                 width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi) 
  }
}



## -----------------------------------------------------------------------------

# Step 1: Read the CSV file
raw_semi <- read_csv(file.path(directory,"Data",specification,"group_semi_elasticities.csv"))

pooled_instate = raw_instate %>%
  filter(state_of_residence == fips | state_of_residence == 58) %>%
  mutate(in_state = case_when(fips == state_of_residence ~ "instate",
                              TRUE ~ "total")) %>%
  dplyr::select(-c(state_of_residence,type_of_freshman)) %>%
  pivot_wider(names_from = "in_state", values_from = "enrollment_fall") %>%
  group_by(unitid) %>%
  summarize(instate = sum(instate),
            total_us = sum(total)) %>%
  mutate(shr_instate = instate/total_us) %>%
  left_join(read_csv(file.path(data_dir,"intermediate/institutional_characteristics.csv")) %>% dplyr::select(c(unitid,inst_group)), by = "unitid") %>%
  filter(!is.na(total_us) & !is.na(shr_instate)) %>%
  ungroup() %>%
  group_by(inst_group) %>%
  summarize(shr_instate = weighted.mean(shr_instate,w = total_us))

# Step 2: Filter the data
semi <- raw_semi %>%
  filter(geo == "cbsa", stage == "_shock", migration_end == 10, dep_var=="same") %>%
  mutate(hs_semi_elasticity_pct = hs_semi_elasticity/100,
         col_semi_elasticity_pct = col_semi_elasticity/100,
         semi_elasticity = case_when(universe == "leave_col" ~ col_semi_elasticity_pct,
                                     universe == "leave_hs" ~ hs_semi_elasticity_pct,
                                     universe == "stay" ~ hs_semi_elasticity_pct)) %>%
  mutate(universe = case_when(universe == "stay" ~ "Same LM for High School and College",
                              universe == "leave_hs" ~ "High School LM for Leavers",
                              universe == "leave_col" ~ "College LM for Leavers")) %>%
  
  dplyr::select(-c(col_semi_elasticity,geo,avg,norm,stage,hs_semi_elasticity,hs_semi_elasticity_pct,col_semi_elasticity_pct)) %>%
  distinct() %>%
  pivot_wider(values_from = semi_elasticity,
              names_from = in_state) %>%
  left_join(pooled_instate, by = "inst_group") %>%
  mutate(Pooled = (shr_instate * `In-State`) + (1 - shr_instate) * `Out-of-State`) %>%
  dplyr::select(-c(shr_instate,dep_var,migration_end)) %>%
  pivot_longer(c(`In-State`,`Out-of-State`,Pooled), names_to = "in_state", values_to = "semi_elasticity") %>%
  mutate(inst_group = factor(inst_group, ordered=TRUE, levels = c("Private, Non-Profit","RPU","PF"))) 

semielasticity_colors = c("Private, Non-Profit" = "#7CAE00",
                  "RPU" = "#00BFC4",
                  "PF" = "#F8766D")

# Step 4: Create the whisker plot
ggplot(semi, aes(y = as.factor(inst_group), x = semi_elasticity, color=inst_group,fill=inst_group,shape = in_state)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 5,
             alpha = 0.7) +
  facet_wrap(
    vars(universe),  # Include both inst_group and geo for mapping
    ncol = 1
  ) +
  #geom_errorbar(aes(xmin = hs_shock_semi - hs_shock_se, xmax = hs_shock_semi + hs_shock_se), # Replace with actual error bounds
  #              width = 0.2, position = position_dodge(width = 0.5)) +
  labs(x = "Percentage Point Change in Return Migration Probability",y=NULL) +
  labs(title = "Effect of One Percent Labor Demand Shock on Return Migration") +
  scale_color_manual(values = semielasticity_colors) +
  scale_fill_manual(values = semielasticity_colors) +
  scale_shape_manual(values = c("In-State" = "triangle down filled", "Out-of-State" = "diamond filled", "Pooled" = "circle filled")) +
  scale_x_continuous(labels = percent_format(),
                     limits = c(0,1.2*max(semi$semi_elasticity)),
                     expand = c(0.003,0),
                     #breaks= seq(0.01, 0.014, 0.002),
                     ) +
  guides(shape = guide_legend(title = "Prior Residency"),
         color = "none",
         fill="none") +  # Adjust other legends similarly if needed
  theme_minimal() +
  theme(#panel.background = element_rect(fill='transparent'),
        panel.background = element_blank(),
        #plot.background = element_rect(fill='transparent', color=NA),
        plot.background = element_blank(),
        text         = element_text(size=10, family="LM Roman 10",color="black"),
        axis.text.x = element_text(family="LM Roman 10",color="black"),
        axis.text.y = element_text(family="LM Roman 10",color="black"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.y = element_blank(),
        axis.line.y = element_line(color="black"),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA))#+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(file = paste(directory,"/Outputs/",specification,"/semielasticity.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)



## ----boe----------------------------------------------------------------------

boe <- return_levels %>%
  filter(geo == "Labor Market") %>%
  group_by(migration) %>%
  summarize(N = sum(N)) %>%
  mutate(shr = round((N/sum(N)) * 100,1),
         N_adj = case_when(migration == "Left and Returned" ~ (sum(N) * 0.01) + N,
                           TRUE ~ N),
         shr_adj = round((N_adj/sum(N_adj)) * 100,1),
         )

# Estimating how much the probability of return would need to increase to grow
# the entire college-educated population by 1 percent 
old_return_prob <- boe$shr[boe$migration == "Left and Returned"]/(boe$shr[boe$migration == "Left and Returned"]+boe$shr[boe$migration == "Left and Never Returned"])
new_return_prob <- boe$shr_adj[boe$migration == "Left and Returned"]/(boe$shr_adj[boe$migration == "Left and Returned"]+boe$shr_adj[boe$migration == "Left and Never Returned"])
prob_increase = new_return_prob - old_return_prob

# Dividing semi-elasticities by prob_increase will tell you what percent 
# the labor demand shock would need to be for each population
# Should probably also do one that pools each institutional group proportional
# to their share of the labor force 
envelope = semi
envelope$prob_increase = prob_increase
envelope$ld_shock_boe = envelope$prob_increase/envelope$semi_elasticity

# Figure out size of median local labor market
#raw_lm = fread(file.path(data_dir,"raw/cbp/cbp19co.txt"),
#              select = c('fipstate','fipscty','naics','emp'),
              # Ensure state and county codes are characters
#              colClasses=list(character=1:3))
# Figure out much taxpayers would save in incentives 
# Remove total and subtotal columns
raw_lm = raw_lm[grepl("-",naics)==FALSE]

# Remove all but three-digit NAICS
# Keeping other columns would result in double, triple, and quadruple counting
lm = raw_lm[grepl("///",naics)==TRUE]

lm[, fips_county := do.call(paste0, .SD), .SDcols = c("fipstate","fipscty")]
# Remove original geo-code data, which could be reconstructed from fips_county
lm = lm[,c("fipstate", "fipscty") := NULL]

# Merge with cbsa codes
lm = lm[unified_cbsa, on = c(fips_county = "GeoFIPS")]

# Summarize table by cbsa, to ease memory constraints
lm_cs = lm[,.(cbsa_total=sum(emp,na.rm=TRUE)),.(cbsa_code)]

# Compute mean size of labor market based on 2019 CBP data
envelope$ld_shock_size = (envelope$ld_shock_boe/100) * mean(lm_cs$cbsa_total)

# Use $196k figure for Bartik 2018 JEP
# https://pubs.aeaweb.org/doi/pdfplus/10.1257/jep.34.3.99 
envelope$ld_shock_cost = envelope$ld_shock_size * 0.196

# Plot cost of only RPU, only PF, only Private then pooled proportionally
# only in-state, out-of-state, then pooled proportionally
envelope_long = envelope %>%
  left_join(read_csv(file.path(data_dir,"intermediate/institutional_characteristics.csv")) %>% 
              group_by(inst_group) %>% 
              summarize(deg_awarded = sum(total_deg_awarded,na.rm=TRUE)), by ="inst_group") %>%
  left_join(pooled_instate, by = "inst_group")

envelope_not_pooled = envelope_long %>%
  filter(in_state != "Pooled") %>%
  mutate(adj_deg_awarded = case_when(in_state == "In-State" ~ deg_awarded * shr_instate,
                                     TRUE ~ deg_awarded * (1-shr_instate)
                                     )) %>%
  mutate(shr_deg_awarded = adj_deg_awarded/sum(adj_deg_awarded))
envelope_pooled = envelope_long %>%
  filter(in_state == "Pooled") %>%
  mutate(adj_deg_awarded = case_when(in_state == "In-State" ~ deg_awarded * shr_instate,
                                     TRUE ~ deg_awarded * (1-shr_instate)
                                     )) %>%
  mutate(shr_deg_awarded = adj_deg_awarded/sum(adj_deg_awarded)) %>%
  dplyr::select(c(inst_group,in_state,ld_shock_cost,universe))

national_cost <- sum(envelope_not_pooled$shr_deg_awarded * envelope_not_pooled$ld_shock_cost)
national_row <- c(inst_group = "National",in_state = "Pooled", ld_shock_cost = national_cost)

env = envelope_not_pooled %>%
  dplyr::select(c(inst_group,in_state,ld_shock_cost,universe)) %>%
  #rbind(national_row) %>%
  rbind(envelope_pooled)
  
ggplot(env, aes(y = as.factor(inst_group), x = ld_shock_cost, color=inst_group,fill=inst_group,shape = in_state)) +
  geom_vline(aes(xintercept = national_cost), 
             color = "gray30", 
             linetype = "dotted", 
             size = 0.5) +
  facet_wrap(
    vars(universe),  # Include both inst_group and geo for mapping
    ncol = 1
  ) +
  #annotate(
  #  "text",
  #  x = national_cost + 12,
  #  y = Inf,  # Place label at the top of the plot
  #  label = " National\nAverage",
  #  color = "black",
  #  vjust = 1.5,  # Slightly above the plot area
  #  size = 3.5,   # Adjust font size
  #  family = "LM Roman 10"  # Match font family
  #) +
  geom_point(position = position_dodge(width = 0.6),
             alpha = 0.7,
             size = 5) +
  #geom_errorbar(aes(xmin = hs_shock_semi - hs_shock_se, xmax = hs_shock_semi + hs_shock_se), # Replace with actual error bounds
  #              width = 0.2, position = position_dodge(width = 0.5)) +
  labs(x = "Incentive Cost of Labor Demand Shock in Millions",y=NULL) +
  labs(title = "Incentive Cost of One Percent Labor Demand Shock by In-State Status") +
  scale_color_manual(values = semielasticity_colors) +
  scale_fill_manual(values = semielasticity_colors) +
  scale_shape_manual(values = c("In-State" = "triangle down filled", "Out-of-State" = "diamond filled", "Pooled" = "circle filled")) +
  scale_x_continuous(labels = dollar_format(),
                     limits = c(0,1.2*max(env$ld_shock_cost)),
                     expand = c(0.001,0),
                     #breaks= seq(400, 600, 50),
                     ) +
  guides(shape = guide_legend(title = "Prior Residency"),
         color = "none",
         fill="none") +  # Adjust other legends similarly if needed
  theme_minimal() +
  
  theme(#panel.background = element_rect(fill='transparent'),
        panel.background = element_blank(),
        #plot.background = element_rect(fill='transparent', color=NA),
        plot.background = element_blank(),
        text         = element_text(size=10, family="LM Roman 10",color="black"),
        axis.text.x = element_text(family="LM Roman 10",color="black"),
        axis.text.y = element_text(family="LM Roman 10",color="black"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.y = element_blank(),
        axis.line.y = element_line(color="black"),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA))
ggsave(file = paste(directory,"/Outputs/",specification,"/semielasticity_cost.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)


## ----shock-levels-------------------------------------------------------------

# Distribution of shocks in the cbsa-level dataset
hs_shock_year_cbsa <- read_csv(file.path(directory,"Data",specification,"hs_shock_year_cbsa.csv"),col_select = -`...1`) %>%
  mutate(across(c(starts_with("p"),"sd"), ~ . * 100)) %>%
  filter(col_end != 2022) %>%
  mutate(reg_obs = case_when(col_end %in% c(2019:2021) ~ "Not in Sample",
                             TRUE ~ "In Sample")) %>%
  group_by(reg_obs) %>%
  mutate(shr_obs = case_when(reg_obs == "Not in Sample" ~ 0,
                             TRUE ~ round((count/sum(count))*100,2)))

hs_yr_cbsa <- xtable(hs_shock_year_cbsa,
       digits = c(0,0,2,2,2,2,2,2,2,2,2,0,2))
print(hs_yr_cbsa,file=file.path(directory,"Outputs",specification,"hs_shock_year_cbsa.tex"),include.rownames = FALSE)

# Distribution of shocks in the state-level dataset
hs_shock_year_state <- read_csv(file.path(directory,"Data",specification,"hs_shock_year_state.csv"),col_select = -`...1`) %>%
  mutate(across(c(starts_with("p"),"sd"), ~ . * 100)) %>%
  filter(col_end != 2022) %>%
  mutate(reg_obs = case_when(col_end %in% c(2019:2021) ~ "Not in Sample",
                             TRUE ~ "In Sample")) %>%
  group_by(reg_obs) %>%
  mutate(shr_obs = case_when(reg_obs == "Not in Sample" ~ 0,
                             TRUE ~ round((count/sum(count))*100,2)))

hs_yr_state <- xtable(hs_shock_year_state,
       digits = c(0,0,2,2,2,2,2,2,2,2,2,0,2))
print(hs_yr_state,file=file.path(directory,"Outputs",specification,"hs_shock_year_state.tex"),include.rownames = FALSE)



## ----bk impulse---------------------------------------------------------------

# How does a 1 percent negative demand shock nine years ago affect the no local ties share of the population
# Start by running regression on one-year shock, before moving to shock aggregates
# RHS: ten years of lagged shocks
# LHS: three separate regressions for the types of local tie shares
tie_stocks <- fread(file.path(directory,"Data",specification,"tie_stocks_alt.csv"))
cbsa_shock <- fread(file.path(data_dir,"intermediate/cbsa_shock.csv"))


# Merge on shock-level data
# Before merging, we need to create a dataset with lagged results dating back ten years for all four shock types
# That way, regression will be simple
shock_cols = grep("^shock",names(cbsa_shock),value=TRUE)
cbsa_cols = grep("^cbsa",names(cbsa_shock),value=TRUE)
unlag_cols <- c(shock_cols,"year",cbsa_cols)
unlagged <- cbsa_shock[, ..unlag_cols]
# Necessary for easy merges later
unlagged[,cbsa_code := as.integer(cbsa_code)]
# Order data for easy lagging
setorder(unlagged,cbsa_code,year)

# I don't need to manually lag shocks
# Just include the relevant ones in the VARX model
for(var in shock_cols)
{
  for(lag in 1:10)
  {
    unlagged[,paste0(var,"_lag_",lag) := data.table::shift(get(var), n = lag, type = "lag"), by = cbsa_code]
  }
}

start_year <- 2000
end_year <- 2017

lagged <- tie_stocks %>%
  table.express::mutate(flow_in = fifelse(!is.na(flow_in),flow_in,0),
                        flow_in_ties = fifelse(!is.na(flow_in_ties),flow_in_ties,0),
                        flow_out = fifelse(!is.na(flow_out),flow_out,0),
                        flow_out_ties = fifelse(!is.na(flow_out_ties),flow_out_ties,0)) %>%
  table.express::mutate(out_migration_rate = flow_out/N_total,
                        in_migration_rate = flow_in/N_total,
                        in_migration_ties_rate = fifelse(stock_local_ties==0,0,flow_in_ties/stock_local_ties),
                        out_migration_ties_rate = fifelse(stock_local_ties==0,0,flow_out_ties/stock_local_ties)) %>%
  merge(unlagged,by.x=c("year","cbsa"), by.y=c("year","cbsa_code")) %>%
  table.express::filter(year >= start_year & year <= end_year) %>%
  #table.express::mutate(local_ties = both + hs_only + col_only) %>%
  #table.express::mutate(no_local_ties = no_local_ties * cbsa_actual) %>%
  table.express::filter(!is.infinite(shock_avg_1)) %>%
  table.express::filter(!is.na(shock_avg_1))

vars <- lagged[, c("cbsa_actual","out_migration_rate","out_migration_ties_rate","in_migration_rate","in_migration_ties_rate","shock_avg_1")]
var_model <- VAR(vars,p=4,type="const")
# By setting impulse equal to "cbsa_actual", we measure how other endogenous variables
# change when labor demand experiences an exogenous shock of 1 percent
irf_results <- irf(var_model,impulse="shock_avg_1",
                   response=c("cbsa_actual","out_migration_rate","in_migration_rate","in_migration_ties_rate","out_migration_ties_rate"),
                   n.ahead = 8, 
                   boot=TRUE)

irf_df <- data.frame(irf_results$irf[["shock_avg_1"]]) 

# Get observed shares at t=0 (before the shock)
t0_values <-  lagged[year %in% c(start_year:end_year), 
                     .(cbsa_actual = mean(cbsa_actual, na.rm=TRUE),
                       out_migration_rate = weighted.mean(out_migration_rate, w=cbsa_actual, na.rm = TRUE),
                       in_migration_rate = weighted.mean(in_migration_rate, w=cbsa_actual, na.rm = TRUE),
                       in_migration_ties_rate = weighted.mean(in_migration_ties_rate, w=cbsa_actual, na.rm = TRUE),
                       out_migration_ties_rate = weighted.mean(out_migration_ties_rate, w=cbsa_actual, na.rm = TRUE))]

# Subtracting cumulative sum so that I get the effect of a negative labor demand shock
irf_shr <- irf_df %>%
  mutate(time = 0:(nrow(irf_df) - 1)) %>%
  #mutate(Population = cumsum(cbsa_actual)/t0_values$cbsa_actual,
  #       `Out-Migration` = t0_values$out_migration_rate - out_migration_rate,
  #       `In-Migration` = t0_values$in_migration_rate - in_migration_rate,
  #       `Net Migration` = `In-Migration` - `Out-Migration`,
  #       `In-Migration with Ties` = t0_values$in_migration_ties_rate - in_migration_ties_rate,
  #       `Out-Migration with Ties` = t0_values$out_migration_ties_rate - out_migration_ties_rate,
  #       `Net Migration with Ties` = `In-Migration with Ties` - `Out-Migration with Ties`) %>%
  mutate(`Out-Migration` = out_migration_rate,
         `In-Migration` = in_migration_rate,
         `Net Migration` = `In-Migration` - `Out-Migration`,
         `In-Migration with Ties` = in_migration_ties_rate,
         `Out-Migration with Ties` = out_migration_ties_rate,
         `Net Migration with Ties` = `In-Migration with Ties` - `Out-Migration with Ties`) %>%
  dplyr::select(-c(out_migration_rate,in_migration_rate,in_migration_ties_rate,out_migration_ties_rate)) %>%
  pivot_longer(cols = c(`Net Migration`,`Out-Migration`, `In-Migration`, `In-Migration with Ties`, `Out-Migration with Ties`, `Net Migration with Ties`),
               names_to = "variable", values_to = "value") 
  #filter(variable %in% c("In-Migration with Ties","In-Migration","Out-Migration","Out-Migration with Ties"))

# Normalizing effects for plot
#irf_shr <- irf_df %>%
#  mutate(time = 0:(nrow(irf_df) - 1)) %>%
#  mutate(Employment = 1-(cumsum(cbsa_actual)/t0_values$cbsa_actual),
#         `College Only` = (t0_values$col_only - cumsum(col_only))*Employment,
#         `No Local Ties` = (t0_values$no_local_ties - cumsum(no_local_ties))*Employment,
#         `HS Only` = (t0_values$hs_only - cumsum(hs_only))*Employment,
#         `Both` = (Employment - `No Local Ties`  - `HS Only`  - `College Only`)) %>%
#  dplyr::select(-c(col_only,no_local_ties,hs_only)) %>%
#  pivot_longer(cols = c(`College Only`, `No Local Ties`, `HS Only`,`Both`),
#               names_to = "Ties to Labor Market", values_to = "value") %>%
#  mutate(`Recoded Ties` = case_when(`Ties to Labor Market` == "College Only" ~ "Attached",
#                                    `Ties to Labor Market` == "HS Only" ~ "Attached",
#                                    `Ties to Labor Market` == "Both" ~ "Attached",
#                                    TRUE ~ "Unattached")) %>%
#  group_by(time,`Recoded Ties`) %>%
#  summarize(value = sum(value))

# Plot actual shares over time
ggplot(irf_shr, aes(x = time, y = value, color = variable, linetype=variable, alpha=variable)) +
  geom_smooth(method = "loess", 
              se = FALSE,
              span = 0.5,
              size = 0.8) +  # Use geom_area to stack the variables
  scale_alpha_manual(values = c("In-Migration"=0.3,
                                "In-Migration with Ties"=0.3,
                                "Out-Migration"=0.3,
                                "Out-Migration with Ties"=0.3,
                                "Net Migration"=1,
                                "Net Migration with Ties"= 1
                                )) +
  #scale_fill_manual(values = c(`College Only` = "#1f77b4", 
  #                             "shr_no_local_ties" = "#ff7f0e", 
  #                             "shr_hs_only" = "#2ca02c")) +  # Custom colors
  scale_x_continuous(expand = c(0.001,0),
                     limits = c(0,8)) + 
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0,0)) + 
  scale_linetype_manual(values = c("In-Migration"="longdash",
                                "In-Migration with Ties"="longdash",
                                "Out-Migration"="F1",
                                "Out-Migration with Ties"="F1",
                                "Net Migration"="solid",
                                "Net Migration with Ties"= "solid"
                                )) +
  scale_color_manual(values = c("In-Migration"="#2c531c",
                                "In-Migration with Ties"="#8B9E7F",
                                "Out-Migration"="#a11d1d",
                                "Out-Migration with Ties"="#AB6C5A",
                                "Net Migration"="#6A0DAD",
                                "Net Migration with Ties"= "#c396d9"
                                )) +
  coord_cartesian(ylim = c(-0.002, 0.002)) + 
  xlab("Years Since Negative Demand Shock") +
  ylab("Effect of Shock") +
  labs(title = "Components of Population Change After Labor Demand Shock") +
  # Adjust legend title width or text wrapping
  guides(color = guide_legend(title = "Component"),
         linetype = guide_legend(title = "Component"),
         alpha= "none") +  # Adjust other legends similarly if needed
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color="black"))
ggsave(file = paste(directory,"/Outputs/",specification,"/irf_shr_decomp.png",sep = ""),
       width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)  



## ----cohort-level-------------------------------------------------------------

hs_cbsa_10 <- read_csv(file.path(data_dir,"intermediate/cohort_return_hs_10.csv"))
col_cbsa_10 <- read_csv(file.path(data_dir,"intermediate/cohort_return_col_10.csv"))

return_levels = hs_cbsa_10 %>%
  mutate(geography = "High School") %>%
  rbind(col_cbsa_10 %>% mutate(geography = "College")) %>%
  rename(shr_return = V1)  %>%
  mutate(col_end = as.numeric(as.character(col_end)))


# Fit the linear model
hs_model <- lm(shr_return ~ col_end, data = return_levels %>% filter(geography == "High School" & col_end %in% cohorts[[specification]]))
col_model <- lm(shr_return ~ col_end, data = return_levels%>% filter(geography == "College" & col_end %in% cohorts[[specification]]))

hs_model_sum <- summary(hs_model)
col_model_sum <- summary(col_model)

hs_r_sq <- round(hs_model_sum$r.squared, 3)
col_r_sq <- round(col_model_sum$r.squared, 3)

hs_coefficients <- hs_model_sum$coefficients
col_coefficients <- col_model_sum$coefficients

# Extract coefficients
hs_intercept <- round(hs_coefficients[1, 1], 3)
hs_slope <- round(hs_coefficients[2, 1], 3)
hs_std_err <- round(hs_coefficients[2, 2], 3)
col_intercept <- round(col_coefficients[1, 1], 3)
col_slope <-  round(col_coefficients[2, 1], 3)
col_std_err <- round(col_coefficients[2, 2], 3)

# Create the equation as a string
hs_equation <- paste0("Best Fit: y = ",hs_intercept, " + ",hs_slope, "x")
hs_r_squared <- paste0("R² = ", hs_r_sq)
hs_std_error <- paste0("Std. Error = ",hs_std_err)
col_equation <- paste0("Best Fit: y = ",col_intercept, " + ",col_slope, "x")
col_r_squared <- paste0("R² = ", col_r_sq)
col_std_error <- paste0("Std. Error = ",col_std_err)

geo_colors = c("#E41A1C","#377EB8")

ggplot(return_levels, aes(x = col_end, y = shr_return)) + 
  geom_point(alpha = 0.4, show.legend = c(size = FALSE),
             aes(color = geography, shape = geography)) +
  geom_smooth(data = return_levels %>% filter(col_end %in% cohorts[[specification]]), method = "lm", aes(color = geography), line = "dashed") +
  scale_color_manual(values = geo_colors) +
  scale_x_continuous(expand = c(0.01,0)) + 
  scale_y_continuous(limits = c(0,0.32),
                     expand = c(0,0),
                     labels = percent_format()) + 
  xlab("Graduation Cohort") +
  ylab("Share Returning to Origin Labor Market") +
  labs(title = "Return Migration by Year of College Graduation") +
  # Adjust legend title width or text wrapping
  guides(color = guide_legend(title = "Geography"),  # Split the title into two lines
         shape = guide_legend(title = "Geography")) +  # Adjust other legends similarly if needed
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color="black")) + 
  annotate("text", x = 1984, y = 0.29, label = hs_r_squared, size = 4, color = "#377EB8", hjust = 0, vjust = 1, family = "LM Roman 10") +
  annotate("text", x = 1984, y = 0.25, label = col_r_squared, size = 4, color = "#E41A1C", hjust = 0, vjust = 1, family = "LM Roman 10")
ggsave(file = paste(directory,"/Outputs/",specification,"/cohort_levels.png",sep = ""),
       width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi) 



## ----intercensal, eval=FALSE--------------------------------------------------
# 
# # Obtain CBSA-level employment counts
# cbsa_shock <- read_csv(file.path(data_dir,"intermediate/cbsa_shock.csv")) %>%
#   dplyr::select(year,cbsa_code,cbsa_actual) %>%
#   filter(year %in% c(2000:2019))
# 
# # https://bookdown.org/dereksonderegger/444/api-data-queries.html
# # I inferred age groups based on the 2010-19 groups
# CensusFactorLevels <- function(name, vintage, variable){
#   file <- str_c('https://api.census.gov/data/',vintage,'/',name,
#                 '/variables/',variable,'.json')
#   print(file)
#   Meta <- jsonlite::read_json(file) %>%
#     .[['values']] %>% .[['item']] %>%
#     unlist() %>% tibble::enframe()
#   colnames(Meta) <- c(variable, str_c(variable,'_DESC'))
#   return(Meta)
# }
# p <- CensusFactorLevels('pep/charagegroups', 2018, 'AGEGROUP')
# 
# # Get county-level intercensal population estimates 2000-2009
# #old <- getCensus(
# #  name = "pep/int_charagegroups",
# #  vintage = 2000,
# #  vars = c("DATE_","STATE","COUNTY","AGEGROUP","POP"),
# #  region = "county:*",
# #  AGEGROUP = 6,
#   #region = "county:001",
# #  regionin = "state:26"
# #)
# # Removal decennial census estimates, which serve as baselines
# #filter(!DATE_ %in% c(1,12)) %>%
# #  mutate(year = as.numeric(DATE_) + 1998,
# #         GEOID = paste0(STATE,COUNTY)) %>%
# #  rename(value = POP) %>%
# #  dplyr::select(year,GEOID,value)
# 
# # Obtain county-level estimates of 25-64 population by year
# get_agegroup <- function(states, agegroups = 5:18) {
# #  get_agegroup <- function(states, agegroups = 28) {
# 
#   results <- list()  # Store results here
# 
#   for (state in states) {
#     for (age in agegroups) {
#       cat("Fetching data for state:", state, "age group:", age, "\n")
# 
#       state_code <- paste0("state:",state)
#       # API call for one state and one age group at a time
#       temp <- getCensus(
#         name = "pep/int_charagegroups",
#         key = Sys.getenv("census_key"),
#         vintage = 2000,
#         vars = c("DATE_","STATE","COUNTY","AGEGROUP","POP"),
#         region = "county:*",
#         AGEGROUP = age,
#         #region = "county:001",
#         regionin = state_code
#       )
# 
#       # Store the data if it's not NULL
#       if (!is.null(temp)) {
#         results <- append(results, list(temp))
#       }
# 
#       #Sys.sleep(1)  # Add delay to avoid API rate limits
#     }
#   }
# 
#   # Combine all results into a single data frame
#   final_data <- bind_rows(results)
#   return(final_data)
# }
# 
# 
# # Example usage: Fetch data for Michigan (26) and Ohio (39)
# fips <- str_pad(c(1,2,4:6,9:13,15:42,44:51,53:56),2,"left",pad="0")
# raw_age_00 <- get_agegroup(fips)
# 
# age_00 <- raw_age_00 %>%
#   # Removal decennial census estimates, which serve as baselines
#   filter(!DATE_ %in% c(1,12)) %>%
#   mutate(year = as.numeric(DATE_) + 1998,
#          GEOID = paste0(STATE,COUNTY)) %>%
#   group_by(year,GEOID) %>%
#   summarize(value = sum(POP, na.rm=TRUE)) %>%
#   dplyr::select(year,GEOID,value)
# 
# # Get county-level intercensal population estimates 2010-2019
# raw_age_10 <- get_estimates(
#   geography = "county",
#   product = "characteristics",
#   breakdown = "AGEGROUP",
#   vintage = 2019,
#   time_series = TRUE,
#   #state = "MI",
#   #county = "Kalamazoo",
#   output = "tidy")
# 
# # Clean 2010-19 intercensal estimates
# age_10 <- raw_age_10 %>%
#   filter(!DATE %in% c(1,12)) %>%
#   filter(AGEGROUP %in% 5:18) %>%
#   mutate(year = 2008 + DATE) %>%
#   group_by(year,GEOID) %>%
#   summarize(value = sum(value, na.rm=TRUE)) %>%
#   dplyr::select(year,GEOID,value)
# 
# raw_laus <- read_csv(file.path(data_dir,"raw/bls/bls_laus_data.csv"))
# # Run first four chunks of 06_regressions.Rmd to get the regression object
# 
# laus = raw_laus %>%
#   mutate(across(starts_with("Annual "),as.double)) %>%
#   filter(Dataset == "6") %>%
#   pivot_longer(cols = starts_with("Annual"),
#                names_prefix = "Annual ",
#                names_to = "year",
#                values_to = "labor_force") %>%
#   mutate(GeoFIPS = str_pad(`s ID`,side="left",pad="0",width=5)) %>%
#   dplyr::select("GeoFIPS","year","labor_force") %>%
#   left_join(unified_cbsa,by="GeoFIPS") %>%
#   mutate(year = as.integer(year)) %>%
#   group_by(cbsa_code,year) %>%
#   summarize(labor_force = sum(labor_force,na.rm=TRUE)) %>%
#   filter(year != 2024)
# 
# # Combine intercensal estimates together, creating CBSA x year panel
# intercensal <- rbind(age_00,age_10) %>%
#   left_join(unified_cbsa, by=c("GEOID"="GeoFIPS")) %>%
#   group_by(cbsa_code,year) %>%
#   filter(year %in% c(2000:2017)) %>%
#   summarize(pop = sum(value,na.rm=TRUE)) %>%
#   left_join(cbsa_shock, by = c("cbsa_code","year")) %>%
#   filter(!is.na(cbsa_actual)) %>%
#   left_join(laus, by = c("cbsa_code","year")) %>%
#   mutate(emp_rate = labor_force / pop)
# 
# write.csv(intercensal,file.path(data_dir,"intermediate/intercensal.csv"),row.names = FALSE)
# 
# 


## ----yagan--------------------------------------------------------------------
intercensal <- read_csv(file.path(data_dir,"intermediate/intercensal.csv"))


yagan <- function(start,end)
{
  # Find the people who moved between 2006 and 2011
  yagan_sample = regression[col_end %in% c(1982:start)]
  yagan_sample[,yrs_grad_start := start - as.numeric(as.character(col_end))]
  yagan_sample[,yrs_grad_end := end - as.numeric(as.character(col_end))]
  # Compute the actual year
  
  ys <- melt(yagan_sample, 
             id.vars = c("user_id","col_end","hs_cbsa_code","col_cbsa_code","yrs_grad_start","yrs_grad_end"), 
             measure.vars = patterns("^cbsa_code_"), 
             variable.name = "years_since_grad", 
             value.name = "cbsa") %>%
    table.express::filter(!is.na(cbsa))
  
  # Extract the number of years since graduation
  ys[, years_since_grad := as.integer(gsub("\\D", "", years_since_grad))]
  ys[, year := as.numeric(as.character(col_end)) + years_since_grad]
  #ys[, cbsa := as.integer(cbsa)]
  ys_f <- ys[years_since_grad == yrs_grad_start | years_since_grad == yrs_grad_end]
  
  # Pivot wider using dcast()
  ys_ff <- dcast(ys_f, user_id + col_end + hs_cbsa_code + col_cbsa_code + yrs_grad_start + yrs_grad_end ~ year, 
                 value.var = "cbsa", 
                 fun.aggregate = identity,
                 fill = NA)# %>%
    #table.express::filter(!is.na({{start}})) %>%
    #table.express::filter(!is.na({{end}})) %>%
    #table.express::mutate(migrant = fifelse({{start}} != {{end}},1,0)) %>%
    #table.express::filter(migrant == 1)
  cbsa_start <- paste0("cbsa_",str_sub(start,3,4))
  cbsa_end <- paste0("cbsa_",str_sub(end,3,4))
  
  # Rename the columns
  setnames(ys_ff, c(as.character(start), as.character(end)), 
           c(cbsa_start,cbsa_end))
  ys_ff <- ys_ff[!is.na(get(cbsa_start))]
  ys_ff <- ys_ff[!is.na(get(cbsa_end))]
  ys_ff <- ys_ff[get(cbsa_start) != get(cbsa_end)]
  

  
  ic <- intercensal %>%
    filter(year %in% c(start,end)) %>%
    dplyr::select(cbsa_code,emp_rate,year) %>%
    pivot_wider(names_from = "year",
                values_from = "emp_rate",
                names_prefix = "emp_rate_") %>%
    # Bins will be weighted by 2006 population
    right_join(intercensal %>% 
                 filter(year == start) %>% 
                 dplyr::select(cbsa_code,pop), by = "cbsa_code") %>%
    #ungroup(cbsa_code) %>%
    # Bin local labor markets by their change in employment rate between 2006 and 2011
    # Define column name dynamically
    mutate(emp_rate_end = .data[[paste0("emp_rate_", end)]])
  
  quantiles <- sapply(seq(0, 1, length.out = 16), function(p) {
    reldist::wtd.quantile(ic$emp_rate_end, q = p, weight = ic$pop, na.rm = TRUE)
  })
  
  ic = ic %>%
    # Bin local labor markets based on employment rate change
    mutate(bin = cut(emp_rate_end,
                     breaks = quantiles,
                     include.lowest = TRUE,
                     labels = FALSE)) %>%
    # Select final columns
    dplyr::select(cbsa_code, bin, pop, emp_rate_end)
  
  ic_means <- ic %>%
    group_by(bin) %>%
    summarize(bin_mean = weighted.mean(emp_rate_end,w=pop)) %>%
    right_join(ic,by="bin") %>%
    mutate(cbsa_code = as.integer(cbsa_code))
  
  ys_fff <- ys_ff %>%
    as.data.frame() %>%
    left_join(ic_means, by = setNames("cbsa_code", cbsa_start)) %>%
    rename(
      orig_emp_rate = emp_rate_end,
      orig_bin = bin,
      orig_bin_mean = bin_mean
    ) %>%
    # Dynamic left join using end year
    left_join(ic_means, by = setNames("cbsa_code", cbsa_end)) %>%
    rename(
      dest_emp_rate = emp_rate_end,
      dest_bin = bin,
      dest_bin_mean = bin_mean
    ) %>%
    dplyr::select(-starts_with("pop"))  # Drop all population columns
  
  # Measure directedness for all migrants
  overall <- ys_fff %>%
    filter(!is.na(orig_bin)) %>%
    group_by(orig_bin, orig_bin_mean) %>%
    summarize(
      dest_bin_mean = mean(dest_emp_rate, na.rm = TRUE),
      n = n()
    ) %>%
    mutate(type = "Overall")
  
  # Split data dynamically based on migration categories
  split <- ys_fff %>%
    mutate(type = case_when(
      .data[[cbsa_end]] == hs_cbsa_code & .data[[cbsa_end]] == col_cbsa_code ~ "Both",
      .data[[cbsa_end]] != hs_cbsa_code & .data[[cbsa_end]] == col_cbsa_code ~ "College",
      .data[[cbsa_end]] == hs_cbsa_code & .data[[cbsa_end]] != col_cbsa_code ~ "HS",
      TRUE ~ "None"
    )) %>%
    filter(!is.na(orig_bin)) %>%
    group_by(orig_bin, orig_bin_mean, type) %>%
    summarize(
      dest_bin_mean = mean(dest_emp_rate, na.rm = TRUE),
      n = n()
    ) %>%
    bind_rows(overall) 
  
  write.csv(split,file.path(directory,"Data",specification,paste0("directed_",start,"_",end,".csv")),row.names = FALSE)
  
  return(split)
  
}

for(s in 0:12)
{
  start = 2000+s
  end = start+5
  split <- yagan(start,end)
  print(paste0("Completed: ",start,"-",end))
}


for(s in 0:12)
{
  start = 2000+s
  end = start+5
  split <- read_csv(file.path(directory,"Data",specification,paste0("directed_",start,"_",end,".csv")))
  
  # Fit separate models for each type
  split_models <- split %>%
    filter(type != "None") %>%
    group_by(type) %>%
    nest() %>%
    mutate(model = map(data, ~ lm(dest_bin_mean ~ orig_bin_mean, data = .x)),
           summary = map(model, summary),
           r_sq = map_dbl(summary, ~ .x$r.squared),
           coefficients = map(summary, ~ .x$coefficients),
           intercept = map_dbl(coefficients, ~ .[1, 1]),
           slope = map_dbl(coefficients, ~ .[2, 1]),
           std_err = map_dbl(coefficients, ~ .[2, 2])) %>%
    dplyr::select(type, intercept, slope, r_sq, std_err) %>%
    mutate(placement = ((max(split$orig_bin_mean) - 0.1)*slope)+intercept)
  
  ggplot(split %>% filter(type != "None"), aes(x = orig_bin_mean, y = dest_bin_mean)) + 
    geom_jitter(aes(color = type, shape = type), 
                alpha = 0.4, 
                width = 0.003, height = 0.003,
                show.legend = c(size = FALSE)) +
    #scale_shape_manual(values = c(15,17,16,18)) +
    #scale_color_manual(values = inst_group_colors) +
    # Add group-specific best-fit lines (thinner)
    geom_smooth(aes(color = type), method = "lm", se = FALSE, linewidth = 0.7) +
    # Add overall best-fit line (thickest)
    scale_x_continuous(expand = c(0.005,0),
                       #limits = c(0.55,0.75),
                       labels = percent_format()) + 
    scale_y_continuous(#limits = c(0.55,0.75),
                       expand = c(0.001,0),
                       labels = percent_format()) + 
    geom_text(data = split_models, 
              aes(x = max(split$orig_bin_mean) - 0.03, y = mean(split_models$placement) - 0.004 * as.numeric(factor(type)), 
                  label = paste0(round(slope, 3), 
                                 " (", round(std_err, 3),")"), 
                  color = type), hjust = 0,family="LM Roman 10") +
    xlab(paste(end,"Employment Rate in",start,"Labor Market")) +
    ylab(paste(end,"Employment Rate in",end,"Labor Market")) +
    labs(title = paste("Lack of Directedness:",start,"-",end,"Migrants")) +
    guides(color = guide_legend(title = "Type", override.aes = list(label = "")),  
           shape = guide_legend(title = "Type", override.aes = list(label = ""))) +
    # Adjust legend title width or text wrapping
    theme(panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA),
          text         = element_text(size=10, family="LM Roman 10"),
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill='transparent',color=NA),
          legend.box.background = element_rect(fill='transparent',color=NA),
          legend.position = "bottom",
          panel.grid.major.y = element_line(color = "grey",
                                            linewidth = 0.25),
          panel.grid.major.x = element_blank(),
          axis.line.x = element_line(color="black"))
  ggsave(file = paste(directory,"/Outputs/",specification,paste0("/directedness_",start,"_",end,".png"),sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)     
}


## ----kappa plot---------------------------------------------------------------

universe_colors <- c("#17BECF", "#FF7F0E", "#9467BD") 

complier_plot <- read_csv(file.path(directory,"Data",specification,"compliers.csv")) %>% 
  as_tibble() %>%
  dplyr::mutate(characteristic = case_when(preferred_value == "In-State" ~ "Attended In-State College",
                                           preferred_value == "RPU" ~ "Regional Public University",
                                           preferred_value == "PF" ~ "Public Flagship",
                                           preferred_value == "Low" ~ "Low-Prestige Occp.",
                                           preferred_value == "Middle" ~ "Medium-Prestige Occp.",
                                           preferred_value == "Traditional" ~ "Completed Degree by 24",
                                           preferred_value == "Transfer" ~ "Transferred Between Colleges",
                                           TRUE ~ preferred_value)) %>%
  dplyr::mutate(coefficient_lower = coefficient - std_error,
         coefficient_upper = coefficient + std_error,
         Characteristic = factor(characteristic,
                                 levels=c("High-Amenity LM","Low-Amenity LM",
                                          "Bottom LD Quartile","Middle LD Quartiles","Top LD Quartile",
                                          "Return in 5-10 Years","Return Within 5 Years",
                                          "Low-Prestige Occp.","Medium-Prestige Occp.",
                                          "Transferred Between Colleges","Completed Degree by 24",
                                          "Private, In-State","RPU, In-State","PF, In-State",
                                          "Regional Public University","Public Flagship","Attended In-State College"
                                          ),
                                 ordered=TRUE)) %>%
  #dplyr::select(c(characteristic,preferred_value,coefficient_lower,coefficient,coefficient_upper,sample,all_migrants,Characteristic)) %>%
  pivot_longer(c(coefficient,sample,all_migrants), names_to = "universe", values_to = "shr") %>%
  dplyr::mutate(universe = case_when(universe == "coefficient" ~ "Compliers",
                                     universe == "all_migrants" ~ "Leavers + Stayers",
                                     universe == "sample" ~ "Leavers"))

ggplot(complier_plot, aes(y = as.factor(Characteristic), x = shr, color=universe,fill=universe,shape = universe)) +
  geom_point(position = position_dodge(width = 0.4),
             alpha = 0.7,
             size = 3) +
  geom_errorbar(data = complier_plot %>% filter(universe == "Compliers"), 
                aes(xmin = coefficient_lower, xmax = coefficient_upper), # Replace with actual error bounds
                width = 0.4, position = position_dodge(width = 0.5)) +
  labs(x = "Share of Population",y=NULL) +
  labs(title = "Characteristics of Compliers") +
  scale_color_manual(values = universe_colors) +
  scale_fill_manual(values = universe_colors) +
  #scale_shape_manual(values = c("In-State" = "triangle down filled", "Out-of-State" = "diamond filled", "Pooled" = "circle filled")) +
  scale_x_continuous(labels = percent_format(),
                     #limits = c(0,0.8),
                     expand = c(0.01,0),
                     #breaks= seq(400, 600, 50),
                     ) +
  guides(shape = guide_legend(title = "Universe"),
         color = guide_legend(title = "Universe"),
         fill = guide_legend(title = "Universe")) +  # Adjust other legends similarly if needed
  theme_minimal() +
  
  theme(#panel.background = element_rect(fill='transparent'),
        panel.background = element_blank(),
        #plot.background = element_rect(fill='transparent', color=NA),
        plot.background = element_blank(),
        text         = element_text(size=10, family="LM Roman 10",color="black"),
        axis.text.x = element_text(family="LM Roman 10",color="black"),
        axis.text.y = element_text(family="LM Roman 10",color="black"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey",
                                          linewidth = 0.25),
        panel.grid.major.y = element_blank(),
        axis.line.y = element_line(color="black"),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA))
ggsave(file = paste(directory,"/Outputs/",specification,"/complier_chars.png",sep = ""),
         width = wid, height = 1.5*hei, units = unit, bg = bkgrnd, dpi = dpi)



## ----pf, eval=FALSE-----------------------------------------------------------
# 
# pf_table = pf %>%
#   dplyr::select(c("institution","state")) %>%
#   rename(Institution = institution,
#          State = state)
# stargazer(pf_table[1:96,],
#           summary = FALSE,
#           rownames = FALSE,
#           digits = 1,
#           #style = "qje",
#           align = FALSE,
#           title = "Public Flagship Universities",
#           float = TRUE,
#           out = file.path(directory,"Outputs",specification,"pf.tex"),
#           #label = "table:observation",
#           digit.separator = ","
#           )
# 


## ----eval=FALSE---------------------------------------------------------------
# 
# h="cbsa"
# b="5"
# 
# returner_data <- fread(file.path(directory,"Data",specification,paste0("returners_", h,"_",b,".csv"))) %>%
#       table.express::filter(!is.na(hs_shock_7_bin)) %>%
#       merge(occp_prestige, by.x = "soc_code_mode", by.y = "socp_raw") %>%
#       table.express::mutate(prestige_bin = factor(prestige_bin,
#                                                   labels=c("High","Middle","Low"),
#                                                   ordered=TRUE))
# 
# oi <- returner_data[,.(returners = sum(returners),
#                         total = sum(total)),
#                      by = .(hs_shock_7_bin,inst_group,prestige_bin)]
# oi[,shr_return := returners/sum(returners)]
# oi[,shr_total:= total/sum(total)]
# oi[,overrep := shr_return/shr_total]


## ----age col grad, include=FALSE----------------------------------------------

state_list = unique(fips_codes$state)[1:51]

# Pull 2023 1-year ACS microdata 
#col_grad_micro = get_pums(variables = c("ESR","AGEP","SCHL","OCCP"),
#                          state = state_list,
#                          survey = "acs1",
#                          year = 2023)

# Filter ACS data to only include college graduates who work
col_grad_filt = col_grad_micro %>%
  mutate(esr = as.integer(ESR)) %>%
  filter(esr %in% c(1,2,3)) %>%
  filter(PWGTP < 10000) %>%
  # Filter to college graduates
  filter(SCHL > 20) %>%
  # Filter to people in labor force
  filter(ESR %nin% c(0,6)) %>%
  mutate(age = as.numeric(AGEP),
         cohort = 2022 - age) %>%
  dplyr::select(-c(AGEP,age)) %>%
  filter(cohort > 1929 & cohort < 2002)

# Calculate total number of college graduates in labor market by age
denominator_cohort = col_grad_filt %>%
  group_by(cohort) %>%
  summarize(count = sum(PWGTP, na.rm = TRUE))

# Calculate share of college-educated in LI sample, by year
#fraction_cohort = regression %>%
#  as.data.frame() %>%
#  group_by(birth) %>%
 # summarize(li_total = n()) %>%
#  
fraction_cohort <- read_csv(file.path(directory,"Data",specification,"fraction_cohort.csv")) %>%
  left_join(denominator_cohort, by = c("birth"="cohort")) %>%
  mutate(li_prop = li_total/count,
         li_cum  = li_total/sum(li_total))

#li_deg_awarded = regression %>%
#  as.data.frame() %>%
#  group_by(col_end) %>%
#  summarize(li_deg_awarded = n()) %>%
#  mutate(year = as.numeric(as.character(col_end))) %>%
#  select(-col_end)
li_deg_awarded <- read_csv(file.path(directory,"Data",specification,"li_deg_awarded.csv"))


# Import Table 318.10 from 2022 Digest of Education Statistics 
# https://nces.ed.gov/programs/digest/d22/tables/dt22_318.10.asp
degrees_awardedfilename = "tabn318.10.xlsx"
deg_awarded = read_excel(file.path(data_dir,"raw/nces/tabn318.10.xlsx")) %>%
  dplyr::select(c("Table 318.10. Degrees conferred by postsecondary institutions, by level of degree and sex of student: Selected academic years, 1869-70 through 2031-32","...6")) %>%
  rename(c("academic_year" = "Table 318.10. Degrees conferred by postsecondary institutions, by level of degree and sex of student: Selected academic years, 1869-70 through 2031-32",
           "deg_awarded" = "...6")) %>%
  separate(academic_year,sep ="-",c("first_year","second_year")) %>%
  filter(is.na(second_year)==FALSE) %>%
  mutate(year = as.numeric(first_year)+1,
         deg_awarded = as.numeric(deg_awarded)) %>%
  dplyr::select(-c(first_year,second_year)) %>%
  filter(year > 1981 & year < 2022) %>% 
  left_join(li_deg_awarded, by = "year") %>%
  mutate(li_deg_prop = li_deg_awarded/deg_awarded)

birth_deg_cohort = fraction_cohort %>%
  full_join(deg_awarded, by = c("birth"="year")) %>%
  dplyr::select(c(birth,li_prop,li_deg_prop)) %>%
  rename(year = birth)

# Calculations required to draw explanatory lines
birth_start_year = c(1985)
intersection_birth <- approx(x = birth_deg_cohort$year, 
                             y = birth_deg_cohort$li_prop, 
                             xout = birth_start_year)$y
intersection_grad <- approx(x = birth_deg_cohort$li_deg_prop, 
                             y = birth_deg_cohort$year, 
                             xout = intersection_birth)$y
birth_to_grad = round(intersection_grad - birth_start_year,1)
birth_to_grad_label = paste(birth_to_grad,"Years to BA")

# Create custom colors for x-axis labels
years = c(seq(1940,2000,20),birth_start_year,intersection_grad)
colors = c(rep("black",length(seq(1940,2000,20))),
#           rep("#2ca25f",2),rep("#40004b",2))
            rep("#2ca25f",1),rep("#40004b",1))
x_axis_labels = data.frame(years = years,
                           colors = colors)

ggplot(birth_deg_cohort, aes(x = year)) + 
  geom_line(aes(y=li_prop,color="Birth Cohort")) +
  geom_line(aes(y=li_deg_prop,color="College Graduation Cohort")) +
  # Create relevant lines for first set of cohort comparisons
  geom_segment(aes(x = birth_start_year[1], xend = intersection_grad[1], 
                   y = intersection_birth[1], yend = intersection_birth[1]),
               color = "gray", linetype = "longdash") +
  geom_segment(aes(x = birth_start_year[1], xend = birth_start_year[1], 
                   y = 0, yend = intersection_birth[1]),
               color = "#2ca25f", linetype = "longdash") +
  geom_segment(aes(x = intersection_grad[1], xend = intersection_grad[1], 
                   y = 0, yend = intersection_birth[1]),
               color = "#40004b", linetype = "longdash") +
  # Create relevant lines for second set of cohort comparisons
  geom_segment(aes(x = birth_start_year[2], xend = intersection_grad[2], 
                   y = intersection_birth[2], yend = intersection_birth[2]),
               color = "gray", linetype = "longdash") +
  geom_segment(aes(x = birth_start_year[2], xend = birth_start_year[2], 
                   y = 0, yend = intersection_birth[2]),
               color = "#2ca25f", linetype = "longdash") +
  geom_segment(aes(x = intersection_grad[2], xend = intersection_grad[2], 
                   y = 0, yend = intersection_birth[2]),
               color = "#40004b", linetype = "longdash") +
  scale_color_manual(values=c("#2ca25f", "#40004b")) +
  scale_x_continuous(breaks = x_axis_labels$years,
                     limits = c(1940,2013),
                     expand = c(0.001,0)) + 
  scale_y_continuous(breaks = c(0,0.02,0.04,0.06),
                     limits = c(0,0.06),
                     expand = c(0,0),
                     labels = scales::percent) + 
  scale_fill_brewer(guide="none") +
  xlab("Year") +
  ylab("Percent Included in Sample") +
  guides(color=guide_legend(title = "")) +
  labs(title = "Year of Birth and College Graduation for LinkedIn Sample") +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25),
        axis.line.x = element_line(color="black"),
        # Remember to keep this even when standardizing the rest of the theme
        axis.text.x = element_text(colour = x_axis_labels$color)) +
  annotate(geom = "text", x = birth_start_year[1], y = -0.01, 
           label = birth_start_year[1], color = "#2ca25f", size=3, family="LM Roman 10") 
  ggsave(file = file.path(directory,"Outputs",specification,"cohort_coverage.png"),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)
    


## ----age, include=FALSE-------------------------------------------------------
  
dens_sam <- read_csv(file.path(directory,"Data",specification,"dens_sam.csv"))
quantiles_sam <- read_csv(file.path(directory,"Data",specification,"quantiles_sam.csv"))
probs <- 0.5
dens_tot <- density(col_grad_filt$cohort, weights = col_grad_filt$PWGTP)
quantiles_tot <- quantile(col_grad_filt$cohort, prob=probs)
quantiles = c(quantiles_sam,quantiles_tot)
df <- data.frame(x=dens_sam$x, dens_sam=dens_sam$y, raw_tot = dens_tot$y) %>%
  mutate(dens_tot = raw_tot/sum(col_grad_filt$PWGTP))

df$quant_tot <- factor(findInterval(df$x,quantiles_sam$value),labels=c("(0,50]",
                                                                 "(50,100]"))
df$quant_sam <- factor(findInterval(df$x,quantiles_tot),labels=c("(0,50]",
                                                                 "(50,100]"))
df$gap <- df$dens_tot - df$dens_sam

# Calculations required to draw horizonal median lines on chart
birth_medians = c(as.numeric(quantiles_tot),as.numeric(quantiles_sam$value))
intersection_sam <- approx(x = df$x,
                           y = df$dens_sam,
                           xout = birth_medians)$y
intersection_tot <- approx(x = df$x,
                           y = df$dens_tot,
                           xout = birth_medians)$y

ggplot(df, aes(x=x)) + 
  geom_line(aes(y=dens_sam,color="LinkedIn Sample")) +
  geom_line(aes(y=dens_tot,color="College-Educated Labor Force")) +
  scale_color_manual(values=c("#2ca25f", "#40004b")) +
  geom_segment(x = birth_medians[1], xend = birth_medians[1],
               y = 0, yend = intersection_tot[1],
               color = "#2ca25f", linetype = "longdash") +
  geom_segment(x = birth_medians[2], xend = birth_medians[2],
               y = 0, yend = intersection_sam[2],
               color = "#40004b", linetype = "longdash") +
  scale_x_continuous(expand = c(0.001,0)) + 
  scale_y_continuous(breaks= seq(0, 0.10, 0.02),
                     limits = c(0,0.10),
                     expand = c(0,0),
                     labels = scales::percent) + 
  xlab("Birth Year") +
  ylab("Share of Observations") +
  labs(title = "Year of Birth Distribution for College-Educated Labor Force and LinkedIn Sample") +
  guides(fill=guide_legend(title="Quartile"),
         color=guide_legend(title ="")) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25),
        axis.line.x = element_line(color="black")) +
  annotate(geom = "text", x = birth_medians[1], y = intersection_tot[1] + 0.007, 
           label = paste("Median:\n",birth_medians[1]),
           color = "#2ca25f", size=4, family="LM Roman 10") +
  annotate(geom = "text", x = birth_medians[2] - 2.2, y = intersection_sam[2] + 0.007, 
           label = paste("Median:\n",birth_medians[2]),
           color = "#40004b", size=4, family="LM Roman 10")
ggsave(file = file.path(directory,"Outputs",specification,"age_distribution.png"),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)



## ----deg pull, eval=FALSE-----------------------------------------------------
# 
# 
# # Pull degrees awarded data
# raw_deg_award_1 = get_education_data(level = "college-university",
#                                  source = "ipeds",
#                                  topic = "completions-cip-2",
#                                  filters = list(award_level = 7,
#                                                 year = c(1991:2022),
#                                                 #fips = 26,
#                                                 race = 99,
#                                                 sex = 99,
#                                                 majornum = 1)
#                                 )
# 
# raw_deg_award_2 = get_education_data(level = "college-university",
#                                  source = "ipeds",
#                                  topic = "completions-cip-6",
#                                  filters = list(award_level = 7,
#                                                 year = c(1983:1990),
#                                                 race = 99,
#                                                 sex = 99)
#                                 )
# 
# # Merge the files so you have total degrees awarded by year by institution
# total_deg = raw_deg_award_2 %>%
#   filter(cipcode_6digit == 99) %>%
#   select(unitid,year,awards_6digit) %>%
#   rename(awards = awards_6digit) %>%
#   rbind(raw_deg_award_1 %>% filter(cipcode == 99) %>% select(unitid,year,awards))
# write.csv(total_deg,file.path(data_dir,"intermediate/total_deg.csv"), row.names = FALSE)
# 
# # Business degrees by year and institution
# business_deg = raw_deg_award_2 %>%
#   # Notice that the 1985 CIP code differs from the 1990
#   # https://nces.ed.gov/pubs2002/cip2000/crosswalk8590.ASP
#   mutate(cip_2 = str_sub(str_pad(cipcode_6digit,6,side="left",pad="0"),1,2)) %>%
#   filter(cip_2 == "06") %>%
#   group_by(year,cip_2,unitid) %>%
#   summarize(awards = sum(awards_6digit,na.rm=TRUE)) %>%
#   ungroup(cip_2) %>%
#   select(-cip_2) %>%
#   rbind(raw_deg_award_1 %>% filter(cipcode == 520000) %>% select(unitid,year,awards))
# write.csv(business_deg,file.path(data_dir,"intermediate/business_deg.csv"), row.names = FALSE)
# 
# # Engineering degrees by year and institution
# engineering_deg = raw_deg_award_2 %>%
#   mutate(cip_2 = str_sub(str_pad(cipcode_6digit,6,side="left",pad="0"),1,2)) %>%
#   filter(cip_2 == 14) %>%
#   group_by(year,cip_2,unitid) %>%
#   summarize(awards = sum(awards_6digit,na.rm=TRUE)) %>%
#   ungroup(cip_2) %>%
#   select(-cip_2) %>%
#   rbind(raw_deg_award_1 %>% filter(cipcode == 140000) %>% select(unitid,year,awards))
# write.csv(engineering_deg,file.path(data_dir,"intermediate/engineering_deg.csv"), row.names = FALSE)
# 
# # Engineering degrees by year and institution
# all_deg = raw_deg_award_2 %>%
#   select(unitid,year,awards_6digit,cipcode_6digit) %>%
#   rename(awards = awards_6digit,cipcode = cipcode_6digit) %>%
#   rbind(raw_deg_award_1 %>% select(unitid,year,cipcode,awards))
# write.csv(all_deg,file.path(data_dir,"intermediate/all_deg.csv"),row.names = FALSE)
# 


## ----sumstat------------------------------------------------------------------

institutional_characteristics = read_csv(file.path(data_dir,"intermediate/institutional_characteristics.csv"), 
                                         col_select = -`...1`) %>%
  # Fix misclassification of Penn State
  # I think public universities in Utah, the next largest group of excluded institutions
  # aren't seaprable because of how the OPEID works
  mutate(unitid = case_when(unitid ==495767 ~ 214777,
                            TRUE ~ unitid))

cohort_x_grad <-fread(file.path(data_dir,"intermediate/cohort_x_grad.csv"))

# Estimate retirement rate by age cohort
# Useful for estimating who remains in the labor force
# and characteristics of their institution
ret_cohort = col_grad_micro %>%
  mutate(esr = as.integer(ESR)) %>%
  filter(esr %in% c(1,2,3,6)) %>%
  filter(PWGTP < 10000) %>%
  # Filter to college graduates
  filter(SCHL > 20) %>%
  # Filter to people in labor force
  mutate(age = as.numeric(AGEP),
         cohort = 2022 - age) %>%
  group_by(cohort,esr) %>%
  summarize(n = sum(PWGTP)) %>%
  pivot_wider(id_cols = "cohort",
              names_from = esr,
              names_glue = "esr_{esr}",
              values_from = n,
              values_fill = 0
              ) %>%
  mutate(total = sum(esr_1,esr_2,esr_3,esr_6,na.rm=TRUE)) %>%
  mutate(shr_retired = esr_6/total) %>%
  left_join(cohort_x_grad, by = c("cohort"="birth")) 

# Estimating the number of college graduates still in labor force who graduated prior
# to 1983
pre_1983_cohorts = ret_cohort %>%
  mutate(post_1983 = shr * total) %>%
  group_by(cohort) %>%
  summarize(post_1983 = sum(post_1983,na.rm=TRUE)) %>%
  # Only interested in measuring characteristics of people who graduated prior to available data
  filter(cohort < 1963) %>%
  left_join(ret_cohort %>% dplyr::select(cohort,total,shr_retired) %>% distinct(), by = "cohort") %>%
  mutate(remaining_pre_1983 = (total - post_1983) * (1 - shr_retired)) %>%
  summarize(post_1983 = sum(post_1983),
            shr_retired = weighted.mean(shr_retired,w=total),
            total = sum(total),
            remaining_pre_1983 = sum(remaining_pre_1983))

# Share not in labor force by year of college graduation
# Accouting for variation in year of college graduation observed in LI data
ret_col_end = ret_cohort %>%
  filter(!is.na(shr)) %>%
  group_by(col_end) %>%
  summarize(shr_retired = weighted.mean(shr_retired, w=shr))

# Import degrees awarded data instead of waiting for slow API
total_deg <- read_csv(file.path(data_dir,"intermediate/total_deg.csv"))
business_deg <- read_csv(file.path(data_dir,"intermediate/business_deg.csv")) %>%
  rename(business_awards = awards)
engineering_deg <- read_csv(file.path(data_dir,"intermediate/engineering_deg.csv")) %>%
  rename(engineering_awards = awards)

# Estimate total number of people still in labor force by year and major
deg <- total_deg %>%
  left_join(business_deg, by = c("year","unitid")) %>%
  left_join(engineering_deg, by = c("year","unitid")) %>%
  # Make pivot to simplify calculation of retirement-adjusted totals
  pivot_longer(c(awards,business_awards,engineering_awards),
               names_to = "type", values_to = "count") %>%
  left_join(ret_col_end, by = c("year"="col_end")) %>%
  mutate(adj_count = ifelse(!is.na(count),(1-shr_retired)*count,0)) %>%
  dplyr::select(-c(count,shr_retired)) %>%
  pivot_wider(names_from = "type", values_from = "adj_count") %>%
  left_join(institutional_characteristics, by = c("unitid")) %>%
  group_by(inst_group,year) %>%
  summarize(awards = sum(awards),
            business_awards = sum(business_awards),
            engineering_awards = sum(engineering_awards)) %>%
  mutate(total = sum(awards))

# I am assuming the share of degrees awarded in years prior to 1983 
# are identical to 1983
shr_inst_1983 = total_deg %>%
  left_join(business_deg, by = c("year","unitid")) %>%
  left_join(engineering_deg, by = c("year","unitid")) %>%
  filter(year == 1983) %>%
  mutate(workers = awards * (1-pre_1983_cohorts$shr_retired),
         bus_workers = business_awards * (1-pre_1983_cohorts$shr_retired),
         eng_workers = engineering_awards * (1-pre_1983_cohorts$shr_retired),
         shr = workers/sum(workers,na.rm=TRUE),
         shr_bus = bus_workers/sum(bus_workers,na.rm=TRUE),
         shr_eng = eng_workers/sum(eng_workers,na.rm=TRUE),
         year = 1982,
         workers = shr * pre_1983_cohorts$remaining_pre_1983,
         bus_workers = shr_bus * pre_1983_cohorts$remaining_pre_1983,
         eng_workers = shr_eng * pre_1983_cohorts$remaining_pre_1983,
         ) %>%
  dplyr::select(-c(awards,shr))

# Decomposing the college-educated population into an institution x cohort panel
shr_inst_cohort = total_deg %>%
  left_join(business_deg, by = c("year","unitid")) %>%
  left_join(engineering_deg, by = c("year","unitid")) %>%
  left_join(ret_col_end, by = c("year"="col_end")) %>%
  mutate(workers = awards * (1-shr_retired),
         bus_workers = business_awards * (1-shr_retired),
         eng_workers = engineering_awards* (1-shr_retired)) %>%
  dplyr::select(-shr_retired) %>%
  group_by(year) %>%
  rbind(shr_inst_1983)

# Aggregating upward to an institution level, regardless of graduation year
shr_inst = shr_inst_cohort %>%
  group_by(unitid) %>%
  summarize(workers = sum(workers,na.rm=TRUE),
            bus_workers = sum(bus_workers,na.rm=TRUE),
            eng_workers = sum(eng_workers,na.rm=TRUE)) %>%
  left_join(institutional_characteristics, by = c("unitid"))


# Measure the college-educated population by the Census region of their college
full_regions <- shr_inst %>%
  left_join(state_region_crosswalk,by=c("state_abbr"="state")) %>%
  group_by(region) %>%
  summarize(region_total = sum(workers,na.rm=TRUE)) %>%
  filter(!is.na(region)) %>%
  mutate(shr_region = (region_total/sum(region_total)*100)) 


# Engineering and business majors as a share of all working college graduates
full_majors <- shr_inst %>%
  mutate(shr_bus = bus_workers/workers,
         shr_eng = eng_workers/workers) %>%
  summarize(shr_bus = weighted.mean(shr_bus, w = workers, na.rm = TRUE)*100,
            shr_eng = weighted.mean(shr_eng, w = workers, na.rm = TRUE)*100)

# These will be the basis for the college-educated labor force column of the
# summary statistics table
full_chetty = shr_inst %>%
  summarize(mobility_rate = weighted.mean(mobility_rate,w=workers,na.rm=TRUE)*100,
            par_mean = weighted.mean(par_mean,w=workers,na.rm=TRUE),
            k_mean = weighted.mean(k_mean,w=workers,na.rm=TRUE),
            avg_acceptance = weighted.mean(avg_acceptance,w=workers,na.rm=TRUE))


# Transforming data so it's appropriate for estimating age
full_age <- ret_cohort %>%
  dplyr::select(cohort,total,shr_retired) %>%
  distinct() %>%
  mutate(workers = (1-shr_retired) * total)
# Estimating the 25th, 50th, and 75th percentiles in the full distribution of college-educated workers
full_quantiles <- weighted.quantile(full_age$cohort, w=full_age$workers, probs=c(0.1,0.25,0.5,0.75,0.9))

# Estimating alumni by institutional grouping
full_inst_groups = shr_inst %>%
  group_by(inst_group) %>%
  summarize(workers = sum(workers,na.rm=TRUE)) %>%
  filter(!is.na(inst_group)) %>%
  mutate(shr_inst_group = (workers/sum(workers))*100)

# Can be grouped at the institution-level too for Chetty measures
full_values <- c(full_inst_groups$shr_inst_group,full_majors$shr_bus,full_majors$shr_eng,full_chetty$mobility_rate[1],full_chetty$par_mean,full_chetty$k_mean,full_chetty$avg_acceptance,full_quantiles,full_regions$shr_region,sum(full_age$workers))



## ----sumstat_dup2-------------------------------------------------------------

# Share not in labor force by year of college graduation
# Accouting for variation in year of college graduation observed in LI data
full_res_ret_col_end = ret_cohort %>%
  filter(col_end %in% c(2000:2013)) %>%
  filter(!is.na(shr)) %>%
  group_by(col_end) %>%
  summarize(shr_retired = weighted.mean(shr_retired, w=shr))

# Import degrees awarded data instead of waiting for slow API
total_deg <- read_csv(file.path(data_dir,"intermediate/total_deg.csv"))
business_deg <- read_csv(file.path(data_dir,"intermediate/business_deg.csv")) %>%
  rename(business_awards = awards)
engineering_deg <- read_csv(file.path(data_dir,"intermediate/engineering_deg.csv")) %>%
  rename(engineering_awards = awards)

# Estimate total number of people still in labor force by year and major
full_res_deg <- total_deg %>%
  left_join(business_deg, by = c("year","unitid")) %>%
  left_join(engineering_deg, by = c("year","unitid")) %>%
  # Make pivot to simplify calculation of retirement-adjusted totals
  pivot_longer(c(awards,business_awards,engineering_awards),
               names_to = "type", values_to = "count") %>%
  left_join(full_res_ret_col_end, by = c("year"="col_end")) %>%
  mutate(adj_count = ifelse(!is.na(count),(1-shr_retired)*count,0)) %>%
  dplyr::select(-c(count,shr_retired)) %>%
  pivot_wider(names_from = "type", values_from = "adj_count") %>%
  left_join(institutional_characteristics, by = c("unitid")) %>%
  group_by(inst_group,year) %>%
  summarize(awards = sum(awards),
            business_awards = sum(business_awards),
            engineering_awards = sum(engineering_awards)) %>%
  mutate(total = sum(awards))

# Decomposing the college-educated population into an institution x cohort panel
full_res_shr_inst_cohort = total_deg %>%
  left_join(business_deg, by = c("year","unitid")) %>%
  left_join(engineering_deg, by = c("year","unitid")) %>%
  filter(year %in% c(2000:2013)) %>%
  left_join(full_res_ret_col_end, by = c("year"="col_end")) %>%
  mutate(workers = awards * (1-shr_retired),
         bus_workers = business_awards * (1-shr_retired),
         eng_workers = engineering_awards* (1-shr_retired)) %>%
  dplyr::select(-shr_retired) 

# Aggregating upward to an institution level, regardless of graduation year
full_res_shr_inst = full_res_shr_inst_cohort %>%
  group_by(unitid) %>%
  summarize(workers = sum(workers,na.rm=TRUE),
            bus_workers = sum(bus_workers,na.rm=TRUE),
            eng_workers = sum(eng_workers,na.rm=TRUE)) %>%
  left_join(institutional_characteristics, by = c("unitid"))

# Measure the college-educated population by the Census region of their college
full_res_regions <- full_res_shr_inst %>%
  left_join(state_region_crosswalk,by=c("state_abbr"="state")) %>%
  group_by(region) %>%
  summarize(region_total = sum(workers,na.rm=TRUE)) %>%
  filter(!is.na(region)) %>%
  mutate(shr_region = (region_total/sum(region_total)*100)) 

# Engineering and business majors as a share of all working college graduates
full_res_majors <- full_res_shr_inst %>%
  mutate(shr_bus = bus_workers/workers,
         shr_eng = eng_workers/workers) %>%
  summarize(shr_bus = weighted.mean(shr_bus, w = workers, na.rm = TRUE)*100,
            shr_eng = weighted.mean(shr_eng, w = workers, na.rm = TRUE)*100)

# These will be the basis for the college-educated labor force column of the
# summary statistics table
full_res_chetty = full_res_shr_inst %>%
  summarize(mobility_rate = weighted.mean(mobility_rate,w=workers,na.rm=TRUE)*100,
            par_mean = weighted.mean(par_mean,w=workers,na.rm=TRUE),
            k_mean = weighted.mean(k_mean,w=workers,na.rm=TRUE),
            avg_acceptance = weighted.mean(avg_acceptance,w=workers,na.rm=TRUE))


# Transforming data so it's appropriate for estimating age
full_res_age <- ret_cohort %>%
  filter(col_end %in% c(2000:2013)) %>%
  mutate(grads = shr*total,
         workers = (1-shr_retired) * grads)
# Estimating the 25th, 50th, and 75th percentiles in the full distribution of college-educated workers
full_res_quantiles <- weighted.quantile(full_res_age$cohort, w=full_res_age$workers, probs=c(0.1,0.25,0.5,0.75,0.9))

# Estimating alumni by institutional grouping
full_res_inst_groups = full_res_shr_inst %>%
  group_by(inst_group) %>%
  summarize(workers = sum(workers,na.rm=TRUE)) %>%
  filter(!is.na(inst_group)) %>%
  mutate(shr_inst_group = (workers/sum(workers))*100)

# Can be grouped at the institution-level too for Chetty measures
col_names <- c("Public Flagship","Private, For Profit","Private, Non-Profit","Regional Public","Business Majors","Engineering Majors","Mobility Rate","Mean Parent Income ($)","Mean Kid Income ($)","Average Acceptance Rate","90th Percentile","75th Percentile","Median","25th Percentile","10th Percentile","Northeast","South","Midwest","West","N")
full_res_values <- c(full_res_inst_groups$shr_inst_group,full_res_majors$shr_bus,full_res_majors$shr_eng,full_res_chetty$mobility_rate,full_res_chetty$par_mean,full_res_chetty$k_mean,full_res_chetty$avg_acceptance,full_res_quantiles,full_res_regions$shr_region,sum(full_res_age$workers))

sample_values <- readRDS(file.path(directory,"Data",specification,"sample_values.rds"))
restrict_values <- readRDS(file.path(directory,"Data",specification,"restrict_values.rds"))
outsample_values <- readRDS(file.path(directory,"Data",specification,"outsample_values.rds"))

sumstat <- data.frame("Measure" = col_names,
                      full_census = full_values,
                      full_li = sample_values,
                      restricted_census = full_res_values,
                      restricted_li = restrict_values[-20]
                      )
names(sumstat) <- c("Measure","All Working Col. Graduates","Full LI Sample","All Working Col. Grads, 2000-13","Main LI Sample, 2000-13")
xsumstat <- xtable(sumstat,
       digits = c(0,0,0,0,0,0))
print(xsumstat,file=file.path(directory,"Outputs",specification,"summary_stats.tex"),include.rownames = FALSE)



## ----sumstat_dup3-------------------------------------------------------------

# Share not in labor force by year of college graduation
# Accouting for variation in year of college graduation observed in LI data
outsample_res_ret_col_end = ret_cohort %>%
  filter(col_end %in% c(1982:1999)) %>%
  filter(!is.na(shr)) %>%
  group_by(col_end) %>%
  summarize(shr_retired = weighted.mean(shr_retired, w=shr))

# Import degrees awarded data instead of waiting for slow API
total_deg <- read_csv(file.path(data_dir,"intermediate/total_deg.csv"))
business_deg <- read_csv(file.path(data_dir,"intermediate/business_deg.csv")) %>%
  rename(business_awards = awards)
engineering_deg <- read_csv(file.path(data_dir,"intermediate/engineering_deg.csv")) %>%
  rename(engineering_awards = awards)

# Estimate total number of people still in labor force by year and major
outsample_res_deg <- total_deg %>%
  left_join(business_deg, by = c("year","unitid")) %>%
  left_join(engineering_deg, by = c("year","unitid")) %>%
  # Make pivot to simplify calculation of retirement-adjusted totals
  pivot_longer(c(awards,business_awards,engineering_awards),
               names_to = "type", values_to = "count") %>%
  left_join(outsample_res_ret_col_end, by = c("year"="col_end")) %>%
  mutate(adj_count = ifelse(!is.na(count),(1-shr_retired)*count,0)) %>%
  dplyr::select(-c(count,shr_retired)) %>%
  pivot_wider(names_from = "type", values_from = "adj_count") %>%
  left_join(institutional_characteristics, by = c("unitid")) %>%
  group_by(inst_group,year) %>%
  summarize(awards = sum(awards),
            business_awards = sum(business_awards),
            engineering_awards = sum(engineering_awards)) %>%
  mutate(total = sum(awards))

# Decomposing the college-educated population into an institution x cohort panel
outsample_res_shr_inst_cohort = total_deg %>%
  left_join(business_deg, by = c("year","unitid")) %>%
  left_join(engineering_deg, by = c("year","unitid")) %>%
  filter(year %in% c(1982:1999)) %>%
  left_join(outsample_res_ret_col_end, by = c("year"="col_end")) %>%
  mutate(workers = awards * (1-shr_retired),
         bus_workers = business_awards * (1-shr_retired),
         eng_workers = engineering_awards* (1-shr_retired)) %>%
  dplyr::select(-shr_retired) 

# Aggregating upward to an institution level, regardless of graduation year
outsample_res_shr_inst = outsample_res_shr_inst_cohort %>%
  group_by(unitid) %>%
  summarize(workers = sum(workers,na.rm=TRUE),
            bus_workers = sum(bus_workers,na.rm=TRUE),
            eng_workers = sum(eng_workers,na.rm=TRUE)) %>%
  left_join(institutional_characteristics, by = c("unitid"))

# Measure the college-educated population by the Census region of their college
outsample_res_regions <- outsample_res_shr_inst %>%
  left_join(state_region_crosswalk,by=c("state_abbr"="state")) %>%
  group_by(region) %>%
  summarize(region_total = sum(workers,na.rm=TRUE)) %>%
  filter(!is.na(region)) %>%
  mutate(shr_region = (region_total/sum(region_total)*100)) 

# Engineering and business majors as a share of all working college graduates
outsample_res_majors <- outsample_res_shr_inst %>%
  mutate(shr_bus = bus_workers/workers,
         shr_eng = eng_workers/workers) %>%
  summarize(shr_bus = weighted.mean(shr_bus, w = workers, na.rm = TRUE)*100,
            shr_eng = weighted.mean(shr_eng, w = workers, na.rm = TRUE)*100)

# These will be the basis for the college-educated labor force column of the
# summary statistics table
outsample_res_chetty = outsample_res_shr_inst %>%
  summarize(mobility_rate = weighted.mean(mobility_rate,w=workers,na.rm=TRUE)*100,
            par_mean = weighted.mean(par_mean,w=workers,na.rm=TRUE),
            k_mean = weighted.mean(k_mean,w=workers,na.rm=TRUE),
            avg_acceptance = weighted.mean(avg_acceptance,w=workers,na.rm=TRUE))

# Transforming data so it's appropriate for estimating age
outsample_res_age <- ret_cohort %>%
  filter(col_end %in% c(1982:1999)) %>%
  mutate(grads = shr*total,
         workers = (1-shr_retired) * grads)
# Estimating the 25th, 50th, and 75th percentiles in the full distribution of college-educated workers
outsample_res_quantiles <- weighted.quantile(outsample_res_age$cohort, w=outsample_res_age$workers, probs=c(0.1,0.25,0.5,0.75,0.9))

# Estimating alumni by institutional grouping
outsample_res_inst_groups = outsample_res_shr_inst %>%
  group_by(inst_group) %>%
  summarize(workers = sum(workers,na.rm=TRUE)) %>%
  filter(!is.na(inst_group)) %>%
  mutate(shr_inst_group = (workers/sum(workers))*100)

# Can be grouped at the institution-level too for Chetty measures
outsample_res_values <- c(outsample_res_inst_groups$shr_inst_group,outsample_res_majors$shr_bus,outsample_res_majors$shr_eng,outsample_res_chetty$mobility_rate,outsample_res_chetty$par_mean,outsample_res_chetty$k_mean,outsample_res_chetty$avg_acceptance,outsample_res_quantiles,outsample_res_regions$shr_region,sum(outsample_res_age$workers))

outsample_sumstat <- data.frame("Measure" = col_names,
                      full_census = full_values,
                      full_li = sample_values,
                      restricted_census = outsample_res_values,
                      restricted_li = outsample_values
                      )
names(outsample_sumstat) <- c("Measure","All Working Col. Graduates","Full LI Sample","All Working Col. Grads, 1982-99","Main LI Sample, 1982-99")
xoutsumstat <- xtable(outsample_sumstat,
       digits = c(0,0,0,0,0,0))
print(xoutsumstat,file=file.path(directory,"Outputs",specification,"outsample_summary_stats.tex"),include.rownames = FALSE)



## ----map----------------------------------------------------------------------

# Map coloring fuunction
nacol <- function(spdf){
    resample <- function(x, ...) x[sample.int(length(x), ...)]
    nunique <- function(x){unique(x[!is.na(x)])}
    np = nrow(spdf)
    adjl = spdep::poly2nb(spdf)
    cols = rep(NA, np)
    cols[1]=1
    nextColour = 2

    for(k in 2:np){
        adjcolours = nunique(cols[adjl[[k]]])
        if(length(adjcolours)==0){
            cols[k]=resample(cols[!is.na(cols)],1)
        }else{
            avail = setdiff(nunique(cols), nunique(adjcolours))
            if(length(avail)==0){
                cols[k]=nextColour
                nextColour=nextColour+1
            }else{
                cols[k]=resample(avail,size=1)
            }
        }
    }
    return(cols)
}


data(county_laea)

map = county_laea %>% 
  dplyr::left_join(unified_cbsa, by = c("GEOID"="GeoFIPS")) %>%
  group_by(cbsa_code) %>%
  dplyr::summarize(geometry = st_union(geometry))

#map$C = nacol(map$C)
colors = nacol(map)

ggplot(map, aes(fill = as.character(colors))) +
  geom_sf(show.legend = FALSE) +
  # Palettes must be at least nine colors
  scale_fill_brewer(palette = "Set3") +
  theme_void() + 
  labs(title = "Map of Local Labor Markets") +
  theme(text = element_text(size=10, family="LM Roman 10"),
  plot.title = element_text(hjust = 0.5))
#  scale_fill_manual(values = colors) +
ggsave(file = paste(directory,"/Outputs/","labor_markets.png",sep = ""),
         width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)  

