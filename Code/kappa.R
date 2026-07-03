# Load necessary libraries
library(fixest)
library(tidyverse)
library(data.table)

#directory = "/Volumes/lsa-areynoso"
directory = "/nfs/turbo/lsa-areynoso"

# Run beginning of step 6 first


regression[,return_to_both := same_cbsa_col_10 * same_cbsa_col_10]
problems <- regression[return_to_both == 1 & col_in_not_hs_cbsa == 1]

# Import residual measurements of places with amenities
# These are places that attract more migrants than predicted by their labor demand
differences_unfilt <- read_csv(file.path(directory,"Data/06","local_differences_unfilt.csv")) %>%
  mutate(cbsa_code = str_pad(hs_cbsa,width=5,side="left",pad="0")) %>%
  select(-c("hs_cbsa","...1")) %>%
  left_join(cbsa_li_crosswalk %>% select(c("cbsa_code","cbsa_core")) %>% distinct(), by = "cbsa_code") %>%
  mutate(difference_cbsa_alt = shr_returners_cbsa - pred_shr_returners_cbsa) %>%
  filter(difference_cbsa_alt > 0)

# Migration destinations are simply CBSA codes of locations where the difference
# between the predicted labor demand and the actual labor demand is positive
# indicating migration above what you'd expect given the labor demand 
destinations = differences_unfilt$cbsa_code

# Evan suggested a column for all migrants in the sample, with no additional conditioning
migrant_universe <- regression[ever_leave_cbsa_hs_10 == 1 | ever_leave_cbsa_col_10 == 1]

# Parameters for complier means table
complier_chars <- c("inst_group","inst_group","in_state","transfer",
                    "non_traditional","prestige_bin","prestige_bin",
                    "inst_group_in_state","inst_group_in_state","inst_group_in_state",
                    "return_within_5","return_within_5","hs_shock_quartile",
                    "hs_shock_quartile","hs_shock_quartile","amenity","amenity")
preferred_values <- c("RPU","PF","In-State","Transfer","Traditional","Low","Middle",
                      "RPU, In-State", "PF, In-State", "Private, In-State",
                      "Return Within 5 Years","Return in 5-10 Years","Bottom LD Quartile",
                      "Middle LD Quartiles","Top LD Quartile","High-Amenity LM", "Low-Amenity LM")

# Initialize results table
compliers <- data.table(characteristic = character(), preferred_value = character(),
                        coefficient = numeric(), std_error = numeric(), sample = numeric())

# Loop over each characteristic and preferred value
for (i in seq_along(complier_chars)) {
  
  char_col <- complier_chars[i]
  preferred_val <- preferred_values[i]
  
  print(paste("Processing:", char_col, "==", preferred_val))
  
  # Create treatment variable
  treatment_col <- paste0(char_col, "_treatment")
  regression_cbsa_10[, (treatment_col) := as.integer(get(char_col) == preferred_val) * same_cbsa_hs_10]
  migrant_universe[, (treatment_col) := as.integer(get(char_col) == preferred_val) * same_cbsa_hs_10]
  
  # Compute unconditional mean
  sample_mean <- regression_cbsa_10[, mean(get(char_col) == preferred_val, na.rm = TRUE)]
  universe_mean <- migrant_universe[, mean(get(char_col) == preferred_val, na.rm = TRUE)]
  
  # Define regression formula
  formula <- paste0(treatment_col, " ~ same_cbsa_hs_10 + inst_group + in_state + (inst_group * in_state) + ",
                    "non_traditional + transfer + col_major + soc_3 + prestige_bin + col_end + hs_state")
  
  # Run regression
  #model <- lm_robust(as.formula(formula), data = regression_cbsa, clusters = hs_cbsa_code)
  model <- fixest::feols(as.formula(formula), data = regression_cbsa_10, cluster = ~hs_cbsa_code)
  
  # Store results
  compliers <- rbind(compliers, data.table(
    characteristic = char_col,
    preferred_value = preferred_val,
    all_migrants = universe_mean,
    sample = sample_mean,
    coefficient = model$coefficients["same_cbsa_hs_10"],
    std_error = model$se["same_cbsa_hs_10"]
    
  ), fill = TRUE)
}

stargazer(compliers[,1:6],
          summary=FALSE)
       label = paste0("table:compliers"),
       #include.ci = FALSE,
       caption = paste0("Complier means"),
       center = FALSE,
       digits = 3,
       
)




