# Load necessary library
library(AER)

# Take a 1% random sample of microdata
set.seed(123)  # For reproducibility
# Remove the "V1" column from microdata if it exists
microdata$V1 <- NULL
# Subset institutional characteristics data 
inst_group = institutional_characteristics[,c("unitid","inst_group")]
# Perform a left join on "unitid" to add inst_group to microdata
microdata_joined <- merge(microdata, inst_group, by.x = "col_unitid", by.y = "unitid", all.x = TRUE)

# Take a 1% sample of the merged data
sample_data <- microdata_joined[sample(1:nrow(microdata_joined), size = 0.01 * nrow(microdata_joined)), ]
sample_data$in_state <- ifelse(sample_data$col_state == sample_data$hs_state, "In-State", "Out-of-State")

# + in_state + (inst_group * in_state)

# Run ivreg with hs_shock_avg_7 and col_shock_avg_7 as instruments
iv_model <- ivreg(same_state_hs ~ inst_group |
                    hs_shock_avg_7 + col_shock_avg_7, 
                  data = sample_data)

# Summary of the model
summary(iv_model, diagnostics = TRUE)