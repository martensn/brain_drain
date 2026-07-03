# Author  : Nick Martens
# Date    : 10 April 2023
# Purpose : Running regressions

# Remove extra columns
regression = all_binary_final %>%
  select(-c(col_start,hs_end,CBSATYPE,yr_postgrad,yr_return)) %>%
  # Rename "999" and "9" as selectivity rating 6
  mutate(barrons = ifelse(barrons > 5,6,barrons),
         selective_binary = ifelse(barrons < 3,1,0)) 

# Standard specification
attempt1 = lm(data = regression, same_cbsa ~ hs_shock + col_shock + (hs_shock * col_hs_diff_cbsa) + (col_shock * col_hs_diff_cbsa) + barrons + col_end)
attempt2 = lm(data = regression, same_cbsa ~ hs_shock + col_shock + (hs_shock * col_hs_diff_cbsa) + (col_shock * col_hs_diff_cbsa) + barrons + col_end + major)
attempt3 = lm(data = regression, same_state ~ hs_shock + col_shock + (hs_shock * col_hs_diff_cbsa) + (col_shock * col_hs_diff_cbsa) + barrons + col_end)
attempt4 = lm(data = regression, same_state ~ hs_shock + col_shock + (hs_shock * col_hs_diff_cbsa) + (col_shock * col_hs_diff_cbsa) + barrons + col_end + major)
attempt5 = lm(data = regression, same_cbsa ~ hs_shock + col_shock + (hs_shock * col_hs_diff_cbsa) + (col_shock * col_hs_diff_cbsa) + selective_binary + col_end)
attempt6 = lm(data = regression, same_state ~ hs_shock + col_shock + (hs_shock * col_hs_diff_cbsa) + (col_shock * col_hs_diff_cbsa) + selective_binary + col_end)


texreg(list(attempt1,attempt3), 
       digits = 4,  
       stars = c(0.01, 0.05, 0.1),
       custom.model.names = c("Same CBSA",
                              "Same State"),
       custom.coef.names = c("Intercept",
                             "High School CBSA Shock",
                             "College CBSA Shock",
                             "Different CBSA for College and High School",
                             "Selectivity",
                             "Year-Fixed Effects",
                             "High School CBSA Shock for Leavers"),
       caption = "Preliminary results using standard Barron's ratings",
       label = "Attempt1",
       caption.above = TRUE)


texreg(list(attempt5,attempt6), 
       digits = 4,  
       stars = c(0.01, 0.05, 0.1),
       custom.model.names = c("Same CBSA",
                              "Same State"),
       custom.coef.names = c("Intercept",
                             "High School CBSA Shock",
                             "College CBSA Shock",
                             "Different CBSA for College and High School",
                             "Selective Binary",
                             "Year-Fixed Effects",
                             "High School CBSA Shock for Leavers"),
       caption = "Preliminary results using Selective Binary",
       label = "Attempt2",
       caption.above = TRUE)
#       siunitx = TRUE,
#       booktabs = TRUE,
#       use.packages = FALSE)


texreg(list(attempt2,attempt4), 
       digits = 4,  
       stars = c(0.01, 0.05, 0.1),
       custom.model.names = c("Same State",
                              "Same State with Major"),
       custom.coef.names = c("Intercept",
                             "High School CBSA Shock",
                             "College CBSA Shock",
                             "Different CBSA for College and High School",
                             "Selectivity",
                             "Year-Fixed Effects",
                             "High School CBSA Shock for Leavers",
                             "Architecture major",
                             "Biology major",
                             "Business major",
                             "Chemistry major",
                             "Economics major",
                             "Education major",
                             "empty major",
                             "Engineering major",
                             "Finannce major",
                             "Information Technology major",
                             "Law major",
                             "Marketing major",
                             "Mathematics major",
                             "Medicine major",
                             "Nursing major",
                             "Physics major",
                             "Statistics major"),
       caption = "Preliminary results",
       label = "ContReg",
       caption.above = TRUE,
       siunitx = TRUE,
       booktabs = TRUE,
       use.packages = FALSE)
# If you go to an elite school in your CBSA, are you still less likely to stay