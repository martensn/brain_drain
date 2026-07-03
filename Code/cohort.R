library(data.table)
library(tidyverse)
library(ggplot2)

# Requires directory/data_dir/out_dir already in the session -- run the
# config snippet (or any other in-scope script) first.

# Remember to re-run step 6 for updated estimates on full universe

#middle_prestige <- regression_cbsa_10[, mean(prestige_bin == "Middle", na.rm = TRUE), by = .(col_end)]
#high_prestige <- regression_cbsa_10[, mean(prestige_bin == "High", na.rm = TRUE), by = .(col_end)]
#low_prestige <- regression_cbsa_10[, mean(prestige_bin == "Low", na.rm = TRUE), by = .(col_end)]

#middle_prestige <- regression_cbsa_10[, mean(inst_group == "RPU", na.rm = TRUE), by = .(col_end)]
#high_prestige <- regression_cbsa_10[, mean(prestige_bin == "High", na.rm = TRUE), by = .(col_end)]
#low_prestige <- regression_cbsa_10[, mean(prestige_bin == "Low", na.rm = TRUE), by = .(col_end)]

hs_cbsa_10 <- read_csv(file.path(data_dir,"intermediate/cohort_return_hs_10.csv"))
col_cbsa_10 <- read_csv(file.path(data_dir,"intermediate/cohort_return_col_10.csv"))

return_levels = hs_cbsa_10 %>%
  mutate(geography = "High School") %>%
  rbind(col_cbsa_10 %>% mutate(geography = "College")) %>%
  rename(shr_return = V1)  %>%
  mutate(col_end = as.numeric(as.character(col_end)))


# Fit the linear model
hs_model <- lm(shr_return ~ col_end, data = return_levels %>% filter(geography == "High School" & col_end %in% c(1982:2013)))
col_model <- lm(shr_return ~ col_end, data = return_levels%>% filter(geography == "College" & col_end %in% c(1982:2013)))

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
  geom_smooth(data = return_levels %>% filter(col_end %in% c(1982,2013)), method = "lm", aes(color = geography), line = "dashed") +
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
ggsave(file = paste(directory,"/Outputs/","cohort_levels.png",sep = ""),
       width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi) 