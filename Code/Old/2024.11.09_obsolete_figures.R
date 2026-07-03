
##    Obsolete 
###   Attrition Plot

```{r}

survivorship_plot = reshaped_cbsa[, .N , by = c("n_grad","geo","value")] %>%
  mutate(group = ifelse(is.na(value)==TRUE,
                        "Complete Profile",
                        "Incomplete Profile")) %>%
  #ifelse(value == 0, 
  #       paste("Not in hs", geo, sep = " "), 
  #       paste("In hs", geo, sep = " ")))) %>%
  as.data.frame() %>%
  group_by(group,n_grad,geo) %>%
  dplyr::summarize(N = sum(N)) %>%
  mutate(pct = N/nrow(survivorship))

# Filtering data by geography to simplify presentation of data
survivorship_cbsa = survivorship_plot %>%
  filter(geo == "cbsa")

write.csv(survivorship_cbsa,file.path(directory,"Outputs/survivorship_cbsa.csv"))
survivorship_cbsa = read_csv(file.path(directory,"Outputs/survivorship_cbsa.csv")) %>%
  select(c(group,n_grad,geo,N,pct))


# Plot shows the age structure of LI user base
# Older workers omit early work history, sometimes to avoid age discrimination
ggplot(survivorship_cbsa, aes(x = n_grad, 
                              y = pct,
                              fill = group)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(limits = c(0,40),
                     expand = c(0.001,0)) +
  scale_y_continuous(limits = c(0,1),
                     expand = c(0,0),
                     labels = scales::percent) +
  xlab("Years Since College Graduation") +
  ylab("Percent of Observations") +
  labs(title = "Attrition and Omission in LinkedIn Profiles",
       fill = "Status of Geodata") +
  theme(#panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    text         = element_text(size=10, family="LM Roman 10"),
    plot.title = element_text(hjust = 0.5),
    legend.background = element_rect(fill='transparent',color=NA),
    legend.box.background = element_rect(fill='transparent',color=NA),
    legend.position = "bottom")
ggsave(file = paste(directory,"/Outputs/","completeness_ngrad.png",sep = ""),
       width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)


```

###   Age Group Maps

```{r hs inclusion cleaning}
#color_list = c('#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837')

map_age = map
left_join(fraction, by = "cbsa_code")

ggplot(map_age, aes(fill = age_18)) +
  geom_sf(show.legend = TRUE) +
  scale_fill_gradientn(name = "% of 18-24\nCollege Grads",
                       labels = scales::percent,
                       colors = color_list,
                       #low = "#40004b",
                       #high = "#1b7837",
                       limits=c(0,0.25),
  ) +
  labs(title = "LI Sample as a Share of College Graduates Aged 18-24 in 2022") +
  theme_void() + 
  theme(text = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5))
ggsave(file = paste(directory,"/Outputs/","age_18-24.png",sep = ""),
       width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)

ggplot(map_age, aes(fill = age_25)) +
  geom_sf(show.legend = TRUE) +
  scale_fill_gradientn(name = "% of 25-34\nCollege Grads",
                       labels = scales::percent,
                       colors = color_list,
                       #low = "#40004b",
                       #high = "#1b7837",
                       limits=c(0,0.13),
  ) +
  labs(title = "LI Sample as a Share of College Graduates Aged 25-34 in 2022") +
  theme_void() + 
  theme(text = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5))
ggsave(file = paste(directory,"/Outputs/","age_25-34.png",sep = ""),
       width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)

ggplot(map_age, aes(fill = age_35)) +
  geom_sf(show.legend = TRUE) +
  scale_fill_gradientn(name = "% of 35-44\nCollege Grads",
                       labels = scales::percent,
                       colors = color_list,
                       #low = "#40004b",
                       #high = "#1b7837",
                       limits=c(0,0.08),
  ) +
  labs(title = "LI Sample as a Share of College Graduates Aged 35-44 in 2022") +
  theme_void() + 
  theme(text = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5))
ggsave(file = paste(directory,"/Outputs/","age_35-44.png",sep = ""),
       width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)

```

###   Age Distribution Plot + Table

```{r}

probs <- c(0.1,0.25, 0.5, 0.75,0.9)

# Create table comparing age distribution of HS omitters and includers
quantile_comp = data.frame(Percentile = c("Analytical Sample","College Graduates in Labor Force")) %>%
  cbind(rbind(as.character(quantile(microdata$birth, prob = probs)),
              as.character(round(weighted.quantile(x = col_grad_micro$cohort,
                                                   w = col_grad_micro$PWGTP,
                                                   probs = probs),0)))) %>%
  setnames(c("1","2","3","4","5"),as.character(probs*100))


stargazer(quantile_comp,
          summary=FALSE,
          rownames = FALSE,
          digits = 1,
          #style = "qje",
          align = TRUE,
          #title = "Age distribution of LinkedIn users who omit and include high school on their profile",
          float = TRUE,
          decimal.mark=NULL,
          out = file.path(directory,"/Outputs/Tables/quantile_comp.tex")
)

ggplot(df, aes(x=x)) + 
  geom_line(aes(y=dens_sam,color="Analytical Sample")) +
  geom_line(aes(y=dens_tot,color="College-Educated Labor Force")) +
  scale_color_manual(values=c("#2ca25f", "#40004b")) +
  geom_vline(xintercept = quantiles_sam, color = "#2ca25f", linetype = "longdash") +
  geom_vline(xintercept = quantiles_tot, color = "#40004b", linetype = "longdash") +
  #geom_segment(aes(x = quantiles_sam, y = 0, xend = quantiles_sam, yend = 0.0363),
  #             color = "#2ca25f", size = 0.3) +
  #geom_segment(aes(x = quantiles_tot, y = 0, xend = quantiles_tot, yend = 0.023),
  #             color = "#40004b", size = 0.3) + 
  # Fills interior of charts based quantiles
  #geom_ribbon(aes(ymin=0, 
  #                ymax=dens_tot - gap, 
  #                fill=quant_tot),
  #            alpha = 0.5) +
  #geom_ribbon(aes(ymin=0, 
  #                ymax=dens_sam + gap, 
#                fill=quant_sam),
#            alpha = 0.5) +
#scale_fill_brewer(direction=-1,
#                  palette="Greys") +
scale_x_continuous(#breaks= major_breaks,
  #labels = major_breaks,
  expand = c(0.001,0)) + 
  scale_y_continuous(breaks= seq(0, 0.06, 0.02),
                     limits = c(0,0.06),
                     expand = c(0,0),
                     labels = scales::percent) + 
  xlab("Birth Year") +
  ylab("Share of Observations") +
  labs(title = "Age Distribution of LinkedIn Sample") +
  guides(fill=guide_legend(title="Quartile"),
         color=guide_legend(title ="")) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        text         = element_text(size=10, family="LM Roman 10"),
        axis.line.x = element_line(color="black"),
        #panel.grid.major.y = element_line(color = "black",
        #                                  linewidth = 0.5),
        plot.title = element_text(hjust = 0.5),
        #panel.grid.minor = element_blank(),
        #legend.title=element_blank(),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.position = "bottom") +
  annotate(geom = "text", x = 1976, y = 0.025, label = "1977",
           color = "#40004b", size=4, family="LM Roman 10") +
  annotate(geom = "text", x = 1987, y = 0.0375, label = "1989",
           color = "#2ca25f", size=4, family="LM Roman 10")

```

###    Table for Measuring Income Distribution by Institutional Group

```{r chetty table}

# Certain public flagships need to be manually recoded
# pf.xlsx contains the correct names
pffilename = "pf.xlsx"
pf = read_excel(file.path(directory,"Data/07",pffilename),
                col_types = "text") %>%
  mutate(col_type = "Public Flagship") 

# Merge manually recoded Public Flagships with Chetty data  
col_inc_dist = read_xlsx(paste(directory,"Data/chetty_college_inc_dist.xlsx",sep="/")) %>%
  as.data.frame() %>%
  mutate(name = str_to_lower(name)) %>%
  left_join(pf, by = c("name"="chetty_name")) %>%
  select(-c(institution,state.y,opeid)) %>%
  mutate(col_type = case_when(super_opeid %in% pf$opeid ~ "Public Flagship",
                              type == "1" & tier %in% c("1","2","3","4") ~ "RPU, More Selective",
                              type == "1" & tier %in% c("5","9","999","Unrated") ~ "RPU, Less Selective",
                              type == "2" ~ "Private, Non-Profit",
                              TRUE ~ "Private, For-Profit"))

inc_by_col_type = col_inc_dist %>% 
  filter(is.na(col_type)==FALSE) %>%
  group_by(col_type) %>% 
  summarize(par_q1_n = sum(par_q1_n),
            par_q2_n = sum(par_q2_n),
            par_q3_n = sum(par_q3_n),
            par_q4_n = sum(par_q4_n),
            par_q5_n = sum(par_q5_n)) %>%
  mutate(total = par_q1_n + par_q2_n + par_q3_n + par_q4_n + par_q5_n) %>%
  mutate(par_q1_pct = round(par_q1_n/total * 100,1),
         par_q2_pct = round(par_q2_n/total * 100,1),
         par_q3_pct = round(par_q3_n/total * 100,1),
         par_q4_pct = round(par_q4_n/total * 100,1),
         par_q5_pct = round(par_q5_n/total * 100,1)) %>%
  select(c(col_type,par_q1_pct:par_q5_pct)) %>%
  rename("Grouping" = "col_type",
         "Bottom" = "par_q1_pct",
         "Lower Middle" = "par_q2_pct",
         "Middle" = "par_q3_pct",
         "Upper Middle" = "par_q4_pct",
         "Top" = "par_q5_pct")

stargazer(inc_by_col_type[1:6,], 
          summary = FALSE,
          rownames = FALSE,
          digits = 3,
          #style = "qje",
          align = TRUE,
          title = "Parental income distribution of students by institutional grouping",
          float = TRUE,
          #label = "table:observation",
          digit.separator = ",",
          out = file.path(directory,"/Outputs/Tables/income-distribution.tex")
)
```




###    Plotting Institutional Grouping by Income Quantile

```{r quantile grouping}

inc_by_col_unnorm = col_inc_dist %>% 
  filter(is.na(col_type)==FALSE) %>%
  group_by(col_type) %>% 
  summarize(par_q1_n = sum(par_q1_n),
            par_q2_n = sum(par_q2_n),
            par_q3_n = sum(par_q3_n),
            par_q4_n = sum(par_q4_n),
            par_q5_n = sum(par_q5_n)) %>%
  rename("Grouping" = "col_type",
         "Bottom" = "par_q1_n",
         "Lower Middle" = "par_q2_n",
         "Middle" = "par_q3_n",
         "Upper Middle" = "par_q4_n",
         "Top" = "par_q5_n") %>%
  pivot_longer(cols = !Grouping,
               names_to = "Quantile",
               values_to = "N") %>%
  mutate(Quantile = factor(Quantile, 
                           levels = c("Bottom","Lower Middle","Middle","Upper Middle","Top"))) %>%
  filter(Grouping != "Private, For-Profit")


ggplot(inc_by_col_unnorm, aes(x = Quantile, y = N, fill = Grouping)) +
  geom_col() + 
  scale_y_continuous(limits = c(0,800000),
                     expand = c(0,0),
                     labels = scales::comma) +
  xlab("Parental Income Quantile") +
  ylab("Bachelor's Degrees Awarded in 2013") +
  labs(title = "Institutional Groupings by Parental Income Quantile in 2013") +
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
        axis.line.x = element_line(color="black"))
ggsave(file = paste(directory,"/Outputs/","grouping_by_quantile.png",sep = ""),
       width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)

```
