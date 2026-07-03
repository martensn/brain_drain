library(reldist)
library(tidyverse)
library(data.table)

directory = "/nfs/turbo/lsa-areynoso"
#directory = "/Volumes/lsa-areynoso"
  
intercensal <- read_csv(file.path(directory,"Data/06/intercensal.csv"))


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
  
  write.csv(split,file.path(directory,"Data/06",paste0("directed_",start,"_",end,".csv")),row.names = FALSE)
  
  return(split)
  
}

for(s in 0:7)
{
  start = 2000+s
  end = start+10
  #split <- yagan(start,end)
 # print(paste0("Completed: ",start,"-",end))
#}

#end = 2016
split <- read_csv(file.path(directory,"Data/06",paste0("directed_",start,"_",end,".csv")))

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
ggsave(file = paste(directory,"/Outputs/",paste0("directedness_",start,"_",end,".png"),sep = ""),
       width = wid, height = hei, units = unit, bg = bkgrnd, dpi = dpi)     
}
