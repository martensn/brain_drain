
##    Fully Automated with All Shocks

models = list()
for(h in 1:length(geos))
{
  #for(i in avgs)
  #{
  #  yr_vars = names(regression)[grepl(paste0("avg_",i), names(regression))]
  yr_vars = names(regression)[grepl("avg_7", names(regression))]
  
  current = paste0("same_",geos[h],"_hs ~ ",paste(yr_vars,collapse=" + ")," + inst_group + in_state + (inst_group * in_state)")
  #simple = paste0("same_",geos[h],"_hs ~ ",paste(yr_vars,collapse=" + ")," + inst_group + in_state + non_traditional + transfer + col_major + hs_state + col_end")
  
  formula = c(current,
              #simple,
              paste0(current," + non_traditional + transfer + col_major"),
              paste0(current," + non_traditional + transfer + col_major + hs_state"),
              paste0(current," + non_traditional + transfer + col_major + hs_state + col_end"))
  #paste0(simple," + (inst_group * ",yr_vars[1],")"),
  #paste0(current," + dist"),
  #paste0(current," + dist + (dist * ",yr_vars[1],")"),
  #paste0(simple," + dist"),
  #paste0(simple," + dist + (dist * ",yr_vars[1],")"),
  #paste0(current," + (inst_group * ",yr_vars[1],") + dist + (dist * ",yr_vars[1],")"),
  #paste0(simple," + (inst_group * ",yr_vars[1],") + dist + (dist * ",yr_vars[1],")"))
  for(f in 1:length(formula))
  {
    form = formula[f]
    models[[as.character(f)]] <- lm(form, data = regression)
  }
  #}
  texreg(list(models[[1]],models[[2]],models[[3]],models[[4]]),
         file = paste0(directory,"/Outputs/Tables/opt_",f,"_",geos[h],".tex"),
         #custom.model.names = shock_lbl,
         label = paste0("table:opt-",f,"-",geos[h]),
         caption = paste0("Probability of returning to ",geos[h]," containing high school"),
         center = TRUE,
         digits = 3)
  #custom.coef.map = nestedlist)
}

##    Fully Automated with 7-Year Shock

for(f in 1:length(formula))
{
  models = list()
  for(h in 1:length(geos))
  {
    #for(i in avgs)
    #{
    yr_vars = names(regression)[grepl(paste0("avg_",i), names(regression))]
    
    current = paste0("same_",geos[h],"_hs ~ ",paste(yr_vars,collapse=" + ")," + inst_group + in_state + non_traditional + transfer + col_major + col_end + (inst_group * in_state)")
    simple = paste0("same_",geos[h],"_hs ~ ",paste(yr_vars,collapse=" + ")," + inst_group + in_state + non_traditional + transfer + col_major + hs_state + col_end")
    
    formula = c(current,
                #simple,
                paste0(current," + (inst_group * ",yr_vars[1],")"),
                #paste0(simple," + (inst_group * ",yr_vars[1],")"),
                paste0(current," + dist"),
                paste0(current," + dist + (dist * ",yr_vars[1],")"),
                #paste0(simple," + dist"),
                #paste0(simple," + dist + (dist * ",yr_vars[1],")"),
                paste0(current," + (inst_group * ",yr_vars[1],") + dist + (dist * ",yr_vars[1],")"))
    #paste0(simple," + (inst_group * ",yr_vars[1],") + dist + (dist * ",yr_vars[1],")"))
    
    form = formula[f]
    models[[as.character(4)]] <- lm(form, data = regression)
  }
  texreg(list(models[[1]]),
         file = paste0(directory,"/Outputs/Tables/opt_",f,"_",geos[h],"_7.tex"),
         custom.model.names = shock_lbl[4],
         label = paste0("table:opt-",f,"-",geos[h]),
         caption = paste0("Probability of returning to ",geos[h]," containing high school"),
         center = TRUE,
         digits = 3,
         custom.coef.map = nestedlist)
}

##    Create Custom Table

for(h in 1:length(geos))
{
  models = list()
  for(f in 1:length(formula))
  {
    #for(i in avgs)
    #{
    yr_vars = names(regression)[grepl(paste0("avg_7"), names(regression))]
    
    current = paste0("same_",geos[h],"_hs ~ ",paste(yr_vars,collapse=" + ")," + col_type + in_state + non_traditional + transfer + col_major + hs_state + col_end + (col_type * in_state)")
    simple = paste0("same_",geos[h],"_hs ~ ",paste(yr_vars,collapse=" + ")," + col_type + in_state + non_traditional + transfer + col_major + hs_state + col_end")
    
    formula = c(current,
                paste0(simple," + (col_type * ",yr_vars[1],")"))
    
    form = formula[f]
    models[[as.character(4)]] <- lm(form, data = regression)
  }
  texreg(list(models[[1]],models[[2]]),
         file = paste0(directory,"/Outputs/Tables/final_",geos[h],"_7.tex"),
         custom.model.names = rep(shock_lbl[4],length(formula)),
         label = paste0("table:final-",geos[h]),
         caption = paste0("Probability of returning to ",geos[h]," containing high school"),
         center = TRUE,
         digits = 3,
         custom.coef.map = nestedlist)
}