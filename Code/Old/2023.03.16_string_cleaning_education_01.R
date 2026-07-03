library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)

starttime = Sys.time()

#	Function used to compute mode of a dataset
Mode <- function(x, na.rm = TRUE)
{
  if(na.rm)
  {
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x,ux)))])
}

#	Read in files
filepath = "/Users/nickmartens/Dropbox (University of Michigan)"
#userfilename = "individual_user_000.csv"
#user = read_delim(paste0(filepath,"/",userfilename),
#                  col_names=TRUE,
#                  show_col_types = FALSE,
#                  delim=",") 
user_edfilename = "/Users/nickmartens/Dropbox (University of Michigan)/Linkedin/Data/OneDrive_1_3-29-2023/education_0000_part_00.csv"
user_ed = read_delim(paste0(filepath,"/",user_edfilename),
                     col_names=TRUE,
                     show_col_types = FALSE,
                     delim=",") 
#user_ed$degree <- make.names(user_ed$degree)

#	Convert degrees to factors for subsequent tabulation
#user_ed$degree <- factor(user_ed$degree, 
#                         c("High.School","Associate","Bachelor","Master","MBA","Doctor"), 
 #                        ordered = TRUE)	

#	Read in and unify position files
#positionfilename0 = "individual_position_000.csv"
#position0 = read_delim(paste0(filepath,"/",positionfilename0),
#                       col_names=TRUE,
#                       show_col_types = FALSE,
#                       delim=",") 
#positionfilename1 = "individual_position_001.csv"
#position1 = read_delim(paste0(filepath,"/",positionfilename1),
#                       col_names=TRUE,
#                       show_col_types = FALSE,
#                       delim=",") 
#positionfilename2 = "individual_position_002.csv"
#position2 = read_delim(paste0(filepath,"/",positionfilename2),
#                       col_names=TRUE,
#                       show_col_types = FALSE,
#                       delim=",") 
#position = rbind(position0,position1,position2)

#position %>%
#  group_by(user_id) %>%
#  summarize(workstart = min(as.Date(startdate)))	


#	Remove additional columns
#	Uncomment 36+37 and comment 38 below to extract a sample
#user_ed <- user_ed[-c(1,3:4,6)]
#user_ed05 <- user_ed02 %>%
#	sample_n(nrow(user_ed02) * .001)

#	At this point, 3,568,627/5,403,448 (66.0% of observations) correspond to a degree
user_ed01 = user_ed

#	Remove all punctuation and symbols from campus names
user_ed01$university_name = str_replace_all(user_ed01$university_name,"[:punct:]","")
user_ed01$university_name = str_replace_all(user_ed01$university_name,"[:symbol:]","")

#	Remove white space and convert all names to lowercase
#	When every campus is lowercase, identical strings with distinct
#	capitalization are not grouped separately
user_ed02 <- user_ed01 %>%
  mutate(across(where(is.character),str_trim)) %>%
  mutate(across(where(is.character),str_squish)) %>%
  mutate(university_name = tolower(university_name))

#	Group users by campus and tabulates most common degree for the campus
#	Thanks to the mode methodology and string cleaning, 1,495,047 degrees can be imputed
#	At this point, 5,063,718/5,403,448 (93.7% of observations) correspond to a degree
user_ed03 <- user_ed02	%>%
  group_by(university_name, university_country) %>%
  summarize(deg_mode = Mode(degree), sample_size = n()) %>%
  filter(deg_mode == "empty")

# "Hidden" secondary institutions
q = user_ed03[which(grepl("high school",user_ed03$university_name,ignore.case = TRUE)==TRUE),]
a = user_ed03[which(grepl("academy",user_ed03$university_name,ignore.case = TRUE)==TRUE),]
# Any institution name containing "High School" or "Academy" converted from
# "empty" to "High School" factor
hidden = rbind(q,a)
hidden %>%
  deg_mode = "High School"

#	Filters 6.3% of campuses without a corresponding degree
#	In the future I would like to consolidate duplicate institutions using fuzzy matching,
#	With the goal of measuring wages and other labor market characteristics by high school
#	Imputing level of educational attainment using deg_mode + start and end dates
user_ed05 <- user_ed02 %>%
  left_join(user_ed04, user_ed02, by = "campus") %>%
  drop_na(deg_mode) %>%
  select(-sample_size) %>%
  relocate(deg_mode, .before = field) %>%
  mutate(dur = as.duration(enddate - startdate)) %>%
  relocate(dur, .before = degree)


#	Few high schools offer other degrees
#	So we can attach diploma to any high school campus with no degree listed 
user_ed06 <- user_ed05 %>%
  mutate(degree = ifelse(is.na(degree)==TRUE,deg_mode,degree)) %>%
  select(-deg_mode) %>%
  mutate(across(where(is.integer),as.factor))
user_ed06$degree <- ordered(recode(user_ed06$degree,'1' = "High.School",'2' = "Associate",'3' = "Bachelor",'4' = "Master",'5' = "MBA",'6' = "Doctor"))
max_ed = user_ed06 %>%
  group_by(user_id) %>%
  summarize(max_ed = max(degree))

#	Creates binary variable for transfer students
transfer = user_ed06 %>%
  filter(degree == "Bachelor") %>%
  filter(is.na(startdate)==FALSE) %>%
  group_by(user_id) %>%
  tally() %>%
  mutate(transfer = ifelse(n>1,1,0)) %>%
  select(-n)

#	Next we want to impute ages for users
#	1,570,558/2,118,307 (74.1%) of users include either high school or bachelors completion dates
colbirthyear = user_ed06 %>%
  filter(degree == "Bachelor") %>%
  filter(is.na(startdate)==FALSE) %>%
  group_by(user_id) %>%
  mutate(birthyear = year(startdate)-18) %>%
  summarise(birthyear = min(birthyear))
hsbirthyear = user_ed06 %>%
  filter(degree == "High.School") %>%
  filter(is.na(enddate)==FALSE) %>%
  group_by(user_id) %>%
  mutate(birthyear = year(min(enddate))-18) %>%
  summarise(birthyear = min(birthyear))
birthyear = colbirthyear %>%
  left_join(hsbirthyear, by = "user_id") %>%
  group_by(user_id) %>%
  mutate(truebirthyear = min(birthyear.x,birthyear.y,na.rm=TRUE)) %>%
  select(-c(birthyear.x,birthyear.y)) %>%
  rename(birthyear = truebirthyear)

#	1,854,425 (56.4%) require no imputation for age
#	1,133,439 (34.5%) require imputation of birth year with name
#	296,703 (9.0%) require imputation of birth year via work history
birth_ed = max_ed %>%
  full_join(birthyear, by = "user_id") %>%
  full_join(user, by = "user_id") %>%
  select(-c(prestige,highest_degree)) %>%
  rename(highest_degree = max_ed)

#	Filters all observations lacking a birth year but not a name
#	SHOULD INCLUDE STARTDATE OF EARLIEST JOB
name_imp = birth_ed %>%
  filter(is.na(birthyear)==TRUE) %>%
  filter(is.na(firstname)==FALSE)

#	If the name is a symbol, space, or single letter we cannot use it as a name
#	Remove all punctuation and symbols from campus names
name_imp$firstname = str_replace_all(user_ed01$campus,"[:digit:]","")
user_ed01$campus = str_replace_all(user_ed01$campus,"[:symbol:]","")

#	Remove white space and convert all names to lowercase
#	When every campus is lowercase, identical strings with distinct
#	capitalization are not grouped separately
name_imp <- name_imp %>%
  mutate(across(where(is.character),str_trim)) %>%
  mutate(across(where(is.character),str_squish)) %>%
  mutate(campus = tolower(campus))


name_rank = name_imp %>%
  group_by(firstname) %>%
  summarize(n = n())


#	Filters all observations lacking both a birth year and a name	
work_imp = birth_ed %>%
  filter(is.na(birthyear)==TRUE) %>%
  filter(is.na(firstname)==TRUE)

user01 = user

endtime = Sys.time()
print(endtime - starttime)