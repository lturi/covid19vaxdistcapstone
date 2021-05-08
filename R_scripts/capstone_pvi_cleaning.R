## This script is to create a working Toxpi data model with headers so that it can be run with the same settings
## used in the CDC's PVI dashboard, but adding Hispanic population data to the Population Demographics slice as
## that is (currently as of 4/8/2021 not included in the CDC model)

## This script relies on having already cloned and pulled the latest Model data from the official PVI repo: https://github.com/COVID19PVI/data
## You will need to set your model home in the variables below as well as the working date that you want to use model 
## data from

## Census data are static and available here in cc-est2019-alldata-42.csv for denominators for rates or percentages. PVI uses 2018 data (year code 11)
## file layout is here: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/cc-est2019-alldata.pdf

## This script assumes all files noted above are available in the working directory, and are ready for cleaning into a single file
## This file is then merged with the appropriate model definition header, and between the two are ready to load as is
## into Toxpi
## TODO - automate pulling updates

#install.packages("stringr")                 
#install.packages("dplyr")
library(dplyr)
library(readr)
library(stringr)                              
library(gdata)
library(plyr)

# set variables before running

working_date <- "20210504"
pvi_model_home <- "C:\\Users\\fiddl\\Documents\\GitHub\\pvi\\data\\Model11.2.1"
today<-format(Sys.time(),"%C%y%m%d")

# pull just the data from PA from the PVI data repository from the CDC
m_data2 <- read_csv(sprintf("%s/Model_11.2.1_%s_data.csv",pvi_model_home,working_date), skip=12)
#cols(
#  row = col_double(),
#  sid = col_character(),
#  casrn = col_double(),
#  name = col_character(),
#  Spread = col_double(),
#  Sick = col_double(),
#  PctCases = col_double(),
#  Cases = col_double(),
#  CountyCaseRate = col_double(),
#  StateCaseRate = col_double(),
#  PctDeaths = col_double(),
#  Deaths = col_double(),
#  PctDeaths2 = col_double(),
#  CountyDeathRate = col_double(),
#  StateDeathRate = col_double(),
#  StatePctTested = col_double(),
#  StateTestRate = col_double(),
#  SVISocioeconomic = col_double(),
#  SVIMinority = col_double(),
#  SVIHousing = col_double(),
#  PctBeds = col_double(),
#  Mobility = col_double(),
#  PctNoIns = col_double(),
#  PctGE65 = col_double(),
#  DaytimePopDensity = col_double(),
#  PrematureDeath = col_double(),
#  Smoking = col_double(),
#  AirPollution = col_double(),
#  Diabetes = col_double(),
#  Traffic = col_double(),
#  DistancingGrade = col_double(),
#  Obesity = col_double(),
#  PctBlack = col_double(),
#  PctNative = col_double()
#)
county_state1 <- t(data.frame(strsplit(m_data2$name,",")))
m_data2$state <-str_to_upper(county_state1[,1],locale="en")
m_data2$county <-str_to_upper(county_state1[,2],locale="en")
m_data2<-data.frame(m_data2)


# add hispanic ethnicity to the dataset (not sure why it's missing when NIH documentation shows Hispanic 
# population bubbles up as a vulnerability factor)

census <- read_csv("cc-est2019-alldata.csv")

#summary(pa_census_2010_2019)
# we want just 2018 to match what was used for the other demographic categories
# by the CDC, total pop, and just the totals for Hispanic Male, Hispanic Female
# (summed, then % so that it matches the method
# for percent Black and percent Native American)
hispanic <- data.frame(census[census$AGEGRP==0 & census$YEAR==11,])

attach(hispanic)
hispanic<-hispanic[,c("STNAME","CTYNAME","H_MALE","H_FEMALE","TOT_POP")]
hispanic$H <- H_FEMALE+H_MALE
hispanic$percentHispanic <- hispanic$H / hispanic$TOT_POP
attach(hispanic)
hispanic$CTYNAME<-trim(str_to_upper(sub('[" "][^.]+$', '', CTYNAME)))
hispanic$STNAME<- trim(str_to_upper(STNAME,locale="en"))
hispanic <- hispanic[,c("STNAME","CTYNAME","percentHispanic")]
colnames(hispanic) <-c("state","county","PctHisp")
m_data2$county<-trim(m_data2$county)
m_data2$county<-str_to_upper(m_data2$county, locale="en")
merged<-join(m_data2, hispanic,
     type = "inner", by=c("state","county"),match="all")

# Now that we've added the appropriate columns, we can clean up the file so it's ready for heads to be applied and run in Toxpi
df <- subset(merged,select=-c(state,county))
df <-df %>% tibble %>% 
  select("row","sid","casrn","name", sort(colnames(.)))

df <-df %>% distinct(name, .keep_all = TRUE)

write_csv(df,"capstone_model.csv")
system(sprintf("type toxpi_model_definition.txt capstone_model.csv > output\\%s_data.csv",working_date) )
system("del capstone_model.csv")
remove(census,county_state1,hispanic,df,m_data2,merged,pvi_model_home,today,working_date)
