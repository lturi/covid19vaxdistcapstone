## This script is to set up cleaning routines to pull all vaccination data for PA into one GIS-linkable file

## Daily update is available for total population here: https://data.pa.gov/Health/COVID-19-Vaccinations-by-Residence-Current-County-/gcnb-epac
## API: https://data.pa.gov/resource/gcnb-epac.csv
## ~ Weekly update for ethnicity updates are available here:  https://data.pa.gov/Health/COVID-19-Vaccinations-by-Ethnicity-Current-County-/7ruj-m7k6
## API: https://data.pa.gov/resource/7ruj-m7k6.csv
## ~ Weekly update for racial updates are available here: https://data.pa.gov/Health/COVID-19-Vaccinations-by-Race-Current-County-Healt/x5z9-57ub
## API: https://data.pa.gov/resource/x5z9-57ub.csv
## ~ Weekly update for age range are available here: https://data.pa.gov/Health/COVID-19-Vaccinations-by-Age-Group-Current-County-/niuh-2xe3
## API: https://data.pa.gov/resource/niuh-2xe3.csv
## ~ Weekly update for gender are available here: https://data.pa.gov/Health/COVID-19-Vaccinations-by-Gender-Current-County-Hea/jweg-3ezy
## API: https://data.pa.gov/resource/jweg-3ezy.csv
## Census data are static and available here in cc-est2019-alldata-42.csv for denominators for rates or percentages. PVI uses 2018 data (year code 11)
## file layout is here: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/cc-est2019-alldata.pdf

## This script assumes all files noted above are available in the working directory, and are ready for cleaning into a single file
## that can be brought into QGIS
## TODO - automate pulling updates

## For POC/Capstone, we are looking at targeted dates to visualize against PVI in each county
## TODO - make this available daily/weekly as cadence allows.

library(stringr)
library(readr)
library(gdata)
library(plyr)
library(dplyr)

## make dir to place source files since they cannot be retrieved again
today<-format(Sys.time(),"%C%y%m%d")
working_date <- "20210415"
#system(sprintf("mkdir %s", today))

## get denominators for race/ethnicity/age over 65 to match PVI indicators
## For race, use BAC_FEMALE and BAC_MALE (Black alone or in combination)
## For Native American, use IAC_FEMALE and IAC_MALE (American Indian and Alaska Native alone or in combination)
## For Hispanic, use H_MALE and H_FEMALE (All Hispanic)
## For age, use age codes 14-18 aggregated to get 65+ 
## Gender is not used in PVI, otherwise, these are the only demographics we have 
## vaccination data on in PA

df.census<-data.frame(read_csv("cc-est2019-alldata-42.csv"))
keeps <- c("CTYNAME","YEAR","AGEGRP","TOT_POP","NHWA_MALE","NHWA_FEMALE","NHAAC_MALE","NHAAC_FEMALE",
           "NHNAC_MALE","NHNAC_FEMALE","NHBAC_MALE","NHBAC_FEMALE","NHIAC_MALE","NHIAC_FEMALE","H_MALE","H_FEMALE")
df.census<-df.census[ keeps]
df.census<-df.census[df.census$YEAR==11,]
df.census$CTYNAME <- trim(str_to_upper(df.census$CTYNAME,locale="en"))
df.census$CTYNAME<-sub('[" "][^.]+$', '', df.census$CTYNAME)
df.census$BAC_TOTAL <- df.census$NHBAC_FEMALE+df.census$NHBAC_MALE
df.census$IAC_TOTAL <- df.census$NHIAC_FEMALE+df.census$NHIAC_MALE
df.census$H_TOTAL <- df.census$H_FEMALE+df.census$H_MALE
df.census$NHWA_TOTAL <- df.census$NHWA_MALE + df.census$NHWA_FEMALE
df.census$NHAAC_TOTAL <- df.census$NHAAC_MALE + df.census$NHAAC_FEMALE
df.census$NHNAC_TOTAL <- df.census$NHNAC_MALE + df.census$NHNAC_FEMALE
df.census<-subset(df.census,select=-c(YEAR,NHBAC_MALE,NHBAC_FEMALE,NHIAC_MALE,NHIAC_FEMALE,H_MALE,H_FEMALE,
                                      NHWA_FEMALE,NHWA_MALE,NHAAC_FEMALE,NHAAC_MALE,NHNAC_MALE,NHNAC_FEMALE))

df.u65 <- df.census[df.census$AGEGRP>=4 & df.census$AGEGRP<=13,]
df.u65.agg <- aggregate(. ~ CTYNAME, data=df.u65, sum)
df.u65.agg <- subset(df.u65.agg,select=c(CTYNAME,TOT_POP))
colnames(df.u65.agg)<-c("county","TOT_U65")

df.age <- df.census[df.census$AGEGRP>=14 & df.census$AGEGRP <=18, ]
df.age.agg<-aggregate(. ~ CTYNAME, data = df.age, sum)
df.age.agg<-subset(df.age.agg,select=c(CTYNAME,TOT_POP))
colnames(df.age.agg)<-c("county","O65_TOT_POP")
df.race_ethnicity<- df.census[df.census$AGEGRP==0,]
df.race_ethnicity<-subset(df.race_ethnicity, select=-c(AGEGRP))

df.denom <- inner_join(df.race_ethnicity,df.age.agg, by = c("CTYNAME" = "county"))
df.denom <- inner_join(df.denom,df.u65.agg, by = c("CTYNAME" = "county"))


colnames(df.denom)<- c("county","TOT_POP","TOT_BAC","TOT_IAC","TOT_H","TOT_NHWA",
                       "TOT_NHAAC","TOT_NHNAC","TOT_O65","TOT_U65")



## get total counts vaccine data
df.all<-data.frame(read_csv(sprintf("%s/COVID-19_Vaccinations_by_Residence_Current_County_Health.csv",working_date)))
df.all$county <- str_to_upper(df.all$county)
df.all <- df.all[order(df.all$county),]
df.all<-join(df.all,df.denom,
               type = "inner", by=c("county"),match="all")
#df.all$chg_pop_since_2018 = df.all$County.Population - df.all$TOT_POP
#df.all$pct_chg = signif(df.all$chg_pop_since_2018/df.all$TOT_POP,digits=4)

## add in vaccination rates for those > 65, 16-64 - PA breaks age groups down fairly
## granularly, but for the purposes of this, we're only looking at these two groups
## as PA does not have age-related vaccination policies other than early on in distribution
## This data may need to be more granular as time goes on, however, so a next 
## step would be to pull denominator data for each corresponding age group, and
## look at those metrics.

df.age <- data.frame(read_csv(sprintf("%s/COVID-19_Vaccinations_by_Age_Group_Current_County_Health.csv",working_date)))
df.age[is.na(df.age)] <- 0
df.age <- data.frame( df.age$county, U65Partial = apply(df.age[2:11], 1, sum) ,
                      O65Partial = apply(df.age[12:20], 1, sum),
                      U65Full = apply(df.age[21:30],1,sum),
                      O65Full = apply(df.age[31:39],1,sum))
colnames(df.age) <- c("county","U65Partial","O65Partial","U65Full","O65Full")
df.all<-join(df.all,df.age,
             type = "inner", by=c("county"),match="all")

df.all$Rate.O65Partial.Per.100K <-df.all$O65Partial/df.all$TOT_O65 *100000
df.all$Rate.O65Full.Per.100K <-df.all$O65Full/df.all$TOT_O65 *100000
df.all$Rate.U65Partial.Per.100K <-df.all$U65Partial/df.all$TOT_U65 *100000
df.all$Rate.U65Full.Per.100K <-df.all$U65Full/df.all$TOT_U65 *100000

## add in vaccination rates for ethnicity (hispanic)

df.eth <- data.frame(read_csv(sprintf("%s/COVID-19_Vaccinations_by_Ethnicity_Current_County_Health.csv",working_date)))
df.eth[is.na(df.eth)] <- 0
df.eth <- subset(df.eth,select=c(zip_county_desc,partially_covered_hispanic,fully_covered_hispanic))
df.all <-inner_join(df.all, df.eth, by = c("county" = "zip_county_desc"))
df.all$Rate.HispanicPartial.Per.100K <-df.all$partially_covered_hispanic/df.all$TOT_H *100000
df.all$Rate.HispanicFull.Per.100K <-df.all$fully_covered_hispanic/df.all$TOT_H *100000

## add in vaccination rates for race (black, native american)
#`County Name` = col_character(),
#`Partially Covered African American` = col_double(),
#`Partially Covered Asian` = col_double(),
#`Partially Covered Native American` = col_double(),
#`Partially Covered Pacific Islander` = col_double(),
#`Partially Covered Multiple Other` = col_double(),
#`Partially Covered White` = col_double(),
#`Partially Covered  Unknown` = col_double(),
#`Fully Covered African American` = col_double(),
#`Fully Covered Asian` = col_double(),
#`Fully Covered Native American` = col_double(),
#`Fully Covered Pacific Islander` = col_double(),
#`Fully Covered Multiple Other` = col_double(),
#`Fully Covered White` = col_double(),
#`Fully Covered Unknown` = col_double()

df.race <- data.frame(read_csv(sprintf("%s/COVID-19_Vaccinations_by_Race_Current_County_Health.csv",working_date)))
df.race <- subset(df.race,select=c(county,partially_covered_african_american,
                                   partially_covered_native_american,
                                   partially_covered_asian,
                                   partially_covered_pacific_islander,
                                   partially_covered_white,
                                   fully_covered_african_american,
                                   fully_covered_native_american,
                                   fully_covered_asian,
                                   fully_covered_pacific_islander,
                                   fully_covered_white
                                   ))
df.race[is.na(df.race)] <- 0
df.all <-inner_join(df.all, df.race, by = c("county"))
df.all$Rate.BlackPartial.Per.100K <-df.all$partially_covered_african_american/df.all$TOT_BAC *100000
df.all$Rate.BlackFull.Per.100K <-df.all$fully_covered_african_american/df.all$TOT_BAC *100000
df.all$Rate.NAPartial.Per.100K <-df.all$partially_covered_native_american/df.all$TOT_IAC *100000
df.all$Rate.NAFull.Per.100K <-df.all$fully_covered_native_american/df.all$TOT_IAC *100000
df.all$Rate.AsianPartial.Per.100K <-df.all$partially_covered_asian/df.all$TOT_NHAAC *100000
df.all$Rate.AsianFull.Per.100K <-df.all$fully_covered_asian/df.all$TOT_NHAAC *100000
df.all$Rate.PIPartial.Per.100K <-df.all$partially_covered_pacific_islander/df.all$TOT_NHNAC *100000
df.all$Rate.PIFull.Per.100K <-df.all$fully_covered_pacific_islander/df.all$TOT_NHNAC *100000
df.all$Rate.WhitePartial.Per.100K <-df.all$partially_covered_white/df.all$TOT_NHWA *100000
df.all$Rate.WhiteFull.Per.100K <-df.all$fully_covered_white/df.all$TOT_NHWA *100000
df.all[is.na(df.all)] <- 0

df.all$Compare.Partial.O65 <- df.all$Rate.O65Partial.Per.100K >= df.all$ratepartiallycovered
df.all$Compare.Full.O65 <- df.all$Rate.O65Full.Per.100K >= df.all$ratefullycovered
df.all$Compare.Partial.U65 <- df.all$Rate.U65Partial.Per.100K >= df.all$ratepartiallycovered
df.all$Compare.Full.U65 <- df.all$Rate.U65Full.Per.100K >= df.all$ratefullycovered
df.all$Compare.Partial.Hispanic <-df.all$Rate.HispanicPartial.Per.100K >= df.all$ratepartiallycovered
df.all$Compare.Full.Hispanic <- df.all$Rate.HispanicFull.Per.100K >= df.all$ratefullycovered
df.all$Compare.Partial.Black <-df.all$Rate.BlackPartial.Per.100K >= df.all$ratepartiallycovered
df.all$Compare.Full.Black <- df.all$Rate.BlackFull.Per.100K >= df.all$ratefullycovered
df.all$Compare.Partial.NA <-df.all$Rate.NAPartial.Per.100K >= df.all$ratepartiallycovered
df.all$Compare.Full.NA <- df.all$Rate.NAFull.Per.100K >= df.all$ratefullycovered
df.all$Compare.Partial.PA <-df.all$Rate.PIPartial.Per.100K >= df.all$ratepartiallycovered
df.all$Compare.Full.PA <- df.all$Rate.PIFull.Per.100K >= df.all$ratefullycovered
df.all$Compare.Partial.Asian <-df.all$Rate.AsianPartial.Per.100K >= df.all$ratepartiallycovered
df.all$Compare.Full.Asian <- df.all$Rate.AsianFull.Per.100K >= df.all$ratefullycovered
df.all$Compare.Partial.White <-df.all$Rate.WhitePartial.Per.100K >= df.all$ratepartiallycovered
df.all$Compare.Full.White <- df.all$Rate.WhiteFull.Per.100K >= df.all$ratefullycovered

remove(df.age,df.age.agg,df.census,df.denom,df.eth,df.race,df.race_ethnicity,df.u65,df.u65.agg,keeps)

#df.all<-subset(df.all,select=-c(County.Name))
# dump full set for review in Excel
write_csv(df.all,sprintf("output/%s_vaccinaton_excel.csv",working_date),quote=FALSE)
# remove what we don't want in qgis
df.all<-subset(df.all,select=-c(county_population,TOT_POP,TOT_BAC,TOT_IAC,TOT_H,
                                TOT_NHWA,TOT_NHAAC,TOT_NHNAC,TOT_O65,TOT_U65,
                                U65Partial,U65Full,partially_covered_hispanic,fully_covered_hispanic,partially_covered_african_american,
                                partially_covered_native_american,fully_covered_african_american,fully_covered_native_american,
                                partially_covered_pacific_islander,fully_covered_pacific_islander,partially_covered_asian,fully_covered_asian,
                                partially_covered_white,fully_covered_white,
                                O65Partial,O65Full))
write_csv(df.all,sprintf("output/%s_vaccinaton_qgis.csv",working_date),quote=FALSE)

