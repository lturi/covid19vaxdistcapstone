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
working_date <- "20210504"
#system(sprintf("mkdir %s", today))

## get denominators for race/ethnicity/age over 65 to match PVI indicators
## For race, use BAC_FEMALE and BAC_MALE (Black alone or in combination)
## For Native American, use IAC_FEMALE and IAC_MALE (American Indian and Alaska Native alone or in combination)
## For Hispanic, use H_MALE and H_FEMALE (All Hispanic)
## For age, use age codes 14-18 aggregated to get 65+ 
## Gender is not used in PVI, otherwise, these are the only demographics we have 
## vaccination data on in PA

df.census<-data.frame(read_csv("cc-est2019-alldata-42.csv"))
keeps <- c("CTYNAME","YEAR","AGEGRP","TOT_POP","WA_MALE","WA_FEMALE","AAC_MALE","AAC_FEMALE",
           "NAC_MALE","NAC_FEMALE","BAC_MALE","BAC_FEMALE","IAC_MALE","IAC_FEMALE","H_MALE","H_FEMALE","NH_MALE","NH_FEMALE")
df.census<-df.census[ keeps]
df.census<-df.census[df.census$YEAR==11,]
df.census<-df.census[df.census$AGEGRP==0,]
df.census$CTYNAME <- trim(str_to_upper(df.census$CTYNAME,locale="en"))
df.census$CTYNAME<-sub('[" "][^.]+$', '', df.census$CTYNAME)
df.census$BAC_TOTAL <- df.census$BAC_FEMALE+df.census$BAC_MALE #black
df.census$IAC_TOTAL <- df.census$IAC_FEMALE+df.census$IAC_MALE # american indian/alaskan native
df.census$WA_TOTAL <- df.census$WA_MALE + df.census$WA_FEMALE # white
df.census$AAC_TOTAL <- df.census$AAC_MALE + df.census$AAC_FEMALE #asian
df.census$NAC_TOTAL <- df.census$NAC_MALE + df.census$NAC_FEMALE #hawaiian native/pacific islander

df.census$H_TOTAL <- df.census$H_FEMALE+df.census$H_MALE # hispanic
df.census$NH_TOTAL <- df.census$NH_FEMALE+df.census$NH_MALE # non-hispanic

df.census<-subset(df.census,select=-c(YEAR,BAC_MALE,BAC_FEMALE,IAC_MALE,IAC_FEMALE,H_MALE,H_FEMALE,
                                      WA_FEMALE,WA_MALE,AAC_FEMALE,AAC_MALE,NAC_MALE,NAC_FEMALE,NH_MALE,NH_FEMALE))
df.census$PCT_WHITE<-df.census$WA_TOTAL/df.census$TOT_POP
df.census$PCT_BLACK<-df.census$BAC_TOTAL/df.census$TOT_POP
df.census$PCT_ASIAN<-df.census$AAC_TOTAL/df.census$TOT_POP
df.census$PCT_NA<-df.census$IAC_TOTAL/df.census$TOT_POP
df.census$PCT_PI<-df.census$NAC_TOTAL/df.census$TOT_POP

df.census$PCT_HISP<-df.census$H_TOTAL/df.census$TOT_POP
df.census$PCT_NHISP<-df.census$NH_TOTAL/df.census$TOT_POP
df.census_keep<-df.census[,c("CTYNAME","PCT_WHITE","PCT_BLACK","PCT_ASIAN","PCT_NA","PCT_PI","PCT_HISP","PCT_NHISP",
                             "BAC_TOTAL","WA_TOTAL","IAC_TOTAL","AAC_TOTAL","NAC_TOTAL","H_TOTAL","NH_TOTAL","TOT_POP")]
remove(df.census)



## get total doses allocated for vaccine data
df.alloc<-data.frame(read_csv(sprintf("output/%s_total_vaccine_doses_allocated_by_county.csv",working_date)))
df.alloc$county<-str_to_upper(df.alloc$county)
#df.all<-join(df.all,df.alloc, type="inner",by=c("county"),match="all")

## get expected porportion of doses based on county allocation and percent demographic
df.alloc <- inner_join(df.alloc,df.census_keep, by = c("county" = "CTYNAME"))
df.alloc$EXP_WHITE<-(df.alloc$total * df.alloc$PCT_WHITE)
df.alloc$EXP_BLACK<-(df.alloc$total * df.alloc$PCT_BLACK)
df.alloc$EXP_NA<-(df.alloc$total * df.alloc$PCT_NA)
df.alloc$EXP_ASIAN<-(df.alloc$total * df.alloc$PCT_ASIAN)
df.alloc$EXP_PI<-(df.alloc$total * df.alloc$PCT_PI)
df.alloc$EXP_HISP<-(df.alloc$total * df.alloc$PCT_HISP)
df.alloc$EXP_NHISP<-(df.alloc$total * df.alloc$PCT_NHISP)
remove(df.census_keep)

## get total doses administered for vaccine data
df.all<-data.frame(read_csv(sprintf("%s/gcnb-epac.csv",working_date)))
df.all$total_adm<-df.all$partiallycovered+df.all$fullycovered
df.all$county <- str_to_upper(df.all$county)
df.all <- df.all[order(df.all$county),]
#df.all<-join(df.all,df.denom,
#             type = "inner", by=c("county"),match="all")
df.adm<-subset(df.all,select=c("county","total_adm"))
remove(df.all)

## add in vaccination rates for ethnicity (hispanic)

df.eth <- data.frame(read_csv(sprintf("%s/7ruj-m7k6.csv",working_date)))
df.eth[is.na(df.eth)] <- 0
df.eth$adm_hisp<-df.eth$partially_covered_hispanic+df.eth$fully_covered_hispanic
df.eth$adm_nhisp <- df.eth$partially_covered_nothispanic+df.eth$fully_covered_not_hispanic
df.eth$adm_unk_eth <- df.eth$partially_covered_unknown+df.eth$fully_covered_unknown
df.eth_all <- subset(df.eth,select=c(zip_county_desc,adm_hisp,adm_nhisp,adm_unk_eth))
df.alloc <-inner_join(df.alloc, df.eth_all, by = c("county" = "zip_county_desc"))
df.alloc<-inner_join(df.alloc,df.adm,by=c("county"))
remove(df.eth,df.eth_all)

## add in vaccination rates for race (black, native american, white, asian, native hawaiian/pacific islander)
#`County Name` = col_character(),

df.race <- data.frame(read_csv(sprintf("%s/x5z9-57ub.csv",working_date)))
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
df.race$adm_white<-df.race$partially_covered_white+df.race$fully_covered_white
df.race$adm_black<-df.race$partially_covered_african_american+df.race$fully_covered_african_american
df.race$adm_na<-df.race$partially_covered_native_american+df.race$fully_covered_native_american
df.race$adm_asian<-df.race$partially_covered_asian+df.race$fully_covered_asian
df.race$adm_pi<-df.race$partially_covered_pacific_islander+df.race$fully_covered_pacific_islander
df.race_all<-subset(df.race,select=c("county","adm_white","adm_black","adm_na","adm_asian","adm_pi"))
df.alloc <-inner_join(df.alloc, df.race_all, by = c("county"))
remove(df.race,df.race_all,df.adm)

df.alloc$rate_total_adm <- df.alloc$total_adm/df.alloc$TOT_POP * 100000
df.alloc$rate_white<- df.alloc$adm_white/df.alloc$WA_TOTAL * 100000
df.alloc$rate_black<- df.alloc$adm_black/df.alloc$BAC_TOTAL * 100000
df.alloc$rate_na<- df.alloc$adm_na/df.alloc$IAC_TOTAL * 100000
df.alloc$rate_asian<- df.alloc$adm_asian/df.alloc$AAC_TOTAL * 100000
df.alloc$rate_pi<- df.alloc$adm_pi/df.alloc$NAC_TOTAL * 100000
df.alloc$rate_hisp<- df.alloc$adm_hisp/df.alloc$H_TOTAL * 100000
df.alloc$rate_nhisp<- df.alloc$adm_nhisp/df.alloc$NH_TOTAL * 100000

df.alloc$rate_total_exp <- df.alloc$total/df.alloc$TOT_POP * 100000
df.alloc$exp_rate_white<- df.alloc$EXP_WHITE/df.alloc$WA_TOTAL * 100000
df.alloc$exp_rate_black<- df.alloc$EXP_BLACK/df.alloc$BAC_TOTAL * 100000
df.alloc$exp_rate_na<- df.alloc$EXP_NA/df.alloc$IAC_TOTAL * 100000
df.alloc$exp_rate_asian<- df.alloc$EXP_ASIAN/df.alloc$AAC_TOTAL * 100000
df.alloc$exp_rate_pi<- df.alloc$EXP_PI/df.alloc$NAC_TOTAL * 100000
df.alloc$exp_rate_hisp<- df.alloc$EXP_HISP/df.alloc$H_TOTAL * 100000
df.alloc$exp_rate_nhisp<- df.alloc$EXP_NHISP/df.alloc$NH_TOTAL * 100000

attach(df.alloc)
## only a percentage of total allocated were actually administered, which affects expected rates
df.alloc$pct_adm<-total_adm/total

##set adjusted expected rates
attach(df.alloc)
df.alloc$adj_exp_white<-pct_adm * EXP_WHITE
df.alloc$adj_exp_black<-pct_adm * EXP_BLACK
df.alloc$adj_exp_na<-pct_adm * EXP_NA
df.alloc$adj_exp_asian<-pct_adm * EXP_ASIAN
df.alloc$adj_exp_pi<-pct_adm * EXP_PI
df.alloc$adj_exp_hisp<-pct_adm * EXP_HISP
df.alloc$adj_exp_nhisp<-pct_adm * EXP_NHISP

## set proportion of actual to expected doses based on original expected 
df.alloc$pae_white<-adm_white/EXP_WHITE
df.alloc$pae_black<-adm_black/EXP_BLACK
df.alloc$pae_na<-adm_na/EXP_NA
df.alloc$pae_asian<-adm_asian/EXP_ASIAN
df.alloc$pae_pi<-adm_pi/EXP_PI
df.alloc$pae_hisp<-adm_hisp/EXP_HISP
df.alloc$pae_nhisp<-adm_nhisp/EXP_NHISP



## calculated variance between actual and expected
attach(df.alloc)
df.alloc$diff_ae_white<-adm_white - EXP_WHITE
df.alloc$diff_ae_black<-adm_black - EXP_BLACK
df.alloc$diff_ae_na<-adm_na - EXP_NA
df.alloc$diff_ae_asian<-adm_asian - EXP_ASIAN
df.alloc$diff_ae_pi<-adm_pi - EXP_PI
df.alloc$diff_ae_hisp<-adm_hisp - EXP_HISP
df.alloc$diff_ae_nhisp<-adm_nhisp - EXP_NHISP

## calculated variance percent from adjusted expected
attach(df.alloc)
df.alloc$pd_ae_white<-((adm_white / EXP_WHITE) -1)*100
df.alloc$pd_ae_black<-((adm_black / EXP_BLACK) -1)*100
df.alloc$pd_ae_na<-((adm_na / EXP_NA) -1)*100
df.alloc$pd_ae_asian<-((adm_asian / EXP_ASIAN) -1)*100
df.alloc$pd_ae_pi<-((adm_pi / EXP_PI) -1)*100
df.alloc$pd_ae_hisp<-((adm_hisp / EXP_HISP) -1)*100
df.alloc$pd_ae_nhisp<-((adm_nhisp / EXP_NHISP) -1)*100

df.alloc$arcgis_county<-sprintf("%s County, Pennsylvania", str_to_title(df.alloc$county))

options(scipen = 999)
plot(rate_white ~ pd_ae_white, data=df.alloc, ylab="Rate Doses Administered",
     xlab="Percent Difference Actual vs Expected Doses", main="Dose Summary - White\n (Rates per 100K Population)")
dev.copy(png,sprintf('../images/dosesummary_white_%s.png',working_date))
dev.off()

plot(rate_black ~ pd_ae_black, data=df.alloc, ylab="Rate Doses Administered",
     xlab="Percent Difference Actual vs Expected Doses", main="Dose Summary - Black\n (Rates per 100K Population)")
dev.copy(png,sprintf('../images/dosesummary_black_%s.png',working_date))
dev.off()

plot(rate_na ~ pd_ae_na, data=df.alloc, ylab="Rate Doses Administered",
     xlab="Percent Difference Actual vs Expected Doses", main="Dose Summary - American Indian/Alaskan Native\n (Rates per 100K Population)")
dev.copy(png,sprintf('../images/dosesummary_na_%s.png',working_date))
dev.off()

plot(rate_asian ~ pd_ae_asian, data=df.alloc, ylab="Rate Doses Administered",
     xlab="Percent Difference Actual vs Expected Doses", main="Dose Summary - Asian\n (Rates per 100K Population)")
dev.copy(png,sprintf('../images/dosesummary_asian_%s.png',working_date))
dev.off()

plot(rate_pi ~ pd_ae_pi, data=df.alloc, ylab="Rate Doses Administered",
     xlab="Percent Difference Actual vs Expected Doses", main="Dose Summary - Native Hawaiian/Pacific Islander\n (Rates per 100K Population)")
dev.copy(png,sprintf('../images/dosesummary_pi_%s.png',working_date))
dev.off()

plot(rate_hisp ~ pd_ae_hisp, data=df.alloc,ylab="Rate Doses Administered",
     xlab="Percent Difference Actual vs Expected Doses", main="Dose Summary - Hispanic\n (Rates per 100K Population)")
dev.copy(png,sprintf('../images/dosesummary_hispanic_%s.png',working_date))
dev.off()

plot(rate_nhisp ~ pd_ae_nhisp, data=df.alloc, ylab="Rate Doses Administered",
     xlab="Percent Difference Actual vs Expected Doses", main="Dose Summary - Non-Hispanic\n (Rates per 100K Population)")
dev.copy(png,sprintf('../images/dosesummary_non-hispanic_%s.png',working_date))
dev.off()


write_csv(df.alloc,sprintf("output/%s_vaccinaton_expvactual.csv",working_date),quote=FALSE)

