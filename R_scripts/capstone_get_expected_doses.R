##
# This script aggregates dose allocations by county
#
# First dose allocation to providers
#https://data.pa.gov/resource/qsii-pka7.csv?$$app_token={app_token}&$limit=10000
# Second dose allocation to providers
# https://data.pa.gov/resource/8jae-5d8i.csv?$$app_token={app_token}&$limit=8000
# Doses allocated via federal program
# https://data.pa.gov/resource/vxbs-jbjq.csv?$$app_token={app_token}&$limit=8000
#
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
library(tidyr)

## make dir to place source files since they cannot be retrieved again
today<-format(Sys.time(),"%C%y%m%d")
working_date <- "20210504"
#system(sprintf("mkdir %s", today))

df.f<-data.frame(read_csv(sprintf("%s/qsii-pka7.csv",working_date)))
df.s<-data.frame(read_csv(sprintf("%s/8jae-5d8i.csv",working_date)))
df.r<-data.frame(read_csv(sprintf("%s/vxbs-jbjq.csv",working_date)))


df.f$county_name<-ifelse(df.f$city=="Benton","Columbia",df.f$county_name)
df.f$county_name<-ifelse(df.f$city=="Jermyn","Lackawanna",df.f$county_name)

to_get_county<-unique(subset(df.f,select=c(city,county_name)))
to_get_county<-to_get_county[order(to_get_county$city,to_get_county$county_name),]
to_get_county<-to_get_county[!duplicated(to_get_county$city),]

df.f<-merge(x = df.f, y = to_get_county, by = "city", all.x = TRUE)


df.f$county<-df.f$county_name.y

df.f<-df.f[df.f$federal_allocation == 'FALSE',]
df.f<-df.f[,c("county","volume_ordered")]
df.f$volume_ordered<-ifelse(is.na(df.f$volume_ordered),0,df.f$volume_ordered)

c_df.f<-df.f %>%
  group_by(county) %>%
  summarize(total=sum(volume_ordered))

df.s<-df.s[,c("county_desc","total_volume_ordered")]
colnames(df.s)<-c("county","doses")
df.s[is.na(df.s)]<-0


c_df.s<-df.s %>%
  group_by(county) %>%
  summarize(total=sum(doses))

df.r<-df.r[,c("county","doses")]
df.r$county<-gsub("Tioa","Tioga",df.r$county)
df.r$county<-gsub("Venego","Venango",df.r$county)
df.r$county<-gsub("McKean","Mckean",df.r$county)
df.r$county<-gsub("Lzuerne","Luzerne",df.r$county)
df.r$county<-gsub("Buck$","Bucks",df.r$county)
df.r$county<-gsub("North Hampton","Northampton",df.r$county)

df.sp<-df.r
df.sp<-separate(
  df.r,
  county,
  c("county","county_spl"),
  sep = " ",
  remove = FALSE,
  convert = FALSE,
  extra = "warn",
  fill = "warn",
)

df.sp<-subset(df.sp,select=-c(county_spl))
df.r<-df.sp
df.r[is.na(df.r)]<-0

c_df.r<-df.r %>%
  group_by(county) %>%
  summarize(total=sum(doses))

df.all<-bind_rows(c_df.f,c_df.r,c_df.s)

df.all$county<-gsub("Mc Kean","Mckean",df.all$county)
df.all$county<-gsub("McKean","Mckean",df.all$county)
c_df.all<-df.all %>%
  group_by(county) %>%  summarize(total=sum(total))

write_csv(c_df.all,sprintf("output/%s_total_vaccine_doses_allocated_by_county.csv",working_date),quote=FALSE)
