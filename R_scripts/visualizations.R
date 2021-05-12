library(ggplot2)

library(stringr)
library(readr)
library(gdata)
library(plyr)
library(dplyr)
dev.off()
today<-format(Sys.time(),"%C%y%m%d")
working_date <- "20210504"
#system(sprintf("mkdir %s", today))

init <- data.frame(read_csv(sprintf("output/%s_vaccinaton_expvactual.csv",working_date)))
gis <- data.frame(read_csv(sprintf("output/%s_pvi_qgis.csv",working_date)))

df <-inner_join(init, gis,  by = c("county" = "COUNTY_NAM"))

options(scipen=999)

attach(df)

plot(rate_total_adm ~rate_total_exp, data=df,
     pch=21, cex=1.4, col="black", xlab="Expected Rate (per 100K)",
     ylab="Rate Administered (per 100K)",
     main="Rate (per 100K) Vaccine Allocated (Expected) vs Administered\nRace",
     ylim=c(0,100000), xlim=c(0,100000))
abline(lm(rate_total_adm ~ rate_total_exp))
#dev.copy(png,sprintf('../images/actualvsexpected_all_%s.png',working_date))

legend("topleft", inset=0.03, 
       legend=c("Total","White","Black","American Indian/Alaskan Native","Asian","Hawaiian Native/Pacific Islander"),
       col=c("black","blue","red","orange","green","purple"),
       pch=c(21,16,16,16,16,16),
       lty=1:2, cex=0.8,
       box.lty=2, box.lwd=2, box.col="black")

points (adm_white ~EXP_WHITE, data=df,
     pch = 16, cex = 1.3, col = "blue", xlab="Expected Rate (per 100K)",
     ylab="Rate Administered (per 100K)",
     ylim=c(0,100000), xlim=c(0,100000))
abline(lm(adm_white ~ EXP_WHITE),col="blue")
#dev.copy(png,sprintf('../images/actualvsexpected_all_%s.png',working_date))
points(adm_black ~EXP_BLACK, data=df, xlab="Expected Rate (per 100K)",
       ylab="Rate Administered (per 100K)",
       ylim=c(0,100000), xlim=c(0,100000),
       pch = 16, cex = 1.3, col = "red")
abline(lm(adm_black ~EXP_BLACK),col="red")

points(adm_na ~EXP_NA, data=df, xlab="Expected Rate (per 100K)",
     ylab="Rate Administered (per 100K)",
     ylim=c(0,100000), xlim=c(0,100000),
     pch = 16, cex = 1.3, col = "orange")
abline(lm(adm_na ~ EXP_NA),col="orange")

points(adm_asian ~EXP_ASIAN, data=df, xlab="Expected Rate (per 100K)",
       ylab="Rate Administered (per 100K)",
       ylim=c(0,100000), xlim=c(0,100000),
       pch = 16, cex = 1.3, col = "green")
abline(lm(adm_asian ~EXP_ASIAN),col="green")
points(adm_pi ~EXP_PI, data=df, xlab="Expected Rate (per 100K)",
       ylab="Rate Administered (per 100K)",
       ylim=c(0,100000), xlim=c(0,100000),
       pch = 16, cex = 1.3, col = "purple")
abline(lm(adm_pi ~ EXP_PI), col="purple")
#dev.copy(png,sprintf('../images/actualvsexpected_all_%s.png',working_date))


dev.copy(png,sprintf('../images/actualvsexpected_race_all_%s.png',working_date))
dev.off()

plot(rate_total_adm ~rate_total_exp, data=df,
     pch=21, cex=1.4, col="black", xlab="Expected Rate (per 100K)",
     ylab="Rate Administered (per 100K)",
     main="Rate (per 100K) Vaccine Allocated (Expected) vs Administered\nEthnicity",
     ylim=c(0,100000), xlim=c(0,100000))
abline(lm(rate_total_adm ~ rate_total_exp))
#dev.copy(png,sprintf('../images/actualvsexpected_all_%s.png',working_date))

legend("topleft", inset=0.03, 
       legend=c("Total","Non-Hispanic","Hispanic"),
       col=c("black","blue","red"),
       pch=c(21,16,16),
       lty=1:2, cex=0.8,
       box.lty=2, box.lwd=2, box.col="black")

points (adm_hisp ~EXP_HISP, data=df,
        pch = 16, cex = 1.3, col = "blue", xlab="Expected Rate (per 100K)",
        ylab="Rate Administered (per 100K)",
        ylim=c(0,100000), xlim=c(0,100000))
abline(lm(adm_hisp ~ EXP_HISP),col="blue")
#dev.copy(png,sprintf('../images/actualvsexpected_all_%s.png',working_date))
points(adm_nhisp ~EXP_NHISP, data=df, xlab="Expected Rate (per 100K)",
       ylab="Rate Administered (per 100K)",
       ylim=c(0,100000), xlim=c(0,100000),
       pch = 16, cex = 1.3, col = "red")
abline(lm(adm_nhisp ~EXP_NHISP),col="red")
dev.copy(png,sprintf('../images/actualvsexpected_ethnicity_all_%s.png',working_date))
dev.off()

#generate each county's bar charts
#expected doses by race
for(i in 1:67) {
pvi_image_path <- df[i,"pvi_image_path"]

df.adams<-c(as.integer(df[i,"total"]),
            as.integer(df[i,"EXP_WHITE"]),as.integer(df[i,"EXP_BLACK"]),
            as.integer(df[i,"EXP_NA"]),as.integer(df[i,"EXP_ASIAN"]),
            as.integer(df[i,"EXP_PI"]))

barplot(df.adams,main=sprintf("Expected Doses By Race for %s COUNTY",df[i,"county"]),
        names.arg=c("Total","White","Black","NA/AN","Asian","HN/PI"),
        col=c("black","blue","red","orange","green","purple"))
dev.copy(png,sprintf('./output/%s%s_expected_by_race.png',working_date,pvi_image_path))
dev.off()

#actual doses by race
df.adams<-c(as.integer(df[i,"total_adm"]),
            as.integer(df[i,"adm_white"]),as.integer(df[i,"adm_black"]),
            as.integer(df[i,"adm_na"]),as.integer(df[i,"adm_asian"]),
            as.integer(df[i,"adm_pi"]))

barplot(df.adams,main=sprintf("Actual Doses By Race for %s COUNTY",df[i,"county"]),
        names.arg=c("Total","White","Black","NA/AN","Asian","HN/PI"),
        col=c("black","blue","red","orange","green","purple"))
dev.copy(png,sprintf('./output/%s%s_actual_by_race.png',working_date,pvi_image_path))
dev.off()
#expected doses by ethnicity

df.adams<-c(as.integer(df[i,"total"]),
            as.integer(df[i,"EXP_NHISP"]),as.integer(df[i,"EXP_HISP"]))

barplot(df.adams,main=sprintf("Expected Doses By Ethnicity for %s COUNTY",df[i,"county"]),
        names.arg=c("Total","Non-Hispanic","Hispanic"),
        col=c("black","blue","red"))
dev.copy(png,sprintf('./output/%s%s_expected_by_ethnicity.png',working_date,pvi_image_path))
dev.off()
#actual doses by ethnicity
df.adams<-c(as.integer(df[i,"total_adm"]),
            as.integer(df[i,"adm_nhisp"]),as.integer(df[i,"adm_hisp"]))

barplot(df.adams,main=sprintf("Actual Doses By Ethnicity for %s COUNTY",df[i,"county"]),
        names.arg=c("Total","Non-Hispanic","Hispanic"),
        col=c("black","blue","red"))
dev.copy(png,sprintf('./output/%s%s_actual_by_ethnicity.png',working_date,pvi_image_path))
dev.off()
#Variance % by Race

df.adams<-c(as.integer(df[i,"pd_ae_white"]),as.integer(df[i,"pd_ae_black"]),
            as.integer(df[i,"pd_ae_na"]),as.integer(df[i,"pd_ae_asian"]),
            as.integer(df[i,"pd_ae_pi"]))


barplot(df.adams,main=sprintf("Variance for %s COUNTY by Race",df[i,"county"]),
        names.arg=c("White","Black","NA/AN","Asian","HN/PI"),
        col=c("blue","red","orange","green","purple"),
        ylim=c(-100,300))
abline(h=0)
dev.copy(png,sprintf('./output/%s%s_variance_by_race.png',working_date,pvi_image_path))
dev.off()
#Variance % by Race

df.adams<-c(as.integer(df[i,"pd_ae_nhisp"]),as.integer(df[i,"pd_ae_hisp"]))


barplot(df.adams,main=sprintf("Variance for %s COUNTY by Ethnicity",df[i,"county"]),
        names.arg=c("Non-Hispanic","Hispanic"),
        col=c("blue","red"),
        ylim=c(-100,300))
abline(h=0)
dev.copy(png,sprintf('./output/%s%s_variance_by_ethnicity.png',working_date,pvi_image_path))
dev.off()
}

