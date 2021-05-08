## This script takes results from Toxpi modeling, and prepares it for working with qgis
## You will need to set your model home in the variables below as well as the working date that you want to use model 
## data from

## make sure that you've converted all exported images from pdf to svg
## on linux from within the directory where the images were placed:
## for f in *.pdf; do pdf2svg $f ${f%%.*}.svg; done

library(readr)

working_date <- "20210504"

data <- read_csv(sprintf("output/%s_results.csv",working_date))
#cols(
#  `ToxPi Score` = col_double(),
#  `HClust Group` = col_double(),
#  `KMeans Group` = col_double(),
#  Name = col_character(),
#  Source = col_character(),
#  `Transmissible Cases!25!0xcc3333ff` = col_double(),
#  `Disease Spread!5!0xe05251ff` = col_double(),
#  `Population Mobility!10!0x57b757ff` = col_double(),
#  `Residential Density!10!0x70c970ff` = col_double(),
#  `Social Distancing!10!0x4258c9ff` = col_double(),
#  `Testing!10!0x5f73ddff` = col_double(),
#  `Population Demographics!10!0x6b0b9eff` = col_double(),
#  `Air Pollution!10!0x7e24aeff` = col_double(),
#  `Age Distribution!10!0x933fbfff` = col_double(),
#  `Comorbidities!10!0xa95ad2ff` = col_double(),
#  `Health Disparities!10!0xc177e7ff` = col_double(),
#  `Hospital Beds!5!0xdb95feff` = col_double()
#)

county_state1 <- t(data.frame(strsplit(data$Name,",")))
#data$arcgis_county <- sprintf("%s County, %s", trim(county_state1[,2]),trim(county_state1[,1]))
data$STNAME <-str_to_upper(county_state1[,1],locale="en")
data$COUNTY_NAM <-trim(str_to_upper(county_state1[,2],locale="en"))

data <- data[data$STNAME=="PENNSYLVANIA",]
data <- subset(data,select=-c(3,Name,Source,STNAME))
n <- colnames(data[3:14])
lst <- lapply(n, function(vec) unique(unlist(strsplit(vec, "!", perl = T))))
df<-t(data.frame(lst))
n<-df[,1]
colnames(data) <- c("Toxpi Score","HCluster",df[,1],"COUNTY_NAM")

## add image names
images<-paste(1:67)
image_names<-data.frame(c(1:67),paste(sprintf("/images/%s_images_",working_date), images, ".png",sep=""))
colnames(image_names)<-c("row","pvi_image_path")
image_names<-data.frame(image_names)
data<-data.frame(data)
data$row<-seq.int(nrow(data))

data<-join(data, image_names,
             type = "inner", by="row",match="all")
data <- subset(data,select=-c(row))
write_csv(data,sprintf("output/%s_pvi_qgis.csv",working_date),quote=FALSE)

remove(county_state1,data,df,image_names,images,lst,n,working_date)
