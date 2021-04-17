library(ggplot2)
library(readr)


df <- read_csv("output/20210411_vaccinaton_qgis.csv")

# Hex color codes for Dem Blue and Rep Red
#party_colors <- c("#2E74C0", "#CB454A") 

p0 <- ggplot(df)+ geom_point(mapping = aes(x = df$Rate.Partially.Covered.per.100.000 ,
                                           y = reorder(county, df$Rate.Partially.Covered.per.100.000),
                                           shape="Partially Covered per 100K"))
p1 <- p0+geom_point(mapping = aes(x = df$Rate.O65Partial.Per.100K,
                                  y = reorder(county, df$Rate.Partially.Covered.per.100.000),
                                  color=c("red"),shape="Over 65 Rate"))
p2 <- p1+ geom_point(mapping = aes(x = df$Rate.U65Partial.Per.100K ,
                                   y = reorder(county, Rate.Partially.Covered.per.100.000),
                                   color=c("blue"),shape="Under 65 Rate"))
p3<-p2+ geom_vline(xintercept = mean(df$Rate.Partially.Covered.per.100.000 ), color = "black") 



p3 +  guides(color=FALSE) + labs(x = "Rate Partially Covered per 100K (Age)", y = "County") +
  theme(axis.text=element_text(size=8))



f0 <- ggplot(df)+ geom_point(mapping = aes(x = df$Rate.Fully.Covered.per.100.000 ,
                                           y = reorder(county, df$Rate.Fully.Covered.per.100.000),
                                           shape="Fully Covered per 100K"))
f1 <- p0+geom_point(mapping = aes(x = df$Rate.O65Full.Per.100K,
                                  y = reorder(county, df$Rate.Fully.Covered.per.100.000),
                                  color=c("red"),shape="Over 65 Rate"))
f2 <- p1+ geom_point(mapping = aes(x = df$Rate.U65Full.Per.100K ,
                                   y = reorder(county, Rate.Fully.Covered.per.100.000),
                                   color=c("blue"),shape="Under 65 Rate"))
f3<-p2+ geom_vline(xintercept = mean(df$Rate.Fully.Covered.per.100.000 ), color = "black",labs="all county avg rate") 



f3 + #facet_wrap(. , ncol=1, scales="free_y") +
  guides(color=FALSE) + labs(x = "Rate Fully Covered per 100K (Age)", y = "County") +
  theme(axis.text=element_text(size=8))



fe0 <- ggplot(df)+ geom_point(mapping = aes(x = log10(df$Rate.Fully.Covered.per.100.000) ,
                                           y = reorder(county, df$Rate.Fully.Covered.per.100.000),
                                           shape=1))
fe1 <- fe0+geom_point(mapping = aes(x = log10(df$Rate.WhiteFull.Per.100K),
                                  y = reorder(county, df$Rate.Fully.Covered.per.100.000),
                                  color=c("pink"),shape=2))
fe2 <- fe1+ geom_point(mapping = aes(x = log10(df$Rate.BlackFull.Per.100K) ,
                                   y = reorder(county, Rate.Fully.Covered.per.100.000),
                                   color=c("blue"),shape=3))
fe3 <- fe2+ geom_point(mapping = aes(x = log10(df$Rate.NAFull.Per.100K ),
                                    y = reorder(county, Rate.Fully.Covered.per.100.000),
                                    color=c("green"),shape=4))
fe4 <- fe3+ geom_point(mapping = aes(x = log10(df$Rate.AsianFull.Per.100K) ,
                                    y = reorder(county, Rate.Fully.Covered.per.100.000),
                                    color=c("orange"),shape=5))
fe5 <- fe4+ geom_point(mapping = aes(x = log10(df$Rate.PIFull.Per.100K ),
                                    y = reorder(county, Rate.Fully.Covered.per.100.000),
                                    color=c("purple"),shape=6))
fe6 <- fe5+ geom_point(mapping = aes(x = log10(df$Rate.HispanicFull.Per.100K) ,
                                     y = reorder(county, Rate.Fully.Covered.per.100.000),
                                     color=c("yellow"),shape=7))
fe7<-fe6+ geom_vline(xintercept = mean(log10(df$Rate.Fully.Covered.per.100.000) ), color = "black",labs="all county avg rate") 



fe7 + #facet_wrap(. , ncol=1, scales="free_y") +
  guides(color=FALSE) + labs(x = "Rate Fully Covered per 100K (Race/Ethnicity)", y = "County") +
  theme(axis.text=element_text(size=8))