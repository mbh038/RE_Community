## Wind compare real and synthetic data

## Callywith wind data (used as basis for CPMs)


library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(plotrix)

## read in data
dat.wide<-read.csv("../data/raw/wind/callywith3year.csv",stringsAsFactors=FALSE)

# convert time to POSIXct
dat.wide$Timestamp<-ymd_hms(dat.wide$Timestamp)

#tidy the data
cw<-gather(dat.wide,height,V,V46:V20,-Dir)
rm(dat.wide)

cw46<-filter(cw,height=="V46")
cw32<-filter(cw,height=="V32")
cw20<-filter(cw,height=="V20")

#Plot histograms of wind speed

g<-ggplot(dat=cw,aes(x=V,fill=height))+geom_density(alpha=0.3)
g

# plot wind rose

