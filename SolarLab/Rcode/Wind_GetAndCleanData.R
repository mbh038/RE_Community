## Callywith wind clean data

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

## read in data
dat.wide<-read.csv("../data/raw/wind/callywith3year.csv",stringsAsFactors=FALSE)

# convert time to POSIXct
dat.wide$Timestamp<-dmy_hm(dat.wide$Timestamp)

#tidy the data
cw<-gather(dat.wide,height,V,V46:V20,-Dir)
rm(dat.wide)

#write to csv
saveRDS(cw,"../data/cleaned/wind/callywith_wind_tidy.rds")
rm(cw)