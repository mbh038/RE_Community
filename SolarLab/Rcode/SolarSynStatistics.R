## Investigate statistics of real and synthetic solar data

source("SolarFunctions.R")

#load one year of real data (eg 2014 from Camborne)

camdata<-readRDS("../data/cleaned/solar/CamBSRN_Solar10min/Cam2014n10min.rds")

# load one year of simulated data

stem="../data/synthetic/CamBSRN_Solar10minSyn/CamSolarSyn10min"
tail=".csv"

for (i in 1:100){
  
if (i<10){
  number=paste0("00",i)
}
if (i>=10 & i<100){
  number=paste0("0",i)
}
  

syndata<-read.csv(paste0(stem,number,tail))

library(rafalib)
library(openintro)
data(COL)

mypar(3,1)
qqplot(camdata$SWD,syndata$SWD,col=COL[11])
abline(0,1,col=COL[26],lwd=2)


hist(syndata$SWD[syndata$SWD>0],xlim=c(0,1000),breaks=50)
hist(camdata$SWD[camdata$SWD>0],xlim=c(0,1000),breaks=50)
}
