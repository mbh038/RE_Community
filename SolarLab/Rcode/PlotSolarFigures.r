## Plot solar Figures

source("SolarFunctions.R")

#load one year of real data (eg 2014 from Camborne)

camdata<-readRDS("../data/cleaned/solar/CamBSRN_Solar10min/Cam2014n10min.rds")

# load one year of simulated data

#### generate one year of clear insolation

syndata<-read.csv("../data/synthetic/CamBSRN_Solar10minSyn/CamSolarSyn10min001.csv")
# choose the latitude
phi=(pi/180)*50
S0=1000

t=seq(0,365,length.out=365*144)
t10min<-seq(1,52560)
Q<-solarFlux(S0,phi,t)

hours<-(0:143)/6

png("../results/simSolarExamples.png",width=595,height=642)

library(rafalib)
mypar(2,1)
library(openintro)
data(COL)

days<-c(4,180) # choose twq days

count=0
for (day in days){
    count=count+1
    daystart=1+(day-1)*144
    dayend=daystart+143
    dayIndex=seq(daystart,dayend)
    plot(hours,Q[dayIndex],
         xlab="Time (hours)",
         ylab="Insolation (W/m2)",
         type="l",
         lwd=2,
         col=COL[1],
         ylim=c(0,1000)
         )
    lines(hours,camdata$SWD[dayIndex],col=COL[2])
    lines(hours,syndata$SWD[dayIndex],col=COL[4])
    if(count==1){
        legend(0,1000,
           c("Clear sky","real data","simulated data"),
           lty=c(1,1,1),
           lwd=c(2.0,2.0,2.0),
           col=c(COL[1],COL[2],COL[4]),
           box.lty=0,
           cex=0.9
           )
    }
    text(20,930,ifelse(count==1,"Winter day","Summer day"))
}

dev.off()
