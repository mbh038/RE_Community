# read in demand files

# domestic demand MW
houses=17852
ddata<-read.csv("../data/profiles/EESP/domDem10.csv")
domDemand<-numeric()
domDemand<-houses*ddata$W/1e6
rm(ddata)

sum(domDemand/6)

#non-domestic demand
ndDemand<-rep(0,144*365)
nProfiles=5
pc10path<-"../data/profiles/UKERC/pc10minThin/"
for (i in 3:8){
    pc10file<-paste0(pc10path,"pc10.",i,".csv")
    pc10<-read.csv(pc10file,stringsAsFactors=FALSE)$W
    #plot(pc10)
    ndDemand<-ndDemand+pc10
}
#plot(ndDemand,type="l")
sum(ndDemand/6000) # MWh per representative UKERC business

nbusiness=1364 # total business in St Austell
medianNonDomUsage<-7.422 # MWh, DECC 2015

totalNdDemand<-ndDemand*(1364*7.422)/sum(ndDemand/6)

sum(totalNdDemand/6) # annual Non Dom demand in MWh

# combine to give total demand
demand=domDemand+totalNdDemand

# plot two weeks

png("../results/simDemandExamples.png",width=595,height=642)

library(rafalib)
mypar(2,1)
library(openintro)
data(COL)

d<-(0:1007)/144
days<-c(1,180) # choose twq days

count=0
for (day in days){
    count=count+1
    daystart=1+(day-1)*144
    dayend=daystart+144*7-1
    dayIndex=seq(daystart,dayend)
    plot(d,demand[dayIndex],
         xlab="Day",
         ylab="Demand (MW)",
         type="l",
         lwd=2,
         col=COL[1],
         ylim=c(0,15)
    )
    lines(d,domDemand[dayIndex],col=COL[2])
    lines(d,totalNdDemand[dayIndex],col=COL[4])
     if(count!=1){
         legend(-0.2,15,
                c("total","domestic","non-domestic"),
                lty=c(1,1,1),
                lwd=c(2.0,2.0,2.0),
                col=c(COL[1],COL[2],COL[4]),
                box.lty=0,
               cex=0.9
       )
    }
    text(6,14.5,ifelse(count==1,"Winter week","Summer week"))
}

dev.off()