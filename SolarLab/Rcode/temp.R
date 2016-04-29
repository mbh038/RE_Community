# EnergySim



windMW<-20#seq(0,50,5)
solarMWp<-0#seq(0,50,5)
  
  windPower<-read.table("../data/specs/windPowerCurve.csv",header=FALSE,sep=",")

# read in demand files
houses=18000
ddata<-read.csv("../data/profiles/EESP/domDem10.csv")
demand<-numeric()
demand<-houses*ddata$W/1e6
rm(ddata)


sipfilepathstem<-"../data/synthetic/CamBSRN_Solar10minSyn/CamSolarSyn10min"
wipfilepathstem<-"../data/synthetic/CallywithWind10minSyn/Cally"
sipfilepathtail<-".csv"
wipfilepathtail<-"_10min.csv"

# set up input file numbers
ipfilename<-function(file,ipfilepathstem,ipfilepathtail){
  ipfilehandle<-as.character(file)
  if (file < 10){
    ipfilehandle<-paste0("00",ipfilehandle)
  }
  if (file >= 10 && file < 100){
    ipfilehandle<-paste0("0",ipfilehandle)
  }
  ipfilename<-paste0(ipfilepathstem,ipfilehandle,ipfilepathtail)
}


wp<-function(x){
  windPower[which(windPower[,1]==x),2]
}

## loop through solar and wind files

numTrials<-10
trial=0
stored=0
res<-data.frame()
#start<-proc.time()
for (i in 1:numTrials){
  trial<<-trial+1
  wfile<-floor(100*runif(1)+1)
  sfile<-floor(100*runif(1)+1)
  
  print(paste("Trial: ",trial," Solar file:",sfile,", Wind file: ",wfile,sep=" "))
  
  wfilename<-ipfilename(wfile,wipfilepathstem,wipfilepathtail)
  sfilename<-ipfilename(sfile,sipfilepathstem,sipfilepathtail)
  
  wdata<-read.csv(wfilename)[,2]
  sdata<-read.csv(sfilename)[,2]
  
  # data$day<-min(365,data$t %/% 144 +1)
  
  solarop<-numeric()
  windop<-numeric()
  totalop<-numeric()
  #solarop<-matrix(length(solarMWp)*length(sdata),length(sdata),length(solarMWp))
  #windop<-matrix(length(windMW)*length(wdata),length(wdata),length(windMW))
  
  # windop<-unlist(sapply(wdata,function(x){
  #   windMW*windPower[which(windPower[,1]==x),2]
  # }))
  
  #print(paste("Solar: ",SolarMWp,", Wind: ",WindMW))
  windop<-windMW*unlist(sapply(wdata,wp))
  # windop<-sapply(windMW,function(x){
  #   x*unlist(sapply(wdata,wp))
  # })
  solarop<-solarMWp*sdata/1000
  # solarop<-sapply(solarMWp,function (x){
  #   x*sdata/1000
  # })
  totalop<-windop+solarop
  balance<-totalop-demand# sweep(totalop,1,demand,FUN="-") #totalop-demand
  ebalance<-cumsum(balance)/6000 # in GWh
  #powerop<-data.frame(windop,solarop,totalop,demand,balance,ebalance)
  # summary(powerop)
  #diff<-proc.time()-start
  #print(diff)
  res[i]<-c(min(balance),max(balance),min(ebalance),max(ebalance))

}

library(rafalib)
mypar(4,1)
hist(res[,1],breaks=50,main="min pbalance")
hist(res[,2],breaks=50,main="max pbalance")
hist(res[,3],breaks=50,main="min ebalance")
hist(res[,4],breaks=50,main="max ebalance")

summary(res)

mypar(3,1)
days<-seq(1,1000)/144
plot(days,demand[1:1000],type="l",
     ylim=c(-12,12),
     xlab="Winter days",
     ylab="Power (MW)"
)
lines(days,solarop[1:1000],col="red")
lines(days,windop[1:1000],col="blue")
lines(days,balance[1:1000],col="green")

plot(days,demand[25001:26000],type="l",
     ylim=c(-12,12),
     xlab="Summer days",
     ylab="Power (MW)"
)
lines(days,solarop[25001:26000],col="red")
lines(days,windop[25001:26000],col="blue")
lines(days,balance[25001:26000],col="green")

ydays<-seq(1,length(ebalance))/144
plot(ydays,ebalance,type="l")

mypar(3,1)
hist(demand)
hist(totalop)
hist(balance)