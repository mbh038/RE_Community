# EnergySim



windMW<-160
windPower<-read.csv("../data/specs/windPowerCurve.csv")
solarMW<-160

# read in demand files
houses=18000
ddata<-read.csv("../data/profiles/EESP/domDem10.csv")
ddata$W<-houses*ddata$W


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

wfile<-floor(100*runif(1)+1)
sfile<-floor(100*runif(1)+1)

print(paste("Solar file:",sfile,", Wind file: ",wfile,sep=" "))

wfilename<-ipfilename(wfile,wipfilepathstem,wipfilepathtail)
sfilename<-ipfilename(sfile,sipfilepathstem,sipfilepathtail)

wdata<-read.csv(wfilename)
sdata<-read.csv(sfilename)

data<-data.frame(wdata[,1],wdata[,2],sdata[,2])
data$day<-min(365,data$t %/% 144 +1)
names(data)<-c("t","w","s")
hist(data$w)

solarop<-numeric(length=length(data))
windop<-numeric(length=length(data))

for (i in 1:nrow(data)){
  windop[i]<-ifelse(data$w[i]>3,windMW*windPower[which(windPower$v==data$w[i]),2],0)
  solarop[i]<-solarMW*(data$s[i]/1000)
}
totalop<-windop+solarop
balance<-totalop-ddata$W
energyop<-data.frame(windop,solarop,totalop,balance)



plot(energyop$totalop[1:1000],type="l")
plot(energyop$totalop[25000:26000],type="l")
hist(energyop$totalop)
summary(energyop)
hist(ddata$W)
hist(balance)
