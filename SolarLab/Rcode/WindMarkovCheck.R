## Wind Markov
library(dplyr)

# get cpm matrix
cpm<-read.table("../tpm/wind/callywith_cpm.csv",sep=",")
cpm

## read in tidy data
inpath<-"../data/cleaned/wind/"
infilehandle<-"callywith_wind_tidy.rds"
fullname<-paste0(inpath,infilehandle)
data<-readRDS(fullname)

# convert time to POSIXct
#data$Timestamp<-dmy_hm(data$Timestamp)

reference<-filter(data,height=="V46")

## TPM 20

reference$bin<-floor(reference$V)+1
table(reference$bin)
sum(table(reference$bin))

maxBin=max(reference$bin)


ipfilepathstem<-"../data/synthetic/CallywithWind10minSyn/Cally"
ipfilepathtail<-"_10min.csv"

ndata<-6*24*365

for (file in 1:100){
    
    # set up output file numbers
    ipfilehandle<-as.character(file)
    if (file < 10){
        ipfilehandle<-paste0("00",ipfilehandle)
    }
    if (file >= 10 && file < 100){
        ipfilehandle<-paste0("0",ipfilehandle)
    }
    ipfilename<-paste0(ipfilepathstem,ipfilehandle,ipfilepathtail)

    newdata<-read.csv(ipfilename)
    print(paste("File",ipfilehandle,"in",sep=" "))

    ndata<-nrow(newdata)
    v<-newdata[,2]
    
#     print(summary(v))
#     summary ((reference$bin[1:ndata]))
    
    v=v-1+runif(ndata)
    mean(v)-mean(reference$V[1:ndata])
    sd(v)-sd(reference$V[1:ndata])
    #print (summary(v))
    #print(summary(reference$V[1:ndata]))
    library(MASS)
    vFit<-fitdistr(v+.01, 'weibull')
    #print (vFit)
    refFit<-fitdistr(reference$V[1:ndata]+.01, 'weibull')
    #print(refFit)
    
    print(paste(round(mean(v),2),round(mean(reference$V[1:ndata]),2),round(vFit$estimate[1],2),round(vFit$estimate[2],2),round(refFit$estimate[1],2),round(refFit$estimate[2],2),sep=" "))
    hist(v,xlim=c(0,20),prob=TRUE)
    d = dweibull(seq(0,20,.2),vFit$estimate[1],vFit$estimate[2])
    points(seq(0,20,.2),d,type='l',col=2)

    plot(v[1:1000],type="l")
    lines(reference$V[1:1000],type="l",col="red")
    


}