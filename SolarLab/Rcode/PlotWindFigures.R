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

library(rafalib)
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
    print (vFit)
    refFit<-fitdistr(reference$V[1:ndata]+.01, 'weibull')
    print(refFit)
    
    mypar(2,1)
    print(paste(round(mean(v),2),round(mean(reference$V[1:ndata]),2),round(vFit$estimate[1],2),round(vFit$estimate[2],2),round(refFit$estimate[1],2),round(refFit$estimate[2],2),sep=" "))
    hist(v,xlim=c(0,20),prob=TRUE)
    d = dweibull(seq(0,20,.2),vFit$estimate[1],vFit$estimate[2])
    points(seq(0,20,.2),d,type='l',col=2)

    plot(v[1:1000],type="l")
    lines(reference$V[1:1000],type="l",col="red")
    


}

png("../results/simWindDist.png",width=595,height=642)

library(openintro)
data(COL)
mypar(2,1)
print(paste(round(mean(v),2),round(mean(reference$V[1:ndata]),2),round(vFit$estimate[1],2),round(vFit$estimate[2],2),round(refFit$estimate[1],2),round(refFit$estimate[2],2),sep=" "))
hist(v,
     xlim=c(0,20),
     prob=TRUE,
     xlab="Wind speed (m/s)",
     ylab="Probability",
     col=COL[3],
     main=""
     )
d = dweibull(seq(0,20,.2),vFit$estimate[1],vFit$estimate[2])
lines(seq(0,20,.2),d,type='l',col=2,lwd=2.5)
text(12,0.13,pos=4,paste0("Shape factor =: ",round(vFit$estimate[1],2)))
text(12,0.115,pos=4,paste0("Scale factor = ",round(vFit$estimate[2],2)))

qqplot(reference$V,v,
       xlab="Measured wind speed (m/s)",
       ylab="Simulated wind speed",
       pch=19,
       col=COL[4]
       )
abline(0,1,col=COL[5])

dev.off()

png("../results/simWindExamples.png",width=595,height=642)

mypar(2,1)

plot(seq(0,7-1/144,1/144),v[1:1008],
     type="l",
     xlab="Time (days)",
     ylab="Wind speed (m/s)",
     col=COL[1],
     lwd=2
     )
lines(seq(0,7-1/144,1/144),reference$V[1:1008],
      type="l",
      col=COL[2],
      lwd=2
      )
legend(-.25,16.2,
       c("Real data (winter)","Simulated data"),
       lty=c(1,1),
       lwd=c(2.0,2.0),
       col=COL[1:2],
       box.lty=0,
       cex=0.9
       )

plot(seq(0,7-1/144,1/144),v[25001:26008],
     type="l",
     xlab="Time (days)",
     ylab="Wind speed (m/s)",
     col=COL[1],
     lwd=2
)
lines(seq(0,7-1/144,1/144),reference$V[25001:26008],
      type="l",
      col=COL[2],
      lwd=2
)
legend(-.25,20.5,
       c("Real data (summer) ","Simulated data"),
       lty=c(1,1),
       lwd=c(2.0,2.0),
       col=COL[1:2],
       box.lty=0,
       cex=0.9
)

dev.off()