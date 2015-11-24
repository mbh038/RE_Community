## Callywith wind analysis


library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(plotrix)

## read in data
dat.wide<-read.csv("callywith3year.csv",stringsAsFactors=FALSE)

# convert time to POSIXct
dat.wide$Timestamp<-dmy_hm(dat.wide$Timestamp)

#tidy the data
cw<-gather(dat.wide,height,V,V46:V20,-Dir)
rm(dat.wide)

cw46<-filter(cw,height=="V46")
cw32<-filter(cw,height=="V32")
cw20<-filter(cw,height=="V20")

## TPM 20

reference<-cw20

reference$bin<-floor(reference$V)+1
table(reference$bin)
sum(table(reference$bin))

maxBin=max(reference$bin)+1

tpm<-matrix(0, nrow = maxBin, ncol =maxBin)
spm<-numeric(maxBin)
cpm<-matrix(0, nrow = maxBin, ncol =maxBin)

for (i in 1:nrow(reference)){
    tpm[reference$bin[i],reference$bin[i+1]] <-tpm[reference$bin[i],reference$bin[i+1]] + 1

    }
tpm[1,]
sum(tpm[1,])

for (i in 1 :nrow(tpm)){
    spm[i]=sum(tpm[i,])
}
spm
sum(spm)

for (i in 1 :nrow(tpm)){
    tpm[i,]=tpm[i,]/sum(tpm[i,])
}

for (i in 1 : nrow(tpm)){
    for (j in 1 :ncol(tpm)){
        cpm[i,j]=sum(tpm[i,1:j])
    }
}
#cpm
v=numeric(nrow(reference))
v[1]=round((maxBin)*runif(1),0)+1
for (i in 2:nrow(reference)){
    colIndex=runif(1)
    j=1
    while (cpm[round(v[i-1],0),j] < colIndex){
        j=j+1
    }
    v[i]=j
}
summary(v)
summary(reference$bin)

v=v-1+runif(nrow(reference))
mean(v)-mean(reference$V)
sd(v)-sd(reference$V)
summary(v)
summary(reference$V)
library(MASS)
vFit<-fitdistr(v+.01, 'weibull')
print (vFit)
refFit<-fitdistr(reference$V+.01, 'weibull')
print(refFit)
library(rafalib)
hist(v,xlim=c(0,20),prob=TRUE)
d = dweibull(seq(0,20,.2),vFit$estimate[1],vFit$estimate[2])
points(seq(0,20,.2),d,type='l',col=2)

