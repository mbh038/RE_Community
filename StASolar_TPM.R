## Callywith wind analysis


library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(plotrix)


df<-read.csv("StAustellSolar.txt")
str(df)
summary(df)
## TPM 20

reference<-df

#reference$bin<-floor(reference$swv_dwn)+1
reference$bin<-round(reference$swv_dwn,0)+1
table(reference$bin)
sum(table(reference$bin))

#maxBin=max(reference$bin)+1
maxBin=length(unique(reference$bin))+1

tpm<-matrix(0, nrow = maxBin, ncol =maxBin)
spm<-numeric(maxBin)
cpm<-matrix(0, nrow = maxBin, ncol =maxBin)

for (i in 1:nrow(reference)){
    tpm[reference$bin[i],reference$bin[i+1]] <-tpm[reference$bin[i],reference$bin[i+1]] + 1

}
tpm
#tpm[1,]
#sum(tpm[1,])

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
cpm
s=numeric(nrow(reference))
s[1]=reference$bin[1]  #round((maxBin)*runif(1),0)+1

for (i in 2:nrow(reference)){
    colIndex=runif(1)
    j=1
    while (cpm[round(s[i-1],0),j] < colIndex){
        j=j+1
    }
    s[i]=j
}
summary(s)
table(s,reference$bin)
summary(reference$bin)

s=s-1+runif(nrow(reference))
mean(s)-mean(reference$swv_dwn)
sd(s)-sd(reference$swv_dwn)
summary(s)
summary(reference$swv_dwn)
#library(MASS)
#vFit<-fitdistr(s+.01, 'weibull')
#print (vFit)
#refFit<-fitdistr(reference$swv_dwn+.01, 'weibull')
#print(refFit)
library(rafalib)
hist(s,xlim=c(0,20),prob=TRUE)
#d = dweibull(seq(0,20,.2),vFit$estimate[1],vFit$estimate[2])
#points(seq(0,20,.2),d,type='l',col=2)
qqplot(s,reference$swv_dwn)

index<-seq(1,365,1)
sol<-data.frame(index,s[182:546],reference$swv_dwn[182:546])
names(sol)<-c("index","sol","ref")
plot(sol$index,sol$ref,type="l")
points(sol$index,sol$sol,type="l",col="red")
