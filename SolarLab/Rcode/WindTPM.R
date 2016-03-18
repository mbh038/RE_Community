## Callywith wind analysis


library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

## read in tidy data
path<-"../data/cleaned/wind/"
filehandle<-"callywith_wind_tidy.rds"
fullname<-paste0(path,filehandle)
data<-readRDS(fullname)

# convert time to POSIXct
data$Timestamp<-dmy_hm(data$Timestamp)

cw46<-filter(data,height=="V46")
cw32<-filter(data,height=="V32")
cw20<-filter(data,height=="V20")

## TPM 20

reference<-cw46

reference$bin<-floor(reference$V)+1
table(reference$bin)
sum(table(reference$bin))

maxBin=max(reference$bin)

tpm<-matrix(0, nrow = maxBin, ncol =maxBin)
spm<-numeric(maxBin)
cpm<-matrix(0, nrow = maxBin, ncol =maxBin)

# loop to generate TPM
for (i in 1:nrow(reference)){
    tpm[reference$bin[i],reference$bin[i+1]] <-tpm[reference$bin[i],reference$bin[i+1]] + 1
}

tpm[1,]

print(tpm)
sum(tpm[1,])

for (i in 1 :nrow(tpm)){
    spm[i]=sum(tpm[i,])
}
spm
sum(spm)


for (i in 1 :nrow(tpm)){
    tpm[i,]=tpm[i,]/sum(tpm[i,])
}

# calculate cumulative probabilities
for (i in 1 : nrow(tpm)){
    for (j in 1 :ncol(tpm)){
        cpm[i,j]=sum(tpm[i,1:j])
    }
}
print (cpm)

write.table(cpm,"../tpm/wind/callywith_cpm.csv",sep=",",row.names=FALSE,col.names=FALSE)


