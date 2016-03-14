#Generates a tpm and cpm matric from actual solar data.


# Load cleaned data
########################################################################
Cam2001n<-readRDS("../data/cleaned/Cam2001n.csv")
Cam2002n<-readRDS("../data/cleaned/Cam2002n.csv")

data<-rbind(Cam2001n,Cam2002n)
rm(Cam2001n,Cam2002n)
# Markov Chain
########################################################################

reference<-data.frame(data$datetime,data$SWD)

names(reference)<-c("datetime","SWD")
reference<-reference[reference$SWD>0,]

# log transform the data
# reference$SWD<-(reference$SWD)
# reference$SWD[reference$SWD=="-Inf"]=0
SWDrange<-range(reference$SWD)

# bin into 100 levels
reference$bin<-floor((reference$SWD/max(reference$SWD))*99.9)+1
table(reference$bin)
sum(table(reference$bin))
hist(reference$bin)

maxBin=max(reference$bin)

tpm<-matrix(0, nrow = maxBin, ncol =maxBin)
spm<-numeric(maxBin)
cpm<-matrix(0, nrow = maxBin, ncol =maxBin)

# loop to generate TPM counts
for (i in 1:(nrow(reference)-1)){
    tpm[reference$bin[i],reference$bin[i+1]] <-tpm[reference$bin[i],reference$bin[i+1]] + 1
}

tpm[1,]
summary(tpm)

#print(tpm)
sum(tpm[1,])

# sums of each row of TPM
for (i in 1 :nrow(tpm)){
    spm[i]=sum(tpm[i,])
}
spm
sum(spm)

tpmr<-tpm
count=0
bins<-seq(1,maxBin)
for (i in 1 :nrow(tpm)){
  if (sum(tpm[i,])==0) {
      bins<-bins[-(i-count)]
      tpmr<-tpmr[-(i-count),]
      tpmr<-tpmr[,-(i-count)]
      count=count+1
  }
}
str(tpmr)
bins

spm<-numeric(nrow(tpmr))
# sums of each row of TPMr
for (i in 1 :nrow(tpmr)){
  spm[i]=sum(tpmr[i,])
}
spm
sum(spm)

tpmp<-tpmr
# TPM as probabilities
for (i in 1 :nrow(tpmr)){
    tpmp[i,]=tpmr[i,]/spm[i]
}
sum(tpmp)

cpm<-tpmp
# TPM-> CPM: cumulative probabilities
for (i in 1 : nrow(tpmp)){
    for (j in 1 :ncol(tpmp)){
        cpm[i,j]=sum(tpmp[i,1:j])
    }
}
cpm<-cbind(bins,cpm)

write.table(cpm,"../tpm/Cam_cpm.csv",sep=",",row.names=FALSE,col.names=FALSE)

rm(data)

