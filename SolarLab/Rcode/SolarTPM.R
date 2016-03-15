#Generates a tpm and cpm matric from actual solar data.

maxSWD<-0
maxBin<-0
nbin<-100

file_handles<-c("Cam2001n.csv","Cam2002n.csv","Cam2003n.csv","Cam2004n.csv","Cam2005n.csv","Cam2006n.csv","Cam2007n.csv","Cam2008n.csv")
path<-"../data/cleaned/"

# find max bin number and  range of data
for (file in 1:length(file_handles)){
  fullname<-paste0(path,file_handles[file])
  #print (fullname)
  data<-readRDS(fullname)
  maxSWD<-max(maxSWD,max(data$SWD))
  maxBin<-max(maxBin,max(floor((data$SWD/maxSWD)*0.999*nbin)+1))
  print (maxBin)
}

# set up TPM
tpm<-matrix(0, nrow = maxBin, ncol =maxBin)

# Load cleaned data
########################################################################
# loop through data file and add bin counts to TPM

for (file in 1:length(file_handles)){
  fullname<-paste0(path,file_handles[file])
  print (paste("Adding ",fullname," to TPM"))
  data<-readRDS(fullname)
  
  
  # Markov Chain
  ########################################################################
  # remove zeros
  data<-data[data$SWD>0,]
  
  # log transform the data
  # reference$SWD<-(reference$SWD)
  # reference$SWD[reference$SWD=="-Inf"]=0
  
  # bin into 100 levels
  data$bin<-floor((data$SWD/maxSWD)*0.999*nbin)+1
  table(data$bin)
  sum(table(data$bin))
  hist(data$bin,main=file_handles[file],xlab="Bin")
 
  # loop to generate TPM counts
  for (i in 1:(nrow(data)-1)){
    # as.POSIXlt(data$datetime[1000])$hour  # am or pm
    tpm[data$bin[i],data$bin[i+1]] <-tpm[data$bin[i],data$bin[i+1]] + 1
  }
  
}
rm(data)

tpm[1,]
#summary(tpm)

#print(tpm)
sum(tpm[1,])

spm<-numeric(maxBin)
# sums of each row of TPM
for (i in 1 :nrow(tpm)){
    spm[i]=sum(tpm[i,])
}
spm
sum(spm)

# remove any rows or columns that contain only zeros.
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
print (paste(count," bins were empty and have been removed"))
#str(tpmr)
bins
maxBin<-max(bins)

spmr<-numeric(nrow(tpmr))
# sums of each row of TPMr
for (i in 1 :nrow(tpmr)){
  spmr[i]=sum(tpmr[i,])
}
spmr
sum(spmr)

tpmp<-tpmr
# TPM as probabilities
for (i in 1 :nrow(tpmr)){
    tpmp[i,]=tpmr[i,]/spmr[i]
}
sum(tpmp)

# cumulative probability matrix
cpm<-matrix(0, nrow = maxBin, ncol =maxBin)
cpm<-tpmp
# TPM-> CPM: cumulative probabilities
for (i in 1 : nrow(tpmp)){
    for (j in 1 :ncol(tpmp)){
        cpm[i,j]=sum(tpmp[i,1:j])
    }
}
cpm<-cbind(bins,cpm)

write.table(cpm,"../tpm/Cam_cpm.csv",sep=",",row.names=FALSE,col.names=FALSE)


