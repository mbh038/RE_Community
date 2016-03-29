## Generates tpm and cpm matrices from actual solar data.

# Writes out to file 3 cpms: a total, am and pm version.


maxSWD<-0
maxBin<-0
nbin<-100

min10<-TRUE

file_handles<-c("Cam2001n","Cam2002n","Cam2003n","Cam2004n","Cam2005n","Cam2006n","Cam2007n","Cam2008n","Cam2014n","Cam2015n")

if (!min10) {
  path<-"../data/cleaned/solar/CamBSRN_Solar1min/" 
} else {
  # for all cleaned data sets
  file_handles<-paste0(file_handles,"10min",sep="")
  path<-"../data/cleaned/solar/CamBSRN_Solar10min/"
  
}
file_handles<-paste0(file_handles,".rds",sep="")

# find max bin number and  range of data
for (file in 1:length(file_handles)){
  fullname<-paste0(path,file_handles[file])
  #print (fullname)
  data<-readRDS(fullname)
  maxSWD<-max(maxSWD,max(data$SWD))
  maxBin<-max(maxBin,max(floor((data$SWD/maxSWD)*0.999*nbin)+1))
  print (maxBin)
}

# set up TPMs
tpm<-matrix(0, nrow = maxBin, ncol =maxBin)
tpm_am<-matrix(0, nrow = maxBin, ncol =maxBin)
tpm_pm<-matrix(0, nrow = maxBin, ncol =maxBin)

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
#   maxSWD<-log(maxSWD)
#   data$SWD<-log(data$SWD)
#   data$SWD[data$SWD=="-Inf"]=0
  
  # bin into 100 levels
  data$bin<-floor((data$SWD/maxSWD)*0.999*nbin)+1
  table(data$bin)
  sum(table(data$bin))
  hist(data$bin,main=file_handles[file],xlab="Bin")
 
  # loop to generate TPM counts
  for (i in 1:(nrow(data)-1)){
    # as.POSIXlt(data$datetime[1000])$hour  # am or pm
    tpm[data$bin[i],data$bin[i+1]] <-tpm[data$bin[i],data$bin[i+1]] + 1
    
    if (as.POSIXlt(data$datetime[i])$hour <12){
        tpm_am[data$bin[i],data$bin[i+1]] <-tpm_am[data$bin[i],data$bin[i+1]] + 1
    }
    else {
        tpm_pm[data$bin[i],data$bin[i+1]] <-tpm_pm[data$bin[i],data$bin[i+1]] + 1
    }
  }
}
rm(data)

tpm[1,]
#summary(tpm)
#print(tpm)
sum(tpm[1,])


## Sums of each row of TPM
spm<-numeric(maxBin)
for (i in 1 :nrow(tpm)){
    spm[i]=sum(tpm[i,])
}
sum(spm)

# repeat for am
spm_am<-numeric(maxBin)
for (i in 1 :nrow(tpm_am)){
    spm_am[i]=sum(tpm_am[i,])
}
sum(spm_am)

# repeat for pm
spm_pm<-numeric(maxBin)
for (i in 1 :nrow(tpm_pm)){
    spm_pm[i]=sum(tpm_pm[i,])
}
sum(spm_pm)

# check that am + pm = total
sum(spm_am)+sum(spm_pm)-sum(spm)

## Remove any rows or columns that contain only zeros.
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

# repeat for am only
tpmr_am<-tpm_am
count=0
bins_am<-seq(1,maxBin)
for (i in 1 :nrow(tpm_am)){
    if (sum(tpm_am[i,])==0) {
        bins_am<-bins_am[-(i-count)]
        tpmr_am<-tpmr_am[-(i-count),]
        tpmr_am<-tpmr_am[,-(i-count)]
        count=count+1
    }
}
print (paste(count," am bins were empty and have been removed"))
#str(tpmr)
bins_am
maxBin_am<-max(bins_am)

# repeat for pm only
tpmr_pm<-tpm_pm
count=0
bins_pm<-seq(1,maxBin)
for (i in 1 :nrow(tpm_pm)){
    if (sum(tpm_pm[i,])==0) {
        bins_pm<-bins_pm[-(i-count)]
        tpmr_pm<-tpmr_pm[-(i-count),]
        tpmr_pm<-tpmr_pm[,-(i-count)]
        count=count+1
    }
}
print (paste(count," pm bins were empty and have been removed"))
#str(tpmr)
bins_pm
maxBin_pm<-max(bins_pm)



## Sums of each row of TPMr
spmr<-numeric(nrow(tpmr))
for (i in 1 :nrow(tpmr)){
  spmr[i]=sum(tpmr[i,])
}
sum(spmr)

# repeat for am
spmr_am<-numeric(nrow(tpmr_am))
for (i in 1 :nrow(tpmr_am)){
    spmr_am[i]=sum(tpmr_am[i,])
}
sum(spmr_am)

# repeat for pm
spmr_pm<-numeric(nrow(tpmr_pm))
for (i in 1 :nrow(tpmr_pm)){
    spmr_pm[i]=sum(tpmr_pm[i,])
}
sum(spmr_pm)

## TPM as probabilities
tpmp<-tpmr
for (i in 1 :nrow(tpmr)){
    tpmp[i,]=tpmr[i,]/spmr[i]
}
sum(tpmp)

# repeat for am
tpmp_am<-tpmr_am
for (i in 1 :nrow(tpmr_am)){
    tpmp_am[i,]=tpmr_am[i,]/spmr_am[i]
}
sum(tpmp_am)

# repeat for pm
tpmp_pm<-tpmr_pm
for (i in 1 :nrow(tpmr_pm)){
    tpmp_pm[i,]=tpmr_pm[i,]/spmr_pm[i]
}
sum(tpmp_pm)



## TPM-> CPM: cumulative probabilities
cpm<-matrix(0, nrow = maxBin, ncol =maxBin)
cpm<-tpmp
for (i in 1 : nrow(tpmp)){
    for (j in 1 :ncol(tpmp)){
        cpm[i,j]=sum(tpmp[i,1:j])
    }
}
cpm<-cbind(bins,cpm)

# repeat for am
cpm_am<-matrix(0, nrow = maxBin_am, ncol =maxBin_am)
cpm_am<-tpmp_am
# TPM-> CPM: cumulative probabilities
for (i in 1 : nrow(tpmp_am)){
    for (j in 1 :ncol(tpmp_am)){
        cpm_am[i,j]=sum(tpmp_am[i,1:j])
    }
}
cpm_am<-cbind(bins_am,cpm_am)

# repeat for pm
cpm_pm<-matrix(0, nrow = maxBin_pm, ncol =maxBin_pm)
cpm_pm<-tpmp_pm
# TPM-> CPM: cumulative probabilities
for (i in 1 : nrow(tpmp_pm)){
    for (j in 1 :ncol(tpmp_pm)){
        cpm_pm[i,j]=sum(tpmp_pm[i,1:j])
    }
}
cpm_pm<-cbind(bins_pm,cpm_pm)

if(!min10){
  # write out to file
  write.table(tpmp,"../tpm/solar/Cam_tpmp.csv",sep=",",row.names=FALSE,col.names=FALSE)
  write.table(tpmp_am,"../tpm/solar/Cam_tpmp_am.csv",sep=",",row.names=FALSE,col.names=FALSE)
  write.table(tpmp_pm,"../tpm/solar/Cam_tpmp_pm.csv",sep=",",row.names=FALSE,col.names=FALSE)
} else {
  write.table(tpmp,"../tpm/solar/Cam_tpmp10min.csv",sep=",",row.names=FALSE,col.names=FALSE)
  write.table(tpmp_am,"../tpm/solar/Cam_tpmp_am10min.csv",sep=",",row.names=FALSE,col.names=FALSE)
  write.table(tpmp_pm,"../tpm/solar/Cam_tpmp_pm10min.csv",sep=",",row.names=FALSE,col.names=FALSE)
}


if(!min10){
  # write out to file
  write.table(cpm,"../tpm/solar/Cam_cpm.csv",sep=",",row.names=FALSE,col.names=FALSE)
  write.table(cpm_am,"../tpm/solar/Cam_cpm_am.csv",sep=",",row.names=FALSE,col.names=FALSE)
  write.table(cpm_pm,"../tpm/solar/Cam_cpm_pm.csv",sep=",",row.names=FALSE,col.names=FALSE)
} else {
  write.table(cpm,"../tpm/solar/Cam_cpm10min.csv",sep=",",row.names=FALSE,col.names=FALSE)
  write.table(cpm_am,"../tpm/solar/Cam_cpm_am10min.csv",sep=",",row.names=FALSE,col.names=FALSE)
  write.table(cpm_pm,"../tpm/solar/Cam_cpm_pm10min.csv",sep=",",row.names=FALSE,col.names=FALSE)
}

