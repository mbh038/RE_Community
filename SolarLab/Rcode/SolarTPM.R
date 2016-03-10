#generates a tpm and cpm matric from actual solar data.

# Solar Functions
################################################################################
# declination angle
deltaOdot<-function(t){
  # t is the time in day number 1-365
  #t<-t*365
  asin(sin(-0.4091)*cos((2*pi/365.24)*(t+10)+0.0334*sin((2*pi/365.24)*(t-2))))
}

#sunrise and sunset hour angle
h0<-function(phi,t){
  #t is day number
  acos(tan(phi)*tan(deltaOdot(t)))
}

#cosine of zenith angle
cosTheta<-function(phi,delta,h){
  # theta is the zenith angle
  sin(phi)*sin(delta)+cos(phi)*cos(delta)*cos(h)
}

# solar flux
solarFlux<-function(S0,phi,t){
  # S0 is solar constant
  # phi is latitude in radians
  # t is day of year
  
  # hour angle -pi to pi
  h<-pi*(2*(t-floor(t))-1)
  # declination angle
  delta<-deltaOdot(t)
  flux<-S0*cosTheta(phi,delta,h)
  # make it zero at nighttime (when cos theta is negative)
  flux<-pmax(flux,rep(0,length(t)))
  flux
}

# Load data
########################################################################
Cam2001n<-readRDS("../data/cleaned/Cam2001n.csv")

# Markov Chain
########################################################################

reference<-data.frame(Cam2001n$datetime,Cam2001n$SWD)
names(reference)<-c("datetime","SWD")

# log transform the data
reference$SWD<-log(reference$SWD)
reference$SWD[reference$SWD=="-Inf"]=0
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
#     if (is.na(tpm[reference$bin[i],reference$bin[i+1]])==TRUE){
#         print (i)
#         print (reference$bin[i])
#         print (reference$bin[i+1])
#         
#     }
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

write.table(cpm,"../tpm/Cam2001cpm.csv",sep=",",row.names=FALSE,col.names=FALSE)

