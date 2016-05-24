## Wind Markov

# generates synthetic wind data, given a CPM

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

cw46<-filter(data,height=="V46")
cw32<-filter(data,height=="V32")
cw20<-filter(data,height=="V20")

## TPM 20

reference<-cw46

reference$bin<-floor(reference$V)+1
table(reference$bin)
sum(table(reference$bin))

maxBin=max(reference$bin)


opfilepathstem<-"../data/synthetic/CallywithWind10minSyn/Cally"
opfilepathtail<-"_10min.csv"
colnames<-c("tmin","v")

ndata<-6*24*365

for (file in 1:100){
    
    # set up output file numbers
    opfilehandle<-as.character(file)
    if (file < 10){
        opfilehandle<-paste0("00",opfilehandle)
    }
    if (file >= 10 && file < 100){
        opfilehandle<-paste0("0",opfilehandle)
    }
    opfilename<-paste0(opfilepathstem,opfilehandle,opfilepathtail)

    ## Stochastic generation of synthetic data
    v=numeric(ndata)
    #randomly choose first wind speed
    v[1]=floor((maxBin*.99)*runif(1))+1
    
    for (i in 2:ndata){
        colIndex=runif(1)
        j=1
        while (cpm[round(v[i-1],0),j] < colIndex){
            j=j+1
        }
        v[i]=j
    }
    
    
    summary(v)
    summary(reference$bin)
    
#     v=v-1+runif(ndata)
#     mean(v)-mean(reference$V)
#     sd(v)-sd(reference$V)
#     summary(v)
#     summary(reference$V)
#     library(MASS)
#     vFit<-fitdistr(v+.01, 'weibull')
#     print (vFit)
#     refFit<-fitdistr(reference$V+.01, 'weibull')
#     print(refFit)
#     hist(v,xlim=c(0,20),prob=TRUE)
#     d = dweibull(seq(0,20,.2),vFit$estimate[1],vFit$estimate[2])
#     points(seq(0,20,.2),d,type='l',col=2)
#     
#     begin<-
#     plot(v[1:1000],type="l")
#     lines(reference$V[1:1000],type="l",col="red")
    
    newdata<-data.frame(1:ndata,v)
    names(newdata)<-colnames
    write.csv(newdata,opfilename,row.names=FALSE)
    print(paste("File",opfilehandle,"done",sep=" "))

}