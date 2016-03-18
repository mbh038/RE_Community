
source("SolarFunctions.R")
#####################################################################

# Load data
#####################################################################

# cleaned data
#data<-readRDS("../data/cleaned/Cam2001n.csv")
data<-readRDS("../data/cleaned/solar/CamBSRN_Solar1min/Cam2014n.rds")

# sunrise and sunset times
srss<-read.csv("../data/h0times.csv",sep=",")
str(srss)
srss$sunrise<-as.integer(srss$sunrise)
srss$sunset<-as.integer(srss$sunset)

# cpm matrix for given location
cpm<-read.table("../tpm/solar/Cam_cpm.csv",sep=",")
cpm_am<-read.table("../tpm/solar/Cam_cpm_am.csv",sep=",")
cpm_pm<-read.table("../tpm/solar/Cam_cpm_pm.csv",sep=",")
# first column is the bins,so separate that off
#cpm[1,]<-NULL
bins<-cpm[,1]
cpm[,1]<-NULL
bins_am<-cpm_am[,1]
cpm_am[,1]<-NULL
bins_pm<-cpm_pm[,1]
cpm_pm[,1]<-NULL


phi=(pi/180)*50
S0=1150


#Stochastic generation of synthetic data
perday<-1440
mdays<-c(31,28,31,30,31,30,31,31,30,31,30,31)
cmdays<-c(0,cumsum(mdays)[1:11])

permonth<-mdays*perday
maxlim<-10
day1<-1
dayspan<-365
daybegin<-1+(day1-1)*perday
dayend<-(day1+dayspan-1)*perday

datrange<-range(data$SWD[daybegin:dayend])[2]

t=seq(daybegin,dayend)
Q<-solarFlux(S0,phi,t/perday)
v<-rep(0,(length(t)))
swd<-rep(0,(length(t)))

for (k in day1:(day1+dayspan-1)){
    
    rmean<-ifelse(k < 90 | k > 270,0.39,0.55)
    rsd<-ifelse(k < 90 | k > 270,0.4,0.2)
    
    #cat(k," ")
    dbegin<-1+(k-1)*perday
    dend<-k*perday
    for(minute in dbegin:srss$sunrise[k]){
        swd[minute-daybegin]=0
    }
    for(minute in srss$sunset[k]:dend){
        swd[minute-daybegin]=0
    }
    v[srss$sunrise[k]+1]<-90#round((nrow(cpm)-1)*runif(1),0)+1#ifelse(k==1,round((nrow(cpm)-1)*runif(1),0)+1,v[srss$sunset[k-1]-1])
    mincount=0
    maxcount=0
    for (i in (srss$sunrise[k]+2):srss$sunset[k]){
        
        mincol=1
        

        colIndex=min(1,max(0.1,rnorm(1,0,rsd)+rmean))
        
        j=floor(ncol(cpm_am))/2
        
        if(as.POSIXlt(data$datetime[i])$hour <12){
            maxcol=ncol(cpm_am)
            
            while (cpm_am[v[i-1],j] < colIndex){
                mincol=j
                j=mincol+(maxcol-min)/2
            }
            
        } else {
            maxcol=ncol(cpm_pm)
            j=floor(ncol(cpm_pm))/2
            while (cpm_pm[v[i-1],j] < colIndex){
                j=j+1
            }           
        }

        v[i]=j
#         if (j==1 && v[i-1]==1) mincount=mincount+1
#         if (j==max(bins) && v[i-1]==max(bins) ) maxcount =maxcount+1
#         if (mincount == maxlim | maxcount == maxlim){
#             print(paste("maxlim reached: min= ",mincount," max= ",maxcount))
#             v[i]=max(bins)/2
#             mincount=0
#             maxcount=0
#         }
        #v[i]=min(j,nrow(cpm))
        #v[i]=max(1,j)
        Qnow<-solarFlux(S0,phi,i/perday)
        swd[i-daybegin]<-(v[i]/max(bins))*Qnow #*datrange)
    }
    #cat(k,":",round(v[i-1],0),j," ")
    cat (k,",",bins[v[i]],": ",sep="")
}

# Summary of outputs compared to real data
sum(swd)/sum(Q)
sum(swd)/sum(data$SWD[daybegin:dayend])
sum(data$SWD[daybegin:dayend])/sum(Q)

months<-dayend/(30*perday)
simtotal=0
meastotal=0
for (i in 1:months) {
    monthbegin<-cmdays[i]*perday+1
    monthend<-(cmdays[i]+mdays[i])*perday
    simtotal<-simtotal+sum(swd[monthbegin:monthend])
    meastotal<-meastotal+sum(data$SWD[monthbegin:monthend])
    monthratio<-sum(swd[monthbegin:monthend])/sum(data$SWD[monthbegin:monthend])
    print(paste(i,": ",monthbegin,monthend,round(monthratio,2),round(simtotal/meastotal,2)))
}

sum(data$SWD[daybegin:dayend])/(60*1000)
sum(swd)/(60*1000)

summary(swd[swd>0])
hist(swd[swd>0],breaks=50)
summary(data$SWD[data$SWD>0])
hist(data$SWD[data$SWD>0],breaks=50)

# plot(t/1440,Cam2001n$SWD[daybegin:dayend],type="l",ylim=c(0,1500))
# lines(t/1440,swd,type="l",col="blue")
# lines(t/1440,Q,type="l",col="red")

#day<-183

for (day in seq(day1,(day1+dayspan-1),by=3)){
    start<-srss[day,2]
    end<-srss[day,3]
    
    ymax<-max(max(Q[start:end]),max(data$SWD[start:end]))
    
    plot(t[start:end]/perday,data$SWD[start:end],type="l",ylim=c(0,ymax))
    lines(t[start:end]/perday,swd[start:end],type="l",col="blue")
    lines(t[start:end]/perday,Q[start:end],type="l",col="red")
}

## Write data to 1min and 10min files
library(dplyr)
newdata<-as.data.frame(cbind(t,swd))
names(newdata)<-c("minutes","swd")
write.csv(newdata,"../data/synthetic/CamBSRN_Solar1minSyn/Cam002_1min.csv")
t10<-10*(t %/% 10)
newdata<-as.data.frame(cbind(t10,swd))
names(newdata)<-c("minutes","swd")
new10<-newdata %>% group_by(minutes) %>% summarise_each(funs(mean))
write.csv(new10,"../data/synthetic/CamBSRN_Solar10minSyn/Cam002_10min.csv")

# clean up
rm(swd,t,t10,v,newdata,new10)
