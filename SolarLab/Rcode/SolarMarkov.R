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

#####################################################################

# Load data
#####################################################################

# cleaned data
Cam2001n<-readRDS("../data/cleaned/Cam2001n.csv")

# cpm matrix for given location
cpm<-read.table("../tpm/Cam2001cpm.csv",sep=",")
# first column is the bins,so separate that off
bins<-cpm[,1]
cpm[,1]<-NULL

phi=(pi/180)*50
S0=1050

reference<-data.frame(Cam2001n$datetime,Cam2001n$SWD)
names(reference)<-c("datetime","SWD")
#Stochastic generation of synthetic data
v=rep(0,(nrow(reference)))
#randomly choose first solar value
v[1]<-0.5*solarFlux(S0,phi,2/1440)
swd<-v
for (i in 2:(1*1440)){
    colIndex=runif(1)
    j=1
    #print (i-1)+1
    while (cpm[min(max(1,round(v[i-1],0)),nrow(cpm)),min(j,ncol(cpm))] < colIndex){
        j=j+1
    }
    #print (j)
    v[i]=bins[j]
    Q<-solarFlux(S0,phi,i/1440)
    swd[i]<-min(Q,exp((v[i]/100)*7.18))
    
    #swd[i]<-Q*v[i]/100
}



summary(v)
summary(swd)
hist(swd)
summary(reference$bin)

t=seq(1,366,length.out=365*1440)
Q<-solarFlux(S0,phi,t)

day<-1
dayspan<-1
daybegin<-day*1440
dayend<-(day+dayspan)*1440

plot(t[daybegin:dayend],Cam2001n$SWD[daybegin:dayend],type="l",ylim=c(0,500))
lines(t[daybegin:dayend],swd[daybegin:dayend],type="l",col="blue")
lines(t[daybegin:dayend],Q[daybegin:dayend],type="l",col="red")