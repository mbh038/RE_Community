##

# declination angle
deltaOdot<-function(t){
  # t is the time in day number 1-365
  #t<-t*365
  asin(sin(-0.4091)*cos((2*pi/365.24)*(t+10)+0.0334*sin((2*pi/365.24)*(t-2))))
}

#sunrise and sunset hour angle
h0<-function(phi,t){
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

library(lubridate)
Cam2001<- read.table("./data/project_label_BSRN_event_label_CAM_2001.tab",sep="\t", header=TRUE,stringsAsFactors=FALSE)
str(Cam2001)
names(Cam2001)<-c("datetime","DIF","LWD","SWD","DIR","ORG")

# convert time to POSIXct
Cam2001$Date.Time<-ymd_hms(Cam2001$datetime)

attach(Cam2001)

Npoint<-7*1440
plot(SWD[1:Npoint],type="l")


######################################################################
day<-305
phi=(pi/180)*50
S0=1368
t=seq(1,366,length.out=365*1440)
daybegin<-day*1440
dayend<-(day+7)*1440
Q<-solarFlux(S0,phi,t)

plot(SWD[daybegin:dayend],type="l",ylim=c(0,1500))
lines(Q[daybegin:dayend],type="l",col="blue")
