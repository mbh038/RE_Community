## Read BSRN 2002 data, write out cleaned csv file

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
#####################################################################

library(lubridate)
Cam2002<- read.table("../data/project_label_BSRN_event_label_CAM_2002.tab",sep="\t", header=TRUE,stringsAsFactors=FALSE)
str(Cam2002)
summary(Cam2002)
names(Cam2002)<-c("datetime","DIF","LWD","SWD","DIR","ORG")

# convert time to POSIXct
Cam2002$datetime<-ymd_hms(Cam2002$datetime)
# remove last column
Cam2002$ORG<-NULL


# Inspect data
######################################################################
str(Cam2002)
summary(Cam2002)

phi=(pi/180)*50
S0=1050

t=seq(1,366,length.out=365*1440)
Q<-solarFlux(S0,phi,t)

day<-355
dayspan<-10
daybegin<-(day-1)*1440+1
dayend<-(day+dayspan-1)*1440+1

plot(t[daybegin:dayend],Cam2002$SWD[daybegin:dayend],type="l",ylim=c(0,500))
lines(t[daybegin:dayend],Q[daybegin:dayend],type="l",col="blue")

plot(Cam2001$datetime[daybegin:dayend])
Cam2001$datetime[(daybegin+270):(daybegin+280)]

# Save data
######################################################################
saveRDS(Cam2002, "../data/cleaned/Cam2002n.csv")
rm(Cam2002)