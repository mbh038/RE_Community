## Read BSRN data, write out cleaned csv file


# Load data
#####################################################################

library(lubridate)
Cam2001<- read.table("../data/project_label_BSRN_event_label_CAM_2001.tab",sep="\t", header=TRUE,stringsAsFactors=FALSE)
str(Cam2001)
summary(Cam2001)
names(Cam2001)<-c("datetime","DIF","LWD","SWD","DIR","ORG")

# convert time to POSIXct
Cam2001$datetime<-ymd_hms(Cam2001$datetime)
# remove last column
Cam2001$ORG<-NULL

# Inspect data
######################################################################
str(Cam2001)
summary(Cam2001)

phi=(pi/180)*50
S0=1050



t=seq(1,366,length.out=365*1440)
Q<-solarFlux(S0,phi,t)

day<-308
dayspan<-1
daybegin<-day*1440
dayend<-(day+dayspan)*1440

plot(t[daybegin:dayend],Cam2001$SWD[daybegin:dayend],type="l",ylim=c(0,1500))
lines(t[daybegin:dayend],Q[daybegin:dayend],type="l",col="blue")

plot(Cam2001$datetime[daybegin:dayend])
Cam2001$datetime[(daybegin+270):(daybegin+280)]

# rows missing, times from "2001-11-05 04:36:00 UTC" "2001-11-05 12:03:00 UTC"
# Include missing rows
begin<-as.POSIXct("2001-11-05 04:37:00 UTC")
end<-as.POSIXct("2001-11-05 12:02:00 UTC")
missing<-cbind(seq(begin,end,by="1 min"),as.data.frame(matrix(rep(0,446*4),446,4)))
names(missing)<-names(Cam2001)
first<-Cam2001[1:443797,]
last<-Cam2001[443798:nrow(Cam2001),]
Cam2001n<-rbind(first,missing,last)

plot(Cam2001n$datetime[daybegin:dayend])
Cam2001n$datetime[(daybegin+270):(daybegin+280)]

str(Cam2001n)
summary(Cam2001n)

saveRDS(Cam2001n, "../data/cleaned/Cam2001n.csv")
