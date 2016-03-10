##Calculate sunrise and sunset times

phi=(pi/180)*50

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

df<-data.frame(seq(1,365),rep(0,365),rep(0,365))
names(df)<-c("day","sunrise","sunset")

for (i in 1:365){
  df$sunrise[i]<-h0(phi,i)
  temp<-24*(df$sunrise[i]+pi)/(2*pi)-12
  df$sunrise[i]<-1440*i+60*temp
  df$sunset[i]<-1440*i+60*(24- temp)
}

summary(df$sunrise)
plot(df$sunset-df$sunrise)

write.csv(df,"../data/h0times.csv",row.names = FALSE)

