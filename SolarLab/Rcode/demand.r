# EESP demand file names
demfilepathstem<-"../data/profiles/EESP/"
demfilepathtail<-"DomesticDemand.csv"
demMonths<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

mdem<-data.frame()
dem5<-data.frame()

# loop over monthly 5 minute EESP demand files
for (month in demMonths){
  demfilename<-paste0(demfilepathstem,month,demfilepathtail)
  #print(demfilename)
  mdem<-read.table(demfilename,stringsAsFactors=FALSE,sep=",",header=FALSE)
  mday=nrow(mdem[!is.na(mdem[,3]),])/(24*12)
  #print (mday)
  dem5<-rbind(dem5,mdem[!is.na(mdem[,3]),][,2:3])
  #print(length(dem))
}

# make 10 minute file for whole year
dem10<-numeric()
dem10<-(dem5[,2][c(TRUE,FALSE)]+dem5[,2][c(FALSE,TRUE)])/2
wds<-dem5[,1][c(TRUE,FALSE)]

pts<-seq(1,length(dem10))
minute<-rep(seq(1,144),365)
day<-as.integer((pts-1) %/% 144 + 1)

domDem10<-data.frame(pts,day,wds,minute,dem10)
names(domDem10)=c("id","day","wd","minute10","W")
# write to profiles folder
write.csv(domDem10,"../data/profiles/EESP/domDem10.csv",row.names=FALSE)
head(domDem10)
# clean up workspace
rm(list=ls()[ls()!="domDem10"])


# Plot demand profiles for winter and summer

winterwe<-domDem10[domDem10$day==4,]
winterwd<-domDem10[domDem10$day==1,]
summerwe<-domDem10[domDem10$day==183,]
summerwd<-domDem10[domDem10$day==180,]

library(rafalib)
mypar(2,1)
plot(winterwd$minute10/6,winterwd$W,type="l",col="blue",ylim=c(0,800),xlab="t (hour)",ylab="Demand (W)",main="Weekday domestic demand")
lines(summerwd$minute10/6,summerwd$W,col="red")

plot(winterwe$minute10/6,winterwe$W,type="l",col="blue",ylim=c(0,800),xlab="t (hour)",ylab="Demand (W)", main="Weekend domestic demand")
main="Weekend domestic demand"
lines(summerwe$minute10/6,summerwe$W,col="red")

# Average demand
sum(domDem10$W/nrow(domDem10))

# Peak demand
max(domDem10$W)

# Load factor
max(domDem10$W) / sum(domDem10$W/nrow(domDem10))
