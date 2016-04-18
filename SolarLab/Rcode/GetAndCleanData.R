## Read BSRN data, write out cleaned csv file

source ("SolarFunctions.R")
# Load data
#####################################################################

year<-"2015"
days<-ifelse(as.numeric(year) %% 4 == 0,366,365)
file_stem<-"../data/raw/project_label_BSRN_event_label_CAM_"
file_end<-".tsv"
full_name<-paste0(file_stem,year,file_end)

library(lubridate)
data<- read.table(full_name,sep="\t", header=TRUE,stringsAsFactors=FALSE)
str(data)
summary(data)
names<-c("datetime","DIF","LWD","SWD","DIR","ORG")
names(data)<-names

# convert time to POSIXct
data$datetime<-ymd_hms(data$datetime)
# remove last column
data$ORG<-NULL

# add first and last minute rows, if missing

firstmin<-ymd_hms(paste0(year,"/01/01 00:00:00"))
if (data$datetime[1]!=firstmin){
    
    missing<-cbind(firstmin,as.data.frame(matrix(rep(0,4),1,4)))
    names(missing)<-names(data)
    data<-rbind(missing,data)
}

finalmin<-ymd_hms(paste0(year,"/12/31 23:59:00"))
if (data$datetime[nrow(data)]!=finalmin){
    
    missing<-cbind(finalmin,as.data.frame(matrix(rep(0,4),1,4)))
    names(missing)<-names(data)
    data<-rbind(data,missing)
}
# Inspect data
######################################################################
str(data)
summary(data)

phi=(pi/180)*50
S0=1050

t=seq(1,days+1,length.out=days*1440)
Q<-solarFlux(S0,phi,t)

# detect missing rows - insert missing datetimes
last<-vector()
first<-vector()
begin<-as.POSIXct(vector())
end<-as.POSIXct(vector())
nmissing<-vector()
count=0
first[1]<-1
for (i in 2:(length(t)-1)){
    if (as.numeric(data$datetime[i]-data$datetime[i-1])>1){
        count=count+1
        last[count]<-i-1
        first[count+1]<-i
        begin[count]<-(data$datetime[i-1]+60)
        end[count]<-(data$datetime[i]-60)
        nmissing[count]<-difftime(end[count],begin[count],units="min")+1
        print (paste0("Missing data from: ",begin[count]," to: ",end[count]," N missing: ",nmissing[count]))
        missing<-cbind(seq(begin[count],end[count],by="1 min"),as.data.frame(matrix(rep(0,nmissing[count]*4),nmissing[count],4)))
        names(missing)<-names(data)
        firstpart<-data[1:last[count],]
        lastpart<-data[(last[count]+1):nrow(data),]
        data<-rbind(firstpart,missing,lastpart)
        print("Hello world")
        #names(data)<-names
        i<-i+nmissing[count]
    }
}
last[count+1]=nrow(data)
last
first
nmissing


# Inspect portions of the data

day<-355
dayspan<-10
daybegin<-day*1440
dayend<-(day+dayspan)*1440

plot(t[daybegin:dayend],data$SWD[daybegin:dayend],type="l",ylim=c(0,1500))
lines(t[daybegin:dayend],Q[daybegin:dayend],type="l",col="blue")


str(data)
summary(data)

## Impute NAs (check there are not too many)
narows<-which(is.na(data$SWD))
narows
if(sum(narows)>0){
    #data$SWD[narows]<-mean(data$SWD[min(narows)-1],data$SWD[max(narows)+1])
    data$SWD[narows]<-0
}
summary(data)
str(data)

# save cleaned data to csv
opfile_stem<-"../data/cleaned/solar/CamBSRN_Solar1min/Cam"
opfile_end<-"n.rds"
opfullfilename<-paste0(opfile_stem,year,opfile_end)
saveRDS(data, opfullfilename)

# clean workspace
rm(data,Q,t)
