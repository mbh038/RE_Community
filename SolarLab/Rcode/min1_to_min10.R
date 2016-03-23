## Convert per minute data to per 10 minutes -take average of each 10 minute block

# requires cleaned data

library (dplyr)

file_handles<-c("Cam2001n","Cam2002n","Cam2003n","Cam2004n","Cam2005n","Cam2006n","Cam2007n","Cam2008n","Cam2014n","Cam2015n")
pathfrom<-"../data/cleaned/solar/CamBSRN_Solar1min/"
pathto<-"../data/cleaned/solar/CamBSRN_Solar10min/"

for (file in 1:length(file_handles)){
  fullname<-paste0(pathfrom,file_handles[file],".rds")
  print (fullname)
  data<-readRDS(fullname)
  data$test<-as.POSIXlt(data$datetime)
  data$test$min<-10*(data$test$min %/% 10)
  table(data$test$min)
  data$datetime<-as.POSIXct(data$test)
  data$test<-NULL
  newdata<-data %>% group_by(datetime) %>% summarise_each(funs(mean))
  newname<-paste0(pathto,file_handles[file],"10min",".rds")
  saveRDS(newdata,newname)
  rm(data,newdata)
}