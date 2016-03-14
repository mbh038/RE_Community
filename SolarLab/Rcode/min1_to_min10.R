## Convert per minute data to per 10 minutes -take average of each 10 minute block

# requires cleaned data

library (dplyr)

file_handles<-c("Cam2001n","Cam2002n")
path<-"../data/cleaned/"

for (file in 1:length(file_handles)){
  fullname<-paste0(path,file_handles[file],".csv")
  #print (fullname)
  data<-readRDS(fullname)
  data$test<-as.POSIXlt(data$datetime)
  data$test$min<-10*(data$test$min %/% 10)
  table(data$test$min)
  data$datetime<-as.POSIXct(data$test)
  data$test<-NULL
  newdata<-data %>% group_by(datetime) %>% summarise_each(funs(mean))
  newname<-paste0(path,file_handles[file],"10min",".csv")
  saveRDS(newdata,newname)
  rm(data,newdata)
}