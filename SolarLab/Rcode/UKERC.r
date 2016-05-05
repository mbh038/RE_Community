##

path<-"../data/profiles/UKERC/"

file<-"UKERCdays.csv"
days<-read.csv(paste0(path,file),stringsAsFactors=FALSE)

daytypes<-unique(days[,2])
length(daytypes)
