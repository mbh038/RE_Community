##

path<-"../data/profiles/UKERC/"

file<-"UKERCdays.csv"
days<-read.table(paste0(path,file),stringsAsFactors=FALSE,header=FALSE,sep=",")[,2]

daytypes<-unique(days)
length(daytypes)

pc10thin<-data.frame(seq(1,52560),seq(1,52560),seq(1,52560),rep("a",52560),seq(1,52560))
names(pc10thin)=c("id","day","t10","daytype","W")
pc10thin$id<-seq(1,52560)
pc10thin$day<-(pc10thin$id -1 )%/% 144 + 1
pc10thin$t10<-rep(seq(1,144),365)
pc10thin$daytype<-days[pc10thin$day]


ipPath30<-"../data/profiles/UKERC/pc30min/"
opPath10<-"../data/profiles/UKERC/pc10minThin/"


for (filenum in 1:8){
  
  ipfile30<-paste0("pc30.",filenum,".csv")
  
  pc30<-read.csv(paste0(ipPath30,ipfile30),stringsAsFactors=FALSE)
  
  # make the last line the first
  pc30<-rbind(pc30[nrow(pc30),],pc30[-nrow(pc30),])
  
  pc10fat <- read.table(text = "",row.names=NULL,col.names = names(pc30),colClasses=rep("numeric",ncol(pc30)))
  
  
  for (i in 1:(nrow(pc30)-1)){
    pc10fat[3*i-2,]=pc30[i,]
    pc10fat[3*i-1,]=pc30[i,]+(1/3)*(pc30[i+1,]-pc30[i,])
    pc10fat[3*i,]=pc30[i,]+(2/3)*(pc30[i+1,]-pc30[i,])
    #print (i)
  }
  
  # add the last one
  pc10fat[3*nrow(pc30)-2,]=pc30[nrow(pc30),]
  pc10fat[3*nrow(pc30)-1,]=pc30[nrow(pc30),]+(1/3)*(pc30[1,]-pc30[nrow(pc30),])
  pc10fat[3*nrow(pc30),]=pc30[nrow(pc30),]+(2/3)*(pc30[1,]-pc30[nrow(pc30),])
  
  row.names(pc10fat)<-NULL
  count<-vector()
  for (i in 1:nrow(pc10thin)){
    #count[i]<-which(daytypes==pc10thin$daytype[i])+1  
    pc10thin$W[i]=1000*pc10fat[pc10thin$t10[i],which(daytypes==pc10thin$daytype[i])]
  }
  pc10thin$W<-pc10thin$W/max(pc10thin$W) # normalise
  print (paste0("File: ",filenum," done"))
  opfile10<-paste0("pc10.",filenum,".csv")
  write.csv(pc10thin,paste0(opPath10,opfile10),row.names=FALSE)
}
