geoMWe=3
for (windMW in seq(0,30, by=5)){
  for (solarMWp in seq(0,30,by=5)){
    

    ipfilename=paste0("wind",windMW,"solar",solarMWp,"geo",geoMWe,".csv")
    ipfilepath="../results/"
    res<-read.csv(paste0(ipfilepath,ipfilename),header=FALSE)
    colnames(res)<-c("minP","maxP","meanP","medP","minE","maxE","meanE","medE")
    
    library(rafalib)
    mypar(4,2)
    par(oma=c(0,0,2,0))
    hist(res[,1],breaks=50,main="min pbalance")
    hist(res[,2],breaks=50,main="max pbalance")
    hist(res[,3],breaks=50,main="mean pbalance")
    hist(res[,4],breaks=50,main="median pbalance")
    hist(res[,5],breaks=50,main="min ebalance")
    hist(res[,6],breaks=50,main="max ebalance")
    hist(res[,7],breaks=50,main="mean ebalance")
    hist(res[,8],breaks=50,main="median ebalance")
    title(paste0(windMW,"MW wind, ",solarMWp,"MWp solar, ",geoMWe,"MWe geo"), outer=TRUE)
    
    summary(res)
    
    # mypar(3,1)
    # days<-seq(1,1000)/144
    # plot(days,demand[1:1000],type="l",
    #      ylim=c(-12,12),
    #      xlab="Winter days",
    #      ylab="Power (MW)"
    # )
    # lines(days,solarop[1:1000],col="red")
    # lines(days,windop[1:1000],col="blue")
    # lines(days,balance[1:1000],col="green")
    # 
    # plot(days,demand[25001:26000],type="l",
    #      ylim=c(-12,12),
    #      xlab="Summer days",
    #      ylab="Power (MW)"
    # )
    # lines(days,solarop[25001:26000],col="red")
    # lines(days,windop[25001:26000],col="blue")
    # lines(days,balance[25001:26000],col="green")
    # 
    # ydays<-seq(1,length(ebalance))/144
    # plot(ydays,ebalance,type="l")
    # 
    # mypar(3,1)
    # hist(demand)
    # hist(totalop)
    # hist(balance)
    
  }
}

