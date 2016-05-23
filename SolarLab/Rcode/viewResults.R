# viewResults

res<-read.table("../results/wind13solar13geo3.csv",header=FALSE,sep=",")
names(res)=c("minpb","maxpb","meanpb","medianpb","mineb","maxeb","meaneb","medianeb")



library(rafalib)
mypar(4,2)
mains=c("min power balance",
        "max power balance",
        "mean power balance",
        "median power balance",
        "min energy balance",
        "max energy balance",
        "mean energy balance",
        "median energy balance"
        )
for (i in 1:8){
a<-hist(res[,i],breaks=50,main=mains[i],xlab=ifelse(i<=4,"Power surplus (MW)","Annual energy surplus (GWh)"))
b<-round(quantile(res[,i],c(0.05,0.95)),2)
abline(v=b,col="red")
text(b[1],0.9*max(a$counts),b[1],pos=2)
text(b[2],0.9*max(a$counts),b[2],pos=4)
}

