# viewResults

# shows statistics of the 1000 run trials

res<-read.table("../results/wind13solar13geo3.csv",header=FALSE,sep=",")
names(res)=c("minpb","maxpb","meanpb","medianpb","mineb","maxeb","meaneb","medianeb")

png("../results/ThousandTrialHistograms_wsg_131303.png",width=595,height=842)

library(rafalib)
mypar(4,2)
mains=c("Min power balance",
        "Max power balance",
        "Mean power balance",
        "Median power balance",
        "Min energy balance",
        "Max energy balance",
        "Mean energy balance",
        "Median energy balance"
        )

for (i in 1:8){
a<-hist(res[,i],breaks=50,main=mains[i],xlab=ifelse(i<=4,"Power surplus (MW)","Annual energy surplus (GWh)"))
b<-round(quantile(res[,i],c(0.025,0.975)),2)
abline(v=b,col="red")
text(b[1],0.9*max(a$counts),b[1],pos=2)
text(b[2],0.9*max(a$counts),b[2],pos=4)
}
dev.off()

