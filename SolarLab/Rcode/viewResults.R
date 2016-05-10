# viewResults

res<-read.table("../results/wind0solar40.csv",header=FALSE,sep=",")
names(res)=c("minpb","maxpb","meanpb","medianpb","mineb","maxeb","meaneb","medianeb")

library(rafalib)
mypar(4,2)
hist(res[,1],breaks=50,main="min pbalance")
hist(res[,2],breaks=50,main="max pbalance")
hist(res[,3],breaks=50,main="mean pbalance")
hist(res[,4],breaks=50,main="median pbalance")
hist(res[,5],breaks=50,main="min ebalance")
hist(res[,6],breaks=50,main="max ebalance")
hist(res[,7],breaks=50,main="mean ebalance")
hist(res[,8],breaks=50,main="median ebalance")