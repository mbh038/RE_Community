## Make tpm images

tpm<-read.table("../tpm/solar/Cam_tpmp10min.csv",sep=",")
tpm_am<-read.table("../tpm/solar/Cam_tpmp_am10min.csv",sep=",")
tpm_pm<-read.table("../tpm/solar/Cam_tpmp_pm10min.csv",sep=",") 



image(as.matrix(tpm),col  = gray((32:0)/32))
image(as.matrix(tpm_am),col  = gray((32:0)/32))
image(as.matrix(tpm_pm),col  = gray((32:0)/32))