library("solaR")

###################################################
### Section "Solar Geometry"
###################################################
lat <- 37.2
SolD <- fSolD(lat,BTd=fBTd(mode="prom"))
SolI <- fSolI(SolD, sample="10 min", keep.night=FALSE)


### Figure 2
mon <- month.abb
p<-xyplot(r2d(AlS)~r2d(AzS),
    groups=month,
    data=SolI, type="l", col="black",
    xlab=expression(psi[s]),ylab=expression(gamma[s]))

p + glayer({
  idx <- round(length(x)/2+1)
  panel.text(x[idx], y[idx], mon[group.value], pos=3, offset=0.2, cex=0.8)
})


### Declination with several methods
lat <- 37.2
BTd <- fBTd(mode="serie")
solStrous <- fSolD(lat, BTd, method="strous")
solSpencer <- fSolD(lat, BTd, method="spencer")
solCooper <- fSolD(lat, BTd, method="cooper")
solMichalsky <- fSolD(lat, BTd, method="michalsky")
decDif <- solMichalsky$decl - cbind(solStrous$decl, solSpencer$decl, solCooper$decl)
names(decDif) <- c("strous", "spencer", "cooper")

xyplot(decDif, auto.key=list(space="right"), superpose=TRUE, ylab="Declination difference")

###################################################
### Section "Solar Radiation"
###################################################

### Subsection "Irradiation and irradiance on the horizontal plane"

## Example for the day no.100

BTd <- fBTd(mode="serie")
SolD<-fSolD(lat, BTd[100])
SolI<-fSolI(SolD, sample="hour")

G0d <- zoo(5000, index(SolD))
fCompD(SolD, G0d, corr = "Page") 
fCompD(SolD, G0d, corr = "CPR") 


## Example with the monthly average days
lat <- 37.2
sol<-calcSol(lat, fBTd(mode="prom"), sample="hour", keep.night=FALSE)
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000
Ta <- c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
BD<-readG0dm(G0dm=G0dm, Ta=Ta, lat=37.2)
compD<-fCompD(sol, BD, corr = "Page") 
compI<-fCompI(sol, compD)


## Figure 4
xyplot(G0+B0+D0~w|month,
       data=compI, type="l",
       xlab=expression(omega),
       ylab="",
       auto.key=list(space="right"))



### Subsection "Meteorological data"

data("helios")
names(helios)=c("date", "G0", "TempMax", "TempMin")

bd <- df2Meteo(helios, dates.col="date", lat=41, source="helios-IES", format="%Y/%m/%d")

bd




### Example with readSIAR
### Figure 5
library("sp")
library("maptools")

## http://solar.r-forge.r-project.org/data/SIAR.csv
SIAR <- read.csv("Data/SIAR.csv")
proj <- CRS("+proj=longlat +ellps=WGS84")
spSIAR <- SpatialPointsDataFrame(SIAR[, c(6, 7)], SIAR[, -c(6, 7)],
                                 proj4str=proj)


## Shapefile with the administrative borders of Spain
## http://www.gadm.org/data/shp/ESP_adm.zip
unzip("Data/ESP_adm.zip", exdir="Data/ESP_adm")
mapaSHP <- readShapeLines("Data/ESP_adm/ESP_adm2.shp", proj4string=proj)

p <- spplot(spSIAR["Comunidad"],
       col.regions=brewer.pal(n=12, "Paired"),
       key.space="right", scales=list(draw=TRUE))

p  + layer(sp.lines(mapaSHP, lwd=0.6))

### Figure 6
## readSIAR needs internet connection
Aranjuez <- readSIAR(28,3,"01/01/2009","31/12/2009")
## save(Aranjuez, file="Data/Aranjuez.RData")

## If needed, load the local copy of previous code
load("Data/Aranjuez.RData")

xyplot(G0~TempMedia|month, data=Aranjuez, type=c("p", "r"))

### Example with fTemp
### Figure 7
lat <- 41 
sol <- calcSol(lat, BTd=indexD(Aranjuez), sample="hour")
Temp <- fTemp(sol,Aranjuez)

wTemp <- window(Temp, start=as.POSIXct("2009-03-01"), end=as.POSIXct("2009-03-31"))
xyplot(wTemp, col="black", ylab="T")+layer_(panel.xblocks(x, DoY, col=c("lightgray", "white")))


###################################################
### Section "The function calcG0"
###################################################
g0<-calcG0(lat=37.2,
	modeRad="siar",
	dataRad=list(prov=28,est=3,
	start="01/01/2009", end="31/12/2009"))

### Figure 8
xyplot(g0)

### Example with NREL-MIDC

## NREL-Hawaii.csv obtained from
## http://www.nrel.gov/midc/apps/plot.pl?site=LANAI&start=20090722&edy=19&emo=11&eyr=2010&zenloc=19&year=2010&month=11&day=1&endyear=2010&endmonth=11&endday=19&time=1&inst=3&inst=4&inst=5&inst=10&type=data&first=3&math=0&second=-1&value=0.0&global=-1&direct=-1&diffuse=-1&user=0&axis=1

lat <- 20.77
lon <- -156.9339
dat <- read.zoo("Data/NREL-Hawaii.csv",
                col.names = c("date", "hour", "G0", "B", "D0", "Ta"),
                index = list(1, 2),
                FUN = function(d, h) as.POSIXct(paste(d, h), format = "%m/%d/%Y %H:%M", tz = "HST"),
                FUN2 = function(x) local2Solar(x, lon),
                header=TRUE, sep=",")

dat$B0 <- dat$G0-dat$D0

NRELMeteo <- zoo2Meteo(dat, lat=lat, source="NREL-La Ola-Lanai")

## Figure 9
xyplot(NRELMeteo)

g0NREL <- calcG0(lat=lat, modeRad="bdI", dataRad=NRELMeteo, corr="none")

g0BRL <- calcG0(lat=lat, modeRad="bdI", dataRad=NRELMeteo, corr="BRL")



### Subsection "Irradiation and irradiance on the generator plane"

## Figure 10
gef<-calcGef(lat=37.2, modeRad="prev", dataRad=g0, beta=30)

xyplot(Gef/G~cosTheta|month, data=gef, type=c("p", "smooth"), cex=0.4, alpha=0.5)


### Example with backtracking
lat <- 37.2
G0dm <- c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814,
  2179)
Ta <- c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom <- list(G0dm=G0dm, Ta=Ta)

structHoriz <- list(L=4.83)
distHoriz <- data.frame(Lew=structHoriz$L*4,H=0)

gefBT <- calcGef(lat=lat, dataRad=prom, sample="10 min",
  modeTrk="horiz",
  modeShd="bt", betaLim=60,
  distances=distHoriz,
  struct=structHoriz)

## Figure 11
xyplot(r2d(Beta)~r2d(w),
     data=gefBT,
     type="l",
     xlab=expression(omega),
     ylab=expression(beta))


###################################################
### Section "Productivity of a Grid Connected PV System
###################################################
inclin <- data.frame(Gef=c(200,400,600,800,1000),Ta=25)
fProd(inclin)

inclin <- data.frame(Gef=800,Ta=30)
gen1 <- list(Nms = 10, Nmp = 11)
inv1 <- list(Ki = c(0.01,0.025,0.05), Pinv=25000, Vmin=420, Vmax=750, Gumb=20)
prod <- fProd(inclin, generator=gen1, inverter=inv1)
print(prod)


1 - with(prod,Vdc*Idc/(Vmpp*Impp))

ProdFixed<-prodGCPV(lat=lat, dataRad=prom, keep.night=FALSE)
Prod2x<-prodGCPV(lat=lat, dataRad=prom, modeTrk="two", keep.night=FALSE)
ProdHoriz<-prodGCPV(lat=lat, dataRad=prom, modeTrk="horiz", keep.night=FALSE)

### Subsection "Using mergesolaR"

EstMadrid <- subset(SIAR, Provincia=="Madrid")
nEstMadrid <- nrow(EstMadrid)
namesMadrid <- EstMadrid$Estacion

## Next code downloads data from internet.
## The result is the "prodMadrid.RData" file (see below)
prodMadrid <- lapply(1:nEstMadrid,
                     function(x){try(prodGCPV(lat=41, modeRad="siar",
                                                  dataRad=list(prov=28, est=x,
                                                    start="01/01/2009", end="31/12/2010"))
                                     )})
names(prodMadrid) <- namesMadrid
okMadrid <- lapply(prodMadrid, class)!="try-error"
prodMadrid <- prodMadrid[okMadrid]

##save(prodMadrid, file="Data/prodMadrid.RData")

## If needed, load the local copy of previous code
load("Data/prodMadrid.RData")

YfMadrid <- do.call(mergesolaR, prodMadrid)

## Figure 12
horizonplot(YfMadrid-rowMeans(YfMadrid),
            origin=0,
            scales=list(y=list(relation="same")),
            colorkey=TRUE)

### Subsection "Shadows"

struct2x <- list(W=23.11, L=9.8, Nrow=2, Ncol=8)
dist2x <- data.frame(Lew=40, Lns=30, H=0)

prod2xShd<-prodGCPV(lat=lat, dataRad=prom, modeTrk="two", 
    modeShd="area", struct=struct2x, distances=dist2x)


#Horizontal N-S tracker
structHoriz <- list(L=4.83)
distHoriz <- data.frame(Lew=structHoriz$L*4,H=0)

#Without Backtracking
prodHorizShd<-prodGCPV(lat=lat, dataRad=prom, sample="10 min", 
    modeTrk="horiz",
    modeShd="area", betaLim=60,
    distances=distHoriz,
    struct=structHoriz)

#With Backtracking
prodHorizBT<-prodGCPV(lat=lat, dataRad=prom, sample="10 min", 
    modeTrk="horiz",
    modeShd="bt", betaLim=60,
    distances=distHoriz,
    struct=structHoriz)

## Figure 13
comp <- compare(ProdFixed, Prod2x, ProdHoriz, prod2xShd, prodHorizShd, prodHorizBT)
head(comp)
## Figure 14
compL <- compareLosses(ProdFixed, Prod2x, ProdHoriz, prod2xShd, prodHorizShd, prodHorizBT)
head(compL)



### Subsection "Position of trackers in a PV plant"
###################################################

struct2x <- list(W=23.11, L=9.8, Nrow=2, Ncol=8)

dist2x <- list(Lew=c(30,50),Lns=c(20,50))


ShdM2x<-optimShd(lat=lat, dataRad=prom, modeTrk="two", 
                 modeShd=c("area","prom"), distances=dist2x, struct=struct2x, 
                 res=5, prog=FALSE)

## Figure 15
shadeplot(ShdM2x)


###################################################
### Section "Simulation of centrifugal pumps"
###################################################
data("pumpCoef") 
CoefSP8A44<-subset(pumpCoef, Qn==8&stages==44)

fSP8A44<-fPump(pump=CoefSP8A44,H=40)

SP8A44 <- with(fSP8A44,{
		Pac=seq(lim[1],lim[2],by=100)
		Pb=fPb(Pac)
		etam=Pb/Pac
		Ph=fPh(Pac)
		etab=Ph/Pb
		f=fFreq(Pac)
		Q=fQ(Pac)
		result=data.frame(Q,Pac,Pb,Ph,etam,etab,f)})


SP8A44$etamb <- with(SP8A44,etab*etam)

## Figure 16
lab <- c(expression(eta[motor]), expression(eta[pump]), expression(eta[mp]))
p<-xyplot(etam+etab+etamb~Pac,data=SP8A44,type="l", ylab="Eficiencia")
print(p+glayer(panel.text(x[1], y[1], lab[group.number], pos=3)))



### Subsection "Nomograms of PVPS"
###################################################
Pg <- seq(3000,5500,by=500)
H <- seq(50,80,by=5)
## Figure 17
NmgSP8A44<-NmgPVPS(pump=CoefSP8A44, Pg=Pg, H=H, Gd=6000,
     title="Selection of Pumps", theme=custom.theme())



###################################################
### Spatial calculations
###################################################


### Subsection "sp"
hh <- as.POSIXct("2011-05-01 11:00:00", tz="CET")
latitude <- seq(70, -70, -1)
longitude <- seq(-179.5, 179.5, 1)
horaLong <- local2Solar(hh, longitude)

solList <- lapply(latitude, calcSol, BTi=horaLong)
Bo0List <- lapply(solList, function(x)as.data.frameI(x)$Bo0) 
Bo0 <- do.call("c", Bo0List)
Bo0[is.na(Bo0)] <- 0

Bo0DF <- expand.grid(lon=longitude, lat=latitude)
Bo0DF$Bo0 <- c(Bo0)
Bo0SP <- SpatialPixelsDataFrame(points=Bo0DF[,1:2],
                                data=Bo0DF["Bo0"], proj4string=proj)

## Figure 18
paleta <- colorRampPalette(rev(brewer.pal("Greys", n=9)))
p <- spplot(Bo0SP, scales=list(draw=TRUE), col.regions=paleta, cuts=50)

library("maps")
library("maptools")
world <- map("world", plot=FALSE)
world_sp <- map2SpatialLines(world, proj4string=proj)

p+layer(sp.lines(world_sp, lwd=0.5))

### Subsection "raster"
library("raster")
unzip("Data/SISmm2008_CMSAF.zip", exdir="Data/CMSAF")
old <- setwd("Data/CMSAF")
listFich <- dir(pattern="2008")
stackSIS <- stack(listFich)
stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2
setwd(old)

idx <- seq(as.Date("2008-01-15"), as.Date("2008-12-15"), "month")

SISmm <- setZ(stackSIS, idx)
projection(SISmm) <- proj
layerNames(SISmm) <- month.abb

## latitude values as a new rasterx
latLayer <- init(SISmm, v="y")

gefFoo <- function(g0){
               gef <- calcGef(lat=g0[1], dataRad=list(G0dm=g0[2:13]))
               result <- as.data.frameY(gef)[c("Gefd", "Befd", "Defd")]
               as.numeric(result)
}


gefCMSAF <- calc(stack(latLayer, SISmm), gefFoo,
             filename="gefCMSAF",
             overwrite=TRUE)

layerNames(gefCMSAF) <- c("Gefd", "Befd", "Defd")
gefCMSAF

### Same calculations with parallel
library("parallel")

cores <- detectCores()
bs <- blockSize(SISmm, minblocks=8*cores)

resCl <- mclapply(seq_len(bs$n),
                  function(i){
                    vals <- getValues(SISmm, bs$row[i], bs$nrows[i])
                    lat <- getValues(latLayer, bs$row[i], bs$nrows[i])
                    vals <- cbind(lat, vals)
                    res0 <- apply(vals, MARGIN=1L, FUN=gefFoo)
                    res0
                  }, mc.cores=cores)
resCl <- t(do.call(cbind, resCl))

gefCMSAF <- brick(SISmm, nl=3) ##empty brick with 3 layers
layerNames(gefCMSAF)=c("Gefd", "Befd", "Defd")

gefCMSAF <- setValues(gefCMSAF, resCl)
gefCMSAF <- writeRaster(gefCMSAF, filename="gefCMSAF")
gefCMSAF


## Figure 19

## Download a shapefile with the administrative borders of Spain
## If you have run the code of the figure 5 you don"t need to
## download this file again
library("maptools")

## Shapefile with the administrative borders of Spain
## http://www.gadm.org/data/shp/ESP_adm.zip
unzip("Data/ESP_adm.zip", exdir="Data/ESP_adm")
mapaSHP <- readShapeLines("Data/ESP_adm/ESP_adm2.shp", proj4string=proj)

library("rasterVis")
levelplot(gefCMSAF, layers="Gefd") + layer(sp.lines(mapaSHP, lwd=0.7))


###################################################
### Section "Statistical analysis of PV plants"
###################################################
data("prodEx")

##Figure 20
day <- as.Date("2008-8-29")
ndays <- c(5, 10, 15, 20)
palette <- brewer.pal(n=length(ndays), name="Set1")
TDColor<-TargetDiagram(prodEx, end=day, ndays=ndays,
                       color=palette)

## Figure 21
TDMadrid <- TargetDiagram(YfMadrid, 
                          end=as.POSIXct("2010-12-31"), 
                          ndays=c(10, 20, 30, 40, 50, 60), cex=0.7)
