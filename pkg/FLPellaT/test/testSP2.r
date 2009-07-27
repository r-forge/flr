library(minpack.lm)
library(FLCore)

################################################################################
## Biomass Dynamic Model in R                                                 ##
################################################################################

source("C:/Stuff/FLR/WorkInProgress/FLPellaT/R/class.R")
source("C:/Stuff/FLR/WorkInProgress/FLPellaT/R/createAccessors.R")
source("C:/Stuff/FLR/WorkInProgress/FLPellaT/R/constructors.R")
source("C:/Stuff/FLR/WorkInProgress/FLPellaT/R/methods.R")

#### Get data, and make catch & cpue globally availably in session #############
#alb <-read.table("C:\\Stuff\\FLR\\WorkInProgress\\FLSP\\alb.dat",header=T)
#attach(alb)
myDir  <-"C:\\Stuff\\FLR\\WorkInProgress\\FLSP\\"
test   <-read.csv(paste(myDir,"test.csv",sep=""))
#catch  <-FLQuant(test[,"catch"],  dimnames=list(age="all",year=test[,"year"]))@.Data[,,1,1,1,1,drop=T]
#stock  <-FLQuant(test[,"biomass"],dimnames=list(age="all",year=test[,"year"]))@.Data[,,1,1,1,1,drop=T]
#index  <-FLQuant(test[,"cpue"],   dimnames=list(age="all",year=test[,"year"]))@.Data[,-length(catch),1,1,1,1,drop=T]
catch  <-FLQuant(test[,"catch"],  dimnames=list(age="all",year=test[,"year"]))
stock  <-FLQuant(test[,"biomass"],dimnames=list(age="all",year=test[,"year"]))
index  <-FLQuant(test[,"cpue"],   dimnames=list(age="all",year=test[,"year"]))
rm(test)

################################################################################
flpt<-FLPellaT(catch=catch,index=index)

flpt<-fit(flpt)

#### Fit

#### plot
par(mfrow=c(2,1))
catchability<-calcQ((stock[-1]+stock[-length(stock)])/2,index[-length(stock)])
stockHat    <-proj(catch,res$par[1],res$par[2],b0=1,mpar=2)
index_Hat   <-catchability*(stock[-1]+stock[-length(stock)])/2

plot( index)
lines(index_Hat)

plot(catchHat(seq(0,res$par[2],length.out=100),res$par[1],res$par[2])~seq(0,res$par[2],length.out=100),type="l",xlab="biomass",ylab="Yield",ylim=c(0,200))
points(catch~stock,type="b")
points(msy(res$par[1],res$par[2])~bmsy(res$par[2]),type="p",cex=3,col="red",pch=16)

