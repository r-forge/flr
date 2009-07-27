library(minpack.lm)
source("C:/Stuff/FLR/WorkInProgress/FLPellaT/R/class.R")
source("C:/Stuff/FLR/WorkInProgress/FLPellaT/R/constructors.R")
source("C:/Stuff/FLR/WorkInProgress/FLPellaT/R/coerce.R")
source("C:/Stuff/FLR/WorkInProgress/FLPellaT/R/createAccessors.R")
source("C:/Stuff/FLR/WorkInProgress/FLPellaT/R/methods.R")
source("C:/Stuff/FLR/WorkInProgress/FLPellaT/R/plot.R")

################################################################################
## Biomass Dynamic Model in R                                                 ##
################################################################################

#### Get data, and make catch & index globally availably in session #############
#alb <-read.table("C:\\Stuff\\FLR\\WorkInProgress\\FLSP\\alb.dat",header=T)
#attach(alb)
test   <-read.csv("C:\\Stuff\\FLR\\WorkInProgress\\FLSP\\test.csv")
catch  <-FLQuant(test[,"catch"],  dimnames=list(age="all",year=test[,"year"])) #@.Data[,,1,1,1,1,drop=T]
stock  <-FLQuant(test[,"biomass"],dimnames=list(age="all",year=test[,"year"])) #@.Data[,,1,1,1,1,drop=T]
idx    <-FLQuant(test[,"cpue"],   dimnames=list(age="all",year=test[,"year"])) #@.Data[,-length(catch),1,1,1,1,drop=T]
rm(test)

pt<-FLPellaT(catch=catch,stock=stock,index=idx)

#### Fit
KGuess<-mean(catch)*10
res<-nls.lm(c(.5,KGuess),residuals,catch=catch(pt)@.Data[,,1,1,1,1,drop=T],index=pt@index@.Data[,-length(catch),1,1,1,1,drop=T])

#### plot
par(mfrow=c(2,1))

#### plot
par(mfrow=c(2,1))
catchability<-calcQ((stock[-1]+stock[-length(stock)])/2,index[-length(stock)])
index_Hat    <-catchability*(stock[-1]+stock[-length(stock)])/2

plot( index)
lines(index_Hat)

plot(catchHat(seq(0,res$par[2],length.out=100),res$par[1],res$par[2])~seq(0,res$par[2],length.out=100),type="l",xlab="biomass",ylab="Yield",ylim=c(0,200))
points(catch~stock,type="b")
points(msy(res$par[1],res$par[2])~bmsy(res$par[2]),type="p",cex=3,col="red",pch=16)

###
catchability<-calcQ((stock[-1]+stock[-length(stock)])/2,index[-length(stock)])
index_Hat    <-catchability*(stock[-1]+stock[-length(stock)])/2

plot( index)
lines(index_Hat)

plot(catchHat(seq(0,res$par[2],length.out=100),res$par[1],res$par[2])~seq(0,res$par[2],length.out=100),type="l",xlab="biomass",ylab="Yield",ylim=c(0,200))
points(catch~stock,type="b")
points(msy(res$par[1],res$par[2])~bmsy(res$par[2]),type="p",cex=3,col="red",pch=16)
