library(FLPellaT)
#source("C:/Stuff/FLR/pkg/FLPellaT/R/class.R")
#source("C:/Stuff/FLR/pkg/FLPellaT/R/constructors.R")
#source("C:/Stuff/FLR/pkg/FLPellaT/R/coerce.R")
#source("C:/Stuff/FLR/pkg/FLPellaT/R/createAccessors.R")
#source("C:/Stuff/FLR/pkg/FLPellaT/R/methods.R")
#source("C:/Stuff/FLR/pkg/FLPellaT/R/plot.R")

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

pt<-FLPellaT(catch=catch,index=idx)

save(pt,file="C:/Stuff/FLR/pkg/FLPellaT/data/pt.RData")

save(stock,file="C:/Stuff/FLR/pkg/FLPellaT/data/stock.RData")

pt<-fit(pt)
plot(pt)
plot(pt,type="diag")
