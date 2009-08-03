################################################################################
## Biomass Dynamic Model in R                                                 ##
################################################################################

library(minpack.lm)
library(FLCore)
library(FLAssess)
library(FLPellaT)

#source("C:/Stuff/FLR/pkg/FLPellaT/R/class.R")
#source("C:/Stuff/FLR/pkg/FLPellaT/R/constructors.R")
#source("C:/Stuff/FLR/pkg/FLPellaT/R/coerce.R")
#source("C:/Stuff/FLR/pkg/FLPellaT/R/methods.R")
#source("C:/Stuff/FLR/pkg/FLPellaT/R/plot.R")
#source("C:/Stuff/FLR/pkg/FLPellaT/R/plotDiagnostics.R")
#source("C:/Stuff/FLR/pkg/FLPellaT/R/createAccessors.R")

#### Get data, and make catch & index globally availably in session #############
test   <-read.csv("C:\\Stuff\\FLR\\WorkInProgress\\FLSP\\test.csv")
catch  <-FLQuant(test[,"catch"],  dimnames=list(age="all",year=2001:2035))
stock  <-FLQuant(test[,"biomass"],dimnames=list(age="all",year=2001:2035))
idx    <-FLQuant(test[,"cpue"],   dimnames=list(age="all",year=2001:2035))
rm(test)

flpt<-FLPellaT(catch=catch,index=idx)

save(flpt,   file="C:/Stuff/FLR/pkg/FLPellaT/data/flpt.RData")

source("C:\\Stuff\\FLR\\WorkInProgress\\FLSP\\fmle_sc-1.R")
source("C:/Stuff/flr/pkg/FLCore/R/FLAccesors.R")
#source("m:/Projects/FS_SP/fmle_sc.R")
#source("c:/Sandbox/R281built/flr/pkg/FLCore/R/FLAccesors.R")

##### Fit Pella Tomlinson on test data
flpt<-fit(flpt)
plot(flpt)
plot(flpt,type="diag")
plot(flpt,type="equil")

##### Double check by profiling liklihood
r.  <-seq(0.01, .6, length.out=100)
logl<-tapply(r.,1:100,function(x) {fit(flpt,fix=c(r=x),start=c(K=1e8))@resDev})
plot(logl~r.,type="l")

points(c(flpt@params["r",]),min(logl),pch=16,col="red",cex=2)

##### Check with missing index years
flpt.<-flpt
index(flpt.)<-index(flpt.)[,3:30]
flpt.<-fit(flpt.)


##### Fit FLSP on test data
sp       <-FLSP(catch=catch(flpt),index=index(flpt))
model(sp)<-pellatom()
sp@mpar  <- 2 # Schaeffer?

# try with scaled fmle
sp <- fmle_sc(sp,start=list(r=0.3,K=1000,Q=.2,sigma2=0.3),lower=c(r=.1,K=10,Q=0.01,sigma2=0.01),upper=c(r=1,K=1e6,Q=10,sigma2=10))
params(sp)
plot(sp)
