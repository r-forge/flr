library(FLCore)
library(FLash)
library(numDeriv)
data(ple4)
load("C:\\Stuff\\backUp\\Publications\\InPrep\\3stocks\\data\\cod4.RData")
#source("C:/Stuff/FLR/pkg/FLash/R/srrAD.R")

##### Tests for AD FLSR add-ins ################################################

##### Likelihoods ##############################################################
#### Check C++ & R code with dummy data
xRho      <-0.0
xResiduals<-FLQuant(rnorm(1000))

system.time(print(.Call("loglAR1", xResiduals,   rho=FLPar(xRho))))
system.time(print(       loglAR1(  xResiduals,   rho=xRho)))          # calls c++
system.time(print(       loglAR1(  xResiduals,   rho=xRho,old=TRUE))) # old R
################################################################################

##### Fitting ##################################################################
obj<-as.FLSR(cod4[["ices"]],model=bevholt)

#### First principles using optim ##############################################
ll  <-function(par,rec,ssb) -logl(obj)(par[1],par[2],rec=rec,ssb=ssb)
llGr<-function(par,rec,ssb) {params(obj)["a"]<-par[1];params(obj)["b"]<-par[2];computeD(obj)[,1:2]}

# with only LL
system.time(print(optim(c(a=700000,b=70000),fn=ll,        rec=rec(obj),ssb=ssb(obj),method="BFGS")))

# with LL & gradient
system.time(print(optim(c(a=700000,b=70000),fn=ll,gr=llGr,rec=rec(obj),ssb=ssb(obj),method="BFGS")))

#### FLSR ######################################################################
obj<-as.FLSR(ple4,model=bevholt)
system.time(obj<-fmle(obj))

# add gradient using computeD & check
gr(obj)<-function(a,b,rec,ssb) {params(obj)["a"]<-a;params(obj)["b"]<-b;ssb(obj)<-ssb;rec(obj)<-rec;FLPar(computeD(obj)[,1:2])}

gr(obj)(1171537,181093,rec(obj),ssb(obj))
gr(obj)(initial(obj)(rec(obj),ssb(obj))["a"],initial(obj)(rec(obj),ssb(obj))["b"],rec(obj),ssb(obj))
system.time(obj<-fmle(obj))

# check LL
logl(obj)(initial(obj)(rec(obj),ssb(obj))["a"],initial(obj)(rec(obj),ssb(obj))["b"],rec(obj),ssb(obj))

# check both
ll  <-function(par,rec,ssb) -logl(obj)(par[1],par[2],rec=rec,ssb=ssb)
llGr<-function(par,rec,ssb) -gr(  obj)(par[1],par[2],rec=rec,ssb=ssb)[1:2]
system.time(optim(c(a=700000,b=70000),fn=ll,        rec=rec(obj),ssb=ssb(obj),method="BFGS"))
system.time(optim(c(a=700000,b=70000),fn=ll,gr=llGr,rec=rec(obj),ssb=ssb(obj),method="BFGS"))

model(obj) <-srModel("bevholt")
params(obj)<-initial(obj)(rec(obj),ssb(obj))
system.time(obj<-fmle(obj))
logl(obj)(params(obj)["a"],params(obj)["b"],rec(obj),ssb(obj))
  gr(obj)(params(obj)["a"],params(obj)["b"],rec(obj),ssb(obj))


library(FLCore)
library(FLash)
library(numDeriv)
data(ple4)
load("C:\\Stuff\\backUp\\Publications\\InPrep\\3stocks\\data\\cod4.RData")
#source("C:/Stuff/FLR/pkg/FLash/R/srrAD.R")

##### Tests for AD FLSR add-ins ################################################

##### Likelihoods ##############################################################
#### Check C++ & R code with dummy data
xRho      <-0.0
xResiduals<-FLQuant(rnorm(1000))

system.time(print(.Call("loglAR1", xResiduals,   rho=FLPar(xRho))))
system.time(print(       loglAR1(  xResiduals,   rho=xRho)))          # calls c++
system.time(print(       loglAR1(  xResiduals,   rho=xRho,old=TRUE))) # old R
################################################################################

##### Fitting ##################################################################
obj<-as.FLSR(cod4[["ices"]],model=bevholt)

#### First principles using optim ##############################################
ll  <-function(par,rec,ssb) -logl(obj)(par[1],par[2],rec=rec,ssb=ssb)
llGr<-function(par,rec,ssb) {params(obj)["a"]<-par[1];params(obj)["b"]<-par[2];computeD(obj)[,1:2]}

# with only LL
system.time(optim(c(a=700000,b=70000),fn=ll,        rec=rec(obj),ssb=ssb(obj),method="BFGS"))

# with LL & gradient
system.time(optim(c(a=700000,b=70000),fn=ll,gr=llGr,rec=rec(obj),ssb=ssb(obj),method="BFGS"))

#### FLSR ######################################################################
obj<-as.FLSR(ple4,model=bevholt)
system.time(obj<-fmle(obj))

# add gradient using computeD & check
gr(obj)<-function(a,b,rec,ssb) {params(obj)["a"]<-a;params(obj)["b"]<-b;ssb(obj)<-ssb;rec(obj)<-rec;FLPar(computeD(obj)[,1:2])}

gr(obj)(1171537,181093,rec(obj),ssb(obj))
gr(obj)(initial(obj)(rec(obj),ssb(obj))["a"],initial(obj)(rec(obj),ssb(obj))["b"],rec(obj),ssb(obj))
system.time(obj<-fmle(obj))

# check LL
logl(obj)(initial(obj)(rec(obj),ssb(obj))["a"],initial(obj)(rec(obj),ssb(obj))["b"],rec(obj),ssb(obj))

# check both
ll  <-function(par,rec,ssb) -logl(obj)(par[1],par[2],rec=rec,ssb=ssb)
llGr<-function(par,rec,ssb) -gr(  obj)(par[1],par[2],rec=rec,ssb=ssb)[1:2]
system.time(optim(c(a=700000,b=70000),fn=ll,        rec=rec(obj),ssb=ssb(obj),method="BFGS"))
system.time(optim(c(a=700000,b=70000),fn=ll,gr=llGr,rec=rec(obj),ssb=ssb(obj),method="BFGS"))

model(obj) <-srModel("bevholt")
params(obj)<-initial(obj)(rec(obj),ssb(obj))
system.time(obj<-fmle(obj))
logl(obj)(params(obj)["a"],params(obj)["b"],rec(obj),ssb(obj))
  gr(obj)(params(obj)["a"],params(obj)["b"],rec(obj),ssb(obj))

