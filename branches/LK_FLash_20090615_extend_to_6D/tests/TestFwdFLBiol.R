library(FLash)
library(FLAssess)
#library(debug)

#source("C:/stuff/FLR/pkgs/FLash/R/fwdControl.R")
#source("C:/stuff/FLR/pkgs/FLash/R/fwd.R")
#source("C:/stuff/FLR/pkgs/FLash/R/fwdSetSRs.R")
#source("C:/stuff/FLR/pkgs/FLash/R/FLCoreVarCon.R")

data(ple4)
landings.n( ple4)      <-0.75*catch.n(ple4)
discards.n( ple4)      <-0.25*catch.n(ple4)
discards.wt(ple4)      <-landings.wt(ple4)

ple4.stf<-stf(ple4,2)
biol <-as(ple4.stf,"FLBiol")
fleet<-as(ple4.stf,"FLFleet")

data(ple4)
ple4SR       <-as.FLSR(ple4)
model(ple4SR)<-ricker()
ple4SR       <-transform(ple4SR,ssb=ssb/1000,rec=rec/1000)
ple4SR       <-fmle(ple4SR)
params(ple4SR)["b",1]<-params(ple4SR)["b",1]/1000
### SRR
#srModel   <-"mean"
#srParams  <-FLPar(400000)
#srDeviates<-FLQuant(1,dimnames=list(age=1,year=2002,iter=1))

## Run
effort<-data.frame(year=2002, fleet=1, metier=1, val=NA, min=.01, max=.75)
target<-data.frame(year=2002, val=15000, quantity="catch", fleet=1, metier=1, spp=1)
ctrl  <-fwdControl(target,effort)

res   <-fwd(sr=ple4SR,object=biol, fleets=fleet, ctrl=ctrl)

effort<-data.frame(year=c(2002,2003), fleet=1, metier=1, val=NA, min=.01, max=.75)
target<-data.frame(year=c(2002,2003), val=15000, quantity="catch", fleet=1, metier=1, spp=1)
ctrl  <-fwdControl(target,effort)

sum((res$catch.n*catch.wt(ple4))[,"2002"],na.rm=T)

effort<-data.frame(year=2002, fleet=1, metier=1, val=NA, min=.001, max=5.)
target<-data.frame( year=2002, val=15000, quantity="landings", fleet=1, metier=1, spp=1)
ctrl  <-fwdControl(target,effort)
res   <-fwdb.(biol, fleet, ctrl=ctrl,sr=list(model=srModel,params=srParams),sr.residuals=srDeviates)
sum((res$landings.n*landings.wt(ple4))[,"2002"],na.rm=T)

effort<-data.frame(year=2002, fleet=1, metier=1, val=NA, min=.01, max=.75)
target<-data.frame( year=2002, val=15000, quantity="discards", fleet=1, metier=1, spp=1)
ctrl  <-fwdControl(target,effort)
res   <-fwd(biol, fleet, ctrl=ctrl,sr=list(model=srModel,params=srParams),sr.residuals=srDeviates)
sum((res$discards.n*discards.wt(ple4))[,"2002"],na.rm=T)

