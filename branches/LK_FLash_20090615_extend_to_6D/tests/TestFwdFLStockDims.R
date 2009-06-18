#===============================================================================
## Date: 19/01/2009
# Version: 0.1-0
# Authors: Laurence Kell
#
# Short description: tests of targets for fwd(FLStock)
#
# ToDo:
#
# References (bibtex):
#
#!Notes:
#===============================================================================

library(FLash)
#source("C:/stuff/FLR/Branch/R/fwdControl.R")
#source("C:/stuff/FLR/Branch/R/fwd.R")
#source("C:/stuff/FLR/Branch/R/setSRs.R")
#source("C:/stuff/FLR/Branch/R/FLCoreVarCon.R")
#source("C:/Stuff/FLR/Branch/R/validityFLSR.r")

data(ple4)

#### add discards
pDiscard<-FLQuant(c(0.5,0.45,0.30,0.15,0.05,0.02,0.01,0.01,0.0,0.0,0.0,0.0,0.0,0.0,0.0),dimnames=list(age=1:15))
discards.n( ple4)<-sweep(catch.n(ple4),1,pDiscard,"*")
landings.n( ple4)<-catch.n(ple4)-discards.n( ple4)
discards.wt(ple4)<-landings.wt(ple4)
landings         <-computeLandings(ple4)
discards         <-computeDiscards(ple4)
catch(ple4)      <-computeCatch(   ple4,"all")

#### FLSR ######################################################################
ple4SR       <-as.FLSR(ple4)
model(ple4SR)<-ricker()
ple4SR       <-transform(ple4SR,ssb=ssb/1000,rec=rec/1000)
ple4SR       <-fmle(ple4SR)
params(ple4SR)["b",1]<-params(ple4SR)["b",1]/1000

#### Tests
ple4<-ple4[,ac(1996:2001)]
yrs <-1998:2001
ctrl<-fwdControl(data.frame(year=yrs,val=0.45,quantity="f")) #,season=1))

##Seasons
dmns       <-dimnames(m(ple4))
dmns$season<-c("spawn","feed")

ssn<-FLStock(catch.wt    =FLQuant(c(catch.wt(    ple4)),dimnames=dmns),
             catch.n     =FLQuant(c(catch.n(    ple4)),dimnames=dmns),
             discards.n  =FLQuant(c(discards.n(  ple4)),dimnames=dmns),
             discards.wt =FLQuant(c(discards.wt( ple4)),dimnames=dmns),
             landings.n  =FLQuant(c(landings.n(  ple4)),dimnames=dmns),
             landings.wt =FLQuant(c(landings.wt( ple4)),dimnames=dmns),
             stock.n     =FLQuant(c(stock.n(     ple4)),dimnames=dmns),
             stock.wt    =FLQuant(c(stock.wt(    ple4)),dimnames=dmns),
             m           =FLQuant(c(m(           ple4)),dimnames=dmns),
             mat         =FLQuant(c(mat(         ple4)),dimnames=dmns),
             harvest     =FLQuant(c(harvest(     ple4)),dimnames=dmns,units="f"),
             harvest.spwn=FLQuant(c(harvest.spwn(ple4)),dimnames=dmns),
             m.spwn      =FLQuant(c(m.spwn(      ple4)),dimnames=dmns))

dmns           <-list(params=c("a","b"),season=c("spawn","feed"),iter=1)
srPar          <-FLPar(c(params(ple4SR)[c("a","b"),]),dimnames=dmns)
srPar[,"feed",]<-NA
ctrl           <-fwdControl(data.frame(year=rep(yrs,each=2),val=.45,quantity="f",season=c("spawn","feed")))
res            <-fwd(ssn,ctrl=ctrl,sr=list(model="ricker",params=srPar))
fbar(res)[,ac(yrs)]

#### NEED TO CHECK
stock.n(res)

##Units
dmns<-dimnames(m(ple4))
dmns$unit<-c("north","south")

unt<-FLStock(catch.wt    =FLQuant(c(catch.wt(    ple4)),dimnames=dmns),
             discards.n  =FLQuant(c(discards.n(  ple4)),dimnames=dmns),
             discards.wt =FLQuant(c(discards.wt( ple4)),dimnames=dmns),
             landings.n  =FLQuant(c(landings.n(  ple4)),dimnames=dmns),
             landings.wt =FLQuant(c(landings.wt( ple4)),dimnames=dmns),
             stock.n     =FLQuant(c(stock.n(     ple4)),dimnames=dmns),
             stock.wt    =FLQuant(c(stock.wt(    ple4)),dimnames=dmns),
             m           =FLQuant(c(m(           ple4)),dimnames=dmns),
             mat         =FLQuant(c(mat(         ple4)),dimnames=dmns),
             harvest     =FLQuant(c(harvest(     ple4)),dimnames=dmns,units="f"),
             harvest.spwn=FLQuant(c(harvest.spwn(ple4)),dimnames=dmns),
             m.spwn      =FLQuant(c(m.spwn(      ple4)),dimnames=dmns))

dmns  <-list(params=c("a","b"),unit=c("north","south"),iter=1)
srPar <-FLPar(c(params(ple4SR)[c("a","b"),]),dimnames=dmns)

ctrl<-fwdControl(data.frame(year=yrs,val=.45,quantity="f",unit=c("north")))
res<-fwd(unt,ctrl=ctrl,sr=list(model="ricker",params=srPar))
fbar(res)[,ac(yrs)]

ctrl<-fwdControl(data.frame(year=yrs,val=.45,quantity="f",unit=c("south")))
res<-fwd(unt,ctrl=ctrl,sr=list(model="ricker",params=srPar))
fbar(res)[,ac(yrs)]

##Areas
dmns     <-dimnames(m(ple4))
dmns$area<-c("90mm","120mm")

ars<-FLStock(catch.wt    =FLQuant(c(catch.wt(    ple4)),dimnames=dmns),
             discards.n  =FLQuant(c(discards.n(  ple4)),dimnames=dmns),
             discards.wt =FLQuant(c(discards.wt( ple4)),dimnames=dmns),
             landings.n  =FLQuant(c(landings.n(  ple4)),dimnames=dmns),
             landings.wt =FLQuant(c(landings.wt( ple4)),dimnames=dmns),
             stock.n     =FLQuant(c(stock.n(     ple4)),dimnames=dmns),
             stock.wt    =FLQuant(c(stock.wt(    ple4)),dimnames=dmns),
             m           =FLQuant(c(m(           ple4)),dimnames=dmns),
             mat         =FLQuant(c(mat(         ple4)),dimnames=dmns),
             harvest     =FLQuant(c(harvest(     ple4)),dimnames=dmns,units="f"),
             harvest.spwn=FLQuant(c(harvest.spwn(ple4)),dimnames=dmns),
             m.spwn      =FLQuant(c(m.spwn(      ple4)),dimnames=dmns))

dmns$year<-1998:2002
avail<-FLQuant(0.25,dimnames=dmns)
avail[,,,,"90mm" ]<-.25
avail[,,,,"120mm"]<-1.0-avail[,,,,"90mm" ]

dmns <-list(params=c("a","b"),area=c("90mm","120mm"),iter=1)
srPar<-FLPar(c(params(ple4SR)[c("a","b"),]),dimnames=dmns)

srPar["a","90mm" ]<-srPar["a","90mm" ]*.25
srPar["a","120mm"]<-srPar["a","120mm"]*.75

ctrl <-fwdControl(data.frame(year=yrs,val=.45,quantity="f",area=c("90mm")))
res  <-fwd(ars,ctrl=ctrl,sr=list(model="ricker",params=srPar),availability=avail)
fbar(res)[,ac(yrs)]

ctrl <-fwdControl(data.frame(year=yrs,val=.45,quantity="f",area=c("120mm")))
res  <-fwd(ars,ctrl=ctrl,sr=list(model="ricker",params=srPar),availability=avail)
fbar(res)[,ac(yrs)]

