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

#### Test all targets, absolute ################################################
#### landings
ctrl<-fwdControl(data.frame(year=yrs,val=25000,quantity="landings",season=1))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
computeLandings(res)[,ac(yrs)]

#### catch
ctrl<-fwdControl(data.frame(year=yrs,val=25000,quantity="catch"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
computeCatch(res)[,   ac(yrs)]

#### discards
ctrl<-fwdControl(data.frame(year=yrs,val=25000,quantity="discards"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
computeDiscards(res)[,   ac(yrs)]

#### ssb
ctrl<-fwdControl(data.frame(year=yrs,val=200000,quantity="ssb"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
ssb(res)[,ac(yrs)]

#### SOMETHING WRONG WITH RECRUITS
res<-fwd(ple4,ctrl=ctrl,sr=list(model="mean",params=FLPar(100000)))
ssb(res)[,ac(yrs)]

#### SOMETHING WRONG WITH RECRUITS
#### biomass
ctrl<-fwdControl(data.frame(year=yrs,val=300000,quantity="biomass"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
computeStock(res)[,ac(yrs)]

stock.wt(res)[1,]<-0
res<-fwd(res,ctrl=ctrl,sr=ple4SR)
computeStock(res)[,ac(yrs)]

#### f
ctrl<-fwdControl(data.frame(year=yrs,val=.45,quantity="f"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
fbar(res)[,ac(yrs)]

#### z
fbarRng<-ac(range(res,"minfbar"):range(res,"maxfbar"))
ctrl<-fwdControl(data.frame(year=yrs,val=.5,quantity="z"))
res <-fwd(ple4,ctrl=ctrl,sr=ple4SR)
fbar(res)[,ac(yrs)]+apply(m(res)[fbarRng,ac(yrs)],2,"mean")

#### f.landings
ctrl<-fwdControl(data.frame(year=yrs,val=0.4,quantity="f.landings"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
apply((harvest(res)*landings.n(ple4)/catch.n(ple4))[fbarRng,ac(yrs)],2,"mean")

#### f.discards
ctrl<-fwdControl(data.frame(year=yrs,val=0.2,quantity="f.discards"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
apply((harvest(res)*discards.n(ple4)/catch.n(ple4))[fbarRng,ac(yrs)],2,"mean")

#### mnsz
ctrl<-fwdControl(data.frame(year=yrs,val=.25,quantity="mnsz"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
apply(stock.n(res)*stock.wt(res),2,sum)/apply(stock.n(res),2,sum)

#ctrl<-fwdControl(data.frame(year=yrs,val=c(ssb(ple4)[,ac(yrs+1)]),quantity="effort"))
#ctrl<-fwdControl(data.frame(year=yrs,val=c(ssb(ple4)[,ac(yrs+1)]),quantity="costs"))
#ctrl<-fwdControl(data.frame(year=yrs,val=c(ssb(ple4)[,ac(yrs+1)]),quantity="revenue"))
#ctrl<-fwdControl(data.frame(year=yrs,val=c(ssb(ple4)[,ac(yrs+1)]),quantity="profit"))

#### Test all targets, relative ################################################
#### landings
ctrl<-fwdControl(data.frame(year=yrs,val=1.1,quantity="landings",rel.year=1997:2000))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
computeLandings(res)[,ac(yrs)]
computeLandings(res)[,ac(yrs)]/c(computeLandings(res)[,ac(1997)])

#### catch
ctrl<-fwdControl(data.frame(year=yrs,val=1.1,quantity="catch",rel.year=1997:2000))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
computeCatch(res)[,   ac(yrs)]
computeCatch(res)[,ac(yrs)]/c(computeCatch(res)[,ac(1997)])

#### discards
ctrl<-fwdControl(data.frame(year=yrs,val=1.1,quantity="discards",rel.year=1997:2000))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
computeDiscards(res)[,   ac(yrs)]/c(computeDiscards(res)[,ac(1997)])

#### ssb
ctrl<-fwdControl(data.frame(year=yrs,val=1.0,quantity="ssb",rel.year=rep(1997,4)))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
ssb(res)[,ac(yrs)]/c(ssb(res)[,ac(1997)])

#### biomass
#### NEED TO CHECK
ctrl<-fwdControl(data.frame(year=yrs,val=1.0,quantity="biomass",rel.year=rep(1996,4)))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
computeStock(res)[,ac(yrs)]/c(computeStock(res)[,ac(1997)])

#### f
ctrl<-fwdControl(data.frame(year=yrs,val=1.1,quantity="f",rel.year=1997:2000))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
fbar(res)[,ac(yrs)]/c(fbar(res)[,ac(1997)])

#### z
ctrl<-fwdControl(data.frame(year=yrs,val=1.1,quantity="z",rel.year=1997:2000))
res <-fwd(ple4,ctrl=ctrl,sr=ple4SR)
(fbar(res)[,ac(yrs)]+apply(m(res)[fbarRng,ac(yrs)],2,"mean"))/c(fbar(res)[,ac(1997)]+apply(m(res)[fbarRng,ac(1997)],2,"mean"))

#### f.landings
ctrl<-fwdControl(data.frame(year=yrs,val=1.1,quantity="f.landings",rel.year=1997:2000))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
  apply((harvest(res)*landings.n(ple4)/catch.n(ple4))[fbarRng,ac( yrs)],2,"mean")/
c(apply((harvest(res)*landings.n(ple4)/catch.n(ple4))[fbarRng,ac(1997)],2,"mean"))

#### f.discards
ctrl<-fwdControl(data.frame(year=yrs,val=1.1,quantity="f.discards",rel.year=1997:2000))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
  apply((harvest(res)*discards.n(ple4)/catch.n(ple4))[fbarRng,ac( yrs)],2,"mean")/
c(apply((harvest(res)*discards.n(ple4)/catch.n(ple4))[fbarRng,ac(1997)],2,"mean"))

#### mnsz
ctrl<-fwdControl(data.frame(year=yrs,val=1,quantity="mnsz",rel.year=1997))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
  (apply(stock.n(res)*stock.wt(res),2,sum)/apply(stock.n(res),2,sum))[,ac( yrs)]/
c((apply(stock.n(res)*stock.wt(res),2,sum)/apply(stock.n(res),2,sum))[,ac(1997)])

#ctrl<-fwdControl(data.frame(year=yrs,val=1,rel.year=1996,quantity="effort"))
#ctrl<-fwdControl(data.frame(year=yrs,val=1,rel.year=1996,quantity="costs"))
#ctrl<-fwdControl(data.frame(year=yrs,val=1,rel.year=1996,quantity="revenue"))
#ctrl<-fwdControl(data.frame(year=yrs,val=1,rel.year=1996,quantity="profit"))

##Iters
res  <-propagate(res,iter=10)
ctrl <-fwdControl(data.frame(year=yrs,val=c(200000,220000,240000,260000),quantity="ssb"))
srRes<-FLQuant(rep(seq(1,2,length.out=10),each=4),dimnames=list(age=1,year=yrs,iter=1:10))
res  <-fwd(ple4, ctrl=ctrl, sr=ple4SR, sr.residuals=srRes)
chkSR(res,ple4SR,yrs,rsd=srRes)
