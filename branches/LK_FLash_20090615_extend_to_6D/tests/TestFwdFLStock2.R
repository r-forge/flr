#=====================================================================
## Date: 19/01/2009
# Version: 0.1-0
# Authors: Laurence Kell
#
# Short description: tests for fwd(FLStock)
#
# ToDo:
#
# References (bibtex):
#
#!Notes:
#=====================================================================

library(FLash)
source("C:/stuff/FLR/Branch/R/fwdControl.R")
source("C:/stuff/FLR/Branch/R/fwd.R")
source("C:/stuff/FLR/Branch/R/setSRs.R")
source("C:/stuff/FLR/Branch/R/FLCoreVarCon.R")
source("C:/Stuff/FLR/Branch/R/validityFLSR.r")

data(ple4)

#### fit FLSR
ple4SR       <-as.FLSR(ple4)
model(ple4SR)<-ricker()
ple4SR       <-transform(ple4SR,ssb=ssb/1000,rec=rec/1000)
ple4SR       <-fmle(ple4SR)
params(ple4SR)["b",1]<-params(ple4SR)["b",1]/1000

ple4<-ple4[,ac(1996:2001)]
yrs <-1998:2001

#### add discards
pDiscard<-FLQuant(c(0.5,0.45,0.30,0.15,0.05,0.02,0.01,0.01,0.0,0.0,0.0,0.0,0.0,0.0,0.0),dimnames=list(age=1:15))
discards.n( ple4)<-sweep(catch.n(ple4),1,pDiscard,"*")
landings.n( ple4)<-catch.n(ple4)-discards.n( ple4)
discards.wt(ple4)<-landings.wt(ple4)
landings         <-computeLandings(ple4)
discards         <-computeDiscards(ple4)
catch(ple4)      <-computeCatch(   ple4,"all")

#### Test the different ways of passing SRRs ###################################
  chkSR<-function(stk,srr,yrs,rsd=NULL,mult=TRUE){
     recYrs<-(1+dims(stk)$min):dims(stk)$year
     ssbYrs<-1:(dims(stk)$year-dims(stk)$min)

     plot(  ssb(srr)*1000,     rec(srr)*1000,      xlab="SSB", ylab="Recruits")
     points(ssb(srr)*1000,     fitted(srr)*1000,   pch=19)
     points(ssb(stk)[,ac(yrs-dims(stk)$min)],rec(stk)[,ac(yrs)], pch=19,col="red")

     if (is.null(rsd)) recs<-rec(stk)[,ac(yrs)]
     else
        if (mult) recs<-sweep(rec(stk)[,ac(yrs)],1:2,rsd[,ac(yrs)],"/")
        else      recs<-sweep(rec(stk)[,ac(yrs)],1:2,rsd[,ac(yrs)],"-")

     return(sweep(recs,1:2,predict(srr,ssb=ssb(stk)[,ac(yrs-dims(stk)$min)]),"-"))
     }

ctrl <-fwdControl(data.frame(year=yrs,val=0.45,quantity="f")) #,season=1))

##### FLSR object
res  <-fwd(ple4,ctrl=ctrl,sr=ple4SR)
chkSR(res,ple4SR,yrs)

##### FLSR object
stock.n(ple4)[1,ac(yrs)]<-NA
res  <-fwd(ple4,ctrl=ctrl,sr=ple4SR)
chkSR(res,ple4SR,yrs)

#### list
##Character
res<-fwd(ple4, ctrl=ctrl, sr=list(model="ricker", params=params(ple4SR)[c("a","b"),1]))
chkSR(res,ple4SR,yrs)

res<-fwd(ple4, ctrl=ctrl, sr=list(model="ricker", params=FLPar(params(ple4SR)[c("a","b"),1,drop=T])))
chkSR(res,ple4SR,yrs)

#### Formula
res<-fwd(ple4, ctrl=ctrl, sr=list(model=formula(rec ~ a * ssb * exp(-b * ssb)), params=FLPar(params(ple4SR)[c("a","b"),1,drop=T])))
chkSR(res,ple4SR,yrs)

#### FLPar by year
srPar<-FLPar(array(params(ple4SR)[c("a","b"),1,drop=T],dim=c(2,4,1),dimnames=list(params=c("a","b"),year=yrs,iter=1)))
res <-fwd(ple4, ctrl=ctrl, sr=list(model="ricker", params=srPar))
chkSR(res,ple4SR,yrs)

#### Residuals
srRes<-FLQuant(array(c(1,1.25,1.5),dim=c(1,4,1),dimnames=list(age=1,year=yrs,iter=1)))

res<-fwd(ple4, ctrl=ctrl, sr=ple4SR, sr.residuals=srRes)
chkSR(res,ple4SR,yrs,rsd=srRes)

res<-fwd(ple4, ctrl=ctrl, sr=ple4SR, sr.residuals=srRes*1000, sr.residuals.mult=FALSE)
chkSR(res,ple4SR,yrs,rsd=srRes*1000,mult=FALSE)



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

#### biomass
ctrl<-fwdControl(data.frame(year=yrs,val=300000,quantity="biomass"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
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
ctrl<-fwdControl(data.frame(year=yrs,val=0.4,quantity="f.discards"))
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
ctrl<-fwdControl(data.frame(year=yrs,val=1.1,quantity="ssb",rel.year=1997:2000))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
ssb(res)[,ac(yrs)]/c(ssb(res)[,ac(1997)])

#### biomass
ctrl<-fwdControl(data.frame(year=yrs,val=1.1,quantity="biomass",rel.year=1997:2000))
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
ple4<-propagate(ple4,iter=10)
ctrl<-fwdControl(data.frame(year=yrs,val=c(ssb(ple4)[,ac(yrs+1),,,,1]),quantity="ssb"))
srRes<-FLQuant(seq(1,2,length.out=10),dimnames=list(age=1,year=1997:1999,iter=1:10))
res<-fwd(ple4, ctrl=ctrl, sr=ple4SR, sr.residuals=srRes)
chkSR(res,ple4SR,yrs,rsd=srRes)

##Seasons
dmns<-dimnames(m(res))
dmns$unit<-c("spawn","feed")

#### f

ctrl<-fwdControl(data.frame(year=yrs,val=.45,quantity="f"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
fbar(res)[,ac(yrs)]


##Units
dmns<-dimnames(m(res))
dmns$unit<-c("north","south")

ctrl<-fwdControl(data.frame(year=yrs,val=.45,quantity="f"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
fbar(res)[,ac(yrs)]


##Areas
dmns<-dimnames(m(res))
dmns$unit<-c("90mm","120mm")

ctrl<-fwdControl(data.frame(year=yrs,val=.45,quantity="f"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
fbar(res)[,ac(yrs)]


