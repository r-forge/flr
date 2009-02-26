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
#library(FLCore)
data(ple4)

source("C:/stuff/FLR/pkgs/FLash/R/fwdControl.R")
source("C:/stuff/FLR/pkgs/FLash/R/fwd.R")
source("C:/stuff/FLR/pkgs/FLash/R/fwdSetSRs.R")
source("C:/stuff/FLR/pkgs/FLash/R/FLCoreVarCon.R")

chk<-function(stk,srr,yrs,rsd=NULL,mult=TRUE){
   recYrs<-(1+dims(stk)$min):dims(stk)$year
   ssbYrs<-1:(dims(stk)$year-dims(stk)$min)
   
   plot(  ssb(srr)*1000,     rec(srr)*1000,      xlab="SSB", ylab="Recruits")
   points(ssb(srr)*1000,     fitted(srr)*1000,   pch=19)
   points(ssb(stk)[,ac(yrs-dims(stk)$min)],rec(stk)[,ac(yrs)], pch=19,col="red")
   
   if (is.null(rsd)) recs<-rec(stk)[,ac(yrs)]
   else
      if (mult) recs<-sweep(rec(stk)[,ac(yrs)],1:2,rsd[,ac(yrs)],"/")
      else      recs<-sweep(rec(stk)[,ac(yrs)],1:2,rsd[,ac(yrs)],"-")
   
   return(sweep(recs,1:2,fitted(srr)[,ac(yrs)]*1000,"-"))
   }

#### Test the different ways of passing SRRs ###################################
## FLSR
ple4SR       <-as.FLSR(ple4)
model(ple4SR)<-ricker()
ple4SR       <-transform(ple4SR,ssb=ssb/1000,rec=rec/1000)
ple4SR       <-fmle(ple4SR)
params(ple4SR)["b",1]<-params(ple4SR)["b",1]/1000

yrs <-1997:1999

ctrl<-fwdControl(data.frame(year=yrs,val=25000,quantity="catch"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
computeCatch(res)[,ac(1997:1999)]

landings.n( ple4)      <-0.5*catch.n(ple4)
discards.n( ple4)      <-0.5*catch.n(ple4)
discards.wt(ple4)      <-landings.wt(ple4)

ctrl<-fwdControl(data.frame(year=yrs,val=25000,quantity="landings"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
computeCatch(res)[,ac(1997:1999)]
computeLandings(res)[,ac(1997:1999)]

ctrl<-fwdControl(data.frame(year=yrs,val=25000,quantity="catch"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
computeCatch(res)[,ac(1997:1999)]
computeLandings(res)[,ac(1997:1999)]

ctrl<-fwdControl(data.frame(year=yrs,val=25000,quantity="ssb"))
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
ssb(res)[,ac(1997:1999)]

## FLSR
data(ple4)
#landings.n( ple4)      <-0.5*catch.n(ple4)
#discards.n( ple4)      <-0.5*catch.n(ple4)
#discards.wt(ple4)      <-landings.wt(ple4)
   
## Test SRR
ctrl<-fwdControl(data.frame(year=yrs,val=c(ssb(iter(ple4,1))[,ac(yrs+1)]),quantity="ssb"))
stock.n(ple4)[1,ac(1997:2000)]<-NA
res<-fwd(ple4,ctrl=ctrl,sr=ple4SR)
chk(res,ple4SR,yrs)

## list
##Character
res<-fwd(ple4, ctrl=ctrl, sr=list(model="ricker", params=params(ple4SR)[c("a","b"),1,drop=T]))
chk(res,ple4SR,yrs)

res<-fwd(ple4, ctrl=ctrl, sr=list(model="ricker", params=FLPar(params(ple4SR)[c("a","b"),1,drop=T])))
chk(res,ple4SR,yrs)

##Formula
res<-fwd(ple4, ctrl=ctrl, sr=list(model=formula(rec ~ a * ssb * exp(-b * ssb)), params=FLPar(params(ple4SR)[c("a","b"),1,drop=T])))
chk(res,ple4SR,yrs)

##FLPar by year
srPar<-FLPar(array(params(ple4SR)[c("a","b"),1,drop=T],dim=c(2,3,1),dimnames=list(params=c("a","b"),year=1997:1999,iter=1)))

res <-fwd(ple4, ctrl=ctrl, sr=list(model="ricker", params=srPar))
chk(res,ple4SR,yrs)

##Residuals
srRes<-FLQuant(array(c(1,1.25,1.5),dim=c(1,3,1),dimnames=list(age=1,year=1997:1999,iter=1)))

res<-fwd(ple4, ctrl=ctrl, sr=ple4SR, sr.residuals=srRes)
chk(res,ple4SR,yrs,rsd=srRes)

res<-fwd(ple4, ctrl=ctrl, sr=ple4SR, sr.residuals=srRes*1000, sr.residuals.mult=FALSE) 
chk(res,ple4SR,yrs,rsd=srRes*1000,mult=FALSE)

#### Test all targets, absolute
ctrl<-fwdControl(data.frame(year=yrs,val=200000,quantity="ssb"))
res<-fwd(ple4, ctrl=ctrl, sr=ple4SR)
ssb(res)[,ac(1998:2000)]-200000
res <-fwd(ple4, ctrl=ctrl, sr=list(model="ricker", params=srPar))
ssb(res)[,ac(1998:2000)]-200000

ctrl<-fwdControl(data.frame(year=yrs,val=300000,quantity="biomass"))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
computeStock(res)[,ac(1998:2000)]-300000

ctrl<-fwdControl(data.frame(year=yrs[1],val=75000,quantity="catch"))
res <-fwd(ple4, ctrl=ctrl, sr=list(model="ricker", params=srPar[,1,]))
computeCatch(res)[,ac(yrs)]-75000
                                                                 
ctrl<-fwdControl(data.frame(year=yrs,val=75000,quantity="landings"))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
computeLandings(res)[,ac(yrs)]-75000
                                                                 
ctrl<-fwdControl(data.frame(year=yrs,val=30000,quantity="discards"))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
computeDiscards(res)[,ac(yrs)]-30000
                                                                 
ctrl<-fwdControl(data.frame(year=yrs,val=0.5,  quantity="f"))
res <-fwd(ple4, ctrl=ctrl, sr=list(model="ricker", params=srPar))
fbar(res)[,ac(yrs)]-0.5
                                        
fbarRng<-ac(range(res,c("minfbar")):range(res,c("maxfbar")))                                                         
ctrl<-fwdControl(data.frame(year=yrs,val=0.75, quantity="z"))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
fbar(res)[,ac(yrs)]+apply(m(res)[fbarRng,ac(yrs)],c(2,6),mean)-0.75
                                                                 
ctrl<-fwdControl(data.frame(year=yrs,val=0.25, quantity="f.landings"))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
apply(harvest(res)[fbarRng,ac(yrs)]*(landings.n(res)[fbarRng,ac(yrs)]/catch.n(res)[fbarRng,ac(yrs)]),c(2,6),mean)-0.25
                                                                 
ctrl<-fwdControl(data.frame(year=yrs,val=0.25, quantity="f.discards"))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
apply(harvest(res)[fbarRng,ac(yrs)]*(discards.n(res)[fbarRng,ac(yrs)]/catch.n(res)[fbarRng,ac(yrs)]),c(2,6),mean)-0.25
                                                                 
#ctrl<-fwdControl(data.frame(year=yrs,val=c(ssb(ple4)[,ac(yrs+1)]),quantity="effort"))
#ctrl<-fwdControl(data.frame(year=yrs,val=c(ssb(ple4)[,ac(yrs+1)]),quantity="costs"))
#ctrl<-fwdControl(data.frame(year=yrs,val=c(ssb(ple4)[,ac(yrs+1)]),quantity="revenue"))
#ctrl<-fwdControl(data.frame(year=yrs,val=c(ssb(ple4)[,ac(yrs+1)]),quantity="profit"))

#### Test all targets, relative
ctrl<-fwdControl(data.frame(year=yrs,val=1,quantity="ssb",rel=1996))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
ssb(res)[,ac(1998:2000)]-ssb(res)[,ac(1997)]

ctrl<-fwdControl(data.frame(year=yrs,val=1,  quantity="biomass",rel=1996))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
computeStock(res)[,ac(1997:1999)]-computeStock(res)[,ac(1997)]

ctrl<-fwdControl(data.frame(year=yrs,val=1,  quantity="catch",rel=1996))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
computeCatch(res)[,ac(1997:1999)]-computeCatch(res)[,ac(1996)]
                                                                 
ctrl<-fwdControl(data.frame(year=yrs,val=1,  quantity="landings",rel=1996))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
computeLandings(res)[,ac(1997:1999)]-computeLandings(res)[,ac(1996)]
                                                                 
####ERROR
ctrl<-fwdControl(data.frame(year=yrs,val=1,  quantity="discards",rel=1996))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
computeDiscards(res)[,ac(1997:1999)]-computeDiscards(res)[,ac(1996)]

ctrl<-fwdControl(data.frame(year=yrs,val=1,  quantity="f",rel=1996))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
fbar(res)[,ac(1997:1999)]- fbar(res)[,ac(1996)]
                                         
fbarRng<-ac(range(res,c("minfbar")):range(res,c("maxfbar")))                                                         
ctrl<-fwdControl(data.frame(year=yrs,val=1, quantity="z",rel=1996))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
tmp<-fbar(res)[,ac(1996:1999)]+apply(m(res)[fbarRng,ac(1996:1999)],c(2,6),mean)
tmp[,ac(1997:1999)]-tmp[,ac(1996)]                                                                 
                                                                 
ctrl<-fwdControl(data.frame(year=yrs,val=1, quantity="f.landings",rel=1996))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
tmp<-apply(harvest(res)[fbarRng,ac(1996:1999)]*(landings.n(res)[fbarRng,ac(1996:1999)]/catch.n(res)[fbarRng,ac(1996:1999)]),c(2,6),mean)
tmp[,ac(1997:1999)]-tmp[,ac(1996)]                                                                 
                                                                 
ctrl<-fwdControl(data.frame(year=yrs,val=1, quantity="f.discards",rel=1996))
res <-fwd(ple4, ctrl=ctrl, sr=ple4SR)
tmp<-apply(harvest(res)[fbarRng,ac(1996:1999)]*(discards.n(res)[fbarRng,ac(1996:1999)]/catch.n(res)[fbarRng,ac(1996:1999)]),c(2,6),mean)
tmp[,ac(1997:1999)]-tmp[,ac(1996)]                                                                 
                                                                  
#ctrl<-fwdControl(data.frame(year=yrs,val=1,rel=1996,quantity="effort"))
#ctrl<-fwdControl(data.frame(year=yrs,val=1,rel=1996,quantity="costs"))
#ctrl<-fwdControl(data.frame(year=yrs,val=1,rel=1996,quantity="revenue"))
#ctrl<-fwdControl(data.frame(year=yrs,val=1,rel=1996,quantity="profit"))

##Iters
ple4<-propagate(ple4,iter=10)
ctrl<-fwdControl(data.frame(year=yrs,val=c(ssb(ple4)[,ac(yrs+1),,,,1]),quantity="ssb"))
srRes<-FLQuant(seq(1,2,length.out=10),dimnames=list(age=1,year=1997:1999,iter=1:10))
res<-fwd(ple4, ctrl=ctrl, sr=ple4SR, sr.residuals=srRes)
chk(res,ple4SR,yrs,rsd=srRes)

