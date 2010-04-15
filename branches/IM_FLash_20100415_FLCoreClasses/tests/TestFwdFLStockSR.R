#===============================================================================
## Date: 19/01/2009
# Version: 0.1-0
# Authors: Laurence Kell
#
# Short description: tests of SR for fwd(FLStock)
#
# ToDo:
#
# References (bibtex):
#
#!Notes:
#===============================================================================


library(FLash)
source("C:/stuff/FLR/Branch/R/fwdControl.R")
source("C:/stuff/FLR/Branch/R/fwd.R")
source("C:/stuff/FLR/Branch/R/setSRs.R")
source("C:/stuff/FLR/Branch/R/FLCoreVarCon.R")
source("C:/Stuff/FLR/Branch/R/validityFLSR.r")

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

#### Test SRRs
chkSR<-function(stk,srr,yrs,rsd=NULL,mult=TRUE){
   recYrs<-(1+dims(stk)$min):dims(stk)$year
   ssbYrs<-1:(dims(stk)$year-dims(stk)$min)

   plot(  ssb(srr)*1000,     rec(srr)*1000,      xlab="SSB", ylab="Recruits")
   points(ssb(srr)*1000,     fitted(srr)*1000,   pch=19)
   points(ssb(stk)[,ac(yrs-dims(stk)$min)],rec(stk)[,ac(yrs)], pch=19,col="red")

   if (is.null(rsd)) recs<-rec(stk)[,ac(yrs)]
   else
      if (mult) recs<-sweep(rec(stk)[,ac(yrs)],c(1:2,6),rsd[,ac(yrs)],"/")
      else      recs<-sweep(rec(stk)[,ac(yrs)],c(1:2,6),rsd[,ac(yrs)],"-")

   return(sweep(recs,c(1:2,6),predict(srr,ssb=ssb(stk)[,ac(yrs-dims(stk)$min)]),"-"))
   }


#### Tests
ple4<-ple4[,ac(1996:2001)]
yrs <-1998:2001
ctrl<-fwdControl(data.frame(year=yrs,val=0.45,quantity="f")) #,season=1))

##### FLSR object
res  <-fwd(ple4,ctrl=ctrl,sr=ple4SR)
chkSR(res,ple4SR,yrs)

##### FLSR object
stock.n(ple4)[1,ac(yrs)]<-NA
res  <-fwd(ple4,ctrl=ctrl,sr=ple4SR)
chkSR(res,ple4SR,yrs)

#### list & Character
res<-fwd(ple4, ctrl=ctrl, sr=list(model="ricker", params=params(ple4SR)[c("a","b"),1]))
chkSR(res,ple4SR,yrs)

res<-fwd(ple4, ctrl=ctrl, sr=list(model="ricker", params=FLPar(params(ple4SR)[c("a","b"),1,drop=T])))
chkSR(res,ple4SR,yrs)

#### list & Formula
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

