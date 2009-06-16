library(FLCore)
library(FLBRP)
library(FLash)

#runOM<-function(OM,fishery,eff,catchability,srPar,avail,diffussion){

#setMethod("fwd", signature(obj="FLStock", fleets = "missing"),
#    function(obj, ctrl,
#               sr          =NULL,
#               sr.residuals=NULL, sr.residuals.mult=TRUE)
#### single area
stk<-FLStock(stock.n=as.FLQuant(read.csv("C:\\Stuff\\FLR\\WorkInProgress\\fwd\\N.csv")[,1:4]),
             harvest=as.FLQuant(read.csv("C:\\Stuff\\FLR\\WorkInProgress\\fwd\\F.csv")[,1:4],units="f"),
             m      =as.FLQuant(read.csv("C:\\Stuff\\FLR\\WorkInProgress\\fwd\\M.csv")[,1:4]))

pSR        <-as.FLSR(stk)
model( pSR)<-geomean()
params(pSR)<-FLPar(array(1,c(1,1,1),list(iter=1,params="a",season=1)))
#stk2       <-fwdStock(stk,fbar=fbar(stk)[,-1],sr=pSR)
#stock.n(stk2)/stock.n(stk)

#### multiple areas
dmns<-dimnames(m(stk))
dmns$area<-1:2
stkA<-FLStock(harvest=FLQuant(NA, dimnames=dmns, units="f"))

stock.n(stkA)[,,,,1]<-stock.n(stk)*.3
stock.n(stkA)[,,,,2]<-stock.n(stk)*.7
harvest(stkA)[,,,,1]<-harvest(stk)
harvest(stkA)[,,,,2]<-harvest(stk)*1.0
m(      stkA)[]     <-m(      stk)

params(pSR) <-FLPar(array(1,c(1,1,1),list(iter=1,params="a",season=1)))

stkA2       <-fwdStock(stkA,fbar=fbar(stkA)[,-1],sr=pSR)
stock.n(stkA2)/stock.n(stkA)

#### multiple areas
