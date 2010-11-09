library(FLipper)
library(FLAssess)
library(FLash)

#### Data
load("c:\\Stuff\\FLR\\pkg\\FLR4SCRS\\Data\\bftE.RData")
source("C:\\Stuff\\FLR\\pkg\\FLR4SCRS\\R\\flpar.R")

vB  =c(Linf=318.85,K=0.093,t0=-0.97-.16667,a=1.96e-8,b=3.0092)

#### 3 different stock objects #################################################
## i) as VPA                                                                   #
## ii) plus group weight esimated from mean age                                #
## iii) plus group pushed out to age 40                                        #
################################################################################

bft                   <-FLStocks()
bft[["VPA"]]          <-bftE
bft[["Mean Age"]]     <-bftE
bft[["Plus Group=40"]]<-setPlusGroup(bftE,40,keepPlusGroup=FALSE)
rm(bftE)

### ii) plus group mean age
## use growth curve for new ages
wt(bft[["Mean Age"]][10])   <-vonBMass(pgAge(bft[["Mean Age"]],nAges=100)+0.5,FLPar(vB))

### iii) plus group pushed out to age 40
## use growth curve for new ages
stock.wt(   bftE[["Plus Group=40"]])[ac(10:40)]<-vonBMass(as.numeric(10:40+0.5),FLPar(vB))
catch.wt(   bftE[["Plus Group=40"]])[ac(10:40)]<-stock.wt(bftE[["Plus Group=40"]])[ac(10:40)]
discards.wt(bftE[["Plus Group=40"]])[ac(10:40)]<-stock.wt(bftE[["Plus Group=40"]])[ac(10:40)]
landings.wt(bftE[["Plus Group=40"]])[ac(10:40)]<-stock.wt(bftE[["Plus Group=40"]])[ac(10:40)]

## Update with new plus for estimated recruits and FBar
ctrl   <-fwdControl(data.frame(quantity="f",val=fbar(bftE)[,-1,drop=T],year=as.numeric(dimnames(m(bftE))$year[-1])))
bftE40<-fwd(bftE40,ctrl=ctrl, sr=list(model="mean",params=FLPar(1)),sr.residuals=rec(bftE))

plot(FLStocks(bftE,bftE40))

#### Compare
pgSSB<-FLQuants("10+"       =apply((stock.wt(bftE   )*stock.n(bftE   ))[ac(10   )],2,sum),
                "Expanded"  =apply((stock.wt(bftE40 )*stock.n(bftE40 ))[ac(10:40)],2,sum),
                "Projected" =apply((stock.wt(bftEPrj)*stock.n(bftEPrj))[ac(10:40)],2,sum))
ggplot(as.data.frame(pgSSB))+geom_line(aes(year,data,group=qname,col=qname),size=2)+expand_limits(y = 0) +
     xlab("Year") + ylab("Biomass in Plus Group") +
 scale_colour_discrete(name = "Plus Group", breaks=c("10+","Expanded","Projected"),labels=c("10+", "Expanded","Projected"))

pgYld<-FLQuants("10+"       =apply((catch.wt(bftE   )*catch.n(bftE   ))[ac(10   )],2,sum),
                "Expanded"  =apply((catch.wt(bftE40 )*catch.n(bftE40 ))[ac(10:40)],2,sum),
                "Projected" =apply((catch.wt(bftEPrj)*catch.n(bftEPrj))[ac(10:40)],2,sum))
ggplot(as.data.frame(pgYld))+geom_line(aes(year,data,group=qname,col=qname),size=2)+expand_limits(y = 0) + xlab("Year") + ylab("Yield from Plus Group") +
 scale_colour_discrete(name = "Plus Group", breaks=c("10+","Expanded","Projected"),labels=c("10+", "Expanded","Projected"))

pgSn<-FLQuants("10+"        =apply(stock.n(bftE   )[ac(10   )],2,sum),
               "Expanded"   =apply(stock.n(bftE40 )[ac(10:40)],2,sum),
               "Projected"  =apply(stock.n(bftEPrj)[ac(10:40)],2,sum))
ggplot(as.data.frame(pgSn))+geom_line(aes(year,data,group=qname,col=qname),size=2)+expand_limits(y = 0) + xlab("Year") + ylab("Numbers in Plus Group") +
 scale_colour_discrete(name = "Plus Group", breaks=c("10+","Expanded","Projected"),labels=c("10+", "Expanded","Projected"))

pgYn <-FLQuants("10+"       =apply(catch.n(bftE   )[ac(10   )],2,sum),
                "Expanded"  =apply(catch.n(bftE40 )[ac(10:40)],2,sum),
                "Projected" =apply(catch.n(bftEPrj)[ac(10:40)],2,sum))
ggplot(as.data.frame(pgYld))+geom_line(aes(year,data,group=qname,col=qname),size=2)+expand_limits(y = 0) + xlab("Year") + ylab("Catch Numbers in Plus Group") +
 scale_colour_discrete(name = "Plus Group", breaks=c("10+","Expanded","Projected"),labels=c("10+", "Expanded","Projected"))

swts<-pgSSB
swts<-FLQuants(swts[[1]]/pgSn[[1]],
               swts[[2]]/pgSn[[2]],
               swts[[3]]/pgSn[[3]],
               vonBMass(pgAge(bftE),FLPar(vB)))
names(swts)<-c("10+","Expanded","Projected","Mean Age")
ggplot(as.data.frame(swts))+geom_line(aes(year,data,group=qname,col=qname),size=2)+expand_limits(y = 0) + xlab("Year") + ylab("Mean Weight in Plus Group") +
 scale_colour_discrete(name = "Plus Group")

cwts<-pgYld
cwts<-FLQuants(cwts[[1]]/pgYn[[1]],
               cwts[[2]]/pgYn[[2]],
               cwts[[3]]/pgYn[[3]],
               vonBMass(pgAge(bftE),FLPar(vB)))
names(cwts)<-c("10+","Expanded","Projected","Mean Age")
ggplot(as.data.frame(cwts))+geom_line(aes(year,data,group=qname,col=qname),size=2)+expand_limits(y = 0) + xlab("Year") + ylab("Mean Weight in Plus Group") +
 scale_colour_discrete(name = "Plus Group")

sop<-apply((stock.n(bftE40)*stock.wt(bftE40)*mat(bftE40))[ac(10:40)],2,sum)/(stock.n(bftE)*stock.wt(bftE)*mat(bftE))["10"]
stock.wt(bftE40)[ac(10:40)]<-sweep(stock.wt(bftE40)[ac(10:40)],2,sop,"/")

sop<-apply((catch.n(bftE40)*catch.wt(bftE40))[ac(10:40)],2,sum)/(catch.n(bftE)*catch.wt(bftE))["10"]
catch.wt(   bftE40)[ac(10:40)]<-sweep(catch.wt(bftE40)[ac(10:40)],2,sop,"/")
landings.wt(bftE40)[ac(10:40)]<-catch.wt(bftE40)[ac(10:40)]
discards.wt(bftE40)[ac(10:40)]<-catch.wt(bftE40)[ac(10:40)]
catch(   bftE40)              <-computeCatch(   bftE40,"all")

plot(FLStocks(bftE,bftE40))

bftE40<-fwd(bftE40,ctrl=ctrl, sr=bftSR,sr.residuals=exp(residuals(bftSR)))
plot(FLStocks(bftE,bftE40))

apply(sweep(stock.n(bftE40)[ac(10:40)],1,10:40,"*"),2,sum)/apply(stock.n(bftE40)[ac(10:40)],2,sum)
pgAge(bftE,keepPlusGroup=F,nAges=40)

### Projections ################################################################
#### SRR
bftSR         <-as.FLSR(bftE)
model(bftSR)  <-bevholt()
bftSR         <-fmle(bftSR)
plot(bftSR)



sr=bftSR #list(model="mean",params=FLPar(mean(rec(bftE))))
# 10+

## Ignoring plus group
prj<-stf(bftE,nyears=20)
projPG<-FLStocks()
projPG[["0.1"]]  <-fwd(prj,ctrl=fwdControl(data.frame(quantity="f",val=0.1,year=dims(bftE)$maxyear+1:20)),sr=sr)
projPG[["0.2"]]  <-fwd(prj,ctrl=fwdControl(data.frame(quantity="f",val=0.2,year=dims(bftE)$maxyear+1:20)),sr=sr)
projPG[["0.3"]]  <-fwd(prj,ctrl=fwdControl(data.frame(quantity="f",val=0.3,year=dims(bftE)$maxyear+1:20)),sr=sr)
projPG[["0.4"]]  <-fwd(prj,ctrl=fwdControl(data.frame(quantity="f",val=0.4,year=dims(bftE)$maxyear+1:20)),sr=sr)
projPG[["0.5"]]  <-fwd(prj,ctrl=fwdControl(data.frame(quantity="f",val=0.5,year=dims(bftE)$maxyear+1:20)),sr=sr)
plot(projPG)

## Modelling mean age
projMA<-FLStocks()
vBGrowth<-function(x,F,sr,vB,yrs=19:0){
  for (i in yrs){
      x  <-fwd(x,ctrl=fwdControl(data.frame(quantity="f",val=F,year=dims(x)$maxyear-i)),sr=sr)
      stock.wt(x)[ac(10),ac(dims(x)$maxyear-i)] <-vonBMass(pgAge(x)[,ac(dims(x)$maxyear-i)],FLPar(vB))
      }
      
  catch.wt(x)[   ac(10),ac(dims(x)$maxyear-yrs)] <-stock.wt(x)[ac(10),ac(dims(x)$maxyear-yrs)]
  discards.wt(x)[ac(10),ac(dims(x)$maxyear-yrs)] <-stock.wt(x)[ac(10),ac(dims(x)$maxyear-yrs)]
  landings.wt(x)[ac(10),ac(dims(x)$maxyear-yrs)] <-stock.wt(x)[ac(10),ac(dims(x)$maxyear-yrs)]
  catch(x)<-computeCatch(x,"all")

  return(x)}
  
projMA<-FLStocks()
projMA[["0.1"]]  <-vBGrowth(prj,0.1, sr=sr,vB)
projMA[["0.2"]]  <-vBGrowth(prj,0.2, sr=sr,vB)
projMA[["0.3"]]  <-vBGrowth(prj,0.3, sr=sr,vB)
projMA[["0.4"]]  <-vBGrowth(prj,0.4, sr=sr,vB)
projMA[["0.5"]]  <-vBGrowth(prj,0.5, sr=sr,vB)
plot(projMA)

## Expanding out at max  vpa year
prj<-stf(bftE40,nyears=20)
### use growth curve for new ages
stock.wt(   prj)[ac(10:40)]<-vonBMass(as.numeric(10:40),FLPar(vB))
catch.wt(   prj)[ac(10:40)]<-stock.wt(prj)[ac(10:40)]
discards.wt(prj)[ac(10:40)]<-stock.wt(prj)[ac(10:40)]
landings.wt(prj)[ac(10:40)]<-stock.wt(prj)[ac(10:40)]

projNew<-FLStocks()
projNew[["0.1"]]  <-fwd(prj,ctrl=fwdControl(data.frame(quantity="f",val=0.1,year=2007+1:20)), sr=sr)
projNew[["0.2"]]  <-fwd(prj,ctrl=fwdControl(data.frame(quantity="f",val=0.2,year=2007+1:20)), sr=sr)
projNew[["0.3"]]  <-fwd(prj,ctrl=fwdControl(data.frame(quantity="f",val=0.3,year=2007+1:20)), sr=sr)
projNew[["0.4"]]  <-fwd(prj,ctrl=fwdControl(data.frame(quantity="f",val=0.4,year=2007+1:20)), sr=sr)
projNew[["0.5"]]  <-fwd(prj,ctrl=fwdControl(data.frame(quantity="f",val=0.5,year=2007+1:20)), sr=sr)
plot(projMA)

plot(FLStocks(projMA[[1]],projNew[[1]],projPG[[1]]))

