library(FLR4SCRS)
library(FLBRP)

#### Get data ##################################################################
lhPar<-read.csv("C:/Stuff/Publications/inPrep/risk/inputs/Gislason_etal_2008_GrowthParameters.csv")[-164,]
lhPel<-lhPar[lhPar[,"Habitat"]=="Pelagic",]

cor(lhPel[,c("M","T","L","Linf","k")])

albPar<-lhPar[lhPar$Species=="Thunnus alalunga",]
################################################################################


#### Indicators ################################################################
#### Functions #################################################################
#### biological correlations and processes
## Von Bertalannfy growth function
growth=function(t,Linf,k,t0=0,a=0.001,b=3) a*(Linf*(1-exp(-k*(t-t0))))^b

## Natural Mortality as a function of Linf
M     =function(L,Linf,M1=0.1,h=1.71,n=-1.66,i=0.8) M1+h*Linf^i*L^n

## Maturity ogive
## Age at 50% maturity
mat50 =function(M,k) log(((3*k+M)/M)/k)

## logistic
logistic=function(x,a50,ato95) 1.0/(1.0+19.0^((a50-x)/ato95))

matOgive=function(x,a50,ato95) sapply(x,logistic,a50,a50+ato95)

# dnormal(x, a1, sL, sR) {{{
dnormal <- function(x,a1,sL,sR){
  pow<-function(a,b) a^b

  func <- function(x,a1,sL,sR){
    if (x < a1)
       return(pow(2.0,-((x-a1)/sL*(x-a1)/sL)))
    else
       return(pow(2.0,-((x-a1)/sR*(x-a1)/sR)))}

  sapply(x,func,a1,sL,sR)}
################################################################################

#### Life History Generator ####################################################
lhGen=function(k,Linf,steepness=0.75,vbiomass=1e3,sel=c(a=1,sL=1,sR=1),mat95=3,sr="bevholt",age=1:75,...){

   ## Dimensions, age by year range
   dms=list(age=age,year=1)

   ## Biological processes
   wts       =FLQuant(growth(age,Linf,k)                          ,dimnames=dms)
   m         =FLQuant(M(c(1000*growth(age+0.5,Linf,k))^(1/3),Linf),dimnames=dms)
   matPerc50 =mat50(c(mean(m)),k)
   mat       =FLQuant(logistic(age,matPerc50,matPerc50+mat95)     ,dimnames=dms)
   selPattern=FLQuant(dnormal(age,(matPerc50+mat95)*sel["a"],
                                  (matPerc50+mat95)*sel["sL"],
                                  (matPerc50+mat95)*sel["sR"]),    dimnames=dms)

   ## create a FLBRP object to calculate expected equilibrium values and ref pts
   res=FLBRP(stock.wt       =wts,
             landings.wt    =wts,
             discards.wt    =wts,
             bycatch.wt     =wts,
             m              =m,
             mat            =mat,
             landings.sel   =FLQuant(selPattern,dimnames=dms),
             discards.sel   =FLQuant(0,         dimnames=dms),
             bycatch.harvest=FLQuant(0,         dimnames=dms),
             harvest.spwn   =FLQuant(0,         dimnames=dms),
             m.spwn         =FLQuant(0,         dimnames=dms),
             availability   =FLQuant(1,         dimnames=dms))

   ## i.e. FApex
   range(res,c("minfbar","maxfbar"))[]<-as.integer((matPerc50+mat95)*sel["a"])

   ## Stock recruitment realationship
   model(res) =do.call(sr,list())$model
   params(res)=FLPar(abPars(sr,spr0=spr0(res),s=steepness,v=vbiomass))

   ## replace any slot passed in as an arg
   args<-list(...)
   for (slt in names(args)[names(args) %in% names(getSlots("FLBRP"))])
     slot(res, slt)<-args[[slt]]

   return(brp(res))}
################################################################################

#### Indicators ################################################################
mnLenCatch<-function(object,a=0.001,b=3)
    apply((catch.wt(object)/a)^(1/b)*catch.n(object),c(2,6),sum)/
    apply(catch.n(object),c(2,6),sum)

mLenStock<-function(object,a=0.001,b=3)
    apply((stock.wt(object)/a)^(1/b)*stock.n(object),c(2,6),sum)/
    apply(stock.n(object),c(2,6),sum)

ZStock <-function(object,Linf,Lc,k,a=0.001,b=3){
    mnSz<-mnSzStock(object,a,b); k*(Linf-mnSz)/(mnSz-Lc)}
ZCatch <-function(object,Linf,Lc,k,a=0.001,b=3){
    mnSz<-mnSzCatch(object,a,b); k*(Linf-mnSz)/(mnSz-Lc)}
################################################################################

mnSzCatch<-function(object,a=0.001,b=3)
    apply((catch.wt(object)/a)^(1/b)*catch.n(object),c(2,6),sum)/
    apply(catch.n(object),c(2,6),sum)

mnSzStock<-function(object,a=0.001,b=3)
    apply((stock.wt(object)/a)^(1/b)*stock.n(object),c(2,6),sum)/
    apply(stock.n(object),c(2,6),sum)

ZS <-function(object,Linf,Lc,k,a=0.001,b=3){
    mnSz<-mnSzStock(object,a,b); k*(Linf-mnSz)/(mnSz-Lc)}
ZC <-function(object,Linf,Lc,k,a=0.001,b=3){
    mnSz<-mnSzCatch(object,a,b); k*(Linf-mnSz)/(mnSz-Lc)}

source("C:/Stuff/my Dropbox/FLRbook/MSE/R/MSE-Funs.R")
################################################################################
k        =1
Linf     =100
Lc       =25
mat95    =5
sel=c(a=1,sL=1,sR=1)

oidBRP<-lhGen(k,Linf,steepness=0.75,vbiomass=1e3,
              sel=sel,mat95=mat95,age=1:10,
              name="oid",desc="data-poor OM")

fbar(oidBRP)<-FLQuant(seq(1,2,length.out=101)*refpts(oidBRP)["msy","harvest"])
oidStk<-as(brp(oidBRP),"FLStock")
m(oidStk)<-propagate(m(oidStk),5)

srDev=FLQuant(rlnorm(dims(oidStk)$year*5,0,0.3),dimnames=list(year=1:101,iter=1:5))
ctrl  =fwdControl(data.frame(year=2:101,val=c(fbar(oidStk)[,-1]),quantity="f"))
oidStk=fwd(oidStk, ctrl=ctrl, sr=list(model=model(oidBRP), params=params(oidBRP)), sr.residuals=srDev)
plot(FLStocks(oidStk,iter(oidStk,1),iter(oidStk,2),iter(oidStk,3)))

ind<-as.data.frame(FLQuants("fbar"      =fbar(oidStk),
                            "mean Stock"=1/mnSzStock(oidStk),
                            "mean Catch"=1/mnSzCatch(oidStk),
                            "Z Stock"   =ZS(oidStk,Linf,Lc,k),
                            "Z Catch"   =ZC(oidStk,Linf,Lc,k)))

ind=ddply(ind,c("qname","iter"),
            transform,data=data/mean(data,na.rm=T))

ggplot(ind) + geom_point(aes(year,data,group=qname,col=qname))  +
              stat_smooth(aes(year,data,group=qname,col=qname)) +
              facet_wrap(~iter)

ggplot(ind) + geom_point(aes(year,data,group=iter,col=iter))  +
              stat_smooth(aes(year,data,group=iter,col=iter)) +
              facet_wrap(~qname)
################################################################################


setwd("C:\\Stuff\\My Dropbox\\SCRS\\ALB\\Seine\\exe")

it<-2

NBreaks      = 10
NFYear       = 1
NXYear       = 101

ObsLength    = c((mnSzStock(iter(oidStk,it))/0.001)^(1/3))
SampleSize   = rep(1,101)
zguess       = rbind(rep(0.5,NBreaks),1)
yguess       = rbind(seq(NFYear,NXYear,NBreaks-1),1)
sigma        = 10.0

dat<-       list("# Number of Breaks",                            NBreaks,
                 "# First Year of Data",                          NFYear,
                 "# Last Year of Data",                           NXYear,

                 "# 1,NYears Observed Mean Lengths",              ObsLength,
                 "# 1,NYears Observed Sample Sizes",              SampleSize,

                 "# VB K Parameter",                              k,
                 "# VB L-Infinity",                               Linf,

                 "# Critical Length - Length at first capture",   Lc,

                 "# 1,NBreaks+1,1,2)",                            zguess,
                 "# 1,NBreaks,  1,2)",                            yguess,

                 "# sigma",                                       sigma,
                 "# stepsize",                                    20,
                 "# casenum",                                     -1)


write.admb(dat,"C:/Stuff/My Dropbox/SCRS/ALB/Seine/exe/seine.dat")
shell("seine.exe")
res<-unlist(read.admb("C:/Stuff/My Dropbox/SCRS/ALB/Seine/exe/seine.par"))

apply(harvest(oidStk)+m(oidStk),2,"mean")
plot(res[1:11],type="l")
 