################################################################################
## Data poor simulator                                                        ##
################################################################################
# Based on                                                                     #
# Gislason H., Daan, N., Rice, J.C. and Pope, J.G. (2008).                     #
# Does natural mortality depend on individual size? ICES CM 2008/F:16          #
################################################################################

library(ggplotFL)
library(FLCore)
library(FLBRP)

#### Data ######################################################################
## get data
 dirMy<- "//192.168.100.101/flr_2011"
dirData<-paste(dirMy,"FLR/Data/inputs",sep="/")
lhPar  <-read.csv(paste(dirData,"Gislason_etal_2008_GrowthParameters.csv",sep="/"))
head(lhPar)

## look at realtionships
## M v Linf
plotLh<-ggplot(lhPar)
plotLh<-plotLh+geom_point(aes(Linf,M,col=Habitat))
plotLh
xyplot(M~Linf,group=Habitat, data=lhPar,type="p",xlab="Linf",ylab="M")

plotLh+stat_smooth(aes(Linf,M,group=Habitat))

## Pelagic relationships
cor(lhPar[lhPar$Habitat=="Pelagic",c("M","T","L","Linf","k")])

ggplot(lhPar)+geom_point(aes(k,M,col=Habitat))+stat_smooth(aes(k,M,group=Habitat))
xyplot(M~k,group=Habitat, data=lhPar,type="p",xlab="k",ylab="M")
################################################################################

#### Example ####################################################################
# Life characteristics
Linf     =100
k        =1
t0       =0
mat95    =3

## Create expected values
dmns=list(age=1:50)

m         =FLQuant(M(c(1000*growth(dmns$age+0.5,Linf,k,t0))^(1/3),Linf),dimnames=dmns)
wts       =FLQuant(growth(dmns$age,Linf,k,t0),                          dimnames=dmns)
matPerc50 =mat50(c(mean(m)),k)
mat       =FLQuant(logistic( dmns$age,matPerc50,matPerc50+mat95),       dimnames=dmns)

## Plot them
plotBiol=ggplot(as.data.frame(FLQuants(Wts=wts,M=m,Maturity=mat))[,c("age","data","qname")]) +
                       geom_line(aes(as.numeric(age), data))       +
                       scale_x_continuous("Age",limits = c(0, 15)) +
                       facet_wrap(~qname,scale="free")
plotBiol
#xyplot(M~Linf,group=Habitat, data=lhPar,type="p",xlab="Linf",ylab="M")

#### Stock recruitment relationship
steepness=0.6
vbiomass =1e3
srModel  ="bevholt"

## SSB range for plotting
SSB    =seq(0,vbiomass*1.2,length.out=101)

## Steepness=0.6
srPar  =abPars("bevholt",spr0= sum(exp(-apply(m,2:6,cumsum))*wts*mat),0.6,vbiomass)
sr     =data.frame(Steepness=as.factor(0.6),SSB=SSB,Recruits=srPar["a"]*SSB/(srPar["b"]+SSB))

## Steepness=0.75
srPar  =abPars("bevholt",spr0= sum(exp(-apply(m,2:6,cumsum))*wts*mat),0.75,vbiomass)
sr     =rbind(sr,data.frame(Steepness=as.factor(0.75),SSB=SSB,Recruits=srPar["a"]*SSB/(srPar["b"]+SSB)))

## Steepness=0.9
srPar  =abPars("bevholt",spr0= sum(exp(-apply(m,2:6,cumsum))*wts*mat),0.9,vbiomass)
sr     =rbind(sr,data.frame(Steepness=as.factor(0.9),SSB=SSB,Recruits=srPar["a"]*SSB/(srPar["b"]+SSB)))

plotSRR=ggplot(sr,aes(SSB, Recruits,col=Steepness)) + geom_line()
plotSRR
xyplot(Recruits~SSB,group=Steepness,data=sr,type="l",xlab="SSB",ylab="Recruits")
################################################################################


## selection patterns flat topped and domed, fully selected at age 5
age   =1:15
selPat=rbind(cbind(Pattern="Flat Topped",
                   data.frame(rbind(cbind(Gear=1,sel=dnormal(age, 5,  5,250)),
                                    cbind(Gear=2,sel=dnormal(age, 7.5,5,250)),
                                    cbind(Gear=3,sel=dnormal(age,10,  5,250))))),
             cbind(Pattern="Domed",
                  data.frame(rbind(cbind(Gear=1,sel=dnormal(age, 7.5,5, 50)),
                                   cbind(Gear=2,sel=dnormal(age, 7.5,5, 20)),
                                   cbind(Gear=3,sel=dnormal(age, 7.5,5, 10))))))

selPat$Gear<-as.factor(selPat$Gear)

#### plot them
plotSel=ggplot(cbind(age=age,selPat))             +
         geom_line(aes(age, sel, colour=Gear))    +
         scale_x_continuous("Age")                +
         scale_y_continuous("Selectivity-at-age") +
         facet_wrap(~Pattern)
plotSel
xyplot(sel~age|Pattern,groups=Gear, data=cbind(age=age,selPat),type="l",xlab="Age",ylab="Selectivity")
################################################################################

#### Life History Generator ####################################################
lhGen=function(k,Linf,steepness=0.75,vbiomass=1e3,sel=c(a=1,sL=1,sR=1),mat95=3,sr="bevholt",age=1:75,...){

   ## Dimensions, age by year range
#   yrs=1:length(fmsy)
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
   range(res,c("minfbar","maxfbar"))<-(matPerc50+mat95)*sel["a"]

   ## Stock recruitment realationship
   model(res) =do.call(sr,list())$model
   params(res)=FLPar(abPars(sr,spr0=spr0(res),s=steepness,v=vbiomass))

   ## replace any slot passed in as an arg
   args<-list(...)
   for (slt in names(args)[names(args) %in% names(getSlots("FLBRP"))])
     slot(res, slt)<-args[[slt]]

   return(brp(res))}

#### Parameters
k        =1
Linf     =100
mat95    =3

sel      =c(a=1,sL=1,sR=1)

oidBRP<-lhGen(k,Linf,steepness=0.75,vbiomass=1e3,sel=sel,mat95=mat95,age=1:10,name="oid",desc="data-poor OM")
landings.sel(oidBRP)

plot(oidBRP)

## Coerce FLBRP into an FLStock, i.e. from equilibrium to dynamic
fbar(oidBRP)<-seq(1,2,length.out=101)*refpts(oidBRP)["msy","harvest"]
oidBRP      <-brp(oidBRP)
oidStk      <-as(oidBRP,"FLStock")

## Project to make dynamic
ctrl  =fwdControl(data.frame(year=2:101,val=c(fbar(oidStk)[,-1]),quantity="f"))
oidStk=fwd(oidStk, ctrl=ctrl, sr=list(model=model(oidBRP), params=params(oidBRP)))
plot(oidStk)
################################################################################

#### Projections ###############################################################
#### Process error in recruitment
srDev=FLQuant(rlnorm(dims(oidStk)$year,0,0.3))
hist(srDev)
mean(srDev)
sd(  srDev)

## Increasing F
ctrl  =fwdControl(data.frame(year=2:101,val=c(fbar(oidStk)[,-1]),quantity="f"))
oidStk=fwd(oidStk, ctrl=ctrl, sr=list(model=model(oidBRP), params=params(oidBRP)), sr.residuals=srDev)
plot(oidStk)

## Add iters so we can perform a Monte Carlo simulation
srDev =rlnorm(5*dims(oidStk)$year,0,0.3)
srDev =FLQuant(srDev,dimnames=list(year=dimnames(ssb(oidStk))$year,iter=1:5))

## bug fix
stock.n(oidStk)=propagate(stock.n(oidStk),iter=5)

oidStk1=fwd(oidStk, ctrl=ctrl, sr=list(model=model(oidBRP), params=params(oidBRP)), sr.residuals=srDev)
plot(FLStocks(iter(oidStk1,1),iter(oidStk1,2),iter(oidStk1,3),iter(oidStk1,4)))
################################################################################
