library(ggplot2)
library(FLXSA)
################################################################################
## Mediterannean swordish assessment example                                  ##
################################################################################

### get data ###################################################################
load("\\\\192.168.100.101\\flr_2011\\FLR\\Data\\RData\\swoSA.RData")
ls()
summary(swo)
plot(swo)

summary(swoCPUE)
is(swoCPUE)

head(as.data.frame(swoCPUE[[1]]))

swoCPUE.df<-ldply(swoCPUE,as.data.frame)
head(swoCPUE.df)

swoCPUE.4 <-swoCPUE.df[swoCPUE.df$age==4 & swoCPUE.df$slot=="index",]
head(swoCPUE.4)
table(swoCPUE.4$.id)

ggplot(swoCPUE.4) + geom_point(aes(year,data))  +
                    stat_smooth(aes(year,data)) +
                    facet_wrap(~.id)
################################################################################

### VPA ########################################################################
swo<-swo+FLXSA(swo,swoCPUE)
plot(swo)

### Monte Carlo simulation
## add undercertainty in catch-at-age based upon CVs from age sclicing
swoMC<-swo

catch.n(swoMC)<-rlnorm(100,log(catch.n(swoMC)),0.3)
plot(catch.n(swoMC),scales="free")

swoMC<-swoMC+FLXSA(swoMC,swoCPUE,diag.flag=FALSE)

plot(swoMC)

### Natural Mortality simulation
## What if M<0.2
swoM<-swo

m(swoM)[]<-0.1
swoM<-swoM+FLXSA(swoM,swoCPUE,diag.flag=FALSE)

plot(FLStocks(swo,swoM))

### Catch simulation
## What if recent catches of large fish IUU
swoC<-swo

catch.n(swoC)[5:10,ac(2000:2008)]<-catch.n(swo)[5:10,ac(2000:2008)]*2
catch(swoC)<-computeCatch(swoC)
swoC<-swoC+FLXSA(swoC,swoCPUE,diag.flag=FALSE)

plot(FLStocks(swo,swoM,swoC))

load("\\\\192.168.100.101\\flr_2011\\FLR\\Data\\RData\\cas.RData")
vB       =c(Linf=238.6,K=0.185,t0=-1.404)

#### Age Slicing
ageIt=function(len=NULL,n=NULL,Linf=NULL,K=NULL,t0=0.0,timing=0.5,plusGroup=30){
     ## expected age at length adjust to beginning of year
     age=pmax(pmin(floor(t0-log(1-pmin(len/Linf,.9999999))/K+timing),plusGroup),0)

     ## calculate frequencies
     res=aggregate(n, list(age=age), sum)

     return(res)}

mcAgeSlice<-function(x,vB,timing,plusGroup) {
   x<-x[sample(1:dim(x)[1],dim(x)[1],TRUE),]
   ageIt(x$len,x$n,vB["Linf"],vB["K"],vB["t0"],0.5,10)[,2]}

mcAS<-array(NA,c(dims(swo)$age,dims(swo)$year,100),dimnames=list(age=dimnames(catch.n(swo))$age,year=dimnames(catch.n(swo))$year,iter=1:100))
for (iYr in dimnames(catch.n(swo)))
   for (i in 1:100)
      mcAS[,iYr,i]<-mcAgeSlice(cas[cas$year==iYr,], vB, 0.5, 10)[-1]

catch.n(swo)<-FLQuant(c(mcAS),dimnames=dimnames(mcAS))
swo         <-swo+FLXSA(swo,swoCPUE,diag.flag=FALSE)

plot(swo)
