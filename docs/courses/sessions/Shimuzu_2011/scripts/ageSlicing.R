#### Chunk 1####################################################################
#### R Packages
library(FLCore)  #FLR
library(FLBRP)   #Biological Reference Points
library(mixdist) #used for fitting mixture distributions
library(ggplot2) #plotting library

plusGroup=10

## growth data
vB       =c(Linf=238.6,K=0.185,t0=-1.404)

## Get the Catch-at-size data
# If read in from CSV file on http:\\www.iccat.int
#cas       =read.table("casSWOm8508_v2.csv",sep=";",header=T)[,c(3:7,9:10)]
#names(cas)=c("year","flag","fleet","gear","ld","len","n")

# If read on from R Data set file
load("cas.RData")

## adjust lengths to mid-year
cas$len   <-cas$len+0.5

## Plot CAS by decade
cas$yr    <-cas$year %% 10
cas$decade<-cas$year-cas$yr

p=ggplot(cas) +
     geom_histogram(aes(len,weight=n),colour="darkgreen",fill="white",binwidth=5) +
     scale_x_continuous(name="Length") + scale_y_continuous(name ="Frequency")   +
     facet_grid(yr~decade)
p
################################################################################

#### Chunk 2####################################################################
#### Growth
vonBert=function(age,Linf, K, t0) Linf*(1.0-exp(-K*(age-t0)))

#### Age Slicing
ageIt=function(len=NULL,n=NULL,Linf=NULL,K=NULL,t0=0.0,timing=0.5,plusGroup=30){
     ## expected age at length adjust to beginning of year
     age=pmax(pmin(floor(t0-log(1-pmin(len/Linf,.9999999))/K+timing),plusGroup),0)

     ## calculate frequencies
     res=aggregate(n, list(age=age), sum)

     return(res)}


#### Wrapper function for statistical ageing with a plot
cas2caa<-function(x,grwPar,constr,ages){
  ## aggregate frequencies by bins
  tst   =with(x,aggregate(n,list(len=len),sum))
  lnDist=mix(tst,grwPar,"norm",constr=constr,emsteps=3,print.level=1)

  plot(lnDist,xlim=c(50,250))
  abline(v=vonBert(ages,vB["Linf"],vB["K"],vB["t0"]),col="green",lty=5)
  mtext(unique(x$year))

  return(data.frame(age=ages,lnDist$parameters))}
################################################################################

#### Chunk 3####################################################################
#### use plyr package (also called by ggplot2) to process by year
caa=ddply(cas, .(year), function(x,vB,plusGroup,timing)
        ageIt(x$len,x$n,vB["Linf"],vB["K"],vB["t0"],timing,plusGroup), vB, plusGroup, 0.5)
                
#### Create an FLQuant for FLR to take advantage of fisheries stuff
names(caa)[3]="data"
caaSlice     =as.FLQuant(caa)
################################################################################

#### Chunk 4####################################################################
## constraints
constr<-mixconstr(conmu ="MFX", fixmu=c(rep(FALSE,3),rep(TRUE,8)), consigma ="CCV")

ages<-0:10

## Set initial proportions based on age slicing,
## expected length on Von Bertalanffy growth equation,
#
# sigma on expert knowledge!
grwPar=mixparam(pi    =c(apply(sweep(caaSlice,2,apply(caaSlice,2,sum),"/"),1,mean)),
                mu    =vonBert(ages+0.5,vB["Linf"],vB["K"],vB["t0"]),
                sigma =0.05) #ageCV)
grwPar$sigma=grwPar$sigma*grwPar$mu

## plot while fitting, again using plyr function ddply
par(mfrow=c(5,5), mar=c(1,1,1,1))
ageThem     =ddply(cas,.(year),cas2caa,grwPar=grwPar,constr=constr,ages=ages)

## process so you get an FLQuant
caaStat     =merge(ageThem,with(cas,aggregate(n,by=list(year=year),sum)))
caaStat$data=caaStat$x*caaStat$pi
caaStat     =as.FLQuant(caaStat[,c("age","year","data")])
################################################################################

#### Chunk 5####################################################################
#### Catch proportions
## scale catch by max within a year put data into data.frame
dat<-rbind(data.frame(Method="Age Slicing",
                        as.data.frame(sweep(caaSlice,2,apply(caaSlice,2,max),"/"))),
           data.frame(Method="Statistical estimation",
                        as.data.frame(sweep(caaStat, 2,apply(caaStat, 2,max),"/"))))

## plot and compare
ggplot(dat) + geom_point(aes(year,age,size=abs(data))) +
              scale_area(to=c(3,10)) +
              facet_wrap(~Method)    +
              opts(legend.position = "none")
################################################################################

#### Chunk 6####################################################################
#### Catch Curves
dat<-rbind(data.frame(Method="Age Slicing",
as.data.frame(FLCohort(log(caaSlice)))),
           data.frame(Method="Statistical estimation",
as.data.frame(FLCohort(log(caaStat )))))

ggplot(dat) + geom_point(aes(age,data,group=cohort,col=cohort)) +
              geom_smooth(aes(age,data)) +
              facet_wrap(~Method) +
              scale_y_continuous(limits=c(0,12))
################################################################################

#### Chunk 7####################################################################
#### function to calculate log catch ratios down a cohort
z=function(x) FLCohort(log(x[1:(dim(x)[1]-1),-dim(x)[2]]/x[2:(dim(x)[1]),-1]))

## get data in data.frame for gglot
dat<-rbind(data.frame(Method="Age Slicing",
                      as.data.frame(FLCohort(z(caaSlice)))),
           data.frame(Method="Statistical estimation",
                      as.data.frame(FLCohort(z(caaStat )))))

## plotting
p = ggplot(dat)

p + geom_line(aes(age,data,col=cohort,group=cohort))    +
    stat_smooth(aes(age,data), method="loess",span=0.3) +
    facet_wrap(~Method)

p + geom_line(aes(cohort+age,data,col=age,group=age)) +
    stat_smooth(aes(cohort+age,data), method="loess",span=0.3) +
    facet_wrap(~Method) +
    scale_x_continuous(limit=c(1985,2005))
################################################################################

#### Chunk 8####################################################################
## 2010 assessment
load("swo2009PG10.RData")
swoBrp<-FLBRP(swo2009PG10,nyears=21)
fbar(swoBrp)<-apply(fbar.obs(swoBrp),1,mean)

ggplot(as.data.frame(harvest(swo2009PG10))) +
      geom_point(aes(age,data))+stat_smooth(aes(age,data))

objFn<-function(par,x,y){
  landings.sel(x)[]<-par
  ctch<-catch.n(brp(x))
  ctch<-sweep(ctch,2,apply(ctch,2,max),"/")
  y   <-apply(sweep(   y,2,apply(   y,2,max),"/"),1,mean)

  return(log(sum((ctch-y)^2)))}

par<-c(catch.sel(swoBrp))
objFn(par,swoBrp,caaStat)
res<-optim(par,objFn,x=swoBrp,y=caaStat)

swoBrp2<-swoBrp
landings.sel(swoBrp2)[]<-res$par
fbar(swoBrp) <-FLQuant(seq(0,1,length.out=51))
fbar(swoBrp2)<-FLQuant(seq(0,1,length.out=51))

swoBrp <-brp(swoBrp)
swoBrp2<-brp(swoBrp2)


rpts<-data.frame(rbind(refpts(swoBrp )[1:2,1:5,1,drop=T],
                       refpts(swoBrp2)[1:2,1:5,1,drop=T]))
rpts<-cbind(slicing=rep(c("deterministic","stochastic"),each=2),
            refpt  =rep(c("F0.1","FMSY"),2),rpts)

ts<-rbind(cbind(slicing="deterministic",model.frame(FLQuants(Fbar=fbar(swoBrp), SSB=ssb(swoBrp), Yield=yield(swoBrp )))[,c("year","Fbar","SSB","Yield")]),
          cbind(slicing="stochastic",   model.frame(FLQuants(Fbar=fbar(swoBrp2),SSB=ssb(swoBrp2),Yield=yield(swoBrp2)))[,c("year","Fbar","SSB","Yield")]))


ggplot(ts)+
  geom_line(aes(Fbar,Yield,col=slicing))+
  geom_point(data=rpts,aes(harvest,yield,col=slicing),size=4,pch=19)
################################################################################

#### Chunk 9####################################################################
#### Age Slicing
## function
mcAgeSlice<-function(x,vB,timing,plusGroup) {
   x<-x[sample(1:dim(x)[1],dim(x)[1],TRUE),]
   ageIt(x$len,x$n,vB["Linf"],vB["K"],vB["t0"],0.5,10)[,2]}

cas1985<-cas[cas$year==1985,]
cas2009<-cas[cas$year==2009,]
mcAS<-array(NA,c(2,11,1000))
for (i in 1:1000) mcAS[1,,i]<-mcAgeSlice(cas1985, vB, timing, plusGroup)[,2]
for (i in 1:1000) mcAS[2,,i]<-mcAgeSlice(cas2009, vB, timing, plusGroup)[,2]

## CVs
apply(mcAS,1:2,function(x) (var(x)^0.5)/mean(x))

#### Statistical estimates
tst   =with(cas[cas$year==2009,],aggregate(n,list(len=len),sum))
lnDist=mix(tst,grwPar,"norm",constr=constr,emsteps=3,print.level=1)
lnDist$se/lnDist$parameters
################################################################################

#### Chunk 10 ##################################################################
################################################################################

