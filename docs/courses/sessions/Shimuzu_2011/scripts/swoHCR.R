##### chunk 1 ##################################################################
library(FLCore)   # use FLSR
library(FLash)    # use fwd for projections
library(FLAssess) # for stf
library(FLBRP)    # refpts

load("\\\\192.168.100.101\\flr_2011\\FLR\\Data\\RData\\swoSA.RData")

ls()
summary(swo)
plot(swo)

#### Set up a short term forecast for an FLStock object by adding extra years
## The default forecast is 3 years,
swo3<-stf(swo)

## Check what´s happened
summary(swo)
summary(swo3)
plot(FLStocks(swo3,swo))

## by default future F is the mean of last 3 years
mean(fbar(swo)[,ac(2007-(0:2))])
fbar(swo3)[,ac(2007+(1:3))]

## by default future F is the mean of last 3 years
mean(fbar(swo)[,ac(2007-(0:2))])
fbar(swo3)[,ac(2007+(1:3))]

getSlots("fwdControl")
################################################################################

##### chunk 2 ##################################################################
## Constant F Projection for a 20 year projection
swo20<-stf(swo,nyear=20)
plot(FLStocks(swo20,swo3,swo))

#### SRR
swoSR       <-as.FLSR(swo)
model(swoSR)<-bevholt()
swoSR       <-fmle(swoSR)
plot(   swoSR)
profile(swoSR)

#### BRPs
swoBRP<-FLBRP(swo,sr=swoSR)
computeRefpts(swoBRP)

swoBRP<-brp(swoBRP)

# Use F0.1 as fishing mortality target
F0.1<-refpts(swoBRP)["f0.1","harvest",drop=T]
#### bug
ctrl     <-fwdControl(data.frame(year    =2008:2011,
                                 val     =F0.1,
                                 quantity="f"))

ctrl     <-fwdControl(data.frame(year    =2008:2028,
                                 val     =F0.1,
                                 quantity="f"))

swoF1     <-fwd(swo20,ctrl=ctrl,sr=swoSR)
plot(swoF1)

ctrl     <-fwdControl(data.frame(
               year    =2008:2028,
               val     =F0.1*0.5,
               quantity="f"))
swoF2     <-fwd(swo20, ctrl=ctrl, sr=swoSR)

ctrl     <-fwdControl(data.frame(
               year    =2008:2028,
               val     =F0.1*2.0,
               quantity="f"))
swoF3     <-fwd(swo20, ctrl=ctrl, sr=swoSR)


## Create an FlStock object
swoF0.1<-FLStocks("F0.1"=swoF1,"half"=swoF2,"double"=swoF3)
plot(swoF0.1)

## Cut the plots
plot(lapply(swoF0.1,window,start=1990))

## Compare alternatives
lapply(lapply(swoF0.1,window,start=2008),computeCatch)

#### saving data
write.csv(model.frame(lapply(lapply(swoF0.1,window,start=2008),computeCatch)),
       file="\\\\192.168.100.101\\flr_2011\\FLR\\swoPrj.csv")

#### Total catch
lapply(lapply(lapply(swoF0.1,window,start=2008),computeCatch),sum)

#### Short-term
unlist(lapply(lapply(lapply(swoF0.1,window,start=2008,end=2013),computeCatch),sum))
#### Medium-term
unlist(lapply(lapply(lapply(swoF0.1,window,start=2016,end=2020),computeCatch),sum))
#### Long-term
unlist(lapply(lapply(lapply(swoF0.1,window,start=2023,end=2028),computeCatch),sum))

lngt<-function(x) sum(computeCatch(window(x,start=2020,end=2024)))
################################################################################


##### chunk 3 ##################################################################
### constant catch startegies
ctch<-mean(computeCatch(swo)[,ac(2003:2007)])

swoC<-FLStocks()
ctrl<-fwdControl(data.frame(year    =2008:2028,
                            val     =ctch,
                            quantity="catch"))
swoC[["1.0"]]     <-fwd(swo20,ctrl=ctrl,sr=swoSR)

ctrl     <-fwdControl(data.frame(year=2008:2028,val=0.5*ctch,quantity="catch"))
swoC[["0.5"]]     <-fwd(swo20,ctrl=ctrl,sr=swoSR)

ctrl     <-fwdControl(data.frame(year=2008:2028,val=1.5*ctch,quantity="catch"))
swoC[["1.5"]]     <-fwd(swo20,ctrl=ctrl,sr=swoSR)
plot(swoC)

#### compare startegies
plot(FLStocks(swoC[[1]],swoF0.1[[1]]))
################################################################################

##### chunk 4 ##################################################################
#### constant catch with an upper F bound
ctrl<-fwdControl(data.frame(year    =rep(2008:2028,each=2),
                            val     =rep(c(12000,NA),21),
                            max     =rep(c(NA,0.35),21),
                            quantity=rep(c("catch","f"),21)))
swoFC<-fwd(swo20,ctrl=ctrl,sr=swoSR)
plot(swoFC);fbar(swoFC);catch(swoFC)
################################################################################

##### chunk 5 ##################################################################
#### 5% F reduction
ctrl<-fwdControl(data.frame(year    =rep(2008:2028,each=2),
                            rel.year=c(t(array(c(2007:2027,rep(NA,21)),c(21,2)))),
                            val     =rep(c(0.95,NA),21),
                            min     =rep(c(NA,F0.1*.5),21),
                            quantity=rep(c("catch","f"),21)))
swoFC<-fwd(swo20,ctrl=ctrl,sr=swoSR)
plot(swoFC)
################################################################################

################################################################################

##### chunk 6 ##################################################################
#### 10% SSB increase
ctrl<-fwdControl(data.frame(year    =2008:2028,
                            rel.year=2008:2028,
                            min     =1.10,
                            quantity="ssb"))
swoSSB<-window(fwd(swo20,ctrl=ctrl,sr=swoSR),end=2027)
plot(swoSSB)
################################################################################

##### chunk 7 ##################################################################
hcrF<-function(iYr,SSB,Bpa,Blim,Fmin,Fmax){
    val <-pmin(Fmax,Fmax-(Fmax-Fmin)*(Bpa-SSB)/(Bpa-Blim))
    trgt<-fwdTarget(year=iYr+1,quantity="f",valueval)

    return(trgt)}
################################################################################

##### chunk 9 ##################################################################
### performs a constant F strategy to recover to an SSB target
data(ple4)

# Set up the stock for the next 6 years
pleProj <-stf(ple4,6)

# Set a constant recruitment based on the geometric mean of last 10 years
mnRec <- FLPar(exp(mean(log(rec(ple4)[,ac(1992:2001)]))))

# Set ssb target to level 19 years ago
ssbTarget <- ssb(ple4)[,"1992"]

## function to minimise
fn <-function(x,stk,ssbTarget,ctrl,sr){
       ctrl@target[,"val"]    <-x
       ctrl@trgtArray[,"val",]<-x

       ssb.<-c(ssb(fwd(stk,ctrl=ctrl,sr=sr))[,"2006"])

       return((ssb.-ssbTarget)^2)}

## Recover stock to BMY in 2006 with a constant F strategy
ctrl<-fwdControl(data.frame(year=2002:2006,val=.5,rel=2001,quantity="f"))

xmin<-optimize(fn, c(0.1, 1.0), tol = 0.0000001, stk=pleProj, ssbTarget=ssbTarget, ctrl=ctrl, sr=list(model="mean",params=mnRec))
ctrl<-fwdControl(data.frame(year=2002:2006,
                            val =xmin$minimum,
                            rel =2001,
                            quantity="f"))
pleProjF     <-fwd(pleProj,ctrl=ctrl,sr=list(model="mean", params=mnRec))

# update catch slot
catch(pleProjF) <- computeCatch(pleProjF)

# Have we reached the target?
ssbTarget
ssb(pleProjF)[,ac(2002:2006)]
# At what level of constant F
fbar(pleProjF)[,ac(2002:2006)]
# plot
plot(pleProjF[,ac(1957:2006)])
################################################################################


##### chunk 10 #################################################################
### performs a constant catch strategy to recover to an SSB target
data(ple4)
pleProj<-stf(ple4,6)

## Recover stock to the desired SSB in 2006 with a constant Catch strategy
# Here val can be anything in the ctrl because it is overwritten in the optimisation loop
ctrl<-fwdControl(data.frame(year=2002:2006,
                            val =c(catch(pleProj)[,"2001"]),
                            quantity="catch"))

xmin<-optimize(fn, c(100, 100000), tol = 0.0000001, stk=pleProj, ssbTarget=ssbTarget, ctrl=ctrl, sr=list(model="mean",params=mnRec))
ctrl<-fwdControl(data.frame(year=2002:2006,val=xmin$minimum,quantity="catch"))
pleProjC      <-fwd(pleProj,ctrl=ctrl,sr=list(model="mean", params=mnRec))

# Have we reached the target?
ssbTarget
ssb(pleProjC)[,ac(2002:2006)]
# At what level of constant catch
computeCatch(pleProjC)[,ac(2002:2006)]
# And at what level of F
fbar(pleProjC)[,ac(2002:2006)]
# Update the catch slot
catch(pleProjC) <- computeCatch(pleProjC)

plot(pleProjC[,ac(1957:2006)])
################################################################################

plot(lapply(FLStocks(pleProjC,pleProjF),
        window,start=2000,end=2006))


################################################################################

##### chunk 11 #################################################################
# Assessment upto and including 2001
data(ple4)
black.bird               <-stf(pleProj,nyear=2)

# set courtship and egg laying in Autumn
black.bird@m.spwn[]      <-0.66
black.bird@harvest.spwn[]<-0.66

# assessment is in year 2002, set catch constraint in 2002 and a first guess for F in 2003
ctrl          <-fwdControl(data.frame(year=2002:2003,val=c(85000,.5),quantity=c("catch","f")))
black.bird    <-fwd(black.bird, ctrl=ctrl, sr=list(model="mean", params=FLPar(25000)))

# HCR specifies F=0.1 if ssb<100000, F=0.5 if ssb>300000
# otherwise linear increase as SSB increases
min.ssb<-50000
max.ssb<-500000
min.f  <-0.1
max.f  <-0.5
 
# slope of HCR
a.    <-(max.f-min.f)/(max.ssb-min.ssb)
b.    <-min.f-a.*min.ssb

# plot of HCR
plot(c(0.0,min.ssb,max.ssb,max.ssb*2),c(min.f,min.f,max.f,max.f),type="l",ylim=c(0,max.f*1.25),xlim=c(0,max.ssb*2))

## find F through iteration
t.    <-999
i     <-0
while (abs(ctrl@target[2,"val"]-t.)>10e-6 & i<50)
   {
   t.<-ctrl@target[2,"val"]  ## save last val of F

   # calculate new F based on SSB last iter
   ctrl@target[2,"val"]    <-a.*c(ssb(black.bird)[,"2003"])+b.
   ctrl@trgtArray[2,"val",]<-a.*c(ssb(black.bird)[,"2003"])+b.
   black.bird<-fwd(black.bird, ctrl=ctrl, sr=list(model="mean", params=FLPar(25000)))

   # 'av a gander
   points(c(ssb(black.bird)[,"2003"]),c(ctrl@target[2,"val"]),cex=1.25,pch=19,col=i)
   print(c(ssb(black.bird)[,"2003"]))
   print(c(ctrl@target[2,"val"]))

   i<-i+1}

# F bounds
black.bird      <-fwd(black.bird, ctrl=ctrl, sr=list(model="mean",params=FLPar(25000)))
plot(FLStocks(black.bird))
################################################################################

################################################################################

##### chunk 12 #################################################################
#### Create a random variable for M
swoM   <-swoF1

m(swoM)<-rlnorm(100,log(m(swo)),.3)

hist(m(swoM))

harvest(swoM)<-computeHarvest(swoM)
catch(  swoM)<-computeCatch(  swoM,"all")

ctrl<-fwdControl(data.frame(year=2008:2028, val=c(fbar(swoF1)[,ac(2008:2028)]),quantity="f"))
swoM<-fwd(swoM,ctrl=ctrl,sr=swoSR)

plot(FLStocks(swoM,swoF1))
################################################################################


##### chunk 13 #################################################################
#### Create a random variable for M
swoM1	   <-swoM
m(swoM1)[1:3,]          <-m(swoM)[1:3,]*2

harvest(swoM1)<-computeHarvest(swoM1)
catch(  swoM1)<-computeCatch(  swoM1,"all")
swoM1         <-fwd(swoM1,ctrl=ctrl,sr=swoSR)

swoM2   <-swoM
m(swoM2)[,ac(2000:2028)]<-m(swoM)[,ac(2000:2028)]*2

harvest(swoM2)<-computeHarvest(swoM2)
catch(  swoM2)<-computeCatch(  swoM2,"all")
swoM2         <-fwd(swoM2,ctrl=ctrl,sr=swoSR)

plot(FLStocks(swoM,swoM1,swoM2))
################################################################################

##### chunk 14 #################################################################
#### process error in recruitment
srDev<-FLQuant(rlnorm(20*100,0.0,0.3),dimnames=list(year=2008:2028,iter=1:100))
swoSR<-fwd(swoM,ctrl=ctrl,sr=swoSR,sr.residuals=srDev)
plot(swoSR)
################################################################################

##### chunk 15 #################################################################
swoSR1<-as.FLSR(swo,model="bevholtSV")
swoSR1<-fmle(swoSR1,fixed=list(spr0=spr0(swo)))
swo1<-fwd(stf(swo,20),ctrl=ctrl,sr=ab(swoSR1),sr.residuals=srDev)

#### SRR regime shifts 30% reduction in virgin biomass
swoSR2<-swoSR1

params(swoSR2)["v"]<-params(swoSR2)["v"]*.7

swo2<-fwd(stf(swo,20),ctrl=ctrl,sr=ab(swoSR2),sr.residuals=srDev)

plot(FLStocks(swo1,swo2))
################################################################################

##### chunk 16 #################################################################
################################################################################

##### chunk 17 #################################################################
################################################################################

##### chunk 18 #################################################################
################################################################################

##### chunk 19 #################################################################
################################################################################

##### chunk 20 #################################################################
################################################################################
