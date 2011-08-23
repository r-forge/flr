#### Examples of forward projections using FLassh::fwd #########################
library(FLAssess)
library(FLash)
library(FLBRP)

#### North Atlantic Albacore
load("C:\\Stuff\\FLR\\flr4mse\\data\\alb.RData")

#### Set up a short term forecast for an FLStock object by adding extra years
## The default forecast is 3 years,
alb3<-stf(albNEA)

## Check what´s happened
summary(albNEA)
summary(alb3)

##### Reference points & SRR ###################################################
#### SRR
albSR       <-as.FLSR(albNEA)
model(albSR)<-bevholt()
albSR       <-fmle(albSR)

#### BRPs
albBRP<-FLBRP(albNEA,sr=albSR)
computeRefpts(albBRP)
albBRP<-brp(albBRP)


##### Projections ##############################################################
## by default future F is the mean of last 3 years
mean(fbar(albNEA)[,ac(2008-(0:2))])
fbar(alb3)[,ac(2008+(1:3))]

# Do a constant F Projection for a 20 year projection
alb20<-stf(albNEA,nyear=20)


# Use F0.1 as fishing mortality target
F0.1<-refpts(albBRP)["f0.1","harvest",drop=T]

#### F0.1
ctrl<-fwdControl(data.frame(year    =2009:2028,
                            val     =F0.1,
                            quantity="f"))
albF1<-fwd(alb20,ctrl=ctrl,sr=albSR)
plot(albF1)

#### F0.1*0.5
ctrl     <-fwdControl(data.frame(year    =2009:2028,
                                 val     =F0.1*0.5,
                                 quantity="f"))
albF2     <-fwd(alb20, ctrl=ctrl, sr=albSR)

#### F0.1*2.0
ctrl     <-fwdControl(data.frame(year    =2009:2028,
                                 val     =F0.1*2.0,
                                 quantity="f"))
albF3     <-fwd(alb20, ctrl=ctrl, sr=albSR)


## Create an FlStocks object
albF0.1<-FLStocks("F0.1"=albF1,"half"=albF2,"double"=albF3)
plot(albF0.1)

## Cut the plots
plot(lapply(albF0.1,window,start=1990))


## Compare alternatives ########################################################
lapply(lapply(albF0.1,window,start=2009),computeCatch)

#### Total catch
lapply(lapply(lapply(albF0.1,window,start=2009),computeCatch),sum)

#### Short-term
unlist(lapply(lapply(lapply(albF0.1,window,start=2009,end=2014),computeCatch),sum))
#### Medium-term
unlist(lapply(lapply(lapply(albF0.1,window,start=2017,end=2021),computeCatch),sum))
#### Long-term
unlist(lapply(lapply(lapply(albF0.1,window,start=2024,end=2028),computeCatch),sum))


#### constant catch startegies #################################################
ctch<-mean(computeCatch(albNEA)[,ac(2004:2008)])

albC<-FLStocks()
ctrl<-fwdControl(data.frame(year=2009:2028,val=ctch,quantity="catch"))
albC[["1.0"]]     <-fwd(alb20,ctrl=ctrl,sr=albSR)

ctrl     <-fwdControl(data.frame(year=2009:2028,val=0.5*ctch,quantity="catch"))
albC[["0.5"]]     <-fwd(alb20,ctrl=ctrl,sr=albSR)

ctrl     <-fwdControl(data.frame(year=2009:2028,val=1.5*ctch,quantity="catch"))
albC[["1.5"]]     <-fwd(alb20,ctrl=ctrl,sr=albSR)
plot(albC)

#### compare startegies ########################################################
plot(FLStocks(albC[[1]],albF0.1[[1]]))