########################################################################################################################
## Test FLSR model specification                                                                                      ##
## Old style                                                                                                          ##
########################################################################################################################

library(FLCore)
library(FLBocadillo)
library(pkpkg)

myDir<-"C:/Stuff/FLR/pkg/FLBocadillo"

load(paste(myDir,"tests/FLSR/data/cod4.RData", sep="/"))
load(paste(myDir,"tests/FLSR/data/ple4.RData", sep="/"))
load(paste(myDir,"tests/FLSR/data/her4.RData", sep="/"))

source(paste(myDir,"tests/FLSR/R/plotSRs.R",  sep="/"))

########################################################################################################################
## Old model()                                                                                                        ##
########################################################################################################################
par(mfrow=c(3,6),mar=c(4,4,1,1))

#### Cod ###############################################################################################################
cod4SR<-FLlst()

cod4SR[["segreg"  ]]<-as.FLSR(cod4[["ices"]],model=segreg)
cod4SR[["bevholt" ]]<-as.FLSR(cod4[["ices"]],model=bevholt)
cod4SR[["ricker"  ]]<-as.FLSR(cod4[["ices"]],model=ricker)
cod4SR[["shepherd"]]<-as.FLSR(cod4[["ices"]],model=shepherd)

## Fit
cod4SR[["segreg"  ]]<-fmle(cod4SR[["segreg"  ]]) #, control=list(parscale=autoParscale(cod4SR[["segreg"  ]])))
cod4SR[["bevholt" ]]<-fmle(cod4SR[["bevholt" ]], control=list(parscale=autoParscale(cod4SR[["bevholt" ]])))
cod4SR[["ricker"  ]]<-fmle(cod4SR[["ricker"  ]]) #, control=list(parscale=autoParscale(cod4SR[["ricker"  ]])))
cod4SR[["shepherd"]]<-fmle(cod4SR[["shepherd"]], control=list(parscale=autoParscale(cod4SR[["shepherd"]])))

## Plotting
plotSR( cod4SR,maxSSB=500000,cols=rainbow(9))
plotSPR(cod4SR,maxSSB=500000,cols=rainbow(9))

## Likelihood profiling
for (i in names(cod4SR)){
  lprof(cod4SR[[i]],scaling=c("rel","rel"),a =seq(0.5,1.5,length.out=10),b =seq(0.5,1.5,length.out=10));mtext(i)}

#### Plaice  ###########################################################################################################
ple4SR<-FLlst()

ple4SR[["segreg"  ]]<-as.FLSR(ple4[["ices"]],model=segreg)
ple4SR[["bevholt" ]]<-as.FLSR(ple4[["ices"]],model=bevholt)
ple4SR[["ricker"  ]]<-as.FLSR(ple4[["ices"]],model=ricker)
ple4SR[["shepherd"]]<-as.FLSR(ple4[["ices"]],model=shepherd)

## Fit
ple4SR[["segreg"  ]]<-fmle(ple4SR[["segreg"  ]]) #, control=list(parscale=autoParscale(ple4SR[["segreg"  ]])))
ple4SR[["bevholt" ]]<-fmle(ple4SR[["bevholt" ]], control=list(parscale=autoParscale(ple4SR[["bevholt" ]])))
ple4SR[["ricker"  ]]<-fmle(ple4SR[["ricker"  ]]) #, control=list(parscale=autoParscale(ple4SR[["ricker"  ]])))
ple4SR[["shepherd"]]<-fmle(ple4SR[["shepherd"]], control=list(parscale=autoParscale(ple4SR[["shepherd"]])))

## Plotting
plotSR( ple4SR,maxSSB=500000,cols=rainbow(9))
plotSPR(ple4SR,maxSSB=500000,cols=rainbow(9))

## Likelihood profiling
for (i in names(ple4SR)){
  lprof(ple4SR[[i]],scaling=c("rel","rel"),a =seq(0.5,1.5,length.out=10),b =seq(0.5,1.5,length.out=10));mtext(i)}
########################################################################################################################

#### Herring ###########################################################################################################
her4SR<-FLlst()

her4SR[["segreg"  ]]<-as.FLSR(her4[["ices"]],model=segreg)
her4SR[["bevholt" ]]<-as.FLSR(her4[["ices"]],model=bevholt)
her4SR[["ricker"  ]]<-as.FLSR(her4[["ices"]],model=ricker)
her4SR[["shepherd"]]<-as.FLSR(her4[["ices"]],model=shepherd)

## Fit
her4SR[["segreg"  ]]<-fmle(her4SR[["segreg"  ]]) #, control=list(parscale=autoParscale(her4SR[["segreg"  ]])))
her4SR[["bevholt" ]]<-fmle(her4SR[["bevholt" ]], control=list(parscale=autoParscale(her4SR[["bevholt" ]])))
her4SR[["ricker"  ]]<-fmle(her4SR[["ricker"  ]], control=list(parscale=autoParscale(her4SR[["ricker"  ]])))
her4SR[["shepherd"]]<-fmle(her4SR[["shepherd"]], control=list(parscale=autoParscale(her4SR[["shepherd"]])))

## Plotting
plotSR( her4SR,maxSSB=5000000,cols=rainbow(9))
plotSPR(her4SR,maxSSB=5000000,cols=rainbow(9))

## Likelihood profiling
for (i in names(her4SR)){
  lprof(her4SR[[i]],scaling=c("rel","rel"),a =seq(0.5,1.5,length.out=10),b =seq(0.5,1.5,length.out=10));mtext(i)}

savePlot(paste(myDir,"tests/FLSR/figs/chkModelOld", sep="/"))
########################################################################################################################

