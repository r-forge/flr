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
par(mfrow=c(3,4),mar=c(4,4,1,1))

#### Cod ###############################################################################################################
cod4SR<-FLlst()

cod4SR[["bevholtSV"]]<-as.FLSR(cod4[["ices"]],model=bevholtSV)
cod4SR[["rickerSV" ]]<-as.FLSR(cod4[["ices"]],model=rickerSV)
#cod4SR[["segreg"  ]]<-as.FLSR(cod4[["ices"]],model=segreg)
#cod4SR[["shepherd"]]<-as.FLSR(cod4[["ices"]],model=shepherd)

## Fit
cod4SR[["bevholtSV"]]<-fmle(cod4SR[["bevholtSV"]],fixed=list(spr0=spr0(cod4[[1]])),control=list(parscale=autoParscale(cod4SR[["bevholtSV"]])[1:2]))
cod4SR[[ "rickerSV"]]<-fmle(cod4SR[[ "rickerSV"]],fixed=list(spr0=spr0(cod4[[1]])),control=list(parscale=autoParscale(cod4SR[[ "rickerSV"]])[1:2]))
#cod4SR[["segreg"  ]]<-fmle(cod4SR[["segreg"  ]]) #, control=list(parscale=autoParscale(cod4SR[["segreg"  ]])))
#cod4SR[["shepherd"]]<-fmle(cod4SR[["shepherd"]], control=list(parscale=autoParscale(cod4SR[["shepherd"]])))

## Plotting
plotSR( cod4SR,maxSSB=500000,cols=rainbow(9))
plotSPR(cod4SR,maxSSB=500000,cols=rainbow(9))

## Likelihood profiling
for (i in names(cod4SR)){
  lprof(cod4SR[[i]],fixed=list(spr0=spr0(cod4[[1]])),.scaling=c("rel","rel"),s=seq(0.5,1.5,length.out=10),v=seq(0.5,1.5,length.out=10));mtext(i)}

#### Plaice  ###########################################################################################################
ple4SR<-FLlst()

ple4SR[["bevholtSV"]]<-as.FLSR(ple4[["ices"]],model=bevholtSV)
ple4SR[["rickerSV" ]]<-as.FLSR(ple4[["ices"]],model=rickerSV)
#ple4SR[["segreg"  ]]<-as.FLSR(ple4[["ices"]],model=segreg)
#ple4SR[["shepherd"]]<-as.FLSR(ple4[["ices"]],model=shepherd)

## Fit
ple4SR[["bevholtSV"]]<-fmle(ple4SR[["bevholtSV"]],fixed=list(spr0=spr0(ple4[[1]])),control=list(parscale=autoParscale(ple4SR[["bevholtSV"]])[1:2]))
ple4SR[[ "rickerSV"]]<-fmle(ple4SR[[ "rickerSV"]],fixed=list(spr0=spr0(ple4[[1]])),control=list(parscale=autoParscale(ple4SR[[ "rickerSV"]])[1:2]))
#ple4SR[["segreg"  ]]<-fmle(ple4SR[["segreg"  ]]) #, control=list(parscale=autoParscale(ple4SR[["segreg"  ]])))
#ple4SR[["shepherd"]]<-fmle(ple4SR[["shepherd"]], control=list(parscale=autoParscale(ple4SR[["shepherd"]])))

## Plotting
plotSR( ple4SR,maxSSB=500000,cols=rainbow(9))
plotSPR(ple4SR,maxSSB=500000,cols=rainbow(9))

## Likelihood profiling
for (i in names(ple4SR)){
  lprof(ple4SR[[i]],,fixed=list(spr0=spr0(ple4[[1]])),.scaling=c("rel","rel"),s=seq(0.5,1.5,length.out=10),v=seq(0.5,1.5,length.out=10));mtext(i)}

########################################################################################################################

#### Herring ###########################################################################################################
her4SR<-FLlst()

her4SR[["bevholtSV"]]<-as.FLSR(her4[["ices"]],model=bevholtSV)
her4SR[["rickerSV" ]]<-as.FLSR(her4[["ices"]],model=rickerSV)
#her4SR[["segreg"  ]]<-as.FLSR(her4[["ices"]],model=segreg)
#her4SR[["shepherd"]]<-as.FLSR(her4[["ices"]],model=shepherd)

## Fit
her4SR[["bevholtSV"]]<-fmle(her4SR[["bevholtSV"]],fixed=list(spr0=spr0(her4[[1]])),control=list(parscale=autoParscale(her4SR[["bevholtSV"]])[1:2]))
her4SR[[ "rickerSV"]]<-fmle(her4SR[[ "rickerSV"]],fixed=list(spr0=spr0(her4[[1]])),control=list(parscale=autoParscale(her4SR[[ "rickerSV"]])[1:2]))
#her4SR[["segreg"  ]]<-fmle(her4SR[["segreg"  ]]) #, control=list(parscale=autoParscale(her4SR[["segreg"  ]])))
#her4SR[["shepherd"]]<-fmle(her4SR[["shepherd"]], control=list(parscale=autoParscale(her4SR[["shepherd"]])))

## Plotting
plotSR( her4SR,maxSSB=5000000,cols=rainbow(9))
plotSPR(her4SR,maxSSB=5000000,cols=rainbow(9))

## Likelihood profiling
for (i in names(her4SR)){
  lprof(her4SR[[i]],fixed=list(spr0=spr0(her4[[1]])),.scaling=c("rel","rel"),s=seq(0.5,1.5,length.out=10),v=seq(0.5,1.5,length.out=10));mtext(i)}

savePlot(paste(myDir,"tests/FLSR/figs/chkModelOld", sep="/"))
########################################################################################################################

