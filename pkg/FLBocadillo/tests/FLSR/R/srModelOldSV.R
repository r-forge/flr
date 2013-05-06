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
par(mfrow=c(3,5),mar=c(4,4,1,1))

#### Cod ###############################################################################################################
cod4SR<-FLlst()
cod4SR[["bevholtSV"]] <-as.FLSR(cod4[["ices"]],model=bevholtSV)
cod4SR[["rickerSV" ]] <-as.FLSR(cod4[["ices"]],model=rickerSV)
cod4SR[["segreg"  ]]  <-as.FLSR(cod4[["ices"]],model=segreg)
cod4SR[["shepherdSV"]]<-as.FLSR(cod4[["ices"]],model=shepherdSV)
cod4SR[["shepherdD"]] <-as.FLSR(cod4[["ices"]],model=shepherdD)

## Fit
cod4SR[["bevholtSV"]] <-fmle(cod4SR[["bevholtSV"]], fixed=list(spr0=spr0(cod4[[1]])),control=list(parscale=autoParscale(cod4SR[["bevholtSV"]])[1:2]))
cod4SR[["rickerSV"]]  <-fmle(cod4SR[["rickerSV"]],  fixed=list(spr0=spr0(cod4[[1]])),control=list(parscale=autoParscale(cod4SR[[ "rickerSV"]])[1:2]))
cod4SR[["segreg"]]    <-fmle(cod4SR[["segreg"  ]]) #, control=list(parscale=autoParscale(cod4SR[["segreg"  ]])))
cod4SR[["shepherdSV"]]<-fmle(cod4SR[["shepherdSV"]],fixed=list(spr0=spr0(cod4[[1]])),control=list(parscale=autoParscale(cod4SR[["shepherdSV"]])[1:3]))
cod4SR[["shepherdD"]] <-fmle(cod4SR[["shepherdD"]],                                  control=list(parscale=autoParscale(cod4SR[["shepherdD"]])))

residuals(cod4SR[["bevholtSV"]]) <-log(rec(cod4SR[["bevholtSV"]]) /fitted(cod4SR[["bevholtSV"]]))
residuals(cod4SR[["rickerSV"]])  <-log(rec(cod4SR[["rickerSV"]])  /fitted(cod4SR[["rickerSV"]]))
residuals(cod4SR[["segreg"]])    <-log(rec(cod4SR[["segreg"]])    /fitted(cod4SR[["segreg"]]))
residuals(cod4SR[["shepherdSV"]])<-log(rec(cod4SR[["shepherdSV"]])/fitted(cod4SR[["shepherdSV"]]))
residuals(cod4SR[["shepherdD"]]) <-log(rec(cod4SR[["shepherdD"]]) /fitted(cod4SR[["shepherdD"]]))

## Plotting
plotSR( cod4SR,maxSSB=500000,cols=rainbow(9))
plotSPR(cod4SR,maxSSB=500000,cols=rainbow(9))
lprof(cod4SR[["bevholtSV"]], fixed=list(spr0=spr0(cod4[[1]])),.scaling=c("rel","rel"),s=seq(0.5,1.5,length.out=10),v=seq(0.5,1.5,length.out=10),xlab="Steepness",ylab="Virgin Biomass");mtext("Beverton & Holt")
lprof(cod4SR[["rickerSV"]],  fixed=list(spr0=spr0(cod4[[1]])),.scaling=c("rel","rel"),s=seq(0.5,1.5,length.out=10),v=seq(0.5,1.5,length.out=10),xlab="Steepness",ylab="Virgin Biomass");mtext("Ricker")
lprof(cod4SR[["shepherdSV"]],fixed=list(spr0=spr0(cod4[[1]])),.scaling=c("rel","rel"),s=seq(0.5,1.5,length.out=10),v=seq(0.5,1.5,length.out=10),xlab="Steepness",ylab="Virgin Biomass");mtext("Shepherd")
########################################################################################################################

#### Plaice  ###########################################################################################################
ple4SR<-FLlst()
ple4SR[["bevholtSV"]] <-as.FLSR(ple4[["ices"]],model=bevholtSV)
ple4SR[["rickerSV" ]] <-as.FLSR(ple4[["ices"]],model=rickerSV)
ple4SR[["segreg"  ]]  <-as.FLSR(ple4[["ices"]],model=segreg)
ple4SR[["shepherdSV"]]<-as.FLSR(ple4[["ices"]],model=shepherdSV)
ple4SR[["shepherdD"]] <-as.FLSR(ple4[["ices"]],model=shepherdD)

## Fit
ple4SR[["bevholtSV"]] <-fmle(ple4SR[["bevholtSV"]], fixed=list(spr0=spr0(ple4[[1]])),control=list(parscale=autoParscale(ple4SR[["bevholtSV"]])[1:2]))
ple4SR[["rickerSV" ]] <-fmle(ple4SR[["rickerSV"]],  fixed=list(spr0=spr0(ple4[[1]])),control=list(parscale=autoParscale(ple4SR[[ "rickerSV"]])[1:2]))
ple4SR[["segreg"]]    <-fmle(ple4SR[["segreg"  ]]) #, control=list(parscale=autoParscale(ple4SR[["segreg"  ]])))
ple4SR[["shepherdSV"]]<-fmle(ple4SR[["shepherdSV"]],fixed=list(spr0=spr0(ple4[[1]])),control=list(parscale=autoParscale(ple4SR[["shepherdSV"]])[1:3]))
ple4SR[["shepherdD"]] <-fmle(ple4SR[["shepherdD"]],                                  control=list(parscale=autoParscale(ple4SR[["shepherdD"]])))

residuals(ple4SR[["bevholtSV"]]) <-log(rec(ple4SR[["bevholtSV"]]) /fitted(ple4SR[["bevholtSV"]]))
residuals(ple4SR[["rickerSV"]])  <-log(rec(ple4SR[["rickerSV"]])  /fitted(ple4SR[["rickerSV"]]))
residuals(ple4SR[["segreg"]])    <-log(rec(ple4SR[["segreg"]])    /fitted(ple4SR[["segreg"]]))
residuals(ple4SR[["shepherdSV"]])<-log(rec(ple4SR[["shepherdSV"]])/fitted(ple4SR[["shepherdSV"]]))
residuals(ple4SR[["shepherdD"]]) <-log(rec(ple4SR[["shepherdD"]]) /fitted(ple4SR[["shepherdD"]]))

## Plotting
plotSR( ple4SR,maxSSB=500000,cols=rainbow(9))
plotSPR(ple4SR,maxSSB=500000,cols=rainbow(9))
lprof(ple4SR[["bevholtSV"]], fixed=list(spr0=spr0(ple4[[1]])),.scaling=c("rel","rel"),s=seq(0.5,1.5, length.out=10),v=seq(0.5,1.5,length.out=10),xlab="Steepness",ylab="Virgin Biomass");mtext("Beverton & Holt")
lprof(ple4SR[["rickerSV"]],  fixed=list(spr0=spr0(ple4[[1]])),.scaling=c("rel","rel"),s=seq(0.5,1.5, length.out=10),v=seq(0.5,1.5,length.out=10),xlab="Steepness",ylab="Virgin Biomass");mtext("Ricker")
lprof(ple4SR[["shepherdSV"]],fixed=list(spr0=spr0(ple4[[1]])),.scaling=c("abs","rel"),s=seq(0.4,0.99,length.out=10),v=seq(0.5,1.5,length.out=10),xlab="Steepness",ylab="Virgin Biomass");mtext("Shepherd")
########################################################################################################################

#### Herring ###########################################################################################################
her4SR<-FLlst()
her4SR[["bevholtSV"]] <-as.FLSR(her4[["ices"]],model=bevholtSV)
her4SR[["rickerSV" ]] <-as.FLSR(her4[["ices"]],model=rickerSV)
her4SR[["segreg"  ]]  <-as.FLSR(her4[["ices"]],model=segreg)
her4SR[["shepherdSV"]]<-as.FLSR(her4[["ices"]],model=shepherdSV)
her4SR[["shepherdD"]] <-as.FLSR(her4[["ices"]],model=shepherdD)

## Fit
her4SR[["bevholtSV"]] <-fmle(her4SR[["bevholtSV"]], fixed=list(spr0=spr0(her4[[1]])),control=list(parscale=autoParscale(her4SR[["bevholtSV"]])[1:2]))
her4SR[["rickerSV"]]  <-fmle(her4SR[["rickerSV"]],  fixed=list(spr0=spr0(her4[[1]])),control=list(parscale=autoParscale(her4SR[[ "rickerSV"]])[1:2]))
her4SR[["segreg"]]    <-fmle(her4SR[["segreg"  ]]) #, control=list(parscale=autoParscale(her4SR[["segreg"  ]])))
her4SR[["shepherdSV"]]<-fmle(her4SR[["shepherdSV"]],fixed=list(spr0=spr0(her4[[1]])),control=list(parscale=autoParscale(her4SR[["shepherdSV"]])[1:3]))
her4SR[["shepherdD"]] <-fmle(her4SR[["shepherdD"]],                                  control=list(parscale=autoParscale(her4SR[["shepherdD"]])))

residuals(her4SR[["bevholtSV"]]) <-log(rec(her4SR[["bevholtSV"]]) /fitted(her4SR[["bevholtSV"]]))
residuals(her4SR[["rickerSV"]])  <-log(rec(her4SR[["rickerSV"]])  /fitted(her4SR[["rickerSV"]]))
residuals(her4SR[["segreg"]])    <-log(rec(her4SR[["segreg"]])    /fitted(her4SR[["segreg"]]))
residuals(her4SR[["shepherdSV"]])<-log(rec(her4SR[["shepherdSV"]])/fitted(her4SR[["shepherdSV"]]))
residuals(her4SR[["shepherdD"]]) <-log(rec(her4SR[["shepherdD"]]) /fitted(her4SR[["shepherdD"]]))

## Plotting
plotSR( her4SR,maxSSB=5000000,cols=rainbow(9))
plotSPR(her4SR,maxSSB=5000000,cols=rainbow(9))
lprof(her4SR[["bevholtSV"]], fixed=list(spr0=spr0(her4[[1]])),.scaling=c("rel","rel"),s=seq(0.5,1.5,length.out=10),v=seq(0.5,1.5,length.out=10),xlab="Steepness",ylab="Virgin Biomass");mtext("Beverton & Holt")
lprof(her4SR[["rickerSV"]],  fixed=list(spr0=spr0(her4[[1]])),.scaling=c("rel","rel"),s=seq(0.5,1.5,length.out=10),v=seq(0.5,1.5,length.out=10),xlab="Steepness",ylab="Virgin Biomass");mtext("Ricker")
lprof(her4SR[["shepherdSV"]],fixed=list(spr0=spr0(her4[[1]])),.scaling=c("rel","rel"),s=seq(0.5,1.5,length.out=10),v=seq(0.5,1.5,length.out=10),xlab="Steepness",ylab="Virgin Biomass");mtext("Shepherd")

savePlot(paste(myDir,"tests/FLSR/figs/chkModelOld", sep="/"))
########################################################################################################################

