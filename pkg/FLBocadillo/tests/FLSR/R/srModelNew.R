########################################################################################################################
## Test FLSR model specification                                                                                      ##
## New Style                                                                                                          ##
########################################################################################################################

library(FLCore)
library(FLBocadillo)

myDir<-"C:/Stuff/FLR/pkg/FLBocadillo"

load(paste(myDir,"data/cod4.RData", sep="/"))
load(paste(myDir,"data/ple4.RData", sep="/"))
load(paste(myDir,"data/her4.RData", sep="/"))

#source(paste(myDir,"R/lprof.R",    sep="/"))
#source(paste(myDir,"R/parscale.R", sep="/"))
#source(paste(myDir,"R/srModel.R",  sep="/"))

source(paste(myDir,"tests/FLSR/R/plotSRs.R",  sep="/"))

########################################################################################################################
# srModel()                                                                                                            #
########################################################################################################################
par(mfrow=c(3,7),mar=c(4,4,1,1))

#### Cod ###############################################################################################################
## Fit
cod4SR<-FLlst()

#### Cod ###############################################################################################################
cod4SR<-FLlst()

cod4SR[["bevholt"]]        <-as.FLSR(cod4[["ices"]])
cod4SR[["ricker"]]         <-as.FLSR(cod4[["ices"]])
cod4SR[["segreg"]]         <-as.FLSR(cod4[["ices"]])
cod4SR[["shepherd"]]       <-as.FLSR(cod4[["ices"]])
cod4SR[["cushing"]]        <-as.FLSR(cod4[["ices"]])

model(cod4SR[["bevholt"]]) <-srModel("bevholt")
model(cod4SR[["ricker"]])  <-srModel("ricker")
model(cod4SR[["segreg"]])  <-srModel("segreg")
model(cod4SR[["shepherd"]])<-srModel("shepherd")
model(cod4SR[["cushing"]]) <-srModel("cushing")

## Fit
cod4SR[["bevholt" ]]<-fmle(cod4SR[["bevholt" ]], control=list(parscale=autoParscale(cod4SR[["bevholt" ]])))
cod4SR[["ricker"  ]]<-fmle(cod4SR[["ricker"  ]])#, control=list(parscale=autoParscale(cod4SR[["ricker"  ]])))
cod4SR[["segreg"  ]]<-fmle(cod4SR[["segreg"  ]]) #, control=list(parscale=autoParscale(cod4SR[["segreg"  ]])))
cod4SR[["shepherd"]]<-fmle(cod4SR[["shepherd"]], control=list(parscale=autoParscale(cod4SR[["shepherd"]])))
cod4SR[["cushing"]] <-fmle(cod4SR[["cushing"]],  control=list(parscale=autoParscale(cod4SR[["cushing"]])))

## Plotting
plotSR( cod4SR,maxSSB=500000,cols=rainbow(9))
plotSPR(cod4SR,maxSSB=500000,cols=rainbow(9))

## Likelihood profiling
for (i in names(cod4SR)){
  lprof(cod4SR[[i]],scaling=c("rel","rel"),a =seq(0.5,1.5,length.out=10),b =seq(0.5,1.5,length.out=10));mtext(i)}

#### Plaice  ###########################################################################################################
ple4SR<-FLlst()

ple4SR[["bevholt"]]        <-as.FLSR(ple4[["ices"]])
ple4SR[["ricker"]]         <-as.FLSR(ple4[["ices"]])
ple4SR[["segreg"]]         <-as.FLSR(ple4[["ices"]])
ple4SR[["shepherd"]]       <-as.FLSR(ple4[["ices"]])
ple4SR[["cushing"]]        <-as.FLSR(ple4[["ices"]])

model(ple4SR[["bevholt"]]) <-srModel("bevholt")
model(ple4SR[["ricker"]])  <-srModel("ricker")
model(ple4SR[["segreg"]])  <-srModel("segreg")
model(ple4SR[["shepherd"]])<-srModel("shepherd")
model(ple4SR[["cushing"]]) <-srModel("cushing")

## Fit
ple4SR[["bevholt" ]]<-fmle(ple4SR[["bevholt" ]], control=list(parscale=autoParscale(ple4SR[["bevholt" ]])))
ple4SR[["ricker"  ]]<-fmle(ple4SR[["ricker"  ]], control=list(parscale=autoParscale(ple4SR[["ricker"  ]])))
ple4SR[["segreg"  ]]<-fmle(ple4SR[["segreg"  ]]) #, control=list(parscale=autoParscale(ple4SR[["segreg"  ]])))
ple4SR[["shepherd"]]<-fmle(ple4SR[["shepherd"]], control=list(parscale=autoParscale(ple4SR[["shepherd"]])))
ple4SR[["cushing"]] <-fmle(ple4SR[["cushing"]])#,  control=list(parscale=autoParscale(ple4SR[["cushing"]])))

## Plotting
plotSR( ple4SR,maxSSB=500000,cols=rainbow(9))
plotSPR(ple4SR,maxSSB=500000,cols=rainbow(9))

## Likelihood profiling
for (i in names(ple4SR)){
  lprof(ple4SR[[i]],scaling=c("rel","rel"),a =seq(0.5,1.5,length.out=10),b =seq(0.5,1.5,length.out=10));mtext(i)}
########################################################################################################################

#### Herring ###########################################################################################################
her4SR<-FLlst()

her4SR[["bevholt"]]        <-as.FLSR(her4[["ices"]])
her4SR[["ricker"]]         <-as.FLSR(her4[["ices"]])
her4SR[["ricker"]]         <-transform(her4SR[["ricker"]],rec=rec/1000,ssb=ssb/1000)
her4SR[["segreg"]]         <-as.FLSR(her4[["ices"]])
her4SR[["shepherd"]]       <-as.FLSR(her4[["ices"]])
her4SR[["cushing"]]        <-as.FLSR(her4[["ices"]])

model(her4SR[["bevholt"]]) <-srModel("bevholt")
model(her4SR[["ricker"]])  <-srModel("ricker")
model(her4SR[["segreg"]])  <-srModel("segreg")
model(her4SR[["shepherd"]])<-srModel("shepherd")
model(her4SR[["cushing"]]) <-srModel("cushing")

## Fit
her4SR[["bevholt" ]]<-fmle(her4SR[["bevholt" ]], control=list(parscale=autoParscale(her4SR[["bevholt" ]])))
her4SR[["ricker"  ]]<-fmle(her4SR[["ricker"  ]])#, control=list(parscale=autoParscale(her4SR[["ricker"  ]])))
her4SR[["segreg"  ]]<-fmle(her4SR[["segreg"  ]]) #, control=list(parscale=autoParscale(her4SR[["segreg"  ]])))
her4SR[["shepherd"]]<-fmle(her4SR[["shepherd"]], control=list(parscale=autoParscale(her4SR[["shepherd"]])))
her4SR[["cushing"]] <-fmle(her4SR[["cushing"]],  control=list(parscale=autoParscale(her4SR[["cushing"]])))

## Plotting
plotSR( her4SR,maxSSB=5000000,cols=rainbow(9))
plotSPR(her4SR,maxSSB=5000000,cols=rainbow(9))

## Likelihood profiling
for (i in names(her4SR)){
  lprof(her4SR[[i]],scaling=c("rel","rel"),a =seq(0.5,1.5,length.out=10),b =seq(0.5,1.5,length.out=10));mtext(i)}

savePlot(paste(myDir,"tests/FLSR/figs/chkModelNew", sep="/"))
########################################################################################################################

