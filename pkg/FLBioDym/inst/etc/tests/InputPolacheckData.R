################################################################################
#                                                                              #
#  Creates test data sets using data from Polacheck at al 1993                 #
#                                                                              #
################################################################################
library(FLBioDym)

myDir<-"C:\\Stuff\\FLR\\pkg\\FLBioDym"

#### catch & cpue
load(paste(myDir,"Data\\rocklob.RData",sep="\\"))
load(paste(myDir,"Data\\alb.RData",    sep="\\"))
load(paste(myDir,"Data\\namhke.RData", sep="\\"))

albBD<-FLBioDym(catch=FLQuant(alb[,"catch"],dimnames=list(year=alb[,"year"])),
                index=FLQuant(alb[,"index"],dimnames=list(year=alb[,"year"])))

lobBD<-FLBioDym(catch=FLQuant(rocklob[,"catch"],dimnames=list(year=rocklob[,"year"])),
                index=FLQuant(rocklob[,"index"],dimnames=list(year=rocklob[,"year"])))

hkeBD<-FLBioDym(catch=FLQuant(namhke[,"catch"],dimnames=list(year=namhke[,"year"])),
                index=FLQuant(namhke[,"index"],dimnames=list(year=namhke[,"year"])))

save(albBD,file=paste(myDir,"Data\\albBD.RData",sep="\\"))
save(lobBD,file=paste(myDir,"Data\\lobBD.RData",sep="\\"))
save(hkeBD,file=paste(myDir,"Data\\hkeBD.RData",sep="\\"))

rm(alb,rocklob,namhke)
                
