################################################################################
#                                                                              #
#  Creates test data sets using data from Polacheck at al 1993                 #
#                                                                              #
################################################################################

myDir<-"C:\\Stuff\\FLR\\pkg\\FLBioDym"

#### parameters
albPar<-c(0.328,26.71,  239.6,  19.65,1.000,  75.51,0.315,  61.4,0.111)
lobPar<-c(0.0659,2.461, 129000,2133.74,1.000,  21.15,0.163,1337.9,0.207)
hkePar<-c(0.379, 4.360,2772.6, 263.2, 1.000,1646.3, 0.594, 435.5,0.124)

names(albPar)<-c("r","q","K","msy","b0","bnow","bnowK","emsy","sigma")
names(lobPar)<-c("r","q","K","msy","b0","bnow","bnowK","emsy","sigma")
names(hkePar)<-c("r","q","K","msy","b0","bnow","bnowK","emsy","sigma")

#### catch & cpue
load(paste(myDir,"Data\\rocklob.RData",sep="\\"))
load(paste(myDir,"Data\\alb.RData",    sep="\\"))
load(paste(myDir,"Data\\namhke.RData", sep="\\"))

albSP<-FLBioDym(catch=FLQuant(alb[,"catch"],dimnames=list(year=alb[,"year"])),
                index=FLQuant(alb[,"index"],dimnames=list(year=alb[,"year"])))
albSP<-fwd(albSP,catch=catch(albSP),par=albPar)

lobSP<-FLBioDym(stock=FLQuant(lobPar["K"],      dimnames=list(year=rocklob[,"year"])),
                catch=FLQuant(rocklob[,"catch"],dimnames=list(year=rocklob[,"year"])),
                index=FLQuant(rocklob[,"index"],dimnames=list(year=rocklob[,"year"])))
lobSP<-fwd(lobSP,catch=catch(lobSP),par=lobPar)

hkeSP<-FLBioDym(stock=FLQuant(hkePar["K"],     dimnames=list(year=namhke[,"year"])),
                catch=FLQuant(namhke[,"catch"],dimnames=list(year=namhke[,"year"])),
                index=FLQuant(namhke[,"index"],dimnames=list(year=namhke[,"year"])))
hkeSP<-fwd(hkeSP,catch=catch(hkeSP),par=hkePar)

save(albSP,albPar,file=paste(myDir,"Data\\albSP.RData",sep="\\"))
save(lobSP,lobSP, file=paste(myDir,"Data\\lobSP.RData",sep="\\"))
save(hkeSP,hkeSP, file=paste(myDir,"Data\\hkeSP.RData",sep="\\"))

rm(alb,rocklob,namhke)
                
albSP<-fit(albSP,fix=albPar[c("r","K")])
sum(residuals(albSP)^2)
albSP<-fit(albSP)
sum(residuals(albSP)^2)

lobSP<-fit(lobSP,fix=lobPar[c("r","K")])
sum(residuals(lobSP)^2)
lobSP<-fit(lobSP,start=lobPar[c("r","K")])
sum(residuals(lobSP)^2)

hkeSP<-fit(hkeSP,fix=hkePar[c("r","K")])
sum(residuals(hkeSP)^2)
hkeSP<-fit(hkeSP)
sum(residuals(hkeSP)^2)

plot(albSP);params(albSP)
plot(lobSP);params(lobSP)
plot(hkeSP);params(hkeSP)
