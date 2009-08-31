################################################################################
#                                                                              #
#  Creates test data sets using data from Polacheck at al 1993                 #
#                                                                              #
################################################################################
library(FLBioDym)

#### parameters
albPar<-c(0.328,  .2671,        239.6, 1.000,    19.65,    75.51,  0.315,    61.4*.002671,   0.111)
lobPar<-c(0.0659, .00002461, 129000,   1.000,  2133.74,  2115.0,   0.163,  1337.9*.0002461,  0.207)
hkePar<-c(0.379,  .0004360,    2772.6, 1.000,   263.2,   1646.3,   0.594,   435.5*.0004360,  0.124)

names(albPar)<-c("r","q","K","b0","msy","bnow","bnowK","emsy","sigma")
names(lobPar)<-c("r","q","K","b0","msy","bnow","bnowK","emsy","sigma")
names(hkePar)<-c("r","q","K","b0","msy","bnow","bnowK","emsy","sigma")

cf<-function(x){
   res<-c(NA,params(x)[c("q","K","b0","msy"),1,drop=T],
          stock(x)[,dims(x)$year,drop=T],
          stock(x)[,dims(x)$year,drop=T]/params(x)["K",1,drop=T],
          fmsy(x)[1,1,drop=T],
          params(x)[c("sigma"),1,drop=T])

  names(res)<-c("r","q","K","b0","msy","bnow","bnowK","emsy","sigma")

  return(res)
  }

albBD <-fit(albBD, fixed=albPar[c("K","msy")], model="fletcher")
lobBD <-fit(lobBD, fixed=lobPar[c("K","msy")], model="fletcher")
hkeBD <-fit(hkeBD, fixed=hkePar[c("K","msy")], model="fletcher")
albBD2<-fit(albBD, start=albPar[c("K","msy")], model="fletcher")
lobBD2<-fit(lobBD, star=lobPar[c("K","msy")], model="fletcher")
hkeBD2<-fit(hkeBD, start=hkePar[c("K","msy")], model="fletcher")

rbind(albPar,cf(albBD),cf(albBD2))[,c("r","K","b0","msy","emsy","bnow","bnowK","q","sigma")]
rbind(lobPar,cf(lobBD),cf(lobBD2))[,c("r","K","b0","msy","emsy","bnow","bnowK","q","sigma")]
rbind(hkePar,cf(hkeBD),cf(hkeBD2))[,c("r","K","b0","msy","emsy","bnow","bnowK","q","sigma")]

sum(residuals(albBD )^2)
sum(residuals(albBD2)^2)

sum(residuals(lobBD )^2)
sum(residuals(lobBD2)^2)

sum(residuals(hkeBD )^2)
sum(residuals(hkeBD2)^2)

plot(albBD)
plot(lobBD)
plot(hkeBD)

refpts(  albBD2)
refpts(  lobBD2)
refpts(  hkeBD2)

refptSE(  albBD2)
refptSE(  lobBD2)
refptSE(  hkeBD2)

albJK<-albBD
index(albJK)<-jacknife(index(albJK))
albJK<-fit(albJK,start=params(albJK)[c("r","K"),1,drop=T])
plot(albJK)

lobJK<-lobBD
index(lobJK)<-jacknife(index(lobJK))
lobJK<-fit(lobJK,start=params(lobJK)[c("r","K"),1,drop=T])
plot(lobJK)

hkeJK<-hkeBD
index(hkeJK)<-jacknife(index(hkeJK))
hkeJK<-fit(hkeJK,start=params(hkeJK)[c("r","K"),1,drop=T])
plot(hkeJK)

