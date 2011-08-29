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
   res<-c(params(x)[c("r","q","K","b0"),1,drop=T],
          msy(x)[1,1,drop=T],
          stock(x)[,dims(x)$year,drop=T],
          stock(x)[,dims(x)$year,drop=T]/params(x)["K",1,drop=T],
          fmsy(x)[1,1,drop=T],
          params(x)[c("sigma"),1,drop=T])

  names(res)<-c("r","q","K","b0","msy","bnow","bnowK","emsy","sigma")

  return(res)
  }

albBD <-fit(albBD, fixed=albPar[c("r","K")], model="fox")
rocklobBD <-fit(rocklobBD, fixed=lobPar[c("r","K")], model="fox")
namhkeBD <-fit(namhkeBD, fixed=hkePar[c("r","K")], model="fox")
albBD2<-fit(albBD, start=c("r"=0.5,"K"= 2000), model="fox")
rocklobBD2<-fit(rocklobBD, start=c("r"=1.5,"K"=50000), model="fox")
namhkeBD2<-fit(namhkeBD, start=c("r"=1.5,"K"=50000), model="fox")

rbind(albPar,cf(albBD),cf(albBD2))[,c("r","K","b0","msy","emsy","bnow","bnowK","q","sigma")]
rbind(lobPar,cf(rocklobBD),cf(rocklobBD2))[,c("r","K","b0","msy","emsy","bnow","bnowK","q","sigma")]
rbind(hkePar,cf(namhkeBD),cf(namhkeBD2))[,c("r","K","b0","msy","emsy","bnow","bnowK","q","sigma")]

sum(residuals(albBD )^2)
sum(residuals(albBD2)^2)

sum(residuals(rocklobBD )^2)
sum(residuals(rocklobBD2)^2)

sum(residuals(namhkeBD )^2)
sum(residuals(namhkeBD2)^2)

plot(albBD)
plot(rocklobBD)
plot(namhkeBD)

refpts(  albBD2)
refpts(  rocklobBD2)
refpts(  namhkeBD2)

refptSE(  albBD2)
refptSE(  rocklobBD2)
refptSE(  namhkeBD2)

albJK<-albBD
index(albJK)<-jacknife(index(albJK))
albJK<-fit(albJK,start=params(albJK)[c("r","K"),1,drop=T])
plot(albJK)

lobJK<-rocklobBD
index(lobJK)<-jacknife(index(lobJK))
lobJK<-fit(lobJK,start=params(lobJK)[c("r","K"),1,drop=T])
plot(lobJK)

hkeJK<-namhkeBD
index(hkeJK)<-jacknife(index(hkeJK))
hkeJK<-fit(hkeJK,start=params(hkeJK)[c("r","K"),1,drop=T])
plot(hkeJK)

