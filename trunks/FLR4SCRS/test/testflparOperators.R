library(FLCore)
library(ggplot2)

source("C:/Stuff/FLR/pkg/FLipper/R/flparOperators.R")
source("C:/Stuff/FLR/pkg/FLipper/R/ageLenWt.R")

#### shoudn´t this work?
params  <-FLPar(c(Linf=238.6,K=0.185,t0=-1.404),params=c("Linf","K","t0"),unit=c("male","female"),season=1:4,iter=1)
params

params  <-FLPar(array(c(Linf=238.6,K=0.185,t0=-1.404),c(3,2,4,1),list(params=c("Linf","K","t0"),unit=c("male","female"),season=1:4,iter=1)))
#### show seems wrong
params
params@.Data

#### doesn´t work
params["Linf","male",,]<-params["Linf","female",,]*.75
params@.Data["Linf","male",,]<-params@.Data["Linf","female",,]*.75

data(ple4)

dmns       <-dimnames(m(ple4))
dmns$season<-1:4
dmns$unit  <-c("male","female")
object     <-ages(FLQuant(c(m(ple4)),dimnames=dmns))
grw        <-as.data.frame(vonB(FLCohort(object),params))


ggplot(grw[!is.na(grw$data),])+geom_line(aes(age,data,group=paste(unit,season),col=paste(unit,season)))


