################################################################################
## Biomass Dynamic Model in R                                                 ##
################################################################################

library(FLPellaT)
source("C:/Stuff/FLR/pkg/FLPellaT/R/class.R")
source("C:/Stuff/FLR/pkg/FLPellaT/R/constructors.R")
source("C:/Stuff/FLR/pkg/FLPellaT/R/coerce.R")
source("C:/Stuff/FLR/pkg/FLPellaT/R/createAccessors.R")
source("C:/Stuff/FLR/pkg/FLPellaT/R/methods.R")
source("C:/Stuff/FLR/pkg/FLPellaT/R/plot.R")
source("C:/Stuff/FLR/pkg/FLPellaT/R/plotDiagnostics.R")

pt<-fit(pt)
plot(pt)
plot(pt,type="diag")
plot(pt,type="equil")

pt.<-pt
index(pt.)<-index(pt.)[,3:30]
pt.<-fit(pt.)

res<-rep(NA,100)
r.<-seq(0.1, 1, length.out=100)
for (i in 1:100)
   res[i]<-fit(pt,fix=c(r=r.[i]),start=c(K=10000))@LL[1]

plot(res~r.)