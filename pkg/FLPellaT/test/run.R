library(FLPellaT)
source("C:/Stuff/FLR/pkg/FLPellaT/R/class.R")
source("C:/Stuff/FLR/pkg/FLPellaT/R/constructors.R")
source("C:/Stuff/FLR/pkg/FLPellaT/R/coerce.R")
source("C:/Stuff/FLR/pkg/FLPellaT/R/createAccessors.R")
source("C:/Stuff/FLR/pkg/FLPellaT/R/methods.R")
source("C:/Stuff/FLR/pkg/FLPellaT/R/plot.R")

################################################################################
## Biomass Dynamic Model in R                                                 ##
################################################################################

pt.<-fit(pt)

plot(pt)
plot(pt,type="diag")
