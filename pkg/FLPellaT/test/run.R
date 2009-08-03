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

test.sp <- FLSP(catch = catch(pt), index = index(pt))
model(test.sp) <- pellatom()

test.sp@mpar <- 2 # Schaeffer?

KGuess<-mean(test.sp@catch)*100
test.sp@delta <- KGuess

# unscaled fmle
test.sp.o <- fmle(test.sp,start=list(r=0.5,K=KGuess,Q=1,sigma2=0.1))
plot(test.sp.o)
# meh...

# try with scaled fmle
test.sp.sc <- fmle_sc(test.sp,start=list(r=0.5,K=KGuess,Q=1,sigma2=0.1),lower=c(r=0,K=0,Q=0,sigma2=0),upper=c(r=1,K=KGuess*100,Q=100,sigma2=10))
params(test.sp.sc)
plot(test.sp.sc)
# Looks good!
