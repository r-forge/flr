library(FLCore)
library(FLash)
source("C:/Stuff/FLR/pkg/FLBioDym/R/class.R")
source("C:/Stuff/FLR/pkg/FLBioDym/R/class.R")
source("C:/Stuff/FLR/pkg/FLBioDym/R/constructors.R")
source("C:/Stuff/FLR/pkg/FLBioDym/R/createAccessors.R")
source("C:/Stuff/FLR/pkg/FLBioDym/R/coerce.R")
source("C:/Stuff/FLR/pkg/FLBioDym/R/methods.R")
source("C:/Stuff/FLR/pkg/FLBioDym/R/sp.R")
source("C:/Stuff/FLR/pkg/FLBioDym/R/fwd.R")
source("C:/Stuff/FLR/pkg/FLBioDym/R/msy.R")
source("C:/Stuff/FLR/pkg/FLBioDym/R/pars.R")

###############################################################################
#                                                                              #
#  Tests production functions and reference points                             #
#                                                                              #
################################################################################

# start test
#setCon()
#zz <- startTest("fwdFLStock.txt")
#tagTest("fwd(FLStock) testing ...")
#checkRun(
#checkRun(

#### Test Data
r.  <-0.5
K   <-100
p   <-2
##### Pella Tomlinson ##########################################################
## Plot Surplus Production and trajectories for constant catch & F             #
################################################################################

## increase F from 0 to 2*FMSY over 50 years
harvest<-FLQuant(seq(0,fmsy("pellat",r=r.,K=K,p=2)*4,length.out=50))
bio    <-FLQuant(rep(K,50))
bio    <-FLQuant(fwd(bio,harvest=harvest,r=r.,K=K,p=2))

tst<-FLBioDym(stock=bio[,-51],catch=bio[,-51]*harvest,index=exp(rnorm(50,0,.3))*(bio[,-51]+bio[,-1])/2)
tst1<-fit(tst,minimiser="optim", start=c(r=.4,K=100))
tst2<-fit(tst,minimiser="optim")
tst3<-fit(tst,minimiser="nls.lm",start=c(r=.4,K=100))
tst4<-fit(tst,minimiser="nls.lm")
plot(tst1)
plot(tst2)
plot(tst3)
plot(tst4)
