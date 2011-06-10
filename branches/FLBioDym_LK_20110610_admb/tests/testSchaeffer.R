################################################################################
#                                                                              #
#  Tests Schaeffer                                                             #
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
##### Schaeffer ################################################################
## Plot Surplus Production and trajectories for constant catch & F             #
################################################################################

## increase F from 0 to 2*FMSY over 50 years
harvest<-FLQuant(seq(0,fmsy("pellat",r=r.,K=K,p=2)*4,length.out=50))
bio    <-FLQuant(rep(K,50))
bio    <-FLQuant(fwd(bio@.Data,harvest=harvest@.Data,r=r.,K=K,p=2))

tst<-FLBioDym(stock=bio[,-51],catch=bio[,-51]*harvest,index=exp(rnorm(50,0,.3))*(bio[,-51]+bio[,-1])/2)
tst1<-fit(tst,minimiser="optim", start=c(r=.4,K=100))
tst2<-fit(tst,minimiser="optim")
tst3<-fit(tst,minimiser="nls.lm",start=c(r=.4,K=100))
tst4<-fit(tst,minimiser="nls.lm")
plot(tst1)
plot(tst2)
plot(tst3)
plot(tst4)

data(albSP)
data(lobSP)
data(hkeSP)

model(albSP)="fletcher"
model(lobSP)="fletcher"
model(hkeSP)="fletcher"
