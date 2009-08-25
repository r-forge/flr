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
bio<-FLQuant(seq(0,K,length.out=100))

##### Pella Tomlinson ##########################################################
## Plot Surplus Production and trajectories for constant catch & F             #
################################################################################
#### p=2.0
#### surplus production curves and reference points
plot(     sp("schaefer",  bio,r.,K)~bio, type="l",xlab="Biomass",ylab="Catch",main="Pella Tomlinson",ylim=c(0,msy("schaefer",r=r.,K=K)*2))
points(  msy("schaefer",      r.,K)~bmsy("schaefer",r.,K),col="blue",cex=1.75,pch=16)
points( bmsy("schaefer",      r.,K)*fmsy("schaefer",r.,K)~bmsy("schaefer",r.,K),col="white",cex=1.25,pch=16)
points(( msy("schaefer",      r.,K)/fmsy("schaefer",r.,K)),msy("schaefer",r.,K),col="red",cex=0.75,pch=16)

#### time series
catch  <-FLQuant(rep( msy("schaefer",r=r.,K=K),100))
harvest<-FLQuant(rep(fmsy("schaefer",r=r.,K=K),100))
stock  <-FLQuant(rep(K,101))

stk<-fwd(stock,harvest=harvest,r=r.,K=K)[,1:100,,,,]
lines(stk,stk*harvest,type="b",pch=16,cex=.5,col="red")
stk<-fwd(stock,catch=catch,r=r.,K=K)[,1:100,,,,]
lines(stk,catch,type="b",pch=16,cex=.5,col="blue")
stk<-fwd(stock*.25,harvest=harvest,r=r.,K=K)[,1:100,,,,]
lines(stk,stk*harvest,type="b",pch=16,cex=.5,col="green")

