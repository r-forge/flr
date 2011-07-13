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
msy <-r.*K/4
bio<-FLQuant(seq(0,K,length.out=100))

##### Pella Tomlinson ##########################################################
## Plot Surplus Production and trajectories for constant catch & F             #
################################################################################
#### p=2.0
#### surplus production curves and reference points
plot(     sp("fletcher",bio,K=K,msy=msy)~bio, type="l",xlab="Biomass",ylab="Catch",main="Pella Tomlinson",ylim=c(0,msy("fletcher",K=K,msy=msy)*2))
points(  msy("fletcher",    K=K,msy=msy)~bmsy("fletcher",K=K),col="blue",cex=1.75,pch=16)
points( bmsy("fletcher",    K=K)        *fmsy("fletcher",K=K,msy=msy)~bmsy("fletcher",K=K),        col="white",cex=1.25,pch=16)
points(( msy("fletcher",    K=K,msy=msy)/fmsy("fletcher",K=K,msy=msy)),msy("fletcher",K=K,msy=msy),col="red",cex=0.75,pch=16)

#### time series
catch  <-FLQuant(rep( msy("fletcher",K=K,msy=msy),100))
harvest<-FLQuant(rep(fmsy("fletcher",K=K,msy=msy),100))
stock  <-FLQuant(rep(K,101))

stk<-fwd(stock,harvest=harvest,model="fletcher",K=K,msy=msy)[,1:100,,,,]
lines(stk,stk*harvest,type="b",pch=16,cex=.5,col="red")
stk<-fwd(stock,catch=catch,model="fletcher",K=K,msy=msy)[,1:100,,,,]
lines(stk,catch,type="b",pch=16,cex=.5,col="blue")
stk<-fwd(stock*.25,harvest=harvest,model="fletcher",K=K,msy=msy)[,1:100,,,,]
lines(stk,stk*harvest,type="b",pch=16,cex=.5,col="green")

