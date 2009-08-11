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

par(mfrow=c(3,1))

##### Pella Tomlinson ##########################################################
## Plot Surplus Production and trajectories for constant catch & F             #
################################################################################
## p=2.0
plot(     sp("pellat",  bio,r.,K,p=2.0)~bio, type="l",xlab="Biomass",ylab="Catch",main="Pella Tomlinson",ylim=c(0,msy("pellat",r=r.,K=K,p=2)*2))
points(  msy("pellat",      r.,K,p=2.0)~bmsy("pellat",r.,K,p=2.0),col="blue",cex=1.75,pch=16)
points( bmsy("pellat",      r.,K,p=2.0)*fmsy("pellat",r.,K,p=2.0)~bmsy("pellat",r.,K,p=2.0),col="white",cex=1.25,pch=16)
points(( msy("pellat",      r.,K,p=2.0)/fmsy("pellat",r.,K,p=2.0)),msy("pellat",r.,K,p=2.0),col="red",cex=0.75,pch=16)

catch  <-FLQuant(rep( msy("pellat",r=r.,K=K,p=2),100))
harvest<-FLQuant(rep(fmsy("pellat",r=r.,K=K,p=2),100))
stock  <-FLQuant(rep(K,101))

lines(fwd(stock,harvest=harvest,par=list(r=r.,K=K,p=2))[,1:100,,,,],fwd(stock,harvest=harvest,r=r.,K=K,p=2)[,1:100,,,,]*harvest,        type="b",pch=16,cex=.5,col="red")
lines(fwd(stock,catch  =catch,  r=r.,K=K,p=2)[,1:100,,,,],catch,                                                              type="b",pch=16,cex=.5,col="blue")
lines(fwd(stock*.25,harvest=harvest,r=r.,K=K,p=2)[,1:100,,,,],fwd(stock*.25,harvest=harvest,r=r.,K=K,p=2)[,1:100,,,,]*harvest,type="b",pch=16,cex=.5,col="green")

## p=1.95
plot(     sp("pellat",  bio,r.,K,p=1.95)~bio, type="l",xlab="Biomass",ylab="Catch",main="Pella Tomlinson",ylim=c(0,msy("pellat",r=r.,K=K,p=1.95)*2))
points(  msy("pellat",      r.,K,p=1.95)~bmsy("pellat",r.,K,p=1.95),col="blue",cex=1.75,pch=16)
points( bmsy("pellat",      r.,K,p=1.95)*fmsy("pellat",r.,K,p=1.95)~bmsy("pellat",r.,K,p=1.95),col="white",cex=1.25,pch=16)
points(( msy("pellat",      r.,K,p=1.95)/fmsy("pellat",r.,K,p=1.95)),msy("pellat",r.,K,p=1.95),col="red",cex=0.75,pch=16)

catch  <-FLQuant(rep( msy("pellat",r=r.,K=K,p=1.95),100))
harvest<-FLQuant(rep(fmsy("pellat",r=r.,K=K,p=1.95),100))
stock  <-FLQuant(rep(K,101))

lines(fwd(stock,harvest=harvest,r=r.,K=K,p=1.95)[,1:100,,,,],fwd(stock,harvest=harvest,r=r.,K=K,p=1.95)[,1:100,,,,]*harvest,        type="b",pch=16,cex=.5,col="red")
lines(fwd(stock,catch  =catch,  r=r.,K=K,p=1.95)[,1:100,,,,],catch,                                                              type="b",pch=16,cex=.5,col="blue")
lines(fwd(stock*.25,harvest=harvest,r=r.,K=K,p=1.95)[,1:100,,,,],fwd(stock*.25,harvest=harvest,r=r.,K=K,p=1.95)[,1:100,,,,]*harvest,type="b",pch=16,cex=.5,col="green")

## p=2.05
plot(     sp("pellat",  bio,r.,K,p=2.05)~bio, type="l",xlab="Biomass",ylab="Catch",main="Pella Tomlinson",ylim=c(0,msy("pellat",r=r.,K=K,p=2.05)*2))
points(  msy("pellat",      r.,K,p=2.05)~bmsy("pellat",r.,K,p=2.05),col="blue",cex=1.75,pch=16)
points( bmsy("pellat",      r.,K,p=2.05)*fmsy("pellat",r.,K,p=2.05)~bmsy("pellat",r.,K,p=2.05),col="white",cex=1.25,pch=16)
points(( msy("pellat",      r.,K,p=2.05)/fmsy("pellat",r.,K,p=2.05)),msy("pellat",r.,K,p=2.05),col="red",cex=0.75,pch=16)

catch  <-FLQuant(rep( msy("pellat",r=r.,K=K,p=2.05),100))
harvest<-FLQuant(rep(fmsy("pellat",r=r.,K=K,p=2.05),100))
stock  <-FLQuant(rep(K,101))

lines(fwd(stock,harvest=harvest,r=r.,K=K,p=2.05)[,1:100,,,,],fwd(stock,harvest=harvest,r=r.,K=K,p=2.05)[,1:100,,,,]*harvest,        type="b",pch=16,cex=.5,col="red")
lines(fwd(stock,catch  =catch,  r=r.,K=K,p=2.05)[,1:100,,,,],catch,                                                              type="b",pch=16,cex=.5,col="blue")
lines(fwd(stock*.25,harvest=harvest,r=r.,K=K,p=2.05)[,1:100,,,,],fwd(stock*.25,harvest=harvest,r=r.,K=K,p=2.05)[,1:100,,,,]*harvest,type="b",pch=16,cex=.5,col="green")
