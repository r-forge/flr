################################################################################
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

#### plots #####################################################################

#### function for plotting msy
bullseye<-function(model,r=NULL,K=NULL,p=2,m=NULL,msy=NULL){
      points(  msy(model,r=r,K=K,p=p,m=m,msy=msy)~bmsy(model,r=r,K=K,p=p,m=m),
                    col="blue", cex=1.75,pch=16)
      points( bmsy(model,r=r,K=K,p=p,m=m)*fmsy(model,r=r,K=K,p=p,m=m,msy=msy)~bmsy(model,r=r,K=K,p=p,m=m),
                    col="white",cex=1.25,pch=16)
      points(( msy(model,r=r,K=K,p=p,m=m,msy=msy)/fmsy(model,r=r,K=K,p=p,m=m,msy=msy)),msy(model,r=r,K=K,p=p,m=m,msy=msy),
                    col="red",  cex=0.75,pch=16)
      }

#### Plot production curves with MSY
par(mfrow=c(3,2))

#### Fox
plot(sp("fox",bio,r.,K)~bio, type="l",xlab="Biomass",ylab="Surplus Production",main="Fox")
bullseye("fox",r=r.,K=K)

#### SChaeffer
plot( sp("schaefer", bio,r.,K       )~bio, type="l",xlab="Biomass",ylab="Surplus Production",main="Schaefer")
bullseye("schaefer",r=r.,K=K)

#### Pella Tomlinson
plot( sp("pellat",  bio,r.,K,p=1.95)~bio, type="l",xlab="Biomass",ylab="Surplus Production",main="Pella Tomlinson")
bullseye("pellat",r.,K,p=1.95)
lines(sp("pellat",  bio,r.,K,p=2.00)~bio, col="red")
bullseye("pellat",r.,K,p=2.00)
lines(sp("pellat",  bio,r.,K,p=2.05)~bio, col="blue")
bullseye("pellat",r.,K,p=2.05)

#### Shepherd
plot( sp("shepherd", bio,r.,K,m=0.24)~bio, type="l",xlab="Biomass",ylab="Surplus Production",main="Shepherd")
bullseye("shepherd",r.,K,m=0.24)
lines(sp("shepherd", bio,r.,K,m=0.25)~bio, col="red")
bullseye("shepherd",r.,K,m=0.25)
lines(sp("shepherd", bio,r.,K,m=0.26)~bio, col="blue")
bullseye("shepherd",r.,K,m=0.26)

#### Gulland
plot( sp("gulland",  bio,r.,K)~bio, type="l",xlab="Biomass",ylab="Surplus Production",main="Gulland")
bullseye("gulland",      r.,K)

#### Fletcher
plot( sp("fletcher", bio,K=K,msy=K/4,p=2)~bio, type="l",xlab="Biomass",ylab="Surplus Production",main="Fletcher")
bullseye("fletcher",K=K,msy=K/4,p=2)

#### Calcs
maximime suprplus production (i.e. MSY) and find BMSY

#### Fox
f    <-function (x,r,K) sp("fox",x,r.,K)
tst  <-optimize(f, c(0, K), tol = 0.0001, maximum=TRUE, r=r., K=K)
tst$objective- msy("fox",r.,K)
tst$maximum  -bmsy("fox",r.,K)

#### Schaeffer
f    <-function (x,r,K) sp("schaefer", x,r.,K       )
tst  <-optimize(f, c(0, K), tol = 0.0001, maximum=TRUE, r=r., K=K)
tst$objective-msy("schaefer",r.,K)
tst$maximum  -bmsy("schaefer",r.,K)

#### Pella Tomlinson
f    <-function (x,r,K) sp("pellat",  x,r.,K,p=2)
tst  <-optimize(f, c(0, K), tol = 0.0001, maximum=TRUE, r=r., K=K)
tst$objective- msy("pellat",  r.,K,p=2)
tst$maximum  -bmsy("pellat",  r.,K,p=2)

#### Shepherd
f    <-function (x,r,K) sp("shepherd", x,r.,K,m=0.25)
tst  <-optimize(f, c(0, K), tol = 0.0001, maximum=TRUE, r=r., K=K)
tst$objective- msy("shepherd", r.,K,m=0.25)
tst$maximum  -bmsy("shepherd", r.,K,m=0.25)

#### Gulland
f    <-function (x,r,K) sp("gulland",  x,r.,K)
tst  <-optimize(f, c(0, K), tol = 0.0001, maximum=TRUE, r=r., K=K)
tst$objective- msy("gulland",  r.,K)
tst$maximum  -bmsy("gulland",  r.,K)

#### Fletcher
f    <-function (x,K,msy,p=2) sp("fletcher", x,K=K,msy=K/4,p=2)
tst  <-optimize(f, c(0, K), tol = 0.0001, maximum=TRUE, K=K,msy=K/4,p=2)
tst$objective- msy("fletcher",K=K,msy=K/4,p=2)
tst$maximum  -bmsy("fletcher",K=K,p=2)


