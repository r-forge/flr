################################################################################
#                                                                              #
#  Simulated data sets                                                         #
#                                                                              #
################################################################################
library(FLBioDym)

# start test
#setCon()
#zz <- startTest("fwdFLStock.txt")
#tagTest("fwd(FLStock) testing ...")
#checkRun(
#checkRun(

#### Create test data sets
simData<-function(model,harvest=NULL,r.=0.5,K=100,p=2,m=0.25,error="log"){

    if (model=="fletcher") msy <-r.*K/4
    
    ## increase F from 0 to 1.5*FMSY over nyr years
#   harvest<-FLQuant(seq(0,fmsy(model,r=r.,K=K,p=p,m=m,msy=msy)*2*finalF,length.out=nyr))
    if (is.null(harvest))
       harvest<-FLQuant(seq(fmsy(model,r=r.,K=K,p=p,m=m,msy=msy),fmsy(model,r=r.,K=K,p=p,m=m,msy=msy),length.out=nyr))
    nyr    <-dims(harvest)$year
    stk    <-FLQuant(rep(K,nyr))
    stk    <-fwd(stk,harvest=harvest,model="pellat",r=r.,K=K)

    index<-switch(error,
                     log   =exp(rnorm(nyr,0,.3))*(stk[,-(nyr+1)]+stk[,-1])/2,
                     normal=exp(rnorm(nyr,0,.3))*(stk[,-(nyr+1)]+stk[,-1])/2,
                     cv    =exp(rnorm(nyr,0,.3))*(stk[,-(nyr+1)]+stk[,-1])/2)

    tst    <-FLBioDym(stock=stk[,-(nyr+1)],catch=stk[,-(nyr+1)]*harvest,index=index,model="fletcher")

    return(tst)
    }

h.  <-FLQuant(c(seq(0,1,length.out=30),seq(1,.5,length.out=20)))*fmsy("fletcher",K=100,msy=12.5, p=2)

res<-array(as.numeric(NA),c(100,2),dimnames=list(1:100,c("msy","K")))
for (i in 1:100){
   tst <-simData("fletcher",harvest=h.)
   tst1<-fit(tst,minimiser="nls.lm", start=c(K=100,msy=12))
   res[i,]<-params(tst1)[c("msy","K"),1,drop=T]}

tst <-simData("schaefer")
tst2<-fit(tst,minimiser="nls.lm", start=c(r=.5,K=100), nlsControl=nls.lm.control(nprint=1))

tst <-simData("fletcher")
tst3<-fit(tst,minimiser="nls.lm", start=c(r=.5,K=100), nlsControl=nls.lm.control(nprint=1))

tst <-simData("fox")
tst4<-fit(tst,minimiser="nls.lm", start=c(r=.5,K=100), nlsControl=nls.lm.control(nprint=1))

tst <-simData("shepherd")
tst5<-fit(tst,minimiser="nls.lm", start=c(r=.5,K=100), nlsControl=nls.lm.control(nprint=1))

tst <-simData("gulland")
tst6<-fit(tst,minimiser="nls.lm", start=c(r=.5,K=100), nlsControl=nls.lm.control(nprint=1))
