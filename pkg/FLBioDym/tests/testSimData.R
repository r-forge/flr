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
simData<-function(model,r.=0.5,K=100,p=2,m=0.25,msy=NULL,error="log",nyr=50,finalF=1.5){

    if (is.null(msy)) msy <-r.*K/4
    
    ## increase F from 0 to 1.5*FMSY over nyr years
#   harvest<-FLQuant(seq(0,fmsy(model,r=r.,K=K,p=p,m=m,msy=msy)*2*finalF,length.out=nyr))
    harvest<-FLQuant(seq(fmsy(model,r=r.,K=K,p=p,m=m,msy=msy),fmsy(model,r=r.,K=K,p=p,m=m,msy=msy),length.out=nyr))
    stk    <-FLQuant(rep(K,nyr))
    stk    <-fwd(stk,harvest=harvest,model="pellat",r=r.,K=K)

    index<-switch(error,
                     log   =exp(rnorm(nyr,0,.3))*(stk[,-(nyr+1)]+stk[,-1])/2,
                     normal=exp(rnorm(nyr,0,.3))*(stk[,-(nyr+1)]+stk[,-1])/2,
                     cv    =exp(rnorm(nyr,0,.3))*(stk[,-(nyr+1)]+stk[,-1])/2)

    tst    <-FLBioDym(stock=stk[,-(nyr+1)],catch=stk[,-(nyr+1)]*harvest,index=index)

    return(tst)
    }
    
tst <-simData("pellat",finalF=1)
tst1<-fit(tst,minimiser="nls.lm", start=c(r=.5,K=100), nlsControl=nls.lm.control(nprint=1))

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
