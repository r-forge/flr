###############################################################################
#                                                                              #
#  Tests production functions and reference points                             #
#                                                                              #
################################################################################
library(FLCore)
library(FLash)
library(FLBRP)
library(ggplot2)

#### Source code
#source("C:/Stuff/FLR/pkg/FLipper/R/bdClass.R")
#source("C:/Stuff/FLR/pkg/FLipper/R/bdConstructors.R")
#source("C:/Stuff/FLR/pkg/FLipper/R/bdCreateAccessors.R")
#source("C:/Stuff/FLR/pkg/FLipper/R/bdMethods.R")

source("C:/Stuff/FLR/pkg/FLipper/R/bdFwd.R")
source("C:/Stuff/FLR/pkg/FLipper/R/bdMsy.R")
source("C:/Stuff/FLR/pkg/FLipper/R/bdSp.R")
source("C:/Stuff/FLR/pkg/flipper/R/bdPars.R")
source("C:/Stuff/FLR/pkg/FLipper/R/bdMSYPar.R")

#### specifying params as bmsy/msy ###############################################
msy2par("fox",     bmsy=1.0,msy=0.2)
msy2par("schaefer",bmsy=1.0,msy=0.2)
msy2par("gulland", bmsy=1.0,msy=0.2)
msy2par("fletcher",bmsy=1.0,msy=0.2,p=1.5)
msy2par("fletcher",bmsy=1.0,msy=0.2,p=2.0)
msy2par("fletcher",bmsy=1.0,msy=0.2,p=2.5)
msy2par("pellat",  bmsy=1.0,msy=0.2,p=1.9)
msy2par("pellat",  bmsy=1.0,msy=0.2,p=2.0)
msy2par("pellat",  bmsy=1.0,msy=0.2,p=2.1)
msy2par("shepherd",bmsy=1.0,msy=0.2,m=1)

#### specifying params as K/msy ###############################################
msy2par("fox",     msy=0.2,K=2.0)
msy2par("schaefer",msy=0.2,K=2.0)
msy2par("gulland", msy=0.2,K=2.0)
msy2par("fletcher",msy=0.2,K=2.0,p=1.5)
msy2par("fletcher",msy=0.2,K=2.0,p=2.0)
msy2par("fletcher",msy=0.2,K=2.0,p=2.5)
msy2par("pellat",  msy=0.2,K=2.0,p=1.9)
msy2par("pellat",  msy=0.2,K=2.0,p=2.0)
msy2par("pellat",  msy=0.2,K=0.2,p=2.1)
msy2par("shepherd",msy=0.2,K=0.2,m=1)

#### Options for testing #######################################################
optns <-rbind.fill(data.frame(model="fox",     msy=0.2,bmsy=1.0,K=2.0),
                   data.frame(model="schaefer",msy=0.2,bmsy=1.0,K=2.0),
                   data.frame(model="gulland", msy=0.2,bmsy=1.0,K=2.0),
                   data.frame(model="fletcher",msy=0.2,bmsy=1.0,K=2.0,p=1.5),
                   data.frame(model="fletcher",msy=0.2,bmsy=1.0,K=2.0,p=2.0),
                   data.frame(model="fletcher",msy=0.2,bmsy=1.0,K=2.0,p=2.5),
                   data.frame(model="pellat",  msy=0.2,bmsy=1.0,K=2.0,p=1.9),
                   data.frame(model="pellat",  msy=0.2,bmsy=1.0,K=2.0,p=2.0),
                   data.frame(model="pellat",  msy=0.2,bmsy=1.0,K=2.0,p=2.1),
                   data.frame(model="shepherd",msy=0.2,bmsy=1.0,K=0.2,      m=1))
optns$model<-ac(optns$model)

#### surplus production curves #################################################
bio<-seq(0,2,length.out=101)

## Specify as BMSY
sPrd<-NULL
for (i in 1:(dim(optns)[1]-1))
  sPrd<-rbind.fill(sPrd,data.frame(optns[i,],bio=bio,sp=c(sp(optns[i,"model"],bio,msy2par(optns[i,"model"],msy=optns[i,"msy"],bmsy=optns[i,"bmsy"],p=optns[i,"p"])))))

p<-ggplot(sPrd)+geom_line(aes(bio,sp,group=p)) +
    scale_y_continuous(name="Surplus production", limits=c(0,0.25)) + facet_wrap(~model)

## Specify as K
sPrd<-NULL
for (i in 1:(dim(optns)[1]-1))
  sPrd<-rbind.fill(sPrd,data.frame(optns[i,],bio=bio,sp=c(sp(optns[i,"model"],bio,msy2par(optns[i,"model"],msy=optns[i,"msy"],K=optns[i,"K"],p=optns[i,"p"])))))

p<-ggplot(sPrd)+geom_line(aes(bio,sp,group=p)) +
    scale_y_continuous(name="Surplus production", limits=c(0,0.25)) + facet_wrap(~model)

#### Reference points ##########################################################
Msy<-NULL
for (i in 1:(dim(optns)[1]-1))
  Msy<-rbind.fill(Msy,data.frame(model=optns[i,"model"],p=optns[i,"p"],msy=c(msy(optns[i,"model"],msy2par(optns[i,"model"],msy=optns[i,"msy"],K=optns[i,"K"],p=optns[i,"p"])))))

BMsy<-NULL
for (i in 1:(dim(optns)[1]-1))
  BMsy<-rbind.fill(BMsy,data.frame(model=optns[i,"model"],p=optns[i,"p"],bmsy=c(bmsy(optns[i,"model"],msy2par(optns[i,"model"],msy=optns[i,"msy"],K=optns[i,"K"],p=optns[i,"p"])))))

FMsy<-NULL
for (i in 1:(dim(optns)[1]-1))
  FMsy<-rbind.fill(FMsy,data.frame(model=optns[i,"model"],p=optns[i,"p"],fmsy=c(fmsy(optns[i,"model"],msy2par(optns[i,"model"],msy=optns[i,"msy"],K=optns[i,"K"],p=optns[i,"p"])))))

rpts<-merge(merge(Msy,BMsy),FMsy)
rm(Msy,BMsy,FMsy)

p<-ggplot(sPrd)+geom_line(aes(bio,sp,group=p))+geom_point(data=rpts,aes(bmsy,msy,group=p)) +facet_wrap(~model) +
   scale_y_continuous(name="Surplus production",limits=c(0,0.25)) +facet_wrap(~model)

#### Compare all ###############################################################
ref<-function(model,params){
  stock<-seq(0,params["K"],length.out=101)
  
  #### surplus production curve
  sprd=data.frame(stock=c(stock),
                  catch=c(sp(model,stock,params=params)))

  #### ref pts, checks that all are consistent
   rpts=data.frame(msy   =c(msy( model,params), bmsy(model,params)*fmsy(model,params),  msy(model,params)),
                   bmsy  =c(bmsy(model,params), bmsy(model,params),                     msy(model,params)/fmsy(model,params)),
                  source=ac(3:1))
  
  #### time series
  ctc =FLQuant(c( msy(model,params)),dimnames=list(year=1:101))
  hrv =FLQuant(c(fmsy(model,params)),dimnames=list(year=1:101))
  stk =FLQuant(c(bmsy(model,params)),dimnames=list(year=1:102))

  stock  =fwd(stk*0.75, model=model,   harvest=hrv,    params=params)[,1:101]
  catch  =stock*hrv
  hcr    =data.frame(stock=c(stock), catch=c(catch), hcr=ac(1))

  stock  =fwd(stk*1.25, model=model, harvest=hrv,  params=params)[,1:101]
  catch  =stock*hrv
  hcr    =rbind(hcr,data.frame(stock=c(stock), catch=c(catch), hcr=ac(2)))

  stock  =fwd(stk*0.75, model=model, catch=ctc,    params=params)[,1:101]
  hcr    =rbind(hcr,data.frame(stock=c(stock), catch=c(ctc), hcr=ac(3)))

  stock  =fwd(stk*1.25, model=model, catch=ctc,    params=params)[,1:101]
  hcr    =rbind(hcr,data.frame(stock=c(stock), catch=c(ctc), hcr=ac(4)))
  
  return(list(sp=sprd,rpts=rpts,hcr=hcr))
  }


#### Summary data sets
tmp<-sPd<-hcr<-rpts<-NULL
for (i in 1:(dim(optns)[1]-1)){
  tmp[[i]]<-ref(optns[i,"model"],msy2par(optns[i,"model"],msy=optns[i,"msy"],K=optns[i,"K"],p=optns[i,"p"]))
  sPd     <-rbind.fill(sPd, data.frame(optns[i,],tmp[[i]]$sp))
  hcr     <-rbind.fill(hcr, data.frame(optns[i,],tmp[[i]]$hcr))
  rpts    <-rbind.fill(rpts,data.frame(optns[i,],tmp[[i]]$rpts))}

#### Plots
p<-ggplot()+geom_line( data=tmp[[1]]$sp,  aes(stock,catch)) +
            geom_point(data=tmp[[1]]$rpts,aes(bmsy,msy,col=source,size=source)) +
            geom_line( data=tmp[[1]]$hcr, aes(stock,catch,group=hcr,col=hcr)) +
   scale_y_continuous(name="Surplus production",limits=c(0,0.25))

p<-ggplot()+geom_line( data=sPd, aes(stock,catch)) +
            geom_point(data=rpts,aes(bmsy,msy,col=source,size=source)) +
            geom_line( data=hcr, aes(stock,catch,group=hcr)) +
   scale_y_continuous(name="Surplus production",limits=c(0,0.25)) + facet_wrap(model~p)
