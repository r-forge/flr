###############################################################################
#                                                                              #
#  Tests production functions and reference points                             #
#                                                                              #
################################################################################
library(FLCore)
library(FLash)
library(FLBRP)
library(ggplot2)
library(plyr)

#### Source code

## Class stryucture
#source("C:/Stuff/FLR/pkg/FLipper/R/bdClass.R")
#source("C:/Stuff/FLR/pkg/FLipper/R/bdConstructors.R")
#source("C:/Stuff/FLR/pkg/FLipper/R/bdCreateAccessors.R")
#source("C:/Stuff/FLR/pkg/FLipper/R/bdMethods.R")

# 1) model & assumptions
## Definition of parameters
source("C:/Stuff/FLR/pkg/flipper/R/bdPars.R")
## Surplus projection functions
source("C:/Stuff/FLR/pkg/FLipper/R/bdSp.R")
## Projection
source("C:/Stuff/FLR/pkg/FLipper/R/bdFwd.R")

# 2) likelihood fitting stuff
source("C:/Stuff/FLR/pkg/FLipper/R/bdFit.R")

# 3) Reference points
source("C:/Stuff/FLR/pkg/FLipper/R/bdMsy.R")
## mapping MSY & BMSY to r & K
source("C:/Stuff/FLR/pkg/FLipper/R/bdMSYPar.R")

# 4) Diagnostics

#### test data set #############################################################
## Parameters
params=msy2par("fox",bmsy=1.0,msy=0.2)
params["b0"]=1

## Time Series
stk   =FLQuant(seq(params["b0"]*params["K"],length.out=51))
hrv   =FLQuant(c(fmsy("fox",params))*1.5,dimnames=list(year=1:50))
stk   =fwd(stk,model="fox",harvest=hrv,params=params)[,1:50]
ctc   =stk*hrv
idx   =ctc/hrv*exp(rlnorm(50,0,0.3))

#### Plots
bio    =c(FLQuant(seq(0,params["K"],length.out=101)))
spCurve=data.frame(bio=bio,
                    sp =c(sp("fox",bio,params=params)))

p=ggplot()+geom_line(data=spCurve,aes(bio,sp)) +
           geom_line(data=data.frame(catch=c(ctc),stock=c(stk)),aes(stock,catch))

#### Fitting ###################################################################
#### Functions
## Likelihood
LL<-function(par,params,model,catch=NULL,index=NULL){

   params[c("r","K")]<-par

   #params["b0"] =calcB0(idx,params)
   stk          =window(catch,end=max(as.numeric(dimnames(catch)$year))+1)
   stk[,1]      =params["b0"]*params["K"]
   stk          =fwd(stk,model=model,catch=catch,params=params)

   params["q"]  =calcQ(stk,idx)
   idxHat       =indexHat(stk,params["q"])
   res          =loglAR1(index,idxHat)

   return(-res)}

## Diagnostics
diags<-function(params,model,catch=NULL,index=NULL){

   stk          =window(catch,end=max(as.numeric(dimnames(catch)$year))+1)
   stk[,1]      =params["b0"]*params["K"]
   stk          =fwd(stk,model=model,catch=catch,params=params)
   params["q"]  =calcQ(stk,idx)
   idxHat       =indexHat(stk,params["q"])
   rsdl         =log(idxHat/index)
   rsdlLag      =FLQuant(NA,dimnames=dimnames(rsdl))
   rsdlLag[,-dim(rsdl)[2]]=rsdl[,-1]
   qq.          =qqnorm(c(rsdl),plot.it=FALSE)
   qqx          =FLQuant(qq.$x,dimnames=dimnames(rsdl))
   qqy          =FLQuant(qq.$y,dimnames=dimnames(rsdl))

   res<-model.frame(FLQuants(x=mnBio(stk),y=index,yHat=idxHat,rsdl=rsdl,rsdlLag=rsdlLag,qqx=qqx,qqy=qqy))

   return(ggplot(res))}

#### Fit
## 1st
res1=optim(fn=LL,par=params[c("r","K")],params=params,model="fox",catch=ctc,index=idx,method="BFGS",hessian=T)
params[c("r","K")]<-res1$par

## with parscaling
grd =grad(LL,params[c("r","K")],params=params,model="fox",catch=ctc,index=idx)
res2=optim(fn=LL,par=params[c("r","K")],params=params,model="fox",catch=ctc,index=idx,method="BFGS",hessian=T,control=list(parscale=1/grd))

#### Plotting ##################################################################
## profiling
fnLL<-function(r,K,params,model,catch,index) c(LL(c(r=r,K=K),params,model,catch,index))
pfl <-expand.grid(r=res2$par["r"]*seq(0.5,1.5,length.out=11),K=res2$par["K"]*seq(0.5,1.5,length.out=11))
pfl <-ddply(pfl,c("r","K"),transform,LL=fnLL(r,K,params=params,model="fox",catch=ctc,index=idx))

ggplot(pfl, aes(r, K, z = LL)) + stat_contour(aes(colour="black")) + geom_tile(aes(fill = LL))

#### Diagnostics
plotDiag<-function(x){
  diag_theme <- theme_update(
    plot.background = theme_rect(fill = "#3366FF"),
    panel.background = theme_rect(fill = "#003DF5"),
    axis.text.x = theme_text(colour = "#CCFF33"),
    axis.text.y = theme_text(colour = "#CCFF33", hjust = 1),
    axis.title.x = theme_text(colour = "#CCFF33", face = "bold"),
    axis.title.y = theme_text(colour = "#CCFF33", face = "bold",
     angle = 90))

  theme_set(diag_theme)

  vplayout <-function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(3,2)))

  p1<-p + geom_point(aes(qqx,qqy),size=0.5)   +
                    opts(title = "Normal Q-Q Plot") + scale_x_continuous(name="Theoretical Quantiles") +
                                                      scale_y_continuous(name="Sample Quantiles")  +
                    geom_abline(intercept=0, slope=1)
  print(p1, vp=vplayout(1,1))

  #plotFunc(obs,prd,indVar,indVar.,xttl=Xttl,yttl=Yttl,mttl="FunctionalForm",splt=c(1,1,2,3),more=TRUE)
  p2<-p+geom_point(aes(x,y))+geom_line(aes(x,yHat))
  print(p2, vp=vplayout(1,2))
  
  #plotResidYr(resid,xttl="Year",yttl='Residuals',mttl="",splt=c(2,1,2,3),more=TRUE)
  p3<-p+geom_point(aes(year,rsdl))+stat_smooth(aes(year,rsdl))  +
        scale_x_continuous(name="Year") + scale_y_continuous(name="Residuals")
  print(p3, vp=vplayout(2,1))

  #plotResidAR1(resid,xttl='Residualsatt',yttl='Residualsatt+1',mttl='AR(1)Residuals',splt=c(1,2,2,3),more=TRUE)
  p4<-p+geom_point(aes(rsdl,rsdlLag))+stat_smooth(aes(rsdl,rsdlLag)) + geom_abline(intercept = 0, slope = 1) +
        scale_x_continuous(name=expression(Residuals[t])) + scale_y_continuous(name=expression(Residuals[t+1]))
  print(p4, vp=vplayout(2,2))

  #plotResidX(resid,indVar,xttl=Xttl,yttl='Residuals',mttl="ResidualsbyIndvar",splt=c(2,2,2,3),more=TRUE)
  p5<-p+geom_point(aes(x,rsdl))+stat_smooth(aes(x,rsdl))  +
        scale_x_continuous(name="Index") + scale_y_continuous(name=expression(Residuals))
  print(p5, vp=vplayout(3,1))

  #plotResidX(resid,hat,xttl=paste(Yttl,"Hat"),yttl='Residuals',mttl="ResidualsbyHat",splt=c(1,3,2,3),more=TRUE)
  p6<-p+geom_point(aes(yHat,rsdl))+stat_smooth(aes(yHat,rsdl))  +
        scale_x_continuous(name=expression(Residuals[t])) + scale_y_continuous(name=expression(Residuals))
  print(p6, vp=vplayout(3,2))}


p=diags(params,"fox",ctc,idx)
plotDiags(p)
