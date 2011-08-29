library(FLCore)
library(FLBRP)
library(numDeriv)
library(FLash)


load("/home/lkell/Dropbox/FLRbook/chapters/SR/Data/srr.RData")

dirMy="/home/lkell/Dropbox/FLRbook/chapters/SR/"

#### Functions #################################################################

#### to calculate gradient for parameter scaling
computeGrad=function(object,method="Richardson",
      method.args=list(eps=1e-4, d=0.0001, zero.tol=sqrt(.Machine$double.eps/7e-7), r=4, v=2, show.details=FALSE), ...){

     ## wrapper function from grad to call
     fn=function(x,sr){
       x.         =as.list(x)
       names(x.)  =dimnames(x)$params

       x.[["ssb"]]=sr@ssb
       x.[["rec"]]=sr@rec

       logl.      =sr@logl
       res        =do.call("logl.",x.)

       return(res)}
     
     grad(fn,x=c(object@initial(object@rec,object@ssb)),
               method=method,method.args=method.args,
               sr=object)}

#### Fit all SRRs
runCheckSRR=function(srr,srType,srModelFlag=FALSE,spr0=NULL){

    ps=srr
    ap=srr

    #### Fit SRRs
    par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
    for (i in names(srr)){
      ssbRng=FLQuant(seq(0,max(ssb(srr[[i]]),na.rm=T),length.out=101))
      if (srModelFlag)
         model(srr[[i]]) =do.call(srType,list())
      else ## FLash::srModel
         model(srr[[i]]) =srModel(srType)
#      gr(srr[[i]])=function() NULL

        plot(rec(srr[[i]])~ssb(srr[[i]]),col="black",xlim=c(0,max(ssb(srr[[i]]))),ylim=c(0,max(rec(srr[[i]]))),pch=19,xaxt ="n",yaxt="n")
        mtext(i,side=3, line=-2)

        if (!is.null(spr0)) try  =try(srr[[i]]<-fmle(srr[[i]])) else
                            try  =try(srr[[i]]<-fmle(srr[[i]],fixed=list(spr0=spr0)))
        if(!is(try,   'try-error')) lines(predict(srr[[  i]],ssb=ssbRng)~ssbRng,col="blue")

        if (!is.null(spr0)) tryPS=try(ps[[i]]<-fmle(srr[[i]],control=list(parscale=1/pmax(0.000001,abs(computeGrad(srr[[i]])))))) else
                            tryPS=try(ps[[i]]<-fmle(srr[[i]],control=list(parscale=1/pmax(0.000001,abs(computeGrad(srr[[i]])))),fixed=list(spr0=spr0)))
        if(!is(tryPS, 'try-error')) lines(predict(ps[[i]],ssb=ssbRng)~ssbRng,col="white")

        if (!is.null(spr0)) tryAP=try(ap[[i]]<-fmle(srr[[i]],autoParscale=TRUE,fixed=list(spr0=spr0))) else
                            tryAP=try(ap[[i]]<-fmle(srr[[i]],autoParscale=TRUE))
        if(!is(tryAP, 'try-error')) lines(predict(ap[[i]],ssb=ssbRng)~ssbRng,col="red")}

    CF=data.frame(srr  =unlist(lapply(srr,logLik)),
                  scale=unlist(lapply(ps, logLik)),
                  auto =unlist(lapply(ap, logLik)))

    print(CF)
    return(list(srr=srr,ps=ps,ap=ap))}

#### Check likelihood profiles
profileSRR=function(srr){
    par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
    for (i in names(srr)){
        profile(srr[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
        mtext(i,side=3, line=-2)}}
################################################################################


#### Beverton & Holt ###########################################################
bh=runCheckSRR(srr,srType="bevholt")
savePlot(paste(myDir,"Figs/bh.png",sep=""),type="png")
save(bh,file=paste(myDir,"Data/bh.RData",  sep=""))
bhLogLik<-array(unlist(lapply(bh, function(x) lapply(x, logLik))),c(51,3),dimnames=list(stock=names(srr),method=c("default","parscale","auto")))
bhLogLik[c(18,25,28,30,41,48),]

profileSRR(bh[["srr"]])
savePlot(paste(myDir,"Figs/bhProfile.png",sep=""),type="png")

profileSRR(bh[["ps"]])
savePlot(paste(myDir,"Figs/bhProfilePS.png",sep=""),type="png")

profileSRR(bh[["as"]])
savePlot(paste(myDir,"Figs/bhProfileAS.png",sep=""),type="png")

### Ricker #####################################################################
rk=runCheckSRR(srr,srType="ricker")
savePlot(paste(myDir,"Figs/rk.png",sep=""),type="png")
save(rk,file=paste(myDir,"Data/rk.RData",  sep=""))
rkLogLik<-array(unlist(lapply(rk, function(x) lapply(x, logLik))),c(51,3),dimnames=list(stock=names(srr),method=c("default","parscale","auto")))
rkLogLik[c(),]

rkLogLik<-array(unlist(lapply(rk, function(x) lapply(x, logLik))),c(51,3),dimnames=list(stock=names(srr),method=c("default","parscale","auto")))
rkLogLik[c(18,25,28,30,41,48),]

profileSRR(rk[["srr"]])
savePlot(paste(myDir,"Figs/rkProfile.png",sep=""),type="png")

profileSRR(rk[["ps"]])
savePlot(paste(myDir,"Figs/rkProfilePS.png",sep=""),type="png")

profileSRR(rk[["as"]])
savePlot(paste(myDir,"Figs/rkProfileAS.png",sep=""),type="png")

### Cushing ####################################################################
ch=runCheckSRR(srr,srType="cushing")
savePlot(paste(myDir,"Figs/ch.png",sep=""),type="png")
save(ch,file=paste(myDir,"Data/ch.RData",  sep=""))
chLogLik<-array(unlist(lapply(ch, function(x) lapply(x, logLik))),c(51,3),dimnames=list(stock=names(srr),method=c("default","parscale","auto")))
chLogLik[c(),]

chLogLik<-array(unlist(lapply(ch, function(x) lapply(x, logLik))),c(51,3),dimnames=list(stock=names(srr),method=c("default","parscale","auto")))
chLogLik[c(18,25,28,30,41,48),]

profileSRR(ch[["srr"]])
savePlot(paste(myDir,"Figs/chProfile.png",sep=""),type="png")

profileSRR(ch[["ps"]])
savePlot(paste(myDir,"Figs/chProfilePS.png",sep=""),type="png")

profileSRR(ch[["as"]])
savePlot(paste(myDir,"Figs/chProfileAS.png",sep=""),type="png")

### Segmented Regression #######################################################
srr<-lapply(srr,function(x) x<-transform(x,rec=rec/1000,ssb=ssb/1000))
sg=runCheckSRR(srr,srType="segreg")
savePlot(paste(myDir,"Figs/sg.png",sep=""),type="png")
save(sg,file=paste(myDir,"Data/sg.RData",  sep=""))

sgLogLik<-array(unlist(lapply(sg, function(x) lapply(x, logLik))),c(51,3),dimnames=list(stock=names(srr),method=c("default","parscale","auto")))
sgLogLik[c(),]

sgLogLik<-array(unlist(lapply(sg, function(x) lapply(x, logLik))),c(51,3),dimnames=list(stock=names(srr),method=c("default","parscale","auto")))
sgLogLik[c(18,25,28,30,41,48),]

profileSRR(sg[["srr"]])
savePlot(paste(myDir,"Figs/sgProfile.png",sep=""),type="png")

profileSRR(sg[["ps"]])
savePlot(paste(myDir,"Figs/sgProfilePS.png",sep=""),type="png")

profileSRR(sg[["as"]])
savePlot(paste(myDir,"Figs/sgProfileAS.png",sep=""),type="png")

### Shepherd ###################################################################
sh=runCheckSRR(srr,srType="shepherd")
savePlot(paste(myDir,"Figs/sh.png",sep=""),type="png")
save(sh,file=paste(myDir,"Data/sh.RData",  sep=""))
shLogLik<-array(unlist(lapply(sh, function(x) lapply(x, logLik))),c(51,3),dimnames=list(stock=names(srr),method=c("default","parscale","auto")))
shLogLik[c(),]

shLogLik<-array(unlist(lapply(sh, function(x) lapply(x, logLik))),c(51,3),dimnames=list(stock=names(srr),method=c("default","parscale","auto")))
shLogLik[c(18,25,28,30,41,48),]

profileSRR(sh[["srr"]])
savePlot(paste(myDir,"Figs/shProfile.png",sep=""),type="png")

profileSRR(sh[["ps"]])
savePlot(paste(myDir,"Figs/shProfilePS.png",sep=""),type="png")

profileSRR(sh[["as"]])
savePlot(paste(myDir,"Figs/shProfileAS.png",sep=""),type="png")

### Mean #######################################################################
mn=runCheckSRR(srr,srType="geomean")
savePlot(paste(myDir,"Figs/mn.png",sep=""),type="png")
save(mn,file=paste(myDir,"Data/mn.RData",  sep=""))
mnLogLik<-array(unlist(lapply(mn, function(x) lapply(x, logLik))),c(51,3),dimnames=list(stock=names(srr),method=c("default","parscale","auto")))
mnLogLik[c(),]

mnLogLik<-array(unlist(lapply(mn, function(x) lapply(x, logLik))),c(51,3),dimnames=list(stock=names(srr),method=c("default","parscale","auto")))
mnLogLik[c(18,25,28,30,41,48),]

profileSRR(mn[["srr"]])
savePlot(paste(myDir,"Figs/mnProfile.png",sep=""),type="png")

profileSRR(mn[["ps"]])
savePlot(paste(myDir,"Figs/mnProfilePS.png",sep=""),type="png")

profileSRR(mn[["as"]])
savePlot(paste(myDir,"Figs/mnProfileAS.png",sep=""),type="png")

#### Profile to check Third Shape Parameter
if (srType=="shepherd"){
   par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
   for (i in names(srr)){
       profile(srr[[i]],which=c("c"),xaxt ="n",yaxt="n")
       mtext(i,side=3, line=-2)}
savePlot(paste(dir,"Figs/",srType,"ProfileC.png",sep=""),type="png")
################################################################################

################################################################################
#### srModel ###################################################################
################################################################################
model(srr[[i]])<-srModel("bevholt")
pars<-initial(srr[[i]])(rec(srr[[i]]),ssb(srr[[i]]))
logl(srr[[i]])(pars["a"],pars["b"],rec(srr[[i]]),ssb(srr[[i]]))
gr(srr[[i]])(pars["a"],pars["b"],rec(srr[[i]]),ssb(srr[[i]]))
gr(srr[[i]])=function() NULL
system.time(srr[[i]]<-fmle(srr[[i]]))
computeGrad(srr[[i]])
plot(srr[[i]])

model(srr[[i]])<-bevholt()
pars<-initial(srr[[i]])(rec(srr[[i]]),ssb(srr[[i]]))
logl(srr[[i]])(pars["a"],pars["b"],rec(srr[[i]]),ssb(srr[[i]]))
system.time(srr[[i]]<-fmle(srr[[i]]))
plot(srr[[i]])

################################################################################
