library(numDeriv)
library(FLash)
library(FLR4SCRS)

load("/home/lkell/Dropbox/FLRbook/chapters/SR/Data/srr.RData")

dirMy="/home/lkell/Dropbox/FLRbook/chapters/SR/"

#### Functions #################################################################

#### Fit all SRRs
runCheckSRR=function(srr,srType){
   #gr=srr
    pr=srr
    nm=srr
    ad=srr
    ag=srr
    sv=srr

    #### Fit SRRs
    par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
    for (i in names(srr)){
        ssbRng=FLQuant(seq(0,max(ssb(srr[[i]]),na.rm=T),length.out=101))
        plot(rec(srr[[i]])~ssb(srr[[i]]),col="black",xlim=c(0,max(ssb(srr[[i]]))),ylim=c(0,max(rec(srr[[i]]))),pch=19,xaxt ="n",yaxt="n")
        mtext(i,side=3, line=-2)
        model(srr[[i]]) =do.call(srType,list())

        if (srType=="bevholt") lower(srr[[i]])[]<-0
        
        #### Default 
        try  =try(srr[[i]]<-fmle(srr[[i]]))
        if(!is(try,   'try-error')) lines(predict(srr[[  i]],ssb=ssbRng)~ssbRng,col="blue")

        #### Nelder Mead 
        tryNM=try(nm[[i]]<-fmle(srr[[i]],method ="Nelder-Mead"))
        if(!is(tryNM, 'try-error')) lines(predict(nm[[  i]],ssb=ssbRng)~ssbRng,col="cyan")
        
        #### Scale parameters
        tryPr=try(pr[[i]]<-fmle(srr[[i]],control=list(parscale=parscale(srr[[i]]))))
        if(!is(tryPr, 'try-error')) lines(predict(pr[[i]],ssb=ssbRng)~ssbRng,col="green")

        #### Steepness
        if (srType!="cushing"){
           model(sv[[i]]) =do.call(paste(srType,"SV",sep=""),list())
      
           spr0=mean(ssb(sv[[i]])/rec(sv[[i]]))*2
           ps  =c(1,mean(ssb(sv[[i]]))*2)
           trySV=try(sv[[i]]<-fmle(sv[[i]],fixed=list(spr0=spr0),control=list(parscale=ps)))
           if(!is(trySV, 'try-error')) lines(predict(sv[[i]],ssb=ssbRng)~ssbRng,col="black")}

#### AD and gradient stuff ############################################################################################
if (FALSE){
# numeric as a check
        tryGr=try(gr[[i]]<-fmle(srr[[i]],control=list(parscale=1/pmax(0.000001,c(abs(computeGrad(srr[[i]])))))))
        if(!is(tryGr, 'try-error')) lines(predict(gr[[i]],ssb=ssbRng)~ssbRng,col="red")
         
# C++ AD 
        model(ad[[i]]) =srModel(srType)
        if (srType=="bevholt") lower(ad[[i]])[]<-0
# Grad & func
        tryAG=try(ag[[i]]<-fmle(ad[[i]]))
        if(!is(tryAG, 'try-error')) lines(predict(ag[[i]],ssb=ssbRng)~ssbRng,col="pink")
# func
        gr(ad[[i]])<-function() NULL
        tryAD=try(ad[[i]]<-fmle(ad[[i]]))  
        if(!is(tryAD, 'try-error')) lines(predict(ad[[i]],ssb=ssbRng)~ssbRng,col="yellow")
}        
#######################################################################################################################
        }

    CF=data.frame(srr  =unlist(lapply(srr,logLik)),
                  nm   =unlist(lapply(nm, logLik)),
                 # ad   =unlist(lapply(ad, logLik)),
                 # ag   =unlist(lapply(ag, logLik)),
                 # gr   =unlist(lapply(gr, logLik)),
                  par  =unlist(lapply(pr, logLik)),
                  sv   =unlist(lapply(sv, logLik)))

    print(CF)
    return(list(srr=srr,nm=nm,#ad=ad,ag=ag,
                pr =pr, sv=sv))}

#### Check likelihood profiles
profileSRR=function(srr){
    par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
    for (i in names(srr)){
        profile(srr[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
        mtext(i,side=3, line=-2)}}
################################################################################

#### Beverton & Holt ###########################################################
bh=runCheckSRR(srr,srType="bevholt")
save(bh,file=paste(dirMy,"Data/bh.RData",  sep=""))
savePlot(paste(dirMy,"Tests/Figs/bh.png",sep=""),type="png")

dmns=list(stock=names(srr),method=names(bh))
bhLogLik<-array(unlist(lapply(bh, function(x) lapply(x, logLik))),
                      dim=unlist(lapply(dmns,length)),dimnames=dmns)
splom(bhLogLik)

profileSRR(bh[["nm"]])
savePlot(paste(dirMy,"Tests/Figs/bhProfile.png",sep=""),type="png")

### Ricker #####################################################################
rk=runCheckSRR(srr,srType="ricker")
save(rk,file=paste(dirMy,"Data/rk.RData",  sep=""))
savePlot(paste(dirMy,"Tests/Figs/rk.png",sep=""),type="png")

rkLogLik<-array(unlist(lapply(rk, function(x) lapply(x, logLik))),unlist(lapply(dmns,length)),dimnames=dmns)
splom(rkLogLik)

profileSRR(rk[["nm"]])
savePlot(paste(dirMy,"Tests/Figs/rkProfile.png",sep=""),type="png")

### Cushing ####################################################################
ch=runCheckSRR(srr,srType="cushing")
save(ch,file=paste(dirMy,"Data/ch.RData",  sep=""))
savePlot(paste(dirMy,"Tests/Figs/ch.png",sep=""),type="png")

chLogLik<-array(unlist(lapply(ch, function(x) lapply(x, logLik))),unlist(lapply(dmns,length)),dimnames=dmns)
splom(chLogLik)

profileSRR(ch[["nm"]])
savePlot(paste(dirMy,"Tests/Figs/chProfile.png",sep=""),type="png")

### Segmented Regression #######################################################
sg=runCheckSRR(srr,srType="segreg")
savePlot(paste(dirMy,"Tests/Figs/sg.png",sep=""),type="png")
save(sg,file=paste(dirMy,"Data/sg.RData",  sep=""))

sgLogLik<-array(unlist(lapply(sg, function(x) lapply(x, logLik))),unlist(lapply(dmns,length)),dimnames=dmns)
splom(sgLogLik)

profileSRR(sg[["nm"]])
savePlot(paste(dirMy,"Tests/Figs/sgProfile.png",sep=""),type="png")

### Shepherd ###################################################################
sh=runCheckSRR(srr,srType="shepherd")
savePlot(paste(dirMy,"Tests/Figs/sh.png",sep=""),type="png")
save(sh,file=paste(dirMy,"Data/sh.RData",  sep=""))
shLogLik<-array(unlist(lapply(sh, function(x) lapply(x, logLik))),unlist(lapply(dmns,length)),dimnames=dmns)
splom(shLogLik)

profileSRR(sh[["nm"]])
savePlot(paste(dirMy,"Tests/Figs/shProfile.png",sep=""),type="png")

#### Profile to check Third Shape Parameter
if (srType=="shepherd"){
   par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
   for (i in names(srr)){
       profile(srr[[i]],which=c("c"),xaxt ="n",yaxt="n")
       mtext(i,side=3, line=-2)}
savePlot(paste(dir,"Figs/",srType,"ProfileC.png",sep=""),type="png")
### Mean #######################################################################
mn=runCheckSRR(srr,srType="geomean")
savePlot(paste(dirMy,"Tests/Figs/mn.png",sep=""),type="png")
save(mn,file=paste(dirMy,"Data/mn.RData",  sep=""))
mnLogLik<-array(unlist(lapply(mn, function(x) lapply(x, logLik))),unlist(lapply(dmns,length)),dimnames=dmns)
mnLogLik

profileSRR(mn[["nm"]])
savePlot(paste(dirMy,"Tests/Figs/mnProfileAS.png",sep=""),type="png")

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


bhLogLik<-array(unlist(lapply(bh, function(x) lapply(x, logLik))),
                            dim     =unlist(lapply(dmns,length)),
                            dimnames=dmns)
                            
apply(bhLogLik,1,function(x) name(min))

