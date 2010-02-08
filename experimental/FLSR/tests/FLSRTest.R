library(ggplot2)
library(FLCore)

########################################################################################################################
########################################################################################################################
source("C:/Stuff/FLR/Experimental/FLSR/R/FLSR-Methods.R")


svPar<-expand.grid(Steepness=seq(0.5,1,length.out=6),v=c(1000))
ssb  <-seq(0,1020,length.out=101)

srr  <-"bevholt"
abPar<-ab(srr,0.5,s=svPar[,"Steepness"],v=svPar[,"v"])
t.   <-by(abPar,1:dim(abPar)[1],function(x) srrFunc(srr,ssb=ssb,a=x[,"a"],b=x[,"b"]))
t.   <-data.frame(cbind(s=rep(svPar[,"Steepness"],each=101),v=rep(svPar[,"v"],each=101),ssb=ssb,rec=unlist(t.)))
srr. <-cbind(t.,srr="Beverton and Holt")

srr  <-"ricker"
abPar<-ab(srr,0.5,s=svPar[,"Steepness"],v=svPar[,"v"])
t.   <-by(abPar,1:dim(abPar)[1],function(x) srrFunc(srr,ssb=ssb,a=x[,"a"],b=x[,"b"]))
t.   <-data.frame(cbind(s=rep(svPar[,"Steepness"],each=101),v=rep(svPar[,"v"],each=101),ssb=ssb,rec=unlist(t.)))
srr. <-rbind(srr.,cbind(t.,srr="Ricker"))

srr  <-"cushing"
abPar<-ab(srr,0.5,s=svPar[,"Steepness"],v=svPar[,"v"])
t.   <-by(abPar,1:dim(abPar)[1],function(x) srrFunc(srr,ssb=ssb,a=x[,"a"],b=x[,"b"]))
t.   <-data.frame(cbind(s=rep(svPar[,"Steepness"],each=101),v=rep(svPar[,"v"],each=101),ssb=ssb,rec=unlist(t.)))
srr. <-rbind(srr.,cbind(t.,srr="Cushing"))

srr  <-"shepherd"
abPar<-ab(srr,0.5,s=svPar[,"Steepness"],c=1,v=svPar[,"v"])
t.   <-by(abPar,1:dim(abPar)[1],function(x) srrFunc(srr,ssb=ssb,a=x[,"a"],b=x[,"b"],c=1))
t.   <-data.frame(cbind(s=rep(svPar[,"Steepness"],each=101),v=rep(svPar[,"v"],each=101),ssb=ssb,rec=unlist(t.)))
srr. <-rbind(srr.,cbind(t.,srr="Shepherd, c=1"))

srr  <-"shepherd"
abPar<-ab(srr,0.5,s=svPar[,"Steepness"],c=3,v=svPar[,"v"])
t.   <-by(abPar,1:dim(abPar)[1],function(x) srrFunc(srr,ssb=ssb,a=x[,"a"],b=x[,"b"],c=3))
t.   <-data.frame(cbind(s=rep(svPar[,"Steepness"],each=101),v=rep(svPar[,"v"],each=101),ssb=ssb,rec=unlist(t.)))
srr. <-rbind(srr.,cbind(t.,srr="Shepherd, c=3"))

srr  <-"segreg"
abPar<-ab(srr,0.5,s=svPar[,"Steepness"],v=svPar[,"v"])
t.   <-by(abPar,1:dim(abPar)[1],function(x) srrFunc(srr,ssb=ssb,a=x[,"a"],b=x[,"b"]))
t.   <-data.frame(cbind(s=rep(svPar[,"Steepness"],each=101),v=rep(svPar[,"v"],each=101),ssb=ssb,rec=unlist(t.)))
srr. <-rbind(srr.,cbind(t.,srr="Segmented Regression"))

p    <- ggplot(srr.,aes(x=ssb, y=rec, group=s)) + facet_wrap(~srr,scale="free")
p    <- p + geom_line(aes(colour=s)) + geom_hline(aes(yintercept=svPar[,"Steepness"]*2000)) + geom_vline(aes(xintercept=200))
p

p    <- ggplot(srr.,aes(x=ssb, y=rec/ssb, group=s)) + facet_wrap(~srr,scale="free")
p    <- p + geom_line(aes(colour=s)) + scale_y_continuous(limits=c(0,15))
p


########################################################################################################################
########################################################################################################################

source("C:/Stuff/FLR/Experimental/FLSR/R/FLSR-srModel.R")
source("C:/Stuff/FLR/Experimental/FLSR/R/FLSR-plotDiagnostics.R")
source("C:/Stuff/FLR/Experimental/FLSR/R/FLSR-lowess.R")
source("C:/Stuff/FLR/Experimental/FLSR/R/FLSR-predict.R")
source("C:/Stuff/FLR/Experimental/FLSR/R/FLSR-deprecated.R")
source("C:/Stuff/FLR/Experimental/FLSR/R/FLSR-Methods.R")

model(nsher)<-bevholt()
nshBH<-fmle(nsher)
plot(nshBH)

model(nsher)<-ricker()
nshRK<-fmle(nsher)
plot(nshRK)

model(nsher)<-cushing()
nshCH<-fmle(nsher)
plot(nshCH)

model(nsher)<-shepherd()
nshSH<-fmle(nsher)
plot(nshSH)

model(nsher)<-segreg()
nshSG<-fmle(nsher)
plot(nshSG)

ssb<-FLQuant(seq(0,max(ssb(nsher)),length.out=dim(ssb(nsher))[2]))
srr.<-model.frame(FLQuants("BevHolt"  =predict(nshBH,ssb=ssb),
                           "Ricker"   =predict(nshRK,ssb=ssb),
                           "Cushing"  =predict(nshCH,ssb=ssb),
                           "Shepherd" =predict(nshSH,ssb=ssb),
                           "SegReg"   =predict(nshSG,ssb=ssb),
                           SSB        =ssb(nsher),
                           Recruits   =rec(nsher)))[,7:13]

srr.$ssbHat<-seq(0,max(ssb(nsher)),length.out=dim(srr.)[1])

p    <- ggplot(srr.,aes(x=SSB, y=Recruits)) + geom_point(aes(x=SSB,y=Recruits))
p    <- p + scale_y_continuous(limits=c(0,max(srr.$Recruits)))+ scale_x_continuous(limits=c(0,max(srr.$SSB)))
p    <- p + geom_line(aes(ssbHat,BevHolt))
p    <- p + geom_line(aes(ssbHat,Ricker))
p    <- p + geom_line(aes(ssbHat,Cushing))
p    <- p + geom_line(aes(ssbHat,SegReg))

p    <- ggplot(srr.) + geom_point(aes(x=SSB,y=Recruits/SSB))
p    <- p + scale_y_continuous(limits=c(0,max(srr.$Recruits/srr.$SSB)))+ scale_x_continuous(limits=c(0,max(srr.$SSB)))
p    <- p + geom_line(aes(ssbHat,BevHolt/ssbHat))
p    <- p + geom_line(aes(ssbHat, Ricker/ssbHat))
p    <- p + geom_line(aes(ssbHat,Cushing/ssbHat))
p    <- p + geom_line(aes(ssbHat, SegReg/ssbHat))

