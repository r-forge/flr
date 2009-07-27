# plot - plot(FLPellaT)

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurence Kell
# Last Change: 26 Feb 2009 16:11

setMethod("plot", signature(x="FLPellaT", y="missing"),
p.<-  function(x, y, type=c("all","index","equil","ts","diag"),...){

      switch(as.character(type[1]),
             "equil"  =plot.e(x),
             "stock"  =plot.s(x),
             "harvest"=plot.h(x),
             "catch"  =plot.c(x),
             "diag"   =plot.d(x),
             "all"    =plot.a(x),
             stop("type must be ´all´, ´index´,´equil´,´ts´,´diag´!"))

      invisible()
		})

plot.u<-function(x){

   mnBio.      <-mnBio(c(stock(x)))
   index.      <-c(index(x))
   catchability<-calcQ(mnBio.,index.,error="LOG")
   indexHat.   <-catchability*mnBio.

   plot( c(index),xlab="Year",ylab="CPUE Index")
   lines(c(indexHat.))
   }
   
plot.s<-function(x){
   plot(stock(x),type="b",xlab="Year",ylab="Stock")
   }

plot.c<-function(x){
   plot(catch(x),type="b",,xlab="Year",ylab="Catch")
   }

plot.h<-function(x){
   h.<-catch(x)/stock(x)[,dimnames(catch(x))$year]

   plot(h.,type="b",xlab="Year",ylab="Harvest")
   }

plot.e<-function(x){
   r   <-x@param["r",   1]
   K   <-x@param["K",   1]
   mpar<-x@param["mpar",1]

   plot(computeCatch(x,seq(0,K,length.out=100))~seq(0,K,length.out=100),type="l",xlab="Stock",ylab="Yield",ylim=c(0,200),lwd=2,col="navy")
   points(catch(x)~stock(x)[,dimnames(catch(x))$year],type="b",lwd=2,pch=16)
   points(msy(x)["catch",1]~msy(x)["stock",1],type="p",cex=3,col="blue", pch=16)
   points(msy(x)["catch",1]~msy(x)["stock",1],type="p",cex=2,col="white",pch=16)
   points(msy(x)["catch",1]~msy(x)["stock",1],type="p",cex=1,col="red",  pch=16)
   }
   
plot.a<-function(x){

    plot.e(x)
    print(plot.s(x),split=c(1,1,2,2),more=T)
    print(plot.h(x),split=c(2,2,2,2),more=T)
    print(plot.c(x),split=c(2,2,3,3),more=T)
    }
    
plot.d<-function(x){
# 		yrs    <-range(x,"minyear"):(range(x,"maxyear"))
    indVar <-x@stock[,-dim(x@stock)[2]]
    indVar.<-FLQuant(seq(0, max(indVar), length=dim(indVar)[2]),dimnames=dimnames(indVar))
    obs    <-x@index #[,     ac(yrs)]
    resid  <-residuals(x) #[,ac(yrs)]

    mnBio.<-mnBio(c(x@stock))
    catchability<-calcQ(mnBio.,c(obs[,-dim(obs)[2]]),x@distribution)

    hat    <-FLQuant(catchability*mnBio.,dimnames=dimnames(obs))
    prd    <-indVar.*c(param(x)["q",])

    diagResidPlot(yrs,hat,indVar,indVar.,prd,obs,resid,xttl="Stock",yttl="CPUE",mttl="Index of abundance")
    }