# plot - plot(FLBioDym)

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurence Kell
# Last Change: 26 Feb 2009 16:11

setMethod("plot", signature(x="FLBioDym", y="missing"),
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
   plot(stock(x),type="b",xlab="Year",ylab="Stock",main="Stock Biomass")
   }

plot.c<-function(x){
   plot(catch(x),type="b",,xlab="Year",ylab="Catch",main="Catch Biomass")
   }

plot.h<-function(x){
   h.<-catch(x)/stock(x)[,dimnames(catch(x))$year]

   plot(h.,type="b",xlab="Year",ylab="Harvest",main="Exploitation Rate")
   }

plot.e<-function(x){
   r   <-x@params["r",   1]
   K   <-x@params["K",   1]
   mpar<-x@params["mpar",1]
   .ylim=c(0,max(msy(x)["catch",1],catch(x)))*1.1

computeCatch(x,seq(0,K,length.out=100))

   plot(computeCatch(x,seq(0,K,length.out=100))~seq(0,K,length.out=100),type="l",xlab="Stock",ylab="Yield",lwd=2,col="navy",ylim=.ylim)
   points(catch(x)~stock(x)[,dimnames(catch(x))$year],type="b",lwd=2,pch=16)
   points(msy(x)["catch",1]~msy(x)["stock",1],type="p",cex=3,col="blue", pch=16)
   points(msy(x)["catch",1]~msy(x)["stock",1],type="p",cex=2,col="white",pch=16)
   points(msy(x)["catch",1]~msy(x)["stock",1],type="p",cex=1,col="red",  pch=16)
   }
   
plot.a<-function(x){

    print(plot.c(x),split=c(1,3,1,3),more=T)
    print(plot.h(x),split=c(1,2,1,3),more=T)
    print(plot.s(x),split=c(1,1,1,3),more=F)
    }
    
plot.d<-function(x){
    yrs    <-ac(dimnames(x@index)$year)
    obs    <-x@index[,yrs]
    resid  <-residuals(x)[,yrs]
    indVar <-x@stock[,yrs]
    indVar.<-FLQuant(seq(0, max(indVar), length=dim(indVar)[2]),dimnames=dimnames(indVar))
    hat    <-sweep(x@stock[,yrs],6, x@params["q",], "*")
    prd    <-sweep(indVar.,6, x@params["q",], "*")

    diagResidPlot(hat,indVar,indVar.,prd,obs,resid,xttl="Stock",yttl="CPUE",mttl="Index of abundance")
    }