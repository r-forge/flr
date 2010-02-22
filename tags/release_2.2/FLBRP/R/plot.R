# plot - plot(FLBRP)

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurence Kell, Cefas & Santiago Cerviño, IEO
# Last Change: 26 Feb 2009 16:11
# $Id$

setMethod("plot", signature(x="FLBRP", y="missing"),
p.<-  function(x, y, type=c("all", "yield.harvest", "ssb.harvest", "rec.ssb", "yield.ssb", "profit.harvest", "profit.ssb", ""),cols=c(1, 2, 8, 5, 6, 10, 12),obs=FALSE,refpts=TRUE,ts="missing",...){

      lim.h=c(0, ifelse(all(is.na(fbar(  x)[fbar(  x)>0])), NA,    max(fbar(  x), na.rm=TRUE)))
      lim.y=c(0, ifelse(all(is.na(yield( x)[yield( x)>0])), NA,    max(yield( x), na.rm=TRUE)))
      lim.p=c(0, ifelse(all(is.na(profit(x)[profit(x)>0])), NA,    max(profit(x), na.rm=TRUE)))
      lim.s=c(0, ifelse(all(is.na(ssb(   x)[ssb(   x)>0])), NA,    max(ssb(   x), na.rm=TRUE)))
      lim.r=c(0, ifelse(all(is.na(rec(   x)[ssb(   x)>0])), NA,    max(rec(   x), na.rm=TRUE)))

      if (obs)
         {
         if (length(fbar.obs(x))  > 0 && !all(is.na(fbar.obs(  x)))) lim.h=c(0, max(c(fbar.obs(x),   lim.h), na.rm=TRUE))
         if (length(profit.obs(x))> 0 && !all(is.na(profit.obs(x)))) lim.p=c(0, max(c(profit.obs(x), lim.p), na.rm=TRUE))
         if (length(yield.obs(x)) > 0 && !all(is.na(yield.obs( x)))) lim.y=c(0, max(c(yield.obs(x),  lim.y), na.rm=TRUE))
         if (length(ssb.obs(x))   > 0 && !all(is.na(ssb.obs(   x)))) lim.s=c(0, max(c(ssb.obs(x),    lim.s), na.rm=TRUE))
         if (length(rec.obs(x))   > 0 && !all(is.na(rec.obs(   x)))) lim.r=c(0, max(c(rec.obs(x),    lim.r), na.rm=TRUE))
         }

      if (!missing(ts))
          {
          if (is(ts,"FLQuants"))
             ts<-model.frame(ts)
          if (!is(ts,"data.frame"))
             stop("ts has to be either data.frame or FLQuants")

          if (("fbar"   %in% names(ts))) if (!all(is.na(ts[,"fbar"]  ))) lim.h=c(0, max(c(ts[,"fbar"],   lim.h), na.rm=TRUE))
          if (("profit" %in% names(ts))) if (!all(is.na(ts[,"profit"]))) lim.y=c(0, max(c(ts[,"profit"], lim.y), na.rm=TRUE))
          if (("yield"  %in% names(ts))) if (!all(is.na(ts[,"yield"] ))) lim.y=c(0, max(c(ts[,"yield"],  lim.y), na.rm=TRUE))
          if (("ssb"    %in% names(ts))) if (!all(is.na(ts[,"ssb"]   ))) lim.s=c(0, max(c(ts[,"ssb"],    lim.s), na.rm=TRUE))
          if (("rec"    %in% names(ts))) if (!all(is.na(ts[,"rec"]   ))) lim.r=c(0, max(c(ts[,"rec"],    lim.r), na.rm=TRUE))
          }

      switch(as.character(type[1]),
             "yield.harvest" =plot.y.h(x,lim.y,lim.h,cols,refpts,obs,ts),
             "ssb.harvest"   =plot.s.h(x,lim.s,lim.h,cols,refpts,obs,ts),
             "rec.ssb"       =plot.r.s(x,lim.r,lim.s,cols,refpts,obs,ts),
             "yield.ssb"     =plot.y.s(x,lim.y,lim.s,cols,refpts,obs,ts),
             "profit.ssb"    =plot.p.s(x,lim.p,lim.s,cols,refpts,obs,ts),
             "profit.harvest"=plot.p.h(x,lim.p,lim.h,cols,refpts,obs,ts),
             "all"           =plot.all(x,lim.h,lim.y,lim.p,lim.s,lim.r,cols,refpts,obs,ts),
             stop("type must be 'all', 'yield.harvest', 'ssb.harvest', 'rec.ssb', 'yield.ssb', 'profit.ssb', 'profit.harvest'!"))

      invisible()
		}
   )

plot.p.h<-function(x,ylim,xlim,cols,refpts,obs,ts)
         {
         plot(fbar(x),profit.hat(x), xlim=xlim, ylim=ylim, type="l",col=cols[1],
               xlab="Fishing Mortality",
               ylab="profit",
               main="Equilibrium profit v F")

         if (refpts)
           {
           points(refpts(x)[,"harvest",],refpts(x)[,"profit",],pch=19,col=cols[1],cex=1.5)
     		   points(refpts(x)[,"harvest",],refpts(x)[,"profit",],pch=19,col=cols[3],cex=1.2)
     		   }

         if (obs)
	         {
         	 lines( fbar.obs(x),profit.obs(x),col=cols[4])
           points(fbar.obs(x),profit.obs(x),col=cols[5],pch=19)
           }

         if (!missing(ts) && all(c("fbar","profit") %in% names(ts)))
	         {
         	 lines( ts[,"fbar"],ts[,"profit"],col=cols[6])
           points(ts[,"fbar"],ts[,"profit"],col=cols[7],pch=19)
           }

        invisible()
	      }

      plot.r.h<-function(x,ylim,xlim,cols,refpts,obs,ts)
         {
         plot(fbar(x), rec(x), xlim=xlim, ylim=ylim, type="l",col=cols[1],
               xlab="Fishing Mortality",
               ylab="Recruits",
               main="Equilibrium Recruits v F")

         if (refpts)
            {
            points(refpts(x)[,"harvest",],refpts(x)[,"rec",],pch=19,col=cols[1],cex=1.5)
			      points(refpts(x)[,"harvest",],refpts(x)[,"rec",],pch=19,col=cols[3],cex=1.2)
            }

         if (obs)
	         {
         	  lines( fbar.obs(x),rec.obs(x),col=cols[4])
            points(fbar.obs(x),rec.obs(x),col=cols[5],pch=19)
            }

         if (!missing(ts) && all(c("fbar","rec") %in% names(ts)))
	         {
         	 lines( ts[,"fbar"],ts[,"rec"],col=cols[6])
           points(ts[,"fbar"],ts[,"rec"],col=cols[7],pch=19)
           }

         invisible()
	      }

plot.p.s<-function(x,ylim,xlim,cols,refpts,obs,ts)
         {
         plot(ssb(x), profit(x), xlim=xlim, ylim=ylim, type="l",col=cols[1],
               xlab="SSB",
               ylab="profit",
               main="Equilibrium profit v SSB")

         if (refpts)
            {
            points(refpts(x)[,"ssb",],refpts(x)[,"profit",],pch=19,col=cols[1],cex=1.5)
   			    points(refpts(x)[,"ssb",],refpts(x)[,"profit",],pch=19,col=cols[3],cex=1.2)
            }

         if (obs)
	         {
         	 lines( ssb.obs(x),profit.obs(x),col=cols[4])
           points(ssb.obs(x),profit.obs(x),col=cols[5],pch=19)
           }

         if (!missing(ts) && all(c("ssb","profit") %in% names(ts)))
	         {
         	 lines( ts[,"ssb"],ts[,"profit"],col=cols[6])
           points(ts[,"ssb"],ts[,"profit"],col=cols[7],pch=19)
           }

			invisible()
      }

plot.all<-function(x,lim.h,lim.y,lim.p,lim.s,lim.r,cols,refpts,obs,ts)
     {
     par.mfrow<-par()$mfrow

		 if (all(is.na(profit.hat(x)))){
			 par(mfrow=c(2,2))
			 plot.s.h(x,lim.s,lim.h,cols,refpts,obs,ts)
			 plot.r.s(x,lim.r,lim.s,cols,refpts,obs,ts)
			 plot.y.h(x,lim.y,lim.h,cols,refpts,obs,ts)
			 plot.y.s(x,lim.y,lim.s,cols,refpts,obs,ts)
			 }
     else {
			 par(mfrow=c(3,2))
			 plot.s.h(x,lim.s,lim.h,cols,refpts,obs,ts)
			 plot.r.s(x,lim.r,lim.s,cols,refpts,obs,ts)
			 plot.y.h(x,lim.y,lim.h,cols,refpts,obs,ts)
			 plot.y.s(x,lim.y,lim.s,cols,refpts,obs,ts)
			 plot.p.h(x,lim.p,lim.h,cols,refpts,obs,ts)
			 plot.p.s(x,lim.p,lim.s,cols,refpts,obs,ts)
			 }

		 invisible()

		 par(mfrow=par.mfrow)
     }

 		plot.y.h<-function(x,ylim,xlim,cols,refpts,obs,ts)
         {
         plot(fbar(x),yield(x), xlim=xlim, ylim=ylim, type="l",col=cols[1],
               xlab="Fishing Mortality",
                 ylab="Yield",
               main="Equilibrium Yield v F")

         if (refpts)
            {
            points(refpts(x)[,"harvest",],refpts(x)[,"yield",],pch=19,col=cols[1],cex=1.5)
			      points(refpts(x)[,"harvest",],refpts(x)[,"yield",],pch=19,col=cols[3],cex=1.2)
            }

         if (obs)
	          {
       	    lines( fbar.obs(x),yield.obs(x),col=cols[4])
            points(fbar.obs(x),yield.obs(x),col=cols[5],pch=19)
            }

         if (!missing(ts) && all(c("fbar","yield") %in% names(ts)))
	         {
         	 lines( ts[,"fbar"],ts[,"yield"],col=cols[6])
           points(ts[,"fbar"],ts[,"yield"],col=cols[7],pch=19)
           }

        invisible()
	      }

 	plot.s.h<-function(x,ylim,xlim,cols,refpts,obs,ts)
         {
         plot(fbar(x), ssb(x), xlim=xlim, ylim=ylim, type="l",col=cols[1],
               xlab="Fishing Mortality",
               ylab="SSB",
               main="Equilibrium SSB v F")

         if (refpts)
            {
            points(refpts(x)[,"harvest",],refpts(x)[,"ssb",],pch=19,col=cols[1],cex=1.5)
     	      points(refpts(x)[,"harvest",],refpts(x)[,"ssb",],pch=19,col=cols[3],cex=1.2)
            }

         if (obs)
	         {
           lines( fbar.obs(x),ssb.obs(x),col=cols[4])
           points(fbar.obs(x),ssb.obs(x),col=cols[5],pch=19)
           }

        if (!missing(ts) && all(c("fbar","ssb") %in% names(ts)))
	         {
         	 lines( ts[,"fbar"],ts[,"ssb"],col=cols[6])
           points(ts[,"fbar"],ts[,"ssb"],col=cols[7],pch=19)
           }

         invisible()
	      }

plot.y.s<-function(x,ylim,xlim,cols,refpts,obs,ts)
         {
         plot(ssb(x), yield(x), xlim=xlim, ylim=ylim, type="l",col=cols[1],
               xlab="SSB",
               ylab="Yield",
               main="Equilibrium Yield v SSB")

         if (refpts)
            {
            points(refpts(x)[,"ssb",],refpts(x)[,"yield",],pch=19,col=cols[1],cex=1.5)
			      points(refpts(x)[,"ssb",],refpts(x)[,"yield",],pch=19,col=cols[3],cex=1.2)
            }

         if (obs)
	         {
          	lines( ssb.obs(x),yield.obs(x),col=cols[4])
            points(ssb.obs(x),yield.obs(x),col=cols[5],pch=19)
            }

         if (!missing(ts) && all(c("ssb","yield") %in% names(ts)))
	         {
         	 lines( ts[,"ssb"],ts[,"yield"],col=cols[6])
           points(ts[,"ssb"],ts[,"yield"],col=cols[7],pch=19)
           }

			invisible()
      }

plot.r.s<-function(x,ylim,xlim,cols,refpts,obs,ts)
         {
         plot(ssb(x), rec(x), xlim=xlim, ylim=ylim, type="l",col=cols[1],
               xlab="SSB",
               ylab="Recruits",
               main="Equilibrium Recruits v SSB")

         if (refpts)
            {
            points(refpts(x)[,"ssb",],refpts(x)[,"rec",],pch=19,col=cols[1],cex=1.5)
			      points(refpts(x)[,"ssb",],refpts(x)[,"rec",],pch=19,col=cols[3],cex=1.2)
            }

         if (obs)
	         {
       	   lines( ssb.obs(x),rec.obs(x),col=cols[4])
           points(ssb.obs(x),rec.obs(x),col=cols[5],pch=19)
           }

        if (!missing(ts) && all(c("ssb","rec") %in% names(ts)))
	         {
         	 lines( ts[,"rec"],ts[,"ssb"],col=cols[6])
           points(ts[,"rec"],ts[,"ssb"],col=cols[7],pch=19)
           }

         invisible()
	      }

## Bivariate CI
bivariateOrder<-function(dt)
    {
    mn      <-apply(dt,2,mean)
    dt[,1]  <-dt[,1]-mn[1]
    dt[,2]  <-dt[,2]-mn[2]
    t.      <-var(dt)
    cholesky<-chol(t.)
    dt      <-as.matrix(dt)%*%ginv(cholesky)
    dist    <-dt[,1]^2+dt[,2]^2

    return(order(dist))
    }

plotBeer=function(v){
  	usr=par("usr")
  	ymin=usr[3]
  	ymax=usr[4]
  	yrng=ymax-ymin
  	ymin1=usr[3]+.1*yrng
  	ymax1=usr[4]-.2*yrng

  	yrng1=ymax1-ymin1
  	ymax2=ymin1+abs(v)*yrng1
  	xmid=(usr[2]+usr[1])/2
  	ymid=(ymax1+ymin1)/2
  	xrng=(usr[2]-usr[1])
  	xpoly=c(xmid-.15*xrng,xmid-(.15+abs(v)*.1)*xrng,xmid+(.15+abs(v)*.1)*xrng,xmid+.15*xrng,xmid-.15*xrng)
  	ypoly=c(ymin1,ymax2,ymax2,ymin1,ymin1)
  	polygon(xpoly,ypoly,col="gold",border="burlywood")
  	bubblex=runif(round(500*abs(v),0),xmid-(.15+abs(v*.95)*.1)*xrng,xmid+(.15+abs(v*.95)*.1)*xrng)
  	bubbley=runif(round(500*abs(v),0),ymax2-.02*yrng1,ymax2+.02*yrng1)
  	points(bubblex,bubbley,pch=21,col = "gold", bg = "white",cex=seq(0.1,1,length=10))
  	points(c(xmid-.15*xrng,xmid+.15*xrng),c(ymin1,ymin1),type="l",lwd=4)
  	points(c(xmid-.15*xrng,xmid-.25*xrng),c(ymin1,ymax1),type="l",lwd=4)
  	points(c(xmid+.15*xrng,xmid+.25*xrng),c(ymin1,ymax1),type="l",lwd=4)
  	if(v<0){
  		text(xmid,ymid,labels=c(paste("-",abs(v))),cex=.5+abs(v)*2)
  	}else{
  		text(xmid,ymid,labels=c(paste("+",abs(v))),cex=.5+abs(v)*2)
      }
    }

##### Tapas is the Kobe plot and Rasta is the time series summary of the p() of being in a kobe quadrant
##### Kobematrix is a set of overlaoded methods to produce Kode plots and variants by a range of differeing objects

## Exported method
#setMethod("plot", signature(x="FLBRP", y="FLStock"),

## Internal function that calls Tapas2 to make Kobe plot
#plotTapasFLBRPFLStock<-function(x,y,yr=NULL,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab=expression(F:F[MSY]),maxX=NULL,maxY=NULL,axs=TRUE)

## Internal function that makes the Kobe plot
#plotTapas2<-function(x,y,ssbPts=NULL,fbrPts=NULL,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab=expression(F:F[MSY]),maxX=NULL,maxY=NULL,axs=TRUE)

## Internal function that calls Rasta2
#plotRastaFLBRPFLStock<-function(x,y,title=""){

## Internal function that makes the Kobe summary of P() of eing in a Kobe quadrant over time
#plotRasta2<-function(ssbTrk,fbrTrk,title){


setMethod("plot", signature(x="FLBRP", y="FLStock"),
    function(x,y,type="tapas",yr=NULL,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab=expression(F:F[MSY]),maxX=NULL,maxY=NULL,axs=TRUE){

    if (type=="tapas")
       plotTapasFLBRPFLStock(x,y,yr=yr,title=title,biCol=biCol,xlab=xlab,ylab=ylab,maxX=maxX,maxY=maxY,axs=axs)
    else if (type=="rasta")
       plotRastaFLBRPFLStock(x,y)
    else stop("type has to be \"rasta\" or \"tapas\"")
    })


plotTapasFLBRPFLStock<-function(x,y,yr=NULL,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab=expression(F:F[MSY]),maxX=NULL,maxY=NULL,axs=TRUE)
    {
    x     <-refpts(x)["msy",,]
    y     <-window(y,start=range(y,"minyear"),end=range(y,"maxyear"))

    ssbPts<-ssb( y)/c(x[,"ssb",    ])
    fbrPts<-fbar(y)/c(x[,"harvest",])

    ssbTrk<-quantile(ssbPts,probs=0.5)
    fbrTrk<-quantile(fbrPts,probs=0.5)

    if (!is.null(yr)){
       ssbTrk<-FLQuants(window(ssbTrk,end  =yr),
                        window(ssbTrk,start=yr))

       fbrTrk<-FLQuants(window(fbrTrk,end  =yr),
                        window(fbrTrk,start=yr))
       }
    else{
       ssbTrk<-FLQuants(ssbTrk)
       fbrTrk<-FLQuants(fbrTrk)
       }

    ssbPts<-ssbPts[,ac(range(y,"maxyear"))]
    fbrPts<-fbrPts[,ac(range(y,"maxyear"))]

    plotTapas2(ssbTrk,fbrTrk,ssbPts=ssbPts,fbrPts=fbrPts,title=title,biCol=biCol,xlab=xlab,ylab=ylab,maxX=maxX,maxY=maxY,axs=axs)
    }

plotRastaFLBRPFLStock<-function(x,y,title=""){
    x<-refpts(x)["msy",,]

    ssbTrk<-ssb( y)/c(x[,"ssb",    ])
    fbrTrk<-fbar(y)/c(x[,"harvest",])

    plotRasta2(ssbTrk,fbrTrk,title=title)
    }

plotRasta2<-function(ssbTrk,fbrTrk,title){
    red<-FLQuant(0,dimnames=dimnames(ssbTrk))
    grn<-red
    ylw<-red

    red[ssbTrk< 1 & fbrTrk> 1]<-1
    grn[ssbTrk>=1 & fbrTrk<=1]<-1
    ylw[red   ==0 & grn   ==0]<-1

#    xyplot(data~year,groups=qname,data=lapply(FLQuants(green=green,red=red,yellow=yellow),function(x) apply(x,2,mean)),
#              col=c("green","red","yellow"),lwd=4,type="l",xlab="Year",ylab="Probability")
    plot( apply(red,2,mean)[,,drop=T]~dimnames(red)$year,col="red",   lwd=4,type="l",xlab="Year",ylab="Probability",ylim=c(0,1))
    lines(apply(grn,2,mean)[,,drop=T]~dimnames(grn)$year,col="green", lwd=4)
    lines(apply(ylw,2,mean)[,,drop=T]~dimnames(ylw)$year,col="yellow",lwd=4)
    }

plotTapas2<-function(x,y,ssbPts=NULL,fbrPts=NULL,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab=expression(F:F[MSY]),maxX=NULL,maxY=NULL,axs=TRUE)
    {
    fish.pg<-function(maxX,maxY)
        {
        polygon(x=c(-0.5,1,1,-0.5),            y=c(1.0,1.0,maxY+.5,maxY+.5),    col="red2")
        polygon(x=c(1.0,maxX+0.5,maxX+0.5,1.0),y=c(-0.5,-0.5,1.0,1.0),          col="lightgreen")
        polygon(x=c(-0.5,1,1,-0.5),            y=c(-0.5,-0.5,1.0,1.0),          col="lightgoldenrod1")
        polygon(x=c(1.0,maxX+0.5,maxX+0.5,1.0),y=c(1.0,1.0,maxY+.5,maxY+.5),    col="lightgoldenrod1")
        }

    maxX.<-max(unlist(lapply(x,max)),ssbPts,na.rm=T)
    if (is.null(maxX))
       maxX<-maxX.

    maxY.<-max(unlist(lapply(y,max)),fbrPts,na.rm=T)
    if (is.null(maxY))
       maxY<-maxY.

    cols<-gray(0:length(x) / length(x))
    
    plot(y[[1]]~x[[1]], col=cols, type="l", lwd=2, xlim=c(0,maxX),ylim=c(0,maxY),xlab=xlab,ylab=ylab,main=paste(title),axes=axs)
    fish.pg(maxX,maxY)
    abline(h=1.0,v=1,col="grey")

    if (!is.null(ssbPts) & !is.null(fbrPts)){
       t.  <-bivariateOrder(cbind(fbrPts,ssbPts))
       col.<-rep(biCol,each=as.integer(length(t.)/length(biCol)))
       points(c(fbrPts[,,,,,t.])~c(ssbPts[,,,,,t.]), col=col.,pch=19,cex=0.75)
       lines(y[[1]]~x[[1]], col=cols,lwd=2)
       }

    if (length(x)==length(y) & length(y)>1)
    for (i in 2:length(y))
       lines(x[[i]],y[[i]],col=cols[i],lwd=2)
    }


