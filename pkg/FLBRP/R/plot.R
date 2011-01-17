# plot - plot(FLBRP)

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurence Kell, Cefas & Santiago Cerviño, IEO
# Last Change: 23 Apr 2010 09:46
# $Id$

setMethod("plot", signature(x="FLBRP", y="missing"),
  function(x, y, type=c("all", "yield.harvest", "ssb.harvest", "rec.ssb", "yield.ssb",
    "profit.harvest", "profit.ssb", ""), cols=rainbow(dim(refpts(x))[1]),
    obs=FALSE,refpts=TRUE,ts="missing",...)
  {
    lim.h=c(0, ifelse(all(is.na(fbar(x)[fbar(x)>0])), NA, max(fbar(x), na.rm=TRUE)))
    lim.y=c(0, ifelse(all(is.na(yield(x)[yield(x)>0])), NA, max(yield(x), na.rm=TRUE)))
    lim.p=c(0, ifelse(all(is.na(profit(x)[profit(x)>0])), NA, max(profit(x), na.rm=TRUE)))
    lim.s=c(0, ifelse(all(is.na(ssb(x)[ssb(x)>0])), NA, max(ssb(x), na.rm=TRUE)))
    lim.r=c(0, ifelse(all(is.na(rec(x)[ssb(x)>0])), NA, max(rec(x), na.rm=TRUE)))

    if (obs)
    {
      if (length(fbar.obs(x))  > 0 && !all(is.na(fbar.obs(  x))))
        lim.h=c(0, max(c(fbar.obs(x),   lim.h), na.rm=TRUE))
      if (length(profit.obs(x))> 0 && !all(is.na(profit.obs(x))))
        lim.p=c(0, max(c(profit.obs(x), lim.p), na.rm=TRUE))
      if (length(yield.obs(x)) > 0 && !all(is.na(yield.obs( x))))
        lim.y=c(0, max(c(yield.obs(x),  lim.y), na.rm=TRUE))
      if (length(ssb.obs(x)) > 0 && !all(is.na(ssb.obs(   x))))
        lim.s=c(0, max(c(ssb.obs(x),    lim.s), na.rm=TRUE))
      if (length(rec.obs(x)) > 0 && !all(is.na(rec.obs(   x))))
        lim.r=c(0, max(c(rec.obs(x),    lim.r), na.rm=TRUE))
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
         plot(fbar(x),profit.hat(x), xlim=xlim, ylim=ylim, type="l",col='black',
               xlab="Fishing Mortality",
               ylab="profit",
               main="Equilibrium profit v F")

         if (refpts)
           {
            ver <- refpts(x)[,"harvest",]
            ver <- ver[!is.na(ver)]
            for(i in seq(length(ver)))
     	        abline(v=ver[i], lty=2, col=cols[i])
     		   points(refpts(x)[,"harvest",],refpts(x)[,"profit",],pch=19,col=cols,cex=1.2)
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
         plot(fbar(x), rec(x), xlim=xlim, ylim=ylim, type="l",col='black',
               xlab="Fishing Mortality",
               ylab="Recruits",
               main="Equilibrium Recruits v F")

         if (refpts)
            {
            ver <- refpts(x)[,"harvest",]
            ver <- ver[!is.na(ver)]
            for(i in seq(length(ver)))
     	        abline(v=ver[i], lty=2, col=cols[i])
			      points(refpts(x)[,"harvest",],refpts(x)[,"rec",],pch=19,col=cols,cex=1.2)
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
         plot(ssb(x), profit(x), xlim=xlim, ylim=ylim, type="l",col='black',
               xlab="SSB",
               ylab="profit",
               main="Equilibrium profit v SSB")

         if (refpts)
            {
            ver <- refpts(x)[,"ssb",]
            ver <- ver[!is.na(ver)]
            for(i in seq(length(ver)))
     	        abline(v=ver[i], lty=2, col=cols[i])
   			    points(refpts(x)[,"ssb",],refpts(x)[,"profit",],pch=19,col=cols,cex=1.2)
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
       l <- layout(matrix(c(1, 2, 3, 4, 5, 5), ncol=2, byrow=T), widths=c(1,1),
           heights=c(4,4,1))
       par(mar = c(2,2,2,2))
			 plot.s.h(x,lim.s,lim.h,cols,refpts,obs,ts)
			 plot.r.s(x,lim.r,lim.s,cols,refpts,obs,ts)
			 plot.y.h(x,lim.y,lim.h,cols,refpts,obs,ts)
			 plot.y.s(x,lim.y,lim.s,cols,refpts,obs,ts)
       plot(0, type='n', axes=FALSE)
       legend('center', '(x,y)', dimnames(refpts(x))[[1]], col=cols, pch=19, bty='n',
           cex=1.2, ncol=dim(refpts(x))[1])
			 }
     else {
       l <- layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 7), ncol=2, byrow=T), widths=c(1,1),
           heights=c(3,3,3,1))
       par(mar = c(2,2,2,2))
			 plot.s.h(x,lim.s,lim.h,cols,refpts,obs,ts)
			 plot.r.s(x,lim.r,lim.s,cols,refpts,obs,ts)
			 plot.y.h(x,lim.y,lim.h,cols,refpts,obs,ts)
			 plot.y.s(x,lim.y,lim.s,cols,refpts,obs,ts)
			 plot.p.h(x,lim.p,lim.h,cols,refpts,obs,ts)
			 plot.p.s(x,lim.p,lim.s,cols,refpts,obs,ts)
       plot(0, type='n', axes=FALSE)
       legend('center', '(x,y)', dimnames(refpts(x))[[1]], col=cols, pch=19, bty='n',
           cex=1.2, ncol=dim(refpts(x))[1])
			 }

		 invisible()

		 par(mfrow=par.mfrow)
     }

 		plot.y.h<-function(x,ylim,xlim,cols,refpts,obs,ts)
         {
         plot(fbar(x),yield(x), xlim=xlim, ylim=ylim, type="l",col='black',
               xlab="Fishing Mortality",
                 ylab="Yield",
               main="Equilibrium Yield v F")

         if (refpts)
            {
            ver <- refpts(x)[,"harvest",]
            ver <- ver[!is.na(ver)]
            for(i in seq(length(ver)))
     	        abline(v=ver[i], lty=2, col=cols[i])
			      points(refpts(x)[,"harvest",],refpts(x)[,"yield",],pch=19,col=cols,cex=1.2)
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
         plot(fbar(x), ssb(x), xlim=xlim, ylim=ylim, type="l",col='black',
               xlab="Fishing Mortality",
               ylab="SSB",
               main="Equilibrium SSB v F")

         if (refpts)
            {
            ver <- refpts(x)[,"harvest",]
            ver <- ver[!is.na(ver)]
            for(i in seq(length(ver)))
     	        abline(v=ver[i], lty=2, col=cols[i])
     	      points(refpts(x)[,"harvest",],refpts(x)[,"ssb",],pch=19,col=cols,cex=1.2)
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
         plot(ssb(x), yield(x), xlim=xlim, ylim=ylim, type="l",col='black',
               xlab="SSB",
               ylab="Yield",
               main="Equilibrium Yield v SSB")

         if (refpts)
            {
            ver <- refpts(x)[,"ssb",]
            ver <- ver[!is.na(ver)]
            for(i in seq(length(ver)))
     	        abline(v=ver[i], lty=2, col=cols[i])
			      points(refpts(x)[,"ssb",],refpts(x)[,"yield",],pch=19,col=cols,cex=1.2)
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
         plot(ssb(x), rec(x), xlim=xlim, ylim=ylim, type="l",col='black',
               xlab="SSB",
               ylab="Recruits",
               main="Equilibrium Recruits v SSB")

         if (refpts)
            {
            ver <- refpts(x)[,"ssb",]
            ver <- ver[!is.na(ver)]
            for(i in seq(length(ver)))
     	        abline(v=ver[i], lty=2, col=cols[i])
			      points(refpts(x)[,"ssb",],refpts(x)[,"rec",],pch=19,col=cols,cex=1.2)
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
