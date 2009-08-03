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

plotRasta<-function(x,y){
    ssbTrk<-ssb( x)/c(y[,"ssb",    ])
    fbrTrk<-fbar(x)/c(y[,"harvest",])

    red   <-FLQuant(0,dimnames=dimnames(ssbTrk))
    green <-red
    yellow<-red

    red[   ssbTrk< 1 & fbrTrk> 1]<-1
    green[ ssbTrk>=1 & fbrTrk<=1]<-1
    yellow[red   ==0 & green ==0]<-1

    print(xyplot(data~year,groups=qname,data=lapply(FLQuants(green=green,red=red,yellow=yellow),function(x) apply(x,2,mean)),
               col=c("green","red","yellow"),lwd=3,type="l",xlab="Year",ylab="Probability of being in the Zone"))
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
    
plotTapas<-function(x,y,title="",biCol=c("grey50","grey75"),xlab="SSB",ylab="F",maxX=NULL,maxY=NULL,year,axs=TRUE)
    {
    x     <-window(x,start=range(x,"minyear"),end=range(x,"maxyear"))
    ssbPts<-ssb( x)/c(y[,"ssb",    ])
    fbrPts<-fbar(x)/c(y[,"harvest",])

    ssbTrk<-quantile(ssbPts,0.5)
    fbrTrk<-quantile(fbrPts,0.5)

    ssbPts<-ssbPts[,ac(range(x,"maxyear"))]
    fbrPts<-fbrPts[,ac(range(x,"maxyear"))]

    fish.pg<-function(maxX,maxY)
        {
        polygon(x=c(-0.5,1,1,-0.5),            y=c(1.0,1.0,maxY+.5,maxY+.5),    col="red2")
        polygon(x=c(1.0,maxX+0.5,maxX+0.5,1.0),y=c(-0.5,-0.5,1.0,1.0),          col="lightgreen")
        polygon(x=c(-0.5,1,1,-0.5),            y=c(-0.5,-0.5,1.0,1.0),          col="lightgoldenrod1")
        polygon(x=c(1.0,maxX+0.5,maxX+0.5,1.0),y=c(1.0,1.0,maxY+.5,maxY+.5),    col="lightgoldenrod1")
        }

    if (is.null(maxX))
       maxX<-max(     ssbTrk,ssbPts,na.rm=T) else
       maxX<-max(maxX,ssbTrk,ssbPts,na.rm=T)

    if (is.null(maxY))
       maxY<-max(     fbrTrk,fbrPts,na.rm=T) else
       maxY<-max(maxY,fbrTrk,fbrPts,na.rm=T)

    plot(fbrTrk~ssbTrk, col="cyan", type="l", xlim=c(0,maxX),ylim=c(0,maxY),xlab=xlab,ylab=ylab,main=paste(title),axes=axs)
    fish.pg(maxX,maxY)
    abline(h=1.0,v=1,col="grey")

    t.  <-bivariateOrder(cbind(fbrPts,ssbPts))
    col.<-rep(biCol,each=as.integer(length(t.)/length(biCol)))
    points(c(fbrPts)[t.]~c(ssbPts)[t.], col=col.,pch=19,cex=0.75)
    lines( fbrTrk~ssbTrk, col="grey50",lwd=2)
    }
