#FriedEggPairs.R
require(KernSmooth)

"fried.egg"<-function(xx,yy,bw=20, ...)
{
	bwx=(max(xx)-min(xx))/bw; bwy=(max(yy)-min(yy))/bw
	est <- bkde2D(cbind(xx,yy),bandwidth=c(bwx,bwy),gridsize=c(81, 81))
	est$fhat=est$fhat/max(est$fhat)
	
	lvs=c(0.05,0.2,0.8,0.99)
	maxct=max(lvs)
	nlvs=length(lvs)
	thelines=contourLines(est$x1,est$x2,est$fhat,levels=lvs)
	polygon(thelines[[nlvs-3]]$x,thelines[[nlvs-3]]$y,col="grey",border="firebrick",lwd=1)
	polygon(thelines[[nlvs-2]]$x,thelines[[nlvs-2]]$y,col="snow",border="snow1",lwd=1)
	xi=sample(1:length(xx),500)
	polygon(thelines[[nlvs-1]]$x,thelines[[nlvs-1]]$y,col="gold",border="gold2",lwd=1)
	#Add salt and pepper
	#if(pepper)points(jitter(xx[xi]),jitter(yy[xi]),pch=".",cex=0.5,col=grey(0:10/10))
	polygon(thelines[[nlvs]]$x,thelines[[nlvs]]$y,col="yellow2",border="yellow2",lwd=1)
	#contour(est$x1,est$x2,est$fhat,drawlabels=T,add=T,levels=lvs,lty=1,lwd=1,labcex= 0.7)
	
	
}
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x,plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$density; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="khaki", ...)
}


egg.pairs <- function(A, ...)
{
	pairs(A,pch=".",lower.panel=fried.egg,diag.panel=panel.hist,...)
}

require(MASS)
Sigma <- matrix(c(10,2.5,2.5,2),2,2)
XY=mvrnorm(n=10000, rep(0, 2), Sigma)
xx=XY[,1]; yy=XY[,2]

egg.pairs(XY)