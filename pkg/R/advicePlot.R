if (!isGeneric("advicePlot"))
	setGeneric("advicePlot", function(b,f, ...)
		standardGeneric("advicePlot"))

setMethod("advicePlot", signature(b="FLQuants", f="FLQuants"),
   advicePlot<-function(b,f,ylab="TAC",xlab="Year",x0n=500,y0n=x0n,...){

    ## Calculate probabilities
    dmns<-list(val=c("joint","f","ssb"),scen=names(b),year=dimnames(b[[1]])$year,iter=dimnames(b[[1]])$iter)
    res <-array(NA,lapply(dmns,length),dimnames=dmns)
    for (iScen in names(b)){
           res["ssb",  iScen,,]<-  qmin(floor(b[[iScen]]),1)[1,drop=T]
           res["f",    iScen,,]<-1-qmin(floor(f[[iScen]]),1)[1,drop=T]
           res["joint",iScen,,]<-res["f",iScen,,]*res["ssb",iScen,,]
           }
    res<-apply(res["joint",,,],1:2,mean)

    ## Plot Joint probabilities
    t.<-defactor(cbind(expand.grid(dimnames(res)[c("scen","year")]),val=c(res)))
    t.<-interp(t.[,2],t.[,1],t.[,3], yo=seq(min(t.[,1]), max(t.[,1]), length=500),
                                     xo=seq(min(t.[,2]), max(t.[,2]), length=500))

    image(  t.,breaks=c(0,.5,.75,1),    col=c("red","yellow","green"),ylab=ylab, xlab="Year")
    contour(t.,levels=c(.5,.75),add=T,  col="grey",  lwd=2)
    contour(t.,levels=c(.90,.60),add=T, col="grey2", lwd=2)
    })

setMethod("advicePlot", signature(b="array", f="array"),
   function(b,f,ylab="TAC",xlab="Year",x0n=500,y0n=x0n,...){
   b<-0
   f<-0
   advicePlot(b,f,ylab=ylab,xlab=xlab,x0n=x0n,y0n=y0n,...)
   })

