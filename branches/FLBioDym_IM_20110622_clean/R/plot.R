# plot.R - 
# FLBioDym/R/plot.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurence Kell, ICCAT
# $Id:  $

# plot {{{
setMethod("plot", signature(x="FLBioDym", y="missing"),
  function(x, y, probs=c(0.95,0.50,0.05), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free"),
    fn=list("stock"=stock, "harvest"=function(x) catch(x)/stock(x)[,dimnames(catch(x))$year],yield=catch),...){
   
    plotComp(x,fn,probs,size,lty,facet)}) 

    plot.sp<-function(object){

      p.<-ggplot(data.frame(stock=seq(0,params(object)["K"],length.out=101),
                            yield=sp(object,seq(0,params(object)["K"],length.out=101)))) +
              geom_line(aes(stock,yield)) +
              geom_point(aes(bmsy,msy),data=cast(as.data.frame(refpts(object)),iter~refpts,value="data"))
      
      print(p.)
      invisible(p.)}
# }}}

# diags {{{
setMethod("diags", signature(object="FLBioDym"),
  function(object, i=NULL) {
    
    #
    x    <- stock(    object)
    y    <- index(    object)
    yHat <- object@index.hat
    residual <- log(y/yHat)

    dmns <- dimnames(x)

    residualLag <- FLQuant(NA, dimnames=dimnames(residual))
    residualLag[,-dim(residual)[2]] <- residual[,-1]

    qq. <- qqnorm(c(residual),plot.it=FALSE)
    qqx <- FLQuant(qq.$x,dimnames=dimnames(residual))
    qqy <- FLQuant(qq.$y,dimnames=dimnames(residual))

    res <- drop(model.frame(mcf(FLQuants(x=x,y=y,yHat=yHat,residual=residual,residualLag=residualLag,qqx=qqx,qqy=qqy)),drop=T))

    return(res)}) 
# }}}
