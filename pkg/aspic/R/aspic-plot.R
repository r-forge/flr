# plot.R - 
# aspic/R/plot.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurence Kell, ICCAT
# $Id:  $

setMethod("plot", signature(x="aspic", y="missing"),
  function(x, y, probs=c(0.95,0.50,0.05), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free",ncol=1),
    fn=list("Stock"=stock, "Harvest"=function(x) catch(x)/stock(x)[,dimnames(catch(x))$year],"Yield"=catch),...){

    plotComp(x,fn,probs,size,lty,facet)})

setMethod("plot", signature(x="aspics", y="missing"),
  function(x, y, probs=c(0.95,0.50,0.05), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free",ncol=1),
    fn=list("Stock"=stock, "Harvest"=function(x) catch(x)/stock(x)[,dimnames(catch(x))$year],"Yield"=catch),...)
   
    plotComps(x,fn,probs,size,lty,facet))



setMethod("diags", signature(x="aspic"),
function(x){
  
    #
    x    <- stock(    object)
    y    <- index(    object)
    hat  <- object@fitted
    residual <- log(y/hat)

    dmns <- dimnames(x)

    residualLag <- FLQuant(NA, dimnames=dimnames(residual))
    residualLag[,-dim(residual)[2]] <- residual[,-1]

    qq. <- qqnorm(c(residual),plot.it=FALSE)
    qqx <- FLQuant(qq.$x,dimnames=dimnames(residual))
    qqy <- FLQuant(qq.$y,dimnames=dimnames(residual))

    res <- drop(model.frame(mcf(FLQuants(x=x,y=y,hat=hat,residual=residual,residualLag=residualLag,qqx=qqx,qqy=qqy)),drop=T))

    p.=ggplot(getDiag(res,diagPlots()[[class(object)]])) + 
        geom_point(aes(x,y))                           +
        facet_wrap(~name,scale="free")                 +
        geom_line(aes(x,hat,colour="red"))+geom_smooth(aes(x,y))
 
  print(p.)
 
  return(invisible(p.))}
)
 
 
# plotSP {{{
plotSP <- function(object,biomass=FLQuant(seq(0,params(object)["K"],length.out=101))) {
   p <-  ggplot(model.frame(FLQuants(stock=biomass, yield=sp(object,biomass)))) +
             geom_line(aes(stock, yield)) +
             geom_point(aes(bmsy,msy),data=cast(as.data.frame(refpts(object)),iter~refpts,value="data")) +
             xlab("Stock") + ylab("Surplus Production")
   print(p)
   invisible(p)
} # }}}


