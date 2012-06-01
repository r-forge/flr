# plot.R - 
# FLBioDym/R/plot.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurence Kell, ICCAT
# $Id:  $

setMethod("plot", signature(x="FLBioDym", y="missing"),
  function(x, y, probs=c(0.95,0.50,0.05), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free",ncol=1),
    fn=list("Stock"=stock, "Harvest"=function(x) catch(x)/stock(x)[,dimnames(catch(x))$year],"Yield"=catch),...)
   
    plotComp(x,fn,probs,size,lty,facet))

setMethod("plot", signature(x="FLBioDyms", y="missing"),
  function(x, y, probs=c(0.95,0.50,0.05), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free",ncol=1),
    fn=list("Stock"=stock, "Harvest"=function(x) catch(x)/stock(x)[,dimnames(catch(x))$year],"Yield"=catch),...)
   
    plotComps(x,fn,probs,size,lty,facet))


# plotSP {{{
plotSP <- function(object,biomass=FLQuant(seq(0,params(object)["K"],length.out=101))) {
   p <-  ggplot(model.frame(FLQuants(stock=biomass, yield=sp(object,biomass)))) +
             geom_line(aes(stock, yield)) +
             geom_point(aes(bmsy,msy),data=cast(as.data.frame(refpts(object)),iter~refpts,value="data")) +
             xlab("Stock") + ylab("Surplus Production")
   print(p)
   invisible(p)
} # }}}

