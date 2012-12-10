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
    fn=list("Stock"=stock, "Harvest"=function(x) catch(x)/stock(x)[,dimnames(catch(x))$year],"Yield"=catch),...){
   
    if("split_labels" %in% names( attributes(x))){
      names(x)=unlist(attributes(x)$split_labels)
      attributes(x)$split_labels=NULL}

    plotComps(x,fn,probs,size,lty,facet)})
