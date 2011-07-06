# class - ?Short one line description?
# FLBioDym/R/class.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell
# Last Change: Wed Jun 22, 2011 at 01:58 PM +0200

validFLBioDym <- function(object)
  {
  ## Catch must be continous
  yrs<-dimnames(catch(object))$year
  
  if (!all(yrs == ac(dims(catch(object))$minyear:dims(catch(object))$maxyear)))
      return("years in catch not continous")

  # range
  dims <-dims(object)
  range<-as.list(object@range)

#  if (!any(dimnames(index(object))$year %in% dimnames(catch(object))$year))
#      return("index has to cover some years of catches")
  
  #if(range$minyear < dims$minyear | range$maxyear > dims$maxyear)
  #  return("mismatch between range and object dimensions")

  return(TRUE)}

setClass('FLBioDym',
  representation(
    "FLComp",
    model         ="character",
    distribution  ="character",
    catch         ='FLQuant',
    index         ='FLQuant',
    stock         ='FLQuant',
    fitted        ='FLQuant',
    bounds        ='array',
    priors        ='array',
    params        ='FLPar',
    vcov          ='array',
    hessian       ='array',
    logLik        ='numeric',
    rsdlVar       ='numeric',
    dof           ='array',
    stopmess      ="character"),
  prototype(
    range       =unlist(list(minyear=as.numeric(NA), maxyear=as.numeric(NA))),
    catch       =FLQuant(),
    index       =FLQuant(),
    fitted      =FLQuant(),
    stock       =FLQuant(),
    model       ="pellat",
    distribution="log",
    params      =FLPar(c(.5,NA,2,1,NA,NA),                    dimnames=list(param=c("r","K","p","b0","q","sigma"),iter=1)),
    bounds      =array(rep(c(1,NA,NA,NA),each=6), dim=c(6,4), dimnames=list(param=c("r","K","p","b0","q","sigma"),c("phase","lower","upper","start"))),
    priors      =array(rep(c(-1,0,0.3,1),each=6), dim=c(6,4), dimnames=list(param=c("r","K","p","b0","q","sigma"),c("weight","a","b","type")))
    ),
	validity=validFLBioDym)
