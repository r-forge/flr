# class - «Short one line description»
# FLBioDym/R/class.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell
# Last Change: 7 July 2009 16:17

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

  return(TRUE)
  }

setClass('FLBioDym',
  representation(
    "FLComp",
    catch         ='FLQuant',
    index         ='FLQuant',
    stock         ='FLQuant',
    model         ="character",
    distribution  ="character",
    params        ='FLPar',
    vcov          ='array',
    hessian       ='array',
    logLik        ='numeric',
    rsdlVar       ='numeric',
    dof           ='array',
    stats         ='array',
    stopmess      ="character"),
  prototype(
    range       =unlist(list(minyear=as.numeric(NA), maxyear=as.numeric(NA))),
    catch       =FLQuant(),
    index       =FLQuant(),
    stock       =FLQuant(),
    model       ="pellat",
    distribution="log",
    params      =FLPar(c(.5,NA,2,1,NA,NA),,dimnames=list(paramss=c("r","K","p","b0","q","sigma"),iter=1))),
	validity=validFLBioDym)
