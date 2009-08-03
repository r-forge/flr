# class - «Short one line description»
# FLPellaT/R/class.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell
# Last Change: 7 July 2009 16:17

validFLPellaT <- function(object)
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

setClass('FLPellaT',
  representation(
    "FLComp",
    catch         ='FLQuant',
    index         ='FLQuant',
    stock         ='FLQuant',
    distribution  ="character",
    params        ='FLPar',
    covar         ='array',
    LL            ='numeric',
    sigma         ='numeric',
    deviance      ='numeric',
    df            ='array',
    stats         ='array',
    message       ="character"),
  prototype(
    range       =unlist(list(minyear=as.numeric(NA), maxyear=as.numeric(NA))),
    catch       =FLQuant(),
    index       =FLQuant(),
    stock       =FLQuant(),
    distribution="log",
    params      =FLPar(c(.5,NA,2,1,NA,NA),,dimnames=list(paramss=c("r","K","mpar","b0","q","sigma"),iter=1))),
	validity=validFLPellaT)
