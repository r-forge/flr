# class - ?Short one line description?
# FLsz/R/class.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell
# Last Change: 7 July 2009 16:17

validFLsz <- function(object)
  { 
  ## no ages
  if (dim(object@obs)[1]!=dim(object@n)[1] & dim(object@obs)[1]!=1)
    warning("obs & n slots must have 1st dim size of 1")
  
  ## turn off 3:5 dims
  if (!all(dim(object@obs)[3:5] != dim(object@n)[3:5]) & !all(dim(object@n)[3:5]==1))
    warning("Only works for unique area, season and unit")
   
  ## len and weightings must be same
  if (!all(unlist(llply(names(object),function(x,a,b) dimnames(a)[[x]]==dimnames(b)[[x]],a=object$obs,b=object$n))))
    warning("dims mismatch in obs and n")
  
  ## years must be continuous
  yrs<-dimnames(object@obs)$year
  
  if (!all(yrs == ac(dims(object@obs)$minyear:dims(object@obs)$maxyear)))
      stop("years not continous")

  if (!all(dimnames(object)$params %in% c("linf","winf","k","t0","Lc","a","b")))
      stop(c("illegal names for grw$params must be in", c("linf","k","t0","lc","a","b")))        

  # range

  return(TRUE)}

setClass('FLsz',
  representation(
    "FLComp",
    model         ='character',
    
    obs           ='FLQuant',
    hat           ='FLQuant',
    n             ='FLQuant',
    residuals     ='FLQuant',
    
    grw           ='FLPar',
    params        ='FLPar',
    se            ='FLPar',
    bounds        ='array',
    priors        ='array',
    
    vcov          ='array',
    hessian       ='array',
    
    logLik        ='numeric',
    rsdlVar       ='numeric',
    dof           ='array',
    stopmess      ="character"),
  prototype(model="vonB"),
	validity=validFLsz)
