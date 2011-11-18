# constructors - constructor methods for FLAdaptControl
# FLAdaptControl/R/constructors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell

setMethod('FLAdaptControl', signature(object='FLQuant'),
  function(object,...){
    args = list(...)

    # empty object
    res     = new("FLAdaptControl")

    
    # Load given slots
    for(i in names(args))
	slot(res, i) = args[[i]]

    return(res)})

setMethod('FLAdaptControl', signature(object='FLStock'),
  function(object,...){
    args = list(...)

    # empty object
    res     = new("FLAdaptControl")

    
    # Load given slots
    for(i in names(args))
      slot(res, i) = args[[i]]

    return(res)})

setMethod('FLAdaptControl', signature(object='character'),
  function(object,...){
    args = list(...)

   # Load given slots
   for(i in names(args))
      slot(res, i) = args[[i]]

    return(res)})


setMethod('FLAdaptControl', signature(object='missing'),
  function(object,...){
    args = list(...)

   
    return(FLAdaptControl(object, ...))})

is.FLAdaptControl = function(x)
	return(inherits(x, "FLAdaptControl"))
