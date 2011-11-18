# constructors - constructor methods for FLAdapt
# FLAdapt/R/constructors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell

setMethod('FLAdapt', signature(object='FLQuant'),
  function(object,...){
    args = list(...)

    # empty object
    object[]= NA
    dims    = dims(object)
    res     = new("FLAdapt")

    stock( res)  = object
    harvest(res) = object
    range(res)  =unlist(list(minyear=dims$minyear, maxyear=dims$maxyear))

    # Load given slots
   for(i in names(args))
	slot(res, i) = args[[i]]

    return(res)})

setMethod('FLAdapt', signature(object='FLStock'),
  function(object,...){
    args = list(...)

    # empty object
    object[]= NA
    dims    = dims(object)
    res     = new("FLAdapt")

    stock( res)  = stock(  object)
    harvest(res) = harvest(object)
    range(res)   =unlist(list(minyear=dims$minyear, maxyear=dims$maxyear))

    # Load given slots
   for(i in names(args))
	  slot(res, i) = args[[i]]

    return(res)})

setMethod('FLAdapt', signature(object='character'),
  function(object,...){
    args = list(...)

   res=readVpa2box(object)  
   # Load given slots
   for(i in names(args))
      slot(res, i) = args[[i]]

    return(res)})


setMethod('FLAdapt', signature(object='missing'),
  function(object,...){
    
    object=new("FLAdapt")      
    
    args = list(...)
    for(i in names(args))
      slot(res, i) = args[[i]]
 
    return(object)})

is.FLAdapt = function(x)
	return(inherits(x, "FLAdapt"))
