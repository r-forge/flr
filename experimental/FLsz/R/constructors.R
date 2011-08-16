# constructors - constructor methods for FLln2z
# FLln2z/R/constructors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell

setGeneric('FLln2z', function(object, ...)
  	standardGeneric('FLln2z'))

setMethod('FLln2z', signature(object='FLQuant'),
  function(object,model="vonb",n=FLQuant(1,dimnames=dimnames(object)),...)
    {
    args = list(...)

    # empty object
    res     =new("FLln2z")
    res@obs =object
    res@n   =n
    range(res)[c("minyear","maxyear","min","max","plusgroup")]=c(unlist(list(minyear=dims(object)$minyear, maxyear=dims(object)$maxyear)),rep(NA,3))


    res@model =model
    #res@params=setParams(model)

    # Load given slots
  	for(i in names(args))
			slot(res, i) = args[[i]]

    return(res)})

setMethod('FLln2z', signature(object='missing'),
  function(...)
    {
    args = list(...)

    # if no FLQuant argument given, then use empty FLQuant
    slots = lapply(args, class)
    slots = names(slots)[slots == 'FLQuant']

    if(length(slots) == 0)
      object = FLQuant()
    else
      object = args[[slots[1]]]

    return(FLln2z(object, ...))})

setMethod('FLln2z', signature(object='FLStock'),
  function(object,model="vonb",...){
    args = list(...)

    res=FLln2z()
    
    # Load given slots
    for(i in names(args))
			slot(res, i) = args[[i]]

    return(res)})

setMethod('FLln2z', signature(object='data.frame'),
  function(object,model="vonb",...){
  
  if (c("n","ln","year") %in% names(object)){  
    ln=FLQuant(object$ln, dimnames=list(year=object$year))
    n =FLQuant(object$n,  dimnames=list(year=object$year))}
  else{
    ln=FLQuant(object[,3], dimnames=list(year=object[,1]))
    n =FLQuant(object[,2], dimnames=list(year=object[,1]))}
 
  FLln2z(ln,n=n,model=model,...)})

    is.FLln2z = function(x)
	return(inherits(x, "FLln2z"))
