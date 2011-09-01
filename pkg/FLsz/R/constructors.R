# constructors - constructor methods for FLsz
# FLsz/R/constructors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell

setGeneric('FLsz', function(object, ...)
  	standardGeneric('FLsz'))

setMethod('FLsz', signature(object='FLQuant'),
  function(object,model="vonb",n=FLQuant(1,dimnames=dimnames(object)),...)
    {
    args = list(...)

    # empty object
    res     =new("FLsz")
    res@obs =object
    res@n   =n
    range(res)[c("minyear","maxyear","min","max","plusgroup")]=c(unlist(list(minyear=dims(object)$minyear, maxyear=dims(object)$maxyear)),rep(NA,3))


    res@model =model
    #res@params=setParams(model)

    # Load given slots
  	for(i in names(args))
			slot(res, i) = args[[i]]

    return(res)})

setMethod('FLsz', signature(object='missing'),
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

    return(FLsz(object, ...))})

setMethod('FLsz', signature(object='FLStock'),
  function(object,model="vonb",...){
    args = list(...)

    res=FLsz()
    
    # Load given slots
    for(i in names(args))
			slot(res, i) = args[[i]]

    return(res)})

setMethod('FLsz', signature(object='data.frame'),
  function(object,model="vonb",...){
  
  if (c("n","ln","year") %in% names(object)){  
    ln=FLQuant(object$ln, dimnames=list(year=object$year))
    n =FLQuant(object$n,  dimnames=list(year=object$year))}
  else{
    ln=FLQuant(object[,3], dimnames=list(year=object[,1]))
    n =FLQuant(object[,2], dimnames=list(year=object[,1]))}
 
  FLsz(ln,n=n,model=model,...)})

    is.FLsz = function(x)
	return(inherits(x, "FLsz"))
