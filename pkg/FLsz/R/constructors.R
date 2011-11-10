# constructors - constructor methods for FLsz
# FLsz/R/constructors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell

setGeneric('FLsz', function(object, ...)
  	standardGeneric('FLsz'))

setMethod('FLsz', signature(object='FLQuant'),
  function(object,model="vonb",n=FLQuant(1,dimnames=dimnames(object)),
                  nbreaks=4,
                  breaks=rev(rev(round(seq(dims(object)$minyear,dims(object)$maxyear,length.out=nbreaks+1)))[-1]),...){
    args = list(...)

    # empty object
    res     =new("FLsz")

    print(breaks)
    res=ini(res,breaks)

    res@obs      =object
    res@n        =n
    res@hat      =FLQuant(NA, dimnames=dimnames(n))
    res@residuals=res@hat
    range(res)[c("minyear","maxyear","min","max","plusgroup")]=c(unlist(list(minyear=dims(object)$minyear, 
                                                                             maxyear=dims(object)$maxyear)),rep(NA,3))

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
  function(object,model="vonb",nbreaks=4,
                  breaks=rev(rev(round(seq(dims(object)$minyear,dims(object)$maxyear,length.out=nbreaks+1)))[-1]),...){
    args = list(...)

    n  =apply(stock.n(object),2:6,sum)
    
    if ("grw" %in% names(args))
       obs=apply(wt2len(args[["grw"]],stock.wt(object))*stock.n(object),2:6,sum)/n
   
    res=FLsz(obs,n=n,breaks=breaks)
     
    range(res)[c("minyear","maxyear","min","max","plusgroup")]=c(unlist(list(minyear=dims(obs)$minyear, 
                                                                             maxyear=dims(obs)$maxyear)),rep(NA,3))   
    # Load given slots
    for(i in names(args))
			slot(res, i) = args[[i]]

    return(res)})

setMethod('FLsz', signature(object='data.frame'),
  function(object,model="vonb",nbreaks=4,
                  breaks=rev(rev(round(seq(dims(object)$minyear,dims(object)$maxyear,length.out=nbreaks+1)))[-1]),...){
  if (all(c("n","ln","year") %in% names(object))){  
    ln=FLQuant(object$ln, dimnames=list(year=object$year))
    n =FLQuant(object$n,  dimnames=list(year=object$year))}
  else{
    ln=FLQuant(object[,3], dimnames=list(year=object[,1]))
    n =FLQuant(object[,2], dimnames=list(year=object[,1]))}
 
  FLsz(ln,n=n,model=model,breaks=breaks,...)})

    is.FLsz = function(x)
	return(inherits(x, "FLsz"))
