# FLIndexPM.R - FLIndexPM class and methods
# FLAdapt/R/FLIndexPM.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurence Kell, Cefas
# $Id: FLIndexPM.R,v 1.60 2008/04/25 13:35:23 imosqueira Exp $

# Reference:
# Notes:

## class :: FLIndexPM     {{{
validFLIndexPM <- function(object) {

  dimnms <- qapply(object, function(x) dimnames(x))

  # iters are 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$iter))))>2)
     stop("Iters in FLIndexPM can only be of length 1 or n")

  # quant is 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$max))))>2)
     stop("quant dimension in FLIndexPM can only be 'all' or n")

  # dims[2:5] match
  for(i in names(dimnms)[-1])
    if(!all.equal(dimnms[[i]][-1], dimnms[[1]][-1]))
      stop(cat("Mismatch in dims for", i))

  # first dim equal for all index.* slots
  for(i in grep('index', names(dimnms), value=TRUE))
    if(!all.equal(dimnms[[i]][1], dimnms[[1]][1]))
      stop(cat("Mismatch in dims for", i))

  #

  # effort should have quant='all'
  if (!(dims(slot(object,"effort"))[1] == 1))
     stop("Effort can only have quant = 'all'")

  # min / max
  dims <- dims(object@catch.n)
  min <- object@range["min"]

  if (!is.na(min) && (min < dims(object@catch.n)$min || min > dims(object@catch.n)$max))
     stop(paste("min is outside quant range in FLQuant slot", i))

  max <- object@range["max"]
  if(!is.na(max) && (max < dims(object@catch.n)$min || max > dims(object@catch.n)$max))
    stop(paste("max is outside quant range in FLQuant slot", i))

  if (!is.na(min) && !is.na(max) && max < min)
    stop(paste("max quant is lower than min quant in FLQuant slot", i))

  # plusgroup
  plusgroup <- object@range["plusgroup"]
  if (!is.na(plusgroup) && (plusgroup < dims$min || plusgroup > dims$max))
     stop("plusgroup is outside [min, max] range in FLQuant slots")

  # minyear / maxyear
  dims <- dims(object@index)
  minyear <- object@range["minyear"]
  if (!is.na(minyear) && (minyear < dims$minyear || minyear > dims$maxyear))
     stop(paste("minyear is outside years range in FLQuant slot", i))
  maxyear <- object@range["maxyear"]
  if (!is.na(maxyear) && (maxyear < dims$minyear || maxyear > dims$maxyear))
     stop(paste("maxyear is outside years range in FLQuant slot", i))
  if (!is.na(minyear) && !is.na(maxyear) && maxyear < minyear)
     stop(paste("maxyear is lower than minyear in FLQuant slot", i))

  # Everything is fine
  return(TRUE)
  }

setClass("FLIndexPM",
    representation(
  		"FLIndex",
  		index.lambda     = "FLQuant",
  		index.varAdd     = "FLQuant"),
    prototype=prototype(
  		index.lambda     = new("FLQuant"),
  		index.varAdd     = new("FLQuant")),
    validity=validFLIndexPM
)

setValidity("FLIndexPM", validFLIndexPM)
remove(validFLIndexPM)    #   }}}

## Accesors {{{
#invisible(createFLAccesors(new("FLIndexPM"), exclude=c('type','distribution','index','index.var',
#                                                  'catch.n','catch.wt','effort','sel.pattern',
#                                                  'index.q')))
# }}}

## FLIndexPM()    {{{
FLIndexPM <- function(name=character(0), desc=character(0), distribution=character(0),
    type=character(0), startf=NA, endf=NA, plusgroup=NA, ...) {

	args <- list(...)
	if(length(args)==0)
		args <- list(index=FLQuant())

	dimnames <- dimnames(args[[names(lapply(args, is.FLQuant)==TRUE)[1]]])
  sdimnames <- dimnames
  sdimnames[1] <- 'all'

	if(!is.FLQuant(args['index']))
		index <- FLQuant(dimnames=dimnames)

	dims <- dims(index)

	new <- new("FLIndexPM", name = name, desc = desc, distribution = distribution,
        type=type,
        index = index,
        index.var = FLQuant(dimnames=dimnames),
        index.q   = FLQuant(dimnames=dimnames),
        index.varAdd=FLQuant(dimnames=dimnames),
        index.lambda=FLQuant(dimnames=dimnames),
        sel.pattern = FLQuant(dimnames=dimnames),
        catch.n = FLQuant(dimnames=dimnames), catch.wt = FLQuant(dimnames=dimnames),
        effort = FLQuant(dimnames=sdimnames),
        range = unlist(list(min=dims$min, max=dims$max,
		plusgroup=NA, minyear=dims$minyear, maxyear=dims$maxyear, startf=startf, endf=endf)))

	# Add extra arguments
	for(i in names(args)[names(args)!='iniFLQuant'])
		slot(new, i) <- args[[i]]

	# Correctly size other FLQuants.
#    emptyQuants <- names(getSlots(class)[getSlots(class)=="FLQuant"])[
#        !names(getSlots(class)[getSlots(class)=="FLQuant"])%in%c("index", "index.var",
#        names(args))]
#    for(i in emptyQuants)
#        slot(new, i) <- FLQuant(index)

	return(new)
}   # }}}

## is.FLIndexPM	{{{
is.FLIndexPM <- function(x)
    return(inherits(x, "FLIndexPM"))
# }}}

## as.FLIndexPM::FLFleet      {{{
if (!isGeneric("as.FLIndexPM"))
    setGeneric("as.FLIndexPM", function(object, ...)
        standardGeneric("as.FLIndexPM"))

setMethod("as.FLIndexPM", signature(object="FLFleet"),
    function(object, catchname="missing", catchtype="missing", ...) {

    fli<-as.FLIndexPM(object, catchname="missing", catchtype="missing", ...)
    fli@index.varAdd<-FLQuant(0.0,dimnames=dimnames(fli@ondex.var))
    fli@index.lambda<-FLQuant(1.0,dimnames=dimnames(fli@ondex.var))

    return(fli)
    }
)   # }}}

setAs('FLStock', 'FLIndexPM',
	function(from)
	{
    dmns<-dimnames(from@catch.n)
    dmns$year<-1
		res<-FLIndexPM(index       =from@catch.n,
                      index.var   =FLQuant(NA,  dimnames=dimnames(from@catch.n)),
                      index.varAdd=FLQuant(0.0, dimnames=dimnames(from@catch.n)),
                      index.lambda=FLQuant(1.0, dimnames=dmns),
                      catch.n     =from@catch.n,
                      catch.wt    =from@catch.wt,
                      effort      =apply(from@harvest[ac(from@range["min"]:from@range["max"])],2:6,mean),
                      sel.pattern =sweep(from@harvest,2:6,apply(from@harvest[ac(from@range["min"]:from@range["max"])],2:6,mean),"/"),
                      index.q     =FLQuant(1,  dimnames=dimnames(from@catch.n)),
                      type="number",
                      name=from@name, desc=paste("Coerced from FLBiol:",from@desc))

    units(res@index)  <-units(from@catch.n)
    units(res@catch.n)<-units(from@catch.n)
    units(res@catch.wt)<-units(from@catch.wt)

  return(res)
	}
)
# }}}

if (!isGeneric("logl"))
    setGeneric("logl", function(object, ...)
        standardGeneric("logl"))

setMethod("logl", signature(object="FLIndexPM"),
   function(object, hat, ...) {

    LL<-0

    x1 <- ((log(ple4@stock.n)-log(object@index.q*object@index))^2)
    x2 <- (object@index.var^2+object@index.varAdd^2)
    x3 <- log(2*pi*x2*hat^2)
    LL <- 0.5*apply(object@index.lambda*apply(x3+x1/x2,c(1,3:6),sum),6,sum)

    return(-LL)
    }
)   # }}}

setMethod("logl", signature(object="FLIndexPM"),
  l.<-   function(object, hat, ...) {

     ## check dims
     dms    <- dims(object@index)
     dmns   <- dimnames(object@index)
     dms.chk<-unlist(dims(object@index))

     ## variance
     if (dims(object@index.var)$year==1)
        object@index.var<-FLQuant(object@index.var,dimnames=dmns)
     if (!all(dms.chk==unlist(dims(object@index.var))))
        stop("index.var dims don't match index")

     ## additional variance
     if (dims(object@index.varAdd)$year==1)
        object@index.varAdd<-FLQuant(object@index.varAdd,dimnames=dmns)
     if (!all(dms.chk==unlist(dims(object@index.varAdd))))
        stop("index.varAdd dims don't match index")

     ## catchability
     if (dims(object@index.q)$year==1)
        object@index.q<-FLQuant(object@index.q,dimnames=dmns)
     if (!all(dms.chk==unlist(dims(object@index.q))))
        stop("index.q dims don't match index")

     ## weighting
     if (!(dims(object@index.lambda)$year==1))
        stop("index.lambda must be fixed for all years")
     if (!all(dms.chk[-(4:6)]==unlist(dims(object@index.lambda))[-(4:6)]))
        stop("index.lambda dims don't match index")

     obs<-log(object@index.q*object@index)
     hat<-log(hat)

     obs[!is.finite(obs)]<-NA
     hat[!is.finite(hat)]<-NA

     x1 <-(hat-obs)^2

     #### options
     ## varAdd fixed
     ## 1) varAdd = 0.0 and var > 0.0
     ## least squares
     if (all(index@varAdd==0.0) && all(index@varAdd>0.0))
        print("Least Squares")

     ## 2) varAdd > 0.0 and var >= 0.0
     ## can estimate index.q
     else if (all(index@varAdd>0.0) && all(index@varAdd>=0.0))
        {
        print("Estimate q & var")
        object@index.q<-apply((x1 )/(object@index.varAdd+object@index.var),c(1,3:6),sum)/
                        apply(1/(object@index.varAdd+object@index.var),c(1,3:6),sum)
        
        }

     ## varAdd to be estimated
     ## 3) var = 0.0
     ## can estimate varAdd
     else if (all(is.na(index@varAdd)) && all(index@varAdd==0.0))
        {
        count<-object@index
        count[!is.na(count)]<-1
        object@index.varAdd<-apply(x1,c(1,3:6),sum)/apply(x1,c(1,3:6),sum)
        }
        
     ## 4) var > 0.0
     ## can estimate index.q and index.varAdd but no closed form


     #  var > 0.0
     ## need to solve for varAdd

     x2 <- (object@index.var+object@index.varAdd)
     x3 <- log(2*pi*x2*hat^2)
     LL <- 0.5*apply(object@index.lambda*apply(x3+x1/x2,c(1,3:6),sum,na.rm=T),6,sum,na.rm=T)
     
     return(-LL)
     }
  )   # }}}



