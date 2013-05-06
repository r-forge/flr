## class :: FLFishery     {{{
validFLFishery <- function(object) {

  dimnms <- qapply(object, function(x) dimnames(x))

  # iters are 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$iter))))>2)
     stop("Iters in FLFishery can only be of length 1 or n")

  # quant is 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$max))))>2)
     stop("quant dimension in FLFishery can only be 'all' or n")

  # dims[2:5] match
  for(i in names(dimnms)[-1])
    if(!all.equal(dimnms[[i]][-1], dimnms[[1]][-1]))
      stop(cat("Mismatch in dims for", i))

  # first dim equal for all index.* slots
  for(i in grep('index', names(dimnms), value=TRUE))
    if(!all.equal(dimnms[[i]][1], dimnms[[1]][1]))
      stop(cat("Mismatch in dims for", i))

  # effort should have quant='all'
  if (!(dims(slot(object,"effort"))[1] == 1))
     stop("Effort can only have quant = 'all'")
  # vcost should have quant='all'
  if (!(dims(slot(object,"vcost"))[1] == 1))
     stop("vcost can only have quant = 'all'")
  # fcost should have quant='all'
  if (!(dims(slot(object,"fcost"))[1] == 1))
     stop("fcost can only have quant = 'all'")

  # min / max
  dims <- dims(object@landings.n)
  min <- object@range["min"]

  if (!is.na(min) && (min < dims(object@landings.n)$min || min > dims(object@landings.n)$max))
     stop(paste("min is outside quant range in FLQuant slot", i))

  max <- object@range["max"]
  if(!is.na(max) && (max < dims(object@landings.n)$min || max > dims(object@landings.n)$max))
    stop(paste("max is outside quant range in FLQuant slot", i))

  if (!is.na(min) && !is.na(max) && max < min)
    stop(paste("max quant is lower than min quant in FLQuant slot", i))

  # plusgroup
  plusgroup <- object@range["plusgroup"]
  if (!is.na(plusgroup) && (plusgroup < dims$min || plusgroup > dims$max))
     stop("plusgroup is outside [min, max] range in FLQuant slots")

  # minyear / maxyear
  dims <- dims(object@landings.n)
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

setClass("FLFishery",
    representation(
  		"FLCatch",
  		effort     = "FLQuant",
  		vcost      = "FLQuant",
  		fcost      = "FLQuant"),
    prototype=prototype(
  		effort     = new("FLQuant"),
  		vcost      = new("FLQuant"),
  		fcost      = new("FLQuant")),
    validity=validFLFishery)

setValidity("FLFishery", validFLFishery)
remove(validFLFishery)    #   }}}

setGeneric('FLFishery', function(object, ...)
		standardGeneric('FLFishery'))

# TODO Fix size of input objects and validity
setMethod('FLFishery', signature(object='FLQuant'),
	function(object, range='missing', name='NA', desc=character(0), ...) {
		# initial objects
		flq <- FLQuant(NA, dimnames=dimnames(object))
		flqa <- quantSums(flq)
		dims <- dims(flq)
		args <- list(...)

    # construct range
		if(missing(range))
			range <- c(min=dims$min, max=dims$max, plusgroup=NA,
				minyear=dims$minyear, maxyear=dims$maxyear)

		# output object
		res <- new('FLFishery', range=range, name=name, desc=desc,
			landings.n=flq, landings.wt=flq, landings.sel=flq, landings=flqa,
			discards.n=flq, discards.wt=flq, discards.sel=flq, discards=flqa,
			catch.q=flqa, price=flq, effort=flqa, fcost=flqa, vcost=flqa)
			
		# Load given slots
		for(i in names(args))
			slot(res, i) <- args[[i]]
		return(res)
	}
)

setMethod('FLFishery', signature(object='missing'),
	function(...)
  {
		# get arguments & select first full FLQuant
		args <- list(...)
		args <- args[lapply(args, class) == 'FLQuant']

		flqs <- args[names(args) != 'landings' & names(args) != 'discards' & names(args) != 'effort' & names(args) != 'fcost'  & names(args) != 'vcost']

		# select full flquant, or small flquant, or create dimnames
		if(length(flqs) > 0)
			dimnames <- dimnames(flqs[[1]])
		else if(length(args) > 0)
			dimnames <- dimnames(args[[1]])
		else
			dimnames <- dimnames(FLQuant())
		return(FLFishery(FLQuant(dimnames=dimnames), ...))
	}
)	# }}}

# validity
vFLFshs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLFishery")) stop("Components must be FLFishery")
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLFisheries", contains="FLlst",
	validity=vFLFshs
)

# constructor
setGeneric("FLFisheries", function(object, ...){
	standardGeneric("FLFisheries")
	}
)

setMethod("FLFisheries", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)

	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1

	# IM 20.08.07 get names
	names <- c(object@name, unlist(lapply(lst1, function(x) x@name)))
	attr(lst, "names") <- names
	attr(lst, "lock") <- TRUE
	new("FLFisheries", lst)
})

setMethod("FLFisheries", "missing", function(...){
	if(missing(...)){
		new("FLFisheries")
	} else {
		lst <- list(...)
		new("FLFisheries", lst)
	}
})

setMethod("FLFisheries", "list", function(object){
	new("FLFisheries", object)
})

# is
setGeneric("is.FLFisheries", function(object, ...){
	standardGeneric("is.FLFisheries")
	}
)

setMethod("is.FLFisheries", "ANY", function(object, ...){
	identical(is(object)[1],"FLFisheries")
})
