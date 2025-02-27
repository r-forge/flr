# FLFleet - FLFleet class and methods
# FLCore/R/FLFleet.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer:
# $Id$

## FLFleet		{{{
validFLFleet <- function(object) {

	# FLQuant slots share dims 3:5 ...
  dnames <- qapply(object, function(x) dimnames(x)[3:5])
	for(i in names(dnames))
		if(!identical(dnames[[i]], dnames[[1]]))
			return('All FLQuant slots must have the same dimensions')

  # ... and are consistent with metiers
  metdnames <- lapply(object@metiers, function(x)
    qapply(object, function(x) dimnames(x)[3:5]))
  for(i in seq(length=length(metdnames)))
    for(j in names(metdnames[[1]]))
	    if(!identical(metdnames[[i]][[j]], dnames[[1]]))
			  return('All FLQuant slots must have the same dimensions')
  
  # Year range of FLFleet covers all metiers
  metyears <- matrix(unlist(lapply(object@metiers, function(x) 
    unlist(dims(x)[c('minyear', 'maxyear')]))), byrow=TRUE, ncol=2)

  if(any(dims(object)$minyear < metyears [,1]) |
    any(dims(object)$maxyear > metyears [,2]))
    return('Year range of fleet should encompass those of metier(s)')

  # iter is consistent between fleet and metiers
  if(any(dims(object)$iter != unlist(lapply(object@metiers, function(x) dims(x)$iter))))
    return('iter must be 1 or N across all slots and levels')

  # effshares must add up to one
  #effshs <- lapply(object@metiers, effshare)
  #if(length(effshs) > 1)
  #  for(i in 2:length(effshs))
  #    effshs[[1]] <- effshs[[1]] + effshs[[i]]
  #if(!isTRUE(all.equal(as.vector(effshs[[1]]), rep(1,prod(dim(effshs[[1]]))))))
  #  return('sum of effshare must add up to 1')

	return(TRUE)
}

setClass('FLFleet',
	representation('FLComp',
		effort='FLQuant',
		fcost='FLQuant',
		capacity='FLQuant',
		crewshare ="FLQuant",
		metiers='FLMetiers'),
	prototype(name=character(0), desc=character(0),
		range= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1)),
		effort=FLQuant(), fcost=FLQuant(), capacity=FLQuant(),
		crewshare=FLQuant(), metiers=FLMetiers()),
	validity=validFLFleet)
remove(validFLFleet)

invisible(createFLAccesors("FLFleet", exclude=c('range', 'effort', 'name', 'desc')))	# }}}

# FLFleet()		{{{
setGeneric('FLFleet', function(object, ...)
		standardGeneric('FLFleet')
)
setMethod('FLFleet', signature(object='FLMetiers'),
	function(object, ...)
	{
		args <- list(...)
		flqs <- unlist(lapply(args, is, 'FLQuant'))
		if(any(flqs))
			flqs <- FLQuant(NA,
				dimnames=c(dimnames(args[[names(flqs[flqs==TRUE])[1]]])[-6], list(iter=1)))
		else
			flqs <- FLQuant()
		res <- new('FLFleet', metiers=object, effort=flqs, fcost=flqs,
			capacity=flqs, crewshare=flqs, range=range(object))

		# extra arguments
		for (i in names(args))
			slot(res, i) <- args[[i]]
		return(res)
	}
)
setMethod('FLFleet', signature(object='FLMetier'),
	function(object, ...)
	{
		FLFleet(FLMetiers(met=object), ...)
	}
)
setMethod('FLFleet', signature(object='FLCatches'),
	function(object, ...)
	{
		FLFleet(FLMetiers(FLMetier(object)), ...)
	}
)
setMethod('FLFleet', signature(object='FLCatch'),
	function(object, ...)
	{
		FLFleet(FLMetiers(FLMetier(FLCatches(object))), ...)
	}
)
setMethod('FLFleet', signature(object='FLFleet'),
	function(object, metier, catch, ...)
	{
    res <- object
    if(!missing(metier))
      res@metiers <- res@metiers[metier]
    if(!missing(catch))

		FLFleet(, ...)
	}
)
setMethod('FLFleet', signature(object='missing'),
	function(object, ...)
	{
		FLFleet(FLMetiers(FLMetier(FLCatches(FLCatch()))), ...)
	}
)	# }}}

# summary	{{{
setMethod('summary', signature(object='FLFleet'),
	function(object, ...)
	{
		callNextMethod(object)
		cat("\n")
		cat("Metiers: ", "\n")
		# TODO What happens when object has no metiers/catches? IM 28.08.07
		for (i in names(object@metiers))
		{
			cat("\t", i, ":\n")
			
			for (j in names(object@metiers[[i]]@catches))
				cat("\t\t", j, ": [", dim(object@metiers[[i]]@catches[[j]]@landings.n),"]\n")
		}
	}
)
# }}}

# metier(fl, me)	{{{
setGeneric('metier', function(object, metier, ...)
		standardGeneric('metier'))
setMethod('metier', signature(object='FLFleet', metier='ANY'),
	function(object, metier, ...)
		return(object@metiers[[metier]])
)
setGeneric('metier<-', function(object, metier, ..., value)
		standardGeneric('metier<-'))
setReplaceMethod('metier', signature(object='FLFleet', metier='ANY', value='FLMetier'),
	function(object, metier, ..., value)
	{
		object@metiers[[metier]] <- value
		return(object)
	}
)	# }}}

## createFLeetAccesors  {{{
createFleetAccesors <- function(slot, fun, level=c(1:5), assigment=TRUE, class='FLQuant')
  {
	# replacement function
  if(assigment == TRUE)
  {
	# generic
	eval(substitute(if (!isGeneric(SLOT)) {
		setGeneric(SLOT, function(object, ..., value)
				standardGeneric(SLOT))}, list(SLOT=paste(slot, '<-', sep=''))))
	# FLCatch
  if(1 %in% level)
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLCatch', value=class),
		function(object, value) {
			slot(object, SLOT) <- value
			return(object)}), list(SLOT=slot)))
	# FLCatches
  if(2 %in% level)
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLCatches', value=class),
		function(object, catch, value) {
			slot(object[[catch]], SLOT) <- value
			return(object)
		}),list(SLOT=slot)))
	# FLMetier
  if(3 %in% level)
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLMetier', value=class),
		function(object, catch, value) {
			slot(object@catches[[catch]], SLOT) <- value
			return(object)
		}),list(SLOT=slot)))
	# FLMetiers
  if(4 %in% level)
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLMetiers', value=class),
		function(object, metier, catch, value) {
			slot(object[[metier]]@catches[[catch]], SLOT) <- value
			return(object)
		}), list(SLOT=slot)))
	# FLFleet
  if(5 %in% level)
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLFleet', value=class),
		function(object, metier, catch, value) {
			slot(object@metiers[[metier]]@catches[[catch]], SLOT) <- value
			return(object)
		}), list(SLOT=slot)))
  }

	# accesor functions
	# generic
	eval(substitute(if (!isGeneric(SLOT)) {
		setGeneric(SLOT, function(object, ...)
				standardGeneric(SLOT))}, list(SLOT=slot)))
	# FLCatch
  if(1 %in% level)
	eval(substitute(setMethod(SLOT, signature(object='FLCatch'),
		function(object)
			return(slot(object, SLOT))), list(SLOT=slot)))
	# FLCatches
  if(2 %in% level)
	eval(substitute(setMethod(SLOT, signature(object='FLCatches'),
		function(object, catch='missing') {
			if(missing(catch))
				return(lapply(object, SLOT))
			else
				return(FUN(object[[catch]]))}),list(SLOT=slot, FUN=fun)))
	# FLMetier
  if(3 %in% level)
	eval(substitute(setMethod(SLOT, signature(object='FLMetier'),
		function(object, ...)
				return(FUN(object@catches, ...))), list(SLOT=slot, FUN=fun)))
	# FLMetiers
  if(4 %in% level)
	eval(substitute(setMethod(SLOT, signature(object='FLMetiers'),
		function(object, metier='missing', catch='missing', ...) {
      # nothing
			if (missing(metier) && missing(catch))
				stop('Either metier or catch must be specified')
      # metier
			else if(!missing(metier) && missing(catch))
				return(FUN(object[[metier]], ...))
      # catch
			else if(missing(metier) && !missing(catch))
      {
				res <- FLQuants()
				for(i in names(object))
        {
          if (catch %in% names(object[[i]]@catches))
  					res[[i]] <- FUN(object[[i]], catch=catch, ...)
        }
				return(res)
      # both
			} else
				return(FUN(object[[metier]], catch=catch, ...))}), list(SLOT=slot, FUN=fun)))
	# FLFleet
  if(5 %in% level)
	eval(substitute(setMethod(SLOT, signature(object='FLFleet'),
		function(object, ...)
				return(FUN(object@metiers, ...))), list(SLOT=slot, FUN=fun)))
}   # }}}

# FLFleet accesors	{{{
createFleetAccesors('catch', catch, c(2:5), assigment=FALSE)
createFleetAccesors('catch.n', catch.n, c(2:5), assigment=FALSE)
createFleetAccesors('catch.wt', catch.wt, c(2:5), assigment=FALSE)
createFleetAccesors('catch.sel', catch.sel, c(2:5), assigment=FALSE)
createFleetAccesors('catch.q', catch.q)
createFleetAccesors('discards', discards)
createFleetAccesors('discards.n', discards.n)
createFleetAccesors('discards.wt', discards.wt)
createFleetAccesors('discards.sel', discards.sel)
createFleetAccesors('landings', landings)
createFleetAccesors('landings.n', landings.n)
createFleetAccesors('landings.wt', landings.wt)
createFleetAccesors('landings.sel', landings.sel)
createFleetAccesors('price', price)
# }}}

# Sums(FLQuants)	{{{
setGeneric('Sums', function(object, ...)
		standardGeneric('Sums')
)
setMethod('Sums', signature(object='FLQuants'),
	function(object, na.rm=FALSE, ...) {
		if(length(object) == 1)
			return(object)
		eval(parse(text=paste('object[[', paste(seq(length(object)),
			collapse=']] + object[['), ']]', sep='')))
	}
)

setGeneric('Products', function(object, ...)
		standardGeneric('Products')
)
setMethod('Products', signature(object='FLQuants'),
	function(object, na.rm=FALSE, ...)
		eval(parse(text=paste('object[[', paste(seq(length(object)),
			collapse=']] * object[['), ']]', sep='')))
)	# }}}

## Arith    {{{
setMethod("Arith", ##  "+", "-", "*", "^", "%%", "%/%", "/"
	signature(e1 = "FLQuants", e2 = "FLQuants"),
	function(e1, e2) {
		for(i in seq(length(e1)))
			e1[[i]] <- callGeneric(e1[[i]], e2[[i]])
		return(e1)
	}
)
#setMethod("*", ##  "+", "-", "*", "^", "%%", "%/%", "/"
#	signature(e1 = "FLQuants", e2 = "FLQuants"),
#	function(e1, e2) {
#		for(i in seq(length(e1)))
#			e1[[i]] <- callGeneric(e1[[i]], e2[[i]])
#		return(e1)
#	}
#)
# }}}

# revenue	{{{
setGeneric('revenue', function(object, ...)
		standardGeneric('revenue')
)
setMethod('revenue', signature('FLCatch'),
	function(object)
    if(!all(is.na(landings.n(object))))
      return(quantSums(landings.n(object) * landings.wt(object) * price(object)))
    else
      return(landings(object) * price(object))
)
setMethod('revenue', signature('FLCatches'),
	function(object, catch=unique(names(object)), ...)
		return(lapply(object, revenue))
)
setMethod('revenue', signature('FLMetier'),
  function(object, ...)
    return(revenue(object@catches, ...))
)
setMethod('revenue', signature('FLMetiers'),
  function(object, metier, catch, ...)
  {
  if(missing(catch) && missing(metier))
    return(TRUE)
  else if(missing(catch))
    revenue(metier(object, metier))
  else if(missing(metier))
    Sums(lapply(object@metiers, revenue))
  else
    return(TRUE)
  }
)
setMethod('revenue', signature('FLFleet'),
  function(object, ...)
    return(revenue(object@metiers, ...))
) # }}}

## iter {{{
setMethod("iter", signature(object="FLFleet"),
	  function(object, iter)
	  {
		# FLQuant slots
		names <- names(getSlots(class(object))[getSlots(class(object))=="FLQuant"])
		for(s in names) 
		{
			if(dims(slot(object, s))$iter == 1)
				slot(object, s) <- iter(slot(object, s), 1)
			else
				slot(object, s) <- iter(slot(object, s), iter)
		}
		# FLMetiers
		names <- names(object@metiers)
		for (s in names)
			metier(object, s) <- iter(metier(object, s), iter)
		
		return(object)
	  }
) # }}}

# catches(fl, me, ca)	{{{
setGeneric('catches', function(object, ...)
		standardGeneric('catches')
)
setMethod('catches', signature(object='FLFleet'),
	function(object, ...)
		return(catches(object@metiers, ...))
)
setMethod('catches', signature(object='FLMetiers'),
	function(object, catch='missing', sum=FALSE, ...)
  {
    # No catch? OK if only one in object
    if(missing(catch))
      if(length(unique(unlist(lapply(object, function(x) names(x@catches))))) == 1)
        catch <- object[[1]]@catches[[1]]@name
      else
        stop('No catch was selected and object holds data for more than one catch')
    
    # identify metiers with this catch.
    idx <- unlist(lapply(object, function(x) any(catchNames(x) == catch)))

    # if index is numeric and only one metier, select from names
    if(length(object) == 1 & is.numeric(catch))
      catch <- catchNames(object)[catch]
    res <- lapply(object[idx], catches, catch=catch)
    
    if(length(res) > 1 && sum==TRUE)
    {
      res[1:2] <- mcf(res[[1]], res[[2]])
      res[[1]] <- addFLCatch(res[[1]], res[[2]])
      if(length(res) > 2)
        for(i in seq(3, length(res)))
        {
          res[[i]] <- mcf(res[[1]], res[[i]])[[2]]
          res[[1]] <- addFLCatch(res[[1]], res[[i]])
        }
      return(FLCatches(res[[1]]))
    }
    return(FLCatches(res))
  }
)
setMethod('catches', signature(object='FLMetier'),
	function(object, catch='missing', ...)
  {
		if(missing(catch))
      return(object@catches)
    if (length(catch) == 1)
      return(object@catches[[catch]])
    else
      return(object@catches[catch])
  }
)	# }}}

# catches<-(fl, ca)	{{{
setGeneric('catches<-', function(object, catch, ..., value)
		standardGeneric('catches<-')
)
setMethod('catches<-', signature(object='FLMetier', value='FLCatch'),
	function(object, catch, ..., value)
  {
    object@catches[[catch]] <- value
    return(object)
  }
)
setMethod('catches<-', signature(object='FLMetier', value='FLCatches'),
	function(object, catch, ..., value)
  {
    object@catches <- value
    return(object)
  }
) # }}}

# FLMetier accesors for FLFleet {{{
setMethod('effshare', signature(object='FLMetiers'),
  function(object, metier=names(object))
  {
    if(length(metier) == 1)
      return(object[[metier]]@effshare)
    else
      return(FLQuants(lapply(object[metier], effshare)))
  }
)
setMethod('effshare', signature(object='FLFleet'),
  function(object, ...)
    return(effshare(object@metiers, ...))
)
setMethod('vcost', signature(object='FLMetiers'),
  function(object, metier=names(object))
  {
    if(length(metier) == 1)
      return(object[[metier]]@vcost)
    else
      return(FLQuants(lapply(object[metier], vcost)))
  }
)
setMethod('vcost', signature(object='FLFleet'),
  function(object, ...)
    return(vcost(object@metiers, ...))
)
# }}}

## dims {{{
setMethod("dims", signature(obj="FLFleet"),
  # Returns a list with different parameters
  function(obj, ...)
	{
		qnames <- names(getSlots(class(obj))[getSlots(class(obj))=="FLQuant"])
		return(list(
      metiers=names(obj@metiers),
      catches=unique(unlist(lapply(obj@metiers, function(x) names(x@catches)))),
      quant = quant(slot(obj, qnames[1])),
      min=min(as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) dimnames(x@landings.n)[[1]][1]))))),
      max=max(as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) dimnames(x@landings.n)[[1]][dim(x@landings.n)[1]]))))),
      minyear=min(as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) dimnames(x@landings.n)[[2]][1]))))),
      maxyear=max(as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) dimnames(x@landings.n)[[2]][dim(x@landings.n)[2]]))))),
      unit=unique(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) length(dimnames(x@landings.n)[[3]]))))),
      season=unique(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) length(dimnames(x@landings.n)[[4]]))))),
      area=unique(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) length(dimnames(x@landings.n)[[5]]))))),
      iter=max(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) qapply(x, function(x) length(dimnames(x)[[6]]))))))
    ))
    }
)    # }}}

## window    {{{
setMethod("window", signature(x="FLFleet"),
	  function(x, start=dims(x)$minyear, end=dims(x)$maxyear, extend=TRUE, frequency=1) {

    # window fleet
    x <- qapply(x, window, start, end, extend, frequency)

    # window metiers
    x@metiers <- lapply(x@metiers, window, start, end, extend, frequency)

    # window catches
    for(i in seq(length(x@metiers)))
      x@metiers[[i]]@catches <- lapply(x@metiers[[i]]@catches, window, start, end, extend, frequency)

		x@range["minyear"] <- start
		x@range["maxyear"] <- end

		return(x)
	}
)	# }}}

## effort		{{{
	setGeneric("effort", function(object, metier, ...)
		standardGeneric("effort"))

setMethod("effort", signature(object="FLFleet", metier="missing"),
	function(object)
    return(slot(object, "effort")))

setMethod("effort", signature(object="FLFleet", metier="character"),
	function(object, metier)
    return(slot(object, "effort") * slot(metier(object, metier), "effshare")))

if (!isGeneric("effort<-"))
	setGeneric("effort<-", function(object, ..., value)
		standardGeneric("effort<-"))
setReplaceMethod("effort", signature(object="FLFleet", value="FLQuant"),
	function(object, value)
  {
		slot(object, "effort") <- value
    return(object)
  })
# }}}

# catchNames {{{
setMethod('catchNames', signature(object='FLCatches'),
  function(object)
  {
    return(unname(unlist(lapply(object, catchNames))))
  }
)
setMethod('catchNames', signature(object='FLMetier'),
  function(object)
  {
    return(catchNames(object@catches))
  }
)
setMethod('catchNames', signature(object='FLMetiers'),
  function(object)
  {
    return(unique(unlist(lapply(object, catchNames))))
  }
)
setMethod('catchNames', signature(object='FLFleet'),
  function(object)
  {
    return(catchNames(object@metiers))
  }
) 
setMethod('catchNames', signature(object='FLFleets'),
  function(object)
  {
    return(unique(unlist(lapply(object, catchNames))))
  }
) # }}}

# trim {{{
setMethod('trim', signature(x='FLFleet'),
  function(x, ...)
  {
    x <- callNextMethod()
    x@metiers <- lapply(x@metiers, trim, ...)
    return(x)
  }
) # }}}

# propagate {{{
setMethod('propagate', signature(object='FLFleet'),
  function(object, ...)
  {
    object <- qapply(object, propagate, ...)
    object@metiers <- lapply(object@metiers, propagate, ...)
    return(object)
  }
) # }}}

# computeCatch  {{{
setMethod('computeCatch', signature(object='FLCatch'),
  function(object)
    return(quantSums(catch.n(object) * catch.wt(object)))
)
setMethod('computeDiscards', signature(object='FLCatch'),
  function(object)
    return(quantSums(discards.n(object) * discards.wt(object)))
)
setMethod('computeLandings', signature(object='FLCatch'),
  function(object)
    return(quantSums(landings.n(object) * landings.wt(object)))
)

setMethod('computeCatch', signature(object='FLMetier'),
  function(object, catch=names(object@catches))
  Sums(lapply(object@catches[catch], computeCatch))
)
setMethod('computeDiscards', signature(object='FLMetier'),
  function(object, catch=names(object@catches))
  lapply(object@catches[catch], computeDiscards)
)
setMethod('computeLandings', signature(object='FLMetier'),
  function(object, catch=names(object@catches))
  lapply(object@catches[catch], computeLandings)
)

setMethod('computeCatch', signature(object='FLFleet'),
  function(object, ...)
  lapply(object@metiers, computeCatch, ...)
)
setMethod('computeDiscards', signature(object='FLFleet'),
  function(object, ...)
  lapply(object@metiers, computeDiscards, ...)
)
setMethod('computeLandings', signature(object='FLFleet'),
  function(object, ...)
  lapply(object@metiers, computeLandings, ...)
) # }}}

# "[" and "[["             {{{
setMethod("[", signature(x="FLFleet", i="ANY", j="missing"),
  function(x, i, drop=FALSE)
  {
	  if (missing(i))
      return(x)
    x@metiers <- x@metiers[i]
    return(x)
	}
)

setMethod("[", signature(x="FLFleet", i="ANY", j="ANY"),
  function(x, i, j, drop=FALSE)
  {
    if(!missing(i))
      x <- x[i]
    if(!missing(j))
      x@metiers <- lapply(x@metiers, '[', j)
    return(x)
	}
)

setMethod("[[", signature(x="FLFleet", i="ANY", j="missing"),
  function(x, i, drop=FALSE)
  {
	  if (missing(i))
      stop("invalid subscript type")
    return(x@metiers[[i]])
	}
) # }}}

# as.data.frame {{{
setMethod('as.data.frame', signature(x='FLFleet', row.names='missing',
  optional='missing'), function(x)
  {
    df <- callNextMethod()
    df <- cbind(df, metier='NA', catch='NA')

    for (i in 1:length(x@metiers))
    {
      df <- rbind(df, cbind(catch='NA', metier=names(x@metiers)[[i]],
        as.data.frame(x@metiers[[i]])))

      for (j in 1:length(x@metiers[[i]]@catches))
      df <- rbind(df, cbind(catch=names(x@metiers[[i]]@catches)[[j]],
        metier=names(x@metiers)[[i]], as.data.frame(x@metiers[[i]]@catches[[j]])))
    }
    return(df)
  }
) # }}}
