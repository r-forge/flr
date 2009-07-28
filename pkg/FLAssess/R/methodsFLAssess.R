# FLAssess-methods - «Short one line description»
# FLAssess/R/FLAssess-methods.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id$

# summary {{{
setMethod("summary", signature(object="FLAssess"),
  function(object)
  {
  invisible(callNextMethod())
  # index
    if(length(object@index) >= 1)
    {
      indnm <- names(object@index)
	    for (i in 1:length(indnm))
	    {
    		cat(indnm[i], ":\n")
		  	cat("\tindex     : [", dim(object@index[[i]]),"]\n")
			  cat("\tindex.res : [", dim(object@index.res[[i]]),"]\n")
  			cat("\tindex.hat : [", dim(object@index.hat[[i]]),"]\n")
	  		cat("\tindex.var : [", dim(object@index.var[[i]]),"]\n")
	    }
    }
  }
) # }}}

# plot  {{{
setMethod('plot', signature(x='FLAssess', y='missing'),
  function(x, ...)
  {
  xyplot(data~year, groups=qname, FLQuants(stock=quantSums(x@stock.n),
  catch=quantSums(x@catch.n)), type='b', pch=19,
  panel = panel.superpose.2)
  }
) # }}}

# merge {{{
# merge results from FLAssess into FLStock
if (!isGeneric("merge")) {
    setGeneric("merge", useAsDefault = merge)
}
setMethod("merge", signature(x="FLStock", y="FLAssess"),
  function(x, y, ...)
  {
    quant <- quant(stock.n(x))
    dnx <- dimnames(stock.n(x))
    dny <- dimnames(y@stock.n)

    # check dimensions match
    if(!all.equal(dnx[c(-2,-6)], dny[c(-2,-6)]))
      stop("Mismatch in dimensions: only year can differ between stock and assess")

    # same plusgroup
    if(x@range['plusgroup'] != x@range['plusgroup'])
      stop("Mismatch in plusgroup: x and y differ")

    # year ranges match?
    if(!all(dny[['year']] %in% dnx[['year']]))
    {
      years <- as.numeric(unique(c(dnx$year, dny$year)))
      x <- window(x, start=min(years), end=max(years))
      y <- window(y, start=min(years), end=max(years))
    }
    x@desc <- paste(x@desc, "+ FLAssess:", y@name)
    x@stock.n <- y@stock.n
    x@harvest <- y@harvest
    x@range=c(unlist(dims(x)[c('min', 'max', 'plusgroup','minyear', 'maxyear')]),
      x@range[c('minfbar', 'maxfbar')])
        
    return(x)
  }
)   # }}}

# "+"      {{{
setMethod("+", signature(e1="FLStock", e2="FLAssess"),
	function(e1, e2) {
    if(validObject(e1) & validObject(e2))
      return(merge(e1, e2))
    else
      stop("Input objects are not valid: validObject == FALSE")
    }
)
setMethod("+", signature(e1="FLAssess", e2="FLStock"),
	function(e1, e2) {
    if(validObject(e1) & validObject(e2))
      return(merge(e2, e1))
    else
      stop("Input objects are not valid: validObject == FALSE")
    }
)   # }}}


# assess  {{{
if (!isGeneric("assess"))
	setGeneric("assess", function(control, ...)
    	standardGeneric("assess")) # }}}

# diagnostics {{{
# }}}

# harvest(FLBiol)	{{{
setMethod('harvest', signature(object='FLBiol'),
	function(object, catch)
	{
	n <- object@n
	m <- object@m
	# 
    if (!all(unlist(dims(m))==unlist(dims(catch)))) stop("m & catch.n dims don't match")
    if (!all(unlist(dims(m))==unlist(dims(n))))     stop("m & stock.n dims don't match")
    res <- .Call("FLRCalcF", m, catch, n)

    units(res)<-"f"
    
    return(res)
    }
)	# }}}
