# refpts - «Short one line description»
# FLBRP/R/refpts.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cerviño, IEO
# Last Change: 28 Feb 2009 23:32
# $Id:  $

# show {{{
setMethod('show', signature(object='refpts'),
  function(object)
  {
		cat("An object of class \"refpts\":\n")
		if(dim(object)[3] > 1)
			cat("iters: ", dim(object)[3],"\n")
    cat("\n")
    show(apply(object, 1:2, median, na.rm=TRUE))
  }
) # }}}

# constructors  {{{
if (!isGeneric("refpts"))
	setGeneric("refpts", function(object, ...)
		standardGeneric("refpts"))

# refpts(array)
setMethod('refpts', signature(object='array'),
  function(object, iter=1, refpt=c('f0.1', 'fmax', 'spr.30', 'msy', 'mey'), ...)
  {
    # reshape object
    if(length(dim(object)) < 3)
      object <- array(object, dim=c(dim(object), iter))
    else if (dim(object)[3] < iter)
      object <- array(object, dim=c(dim(object)[-3], iter))

    # add dimnames
    if(is.null(dimnames(object)))
    {  
      # default dnames
      dimnames(object) <- list(
        refpt=refpt[1:dim(object)[1]],
        value=c('harvest', 'yield', 'rec', 'ssb', 'biomass', 'revenue', 'cost',
          'profit')[1:dim(object)[2]],
        iter=1:dim(object)[3])
    }
     
     return(new('refpts', object))
  }
)
setMethod('refpts', signature(object='missing'),
  function(...)
  {
    refpts(array(as.numeric(NA), dim=c(5,8)), ...)
  }
)

setMethod('refpts', signature(object='numeric'),
  function(object, ...)
  {
    refpts(array(object, dim=c(1,8,1)), ...)
  }
)
setMethod('refpts', signature(object='refpts'),
  function(object, ...)
  {
    refpts(object@.Data, ...)
  }
)

# }}}

# propagate {{{
setMethod('propagate', signature(object='refpts'),
  function(object, iter, fill.iter=TRUE)
  {
    res <- refpts(object, iter=iter)
    if(fill.iter== FALSE)
      res[,,2:iter] <- as.numeric(NA)
    return(res)
   }
) # }}}
