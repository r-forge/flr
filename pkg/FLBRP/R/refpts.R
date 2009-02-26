# refpts - «Short one line description»
# refpts

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 26 Feb 2009 12:11
# $Id:  $

# show {{{
setMethod('show', signature(object='refpts'),
  function(object)
  {
		cat("An object of class \"refpts\":\n")
		if(dim(object)[3] > 1)
			cat("iters: ", dim(object)[1],"\n")
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
  function(object, iter=1, ...)
  {
    # reshape object
    if(length(dim(object)) == 2)
      object <- array(object, dim=c(dim(object), iter))

    # add dimnames
    if(is.null(dimnames(object)))
    {  
      # default dnames
      dimnames(object) <- list(
        refpt=c('f0.1', 'fmax', 'spr.30', 'msy', 'mey')[1:dim(object)[1]],
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
# }}}
