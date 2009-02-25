# refpts - «Short one line description»
# refpts

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 25 Feb 2009 13:03
# $Id:  $

# refpts Class {{{
validrefpts <- function(object)
{
  return(TRUE)
}

setClass('refpts', representation('FLPar'),
  prototype=prototype(new('FLPar', array(NA, dim=c(5,8,1),
  dimnames=list(refpt=c('f0.1', 'fmax', 'spr.30', 'msy', 'mey'), value=c('harvest', 
  'yield', 'rec', 'ssb', 'biomass', 'revenue', 'cost', 'profit'), iter=1)))),
  validity=validrefpts)

# }}}

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
  function(object, ...)
  {
    #
    
    # look for dimnames in array, and reshape
    if(is.null(dimnames(object)))
        dimnames(object) <- list(refpt=c('f0.1', 'fmax', 'spr.30', 'msy', 'mey'),
        value=c('harvest', 'yield', 'rec', 'ssb', 'biomass', 'revenue', 'cost',
        'profit'), iter=1:dim(object)[3])

    return(new('refpts', object))
  }
)
# }}}
