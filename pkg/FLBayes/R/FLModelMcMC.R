# FLModelMcMC - «Short one line description»
# FLModelMcMC

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id: FLModel.R,v 1.27 2007/11/01 11:05:58 imosqueira Exp $

## McMCpdfs  {{{
validMcMCpdfs <- function(object) {
	if(!all(lapply(b, is, "Distribution")))
		stop("Elements of the McMCpdfs list must be of class 'Distribution'")
	return(TRUE)
}

setClass("McMCpdfs",
	representation("FLlst"),
	prototype(FLlst()),
	validity=validMcMCpdfs
)

# show
setMethod("show", signature(object="McMCpdfs"),
	function(object) {
		lapply(object, print)
	}
)

# plot
setMethod("plot", signature(x="McMCpdfs", y="missing"),
	function(x, ...){
		
		npdfs <- length(x)
		par(mfrow=c(ceiling(npdfs/2), 2))

		for (i in names(x)) {

			if(is.finite(q(x[[i]])(0))) lower <- q(x[[i]])(0)
            else lower <- q(x[[i]])(TruncQuantile)
   
            if(is.finite(q(x[[i]])(1))) upper <- q(x[[i]])(1)
            else upper <- q(x[[i]])(1 - TruncQuantile)

            dist = upper - lower
            grid <- seq(from = lower - 0.1 * dist, to = upper + 0.1 * dist, length = 1000)

			plot(grid, d(x[[i]])(grid), type='l', xlab=i, ylab="", yaxt="n", ...)
		}
	}
)   # }}}

# FLModelMcMC
setClass('FLModelMcMC', representation('FLModel', pdfs='McMCpdfs'),
  prototype(params=FLPar(dimnames=list(iter=1, params="", chains=1)), 
  pdfs=new('McMCpdfs')))

# FLchain
setClass('FLchain', representation('FLPar', mcvar='numeric', rngseed='numeric'))

setGeneric('FLchain', function(object, ...)
		standardGeneric('FLchain'))
setMethod('FLchain', signature(object='array'),
  function(object, rngseed=rep(1, dim(object)[2]), mcvar=rep(1, dim(object)[2]), ...)
  {
    new('FLchain', FLPar(object, dimnames=list(iter=1:dim(object)[1],
      params=1:dim(object)[2], iter=1:dim(object)[3])), mcvar=mcvar, rngseed=rngseed)
  }
)

# FLModelMcMC
setGeneric('FLModelMcMC', function(object, ...)
		standardGeneric('FLModelMcMC'))

setMethod('FLModelMcMC', signature(object='formula'),
  function(object, ...)
  {
    args <- list(...)
    res <- do.call('FLModel', list(object, args[names(args)!='pdfs']))
    if(!is.null(args[['pdfs']]))
      slot(object, 'pdfs') <- args[['pdfs']]
    return(res)
  }
)

# pdfs = list
# pdfs = McMCpdfs

# summary
setMethod('summary', signature(object='FLModelMcMC'),
  function(object)
  {
    callNextMethod()
  }
)
