# FLlst-class.R - 
# FLCore/R/FLlst-class.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# FLlst class{{{
vFLl <- function(object){

	# Make sure the list contains all items of the same class
	cls <- unlist(lapply(object, class))
  if(any(cls != cls[1]))
	  stop("Components must be of the same class!")	

  # All elements in the list are validObjects themselves
  if(!all(unlist(lapply(object, validObject))))
	  stop("Components must be valid objects themselves (validObject == TRUE)")	

	# Everything is fine
	return(TRUE)
}

# class
setClass("FLlst", contains="list",
  representation(names="character", desc="character", lock="logical"),
	prototype(lock=FALSE),
	validity=vFLl
) # }}}

# getPlural {{{
getPlural <- function(object)
{
  switch(class(object),
    'FLQuant'='FLQuants',
    'FLCohort'='FLCohorts',
    'FLCatch'='FLCatches',
    'FLMetier'='FLMetiers',
    'FLStock'='FLStocks',
    'FLIndex'='FLIndices',
    'FLBiol'='FLBiols',
    'FLFleet'='FLFleets',
    'list'
    )
} # }}}

# FLlst() {{{
setMethod("FLlst", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLlst", lst)
})

setMethod("FLlst", signature(object="missing"), function(...){
	if(missing(...)){
		new("FLlst")
	} else { 
		lst <- list(...)
		new("FLlst", lst)
	}
})

setMethod("FLlst", "list", function(object){
	new("FLlst", object)
}) # }}}

# coerce {{{
setAs("FLlst", "list", function(from){
	lst <- from@.Data
	names(lst) <- from@names
	attr(lst, "desc") <- from@desc # check when it's empty insert something
	lst
}) # }}}


# FLComps {{{
vFLCs <- function(object) {

  # all elements inherit from class FLComp
  if(!all(unlist(lapply(object, is, 'FLComp'))))
    return("All elements must be of a class that inherits from FLComp")

  return(TRUE)
}

setClass("FLComps", contains=c("FLlst"), validity=vFLCs)
# }}}

# FLStocks {{{
vFLSs <- function(object){
	
  # All items are FLStock
  if(!all(unlist(lapply(object, is, 'FLStock'))))
      return("Components must be FLStock")	
	
	return(TRUE)
}

# class
setClass("FLStocks", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLStocks", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLStocks", lst)
})

setMethod("FLStocks", signature(object="missing"),
  function(...) {
  	if(missing(...)){
	  	new("FLStocks")
  	} else { 
	  	lst <- list(...)
		  new("FLStocks", lst)
	  }
  }
)

setMethod("FLStocks", "list", function(object){
	new("FLStocks", object)
}) # }}}

# FLIndices {{{
vFLSs <- function(object){
	
  # All items are FLIndex
  if(!all(unlist(lapply(object, is, 'FLIndex'))))
      return("Components must be FLIndex")	
	
	return(TRUE)
}

# class
setClass("FLIndices", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLIndices", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLIndices", lst)
})

setMethod("FLIndices", signature(object="missing"),
  function(...) {
  	if(missing(...)){
	  	new("FLIndices")
  	} else { 
	  	lst <- list(...)
		  new("FLIndices", lst)
	  }
  }
)

setMethod("FLIndices", "list", function(object){
	new("FLIndices", object)
}) # }}}

# FLBiols {{{
vFLSs <- function(object){
	
  # All items are FLBiol
  if(!all(unlist(lapply(object, is, 'FLBiol'))))
      return("Components must be FLBiol")	
	
	return(TRUE)
}

# class
setClass("FLBiols", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLBiols", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLBiols", lst)
})

setMethod("FLBiols", signature(object="missing"),
  function(...) {
  	if(missing(...)){
	  	new("FLBiols")
  	} else { 
	  	lst <- list(...)
		  new("FLBiols", lst)
	  }
  }
)

setMethod("FLBiols", "list", function(object){
	new("FLBiols", object)
}) # }}}

# FLCatches {{{
vFLSs <- function(object){
	
  # All items are FLCatch
  if(!all(unlist(lapply(object, is, 'FLCatch'))))
      return("Components must be FLCatch")	
	
	return(TRUE)
}

# class
setClass("FLCatches", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLCatches", signature(object="FLCatch"), function(object, ...) {
    lst <- c(object, list(...))
    FLCatches(lst)
})

setMethod("FLCatches", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLCatches")
    # or not
  	} else {
      FLCatches(list(...))
	  }
  }
)

setMethod("FLCatches", signature(object="list"),
  function(object, ...) {

    args <- list(...)

    if(!"names" %in% names(args)) {
      if(is.null(names(object))) {
        names <- unlist(lapply(object, name))
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    } else
      names <- names(object)

    new("FLCatches", object, names=names)
}) # }}}

# FLMetiers {{{
vFLSs <- function(object){
	
  # All items are FLMetier
  if(!all(unlist(lapply(object, is, 'FLMetier'))))
      return("Components must be FLMetier")	
	
	return(TRUE)
}

# class
setClass("FLMetiers", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLMetiers", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLMetiers", lst)
})

setMethod("FLMetiers", signature(object="missing"),
  function(...) {
  	if(missing(...)){
	  	new("FLMetiers")
  	} else { 
	  	lst <- list(...)
		  new("FLMetiers", lst)
	  }
  }
)

setMethod("FLMetiers", "list", function(object){
	new("FLMetiers", object)
}) # }}}

# FLFleets {{{
vFLSs <- function(object){
	
  # All items are FLFleet
  if(!all(unlist(lapply(object, is, 'FLFleet'))))
      return("Components must be FLFleet")	
	
	return(TRUE)
}

# class
setClass("FLFleets", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLFleets", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLFleets", lst)
})

setMethod("FLFleets", signature(object="missing"),
  function(...) {
  	if(missing(...)){
	  	new("FLFleets")
  	} else { 
	  	lst <- list(...)
		  new("FLFleets", lst)
	  }
  }
)

setMethod("FLFleets", "list", function(object){
	new("FLFleets", object)
}) # }}}

# FLQuants {{{
# validity
vFLQs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLQuant")) stop("Components must be FLQuant")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLQuants", contains="FLlst",
	validity=vFLQs
)

# constructor
setGeneric("FLQuants", function(object, ...){
	standardGeneric("FLQuants")
	}
)

setMethod("FLQuants", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLQuants", lst)
})

setMethod("FLQuants", "missing", function(...){
	if(missing(...)){
		new("FLQuants")
	} else { 
		lst <- list(...)
		new("FLQuants", lst)
	}
})

setMethod("FLQuants", "list", function(object){
	new("FLQuants", object)
})

setMethod("FLQuants", "FLQuants", function(object){
	return(object)
}) # }}}

#! FLCohorts

# validity
vFLQs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLCohort")) stop("Components must be FLCohort")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLCohorts", contains="FLlst",
	validity=vFLQs
)

# constructor
setGeneric("FLCohorts", function(object, ...){
	standardGeneric("FLCohorts")
	}
)

setMethod("FLCohorts", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLCohorts", lst)
})

setMethod("FLCohorts", "missing", function(...){
	if(missing(...)){
		new("FLCohorts")
	} else { 
		lst <- list(...)
		new("FLCohorts", lst)
	}
})

setMethod("FLCohorts", "list", function(object){
	new("FLCohorts", object)
})

setMethod("FLCohorts", "FLCohorts", function(object){
	return(object)
})
