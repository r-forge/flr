# FLlst-class.R - 
# FLCore/R/FLlst-class.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: FLlst-class.R 1195 2011-08-30 08:33:23Z imosqueira $

# getPlural {{{
setMethod("getPlural", signature(object="ANY"),
  function(object) {
    return("list")
  }
)
setMethod("getPlural", signature(object="character"),
  function(object) {
    getPlural(new(object))
  }
)

setMethod("getPlural", signature(object="FLStock"),
  function(object) {
    return("FLStocks")
  }
)

setMethod("getPlural", signature(object="FLQuant"),
  function(object) {
    return("FLQuants")
  }
)

setMethod("getPlural", signature(object="FLCohort"),
  function(object) {
    return("FLCohorts")
  }
)

setMethod("getPlural", signature(object="FLCatch"),
  function(object) {
    return("FLCatches")
  }
)

setMethod("getPlural", signature(object="FLMetier"),
  function(object) {
    return("FLMetiers")
  }
)

setMethod("getPlural", signature(object="FLIndex"),
  function(object) {
    return("FLIndices")
  }
)

setMethod("getPlural", signature(object="FLBiol"),
  function(object) {
    return("FLBiols")
  }
)

setMethod("getPlural", signature(object="FLFleet"),
  function(object) {
    return("FLFleets")
  }
)

setMethod("getPlural", signature(object="FLSR"),
  function(object) {
    return("FLSRs")
  }
)


# }}}

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
