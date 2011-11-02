setClass("FLBioDyms",
   representation(
      "FLlst"))

      setGeneric('FLBioDyms', function(object, ...)
  	standardGeneric('FLBioDyms'))

# constructor
setMethod("FLBioDyms", signature(object="FLBioDym"), function(object, ...) {
    lst <- c(object, list(...))
    FLBioDyms(lst)
})

setMethod("FLBioDyms", signature(object="missing"),
  function(...) {
    # empty
    if(missing(...)){
	  	new("FLBioDyms")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLBioDyms',  c(list(object=object), args))
	  }
  }
)

setMethod("FLBioDyms", signature(object="list"),
  function(object, ...) {
    
    args <- list(...)
    
    # names in args, ... 
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLBioDyms", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}



