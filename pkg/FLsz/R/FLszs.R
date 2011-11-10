setClass("FLszs",
   representation(
      "FLlst"))

      setGeneric('FLszs', function(object, ...)
  	standardGeneric('FLszs'))

# constructor
setMethod("FLszs", signature(object="FLsz"), function(object, ...) {
    lst <- c(object, list(...))
    FLszs(lst)
})

setMethod("FLszs", signature(object="missing"),
  function(...) {
    # empty
    if(missing(...)){
	  	new("FLszs")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLszs',  c(list(object=object), args))
	  }
  }
)

setMethod("FLszs", signature(object="list"),
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
    args <- c(list(Class="FLszs", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}



