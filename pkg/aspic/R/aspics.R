setClass("aspics",
   representation(
      "FLlst"))

      setGeneric('aspics', function(object, ...)
  	standardGeneric('aspics'))

# constructor
setMethod("aspics", signature(object="aspic"), function(object, ...) {
    lst <- c(object, list(...))
    aspics(lst)
})

setMethod("aspics", signature(object="missing"),
  function(...) {
    # empty
    if(missing(...)){
	  	new("aspics")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('aspics',  c(list(object=object), args))
	  }
  }
)

setMethod("aspics", signature(object="list"),
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
    args <- c(list(Class="aspics", .Data=object, names=names),
      args[!names(args) %in% 'names'])

    return(
      do.call('new', args)
      )

}) # }}}



