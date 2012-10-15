setClass("cpues",
   representation(
      "FLlst"))

      setGeneric('cpues', function(object, ...)
  	standardGeneric('cpues'))

setMethod("cpues", signature(object="cpue"), function(object, ...) {
    lst <- c(object, list(...))
    cpues(lst)})

setMethod("cpues", signature(object="missing"),
  function(...) {
    # empty
    if(missing(...)){
	  	new("cpues")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('cpues',  c(list(object=object), args))}})

setMethod("cpues", signature(object="list"),
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
    args <- c(list(Class="cpues", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args))}) 



