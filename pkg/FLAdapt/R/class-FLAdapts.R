setClass("FLAdapts",
   representation(
      "FLlst"))

      setGeneric('FLAdapts', function(object, ...)
  	standardGeneric('FLAdapts'))

# constructor
setMethod("FLAdapts", signature(object="FLAdapt"), function(object, ...) {
    lst <- c(object, list(...))
    FLAdapts(lst)})

setMethod("FLAdapts", signature(object="missing"),
  function(...) {
    # empty
    if(missing(...)){
	  	new("FLAdapts")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLAdapts',  c(list(object=object), args))}})

setMethod("FLAdapts", signature(object="list"),
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
    args <- c(list(Class="FLAdapts", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(do.call('new', args))}) 


is.FLAdapts = function(x)
	return(inherits(x, "FLAdapts"))




