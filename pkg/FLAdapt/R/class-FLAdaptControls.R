##########################################################################
### FLAdapt
### This file contains code just for the class FLAdapt

### 04-03-2005 implemented as a S4 Class by L T Kell
### using original Fortran code by Clay Porch 
#########################################################################
## Two main classes the Adapt class and the control class
### classes ##############################################################

setClass("FLAdaptControls",
   representation(
      "FLlst"))

      setGeneric('FLAdaptControls', function(object, ...)
    standardGeneric('FLAdaptControls'))

# constructor
setMethod("FLAdaptControls", signature(object="FLAdaptControl"), function(object, ...) {
    lst <- c(object, list(...))
    FLAdaptControls(lst)
})

setMethod("FLAdaptControls", signature(object="missing"),
  function(...) {
    # empty
    if(missing(...)){
	  	new("FLAdaptControls")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLAdaptControls',  c(list(object=object), args))}})

setMethod("FLAdaptControls", signature(object="list"),
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
    args <- c(list(Class="FLAdaptControls", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(do.call('new', args))}) 


is.FLAdaptControls = function(x)
	return(inherits(x, "FLAdaptControls"))

