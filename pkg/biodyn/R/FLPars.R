
# FLPars {{{
FLPars <- setClass("FLPars", contains=c("FLlst"),
                   validity=function(object) {
                     # All items are FLPar
                     if(!all(unlist(lapply(object, is, 'FLPar'))))
                       return("Components must be FLPar")	
                     
                     return(TRUE)})

setGeneric('FLPars', function(object, ...)
  standardGeneric('FLPars'))

# constructor
setMethod("FLPars", signature(object="FLPar"), function(object, ...) {
  lst <- c(object, list(...))
  FLPars(lst)
})

setMethod("FLPars", signature(object="missing"),
          function(...) {
            # empty
            if(missing(...)){
              new("FLPars")
              # or not
            } else {
              args <- list(...)
              object <- args[!names(args)%in%c('names', 'desc', 'lock')]
              args <- args[!names(args)%in%names(object)]
              do.call('FLPars',  c(list(object=object), args))
            }
          }
)

setMethod("FLPars", signature(object="list"),
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
            args <- c(list(Class="FLPars", .Data=object, names=names),
                      args[!names(args)%in%'names'])
            
            return(
              do.call('new', args)
            )
            
          }) # }}}