validAspic <- function(object) {
  ## Catch must be continous
  yrs<-dimnames(catch(object))$year
  
  if (!all(yrs == ac(dims(catch(object))$minyear:dims(catch(object))$maxyear)))
      return("years in catch not continous")

  # range
  dims <-dims(object)
  range<-as.list(object@range)

  return(TRUE)}

models=c("LOGISTIC", #Schaefer
         "GENGRID",  #generalized model at grid of values or at one specified value
         "FOX",      #Fox
         "GENFIT")   #Fit the generalized model and estimate its exponent directly.

conditioning=c("YLD", #Condition fitting on yield (recommended for most analyses).
               "EFT") #Condition fitting on fishing-effort rate

objFn=c("SSE",     #Sum of squared errors (recommended default).
        "WTDSSE",  #SSE with annual data weighting
        "LAV")     #Least absolute values (robust objective function).


setClass('aspic', representation(
    "FLComp",
    model         ="character",
    catch         ='FLQuant',
    stock         ='FLQuant',
    harvest       ='FLQuant',
    ctrl          ='aspicControl',
    params        ='FLPar',
    vcov          ='array',
    hessian       ='array',
    diags         ="data.frame",
    logLik        ='numeric',
    rsdlVar       ='numeric',
    dof           ='array',
    stopmess      ="character"),
  prototype(
    range         =unlist(list(minyear=as.numeric(NA), maxyear=as.numeric(NA))),
    model         ="schaefer",
    catch         =FLQuant(),
    stock         =FLQuant(),
    harvest       =FLQuant(),
    ctrl          =aspicControl(),
    params        =FLPar(NA,dimnames=list(param=c("msy","k","b0"),iter=1))
    ),
  validity=validAspic) # }}}
  
is.aspic <- function(x)
  return(inherits(x, "aspic"))

setClass("aspics",
   representation(
      "FLlst"))

      setGeneric('aspics', function(object, ...)
    standardGeneric('aspics'))

setMethod("aspics", signature(object="aspic"), function(object, ...) {
    lst <- c(object, list(...))
    aspics(lst)})

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
      do.call('aspics',  c(list(object=object), args))}})

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
      args[!names(args)%in%'names'])

    return(do.call('new', args))})


