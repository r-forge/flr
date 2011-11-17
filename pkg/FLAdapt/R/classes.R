##########################################################################
### FLAdapt
### This file contains code just for the class FLAdapt

### 04-03-2005 implemented as a S4 Class by L T Kell
### using original Fortran code by Clay Porch 
#########################################################################
## Two main classes the Adapt class and the control class

### classes ##############################################################
validFLAdaptControl<-function(object){
	# Everything is fine
	return(TRUE)}

setClass("FLAdaptControl",
		representation(
				##Simplex options
				seed         ="integer",   #Random number seed
				maxit        ="integer",   #Maximum number of amoeba simplex search restarts
				check        ="logical",   #check flag, convergence is declared when CHECKFLAG number of consecutive restarts result in parameter estimates that vary by less than 1% 
				pdev         ="numeric",   #standard deviation controlling the random specification of vertices for the initial simplex of each restart

				##General options
				season		   ="numeric",   #Spawning season as fraction of a year
				optionF      ="logical",   #Option to use F's as terminal year parameters default is true, if false then use N's
				
				##Index options
				qEst        ="logical",   #Estimate q in search if true, default is false use concentrated MLE's
				qScale      ="logical",   #Scale indices (i.e. divide index values by their mean, default is true
				qCv         ="numeric",   #Index weighting option default is "sd", alternative options are "input" or "mlm", so-called maximum likelihood method
				qAdd        ="logical",   #Variance scaling factor default is false (i.e. multipicative)
				
				##Selectivity options
				selPenalty   ="logical",   #Links selectivities in the last n years, default is false
				selSigma     ="numeric",   #sigma (i.e. penalty)
				selNyr       ="integer",  #number of years
				selMinage    ="integer",  #first age
				selMaxage    ="integer",  #last age
				
				##Stock recruit options
				srPenalty   ="logical",   #Imposes stock recruitment relationship (penalises departures from Beverton and Holt model)   
				srSigma     ="numeric",   #sigma (i.e. penalty)
				srNyr       ="integer",   #number of years
				srPdf       ="character", #Error model
				
				##Recruitment options
				recPenalty  ="logical",   #Links rectuitments in th elast n years
				recSigma    ="numeric",   #sigma (i.e. penalty)
				recMinyr    ="numeric",   #first year to which penalty applies
				recMaxyr    ="numeric",   #last year to which penalty applies
				
				##Catch options
				catchPenalty="logical",    #
				catchSigma  ="numeric",    #sigma (i.e. penalty)
				catchPdf    ="character",  #PDF of catch
				
				##Parameter options as in the VPA2Box *.p file
				paramTermage="data.frame", #terminal ages
				paramFratio ="data.frame", #f ratio for oldest age or plusgroup
 			 paramSrr   ="data.frame", #stock recruit parameters
				paramVar    ="data.frame", #variance scaling parameters
				paramQ      ="data.frame"  #catchability parameters
				),
		prototype=prototype(
				seed         =as.integer(-911),
				maxit        =as.integer(20), 
				check        =as.logical(TRUE),   
				pdev         =as.numeric(1.0),   
				season       =as.numeric(0.0),         
				optionF     =as.logical(TRUE),  
				qEst        =as.logical(FALSE),
				qScale      =as.logical(TRUE),              
				qCv         =as.numeric(1.0),         
				qAdd        =as.logical(FALSE),     
				selPenalty  =as.logical(FALSE),
				selSigma    =as.numeric(1.0),         
				selNyr      =as.integer(NA),
				selMinage   =as.integer(NA),
				selMaxage   =as.integer(NA),
				srPenalty   =as.logical(TRUE),
				srSigma     =as.numeric(1.0),         
				srNyr       =as.integer(NA),
				srPdf       =as.character("log"),
				recPenalty  =as.logical(TRUE),
				recSigma    =as.numeric(1.0),         
				recMinyr    =as.integer(NA),
				recMaxyr    =as.integer(NA),
				catchPenalty=as.logical(FALSE),         
				catchSigma  =as.numeric(1.0),         
				catchPdf    =as.character("log")
				),
		validity=validFLAdaptControl)

setValidity("FLAdaptControl", validFLAdaptControl)
remove(validFLAdaptControl)	# We do not need this function any more


## FLAdapt ######################
validFLAdapt <- function(object){
	# All FLQuant objects must have same dimensions
	Dim <- dim(object@stock.n)
	if (!all(dim(object@f) == Dim))
		return("n and f arrays must have same dimensions")
	# Everything is fine
	return(TRUE)}

setClass("FLAdapt",
	representation(
    "FLComp",
		call     ="character",
		control  ="FLAdaptControl",
		stock.n  ="FLQuant",
		harvest  ="FLQuant",
    diags    ="data.frame"),
	prototype=prototype(
		call     ="new(\"FLAdapt\")",
		control  =new("FLAdaptControl"),
		stock.n  =new("FLQuant"),
		harvest  =new("FLQuant")),
	validity=validFLAdapt)

setValidity("FLAdapt", validFLAdapt)
remove(validFLAdapt)	# We do not need this function any more

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
      do.call('FLAdaptControls',  c(list(object=object), args))
	  }
  }
)

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


