#### Function to automatically scale parameters, to aid in fiting non-linear models

# getSlotNamesClass {{{
getSlotNamesClass <- function(object, class)
{
    slots <- names(getClass(class(object))@slots)
    contains <- as.list(rep(FALSE, length(slots)))
    names(contains) <- slots
    for(what in slots)
      if(is(slot(object, what), class))
        contains[[what]] <- TRUE
    return(names(contains[contains == TRUE]))
} # }}}

if (!isGeneric("auto_parscale"))
setGeneric("auto_parscale", function(obj, ...)
		standardGeneric("auto_parscale"))

setMethod("auto_parscale", signature(obj="FLModel"),
   aps <- function(obj)
   {

   if (dims(obj)$iter>1) stop("only works for a single iter")

   datanm <- getSlotNamesClass(obj, 'FLArray')
   datanm <- c(datanm, getSlotNamesClass(obj, 'numeric'))
   #   get those in formals of logl
   datanm <- datanm[datanm%in%names(formals(obj@logl))]

   # input data
   alldata <- list()
   for (i in datanm)
      alldata[[i]] <- slot(obj, i)

   data <- alldata

   npar <- dim(params(obj))[1]
   iv   <-do.call(obj@initial, args=data[names(formals(obj@initial))])

   #iv   <- initial(obj)(rec(obj),ssb(obj))
   tiny_number <- 1e-10
   dll  <- rep(NA,npar)

   # Make a list of the LogL arguments with the initial values
   ll_args_orig <- unlist(formals(logl(obj)))
   for (i in datanm)
     ll_args_orig[[i]] <- slot(obj,i)

   #ll_args_orig[["ssb"]] <- data[names(formals(obj@initial))]
   for (i in dimnames(params(obj))$params)
     ll_args_orig[[i]] <- iv[[i]]

   ll_orig         <- do.call(logl(obj),ll_args_orig)
   ll_bump1        <- rep(NA,npar)
   names(ll_bump1) <- dimnames(params(obj))$params
   ll_bump2        <- ll_bump1

   # cycle over each parameter, bump it and get the new LL
   for (i in dimnames(params(obj))$params){
      ll_args_bump1      <- ll_args_orig
      ll_args_bump1[[i]] <- ll_args_bump1[[i]] * (1+tiny_number)
      ll_bump1[i]        <-do.call(logl(obj),ll_args_bump1)

      ll_args_bump2      <- ll_args_orig
      ll_args_bump2[[i]] <- ll_args_bump2[[i]] * (1-tiny_number)
      ll_bump2[i]        <-do.call(logl(obj),ll_args_bump2)
      }
      
   dll <- (ll_bump1-ll_bump2) / (unlist(ll_args_orig)[dimnames(params(obj))$params] * (tiny_number*2))

   return(abs(1/dll))})


#grad(sin, pi)
#function(x)      ll_bump[i] <-do.call(logl(obj),ll_args_bump)}
