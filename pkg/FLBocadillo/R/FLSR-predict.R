setGeneric("predict", useAsDefault = predict)

getSlotNamesClass <- function(object, class){
    slots <- names(getClass(class(object))@slots)
    contains <- as.list(rep(FALSE, length(slots)))
    names(contains) <- slots
    for(what in slots)
      if(is(slot(object, what), class))
        contains[[what]] <- TRUE
    return(names(contains[contains == TRUE]))}

predictFLModel<-function(object, ...)
    {
    args <- list(...)
    if(length(args) > 0 && is.null(names(args)))
      stop('FLQuant or FLCohort inputs must be named to apply formula')
    # call
    call <- as.list(object@model)[[3]]

    # check vars in call match input in args
    if(length(args) > 0 & !any(names(args)%in%all.vars(call)))
      warning(paste("Input names do not match those in model formula: '",
        paste(names(args)[!names(args)%in%all.vars(call)], collapse=','), "'", sep=""))

    # create list of input data
    #   get FLQuant/FLCohort slots' names
    datanm <- getSlotNamesClass(object, 'FLArray')
    datanm <- c(datanm, getSlotNamesClass(object, 'numeric'))

    # add dimnames if used
    dimna <- dimnames(slot(object, datanm[1]))[names(slot(object, datanm[1]))%in%
      all.vars(object@model)]
    # get them in the right shape
    dimdat <- lapply(dimna, function(x)
      {
        out <- slot(object, datanm[1])
        out[] <- as.numeric(x)
        return(out)
      })

    # iterations
    #   from object
    iter <- max(unlist(qapply(object, function(x) dims(x)$iter)))
    #   from extra input
    if(length(args) > 0)
    {
      iterarg <- lapply(args, function(x) {
        itera <- try(dims(x)$iter)
        if(class(iter) =='try-error')
          return(1)
        else
          return(itera)
      })
      iterarg <- max(unlist(iterarg))
    }
    else
      iterarg <- 1
    #   decision
    if (iter == iterarg)
      iters <- iter
    else if(iter > iterarg && iterarg == 1)
      iters <- iter
    else if(iterarg > iter && iter == 1)
      iters <- iterarg
    else
      stop("Iter for object and input arguments do not match")

    for (it in 1:iters)
    {
     obj <- iter(object, it)

      #   input data
      data <- list()
      for (i in datanm)
        data[[i]] <- slot(obj, i)

      # add covar if defined and available
      if('covar' %in% slotNames(obj))
      {
        covarnm <- names(obj@covar)
        if(length(covarnm))
          data <- c(data, covar(obj)[covarnm])
      }

      # add newdata
      data[names(args)] <- lapply(args, iter, it)

      params <- as.vector(obj@params@.Data)
      names(params) <- dimnames(obj@params)[['params']]
      # check inputs
      if(it == 1)
      {
        res <- propagate(do.call(class(object@fitted), list(eval(call,
          envir=c(params, data, dimdat)))), iters, fill.iter=FALSE)
      }
      else
      {
        iter(res, it) <- do.call(class(object@fitted), list(eval(call,
          envir=c(params, data, dimdat))))
      }
    }
    return(res)
  }

setMethod('predict', signature(object='FLModel'),
  function(object, ...){

   lowess.<-function(object,f=2/3,iter=3,delta=0.01*diff(range(ssb(object)))){
       res <- lowess(rec(object)~ssb(object),f=f,delta=delta,iter=iter)
       return(FLQuants(rec=FLQuant(res$y, dimnames=dimnames(rec(object)[1,order(ssb(object)[1,,drop=T])])),
                       ssb=FLQuant(res$x, dimnames=dimnames(ssb(object)[1,order(ssb(object)[1,,drop=T])]))))}

  if (length(model(object))==0 | grep("lowess",ac(model(lowess(nsher)))[3])>0) return(lowess.(object)$rec) else
                                return(predictFLModel(object,...))
  })
  
