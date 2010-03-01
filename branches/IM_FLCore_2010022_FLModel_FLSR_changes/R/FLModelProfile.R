# FLModelProfile - Likelihood surfaces and profiles
# FLCore/R/FLModelProfile

# Copyright 2003-2010 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id:  $

# FLModelProfile {{{
validFLModelProfile <-  function(object) {
  # check iters in params equal length of logLik
  if(length(dimnames(params(object))$iter) != length(logLik(object)))
    return("number of elements in logLik and iters in params must match")
  return(TRUE)
}
setClass("FLModelProfile", representation("FLModel"), prototype(FLModel()),
  validity=validFLModelProfile) # }}}

# FLModelSurface {{{
validFLModelSurface <-  function(object) {
  # check iters in params equal length of logLik
  if(length(dimnames(params(object))$iter) != length(logLik(object)))
    return("number of elements in logLik and iters in params must match")
  return(TRUE)
}
setClass("FLModelSurface", representation("FLModel"), prototype(FLModel()),
  validity=validFLModelSurface) # }}}

# plot(FLModelProfile)  {{{
setMethod("plot", signature(x="FLModelProfile", y="missing"),
  function(x, ...) {

    # create data.frame
    data <- as.data.frame(params(x))
    data <- cbind(data, logLik=c(logLik(x)))
    data <- aggregate(data$logLik, by=list(value=data$data, param=data$param), mean)

    # plot
    xyplot(x ~ value | param, data=data, type='l', scales=list(x=list(relation='free')),
        xlab="", ylab='logLik')
  }
) # }}}

# plot(FLModelSurface)  {{{
setMethod("plot", signature(x="FLModelSurface", y="missing"),
  function(x, xlab=dimnames(params(x))$params[1], ylab= dimnames(params(x))$params[1],
      ci=c(0.5, 0.75, 0.9, 0.95), ...) {

    # create data.frame
    data <- data.frame(t(as.matrix((params(x)@.Data))), logLik=c(logLik(x)))

    grid <- list(x = sort(unique(data[,1])), y = sort(unique(data[,2])),
        z= tapply(data[,"logLik"], list(data[,1],data[,2]),mean))

    # CIs
    cis <- mean(grid[['z']]) - qchisq(ci, 2)

    # plot
    do.call('image', c(grid, list(xlab=xlab, ylab=ylab), list(...)))
    do.call('contour', c(grid, list(levels=cis, col='grey', lwd=2, labels=ci, add=TRUE)))
  }
) # }}}

# surface {{{
setMethod("surface", signature(fitted="FLModel"),
  function(fitted, maxsteps=10, range=0.1, ...)
  {
    # vars
    foo <- logl(fitted)
    params <- params(fitted)
    parnames <- dimnames(params)$params
    
    # create grid of param values:
    exgrid <- list()
    for(i in parnames) {
      # steps for param[i]
      estim <- c(params[i,])
      steps <- seq(estim - (estim*range), estim + (estim*range), length=maxsteps)
      exgrid[[i]] <- steps
    }
    grid <- do.call(expand.grid, exgrid)

    # col for logLik
    grid$logLik <- as.numeric(NA)

    args <- list()

    # data
    data <- names(formals(foo))
    data <- data[data %in% slotNames(fitted)]
    for(i in data)
      args[i] <- list(slot(fitted, i))

    # calculate logLik for grid
    for(i in seq(nrow(grid))) {
      grid[i, 'logLik'] <- do.call(logl(fitted), c(args, as.list(grid[i,parnames])))
    }
        
    # FLPar
    params(fitted) <- propagate(params(fitted), nrow(grid))
    # TODO: Fix FLPar!!!!!
    params(fitted)[] <- new('FLPar', aperm(as.matrix(grid[, parnames]), c(2,1)))

    # logLik
    logLik(fitted) <- c(grid$logLik)
    
    return(new('FLModelSurface', fitted))
  }
) # }}}

# profile {{{
setMethod("profile", signature(fitted="FLModel"),
  function(fitted, maxsteps=10, range=0.1, fixed=missing, ...)
  {
    # vars
    foo <- logl(fitted)
    params <- params(fitted)
    parnames <- dimnames(params)$params

    # fixed params
    if(!missing(fixed)) {
      parnames <- parnames[!parnames %in% names(fixed)]
    }
    
    # create grid of param values:
    grid <- list()
    for(i in parnames) {
      # steps for param[i]
      estim <- c(params[i,])
      steps <- seq(estim - (estim*range), estim + (estim*range), length=maxsteps)
      grid[[i]] <- steps
    }

    # estimate
    logLik <- lapply(grid, function(x) rep(NA, length(x)))
    for(i in parnames) {
      for (j in seq(length(grid[[i]]))) {
        profiled <- list(fixed=grid[[i]][j])
        names(profiled) <- i
        logLik[[i]][j] <- c(logLik(fmle(fitted, fixed=profiled)))
      }
    }

    # FLPar
    params(fitted) <- propagate(params(fitted), maxsteps * length(grid), fill.iter=FALSE)
    
    newparams <- matrix(NA, nrow=length(parnames), ncol=length(parnames)*maxsteps)
    for(i in seq(length(grid))) {
      newparams[i,seq(1+((i-1)*maxsteps), maxsteps*(i-1)+maxsteps)] <- t(grid[[i]])
    }

    params(fitted)[] <- newparams
    
    # logLik
    logLik(fitted) <- c(unlist(logLik, use.names=FALSE))
    
    return(new('FLModelProfile', fitted))
  }
) # }}}
