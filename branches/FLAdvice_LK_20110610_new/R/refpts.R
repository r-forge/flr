# refpts - methods for the refpts class
# FLBRP/R/refpts.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cerviño, IEO
# $Id: refpts.R 888 2011-01-17 00:56:11Z lauriekell $

# constructors  {{{
# array
setMethod("refpts", signature(object="array"),
  function(object, refpt=c('f0.1', 'fmax', 'spr.30', 'msy', 'mey'),
    quantity=c('harvest', 'yield', 'rec', 'ssb', 'biomass', 'revenue', 'cost', 'profit'),
    iter=1, dimnames=list(refpt=refpt, quantity=quantity, iter=iter), ...) {

    # reshape object
    #  if 2D, object will get 3D with dim[3]=iter
    if(length(dim(object)) == 2)
      if(is.null(dimnames(object)))
        object <- array(c(object), dim=c(dim(object), iter))
      # keeping dimnames if present
      else
        object <- array(c(object), dim=c(dim(object), iter),
          dimnames=c(dimnames(object), list(iter=1:iter)))
    
    # dimnames given
    if(!missing(dimnames))
      dimnames(object) <- dimnames
    # or not
    else {
      if(!is.null(dimnames(object)))
        dmns <- dimnames(object)
      else
        dmns <- list(refpt=refpt[1:dim(object)[1]],
          quantity=quantity[1:dim(object)[2]],
          iter=1:dim(object)[3])
      if (!missing(refpt))
        dmns$refpt <- refpt
      if (!missing(quantity))
        dmns$quantity <- quantity
      if (!missing(iter))
        dmns$iter <- iter
      dimnames(object) <- dmns
    }

    return(new('refpts', object))
  }
)

# vector
setMethod("refpts", signature(object="vector"),
  function(object, refpt="missing", quantity="missing", iter="missing",
    dimnames="missing", ...) {
    
    # get dims from args
    if(!missing(dimnames))
      object <- array(object, dimnames=dimnames, dim=unlist(lapply(dimnames, length)))
    else {
      dmns <- dimnames(new("refpts"))
      if(!missing(refpt))
        dmns$refpt <- refpt
      if(!missing(quantity))
        dmns$quantity <- quantity
      if(!missing(iter))
        dmns$iter <- iter
      object <- array(object, dimnames=dmns, dim=unlist(lapply(dmns, length)))
    }

    return(refpts(object, ...))
  }
)

# missing
setMethod("refpts", signature(object="missing"),
  function(object, ...) {
    return(refpts(as.numeric(NA), ...))
  }
)

# logical
setMethod("refpts", signature(object="logical"),
  function(object, ...) {
    return(refpts(as.numeric(object), ...))
  }
)

# }}}

# show {{{
setMethod('show', signature(object='refpts'),
  function(object){
		cat("An object of class \"refpts\":\n")

       if(dim(object)[3] > 1){
   		  v1 <- apply(object, 1:2, median, na.rm=TRUE)
     		v2 <- apply(object, 1:2, mad,    na.rm=TRUE)
        v3 <- paste(format(v1,digits=5),"(", format(v2, digits=3), ")", sep="")}
      else
        v3 <- paste(format(apply(object, 1:2, median, na.rm=TRUE),digits=5))

    print(array(v3, dim=dim(object)[1:2], dimnames=dimnames(object)[1:2]), quote=FALSE)

		if(dim(object)[3] != 1)
			cat("iters: ", dim(object)[3],"\n\n")
  }
) # }}}

# propagate {{{
setMethod('propagate', signature(object='refpts'),
  function(object, iter, fill.iter=TRUE){
  
    res <- refpts(object, iter=iter)
    if(fill.iter== FALSE)
      res[,,2:iter] <- as.numeric(NA)
    return(res)
  }
) # }}}

# refpts<- {{{
setMethod('refpts<-', signature(object='FLBRP', value='refpts'),
  function(object, value){
    slot(object, 'refpts') <- value

    return(object)
  }
)
    
setMethod('refpts<-', signature(object='FLBRP', value='numeric'),
  function(object, ..., value){
    args <- list(...)

    # selection required
    if(length(args) > 0){
      # match and sort args names
      if(!is.null(names(args)))
        args <- args[match(names(dimnames(refpts(object))), names(args))]
      names(args) <- c('i', 'j', 'k')[seq(length(args))]
      args <- args[!unlist(lapply(args, is.null))]
      args <- lapply(args, as.character)

      refpts(object) <- do.call('[<-', c(list(x=refpts(object)), args, list(value=value)))
    }else{
      refpts(object)[] <- value}
      
    return(object)})
# }}}

setMethod('refpts', signature(object='FLBRP'),
  function(object, ...){
    args <- list(...)
    refpts <- slot(object, 'refpts')
    
    # selection required
    if(length(args) > 0){
      # match and sort args names
      if(!is.null(names(args)))
        args <- args[match(names(dimnames(refpts)), names(args))]
      names(args) <- c('i', 'j', 'k')[seq(length(args))]
      args <- args[!unlist(lapply(args, is.null))]
      args <- lapply(args, as.character)

      return(do.call('[', c(list(x=refpts), args)))
    }else
      return(refpts)})

setMethod("msy", signature(object="FLBRP"),
  function(object) {
    refpts(object) <- refpts(as.numeric(NA), refpt='msy',
      iter=as.numeric(dimnames(object@refpts)$iter))
    computeRefpts(object)})

f0.1 <- function(object){
  refpts(object) <- refpts(as.numeric(NA), refpt='f0.1', iter=as.numeric(dimnames(object@refpts)$iter))

  computeRefpts(object)}

fmax <- function(object){
  refpts(object) <- refpts(as.numeric(NA), refpt='fmax', iter=as.numeric(dimnames(object@refpts)$iter))
  computeRefpts(object)}

sprr <- function(object, spr='.30'){
  refpts(object) <- refpts(as.numeric(NA), refpt=paste('spr', sub('0.', '.', ac(spr)),sep=''), iter=as.numeric(dimnames(object@refpts)$iter))
  computeRefpts(object)}
