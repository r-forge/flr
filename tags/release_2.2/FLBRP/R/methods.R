# methods - methods for FLBRP
# FLBRP/R/methods.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cerviño, IEO
# $Id$

# landings.n  {{{
setMethod('landings.n', signature(object='FLBRP'),
  function(object)
  {
    .Call('landings_n', object, SRchar2code(SRModelName(object@model)),
              FLQuant(c(params(object)),dimnames=dimnames(params(object))))
  }
) # }}}

# discards.n  {{{
setMethod('discards.n', signature(object='FLBRP'),
  function(object)
  {
   .Call('discards_n', object, SRchar2code(SRModelName(object@model)),
              FLQuant(c(params(object)),dimnames=dimnames(params(object))))
  }
) # }}}

# stock.n  {{{
setMethod('stock.n', signature(object='FLBRP'),
  function(object)
  {
    .Call('stock_n', object, SRchar2code(SRModelName(object@model)),
              FLQuant(c(params(object)),dimnames=dimnames(params(object))))
  }
) # }}}

# catch.n {{{
setMethod('catch.n', signature(object='FLBRP'),
  function(object) {
    res <- landings.n(object) + discards.n(object)
    if (units(discards.n(object)) == units(landings.n(object)))
		  units(res) <- units(discards.n(object))
    else
      warning("units of discards.n and landings.n do not match")
    return(res)
  }
) # }}}

# catch.wt  {{{
setMethod('catch.wt', signature(object='FLBRP'),
  function(object) {
#      idx1 <- landings.sel(object) == 0
#      idx2 <- discards.sel(object) == 0

#      if(dim(idx1)[6] > dim(idx2)[6]) {
#        idx <- array(idx2, dim=dim(idx1)) == TRUE & idx1 == TRUE
#        landings.sel(object)[idx] <- 1
#        discards.sel(object)[apply(idx, 1:5, function(x) as.logical(sum(x)))] <- 1
#      }

#      if(dim(idx2)[6] > dim(idx1)[6]) {
#        idx <- array(idx1, dim=dim(idx2)) == TRUE & idx2 == TRUE
#        discards.sel(object)[idx] <- 1
#        landings.sel(object)[apply(idx, 1:5, function(x) as.logical(sum(x)))] <- 1
#      }

      denom<-landings.sel(object) + discards.sel(object)
      denom[denom==0]<-1
      
      res <- (landings.wt(object) * landings.sel(object) +
              discards.wt(object) * discards.sel(object)) / denom

#(landings.sel(object) + discards.sel(object))

    if (units(discards.wt(object)) == units(landings.wt(object)))
				units(res) <- units(discards.wt(object))
    return(res)
  }
)  # }}}

# catch.sel {{{
setMethod('catch.sel', signature(object='FLBRP'),
  function(object) {
    return(landings.sel(object) + discards.sel(object))
  }
) # }}}

# catch.obs {{{
setGeneric('catch.obs', function(object, ...)
		standardGeneric('catch.obs'))
setMethod('catch.obs', signature(object='FLBRP'),
  function(object) {
    return(discards.obs(object)+landings.obs(object))
    }
) # }}}


# yield.obs {{{
setGeneric('yield.obs', function(object, ...)
		standardGeneric('yield.obs'))
setMethod('yield.obs', signature(object='FLBRP'),
  function(object) {
    return(landings.obs(object))
    }
) # }}}

# computeFbar  {{{
setGeneric('computeFbar', function(object, ...)
		standardGeneric('computeFbar'))
setMethod('computeFbar', signature(object='FLBRP'),
  function(object)
  {
    return(apply(harvest(object)[ac(object@range["minfbar"]:object@range["maxfbar"])],c(2:6),mean))
  }
) # }}}

# rec {{{
setGeneric('rec', function(object, ...)
		standardGeneric('rec'))
setMethod('rec', signature(object='FLBRP'),
  function(object) {
    return(stock.n(object)[1,])
    }
) # }}}

# rec.hat {{{
setGeneric('rec.hat', function(object, ...)
   standardGeneric('rec.hat'))
setMethod('rec.hat', signature(object='FLBRP'),
   function(object)
   {
    return(stock.n(object)[1,]) 
   }
) # }}}

## harvest {{{
setMethod("harvest", signature(object="FLBRP", catch="missing"),
	function(object)
  {
    # selectivity
    sel <- expand(landings.sel(object) + discards.sel(object),
      year=dimnames(fbar(object))$year)
    sel[,] <- sel[,1]
    sel <- sweep(sel, 2:6, fbar(object), '*')
    units(sel) <- 'f'
    return(sel)
  }
) # }}}

# spr {{{
setGeneric('spr', function(object, ...)
		standardGeneric('spr'))
setMethod('spr', signature(object='FLBRP'),
  function(object)
  {
    params(object)<-FLPar(1)
    model( object)<-formula(rec~a)
    
    res <- .Call("spr", object, SRchar2code(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))), 
      PACKAGE = "FLBRP")

    return(res)
  }
) # }}}

# ypr   {{{
setGeneric('ypr', function(object, ...)
		standardGeneric('ypr'))
setMethod('ypr', signature(object='FLBRP'),
  function(object)
  {
    params(object)<-FLPar(1)
    model( object)<-formula(rec~a)
    
    res<-.Call("ypr", object, SRchar2code(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))),
      PACKAGE = "FLBRP")

    return(res)
  }
) # }}}

# computeRefpts {{{
setGeneric('computeRefpts', function(object, ...)
		standardGeneric('computeRefpts'))
setMethod('computeRefpts', signature(object='FLBRP'),
 function(object)
  {
    # check dims in object and params
    iter <- c(dims(object)$iter, length(dimnames(params(object))$iter))
    
    # if > 1, they should be equal
    if(all(iter > 1))
      if(iter[1] != iter[2])
        stop('iter in FLQuant slots and params do not match, ',
          paste(iter, collapse=' vs. '))

    # extend refpts as needed
    iter <- max(iter)
    if(iter > 1){
      refpts <- propagate(refpts(object), iter, fill.iter=F)
      }
    else{
      refpts <- refpts(object)}
    

    if ("virgin" %in% dimnames(refpts)$refpt){
       refpts["virgin",,]<-NA
       refpts["virgin","harvest",]<-0
       }
    
    res <- .Call("computeRefpts", object, refpts,
      SRchar2code(SRModelName(object@model)),
                  FLQuant(c(params(object)), dimnames=dimnames(params(object))), PACKAGE = "FLBRP")

    return(res)
  }
) # }}}

# brp  {{{
setGeneric('brp', function(object, ...)
		standardGeneric('brp'))
setMethod('brp', signature(object='FLBRP'),
  function(object)
  {
    # check dims in object and params
    iter <- c(dims(object)$iter, length(dimnames(params(object))$iter))
    # if > 1, they should be equal
    if(all(iter > 1))
      if(iter[1] != iter[2])
        stop('iter in FLQuant slots and params do not match, ',
          paste(iter, collapse=' vs. '))

    # extend refpts as needed
    iter <- max(iter)
    if(iter > 1){
      refpts <- propagate(refpts(object), iter)}
    else{
      refpts <- refpts(object)}

   if ("virgin" %in% dimnames(refpts)$refpt){
       refpts@.Data["virgin",,         ]<-as.numeric(NA)
       refpts@.Data["virgin","harvest",]<-0}

    res <- .Call("brp", object, refpts, SRchar2code(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))),
      PACKAGE = "FLBRP")

    return(res)
  }
) # }}}

# hcrYield  {{{
setGeneric('hcrYield', function(object, fbar, ...)
		standardGeneric('hcrYield')
)
setMethod('hcrYield', signature(object='FLBRP', fbar='FLQuant'),
  function(object, fbar)
  {
    # check input fbar dims
    if(!identical(dim(fbar), dim(fbar(object))))
      stop("input fbar must be the same length as fbar(object)")

    if(dims(object)$iter!=1 && dims(object@params)$iter ==1)
       m(object)<-propagate(m(object),iter=dims(params(object))$iter)
    else if (dims(object)$iter!=1 && dims(object@params)$iter !=1)
       if (dims(object)$iter!= dims(object@params)$iter)
          stop("Iters in params don't match")

    res <- .Call("hcrYield", object, SRchar2code(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))),
      fbar, PACKAGE = "FLBRP")
    
    # propagate landings.wt
    if(dims(res)$iter != dims(landings.wt(object))$iter)
      landings.wt(object) <- propagate(landings.wt(object), dims(res)$iter)

    return(apply(sweep(res, c(1,3:6), landings.wt(object), "*"), 2, sum))
   }
)
setMethod('hcrYield', signature(object='FLBRP', fbar='numeric'),
  function(object, fbar)
  {
    hcrYield(object, FLQuant(fbar))
  }
) # }}}

# spr0 {{{
setMethod('spr0', signature(ssb='FLBRP', rec='missing', fbar='missing'),
  function(ssb)
  {
    params(ssb)<-FLPar(1)
    model(ssb)<-formula(rec~a)
    fbar(ssb) <- FLQuant(0)
    
    res <- .Call("spr", ssb, SRchar2code(SRModelName(ssb@model)),
      FLQuant(c(params(ssb)),dimnames=dimnames(params(ssb))),
      PACKAGE = "FLBRP")

    return(res)
  }
) # }}}

# propagate {{{
setMethod('propagate', signature(object='FLBRP'),
  function(object, iter, fill.iter=TRUE, obs=FALSE, params=FALSE)
  {
    # obs FLQuants
    if(obs)
    {
      obs <- c('fbar.obs', 'landings.obs', 'discards.obs', 'rec.obs', 'profit.obs')
      for(i in obs)
        slot(object, i) <- propagate(slot(object, i), iter=iter, fill.iter=fill.iter)
    }

    # other FLQuants
    quants <- c("fbar", "landings.sel", "discards.sel", "bycatch.harvest", "stock.wt",
      "landings.wt", "discards.wt", "bycatch.wt", "m", "mat", "harvest.spwn", "m.spwn", 
      "availability", "price", "vcost", "fcost")
    for(i in quants)
        slot(object, i) <- propagate(slot(object, i), iter=iter, fill.iter=fill.iter)

    # refpts
    refpts(object) <- propagate(refpts(object), iter=iter, fill.iter=fill.iter)

    # params
    if(params)
      params(object) <- propagate(params(object), iter=iter, fill.iter=fill.iter)
  
    return(object)
  }
) # }}}

# iter {{{
setMethod('iter', signature(object='FLBRP'),
  function(object, iter, ...)
  {
    object <- callNextMethod(object, iter, ...)
    params(object) <- iter(params(object), iter)
    if(dim(refpts(object))[3] > 1)
      refpts(object) <- refpts(object)[,,iter]
    return(object)
  }
) # }}}

# catch {{{
setMethod('catch', signature(object='FLBRP'),
  function(object) {
    res <- landings(object) + discards(object)
    if (units(discards(object)) == units(landings(object)))
		  units(res) <- units(discards(object))
    else
      warning("units of discards and landings do not match")
    return(res)
  }
) # }}}


# catch.hat {{{
setGeneric('catch.hat', function(object, ...)
		standardGeneric('catch.hat'))

setMethod('catch.hat', signature(object='FLBRP'),
  function(object) {
    return(catch(object))
    }
) # }}}

# yield {{{
setGeneric('yield', function(object, ...)
		standardGeneric('yield'))
setMethod('yield', signature(object='FLBRP'),
  function(object) {
    return(landings(object))
    }
) # }}}

# yield.hat {{{
setGeneric('yield.hat', function(object, ...)
		standardGeneric('yield.hat'))
setMethod('yield.hat', signature(object='FLBRP'),
  function(object) { return(landings(object))
    }
) # }}}

# discards  {{{
setMethod('discards', signature(object='FLBRP'),
  function(object) {
    return(apply(sweep(discards.n(object),c(1,3:6),discards.wt(object),"*"),2,sum))
    }
) # }}}

# discards.hat  {{{
setGeneric('discards.hat', function(object, ...)
		standardGeneric('discards.hat'))
setMethod('discards.hat', signature(object='FLBRP'),
  function(object) return(discards(object))
) # }}}

# landings  {{{
setMethod('landings', signature(object='FLBRP'),
  function(object)
  {
    return(apply(sweep(landings.n(object),c(1,3:6),landings.wt(object),"*"),2,sum))
  }
) # }}}

# landings.hat  {{{
setGeneric('landings.hat', function(object, ...)
		standardGeneric('landings.hat'))
setMethod('landings.hat', signature(object='FLBRP'),
  function(object) return(landings(object))
) # }}}

# stock {{{
setMethod('stock', signature(object='FLBRP'),
  function(object)
  {
    return(apply(sweep(stock.n(object),c(1,3:6),stock.wt(object),"*"),2,sum))
  }
) # }}}

# stock.hat {{{
setGeneric('stock.hat', function(object, ...)
		standardGeneric('stock.hat'))
setMethod('stock.hat', signature(object='FLBRP'),
  function(object) return(stock(object))
) # }}}

# ssb {{{
setMethod('ssb', signature(object='FLBRP'),
  function(object)
     {
     f    <-sweep(harvest(object), c(1,3:6), harvest.spwn(object), "*")
     M    <-sweep(      m(object), c(1,3:6),       m.spwn(object), "*")
     expZ <-exp(-sweep(f, c(1,3:6), M, "+"))

     return(apply(sweep(stock.n(object) * expZ, c(1,3:6), stock.wt(object)*mat(object),"*"),2,sum))
     }
) # }}}

# ssb.hat {{{
setGeneric('ssb.hat', function(object, ...)
		standardGeneric('ssb.hat'))
setMethod('ssb.hat', signature(object='FLBRP'),
  function(object) return(ssb(object))
) # }}}

# revenue {{{
setMethod('revenue', signature(object='FLBRP'),
  function(object) {
    return(apply(sweep(landings.n(object),c(1,3:6),price(object)*landings.wt(object),"*"),2,sum))
    }
) # }}}

# cost {{{
setGeneric('cost', function(object, ...)
		standardGeneric('cost'))
setMethod('cost', signature(object='FLBRP'),
  function(object)
    {
    res<-apply(sweep(sweep(fbar(object),3:6,vcost(object),"*"),3:6,fcost(object),"+"),2,sum)
    return(res)
    }
) # }}}


# profit  {{{
setGeneric('profit', function(object, ...)
		standardGeneric('profit'))
setMethod('profit', signature(object='FLBRP'),
  function(object) {
    return(revenue(object)-cost(object))
  }
) # }}}

# profit.hat  {{{
setGeneric('profit.hat', function(object, ...)
		standardGeneric('profit.hat'))
setMethod('profit.hat', signature(object='FLBRP'),
  function(object) return(profit(object))
) # }}}


