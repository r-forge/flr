# methods - «Short one line description»
# FLBRP/R/methods.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 26 Feb 2009 09:38
# $Id:  $

# landings.n  {{{
setMethod('landings.n', signature(object='FLBRP'),
  function(object)
  {
    .Call('landings_n', object, SRchar2code(SRModelName(object@model)))
  }
) # }}}

# discards.n  {{{
setMethod('discards.n', signature(object='FLBRP'),
  function(object)
  {
    .Call('discards_n', object, SRchar2code(SRModelName(object@model)))
  }
) # }}}

# stock.n  {{{
setMethod('stock.n', signature(object='FLBRP'),
  function(object)
  {
    .Call('stock_n', object, SRchar2code(SRModelName(object@model)))
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
      idx <- landings.sel(object) == 0 & discards.sel(object) == 0
      landings.sel(object)[idx] <- 1
      discards.sel(object)[idx] <- 1
      res <- (landings.wt(object) * landings.sel(object) +
              discards.wt(object) * discards.sel(object)) /
             (landings.sel(object) + discards.sel(object))

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

# catch.hat {{{
setGeneric('catch.hat', function(object, ...)
		standardGeneric('catch.hat'))
setMethod('catch.hat', signature(object='FLBRP'),
  function(object) {
    return(discards.hat(object)+landings.hat(object))
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

# yield.obs {{{
setGeneric('yield.obs', function(object, ...)
		standardGeneric('yield.obs'))
setMethod('yield.obs', signature(object='FLBRP'),
  function(object) {
    return(landings.obs(object))
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
    expZ <- exp(-sweep(sweep(harvest(object), c(1,3:6), harvest.spwn(object), "*"),
      c(1,3:6), m(object) * m.spwn(object), "+"))
                 
     return(apply(sweep(stock.n(object) * expZ, c(1,3:6), stock.wt(object) *
      mat(object),"*"),2:6,sum))
   }
) # }}}

# ssb.hat {{{
setGeneric('ssb.hat', function(object, ...)
		standardGeneric('ssb.hat'))
setMethod('ssb.hat', signature(object='FLBRP'),
  function(object) return(ssb(object))
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

# revenue {{{
setMethod('revenue', signature(object='FLBRP'),
  function(object) {
    return(apply(sweep(landings.n(object),c(1,3:6),price(object)*landings.wt(object),"*"),2:6,sum))
    }
) # }}}

# cost {{{
setGeneric('cost', function(object, ...)
		standardGeneric('cost'))
setMethod('cost', signature(object='FLBRP'),
  function(object) 
  {
    res<-sweep(sweep(fbar(object),3:6,vcost(object),"*"),3:6,fcost(object),"+")
    return(res)
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

# profit  {{{
setGeneric('profit', function(object, ...)
		standardGeneric('profit'))
setMethod('profit', signature(object='FLBRP'),
  function(object) {
    return(apply(revenue(object),2,sum)-cost(object))
  }
) # }}}

## harvest {{{
setMethod("harvest", signature(object="FLBRP", catch="missing"),
	function(object)
		return(slot(object, "harvest"))
)

## harvest<-
setMethod("harvest<-", signature(object="FLBRP", value="FLQuant"),
	function(object, value) {
		slot(object, "harvest") <- value
		return(object)
	}
)
setMethod("harvest<-", signature(object="FLBRP", value="numeric"),
	function(object, value) {
		slot(object, "harvest")[] <- value
		return(object)
	}
) # }}}

# profit.hat  {{{
setGeneric('profit.hat', function(object, ...)
		standardGeneric('profit.hat'))
setMethod('profit.hat', signature(object='FLBRP'),
  function(object) return(profit(object))
) # }}}

# spr {{{
setGeneric('spr', function(object, ...)
		standardGeneric('spr'))
setMethod('spr', signature(object='FLBRP'),
  function(object)
  {
    params(object)<-FLPar(1)
    model( object)<-formula(rec~a)
    
    res<-.Call("spr", object, SRchar2code(SRModelName(object@model)), PACKAGE = "FLBRP")

    return(res)
  }
) # }}}

# ypr   {{{
setGeneric('ypr', function(object, ...)
		standardGeneric('ypr'))
setMethod('ypr', signature(object='FLBRP'),
  function(object)
  {
    sr.params(object)<-FLPar(1)
    sr.model( object)<-formula(rec~a)
    
    res<-.Call("ypr", object, SRchar2code(SRModelName(object@model)), PACKAGE = "FLBRP")

    return(res)
  }
) # }}}

setGeneric('computeRefpts', function(object, ...)
		standardGeneric('computeRefpts'))
setMethod('computeRefpts', signature(object='FLBRP'),
 function(object)
    {
    ## check iters in FLquants and sr.params
    #if (dims(object)$iter==1 && dims(object@sr.params)$iter ==1) OK
    #if (dims(object)$iter==1 && dims(object@sr.params)$iter !=1) OK
    if (dims(object)$iter!=1 && dims(object@sr.params)$iter ==1)
       {
       if (dims(m(object))$iter==1) m(object)<-propagate(m(object),iter=dims(sr.params(object))$iter)
       }
    else if (dims(object)$iter!=1 && dims(object@sr.params)$iter !=1)
       if (dims(object)$iter!= dims(object@sr.params)$iter)
          stop("Iters in sr.params don't match")

    srCode<-setSRCode(object)
    
    res<-.Call("computeRefpts", object, refpts(object), srCode, PACKAGE = "FLBRP")

    return(res)
    return(FLPar(res))
    })

setGeneric('brp', function(object, ...)
		standardGeneric('brp'))
setMethod('brp', signature(object='FLBRP'),
  function(object)
    {
    ## check iters in FLquants and sr.params
    #if (dims(object)$iter==1 && dims(object@sr.params)$iter ==1) OK
    #if (dims(object)$iter==1 && dims(object@sr.params)$iter !=1) OK
    if (dims(object)$iter==1 && dims(object@params)$iter !=1)
       m(object)<-propagate(m(object),iter=dims(params(object))$iter)
    else if (dims(object)$iter!=1 && dims(object@params)$iter !=1)
       if (dims(object)$iter!= dims(object@params)$iter)
          stop("Iters in params don't match")

    srCode<-setSRCode(object)

    res<-.Call("brp", object, refpts(object), srCode, PACKAGE = "FLBRP")

    units(harvest(res))<-"f"

    return(res)
    })

setGeneric('hcrYield', function(object,fbar,...)
		standardGeneric('hcrYield')
)
setMethod('hcrYield', signature(object='FLBRP'),
  function(object,fbar)
    {
    ## check iters in FLquants and sr.params
    #if (dims(object)$iter==1 && dims(object@sr.params)$iter ==1) OK
    #if (dims(object)$iter==1 && dims(object@sr.params)$iter !=1) OK
    if      (dims(object)$iter!=1 && dims(object@sr.params)$iter ==1)
       m(object)<-propagate(m(object),iter=dims(sr.params(object))$iter)
    else if (dims(object)$iter!=1 && dims(object@sr.params)$iter !=1)
       if (dims(object)$iter!= dims(object@sr.params)$iter)
          stop("Iters in sr.params don't match")

    if (!(inherits(fbar,"FLQuant")) && inherits(fbar,"numeric")) fbar<-as.FLQuant(fbar)
    if (!(inherits(fbar,"FLQuant"))) stop("fbar has to be of type FLQuant")

    srCode<-setSRCode(object)

    res<-.Call("hcrYield", object, srCode, fbar, PACKAGE = "FLBRP")

    #return(res)
    return(apply(sweep(res,c(1,3:6),landings.wt(object),"*"),2,sum))
   }
)

  setMethod('spr0', signature(ssb='FLBRP', rec='missing', fbar='missing'),
     function(ssb)
        {
        res<-equilibrium(FLBRP(ssb,fbar=0))

        return(sum(stock.n(res)[,1]*stock.wt(res)[,1]*mat(res)[,1]*exp(-m(res)*m.spwn(res)[,1])))
        })
