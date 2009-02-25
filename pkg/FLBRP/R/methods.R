# methods - «Short one line description»
# FLBRP/R/methods.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 26 Feb 2009 00:09
# $Id:  $

#landings.n
setMethod('landings.n', signature(object='FLBRP'),
  function(object)
  {
    .Call('landings_n', object, SRchar2code(SRModelName(object@model)))
  }
)

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

# catch.wt
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
  })

# catch.sel
setMethod('catch.sel', signature(object='FLBRP'),
  function(object) {
    return(landings.sel(object) + discards.sel(object))
  })

# catch.hat
setGeneric('catch.hat', function(object, ...)
		standardGeneric('catch.hat'))
setMethod('catch.hat', signature(object='FLBRP'),
  function(object) {
    return(discards.hat(object)+landings.hat(object))
    })

# catch.obs
setGeneric('catch.obs', function(object, ...)
		standardGeneric('catch.obs'))
setMethod('catch.obs', signature(object='FLBRP'),
  function(object) {
    return(discards.obs(object)+landings.obs(object))
    })

# yield
setGeneric('yield', function(object, ...)
		standardGeneric('yield'))
setMethod('yield', signature(object='FLBRP'),
  function(object) {
    return(landings(object))
    })

# yield.hat
setGeneric('yield.hat', function(object, ...)
		standardGeneric('yield.hat'))
setMethod('yield.hat', signature(object='FLBRP'),
  function(object) { return(landings(object))
    })

# yield.obs
setGeneric('yield.obs', function(object, ...)
		standardGeneric('yield.obs'))
setMethod('yield.obs', signature(object='FLBRP'),
  function(object) {
    return(landings.obs(object))
    })

# discards
setMethod('discards', signature(object='FLBRP'),
  function(object) {
    return(apply(sweep(discards.n(object),c(1,3:6),discards.wt(object),"*"),2,sum))
    })

setGeneric('discards.hat', function(object, ...)
		standardGeneric('discards.hat'))
setMethod('discards.hat', signature(object='FLBRP'),
  function(object) return(discards(object)))

# landings
setMethod('landings', signature(object='FLBRP'),
  function(object)
  {
    return(apply(sweep(landings.n(object),c(1,3:6),landings.wt(object),"*"),2,sum))
  }
)

setGeneric('landings.hat', function(object, ...)
		standardGeneric('landings.hat'))
setMethod('landings.hat', signature(object='FLBRP'),
  function(object) return(landings(object)))

# catch
setMethod('catch', signature(object='FLBRP'),
  function(object) {
    return(apply(sweep(catch.n(object),c(1,3:6),catch.wt(object),"*"),2,sum))
    })

# stock
setMethod('stock', signature(object='FLBRP'),
  function(object) {
    return(apply(sweep(stock.n(object),c(1,3:6),stock.wt(object),"*"),2,sum))
    })
setGeneric('stock.hat', function(object, ...)
		standardGeneric('stock.hat'))
setMethod('stock.hat', signature(object='FLBRP'),
  function(object) return(stock(object)))

# ssb
setMethod('ssb', signature(object='FLBRP'),
  function(object) {
     expZ <- exp(-sweep(sweep(harvest(object),c(1,3:6),harvest.spwn(object),"*"),
                        c(1,3:6), m(object)*m.spwn(object),"+"))
                 
     return(apply(sweep(stock.n(object)*expZ,c(1,3:6),stock.wt(object)*mat(object),"*"),2:6,sum))
     })

setGeneric('ssb.hat', function(object, ...)
		standardGeneric('ssb.hat'))
setMethod('ssb.hat', signature(object='FLBRP'),
  function(object) return(ssb(object)))

# fbar
setGeneric('computeFbar', function(object, ...)
		standardGeneric('computeFbar'))
setMethod('computeFbar', signature(object='FLBRP'),
  function(object) {
    return(apply(harvest(object)[ac(object@range["minfbar"]:object@range["maxfbar"])],c(2:6),mean))
    })

# revenue
setMethod('revenue', signature(object='FLBRP'),
  function(object) {
    return(apply(sweep(landings.n(object),c(1,3:6),price(object)*landings.wt(object),"*"),2:6,sum))
    })

# costs
setGeneric('costs', function(object, ...)
		standardGeneric('costs'))
setMethod('costs', signature(object='FLBRP'),
  function(object) {
    res<-sweep(sweep(fbar(object),3:6,vcost(object),"*"),3:6,fcost(object),"+")
    return(res)
    })

# recruit
setGeneric('rec', function(object, ...)
		standardGeneric('rec'))
setMethod('rec', signature(object='FLBRP'),
  function(object) {
    return(stock.n(object)[1,])
    })

# rec.hat
setGeneric('rec.hat', function(object, ...)
   standardGeneric('rec.hat'))
setMethod('rec.hat', signature(object='FLBRP'),
   function(object) {
        return(stock.n(object)[1,])
        })

# r.hat
setGeneric('r.hat', function(object, ...)
   standardGeneric('r.hat'))
setMethod('r.hat', signature(object='FLBRP'),
   function(object) {
        return(stock.n(object)[1,])
        })

# r.obs
setGeneric('r.obs', function(object, ...)
   standardGeneric('r.obs'))
setMethod('r.obs', signature(object='FLBRP'),
   function(object) {
        return(rec.obs(object))
        })
        
# profit
setGeneric('profit', function(object, ...)
		standardGeneric('profit'))
setMethod('profit', signature(object='FLBRP'),
  function(object) {
    return(apply(revenue(object),2,sum)-costs(object))
    })

## harvest
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
)

setGeneric('profit.hat', function(object, ...)
		standardGeneric('profit.hat'))
setMethod('profit.hat', signature(object='FLBRP'),
  function(object) return(profit(object)))

setGeneric('equilibrium', function(object, ...)
		standardGeneric('equilibrium')
)
setMethod('equilibrium', signature(object='FLBRP'),
equilibrium.<-  function(object)
    {
    ## check iters in FLquants and sr.params
    #if (dims(object)$iter==1 && dims(object@sr.params)$iter ==1) OK
    #if (dims(object)$iter==1 && dims(object@sr.params)$iter !=1) OK
    if      (dims(object)$iter!=1 && dims(object@params)$iter ==1)
       m(object)<-propagate(m(object),iter=dims(params(object))$iter)
    else if (dims(object)$iter!=1 && dims(object@params)$iter !=1)
       if (dims(object)$iter!= dims(object@params)$iter)
          stop("Iters in params don't match")
          
    srCode<-setSRCode(object)

    res<-.Call("equilibrium", object, srCode, PACKAGE = "FLBRP")

    units(harvest(res))<-"f"
    
    for (i in c(flq.hat,"landings.sel","discards.sel"))
       slot(object,i)<-slot(res,i)
    
    return(object)
    }
)

setGeneric('spr', function(object, ...)
		standardGeneric('spr'))
setMethod('spr', signature(object='FLBRP'),
  function(object){
    return(ssb(object)/rec(object))

    sr.params(object)<-FLPar(1)
    sr.model( object)<-formula(rec~a)
    
    srCode<-setSRCode(object)

    res<-.Call("spr", object, srCode, PACKAGE = "FLBRP")

    return(res)})

setGeneric('ypr', function(object, ...)
		standardGeneric('ypr'))
setMethod('ypr', signature(object='FLBRP'),
  function(object){
    return(yield(object)/rec(object))
    
    sr.params(object)<-FLPar(1)
    sr.model( object)<-formula(rec~a)
    
    srCode<-setSRCode(object)

    res<-.Call("ypr", object, srCode, PACKAGE = "FLBRP")

    return(res)})

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

# as.FLSR {{{
setMethod("as.FLSR", signature(object="FLBRP"),
    function(object, ...)
	{
        validObject(object)

        rec <- rec.obs(object)
        ssb <- ssb.obs(object)

        # create the FLSR object

        sr <- new("FLSR", rec = rec,ssb = ssb, name = object@name,
            desc = "'rec' and 'ssb' slots obtained from a 'FLBRP' object")

        slot(sr, "fitted")    <- FLQuant(dimnames = dimnames(slot(sr, "rec")))
        slot(sr, "residuals") <- FLQuant(dimnames = dimnames(slot(sr, "rec")))

        units(slot(sr, "fitted")) <- units(slot(sr, "rec"))

        validObject(sr)
        return(sr)
   }
) # }}}

# as.FLStock {{{
setMethod("as.FLStock", signature(object="FLBRP"),
    function(object, ...){
    validObject(object)

    dms     <-dimnames(m(oidBrp))
    dms$year<-dimnames(fbar(object))$year
    res     <-FLStock(FLQuant(NA,dimnames=dms))

    stock.wt(   res)[]<-stock.wt(   object)
    catch.wt(   res)[]<-catch.wt(   object)
    discards.wt(res)[]<-discards.wt(object)
    landings.wt(res)[]<-landings.wt(object)

    m(            res)[]<-m(           object)
    mat(          res)[]<-mat(         object)
    harvest(      res)[]<-harvest(     object)
    units(harvest(res))<-units(harvest(object))
    harvest.spwn (res)[]<-harvest.spwn(object)
    m.spwn(       res)[]<-m.spwn(      object)

    stock(     res)[]<-stock(     object)
    stock.n(   res)[]<-stock.n(   object)
    discards(  res)[]<-discards(  object)
    discards.n(res)[]<-discards.n(object)
    catch(     res)[]<-catch(     object)
    catch.n(   res)[]<-catch.n(   object)

    range(res,c("min","max","plusgroup","minfbar","maxfbar"))<-range(res,c("min","max","plusgroup","minfbar","maxfbar"))
    range(res,c("minyear","maxyear"))<-c(dims(fbar(object))$minyear,dims(fbar(object))$maxyear)

    name(res)<-name(object)
    desc(res)<-paste("Created by coercion from 'FLStock'", desc(object))

    return(res)
    })  # }}}
