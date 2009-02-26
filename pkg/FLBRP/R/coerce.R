# coerce - «Short one line description»
# coerce

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 26 Feb 2009 09:38
# $Id:  $

# Reference:
# Notes:

# TODO Thu 26 Feb 2009 09:37:46 AM CET IM:

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
