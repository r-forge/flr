# setplusgroup - «Short one line description»
# setplusgroup

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 25 Feb 2009 13:03
# $Id:  $

# Reference:
# Notes:

# TODO Wed 25 Feb 2009 01:03:43 PM CET IM:

setGeneric('expandAge', function(object, ...)
		standardGeneric('expandAge'))

setMethod('expandAge', signature(object='FLBRP'),
  function(object,maxage)
    {
    newMaxage<-maxage
    dmns     <-dimnames(m(object))
    oldMaxage<-dims(object)$max
    dmns$age <-as.numeric(dmns$age[1]):newMaxage

    slts<-c("landings.sel",
            "discards.sel",
            "bycatch.harvest",
            "stock.wt",
            "landings.wt",
            "discards.wt",
            "bycatch.wt",
            "m",
            "mat",
            "harvest.spwn",
            "m.spwn",
            "availability",
            "price")
            
    for (i in slts) {
       slot(object,i)<-FLQuant(slot(object,i),dimnames=dmns)
       slot(object,i)[ac(oldMaxage:newMaxage)]<-slot(object,i)[ac(oldMaxage)]
       }

   dmns     <-dimnames(harvest(object))
    oldMaxage<-dims(object)$max
    dmns$age <-as.numeric(dmns$age[1]):newMaxage

    slts<-c("stock.n",
            "landings.n",
            "discards.n",
            "harvest")

    for (i in slts) {
       slot(object,i)<-FLQuant(slot(object,i),dimnames=dmns)
       }

    return(object)
    })
