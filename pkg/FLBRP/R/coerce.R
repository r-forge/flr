# coerce - cpoercion methods for FLPellaT
# FLPellaT/R/coerce.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell
# Last Change: 12 Mar 2009 14:56


setMethod('FLPellaT', signature(object='FLIndex'),
  function(object,...)
    {
    args <-list(...)
    slots<-getSlots("FLPellaT")
    res  <-do.call(new, c(list(Class='FLPellaT'),args))

    range(res)<-range(ple4,c("minyear","maxyear"))
    name( res)<-name(object)
    desc( res)<-paste("created from FLIndex", desc(object))

    catch(res)<-apply(catch.n(object)*catch.wt(object),c(2,6),sum)
    if (type=="biomass")
       cpue( res)[[1]]<-apply(index(object),c(2,6),sum)

    # find slots not provided as argument
    empty<-!slots %in% names(args)


    return(res)
    })
