# coerce - cpoercion methods for FLBRP
# FLBRP/R/coerce.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cervi?o, IEO
# Last Change: Fri Jul 01, 2011 at 11:24 AM +0200
# $Id: coerce.R 995 2011-06-03 15:29:02Z lauriekell $

# as.FLSR {{{
setAs('FLBRP', 'FLSR',
  function(from)
	{

    sr <- FLSR(name=from@name, desc = "created from a 'FLBRP' object",
      rec=rec.obs(from), ssb=ssb.obs(from))
    model(sr) <- SRModelName(model(from))
    params(sr) <- params(from)
    
    if(validObject(sr))
      return(sr)
    else
      stop("invalid object created. Please check input object")
  }
)

setMethod("as.FLSR", signature(object="FLBRP"),
  function(object, ...){

	  rec.age  <-range(object,"min")
	  recYrCls <-as.numeric(dimnames(rec.obs(object))$year)-rec.age
    ssbYrCls <-as.numeric(dimnames(ssb.obs(object))$year)

    ssbYrCls<-ssbYrCls[ssbYrCls %in% recYrCls]
    recYrCls<-ssbYrCls+rec.age

    # calculate ssb and create FLSR object incorprating rec.age
    rec <- rec.obs(object)[,ac(recYrCls)]
    ssb <- ssb.obs(object)[,ac(ssbYrCls)]

   # create the FLSR object
   sr = FLSR(name    =object@name,
	           rec     =rec,
             ssb     =ssb,
             desc    ="'rec' and 'ssb' slots obtained from a 'FLBRP' object", ...)

    validObject(sr)
    return(sr)
    })

# }}}

# as.data.frame {{{
setMethod("as.data.frame", 
signature(x="FLBRP", row.names="ANY", optional="character"),
  function(x, row.names=NULL, optional)
  {
    if(any(c('model', 'params', 'refpts', 'name', 'desc', 'range') %in% optional))
      stop("Only 'FLQuant' slots can be converted")
    df <- function(x, slots, names=slots)
    {
      res <-FLQuants(mlply(slots, function(x,fl)
        do.call(x,list(fl)), fl=x))

      names(res)<-slots
      return(
          as.data.frame(res, row.names=row.names)
          )
    }

    return(df(x,optional))
  }
) # }}}


