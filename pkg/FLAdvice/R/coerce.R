# coerce - cpoercion methods for FLBRP
# FLBRP/R/coerce.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cervi?o, IEO
# Last Change: Fri Jul 01, 2011 at 03:10 PM +0200
# $Id: coerce.R 995 2011-06-03 15:29:02Z lauriekell $

# as(FLSR) {{{
setAs('FLBRP', 'FLSR',
  function(from) {

	  rec.age  <-range(from,"min")
	  recYrCls <-as.numeric(dimnames(rec.obs(from))$year)-rec.age
    ssbYrCls <-as.numeric(dimnames(ssb.obs(from))$year)

    ssbYrCls<-ssbYrCls[ssbYrCls %in% recYrCls]
    recYrCls<-ssbYrCls+rec.age

    # calculate ssb and create FLSR from incorprating rec.age
    rec <- rec.obs(from)[,ac(recYrCls)]
    ssb <- ssb.obs(from)[,ac(ssbYrCls)]

   # create the FLSR from
   return(FLSR(name=from@name,
	  rec     =rec,
    ssb     =ssb,
    desc    = "'rec' and 'ssb' slots obtained from a 'FLBRP' from"))

  }
) # }}}

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


