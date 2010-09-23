# coerce - cpoercion methods for FLBRP
# FLBRP/R/coerce.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cerviño, IEO
# Last Change: 23 Sep 2010 16:38
# $Id$

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
) # }}}

# as.FLStock {{{
setAs('FLBRP', 'FLStock',
  function(from)
  {
    years <- dimnames(fbar(from))$year
    
    catch.wt <- expand(catch.wt(from), year=years)
    catch.wt[,] <- catch.wt[,1]
    
    landings.wt <- expand(landings.wt(from), year=years)
    landings.wt[,] <- landings.wt[,1]
    
    discards.wt <- expand(discards.wt(from), year=years)
    discards.wt[,] <- discards.wt[,1]
    
    stock.wt <- expand(stock.wt(from), year=years)
    stock.wt[,] <- stock.wt[,1]

    m <- expand(m(from), year=years)
    m[,] <- m[,1]

    mat <- expand(mat(from), year=years)
    mat[,] <- mat[,1]

    harvest.spwn <- expand(harvest.spwn(from), year=years)
    harvest.spwn[,] <- harvest.spwn[,1]

    m.spwn <- expand(m.spwn(from), year=years)
    m.spwn[,] <- m.spwn[,1]

    # FLStock()
    res <- FLStock(
      # TODO extend slots for years: check all slots present
      catch=catch(from), catch.n=catch.n(from),
      catch.wt=catch.wt,
      landings=landings(from), landings.n=landings.n(from),
      landings.wt=landings.wt,
      discards=discards(from), discards.n=discards.n(from),
      discards.wt=discards.wt,
      stock=stock(from), stock.n=stock.n(from),
      stock.wt=stock.wt,
      m=m, mat=mat,
      harvest=harvest(from),
      harvest.spwn=harvest.spwn, m.spwn=m.spwn,
      name=name(from), desc=paste("Created by coercion from 'FLBRP'", desc(from)))

    # range
    range(res, c('minyear', 'maxyear')) <- unlist(dims(fbar(from))[c('minyear',
      'maxyear')])
    
    if(validObject(res))
      return(res)
    else
      stop("invalid object created. Please check input object")
  }
) # }}}

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
