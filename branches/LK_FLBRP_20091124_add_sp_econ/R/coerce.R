# coerce - cpoercion methods for FLBRP
# FLBRP/R/coerce.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cerviño, IEO
# Last Change: 12 Mar 2009 14:56
# $Id$

# as.FLSR {{{
setAs('FLBRP', 'FLSR',
  function(from)
	{

    FLSR(name=from@name, desc = "'rec' and 'ssb' slots obtained from a 'FLBRP'",
      rec=rec.obs(from), ssb=ssb.obs(from))
    
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
