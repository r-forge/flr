# coerce - «Short one line description»
# coerce

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 26 Feb 2009 10:49
# $Id:  $

# as.FLSR {{{
setAs('FLBRP', 'FLSR',
  function(from)
	{

    FLSR(name=from@name, desc = "'rec' and 'ssb' slots obtained from a 'FLBRP' from",
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
    # FLStock()
    res <- FLStock(stock.wt=stock.wt(from), catch.wt=catch.wt(from),
      discards.wt=discards.wt(from), landings.wt=landings.wt(from),
      m=m(from), mat=mat(from), harvest=harvest(from),
      harvest.spwn=harvest.spwn(from), m.spwn=m.spwn(from),
      stock=stock(from), stock.n=stock.n(from),
      discards=discards(from), discards.n=discards.n(from),
      catch=catch(from), catch.n=catch.n(from),
      name=name(from), desc=paste("Created by coercion from 'FLStock'", desc(object)))

    # range
    range(res, c('minyear', 'maxyear')) <- unlist(dims(fbar(from))[c('minyear',
      'maxyear')])
    
    return(res)
  }
) # }}}
