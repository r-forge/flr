# constructors - constructor methods for FLBRP
# FLBRP/R/constructors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cerviño, IEO
# Last Change: 26 Feb 2009 16:14
# $Id:  $

# FLBRP {{{
setGeneric('FLBRP', function(object, sr, ...)
  standardGeneric('FLBRP'))

# FLBRP(object=FLStock, sr=FLSR)
setMethod('FLBRP', signature(object='FLStock', sr='FLSR'),
  function(object, sr, ...)
  {
    FLBRP(object=object, model=sr@model, params=sr@params, ...)
  }
)

# FLBRP(object=FLStock, sr=missing)
setMethod('FLBRP', signature(object='FLStock', sr='missing'),
  function(object, model=formula(rec~a), params=FLPar(1, params='a'),
    fbar=seq(0, 4, 0.04), nyrs=3, na.rm=TRUE, mean='arithmetic', ...)
  {
    # dims & dimnames
    dims <- dims(object)
    maxyear <- dims$maxyear
    years <- ac(seq(maxyear-nyrs+1, maxyear))
    fages <- ac(seq(object@range['minfbar'], object@range['maxfbar']))
    snames <- dimnames(object@catch)
    dnames <- dimnames(object@catch.n)
    dnames[['year']] <- '1'
    
    # mean
    if(mean == 'arithmetic')
      foo <- function(x) mean(x, na.rm=na.rm)
    else if (mean =='geometric')
      foo <- function(x) exp(mean(log(x), na.rm=na.rm))

    # scaling
    scaling  <- sweep(object@harvest[,years], 2:6, apply(object@harvest[fages,years] ,2:6,
      'mean', na.rm=na.rm), "/")
    scaling <- apply(scaling, c(1,3:6), foo)

    # NEW FLBRP
    res <- new('FLBRP', range=object@range,
      
      # fbar
      fbar=FLQuant(fbar, units=units(object@harvest)),

      # slots to be mean of nyrs (m, mat, harvest,spwn, m.spwn, discards.wt, landings.wt)
      m = apply(object@m[,years], c(1,3:6), foo),
      mat = apply(object@mat[,years], c(1,3:6), foo),
      stock.wt = apply(object@stock.wt[,years], c(1,3:6), foo),
      harvest.spwn = apply(object@harvest.spwn[,years], c(1,3:6), foo),
      m.spwn = apply(object@m.spwn[,years], c(1, 3:6), foo),
      discards.wt = apply(object@discards.wt[,years], c(1,3:6), foo),
      landings.wt = apply(object@landings.wt[,years], c(1,3:6), foo),

      # rec.obs
      rec.obs = object@stock.n[ac(dims$min)],

      # ssb.obs
      ssb.obs= ssb(object),

      # landings & discards
      landings.obs = object@landings,
      discards.obs = object@discards,

      # fbar.obs
      fbar.obs = fbar(object),

      # profit.obs
      profit.obs = FLQuant(dimnames=snames),

      # discards.sel & landings.sel
      discards.sel = scaling * apply(object@discards.n/(object@discards.n +
        object@landings.n), c(1,3:6), foo),
      landings.sel = scaling * apply(object@landings.n/(object@discards.n +
        object@landings.n), c(1,3:6), foo),

      # bycatch.wt & bycatch.harvest
      bycatch.wt = FLQuant(0, dimnames=dnames),
      bycatch.harvest = FLQuant(0, dimnames=dnames, units=units(object@harvest)),

      # availability
      availability = FLQuant(1, dimnames=dnames),

      # price
      price = FLQuant(as.numeric(NA), dimnames=dnames),

      # model & params
      model = model,
      params = params
    )

    # extra args
    args <- list(...)
    for (i in names(args))
      slot(res, i) <- args[[i]]

    return(res)
  }
)

# FLBRP(object=missing, sr=ANY)
# }}}
