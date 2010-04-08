# constructors - constructor methods for FLBRP
# FLBRP/R/constructors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cerviño, IEO
# $Id$

# FLBRP
setGeneric('FLBRP', function(object,sr, ...)
		standardGeneric('FLBRP'))

# FLBRP(object='missing', sr='missing') {{{
setMethod('FLBRP', signature(object='missing', sr='missing'),
  function(..., model=formula(rec~a), params=FLPar(1, params='a'),
    fbar=FLQuant(seq(0, 4, 0.04)))
  {
    args <- list(...)

    res <- do.call(new, c(list(Class='FLBRP', model=model, params=params, fbar=fbar),
      args))
    # resize: years
    slots <- c('fbar.obs', 'landings.obs', 'discards.obs', 'rec.obs', 'ssb.obs', 'stock.obs','profit.obs')
    # find slots not provided as argument
    empty <- !slots %in% names(args)
    # if any of them given, use for sizing
    if(any(empty == FALSE))
      for(i in slots[empty])
        slot(res, i) <- FLQuant(dimnames=dimnames(slot(res, slots[!empty][1])))

    # warn: slots to dissapear
    # c('stock.n', 'landings.n', 'discards.n', 'harvest')

    # resize: ages
    slots <- c('landings.sel', 'discards.sel', 'bycatch.harvest', 'stock.wt',
      'landings.wt', 'discards.wt', 'bycatch.wt', 'm', 'mat', 'harvest.spwn', 'm.spwn',
      'availability', 'price')
    # find slots not provided as argument
    empty <- !slots %in% names(args)
    # if any of them given, use for sizing
    if(any(empty == FALSE))
      for(i in slots[empty])
        slot(res, i) <- FLQuant(dimnames=dimnames(slot(res, slots[!empty][1])))

    # range
    if(!'range' %in% names(args))
    {
      if(exists('i'))
      {
        dims <- dims(slot(res, i))
        range <- list(min=dims$min, max=dims$max, minfbar=dims$min, maxfbar=dims$max,
          plusgroup=dims$max)
        slot(res, 'range') <- unlist(range)
      }
    }
  
    # resize: cost
    slots <- c('vcost', 'fcost')
    # find slots not provided as argument
    empty <- !slots %in% names(args)
    # if any of them given, use for sizing
    if(any(empty == FALSE))
      for(i in slots[empty])
        slot(res, i) <- FLQuant(dimnames=dimnames(slot(res, slots[!empty][1])))

    if (!("discards.wt"     %in% names(args))) discards.wt    (res)[]<-0
    if (!("discards.sel"    %in% names(args))) discards.sel   (res)[]<-0
    if (!("bycatch.harvest" %in% names(args))) bycatch.harvest(res)[]<-0
    if (!("bycatch.wt"      %in% names(args))) bycatch.wt     (res)[]<-0
    if (!("availability"    %in% names(args))) availability   (res)[]<-1

    return(res)

  }
) # }}}

# FLBRP(object='missing', sr='FLSR')  {{{
setMethod('FLBRP', signature(object='missing', sr='FLSR'),
  function(sr, ...)
  {
    args <- list(...)

    do.call('FLBRP', c(list(model=model(sr), params=params(sr)), args))
  }
) # }}}

# FLBRP(object=FLStock, sr=FLSR)  {{{
setMethod('FLBRP', signature(object='FLStock', sr='FLSR'),
  function(object, sr, ...)
  {
    FLBRP(object=object, model=sr@model, params=sr@params, ...)
  }
) # }}}

# FLBRP(object=FLStock, sr=missing) {{{
setMethod('FLBRP', signature(object='FLStock', sr='missing'),
  function(object, model=formula(rec~a), params=FLPar(1, params='a'),
    fbar=seq(0, 4, 0.04), nyears=3, biol.nyears=nyears, fbar.nyears=nyears,
    sel.nyears=fbar.nyears, na.rm=TRUE, mean='arithmetic', ...)
    {
    # dims & dimnames
    dims <- dims(object)
    if (!all(c("minfbar","maxfbar") %in% names(range(object))))
       stop("'minfbar' and 'maxfbar' missing from range")
    
    maxyear <- dims$maxyear
    byears <- ac(seq(maxyear-biol.nyears+1, maxyear))
    fyears <- ac(seq(maxyear-fbar.nyears+1, maxyear))
    syears <- ac(seq(maxyear-sel.nyears+1, maxyear))
    fages <- ac(seq(object@range['minfbar'], object@range['maxfbar']))
    snames <- dimnames(object@catch)
    dnames <- dimnames(object@catch.n)
    dnames[['year']] <- '1'

    # mean
    if(mean == 'arithmetic')
      foo <- function(x) ifelse(all(is.na(x)), 0, mean(x, na.rm=na.rm))
    else if (mean =='geometric')
      foo <- function(x) ifelse(all(is.na(x)), 0, exp(mean(log(x), na.rm=na.rm)))

    # scaling
    # 1. harvest values are divided for that year fbar (mean harvest for fages)
    scaling  <- sweep(object@harvest[,fyears], 2:6, apply(object@harvest[fages,fyears] ,
      2:6, 'mean', na.rm=na.rm), "/")
    # 2. mean across fyears. All years are thus given equal weight
    scaling <- apply(scaling, c(1,3:6), foo)
    
    # NEW FLBRP
    res <- new('FLBRP',
      # range
      range=object@range[c('min', 'max', 'plusgroup', 'minfbar', 'maxfbar')],

      # fbar
      fbar=FLQuant(fbar, units=units(object@harvest), quant=dims$quant),

      # slots to be mean of byears
      # (m, mat, harvest,spwn, m.spwn, discards.wt, landings.wt)
      m = apply(object@m[,byears], c(1,3:6), foo),
      mat = apply(object@mat[,byears], c(1,3:6), foo),
      stock.wt = apply(object@stock.wt[,byears], c(1,3:6), foo),
      harvest.spwn = apply(object@harvest.spwn[,byears], c(1,3:6), foo),
      m.spwn = apply(object@m.spwn[,byears], c(1, 3:6), foo),
      discards.wt = apply(object@discards.wt[,byears], c(1,3:6), foo),
      landings.wt = apply(object@landings.wt[,byears], c(1,3:6), foo),

      # rec.obs
      rec.obs = object@stock.n[ac(dims$min)],

      # ssb.obs
      ssb.obs= ssb(object),

      stock.obs= computeStock(object),

      # landings & discards
      landings.obs = object@landings,
      discards.obs = object@discards,

      # fbar.obs
      fbar.obs = fbar(object),

      # profit.obs
      profit.obs = FLQuant(dimnames=snames),

      # vcost & fcost
      vcost=FLQuant(dimnames=dnames),
      fcost=FLQuant(dimnames=dnames),
      
      # discards.sel & landings.sel
      discards.sel = scaling * apply(object@discards.n[,syears]/
        (object@discards.n[,syears] + object@landings.n[,syears]), c(1,3:6), foo),
      landings.sel = scaling * apply(object@landings.n[,syears]/
        (object@discards.n[,syears] + object@landings.n[,syears]), c(1,3:6), foo),

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
) # }}}
