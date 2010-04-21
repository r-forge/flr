# class - «Short one line description»
# FLBRP/R/class.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, ICCAT & Santiago Cerviño, IEO
# $Id$


# refpts class {{{
validrefpts <- function(object)
{
  # array must have 3 dims
  if(length(dim(object)) != 3 )
    return('object array must have 3 dimensions')

  # names of dimnames must be refpt, quantity and iter
  if(!all.equal(names(dimnames(object)), c('refpt', 'quantity', 'iter')))
    return('dimnames must be refpt, quantity and iter')

  return(TRUE)
}

setClass('refpts', representation('FLPar'),
  prototype=prototype(new('FLPar', array(as.numeric(NA), dim=c(5,8,1),
  dimnames=list(refpt=c('f0.1', 'fmax', 'spr.30', 'msy', 'mey'), quantity=c('harvest', 
  'yield', 'rec', 'ssb', 'biomass', 'revenue', 'cost', 'profit'), iter=1)))),
  validity=validrefpts)

# }}}

# FLBRP {{{
validFLBRP <- function(object)
{
  # (1) FLQuant objects must share dimnames[1:5] as follows
  #  fbar.obs with landings.obs, discards.obs, ssb.obs, profit.obs
  for (i in c("landings.obs", "discards.obs", "ssb.obs", "stock.obs", "profit.obs"))
    if(!all.equal(dimnames(object@fbar.obs), dimnames(slot(object, i))))
      return(paste("dimnames mismatch:", i))
  # with rec.obs, except dimnames[1]
  if(!all.equal(dimnames(object@fbar.obs)[-1], dimnames(object@rec.obs)[-1]))

  # and slots with quant > 1
  for(i in c("discards.sel", "stock.wt", "landings.wt", "discards.wt", "bycatch.wt",
    "bycatch.harvest", "m", "mat", "harvest.spwn", "m.spwn", "price", "availability"))
    if(!all.equal(dimnames(object@landings.sel), dimnames(slot(object, i))))
      return(paste("dimnames mismatch:", i))

  # range
  dims <- dims(object)
  range <- as.list(object@range)
  if(range$min < dims$min | range$max > dims$max | range$plusgroup > dims$max |   
    range$minfbar < dims$min | range$maxfbar > dims$max)
    return("mismatch between range and object dimensions")

  return(TRUE)
}

   
setClass("FLBRP",
   representation(
      "FLComp",
      model          ="formula",
      params         ="FLPar",
      refpts         ="refpts",
      fbar           ="FLQuant",
      fbar.obs       ="FLQuant",
      landings.obs   ="FLQuant",
      discards.obs   ="FLQuant",
      rec.obs        ="FLQuant",
      ssb.obs        ="FLQuant",
      stock.obs      ="FLQuant",
      profit.obs     ="FLQuant",
      landings.sel   ="FLQuant",
      discards.sel   ="FLQuant",
      bycatch.harvest="FLQuant",
      stock.wt       ="FLQuant",
      landings.wt    ="FLQuant",
      discards.wt    ="FLQuant",
      bycatch.wt     ="FLQuant",
      m              ="FLQuant",
      mat            ="FLQuant",
      harvest.spwn   ="FLQuant",
      m.spwn         ="FLQuant",
      availability   ="FLQuant",
      price          ="FLQuant",
      vcost          ="FLQuant",
      fcost          ="FLQuant"),
   prototype=prototype(
      name            =character(0),
      desc            =character(0),
      range           =unlist(list(min=as.numeric(NA), max=as.numeric(NA),
        plusgroup=as.numeric(NA), minfbar=as.numeric(NA), maxfbar=as.numeric(NA))),
      model        =formula(rec~a),
      params       =FLPar(1),
      refpts          =new('refpts'),
      fbar            =new("FLQuant"),
      landings.sel    =new("FLQuant"),
      fbar.obs        =new("FLQuant"),
      landings.obs    =new("FLQuant"),
      discards.obs    =new("FLQuant"),
      rec.obs         =new("FLQuant"),
      ssb.obs         =new("FLQuant"),
      stock.obs       =new("FLQuant"),
      profit.obs      =new("FLQuant"),
      landings.sel    =new("FLQuant"),
      discards.sel    =new("FLQuant"),
      bycatch.harvest =new("FLQuant"),
      stock.wt        =new("FLQuant"),
      landings.wt     =new("FLQuant"),
      discards.wt     =new("FLQuant"),
      bycatch.wt      =new("FLQuant"),
      m               =new("FLQuant"),
      mat             =new("FLQuant"),
      harvest.spwn    =new("FLQuant"),
      m.spwn          =new("FLQuant"),
      availability    =new("FLQuant"),
      price           =new("FLQuant"),
      vcost           =new("FLQuant"),
      fcost           =new("FLQuant"),
      validity        =validFLBRP
      ))  # }}}
