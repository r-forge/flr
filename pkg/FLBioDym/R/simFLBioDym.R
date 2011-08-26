# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# simFLBioDym {{{
simFLBioDym <- function(model='pellat', params=FLPar(r=0.5, K=100, p=1, b0=1.0, sigma=0,3),
                        harvest,bounds=c(0.1,10),...) {

   args <- list(...)

    nyr <- dims(harvest)$year

    object <- FLBioDym(model='pellat',
      stock =FLQuant(rep(params["K"], nyr), dimnames=dimnames(harvest)),
      params=params)

    object <- fwd(object, harvest=harvest)

    # bounds
    object@bounds["r",     "start"]=params["r"]
    object@bounds["K",     "start"]=params["K"]
    object@bounds["p",     "start"]=params["p"]
    object@bounds["b0",    "start"]=params["b0"]
    object@bounds["q",     "start"]=1.0
    object@bounds["sigma", "start"]=params["sigma"]
  
    object@bounds[,"lower"]=object@bounds[,"start"]*bounds[1]
    object@bounds[,"upper"]=object@bounds[,"start"]*bounds[2]
    
    object@bounds["p", "phase"]=-1
    object@bounds["b0","phase"]=-1
    object@priors[,1]=-1
   
    # Load given slots
    for(i in names(args))
			slot(object, i) <- args[[i]]

    return(object)
} # }}}
