# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# simFLBioDym {{{
simFLBioDym <- function(model='pellat', harvest, error='log', cv=0.3,
  params=list(r=0.5, K=100, p=1, m=0.25, b0=1.0)) {

    nyr <- dims(harvest)$year

    res <- FLBioDym(model='pellat',
      stock=FLQuant(rep(K, nyr), dimnames=dimnames(harvest)),
      params=FLPar(c(r=r,K=K,p=p,b0=b0,q=1,sigma=0.3)))

    res <- fwd(res, harvest=harvest)

    stk <- stock(object)

    index(object) <- switch(error,
      'log' = exp(rnorm(nyr, 0, cv))*(stk[,-(nyr+1)]+stk[,-1])/2,
      'normal'= exp(rnorm(nyr,0,cv))*(stk[,-(nyr+1)]+stk[,-1])/2,
      'cv' = exp(rnorm(nyr,0,cv))*(stk[,-(nyr+1)]+stk[,-1])/2)

    # bounds
    object@bounds["r",     "start"]=r
    object@bounds["K",     "start"]=K
    object@bounds["p",     "start"]=p
    object@bounds["b0",    "start"]=b0
    object@bounds["q",     "start"]=1.0
    object@bounds["sigma", "start"]=0.3
  
    object@bounds[,"lower"]=object@bounds[,"start"]*.1
    object@bounds[,"upper"]=object@bounds[,"start"]*10
    
    object@bounds["p", "phase"]=-1
    object@bounds["b0","phase"]=-1
    object@priors[,1]=-1

    return(object)
} # }}}
