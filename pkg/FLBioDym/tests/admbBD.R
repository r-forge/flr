# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(FLBioDym)
library(plyr)

#library(doMC)

#registerDoMC(3)

#
params <- FLPar(r=0.5, K=1000, p=1, b0=1, q=1, sigma=0.3)
harvest <- FLQuant(seq(0,1.5,length.out=40)*fmsy("pellat",params))

bd <- simFLBioDym(harvest=harvest, params=params)

OEMSurveyBD <- function(object, error="log") {
  nyr <- dims(harvest(object))$year
  stk <- stock(object)
   
  res <- switch(error,
    'log' = exp(rnorm(prod(dim(stk)[-2])*nyr, 0,
        params["sigma"]))*(stk[,-(nyr+1)]+stk[,-1])/2, NULL)
      
  return(res)
}

bd <- propagate(bd, 5)

index(bd) <- OEMSurveyBD(bd)

res <- admbBD(bd)
