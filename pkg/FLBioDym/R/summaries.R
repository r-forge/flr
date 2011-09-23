# summaries.R - 
# FLBioDym/R/summaries.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $
  
# summaryStats {{{
summaryStats <- function(bd) {
  stk   =as.data.frame(stock(  bd)[,dims(bd)$year-1])[,6:7]
  hvt   =as.data.frame(catch(bd)[,dims(bd)$year-1]/stock(bd)[,dims(bd)$year-1])[,6:7]
  stkRel=as.data.frame(stock(bd)[,dims(bd)$year-1]/c(refpts(bd)["bmsy"]))[,6:7]
  hvtRel=as.data.frame((catch(bd)[,dims(bd)$year-1]/stock(  bd)[,dims(bd)$year-1])/c(refpts(bd)["fmsy"]))[,6:7]
  stkK  =as.data.frame(stock(bd)[,dims(bd)$year-1]/c(params(bd)["K"]))[,6:7]
  rps   =as.data.frame(refpts(bd))
  par   =as.data.frame(params(bd))
    
  names(rps)[1]="params"
  names(par)[1]="params"

  res<-rbind(rps,par,
    data.frame(params="stock",     stk),
    data.frame(params="harvest",   hvt),
    data.frame(params="stockMSY",  stkRel),
    data.frame(params="harvestMSY",hvtRel),
    data.frame(params="stockK",    stkK))
 
   return(as.FLQuant(res))
}
# }}}
