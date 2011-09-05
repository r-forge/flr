# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# $Id:  $

library(FLBioDym)

dat <- read.table("alb.dat")

satlalb <- FLBioDym(name="SATL_ALB",
  desc=paste('South Atlantic Albacore catch and CPUE. Polacheck (1993). Created on',
    date(), ".",  R.Version()$version.string),
  catch=FLQuant(dat$catch, dimnames=list(year=dat$year)),
  index=FLQuant(dat$index, dimnames=list(year=dat$year)),
  model="pellat",
  distribution=as.factor('lnorm')
)

save(satlalb, file='satlalb.RData')
