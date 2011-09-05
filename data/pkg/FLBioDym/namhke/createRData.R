# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# $Id:  $

library(FLBioDym)

dat <- read.table("namhke.dat")

namhke <- FLBioDym(name="NA_HKE",
  desc=paste("Namibian hake. Created on", date(), ".",  R.Version()$version.string),
  catch=FLQuant(dat$catch, dimnames=list(year=dat$year)),
  index=FLQuant(dat$index, dimnames=list(year=dat$year)),
  model="pellat",
  distribution=as.factor('lnorm')
)

namhke <- admbBD(namhke)

save(namhke, file='namhke.RData')
