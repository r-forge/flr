# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(FLBioDym)
data(alb)

albBD <- FLBioDym(name='ALB_SATL',
  desc='South Atlantic Albacore catch and CPUE from Polacheck et al, 2001.',
  catch=FLQuant(alb$catch, dimnames=list(year=alb$year)),
  index=FLQuant(alb$index, dimnames=list(year=alb$year)),
  distribution='log')

save(albBD, file='albBD.RData')
