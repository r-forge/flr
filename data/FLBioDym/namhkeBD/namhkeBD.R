# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(FLBioDym)
data(namhke)

namhkeBD <- FLBioDym(name='HKE_NAM',
  desc='Namibian hake',
  catch=FLQuant(namhke$catch, dimnames=list(year=namhke$year)),
  index=FLQuant(namhke$index, dimnames=list(year=namhke$year)),
  distribution='log')

save(namhkeBD, file='namhkeBD.RData')
