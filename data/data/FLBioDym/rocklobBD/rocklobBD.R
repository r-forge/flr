# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(FLBioDym)
data(rocklob)

rocklobBD <- FLBioDym(name='LOB',
  desc='Rock Losbter',
  catch=FLQuant(rocklob$catch, dimnames=list(year=rocklob$year)),
  index=FLQuant(rocklob$index, dimnames=list(year=rocklob$year)),
  distribution='log')

save(rocklobBD, file='rocklobBD.RData')
