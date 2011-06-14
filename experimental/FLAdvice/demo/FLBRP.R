# FLBRP - «Short one line description»
# FLBRP

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 27 May 2009 17:11
# $Id:  $

# Reference:
# Notes:

# TODO Wed 18 Mar 2009 10:43:59 AM CET IM:

library(FLBRP)

data(ple4)

# Create an FLBRP object from ple4 with default values
pleBrp <- FLBRP(ple4)

# plot of selection pattern in catch and discards, stock.wt-at-age,
#  catch.wt-at-age, maturity and natural mortality
xyplot(data~age|qname,data=FLQuants(catch.sel = catch.sel(pleBrp),
  discards.sel = discards.sel(pleBrp),
  stock.wt = stock.wt(pleBrp),
  catch.wt = catch.wt(pleBrp),
  maturity =     mat(pleBrp),
  M = m(pleBrp)), type="l", scale="free", ylab="")

# The fbar slot contains F values for whcih equilibrium conditions are calculated
fbar(pleBrp)

# The FLBRP creator deafults to a range of F from 0 to 4 at step of 0.04,
#  but that can be changed

fbar(pleBrp) <- FLQuant(seq(0, 3, by=0.05))

# Once the FLBRP object has been created, reference points and equilibrium values
#  can be calculated.

# A default stock-recruitment relationship of 'geomean' with a = 1 is used.
model(pleBrp)
params(pleBrp)

# the object is sent to brp() for calculation of refpts
pleBrp <- brp(pleBrp)

# and the equilibirum values are now available
harvest(pleBrp)
stock.n(pleBrp)
landings.n(pleBrp)
yield(pleBrp)
ssb(pleBrp)

# reference points are also stored in the object, at the 'refpts' slot
refpts(pleBrp)

# Fmsy is the same as Fmax, since the default assumed stock recruitment relationship is 
# mean recruitment. You can plot the reference points and expected quantities over the 
# range of fbar values.
plot(pleBrp)

# A number of refpts are calculated by default, as specified in the default
# refpts object
dimnames(refpts())$refpt

# The spr.30 refpt coresponds to the percentage of virgin e.g. spr.30
# would be 30% of virgin. To estimate alternative values, especify spr.* in
# a new refpts object
refpts(pleBrp) <- refpts(as.numeric(NA), refpt='spr.40')
computeRefpts(pleBrp)
