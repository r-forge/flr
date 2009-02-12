# FLSP - «Short one line description»
# FLSP

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 10 Feb 2009 22:56
# $Id:  $

# Reference:
# Notes:

# TODO Tue 10 Feb 2009 10:56:25 PM CET IM:


data(ple4)

# create a new FLSP from ple4
fsp <- FLSP(catch=catch(ple4), index=stock(ple4), mpar=2, delta=1, name='ple4SP',
  model='pellatom')

fsp <- fmle(fsp, start=list(r=0.5, K=300000, Q=10, sigma2=10))

plot(fsp)

