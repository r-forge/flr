# FLSP - «Short one line description»
# FLSP

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 18 Feb 2009 13:40
# $Id$

# TO DO

# Reference:
# Notes:

# TODO Tue 10 Feb 2009 10:56:25 PM CET IM:

#library(FLAssess)

#data(ple4)
#
## create a new FLSP from ple4
#fsp <- FLSP(index=stock(ple4), catch=catch(ple4), mpar=2, delta=1, name='ple4SP', model='pellatomC')
#
#fsp <- fmle(fsp, start=list(r=0.5, K=500000, Q=10, sigma2=10), lower=c(1e-8, max(catch(fsp)), 1e-8, 1e-8), upper=c(1, Inf, Inf, Inf))
#
#plot(fsp)
#
## Fix r
#fsp <- fmle(fsp, start=list(K=300000, Q=10, sigma2=10), fixed=list(r=0.5))
#
#data(nhke)
#
##nhke <- fmle(nhke, start=list(K=2000, r=0.5, Q=100, sigma2=50))
