# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(FLBioDym)

data(ple4)

obj <- FLBioDym(catch=catch(ple4), index=ssb(ple4))

catch(obj)
bounds(obj)

# {{{
# }}}
