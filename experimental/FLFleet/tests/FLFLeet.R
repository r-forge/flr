# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# {{{
# }}}

library(FLFleet)

data(ple4)

df <- as.data.frame(catch.n(ple4))

.Call('asFLQuant', df, 'age')
