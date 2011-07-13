# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# {{{
# }}}


flqm <- showMethods(classes='FLQuant', printTo=FALSE, showEmpty=FALSE)
flqa <- showMethods(classes='FLArray', printTo=FALSE, showEmpty=FALSE)

summary(flqm)

flqm[grep('Function', flqm)]
