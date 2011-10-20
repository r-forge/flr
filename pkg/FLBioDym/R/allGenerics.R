# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

setGeneric('FLBioDym', function(object, ...)
  standardGeneric('FLBioDym'))

setGeneric( 'msy', function(object,params,...)
    standardGeneric( 'msy'))
 
setGeneric('fmsy', function(object,params,...)
  standardGeneric('fmsy'))

setGeneric('bmsy', function(object,params,...)
  standardGeneric('bmsy'))

setGeneric('refptSE', function(object,...)
  standardGeneric('refptSE'))

setGeneric('admbBD', function(object,...)
  standardGeneric('admbBD'))
 
setGeneric('hcr', function(object, ...)
    standardGeneric('hcr'))

setGeneric('TAC', function(object, ...)
  standardGeneric('TAC'))
