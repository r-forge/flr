# genericMethods - «Short one line description»
# genericMethods

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Thu 21 Jan 2010 04:55:58 PM CET IM:

setGeneric('FLBRP', function(object, sr, ...)
  standardGeneric('FLBRP'))

setGeneric('catch.obs', function(object, ...)
	standardGeneric('catch.obs'))

setGeneric('biomass.obs', function(object, ...)
	standardGeneric('biomass.obs'))

setGeneric('yield.obs', function(object, ...)
	standardGeneric('yield.obs'))

setGeneric('computeFbar', function(object, ...)
	standardGeneric('computeFbar'))

setGeneric('rec.hat', function(object, ...)
  standardGeneric('rec.hat'))

setGeneric('spr', function(object, ...)
	standardGeneric('spr'))

setGeneric('ypr', function(object, ...)
	standardGeneric('ypr'))

setGeneric('computeRefpts', function(object, ...)
	standardGeneric('computeRefpts'))

setGeneric('brp', function(object, ...)
	standardGeneric('brp'))

setGeneric('hcrYield', function(object, fbar, ...)
	standardGeneric('hcrYield'))

setGeneric('catch.hat', function(object, ...)
	standardGeneric('catch.hat'))

setGeneric('yield', function(object, ...)
	standardGeneric('yield'))

setGeneric('yield.hat', function(object, ...)
	standardGeneric('yield.hat'))

setGeneric('discards.hat', function(object, ...)
	standardGeneric('discards.hat'))

setGeneric('landings.hat', function(object, ...)
	standardGeneric('landings.hat'))

setGeneric('stock.hat', function(object, ...)
	standardGeneric('stock.hat'))

setGeneric('ssb.hat', function(object, ...)
	standardGeneric('ssb.hat'))

setGeneric('cost', function(object, ...)
	standardGeneric('cost'))

setGeneric('profit.hat', function(object, ...)
	standardGeneric('profit.hat'))

setGeneric('profit', function(object, ...)
	standardGeneric('profit'))

setGeneric("kobe", function(b,f, ...)
  standardGeneric("kobe"))

setGeneric("refpts", function(object, ...)
  standardGeneric("refpts"))

setGeneric("refpts<-", function(object, ..., value)
	standardGeneric("refpts<-"))

setGeneric('FLBRP', function(object, sr, ...)
  standardGeneric('FLBRP'))

setGeneric("msy", function(object, ...)
  standardGeneric("msy"))

setGeneric("spr0", function(ssb, rec, fbar, ...)
  standardGeneric("spr0"))
