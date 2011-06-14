# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, ICCAT & Santiago Cerviño, IEO
# $Id:  $

setGeneric("catch.obs", function(object, ...)
	standardGeneric("catch.obs"))

setGeneric("biomass.obs", function(object, ...)
	standardGeneric("biomass.obs"))

setGeneric("yield.obs", function(object, ...)
	standardGeneric("yield.obs"))

setGeneric("computeFbar", function(object, ...)
	standardGeneric("computeFbar"))

setGeneric("rec.hat", function(object, ...)
	standardGeneric("rec.hat"))

setGeneric("ypr", function(object, ...)
	standardGeneric("ypr"))

setGeneric("computeRefpts", function(object, ...)
	standardGeneric("computeRefpts"))

setGeneric("brp", function(object, ...)
	standardGeneric("brp"))

setGeneric("hcrYield", function(object, fbar, ...)
	standardGeneric("hcrYield"))

setGeneric("catch.hat", function(object, ...)
	standardGeneric("catch.hat"))

setGeneric("yield", function(object, ...)
	standardGeneric("yield"))

setGeneric("yield.hat", function(object, ...)
	standardGeneric("yield.hat"))

setGeneric("discards.hat", function(object, ...)
	standardGeneric("discards.hat"))

setGeneric("landings.hat", function(object, ...)
	standardGeneric("landings.hat"))

setGeneric("stock.hat", function(object, ...)
	standardGeneric("stock.hat"))

setGeneric("ssb.hat", function(object, ...)
	standardGeneric("ssb.hat"))

setGeneric("revenue.hat", function(object, ...)
	standardGeneric("revenue.hat"))

setGeneric("cost", function(object, ...)
	standardGeneric("cost"))

setGeneric("profit", function(object, ...)
	standardGeneric("profit"))

setGeneric("profit.hat", function(object, ...)
	standardGeneric("profit.hat"))

setGeneric("spr", function(object, ...)
	standardGeneric("spr"))

setGeneric("spr0", function(ssb, rec, fbar, ...)
	standardGeneric("spr0"))

setGeneric("refpts", function(object, ...)
	standardGeneric("refpts"))

setGeneric("refpts<-", function(object, ..., value)
	standardGeneric("refpts"))

setGeneric("msy", function(object, ...)
	standardGeneric("msy"))

setGeneric('FLBRP', function(object, sr, ...)
		standardGeneric('FLBRP'))

setGeneric("npv", function(object, ...)
	standardGeneric("npv"))

