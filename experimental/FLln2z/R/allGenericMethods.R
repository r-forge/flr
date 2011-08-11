# genericMethods - «Short one line description»
# genericMethods

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Thu 21 Jan 2010 04:55:58 PM CET IM:

setGeneric('FLln2z', function(object, ...)   standardGeneric('FLln2z'))

setGeneric('obs',       function(object,  ...) standardGeneric('obs'))
setGeneric('hat',       function(object,  ...) standardGeneric('hat'))
setGeneric('residuals', function(object,  ...) standardGeneric('residuals'))
setGeneric('bounds',    function(object,  ...) standardGeneric('bounds'))

setMethod("obs",       signature(object="FLln2z"),             function(object) object@obs)
setMethod("hat",       signature(object="FLln2z"),             function(object) object@hat)
setMethod("residuals", signature(object="FLln2z"),             function(object) object@residuals)
setMethod("bounds",    signature(object="FLln2z"),             function(object) object@bounds)