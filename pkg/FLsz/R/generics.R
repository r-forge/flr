# genericMethods - «Short one line description»
# genericMethods

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Thu 21 Jan 2010 04:55:58 PM CET IM:

setGeneric('FLsz', function(object, ...)   standardGeneric('FLsz'))

setGeneric('obs',       function(object,  ...) standardGeneric('obs'))
setGeneric('hat',       function(object,  ...) standardGeneric('hat'))
setGeneric('residuals', function(object,  ...) standardGeneric('residuals'))
setGeneric('bounds',    function(object,  ...) standardGeneric('bounds'))
setGeneric('grw',       function(object,  ...) standardGeneric('grw'))
setGeneric('fit',       function(object,  ...) standardGeneric('fit'))

setMethod("obs",       signature(object="FLsz"),             function(object) object@obs)
setMethod("hat",       signature(object="FLsz"),             function(object) object@hat)
setMethod("residuals", signature(object="FLsz"),             function(object) object@residuals)
setMethod("bounds",    signature(object="FLsz"),             function(object) object@bounds)
setMethod("grw",       signature(object="FLsz"),             function(object) object@grw)