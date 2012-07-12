# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

setGeneric('aspic',        function(object,idx,...)   standardGeneric('aspic'))

setGeneric("readAspic",    function(x,...)            standardGeneric("readAspic"))

setGeneric("aspicRuns",    function(x,scen,...)       standardGeneric("aspicRuns"))
setGeneric("aspicProj",    function(x,scen,...)       standardGeneric("aspicProj"))

setGeneric("aspicCpue",    function(x,...)            standardGeneric("aspicCpue"))

setGeneric("writeAspic",   function(x,...)            standardGeneric("writeAspic"))

setGeneric("+",            function(e1,e2,...)        standardGeneric("+"))

setGeneric("diags",        function(x,...)            standardGeneric("diags"))



