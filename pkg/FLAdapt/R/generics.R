# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

setGeneric("readPro2box",  function(x,type,...)   standardGeneric("readPro2box"))
setGeneric("readVpa2box",  function(x,type,...)   standardGeneric("readVpa2box"))
 
setGeneric('fit',    function(object, ...)         standardGeneric('fit'))
setGeneric('boot',   function(object, ...)         standardGeneric('boot'))
setGeneric('pro2box',function(object, ctrl,...)    standardGeneric('pro2box'))
 
setGeneric('FLAdapt',        function(object, ...)         standardGeneric('FLAdapt'))
setGeneric('FLAdaptControl', function(object, ...)         standardGeneric('FLAdaptControl'))
#setGeneric('FLAdaptControl<-', function(object,...,value)  standardGeneric('FLAdaptControl<-'))


