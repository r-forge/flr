# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $


setGeneric("readAspic",    function(x,...)      standardGeneric("readAspic"))
setGeneric("aspicRuns",    function(x,scen,...) standardGeneric("aspicRuns"))
setGeneric("aspicProj",    function(x,scen,...) standardGeneric("aspicProj"))

setGeneric("readPro2box",  function(x,type="missing",...) standardGeneric("readPro2box"))
setGeneric("readVpa2box",  function(x,type="missing",...) standardGeneric("readVpa2box"))

setGeneric("readAdmb",     function(file,...)   standardGeneric("readAdmb"))
setGeneric("writeAdmb",    function(file,x,...) standardGeneric("writeAdmb"))

setGeneric("diags",        function(x,...)   standardGeneric("diags"))

#setGeneric('diags<-', function(object, ..., value) standardGeneric('diags<-'))

#setMethod("diags<-", signature(object="FLAdapt", value="character"),
#  function(object, value) {
#		slot(object, "diags") <- readVPA2boxDiags(value)
		
#  return(object)}) 