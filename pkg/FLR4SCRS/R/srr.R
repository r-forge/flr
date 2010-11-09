################################################################################
#### "Simpler" SRR methods #####################################################
################################################################################

setGeneric('srr', function(model,params,ssb,rec, ...)
  standardGeneric('srr'))
  
setMethod("srr", signature(model="character",params="FLPar",ssb="numeric",rec="numeric"),
   function(model,params,ssb,rec) "do.call stuff" )

setMethod("srr", signature(model="formula",  params="FLPar",ssb="numeric",rec="numeric"),
   function(model,params,ssb,rec) "do.call stuff" )

setMethod("srr", signature(model="character",params="FLPar",ssb="FLQuant",rec="FLQuant"),
   function(model,params,ssb,rec) "do.call stuff" )

setMethod("srr", signature(model="formula",  params="FLPar",ssb="FLQuant",rec="FLQuant"),
   function(model,params,ssb,rec) "do.call stuff" )

setMethod("srr", signature(model="character",params="FLPar",ssb="FLStock"),
   function(model,params,ssb,rec) "do.call stuff" )

setMethod("srr", signature(model="formula",  params="FLPar",ssb="FLStock"),
   function(model,params,ssb,rec) "do.call stuff" )

setMethod("srr", signature(model="character",params="FLPar",ssb="FLBiol"),
   function(model,params,ssb,rec) "do.call stuff" )

setMethod("srr", signature(model="formula",  params="FLPar",ssb="FLBiol"),
   function(model,params,ssb,rec) "do.call stuff" )
