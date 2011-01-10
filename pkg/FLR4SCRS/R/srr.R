################################################################################
#### "Simpler" SRR methods #####################################################
################################################################################

setGeneric('srr', function(params,ssb,model, ...)
  standardGeneric('srr'))
  
setMethod("srr", signature(params="FLPar",ssb="numeric",model="character"),
   function(params,ssb,model) print("do.call stuff"))

setMethod("srr", signature(params="FLPar",ssb="numeric",model="formula"),
   function(params,ssb,model) "do.call stuff" )

setMethod("srr", signature(params="FLPar",ssb="FLQuant",model="character"),
   function(params,ssb,model) "do.call stuff" )

setMethod("srr", signature(params="FLPar",ssb="FLQuant",model="formula"),
   function(params,ssb,model) "do.call stuff" )

setMethod("srr", signature(params="FLPar",ssb="FLStock",model="character"),
   function(params,ssb,model) "do.call stuff" )

setMethod("srr", signature(params="FLPar",ssb="FLStock",model="formula"),
   function(params,ssb,model) "do.call stuff" )

setMethod("srr", signature(params="FLPar",ssb="FLBiol",model="character"),
   function(params,ssb,model) "do.call stuff" )

setMethod("srr", signature(params="FLPar",ssb="FLBiol",model="formula"),
   function(params,ssb,model) "do.call stuff" )
