setGeneric("computeGrad", function(object, ...)
  standardGeneric("computeGrad"))
setGeneric("computeLogl", function(object, ...)
  standardGeneric("computeLogl"))
setGeneric("computeInitial", function(object, ...)
  standardGeneric("computeInitial"))

if (FALSE) {
setMethod('computeGrad', signature(object='FLModel'),
   function(object) srrFunc.(object,what="grad"))
setMethod('computeGrad', signature(object='FLModel',par="FLPar"),
   function(object) {
         params(object)<-par  
         srrFunc.(object,what="logl")})

setMethod('computeLogl', signature(object='FLModel'),
   function(object) srrFunc.(object,what="logl"))
setMethod('computeLogl', signature(object='FLModel',par="FLPar"),
   function(object) {
         params(object)<-par  
         srrFunc.(object,what="logl")})

setMethod('computeInitial', signature(object='FLModel'),
   function(object) srrFunc.(object,what="grad"))
setMethod('computeInitial', signature(object='FLModel',par="FLPar"),
   function(object) {
         params(object)<-par  
         srrFunc.(object,what="logl")})
}

