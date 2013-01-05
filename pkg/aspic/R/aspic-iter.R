if (!isGeneric("iter")) setGeneric("iter", function(obj, ...)
  standardGeneric("iter"))

setMethod("iter", signature(obj="FLComp"),
          function(obj, iter) {
            
          obj@params=iter(obj@params,  iter)
          obj@control=iter(obj@control,iter)
          obj@catch =iter(obj@catch,   iter)
          obj@stock =iter(obj@stock,   iter)
          
          if ("iter" %in% names(obj@index)){
          flag=obj@index$iter %in% iter
          obj@index=obj@index[flag,]}
          
          return(obj)})
            