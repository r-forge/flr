setGeneric("iter", function(obj, ...)
  standardGeneric("iter"))

setMethod("iter", signature(obj="FLComp"),
          function(obj, iter) {
            
          obj@params=iter(obj@params,iter)
          obj@bounds=iter(obj@bounds,iter)
          obj@catch =iter(obj@catch, iter)
          obj@stock =iter(obj@stock, iter)
          
          if ("iter" %in% names(obj@cpue)){
          flag=obj@cpue$iter %in% iter
          obj@cpue=obj@cpue[flag,]}
          
          return(obj)})
            