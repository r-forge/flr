setMethod("aspics", signature(object="aspic"), function(object, ...) {
    lst <- c(object, list(...))
    aspics(lst)})

getGeneric("spFn", function(biomass,params,...){
     value  <-  standardGeneric("spFn")
   	return(value)})

setGeneric('fwd', function(object,ctrl,...)
  standardGeneric('fwd'))

 setGeneric("aspicCpues", function(biomass,params,...){
     value  <-  standardGeneric("aspicCpues")
   	return(value)})
 
