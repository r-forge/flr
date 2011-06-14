setGeneric("fbar<-", function(object,value,...){
	standardGeneric("fbar<-")})
setMethod("fbar<-", signature(object="FLBRP", value="numeric"),
	function(object, value,...) {

		object@fbar<-FLQuant(value,quant="age")

		return(object)})

setMethod("fbar<-", signature(object="FLBRP", value="FLQuant"),
	function(object, value,...) {

		object@fbar<-value

		return(object)})