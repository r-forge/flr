# pmax
if (!isGeneric("qmax")) {
	setGeneric("qmax", function(object, ...){
		standardGeneric("qmax")
	})}
	
setMethod("qmax", signature(object="FLQuant"),
qmax<-function(object,...) {

  res<-FLQuant(pmax(object@.Data,...),units=units(object))

  return(res)
	})
	
# pmin
if (!isGeneric("qmin")) {
	setGeneric("qmin", function(object, ...){
		standardGeneric("qmin")
	})}

setMethod("qmin", signature(object="FLQuant"),
qmin<-function(object,...) {

  res<-FLQuant(pmin(object@.Data,...),units=units(object))

  return(res)
	})
