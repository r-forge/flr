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

if (demoIt)
  fbar(pBrp)<-1:10

#### 1.3 agreed fix
## add fbar<- overloads to FLBRP
################################################################################

### 2) simple refpt methods to allow extarction of common reference points
setGeneric("msy", function(object,...){
	standardGeneric("msy")})
setMethod("msy", signature(object="FLBRP"),
	function(object,...) refpts(object)["msy","yield"])

setGeneric("fmsy", function(object,...){
	standardGeneric("fmsy")})
setMethod("fmsy", signature(object="FLBRP"),
	function(object,...) refpts(object)["msy","harvest"])

setGeneric("bmsy", function(object,...){
	standardGeneric("bmsy")})
setMethod("bmsy", signature(object="FLBRP"),
	function(object,...) refpts(object)["msy","ssb"])
