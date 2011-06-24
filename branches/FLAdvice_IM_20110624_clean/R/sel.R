setGeneric("catchSel", function(object, ...){
	standardGeneric("catchSel")})
setMethod("catchSel", signature(object="FLStock"),
   function(object,fn="fapex") sweep(harvest(object),2:6,do.call(fn,list(object)),"/"))

setGeneric("landingsSel", function(object, ...){
	standardGeneric("landingsSel")})
setMethod("landingsSel", signature(object="FLStock"),
   function(object,fn="fapex") catchSel(object,fn)*landings.n(object)/(landings.n(object)+discards.n(object)))
   
setGeneric("discardsSel", function(object, ...){
	standardGeneric("discardsSel")})
setMethod("discardsSel", signature(object="FLStock"),
   function(object,fn="fapex") catchSel(object,fn)*discards.n(object)/(landings.n(object)+discards.n(object)))

setGeneric("computeSel", function(x, ...)
  standardGeneric("computeSel"))
setMethod('computeSel', signature(x='FLStock'),
   function(x,mean="geomean",...) {
     
    fnMn=switch(substr(tolower(mean)[1],1,1),
              "a"=function(x) FLQuant(apply(x,c(1,3:6),function(x) mean(x,na.rm=T))),
                  function(x) FLQuant(apply(x,c(1,3:6),function(x) exp(mean(log(x),na.rm=T)))))

      h   <-fnMn(harvest(x))
      l   <-fnMn(landings.n(x)/catch.n(x))
      d   <-1.0-l

      return(FLQuants("harvest"   =h,
                      "landings.n"=l,
                      "discards.n"=d,
                      "catch.n"   =FLQuant(1,dimnames=dimnames(h))))})

setGeneric("sel<-", function(object,value){
	standardGeneric("sel<-")})
setMethod("sel<-", signature(object="FLStock", value="FLQuants"),
	function(object, value) {

   catch.n(   object)[]<-NA
   harvest(   object)<-recycle6Dims(value[["harvest"]],   harvest(   object))
   discards.n(object)<-recycle6Dims(value[["discards.n"]],discards.n(object))
   landings.n(object)<-recycle6Dims(value[["landings.n"]],landings.n(object))

   return(object)})

