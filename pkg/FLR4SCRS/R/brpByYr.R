setGeneric("brpByYr", function(object, ...)
	standardGeneric("brpByYr"))
setMethod("brpByYr", signature("FLStock"), 
    function(object,returnAll=FALSE,...){

   i2y<-function(x){
      res  <-aperm(x,c(1,6,3:5,2))
      dmns <-dimnames(res)
      names(dmns)[c(2,6)]<-names(dmns)[c(6,2)]

      FLQuant(res,dimnames=dmns)}

  smoothAY<-function(x){
    fnLoess<-function(x) {xvar<-1:length(x); loess(x~1:xvar)$fitted}

    res <-apply(x,  c(1,2), median)
    res2<-adply(res,  1,    fnLoess)
   
    as.FLQuant(unlist(res2[,-1]),dimnames=dimnames(res))}

   res <-qapply(object, smoothAY)
   res <-qapply(res, i2y)
   units(harvest(res))<-"f"

   res<-brp(FLBRP(res,nyears=1))

   if(returnAll)
      return(res)

   res<-refpts(brp(FLBRP(res,nyears=1)))@.Data[1:4,1:5,]
 
   dmns          <-dimnames(res)
   dmns[[3]]     <-dimnames(catch.n(object))$year
   names(dmns)[3]<-"year"
   dimnames(res) <-dmns
  
   return(res)})
  
  