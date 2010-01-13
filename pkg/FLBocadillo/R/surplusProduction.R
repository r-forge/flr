#Surplus production

# sp
setGeneric('sp', function(object, ...)
		standardGeneric('sp'))

setMethod('sp', signature('FLStock'),
	function(object,rel=TRUE){

   rng1<-ac( range(object,"minyear")   :(range(object,"maxyear")-1))
   rng2<-ac((range(object,"minyear")+1): range(object,"maxyear"))

   B     <-computeStock(object)
   deltaB<-B[,rng1]-B[,rng2]

   res<-deltaB+computeCatch(object)[,rng1]

   if (rel)
      return(res/B[,rng1])
   else
      return(res)
   })
   
setMethod('sp', signature('FLBRP'),
sp.<-	function(object,rel=TRUE){

   #rng1<-ac( range(object,"minyear")   :(range(object,"maxyear")-1))
   #rng2<-ac((range(object,"minyear")+1): range(object,"maxyear"))

   # bug as should be range
   rng1<-ac( dims(yield.obs(object))$minyear   :(dims(yield.obs(object))$maxyear-1))
   rng2<-ac((dims(yield.obs(object))$minyear+1): dims(yield.obs(object))$maxyear)

   # bug as should be biomass/stock.obs nut is missing from FLBRP
   B     <-ssb.obs(object)
   deltaB<-B[,rng1]-B[,rng2]

   res<-deltaB+catch.obs(object)[,rng1]

   if (rel)
      return(res/B[,rng1])
   else
      return(res)
   })
