sp<-function(x,rel=FALSE){
   rng1<-ac( range(x,"minyear")   :(range(x,"maxyear")-1))
   rng2<-ac((range(x,"minyear")+1): range(x,"maxyear"))

   B     <-computeStock(x)
   deltaB<-B[,rng1]-B[,rng2]

   res<-deltaB+computeCatch(x)[,rng1]

   if (rel)
      return(res/B[,rng1])
   else
      return(res)
   }
   
