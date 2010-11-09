setGeneric('reFac', function(x, ...)
	standardGeneric('reFac'))

setMethod("reFac", signature(x="data.frame"),
 function(x){

  for (i in names(x))
      if (is(x[,i],"factor"))
         x[,i]<-factor(x[,i])

   return(x)})

setGeneric('latLon', function(x, ...)
	standardGeneric('latLon'))

setMethod("latLon", signature(x="data.frame"),
latLon<-function(x){

  x$lat<-x$lat + switch(x$square,
                           "1x1"    = 0.5,
                           "5x5"    = 2.5,
                           "10x20"  = 5.0,
                           "10x10"  = 5.0,
                           "5x10"   = 2.5,
                           "20x20"  = 10.0,
                           "none"   = 0.0)

  x$lon<-x$lon + switch(x$square,
                           "1x1"    =  0.5,
                           "5x5"    =  2.5,
                           "10x20"  = 10.0,
                           "10x10"  =  5.0,
                           "5x10"   =  5.0,
                           "20x20"  = 10.0,
                           "none"   =  0.0)

   x[x$quad %in% 3:4, "lon"]<--x[x$quad %in% 3:4, "lon"]
   x[x$quad %in% 2:3, "lat"]<--x[x$quad %in% 2:3, "lat"]

   return(x)})


setGeneric('iccatMonth', function(x, ...)
	standardGeneric('iccatMonth'))

setMethod("iccatMonth", signature(x="numeric"),
function(x){
  x <-pmax(0,x)

  fn<-function(x) switch(ac(x),
                         "13"= 2,
                         "14"= 5,
                         "15"= 8,
                         "16"=11,
                         "17"= 6,
                         "18"= 3,
                         "19"= 9,
                         x)

  sapply(x,fn)})
