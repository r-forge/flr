################################################################################
#### Functions to generate ages from length etc ################################
################################################################################

### Age Slicing
setGeneric('ageIt', function(len,n,params,...)
   standardGeneric('ageIt'))
setMethod("ageIt", signature(len="numeric",n="numeric",params="FLPar"),
  function(len,n,params,timing=0.5,plusGroup=30){
 #   .pmin<-function(object,y)  FLPar(pmin(object@.Data,y))
 #   .pmax<-function(object,y)  FLPar(pmax(object@.Data,y))

     ## expected age at length adjust to beginning of year
     age=pmax(pmin(ceiling(params["t0"]@.Data-log(1-pmin(len/params["Linf"]@.Data,.9999999))/params["K"]@.Data-timing),plusGroup),0)
#     age=params["t0"]-log(1-pmin(len/params["Linf"],.9999999))/params["K"]
#     age=pmax(pmin(round(age-timing),plusGroup),0)

     ## calculate frequencies
     frq=aggregate(n, list(age=age), sum)
     names(frq)[2]<-"freq"

     ## calculate mean length
     ln= aggregate(n*len, list(age=age), sum)[2]/
         aggregate(n,     list(age=age), sum)[2]

     return(data.frame(frq,len=ln))})
     
setMethod("ageIt", signature(len="data.frame",n="missing",params="FLPar"),
  function(len,params,timing=0.5,plusGroup=30)
     ageIt(len=len$len,n=len$n,params=params,timing=timing,plusGroup=plusGroup))

