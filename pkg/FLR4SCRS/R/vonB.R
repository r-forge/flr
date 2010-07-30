################################################################################
#### Functions to generate ages from length etc ################################
################################################################################

#### Growth
setGeneric('vonB', function(object, params, ...)
  standardGeneric('vonB'))
setMethod("vonB", signature(object="numeric", params="FLPar"),
   function(object,params) params["Linf"]*(1.0-exp(-params["K"]*(object-params["t0"]))))
setMethod("vonB", signature(object="FLQuant", params="FLPar"),
   function(object,params) params["Linf"]*(1.0-exp(-params["K"]*(object-params["t0"]))))
setMethod("vonB", signature(object="FLCohort", params="FLPar"),
   function(object,params) params["Linf"]*(1.0-exp(-params["K"]*(object-params["t0"]))))
setMethod("vonB", signature(object="FLQuant",params="missing"),
   function(object,Linf=NA,K=NA,t0=NA) {
      params<-FLPar(Linf=Linf,K=K,t0=t0)
      params["Linf"]*(1.0-exp(-params["K"]*(object-params["t0"])))})
setMethod("vonB", signature(object="FLCohort",params="missing"),
   function(object,Linf=NA,K=NA,t0=NA) {
      params<-FLPar(Linf=Linf,K=K,t0=t0)
      params["Linf"]*(1.0-exp(-params["K"]*(object-params["t0"])))})
setMethod("vonB", signature(object="numeric",params="missing"),
   function(object,Linf=NA,K=NA,t0=NA) {
      params<-FLPar(Linf=Linf,K=K,t0=t0)
      params["Linf"]*(1.0-exp(-params["K"]*(object-params["t0"])))})

setGeneric('vonBMass', function(object, params, ...)
  standardGeneric('vonBMass'))
setMethod("vonBMass", signature(object="FLQuant", params="FLPar"),
   function(object,params) params["a"]*vonB(object,params)^params["b"])
setMethod("vonBMass", signature(object="FLCohort", params="FLPar"),
   function(object,params) params["a"]*vonB(object,params)^params["b"])
setMethod("vonBMass", signature(object="numeric", params="FLPar"),
   function(object,params) params["a"]*vonB(object,params)^params["b"])
setMethod("vonBMass", signature(object="FLQuant", params="missing"),
   function(object,Linf=NA,K=NA,t0=NA,a=NA,b=NA){
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return(params["a"]*vonB(object,params)^params["b"])})
setMethod("vonBMass", signature(object="FLCohort", params="missing"),
   function(object,Linf=NA,K=NA,t0=NA,a=NA,b=NA){
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return(params["a"]*vonB(object,params)^params["b"])})
setMethod("vonBMass", signature(object="numeric", params="missing"),
   function(object,Linf=NA,K=NA,t0=NA,a=NA,b=NA){
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return(params["a"]*vonB(object,params)^params["b"])})

setGeneric('ages', function(object, ...)
   standardGeneric('ages'))
setMethod("ages", signature(object="FLQuant"),
   function(object,timing=NULL){
      res<-FLQuant(dimnames(object)$age,dimnames=dimnames(object))

      if (is.null(timing))
         res<-sweep(res,4,(1:dim(res)[4]-1)/dim(res)[4],"+") else
         res<-sweep(res,4,timing,"+")

      return(res)})

