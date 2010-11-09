################################################################################

#### Growth functions ##########################################################
#                                                                              #
# 1) FLQuant to ages                                                           #
# 2) length to weight                                                          #
# 3) Weight to length                                                          #
# 4) Von Bertalanffy, length                                                   #
# 5) Von Bertalanffy, weigth                                                   #
# 6) Gompertz                                                                  #
# 7) Logistic                                                                  #
# 8) Richards                                                                  #
# 9) Schnute                                                                   #
################################################################################

iniVonB<-function(object,Minf,K,t0,a){
   params<-FLPar(Minf=Minf,K=K,t0=t0,a=a)
   plot(object$age,object$data)
   points(object$age,params["Minf"]*(1.0-exp(-params["K"]*(object$age-params["t0"])))^a,col="red",pch=17)

   params<-as.list(params)
   names(params)<-c("Minf","K","t0","a")
   return(params)}

iniLog<-function(object,a50,ato95,asym){
   plot(  object$age,object$data)
   points(logisticFn(a50,ato95,asym,object$age),col="red",pch=17)

   return(c(a50=a50,ato95=ato95,asym=asym))}

iniRich<-function(object,a50,ato95,beta){
   plot(  object$age,object$data)
   points(logisticFn(a50,ato95,beta,object$age),col="red",pch=17)

   return(c(a50=a50,ato95=ato95,beta=beta))}

iniSchn<-function(object,a,b,t1,t2,y1,y2){
   plot(  object$age,object$data)
   points(schnute(object$age,FLPar(a=a,b=b,t1=t1,t2=t2,y1=y1,y2=y2)),col="red",pch=17)

   return(c(a=a,b=b,t1=t1,t2=t2,y1=y1,y2=y2))}


# 1) FLQuant to ages ###########################################################
# creates FLQuant with ages in cell
setGeneric('ages', function(object, ...)
   standardGeneric('ages'))
setMethod("ages", signature(object="FLQuant"),
   function(object,timing=NULL){
      res<-FLQuant(dimnames(object)$age,dimnames=dimnames(object))

      if (is.null(timing))
         res<-sweep(res,4,(1:dim(res)[4]-1)/dim(res)[4],"+") else
         res<-sweep(res,4,timing,"+")

      return(res)})
################################################################################

# 2) length to weight ##########################################################
## converts wt to len using condition factor
setGeneric('len2wt', function(object, params, ...)
  standardGeneric('len2wt'))
setMethod("len2wt", signature(object="FLQuant", params="FLPar"),
   function(object,params) params["a"]*object^params["b"])
setMethod("len2wt", signature(object="FLCohort", params="FLPar"),
   function(object,params) params["a"]*object^params["b"])
setMethod("len2wt", signature(object="numeric", params="FLPar"),
   function(object,params) params["a"]*object^params["b"])
setMethod("len2wt", signature(object="FLQuant", params="numeric"),
   function(object,params) params["a"]*object^params["b"])
setMethod("len2wt", signature(object="FLCohort", params="numeric"),
   function(object,params) params["a"]*object^params["b"])
setMethod("len2wt", signature(object="numeric", params="numeric"),
   function(object,params) params["a"]*object^params["b"])
################################################################################

# 3) Weight to length ##########################################################
## converts len to wr using condition factor
setGeneric('wt2len', function(object, params, ...)
  standardGeneric('wt2len'))
setMethod("wt2len", signature(object="FLQuant", params="FLPar"),
   function(object,params) (object/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(object="FLCohort", params="FLPar"),
   function(object,params) (object/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(object="numeric", params="FLPar"),
   function(object,params) (object/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(object="FLQuant", params="numeric"),
   function(object,params) (object/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(object="FLCohort", params="numeric"),
   function(object,params) (object/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(object="numeric", params="numeric"),
   function(object,params) (object/params["a"])^(1/params["b"]))
################################################################################

# 4) Von Bertalanffy, length ###################################################
vonBInitial<-function(object,nAges=1:3){
  res<-c(coefficients(lm(age~data,data=object[object$age %in% unique(object$age)[nAges],]))[1],
         coefficients(lm(data~age,data=object[!(object$age %in% unique(object$age)[nAges]) &
                                                 !(object$age %in% rev(unique(object$age))[nAges]),]))[2],
         mean(object[object$age %in% rev(unique(object$age))[nAges],"data"]))

  return(FLPar(res,params=c("t0","K","Minf")))}

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
################################################################################

# 5) Von Bertalanffy, weigth ###################################################
setGeneric('vonBMass', function(object, params, ...)
  standardGeneric('vonBMass'))
setMethod("vonBMass", signature(object="FLQuant", params="FLPar"),
   function(object,params) params["Minf"]*(1.0-exp(-params["K"]*(object-params["t0"])))^params["a"])
setMethod("vonBMass", signature(object="FLCohort", params="FLPar"),
   function(object,params) params["Minf"]*(1.0-exp(-params["K"]*(object-params["t0"])))^params["a"])
setMethod("vonBMass", signature(object="numeric", params="FLPar"),
   function(object,params) params["Minf"]*(1.0-exp(-params["K"]*(object-params["t0"])))^params["a"])
setMethod("vonBMass", signature(object="FLQuant", params="missing"),
   function(object,Minf=NA,K=NA,t0=NA,a=NA,b=NA){
      params<-FLPar(Minf=Minf,K=K,t0=t0,a=a,b=b)
      params["Minf"]*(1.0-exp(-params["K"]*(object-params["t0"])))^params["a"]})
setMethod("vonBMass", signature(object="FLCohort", params="missing"),
   function(object,Minf=NA,K=NA,t0=NA,a=NA,b=NA){
      params<-FLPar(Minf=Minf,K=K,t0=t0,a=a,b=b)
      params["Minf"]*(1.0-exp(-params["K"]*(object-params["t0"])))^params["a"]})
setMethod("vonBMass", signature(object="numeric", params="missing"),
   function(object,Minf=NA,K=NA,t0=NA,a=NA,b=NA){
      params<-FLPar(Minf=Minf,K=K,t0=t0,a=a,b=b)
      params["Minf"]*(1.0-exp(-params["K"]*(object-params["t0"])))^params["a"]})
################################################################################

# 6) Gompertz ##################################################################
setGeneric('gompertz', function(object, params, ...)
  standardGeneric('gompertz'))
setMethod("gompertz", signature(object="numeric", params="FLPar"),
   function(object,params) params["Asym"]*exp(-params["b2"]*params["b3"]^object))
setMethod("gompertz", signature(object="FLQuant", params="FLPar"),
   function(object,params) params["Asym"]*exp(-params["b2"]*params["b3"]^object))
setMethod("gompertz", signature(object="FLCohort", params="FLPar"),
   function(object,params) params["Asym"]*exp(-params["b2"]*params["b3"]^object))
setMethod("gompertz", signature(object="FLQuant",params="missing"),
   function(object,Asym=NA,b2=NA,b3=NA) {
      params<-FLPar(Asym=Asym,b2=b2,b3=b3)
      params["Asym"]*exp(-params["b2"]*params["b3"]^object)})
setMethod("gompertz", signature(object="FLCohort",params="missing"),
   function(object,Asym=NA,b2=NA,b3=NA) {
      params<-FLPar(Asym=Asym,b2=b2,b3=b3)
      params["Asym"]*exp(-params["b2"]*params["b3"]^object)})
setMethod("gompertz", signature(object="numeric",params="missing"),
   function(object,Asym=NA,b2=NA,b3=NA) {
      params<-FLPar(Asym=Asym,b2=b2,b3=b3)
      params["Asym"]*exp(-params["b2"]*params["b3"]^object)})
################################################################################

# 7) Logistic ##################################################################
pow<-function(a,b) a^b

logisticFn<-function(x,a50,ato95,asym=1.0){
    #return(c/(1+19^((a-x)/b)))
  pow<-function(a,b) a^b

  func<-function(x,a50,ato95){
    if ((a50-x)/ato95 > 5)
      return(0)
    if ((a50-x)/ato95 < -5)
      return(asym)

    return(asym/(1.0+pow(19.0,(a50-x)/ato95)))}

  sapply(x,func,a50,ato95)}

setGeneric('logistic', function(object, params, ...)
  standardGeneric('logistic'))
setMethod("logistic", signature(object="numeric", params="FLPar"),
   function(object,params) logisticFn(object,params["a50"],params["ato95"],params["asym"]))
setMethod("logistic", signature(object="FLQuant", params="FLPar"),
   function(object,params) logisticFn(object,params["a50"],params["ato95"],params["asym"]))
setMethod("logistic", signature(object="FLCohort", params="FLPar"),
   function(object,params) logisticFn(object,params["a50"],params["ato95"],params["asym"]))
setMethod("logistic", signature(object="FLQuant",params="missing"),
   function(object,a50=NA,ato95=NA,asym=NA) {
      params<-FLPar(a50=a50,ato95=ato95,asym=asym)
   logisticFn(object,params["a50"],params["ato95"],params["asym"])})
setMethod("logistic", signature(object="FLCohort",params="missing"),
   function(object,a50=NA,ato95=NA,asym=NA) {
      params<-FLPar(a50=a50,ato95=ato95,asym=asym)
      logisticFn(object,params["a50"],params["ato95"],params["asym"])})
setMethod("logistic", signature(object="numeric",params="missing"),
   function(object,a50=NA,ato95=NA,asym=NA) {
      params<-FLPar(a50=a50,ato95=ato95,asym=asym)
      logisticFn(object,params["a50"],params["ato95"],params["asym"])})
################################################################################

# 8) Richards ##################################################################
richardsFn<-function(x, a50, ato95, beta){
  gamma <-ato95*log(19)/(log(2^beta-1)-log((20/19)^beta-1))
  alpha <-a50+gamma*log(2^beta-1)/log(19)

  (1/(1+19^(alpha-x)/beta))^1/beta}

setGeneric('Richards', function(object, params, ...)
  standardGeneric('Richards'))
setMethod("Richards", signature(object="numeric", params="FLPar"),
   function(object,params) richardsFn(object,params["a50"],params["ato95"],params["beta"]))
setMethod("Richards", signature(object="FLQuant", params="FLPar"),
   function(object,params) richardsFn(object,params["a50"],params["ato95"],params["beta"]))
setMethod("Richards", signature(object="FLCohort", params="FLPar"),
   function(object,params) richardsFn(object,params["a50"],params["ato95"],params["beta"]))
setMethod("Richards", signature(object="FLQuant",params="missing"),
   function(object,a50=NA,ato95=NA,beta=NA) {
      params<-FLPar(a50=a50,ato95=ato95,beta=beta)
   richardsFn(object,params["a50"],params["ato95"],params["beta"])})
setMethod("Richards", signature(object="FLCohort",params="missing"),
   function(object,a50=NA,ato95=NA,beta=NA) {
      params<-FLPar(a50=a50,ato95=ato95,beta=beta)
      richardsFn(object,params["a50"],params["ato95"],params["beta"])})
setMethod("Richards", signature(object="numeric",params="missing"),
   function(object,a50=NA,ato95=NA,beta=NA) {
      params<-FLPar(a50=a50,ato95=ato95,beta=beta)
      richardsFn(object,params["a50"],params["ato95"],params["beta"])})
################################################################################

# 9) Schnute ###################################################################
#### parameters
# y1: length at age t1
# y2: length at age t2
# t1: age at length y1
# t2: age at length y2


schnuteFn<-function(object,params){
  fn1<-function(object,params) (params["y1"]^params["b"]+(params["y2"]^params["b"]-params["y1"]^params["b"])*(1.0-exp(-params["a"]*(object-params["t1"])))/(1.0-exp(-params["a"]*(params["t2"]-params["t1"]))))^(-1/params["b"])
  fn2<-function(object,params)  params["y1"]*exp(log(params["y2"]/params["y1"])*(1.0-exp(-params["a"]*(object-params["t1"])))/(1.0-exp(-params["a"]*(params["t2"]-params["t1"]))))
  fn3<-function(object,params) (params["y1"]^params["b"]+(params["y2"]^params["b"]-params["y1"]^params["b"])*(object-params["t1"])/(params["t2"]-params["t1"]))^(-1/params["b"])
  fn4<-function(object,params)  params["y1"]*exp(log(params["y2"]/params["y1"])*(object-params["t1"])/(params["t2"]-params["t1"]))

  if (params["a"]!=0 & params["b"]!=0) return(fn1(object,params))
  if (params["a"]!=0 & params["b"]==0) return(fn2(object,params))
  if (params["a"]==0 & params["b"]!=0) return(fn3(object,params))
  if (params["a"]==0 & params["b"]==0) return(fn4(object,params))
  }

#### Growth
setGeneric('schnute', function(object, params, ...)
  standardGeneric('schnute'))
setMethod("schnute", signature(object="numeric", params="FLPar"),
   function(object,params) schnuteFn(object,params))
setMethod("schnute", signature(object="FLQuant", params="FLPar"),
   function(object,params) schnuteFn(object,params))
setMethod("schnute", signature(object="FLCohort", params="FLPar"),
   function(object,params) schnuteFn(object,params))
setMethod("schnute", signature(object="FLQuant",params="missing"),
   function(object,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA) {
      params<-FLPar(a=a,b=b,y1=y1,y2=y2,t1=t1,t2=t2)
      schnuteFn(object,params)})
setMethod("schnute", signature(object="FLCohort",params="missing"),
   function(object,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA) {
      params<-FLPar(a=a,b=b,y1=y1,y2=y2,t1=t1,t2=t2)
      schnuteFn(object,params)})
setMethod("schnute", signature(object="numeric",params="missing"),
   function(object,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA) {
      params<-FLPar(a=a,b=b,y1=y1,y2=y2,t1=t1,t2=t2)
      schnuteFn(object,params)})

setGeneric('schnuteMass', function(object, params, ...)
  standardGeneric('schnuteMass'))
setMethod("schnuteMass", signature(object="FLQuant", params="FLPar"),
   function(object,params) params["cf"]*schnute(object,params)^params["pow"])
setMethod("schnuteMass", signature(object="FLCohort", params="FLPar"),
   function(object,params) params["cf"]*schnute(object,params)^params["pow"])
setMethod("schnuteMass", signature(object="numeric", params="FLPar"),
   function(object,params) params["cf"]*schnute(object,params)^params["pow"])
setMethod("schnuteMass", signature(object="FLQuant", params="missing"),
   function(object,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return(params["cf"]*schnute(object,params)^params["pow"])})
setMethod("schnuteMass", signature(object="FLCohort", params="missing"),
   function(object,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return(params["cf"]*schnute(object,params)^params["pow"])})
setMethod("schnuteMass", signature(object="numeric", params="missing"),
   function(object,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return(params["cf"]*schnute(object,params)^params["pow"])})
################################################################################



