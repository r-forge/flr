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

iniVonB<-function(data,Minf,K,t0,a){
   params<-FLPar(Minf=Minf,K=K,t0=t0,a=a)
   plot(data$age,data$data)
   points(data$age,params["Minf"]*(1.0-exp(-params["K"]*(data$age-params["t0"])))^a,col="red",pch=17)

   params<-as.list(params)
   names(params)<-c("Minf","K","t0","a")
   return(params)}

iniLog<-function(data,a50,ato95,asym){
   plot(  data$age,data$data)
   points(logisticFn(a50,ato95,asym,data$age),col="red",pch=17)

   return(c(a50=a50,ato95=ato95,asym=asym))}

iniRich<-function(data,a50,ato95,beta){
   plot(  data$age,data$data)
   points(logisticFn(a50,ato95,beta,data$age),col="red",pch=17)

   return(c(a50=a50,ato95=ato95,beta=beta))}

iniSchn<-function(data,a,b,t1,t2,y1,y2){
   plot(  data$age,data$data)
   points(schnute(data$age,FLPar(a=a,b=b,t1=t1,t2=t2,y1=y1,y2=y2)),col="red",pch=17)

   return(c(a=a,b=b,t1=t1,t2=t2,y1=y1,y2=y2))}


# 1) FLQuant to ages ###########################################################
# creates FLQuant with ages in cell
setGeneric('ages', function(data, ...)
   standardGeneric('ages'))
setMethod("ages", signature(data="FLQuant"),
   function(data,timing=NULL){
      res<-FLQuant(dimnames(data)$age,dimnames=dimnames(data))

      if (is.null(timing))
         res<-sweep(res,4,(1:dim(res)[4]-1)/dim(res)[4],"+") else
         res<-sweep(res,4,timing,"+")

      return(res)})
################################################################################

# 2) length to weight ##########################################################
## converts wt to len using condition factor
setGeneric('len2wt', function(data, params, ...)
  standardGeneric('len2wt'))
setMethod("len2wt", signature(params="FLPar", data="FLQuant"),
   function(params,data) params["a"]*data^params["b"])
setMethod("len2wt", signature(params="FLPar", data="FLCohort"),
   function(params,data) params["a"]*data^params["b"])
setMethod("len2wt", signature(params="FLPar", data="numeric"),
   function(params,data) params["a"]*data^params["b"])
setMethod("len2wt", signature(data="FLQuant", params="numeric"),
   function(params,data) params["a"]*data^params["b"])
setMethod("len2wt", signature(data="FLCohort", params="numeric"),
   function(params,data) params["a"]*data^params["b"])
setMethod("len2wt", signature(data="numeric", params="numeric"),
   function(params,data) params["a"]*data^params["b"])
################################################################################

# 3) Weight to length ##########################################################
## converts len to wr using condition factor
setGeneric('wt2len', function(data, params, ...)
  standardGeneric('wt2len'))
setMethod("wt2len", signature(params="FLPar", data="FLQuant"),
   function(params,data) (data/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(params="FLPar", data="FLCohort"),
   function(params,data) (data/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(params="FLPar", data="numeric"),
   function(params,data) (data/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(data="FLQuant", params="numeric"),
   function(params,data) (data/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(data="FLCohort", params="numeric"),
   function(params,data) (data/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(data="numeric", params="numeric"),
   function(params,data) (data/params["a"])^(1/params["b"]))
################################################################################

# 4) Von Bertalanffy, length ###################################################
vonBInitial<-function(data,nAges=1:3){
  res<-c(coefficients(lm(age~data,data=data[data$age %in% unique(data$age)[nAges],]))[1],
         coefficients(lm(data~age,data=data[!(data$age %in% unique(data$age)[nAges]) &
                                                 !(data$age %in% rev(unique(data$age))[nAges]),]))[2],
         mean(data[data$age %in% rev(unique(data$age))[nAges],"data"]))

  return(FLPar(res,params=c("t0","K","Minf")))}

setGeneric('vonB', function(data, params, ...)
  standardGeneric('vonB'))
setMethod("vonB", signature(params="FLPar", data="numeric"),
   function(params,data) params["Linf"]*(1.0-exp(-params["K"]*(data-params["t0"]))))
setMethod("vonB", signature(params="FLPar", data="FLQuant"),
   function(params,data) params["Linf"]*(1.0-exp(-params["K"]*(data-params["t0"]))))
setMethod("vonB", signature(params="FLPar", data="FLCohort"),
   function(params,data) params["Linf"]*(1.0-exp(-params["K"]*(data-params["t0"]))))
setMethod("vonB", signature(data="FLQuant",params="missing"),
   function(data,Linf=NA,K=NA,t0=NA) {
      params<-FLPar(Linf=Linf,K=K,t0=t0)
      params["Linf"]*(1.0-exp(-params["K"]*(data-params["t0"])))})
setMethod("vonB", signature(data="FLCohort",params="missing"),
   function(data,Linf=NA,K=NA,t0=NA) {
      params<-FLPar(Linf=Linf,K=K,t0=t0)
      params["Linf"]*(1.0-exp(-params["K"]*(data-params["t0"])))})
setMethod("vonB", signature(data="numeric",params="missing"),
   function(data,Linf=NA,K=NA,t0=NA) {
      params<-FLPar(Linf=Linf,K=K,t0=t0)
      params["Linf"]*(1.0-exp(-params["K"]*(data-params["t0"])))})
################################################################################

# 5) Von Bertalanffy, weigth ###################################################
setGeneric('vonBMass', function(data, params, ...)
  standardGeneric('vonBMass'))

setMethod("vonBMass", signature(params="FLPar", data="FLQuant"),
   function(params,data) params["Minf"]*(1.0-exp(-params["K"]*(data-params["t0"])))^params["a"])
setMethod("vonBMass", signature(params="FLPar", data="FLCohort"),
   function(params,data) params["Minf"]*(1.0-exp(-params["K"]*(data-params["t0"])))^params["a"])
setMethod("vonBMass", signature(params="FLPar", data="numeric"),
   function(params,data) params["Minf"]*(1.0-exp(-params["K"]*(data-params["t0"])))^params["a"])

setMethod("vonBMass", signature(params="missing",data="FLQuant"),
   function(data,Minf=NA,K=NA,t0=NA,a=NA,b=NA) vonBMass(FLPar(Minf=Minf,K=K,t0=t0,a=a),data))
setMethod("vonBMass", signature(params="missing", data="FLCohort"),
   function(data,Minf=NA,K=NA,t0=NA,a=NA,b=NA) vonBMass(FLPar(Minf=Minf,K=K,t0=t0,a=a),data))
setMethod("vonBMass", signature(params="missing", data="numeric"),
   function(data,Minf=NA,K=NA,t0=NA,a=NA,b=NA) vonBMass(FLPar(Minf=Minf,K=K,t0=t0,a=a),data))

setMethod("vonBMass", signature(params="numeric",data="FLQuant"),
   function(params,data) vonBMass(FLPar(params),data))
setMethod("vonBMass", signature(params="numeric", data="FLCohort"),
   function(params,data) vonBMass(FLPar(params),data))
setMethod("vonBMass", signature(params="numeric", data="numeric"),
   function(params,data) vonBMass(FLPar(params),data))

setMethod("vonBMass", signature(params="numeric",data="FLQuant"),
   function(params,data) vonBMass(FLPar(unlist(params)),data))
setMethod("vonBMass", signature(params="numeric", data="FLCohort"),
   function(params,data) vonBMass(FLPar(unlist(params)),data))
setMethod("vonBMass", signature(params="numeric", data="numeric"),
   function(params,data) vonBMass(FLPar(unlist(params)),data))
################################################################################

# 6) Gompertz ##################################################################
setGeneric('gompertz', function(data, params, ...)
  standardGeneric('gompertz'))
setMethod("gompertz", signature(params="FLPar", data="numeric"),
   function(params,data) params["Asym"]*exp(-params["b2"]*params["b3"]^data))
setMethod("gompertz", signature(params="FLPar", data="FLQuant"),
   function(params,data) params["Asym"]*exp(-params["b2"]*params["b3"]^data))
setMethod("gompertz", signature(params="FLPar", data="FLCohort"),
   function(params,data) params["Asym"]*exp(-params["b2"]*params["b3"]^data))
setMethod("gompertz", signature(data="FLQuant",params="missing"),
   function(data,Asym=NA,b2=NA,b3=NA) {
      params<-FLPar(Asym=Asym,b2=b2,b3=b3)
      params["Asym"]*exp(-params["b2"]*params["b3"]^data)})
setMethod("gompertz", signature(data="FLCohort",params="missing"),
   function(data,Asym=NA,b2=NA,b3=NA) {
      params<-FLPar(Asym=Asym,b2=b2,b3=b3)
      params["Asym"]*exp(-params["b2"]*params["b3"]^data)})
setMethod("gompertz", signature(data="numeric",params="missing"),
   function(data,Asym=NA,b2=NA,b3=NA) {
      params<-FLPar(Asym=Asym,b2=b2,b3=b3)
      params["Asym"]*exp(-params["b2"]*params["b3"]^data)})
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

setGeneric('logistic', function(data, params, ...)
  standardGeneric('logistic'))
setMethod("logistic", signature(params="FLPar", data="numeric"),
   function(params,data) logisticFn(params,data["a50"],params["ato95"],params["asym"]))
setMethod("logistic", signature(params="FLPar", data="FLQuant"),
   function(params,data) logisticFn(params,data["a50"],params["ato95"],params["asym"]))
setMethod("logistic", signature(params="FLPar", data="FLCohort"),
   function(params,data) logisticFn(params,data["a50"],params["ato95"],params["asym"]))
setMethod("logistic", signature(data="FLQuant",params="missing"),
   function(data,a50=NA,ato95=NA,asym=NA) {
      params<-FLPar(a50=a50,ato95=ato95,asym=asym)
   logisticFn(params,data["a50"],params["ato95"],params["asym"])})
setMethod("logistic", signature(data="FLCohort",params="missing"),
   function(data,a50=NA,ato95=NA,asym=NA) {
      params<-FLPar(a50=a50,ato95=ato95,asym=asym)
      logisticFn(params,data["a50"],params["ato95"],params["asym"])})
setMethod("logistic", signature(data="numeric",params="missing"),
   function(data,a50=NA,ato95=NA,asym=NA) {
      params<-FLPar(a50=a50,ato95=ato95,asym=asym)
      logisticFn(params,data["a50"],params["ato95"],params["asym"])})
################################################################################

# 8) Richards ##################################################################
richardsFn<-function(x, a50, ato95, beta){
  gamma <-ato95*log(19)/(log(2^beta-1)-log((20/19)^beta-1))
  alpha <-a50+gamma*log(2^beta-1)/log(19)

  (1/(1+19^(alpha-x)/beta))^1/beta}

setGeneric('Richards', function(data, params, ...)
  standardGeneric('Richards'))
setMethod("Richards", signature(params="FLPar", data="numeric"),
   function(params,data) richardsFn(params,data["a50"],params["ato95"],params["beta"]))
setMethod("Richards", signature(params="FLPar", data="FLQuant"),
   function(params,data) richardsFn(params,data["a50"],params["ato95"],params["beta"]))
setMethod("Richards", signature(params="FLPar", data="FLCohort"),
   function(params,data) richardsFn(params,data["a50"],params["ato95"],params["beta"]))
setMethod("Richards", signature(data="FLQuant",params="missing"),
   function(data,a50=NA,ato95=NA,beta=NA) {
      params<-FLPar(a50=a50,ato95=ato95,beta=beta)
   richardsFn(params,data["a50"],params["ato95"],params["beta"])})
setMethod("Richards", signature(data="FLCohort",params="missing"),
   function(data,a50=NA,ato95=NA,beta=NA) {
      params<-FLPar(a50=a50,ato95=ato95,beta=beta)
      richardsFn(params,data["a50"],params["ato95"],params["beta"])})
setMethod("Richards", signature(data="numeric",params="missing"),
   function(data,a50=NA,ato95=NA,beta=NA) {
      params<-FLPar(a50=a50,ato95=ato95,beta=beta)
      richardsFn(params,data["a50"],params["ato95"],params["beta"])})
################################################################################

# 9) Schnute ###################################################################
#### parameters
# y1: length at age t1
# y2: length at age t2
# t1: age at length y1
# t2: age at length y2


schnuteFn<-function(params,data){
  fn1<-function(params,data) (params["y1"]^params["b"]+(params["y2"]^params["b"]-params["y1"]^params["b"])*(1.0-exp(-params["a"]*(data-params["t1"])))/(1.0-exp(-params["a"]*(params["t2"]-params["t1"]))))^(-1/params["b"])
  fn2<-function(params,data)  params["y1"]*exp(log(params["y2"]/params["y1"])*(1.0-exp(-params["a"]*(data-params["t1"])))/(1.0-exp(-params["a"]*(params["t2"]-params["t1"]))))
  fn3<-function(params,data) (params["y1"]^params["b"]+(params["y2"]^params["b"]-params["y1"]^params["b"])*(data-params["t1"])/(params["t2"]-params["t1"]))^(-1/params["b"])
  fn4<-function(params,data)  params["y1"]*exp(log(params["y2"]/params["y1"])*(data-params["t1"])/(params["t2"]-params["t1"]))

  if (params["a"]!=0 & params["b"]!=0) return(fn1(params,data))
  if (params["a"]!=0 & params["b"]==0) return(fn2(params,data))
  if (params["a"]==0 & params["b"]!=0) return(fn3(params,data))
  if (params["a"]==0 & params["b"]==0) return(fn4(params,data))
  }

#### Growth
setGeneric('schnute', function(data, params, ...)
  standardGeneric('schnute'))
setMethod("schnute", signature(params="FLPar", data="numeric"),
   function(params,data) schnuteFn(params,data))
setMethod("schnute", signature(params="FLPar", data="FLQuant"),
   function(params,data) schnuteFn(params,data))
setMethod("schnute", signature(params="FLPar", data="FLCohort"),
   function(params,data) schnuteFn(params,data))
setMethod("schnute", signature(data="FLQuant",params="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA) {
      params<-FLPar(a=a,b=b,y1=y1,y2=y2,t1=t1,t2=t2)
      schnuteFn(params,data)})
setMethod("schnute", signature(data="FLCohort",params="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA) {
      params<-FLPar(a=a,b=b,y1=y1,y2=y2,t1=t1,t2=t2)
      schnuteFn(params,data)})
setMethod("schnute", signature(data="numeric",params="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA) {
      params<-FLPar(a=a,b=b,y1=y1,y2=y2,t1=t1,t2=t2)
      schnuteFn(params,data)})

setGeneric('schnuteMass', function(data, params, ...)
  standardGeneric('schnuteMass'))
setMethod("schnuteMass", signature(params="FLPar", data="FLQuant"),
   function(params,data) params["cf"]*schnute(params,data)^params["pow"])
setMethod("schnuteMass", signature(params="FLPar", data="FLCohort"),
   function(params,data) params["cf"]*schnute(params,data)^params["pow"])
setMethod("schnuteMass", signature(params="FLPar", data="numeric"),
   function(params,data) params["cf"]*schnute(params,data)^params["pow"])
setMethod("schnuteMass", signature(data="FLQuant", params="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return(params["cf"]*schnute(params,data)^params["pow"])})
setMethod("schnuteMass", signature(data="FLCohort", params="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return(params["cf"]*schnute(params,data)^params["pow"])})
setMethod("schnuteMass", signature(data="numeric", params="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return(params["cf"]*schnute(params,data)^params["pow"])})
################################################################################



