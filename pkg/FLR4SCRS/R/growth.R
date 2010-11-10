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

setMethod("len2wt", signature(params="numeric",data="FLQuant"),
   function(params,data) len2wt(FLPar(params),data))
setMethod("len2wt", signature(params="numeric", data="FLCohort"),
   function(params,data) len2wt(FLPar(params),data))
setMethod("len2wt", signature(params="numeric", data="numeric"),
   function(params,data) len2wt(FLPar(params),data))

setMethod("len2wt", signature(params="list",data="FLQuant"),
   function(params,data) len2wt(FLPar(unlist(params)),data))
setMethod("len2wt", signature(params="list", data="FLCohort"),
   function(params,data) len2wt(FLPar(unlist(params)),data))
setMethod("len2wt", signature(params="list", data="numeric"),
   function(params,data) len2wt(FLPar(unlist(params)),data))

setMethod("len2wt", signature(params="missing",data="FLQuant"),
   function(data,a=NA,b=NA) len2wt(FLPar(a=a,b=b),data))
setMethod("len2wt", signature(params="missing", data="FLCohort"),
   function(data,a=NA,b=NA) len2wt(FLPar(a=a,b=b),data))
setMethod("len2wt", signature(params="missing", data="numeric"),
   function(data,a=NA,b=NA) len2wt(FLPar(a=a,b=b),data))
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

setMethod("wt2len", signature(params="numeric",data="FLQuant"),
   function(params,data) wt2len(FLPar(params),data))
setMethod("wt2len", signature(params="numeric", data="FLCohort"),
   function(params,data) wt2len(FLPar(params),data))
setMethod("wt2len", signature(params="numeric", data="numeric"),
   function(params,data) wt2len(FLPar(params),data))

setMethod("wt2len", signature(params="list",data="FLQuant"),
   function(params,data) wt2len(FLPar(unlist(params)),data))
setMethod("wt2len", signature(params="list", data="FLCohort"),
   function(params,data) wt2len(FLPar(unlist(params)),data))
setMethod("wt2len", signature(params="list", data="numeric"),
   function(params,data) wt2len(FLPar(unlist(params)),data))

setMethod("wt2len", signature(params="missing",data="FLQuant"),
   function(data,a=NA,b=NA) wt2len(FLPar(a=a,b=b),data))
setMethod("wt2len", signature(params="missing", data="FLCohort"),
   function(data,a=NA,b=NA) wt2len(FLPar(a=a,b=b),data))
setMethod("wt2len", signature(params="missing", data="numeric"),
   function(data,a=NA,b=NA) wt2len(FLPar(a=a,b=b),data))
################################################################################

# 4) Von Bertalanffy, length ###################################################
iniVonB<-function(data,Minf,K,t0,a){
   params<-FLPar(Minf=Minf,K=K,t0=t0,a=a)
   plot(data$age,data$data)
   points(data$age,params["Minf"]*(1.0-exp(-params["K"]*(data$age-params["t0"])))^a,col="red",pch=17)

   params<-as.list(params)
   names(params)<-c("Minf","K","t0","a")
   return(params)}

setGeneric('vonB', function(params,data, ...)
  standardGeneric('vonB'))

setMethod("vonB", signature(params="FLPar", data="FLQuant"),
   function(params,data) params["Linf"]*(1.0-exp(-params["K"]*(data-params["t0"]))))
setMethod("vonB", signature(params="FLPar", data="FLCohort"),
   function(params,data) params["Linf"]*(1.0-exp(-params["K"]*(data-params["t0"]))))
setMethod("vonB", signature(params="FLPar", data="numeric"),
   function(params,data) params["Linf"]*(1.0-exp(-params["K"]*(data-params["t0"]))))

setMethod("vonB", signature(params="numeric",data="FLQuant"),
   function(params,data) vonB(FLPar(params),data))
setMethod("vonB", signature(params="numeric", data="FLCohort"),
   function(params,data) vonB(FLPar(params),data))
setMethod("vonB", signature(params="numeric", data="numeric"),
   function(params,data) vonB(FLPar(params),data))

setMethod("vonB", signature(params="list",data="FLQuant"),
   function(params,data) vonB(FLPar(unlist(params)),data))
setMethod("vonB", signature(params="list", data="FLCohort"),
   function(params,data) vonB(FLPar(unlist(params)),data))
setMethod("vonB", signature(params="list", data="numeric"),
   function(params,data) vonB(FLPar(unlist(params)),data))

setMethod("vonB", signature(params="missing",data="FLQuant"),
   function(data,Linf=NA,K=NA,t0=NA) vonB(FLPar(Linf=Linf,K=K,t0=t0),data))
setMethod("vonB", signature(params="missing", data="FLCohort"),
   function(data,Linf=NA,K=NA,t0=NA) vonB(FLPar(Linf=Linf,K=K,t0=t0),data))
setMethod("vonB", signature(params="missing", data="numeric"),
   function(data,Linf=NA,K=NA,t0=NA) vonB(FLPar(Linf=Linf,K=K,t0=t0),data))
################################################################################

# 5) Von Bertalanffy, mass #####################################################
setGeneric('vonBMass', function(params,data, ...)
  standardGeneric('vonBMass'))

setMethod("vonBMass", signature(params="FLPar", data="FLQuant"),
   function(params,data) params["Minf"]*(1.0-exp(-params["K"]*(data-params["t0"])))^params["a"])
setMethod("vonBMass", signature(params="FLPar", data="FLCohort"),
   function(params,data) params["Minf"]*(1.0-exp(-params["K"]*(data-params["t0"])))^params["a"])
setMethod("vonBMass", signature(params="FLPar", data="numeric"),
   function(params,data) params["Minf"]*(1.0-exp(-params["K"]*(data-params["t0"])))^params["a"])

setMethod("vonBMass", signature(params="missing",data="FLQuant"),
   function(data,Minf=NA,K=NA,t0=NA,a=NA) vonBMass(FLPar(Minf=Minf,K=K,t0=t0,a=a),data))
setMethod("vonBMass", signature(params="missing", data="FLCohort"),
   function(data,Minf=NA,K=NA,t0=NA,a=NA) vonBMass(FLPar(Minf=Minf,K=K,t0=t0,a=a),data))
setMethod("vonBMass", signature(params="missing", data="numeric"),
   function(data,Minf=NA,K=NA,t0=NA,a=NA) vonBMass(FLPar(Minf=Minf,K=K,t0=t0,a=a),data))

setMethod("vonBMass", signature(params="numeric",data="FLQuant"),
   function(params,data) vonBMass(FLPar(params),data))
setMethod("vonBMass", signature(params="numeric", data="FLCohort"),
   function(params,data) vonBMass(FLPar(params),data))
setMethod("vonBMass", signature(params="numeric", data="numeric"),
   function(params,data) vonBMass(FLPar(params),data))

setMethod("vonBMass", signature(params="list",data="FLQuant"),
   function(params,data) vonBMass(FLPar(unlist(params)),data))
setMethod("vonBMass", signature(params="list", data="FLCohort"),
   function(params,data) vonBMass(FLPar(unlist(params)),data))
setMethod("vonBMass", signature(params="list", data="numeric"),
   function(params,data) vonBMass(FLPar(unlist(params)),data))
################################################################################

# 6) Gompertz ##################################################################
iniGompertz<-function(data,asym,b2,b3){
   params<-FLPar(asym=asym,b2=b2,b3=b3)
   plot(data$age,data$data)
   points(data$age,gompertz(params,data$age),col="red",pch=17)

   return(params)}

setGeneric('gompertz', function(params, data, ...)
  standardGeneric('gompertz'))

setMethod("gompertz", signature(params="FLPar", data="numeric"),
   function(params,data) params["asym"]*exp(-params["b2"]*params["b3"]^data))
setMethod("gompertz", signature(params="FLPar", data="FLQuant"),
   function(params,data) params["asym"]*exp(-params["b2"]*params["b3"]^data))
setMethod("gompertz", signature(params="FLPar", data="FLCohort"),
   function(params,data) params["asym"]*exp(-params["b2"]*params["b3"]^data))

setMethod("gompertz", signature(params="numeric",data="FLQuant"),
   function(params,data) gompertz(FLPar(params),data))
setMethod("gompertz", signature(params="numeric", data="FLCohort"),
   function(params,data) gompertz(FLPar(params),data))
setMethod("gompertz", signature(params="numeric", data="numeric"),
   function(params,data) gompertz(FLPar(params),data))

setMethod("gompertz", signature(params="list",data="FLQuant"),
   function(params,data) gompertz(FLPar(unlist(params)),data))
setMethod("gompertz", signature(params="list", data="FLCohort"),
   function(params,data) gompertz(FLPar(unlist(params)),data))
setMethod("gompertz", signature(params="list", data="numeric"),
   function(params,data) gompertz(FLPar(unlist(params)),data))

setMethod("gompertz", signature(params="missing",data="FLQuant"),
   function(data,asym=NA,b2=NA,b3=NA) gompertz(FLPar(asym=asym,b2=b2,b3=b3),data))
setMethod("gompertz", signature(params="missing", data="FLCohort"),
   function(data,asym=NA,b2=NA,b3=NA) gompertz(FLPar(asym=asym,b2=b2,b3=b3),data))
setMethod("gompertz", signature(params="missing", data="numeric"),
   function(data,asym=NA,b2=NA,b3=NA) gompertz(FLPar(asym=asym,b2=b2,b3=b3),data))
################################################################################

# 7) Logistic ##################################################################
iniLog<-function(data,a50,ato95,asym){
   plot(  data$age,data$data)
   points(logisticFn(a50,ato95,asym,data$age),col="red",pch=17)

   return(c(a50=a50,ato95=ato95,asym=asym))}

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
   function(params,data) logisticFn(data,params["a50"],params["ato95"],params["asym"]))
setMethod("logistic", signature(params="FLPar", data="FLQuant"),
   function(params,data) logisticFn(data,params["a50"],params["ato95"],params["asym"]))
setMethod("logistic", signature(params="FLPar", data="FLCohort"),
   function(params,data) logisticFn(data,params["a50"],params["ato95"],params["asym"]))

setMethod("logistic", signature(params="numeric",data="FLQuant"),
   function(params,data) logistic(FLPar(params),data))
setMethod("logistic", signature(params="numeric", data="FLCohort"),
   function(params,data) logistic(FLPar(params),data))
setMethod("logistic", signature(params="numeric", data="numeric"),
   function(params,data) logistic(FLPar(params),data))

setMethod("logistic", signature(params="list",data="FLQuant"),
   function(params,data) logistic(FLPar(unlist(params)),data))
setMethod("logistic", signature(params="list", data="FLCohort"),
   function(params,data) logistic(FLPar(unlist(params)),data))
setMethod("logistic", signature(params="list", data="numeric"),
   function(params,data) logistic(FLPar(unlist(params)),data))

setMethod("logistic", signature(params="missing",data="FLQuant"),
   function(data,asym=NA,a50=NA,ato95=NA) logistic(FLPar(asym=asym,a50=a50,ato95=ato95),data))
setMethod("logistic", signature(params="missing", data="FLCohort"),
   function(data,asym=NA,a50=NA,ato95=NA) logistic(FLPar(asym=asym,a50=a50,ato95=ato95),data))
setMethod("logistic", signature(params="missing", data="numeric"),
   function(data,asym=NA,a50=NA,ato95=NA) logistic(FLPar(asym=asym,a50=a50,ato95=ato95),data))
################################################################################

# 8) Richards ##################################################################

iniRichards<-function(data,beta,a50,ato95){
   params<-FLPar(beta=beta,a50=a50,ato95=ato95)
   plot(data$age,data$data)
   points(data$age,richards(params,data$age),col="red",pch=17)

   return(params)}

richardsFn<-function(x, beta, a50, ato95){
  gamma <-ato95*log(19)/(log(2^beta-1)-log((20/19)^beta-1))
  alpha <-a50+gamma*log(2^beta-1)/log(19)

  (1/(1+19^(alpha-x)/beta))^1/beta}

setGeneric('richards', function(params,data, ...)
  standardGeneric('richards'))

setMethod("richards", signature(params="FLPar", data="numeric"),
   function(params,data) richardsFn(data,params["beta"],params["a50"],params["ato95"]))
setMethod("richards", signature(params="FLPar", data="FLQuant"),
   function(params,data) richardsFn(data,params["beta"],params["a50"],params["ato95"]))
setMethod("richards", signature(params="FLPar", data="FLCohort"),
   function(params,data) richardsFn(data,params["beta"],params["a50"],params["ato95"]))

setMethod("richards", signature(params="numeric",data="FLQuant"),
   function(params,data) richards(FLPar(params),data))
setMethod("richards", signature(params="numeric", data="FLCohort"),
   function(params,data) richards(FLPar(params),data))
setMethod("richards", signature(params="numeric", data="numeric"),
   function(params,data) richards(FLPar(params),data))

setMethod("richards", signature(params="list",data="FLQuant"),
   function(params,data) richards(FLPar(unlist(params)),data))
setMethod("richards", signature(params="list", data="FLCohort"),
   function(params,data) richards(FLPar(unlist(params)),data))
setMethod("richards", signature(params="list", data="numeric"),
   function(params,data) richards(FLPar(unlist(params)),data))

setMethod("richards", signature(params="missing",data="FLQuant"),
   function(data,beta=NA,a50=NA,ato95=NA) richards(FLPar(beta=beta,a50=a50,ato95=ato95),data))
setMethod("richards", signature(params="missing", data="FLCohort"),
   function(data,beta=NA,a50=NA,ato95=NA) richards(FLPar(beta=beta,a50=a50,ato95=ato95),data))
setMethod("richards", signature(params="missing", data="numeric"),
   function(data,beta=NA,a50=NA,ato95=NA) richards(FLPar(beta=beta,a50=a50,ato95=ato95),data))
################################################################################

# 9) Schnute ###################################################################
#### parameters
# y1: length at age t1
# y2: length at age t2
# t1: age at length y1
# t2: age at length y2


iniSchn<-function(data,a,b,t1,t2,y1,y2){
   plot(  data$age,data$data)
   points(schnute(data$age,FLPar(a=a,b=b,t1=t1,t2=t2,y1=y1,y2=y2)),col="red",pch=17)

   return(c(a=a,b=b,t1=t1,t2=t2,y1=y1,y2=y2))}

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



##### Inverse growth curve
#### Growth
setGeneric('invVonB', function(object, params, ...)
  standardGeneric('invVonB'))
setMethod("invVonB", signature(object="numeric", params="FLPar"),
   function(object,params) params["t0"]-log(1.0-object/params["Linf"])/params["K"])
setMethod("invVonB", signature(object="FLQuant", params="FLPar"),
   function(object,params) params["t0"]-log(1.0-object/params["Linf"])/params["K"])
setMethod("invVonB", signature(object="FLCohort", params="FLPar"),
   function(object,params) params["t0"]-log(1.0-object/params["Linf"])/params["K"])
setMethod("invVonB", signature(object="FLQuant",params="missing"),
   function(object,Linf=NA,K=NA,t0=NA) {
      params<-FLPar(Linf=Linf,K=K,t0=t0)
      params["t0"]-log(1.0-object/params["Linf"])/params["K"]})
setMethod("invVonB", signature(object="FLCohort",params="missing"),
   function(object,Linf=NA,K=NA,t0=NA) {
      params<-FLPar(Linf=Linf,K=K,t0=t0)
      params["t0"]-log(1.0-object/params["Linf"])/params["K"]})
setMethod("invVonB", signature(object="numeric",params="missing"),
   function(object,Linf=NA,K=NA,t0=NA) {
      params<-FLPar(Linf=Linf,K=K,t0=t0)
      params["t0"]-log(1.0-object/params["Linf"])/params["K"]})

setGeneric('invVonBMass', function(object, params, ...)
  standardGeneric('invVonBMass'))
setMethod("invVonBMass", signature(object="FLQuant", params="FLPar"),
   function(object,params) invVonB((object/params["a"])^(1.0/params["b"]),params))
setMethod("invVonBMass", signature(object="FLCohort", params="FLPar"),
   function(object,params) invVonB((object/params["a"])^(1.0/params["b"]),params))
setMethod("invVonBMass", signature(object="numeric", params="FLPar"),
   function(object,params) invVonB(c((object/params["a"])^(1.0/params["b"])),params))
setMethod("invVonBMass", signature(object="FLQuant", params="missing"),
   function(object,Linf=NA,K=NA,t0=NA,a=NA,b=NA){
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return((object/params["a"])^(1.0/params["b"]))})
setMethod("invVonBMass", signature(object="FLCohort", params="missing"),
   function(object,Linf=NA,K=NA,t0=NA,a=NA,b=NA){
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return((object/params["a"])^(1.0/params["b"]))})
setMethod("invVonBMass", signature(object="numeric", params="missing"),
   function(object,Linf=NA,K=NA,t0=NA,a=NA,b=NA){
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return(invVonB(c(object/params["a"])^(1.0/params["b"]),params))})

#      return(params["a"]*invVonB(object,params)^params["b"])})

