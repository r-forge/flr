################################################################################

#### Growth functions ##########################################################
# 1)  Add parameters                                                           #
# 2)  FLQuant to ages                                                          #
# 3)  length to weight                                                         #
# 4)  Weight to length                                                         #
# 5)  Von Bertalanffy                                                          #
# 6)  Inverse of Von Bertalanffy                                               #
# 7)  Gompertz                                                                 #
# 8)  Logistic                                                                 #
# 9)  Richards                                                                 #
# 10) Schnute                                                                  #
################################################################################

# 1) ###########################################################################                    dd
addPar<-function(par1,nm,val){
    dmns       <-dimnames(par1)
    dmns$params<-c(dmns$params,nm)
    par2       <-FLPar(array(NA,unlist(lapply(dmns,length)),dimnames=dmns))

    par2[dimnames(par1)$params]<-par1
    par2[nm]                   <-val

    return(par2)}
    
################################################################################

# 2) FLQuant to ages ###########################################################
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

# 3) length to weight ##########################################################
## converts wt to len using condition factor
setGeneric('len2wt', function(params,data,...)
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

# 4) Weight to length ##########################################################
## converts len to wr using condition factor
setGeneric('wt2len', function(params,data, ...)
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

# 5) Von Bertalanffy, length ###################################################
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
   function(params,data) {
         if (!("a" %in% dimnames(params)$params)) params<-addPar(params,"a",1)
         print(params)
         params["Sinf"]*(1.0-exp(-params["K"]*(data-params["t0"])))^params["a"]})
setMethod("vonB", signature(params="FLPar", data="FLCohort"),
   function(params,data) {
         if (!("a" %in% dimnames(params)$params)) params<-addPar(params,"a",1)
         params["Sinf"]*(1.0-exp(-params["K"]*(data-params["t0"])))^params["a"]})
setMethod("vonB", signature(params="FLPar", data="numeric"),
   function(params,data) {
         if (!("a" %in% dimnames(params)$params)) params<-addPar(params,"a",1)
         params["Sinf"]*(1.0-exp(-params["K"]*(data-params["t0"])))^params["a"]})

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
   function(data,Sinf=NA,K=NA,t0=NA,a=1) vonB(FLPar(Sinf=Sinf,K=K,t0=t0,a=a),data))
setMethod("vonB", signature(params="missing", data="FLCohort"),
   function(data,Sinf=NA,K=NA,t0=NA,a=1) vonB(FLPar(Sinf=Sinf,K=K,t0=t0,a=a),data))
setMethod("vonB", signature(params="missing", data="numeric"),
   function(data,Sinf=NA,K=NA,t0=NA,a=1) vonB(FLPar(Sinf=Sinf,K=K,t0=t0,a=a),data))
################################################################################

# 6) Von Bertalanffy, inverse ##################################################
setGeneric('inverseVonB', function(params,data, ...)
  standardGeneric('inverseVonB'))

setMethod("inverseVonB", signature(params="FLPar", data="FLQuant"),
   function(params,data) {
       if (!("a" %in% dimnames(params)$params)) params<-addPar(params,"a",1)
       params["t0"]-log(1.0-(object/params["Sinf"])^(1/params["a"]))/params["K"]})
setMethod("inverseVonB", signature(params="FLPar", data="FLCohort"),
   function(params,data) {
       if (!("a" %in% dimnames(params)$params)) params<-addPar(params,"a",1)
       params["t0"]-log(1.0-(object/params["Sinf"])^(1/params["a"]))/params["K"]})
setMethod("inverseVonB", signature(params="FLPar", data="numeric"),
   function(params,data) {
       if (!("a" %in% dimnames(params)$params)) params<-addPar(params,"a",1)
       params["t0"]-log(1.0-(object/params["Sinf"])^(1/params["a"]))/params["K"]})

setMethod("inverseVonB", signature(params="missing",data="FLQuant"),
   function(data,Sinf=NA,K=NA,t0=NA,a=NA) inverseVonB(FLPar(Sinf=Sinf,K=K,t0=t0,a=a),data))
setMethod("inverseVonB", signature(params="missing", data="FLCohort"),
   function(data,Sinf=NA,K=NA,t0=NA,a=NA) inverseVonB(FLPar(Sinf=Sinf,K=K,t0=t0,a=a),data))
setMethod("inverseVonB", signature(params="missing", data="numeric"),
   function(data,Sinf=NA,K=NA,t0=NA,a=NA) inverseVonB(FLPar(Sinf=Sinf,K=K,t0=t0,a=a),data))

setMethod("inverseVonB", signature(params="numeric",data="FLQuant"),
   function(params,data) inverseVonB(FLPar(params),data))
setMethod("inverseVonB", signature(params="numeric", data="FLCohort"),
   function(params,data) inverseVonB(FLPar(params),data))
setMethod("inverseVonB", signature(params="numeric", data="numeric"),
   function(params,data) inverseVonB(FLPar(params),data))

setMethod("inverseVonB", signature(params="list",data="FLQuant"),
   function(params,data) inverseVonB(FLPar(unlist(params)),data))
setMethod("inverseVonB", signature(params="list", data="FLCohort"),
   function(params,data) inverseVonB(FLPar(unlist(params)),data))
setMethod("inverseVonB", signature(params="list", data="numeric"),
   function(params,data) inverseVonB(FLPar(unlist(params)),data))
################################################################################

# 7) Gompertz ##################################################################
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

# 8) Logistic ##################################################################
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

setGeneric('logistic', function(params,data, ...)
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

# 9) Richards ##################################################################

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

# 10) Schnute ###################################################################
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
  if (params["a"]==0 & params["b"]==0) return(fn4(params,data))}

#### Growth
setGeneric('schnute', function(params,data, ...)
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

setGeneric('schnuteMass', function(params,data, ...)
  standardGeneric('schnuteMass'))
setMethod("schnuteMass", signature(params="FLPar", data="FLQuant"),
   function(params,data) params["cf"]*schnute(params,data)^params["pow"])
setMethod("schnuteMass", signature(params="FLPar", data="FLCohort"),
   function(params,data) params["cf"]*schnute(params,data)^params["pow"])
setMethod("schnuteMass", signature(params="FLPar", data="numeric"),
   function(params,data) params["cf"]*schnute(params,data)^params["pow"])
setMethod("schnuteMass", signature(data="FLQuant", params="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      params<-FLPar(Sinf=Sinf,K=K,t0=t0,a=a,b=b)

      return(params["cf"]*schnute(params,data)^params["pow"])})
setMethod("schnuteMass", signature(data="FLCohort", params="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      params<-FLPar(Sinf=Sinf,K=K,t0=t0,a=a,b=b)

      return(params["cf"]*schnute(params,data)^params["pow"])})
setMethod("schnuteMass", signature(data="numeric", params="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      params<-FLPar(Sinf=Sinf,K=K,t0=t0,a=a,b=b)

      return(params["cf"]*schnute(params,data)^params["pow"])})


################################################################################
# Sinf:    Infinite size, i.e. assymptotic size reached after an infinite period of growth
# K:       Constant describing "how fast" the animal grows
# to:      Age of the animal at size zero
# tprime;  Age at inflexion point

#if (a!=0 & b!=0)

