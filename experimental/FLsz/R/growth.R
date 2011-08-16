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
# 10) Double Normal                                                            #
# 11) M                                                                        #
# 12) mat50                                                                    #
# 13) Schnute                                                                  #
# 14) Density Dependence                                                       #
################################################################################

# 1) ###########################################################################
addPar<-function(par1,nm,val){
    dmns       <-dimnames(par1)
    dmns$param<-c(dmns$param,nm)
    par2       <-FLPar(array(NA,unlist(lapply(dmns,length)),dimnames=dmns))

    par2[dimnames(par1)$param]<-par1
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
setGeneric('len2wt', function(param,data,...)
  standardGeneric('len2wt'))
  
setMethod("len2wt", signature(param="FLPar", data="FLQuant"),
   function(param,data) param["a"]*data^param["b"])
setMethod("len2wt", signature(param="FLPar", data="FLCohort"),
   function(param,data) param["a"]*data^param["b"])
setMethod("len2wt", signature(param="FLPar", data="numeric"),
   function(param,data) param["a"]*data^param["b"])

setMethod("len2wt", signature(param="numeric",data="FLQuant"),
   function(param,data) len2wt(FLPar(param),data))
setMethod("len2wt", signature(param="numeric", data="FLCohort"),
   function(param,data) len2wt(FLPar(param),data))
setMethod("len2wt", signature(param="numeric", data="numeric"),
   function(param,data) len2wt(FLPar(param),data))

setMethod("len2wt", signature(param="list",data="FLQuant"),
   function(param,data) len2wt(FLPar(unlist(param)),data))
setMethod("len2wt", signature(param="list", data="FLCohort"),
   function(param,data) len2wt(FLPar(unlist(param)),data))
setMethod("len2wt", signature(param="list", data="numeric"),
   function(param,data) len2wt(FLPar(unlist(param)),data))

setMethod("len2wt", signature(param="missing",data="FLQuant"),
   function(data,a=NA,b=NA) len2wt(FLPar(a=a,b=b),data))
setMethod("len2wt", signature(param="missing", data="FLCohort"),
   function(data,a=NA,b=NA) len2wt(FLPar(a=a,b=b),data))
setMethod("len2wt", signature(param="missing", data="numeric"),
   function(data,a=NA,b=NA) len2wt(FLPar(a=a,b=b),data))
################################################################################

# 4) Weight to length ##########################################################
## converts len to wr using condition factor
setGeneric('wt2len', function(param,data, ...)
  standardGeneric('wt2len'))

setMethod("wt2len", signature(param="FLPar", data="FLQuant"),
   function(param,data) (data/param["a"])^(1/param["b"]))
setMethod("wt2len", signature(param="FLPar", data="FLCohort"),
   function(param,data) (data/param["a"])^(1/param["b"]))
setMethod("wt2len", signature(param="FLPar", data="numeric"),
   function(param,data) (data/param["a"])^(1/param["b"]))

setMethod("wt2len", signature(param="numeric",data="FLQuant"),
   function(param,data) wt2len(FLPar(param),data))
setMethod("wt2len", signature(param="numeric", data="FLCohort"),
   function(param,data) wt2len(FLPar(param),data))
setMethod("wt2len", signature(param="numeric", data="numeric"),
   function(param,data) wt2len(FLPar(param),data))

setMethod("wt2len", signature(param="list",data="FLQuant"),
   function(param,data) wt2len(FLPar(unlist(param)),data))
setMethod("wt2len", signature(param="list", data="FLCohort"),
   function(param,data) wt2len(FLPar(unlist(param)),data))
setMethod("wt2len", signature(param="list", data="numeric"),
   function(param,data) wt2len(FLPar(unlist(param)),data))

setMethod("wt2len", signature(param="missing",data="FLQuant"),
   function(data,a=NA,b=NA) wt2len(FLPar(a=a,b=b),data))
setMethod("wt2len", signature(param="missing", data="FLCohort"),
   function(data,a=NA,b=NA) wt2len(FLPar(a=a,b=b),dta))
setMethod("wt2len", signature(param="missing", data="numeric"),
   function(data,a=NA,b=NA) wt2len(FLPar(a=a,b=b),data))
################################################################################

setGeneric('vonB', function(param,data, ...)
  standardGeneric('vonB'))

vonBparam<-function(param){
         dimnames(param)$param<-tolower(dimnames(param)$param)
         if (!("b"  %in% dimnames(param)$param)) param<-addPar(param,"b" ,1)
         if (!("t0" %in% dimnames(param)$param)) param<-addPar(param,"t0",0)
         dimnames(param)$param[substr(dimnames(param)$param,2,nchar(dimnames(param)$param)) %in% "inf"]<-"sinf"

         return(param)}
      
setMethod("vonB", signature(param="FLPar", data="FLQuant"),
   function(param,data) {
         param<-vonBparam(param)  
         param["sinf"]*(1.0-exp(-param["k"]*(data-param["t0"])))^param["b"]})
setMethod("vonB", signature(param="FLPar", data="FLCohort"),
   function(param,data) {
         param<-vonBparam(param)  
         param["sinf"]*(1.0-exp(-param["k"]*(data-param["t0"])))^param["b"]})
setMethod("vonB", signature(param="FLPar", data="numeric"),
   function(param,data) {
         param<-vonBparam(param)  
         param["sinf"]*(1.0-exp(-param["k"]*(data-param["t0"])))^param["b"]})

setMethod("vonB", signature(param="numeric",data="FLQuant"),
   function(param,data) vonB(FLPar(param),data))
setMethod("vonB", signature(param="numeric", data="FLCohort"),
   function(param,data) vonB(FLPar(param),data))
setMethod("vonB", signature(param="numeric", data="numeric"),
   function(param,data) vonB(FLPar(param),data))

setMethod("vonB", signature(param="list",data="FLQuant"),
   function(param,data) vonB(FLPar(unlist(param)),data))
setMethod("vonB", signature(param="list", data="FLCohort"),
   function(param,data) vonB(FLPar(unlist(param)),data))
setMethod("vonB", signature(param="list", data="numeric"),
   function(param,data) vonB(FLPar(unlist(param)),data))

setMethod("vonB", signature(param="missing",data="FLQuant"),
   function(data,sinf=NA,K=NA,t0=NA,a=1) vonB(FLPar(sinf=sinf,K=K,t0=t0,a=a),data))
setMethod("vonB", signature(param="missing", data="FLCohort"),
   function(data,sinf=NA,K=NA,t0=NA,a=1) vonB(FLPar(sinf=sinf,K=K,t0=t0,a=a),data))
setMethod("vonB", signature(param="missing", data="numeric"),
   function(data,sinf=NA,K=NA,t0=NA,a=1) vonB(FLPar(sinf=sinf,K=K,t0=t0,a=a),data))
################################################################################

# 6) Von Bertalanffy, inv ##################################################
setGeneric('invVonB', function(param,data, ...)
  standardGeneric('invVonB'))

setMethod("invVonB", signature(param="FLPar", data="FLQuant"),
   function(param,data) {
	param<-vonBparam(param)  
        -log(1.0-(data/param["sinf"])^(1/param["b"]))/param["k"]+param["t0"]})
setMethod("invVonB", signature(param="FLPar", data="FLCohort"),
   function(param,data) {
        param<-vonBparam(param)  
        -log(1.0-(data/param["sinf"])^(1/param["b"]))/param["k"]+param["t0"]})
setMethod("invVonB", signature(param="FLPar", data="numeric"),
   function(param,data) {
       param<-vonBparam(param)  
        -log(1.0-(data/param["sinf"])^(1/param["b"]))/param["k"]+param["t0"]})

setMethod("invVonB", signature(param="missing",data="FLQuant"),
   function(data,sinf=NA,K=NA,t0=NA,a=NA) invVonB(FLPar(sinf=sinf,K=K,t0=t0,a=a),data))
setMethod("invVonB", signature(param="missing", data="FLCohort"),
   function(data,sinf=NA,K=NA,t0=NA,a=NA) invVonB(FLPar(sinf=sinf,K=K,t0=t0,a=a),data))
setMethod("invVonB", signature(param="missing", data="numeric"),
   function(data,sinf=NA,K=NA,t0=NA,a=NA) invVonB(FLPar(sinf=sinf,K=K,t0=t0,a=a),data))

setMethod("invVonB", signature(param="numeric",data="FLQuant"),
   function(param,data) invVonB(FLPar(param),data))
setMethod("invVonB", signature(param="numeric", data="FLCohort"),
   function(param,data) invVonB(FLPar(param),data))
setMethod("invVonB", signature(param="numeric", data="numeric"),
   function(param,data) invVonB(FLPar(param),data))

setMethod("invVonB", signature(param="list",data="FLQuant"),
   function(param,data) invVonB(FLPar(unlist(param)),data))
setMethod("invVonB", signature(param="list", data="FLCohort"),
   function(param,data) invVonB(FLPar(unlist(param)),data))
setMethod("invVonB", signature(param="list", data="numeric"),
   function(param,data) invVonB(FLPar(unlist(param)),data))
################################################################################

# 7) Gompertz ##################################################################

iniGompertz<-function(data,asym,b2,b3){
   param<-FLPar(asym=asym,b2=b2,b3=b3)
   plot(data$age,data$data)
   points(data$age,gompertz(param,data$age),col="red",pch=17)

   return(param)}

setGeneric('gompertz', function(param, data, ...)
  standardGeneric('gompertz'))
setMethod("gompertz", signature(param="FLPar", data="numeric"),
   function(param,data) param["asym"]*exp(-param["b2"]*param["b3"]^data))
setMethod("gompertz", signature(param="FLPar", data="FLQuant"),
   function(param,data) param["asym"]*exp(-param["b2"]*param["b3"]^data))
setMethod("gompertz", signature(param="FLPar", data="FLCohort"),
   function(param,data) param["asym"]*exp(-param["b2"]*param["b3"]^data))

setMethod("gompertz", signature(param="numeric",data="FLQuant"),
   function(param,data) gompertz(FLPar(param),data))
setMethod("gompertz", signature(param="numeric", data="FLCohort"),
   function(param,data) gompertz(FLPar(param),data))
setMethod("gompertz", signature(param="numeric", data="numeric"),
   function(param,data) gompertz(FLPar(param),data))

setMethod("gompertz", signature(param="list",data="FLQuant"),
   function(param,data) gompertz(FLPar(unlist(param)),data))
setMethod("gompertz", signature(param="list", data="FLCohort"),
   function(param,data) gompertz(FLPar(unlist(param)),data))
setMethod("gompertz", signature(param="list", data="numeric"),
   function(param,data) gompertz(FLPar(unlist(param)),data))

setMethod("gompertz", signature(param="missing",data="FLQuant"),
   function(data,asym=NA,b2=NA,b3=NA) gompertz(FLPar(asym=asym,b2=b2,b3=b3),data))
setMethod("gompertz", signature(param="missing", data="FLCohort"),
   function(data,asym=NA,b2=NA,b3=NA) gompertz(FLPar(asym=asym,b2=b2,b3=b3),data))
setMethod("gompertz", signature(param="missing", data="numeric"),
   function(data,asym=NA,b2=NA,b3=NA) gompertz(FLPar(asym=asym,b2=b2,b3=b3),data))

################################################################################



iniLog<-function(data,a50,ato95,asym){
   plot(  data$age,data$data)
   points(logisticFn(a50,ato95,asym,data$age),col="red",pch=17)

   return(c(a50=a50,ato95=ato95,asym=asym))}

logisticFn<-function(x,a50,ato95,asym=1.0){
  pow<-function(a,b) a^b
  res<-x

  gt=(a50-x)/ato95 > 5
  lt=(a50-x)/ato95 < -5

  res[gt]<-0
  res[lt]<-asym
  res[!gt & !lt]<-asym/(1.0+pow(19.0,(a50-x[!gt & !lt])/ato95))

  return(res)}

setGeneric('logistic', function(param,data, ...)
  standardGeneric('logistic'))

setMethod("logistic", signature(param="FLPar", data="numeric"),
   function(param,data) logisticFn(data,param["a50"],param["ato95"],param["asym"]))
setMethod("logistic", signature(param="FLPar", data="FLQuant"),
   function(param,data) logisticFn(data,param["a50"],param["ato95"],param["asym"]))
setMethod("logistic", signature(param="FLPar", data="FLCohort"),
   function(param,data) logisticFn(data,param["a50"],param["ato95"],param["asym"]))

setMethod("logistic", signature(param="numeric",data="FLQuant"),
   function(param,data) logistic(FLPar(param),data))
setMethod("logistic", signature(param="numeric", data="FLCohort"),
   function(param,data) logistic(FLPar(param),data))
setMethod("logistic", signature(param="numeric", data="numeric"),
   function(param,data) logistic(FLPar(param),data))

setMethod("logistic", signature(param="list",data="FLQuant"),
   function(param,data) logistic(FLPar(unlist(param)),data))
setMethod("logistic", signature(param="list", data="FLCohort"),
   function(param,data) logistic(FLPar(unlist(param)),data))
setMethod("logistic", signature(param="list", data="numeric"),
   function(param,data) logistic(FLPar(unlist(param)),data))

setMethod("logistic", signature(param="missing",data="FLQuant"),
   function(data,asym=NA,a50=NA,ato95=NA) logistic(FLPar(asym=asym,a50=a50,ato95=ato95),data))
setMethod("logistic", signature(param="missing", data="FLCohort"),
   function(data,asym=NA,a50=NA,ato95=NA) logistic(FLPar(asym=asym,a50=a50,ato95=ato95),data))
setMethod("logistic", signature(param="missing", data="numeric"),
   function(data,asym=NA,a50=NA,ato95=NA) logistic(FLPar(asym=asym,a50=a50,ato95=ato95),data))
################################################################################

# 9) Richards ##################################################################

iniRichards<-function(data,beta,a50,ato95){
   param<-FLPar(beta=beta,a50=a50,ato95=ato95)
   plot(data$age,data$data)
   points(data$age,richards(param,data$age),col="red",pch=17)

   return(param)}

richardsFn<-function(x, beta, a50, ato95){
  gamma <-ato95*log(19)/(log(2^beta-1)-log((20/19)^beta-1))
  alpha <-a50+gamma*log(2^beta-1)/log(19)

  (1/(1+19^(alpha-x)/beta))^1/beta}

setGeneric('richards', function(param,data, ...)
  standardGeneric('richards'))

setMethod("richards", signature(param="FLPar", data="numeric"),
   function(param,data) richardsFn(data,param["beta"],param["a50"],param["ato95"]))
setMethod("richards", signature(param="FLPar", data="FLQuant"),
   function(param,data) richardsFn(data,param["beta"],param["a50"],param["ato95"]))
setMethod("richards", signature(param="FLPar", data="FLCohort"),
   function(param,data) richardsFn(data,param["beta"],param["a50"],param["ato95"]))

setMethod("richards", signature(param="numeric",data="FLQuant"),
   function(param,data) richards(FLPar(param),data))
setMethod("richards", signature(param="numeric", data="FLCohort"),
   function(param,data) richards(FLPar(param),data))
setMethod("richards", signature(param="numeric", data="numeric"),
   function(param,data) richards(FLPar(param),data))

setMethod("richards", signature(param="list",data="FLQuant"),
   function(param,data) richards(FLPar(unlist(param)),data))
setMethod("richards", signature(param="list", data="FLCohort"),
   function(param,data) richards(FLPar(unlist(param)),data))
setMethod("richards", signature(param="list", data="numeric"),
   function(param,data) richards(FLPar(unlist(param)),data))

setMethod("richards", signature(param="missing",data="FLQuant"),
   function(data,beta=NA,a50=NA,ato95=NA) richards(FLPar(beta=beta,a50=a50,ato95=ato95),data))
setMethod("richards", signature(param="missing", data="FLCohort"),
   function(data,beta=NA,a50=NA,ato95=NA) richards(FLPar(beta=beta,a50=a50,ato95=ato95),data))
setMethod("richards", signature(param="missing", data="numeric"),
   function(data,beta=NA,a50=NA,ato95=NA) richards(FLPar(beta=beta,a50=a50,ato95=ato95),data))
################################################################################

## 10) Double Normal ###########################################################
setGeneric('doubleNormal', function(param,data, ...)
  standardGeneric('doubleNormal'))

dnormal <- function(param,age){
   pow <-function(a,b) a^b
   func<- function(age,a1,sl,sr){
      if (age < a1)
	  return(pow(2.0,-((age-a1)/sl*(age-a1)/sl)))
      else
	  return(pow(2.0,-((age-a1)/sr*(age-a1)/sr)))}

    sapply(age,func,param["a1"],param["sl"],param["sr"])}

doubleNormalparam<-function(param){
         dimnames(param)$param<-tolower(dimnames(param)$param)
         return(param)}
      
setMethod("doubleNormal", signature(param="FLPar", data="FLQuant"),
   function(param,data)  
         dnormal(doubleNormalparam(param),data))
setMethod("doubleNormal", signature(param="FLPar", data="FLCohort"),
   function(param,data)  
         dnormal(doubleNormalparam(param),data))
setMethod("doubleNormal", signature(param="FLPar", data="numeric"),
   function(param,data)  
         dnormal(doubleNormalparam(FLPar(param)),data))

setMethod("doubleNormal", signature(param="numeric",data="FLQuant"),
   function(param,data) doubleNormal(FLPar(param),data))
setMethod("doubleNormal", signature(param="numeric", data="FLCohort"),
   function(param,data) doubleNormal(FLPar(param),data))
setMethod("doubleNormal", signature(param="numeric", data="numeric"),
   function(param,data) doubleNormal(FLPar(param),data))

setMethod("doubleNormal", signature(param="list",data="FLQuant"),
   function(param,data) doubleNormal(FLPar(unlist(param)),data))
setMethod("doubleNormal", signature(param="list", data="FLCohort"),
   function(param,data) doubleNormal(FLPar(unlist(param)),data))
setMethod("doubleNormal", signature(param="list", data="numeric"),
   function(param,data) doubleNormal(FLPar(unlist(param)),data))

setMethod("doubleNormal", signature(param="missing",data="FLQuant"),
   function(data,a1=NA,sl=NA,sr=NA) doubleNormal(FLPar(a1=a1,sl=sl,sr=sr),data))
setMethod("doubleNormal", signature(param="missing", data="FLCohort"),
   function(data,a1=NA,sl=NA,sr=NA) doubleNormal(FLPar(a1=a1,sl=sl,sr=sr),data))
setMethod("doubleNormal", signature(param="missing", data="numeric"),
   function(data,a1=NA,sl=NA,sr=NA) doubleNormal(FLPar(a1=a1,sl=sl,sr=sr),data))
################################################################################

#################################################################################
## 11) M ########################################################################
Mg  =function(L,Linf,M1=0.1,h=1.71,n=-1.66,i=0.8) M1+h*Linf^i*L^n
Mhh =function(maxAge) exp(1.44-0.982*log(maxAge))
MRoT=function(maxAge,val=3) val/maxAge
       

## 12) mat50 ####################################################################
## Maturity age at 50% maturity
mat50 =function(M,k) log(((3*k+M)/M)/k)

Mfunc  =function(L,Linf,k) exp(0.55 - 1.61*log(L) + 1.44*log(Linf) + log(k))
Matfunc=function(Linf,k) 0.8776*Linf-0.038

## 13) Schnute ###################################################################
#### parameters
# y1: length at age t1
# y2: length at age t2
# t1: age at length y1
# t2: age at length y2


iniSchn<-function(data,a,b,t1,t2,y1,y2){
   plot(  data$age,data$data)
   points(schnute(data$age,FLPar(a=a,b=b,t1=t1,t2=t2,y1=y1,y2=y2)),col="red",pch=17)

   return(c(a=a,b=b,t1=t1,t2=t2,y1=y1,y2=y2))}

schnuteFn<-function(param,data){
  fn1<-function(param,data) (param["y1"]^param["b"]+(param["y2"]^param["b"]-param["y1"]^param["b"])*(1.0-exp(-param["a"]*(data-param["t1"])))/(1.0-exp(-param["a"]*(param["t2"]-param["t1"]))))^(-1/param["b"])
  fn2<-function(param,data)  param["y1"]*exp(log(param["y2"]/param["y1"])*(1.0-exp(-param["a"]*(data-param["t1"])))/(1.0-exp(-param["a"]*(param["t2"]-param["t1"]))))
  fn3<-function(param,data) (param["y1"]^param["b"]+(param["y2"]^param["b"]-param["y1"]^param["b"])*(data-param["t1"])/(param["t2"]-param["t1"]))^(-1/param["b"])
  fn4<-function(param,data)  param["y1"]*exp(log(param["y2"]/param["y1"])*(data-param["t1"])/(param["t2"]-param["t1"]))

  if (param["a"]!=0 & param["b"]!=0) return(fn1(param,data))
  if (param["a"]!=0 & param["b"]==0) return(fn2(param,data))
  if (param["a"]==0 & param["b"]!=0) return(fn3(param,data))
  if (param["a"]==0 & param["b"]==0) return(fn4(param,data))}

#### Growth
setGeneric('schnute', function(param,data, ...)
  standardGeneric('schnute'))

setMethod("schnute", signature(param="FLPar", data="numeric"),
   function(param,data) schnuteFn(param,data))
setMethod("schnute", signature(param="FLPar", data="FLQuant"),
   function(param,data) schnuteFn(param,data))
setMethod("schnute", signature(param="FLPar", data="FLCohort"),
   function(param,data) schnuteFn(param,data))
setMethod("schnute", signature(data="FLQuant",param="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA) {
      param<-FLPar(a=a,b=b,y1=y1,y2=y2,t1=t1,t2=t2)
      schnuteFn(param,data)})
setMethod("schnute", signature(data="FLCohort",param="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA) {
      param<-FLPar(a=a,b=b,y1=y1,y2=y2,t1=t1,t2=t2)
      schnuteFn(param,data)})
setMethod("schnute", signature(data="numeric",param="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA) {
      param<-FLPar(a=a,b=b,y1=y1,y2=y2,t1=t1,t2=t2)
      schnuteFn(param,data)})

setGeneric('schnuteMass', function(param,data, ...)
  standardGeneric('schnuteMass'))
setMethod("schnuteMass", signature(param="FLPar", data="FLQuant"),
   function(param,data) param["cf"]*schnute(param,data)^param["pow"])
setMethod("schnuteMass", signature(param="FLPar", data="FLCohort"),
   function(param,data) param["cf"]*schnute(param,data)^param["pow"])
setMethod("schnuteMass", signature(param="FLPar", data="numeric"),
   function(param,data) param["cf"]*schnute(param,data)^param["pow"])
setMethod("schnuteMass", signature(data="FLQuant", param="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      param<-FLPar(sinf=sinf,K=K,t0=t0,a=a,b=b)

      return(param["cf"]*schnute(param,data)^param["pow"])})
setMethod("schnuteMass", signature(data="FLCohort", param="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      param<-FLPar(sinf=sinf,K=K,t0=t0,a=a,b=b)

      return(param["cf"]*schnute(param,data)^param["pow"])})
setMethod("schnuteMass", signature(data="numeric", param="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      param<-FLPar(sinf=sinf,K=K,t0=t0,a=a,b=b)

      return(param["cf"]*schnute(param,data)^param["pow"])})


################################################################################
# sinf:    Infinite size, i.e. assymptotic size reached after an infinite period of growth
# K:       Constant describing "how fast" the animal grows
# to:      Age of the animal at size zero
# tprime;  Age at inflexion point

#if (a!=0 & b!=0)

## 14) Density dependence ######################################################
#                                                                              #
# Where a parameter is a function of a covariate                               # 
#                                                                              #
################################################################################
dd<-function(par,dd,covar,ff="linear",multiplicative=TRUE){
    logistic<-function(x,min,max){
	y=1/(1+exp(-x))
        return((max-min)*y+min)}


    delta<-switch(ff,
          linear   =dd["a"]+dd["b"]*covariate,
          loglinear=exp(dd["a"]+dd["b"]*log(covariate)),
          logistic =logistic(covariate,dd["a"]+dd["b"]),
          )

    if (multiplicative) par=par*(1+delta)
    else                par=par+delta

    return(delta)}


#x<-seq(-10,10,length.out=100)
#plot(bnd(x,10,50)~x,type="l")

SS3SelParam<-function(param){
    #p1 – PEAK: ascending inflection size (in cm)
    #p2 – TOP: width of plateau, as logistic between PEAK and MAXLEN
    #p3 – ASC-WIDTH: parameter value is ln(width)
    #p4 – DESC-WIDTH: parameter value is ln(width)
    #p5 – INIT: selectivity at first bin, as logistic between 0 and 1.
    #P6 – FINAL: selectivity at last bin, as logistic between 0 and 1.


    #Lmin is the midpoint of the smallest length bin,
    #Lmax is the midpoint of the largest length bin, and
    beta   <-numeric(5)

    #β1 is the size at which selectivity=1.0 begins,
    beta[1]<-p1

    #β2 is the size at which selectivity=1.0 ends,
    beta[2]<-p2-p1

    #β3 determines the slope of the ascending section,
    beta[3]<-p3

    #β4 determines the slope of the descending section,
    beta[4]<-p4

    #β5 is the selectivity at Lmin,
    beta[5]<-p5

    #β6 is the selectivity at Lmin,
    beta[6]<-p6

    return(beta)}


#ss3SelDN<-function(L,beta,Lmin,Lmax){
#
#   join=function(L,beta)
#         (1+exp(-20*((L-beta)/(1+abs(L-beta)))))^(-1)
	  
#   nrml=function(L,Lbnd,a,b,c){
#           term1 = 1-exp(-(L   -a)^2/b)
#           term2 = 1-exp(-(Lbnd-a)^2/b)         
           
#           return(1-(1-c)*((term1)/(term2)))}

#   asc=function(L,Lmin,beta) nrml(L,Lbnd,beta[1],beta[3],beta[5])
#   dsc=function(L,Lmax,beta) nrml(L,Lbnd,beta[2],beta[4],beta[6])

#   return(data.frame(Len=L,
#                     asc=asc(L,Lmin,beta),join1=join(L,beta[1]),join2=join(L,beta[2]),desc=dsc(L,Lmax,beta),   
#                     sel=asc(L,Lmin,beta)*(1-join(L,beta[1]))+join(L,beta[1])*(1-join(L,beta[2])+dsc(L,Lmax,beta)*join(L,beta[2]))))

#   return(sel)}

#beta<-c("1"=10.0,"2"=15,"3"=1,"4"=1,"5"=.12,"6"=0.1)
#Lmin=min(L[,1])
#Lmax=max(L[,1])


#beta<-c("1"=10.0,"2"=15,"3"=1,"4"=1,"5"=.12,"6"=0.1)
#ggplot(melt(ss3SelDN(L[,1],beta,Lmin,Lmax),id.var="Len")) + 
#       geom_line(aes(Len,value,group=variable,colour=variable)) + 
#       scale_y_continuous(limits=c(0,1))

#beta<-c("1"=10.0,"2"=15,"3"=.2,"4"=.2,"5"=.12,"6"=0.1)
#ggplot(melt(ss3SelDN(L[,1],beta,Lmin,Lmax),id.var="Len")) + 
#       geom_line(aes(Len,value,group=variable,colour=variable)) + 
#       scale_y_continuous(limits=c(0,1))
