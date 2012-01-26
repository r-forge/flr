#linf,winf,a,b,k,t,amat
gundersonDygert=function(gsi)
  0.03 + 1.68*gsi

#http://icesjms.oxfordjournals.org/content/66/9/1978.full
#http://onlinelibrary.wiley.com/doi/10.1111/j.1467-2979.2009.00350.x/full

## Constant values
pauly1=function(winf,k,t=10)
   exp(-0.2107-0.0824*log(winf)+0.6757*log(k)+0,4627*log(t))

pauly2=function(linf,k,t=10)
   exp(-0.0066-0.279*log(linf)+0.6543*log(k)+0.4634*log(t))

hoenig=function(amax)
   4.22*amax

jensen=function(k)
   1.5*k

RickterEfanov=function(amat)
   1.52/(amat^0.72)-0.16 
  
PetersonWroblewski=function(wt)
   1.29*wt-0.25

Lorenzen=function(wt)
   3.00*wt-0.288

McGurk=function(wt)
   0.00526*wt-0.25

## variable
gislason=function(l,linf,k) 
   exp(0.55-1.61*log(l) + 1.44*log(linf) + log(k))

ChenWatanabe=function(age,k,t0=-0.1){
   m =k/(1-exp(-k*(age-t0)))

   tm =-(1/k)*log(1-exp(k*t0))+t0
   print(tm)
   bit=exp(-k*(tm-t0))
   
   a0=1-bit
   a1=k*bit
   a2=-0.5*k^2*bit
   age.=age>tm
   m[age.] =k/(a0+a1*(age[age.]-tm)+a2*(age[age.]-tm)^2)
  
   return(m)}   


################################################################################

#### Growth functions ##########################################################
# 2)  FLQuant/FLCohort to ages                                                 #
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
setMethod("ages", signature(data="FLCohort"),
   function(data,timing=NULL){
      res<-FLCohort(dimnames(data)$age,dimnames=dimnames(data))

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
   function(data,a=NA,b=NA) wt2len(FLPar(a=a,b=b),dta))
setMethod("wt2len", signature(params="missing", data="numeric"),
   function(data,a=NA,b=NA) wt2len(FLPar(a=a,b=b),data))
################################################################################

setGeneric('vonB', function(params,data, ...)
  standardGeneric('vonB'))

vonBParams<-function(params){
         dimnames(params)$params=tolower(dimnames(params)$params)
         if (!("b"  %in% dimnames(params)$params)) params<-addPar(params,"b" ,1)
         if (!("t0" %in% dimnames(params)$params)) params<-addPar(params,"t0",0)
         dimnames(params)$params[substr(dimnames(params)$params,2,nchar(dimnames(params)$params)) %in% "inf"]<-"sinf"

         return(params)}
      
setMethod("vonB", signature(params="FLPar", data="FLQuant"),
   function(params,data) {
         params<-vonBParams(params)  
         params["sinf"]*(1.0-exp(-params["k"]*(data-params["t0"])))^params["b"]})
setMethod("vonB", signature(params="FLPar", data="FLCohort"),
   function(params,data) {
         params<-vonBParams(params)  
         params["sinf"]*(1.0-exp(-params["k"]*(data-params["t0"])))^params["b"]})
setMethod("vonB", signature(params="FLPar", data="numeric"),
   function(params,data) {
         params<-vonBParams(params)  
         params["sinf"]*(1.0-exp(-params["k"]*(data-params["t0"])))^params["b"]})

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
   function(data,sinf=NA,K=NA,t0=NA,a=1) vonB(FLPar(sinf=sinf,K=K,t0=t0,a=a),data))
setMethod("vonB", signature(params="missing", data="FLCohort"),
   function(data,sinf=NA,K=NA,t0=NA,a=1) vonB(FLPar(sinf=sinf,K=K,t0=t0,a=a),data))
setMethod("vonB", signature(params="missing", data="numeric"),
   function(data,sinf=NA,K=NA,t0=NA,a=1) vonB(FLPar(sinf=sinf,K=K,t0=t0,a=a),data))
################################################################################

# 6) Von Bertalanffy, inv ##################################################
setGeneric('invVonB', function(params,data, ...)
  standardGeneric('invVonB'))

setMethod("invVonB", signature(params="FLPar", data="FLQuant"),
   function(params,data) {
  params<-vonBParams(params)  
        -log(1.0-(data/params["sinf"])^(1/params["b"]))/params["k"]+params["t0"]})
setMethod("invVonB", signature(params="FLPar", data="FLCohort"),
   function(params,data) {
        params<-vonBParams(params)  
        -log(1.0-(data/params["sinf"])^(1/params["b"]))/params["k"]+params["t0"]})
setMethod("invVonB", signature(params="FLPar", data="numeric"),
   function(params,data) {
       params<-vonBParams(params)  
        -log(1.0-(data/params["sinf"])^(1/params["b"]))/params["k"]+params["t0"]})

setMethod("invVonB", signature(params="missing",data="FLQuant"),
   function(data,sinf=NA,K=NA,t0=NA,a=NA) invVonB(FLPar(sinf=sinf,K=K,t0=t0,a=a),data))
setMethod("invVonB", signature(params="missing", data="FLCohort"),
   function(data,sinf=NA,K=NA,t0=NA,a=NA) invVonB(FLPar(sinf=sinf,K=K,t0=t0,a=a),data))
setMethod("invVonB", signature(params="missing", data="numeric"),
   function(data,sinf=NA,K=NA,t0=NA,a=NA) invVonB(FLPar(sinf=sinf,K=K,t0=t0,a=a),data))

setMethod("invVonB", signature(params="numeric",data="FLQuant"),
   function(params,data) invVonB(FLPar(params),data))
setMethod("invVonB", signature(params="numeric", data="FLCohort"),
   function(params,data) invVonB(FLPar(params),data))
setMethod("invVonB", signature(params="numeric", data="numeric"),
   function(params,data) invVonB(FLPar(params),data))

setMethod("invVonB", signature(params="list",data="FLQuant"),
   function(params,data) invVonB(FLPar(unlist(params)),data))
setMethod("invVonB", signature(params="list", data="FLCohort"),
   function(params,data) invVonB(FLPar(unlist(params)),data))
setMethod("invVonB", signature(params="list", data="numeric"),
   function(params,data) invVonB(FLPar(unlist(params)),data))
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

  if (length(x[!gt & !lt])>0)
     res[!gt & !lt]<-asym/(1.0+pow(19.0,(a50-x[!gt & !lt])/ato95))

  return(res)}

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

## 10) Double Normal ###########################################################
setGeneric('doubleNormal', function(params,data, ...)
  standardGeneric('doubleNormal'))

dnormal <- function(params,age){
   pow <-function(a,b) a^b
   func<- function(age,a1,sl,sr){
      if (age < a1)
	  return(pow(2.0,-((age-a1)/sl*(age-a1)/sl)))
      else
	  return(pow(2.0,-((age-a1)/sr*(age-a1)/sr)))}

    sapply(age,func,params["a1"],params["sl"],params["sr"])}

doubleNormalParams<-function(params){
         dimnames(params)$params<-tolower(dimnames(params)$params)
         return(params)}
      
setMethod("doubleNormal", signature(params="FLPar", data="FLQuant"),
   function(params,data)  
         dnormal(doubleNormalParams(params),data))
setMethod("doubleNormal", signature(params="FLPar", data="FLCohort"),
   function(params,data)  
         dnormal(doubleNormalParams(params),data))
setMethod("doubleNormal", signature(params="FLPar", data="numeric"),
   function(params,data)  
         dnormal(doubleNormalParams(FLPar(params)),data))

setMethod("doubleNormal", signature(params="numeric",data="FLQuant"),
   function(params,data) doubleNormal(FLPar(params),data))
setMethod("doubleNormal", signature(params="numeric", data="FLCohort"),
   function(params,data) doubleNormal(FLPar(params),data))
setMethod("doubleNormal", signature(params="numeric", data="numeric"),
   function(params,data) doubleNormal(FLPar(params),data))

setMethod("doubleNormal", signature(params="list",data="FLQuant"),
   function(params,data) doubleNormal(FLPar(unlist(params)),data))
setMethod("doubleNormal", signature(params="list", data="FLCohort"),
   function(params,data) doubleNormal(FLPar(unlist(params)),data))
setMethod("doubleNormal", signature(params="list", data="numeric"),
   function(params,data) doubleNormal(FLPar(unlist(params)),data))

setMethod("doubleNormal", signature(params="missing",data="FLQuant"),
   function(data,a1=NA,sl=NA,sr=NA) doubleNormal(FLPar(a1=a1,sl=sl,sr=sr),data))
setMethod("doubleNormal", signature(params="missing", data="FLCohort"),
   function(data,a1=NA,sl=NA,sr=NA) doubleNormal(FLPar(a1=a1,sl=sl,sr=sr),data))
setMethod("doubleNormal", signature(params="missing", data="numeric"),
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
      params<-FLPar(sinf=sinf,K=K,t0=t0,a=a,b=b)

      return(params["cf"]*schnute(params,data)^params["pow"])})
setMethod("schnuteMass", signature(data="FLCohort", params="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      params<-FLPar(sinf=sinf,K=K,t0=t0,a=a,b=b)

      return(params["cf"]*schnute(params,data)^params["pow"])})
setMethod("schnuteMass", signature(data="numeric", params="missing"),
   function(data,a=NA,b=NA,y1=NA,y2=NA,t1=NA,t2=NA,cf=NA,pow=3) {
      params<-FLPar(sinf=sinf,K=K,t0=t0,a=a,b=b)

      return(params["cf"]*schnute(params,data)^params["pow"])})


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

gislasim=function(par,sl=5,sr=5000){
   
  names(dimnames(par))=tolower(names(dimnames(par)))

  ## growth parameters
  if (!("t0" %in% dimnames(par)$params)) par=rbind(par,FLPar("t0"=0))
  if (!("a"  %in% dimnames(par)$params)) par=rbind(par,FLPar("a" =0.001))
  if (!("b"  %in% dimnames(par)$params)) par=rbind(par,FLPar("b" =3))
    
  if (!("k"  %in% dimnames(par)$params)) par=rbind(par,FLPar("k"=exp(0.5236+c(log(par["linf"]))*-0.4540)))
 
  ### why does rbind(FLPar,numeric) not return an FLPar
  par=rbind(par,FLPar("sinf"=par["a"]*par["linf"]^par["b"]))

  ## maturity parameters
  par=FLPar(rbind(par,FLPar(c("a50"=0.8776*par["linf",]-0.038,"ato95"=0,"asym"=1.0))))
  par["a50"]=invVonB(par,c(par["a50"]))
  
  ## selectivity
  par=rbind(par,FLPar(a1=par["a50"],sl=sl,sr=sr))
  
  return(par)}

#### Life Hisotory Generator ####################################################
# TODO lh(gislasim(par))
# TODO Add support for FLSR
lh=function(par,
            mass         =vonB,
            mFn          =function(par,len,T=290,a=FLPar(c(-2.1104327,-1.7023068,1.5067827,0.9664798,763.5074169)))
                                    exp(a[1]+a[2]*log(len) + a[3]*log(par["linf"]) + a[4]*log(par["k"]) + a[5]/T),
#            mFn          =function(par,len) exp(0.55 - 1.61*log(len) + 1.44*log(par["linf"]) + log(par["k"])),
            matFn        =logistic,
            selFn        =doubleNormal,
            sr           =list(model="bevholt",steepness=0.9,vbiomass=1e3),
            age=1:40+0.5,T=290,...){
  
   age=FLQuant(age,dimnames=list(age=floor(age)))
   len=mass(par,age)
   wts=par["a"]*len^par["b"]
   
   m.   =qmin(mFn(  par=par,len=len,T=T),1)
   mat. =matFn(par,age)
   sel. =selFn(par,age)

   ## create a FLBRP object to   calculate expected equilibrium values and ref pts
   dms=dimnames(m.)
   res=FLBRP(stock.wt       =wts,
             landings.wt    =wts,
             discards.wt    =wts,
             bycatch.wt     =wts,
             m              =m.,
             mat            =mat.,
             landings.sel   =FLQuant(sel., dimnames=dimnames(m.)),
             discards.sel   =FLQuant(0,    dimnames=dimnames(m.)),
             bycatch.harvest=FLQuant(0,    dimnames=dimnames(m.)),
             harvest.spwn   =FLQuant(0,    dimnames=dimnames(m.)),
             m.spwn         =FLQuant(0,    dimnames=dimnames(m.)),
             availability   =FLQuant(1,    dimnames=dimnames(m.)))

   ## FApex
   range(res,c("minfbar","maxfbar"))[]<-as.numeric(dimnames(landings.sel(res)[landings.sel(res)==max(landings.sel(res))][1])$age)

   ## replace any slot passed in as an arg
   args<-list(...)
   for (slt in names(args)[names(args) %in% names(getSlots("FLBRP"))[names(getSlots("FLBRP"))!="fbar"]])
     slot(res, slt)<-args[[slt]]

   ## Stock recruitment relationship
   model(res) =do.call(sr$model,list())$model
   params(res)=FLPar(abPars(sr$model,spr0=spr0(res),s=sr$steepness,v=sr$vbiomass))

   dimnames(refpts(res))$refpt[5]="crash"

   res=brp(res)
   
   if ("fbar" %in% names(args)) 
       fbar(res)<-args[["fbar"]]
   else
       fbar(res)<-FLQuant(seq(0,1,length.out=101))*refpts(res)["crash","harvest"]
  
   return(brp(res))}






             
