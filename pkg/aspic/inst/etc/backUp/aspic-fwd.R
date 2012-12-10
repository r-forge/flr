# fwd.R - 
# aspic/R/fwd.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# $Id:  $


setGeneric("spFn", function(biomass,params,...){
    value  <-  standardGeneric("spFn")
  	return(value)})


setGeneric('fwd', function(object,ctrl,...)
  standardGeneric('fwd'))


setMethod('spFn', signature(biomass="FLQuant",params="FLPar"),
 function(biomass,params,model="pellat") {
    foxFn <-function(biomass, params)
          params["r"]*biomass*(1-log(biomass)/log(params["k"]))
    schaeferFn <- function(biomass, params) { #logistic
     
          if ("msy" %in% dimnames(params)$param) { 
            params["msy"]=4*params["msy"]/params["k"]
            dimnames(params)$param[1]="r"
            }
          params["r"]*biomass*(1-biomass/params["k"])}
#     pellat <- function(biomass, params)
#           params["r"]/params["p"]*biomass*(1-(biomass/params["k"])^params["p"])
     pellatFn <- function(biomass, params){
           a=biomass*params["r"]/params["p"]
           b=biomass/params["k"]
           c=b^params["p"]
           a*(1-c)}
    shepherdFn <- function(biomass,params)
          params["r"]*biomass/(1+biomass/params["k"])-params["m"]*biomass
    gullandFn <- function(biomass,params)
          params["r"]*biomass*(params["k"]-biomass)
    fletcherFn <- function(biomass,params) {
          lambda <- (params["p"]^(params["p"]/(params["p"]-1)))/(params["p"]-1)
          lambda*msy*(biomass/params["k"])-lambda*params["msy"]*(biomass/params["k"])^params["p"]}

    res <- switch(model,
           "fox"     =foxFn(     biomass,params),
           "schaefer"=schaeferFn(biomass,params),
           "gulland" =gullandFn( biomass,params),
           "fletcher"=fletcherFn(biomass,params),
           "pellat"  =pellatFn(  biomass,params),
           "shepherd"=shepherdFn(biomass,params))

    return(res)}) 

# calcs a relative value
relFn=function(val,ratio,lag=0){
  if (lag==0) return(ratio)
  
  yrs=ac(as.numeric(dimnames(ratio)$year)-lag)
  yrs=yrs[yrs %in% dimnames(val)$year]
  yr2=ac(as.numeric(yrs)+lag)
  res=ratio[,yr2]*val[,yrs]
  
  return(res)}

## sets a minimum F
minFn=function(val,bnd){
  val[is.na(val)]=0
  
  res=val
  res[val<bnd]=bnd[val<bnd]
  
  return(res)}

## sets a maximum F
maxFn=function(val,bnd){
  val[is.na(val)]=0
  
  res=val
  res[val>bnd]=bnd[val>bnd]
 
  return(res)}

## calculates inter-annual bounds
iavFn=function(val,bnd,lag=1){
  if (all(!is.finite(bnd)) | lag<1) stop("can´t have lags < 1")
  
  ref=apply(val[,dims(val)[2]-lag],2:6,mean)
  
  bnd=val[,dims(val)[2]]*(1-bnd[1])    
  if (finite(bnd[1])) val[,dims(val)[2]][ref<bnd[1]]=ref[ref<bnd[1]]
  
  bnd=val[,dims(val)[2]]*(1+bnd[2])    
  if (finite(bnd[2])) val[,dims(val)[2]][ref>bnd[2]]=ref[ref>bnd[2]]
 
  return(val[,dims(val)[2]])}


#val=FLQuant(rlnorm(200),dimnames=list(age=1:5,year=1:10,iter=1:4))
#bnd=FLQuant(1,          dimnames=list(age=1:5,year=1:10,iter=1:4))

# fwd(aspic) {{{
setMethod("fwd", signature(object="aspic",ctrl="missing"),
  function(object, catch=NULL, harvest=NULL, stock=NULL, hcr=NULL, pe=NULL, peMult=TRUE,minF=0,maxF=2,lag=0,
           bounds=list(catch=c(Inf,Inf)),end=NULL,...) {

# object =simaspic()
# harvest=FLQuant(0.9,dimnames=list(year=50:80))
# catch  =NULL
# stock  =NULL
# pe     =NULL
# pwMult =TRUE
# minF   =0
# maxF   =2
# lag    =1
# bounds=list(catch=c(Inf,Inf))

  lag=max(lag,0)

  ## catch, harvest or stock?
  
  if (!is.null(catch))   ctcTrgt=TRUE else ctcTrgt=FALSE
  if (!is.null(harvest)) hvtTrgt=TRUE else hvtTrgt=FALSE
  if (!is.null(stock))   stkTrgt=TRUE else stkTrgt=FALSE
  if (!is.null(hcr))     hcrTrgt=TRUE else hcrTrgt=FALSE
    
  if(!ctcTrgt & hvtTrgt & stkTrgt & hcrTrgt)
      stop("must supply catch, harvest or stock as a target or a HCR")
 
  if (ctcTrgt) if (dims(stock(object))$maxyear < dims(  catch)$maxyear) object=window(object,end=dims(  catch)$maxyear)
  if (hvtTrgt) if (dims(stock(object))$maxyear < dims(harvest)$maxyear) object=window(object,end=dims(harvest)$maxyear)
  if (stkTrgt) if (dims(stock(object))$maxyear < dims(  stock)$maxyear) object=window(object,end=dims(  stock)$maxyear)
   
  if (stkTrgt) catch=ssb*0

  ## check year range
  if (ctcTrgt | stkTrgt) {
    if (!(all(dimnames(catch)$year %in% dimnames(catch(object))$year)))
         object = window(object,end=dims(catch)$maxyear)
      catch(object)[,dimnames(catch)$year] <- catch
      yrs <- dimnames(catch)$year
  } else if (hvtTrgt) {
      if (!(all(dimnames(harvest)$year %in% dimnames(catch(object))$year)))
        stop("years in harvest & stock dont match")
      yrs <- dimnames(harvest)$year
  } else if (hcrTrgt) {
     yrs=ac(dims(stock(object))$maxyear:end) 
     object=window(object,end=end)
  }  
        
  ## B0 in year 1?
  if (as.numeric(yrs[1]) == range(object,"minyear"))
     stock(object)[,ac(range(object,"minyear"))] = params(object)["k"] * params(object)["b0"]

  ## maxyear
  if (max(as.numeric(yrs)) == range(object,"maxyear"))
     stock(object) <- window(stock(object),end=range(object,"maxyear")+1)

  ## niters
  nits=dims(object)$iter
  if (!is.null(pe)) nits=max(nits,dims(pe)$iter)
  if (hvtTrgt) nits=max(nits,dims(harvest)$iter) else
  if (ctcTrgt) nits=max(nits,dims(catch  )$iter) else
  if (stkTrgt) nits=max(nits,dims(stock  )$iter) 
  if (nits>1){ 
     catch(object) =propagate(catch(object),nits)
     stock(object) =propagate(stock(object),nits)
     if (hvtTrgt) harvest=propagate(harvest,nits)
   
     if (dims(params(object))$iter==1) params(object)=propagate(params(object),nits)
     if (!is.null(pe))                 pe            =propagate(pe            ,nits)
     } 
   
  ## projections
  if (hvtTrgt) object@harvest[,dimnames(harvest)$year]=harvest
  for(y in as.numeric(yrs)) {
     ## sp & process error
     if (!is.null(pe)) {
        if (peMult) sp.=spFn(stock(object),object@params,object@model)[, ac(y)]*pe[, ac(y)] 
        else        sp.=spFn(stock(object),object@params,object@model)[, ac(y)]+pe[, ac(y)]
     }else{
       sp.=qmax(spFn(stock(object),object@params,object@model)[, ac(y)],0)
       }
     
     ## targets 
     if (hcrTrgt)
         object@harvest[,ac(y)]=hcr(window(object,end=y))
  
     if (hvtTrgt | hcrTrgt)
         catch(object)[,ac(y)]=stock(object)[,ac(y)]*relFn(object@harvest,harvest[,ac(y)],lag)
     else if (ctcTrgt){
         catch(object)[,ac(y)]=relFn(catch(object),catch[,ac(y)],lag)
         }
     else 
         catch(object)[,ac(y)]=relFn(stock(object),stock[,ac(y+1)],lag) - stock(object)[,ac(y)] - sp.

     stock(  object)[,ac(y+1)] = stock(  object)[,ac(y)] - catch(object)[,ac(y)] + sp.
     harvest(object)[,ac(y)]   = catch(  object)[,ac(y)] / stock(object)[,ac(y)]

    ### bounds
    if ("catch" %in% bounds){ 
       catch(object)[,y]       =iavFn(window(catch(object),end=y),bnd,lag=1)
       stock(object)[,ac(y+1)] = stock(object)[,ac(y)] - catch(object)[,ac(y)] + sp.
       }

     ### min/max
     object@harvest[, ac(y)]  =maxFn(minFn(object@harvest[,ac(y)],minF),maxF)
     catch(  object)[,ac(y)]  =stock(object)[,ac(y)]*object@harvest[,ac(y)]
     stock(  object)[,ac(y+1)]=stock(object)[,ac(y)]-catch(object)[,ac(y)] + sp.
     }

    stock(  object)[stock(  object) < 0] = 0
    catch(  object)[catch(  object) < 0] = 0

    return(object)}) 

setMethod("fwd", signature(object="aspic",ctrl="FLQuants"),
  function(object, ctrl, pe=NULL, peMult=TRUE,minF=0,maxF=2,lag=0,
           bounds=list(catch=c(Inf,Inf)),...) {
    
  res=aspics(mlply(seq(length(names(ctrl))),
      function(x,object,ctrl,pe,peMult,minF,maxF,lag,bounds){
        print(names(ctrl[x]))
        if (names(ctrl)[x]=="catch")  return(fwd(object,catch  =ctrl[[x]],pe=pe,peMult=peMult,minF=minF,maxF=maxF,lag=lag,bounds=bounds))
        if (names(ctrl)[x]=="harvest")return(fwd(object,harvest=ctrl[[x]],pe=pe,peMult=peMult,minF=minF,maxF=maxF,lag=lag,bounds=bounds))
        if (names(ctrl)[x]=="stock")  return(fwd(object,stock  =ctrl[[x]],pe=pe,peMult=peMult,minF=minF,maxF=maxF,lag=lag,bounds=bounds))},
       object=object,ctrl=ctrl,pe=pe,peMult=peMult,minF=minF,maxF=maxF,lag=lag,bounds=bounds))
      
  names(res)=names(ctrl)
        
  return(res)})
      
setMethod("fwd", signature(object="aspic",ctrl="list"),
  function(object, ctrl, pe=NULL, peMult=TRUE,minF=0,maxF=2,lag=0,
           bounds=list(catch=c(Inf,Inf)),end=range(object,"maxyear")+15,...) {
    
  res=aspics(mlply(seq(length(names(ctrl))),
      function(x,object,ctrl,pe,peMult,minF,maxF,lag,bounds,end){
        print(names(ctrl[x]))
        return(fwd(object,hcr=ctrl[[x]],pe=pe,peMult=peMult,minF=minF,maxF=maxF,lag=lag,bounds=bounds,end=end))},
       object=object,ctrl=ctrl,pe=pe,peMult=peMult,minF=minF,maxF=maxF,lag=lag,bounds=bounds,end=end))
      
  names(res)=names(ctrl)
        
  return(res)})
    
