# fwd.R - 
# FLBioDym/R/fwd.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

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
  res[val>bnd]=val[val>bnd]
 
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

# fwd(FLBioDym) {{{
setMethod("fwd", signature(object="FLBioDym",ctrl="missing"),
 function(object, catch=NULL, harvest=NULL, stock=NULL, hcr=NULL, pe=NULL, peMult=TRUE,minF=0,maxF=2,lag=0,
           bounds=list(catch=c(Inf,Inf)),end=NULL,...) {
#  object =simFLBioDym()
#  harvest=FLQuant(0.9,dimnames=list(year=50:80))
#  catch  =NULL
#  stock  =NULL
#  pe     =NULL
#  pwMult =TRUE
#  minF   =0
#  maxF   =2
#  lag    =1
#  bounds=list(catch=c(Inf,Inf))

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
      if (dims(catch(object))$iter==1 & dims(catch)$iter>1) catch(object)=propagate(catch(object), dims(catch)$iter)
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
  if (as.numeric(yrs[1]) == range(object,"minyear")){
     if (!("year" %in% names(dimnames(params(object)["K"]))))  
       stock(object)[,ac(range(object,"minyear"))] = params(object)["K"] * params(object)["b0"] else
       stock(object)[,ac(range(object,"minyear"))] = params(object)["K",ac(range(object,"minyear"))] * params(object)["b0",ac(range(object,"minyear"))]          
     }
  
  ## maxyear
  if (max(as.numeric(yrs)) == range(object,"maxyear"))
     stock(object) <- window(stock(object),end=range(object,"maxyear")+1)

  ## niters
  nits=dims(object)$iter
  if (hvtTrgt) nits=max(nits,dims(harvest)$iter)
  if (ctcTrgt) nits=max(nits,dims(  catch)$iter)
  if (stkTrgt) nits=max(nits,dims(  stock)$iter)
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
  if (!hvtTrgt) harvest=harvest(object)
  for(y in as.numeric(yrs)) {
     ## sp & process error
     if (!is.null(pe)) {
      
        if (peMult) sp.=sp(object)[, ac(y)]*pe[, ac(y)] 
        else        sp.=sp(object)[, ac(y)]+pe[, ac(y)]
     }else sp.=sp(object)[, ac(y)]

     ## targets 
     if (hcrTrgt)
         harvest[,ac(y)]=hcr(window(object,end=y))
     if (hvtTrgt | hcrTrgt)
         catch(object)[,ac(y)]=stock(object)[,ac(y)]*relFn(harvest(object),harvest[,ac(y)],lag)
     else if (ctcTrgt)
         catch(object)[,ac(y)]=relFn(catch(object),catch[,ac(y)],lag)
     else 
         catch(object)[,ac(y)]=relFn(stock(object),stock[,ac(y+1)],lag) - stock(object)[,ac(y)] - sp.

     stock(object)[,ac(y+1)] = stock(object)[,ac(y)] - catch(object)[,ac(y)] + sp.
          
     ### bounds
#      if ("catch" %in% bounds){ 
#           catch(object)[,y]=iavFn(window(catch(object),end=y),bnd,lag=1)
#           stock(object)[,ac(y+1)] = stock(object)[,ac(y)] - catch(object)[,ac(y)] + sp.
#           }
 
      ### min/max
      harvest[,ac(y)]        =maxFn(minFn(harvest(object)[,ac(y)],minF),maxF)
      catch(  object)[,ac(y)]=stock(object)[,ac(y)]*harvest[,ac(y)]
      stock(object)[,ac(y+1)]=stock(object)[,ac(y)] - catch(object)[,ac(y)] + sp.
     }

    stock(  object)[stock(  object) < 0] = 0
    catch(  object)[catch(  object) < 0] = 0

    return(object)}) 

setMethod("fwd", signature(object="FLBioDym",ctrl="FLQuants"),
  function(object, ctrl, pe=NULL, peMult=TRUE,minF=0,maxF=2,lag=0,
           bounds=list(catch=c(Inf,Inf)),...) {
    
  res=FLBioDyms(mlply(seq(length(names(ctrl))),
      function(x,object,ctrl,pe,peMult,minF,maxF,lag,bounds){
        print(names(ctrl[x]))
        if (names(ctrl)[x]=="catch")  return(fwd(object,catch  =ctrl[[x]],pe=pe,peMult=peMult,minF=minF,maxF=maxF,lag=lag,bounds=bounds))
        if (names(ctrl)[x]=="harvest")return(fwd(object,harvest=ctrl[[x]],pe=pe,peMult=peMult,minF=minF,maxF=maxF,lag=lag,bounds=bounds))
        if (names(ctrl)[x]=="stock")  return(fwd(object,stock  =ctrl[[x]],pe=pe,peMult=peMult,minF=minF,maxF=maxF,lag=lag,bounds=bounds))},
       object=object,ctrl=ctrl,pe=pe,peMult=peMult,minF=minF,maxF=maxF,lag=lag,bounds=bounds))
      
  names(res)=names(ctrl)
        
  return(res)})
      
setMethod("fwd", signature(object="FLBioDym",ctrl="list"),
  function(object, ctrl, pe=NULL, peMult=TRUE,minF=0,maxF=2,lag=0,
           bounds=list(catch=c(Inf,Inf)),end=range(object,"maxyear")+15,...) {
    
  res=FLBioDyms(mlply(seq(length(names(ctrl))),
      function(x,object,ctrl,pe,peMult,minF,maxF,lag,bounds,end){
        print(names(ctrl[x]))
        return(fwd(object,hcr=ctrl[[x]],pe=pe,peMult=peMult,minF=minF,maxF=maxF,lag=lag,bounds=bounds,end=end))},
       object=object,ctrl=ctrl,pe=pe,peMult=peMult,minF=minF,maxF=maxF,lag=lag,bounds=bounds,end=end))
      
  names(res)=names(ctrl)
        
  return(res)})
      


    
