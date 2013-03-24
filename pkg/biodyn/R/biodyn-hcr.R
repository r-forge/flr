##############################################################
#' tac , 
#' 
#' Calculates the Total Allowable Catch for a given harvest rate and stock biomass
#'
#' @param  \code{object}, an object of class \code{biodyn} or
#' @param  \code{harvest}, an \code{FLQuant} object with harvest rate
#' @return \code{FLQuant} object with TAC value(s)
#' 
#' @seealso \code{\link{hcr}},  \code{\link{fwd}}
#' 
#' @export
#' @docType methods
#' @rdname tac
#'
#' @examples
#' #tac("logistic",FLPar(msy=100,k=500))
#'
setMethod( 'tac', signature(object='biodyn'),
           function(object,harvest,...){
             
             yr           =dims(harvest)$maxyear
             catch(object)=propagate(catch(object),dims(object)$iter)
             object       =fwd(window(object,end=yr), harvest=harvest)
             
             return(catch(object)[,ac(yr)])})

##############################################################
#' hcrParams 
#' 
#' Combines reference points into the HCR breakpts
#'
#' @param  \code{ftar}, an object of class \code{FLPar}
#' @param  \code{btrig}, an object of class \code{FLPar}
#' @param  \code{fmin}, an object of class \code{FLPar}
#' @param  \code{blim}, an object of class \code{FLPar}
#' 
#' @seealso \code{\link{hcr}},  \code{\link{hcrPlot}}
#' 
#' @export
#' @docType methods
#' @rdname tac
#'
#' @examples
#' #tac("logistic",FLPar(msy=100,k=500))
#'
hcrParams=function(ftar,btrig,fmin,blim){
  
  setNms=function(x,nm,nits){
    
    names(dimnames(x))[1]="params"
    dimnames(x)[[1]]     =nm
    if (nits!=dims(x)$iter)
      x=propagate(x,nits)
    
    return(x)}
  
  nits=max(laply(list(ftar,btrig,fmin,blim), function(x) dims(x)$iter))
  
  ftar =setNms(ftar, nm="ftar", nits)
  btrig=setNms(btrig,nm="btrig",nits)
  fmin =setNms(fmin, nm="fmin", nits)
  blim =setNms(blim, nm="blim", nits)
  
  if (nits==1) res=FLPar(  array(c(ftar,btrig,fmin,blim),c(4,nits),dimnames=list(params=c("ftar","btrig","fmin","blim"),iter=seq(nits)))) else
               res=FLPar(t(array(c(ftar,btrig,fmin,blim),c(nits,4),dimnames=list(iter=seq(nits),params=c("ftar","btrig","fmin","blim")))))
  
  return(as(res,"FLQuant"))}

##############################################################
#' HCR
#' 
#' Harvest Control Rule, calculates F, or Total Allowable Catch (TAC) based on a hockey stock harvest control rule.
#'
#' @param  \code{object} an object of class \code{biodyn} or
#' @param  \code{params} \code{FLPar} object with hockey stock HCR parameters
#' @param  \code{refYrs}, numeric vector with years used to for stock/ssb in HCR
#' @param  \code{yrs}, numeric vector with yrs for HCR prediction
#' @param  \code{tac}, \code{logical} should return value be TAC rather than F?
#' @param  \code{bndF}, \code{vector} with bounds on iter-annual variability on  F
#' @param  \code{bndTac}, \code{vector} with bounds on iter-annual variability on TAC
#' 
#' @return a \code{FLPar} object with value(s) for F or TAC if tac==TRUE
#' 
#' @seealso \code{\link{bmsy}}, \code{\link{fmsy}}, \code{\link{fwd}}, \code{\link{hcr} and \code{\link{hcrParams}}
#' 
#' @export
#' @docType methods
#' @rdname hcr
#'
#' @examples
#' #hcr("logistic",FLPar(msy=100,k=500))
#'
setMethod('hcr', signature(object='biodyn'),
  function(object, 
           params=hcrParams(ftar =0.80*refpts(object)["fmsy"],
                            btrig=0.80*refpts(object)["bmsy"],
                            fmin =0.02*refpts(object)["fmsy"],
                            blim =0.20*refpts(object)["bmsy"]),
           yrs   =dims(object)$maxyear+1,
           refYrs=dims(object)$maxyear,
           tac   =FALSE,
           bndF  =NULL, #c(1,Inf),
           bndTac=NULL, #c(1,Inf),
           ...) {

  ## HCR
  #if (blim>=btrig) stop("btrig must be greater than blim")
  a=(params["ftar"]-params["fmin"])/(params["btrig"]-params["blim"])
  b=params["ftar"]-a*params["btrig"]

  ## Calc F
  # bug
  #val=(SSB%*%a) %+% b

  ssb=FLCore:::apply(stock(object)[,ac(refYrs)],6,mean)
  #ssb=stock(object)[,ac(refYrs)]
  rtn=(ssb%*%a)    
  rtn=rtn%+%b
  
  fmin=as(params["fmin"],"FLQuant")
  ftar=as(params["ftar"],"FLQuant")

  for (i in seq(dims(rtn)$iter)){
    iter(rtn,i)[]=max(iter(rtn,i),iter(fmin,i))
    iter(rtn,i)[]=min(iter(rtn,i),iter(ftar,i))} 
  dimnames(rtn)$year=min(yrs)  
  
  
  if (length(yrs)>1){
    rtn=window(rtn,end=max(yrs))
    rtn[,ac(yrs)]=rtn[,ac(min(yrs))]}

  ### Bounds ##################################################################################
  ## F
  if (!is.null(bndF)){  
      rtn[,ac(min(yrs))]=qmax(rtn[,ac(min(yrs))],harvest(object)[,ac(min(yrs)-1)]*(1-bndF[1]))
      rtn[,ac(min(yrs))]=qmin(rtn[,ac(min(yrs))],harvest(object)[,ac(min(yrs)-1)]*(1+bndF[1]))
      
      if (length(yrs)>1)        
        for (i in yrs[-1]){
          rtn[,ac(i)]=qmax(rtn[,ac(i)],rtn[,ac(i-1)]*(1-bndF[1]))
          rtn[,ac(i)]=qmin(rtn[,ac(i)],rtn[,ac(i-1)]*(1+bndF[1]))}}
  
   ## TAC
   if (tac){
      args=list(...)
      if ("catch" %in% names(args))   object=fwd(object,catch  =args[["catch"]]) else
      if ("harvest" %in% names(args)) object=fwd(object,harvest=args[["harvest"]])
      rtn=tac(object,harvest=rtn)    
    
      
      if (!is.null(bndTac)){  
        rtn[,ac(min(yrs))]=qmax(rtn[,ac(min(yrs))],catch(object)[,ac(min(yrs)-1)]*(1-bndF[1]))
        rtn[,ac(min(yrs))]=qmin(rtn[,ac(min(yrs))],catch(object)[,ac(min(yrs)-1)]*(1+bndF[1]))
        
        if (length(yrs)>1)        
          for (i in yrs[-1]){
            rtn[,ac(i)]=qmax(rtn[,ac(i)],rtn[,ac(i-1)]*(1-bndTac[1]))
            rtn[,ac(i)]=qmin(rtn[,ac(i)],rtn[,ac(i-1)]*(1+bndTac[1]))}}          
       }
  
  return(rtn)})

##############################################################
#' hcrPlot
#'
#' Calculates break pointts for a hockey stick HCR
#'
#' @param  \code{object} an object of class \code{biodyn} or
#' @param  \code{params} \code{FLPar} object with hockey stock HCR parameters
#' 
#' @return a \code{FLPar} object with value(s) for HCR
#' 
#' @seealso \code{\link{hcr}},  \code{\link{msy}},  \code{\link{bmsy}}, \code{\link{fmsy}} and  \code{\link{refpts}}
#' 
#' @export
#' @rdname hcrPlot
#'
#' @examples
#' hcrPlot(object)
setMethod('hcrPlot', signature(object='biodyn'),
  function(object,params=FLPar(ftar=0.7, btrig=0.7, fmin=0.025, blim=0.20) ,maxB=1){
  
  pts=rbind(cbind(refpt="Target",model.frame(rbind(bmsy(object)*c(params["btrig"]),
                                                   fmsy(object)*c(params["ftar"])))),
            cbind(refpt="Limit", model.frame(rbind(bmsy(object)*c(params["blim"]),
                                                   fmsy(object)*c(params["fmin"])))))
  pts.=pts
  pts.[1,"bmsy"]=params(object)["k"]*maxB
  pts.[2,"bmsy"]=0
  pts.[,1]=c("")
  
  pts=rbind(pts.[1,],pts[1:2,],pts.[2,])
  
  names(pts)[2:3]=c("biomass","harvest")
  pts[,"biomass"]=pts[,"biomass"]/bmsy(object)
  pts[,"harvest"]=pts[,"harvest"]/fmsy(object)
  
  pts})