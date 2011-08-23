################################################################################
# MSE functions                                                                #
################################################################################

#### Initial Stuff #############################################################
## FLR Packages used
library(FLCore)
library(FLash)
library(FLBRP)
library(FLXSA)

#### Observation Error Models ##################################################
# These generate psuedo data from the OM for use by the MP                     #
################################################################################

#### OEM for the stock
OEMStock<-function(OM,start,end=NULL,plusgroup=NULL,...){

    ## determines sampling years
    if (missing(end))
       end<-start
    stk<-trim(OM, year=start:end)
    yrs<-as.character(start:end)

    ## replace any slots desired
    args <- list(...)
    slt<-names(getSlots("FLStock"))[getSlots("FLStock")=="FLQuant"]
    for(i in names(args)[names(args) %in% slt]){
       yrs.      <-yrs[yrs %in% dimnames(slot(stk, i))$year]

       if (dims(slot(stk, i))$iter!=dims(args[[i]])$iter){
         if (dims(slot(stk, i))$iter==1 & dims(args[[i]])$iter>1)
            slot(stk, i)=propagate(slot(stk, i), dims(args[[i]])$iter)
         else stop("iter mismatch")}

      slot(stk, i)[,yrs.]<-args[[i]][,yrs.]}

    ## does plusgroup calculation
    if (!is.null(plusgroup))
       stk<-setPlusGroup(stk,plusgroup)

    return(stk)}

#### OEM for CPUE
OEMIndex<-function(stk,start,end=NULL,plusgroup=NULL,startf=0,endf=0.01,deviates=NULL){

    ## unbiased population estimates

    ## determines sampling years
    if (missing(end))
       end<-start
    stk<-trim(stk, year=start:end)
    yrs<-as.character(start:end)

    if (!is.null(plusgroup))
       stk<-setPlusGroup(stk,plusgroup)@n

     idx<-as(stk,"FLIndex")

     if (!is.null(startf)) idx@range["startf"]<-startf
     if (!is.null(endf))   idx@range["endf"]  <-endf

     if (!is.null(deviates))
        idx@index<-idx@index*deviates[dimnames(idx@index)$age,ac(yrs)]

     return(idx)}
     
### cas simulator
casSimulator<-function(flq,Linf,K,t0=0,CV=0.3,niters=100){

      la<-function(age,Linf,K,t0) Linf*(1-exp(-K*(age-t0)))

   dmns     <-dimnames(flq)
   dmns$unit<-1:niters
   cas      <-FLQuant(la(as.numeric(dmns$age),Linf,K,t0),dimnames=dmns)
   cas      <-cas*rlnorm(prod(dim(cas)),0,CV)

   return(cas)}

## function to apply a linearly increasing trend to an FLQuant
biasLinear<-function(x,obj){
   if (x>0)
      res  <-1-(sort(cumsum(rep(x, dims(obj)$year)),d=T)-x)
   else
      res  <-sort(1-(sort(cumsum(rep(x, dims(obj)$year)),d=T)-x),d=T)

   return(obj*FLQuant(rep(res,each=dims(obj)$age),dimnames=dimnames(obj)))}

## VPA Based MP
vpaMP<-function(MP,iYr,CV=0.25){
   harvest(MP)[,ac(iYr)]<-apply(harvest(MP)[,ac(iYr-(0:2))]*rlnorm(prod(unlist(dims(stock.n(MP)))[c(1,10)]),0,CV),c(1,6),mean)
   
   harvest(MP)[,ac(iYr)]<-harvest(MP)[,ac(iYr)]*.90
#   harvest(MP)[,ac(iYr)]<-apply(harvest(MP)[,ac(iYr-(0:2))]*rlnorm(prod(unlist(dims(stock.n(MP)))[c(1,10)]),0,CV),c(1,6),mean)
   MP<-MP+VPA(MP,fratio=1)

   return(MP)}

## PA HCR
hcrF<-function(SSB,Bpa,Blim,Fmin,Fmax){
    val <-qmax(qmin((Fmax-(Fmax-Fmin)*(Bpa-SSB)/(Bpa-Blim)),Fmax),Fmin)

    return(val)}

runMSE<-function(OM,start,srPar,srRsdl=srRsdl,fmult=1){
  
OM=window(albPrj[[1]],start=1990,end=2021)
start=2006
srPar=params(albBrp[[1]])
srRsdl=FLQuant(1.0,dimnames=dimnames(m(albPrj[[1]])[1]))

  ## Get number of iterations in OM
  nits <-dims(OM)$iter

  ## XSA options for running
  XSActrl<-FLXSA.control(maxit=20,tsrange=100,tspower=0)

  #### Observation Error (OEM) setup
  dmns  <- dimnames(stock.n(OM))

  ## Random variation for CPUE, CV=0.25%
  idxDev<-FLQuant(rlnorm( prod(unlist(lapply(dmns,length))),0,.25),dimnames=dmns)

  ## Assume same variability on catch-at-age
  ctcDev<-sweep(idxDev[dimnames(catch.n(OM))$age,dimnames(catch.n(OM))$year],c(1:2,6),catch.n(OM),"*")

  ## Sample from Operating Model
  MPstk <-OEMStock(OM,start=range(OM,"minyear"),end=range(OM,"maxyear"),catch.n=ctcDev,landings.n=ctcDev)
  MPidx <-OEMIndex(OM,start=range(OM,"minyear"),end=range(OM,"maxyear"),startf=0,endf=0.0,deviates=idxDev)

  ## Loop round years
  for (iYr in start:(range(OM,"maxyear")-2)){
     cat("===================", iYr, "===================\n")
     #### OEM, i.e. sample from OM with error
     ctcDev                <-sweep(catch.n(OM),c(1:2,6),idxDev[dimnames(catch.n(OM))$age,dimnames(catch.n(OM))$year],"*")
     
     MPstk                 <-OEMStock(OM,start=range(OM,"minyear"),end=range(OM,"maxyear"),catch.n=ctcDev,landings.n=ctcDev)
     MPstk                 <-window(MPstk,end  =iYr)
     MPstk[,ac(iYr)]       <-OEMStock(OM, start=iYr)
     MPidx                 <-window(MPidx,end  =iYr)
     MPidx@index[,ac(iYr)] <-OEMIndex(OM,start=iYr,deviates=idxDev)@index

     #### Stock Assessment
     MPstk      <-MPstk+FLXSA(MPstk,FLIndices(MPidx),XSActrl,diag.flag=FALSE)

     #### In 1st year calculate reference points
     if (iYr==start)
        MPbrp      <-brp(FLBRP(MPstk))

     #### Calculate TAC using fwd ##############################################
     MP  <-stf(MPstk,nyears=2)

     #### Project to TAC year
     ctrl<-fwdControl(data.frame(year=1:2+iYr,quantity=c("f","f")))

     #### calc TAC
     dms     <-dimnames(ctrl@trgtArray)
     dms$iter<-1:nits
     ctrl@trgtArray<-array(NA,lapply(dms,length),dms)
     ctrl@trgtArray[1,"val", ]<-mean(fbar(MP)[,ac(iYr-(1:3)),drop=T])
     ctrl@trgtArray[2,"val", ]<-MPbrp@refpts["msy","harvest",,drop=T]*fmult

     SRrs<-FLQuant(c(apply(rec(MP)[,ac(range(OM,"minyear"):(iYr-1))],6,function(x) exp(mean(log(x))))),dimnames=list(year=0:2+iYr,iter=1:nits))

     MP  <-fwd(MP,ctrl=ctrl,sr=list(model="mean",params=FLPar(1)),sr.residuals=SRrs)

     TAC<-catch(MP)[,ac(iYr+2),drop=T]
     ###########################################################################
     
     #### Now you have TAC take it from OM
     ctrl    <-fwdControl(data.frame(year=iYr+2,max=c(NA,2),quantity=c("catch","f")))
     dms     <-dimnames(ctrl@trgtArray)
     dms$iter<-1:nits
     ctrl@trgtArray<-array(NA,lapply(dms,length),dms)
     ctrl@trgtArray[1,"val", ]<-TAC
     ctrl@trgtArray[2,"max", ]<-2.0

     ctrl@target[1,"val"]=TAC
     ctrl@target[2,"val"]=2.0
     
     OM <-fwd(OM,ctrl=ctrl,sr=list(model="bevholt",params=srPar),sr.residuals=srRsdl)

     print(plot(lapply(FLStocks(OM,MP),window,end=iYr+2)))
     }

   #plot(plot(lapply(FLStocks(OM,MP),window,end=iYr+2)))

   invisible(list(OM=OM,MP=MP))}
