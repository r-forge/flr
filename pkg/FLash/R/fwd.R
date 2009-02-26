# fwd.R
# FLash/R/fwd.R
# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, Cefas
# Last Change: 27 Nov 2007 19:20
# $Id: fwd.R,v 1.44 2009/02/26 09:26:11 fas00 Exp $

  
## fwd(FLStock)
if (!isGeneric("fwd"))
  setGeneric("fwd", function(object, fleets, ...)
	  standardGeneric("fwd"))

setMethod("fwd", signature(object="FLStock", fleets = "missing"),
    function(object, ctrl,
               sr          =NULL,
               sr.residuals=NULL, sr.residuals.mult=TRUE)
    {
    object<-CheckNor1(object)

    if (!(units(object@harvest)=="f"))
       stop("harvest slot has to have units of type 'f'")
    if (!validObject(ctrl))
       stop("ctrl not valid")
        
    yrs<-as.numeric(sort(unique(ctrl@target[,"year"])))
    
    ## check years
    ## years in ctrl have to be in FLStock object
    if (!all(ac(yrs) %in% ac(dims(object)$minyear:(dims(object)$maxyear))))
       stop("years in ctrl outside of those in stock object")
    ## Need year+1 in FLStock object
    if (!all(ac(yrs) %in% ac(dims(object)$minyear:(dims(object)$maxyear-1))))
       stop("Need to have year+1 in stock object compared to ctrl to estimate survivors")
    ## years in ctrl have to be in order
    if (!all(yrs==sort(yrs)))
       stop("years in ctrl not in order")
    ## no gaps in years
    if (length(min(yrs):max(yrs))!=length(unique(yrs)))
       stop("years in ctrl not contiguous")

    sr<-setSR(sr=sr, object = object, yrs=yrs, sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals.mult)          
    #if (is.character(sr)) stop(sr)

    ## check iters in ctrl are '1 or n' and correct if necessary
    ctrl@trgtArray <- chkTrgtArrayIters(object,ctrl@trgtArray,sr)
#    its<-sort(unique(c(length(dimnames(ctrl@trgtArray)$iter), dims(object)$iter, length(dimnames(sr$params[[1]])$iter), length(dimnames(sr$residuals[[1]])$iter))))
#    if (length(its)>2 | (length(its)>1 & its[1]!=1)) stop("Iters not 1 or n") 
#    if (length(its)==2 & dimnames(ctrl@trgtArray)$iter == 1){
#          dmns<-dimnames(ctrl@trgtArray)
#          dmns$iters<-1:its[2]
#          ctrl@trgtArray<-array(ctrl@trgtArray,dim=unlist(lapply(dmns,length)),dimnames=dmns)}

ctrl@target <- chkTargetQuantity(ctrl@target)
#     if (!is(ctrl@target[,"quantity"],"factor"))
#        ctrl@target[,"quantity"]<-factor(ctrl@target[,"quantity"],quantityNms())
#     if (!all(as.character(ctrl@target[,"quantity"]) %in% quantityNms()))
#         stop("invalid quantity in control target")
         
     if (any(as.character(ctrl@target[,"quantity"]) %in% c("effort","costs","revenue","profit")))
         stop("fwd(FLStock) not implemented for 'effort','costs','revenue' or 'profit'")         

    stock.n(object)[1,ac(min(ctrl@target[,"year"]))]<-NA
          
    x<-.Call("_fwd_adolc_FLStock", object, matrixTarget(ctrl@target), ctrl@trgtArray, yrs, sr$model, sr$params, sr$residuals, sr$residual.mult)

    #if (is.numeric(x)) stop(x)
    
    units(x@harvest)<-"f"

    stock.n(x)[is.na(stock.n(x))]<-0.0

    catch(   x)<-computeCatch(   x)
    landings(x)<-computeLandings(x)
    discards(x)<-computeDiscards(x)
    catch(   x)<-computeStock(   x)
    name(    x)<-name(object)
    desc(    x)<-desc(object)

    return(x)
    }) 

#************ FLBiol and fleet methods ***********************************************

setMethod("fwd", signature(object='FLBiol', fleets='FLCatch'),
    function(object,fleets,ctrl,
                sr           =NULL,
                sr.residuals =NULL, sr.residuals.mult=TRUE)
{                         
    object<-FLBiols(object)
    if (length(object)>1) stop("Only implemented for 1 FLBiol")
    fleets <- FLFleets(FLFleet(fleets))
    fwd(object=object, fleets=fleets, ctrl=ctrl,sr=sr,sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals) 
}
)

setMethod("fwd", signature(object='FLBiol', fleets='FLFleet'),
    function(object,fleets,ctrl,
                sr           =NULL,
                sr.residuals =NULL, sr.residuals.mult=TRUE)
{                         
    object<-FLBiols(object)
    if (length(object)>1) stop("Only implemented for 1 FLBiol")
    fwd(object = object, fleets = FLFleets(fleets), ctrl=ctrl, sr=sr,sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals) 
}
)

setMethod("fwd", signature(object='FLBiol', fleets='FLFleets'),
    function(object,fleets,ctrl,
                sr           =NULL,
                sr.residuals =NULL, sr.residuals.mult=TRUE)
   {                         
    object<-FLBiols(object)
    if (length(object)>1) stop("Only implemented for 1 FLBiol")
    fwd(object = object, fleets = fleets(fleets), ctrl=ctrl, sr=sr,sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals) 

})

setMethod("fwd", signature(object='FLBiols', fleets='FLCatch'),
    function(object,fleets,ctrl,
                sr           =NULL,
                sr.residuals =NULL, sr.residuals.mult=TRUE)
{                         
    if (length(object)>1) stop("Only implemented for 1 FLBiol")
    fleets <- FLFleets(FLFleet(fleets))
    fwd(object=object, fleets=fleets, ctrl=ctrl,sr=sr,sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals) 
}
)

setMethod("fwd", signature(object='FLBiols', fleets='FLFleet'),
    function(object,fleets,ctrl,
                sr           =NULL,
                sr.residuals =NULL, sr.residuals.mult=TRUE)
{                         
    if (length(object)>1) stop("Only implemented for 1 FLBiol")
    fwd(object = object, fleets = FLFleets(fleets), ctrl=ctrl, sr=sr,sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals) 
}
)

# The main one
setMethod("fwd", signature(object='FLBiols', fleets='FLFleets'),
    function(object,fleets,ctrl,
                sr           =NULL,
                sr.residuals =NULL, sr.residuals.mult=TRUE)
   {                         

    # Turn biol into FLBiols
#    object<-FLBiols(object)
    if (length(object)>1) stop("Only implemented for 1 FLBiol")
    biol  <-CheckNor1(object)

#    ## Sort out fleets input into FLFleets 
#    if (is(fleets,"FLCatch")){
#        fleets<-FLFleets(FLFleet())
#        fleets[[1]]@metiers[[1]]<-as(fleets,"FLMetier")}
#   if (is(fleets,"FLFleet"))
#      fleets<-FLFleets(fleets)
#   #if (!is(fleets,"FLFleets"))
#   #   stop("'fleets' not of type FLFleet(s)")     
    if (length(fleets)>1) stop("Only implemented for 1 FLFleet")
    fleets<-CheckNor1(fleets)

    yrs<-as.numeric(sort(unique(ctrl@target[,"year"])))
    sr<-setSR(sr=sr, object=biol[[1]], yrs=yrs, sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals.mult)          

    if (!is(ctrl,"fwdControl")) stop("ctrl not a valid 'fwdControl' object")

    # check iters in residuals, biol and ctrl@trgtArray are 1 or n and correct trgtArray if necessary
ctrl@trgtArray <- chkTrgtArrayIters(object,ctrl@trgtArray,sr)
#    its<-(unique(c(length(dimnames(ctrl@trgtArray)$iters), dims(object[[1]])$iters, length(dimnames(sr.residuals)$iter))))
#    if (length(its)>2 | (length(its)>1 & its[1]!=1)) stop("Iters not 1 or n")
#    if (length(its)==2 & dimnames(ctrl@trgtArray)$iter == 1){
#          dmns<-dimnames(ctrl@trgtArray)
#          dmns$iters<-1:its[2]
#          ctrl@trgtArray<-array(ctrl@trgtArray,dim=unlist(lapply(dmns,length)),dimnames=dmns)}

ctrl@target <- chkTargetQuantity(ctrl@target)
#     if (!is(ctrl@target[,"quantity"],"factor"))
#        ctrl@target[,"quantity"]<-factor(ctrl@target[,"quantity"],quantityNms())
#     if (!all(as.character(ctrl@target[,"quantity"]) %in% quantityNms()))
#         stop("invalid quantity in control target")

   if(!(length(slot(ctrl, 'effort'))>0) & length(fleets)==1){
      ctrl@effArray  <-ctrl@trgtArray
      ctrl@effArray[]<-NA
      
      ctrl@effort <-data.frame(year=ctrl@target[,"year"],min=NA,val=NA,max=NA,fleet=1,metier=1,
                               rel.year=NA,rel.fleet=NA,rel.metier=NA,rel.bound=NA)

      }
   if (!validObject(ctrl))
       stop("ctrl not a valid 'fwdControl' object")


    #browser()


   ##check dims of fleet and biol
   dms<-chkDms(biol[[1]],fleets[[1]],yrs)
   #if (class(dms)=="character") 
   #   stop(dms)
      
   ## Check iters are 1 or n
   iters<-unique(c(unlist(lapply(fleets, function(x) dims(x)$iters)),
                   unlist(lapply(biol, function(x) dims(x)$iter))))
   if ((length(iters) >2) | (length(iters)==2 & !(1 %in% iters)))
      stop("Iters need to be '1 or n'")

   if (!all(unlist(dims(biol[[1]])[  c("minyear","maxyear","seasons","areas", "iters")])==
            unlist(dims(fleets[[1]])[c("minyear","maxyear","seasons","areas","iters")])))
      stop("years, seasons, areas and iters have to match in FLBiol & FLFleet")
   
   ## needed cos m plays a big role in the C++ code
   for (i in 1:length(biol))
     if (dims(m(biol[[i]]))$iter==1 && dims(biol[[i]])$iter>dims(m(biol[[i]]))$iter)
        m(biol[[i]])<-propagate(m(biol[[i]]),iter=dims(biol[[i]])$iter)
   
   ## FLSR    
#   if (is(sr,"FLSR")){
#      sr.model <-NULL
#      sr.params<-NULL}
#   else if (is(sr,"list") & all(c("model","params") %in% names(sr))){  
#      sr.model <-sr$model
#      sr.params<-sr$params
#      sr       <-NULL}   
#   else 
#      stop("sr has to either be an FLSR object or a list with items 'model' & 'params'")
#      
#   if (!is.null(sr.residuals))
#      if(dims(sr.residuals)$iter != dms["iters"])
#         stop("iters in object and sr.residuals don't match")
    #sr<-setSR(sr=sr, object = object, yrs=yrs, sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals.mult)          
#browser()
#    sr<-setSR(sr=sr, object=biol[[1]], yrs=yrs, sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals.mult)          
   #if (is.character(sr)) 
   #   stop(sr)

   dms<-dms[c("minyear","maxyear","units","seasons","areas","iters")]

   for (i in 1:length(biol))
      if ((yrs[length(yrs)] == dims(biol[[i]])$maxyear)+1  )
         biol[[i]]<-window(biol[[i]],start=biol[[i]]@range["minyear"],end=yrs[length(yrs)]+1)

   res<-.Call("fwd_adolc_FLBiols", biol, fleets,
                                   matrixTarget(ctrl@target), ctrl@trgtArray,
                                   matrixEffort(ctrl@effort), ctrl@effArray,
                                   yrs, dms, sr)


   names(res)<-c("landings.n","discards.n","effort","n","f","catch.n")

   return(res)
   })

#***************************************************      

# This needs checking...
#setMethod("fwd", signature(object="character"),
#       function(object,fltNm,ctrl,
#                sr           =NULL,
#                sr.residuals =NULL, sr.residuals.mult=TRUE)
#   {
#
#    biol<-get(object,envir = as.environment(parent.frame()))
#    if (!is(biol,"FLBiol")) stop("named object needs to be of type FLBiol")
#
#    if (is(fltNm,"character"))
#        fleet<-get(fltNm,envir = as.environment(parent.frame()))
#    else stop("fltNm not character")
#    if (!is(fleet,"FLFLeet")) stop("named object needs to be of type FLFleet")
#   
#   res<-fwd(biol,fleets,ctrl,sr,sr.residuals,sr.residuals.mult)
#
#   n(biol              )[,dimnames(res$n         )$year]<-res$n
#   landings.n(fleet,1,1)[,dimnames(res$landings.n)$year]<-res$landings.n
#   discards.n(fleet,1,1)[,dimnames(res$discards.n)$year]<-res$discards.n
#   landings(  fleet,1,1)[,dimnames(res$landings.n)$year]<-apply(res$landings.n*landings.wt(fleet,1,1)[,dimnames(res$landings.n)$year],2:6,sum)
#   discards(  fleet,1,1)[,dimnames(res$discards.n)$year]<-apply(res$discards.n*discards.wt(fleet,1,1)[,dimnames(res$discards.n)$year],2:6,sum)
#   effort(fleet        )[,dimnames(res$effort    )$year]<-res$effort
#
#   name(    x)<-name(object)
#   desc(    x)<-desc(object)
#
#   assign(object,biol, envir=as.environment(parent.frame()))
#   assign(fltNm, fleet,envir=as.environment(parent.frame()))
#
#   invisible(res)
#   })
#
