# List of vectors of slots to sort out what slots can vary and which are constant.
# This is for checking / forcing some slots to have 1 or multiple iterations
# Only the Var slots can have multiple iterations

flqVar <- function()
{
    res <- list()
    res[["FLStock"]] <- c("stock", "stock.n", "catch", "catch.n", "discards", "landings", "discards.n", "landings.n", "harvest")
    res[["FLIndex"]] <- c("index", "catch.n")
    res[["FLBiol"]] <-c("n")
    res[["FLFleet"]] <-c("effort")
    res[["FLMetier"]]<- c("effshare", "vcost")
    res[["FLCatch"]] <- c("landings", "landings.n", "discards", "discards.n")
    return(res)
}

flqCon <- function()
{
    res<-list()
    res[["FLStock"]] <-c("catch.wt", "discards.wt", "landings.wt", "stock.wt", "m", "mat", "harvest.spwn", "m.spwn")
    res[["FLIndex"]] <-c("index.var", "catch.wt", "effort", "sel.pattern", "index.q")
    res[["FLBiol"]]  <-c("m", "wt", "fec", "spwn")
    res[["FLFleet"]] <-c("fcost", "capacity", "crewshare")
    res[["FLMetier"]]<-NULL
    res[["FLCatch"]] <-c("discards.wt", "landings.wt", "landings.sel", "discards.sel", "catch.q", "price")
    return(res)
}

CheckNor1<-function(x,Var=flqVar(),Con=flqCon())
   {
   if (!validObject(x)) stop("Object not valid")

   cls  <-class(x)
   nmFlq<-names(Var)
   
   if (!(cls %in% nmFlq)) return(x)
   
   if (dims(x)$iter>1)
      for (i in Var[[class(x)]])
         if (dims(slot(x,i))$iter==1) slot(x,i)<-propagate(slot(x,i),iter=dims(x)$iter)
   else if (is(x,"FLlst"))
      for (i in 1:length(x))
         x[[i]]<-CheckNor1(x[[i]])

   return(x)
   }

# Used for burrowing into a Fleet list and checking the dims
chkFLlst<-function(x){
  snms    <-names(getSlots(class(x)))
  anyFLlst<-unlist(lapply(snms, function(arg) is(slot(x,arg),"FLlst")))
  if (any(anyFLlst))
    chkFLlst(slot(x,snms[anyFLlst]))
  }
# fwd.R
# FLash/R/fwd.R
# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, Cefas
# Last Change: 06 Mar 2009 19:17
# $Id: fwdControl.R 366 2009-10-26 09:37:13Z lauriekell $

trgtNms    <-function() return(c("year","min","val","max","quantity","season","area","unit","spp","fleet","metier","rel.year","rel.season","rel.area","rel.unit"))
effNms     <-function() return(c("year","min","val","max","fleet","metier","rel.year","rel.fleet","rel.metier","rel.bound"))
quantityNms<-function() return(c("ssb","biomass","catch","landings","discards","f","z","f.landings","f.discards","effort","costs","revenue","profit","mnsz"))

validFwdControl <- function(object){
	return(TRUE)

  if (dim(object@target)[1]!=dim(object@trgtArray)[1]){
     warning("rows in target & trgtArray don't match")
     return(FALSE)}

  if (any(object@target[,"quantity"] %in% names(quantity))){
     warning("quantity not recognised")
     return(FALSE)}

  if (length(slot(object, 'effort'))>0){
     if (dim(object@effort)[1]!=dim(object@effArray)[1]){
        warning("rows in effort & effArray don't match")
        return(FALSE)}

     if (dim(object@target)[1]!=dim(object@effort)[1]){
        warning("rows in target & effort don't match")
        return(FALSE)}

     if (dim(object@trgtArray)[3]!=dim(object@effArray)[3]){
        warning("iter in trgtArray & effArray don't match")
        return(FALSE)}
     }

	# Everything is fine
	return(TRUE)
  }

setClass("fwdControl",
	representation(
		target   ="data.frame",
		effort   ="data.frame",
    trgtArray="array",
		effArray ="array",
		block    ="numeric"), ## specifies if mulitple rows are done together
	prototype=prototype(
		target   =data.frame(NULL),
		effort   =data.frame(NULL),
    trgtArray=array(),
		effArray =array(),
		block    =numeric()),
	validity=validFwdControl
  )

if (!isGeneric("fwdControl")) {
	setGeneric("fwdControl", function(object, ...){
		value  <-  standardGeneric("fwdControl")
		value
	})}

setMethod("fwdControl", signature(object="data.frame"),
fwdControl.<-function(object,effort=NULL,trgtArray=NULL,effArray=NULL,...){

    ##### Internal Functions ###################################################
    setArray<-function(x,nrws,nits=NULL,type="trgtArray"){
       if (is(x,"list") & any(names(x) %in% c("min","val","max"))){
         if (!all(lapply(x,class) %in% c("array","matrix","numeric")))
            stop(paste(type,": elements of list neither 'array', 'matrix' or 'numeric'"))

         if (is.null(nits))
            if      (is(x[[1]],"numeric"))                     nits<-length(x[[1]])
            else if (is(x[[1]],"array") | is(x[[1]],"matrix")) nits<-dim(x[[1]])[length(dim(x[[1]]))]
            else stop("")

         res<-array(NA,dim=c(nrws,3,nits),dimnames=list(1:nrws,c("min","val","max"),iter=1:nits))
         if ("val" %in% names(x)){
            if (is.vector(x$val)) x$val<-array(x$val,dim=c(1,length(x$val)))
            if (nits == dim(x$val)[2])
               res[,"val",]<-x$val
            }
         if ("min" %in% names(x)){
            if (is.vector(x$min)) x$min<-array(x$min,dim=c(1,length(x$min)))
            if (nits == dim(x$min)[2])
               res[,"min",]<-x$min
            }
         if ("max" %in% names(x)) {
            if (is.vector(x$max)) x$max<-array(x$max,dim=c(1,length(x$max)))
            if (nits == dim(x$max)[2])
               res[,"max",]<-x$max}
            }
       else if (is(x,"array") & (length(dim(x))==3)){
          if (is.null(nits))
             nits<-dim(x)[3]

          res<-array(NA,dim=c(nrws,3,nits),dimnames=list(1:nrws,c("min","val","max"),iter=1:nits))

          res[dimnames(x)[[1]],dimnames(x)[[2]],]<-x
          }
       else stop("Has to be either a 3D array or list with 'min', 'max' or 'val' vectors")

       return(res)
       }
	# Creates data.frame with desired column names (nms) and no. of rows (no. yrs)
	# Used for creating target and effort dataframes
    df<-function(yrs,nms){
      df<-NULL
      for (i in nms)
         df<-cbind(df,rep(NA,length(yrs)))
      dimnames(df)<-list(1:length(yrs),nms)
      return(data.frame(df))
      }

    checkMinMax<-function(object)
        {
        # check that if max or min specified then no target & vice versa
        if (any((!is.na(object[,"min"]) | !is.na(object[,"max"])) & !is.na(object[,"val"]))) {
           cat("Can't specify val and both a min or max values")
           return(FALSE)}
        else if (any((!is.na(object[,"min"]) & !is.na(object[,"max"])) & object[,"max"]<=object[,"min"])){
           cat("max less than than min value")
           return(FALSE)}
        else
           return(TRUE)
        }
    ##### End Internal Functions ###############################################

    if (!is(object,"data.frame"))
       stop("target not data.frame")

    if (!("year" %in% names(object)))
       stop("year not specified in object")
    yrs<-object[,"year"]

    res<-new("fwdControl")

    ##Targets ##################################################################
    ## Create complete target data frame
    res@target<-df(yrs,trgtNms())
    res@target[,dimnames(object)[[2]]]<-object[,dimnames(object)[[2]]]
    if (!checkTarget(res@target))
       stop("target not valid")

    if (!is.null(trgtArray)){
       res@trgtArray<-setArray(trgtArray,length(yrs),type="trgtArray")
       if (length(dim(res@trgtArray[,1,]))==2){
          res@target[,"min"]<-apply(res@trgtArray[,"min",],1,median)
          res@target[,"max"]<-apply(res@trgtArray[,"max",],1,median)
          res@target[,"val"]<-apply(res@trgtArray[,"val",],1,median)}
      else{
          res@target[,"min"]<-median(res@trgtArray[,"min",])
          res@target[,"max"]<-median(res@trgtArray[,"max",])
          res@target[,"val"]<-median(res@trgtArray[,"val",])}}
    else{
       res@trgtArray<-array(as.numeric(NA),dim=c(length(res@target[,1]),3,1),dimnames=list(1:length(res@target[,1]),c("min","val","max"),iter=1))}

    res@target[,"quantity"]<-factor(res@target[,"quantity"],levels=c("ssb","biomass","catch","landings","discards","f","z","f.landings","f.discards","effort","costs","revenue","profit","mnsz"))

    for (i in 1:length(res@target[,1])){
       if (any(is.na(res@trgtArray[i,"min",]))) res@trgtArray[i,"min",]<-res@target[i,"min"]
       if (any(is.na(res@trgtArray[i,"val",]))) res@trgtArray[i,"val",]<-res@target[i,"val"]
       if (any(is.na(res@trgtArray[i,"max",]))) res@trgtArray[i,"max",]<-res@target[i,"max"]}

    if (!checkMinMax(res@target)) {
       cat(" in target\n")
       stop()}

    ##Effort ###################################################################
    if (!is.null(effort)){
      res@effort<-df(yrs,effNms())
      res@effort[ ,dimnames(effort)[[2]]]<-effort[,dimnames(effort)[[2]]]
      if (!is.null(effArray))
         res@effArray<-setArray(effArray,length(yrs),type="effArray")

      if (!is.null(effArray)){
         res@effArray<-setArray(effArray,length(yrs),type="effArray")
         if (length(dim(res@effArray[,1,]))==2){
            res@effort[,"min"]<-apply(res@effArray[,"min",],1,median)
            res@effort[,"max"]<-apply(res@effArray[,"max",],1,median)
            res@effort[,"val"]<-apply(res@effArray[,"val",],1,median)}
         else{
            res@effort[,"min"]<-median(res@effArray[,"min",])
            res@effort[,"max"]<-median(res@effArray[,"max",])
            res@effort[,"val"]<-median(res@effArray[,"val",])}}
      else
         res@effArray<-array(as.numeric(NA),dim=c(length(res@effort[,1]),3,1),dimnames=list(1:length(res@effort[,1]),c("min","val","max"),iter=1))

    for (i in 1:length(res@effort[,1])){
       if (any(is.na(res@effArray[i,"min",]))) res@effArray[i,"min",]<-res@effort[i,"min"]
       if (any(is.na(res@effArray[i,"val",]))) res@effArray[i,"val",]<-res@effort[i,"val"]
       if (any(is.na(res@effArray[i,"max",]))) res@effArray[i,"max",]<-res@effort[i,"max"]}

    if (!checkMinMax(res@effort)){
       cat(" in effort\n")
       stop()}}

   return(res)
   })

showArray<-function(object){
    if(dim(object)[3] > 1){
		  v1 <- apply(object, 1:2, median, na.rm=TRUE)
  		v2 <- apply(object, 1:2, mad,    na.rm=TRUE)
      v3 <- paste(format(v1,digits=5),"(", format(v2, digits=3), ")", sep="")}
    else
      v3 <- paste(format(apply(object, 1:2, median, na.rm=TRUE),digits=5))

    print(array(v3, dim=dim(object)[1:2], dimnames=dimnames(object)[1:2]), quote=FALSE)

		if(dim(object)[3] != 1)
			cat("iter: ", dim(object)[3],"\n\n")}

setMethod('show', signature(object='fwdControl'),
  function(object){

  showDFTarget<-function(object){

      nm      <-names(object@target)
      optional<-c("season","area","unit","rel.year","rel.season","rel.area","rel.unit")
      flag    <-apply(as.matrix(!is.na(object@target[,optional])),2,any)
      
      print(object@target[,c("year","quantity","min","val","max",names(flag[flag]))])

      cat("\n")}

  showDFEffort<-function(object){

      nm      <-names(object@effort)
      optional<-c("fleet","metier","rel.year","rel.fleet","rel.metier","rel.bound")
      flag    <-apply(as.matrix(!is.na(object@target[,optional])),2,any)

      print(object@effort[,c("year","min","val","max",names(flag[flag]))])

      cat("\n")}

  cat("\nTarget\n")
  showDFTarget(object)
  if (any(!is.na(object@trgtArray)))
     showArray(object@trgtArray)

  if (length(slot(object, 'effort'))>0){
     cat("\n\nEffort\n")
     showDFEffort(object)
     if (any(!is.na(object@effArray)))
        showArray(object@effArray)}
  })

chkFwdControl<-function(ctrl,sr,x,y=NULL){
   if (is(x,"FLStock")){

      return(ctrl)
      }
   else if (is(x,"FLBiol")){
      return(ctrl)
      }
   }

checkTarget<-function(target)
    {
    # check that if max or min specified then no target & vice versa
    if (any((!is.na(target[,"min"]) | !is.na(target[,"max"])) & !is.na(target[,"val"]))) {
       warning("Can't specify a val and a min or max values")
       return(FALSE)}

    if (any((!is.na(target[,"min"]) & !is.na(target[,"max"])) & target[,"max"]<=target[,"min"])){
       warning("max less than than min value")
       return(FALSE)}

	# Should also check quantity

    return(TRUE)
    }

matrixTarget <- function(target)
    {
    #reorder columns for C code (???)
    target <- target[,trgtNms()]
    for(i in names(target))
        target[,i] <- as.double(target[,i])

    return(matrix(unlist(target),dim(target)))
    }

checkTarget<-function(target)
    {
    # check that if max or min specified then no target & vice versa
    if (any((!is.na(target[,"min"]) | !is.na(target[,"max"])) & !is.na(target[,"val"]))) {
       warning("Can't specify a val and a min or max values")
       return(FALSE)}

    if (any((!is.na(target[,"min"]) & !is.na(target[,"max"])) & target[,"max"]<=target[,"min"])){
       warning("max less than than min value")
       return(FALSE)}

	  # Should also check quantity

    return(TRUE)
    }

matrixEffort <- function(effort)
    matrix(apply(effort,2,as.double),dim(effort))


chkTrgtArrayIters <- function(object,trgtArray,sr)
    {
    if (is(object,'FLlst')) object <- object[[1]]
    # get iterations from trgtArray, stock, SR parameters and SR residuals
    its<-sort(unique(c(length(dimnames(trgtArray)$iter), dims(object)$iter, length(dimnames(sr$params[[1]])$iter), length(dimnames(sr$residuals[[1]])$iter))))
    if (length(its)>2 | (length(its)>1 & its[1]!=1)) stop("iter not 1 or n")
    if (length(its)==2 & length(dimnames(trgtArray)$iter == 1)){
        dmns<-dimnames(trgtArray)
        dmns$iter<-1:its[2]
        trgtArray<-array(trgtArray,dim=unlist(lapply(dmns,length)),dimnames=dmns)}

    return(trgtArray)
    }

# check target quantity is factor and that it is currently implemented
chkTargetQuantity <- function(target,object)
    {
    ordDmn<-function(dmn,val){
      tmp       <-1:length(dmn)
      names(tmp)<-dmn

      return(tmp[ac(val)])
      }

    if (!is(target[,"quantity"],"factor"))
        target[,"quantity"]<-factor(target[,"quantity"],quantityNms())
    if (!all(as.character(target[,"quantity"]) %in% quantityNms()))
        stop("invalid quantity in control target")
    if (any(as.character(target[,"quantity"]) %in% c("effort","costs","revenue","profit")))
        stop("fwd not yet implemented for 'effort','costs','revenue' or 'profit'")

    if (!is.numeric(target[,"season"])) target[,"season"]<-ordDmn(dimnames(m(object))$season,target[,"season"])
    if (!is.numeric(target[,"unit"]  )) target[,"unit"]  <-ordDmn(dimnames(m(object))$unit,  target[,"unit"])
    if (!is.numeric(target[,"area"]  )) target[,"area"]  <-ordDmn(dimnames(m(object))$area,  target[,"area"])

	  return(target)
    }
# fwd.R
# FLash/R/fwd.R
# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, Cefas
# Last Change: 13 Mar 2009 16:36
# $Id: fwd.R 377 2009-11-13 09:51:15Z lauriekell $

## fwd(FLStock)
if (!isGeneric("fwd"))
  setGeneric("fwd", function(object, fleets, ...)
	  standardGeneric("fwd"))

setMethod("fwd", signature(object="FLStock", fleets = "missing"),
    function(object, ctrl,
               sr =NULL, sr.residuals=NULL, sr.residuals.mult=TRUE,
               availability=NULL)
    {
    object<-CheckNor1(object)

    if (!(units(object@harvest)=="f"))
       stop("harvest slot has to have units of type 'f'")
    if (!validObject(ctrl))
       stop("ctrl not valid")

    yrs<-as.numeric(sort(unique(ctrl@target[,"year"])))

    ## check years
    ## years in ctrl have to be in order
    if (!all(yrs==sort(yrs)))
       stop("years in ctrl not in order")
    ## no gaps in years
    if (length(min(yrs):max(yrs))!=length(unique(yrs)))
       stop("years in ctrl not contiguous")
    ## years in ctrl have to be in FLStock object
    if (!all(ac(yrs) %in% ac(dims(object)$minyear:(dims(object)$maxyear))))
       stop("years in ctrl outside of those in stock object")
    ## Need year+1 in FLStock object
    if (max(yrs) == dims(object)$maxyear){
       endYr<-dims(object)$maxyear+1
       object <- window(object, end=dims(object)$maxyear+1)}
    else
       endYr<-NULL

    if (is.null(availability)) availability<-sweep(stock.n(object),c(1:4,6),apply(stock.n(object),c(1:4,6), sum),"/")
    sr<-setSR(sr=sr, object=object, yrs=yrs, sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals.mult, availability=availability)
    
    ## check iters in ctrl are '1 or n' and correct if necessary
    ctrl@trgtArray <- chkTrgtArrayIters(object,ctrl@trgtArray,sr)

    ## Season
    if (any(is.na(ctrl@target$season)) & dims(object)$season==1)
       ctrl@target$season<-1
    else if (any(is.na(ctrl@target$season)) & dims(object)$season>1)
       stop("need to specific season in target")

    ## Unit
    if (any(is.na(ctrl@target$unit)) & dims(object)$unit==1)
       ctrl@target$unit<-1
    else if (any(is.na(ctrl@target$unit)) & dims(object)$unit>1)
       stop("need to specific unit in target")

    ## Area
    if (any(is.na(ctrl@target$area)) & dims(object)$area==1)
       ctrl@target$area<-1
    else if (any(is.na(ctrl@target$area)) & dims(object)$area>1)
       stop("need to specific area in target")

    ctrl@target    <- chkTargetQuantity(ctrl@target,object)

    stock.n(object)[1,ac(min(ctrl@target[,"year"]))]<-NA

    ## Availability check
    if (dims(object)$area>1){
       if (is.null(availability)) stop("need to specify availability as areas>1")
       if (any(unlist(dims(availability))[c("age","min","max","unit","season","area","iter")]!=
               unlist(dims(m(object)))[   c("age","min","max","unit","season","area","iter")]))
          stop("dims mismatch in availability")
       if (!all(dimnames(availability)$year %in% (dimnames(m(object))$year)))
          stop("dim year mismatch in availability")
       }

    ##x<-.Call("_fwd_adolc_FLStock", object, matrixTarget(ctrl@target), ctrl@trgtArray, yrs, sr$model, sr$params, sr$residuals, sr$residuals.mult[[1]])
    x<-.Call("fwd_adolc_FLStock", object, matrixTarget(ctrl@target), ctrl@trgtArray, yrs, sr$model, sr$params, sr$residuals, sr$residuals.mult[[1]], availability)

    if (is.numeric(x)) stop(x)

    units(x@harvest)<-"f"

    stock.n(x)[is.na(stock.n(x))]<-0.0
    catch(   x)<-computeCatch(   x)
    landings(x)<-computeLandings(x)
    discards(x)<-computeDiscards(x)
    stock(   x)<-computeStock(   x)
    name(    x)<-name(object)
    desc(    x)<-desc(object)
    if (!is.null(endYr)) x <- window(x, end=endYr-1)

    return(x)
    }) 

#************ FLBiol and fleet methods ***********************************************

setMethod("fwd", signature(object='FLBiol', fleets='FLCatch'),
    function(object,fleets,ctrl,
                sr           =NULL,
                sr.residuals =NULL, sr.residuals.mult=TRUE){
                
    object<-FLBiols(object)
    if (length(object)>1) stop("Only implemented for 1 FLBiol")
    fleets <- FLFleets(FLFleet(fleets))
    fwd(object=object, fleets=fleets, ctrl=ctrl,sr=sr,sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals) 
    })

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


   ##check dims of fleet and biol
   dms<-chkDms(biol[[1]],fleets[[1]],yrs)
   #if (class(dms)=="character") 
   #   stop(dms)
      
   ## Check iters are 1 or n
   iter<-unique(c(unlist(lapply(fleets, function(x) dims(x)$iters)),
                  unlist(lapply(biol, function(x) dims(x)$iter))))
   if ((length(iter) >2) | (length(iter)==2 & !(1 %in% iter)))
      stop("Iter needs to be '1 or n'")

   if (!all(unlist(dims(biol[[1]])[  c("minyear","maxyear","season","area","iter")])==
            unlist(dims(fleets[[1]])[c("minyear","maxyear","season","area","iter")])))
      stop("year, season, area and iter have to match in FLBiol & FLFleet")
   
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
#      if(dims(sr.residuals)$iter != dms["iter"])
#         stop("iter in object and sr.residuals don't match")
    #sr<-setSR(sr=sr, object = object, yrs=yrs, sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals.mult)          
#browser()
#    sr<-setSR(sr=sr, object=biol[[1]], yrs=yrs, sr.residuals=sr.residuals, sr.residuals.mult=sr.residuals.mult)          
   #if (is.character(sr)) 
   #   stop(sr)

   dms<-dms[c("minyear","maxyear","unit","season","area","iter")]

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

fwdStock<-function(obj,fbar,sr,sr.residuals=NULL,distribution=NULL){

   if (is.null(sr.residuals))
      sr.residuals<-FLQuant(1,dimnames=dimnames(fbar))

   #### check dims
   if (!all(dimnames(fbar)$area   %in% dimnames(m(obj))$area))
      stop("Areas in FBar and obj don't match")
   if (!all(dimnames(fbar)$season %in% dimnames(m(obj))$season))
      stop("Seasons in FBar and obj don't match")
   if (!all(dimnames(fbar)$unit   %in% dimnames(m(obj))$unit))
      stop("Units in FBar and obj don't match")
   if (!all(dimnames(fbar)$year   %in% dimnames(m(obj))$year))
      stop("Years in FBar and obj don't match")
      
   yrs <-dimnames(fbar)$year

   if (is.null(distribution)){
      distribution<-sweep(stock.n(obj)[,yrs],c(1:4,6),apply(stock.n(obj)[,yrs],c(1:4,6),"sum"),"/")
      if (dims(fbar)$maxyear==dims(obj)$maxyear){
         distribution<-window(distribution,end=dims(obj)$maxyear+1)
         distribution[,ac(dims(obj)$maxyear+1)]<-distribution[,ac(dims(obj)$maxyear)]
         }}
         
   extendFlag<-FALSE
   if (dims(fbar)$maxyear==dims(obj)$maxyear){
      extendFlag<-TRUE
      obj<-window(obj,end=dims(obj)$maxyear+1)
      }

   ##Fishing Mortality
   fbRng             <-ac(range(obj,"minfbar"):range(obj,"maxfbar"))
   scaling           <-fbar/apply(harvest(obj)[fbRng,yrs],2:6,mean)
   harvest(obj)[,yrs]<-sweep(harvest(obj)[,yrs]@.Data,2:6,scaling@.Data,"*")

   stock.n(obj)[1,yrs]<-0
   for (iY in yrs){
      #cat("Year\t",iY,"\n")
      for (iU in dimnames(m(obj))$unit){
         #cat("Unit\t",iU,"\n")
         for (iS in dimnames(m(obj))$season){
           #cat("Season\t",iS,"\n")

           ## Recruitment
           if (iS %in% dimnames(params(sr))$season){
             recY<-ac(as.integer(iY)-range(obj,"min"))
             ssb.<-apply(ssb(obj)[,recY,iU,iS],c(3,5:6),sum)

             stock.n(obj)[1,iY,iU,iS]<-distribution[1,iY,iU,iS] #predict(sr,ssb=ssb.)*sr.residuals[,iY,iU,iS]
             }

           z                         <-m(obj)[,iY,iU,iS]+harvest(obj)[,iY,iU,iS]
           ## Catches
           catch.n(   obj)[,iY,iU,iS]<-stock.n(obj)[,iY,iU,iS]*harvest(obj)[,iY,iU,iS]/(z)*(1-exp(-z))
           landings.n(obj)[,iY,iU,iS]<-catch.n(obj)[,iY,iU,iS]*landings.n(obj)[,iY,iU,iS]/(landings.n(obj)[,iY,iU,iS]+discards.n(obj)[,iY,iU,iS])
           discards.n(obj)[,iY,iU,iS]<-catch.n(obj)[,iY,iU,iS]-discards.n(obj)[,iY,iU,iS]

           ## next season?
           if (iS!=dimnames(m(obj))$season[dims(obj)$season]){
             stock.n(obj)[,iY,iU,ac(as.integer(iS)+1)]<-stock.n(obj)[,iY,iU,iS]*exp(-z)
           ##next year
           }else if (as.integer(iY)<dims(obj)$maxyear){
             stock.n(obj)[-1,ac(as.integer(iY)+1),iU,1]<-stock.n(obj)[-dims(obj)$max,iY,iU,iS]*exp(-z[-dims(obj)$max])

           ## plusgroup
           if (!is.na(range(obj,"plusgroup")))
             stock.n(obj)[dims(obj)$max,ac(as.integer(iY)+1),iU,1]<-stock.n(obj)[dims(obj)$max,ac(as.integer(iY)+1),iU,1]+stock.n(obj)[dims(obj)$max,iY,iU,iS]*exp(-z[ dims(obj)$max])
           }
         }
       }
     }

   if (extendFlag)
      obj<-window(obj,end=dims(obj)$maxyear-1)

   return(obj)
   }

calcF<-function(m,catch,n)
   {
   its<-unique(c(dims(    m)$iter, 
                 dims(catch)$iter, 
                 dims(    n)$iter))

   if (dims(m)$iter==1) 
      m<-propagate(m,max(its))   
   if (dims(catch)$iter==1) 
      catch<-propagate(catch,max(its))
   if (dims(n)$iter==1) 
      n<-propagate(n,max(its))
   
   if (length(its)> 2) stop("iter mismatch")
   if (length(its)==2 & !any((1 %in% its))) stop("iters have to be 1 or n")
      
   res       <-.Call("CalcF",m,catch,n)
   units(res)<-"f"

   return(res)
   }

setGeneric('computeHarvest', function(object, ...)
		standardGeneric('computeHarvest'))

setMethod('computeHarvest', signature(object='FLStock'),
  function(object, catch)
     {
     if (names(dims(m(object)))[1]!="age") warning("quant dim not age, harvest only valid for age")
     
     res <-calcF(m(object),catch.n(object),stock.n(object))

     return(res)
     })     
     
setMethod('harvest', signature(object='FLBiol', catch='FLQuant'),
  function(object, catch)
     {
     if (names(dims(catch)[1])!="age") warning("quant dim not age, harvest only valid for age")

     res <-calcF(m(object),catch, n(object))

     return(res)
     })

setMethod('harvest', signature(object='FLBiol', catch='FLCatch'),
  function(object, catch)
     {
     if (names(dims(m(object)))[1]!="age") warning("quant dim not age, harvest only valid for age")

     res <-calcF(m(object),catch.n(catch),n(object))

     return(res)
     })

setMethod('harvest', signature(object='FLBiol', catch='FLMetier'),
  function(object, catch, spp)
     {
     if (names(dims(m(object)))[1]!="age") warning("quant dim not age, harvest only valid for age")

     res <-calcF(m(object),catch.n(catch)[[spp]],n(object))

     return(res)
     })

setMethod('harvest', signature(object='FLBiol', catch='FLMetiers'),
  function(object,catch,spp)
     {
     if (names(dims(m(object)))[1]!="age") warning("quant dim not age, harvest only valid for age")

     res <-calcF(m(object),catch.n(catch,spp),n(object))

     return(res)
     })

setMethod('harvest', signature(object='FLBiol', catch='FLFleet'),
  function(object, catch, spp, mtr)
     {
     if (names(dims(m(object)))[1]!="age") warning("quant dim not age, harvest only valid for age")

     res <-calcF(m(object),catch.n(catch, mtr, spp),n(object))

     return(res)
     })

# Rewrite fwdSetSRs to be methods rather than functions
# Still outputs the same thing (list of 4 elements, each of length number of SRs)

# Two dispatch methods.  One where SR is FLSR, the other SR is missing.
# If missing need to specify model and params.  Model can be text string, formula, or function (the sr function used by FLSR).
# Gives a total of four different methods of making the SR


# Will eventually be expanded so that the multiple SRs can be returned.  For the moment s a single SR is returned

if(!isGeneric('setSR'))
    setGeneric('setSR', function(sr, ...) standardGeneric('setSR'))

setMethod('setSR', signature(sr='FLSR'),
    function(sr,object,yrs,sr.residuals=NULL,sr.residuals.mult=TRUE,availability=NULL) {

    # Strip out sr model and params then call that method
    setSR(list(model = model(sr), params = params(sr)), object = object, yrs = yrs, sr.residuals = sr.residuals, sr.residuals.mult = sr.residuals.mult, availability=availability)
    }
)

setMethod('setSR', signature(sr='list'),
    function(sr,object,yrs,sr.residuals=NULL,sr.residuals.mult=TRUE,availability=NULL)
{
# ****** Check arguments are all present and of correct type *******
    if(!(is(sr,"list") & all(c("model","params") %in% names(sr))))
        stop("sr has to be a list with items 'model' & 'params'")
    model <- sr$model
    params <- sr$params

    if (!is(params,'FLPar') & !is(params,'FLQuant')) stop("params must be of type FLPar or FLQuant")
    # Sort out what type of object sr is; can be character string, formula or function
    if (!is(model,'formula') & !is(model,'character') & !is(model,'function'))  stop("model m ust be of type formula, character or function")
    if (!is(object,'FLStock') & !is(object,'FLBiol')) stop("object must be an FLStock or FLBiol")
    if (!is(yrs,'numeric') & !is(yrs,'character')) stop("yrs must be a numeric or character")

#****** Check yrs are in object year range *********
    if(is(yrs,'numeric')) yrs <- as(yrs,'character')
    if (!all(yrs  %in% dimnames(object@m)$year))
       stop("yrs range not found in object")

#****** Check and sort out residuals **********
    # Make an FLQuant of correct dimension with residuals
    if (!is(sr.residuals, "FLQuant") & !is.null(sr.residuals))
       stop("sr.residuals must be an FLQuant")
    dmns     <-dimnames(rec(object))
    # Add extra year - needed to smooth over bumps in fwd
    yrs   <-c(yrs,as.integer(yrs[length(yrs)])+1)
    dmns$year<-yrs
    # If no sr.residuals then thay are NULL and just a quant of 1s is passed
    residuals<-FLQuant(1,dimnames=dmns)
    resYrs<-yrs[yrs %in% dimnames(sr.residuals)$year]

    if(!is(sr.residuals,"NULL") & !any(dimnames(sr.residuals)$year %in% yrs[1:(length(yrs)-1)])) warning("Year range of residuals is not within yrs range")

    residuals[,resYrs]<-sr.residuals[,resYrs,,,,dimnames(residuals)$iter]

#****** Check and force iterations **********
    # Iters of stock numbers and residuals should be the same or one of them should be 1
    if (is(object,"FLStock") && (!all(dimnames(sr.residuals)$iter  %in% dimnames(object@stock.n)$iter) && !(dimnames(sr.residuals)$iter=="1" || dimnames(object@stock.n)$iter=="1")))
       stop("iters in sr.residuals do not those in object")
    if (is(object,"FLBiol") && (!all(dimnames(sr.residuals)$iter  %in% dimnames(object@n)$iter) && !(dimnames(sr.residuals)$iter=="1" || dimnames(object@n)$iter=="1")))
       stop("iters in sr.residuals do not those in object")

#****** Get model type ******************
    # make array of model type
    t.       <- vector(mode="numeric",length(yrs))
    names(t.)<-yrs
    if (is(model,"formula")) model<-SRModelName(model)
    t.[]     <-SRchar2code(model)
    model <-t.

#****** Check and force parameters for all years ********
    # Turn the FLPar or Quant into a Quant of right dimensions
    dmns <-list(params=SRParams(SRcode2char(ac(model[1]))),
                year  =yrs,
                unit  =dimnames(m(object))$unit,
                season=dimnames(m(object))$season,
                area  =dimnames(m(object))$area,
                iter  =dimnames(params)$iter)
    dmns.<-dmns
    dmns.[[2]]<-dmns[[2]][-length(dmns[[2]])]# dmns of original years

    # Trim off extra params if coming from FLSR (e.g. Ricker has extra param sigma)
    params <- params[dmns$params,]
    # Reorder if necessary
    if (length(length(dimnames(params)$params))>1)
      if (all(dimnames(sr)$params[1:2]==c("b","a")))
        params<-params[c("a","b")]

    if (!any(dimnames(params) %in% dmns.))
      stop("Dims for sr.params illegal")

    params<-validSRPar(object, sr=params, yrs=yrs, availability=availability)
    params<-FLQuant(params)

#****** Cobble together into output format **********
    # At the moment each element is only length 1.  Eventually, multiple SRs will be possible.
    res <- list(model=list(model),
        params=FLQuants(params),
        residuals=FLQuants(residuals),
        residuals.mult=list(sr.residuals.mult))

    return(res)
    }
)

SRchar2code<-function(strCode){
   res<-as.integer(switch(strCode, "mean"            = 1,
                                   "geomean"         = 1,
                                   "bevholt"         = 2,
                                   "ricker"          = 3,
                                   "segreg"          = 4,
                                   "shepherd"        = 5,
                                   "bevholt.d"       = 21,
                                   "bevholt.c.a"     = 22,
                                   "bevholt.c.b"     = 23,
                                   "bevholt.sv"      = 24,
                                   "bevholt.ndc"     = 25,
                                   "bevholt.ar1"     = 26,
                                   "ricker.d"        = 31,
                                   "ricker.c.a"      = 32,
                                   "ricker.c.b"      = 33,
                                   "ricker.sv"       = 34,
                                   "ricker.ndc"      = 35,
                                   "ricker.ar1"      = 36,
                                   default           = 0))

   return(res)
   }

SRcode2char<-function(strCode){
   res<-switch(strCode,  "1"    = "geomean",       
                         "2"    = "bevholt",       
                         "3"    = "ricker",        
                         "4"    = "segreg",        
                         "5"    = "shepherd",      
                         "21"   = "bevholt.d",     
                         "22"   = "bevholt.c.a",   
                         "23"   = "bevholt.c.b",   
                         "24"   = "bevholt.sv",    
                         "25"   = "bevholt.ndc",   
                         "26"   = "bevholt.ar1",   
                         "31"   = "ricker.d",      
                         "32"   = "ricker.c.a",    
                         "33"   = "ricker.c.b",    
                         "34"   = "ricker.sv",    
                         "35"   = "ricker.ndc",    
                         "36"   = "ricker.ar1",    
                         "0"    = default)         

   return(res)
   }

SRParams<-function(strCode){
   res<-as.integer(switch(strCode, "mean"            = 1,
                                   "geomean"         = 1,
                                   "bevholt"         = 2,
                                   "ricker"          = 2,
                                   "segreg"          = 2,
                                   "shepherd"        = 3,
                                   "bevholt.d"       = 3,
                                   "bevholt.c.a"     = 3,
                                   "bevholt.c.b"     = 3,
                                   "bevholt.sv"      = 2,
                                   "bevholt.ndc"     = 3,
                                   "bevholt.ar1"     = 2,
                                   "ricker.d"        = 3,
                                   "ricker.c.a"      = 3,
                                   "ricker.c.b"      = 3,
                                   "ricker.sv"       = 2,
                                   "ricker.ndc"      = 3,
                                   "ricker.ar1"      = 3,
                                   default           = 0))
                                   
                          
   return(c("a","b","c")[1:res])
   }   
# fwd.R
# FLash/R/fwd.R
# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, Cefas
# Last Change: 21 Jan 2010 09:51
# $Id: fwd.R 232 2009-04-30 15:44:58Z fscott $


setGeneric('validSRPar', function(object, ...)
		standardGeneric('validSRPar'))


.validSRPar<-function(object, sr, yrs=NULL, availability=NULL)
     {
     #### check that sr has dims in FLQuant
     if (!all(names(sr) %in% c("params","year","unit","season","area","iter")))
        stop("dims in sr not recognised")
        
     #### Check yrs
     if (!is.null(yrs)){
        yrs<-ac(yrs)
        if (!all(yrs %in% ac(dims(object)$minyear:dims(object)$maxyear)))
           stop("yrs exceed years in object")
     }else
        yrs<-ac(dims(object)$minyear:dims(object)$maxyear)

     if ("year" %in% names(sr)){
        if (!all(yrs[-length(yrs)] %in% dimnames(sr)$year)) ## cos of extra year needed in C++
           stop("yrs exceed years in sr")}
     else{
         dmns  <-dimnames(sr)

         params<-list(params=dmns$params,year=yrs)

         if ("iter"   %in% names(dmns)) params[["iter"]]  <-dmns$iter
         if ("unit"   %in% names(dmns)) params[["unit"]]  <-dmns$unit
         if ("area"   %in% names(dmns)) params[["area"]]  <-dmns$area
         if ("season" %in% names(dmns)) params[["season"]]<-dmns$season

         srTmp<-FLQuant(c(sr),        dimnames=dimnames(sr))
         res  <-FLQuant(as.numeric(0),dimnames=params)
         sr   <-sweep(res,(1:6)[c("params","year","unit","season","area","iter") %in% names(dimnames(sr))],srTmp,"+")
         }

     #### create FLQuant compatible FLPar
     sr <-FLPar(as.FLQuant(as.data.frame(sr)))

     #### Check availability
     #### Needed to distribute recruits if SR$area==1
     if (dims(object)$area>1){
        if (is.null(availability))
           stop("availability needs to be provided if multiple areas")

        if (!all(yrs %in% dimnames(availability)$year))
           stop("years in availability mismatch")
        }

     #### Check iters
     niter<-unique(c(dims(object)$iter,1))

     if (!max(dims(sr)$iter) %in% niter)
        stop("Iters in sr don't match those in object")

     if (!is.null(availability))
        if (!(dims(availability)$iter %in% niter))
           stop("Iters in availability don't match those in object")

     dmns<-list(params=dimnames(sr)$params,
                year  =yrs,
                unit  =dimnames(m(object))$unit,
                season=dimnames(m(object))$season,
                area  =dimnames(m(object))$area,
                iter  =dimnames(sr)$iter)

     #### check units, seasons and areas
     if (any(unlist(dims(        object)[c("season","area","unit")])>1) |
         any(unlist(dims(as.FLQuant(sr))[c("season","area","unit")])>1)){

        #### check units
        if (!all(dimnames(sr)$unit %in% dimnames(m(object))$unit))
           stop("unit in sr and object don't match")

        #### check season
        if (!all(dimnames(sr)$season %in% dimnames(m(object))$season))
           stop("season in sr and object don't match")

        #### check area
        if (!all(dimnames(sr)$area %in% dimnames(m(object))$area))
           stop("area in sr and object don't match")
        }

     res<-FLQuant(as.numeric(NA),dimnames=dmns)

     dm<-list(unit=dimnames(sr)$unit,season=dimnames(sr)$season,area=dimnames(sr)$area)
     if (dim(res)[3]==1 & dm$unit  =="unique") dm$unit  <-dimnames(res)$unit
     if (dim(res)[4]==1 & dm$season=="all")    dm$season<-dimnames(res)$season
     if (dim(res)[5]==1 & dm$area  =="unique") dm$area  <-dimnames(res)$area

##bug if sr doesn´t match control
     res[,dimnames(sr)$year,dm$unit,dm$season,dm$area,]<-as.FLQuant(sr)

     return(FLPar(res))
     }

setMethod('validSRPar', signature(object='FLStock'),
  function(object, sr, yrs=NULL, availability=NULL)
     {
     return(.validSRPar(object,sr,yrs,availability))
     })

setMethod('validSRPar', signature(object='FLBiol'),
  function(object, sr, yrs, availability)
     {
     return(.validSRPar(object,sr,yrs,availability))
     })

#setMethod('validSRPar', signature(object='FLBRP'),
#  function(object, sr, yrs)
#     {
#     return(.validSRPar(object,sr,yrs,availability(object)))
#     })

validSRRes<-function(object,sr,res){
   if (dims(res)$iter   != dims(object)$iter) stop("iters in residuals and object don't match")
   if (dims(res)$year   != dims(sr)$year)     stop("years in residuals and sr don't match")
   
   if (dims(res)$unit   != dims(sr)$units)    stop("unit in residuals and sr don't match")
   if (dims(res)$area   != dims(sr)$area)     stop("area in residuals and sr don't match")
   if (dims(res)$season != dims(sr)$season)   stop("season in residuals and sr don't match")
   }
    
